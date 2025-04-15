; Disassembly of the MTU-130 Floppy Disk Controller Boot ROM by Eduardo Casino (mail@eduardocasino.es)
;
; Original dump by Eric Wright (https://hackaday.io/ericinidahofalls)
;
; References:   MTU-130 Double Density Disk Controller Hardware Manual
;               MTU-130 Monomeg Single Board Computer Hardware Manual
;               MTU-130 CODOS Release 2.0 User Manual
;               See Hans Otten's site: http://retro.hansotten.nl/6502-sbc/mtu/
;

            ; MTU-130 (Monomeg) addresses
            ;
SYS1VIA     =       $BFE0
SYS1CTDAT   =       SYS1VIA
SYS1KBDAT   =       SYS1VIA + 1
SYS1CTDIR   =       SYS1VIA + 2

            ; CODOS 2.0 addresses
            ;
WARMRST     =       $0300

            ;   Disk Controller Registers
            ;
HSRCW       =       $FFE8           ; Read  - Hardware Status Read
                                    ; Write - Hardware Control Write
ADMA        =       $FFEA           ; Write - Set DMA Address Register

            ;   uPD765 Registers
            ;
MSTR        =       $FFEE           ; Read  - uPD765 Main Status Register
DATR        =       $FFEF           ; R/W   - uPD765 Data Register

            ; Interrupt vectors
            ;
IRQV        =       $02FA
NMIV        =       $02FD           ; NOTE: $02FD in the docs, $FDFD in the dump!!!

            ; Bootstrap loader page 0 addresses
            ;
            *=      $C3
            
            ; $00C3-$00D6 : NEC-765 Command Strings
            ;
SPECSTR:    .BYTE   ?, ?, ?, ?
RECASTR:    .BYTE   ?, ?, ?
SENSSTR:    .BYTE   ?, ?
READSTR:    .BYTE   ?, ?, ?, ?, ?
STASECT:    .BYTE   ?
            .BYTE   ?
ENDSECT:    .BYTE   ?
            .BYTE   ?, ?
            .BYTE   ?

PG0CMDS     =   SPECSTR
CMDSSIZE    =   * - PG0CMDS

            ; uPD765 command index
            ;
SPECIFY     =   0
RECAL       =   RECASTR - SPECSTR
SENSE       =   SENSSTR - SPECSTR
READ        =   READSTR - SPECSTR

            ; $00D7-$00DF : Scratch RAM, Result phase readouts from NEC-765
            ;
SCRTCHRAM:  .BYTE   ?, ?, ?, ?, ?, ?, ?, ?, ?


            ; Boot ROM start address
            ;
            *=      $FF00

START:      CLD                     ; Clear decimal mode
            LDX     #$FF            ; Set stack pointer
            TXS                     ;
            LDA     SYS1KBDAT       ; Test the keyboard "MOD" key
            AND     #$01            ;   Pressed?
            BNE     CPYCMDS         ;     Yes, continue
            JMP     WARMRST         ;     No, jump to CODOS warm reset routines

CPYCMDS:    LDX     #CMDSSIZE       ; Copy disk controller command strings 
CPYLOOP:    LDA     CMDSTR-1, X     ;  from PROM
            STA     PG0CMDS-1, X    ;  into memory, from $00C3 to $00D6 
            DEX                     ; Last byte?
            BNE     CPYLOOP         ; Loop if not

            LDA     HSRCW           ; Read Hardware Status Register
            BMI     INIT            ; Jump if no interrupt is pending
            JSR     SRVINT          ; Serve interrupt

            ; Set the drive in known status
            ;
INIT:       LDX     #SPECIFY        ; Establish disk operating parameters
            JSR     CMDEXEC
            LDX     #RECAL          ; Retract head of drive 0 to track 0
            JSR     CMDEXEC
            JSR     WSRVINT         ; Wait for and service interrupt
            LDA     SCRTCHRAM       ; Load result into accumulator
            AND     #$D8            ; 11 0 1 1000
                                    ; ||   | |
                                    ; ||   | +-> Not Ready
                                    ; ||   +---> Track 0 signal fails
                                    ; ++-------> Abnormal termination
            BNE     RESTART         ; Restart if anything goes wrong

            ; Read track 0, sector 0 of drive 0 into locations $FE00 through $FEFF.
            ;
            STA     STASECT         ; Set start sector to zero
            LDA     #$F8            ; Set destination address to $FE00 (See section 2.1.4 of the manual)
            JSR     READSECT        ; Read first sector   
            INC     STASECT         ; Set start sector to one

            ; Determine loading information for the actual program to be loaded by
            ; examining the following addresses:
            ;   FE3C = FINALS = Final sector number for the load.
            ;   FE3D = DMAPG = DMA Address code for loading of the first sector.
            ;   FE3E, FE3F = ENTRY = Address-1 of entry point into program.
            ;
            LDX     #$03            ; Number of bytes to read
GETADDR:    LDA     $FE3C,X         ; Read backwards
            PHA                     ; Save to the stack
            DEX                     ; Next byte
            BPL     GETADDR         ;
            PLA                     ; Get final sector number to read from stack
            STA     ENDSECT         ; Update the READ command string
            PLA                     ; Get the DMA address code for loading the first sector
                                    ;   (See section 2.1.4 of the manual)

            ; Read sector(s) from track 0
            ;
READSECT:   LDX     #$01            ; Set DMA direction bit to write
            STX     HSRCW           ;
            STA     ADMA            ; Set destination address to the DMA access register
            LDX     #READ           ; Execute read
            JSR     CMDEXEC         ;
RDLOOP:     LDA     HSRCW           ; Wait for command completion
            BMI     RDLOOP          ;
            JSR     READRES         ; And read result
            LDA     SCRTCHRAM       ; Load result into accumulator
            AND     #$D8            ; 11 0 1 1000
                                    ; ||   | |
                                    ; ||   | +-> Not Ready
                                    ; ||   +---> Track 0 signal fails
                                    ; ++-------> Abnormal termination
            BEQ     JUMP            ; If finished OK, return or jump to the enry point
            CMP     #$40            ; Not ready?
            BNE     RESTART         ; Try again
            LDA     SCRTCHRAM+1     ; Any other error, get status register 1
            AND     #$B7            ; 1 0 11 0 111
                                    ; |   ||   |||
                                    ; |   ||   ||+-> Missing address
                                    ; |   ||   |+--> Not writable (Why check this?)
                                    ; |   ||   +---> Read deleted data
                                    ; |   |+-------> Overrun
                                    ; |   +--------> CRC error
                                    ; +------------> End of cylinder
            CMP     #$80            ; If status is "End of cylinder", all good 
            BNE     RESTART         ; Otherwise, try again
JUMP:       RTS                     ; If READSECT was called as a subroutine for reading sector 0,
                                    ; just returns. If it was entered for reading the rest of the sectors,
                                    ; jumps to the entry point address, as address-1 was pushed onto the
                                    ; stack a few lines earlier

            ; Restart in case of failure
            ;
            ; First, output the status result bytes (preceded by the AA marker) to
            ; the top of the screen ($C000 in Data Bank 1)
            ;
RESTART:    LDY     #$FE            ; Select Data Bank 1
            STY     SYS1CTDAT       ;
            INY                     ; Set all lines of Port B as outputs
            STY     SYS1CTDIR       ;
            INY                     ; Zero Y
            STY     $C1             ; Store in $C1 (Bank 0, as it is a direct reference)
            LDA     #$C0            ; 
            STA     $C2             ; Store $C0 in %C2 (Again, Bank 0)
BYTECPY:    LDA     SCRTCHRAM-1,Y   ; Copy bytes from $00D6 to $00DA (Bank 0)
            STA     ($C1),Y         ; Into addresses $C000 to $C004 (Bank 1)
            INY                     ;
            CPY     #$05            ;
            BNE     BYTECPY         ; Loop until fourth byte
            LDY     #$FF            ; Select Data Bank 1 
            STY     SYS1CTDAT       ;
            JMP     CPYCMDS         ; And start again

CMDEXEC:    LDY     PG0CMDS,X       ; Command length to register Y
CMDLOOP:    INX
            LDA     PG0CMDS,X       ; Load command byte
            JSR     WAITRDY         ; Wait for controller ready
            STA     DATR            ; Send command byte
            DEY                     ; Loop until last command byte
            BNE     CMDLOOP         ;
            RTS

WAITRDY:    BIT     MSTR            ; Check if bit os the status register is 1 (Data ready)
            BPL     WAITRDY         ; Loop if not
            RTS

            ; Wait until interrupt and... 
            ;
WSRVINT:    LDA     HSRCW           ; Loop until interrupt
            BMI     WSRVINT

            ; ...Service interrupt
            ;
SRVINT:     LDX     #SENSE          ; Sense interrupt status
            JSR     CMDEXEC         ; Execute command

            ; Read command result
            ;
READRES:    LDX     #$00
            JSR     WAITRDY         ; Wait until ready
READDATR:   LDA     DATR            ; Read status register
            STA     SCRTCHRAM,X     ; Store to memory
            NOP                     ; Wait a couple of cycles
            INX                     ; Next byte
            LDA     $FFA5           ; OK, this is weird. At $FFA5 it is opcode BPL ($10)
                                    ; which is exactly what we need but... Why not LDA #$10?
                                    ; Probably because LDA # takes 2 cycles and abbsolute takes 4
                                    ; so that gives a little time to the uPD765 to put the data
                                    ; into the Data Register
            AND     MSTR            ; More data available from the data register?
            BNE     READDATR        ; Yes, continue reading
            RTS                     ; No, return

            ; Command strings used by the bootloader
            ;
            ;       String              | Command Name:
            ;
CMDSTR:     .BYTE   $03, $03, $6F, $20  ; SPECIFY:
                                        ;   Step Rate Time   = 10ms
                                        ;   Head Unload Time = 240ms
                                        ;   Head Load Time   = 32ms
                                        ;   DMA mode

            .BYTE   $02, $07, $00       ; RECALIBRATE drive 0

            .BYTE   $01, $08            ; SENSE INTERRUPT STATUS

            .BYTE   $09, $46, $00, $00  ; READ:
            .BYTE   $00, $00, $01, $00  ;   Double-density, no distinction between normal and deleted
            .BYTE   $0E, $FF            ;   data address marks, do not automatically continue from one
                                        ;   side to the other, drive 0, track 0, side 0, start at sector 0,
                                        ;   256 byte sector, last sector to read is #0, gap length $0E

            .BYTE   $AA                 ; This is output to the screen preceeding the status bytes
                                        ; when an error occurs.

CPYRGHT:    .TEXT   "(C) 1981 MTU", 0

            ; Filler bytes (addresses used by the adapter registers $FFE8-$FFEF)
            ;
            .BYTE   $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF

            ; FIller bytes
            ;
            .BYTE   $00, $00, $00, $00, $00, $DD, $4B, $69, $C5, $8C

            ; Vectors
            ;
            .WORD   IRQV        ; NMI
            .WORD   START       ; RESET
            .WORD   NMIV        ; IRQ

            .END
