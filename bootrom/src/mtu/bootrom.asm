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
SYS1VIA     =   $BFE0
SYS1CTDAT   =   SYS1VIA
SYS1KBDAT   =   SYS1VIA + 1
SYS1CTDIR   =   SYS1VIA + 2

            ; CODOS 2.0 addresses
            ;
WARMRST     =   $0300

            ;   Disk Controller Registers
            ;
HSRCW       =   $FFE8               ; Read  - Hardware Status Read
                                    ; Write - Hardware Control Write
ADMA        =   $FFEA               ; Write - Set DMA Address Register

            ;   uPD765 Registers
            ;
MSTR        =   $FFEE               ; Read  - uPD765 Main Status Register
DATR        =   $FFEF               ; R/W   - uPD765 Data Register

            ; Interrupt vectors
            ;
NMIV        =   $02FA
IRQV        =   $02FD               ; NOTE: $02FD in the docs, $FDFD in the dump!!!

            ; Boot sector offsets
            ;
FINALS      =   $3C                 ; Final sector number for the load
DMAPG       =   $3D                 ; DMA Address code for loading of the first sector
ENTRY       =   $3E                 ; Address-1 of entry point into program

            ; Bootstrap loader page 0 addresses
            ;

            .zeropage

SCREEN      =   $C000               ; Address of MTU-130 screen
SCREENPTR:  .res    2

            ; $00C3-$00D6 : NEC-765 Command Strings
            ;
SPECSTR:    .res    4
RECASTR:    .res    3
SENSSTR:    .res    2
READSTR:    .res    5
STASECT:    .res    1
            .res    1
ENDSECT:    .res    1
            .res    2
            .res    1

PG0CMDS     :=  SPECSTR
CMDSSIZE    =   * - SPECSTR

            ; uPD765 command index
            ;
SPECIFY     =   0
RECAL       =   RECASTR - SPECSTR
SENSE       =   SENSSTR - SPECSTR
READ        =   READSTR - SPECSTR

            ; $00D7-$00DF : Scratch RAM, Result phase readouts from NEC-765
            ;
SCRTCHRAM:  .res    9

            .segment "buffer"

BUFFER:     .res    $FF

            .code

            ; Boot ROM start address
            ;
START:      cld                     ; Clear decimal mode
            ldx     #$FF            ; Set stack pointer
            txs                     ;
            lda     SYS1KBDAT       ; Test the keyboard "MOD" key
            and     #$01            ;   Pressed?
            bne     CPYCMDS         ;     Yes, continue
            jmp     WARMRST         ;     No, jump to CODOS warm reset routines

CPYCMDS:    ldx     #CMDSSIZE       ; Copy disk controller command strings 
CPYLOOP:    lda     CMDSTR-1, X     ;  from PROM
            sta     PG0CMDS-1, X    ;  into memory, from $00C3 to $00D6 
            dex                     ; Last byte?
            bne     CPYLOOP         ; Loop if not

            lda     HSRCW           ; Read Hardware Status Register
            bmi     INIT            ; Jump if no interrupt is pending
            jsr     SRVINT          ; Serve interrupt

            ; Set the drive in known status
            ;
INIT:       ldx     #SPECIFY        ; Establish disk operating parameters
            jsr     CMDEXEC
            ldx     #RECAL          ; Retract head of drive 0 to track 0
            jsr     CMDEXEC
            jsr     WSRVINT         ; Wait for and service interrupt
            lda     SCRTCHRAM       ; Load result into accumulator
            and     #$D8            ; 11 0 1 1000
                                    ; ||   | |
                                    ; ||   | +-> Not Ready
                                    ; ||   +---> Track 0 signal fails
                                    ; ++-------> Abnormal termination
            bne     RESTART         ; Restart if anything goes wrong

            ; Read track 0, sector 0 of drive 0 into locations $FE00 through $FEFF.
            ;
            sta     STASECT         ; Set start sector to zero
            lda     #$F8            ; Set destination address to $FE00 (See section 2.1.4 of the manual)
            jsr     READSECT        ; Read first sector   
            inc     STASECT         ; Set start sector to one

            ; Determine loading information for the actual program to be loaded by
            ; examining the following addresses:
            ;   FE3C = FINALS = Final sector number for the load.
            ;   FE3D = DMAPG = DMA Address code for loading of the first sector.
            ;   FE3E, FE3F = ENTRY = Address-1 of entry point into program.
            ;

            ldx     #$03            ; Number of bytes to read
GETADDR:    lda     BUFFER+FINALS,X  ; Read backwards
            pha                     ; Save to the stack
            dex                     ; Next byte
            bpl     GETADDR         ;
            pla                     ; Get final sector number to read from stack
            sta     ENDSECT         ; Update the READ command string
            pla                     ; Get the DMA address code for loading the first sector
                                    ;   (See section 2.1.4 of the manual)

            ; Read sector(s) from track 0
            ;
READSECT:   ldx     #$01            ; Set DMA direction bit to write
            stx     HSRCW           ;
            sta     ADMA            ; Set destination address to the DMA access register
            ldx     #READ           ; Execute read
            jsr     CMDEXEC         ;
RDLOOP:     lda     HSRCW           ; Wait for command completion
            bmi     RDLOOP          ;
            jsr     READRES         ; And read result
            lda     SCRTCHRAM       ; Load result into accumulator
            and     #$D8            ; 11 0 1 1000
                                    ; ||   | |
                                    ; ||   | +-> Not Ready
                                    ; ||   +---> Track 0 signal fails
                                    ; ++-------> Abnormal termination
            beq     JUMP            ; If finished OK, return or jump to the enry point
            cmp     #$40            ; Not ready?
            bne     RESTART         ; Try again
            lda     SCRTCHRAM+1     ; Any other error, get status register 1
            and     #$B7            ; 1 0 11 0 111
                                    ; |   ||   |||
                                    ; |   ||   ||+-> Missing address
                                    ; |   ||   |+--> Not writable (Why check this?)
                                    ; |   ||   +---> Read deleted data
                                    ; |   |+-------> Overrun
                                    ; |   +--------> CRC error
                                    ; +------------> End of cylinder
            cmp     #$80            ; If status is "End of cylinder", all good 
            bne     RESTART         ; Otherwise, try again
JUMP:       rts                     ; If READSECT was called as a subroutine for reading sector 0,
                                    ; just returns. If it was entered for reading the rest of the sectors,
                                    ; jumps to the entry point address, as address-1 was pushed onto the
                                    ; stack a few lines earlier

            ; Restart in case of failure
            ;
            ; First, output the status result bytes (preceded by the AA marker) to
            ; the top of the screen ($C000 in Data Bank 1)
            ;
RESTART:    ldy     #$FE            ; Select Data Bank 1
            sty     SYS1CTDAT       ;
            iny                     ; Set all lines of Port B as outputs
            sty     SYS1CTDIR       ;
            iny                     ; Zero Y
            sty     SCREENPTR       ; Store in SCREENPTR (Bank 0, as it is a direct reference)
            lda     #>SCREEN        ; 
            sta     SCREENPTR+1     ; Store $C0 in %C2 (Again, Bank 0)
BYTECPY:    lda     SCRTCHRAM-1,Y   ; Copy bytes from $00D6 to $00DA (Bank 0)
            sta     (SCREENPTR),Y   ; Into addresses $C000 to $C004 (Bank 1)
            iny                     ;
            cpy     #$05            ;
            bne     BYTECPY         ; Loop until fourth byte
            ldy     #$FF            ; Select Data Bank 1 
            sty     SYS1CTDAT       ;
            jmp     CPYCMDS         ; And start again

CMDEXEC:    ldy     PG0CMDS,X       ; Command length to register Y
CMDLOOP:    inx
            lda     PG0CMDS,X       ; Load command byte
            jsr     WAITRDY         ; Wait for controller ready
            sta     DATR            ; Send command byte
            dey                     ; Loop until last command byte
            bne     CMDLOOP         ;
            rts

WAITRDY:    bit     MSTR            ; Check if bit os the status register is 1 (Data ready)
OPCODE10:   bpl     WAITRDY         ; Loop if not
            rts

            ; Wait until interrupt and... 
            ;
WSRVINT:    lda     HSRCW           ; Loop until interrupt
            bmi     WSRVINT

            ; ...Service interrupt
            ;
SRVINT:     ldx     #SENSE          ; Sense interrupt status
            jsr     CMDEXEC         ; Execute command

            ; Read command result
            ;
READRES:    ldx     #$00
            jsr     WAITRDY         ; Wait until ready
READDATR:   lda     DATR            ; Read status register
            sta     SCRTCHRAM,X     ; Store to memory
            nop                     ; Wait a couple of cycles
            inx                     ; Next byte
            lda     OPCODE10        ; OK, this is weird. At $FFA5 it is opcode bpl ($10)
                                    ; which is exactly what we need but... Why not lda #$10?
                                    ; Probably because lda # takes 2 cycles and abbsolute takes 4
                                    ; so that gives a little time to the uPD765 to put the data
                                    ; into the Data Register
            and     MSTR            ; More data available from the data register?
            bne     READDATR        ; Yes, continue reading
            rts                     ; No, return

            ; Command strings used by the bootloader
            ;
            ;       String              | Command Name:
            ;
CMDSTR:     .byte   $03, $03, $6F, $20  ; SPECIFY:
                                        ;   Step Rate Time   = 10ms
                                        ;   Head Unload Time = 240ms
                                        ;   Head Load Time   = 32ms
                                        ;   DMA mode

            .byte   $02, $07, $00       ; RECALIBRATE drive 0

            .byte   $01, $08            ; SENSE INTERRUPT STATUS

            .byte   $09, $46, $00, $00  ; READ:
            .byte   $00, $00, $01, $00  ;   Double-density, no distinction between normal and deleted
            .byte   $0E, $FF            ;   data address marks, do not automatically continue from one
                                        ;   side to the other, drive 0, track 0, side 0, start at sector 0,
                                        ;   256 byte sector, last sector to read is #0, gap length $0E

            .byte   $AA                 ; This is output to the screen preceeding the status bytes
                                        ; when an error occurs.

CPYRGHT:    .byte   "(C) 1981 MTU", 0

            ; Filler bytes (addresses used by the adapter registers $FFE8-$FFEF)
            ;
            .byte   $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF

            ; FIller bytes
            ;
            .byte   $00, $00, $00, $00, $00, $DD, $4B, $69, $C5, $8C

            ; Vectors
            ;
            .segment "vectors"

            .addr   NMIV        ; NMI
            .addr   START       ; RESET
            .addr   IRQV        ; IRQ

            .end
