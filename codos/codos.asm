; Disassembly of CODOS 2.0 by Eduardo Casino (mail@eduardocasino.es)
;

; da65 V2.18 - Ubuntu 2.19-1
; Created:    2023-11-25 21:28:31
; Input file: codos.bin
; Page:       1

    ; Possible values: 14, 15, 17
    ; If none is specified, 15 is assumed
    ; Codos 11 is the same as Codos 15

.ifndef CODOS2_VER
    CODOS2_VER  = 15
.endif

    FNAMLEN         = 12            ; Max file name length

    ;*****  CODOS SVC EQUATES

    SVCRET          = 0             ; RETURN WITH REGISTER PRINT
    SVCRTS          = 1             ; RETURN WITHOUT REGISTER PRINT
    SVCOUM          = 2             ; OUTPUT INLINE MESSAGE SVC
    SVCINB          = 3             ; INPUT BYTE SVC
    SVCOUB          = 4             ; OUTPUT BYTE SVC
    SVCINL          = 5             ; INPUT LINE SVC
    SVCOUL          = 6             ; OUTPUT LINE SVC
    SVCOUS          = 7             ; OUTPUT STRING SVC
    SVCDCX          = 8             ; DECODE ASCII HEX TO BINARY
    SVCDCD          = 9             ; DECODE ASCII DECIMAL TO BINARY
    SVCENX          = 10            ; ENCODE BINARY TO ASCII HEX
    SVCEND          = 11            ; ENCODE BINARY TO ASCII DECIMAL
    SVCDFB          = 12            ; ESTABLISH DEFAULT BUFFER ADDRESS
    SVCMON          = 13            ; EXECUTE CODOS MONITOR COMMAND
    SVCQCA          = 14            ; QUERY CHANNEL ASSIGNMENT
    SVCINR          = 15            ; READ RECORD FROM FILE
    SVCOUR          = 16            ; WRITE RECORD TO FILE
    SVCBOF          = 17            ; POSITION TO BEGINNING OF FILE
    SVCEOF          = 18            ; POSITION FILE TO END OF FILE
    SVCPSF          = 19            ; POSITION FILE TO ARBUTRARY LOCATION
    SVCQFP          = 20            ; QUERY FILE POSITION
    SVCASS          = 21            ; ASSIGN CHANNEL TO FILE OR DEVICE
    SVCFRE          = 22            ; FREE CHANNEL
    SVCTNC          = 23            ; TRUNCATE FILE AT PRESENT POSITION
    SVCSPI          = 24            ; DEFINE USER INTERRUPT SERVICE ROUTINE
    SVCUER          = 25            ; DEFINE USER ERROR RECOVERY
    SVCCER          = 26            ; RESTORE CODOS ERROR RECOVERY
    SVCP16          = 27            ; ENTER 16 BIT PSEUDO PROCESSOR
    SVCQVN          = 28            ; QUERY CODOS VERSION
    SVCFLS          = 29            ; QUERY FILE STATUS
    SVCDAT          = 30            ; QUERY CURRENT DATE

    BANKSW          = $0100

    COL             = $0200         ; Current column location of text cursor, 1 - 80.
    LINE            = $0201         ; Current line number of text cursor, 1 - NLINET.
    NLINET          = $021E         ; Number of text lines in the text window. 
    YTDOWN          = $021F         ; 255-(Y coordinate of top of the text window).
    CURDLA          = $0222         ; Determines cursor blink speed, 0=no blink.
    NOLFCR          = $0210         ; If bit 7=1 then no automatic line feed after CR.
    NOSCRL          = $0211         ; If bit 7=1 then instead of scrolling, the text window is
                                    ; cleared and the cursor is homed when text goes beyond the
                                    ; bottom line.
    NOCLIK          = $0213         ; BIT 7=1=NO KEY CLICK

    UNDRLN          = $0212         ; If bit then all characters underlined when drawn. 
    NOBELL          = $0214         ; If bit then BEL character is ignored. 
    RVIDEO          = $0215         ; If bit 7=1 then characters are drawn in reverse video.
    SHODEL          = $0216         ; If bit 7 then display DEL (RUBOUT) as a character shape.
    SHOUL           = $0217         ; If bit 7=1 then character cell is erased before the
                                    ; underline character is drawn.
    EXCCP           = $0218         ; If bit 7=1 then call user control character processor.
    EXTHI           = $0219         ; If bit 7=1 then call user routine to process all characters
                                    ; with bit 7 set.
    EXFONT          = $021A         ; If bit 7=1 then use external font table.

    BELPER          = $0227         ; Bell sound waveform period in units of 200 microseconds.
    BELVOL          = $0228         ; Bell sound volume, $00 = minimum, $7F = maximum.
    BELCY           = $0229         ; Bell sound duration in units of complete waveform cycles.
    QEXCC           = $022F         ; Address of external control character processor if used.
    QEXFNT          = $0231         ; Address of external font table if used.
    QEXHI7          = $0233         ; Address of external processor for characters with bit 7=1.
    EXFTBK          = $0237         ; Memory bank number containing external font table.
    YLNLIM          = $0238         ; Line size limit for INLINE and EDLINE entry points.
    KEYSTR          = $0400         ; (256 bytes) Function key substitution string table. Contains 8 groups
                                    ; of 32 characters which represent the the character
                                    ; strings to be substituted for the associated function keys
                                    ; when using the input-line or edit-line functions. See
                                    ; INLINE entry point for details. 
    LEGTBL          = $05C0         ; (64 bytes) Function key legend table. Contains 8 groups of 8 characters which are the displayed legends for the 8 function
                                    ; keys. The label for the f1 key is first. See DRWLEG entry
                                    ; point for details. 
    TABTBL          = $06E0         ; (32 bytes) Tab stop table. A tab stop is located at the column number
                                    ; specified by cach non-zero byte. See OUTCH entry point for
                                    ; details. 

    SEEIO           = $02F9         ; I-O space enable semaphore

    NMIPRC          = $02FA         ; Jump to NMI processor
    IRQBRK          = $02FD         ; Jump to IQR and BRK processor
    WARMRS          = $0300         ; Jump to operating system warm reset entry
    CNTRLC          = $0303         ; Jump executed when CNTRL-C is entered from console.

    ; RELEVANT KEYBOARD AND TEXT DISPLAY DRIVER ENTRY POINTS
    ;
    GETKEY          = $0306         ; Wait until a keyboard key is struck and return character in A
    OUTCH           = $0309         ; Display printable character or interpret control character.
    TSTKEY          = $030C         ; Test if a key is pressed
    INITIO          = $030F         ; Clear screen and set default values of display parameters.
    CLRDSP          = $0312         ; Clear the entire 480 by 256 screen.
    DRWLEG          = $0315         ; Draw the function key legend boxes and their labels. 
    INLINE          = $031E         ; Input an entire line from the keyboard
    INITTW          = $0363         ; Initialize the text window to 24 lines and clear the text window 
    DEFTW           = $0366         ; Set the position and size of the text window. 
    CLRHTW          = $0369         ; Clear the text window and home the cursor. 
    HOMETW          = $036C         ; Place the cursor in the home position (COL=1, LINE=1) 
    

    SVIA1PORT       = $BFE0         ; System 1 6522 System control port data register
    SVIA1DIR        = $BFE2         ; System 1 6522 System control port direction register
    BNKCTL          = SVIA1PORT     ; System 1 6522 (Bank control data register)

    SVCPROC         = $DD20         ; SVC processor
    LDD23           = $DD23

    USRRAM          = $C000         ; K-1013 onboard user RAM
    SYSRAM          = $E000         ; K-1013 onboard system RAM

    IOENABLE        = $FFFE         ; Enable I/O space from $BE00 to $BFFF
    IODISABLE       = $FFFF         ; Disable I/O space (enable RAM) from $BE00 to $BFFF

            ;   Disk Controller Registers
            ;
    HSRCW           = $FFE8         ; Read  - Hardware Status Read
                                    ; Write - Hardware Control Write
    ADMA            = $FFEA         ; Write - Set DMA Address Register

            ;   uPD765 Registers
            ;
    MSTR            = $FFEE         ; Read  - uPD765 Main Status Register
    DATR            = $FFEF         ; R/W   - uPD765 Data Register

             ; uPD765 command index
    SPECIFY         = $00
    RECALIBRATE     = $04
    SEEK            = $07
    SENSEINT        = $0B
    READWRITE       = $0D
    FORMAT          = $17
    SENSEDRV        = $1E

            .segment "header"

            .byte "MTU-130 CODOS 2.0", $0D
            .byte "COPYRIGHT (C) 1981, MICRO TECHNOLOGY UNLIMIMITED", $0D
            .byte "PO BOX 12106, 2806 HILLSBOROUGH ST.", $0D
            .byte "RALEIGH, NC 27605 USA", $0D
            .byte "Written by Bruce D. Carbrey", $0D
            .byte "ASM 1/18/82 patch 6/14/82", $0D, $0D, $0D, $20

            .segment "zp" : zeropage

            ; $B0 - $C0 Pseudo registers

U0:         .res 2                  ; $B0
U1:         .res 2                  ; $B2
U2:         .res 2                  ; $B4
U3:         .res 2                  ; $B6
U4:         .res 2                  ; $B8
U5:         .res 2                  ; $BA  Input buffer pointer
U6:         .res 2                  ; $BC  Output buffer pointer
U7:         .res 3                  ; $BE  File position (3 bytes)

            ; $C1 - $EC : Scratch RAM used by CODOS nucleus,
            ; SVC Processor and Command Proc. 

            .export P0SCRATCH, MEMBUFF, PCSAVE, CMDLIDX

P0SCRATCH:  .res 1                  ; $C1
            .res 1                  ; $C2 
MEMBUFF:    .res 2                  ; $C3-$C4 (word) Pointer to buffer for memory copy operations
MEMCOUNT:   .res 2                  ; $C5-$C6 (word) Counter for memory copy operations
            .res 1                  ; $C7
            .res 1                  ; $C8
            .res 1                  ; $C9
            .res 1                  ; $CA
INPBUFP:    .res 2                  ; $CB-$CC (word) Pointer to input buffer
OUTBUFP:    .res 2                  ; $CD-$CE (word) Pointer to output buffer
            .res 1                  ; $CF
            .res 1                  ; $D0
            .res 1                  ; $D1
L00D2:      .res 1                  ; $D2  $D2-$D9 is a temporary area
            .res 1                  ; $D3       with different uses, like
            .res 1                  ; $D4       the switch and jump routine
            .res 1                  ; $D5
            .res 1                  ; $D6
            .res 1                  ; $D7
TMPPTR:     .res 2                  ; $D8-$D9 (word) Temporary pointer
PCSAVE:     .res 2                  ; $DA-$DB (word) Program counter
            .res 1                  ; $DC
            .res 1                  ; $DD
            .res 1                  ; $DE
            .res 1                  ; $DF
            .res 1                  ; $E0
            .res 1                  ; $E1
            .res 1                  ; $E2
            .res 1                  ; $E3
            .res 1                  ; $E4
            .res 1                  ; $E5
            .res 1                  ; $E6
DMAENC:     .res 1                  ; $E7 (byte) Encoded K-1013 DMA address
            .res 1                  ; $E8
            .res 1                  ; $E9
            .res 1                  ; $EA
CMDLIDX:    .res 1                  ; $EB  (byte) Current char position in command line
INTSVA:     .res 1                  ; $EC  (byte) Accumulator save during SVC or IRQ processing.
    
            ; $ED - $EF : Global RAM used by CODOS
 
            .exportzp ERRNUM, SVCENB

ERRNUM:     .res 1                  ; $ED  Error number for user-defined error recovery.
SVCENB:     .res 1                  ; $EE  ADDRESS OF SVC ENABLE FLAG
SAVEACC:    .res 1                  ; $EF  TODO: Unknown

            ; $F0 - $FF : Scratch RAM for console I-0. 
 
UNKNWN2:    .res 1                  ; $F0
UNKNWN3:    .res 1                  ; $F1
UNKNWN4:    .res 1                  ; $F2
UNKNWN5:    .res 1                  ; $F3
UNKNWN6:    .res 1                  ; $F4
UNKNWN7:    .res 1                  ; $F5
UNKNWN8:    .res 1                  ; $F6
UNKNWN9:    .res 1                  ; $F7
UNKNWN10:   .res 1                  ; $F8
UNKNWN11:   .res 1                  ; $F9
UNKNWN12:   .res 1                  ; $FA
UNKNWN13:   .res 1                  ; $FB
UNKNWN14:   .res 1                  ; $FC
UNKNWN15:   .res 1                  ; $FD
UNKNWN16:   .res 1                  ; $FE
UNKNWN17:   .res 1                  ; $FF

            .segment "scratch1"

TEMP1:      .res 1                  ; $0283
TEMP2:      .res 1                  ; $0284
SAVEY1:     .res 1                  ; $0285 Used to preserve Y register during disk operations
SAVEX1:     .res 1                  ; $0286
SAVEX2:     .res 1                  ; $0287
SAVEA1:     .res 1                  ; $0288
SAVEY2:     .res 1                  ; $0289
SAVEX3:     .res 1                  ; $028A
SAVEY3:     .res 1                  ; $028B
SAVEX4:     .res 1                  ; $028C
SAVEX5:     .res 1                  ; $028D
SAVEY4:     .res 1                  ; $028E
SAVEX6:     .res 1                  ; $028F
            .res 1                  ; $0290
TEMP3:      .res 1                  ; $0291
SAVEA2:     .res 1                  ; $0292
            .res 1                  ; $0293
SAVEA3:     .res 1                  ; $0294
SAVEX7:     .res 1                  ; $0295
            .res 1                  ; $0296
SAVEX8:     .res 1                  ; $0297
            .res 1                  ; $0298
SAVEA4:     .res 1                  ; $0299
SAVEX9:     .res 1                  ; $029A
TEMP4:      .res 1                  ; $029B
SAVEX10:    .res 1                  ; $029C
            .res 1                  ; $029D
            .res 1                  ; $029E
UNKFLAG1:   .res 1                  ; $029F
            .res 1                  ; $02A0
SAVEY5:     .res 1                  ; $02A1
SAVEY6:     .res 1                  ; $02A2
            .res 1                  ; $02A3
            .res 1                  ; $02A4
            .res 1                  ; $02A5
            .res 1                  ; $02A6
            .res 1                  ; $02A7
            .res 1                  ; $02A8
            .res 1                  ; $02A9
            .res 1                  ; $02AA
            .res 1                  ; $02AB
            .res 1                  ; $02AC
            .res 1                  ; $02AD
            .res 1                  ; $02AE
            .res 1                  ; $02AF


    YOUT            = $D27D         ; "Y" output (console and printer) entry point
                                    ; Must be set by hand at STARTUP.J with
                                    ; SET D27D=20 21 E6 (jsr JCOUT)
    PRTOUT          = $D280         ; Printer output entry point

            .segment "cmdproc"

            .segment "codos"
            
            ; Jump table (page 179)
            ;
            jmp     COLDST
JWARMST:    jmp     WARMST
JGETKEY:    jmp     GETKEY          ; Console character input
JOUTCH:     jmp     OUTCH           ; Console character output
JTSTKEY:    jmp     TSTKEY          ; Console Key-depressed test subroutine
            jmp     NMIPROC
            jmp     IRQPROC
LE615:      jmp     LDD23
JERROR37:   jmp     ERROR37         ; Jump to "Required software package not loaded" error message
JINLINE:    jmp     INLINE          ; Jump to input an entire line from the keyboard
            jmp     CIN
JCOUT:      jmp     COUT            ; Jump to console-character-out routine with CTRL-S/Q (XON/XOFF)
            jmp     JERROR37        ; Required software package not loaded in memory
            jmp     JERROR37        ; Required software package not loaded in memory

            ; Device name table

DNT:        .byte   "N"             ; Null device driver
            .byte   "C"             ; Console device
            .byte   "P"             ; Printer device
            .byte   "Y"             ; "Y" device: outputs to console and printer at once
            .byte   $00             ; Reserved for custom devices
            .byte   $00             ;
            .byte   $00             ;
            .byte   $00             ;

            ; Device driver dispatch table for input

DDTI:       .word   NULDRVI         ; Input null driver device (DTI=$80)
CINP:       .word   CIN             ; Console input routine (DTI=$82)
            .word   ERROR33         ; Input from output-only device, or vice-versa
            .word   ERROR33         ; Input from output-only device, or vice-versa
            .word   ERROR33         ; Input from output-only device, or vice-versa
            .word   ERROR33         ; Input from output-only device, or vice-versa
            .word   ERROR33         ; Input from output-only device, or vice-versa
            .word   ERROR33         ; Input from output-only device, or vice-versa

            ; Device driver dispatch table for output

DDTO:       .word   NULDRVO         ; Output null driver device (DTI=$80)
COUTP:      .word   COUT            ; Console output routine (DTI = $82)
            .word   PRTOUT          ; Printer output routine (DTI = $84)
            .word   YOUT            ; "Y" output routine (DTI = $86)
            .word   ERROR33         ; Input from output-only device, or vice-versa
            .word   ERROR33         ; Input from output-only device, or vice-versa
            .word   ERROR33         ; Input from output-only device, or vice-versa
            .word   ERROR33         ; Input from output-only device, or vice-versa

            ; I/O Channel Table

            .export CHANN1

IOCHTBL:    .byte   $00             ; Channel 0: reserved for internal CODOS operation
CHANN1:     .byte   $82             ; Channel 1: Input commands to system monitor
CHANN2:     .byte   $82             ; Channel 2: Output from system monitor
            .byte   $00             ; Channel 3: Available (Input preferable)
            .byte   $00             ; Channel 4: Available (Input preferable)
            .byte   $00             ; Channel 5: Standard input for programs
            .byte   $00             ; Channel 6: Standard output for programs
            .byte   $00             ; Channel 7: Available
            .byte   $00             ; Channel 8: Available (Output preferable)
            .byte   $00             ; Channel 9: Available (Output preferable)

            ; 
LE65C:      .byte   $06
FILEDRV:    .byte   $00             ; Drive of file
LE65E:      .byte   $00

            .byte   $00
            .byte   $00
            .byte   $00

            .byte   $00
            .byte   $00
            .byte   $00

            .byte   $00
            .byte   $00
            .byte   $E2

            .byte   $88
            .byte   $00
            .byte   $00

            .byte   $00
            .byte   $00
            .byte   $00

            .byte   $00
            .byte   $00
            .byte   $00

            .byte   $00
            .byte   $00
            .byte   $00

            .byte   $E1
            .byte   $84
            .byte   $00

            .byte   $00
            .byte   $00
            .byte   $00

            .byte   $00
            .byte   $00
            .byte   $00

            .byte   $00
            .byte   $00
            .byte   $00

            .byte   $00
            .byte   $E0
            .byte   $80

            .byte   $00
            .byte   $00
            .byte   $00

            .byte   $00
            .byte   $00
            .byte   $00

            .byte   $00
            .byte   $00
            .byte   $00

            .byte   $00
            .byte   $00
            .byte   $D7

            .byte   $5C
            .byte   $00
            .byte   $00

            .byte   $00
            .byte   $00
            .byte   $00

            .byte   $00
            .byte   $00
            .byte   $00

            .byte   $00
            .byte   $00
            .byte   $00

            .byte   $D6
            .byte   $58
            .byte   $00

            .byte   $00
            .byte   $00
            .byte   $00

            .byte   $00
            .byte   $00
            .byte   $00

            .byte   $00
            .byte   $00
            .byte   $00

            .byte   $00
            .byte   $D5
            .byte   $54

            .byte   $00
            .byte   $00
            .byte   $00

            .byte   $00
            .byte   $00
            .byte   $00

            .byte   $00
            .byte   $00
            .byte   $00

            .byte   $00
            .byte   $00
            .byte   $D4

            .byte   $50
            .byte   $00
            .byte   $00

            .byte   $00
            .byte   $00
            .byte   $00

            .byte   $00
            .byte   $00
            .byte   $00

            .byte   $00
            .byte   $00
            .byte   $00

            .byte   $D3
            .byte   $4C
            .byte   $00

; K-1013 DMA buffer encoded addresses
;
DMAT:       .byte   $90   
            .byte   $8C
            .byte   $88
            .byte   $84

STACKP:     .byte   $FF             ; Stack pointer
PROCST:     .byte   $04             ; Processor Status
YREG:       .byte   $00             ; Y
XREG:       .byte   $00             ; X
ACCUM:      .byte   $00             ; Accumulator

            .export PRGBANK

PRGBANK:    .BYTE   $00             ; Current program bank
DATBANK:    .BYTE   $00             ; Current data bank
BNKCFG:     .BYTE   $00             ; Current bank configuration
SVCSTAT:    .BYTE   $00             ; SVC status (enabled/disables) at interrupt?
DSTBANK:    .BYTE   $00             ; Destination bank for memory copy operations?

LE6D3:      .byte   $7F

            .export NEWBNK, CHGBNKFLG

NEWBNK:     .byte   $00             ; TODO:
CHGBNKFLG:  .byte   $00             ; If set, switches to NEWBNK
LE6D6:      .byte   $00
LE6D7:      .byte   $00

            .export DEFBNK

DEFBNK:     .byte   $7F             ; Default bank configuration
CHANNEL:    .byte   $00             ; Current channel for I/O operations
DEVICE:     .byte   $00             ; Current device/file for I/O operations

LE6DB:      .byte   $00

            .export CURRDRV

CURRDRV:    .byte   $00             ; Current disk drive number

LE6DD:      .byte   $01

FNAMBUF:    .byte   "NONAME.Z  ", $00, $00, $2E, $00

LE6EC:      .byte   $00
            .byte   $80
            .byte   $40
            .byte   $00
            .byte   $00
LE6F1:      .byte   $00
LE6F2:      .byte   $00

TDATE:      .byte   "*UNDATED*"     ; Today's date
            .byte   $00
            .byte   $00
            .byte   $00
            .byte   $00
            .byte   $00
            .byte   $00
            .byte   $00
            .byte   $00
            .byte   $00
            .byte   $20
            .byte   $20
            .byte   $20
            .byte   $20
            .byte   $20
            .byte   $20
            .byte   $20
            .byte   $20
            .byte   $20
            .byte   $20
            .byte   $20
            .byte   $20
            .byte   $20
            .byte   $20
            .byte   $20
            .byte   $20
            .byte   $20
            .byte   $20
            .byte   $20
            .byte   $20
            .byte   $19
            .byte   $98
            .byte   $FF
            .byte   $E5
    
; E71D
SAVEDMAGIC: .byte   $58             ; Magic number for loadable (SAVEd) files
LE71E:      .byte   $00
LE71F:      .byte   $00
LE720:      .byte   $00
LE721:      .byte   $00
            .byte   $00
            .byte   $00
            .byte   $00
            .byte   $00
            .byte   $00

        ; uPD765 Command Table
        ;
        ; $00   SPECIFY
        ; $04   RECALIBRATE
        ; $07   SEEK
        ; $0B   SENSE INTERRUPT STATUS
        ; $0D   READ/WRITE
        ; $17   FORMAT WRITE
        ; $1E   SENSE DRIVE STATUS
        ;

CMDTBL:     .byte   $03             ; Command length 3
            .byte   $03             ; Specify
.if CODOS2_VER = 14
            .byte   $AF             ; Stepping Rate Time $A (6ms), Head Unload Time $F (240ms)
            .byte   $30             ; Head Load Time $30 (48ms)
.else
            .byte   $DF             ; Stepping Rate Time $D (3ms), Head Unload Time $F (240ms)
            .byte   $26             ; Head Load Time $26 (38ms)
.endif
            .byte   $02             ; Command length 2
            .byte   $07             ; Recalibrate
LE72D:      .byte   $00             ; Drive and head:
                                    ;   XXXXX 0 00
                                    ;         | ||
                                    ;         | ++-> Drive (0-3)
                                    ;         +----> Head

            .byte   $03             ; Command length 3
            .byte   $0F             ; Seek
DRVHD:      .byte   $00             ; Drive and head (see above)
TRACK:      .byte   $00             ; Track

            .byte   $01             ; Command length 1
            .byte   $08             ; Sense interrupt status

            .byte   $09             ; Command length 9
                                    ; Read command. Same sequence is used for write,
RDWRD:      .byte   $46             ; storing $45 at this location
                                    ; MFM, no MT, no skip
LE736:      .byte   $00             ; Disk and Header info
LE737:      .byte   $00             ; C- Cylinder
LE738:      .byte   $00             ; H - Head
LE739:      .byte   $00             ; R - Sector
            .byte   $01             ; N - 256 bytes/sector
LE73B:      .byte   $00             ; EOT
            .byte   $0E             ; GPL
            .byte   $FF             ; DTL (ignored as N != 0)

            .byte   $06             ; Command length 6
            .byte   $4D             ; Format command (MFM)
            .byte   $00             ; HD = 0, Drive = 0
            .byte   $01             ; 256 bytes sectors
            .byte   $1A             ; 26 sectors/track
            .byte   $34             ; Gap 3
            .byte   $00             ; Filler byte

            .byte   $02             ; Command length 2
            .byte   $04             ; Sense drive status command   
DRVNUM:     .byte   $00

DSKSTAT:    .byte   $00
ST1:        .byte   $00
            .byte   $00
            .byte   $00
            .byte   $00
            .byte   $00
            .byte   $00
NDRIVES:    .byte   $02             ; Number of disk drives in system, 1 to 4
DRVNFO:     .byte   $00             ; Drive info table (one byte per drive)
            .byte   $00             ; 0b10000000 : Two sides
            .byte   $00
            .byte   $00

            ; Open drives flag table

ODRIVES:    .byte   $00
            .byte   $00
            .byte   $00
            .byte   $00

LE758:      .byte   $00
            .byte   $00
            .byte   $00
            .byte   $00

            ; Error recovery

ERRADDR:    .word   $0000           ; Address+1 where last error was detected by CODOS.
ERRORS:     .byte   $00             ; Stack pointer at error
ERRORP:     .byte   $00             ; Processor status at error
ERRORY:     .byte   $00             ; Y at error
ERRORX:     .byte   $00             ; X at error
ERRORA:     .byte   $00             ; A at error


RDERRCNT:   .byte   $00             ; Cumulative count of soft disk read errors.
WRERRCNT:   .byte   $00             ; Cumulative count of soft disk write errors.
RCERRCNT:   .byte   $00             ; Cumulative count of recalibrate commands issued to disk controller
                                    ; during read/write error recoveries.
SECERRNUM:  .byte   $FF             ; Sector number for last disk error causing a recalibrate.
TRKERRNUM:  .byte   $FF             ; Track number for last disk error causing a recalibrate.

LE768:      .byte   $00
N765ECNT:   .byte   $00             ; uPD765 error count
CMDIDX:     .byte   $01             ; uPD765 command index
SAVEDRV:    .byte   $FF             ; Used for temporary storage
LE76C:      .byte   $00
LE76D:      .byte   $00
LE76E:      .byte   $00
DSIDE:      .byte   $00             ; Dual side. Zero means single side
CDRIVE:     .byte   $00             ; Current drive number
LE771:      .byte   $00
LE772:      .byte   $00
LE773:      .byte   $07
LE774:      .byte   $00
LE775:      .byte   $00
LE776:      .byte   $00             ; DMA Direction?

            .export IGNORWRP, UNPROTFLG

IGNORWRP:   .byte   $00             ; Flag. If bit 7 = 1, ignore memory write protection 
UNPROTFLG:  .byte   $00             ; Flag. If bit 7 = 1, unprotect SYSRAM
IGNORERR:   .byte   $00             ; Flag. If bit 7 = 1 then system will ignore (continue after)
                                    ; irrecoverable disk read errors (use a last resort only).
SAVEOVERWR: .byte   $00             ; Flag. If bit 7 = 1 then permits save command to overwrite an
                                    ; existing file with the same name.
LE77B:      .byte   $00
LE77C:      .byte   $00

; Command processor and SVC variables?

            .export UNKFLAG2, UNKFLAG3, UNKFLAG4, SVC13FLG

UNKFLAG2:   .byte   $00             ; Flag      (set by command processor)
UNKFLAG3:   .byte   $00             ; Flag      (set by command processor)
UNKFLAG4:   .byte   $00             ; Flag      (set by SVC)
SVC13FLG:   .byte   $00             ; Flag. If bit 7 = 1 then program executing was invoked by SVC #13.
UNKFLAG5:   .byte   $00

VERBOSE:    .byte   $00             ; Flag. If bit 7 = 1, print human readable error msgs

IRQFLAG:    .byte   $00             ; Flag. If bit 7 = 1, interrupt is IRQ (0 is BRK)
NMIFLAG:    .byte   $00             ; Flag. If bit 7 = 1, interrupt is NMI
DEFSVCFLAG: .byte   $00             ; Flag. If bit 7 = 1, SVC enabled by default
LE786:      .byte   $00
LE787:      .byte   $00
KBDECHO:    .byte   $00             ; Keyboard echo flag for CODOS. Set to $80 to enable echo.
ETX:        .byte   $03             ; ETX (CTRL-C)
XOFF:       .byte   $13             ; XOFF
EOF:        .byte   $1A             ; End of file
ULINE:      .byte   $5F             ; _
SCOLON:     .byte   $3B             ; ;
PERIOD:     .byte   $2E             ; .
DOLLAR:     .byte   $24             ; $
COLON:      .byte   $3A             ; :
CARET:      .byte   $5E             ; ^
DQUOTE:     .byte   $22             ; "
DEFAULTEXT: .byte   "C"             ; Current ASCII default file extension character ("C").
NUMOVL:     .byte   $11             ; Number of system overlays
CURROVL:    .byte   $00             ; Current overlay number

            .export DEFDRV, CMDFNP

DEFDRV:     .byte   $00             ; Current default drive number (Set by DRIVE command).
LE797:      .byte   $4F
LE798:      .byte   $05             ; Number of file names per line for FILES command (5 or less).
LE799:      .byte   $10             ; Number of    .byte s to dump per display line.
            .byte   $02
LE79B:      .byte   $2B             ; "+"   (List of forbiden chars in file name????)
            .byte   $2D             ; "-"
            .byte   $2A             ; "*"
            .byte   $2F             ; "/"
LE79F:      .byte   $5C             ; ASCII character to be used in lieu of Backslash "\"
SYSERRMNAM: .byte   "SYSERRMSG.Z"
CMDPROCNAM: .byte   "COMDPROC.Z"
STARTUPNAM: .byte   "STARTUP.J"
INPLBUF:    .word   $0500           ; Pointer to start of system input line buffer.
OUTLBUF:    .word   $0600           ; Pointer to start of system output line buffer
LE7C2:      .word   $A000           ; Pointer to large transient buffer for COPYF, ETC.
LE7C4:      .word   $1400           ; Size (NOT. final address) of large transient buffer.
INTSRVP:    .word   INTSRV          ; Pointer to user-defined interrupt service routine.
ERRRCVRYP:  .word   ERRRCVRY        ; Pointer to user-defined error recovery routine.     
LE7CA:      .byte   $1A
CMDFNP:     .word   $0000           ; Pointer to internal command function
            .byte   $00
            .byte   $00
DRIVERP:    .word   $0000           ; Pointer to current device driver
SAVECH:     .byte   $00             ; Used for temporary save character in I/O functions

; Breakpoint table

            .export BPBANK, BPADDRLO, BPADDRHI, BPOP

BPBANK:     .byte   $FF             ; BP 0
            .byte   $FF             ; BP 1
            .byte   $FF             ; BP 2
BPADDRLO:   .byte   $00
            .byte   $00
            .byte   $00
BPADDRHI:   .byte   $00
            .byte   $00
            .byte   $00
BPOP:       .byte   $00
            .byte   $00
            .byte   $00

; Jump table
;
JMPTBL:     jmp     NMIPROC         ; Jump to NMI processor               
            jmp     IRQPROC         ; Jump to IRQ and BRK processor
            jmp     LE859           ; Jump to CNTRL/C processor
            jmp     JWARMST         ; Jump to OS warm reset entry


COLDST:     cld                     ; Clear decimal mode
            ldx     #$FF            ; Set stack pointer
            txs                     ;
            jsr     SYSINIT         ; Init variables, pointers and jump tables
            lda     #$BF            ; Set maximum record length for input line
            sta     YLNLIM          ;
            jsr     OPENDRV0        ; Open disk drive 0
            jsr     LDCMDPR         ; Load command processor

            ; Load STARTUP.J
            ;
            ldx     #$08            ; Copy file name to buffer 
LE7FE:      lda     STARTUPNAM,x    ;
            sta     FNAMBUF,x       ;
            dex                     ;
            bpl     LE7FE           ;
            ldx     #$00            ; Set drive 0
            stx     CURRDRV         ;
            jsr     LF77A
            bne     LE816
            ldx     #$01
            jsr     LF594
LE816:      jmp     WARMST

; Init system variables, pointers and jump tables
;
SYSINIT:    ldx     #$00            ; Set DMA direction bit to read
            stx     HSRCW           ;
            stx     PRGBANK         ; Init current program bank
            stx     DATBANK         ; Init current data bank
            stx     ERRNUM          ; Init error number
            stx     OVLORG          ; Init overlays
            stx     DEFDRV          ; Init default drive
            lda     #$7F            ; Bank 0, write disable $8000 to $BFFF 
            sta     DEFBNK          ; 
            jsr     INIMMAP

            lda     #$EA            ; Init $EAFA to $EAFC with NOPs
            ldx     #$02
@LOOP1:     sta     LEAFA,x
            dex
            bpl     @LOOP1

            ; Copy jump table 
            ;
            ldx     #$0B            ; Table size - 1 
@LOOP2:     lda     JMPTBL,x        ;
            sta     NMIPRC,x        ;
            dex                     ;
            bpl     @LOOP2          ;

            lda     #'C'            ; Current ASCII default file extension character ("C")
            sta     DEFAULTEXT
            lda     #<ERRRCVRY      ; Set pointer to error recovery routine
            sta     ERRRCVRYP       ;
            lda     #>ERRRCVRY      ;
            sta     ERRRCVRYP+1     ;
            rts

LE859:      cld
            ldx     #$FF
            tsx
            jsr     SYSINIT
            lda     #$00
            ldy     NDRIVES
            dey
LE866:      tya
            jsr     CLDRIVE
            dey
            bpl     LE866
            jsr     DEFSETOUTB
            jsr     INITIO
            jsr     EXSENSEINT
            jsr     OPENDRV0
            jsr     PRNSTR
            .byte   "RESET.", $00
            jmp     WARMST

NMIPROC:    sta     INTSVA
            lda     #$00
            sta     HSRCW
            sec                     ; Set NMI flag
            ror     NMIFLAG         ;
            jmp     INTPROC

IRQPROC:    sta     INTSVA          ; Save accumulator on entry
            pla                     ; Get and save back processor status register
            pha                     ;
            and     #$10            ; Check if BRK
            bne     SVCINT          ; Yes, could be an SVC
            lda     INTSVA          ; Recover accumulator
            jmp     (INTSRVP)       ; Jump to user-defined interrupt service routine


; Interrupt service routine
;
INTSRV:     lda     #$00
            sta     HSRCW
            sta     NMIFLAG         ; Clear NMI flag
INTPROC:    sec                     ; Set IRQ flag
            ror     IRQFLAG         ;
            pla                     ; Get and save processor status register
            sta     PROCST          ;
            pla                     ; Get and save
            sta     PCSAVE          ; Program counter (low)
            pla                     ; Program counter (high) in A
            jmp     INTCONT

SVCINT:     lda     #$00
            sta     HSRCW
            sta     IRQFLAG         ; Clear IRQ flag
            pla                     ; Get and save processor status register
            and     #$EF            ; with N flag cleared
            sta     PROCST          ;
            pla                     ; Get and save address off SVC number (low)
            sec                     ;
            sbc     #$02            ;
            sta     PCSAVE          ; 
            pla                     ; Get address of SVC number (high)
            sbc     #$00            ;

; Interrupt service routine (continued)

INTCONT:    sta     PCSAVE+1        ; Save program counter (high)
            stx     XREG            ; Save registers on entry
            sty     YREG            ;
            cld
            tsx                     ; Save stack pointer on entry
            stx     STACKP          ;
            lda     BNKCTL          ; Save current I/O and bank config
            sta     BNKCFG          ;
            and     #$03            ; Save current data bank
            eor     #$03            ;
            sta     DATBANK         ;
            lda     BNKCFG          ; Save current program bank
            lsr     a               ;
            lsr     a               ;
            and     #$03            ;
            eor     #$03            ;
            sta     PRGBANK         ;
            lda     BNKCFG          ; TODO 
            ora     #$0F            ;
            sta     DEFBNK          ;
            jsr     INIMMAP         ;
            lda     INTSVA          ; 
            sta     ACCUM
            bit     IRQFLAG         ; Is it an IRQ?
            bmi     @DOIRQ
            sec                     ; No, set the ???? flag
            ror     UNKFLAG4        ;
            ldx     #$02            ; Check which Break Point it is
@LOOP:      lda     BPBANK,x        ; Is it the same program bank?
            cmp     PRGBANK         ;
            bne     @NEXT           ; No, check next BP
            lda     BPADDRLO,x      ; Is it the same address?
            cmp     PCSAVE          ;
            bne     @NEXT           ; No, check next
            lda     BPADDRHI,x      ; Maybe,
            cmp     PCSAVE+1        ; lets see the MSB
            bne     @NEXT           ; No, check next
            ldy     #$00            ; Yes
            lda     PRGBANK         ; TODO:
            eor     DEFBNK          ;    What is it doing here?
            sta     BNKCTL          ;
            lda     BPOP,x          ; Get saved instruction byte at BP 
            sta     (PCSAVE),y      ; and restore it to the PC
            lda     DEFBNK          ; TODO: Again
            sta     BNKCTL          ;    Why?
            lda     #$FF            ; Invalidate/clear BP
            sta     BPBANK,x        ;
            jsr     DEFSETINPB         ; Set input buffer
            jsr     DEFSETOUTB           ;       output device
            jsr     PRNSTR
            .byte   $0d, "BP", $00
            jmp     @PRNSTAT           ; Print status (Registers, pointers) and warm-start
            ; Not reached
@NEXT:      dex
            bpl     @LOOP

            ; If we are here, either it is an SVC or just a BRK

            lda     PRGBANK         ; SVC only available in bank 0
            bne     @DOIRQ          ;
            lda     SVCENB          ; Are SVC enabled?
            sta     SVCSTAT         ; Save SVC status
            bpl     @DOIRQ          ; No, should be BRK
            jmp     SVCPROC         ; Yes, serve it

@DOIRQ:     jsr     DEFSETINPB         ; Set input buffer
            jsr     DEFSETOUTB           ;       output device
            bit     IRQFLAG         ; Is it an IRQ?
            bpl     @DOBRK          ; No, then it is a BRK
            jsr     PRNSTR          ; Yes, print
            .byte   $0D, "INTERRUPT (", $00
            bit     NMIFLAG         ; Is it an NMI?
            bpl     @NOTNMI         ; No, print IRQ
            jsr     PRNSTR          ; Yes, print NMI
            .byte   "NMI)", $00
            jmp     @PRNSTAT

@NOTNMI:    jsr     PRNSTR
            .byte   "IRQ)", $00
            jmp     @PRNSTAT

@DOBRK:     jsr     PRNSTR
            .byte   $0D, "BRK", $00

@PRNSTAT:   jsr     PRNSTR
            .byte   ", ", $00
            jsr     PRNREGS
            jmp     WARMST
            ; Not reached

; Error routines
;
            .export ERROR01, ERROR02, ERROR03, ERROR04, ERROR05, ERROR06, ERROR07, ERROR08
            .export ERROR09, ERROR10, ERROR11, ERROR12, ERROR13, ERROR14, ERROR15, ERROR16
            .export ERROR17, ERROR18, ERROR19, ERROR20, ERROR21, ERROR22, ERROR23, ERROR24
            .export ERROR25, ERROR26, ERROR27, ERROR28, ERROR29, ERROR30, ERROR31, ERROR32
            .export ERROR33, ERROR34, ERROR35, ERROR36, ERROR37, ERROR38, ERROR39, ERROR40
            .export ERROR41, ERROR42, ERROR43, ERROR44, ERROR45, ERROR46, ERROR47, ERROR48
            .export ERROR49, ERROR50, ERROR51, ERROR52

ERROR52:    inc     ERRNUM
ERROR51:    inc     ERRNUM          ; Missing or illegal memory bank number
ERROR50:    inc     ERRNUM
ERROR49:    inc     ERRNUM
ERROR48:    inc     ERRNUM
ERROR47:    inc     ERRNUM
ERROR46:    inc     ERRNUM
ERROR45:    inc     ERRNUM
ERROR44:    inc     ERRNUM
ERROR43:  	inc     ERRNUM
ERROR42:  	inc     ERRNUM
ERROR41:  	inc     ERRNUM
ERROR40:  	inc     ERRNUM
ERROR39:  	inc     ERRNUM
ERROR38:  	inc     ERRNUM
ERROR37:  	inc     ERRNUM          ; Required software package not loaded in memory. 
ERROR36:  	inc     ERRNUM
ERROR35:  	inc     ERRNUM
ERROR34:  	inc     ERRNUM
ERROR33:  	inc     ERRNUM
ERROR32:  	inc     ERRNUM
ERROR31:  	inc     ERRNUM          ; Breakpoint table full (3 BP's already set).
ERROR30:  	inc     ERRNUM
ERROR29:  	inc     ERRNUM
ERROR28:  	inc     ERRNUM
ERROR27:  	inc     ERRNUM
ERROR26:  	inc     ERRNUM
ERROR25:  	inc     ERRNUM          ; New file name is already on selected diskette
ERROR24:  	inc     ERRNUM
ERROR23:  	inc     ERRNUM
ERROR22:  	inc     ERRNUM
ERROR21:  	inc     ERRNUM
ERROR20:  	inc     ERRNUM          ; <entry> address missing or illegal
ERROR19:  	inc     ERRNUM
ERROR18:  	inc     ERRNUM
ERROR17:  	inc     ERRNUM
ERROR16:  	inc     ERRNUM
ERROR15:  	inc     ERRNUM          ; <to> address missing or illegal
ERROR14:  	inc     ERRNUM          ; <from> address missing or illegal
ERROR13:  	inc     ERRNUM          ; Not a loadable ("SAVEd") file
ERROR12:  	inc     ERRNUM          ; Missing or illegal file name
ERROR11:  	inc     ERRNUM          ; Missing or illegal device or file name
ERROR10:  	inc     ERRNUM
ERROR09:  	inc     ERRNUM
ERROR08:  	inc     ERRNUM          ; Missing or illegal channel number
ERROR07:  	inc     ERRNUM
ERROR06:  	inc     ERRNUM
ERROR05:  	inc     ERRNUM          ; Missing or illegal disk drive number
ERROR04:  	inc     ERRNUM
ERROR03:  	inc     ERRNUM          ; Drive needed is not open
ERROR02:  	inc     ERRNUM          ; File not found
ERROR01:  	inc     ERRNUM          ; Command not found
            jmp     (ERRRCVRYP)

; Error recovery routine
;
ERRRCVRY:   pha
            lda     #$00            ; Unprotect SYSRAM
            sta     HSRCW           ;
            cld
            jsr     INIMMAP         ; Set default memory config
            bit     VERBOSE
            bpl     LEA2B
            pla
            jmp     WARMST

LEA2B:      pla
            sta     ERRORA          ; Save A
            stx     ERRORX          ; Save X
            sty     ERRORY          ; Save Y
            tsx
            stx     ERRORS          ; Save SP
            php
            pla
            sta     ERRORP          ; Save processor status reg
            pla                     ; Get return address
            sec                     ; Point to where error occurred
            sbc     #$02            ; and save it 
            sta     ERRADDR         ; 
            pla                     ;
            sbc     #$00            ; 
            sta     ERRADDR+1       ;
            lda     #$80            ; Set flag
            sta     VERBOSE         ;
            jsr     DEFSETINPB         ; Set buffer for 
            jsr     DEFSETOUTB
            jsr     PRNSTR
            .byte   $0d, "CODOS ERROR #", $00
            lda     ERRNUM
            jsr     HEXBYTE
            jsr     LF9D1
            bit     UNKFLAG2
            bpl     LEA9B
LEA75:      lda     (INPBUFP),y
            cmp     #$0D
            beq     LEA81
            jsr     PRNCHAR
            iny
            bne     LEA75
LEA81:      jsr     LF9D6
            ldy     CMDLIDX
            beq     LEA90
            lda     #$20
LEA8A:      jsr     PRNCHAR
            dey
            bne     LEA8A
LEA90:      lda     #$5E
            jsr     PRNCHAR
            jsr     LF9D6
            jmp     LEABD

LEA9B:      bit     UNKFLAG4
            bmi     LEABA
            bit     UNKFLAG5
            bmi     LEABD
            lda     ERRADDR
            sta     PCSAVE
            lda     ERRADDR+1
            sta     PCSAVE+1
            ldx     #$04
LEAB1:      lda     ERRORS,x        ; Copy registers at error
            sta     STACKP,x
            dex
            bpl     LEAB1
LEABA:      jsr     PRNREGSLB
LEABD:      bit     VERBOSE         ; Quiet? (Do not print error messages)
            bpl     WARMST          ; N
            ldx     #$0B            ; Get file with error messages
@LOOP:      lda     SYSERRMNAM,x    ;
            sta     FNAMBUF,x       ;
            dex                     ;
            bpl     @LOOP           ;
            inx                     ; X == 0
            stx     CURRDRV         ; Set drive 0
            jsr     LF77A
            bne     WARMST
            jsr     LF592
LEAD9:      ldx     #$00
            jsr     GETLINE
            bcs     LEAF7
            dec     ERRNUM
            bne     LEAD9
            tay
            tax
LEAE6:      lda     (INPBUFP),y
            sta     (OUTBUFP),y
            dey
            bpl     LEAE6
            txa
            tay
            ldx     #$02
            jsr     LF9D6
            jsr     LF9D1
LEAF7:      jsr     FREECH0

LEAFA:      nop                     ; TODO: Probably room for inserting a subroutine
            nop                     ; or a jump to a different WARMST sequence
            nop                     ;

; $EAFD
            .export WARMST

WARMST:     cld
            lda     #$00
            sta     HSRCW
            jsr     CKCMDPR
            jmp     CMDPROC

; Execute command, making sure that the command processor is loaded first
;
            .export CKCMDEXEC

CKCMDEXEC:  jsr     CKCMDPR         ; Make sure that command processor is loaded
            jmp     CMDEXEC         ; Execute command


; Note: Odd that these command functions are here and not in COMDPROC
;

; NEXT Command
;
; DESCRIPTION:  Resume execution after a break or interrupt or initiate execution
;               of a machine language program in memory. 
; SYNTAX:       NEXT [<addr>]
; ARGUMENTS:    <addr>=starting address. Defaults to current value of the Program
;               Counter (P), as displayed by the REG command.
;
            .export NEXT

NEXT:       jsr     GETPC           ; Get program counter from command args
            bit     CHGBNKFLG       ; Is there a bank switch
            bpl     @SKIP           ; No, skip it
            lda     NEWBNK          ; Yes, switch to the new bank
            sta     PRGBANK         ;
            sta     DATBANK         ;
@SKIP:      jmp     NEXTGOCOM       ; Continue with command execution


; GO Command
;
; DESCRIPTION:  Begin execution of a machine-language program in memory.
; SYNTAX:       GO [<addr>]
; ARGUMENTS:    <addr>=starting address. Defaults to current value of the Program
;               Counter (P), as displayed by the REG command.
;
            .export GOCMD

GOCMD:      jsr     GETPC           ; Get program counter from command args
            lda     NEWBNK          ; Switches 
            sta     PRGBANK
            sta     DATBANK
            ldx     #$7F
            stx     DEFBNK
            ldx     DEFSVCFLAG      ; Get default SVC state (enabled or disabled)
            stx     SVCSTAT         ; And set it as current
            ldx     #$FF            ; Discard stack
            bit     SVC13FLG        ; Was the GO invoked by an SVC?
            bpl     NOSVCGO         ; No, skip

; Common code for NEXT and GO commands

NEXTGOCOM:  jsr     RESTORE
            jmp     L00D2           ; Switch bank and jump

NOSVCGO:    stx     STACKP          ; Set stack (Discarded in case of GO)
            jsr     RESTORE
            jsr     BANKSW          ; TODO: Understand what it does
            php
            cld
            lda     #$00
            sta     HSRCW
.if  CODOS2_VER = 17
            lda     SAVEACC
.else
            lda     INTSVA
.endif
            sta     ACCUM
            stx     XREG
            sty     YREG
            pla
            sta     PROCST
            tsx
            stx     STACKP
            lda     #$7F
            sta     DEFBNK
            jmp     WARMST

; Set/Restores stack pointer, memory bank config, memory protection
; restore registers and copies the switch and jump routine to its page 0
; location.
;
RESTORE:    pla                     ; Get return address (low)
            tay                     ; and save it in Y
            pla                     ; Get return address (hugh)
            bit     SVC13FLG        ; Was this invoked by an SVC?
            bmi     @SKIP           ; Yes, skip
            ldx     STACKP          ; Set stack pointer
            txs                     ;
@SKIP:      pha                     ; Push back the return address
            tya                     ;
            pha                     ;
            lda     PRGBANK         ; Set new memory bank config
            asl     a               ;
            asl     a               ;
            eor     DEFBNK          ;
            eor     DATBANK         ;
            sta     BNKCFG          ;
            lda     #$7F            ;
            sta     SVIA1DIR        ;
            jsr     CPYSWNJMP       ; Copy switch and jump routine to page 0
            lda     #$00            ; Clear flags
            sta     UNKFLAG2        ;
            sta     UNKFLAG4        ;
            lda     #$03            ; Protect SYS memory and DMA direction to read
            bit     UNPROTFLG       ; Unprotect SYSRAM?
            bpl     @SKIP2          ; No, skip
            lda     #$01            ; Set DMA direction byte to read, no memory protection
@SKIP2:     sta     HSRCW           ;
            lda     SVCSTAT         ; Restore SVC status
            sta     SVCENB          ;
            lda     ACCUM           ; Get and save A
.if  CODOS2_VER = 17
            sta     SAVEACC
.else
            sta     INTSVA
.endif
            ldy     YREG            ; Get Y
            ldx     XREG            ; Get X
            lda     PROCST          ; Get Status register
            pha                     ; Save it (so next operation does not alter it)
            lda     BNKCFG          ; Get memory bank status
            plp                     ; Recover Status register
            rts

; Check if Command Processor is loaded, load it if not
; 
CKCMDPR:    lda     CMDPROC         ; Check if Command Processor is loaded
            cmp     #$D8            ; First byte should be $D8 (CLD)
            bne     LDCMDPR         ; No, go ahead and load from disk
            rts                     ; Yes, return

; Loads Command Processor from disk
;
LDCMDPR:    ldx     #$09            ; Get file name
@LOOP:      lda     CMDPROCNAM,x    ;
            sta     FNAMBUF,x       ;
            dex                     ;
            bpl     @LOOP           ;
            ldx     #$00            ; Set drive 0
            stx     CURRDRV         ;
            jsr     LF592
            ldx     #$00
            txa
            jsr     LFD05
            bcc     LEBEA
            jsr     ERROR13         ; Not a loadable ("SAVEd") file. 
LEBEA:      ldx     #$00
            txa
            jsr     LFD05
            bcc     LEBEA
            jmp     FREECH0

; Init memory map config
;
            .export INIMMAP

INIMMAP:    sec
            ror     SEEIO           ; Set I/O space enable semaphore
            lda     #$00            ; Set destination bank for memory copy ops?
            sta     DSTBANK         ;
            lda     DEFBNK          ; Set default bank config
            sta     BNKCTL          ;
            lda     #$7F            ;
            sta     SVIA1DIR        ;

            ; This clears the break flag by forcing an rti

            lda     #>@RETURN       ; Set return address to $EC11
            pha
            lda     #<@RETURN
            pha
            php
            rti
@RETURN:    rts


; Copy bank switch/restore routine to $0100-$0112

            .export CPYBNKSW

CPYBNKSW:   ldx     #$12
LEC14:      lda     BANKSW_O,x
            sta     BANKSW,x
            dex
            bpl     LEC14
            rts

CPYSWNJMP:  ldx     #$07
@LOOP:      lda     SWITCHNJMP_O,x
            sta     a:L00D2,x
            dex
            bpl     @LOOP
            rts

; Switches bank and jumps to PC. Copied to $D2
;
SWITCHNJMP_O:
            php
            sta     BNKCTL
.if  CODOS2_VER = 17
            lda     SAVEACC
.else
            lda     INTSVA
.endif
            plp
            .byte   $4C             ; JMP Absolute. As SWITCHNJMP_O is copied to $D2, it
                                    ; means it jumps to address contained in
                                    ; $DA-$DB, which is PCSAVE (saved Program Counter)
                                    ; which was set by the interrupy service routine

; Bank switch/restore routine. Copied to 0100-0112
;
BANKSW_O:   jsr     L00D2       
            php
.if  CODOS2_VER = 17
            sta     SAVEACC
.else
            sta     INTSVA
.endif
            sta     IOENABLE            ; Enable I/O space from $BE00 to $BFFF
            lda     #$FF                ; Sets bank 0 and write enable $8000 to $BFFF
            sta     BNKCTL              ;
            sta     SVIA1DIR
            plp
            rts


;       Send command to uPD765
;       X : Command index
;
SNDCMD:     sty     SAVEY1          ; Save Y register (restored in COMMPH)
            stx     CMDIDX          ; Save command index
            ldy     CMDTBL,x        ; Get command length

@CHKBSY:    lda     #$10            ; Check if uPD765 is busy processing a command (it shouldn't)
            and     MSTR            ;
            beq     COMMPH          ; No, send command
@WAITRD:    bit     MSTR            ; Yes, wait until finished
            bpl     @WAITRD         ;
            bvs     @READST         ; Jump if data register needs to be read
            lda     #$00            ; Otherwise, try to complete command sequence
            sta     DATR            ;
@RETRY:     nop                     ; Wait a couple of cycles
            inc     N765ECNT        ; Increment controller error count
            jmp     @CHKBSY         ; And try again

@READST:    lda     DATR            ; Read status register
            jmp     @RETRY          ; And try again
            ; Not reached

;       Write command to uPD765 Data Register
;       X -> index to command byte in command table
;       Y -> command length 
;
COMMPH:
@WAITRD:    lda     MSTR            ; Read uPD765 Main Status Register
            bpl     @WAITRD         ; Wait until bit 7 is 1 (Ready)
            and     #$40            ; Check data direction
            beq     @CONT           ; Jump if data register is to be written
            jsr     ERROR48         ; System crash: NEC 765 chip command phase error.
@CONT:      lda     CMDTBL+1,x      ; Write command byte
            sta     DATR            ;
            inx                     ; next command byte
            dey                     ;
            bne     @WAITRD         ; Until command length
            ldy     SAVEY1          ; Restore Y register
            rts                     ; And return

; Sense drive status command
;
EXSENSEDRV: stx     DRVNUM          ; Send Sense drive command to the disk controller
            ldx     #SENSEDRV
            bne     EXSENSECMD
            ; Always jump

; Sense interrupt command
;
EXSENSEINT: ldx     #SENSEINT   ; Send Sense interrupt command to the disk controller
            ; Fall through

; Execute SENSE type of command
;
EXSENSECMD: jsr     SNDCMD
            ; Fall through

;       Read result from uPD765 Data Register
;
RSLTPH:     ldx     #$00        ; Execute result phase
@WAITRD:    lda     MSTR        ; Read uPD765 Main Status Register
            bpl     @WAITRD     ; Wait until bit 7 is 1 (Ready)
            and     #$40        ; Check data direction
            bne     @CONT       ; Jump if data register is to be read
            jsr     ERROR49     ; System crash: NEC 765 chip result phase error
@CONT:      lda     DATR
            sta     DSKSTAT,x
            nop
            nop
            inx
            lda     MSTR
            and     #$10
            bne     @WAITRD
            rts

; Send SEEK type command to uPD765 and process status
;
SNDSKCMDST: jsr     SNDCMD

@WAITINT:   lda     HSRCW           ; Wait for interrupt ( Bit 7 of HSRCW is 0)
            bmi     @WAITINT
            ; Fall through

; Execute a Sense interrupt command and return
; Carry set if error, carry clear otherwise
;
SNSINTST:   jsr     EXSENSEINT
            lda     DSKSTAT
            cmp     #$C0
            rts

; Init disk drive
;
; Performs a complete init sequence of SPECIFY and RECALIBRATE and get
; disk information (single or dual side)
;
INITDRV:    jsr     DRVVALID
            stx     LE72D
LECCA:      lda     HSRCW
            bmi     LECD5
            jsr     EXSENSEINT
            jmp     LECCA

LECD5:      ldx     #SPECIFY
            jsr     SNDCMD
            ldx     #RECALIBRATE
            jsr     SNDSKCMDST
            and     #$D8            ; Delete don't care bits from ST0
            beq     LECED           ; No error
            and     #$08            ; Fail: Check if ready
            bne     LECEA           ; Not ready
            jsr     ERROR42         ; Unformatted diskette or hardware drive fault
LECEA:      jsr     ERROR06         ; Drive not ready error
LECED:      lda     ST1             ; Get status register 1
            beq     LECF5           ; All clear
            jsr     ERROR41         ; Unformatted diskette or irrecoverable seek error
LECF5:      ldx     LE72D           ; Get drive
.if  CODOS2_VER = 17
            jsr     EXSENSEDRV17    ; Sense drive
.else
            jsr     EXSENSEDRV      ; Sense drive
.endif
            ldx     LE72D           ; Get drive
            lda     DSKSTAT         ; Get status result
            and     #$08            ; Filter out except Two Sides flag
            beq     LED07           ; One side
            lda     #$80            ; Two sides
LED07:      sta     DRVNFO,x        ; Store info for drive
            rts

; Seeks track A on drive X, checking that drive is valid and performing
; a retry
;
SEEKTRK:    jsr     DRVVALIDO       ; Verify drive X is valid and open
            jsr     EXSEEK          ; Execute seek command (X drive, A track)
            bcs     DORTS           ; Return if OK
            jsr     INITDRV         ; Reinit drive
            jsr     GETDRVTRK       ; Recover Drive/Track info
            sta     TRKERRNUM       ; Store track that caused last error
            jsr     EXSEEK          ; And retry
            bcs     DORTS           ; Return if OK
            jsr     ERROR41         ; Unformatted diskette or irrecoverable seek error
            ; Not reached

; Seek track A of drive X
;
; Returns resulting drive ans track in XA
;
; NOTE: In this case Carry Set means success!!
;
EXSEEK:     stx     DRVHD           ; Set drive for seek command
            stx     CDRIVE          ; Save as current
            cmp     #$4D            ; Check it is a valid track
            bcc     @VALID
            jsr     ERROR47         ; System crash: illegal track on disk
            ; Not reached

@VALID:     sta     TRACK           ; Set track for seek command
            lda     DSIDE           ; Get single or dual side disk
            beq     DOSEEK          ; Single side, head 0 (no need to change DRVHD)
            lda     #$04            ; Dual sided, select head 1
            ora     DRVHD           ; Combine with drive number
            sta     DRVHD           ; Update drive and head
DOSEEK:     jsr     SRVINT          ; Serve any pending interrupt (if any)
            ldx     #SEEK           ; Send SEEK command
            jsr     SNDSKCMDST      ;
            bcs     SKERROR         ; Jump if error
            and     #$F8            ; Mask out non important bits
            cmp     #$20            ; Check for SEEK end
            beq     GETDRVTRK       ; Yes, return
            clc
            ; Fall through

; Get drive and trck in XA
;
GETDRVTRK:  ldx     CDRIVE
            lda     TRACK
DORTS:      rts

; Manage seek errors
;
SKERROR:    and     #$03            ; Is it our drive
            cmp     CDRIVE          ;
            bne     @RETRY          ; No, close and retry
            jsr     ERROR06         ; Drive needed is not ready.
            ; Not reached

@RETRY:     jsr     CLDRIVE
            jmp     DOSEEK
            ; Not reached

LED69:      lda     DMAENC
            sta     ADMA
            lda     LE73B
            cmp     #$1A
            bcc     LED78
            jsr     ERROR44         ; System crash: illegal sector on disk.
LED78:      lda     LE776
            sta     HSRCW
            ldx     #READWRITE
            jsr     SNDCMD
LED83:      lda     HSRCW
            bmi     LED83
            jsr     RSLTPH
            lda     DSKSTAT
            and     #$D8
            beq     LEDA4
            cmp     #$40
            beq     LED99
            jsr     ERROR40         ; Unformatted diskette or drive went not-ready.
LED99:      lda     ST1
            and     #$B7
            cmp     #$80
            beq     LEDA4
            sec
            rts
LEDA4:      clc
            rts

LEDA6:  sta     LE739
        lda     #$45                    ; Set command to write
        sta     RDWRD
        lda     #$00                    ; Set DMA to read mode
        beq     LEDBF                   ; Always jump
LEDB2:  jsr     LEFD0
LEDB5:  sta     LE739
        lda     #$46
        sta     RDWRD                   ; Set command to read
        lda     #$01                    ; Set DMA to write mode
LEDBF:  sta     LE776
        cmp     LE776
        bne     LEDCF
        inc     LE776
        cmp     LE776
        bne     LEDD2
LEDCF:  jsr     ERROR36                 ; Illegal entry into CODOS system. 
LEDD2:  sta     LE776
        sta     HSRCW                   ; Set DMA mode
        jsr     DRVVALIDO
        stx     LE736
        stx     LE771
        lda     #$00
        sta     DSIDE
        sta     LE738
        lda     LE739
        cmp     #$1A
        bcc     LEE0D
        sbc     #$1A
        sta     LE739
        lda     DRVNFO,x                ; Check if one or two sides
        bmi     LEDFD                   ; Two sides
        jsr     ERROR44                 ; System crash: illegal sector on disk.
LEDFD:  lda     #$04
        ora     LE736
        sta     LE736
        lda     #$01
        sta     LE738
        lda     LE739
LEE0D:  sta     LE73B
        lda     TRACK
        sta     LE737
        jsr     LED69
        bcs     LEE22
LEE1B:  ldx     LE771
        lda     LE739
        rts

LEE22:  lda     RDWRD
        cmp     #$46                    ; Is it a read command?
        bne     LEE63
        inc     RDERRCNT
LEE2C:  jsr     LED69
        bcc     LEE1B
        inc     RCERRCNT
        ldx     LE771
        jsr     INITDRV
        lda     LE737
        sta     TRKERRNUM
        jsr     SEEKTRK
        lda     LE73B
        sta     SECERRNUM
        lda     #$10
        sta     LE768
LEE4E:  jsr     LED69
        bcc     LEE1B
        dec     LE768
        bne     LEE4E
        bit     IGNORERR
        bmi     LEE1B
        jsr     ERROR30         ; Unformatted disk or irrecoverable read/write error.
        jmp     LEE1B

LEE63:  inc     WRERRCNT
        lda     ST1
        and     #$02
        beq     LEE2C
        jsr     ERROR10         ; Diskette is write-protected.
        ; Not reached

LEE70:  jsr     LEEE3
        lda     $DD
        sta     CURRDRV
LEE78:  lda     #$00
        sta     $E9
        lda     #$E4
        sec
        sbc     CURRDRV
        sta     $EA
        rts

LEE85:  stx     $0295
        sta     ($E9),y
        lda     #$E4
        sec
        sbc     $EA
        tax
        lda     #$80
        sta     LE758,x
        lda     ($E9),y
        ldx     $0295
        rts

LEE9B:  sta     LE775
LEE9E:  jsr     LEEC2
        jsr     LEDB5
        rts

LEEA5:  lda     #$00
        sta     LE775
LEEAA:  jsr     LEEC2
        jsr     LEDA6
        cmp     #$00
        bne     LEEE2
        lda     #$11
        jsr     LEDA6
        ldx     CURRDRV
        lda     #$00
        sta     LE758,x
        rts

LEEC2:  lda     #$00
        sta     DSIDE
        ldx     CURRDRV
        jsr     DRVVALIDO
        lda     #$0C
        jsr     SEEKTRK
        lda     #$94
        sta     DMAENC
        lda     LE775
        bne     LEEE2
        lda     DMAT,x
        sta     DMAENC
        lda     #$00
LEEE2:  rts

LEEE3:  ldy     DEVICE
        ldx     #$00
LEEE8:  lda     LE65C,y
        sta     $DC,x
        iny
        inx
        cpx     #$0D
        bmi     LEEE8
        rts

LEEF4:  ldy     DEVICE
        ldx     #$00
LEEF9:  lda     $DC,x
        sta     LE65C,y
        iny
        inx
        cpx     #$09
        bmi     LEEF9
        rts

LEF05:  lda     $E2
        cmp     $DF
        lda     $E3
        sbc     $E0
        lda     $E4
        sbc     $E1
        rts

LEF12:  sec
        lda     $DF
        sbc     $E2
        sta     $CF
        lda     $E0
        sbc     $E3
        sta     $D0
        lda     $E1
        sbc     $E4
        sta     $D1
        rts

LEF26:  lda     $E4
        sta     $E1
        lda     $E3
        sta     $E0
        lda     $E2
        sta     $DF
        rts

; Load overlay A from disk
;
            .export OVERLAY

OVERLAY:    cmp     #$00            ; Is it an overlay
            beq     @RETURN         ; No, return
            cmp     OVLORG          ; Is it the overlay loaded
            bne     @LOAD           ; No, go load it
@RETURN:    rts                     ; Yes, return

@LOAD:      stx     SAVEX9          ; Save X        
            cmp     NUMOVL          ; Validate overlay number
            bcc     @OVLOK          ; Correct, go on
            jsr     ERROR43         ; System crash: illegal system overlay number
            ; Not reached

@OVLOK:     sta     CURROVL         ; Set current overlay
            ldx     #$00            ; Init dual side disk flag
            stx     DSIDE           ;
            ldx     #$0C
            cmp     #$09
            bcc     @SSIDE
            inx
            bit     DRVNFO          ; Check if disk in drive 0 is one or two sides
            bpl     @SSIDE          ; One side
            dex                     ; Two sides
            lda     #$01            ; Set dual side flag
            sta     DSIDE           ;
@SSIDE:     txa
            ldx     #$00            ; Seek track on drive 0
            jsr     SEEKTRK         ;
            lda     #$F8
            sta     DMAENC
            lda     CURROVL
            clc
            adc     #$11
            cmp     #$1A
            bcc     LEF7E
            bit     DRVNFO          ; Check if one or two sides
            bmi     LEF7E           ; Two sides
            sec                     ; One side
            sbc     #$1A
LEF7E:      jsr     LEDB5
            lda     CURROVL
            cmp     OVLORG
            beq     LEF8C
            jsr     ERROR35         ; No CODOS on drive 0, or system overlay load error.
LEF8C:      ldx     SAVEX9
            rts

; Check that drive X is valid and open
;
            .export DRVVALIDO

DRVVALIDO:  jsr     DRVVALID
            ; Fall through

; Check if drive is open.
; Does not return in case of error
;
            .export ISDRVOPEN

ISDRVOPEN:  pha
            lda     ODRIVES,x
            bmi     @RETURN
            pla
            jsr     ERROR03         ; Drive needed is not open
@RETURN:    pla
            rts

; Check if drive is valid
;
            .export DRVVALID

DRVVALID:   cpx     NDRIVES         ; Check drive number
            bcc     @RETURN         ; Drive between 0 - 3  
            jsr     ERROR05         ; Missing or illegal disk drive number
@RETURN:    rts


; Get device or file from channel
;
; Saves device into DEVICE and returns it into A
; Does not return on error
;
            .export GETDEV

GETDEV:     stx     CHANNEL         ; Save channel
            cpx     #$0A            ; Is it valid?
            bcc     @CONT           ; Yes, go on
            jsr     ERROR08         ; Nop, Missing or illegal channel number.
@CONT:      lda     IOCHTBL,x       ; Get device or file
            sta     DEVICE          ; And save it
            rts

; Get assigned device/file to channel in X
;
; Saves device into DEVICE and returns device in X.
; Does not return on error
;
            .export ASSIGNED

ASSIGNED:   jsr     GETDEV          ; Get device for channel
            bne     @RET            ; Valid, return            
            jsr     ERROR09         ; Channel needed is not assigned
@RET:       tax
            rts


LEFC3:      jsr     LEE70
LEFC6:      lda     $DC
            and     #$20
            beq     @RET
            jsr     ERROR07         ; Locked file violation
@RET:       rts

LEFD0:  lda     #$00
        sta     DSIDE
        lda     #$07
        sta     LE773
        ldx     $DD
        lda     DRVNFO,x
        sta     LE772
        lda     $E4
        sta     TEMP1
        lda     $E3
        lsr     TEMP1
        ror     a
        lsr     TEMP1
        ror     a
        lsr     TEMP1
        ror     a
        bit     LE772
        bpl     LF003
        ldx     #$0F
        stx     LE773
        lsr     TEMP1
        ror     a
LF003:  tax
        lda     $DE
        inx
        bne     LF00C
LF009:  tay
        lda     ($E9),y
LF00C:  dex
        bne     LF009
        sta     LE76C
        sec
        sbc     #$01
        ldx     #$FF
        sec
LF018:  inx
        sbc     #$0D
        bcs     LF018
        adc     #$0D
        asl     a
        asl     a
        asl     a
        bit     LE772
        bpl     LF028
        asl     a
LF028:  sta     TEMP1
        txa
        asl     a
        asl     a
        tax
        lda     #$1A
        sta     LE7CA
        lda     #$07
        bit     LE772
        bpl     LF042
        lda     #$34
        sta     LE7CA
        lda     #$0F
LF042:  and     $E3
        sta     LE774
        clc
        adc     TEMP1
        dex
LF04C:  inx
        sec
        sbc     LE7CA
        bcs     LF04C
        adc     LE7CA
        cpx     #$0C
        bcc     LF065
        adc     #$11
        cmp     LE7CA
        bcc     LF065
        sbc     LE7CA
        inx
LF065:  stx     LE76D
        sta     LE76E
        ldx     $DD
        lda     LE76E
        cmp     #$1A
        bcc     LF077
        inc     DSIDE
LF077:  lda     LE76D
        jsr     SEEKTRK
        lda     LE76E
        rts

;
;
LF081:      jsr     LEE70
LF084:      bit     $DC
            bvc     LF094
LF088:      jsr     LEFD0
            jsr     LEDA6
            lda     $DC
            and     #$BF
            sta     $DC
LF094:      rts

        lda     #$FF
        sta     $D1
        jmp     LF0A4

        lda     #$00
        sta     $CF
        sta     $D0
        sta     $D1
LF0A4:  jsr     ASSIGNED
        bmi     LF0D5
        jsr     LF081
        lda     $CF
        clc
        adc     #$40
        sta     $E2
        lda     $D0
        adc     #$00
        sta     $E3
        lda     $D1
        adc     #$00
        sta     $E4
        bcs     LF0C6
        jsr     LEF05
        bcc     LF0CF
LF0C6:  ldx     #$02
LF0C8:  lda     $DF,x
        sta     $E2,x
        dex
        bpl     LF0C8
LF0CF:  jsr     LEDB2
        jsr     LEEF4
LF0D5:  rts

        jsr     ASSIGNED
        bmi     LF0FB
        jsr     LEFC3
        jsr     LEFD0
        jsr     LEF26
        ldy     LE76C
        lda     ($E9),y
        cmp     #$F9
        bcs     LF0F8
        tax
        lda     #$FC
        jsr     LEE85
        txa
        tay
        jsr     LF691
LF0F8:  jsr     LEEF4
LF0FB:  rts

LF0FC:      jsr     ASSIGNED        ; Get assigned device/file
            bpl     @ISFILE         ; Check if a file
            jmp     @ISDEV          ; Jump if a device

@ISFILE:    jsr     LF221
            lda     $E5
            sta     @LF146
            sta     @LF138
            sta     @LF186
            sta     @LF132
            lda     $E6
            sta     @LF147
            sta     @LF139
            sta     @LF187
            sta     @LF133
@LF123:     lda     LE6D3
            sta     BNKCTL
            lda     L00D2+1
            beq     @LF17D
            ldy     $E2
            bne     @LF141

            ; Ugh, self modifying code ahead!

@LF131:     .byte   $B9             ; lda $E000, y
@LF132:     .byte   <SYSRAM
@LF133:     .byte   >SYSRAM

            sta     (MEMBUFF), y
            iny

            .byte   $B9             ; lda $E000, y
@LF138:     .byte   <SYSRAM
@LF139:     .byte   >SYSRAM

            sta     (MEMBUFF), y
            iny

            bne     @LF131
            beq     @LF158
@LF141:     ldx     $E2
            ldy     #$00

@LF145:     .byte   $BD             ; lda $E000, x
@LF146:     .byte   <SYSRAM
@LF147:     .byte   >SYSRAM

            sta     (MEMBUFF), y
            iny

            inx
            bne     @LF145
            tya
            beq     @LF158
            clc
            adc     MEMBUFF
            sta     MEMBUFF
            bcc     @LF15A
@LF158:     inc     MEMBUFF+1
@LF15A:     lda     $E2
            beq     @LF165
            clc
            adc     L00D2
            sta     L00D2
            bcs     @LF167
@LF165:     dec     L00D2+1
@LF167:     inc     $E3
            bne     @LF16D
            inc     $E4
@LF16D:     lda     DEFBNK
            sta     BNKCTL
            ldx     #$00
            stx     $E2
            jsr     LEDB2
            jmp     @LF123

@LF17D:     lda     L00D2
            beq     @LF19F
            ldy     $E2
            ldx     #$00

@LF185:     .byte   $B9             ; lda $E000, y
@LF186:     .byte   <SYSRAM
@LF187:     .byte   >SYSRAM

            sta     (MEMBUFF,x)
            inc     MEMBUFF
            bne     @LF190
            inc     MEMBUFF+1
@LF190:     dec     L00D2
            beq     @LF19A
            iny
            bne     @LF185
            jmp     @LF167

@LF19A:     iny
            beq     @LF167
            sty     $E2
@LF19F:     lda     DEFBNK
            sta     BNKCTL
            jsr     LEEF4
            jmp     @LF1EB

@ISDEV:     and     #$7F            ; Clear device's higher bit
            tax                     ; and use it as an index
            lda     DDTI,x          ; to get the device's driver
            sta     DRIVERP         ;
            lda     DDTI+1,x        ;
            sta     DRIVERP+1       ;
            jsr     LF24F
            jsr     LF246
            lda     #$00
            sec
            sbc     MEMCOUNT
            sta     MEMCOUNT
            lda     #$00
            sbc     MEMCOUNT+1
            sta     MEMCOUNT+1
            ora     MEMCOUNT
            beq     @LF1EB
@LF1D1:     jsr     LF1F7
            bcs     @LF1DE
            inc     MEMCOUNT
            bne     @LF1D1
            inc     MEMCOUNT+1
            bne     @LF1D1
@LF1DE:     lda     L00D2
            clc
            adc     MEMCOUNT
            sta     MEMCOUNT
            lda     L00D2+1
            adc     MEMCOUNT+1
            sta     MEMCOUNT+1
@LF1EB:     ldx     CHANNEL
            clc
            lda     MEMCOUNT
            ora     MEMCOUNT+1
            bne     @RET
            sec
@RET:       rts


LF1F7:      lda     DEFBNK
            sta     BNKCTL
            jsr     JDRIVERP
            ldx     LE6D3
            stx     BNKCTL
            nop
            nop
            nop
            bcs     LF217
            clc
            ldx     #$00
            sta     (MEMBUFF,x)
            inc     MEMBUFF
            bne     LF216
            inc     MEMBUFF+1
LF216:      rts

LF217:      sec
            rts

JDRIVERP:   jmp     (DRIVERP)       ; Jumps to current device driver's routine

NULDRVI:    lda     EOF             ; Just returns "End of file"
NULDRVO:    sec                     ; Error
            rts                     ;

LF221:      jsr     LF24F
            jsr     LF081
            jsr     LEF12
            bcs     LF22F
            jsr     ERROR46         ; System crash: file ordinal check error
LF22F:      sec                     ; MEMCOUNT > File size?
            lda     $CF             ;
            sbc     MEMCOUNT        ;
            lda     $D0             ;
            sbc     MEMCOUNT+1      ;
            lda     $D1             ;
            sbc     #$00            ;
            bcs     LF246           ; No
            lda     $CF             ; Yes, adjust MEMCOUNT to file size
            sta     MEMCOUNT        ;
            lda     $D0             ;
            sta     MEMCOUNT+1      ;
LF246:      lda     MEMCOUNT        ; Set bytes to transfer
            sta     L00D2           ;
            lda     MEMCOUNT+1      ;
            sta     L00D2+1         ;
            rts


LF24F:      lda     DSTBANK         ; Check destination bank
            bne     @DESTOK         ; Is not system bank, so don't check for
                                    ; protected memory
            lda     MEMBUFF+1
            bne     @NOTZP          ; Jump if orig not in ZP
            lda     MEMBUFF         ; Check if orig is in reserved ZP space
            cmp     #$B0            ;
            bcc     @DESTOK         ; Orig below $00B0 (OK)
            bcs     @DESTPRTCT      ; Orig gt or eq $00B0 (reserved CODOS space)
            ; Not reached

@NOTZP:     cmp     #$02            ; Check if orig below $0200 (reserved CODOS space)
            bcs     @DESTOK         ; Orig gt or eq $0200
@DESTPRTCT: bit     IGNORWRP        ; Is copy to CODOS space allowed?
            bmi     @DESTOK         ; Yes, go on
            jsr     ERROR17         ; No, reserved or protected memory violation

@DESTOK:    lda     MEMCOUNT        ; Check if <from> + <count> > $FFFF
            clc
            adc     MEMBUFF
            lda     MEMCOUNT+1
            adc     MEMBUFF+1
            bcc     @DESTOK2        ; Dest beyond 0xFFFF?
            jsr     ERROR16         ; yes, <from> address greater than <to> address
@DESTOK2:   cmp     #>SYSRAM        ; Is it in SYSRAM (protected)
            bcc     @DESTOK3        ; No, go on
            lda     DSTBANK         ; Bank 0?
            bne     @DESTOK3        ; No, go on
            bit     IGNORWRP        ; Is copy to CODOS space allowed?
            bmi     @DESTOK3        ; Yes, go on
            jsr     ERROR17         ; No, reserved or protected memory violation
@DESTOK3:   nop
            ; Fall through

; HERE

LF28C:      lda     DSTBANK
            and     #$03
            eor     DEFBNK
            sta     LE6D3
            rts

LF298:      jsr     ASSIGNED
            bpl     LF2A0
            jmp     LF385

LF2A0:      jsr     LF28C
            jsr     LEFC3
            lda     $E5
            sta     LF2EA
            sta     LF2D6
            sta     LF2DC
            sta     LF341
            lda     $E6
            sta     LF2EB
            sta     LF2D7
            sta     LF2DD
            sta     LF342
LF2C2:      lda     LE6D3
            sta     BNKCTL
            lda     MEMCOUNT+1
            bne     LF2CF
            jmp     LF336

LF2CF:      ldy     $E2
            bne     LF2E3
LF2D3:      lda     (MEMBUFF),y
        
            .byte   $99             ; sta $E000, y
LF2D6:      .byte   $00
LF2D7:      .byte   $E0

            iny
            lda     (MEMBUFF),y
        
            .byte   $99             ; sta $E000, y
LF2DC:      .byte   $00
LF2DD:      .byte   $E0     
            
            iny
            bne     LF2D3
            beq     LF2F8
LF2E3:      ldx     $E2
            ldy     #$00
LF2E7:      lda     (MEMBUFF),y

            .byte   $9D             ; sta $E000, x
LF2EA:      .byte   $00
LF2EB:      .byte   $E0

            iny
            inx
            bne     LF2E7
            tya
            clc
            adc     MEMBUFF
            sta     MEMBUFF
            bcc     LF2FA
LF2F8:      inc     MEMBUFF+1
LF2FA:      lda     $E2
            beq     LF305
            clc
            adc     MEMCOUNT
            sta     MEMCOUNT
            bcs     LF307
LF305:      dec     MEMCOUNT+1
LF307:      lda     DEFBNK
            sta     BNKCTL
            jsr     LF088
            inc     $E3
            bne     LF316
            inc     $E4
LF316:      ldy     #$00
            sty     $E2
            jsr     LEF05
            bcc     LF375
            jsr     LEF26
            lda     LE774
            cmp     LE773
            bne     LF2C2
            jsr     LF3D2
            ldy     LE76C
            jsr     LEE85
            jmp     LF2C2

LF336:      lda     MEMCOUNT
            beq     LF35E
            ldy     $E2
            ldx     #$00
LF33E:      lda     (MEMBUFF,x)

            .byte   $99             ; sta $E000, y
LF341:      .byte   $00
LF342:      .byte   $E0     

            inc     MEMBUFF

            bne     LF349
            inc     MEMBUFF+1
LF349:      dec     MEMCOUNT
            beq     LF353
            iny
            bne     LF33E
            jmp     LF307

LF353:      iny
            beq     LF307
            sty     $E2
            lda     $DC
            ora     #$40
            sta     $DC
LF35E:      lda     DEFBNK
            sta     BNKCTL
            jsr     LEF05
            php
            bcc     LF36D
            jsr     LEF26
LF36D:      jsr     LEEF4
            ldx     CHANNEL
            plp
            rts

LF375:      jsr     LEFD0
            lda     MEMCOUNT+1
            bne     LF382
            lda     LE76E
            jsr     LEDB5
LF382:      jmp     LF2C2

LF385:      and     #$7F
            tax
            lda     DDTO,x
            sta     DRIVERP
            lda     DDTO+1,x
            sta     DRIVERP+1
            jsr     LF28C
            lda     #$00
            sec
            sbc     MEMCOUNT
            sta     MEMCOUNT
            lda     #$00
            sbc     MEMCOUNT+1
            sta     MEMCOUNT+1
            ora     MEMCOUNT
            beq     LF3B3
LF3A8:      jsr     LF3B8
            inc     MEMCOUNT
            bne     LF3A8
            inc     MEMCOUNT+1
            bne     LF3A8
LF3B3:      sec
            ldx     CHANNEL
            rts

LF3B8:      ldx     DEFBNK
            stx     BNKCTL
            ldx     #$00
            lda     (MEMBUFF,x)
            jsr     JDRIVERP
            lda     LE6D3
            sta     BNKCTL
            inc     MEMBUFF
            bne     LF3D1
            inc     MEMBUFF+1
LF3D1:      rts

LF3D2:      ldy     #$FF
            lda     ($E9),y
            tay
LF3D7:      iny
            lda     ($E9),y
            bne     LF3D7
            cpy     #$F9
            bcc     LF3FE
            ldy     #$00
LF3E2:      iny
            lda     ($E9),y
            bne     LF3E2
            cpy     #$F9
            bcc     LF3FE
            lda     $E3
            bne     LF3F1
            dec     $E4
LF3F1:      dec     $E3
            dec     $E2
            jsr     LEF26
            jsr     LEEF4
            jsr     ERROR38         ; Diskette is full; all blocks already allocated.
LF3FE:      lda     #$FC
            jsr     LEE85
            tya
            ldy     #$FF
            sta     ($E9),y
            rts

; Open drive 0
;
OPENDRV0:   ldx     #$00
            ; Fall through

; Open drive X
;
            .export OPENDRV

OPENDRV:    stx     CURRDRV
            jsr     DRVVALID        ; Check if valid (does not return if not)
            lda     ODRIVES,x       ; Check if open
            bpl     @CONT           ; No, go on
            txa
            jsr     CLDRIVE
@CONT:      ldx     CURRDRV
            jsr     INITDRV
            ldx     CURRDRV
            lda     #$80
            sta     ODRIVES,x
            lda     #$00
            jmp     LEE9B
            ; Not reached


; Close drive X
;
            .export CLOSEDRV

CLOSEDRV:   jsr     DRVVALID        ; Check if valid (does not return if not)
            lda     ODRIVES,x       ; Is it open?
            bpl     @RETURN         ; No, return
            stx     CURRDRV         ; Save to current drive
            ldx     #$09
@LOOP:      jsr     GETDEV          ; Get device for channel
            beq     @NEXT           ; Not assigned, check next
            bmi     @NEXT           ; Not a file, check next
            tax
            lda     FILEDRV,x       ; Get drive of file
            cmp     CURRDRV         ; Is it ours?
            bne     @CNEXT          ; No, check next
            ldx     CHANNEL         ; Recover channel
            jsr     FREECH
@CNEXT:     ldx     CHANNEL
@NEXT:      dex
            bpl     @LOOP
            ldx     CURRDRV
            lda     #$00
            sta     ODRIVES,x
@RETURN:    rts

; Serve pending interrupt (if any)
;
SRVINT:
@CHECK:     bit     HSRCW
            bmi     @RETURN         ; No pending interrupt
            jsr     SNSINTST        ; Execute a Sense Interrupt command
            bcc     @RETURN         ; If success, return
            jsr     CLDRIVE         ; If not, close drive
            jmp     @CHECK          ; Repeat until no pending interrupt
            ; Not reached
@RETURN:    rts

; Close drive A (internal)
;
;
CLDRIVE:    sty     SAVEY6          ; Save Y
            and     #$03            ; Mask out track
            sta     SAVEDRV         ; And save it
            ldy     #$09
@LOOP:      ldx     IOCHTBL,y       ; Get channel's device or file
            bmi     @NEXT           ; Check next if it is a device driver
            beq     @NEXT           ; or not assigned
            lda     FILEDRV,x       ; Get drive
            cmp     SAVEDRV         ; Is it our drive
            bne     @NEXT           ; No, check next
            lda     #$00            ; 
            sta     LE65C,x         ; Invalidate
            sta     IOCHTBL,y       ; Unassign channel
@NEXT:      dey
            bpl     @LOOP
            ldx     SAVEDRV         ;
            lda     #$00            ; Close drive
            sta     ODRIVES,x       ;
            ldy     SAVEY6          ; Restore Y
            rts

LF4A0:  ldx     #$00
        jsr     LF5AD
        cmp     NDRIVES
        bcc     LF4AD
        jmp     LF574

LF4AD:  sec
        ror     LE786
        jsr     LF77A
        bne     LF4B9
        jmp     LF531

LF4B9:  lsr     LE786
        ldx     CURRDRV
        jsr     EXSENSEDRV
        bit     DSKSTAT
        bvc     LF4CA
        jsr     ERROR21         ; New file on write-protected diskette.
LF4CA:  ldy     #$FD
        lda     ($E9),y
        cmp     #$F8
        bcc     LF4D5
        jsr     ERROR39         ; Diskette is full; no room left in directory.
LF4D5:  jsr     LF3D2
        sta     LE6EC
        jsr     LF6A5
        beq     LF4E3
        jsr     ERROR45         ; System crash: directory/file table check error.
LF4E3:  jsr     LF812
        lda     LE6F2
        jsr     LEE9B
        ldy     LE6F1
        dey
        ldx     #$00
LF4F2:  lda     LE6DD,x
        sta     $E500,y
        iny
        inx
        cpx     #$10
        bcc     LF4F2
        jsr     LEEAA
        lda     #$80
        sta     $DC
        jsr     LF567
        lda     #$40
        sta     MEMCOUNT
        lda     #$00
        sta     MEMCOUNT+1
        lda     #$DD
        sta     MEMBUFF
        lda     #$E6
        sta     MEMBUFF+1
        ldx     CHANNEL
        jsr     LF298
        jsr     LF088
        jsr     LEEF4
        ldy     #$FD
        lda     ($E9),y
        clc
        adc     #$01
        jsr     LEE85
        jmp     LEEA5

LF531:  ldx     LE6DB
        lda     $E50E,x
        sta     LE6EC
        jsr     LF6A5
        beq     LF542
        jsr     LF084
LF542:  lda     #$40
        jsr     LF81E
        jsr     LEDB2
        lda     $DC
        bne     LF560
        ldx     #$02
        ldy     #$13
LF552:  lda     ($E5),y
        sta     $DF,x
        dey
        dex
        bpl     LF552
        ldy     #$10
        lda     ($E5),y
        sta     $DC
LF560:  lda     $DC
        ora     #$C0
        sta     LE786
LF567:  jsr     LEEF4
        ldx     CHANNEL
        lda     DEVICE
        sta     IOCHTBL,x
        rts

LF574:  ldy     #$08
LF576:  lda     DNT,y
        cmp     CURRDRV
        beq     LF584
        dey
        bpl     LF576
        jsr     ERROR11         ; Missing or illegal device or file name
LF584:  tya
        asl     a
        ora     #$80
        ldx     CHANNEL
        sta     IOCHTBL,x
        sta     DEVICE
        rts

;
;
LF592:      ldx     #$00
            ; Fall through

;
; Channel in X
;
LF594:      jsr     LF5AD
            jsr     DRVVALIDO
            jsr     LF77A
            beq     LF5AA
            bit     UNKFLAG3
            bpl     LF5A7
            jsr     ERROR01         ; Command not found
LF5A7:      jsr     ERROR02         ; File not found
LF5AA:      jmp     LF531

;
;
LF5AD:      lda     CURRDRV         ; Get current drive
            sta     SAVEA2          ; Save it
            jsr     FREECH          ; Free channel
            lda     #$00
            sta     LE786
            lda     SAVEA2          ; Recover drive
            tax
            sta     CURRDRV
            rts

; Free channel 0
;
            .export FREECH0

FREECH0:    ldx     #$00
            ; Fall through

; Free channel in X
;
            .export FREECH

FREECH:     jsr     GETDEV          ; Get device or file for the channel
            beq     @RETURN         ; If not assigned, return

            ldx     #$09
@LOOP:      lda     IOCHTBL,x       ; Get file or device assigned to the channel
            cmp     DEVICE          ; Is it our device?
            bne     @NEXT           ; No, check next
            cpx     CHANNEL         ; Is it our channel
            bne     @DOFREE         ; No, free it
@NEXT:      dex                     ; Yes, continue search
            bpl     @LOOP           ;

            ldx     DEVICE          ; Device not assigned to any other channel
            bmi     @DOFREE         ; If it is not a file, go to unassign channel
            jsr     LF081
            jsr     LF81C
            jsr     LEDB2
            lda     $E0
            ora     $E1
            bne     @LF5F9
            lda     $DF
            cmp     #$41
            bcs     @LF5F9
            jmp     LF639

@LF5F9:     ldy     #$13
            ldx     #$02
@LF5FD:     lda     $DF,x
            cmp     ($E5),y
            beq     @LF60B
            sta     ($E5),y
            lda     $DC
            ora     #$40
            sta     $DC
@LF60B:     dey
            dex
            bpl     @LF5FD
            jsr     LF084
            ldx     $DD
            lda     LE758,x
            bpl     @LF61C
            jsr     LEEA5
@LF61C:     lda     #$00
            ldx     DEVICE
            sta     LE65C,x
@DOFREE:    ldx     CHANNEL         
            lda     #$00
            sta     IOCHTBL,x
@RETURN:    rts

        jsr     DRVVALIDO
        stx     CURRDRV
        jsr     LF592
        jsr     LEFC6
LF639:  ldy     #$15
        lda     ($E5),y
        jsr     LEE9B
        ldy     #$14
        lda     ($E5),y
        tax
        sta     LE6DB
        ldy     #$01
LF64A:  lda     $E500,x
        cmp     #$2E
        beq     LF65C
        cmp     ($E5),y
        beq     LF658
        jsr     ERROR50         ; System crash: Directory redundancy check failed.
LF658:  iny
        inx
        bne     LF64A
LF65C:  ldx     LE6DB
        lda     #$00
        sta     $E500,x
        jsr     LEEAA
        ldy     #$FD
        lda     ($E9),y
        sec
        sbc     #$01
        jsr     LEE85
        ldy     $DE
        jsr     LF691
        lda     #$00
        ldx     DEVICE
        sta     LE65C,x
        ldx     #$09
LF680:  lda     IOCHTBL,x
        cmp     DEVICE
        bne     LF68D
        lda     #$00
        sta     IOCHTBL,x
LF68D:  dex
        bpl     LF680
        rts

LF691:  lda     ($E9),y
        tax
        lda     #$00
        jsr     LEE85
        cpx     #$F9
        bcs     LF6A2
        txa
        tay
        jmp     LF691

LF6A2:  jmp     LEEA5

LF6A5:  lda     #$00
        sta     DEVICE
        ldx     LE797
LF6AD:  txa
        sec
        sbc     #$0D
        bmi     LF6D8
        tax
        lda     LE65C,x
        beq     LF6D2
        lda     FILEDRV,x
        cmp     CURRDRV
        bne     LF6AD
        lda     LE65E,x
        cmp     LE6EC
        bne     LF6AD
        stx     DEVICE
        jsr     LEEE3
        lda     $DC
        rts

LF6D2:  stx     DEVICE
        jmp     LF6AD

LF6D8:  ldx     DEVICE
        bne     LF6E0
        jsr     ERROR29         ; All buffers in use (free a chan. assigned to a file)
LF6E0:  jsr     LEEE3
        lda     CURRDRV
        sta     $DD
        lda     LE6EC
        sta     $DE
        lda     #$00
        sta     $DC
        rts

        lda     ($C7),y
        jsr     ISALPHA
        bcs     LF712
        tax
        iny
        lda     ($C7),y
        jsr     VALFNCHR
        bcc     LF711
        txa
        ldx     #$07
LF705:  cmp     DNT,x
        beq     LF70F
        dex
        bpl     LF705
        ora     #$80
LF70F:  sec
        rts

LF711:  dey
LF712:  lda     DEFDRV
        sta     CURRDRV
        lda     #$00
        sta     LE787
        jsr     FNAMFROMBUF
        bcs     LF766
        lda     ($C7),y
        cmp     COLON
        bne     LF73E
LF729:  iny
        lda     ($C7),y
        cmp     #$20
        beq     LF729
        sec
        sbc     #$30
        bcc     LF766
        cmp     NDRIVES
        bcs     LF766
        sta     CURRDRV
        iny
LF73E:  jsr     LFDEF
        lda     ODRIVES,x
        bpl     LF76A
        sty     $02A1
        jsr     EXSENSEDRV
        lda     DSKSTAT
        and     #$40
        lsr     a
        lsr     a
        sta     LE787
        jsr     LF77A
        php
        lda     #$00
        ldy     $02A1
        plp
        bne     LF76C
        lda     #$20
        bne     LF76C
LF766:  lda     #$80
        bne     LF76C
LF76A:  lda     #$40
LF76C:  ora     LE787
        ora     CURRDRV
        sta     LE787
        clc
        bit     LE787
        rts

LF77A:  jsr     LEE78
        ldx     #$00
        stx     LE6F2
        stx     LE6F1
        inx
        stx     LE775
        stx     LE6DB
        ldy     #$FD
        lda     ($E9),y
        sta     $029B
        bne     LF79F
        inc     LE6F2
        inc     LE6F1
LF79B:  lda     LE6F1
        rts

LF79F:  jsr     LEE9E
LF7A2:  ldy     LE6DB
        ldx     #$00
LF7A7:  lda     $E500,y
        beq     LF7E1
        cmp     #$2E
        beq     LF7BA
        cmp     FNAMBUF,x
        bne     LF7CF
        inx
        iny
        jmp     LF7A7

LF7BA:  lda     FNAMBUF,x
        cmp     #$2E
        bne     LF7CF
        lda     $E501,y
        cmp     FNAMBUF+1,x
        bne     LF7CF
        lda     #$00
        sta     LE6F1
        rts

LF7CF:  dec     $029B
        bne     LF7DB
        lda     LE6F1
        bne     LF79B
        beq     LF7FA
LF7DB:  jsr     LF800
        jmp     LF7A2

LF7E1:  lda     LE6F2
        bne     LF7DB
        lda     LE775
        sta     LE6F2
        lda     LE6DB
        sta     LE6F1
        lda     $029B
        bne     LF7DB
        jmp     LF79B

LF7FA:  jsr     LF800
        jmp     LF7E1

LF800:  clc
        lda     LE6DB
        adc     #$10
        sta     LE6DB
        bcc     LF811
        inc     LE775
        jsr     LEE9E
LF811:  rts

LF812:  lda     #$40
        sta     $DF
        lda     #$00
        sta     $E0
        sta     $E1
LF81C:  lda     #$00
LF81E:  sta     $E2
        lda     #$00
        sta     $E3
        sta     $E4
        rts

; TODO: Should it be a CODOS entry point?
;
; Copy file name from buffer pointed by ($C7) to FNAMBUF
;
            .export FNAMFROMBUF0

FNAMFROMBUF0:
            ldy     #$00
            ; Fall through    

; Copy file name from buffer pointed by ($C7),y to FNAMBUF
;
            .export FNAMFROMBUF

FNAMFROMBUF:
            ldx     #$00
@LOOP:      lda     ($C7),y
            jsr     VALFNCHR        ; Is it a valid file name character?
            bcs     @CHKEXT         ; No, check if extension
            sta     FNAMBUF,x       ; Yes, store
            iny                     ; And advance
            inx                     ;
            cpx     #FNAMLEN+1      ; Are we past max filename lenght?
            bcc     @LOOP           ; No, copy next char
            bcs     @RETURN         ; Yes, return with CS (error)
            ; Not reached

@CHKEXT:    cmp     #'.'            ; Extension?
            bne     @NOEXT          ; No, assume end and add default extension
            sta     FNAMBUF,x       ; Yes, store the dot
            iny                     ; Get the extension char
            lda     ($C7),y         ;
            iny                     ; Advance 1 pos
            bne     @STOREXT        ; And go to store extension (always jumps)
            ; Not reached

@NOEXT:     lda     #'.'            ; Store default extension
            sta     FNAMBUF,x       ;
            lda     DEFAULTEXT      ;
@STOREXT:   sta     FNAMBUF+1,x     ; Store extension
            lda     FNAMBUF         ; Check that the first char of file name
            jsr     ISALPHA          ; is a letter
            bcs     @RETURN         ; If not, return with CS (error)
            ldx     #$01            ; Advance to second char
@LOOP2:     lda     FNAMBUF,x       ; Get char
            cmp     #'.'            ; Is it the extension separator?
            beq     @VALEXT         ; Yes, go validate extension
            jsr     VALFNCHR        ; No, check it is a valid file name char
            bcs     @RETURN         ; If not, return with CS (error)
            inx                     ; Advance to next char
            cpx     #FNAMLEN+1      ; Are we past max filename lenght?
            bcc     @LOOP2          ; No, continue with next char
            bcs     @RETURN         ; Yesm return with CS (error)
            ; Not reached

@VALEXT:    cpx     #$01            ; File name length too short?
            beq     @RETURN         ; Yes, return (Shouldn't it set the carry flag?)
            lda     FNAMBUF+1,x     ; Get the extension char
            jsr     ISALPHANUM       ; Validate that it is alphanumeric
@RETURN:    rts


; Character validation routines.
; Character in A
; Return carry clear if vaild, carry set if not
;
; Check if char is alphanumeric
;
ISALPHANUM:  jsr     ISNUM          ; Is it a number?
            bcc     RETVAL          ; Yes, return CC 
            ; Fall through


; Check if character is alphabetic
;
            .export ISALPHA

ISALPHA:    cmp     #'A'           ; Is it a letter
            bcs     CHKZ            ; Maybe, complete check
NOVAL:      sec                     ; Definitely not, return CS
            rts                     ;
CHKZ:       cmp     #'Z'+1          ; Is it 'Z' or lower?
RETVAL:     rts                     ; Yes, return CC; No, return CS

; Check if character is numeric
;
ISNUM:      cmp     #'0'            ; Is it a number?
            bcc     NOVAL           ; No, return CS
            cmp     #'9'+1          ; Is it '9' or lower
            rts                     ; Yes, return CC; No, return CS

; Check if A is a valid filename character
; Returns CC if char is '_' or alphanumeric, CS otherwise
;
            .export VALFNCHR

VALFNCHR:   cmp     ULINE           ; Is underline?
            bne     ISALPHANUM      ; No, check alphanumeric
            clc                     ; Yes, return OK
            rts

HEXWORD0:   ldx     #$00

; Converts word at P0SCRATCH,x into its 4-char ascii hex representation
; at  (OUTBUFP),y
;
HEXWORD:    lda     P0SCRATCH+1,x   ; Gets most significant byte
            jsr     HEXBYTE         ; Converts it
            lda     P0SCRATCH,x     ; Gets less significant byte
            ; Fall through

; Converts byte in A into its 2-char ascii hex representation
; at  (OUTBUFP),y
;
HEXBYTE:    pha                     ; Save byte
            lsr     a               ; Get upper nibble
            lsr     a               ;
            lsr     a               ;
            lsr     a               ;
            jsr     @NIBBLE         ; Convert it
            pla                     ; Recover byte
@NIBBLE:    and     #$0F            ; and get lower nibble
            clc
            adc     #$30            ; Adds "0"
            cmp     #$3A            ; Is it "9" or lower?
            bmi     @STORE          ; Yes, goto store it
            adc     #$06            ; Nope, add 7 (6 + carry) to get hex digit
@STORE:     sta     (OUTBUFP),y     ; And store it
            iny                     ; Next position
            rts                     ; and return


LF8BA:      lda     #$0A        
            jsr     LF915
LF8BF:      sec
            sbc     #$30
            bcc     LF8EB
            cmp     TMPPTR
            bcs     LF8EB
            jsr     LF8F1
            jmp     LF8BF

LF8CE:      lda     #$10
            jsr     LF915

LF8D3:      sec
            sbc     #$30
            bcc     LF8EB
            cmp     #$0A
            bcc     LF8E6
            sbc     #$07
            cmp     #$0A
            bcc     LF8EB
            cmp     #$10
            bcs     LF8EB
LF8E6:      jsr     LF8F1
            bne     LF8D3
LF8EB:      rol     $029F           ; Clear flag
            jmp     GETNEXTCH

LF8F1:  pha
        stx     $0286
        ldx     #$17
        jsr     LFB29
        pla
        clc
        adc     P0SCRATCH
        sta     P0SCRATCH
        lda     P0SCRATCH+1
        adc     #$00
        sta     P0SCRATCH+1
        bcc     LF90B
        jsr     ERROR19             ; Arithmetic overflow.
        ; Not reached

LF90B:  dec     $029F
        ldx     $0286
        iny
        lda     (INPBUFP),y
        rts

LF915:      sta     TMPPTR          ; Save count in TMPPTR
            lda     #$00            ;
            sta     TMPPTR+1        ;
            sta     P0SCRATCH       ; Init buffer pointer
            sta     P0SCRATCH+1     ;
            sta     $029F           ; Clear which flag???
            beq     GETNEXTNB       ; Always jump to get next non blank
            ; Not reached

; Get next non-blank character from buffer starting at current pos + 1
; Y contains current position at INPBUFP and it is updated at exit
;
            .export GETNEXTNB1

GETNEXTNB1: iny
            ; Fall through


; Get next non-blank character from input buffer starting at current position
; Y contains current position at INPBUFP and it is updated at exit
;
            .export GETNEXTNB

GETNEXTNB:  jsr     GETNEXTCH
            beq     @RETURN         ; If null or semicolon, return
            cmp     #$20            ; If blank,
            beq     GETNEXTNB1           ;   get next char
@RETURN:    rts

            iny                     ; Dead code?

; Get char from (INPBUFP),y and return it in A
; If No more chars (NULL , ';' or EOL), zero flag is set
; Preserves carry flag
;
            .export GETNEXTCH

GETNEXTCH:  lda     (INPBUFP),y     ; Get char from input buffer
            beq     @RETURN         ; if null, return
            bcs     @CSCONT         ; We come from carry set?
            cmp     #$0D            ; No, end of line?
            beq     @CCRET          ; Yes, return success
            cmp     SCOLON          ; Set zero flag if semicolon
@CCRET:     clc                     ; Clear carry
            rts

@CSCONT:    cmp     #$0D            ; End of line
            beq     @CSRET          ; Yes, return with carry set
            cmp     SCOLON          ; Set zero flag if semicolon
@CSRET:     sec
@RETURN:    rts


; Print program counter and registers to output line buffer
;
            .export PRNREGSLB

PRNREGSLB:  jsr     SETOUTB         ; Set output line buffer as destination
            ; Fall through

; Print program counter and registers to output buffer
;
PRNREGS:    jsr     PRNSTR
            .byte   "P=", $0
            ldx     #$19
            jsr     HEXWORD
            lda     #':'
            sta     (OUTBUFP),y
            iny
            lda     PRGBANK         ; Load Current Program Bank
            clc
            adc     #'0'            ; Convert to ASCII
            sta     (OUTBUFP),y
            iny
            lda     #'/'
            sta     (OUTBUFP),y
            iny
            lda     DATBANK         ; Load Current Data Bank
            clc
            adc     #'0'            ; Convert to ASCII
            sta     (OUTBUFP),y
            iny
            lda     #' '
            sta     (OUTBUFP),y
            iny
            lda     #'('
            sta     (OUTBUFP),y
            iny
            jsr     LF9DB
            jsr     LF9BA
            jsr     HEXBYTE
            dey
            jsr     LF9BA
            iny
            jsr     HEXBYTE
LF98D:      dey
            dey
            jsr     LF9BA
            iny
            iny
            jsr     HEXBYTE
            lda     #$29
            sta     (OUTBUFP),y
            iny
            ldx     #$04
LF99E:      lda     #$20
            sta     (OUTBUFP),y
            iny
            lda     REGDESC,x
            sta     (OUTBUFP),y
            iny
            lda     #$3D
            sta     (OUTBUFP),y
            iny
            lda     STACKP,x
            jsr     HEXBYTE
            dex
            bpl     LF99E
            jmp     LF9DB

LF9BA:      lda     PRGBANK
            eor     DEFBNK
            sta     BNKCTL
            lda     (PCSAVE),y
            ldx     DEFBNK
            stx     BNKCTL
            rts

REGDESC:    .byte   "SFYXA"

LF9D1:  ldx     #$02
        jsr     LF9DD
LF9D6:  lda     #$0D
        jmp     PRNCHAR

LF9DB:  ldx     #$02
LF9DD:  sty     MEMCOUNT
        lda     #$00
        sta     MEMCOUNT+1
        lda     OUTBUFP
        sta     MEMBUFF
        lda     OUTBUFP+1
        sta     MEMBUFF+1
        jsr     LF298
        ldy     #$00
        rts

; Get line from input channel in X and store into INPBUFP
; Returns carry clear if success, carry set on error (no input)
; Returns length (excluding end of line) in A
;
;LF9F1
            .export GETLINE

GETLINE:    ldy     #$00
            lda     IOCHTBL,x       ; Get device/file for channel in X
            cmp     #$82            ; Is it the console?
            bne     @FROMDEV        ; No, jump
            lda     INPBUFP         ; Yes, set console input buffer
            sta     $F0             ;
            lda     INPBUFP+1       ;
            sta     $F1             ;
            jmp     JINLINE         ; Get entire line from keyboard and place it
                                    ; into ($F0) (which now it is INPBUFP)

@FROMDEV:   jsr     GETCHAR         ; Get character from device
            bcs     @END            ; If none, end with carry set (error)
            cmp     #$0D            ; End of line?
            beq     @CCEND          ; Yes, end with carry clear (success)
            sta     (INPBUFP),y     ; Store in buffer
            iny                     ; Increment buffer index
            cpy     YLNLIM          ; Buffer full?
            bcc     @FROMDEV        ; No, get next character
@CCEND:     clc                     ; Clear carry (success)
@END:       lda     #$0D            ; Store end of line
            sta     (INPBUFP),y     ;
            tya                     ; Return length (excluding end of line)
            beq     @RETURN         ; No input, return
            clc
@RETURN:    ldy     #$00
            rts

; Get character from input channel in X and return it in A
; Carry clear on success, carry set if no input
;
            .export GETCHAR

GETCHAR:    lda     IOCHTBL,x       ; Get devive/file for channel in X
            cmp     #$82            ; Is it the console?
            bne     @FROMDEV        ; No, jump
            jsr     JCINP           ; Yes, get character from console
            cmp     EOF             ; No input?
            beq     @RETURN         ; Return
            clc                     ; Clear carry (success)
@RETURN:    rts

@FROMDEV:   jsr     LFAA4
            sec                     ; Set ignore memory write protection flag
            ror     IGNORWRP        ;
            jsr     LF0FC
            php                     ; Save processor status
            clc                     ; Clear gnore memory write protection flag
            rol     IGNORWRP        ;

            ; Fall through

LFA42:      ldy     $028B
            ldx     $028A
            lda     SAVECH          ; Recover char
            plp                     ; Recover processor status
            rts

JCINP:      jmp     (CINP)          ; Jump to console input routine


; Print string immediately following the JSR call
;
            .export PRNSTR

PRNSTR:     stx     $0287           ; Save X
            ldx     #$02            ; X = 2
            bne     LFA5A           ; Always jump
            stx     $0287           ; Dead code?
LFA5A:      sta     $0288           ; Save A
            sty     $0289           ; Save Y
            pla                     ; Get PC and save in TMPPTR. PC points
            sta     TMPPTR          ; to last opcode of instruction
            pla                     ;
            sta     TMPPTR+1        ;
LFA66:      inc     TMPPTR          ; Increment PC (points to first char of string)
            bne     LFA6C           ;
            inc     TMPPTR+1        ;
LFA6C:      ldy     #$00
            lda     (TMPPTR),y      ; Get char
            beq     LFA78           ; If null, end of string
            jsr     PRNCHAR         ; Print char
            jmp     LFA66           ; Loop

LFA78:      lda     TMPPTR+1        ; Push new PC to the stack
            pha                     ;
            lda     TMPPTR          ;
            pha                     ;
            ldy     $0289           ; Restore indexes
            lda     $0288           ;
            ldx     $0287           ;
            rts

; Print character in A to channel in X
;
PRNCHAR:    sta     SAVECH          ; Save char
            lda     IOCHTBL,x       ; Get device/file for channel
            cmp     #$82            ; Is it console?
            bne     LFA9A           ; No, jump
            lda     SAVECH          ; Recover char
            jsr     JCOUTP          ; And output to console
            sec                     ; Set carry (Why?)
            rts

LFA9A:      jsr     LFAA7
            jsr     LF298
            php
            jmp     LFA42

LFAA4:      sta     SAVECH          ; Save char
LFAA7:      stx     $028A           ; Save X
            sty     $028B           ; Save Y
            lda     #$01
            sta     MEMCOUNT
            lda     #$00
            sta     MEMCOUNT+1
            lda     #<SAVECH
            sta     MEMBUFF
            lda     #>SAVECH
            sta     MEMBUFF+1
            rts

JCOUTP:     jmp     (COUTP)             ; Jump to console output routine

LFAC1:  stx     $028C
        lda     #$00
        sta     $029F
        ldx     #$06

LFACB:  lda     LFB05,x
        sta     TMPPTR
        lda     LFB05+1,x
        sta     TMPPTR+1
        stx     $028D
        ldx     #$17
        jsr     LFB70
        lda     P0SCRATCH
        bne     LFAE8
        bit     $029F
        bmi     LFAEC
        bpl     LFAEF
LFAE8:  sec
        ror     $029F
LFAEC:  jsr     LFAFE
LFAEF:  jsr     LFBAA
        ldx     $028D
        dex
        dex
        bpl     LFACB
        ldx     $028C
        lda     P0SCRATCH
LFAFE:  clc
        adc     #$30
        sta     (OUTBUFP),y
        iny
        rts

LFB05:  .word   $000A
        .word   $0064
        .word   $03E8
        .word   $2710

LFB0D:  clc
        lda     P0SCRATCH,x
        adc     P0SCRATCH
        sta     P0SCRATCH
        lda     P0SCRATCH+1,x
        adc     P0SCRATCH+1
        sta     P0SCRATCH+1
        rts

LFB1B:  sec
        lda     P0SCRATCH
        sbc     P0SCRATCH,x
        sta     P0SCRATCH
        lda     P0SCRATCH+1
        sbc     P0SCRATCH+1,x
        sta     P0SCRATCH+1
        rts

LFB29:  jsr     LFB35
        lda     L00D2+1
        ora     L00D2
        bne     LFB6D
        lda     P0SCRATCH+1
LFB34:  rts

LFB35:  stx     $029C
        lda     #$00
        sta     L00D2
        sta     L00D2+1
        lda     P0SCRATCH,x
        sta     $D4
        lda     P0SCRATCH+1,x
        sta     $D5
        ldx     #$11
        clc
LFB49:  ror     L00D2+1
        ror     L00D2
        ror     P0SCRATCH+1
        ror     P0SCRATCH
        dex
        beq     LFB65
        bcc     LFB49
        lda     L00D2
        clc
        adc     $D4
        sta     L00D2
        lda     L00D2+1
        adc     $D5
        sta     L00D2+1
        bcc     LFB49
LFB65:  ldx     $029C
        rts

LFB69:  lda     P0SCRATCH+1,x
        bne     LFB7B
LFB6D:  jsr     ERROR19         ; Arithmetic overflow.
LFB70:  sty     $028E
        lda     P0SCRATCH,x
        sta     $D4
        beq     LFB69
        lda     P0SCRATCH+1,x
LFB7B:  sta     $D5
        lda     #$00
        sta     P0SCRATCH,x
        sta     P0SCRATCH+1,x
        ldy     #$11
        clc
        bcc     LFB9F
LFB88:  rol     P0SCRATCH,x
        rol     P0SCRATCH+1,x
        lda     P0SCRATCH,x
        sec
        sbc     $D4
        sta     L00D2
        lda     P0SCRATCH+1,x
        sbc     $D5
        bcc     LFB9F
        sta     P0SCRATCH+1,x
        lda     L00D2
        sta     P0SCRATCH,x
LFB9F:  rol     P0SCRATCH
        rol     P0SCRATCH+1
        dey
        bne     LFB88
        ldy     $028E
        rts

LFBAA:  lda     P0SCRATCH,x
        sta     P0SCRATCH
        lda     P0SCRATCH+1,x
        sta     P0SCRATCH+1
        rts

LFBB3:  lda     P0SCRATCH
        sta     P0SCRATCH,x
        lda     P0SCRATCH+1
        sta     P0SCRATCH+1,x
        rts

LFBBC:  lda     P0SCRATCH+1,x
        pha
        lda     P0SCRATCH,x
        pha
        jsr     LFBB3
        pla
        sta     P0SCRATCH
        pla
        sta     P0SCRATCH+1
        rts

            .export LFBCC

LFBCC:      stx     SAVEX8          ; Save X
            ldx     #$15
            jsr     LFBE1
            lda     $D7
            beq     LFBDB
            jsr     ERROR18         ; <value> out of range (greater than $FF or less than 0).
LFBDB:      lda     $D6
            ldx     SAVEX8
            rts

; X is the position in P0SCRATCH
;
LFBE1:      lda     #$00
            sta     P0SCRATCH,x     ; Init buffer at pos X and X+1
            sta     P0SCRATCH+1,x   ;
            stx     SAVEX6          ; Save X
            tax                     ; X = 0
            jsr     GETNEXTNB       ; Get next char from command line
            cmp     #'-'
            bne     @NODASH
            inx                     ; Skip dash
@LFBF3:     iny                     ;
@NODASH:    stx     $0291           
            ldx     SAVEX6               
            jsr     GETNEXTNB
            cmp     PERIOD
            beq     @LFC59
            cmp     DOLLAR
            bne     @LFC0A
            jsr     GETNEXTNB1
@LFC0A:     jsr     LF8CE
            bcc     @LFC61
@LFC0F:     jsr     LFBBC
            lda     $0291
            bne     @LFC32
            jsr     LFB0D
@LFC1A:     jsr     LFBBC
@LFC1D:     jsr     GETNEXTNB
            ldx     #$04
@LFC22:     cmp     LE79B,x
            beq     @LFBF3
            dex
            bpl     @LFC22
            ldx     SAVEX6
            jsr     GETNEXTCH
            sec
            rts

@LFC32:     dec     $0291
            bne     @LFC3D
            jsr     LFB1B
            jmp     @LFC1A

@LFC3D:     dec     $0291
            bne     @LFC48
            jsr     LFB29
            jmp     @LFC1A

@LFC48:     dec     $0291
            bne     @LFC53
            jsr     LFB70
            jmp     @LFC1A

@LFC53:     jsr     LFB70
            jmp     @LFC1D

@LFC59:     jsr     GETNEXTNB1
            jsr     LF8BA
            bcs     @LFC0F
@LFC61:     rts

        sta     LE71E
        lda     #$00
        sta     LE720
        lda     LE6D6
        bit     LE77B
        bpl     LFC75
        lda     LE6D7
LFC75:  sta     LE71F
        lda     #$58
        sta     SAVEDMAGIC
        lda     MEMCOUNT
        sec
        sbc     MEMBUFF
        sta     MEMCOUNT
        lda     MEMCOUNT+1
        sbc     MEMBUFF+1
        sta     MEMCOUNT+1
        bcs     LFC8F
        jsr     ERROR16         ; <from> address greater than to address.
LFC8F:  inc     MEMCOUNT
        bne     LFC95
        inc     MEMCOUNT+1
LFC95:  bit     LE77B
        bpl     LFC9D
        jsr     LFCF5
LFC9D:  ldx     #$02
        bit     LE77C
        bpl     LFCA6
        ldx     #$06
LFCA6:  jsr     LFBAA
        ldx     #$05
LFCAB:  lda     P0SCRATCH,x
        sta     LE721,x
        dex
        bpl     LFCAB
        jsr     LFCD9
        ldx     CHANNEL
        jsr     LF298
        jsr     LFCEA
        bit     LE77B
        bpl     LFCC7
        jsr     LFCF5
LFCC7:  ldx     CHANNEL
        lda     LE6D6
        sta     DSTBANK
        jsr     LF298
        lda     #$00
        sta     DSTBANK
        rts

LFCD9:  lda     #$0A
        sta     MEMCOUNT
        lda     #$00
        sta     MEMCOUNT+1
        lda     #<SAVEDMAGIC
        sta     MEMBUFF
        lda     #>SAVEDMAGIC
        sta     MEMBUFF+1
        rts

LFCEA:  ldx     #$05
LFCEC:  lda     LE721,x
        sta     P0SCRATCH,x
        dex
        bpl     LFCEC
        rts

LFCF5:  ldx     #$01
LFCF7:  lda     $C9,x
        pha
        lda     MEMBUFF,x
        sta     $C9,x
        pla
        sta     MEMBUFF,x
        dex
        bpl     LFCF7
        rts

LFD05:  sta     $0294               ; Save A in temporary storage
        jsr     LFD35
        bcs     LFD33               ; Return with error
        lda     LE71E
        cmp     $0294
        bne     LFD33               ; Return with error
        jsr     LFCEA
        lda     LE71F
        sta     DSTBANK
        bit     LE77B
        bpl     LFD2C
        jsr     LFCF5
        lda     LE6D7
        sta     DSTBANK
LFD2C:  ldx     CHANNEL
        jsr     LF0FC
        rts

LFD33:  sec
        rts

LFD35:      jsr     LFCD9
            lda     #$00
            sta     DSTBANK
            sec                     ; Set ignore memory write protection flag
            ror     IGNORWRP        ;
            jsr     LF0FC
            bcs     LFD51
            rol     IGNORWRP        ; Clear ignore memory write protection flag
            lda     SAVEDMAGIC      ; Check that the magic number is correct
            cmp     #$58            ;
            beq     LFD52           ; Yes, jump to return OK
            sec                     ; Return error
LFD51:      rts

LFD52:      clc                     ; Return OK
            rts

; Assigns default input device and set input buffer
;
DEFSETINPB: lda     #$00
            ldx     CHANN1          ; Get input channel device
            sta     CHANN1          ; And clears it
            jsr     UNASSIGN
            ; Fall through

; Set input buffer to input line buffer
;
            .export SETINPB

SETINPB:    lda     CHANN1          ; Get input channel device
            bne     @CONT           ; If set, go on
            lda     #$82            ; If not, set default (console)
            sta     CHANN1          ;
@CONT:      ldy     #$00
            lda     INPLBUF         ; Set input line buffer
            sta     INPBUFP         ;
            lda     INPLBUF+1       ;
            sta     INPBUFP+1       ;
            rts

; Set output buffer to output line buffer
; Also clears Y
;
            .export SETOUTB

SETOUTB:    lda     CHANN2          ; Get output channel device
            bne     @CONT           ; If set, continue
            lda     #$82            ; If not, set to console
            sta     CHANN2          ;
@CONT:      ldy     #$00
            lda     OUTLBUF         ; Set output line buffer
            sta     OUTBUFP         ;
            lda     OUTLBUF+1       ;
            sta     OUTBUFP+1       ;
            rts

; Assigns default output device and set output buffer
;
DEFSETOUTB: lda     #$00            ; Get and clear channel 2 device or file
            ldx     CHANN2          ; and save it in X
            sta     CHANN2          ;
            jsr     UNASSIGN        ; Unassign file (if it is a file)
            jmp     SETOUTB         ; And set output buffer

; Unassign file
;
; Device or file number in X
;
UNASSIGN:   bmi     @RETURN         ; If it is a device or
            beq     @RETURN         ; not assigned, return
            stx     DEVICE          ; Save device
            ldx     #$09
@LOOP:      lda     IOCHTBL,x       ; Search for file in the I/O channel table
            cmp     DEVICE          ; If it is assigned
            beq     @RETURN         ; returns
            dex
            bpl     @LOOP
            ldx     DEVICE          ; Not found in the device table
            lda     #$00            ; Close or free entry
            sta     LE65C,x         ;
@RETURN:    rts

; Console Input Routine
;
CIN:        jsr     JGETKEY         ; Get key
            cmp     ETX             ; End of text?
            beq     JCNTRLC         ; Yes, process CTRL-C
            cmp     EOF             ; End of file?
            beq     @RETURN         ; Yes, return
            bit     KBDECHO         ; No, is ECHO on?
            bpl     @CCRET          ; No, return with carry clear
            jsr     JOUTCH          ; Yes, echo character
@CCRET:     clc
@RETURN:    rts

; Console Output Routine
;
COUT:       sta     $0299           ; Save char
            jsr     JTSTKEY         ; Check if key pressed
            bcc     COUTC           ; No, output char
CHKCTLC:    cmp     ETX             ; Is it CTRL-C?
            bne     KEYPR           ; No, go on
JCNTRLC:    jmp     CNTRLC          ; Process CTRL-C
KEYPR:      cmp     XOFF            ; Is it XOFF?
            bne     COUTC           ; No, output char
            jsr     JGETKEY         ; Yes, get key
            bpl     CHKCTLC         ; If it is atandard ASCII, check again
COUTC:      lda     $0299           ; Output char
            jmp     JOUTCH          ;

LFDEF:      jsr     SRVINT
            ldx     CURRDRV
            rts

.if  CODOS2_VER = 17
EXSENSEDRV17:
            jsr     EXSENSEDRV
@LOOP:      jsr     LF766
            inx
            bne     @LOOP
            rts
.endif
        ; ORIGIN OF CODOS OVERLAYS

        .segment "overlays"
OVLORG:
        .end
