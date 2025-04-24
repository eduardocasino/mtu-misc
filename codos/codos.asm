; Disassembly of CODOS 2.0 by Eduardo Casino (mail@eduardocasino.es)
;

; da65 V2.18 - Ubuntu 2.19-1
; Created:    2023-11-25 21:28:31
; Input file: codos.bin
; Page:       1

    ; Possible values: 11, 14, 15, 17
    ; If none is specified, 15 is assumed

.ifndef CODOS2_VER
    CODOS2_VER  = 15
.endif

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

    IOENABLE        = $FFFE         ; Enable I/O space from $BE00 to $BFFF
    IODISABLE       = $FFFF         ; Disable I/O space (enable RAM) from $BE00 to $BFFF

            ;   Relevant scratch RAM addresses
            ;
    SAVEDY          = $0285         ; Use to preserve Y register during disk operations

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

            ; $C1 - $EC : Seratch RAM used by CODOS nucleus,
            ; SVC Processor and Command Proc. 

CODOSSCRT:  .res $2b                ; $C1 - $EB
    
    L00D2           = $00D2         ; TODO

INTSVA:     .res 1                  ; $EC  Accumulator save during SVC or IRQ processing.

            ; $ED - $EF : Global RAM used by CODOS 

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
    
            .segment "cmdproc"

CMDPROC:

    LD846           = $D846
    LD9FE           = $D9FE

            .segment "codos"
            
            ; Jump table (page 179)
            ;
            jmp     COLDST
JWARMST:    jmp     WARMST
JGETKEY:    jmp     GETKEY
JOUTCH:     jmp     OUTCH
JTSTKEY:    jmp     TSTKEY
            jmp     NMIPROC
            jmp     IRQPROC
LE615:      jmp     LDD23
JERROR37:   jmp     ERROR37         ; Jump to " Required software package not loaded" error message
JINLINE:    jmp     INLINE
            jmp     CIN
LE621:      jmp     COUT            ; Jump to console-character-out routine with CTRL-S/Q (XON/XOFF)
            jmp     JERROR37        ; Required software package not loaded in memory
            jmp     JERROR37        ; Required software package not loaded in memory

            ; Device name table

DNT:        .byte   "N"             ; Null device driver
            .byte   "C"             ; Console device
            .byte   "P"             ; Printer device
            .byte   "Y"             ; ???
            .byte   $00             ; Reserved for custom devices
            .byte   $00             ;
            .byte   $00             ;
            .byte   $00             ;

            ; Device driver dispatch table for input

DDTI:       .word   NULDRV          ; Null driver device (DTI=$80)
CINP:       .word   CIN             ; Console input routine (DTI=$82)
            .word   ERROR33         ; Input from output-only device, or visa-versa
            .word   ERROR33         ; Input from output-only device, or visa-versa
            .word   ERROR33         ; Input from output-only device, or visa-versa
            .word   ERROR33         ; Input from output-only device, or visa-versa
            .word   ERROR33         ; Input from output-only device, or visa-versa
            .word   ERROR33         ; Input from output-only device, or visa-versa

            ; Device driver dispatch table for output

DDTO:       .word   NULDRVO         ; Null driver device (DTI=$80)
COUTP:      .word   COUT            ; Console output routine (DTI = $82)
            .word   $D280           ; Printer output routine (DTI = $84)
            .word   $D27D           ; ??????? output routine (DTI = $86)
            .word   ERROR33         ; Input from output-only device, or visa-versa
            .word   ERROR33         ; Input from output-only device, or visa-versa
            .word   ERROR33         ; Input from output-only device, or visa-versa
            .word   ERROR33         ; Input from output-only device, or visa-versa

LE652:      .byte   $00
LE653:      .byte   $82
LE654:      .byte   $82
            .byte   $00
            .byte   $00
            .byte   $00
            .byte   $00
            .byte   $00
            .byte   $00
            .byte   $00
LE65C:      .byte   $06
LE65D:      .byte   $00
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
LE6C5:      .byte   $90   
            .byte   $8C
            .byte   $88
            .byte   $84

STACKP:     .byte   $FF             ; Stack pointer
PROCST:     .byte   $04             ; Processor Status
YREG:       .byte   $00             ; Y
XREG:       .byte   $00             ; X
ACCUM:      .byte   $00             ; Accumulator

PRGBANK:    .BYTE   $00             ; Current program bank
DATBANK:    .BYTE   $00             ; Current data bank
BNKCFG:     .BYTE   $00             ; Current bank configuration
LE6D1:      .BYTE   $00
LE6D2:      .BYTE   $00

LE6D3:      .byte   $7F
LE6D4:      .byte   $00
LE6D5:      .byte   $00
LE6D6:      .byte   $00
LE6D7:      .byte   $00
DEFBNK:     .byte   $7F             ; Default bank configuration
LE6D9:      .byte   $00
LE6DA:      .byte   $00

LE6DB:      .byte   $00

LE6DC:      .byte   $00             ; Another variable RETRIES?

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
LE730:      .byte   $00             ; Drive and head (see above)
LE731:      .byte   $00

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

LE754:      .byte   $00
            .byte   $00
            .byte   $00
            .byte   $00
LE758:      .byte   $00
            .byte   $00
            .byte   $00
            .byte   $00

            ; Error recovery

ERRADDR:
            .word   $0000           ; Address+1 where last error was detected by CODOS.
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
LE769:      .byte   $00             ; uPD765 error count???
CMDIDX:     .byte   $01             ; uPD765 command index
LE76B:      .byte   $FF
LE76C:      .byte   $00
LE76D:      .byte   $00
LE76E:      .byte   $00
LE76F:      .byte   $00
LE770:      .byte   $00
LE771:      .byte   $00
LE772:      .byte   $00
LE773:      .byte   $07
LE774:      .byte   $00
LE775:      .byte   $00
LE776:      .byte   $00             ; DMA Direction?
LE777:      .byte   $00
LE778:      .byte   $00
IGNORERR:   .byte   $00             ; Flag. If bit 7 = 1 then system will ignore (continue after)
                                    ; irrecoverable disk read errors (use a last resort only).
SAVEOVERWR: .byte   $00             ; Flag. If bit 7 = 1 then permits save command to overwrite an
                                    ; existing file with the same name.
LE77B:      .byte   $00
LE77C:      .byte   $00
LE77D:      .byte   $00
LE77E:      .byte   $00
LE77F:      .byte   $00
SVC13FLG:   .byte   $00             ; Flag. If bit 7 = 1 then program executing was invoked by SVC #13.
LE781:      .byte   $00
LE782:      .byte   $00
LE783:      .byte   $00
LE784:      .byte   $00
LE785:      .byte   $00
LE786:      .byte   $00
LE787:      .byte   $00
KBDECHO:    .byte   $00             ; Keyboard echo flag for CODOS. Set to $80 to enable echo.
LE789:      .byte   $03
LE78A:      .byte   $13
LE78B:      .byte   $1A
LE78C:      .byte   $5F             ; _
LE78D:      .byte   $3B             ; ;
LE78E:      .byte   $2E             ; .
LE78F:      .byte   $24             ; $
LE790:      .byte   $3A             ; :
            .byte   $5E             ; ^
            .byte   $22             ; "
DEFAULTEXT: .byte   "C"             ; Current ASCII default file extension character ("C").
LE794:      .byte   $11
LE795:      .byte   $00
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
LE7BE:      .word   $0500           ; Pointer to start of system input line buffer.
LE7C0:      .word   $0600           ; Pointer to start of system output line buffer
LE7C2:      .word   $A000           ; Pointer to large transient buffer for COPYF, ETC.
LE7C4:      .word   $1400           ; Size (NOT. final address) of large transient buffer.
INTSRVP:    .word   INTSRV          ; Pointer to user-defined interrupt service routine.
ERRRCVRYP:  .word   ERRRCVRY        ; Pointer to user-defined error recovery routine.     
LE7CA:      .byte   $1A
            .byte   $00
            .byte   $00
            .byte   $00
            .byte   $00
LE7CF:      .byte   $00
LE7D0:      .byte   $00
LE7D1:      .byte   $00
LE7D2:      .byte   $FF
            .byte   $FF
            .byte   $FF
LE7D5:      .byte   $00
            .byte   $00
            .byte   $00
LE7D8:      .byte   $00
            .byte   $00
            .byte   $00
LE7DB:      .byte   $00
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
            jsr     LF409
            jsr     LEBCC

            ; Load STARTUP.J
            ;
            ldx     #$08            ; Copy file name to buffer 
LE7FE:      lda     STARTUPNAM,x    ;
            sta     FNAMBUF,x       ;
            dex                     ;
            bpl     LE7FE           ;
            ldx     #$00            ; What is this for? Retry?
            stx     LE6DC           ;
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
            jsr     LEBF5

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
            jsr     LF470
            dey
            bpl     LE866
            jsr     LFD8D
            jsr     INITIO
            jsr     SENSE
            jsr     LF409
            jsr     PRNSTR
            .byte   "RESET.", $00
            jmp     WARMST

NMIPROC:    sta     INTSVA
            lda     #$00
            sta     HSRCW
            sec
            ror     LE784
            jmp     LE8A9

IRQPROC:    sta     INTSVA
            pla
            pha
            and     #$10
            bne     LE8B8
            lda     INTSVA
            jmp     (INTSRVP)       ; Jump to user-defined interrupt service routine


; Interrupt service routine
;
INTSRV:     lda     #$00
            sta     HSRCW
            sta     LE784
LE8A9:      sec
            ror     LE783
            pla
            sta     PROCST
            pla
            sta     $DA
            pla
            jmp     LE8CF

LE8B8:      lda     #$00
            sta     HSRCW
            sta     LE783
            pla
            and     #$EF
            sta     PROCST
            pla
            sec
            sbc     #$02
            sta     $DA
            pla
            sbc     #$00

LE8CF:  sta     $DB
        stx     XREG
        sty     YREG
        cld
        tsx
        stx     STACKP
        lda     BNKCTL
        sta     BNKCFG
        and     #$03
        eor     #$03
        sta     DATBANK
        lda     BNKCFG
        lsr     a
        lsr     a
        and     #$03
        eor     #$03
        sta     PRGBANK
        lda     BNKCFG
        ora     #$0F
        sta     DEFBNK
        jsr     LEBF5
        lda     INTSVA
        sta     ACCUM
        bit     LE783
        bmi     LE963
        sec
        ror     LE77F
        ldx     #$02
LE910:  lda     LE7D2,x
        cmp     PRGBANK
        bne     LE951
        lda     LE7D5,x
        cmp     $DA
        bne     LE951
        lda     LE7D8,x
        cmp     $DB
        bne     LE951
        ldy     #$00
        lda     PRGBANK
        eor     DEFBNK
        sta     BNKCTL
        lda     LE7DB,x
        sta     ($DA),y
        lda     DEFBNK
        sta     BNKCTL
        lda     #$FF
        sta     LE7D2,x
        jsr     LFD54
        jsr     LFD8D
        jsr     PRNSTR
        .byte   $0d, "BP", $00
        jmp     LE9A1

LE951:  dex
        bpl     LE910
        lda     PRGBANK
        bne     LE963
        lda     SVCENB
        sta     LE6D1
        bpl     LE963
        jmp     SVCPROC

LE963:  jsr     LFD54
        jsr     LFD8D
        bit     LE783
        bpl     LE999
        jsr     PRNSTR
        .byte   $0D, "INTERRUPT (", $00
        bit     LE784
        bpl     LE98E
        jsr     PRNSTR
        .byte   "NMI)", $00
        jmp     LE9A1

LE98E:  jsr     PRNSTR
        .byte   "IRQ)", $00
        jmp     LE9A1

LE999:  jsr     PRNSTR
        .byte   $0D, "BRK", $00

LE9A1:  jsr     PRNSTR
        .byte   ", ", $00
        jsr     LF94B
        jmp     WARMST

ERROR52:  inc     ERRNUM
ERROR51:  inc     ERRNUM
ERROR50:  inc     ERRNUM
ERROR49:  inc     ERRNUM
ERROR48:  inc     ERRNUM
ERROR47:  inc     ERRNUM
ERROR46:  inc     ERRNUM
ERROR45:  inc     ERRNUM
ERROR44:  inc     ERRNUM
ERROR43:  inc     ERRNUM
ERROR42:  inc     ERRNUM
ERROR41:  inc     ERRNUM
ERROR40:  inc     ERRNUM
ERROR39:  inc     ERRNUM
ERROR38:  inc     ERRNUM
ERROR37:  inc     ERRNUM        ; Required software package not loaded in memory. 
ERROR36:  inc     ERRNUM
ERROR35:  inc     ERRNUM
ERROR34:  inc     ERRNUM
ERROR33:  inc     ERRNUM
ERROR32:  inc     ERRNUM
ERROR31:  inc     ERRNUM
ERROR30:  inc     ERRNUM
ERROR29:  inc     ERRNUM
ERROR28:  inc     ERRNUM
ERROR27:  inc     ERRNUM
ERROR26:  inc     ERRNUM
ERROR25:  inc     ERRNUM
ERROR24:  inc     ERRNUM
ERROR23:  inc     ERRNUM
ERROR22:  inc     ERRNUM
ERROR21:  inc     ERRNUM
ERROR20:  inc     ERRNUM
ERROR19:  inc     ERRNUM
ERROR18:  inc     ERRNUM
ERROR17:  inc     ERRNUM
ERROR16:  inc     ERRNUM
ERROR15:  inc     ERRNUM
ERROR14:  inc     ERRNUM
ERROR13:  inc     ERRNUM
ERROR12:  inc     ERRNUM
ERROR11:  inc     ERRNUM
ERROR10:  inc     ERRNUM
ERROR09:  inc     ERRNUM
ERROR08:  inc     ERRNUM
ERROR07:  inc     ERRNUM
ERROR06:  inc     ERRNUM
ERROR05:  inc     ERRNUM
ERROR04:  inc     ERRNUM
ERROR03:  inc     ERRNUM
ERROR02:  inc     ERRNUM
ERROR01:  inc     ERRNUM
          jmp     (ERRRCVRYP)

        ; Error recovery routine
        ;
ERRRCVRY:
        pha
        lda     #$00
        sta     HSRCW
        cld
        jsr     LEBF5
        bit     LE782
        bpl     LEA2B
        pla
        jmp     WARMST

LEA2B:  pla
        sta     ERRORA
        stx     ERRORX
        sty     ERRORY
        tsx
        stx     ERRORS
        php
        pla
        sta     ERRORP
        pla
        sec
        sbc     #$02
        sta     ERRADDR
        pla
        sbc     #$00
        sta     ERRADDR+1
        lda     #$80
        sta     LE782
        jsr     LFD54
        jsr     LFD8D
        jsr     PRNSTR
        .byte   $0d, "CODOS ERROR #", $00
        lda     ERRNUM
        jsr     HEXBYTE
        jsr     LF9D1
        bit     LE77D
        bpl     LEA9B
LEA75:  lda     ($CB),y
        cmp     #$0D
        beq     LEA81
        jsr     PRNCHAR
        iny
        bne     LEA75
LEA81:  jsr     LF9D6
        ldy     $EB
        beq     LEA90
        lda     #$20
LEA8A:  jsr     PRNCHAR
        dey
        bne     LEA8A
LEA90:  lda     #$5E
        jsr     PRNCHAR
        jsr     LF9D6
        jmp     LEABD

LEA9B:  bit     LE77F
        bmi     LEABA
        bit     LE781
        bmi     LEABD
        lda     ERRADDR
        sta     $DA
        lda     ERRADDR+1
        sta     $DB
        ldx     #$04
LEAB1:  lda     ERRORS,x            ; Copy registers at error
        sta     STACKP,x
        dex
        bpl     LEAB1
LEABA:  jsr     LF948
LEABD:  bit     LE782
        bpl     WARMST
        ldx     #$0B
LEAC4:  lda     SYSERRMNAM,x        ; Get file with error messages
        sta     FNAMBUF,x           ;
        dex                         ;
        bpl     LEAC4               ;
        inx                         ; X == 0
        stx     LE6DC               ; Init retries?
        jsr     LF77A
        bne     WARMST
        jsr     LF592
LEAD9:  ldx     #$00
        jsr     LF9F1
        bcs     LEAF7
        dec     ERRNUM
        bne     LEAD9
        tay
        tax
LEAE6:  lda     ($CB),y
        sta     ($CD),y
        dey
        bpl     LEAE6
        txa
        tay
        ldx     #$02
        jsr     LF9D6
        jsr     LF9D1
LEAF7:  jsr     LF5C3

LEAFA:  nop                     ; TODO: Probably room for inserting a subroutine
        nop                     ; or a jump to a different WARMST sequence
        nop                     ;

; $EAFD
WARMST: cld
        lda     #$00
        sta     HSRCW
        jsr     LEBC4
        jmp     CMDPROC

        jsr     LEBC4
        jmp     LD846

        jsr     LD9FE
        bit     LE6D5
        bpl     LEB20
        lda     LE6D4
        sta     PRGBANK
        sta     DATBANK
LEB20:  jmp     LEB41

        jsr     LD9FE
        lda     LE6D4
        sta     PRGBANK
        sta     DATBANK
        ldx     #$7F
        stx     DEFBNK
        ldx     LE785
        stx     LE6D1
        ldx     #$FF
        bit     SVC13FLG
        bpl     LEB47
LEB41:  jsr     LEB72
        jmp     L00D2

LEB47:  stx     STACKP
        jsr     LEB72
        jsr     BANKSW
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

LEB72:  pla
        tay
        pla
        bit     SVC13FLG
        bmi     LEB7E
        ldx     STACKP
        txs
LEB7E:  pha
        tya
        pha
        lda     PRGBANK
        asl     a
        asl     a
        eor     DEFBNK
        eor     DATBANK
        sta     BNKCFG
        lda     #$7F
        sta     SVIA1DIR
        jsr     LEC1E
        lda     #$00
        sta     LE77D
        sta     LE77F
        lda     #$03
        bit     LE778
        bpl     LEBA8
        lda     #$01                    ; Set DMA direction byte to write
LEBA8:  sta     HSRCW                   ;
        lda     LE6D1
        sta     SVCENB
        lda     ACCUM
.if  CODOS2_VER = 17
        sta     SAVEACC
.else
        sta     INTSVA
.endif
        ldy     YREG
        ldx     XREG
        lda     PROCST
        pha
        lda     BNKCFG
        plp
        rts

LEBC4:  lda     CMDPROC                 ; Check if Command Processor is loaded
        cmp     #$D8                    ; First byte should be $D8 (CLD)
        bne     LEBCC                   ; No, go ahead and load from disk
        rts                             ; Yes, return

LEBCC:  ldx     #$09                    ; Get file name
LEBCE:  lda     CMDPROCNAM,x            ;
        sta     FNAMBUF,x               ;
        dex                             ;
        bpl     LEBCE                   ;
        ldx     #$00
        stx     LE6DC               ; Init retries?
        jsr     LF592
        ldx     #$00
        txa
        jsr     LFD05
        bcc     LEBEA
        jsr     ERROR13             ; Not a loadable ("SAVEd") file. 
LEBEA:  ldx     #$00
        txa
        jsr     LFD05
        bcc     LEBEA
        jmp     LF5C3

LEBF5:  sec
        ror     SEEIO               ; Set I/O space enable semaphore
        lda     #$00
        sta     LE6D2               ; TODO: Unknown variable
        lda     DEFBNK              ; Set default bank config
        sta     BNKCTL              ;
        lda     #$7F                ;
        sta     SVIA1DIR            ;

        ; This clears the break flag by forcing an rti

        lda     #>LEC11            ; Set return address to $EC11
        pha
        lda     #<LEC11
        pha
        php
        rti

LEC11:  rts


; Copy bank switch/restore routine to $0100-$0112

CPYBNKSW: 
        ldx     #$12
LEC14:  lda     LEC32,x
        sta     BANKSW,x
        dex
        bpl     LEC14
        rts

LEC1E:  ldx     #$07
LEC20:  lda     LEC2A,x
        sta     a:L00D2,x
        dex
        bpl     LEC20
        rts

LEC2A:  php
        sta     BNKCTL
.if  CODOS2_VER = 17
        lda     SAVEACC
.else
        lda     INTSVA
.endif
        plp
        .byte   $4C                 ; JMP Absolute. As LEC2A is copied to $D2, it
                                    ; means it jumps to address contained in
                                    ; $DA-$DB

        ; Bank switch/restore routine. Copied to 0100-0112

LEC32:  jsr     L00D2
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
SNDCMD: sty     SAVEDY          ; Save registers
        stx     CMDIDX          ;     Note: does not seem that X is restored afterwards...
        ldy     CMDTBL,x        ; Get command length

LEC4E:  lda     #$10            ; Check if uPD765 is busy processing a command (it shouldn't)
        and     MSTR            ;
        beq     LEC6E           ; No, send command
LEC55:  bit     MSTR            ; Yes, wait until finished
        bpl     LEC55           ;
        bvs     LEC68           ; Jump if data register needs to be read
        lda     #$00            ; Otherwise, try to complete command sequence
        sta     DATR            ;
LEC61:  nop                     ; Wait a couple of cycles
        inc     LE769           ; Increment error count????
        jmp     LEC4E           ; And try again

LEC68:  lda     DATR            ; Read status register
        jmp     LEC61           ; And try again

        ;       Write command to uPD765 Data Register
        ;       X -> index to command byte in command table
        ;       Y -> command length 
        ;
LEC6E:  lda     MSTR            ; Read uPD765 Main Status Register
        bpl     LEC6E           ; Jump if bit 7 is 0 (Not ready)
        and     #$40            ; Check data direction
        beq     LEC7A           ; Jump if data register is to be written
        jsr     ERROR48
LEC7A:  lda     CMDTBL+1,x      ; Write command byte
        sta     DATR            ;
        inx                     ; next command byte
        dey                     ;
        bne     LEC6E           ; Until command length
        ldy     SAVEDY          ; Restore Y register
        rts                     ; And return

GETDRVST:
        stx     DRVNUM
        ldx     #SENSEDRV
        bne     LEC91           ; Always jump
SENSE:  ldx     #SENSEINT       ; Send Sense interrupt command to the disk controller
LEC91:  jsr     SNDCMD
RSLTPH:  ldx     #$00
LEC96:  lda     MSTR
        bpl     LEC96
        and     #$40
        bne     LECA2
        jsr     ERROR49
LECA2:  lda     DATR
        sta     DSKSTAT,x
        nop
        nop
        inx
        lda     MSTR
        and     #$10
        bne     LEC96
        rts

        ; Send command to uPD765 and process status
        ;
SNDCMDST:  jsr     SNDCMD
SNDCMD1:   lda     HSRCW           ; Wait for interrupt ( Bit 7 of HSRCW is 0)
           bmi     SNDCMD1
LECBB:  jsr     SENSE
        lda     DSKSTAT
        cmp     #$C0
        rts

LECC4:  jsr     LEF9F
        stx     LE72D
LECCA:  lda     HSRCW
        bmi     LECD5
        jsr     SENSE
        jmp     LECCA

LECD5:  ldx     #SPECIFY
        jsr     SNDCMD
        ldx     #RECALIBRATE
        jsr     SNDCMDST
        and     #$D8            ; Delete don't care bits from ST0
        beq     LECED           ; No error
        and     #$08            ; Fail: Check if ready
        bne     LECEA           ; Not ready
        jsr     ERROR42         ; Unformatted diskette or hardware drive fault
LECEA:  jsr     ERROR06         ; Drive not ready error
LECED:  lda     ST1             ; Get status register 1
        beq     LECF5           ; All clear
        jsr     ERROR41         ; Unformatted diskette or irrecoverable seek error
LECF5:  ldx     LE72D           ; Get drive
.if  CODOS2_VER = 17
        jsr     GETDRVSTKLUDGE  ; Sense drive
.else
        jsr     GETDRVST        ; Sense drive
.endif
        ldx     LE72D           ; Get drive
        lda     DSKSTAT         ; Get status result
        and     #$08            ; Filter out except Two Sides flag
        beq     LED07           ; One side
        lda     #$80            ; Two sides
LED07:  sta     DRVNFO,x        ; Store info for drive
        rts

LED0B:  jsr     LEF90
        jsr     LED24
        bcs     LED58
        jsr     LECC4
        jsr     LED52
        sta     TRKERRNUM
        jsr     LED24
        bcs     LED58
        jsr     ERROR41
LED24:  stx     LE730
        stx     LE770
        cmp     #$4D
        bcc     LED31
        jsr     ERROR47
LED31:  sta     LE731
        lda     LE76F
        beq     LED41
        lda     #$04
        ora     LE730
        sta     LE730
LED41:  jsr     LF45F
        ldx     #SEEK
        jsr     SNDCMDST
        bcs     LED59
        and     #$F8
        cmp     #$20
        beq     LED52
        clc
LED52:  ldx     LE770
        lda     LE731
LED58:  rts

LED59:  and     #$03
        cmp     LE770
        bne     LED63
        jsr     ERROR06
LED63:  jsr     LF470
        jmp     LED41

LED69:  lda     $E7             ; Set DMA register to 
        sta     ADMA
        lda     LE73B
        cmp     #$1A
        bcc     LED78
        jsr     ERROR44
LED78:  lda     LE776
        sta     HSRCW
        ldx     #READWRITE
        jsr     SNDCMD
LED83:  lda     HSRCW
        bmi     LED83
        jsr     RSLTPH
        lda     DSKSTAT
        and     #$D8
        beq     LEDA4
        cmp     #$40
        beq     LED99
        jsr     ERROR40
LED99:  lda     ST1
        and     #$B7
        cmp     #$80
        beq     LEDA4
        sec
        rts

LEDA4:  clc
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
LEDCF:  jsr     ERROR36
LEDD2:  sta     LE776
        sta     HSRCW                   ; Set DMA mode
        jsr     LEF90
        stx     LE736
        stx     LE771
        lda     #$00
        sta     LE76F
        sta     LE738
        lda     LE739
        cmp     #$1A
        bcc     LEE0D
        sbc     #$1A
        sta     LE739
        lda     DRVNFO,x                ; Check if one or two sides
        bmi     LEDFD                   ; Two sides
        jsr     ERROR44                 ; One side
LEDFD:  lda     #$04
        ora     LE736
        sta     LE736
        lda     #$01
        sta     LE738
        lda     LE739
LEE0D:  sta     LE73B
        lda     LE731
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
        jsr     LECC4
        lda     LE737
        sta     TRKERRNUM
        jsr     LED0B
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
        jsr     ERROR30
        jmp     LEE1B

LEE63:  inc     WRERRCNT
        lda     ST1
        and     #$02
        beq     LEE2C
        jsr     ERROR10
LEE70:  jsr     LEEE3
        lda     $DD
        sta     LE6DC
LEE78:  lda     #$00
        sta     $E9
        lda     #$E4
        sec
        sbc     LE6DC
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
        ldx     LE6DC
        lda     #$00
        sta     LE758,x
        rts

LEEC2:  lda     #$00
        sta     LE76F
        ldx     LE6DC
        jsr     LEF90
        lda     #$0C
        jsr     LED0B
        lda     #$94
        sta     $E7
        lda     LE775
        bne     LEEE2
        lda     LE6C5,x
        sta     $E7
        lda     #$00
LEEE2:  rts

LEEE3:  ldy     LE6DA
        ldx     #$00
LEEE8:  lda     LE65C,y
        sta     $DC,x
        iny
        inx
        cpx     #$0D
        bmi     LEEE8
        rts

LEEF4:  ldy     LE6DA
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

LEF33:  cmp     #$00
        beq     LEF3C
        cmp     OVLORG
        bne     LEF3D
LEF3C:  rts

LEF3D:  stx     $029A
        cmp     LE794
        bcc     LEF48
        jsr     ERROR43
LEF48:  sta     LE795
        ldx     #$00
        stx     LE76F
        ldx     #$0C
        cmp     #$09
        bcc     LEF62
        inx
        bit     DRVNFO                  ; Check if one or two sides
        bpl     LEF62                   ; One side
        dex                             ; Two sides
        lda     #$01
        sta     LE76F
LEF62:  txa
        ldx     #$00
        jsr     LED0B
        lda     #$F8
        sta     $E7
        lda     LE795
        clc
        adc     #$11
        cmp     #$1A
        bcc     LEF7E
        bit     DRVNFO                  ; Check if one or two sides
        bmi     LEF7E                   ; Two sides
        sec                             ; One side
        sbc     #$1A
LEF7E:  jsr     LEDB5
        lda     LE795
        cmp     OVLORG
        beq     LEF8C
        jsr     ERROR35
LEF8C:  ldx     $029A
        rts

LEF90:  jsr     LEF9F
        pha
        lda     LE754,x
        bmi     LEF9D
        pla
        jsr     ERROR03
LEF9D:  pla
        rts

LEF9F:  cpx     NDRIVES
        bcc     LEFA7
        jsr     ERROR05
LEFA7:  rts

LEFA8:  stx     LE6D9
        cpx     #$0A
        bcc     LEFB2
        jsr     ERROR08
LEFB2:  lda     LE652,x
        sta     LE6DA
        rts

LEFB9:  jsr     LEFA8
        bne     LEFC1
        jsr     ERROR09
LEFC1:  tax
        rts

LEFC3:  jsr     LEE70
LEFC6:  lda     $DC
        and     #$20
        beq     LEFCF
        jsr     ERROR07
LEFCF:  rts

LEFD0:  lda     #$00
        sta     LE76F
        lda     #$07
        sta     LE773
        ldx     $DD
        lda     DRVNFO,x
        sta     LE772
        lda     $E4
        sta     $0283
        lda     $E3
        lsr     $0283
        ror     a
        lsr     $0283
        ror     a
        lsr     $0283
        ror     a
        bit     LE772
        bpl     LF003
        ldx     #$0F
        stx     LE773
        lsr     $0283
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
LF028:  sta     $0283
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
        adc     $0283
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
        inc     LE76F
LF077:  lda     LE76D
        jsr     LED0B
        lda     LE76E
        rts

LF081:  jsr     LEE70
LF084:  bit     $DC
        bvc     LF094
LF088:  jsr     LEFD0
        jsr     LEDA6
        lda     $DC
        and     #$BF
        sta     $DC
LF094:  rts

        lda     #$FF
        sta     $D1
        jmp     LF0A4

        lda     #$00
        sta     $CF
        sta     $D0
        sta     $D1
LF0A4:  jsr     LEFB9
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

        jsr     LEFB9
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

LF0FC:  jsr     LEFB9
        bpl     LF104
        jmp     LF1AB

LF104:  jsr     LF221
        lda     $E5
        sta     LF146
        sta     LF138
        sta     LF186
        sta     LF132
        lda     $E6
        sta     LF147
        sta     LF139
        sta     LF187
        sta     LF133
LF123:  lda     LE6D3
        sta     BNKCTL
        lda     $D3
        beq     LF17D
        ldy     $E2
        bne     LF141
LF131:  .byte   $B9
LF132:  brk
LF133:  cpx     #$91
        .byte   $C3
        iny
        .byte   $B9
LF138:  brk
LF139:  cpx     #$91
        .byte   $C3
        iny
        bne     LF131
        beq     LF158
LF141:  ldx     $E2
        ldy     #$00
LF145:  .byte   $BD
LF146:  brk
LF147:  cpx     #$91
        .byte   $C3
        iny
        inx
        bne     LF145
        tya
        beq     LF158
        clc
        adc     $C3
        sta     $C3
        bcc     LF15A
LF158:  inc     $C4
LF15A:  lda     $E2
        beq     LF165
        clc
        adc     L00D2
        sta     L00D2
        bcs     LF167
LF165:  dec     $D3
LF167:  inc     $E3
        bne     LF16D
        inc     $E4
LF16D:  lda     DEFBNK
        sta     BNKCTL
        ldx     #$00
        stx     $E2
        jsr     LEDB2
        jmp     LF123

LF17D:  lda     L00D2
        beq     LF19F
        ldy     $E2
        ldx     #$00
LF185:  .byte   $B9
LF186:  brk
LF187:  cpx     #$81
        .byte   $C3
        inc     $C3
        bne     LF190
        inc     $C4
LF190:  dec     L00D2
        beq     LF19A
        iny
        bne     LF185
        jmp     LF167

LF19A:  iny
        beq     LF167
        sty     $E2
LF19F:  lda     DEFBNK
        sta     BNKCTL
        jsr     LEEF4
        jmp     LF1EB

LF1AB:  and     #$7F
        tax
        lda     DDTI,x
        sta     LE7CF
        lda     DDTI+1,x
        sta     LE7D0
        jsr     LF24F
        jsr     LF246
        lda     #$00
        sec
        sbc     $C5
        sta     $C5
        lda     #$00
        sbc     $C6
        sta     $C6
        ora     $C5
        beq     LF1EB
LF1D1:  jsr     LF1F7
        bcs     LF1DE
        inc     $C5
        bne     LF1D1
        inc     $C6
        bne     LF1D1
LF1DE:  lda     L00D2
        clc
        adc     $C5
        sta     $C5
        lda     $D3
        adc     $C6
        sta     $C6
LF1EB:  ldx     LE6D9
        clc
        lda     $C5
        ora     $C6
        bne     LF1F6
        sec
LF1F6:  rts

LF1F7:  lda     DEFBNK
        sta     BNKCTL
        jsr     LF219
        ldx     LE6D3
        stx     BNKCTL
        nop
        nop
        nop
        bcs     LF217
        clc
        ldx     #$00
        sta     ($C3,x)
        inc     $C3
        bne     LF216
        inc     $C4
LF216:  rts

LF217:  sec
        rts

LF219:  jmp     (LE7CF)

NULDRV:     lda     LE78B
NULDRVO:    sec
            rts

LF221:  jsr     LF24F
        jsr     LF081
        jsr     LEF12
        bcs     LF22F
        jsr     ERROR46
LF22F:  sec
        lda     $CF
        sbc     $C5
        lda     $D0
        sbc     $C6
        lda     $D1
        sbc     #$00
        bcs     LF246
        lda     $CF
        sta     $C5
        lda     $D0
        sta     $C6
LF246:  lda     $C5
        sta     L00D2
        lda     $C6
        sta     $D3
        rts

LF24F:  lda     LE6D2
        bne     LF26C
        lda     $C4
        bne     LF260
        lda     $C3
        cmp     #$B0
        bcc     LF26C
        bcs     LF264
LF260:  cmp     #$02
        bcs     LF26C
LF264:  bit     LE777
        bmi     LF26C
        jsr     ERROR17
LF26C:  lda     $C5
        clc
        adc     $C3
        lda     $C6
        adc     $C4
        bcc     LF27A
        jsr     ERROR16
LF27A:  cmp     #$E0
        bcc     LF28B
        lda     LE6D2
        bne     LF28B
        bit     LE777
        bmi     LF28B
        jsr     ERROR17
LF28B:  nop
LF28C:  lda     LE6D2
        and     #$03
        eor     DEFBNK
        sta     LE6D3
        rts

LF298:  jsr     LEFB9
        bpl     LF2A0
        jmp     LF385

LF2A0:  jsr     LF28C
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
LF2C2:  lda     LE6D3
        sta     BNKCTL
        lda     $C6
        bne     LF2CF
        jmp     LF336

LF2CF:  ldy     $E2
        bne     LF2E3
LF2D3:  lda     ($C3),y
        .byte   $99
LF2D6:  brk
LF2D7:  cpx     #$C8
        lda     ($C3),y
        .byte   $99
LF2DC:  brk
LF2DD:  cpx     #$C8
        bne     LF2D3
        beq     LF2F8
LF2E3:  ldx     $E2
        ldy     #$00
LF2E7:  lda     ($C3),y
        .byte   $9D
LF2EA:  brk
LF2EB:  cpx     #$C8
        inx
        bne     LF2E7
        tya
        clc
        adc     $C3
        sta     $C3
        bcc     LF2FA
LF2F8:  inc     $C4
LF2FA:  lda     $E2
        beq     LF305
        clc
        adc     $C5
        sta     $C5
        bcs     LF307
LF305:  dec     $C6
LF307:  lda     DEFBNK
        sta     BNKCTL
        jsr     LF088
        inc     $E3
        bne     LF316
        inc     $E4
LF316:  ldy     #$00
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

LF336:  lda     $C5
        beq     LF35E
        ldy     $E2
        ldx     #$00
LF33E:  lda     ($C3,x)
        .byte   $99
LF341:  brk
LF342:  cpx     #$E6
        .byte   $C3
        bne     LF349
        inc     $C4
LF349:  dec     $C5
        beq     LF353
        iny
        bne     LF33E
        jmp     LF307

LF353:  iny
        beq     LF307
        sty     $E2
        lda     $DC
        ora     #$40
        sta     $DC
LF35E:  lda     DEFBNK
        sta     BNKCTL
        jsr     LEF05
        php
        bcc     LF36D
        jsr     LEF26
LF36D:  jsr     LEEF4
        ldx     LE6D9
        plp
        rts

LF375:  jsr     LEFD0
        lda     $C6
        bne     LF382
        lda     LE76E
        jsr     LEDB5
LF382:  jmp     LF2C2

LF385:  and     #$7F
        tax
        lda     DDTO,x
        sta     LE7CF
        lda     DDTO+1,x
        sta     LE7D0
        jsr     LF28C
        lda     #$00
        sec
        sbc     $C5
        sta     $C5
        lda     #$00
        sbc     $C6
        sta     $C6
        ora     $C5
        beq     LF3B3
LF3A8:  jsr     LF3B8
        inc     $C5
        bne     LF3A8
        inc     $C6
        bne     LF3A8
LF3B3:  sec
        ldx     LE6D9
        rts

LF3B8:  ldx     DEFBNK
        stx     BNKCTL
        ldx     #$00
        lda     ($C3,x)
        jsr     LF219
        lda     LE6D3
        sta     BNKCTL
        inc     $C3
        bne     LF3D1
        inc     $C4
LF3D1:  rts

LF3D2:  ldy     #$FF
        lda     ($E9),y
        tay
LF3D7:  iny
        lda     ($E9),y
        bne     LF3D7
        cpy     #$F9
        bcc     LF3FE
        ldy     #$00
LF3E2:  iny
        lda     ($E9),y
        bne     LF3E2
        cpy     #$F9
        bcc     LF3FE
        lda     $E3
        bne     LF3F1
        dec     $E4
LF3F1:  dec     $E3
        dec     $E2
        jsr     LEF26
        jsr     LEEF4
        jsr     ERROR38
LF3FE:  lda     #$FC
        jsr     LEE85
        tya
        ldy     #$FF
        sta     ($E9),y
        rts

LF409:  ldx     #$00
        stx     LE6DC
        jsr     LEF9F
        lda     LE754,x
        bpl     LF41A
        txa
        jsr     LF470
LF41A:  ldx     LE6DC
        jsr     LECC4
        ldx     LE6DC
        lda     #$80
        sta     LE754,x
        lda     #$00
        jmp     LEE9B

        jsr     LEF9F
        lda     LE754,x
        bpl     LF45E
        stx     LE6DC
        ldx     #$09
LF43A:  jsr     LEFA8
        beq     LF453
        bmi     LF453
        tax
        lda     LE65D,x
        cmp     LE6DC
        bne     LF450
        ldx     LE6D9
        jsr     LF5C5
LF450:  ldx     LE6D9
LF453:  dex
        bpl     LF43A
        ldx     LE6DC
        lda     #$00
        sta     LE754,x
LF45E:  rts

LF45F:  bit     HSRCW
        bmi     LF46F
        jsr     LECBB
        bcc     LF46F
        jsr     LF470
        jmp     LF45F

LF46F:  rts

LF470:  sty     $02A2
        and     #$03
        sta     LE76B
        ldy     #$09
LF47A:  ldx     LE652,y
        bmi     LF491
        beq     LF491
        lda     LE65D,x
        cmp     LE76B
        bne     LF491
        lda     #$00
        sta     LE65C,x
        sta     LE652,y
LF491:  dey
        bpl     LF47A
        ldx     LE76B
        lda     #$00
        sta     LE754,x
        ldy     $02A2
        rts

        ldx     #$00
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
        ldx     LE6DC
        jsr     GETDRVST
        bit     DSKSTAT
        bvc     LF4CA
        jsr     ERROR21
LF4CA:  ldy     #$FD
        lda     ($E9),y
        cmp     #$F8
        bcc     LF4D5
        jsr     ERROR39
LF4D5:  jsr     LF3D2
        sta     LE6EC
        jsr     LF6A5
        beq     LF4E3
        jsr     ERROR45
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
        sta     $C5
        lda     #$00
        sta     $C6
        lda     #$DD
        sta     $C3
        lda     #$E6
        sta     $C4
        ldx     LE6D9
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
        ldx     LE6D9
        lda     LE6DA
        sta     LE652,x
        rts

LF574:  ldy     #$08
LF576:  lda     DNT,y
        cmp     LE6DC
        beq     LF584
        dey
        bpl     LF576
        jsr     ERROR11
LF584:  tya
        asl     a
        ora     #$80
        ldx     LE6D9
        sta     LE652,x
        sta     LE6DA
        rts

LF592:  ldx     #$00
LF594:  jsr     LF5AD
        jsr     LEF90
        jsr     LF77A
        beq     LF5AA
        bit     LE77E
        bpl     LF5A7
        jsr     ERROR01         ; Command not found. 
LF5A7:  jsr     ERROR02         ; File not found.
LF5AA:  jmp     LF531

LF5AD:  lda     LE6DC
        sta     $0292
        jsr     LF5C5
        lda     #$00
        sta     LE786
        lda     $0292
        tax
        sta     LE6DC
        rts

LF5C3:  ldx     #$00
LF5C5:  jsr     LEFA8
        beq     LF62C
        ldx     #$09
LF5CC:  lda     LE652,x
        cmp     LE6DA
        bne     LF5D9
        cpx     LE6D9
        bne     LF624
LF5D9:  dex
        bpl     LF5CC
        ldx     LE6DA
        bmi     LF624
        jsr     LF081
        jsr     LF81C
        jsr     LEDB2
        lda     $E0
        ora     $E1
        bne     LF5F9
        lda     $DF
        cmp     #$41
        bcs     LF5F9
        jmp     LF639

LF5F9:  ldy     #$13
        ldx     #$02
LF5FD:  lda     $DF,x
        cmp     ($E5),y
        beq     LF60B
        sta     ($E5),y
        lda     $DC
        ora     #$40
        sta     $DC
LF60B:  dey
        dex
        bpl     LF5FD
        jsr     LF084
        ldx     $DD
        lda     LE758,x
        bpl     LF61C
        jsr     LEEA5
LF61C:  lda     #$00
        ldx     LE6DA
        sta     LE65C,x
LF624:  ldx     LE6D9
        lda     #$00
        sta     LE652,x
LF62C:  rts

        jsr     LEF90
        stx     LE6DC
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
        jsr     ERROR50
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
        ldx     LE6DA
        sta     LE65C,x
        ldx     #$09
LF680:  lda     LE652,x
        cmp     LE6DA
        bne     LF68D
        lda     #$00
        sta     LE652,x
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
        sta     LE6DA
        ldx     LE797
LF6AD:  txa
        sec
        sbc     #$0D
        bmi     LF6D8
        tax
        lda     LE65C,x
        beq     LF6D2
        lda     LE65D,x
        cmp     LE6DC
        bne     LF6AD
        lda     LE65E,x
        cmp     LE6EC
        bne     LF6AD
        stx     LE6DA
        jsr     LEEE3
        lda     $DC
        rts

LF6D2:  stx     LE6DA
        jmp     LF6AD

LF6D8:  ldx     LE6DA
        bne     LF6E0
        jsr     ERROR29
LF6E0:  jsr     LEEE3
        lda     LE6DC
        sta     $DD
        lda     LE6EC
        sta     $DE
        lda     #$00
        sta     $DC
        rts

        lda     ($C7),y
        jsr     LF882
        bcs     LF712
        tax
        iny
        lda     ($C7),y
        jsr     LF892
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
        sta     LE6DC
        lda     #$00
        sta     LE787
        jsr     LF829
        bcs     LF766
        lda     ($C7),y
        cmp     LE790
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
        sta     LE6DC
        iny
LF73E:  jsr     LFDEF
        lda     LE754,x
        bpl     LF76A
        sty     $02A1
        jsr     GETDRVST
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
        ora     LE6DC
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

        ldy     #$00
LF829:  ldx     #$00
LF82B:  lda     ($C7),y
        jsr     LF892
        bcs     LF83D
        sta     FNAMBUF,x
        iny
        inx
        cpx     #$0D
        bcc     LF82B
        bcs     LF87C
LF83D:  cmp     #$2E
        bne     LF84A
        sta     FNAMBUF,x
        iny
        lda     ($C7),y
        iny
        bne     LF852
LF84A:  lda     #$2E
        sta     FNAMBUF,x
        lda     DEFAULTEXT
LF852:  sta     FNAMBUF+1,x
        lda     FNAMBUF
        jsr     LF882
        bcs     LF87C
        ldx     #$01
LF85F:  lda     FNAMBUF,x
        cmp     #$2E
        beq     LF872
        jsr     LF892
        bcs     LF87C
        inx
        cpx     #$0D
        bcc     LF85F
        bcs     LF87C
LF872:  cpx     #$01
        beq     LF87C
        lda     FNAMBUF+1,x
        jsr     LF87D
LF87C:  rts

LF87D:  jsr     LF88B
        bcc     LF88A
LF882:  cmp     #$41
        bcs     LF888
LF886:  sec
        rts

LF888:  cmp     #$5B
LF88A:  rts

LF88B:  cmp     #$30
        bcc     LF886
        cmp     #$3A
        rts

LF892:  cmp     LE78C
        bne     LF87D
        clc
        rts

        ldx     #$00

        ; Converts word at $C1-$C2,x into its 4-char ascii hex representation
        ; at  ($CD),y
HEXWORD: 
        lda     $C2,x           ; Gets most significant byte
        jsr     HEXBYTE           ; Converts it
        lda     $C1,x           ; Gets less significant byte
HEXBYTE: 
        pha                     ; Save byte
        lsr     a               ; Get upper nibble
        lsr     a               ;
        lsr     a               ;
        lsr     a               ;
        jsr     HEXNIBBLE           ; Convert it
        pla                     ; Recover byte
HEXNIBBLE: 
        and     #$0F            ; and get lower nibble
        clc
        adc     #$30            ; Adds "0"
        cmp     #$3A            ; Is it "9" or lower?
        bmi     LF8B6           ; Yes, goto store it
        adc     #$06            ; Nope, add 7 (6 + carry) to get hex digit
LF8B6:  sta     ($CD),y         ; And store it
        iny                     ; Next position
        rts                     ; and return

LF8BA:  lda     #$0A
        jsr     LF915
LF8BF:  sec
        sbc     #$30
        bcc     LF8EB
        cmp     $D8
        bcs     LF8EB
        jsr     LF8F1
        jmp     LF8BF

LF8CE:  lda     #$10
        jsr     LF915
LF8D3:  sec
        sbc     #$30
        bcc     LF8EB
        cmp     #$0A
        bcc     LF8E6
        sbc     #$07
        cmp     #$0A
        bcc     LF8EB
        cmp     #$10
        bcs     LF8EB
LF8E6:  jsr     LF8F1
        bne     LF8D3
LF8EB:  rol     $029F
        jmp     LF930

LF8F1:  pha
        stx     $0286
        ldx     #$17
        jsr     LFB29
        pla
        clc
        adc     $C1
        sta     $C1
        lda     $C2
        adc     #$00
        sta     $C2
        bcc     LF90B
        jsr     ERROR19
LF90B:  dec     $029F
        ldx     $0286
        iny
        lda     ($CB),y
        rts

LF915:  sta     $D8
        lda     #$00
        sta     $D9
        sta     $C1
        sta     $C2
        sta     $029F
        beq     LF925
LF924:  iny
LF925:  jsr     LF930
        beq     LF92E
        cmp     #$20
        beq     LF924
LF92E:  rts

        iny
LF930:  lda     ($CB),y
        beq     LF947
        bcs     LF93F
        cmp     #$0D
        beq     LF93D
        cmp     LE78D
LF93D:  clc
        rts

LF93F:  cmp     #$0D
        beq     LF946
        cmp     LE78D
LF946:  sec
LF947:  rts

LF948:  jsr     LFD76           ; Set output line buffer as destination
LF94B:  jsr     PRNSTR
        .byte   "P=", $0
        ldx     #$19
        jsr     HEXWORD
        lda     #':'
        sta     ($CD),y
        iny
        lda     PRGBANK           ; Load Current Program Bank
        clc
        adc     #$30            ; Convert to ASCII
        sta     ($CD),y
        iny
        lda     #'/'
        sta     ($CD),y
        iny
        lda     DATBANK           ; Load Current Data Bank
        clc
        adc     #$30            ; Convert to ASCII
        sta     ($CD),y
        iny
        lda     #' '
        sta     ($CD),y
        iny
        lda     #'('
        sta     ($CD),y
        iny
        jsr     LF9DB
        jsr     LF9BA
        jsr     HEXBYTE
        dey
        jsr     LF9BA
        iny
        jsr     HEXBYTE
LF98D:  dey
        dey
        jsr     LF9BA
        iny
        iny
        jsr     HEXBYTE
        lda     #$29
        sta     ($CD),y
        iny
        ldx     #$04
LF99E:  lda     #$20
        sta     ($CD),y
        iny
        lda     LF9CC,x
        sta     ($CD),y
        iny
        lda     #$3D
        sta     ($CD),y
        iny
        lda     STACKP,x
        jsr     HEXBYTE
        dex
        bpl     LF99E
        jmp     LF9DB

LF9BA:  lda     PRGBANK
        eor     DEFBNK
        sta     BNKCTL
        lda     ($DA),y
        ldx     DEFBNK
        stx     BNKCTL
        rts

LF9CC:  .byte   "SFYXA"

LF9D1:  ldx     #$02
        jsr     LF9DD
LF9D6:  lda     #$0D
        jmp     PRNCHAR

LF9DB:  ldx     #$02
LF9DD:  sty     $C5
        lda     #$00
        sta     $C6
        lda     $CD
        sta     $C3
        lda     $CE
        sta     $C4
        jsr     LF298
        ldy     #$00
        rts

LF9F1:  ldy     #$00
        lda     LE652,x
        cmp     #$82
        bne     LFA05
        lda     $CB
        sta     $F0
        lda     $CC
        sta     $F1
        jmp     JINLINE

LFA05:  jsr     LFA22
        bcs     LFA17
        cmp     #$0D
        beq     LFA16
        sta     ($CB),y
        iny
        cpy     YLNLIM
        bcc     LFA05
LFA16:  clc
LFA17:  lda     #$0D
        sta     ($CB),y
        tya
        beq     LFA1F
        clc
LFA1F:  ldy     #$00
        rts

LFA22:  lda     LE652,x
        cmp     #$82
        bne     LFA33
        jsr     LFA4D
        cmp     LE78B
        beq     LFA32
        clc
LFA32:  rts

LFA33:  jsr     LFAA4
        sec
        ror     LE777
        jsr     LF0FC
        php
        clc
        rol     LE777

LFA42:  ldy     $028B
        ldx     $028A
        lda     LE7D1
        plp
        rts

LFA4D:  jmp     (CINP)              ; Jump to console input routine


; Print string immediately following the JSR call
;
PRNSTR: stx     $0287               ; Save X
        ldx     #$02                ; X = 2
        bne     LFA5A               ; Always jump
        stx     $0287               ; Dead code?
LFA5A:  sta     $0288               ; Save A
        sty     $0289               ; Save Y
        pla                         ; Get PC and save in $D8-$D9. PC points
        sta     $D8                 ; to last opcode of instruction
        pla                         ;
        sta     $D9                 ;
LFA66:  inc     $D8                 ; Increment PC
        bne     LFA6C
        inc     $D9
LFA6C:  ldy     #$00
        lda     ($D8),y             ; Get char
        beq     LFA78               ; If null, end of string
        jsr     PRNCHAR               ; Print char
        jmp     LFA66               ; Loop

LFA78:  lda     $D9                 ; Push new PC to the stack
        pha                         ;
        lda     $D8                 ;
        pha                         ;
        ldy     $0289               ; Restore indexes
        lda     $0288               ;
        ldx     $0287               ;
        rts

; Print character in A
; PRNCHAR
PRNCHAR: 
        sta     LE7D1
        lda     LE652,x
        cmp     #$82
        bne     LFA9A
        lda     LE7D1
        jsr     LFABE
        sec
        rts

LFA9A:  jsr     LFAA7
        jsr     LF298
        php
        jmp     LFA42

LFAA4:  sta     LE7D1               ; Save char
LFAA7:  stx     $028A               ; Save X
        sty     $028B               ; Save Y
        lda     #$01
        sta     $C5
        lda     #$00
        sta     $C6
        lda     #$D1
        sta     $C3
        lda     #$E7
        sta     $C4
        rts

LFABE:  jmp     (COUTP)             ; Jump to console output routine

        stx     $028C
        lda     #$00
        sta     $029F
        ldx     #$06
LFACB:  lda     LFB05,x
        sta     $D8
        lda     LFB06,x
        sta     $D9
        stx     $028D
        ldx     #$17
        jsr     LFB70
        lda     $C1
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
        lda     $C1
LFAFE:  clc
        adc     #$30
        sta     ($CD),y
        iny
        rts

LFB05:  .byte   $0A
LFB06:  .byte   $00
        .byte   $64
        .byte   $00
        .byte   $E8
        .byte   $03
        bpl     LFB34       ; $10 $2F      ??

LFB0D:  clc
        lda     $C1,x
        adc     $C1
        sta     $C1
        lda     $C2,x
        adc     $C2
        sta     $C2
        rts

LFB1B:  sec
        lda     $C1
        sbc     $C1,x
        sta     $C1
        lda     $C2
        sbc     $C2,x
        sta     $C2
        rts

LFB29:  jsr     LFB35
        lda     $D3
        ora     L00D2
        bne     LFB6D
        lda     $C2
LFB34:  rts

LFB35:  stx     $029C
        lda     #$00
        sta     L00D2
        sta     $D3
        lda     $C1,x
        sta     $D4
        lda     $C2,x
        sta     $D5
        ldx     #$11
        clc
LFB49:  ror     $D3
        ror     L00D2
        ror     $C2
        ror     $C1
        dex
        beq     LFB65
        bcc     LFB49
        lda     L00D2
        clc
        adc     $D4
        sta     L00D2
        lda     $D3
        adc     $D5
        sta     $D3
        bcc     LFB49
LFB65:  ldx     $029C
        rts

LFB69:  lda     $C2,x
        bne     LFB7B
LFB6D:  jsr     ERROR19
LFB70:  sty     $028E
        lda     $C1,x
        sta     $D4
        beq     LFB69
        lda     $C2,x
LFB7B:  sta     $D5
        lda     #$00
        sta     $C1,x
        sta     $C2,x
        ldy     #$11
        clc
        bcc     LFB9F
LFB88:  rol     $C1,x
        rol     $C2,x
        lda     $C1,x
        sec
        sbc     $D4
        sta     L00D2
        lda     $C2,x
        sbc     $D5
        bcc     LFB9F
        sta     $C2,x
        lda     L00D2
        sta     $C1,x
LFB9F:  rol     $C1
        rol     $C2
        dey
        bne     LFB88
        ldy     $028E
        rts

LFBAA:  lda     $C1,x
        sta     $C1
        lda     $C2,x
        sta     $C2
        rts

LFBB3:  lda     $C1
        sta     $C1,x
        lda     $C2
        sta     $C2,x
        rts

LFBBC:  lda     $C2,x
        pha
        lda     $C1,x
        pha
        jsr     LFBB3
        pla
        sta     $C1
        pla
        sta     $C2
        rts

        stx     $0297
        ldx     #$15
        jsr     LFBE1
        lda     $D7
        beq     LFBDB
        jsr     ERROR18
LFBDB:  lda     $D6
        ldx     $0297
        rts

LFBE1:  lda     #$00
        sta     $C1,x
        sta     $C2,x
        stx     $028F
        tax
        jsr     LF925
        cmp     #$2D
        bne     LFBF4
        inx
LFBF3:  iny
LFBF4:  stx     $0291
        ldx     $028F
        jsr     LF925
        cmp     LE78E
        beq     LFC59
        cmp     LE78F
        bne     LFC0A
        jsr     LF924
LFC0A:  jsr     LF8CE
        bcc     LFC61
LFC0F:  jsr     LFBBC
        lda     $0291
        bne     LFC32
        jsr     LFB0D
LFC1A:  jsr     LFBBC
LFC1D:  jsr     LF925
        ldx     #$04
LFC22:  cmp     LE79B,x
        beq     LFBF3
        dex
        bpl     LFC22
        ldx     $028F
        jsr     LF930
        sec
        rts

LFC32:  dec     $0291
        bne     LFC3D
        jsr     LFB1B
        jmp     LFC1A

LFC3D:  dec     $0291
        bne     LFC48
        jsr     LFB29
        jmp     LFC1A

LFC48:  dec     $0291
        bne     LFC53
        jsr     LFB70
        jmp     LFC1A

LFC53:  jsr     LFB70
        jmp     LFC1D

LFC59:  jsr     LF924
        jsr     LF8BA
        bcs     LFC0F
LFC61:  rts

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
        lda     $C5
        sec
        sbc     $C3
        sta     $C5
        lda     $C6
        sbc     $C4
        sta     $C6
        bcs     LFC8F
        jsr     ERROR16
LFC8F:  inc     $C5
        bne     LFC95
        inc     $C6
LFC95:  bit     LE77B
        bpl     LFC9D
        jsr     LFCF5
LFC9D:  ldx     #$02
        bit     LE77C
        bpl     LFCA6
        ldx     #$06
LFCA6:  jsr     LFBAA
        ldx     #$05
LFCAB:  lda     $C1,x
        sta     LE721,x
        dex
        bpl     LFCAB
        jsr     LFCD9
        ldx     LE6D9
        jsr     LF298
        jsr     LFCEA
        bit     LE77B
        bpl     LFCC7
        jsr     LFCF5
LFCC7:  ldx     LE6D9
        lda     LE6D6
        sta     LE6D2
        jsr     LF298
        lda     #$00
        sta     LE6D2
        rts

LFCD9:  lda     #$0A
        sta     $C5
        lda     #$00
        sta     $C6
        lda     #<SAVEDMAGIC
        sta     $C3
        lda     #>SAVEDMAGIC
        sta     $C4
        rts

LFCEA:  ldx     #$05
LFCEC:  lda     LE721,x
        sta     $C1,x
        dex
        bpl     LFCEC
        rts

LFCF5:  ldx     #$01
LFCF7:  lda     $C9,x
        pha
        lda     $C3,x
        sta     $C9,x
        pla
        sta     $C3,x
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
        sta     LE6D2
        bit     LE77B
        bpl     LFD2C
        jsr     LFCF5
        lda     LE6D7
        sta     LE6D2
LFD2C:  ldx     LE6D9
        jsr     LF0FC
        rts

LFD33:  sec
        rts

LFD35:  jsr     LFCD9
        lda     #$00
        sta     LE6D2
        sec
        ror     LE777
        jsr     LF0FC
        bcs     LFD51
        rol     LE777
        lda     SAVEDMAGIC          ; Check that the magic number is correct
        cmp     #$58                ;
        beq     LFD52               ; Yes, jump to return OK
        sec                         ; Return error
LFD51:  rts

LFD52:  clc                         ; Return OK
        rts

LFD54:  lda     #$00
        ldx     LE653
        sta     LE653
        jsr     LFD9B
        lda     LE653
        bne     LFD69
        lda     #$82
        sta     LE653
LFD69:  ldy     #$00
        
        lda     LE7BE
        sta     $CB
        lda     LE7BE+1
        sta     $CC
        rts

LFD76:  lda     LE654
        bne     LFD80
        lda     #$82
        sta     LE654
LFD80:  ldy     #$00
        lda     LE7C0
        sta     $CD
        lda     LE7C0+1
        sta     $CE
        rts

LFD8D:  lda     #$00
        ldx     LE654
        sta     LE654
        jsr     LFD9B
        jmp     LFD76

LFD9B:  bmi     LFDB7
        beq     LFDB7
        stx     LE6DA
        ldx     #$09
LFDA4:  lda     LE652,x
        cmp     LE6DA
        beq     LFDB7
        dex
        bpl     LFDA4
        ldx     LE6DA
        lda     #$00
        sta     LE65C,x
LFDB7:  rts

; Console Input Routine
;
CIN:    jsr     JGETKEY
        cmp     LE789
        beq     LFDDC
        cmp     LE78B
        beq     LFDCE
        bit     KBDECHO
        bpl     LFDCD
        jsr     JOUTCH
LFDCD:  clc
LFDCE:  rts

; Console Output Routine
;
COUT:   sta     $0299
        jsr     JTSTKEY
        bcc     LFDE9
LFDD7:  cmp     LE789
        bne     LFDDF
LFDDC:  jmp     CNTRLC

LFDDF:  cmp     LE78A
        bne     LFDE9
        jsr     JGETKEY
        bpl     LFDD7
LFDE9:  lda     $0299
        jmp     JOUTCH

LFDEF:  jsr     LF45F
        ldx     LE6DC
        rts

.if  CODOS2_VER = 17
GETDRVSTKLUDGE:
        jsr     GETDRVST
@LOOP:  jsr     LF766
        inx
        bne     @LOOP
        rts
.endif
        ; ORIGIN OF CODOS OVERLAYS

        .segment "overlays"
OVLORG:
        .end
