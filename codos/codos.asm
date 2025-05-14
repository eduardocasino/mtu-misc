; Disassembly of CODOS 2.0 by Eduardo Casino (mail@eduardocasino.es)
;

; da65 V2.18 - Ubuntu 2.19-1
; Created:    2023-11-25 21:28:31
; Input file: codos.bin
; Page:       1

                    .include "codos.inc"

    BANKSW          := $0100

                    .export YLNLIM
    
    YLNLIM          := $0238        ; Line size limit for INLINE and EDLINE entry points.
    SEEIO           := $02F9        ; I-O space enable semaphore
    NMIPRC          := $02FA        ; Jump to NMI processor
    IRQBRK          := $02FD        ; Jump to IQR and BRK processor
    WARMRS          := $0300        ; Jump to operating system warm reset entry
    CNTRLC          := $0303        ; Jump executed when CNTRL-C is entered from console.

    ; RELEVANT KEYBOARD AND TEXT DISPLAY DRIVER ENTRY POINTS
    ;
    GETKEY          := $0306        ; Wait until a keyboard key is struck and return character in A
    OUTCH           := $0309        ; Display printable character or interpret control character.
    TSTKEY          := $030C        ; Test if a key is pressed
    INITIO          := $030F        ; Clear screen and set default values of display parameters.
    INLINE          := $031E        ; Input an entire line from the keyboard
    

                    .export SVIA1PORT, SVIA1DIR, BNKCTL

    SVIA1PORT       := $BFE0        ; System 1 6522 System control port data register
    SVIA1DIR        := $BFE2        ; System 1 6522 System control port direction register
    BNKCTL          := SVIA1PORT    ; System 1 6522 (Bank control data register)

    IOENABLE        := $FFFE        ; Enable I/O space from $BE00 to $BFFF
    IODISABLE       := $FFFF        ; Disable I/O space (enable RAM) from $BE00 to $BFFF

            ;   Disk Controller Registers
            ;

            .export HSRCW

    HSRCW           := SYSRAM+$1FE8 ; Read  - Hardware Status Read
                                    ; Write - Hardware Control Write
    ADMA            := SYSRAM+$1FEA ; Write - Set DMA Address Register

            ;   uPD765 Registers
            ;
    MSTR            := SYSRAM+$1FEE ; Read  - uPD765 Main Status Register
    DATR            := SYSRAM+$1FEF ; R/W   - uPD765 Data Register

             ; uPD765 command index
             ;
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

            .export U0, U1, U2, U3, U4, U5, U6, U7

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
            ;
            ; The first positions hold a copy of pseudo registers when an SVC
            ; is entered

            .export P0SCRATCH, MEMBUFF, MEMCOUNT, TMPBUFP, INPBUFP, OUTBUFP
            .export DESTBUFF, BYTRES, PCSAVE, FILEPOS

P0SCRATCH:  .res 2                  ; $C1-$C2 (word)
MEMBUFF:    .res 2                  ; $C3-$C4 (word) Pointer to buffer for memory copy operations
MEMCOUNT:   .res 2                  ; $C5-$C6 (word) Counter for memory copy operations
TMPBUFP:    .res 2                  ; $C7-$C8 (word) Used in FNAMFROMBUF and as <entry> buffer
DESTBUFF:   .res 2                  ; $C9-$CA (word) <dest> buffer when specified in command
INPBUFP:    .res 2                  ; $CB-$CC (word) Pointer to input buffer
OUTBUFP:    .res 2                  ; $CD-$CE (word) Pointer to output buffer
FILEPOS:    .res 3                  ; $CF-$D1 (24 bit) File position. Also used to
                                    ;    calculate remaining file size from file pos

L00D2:      .res 1                  ; $D2  $D2-$D9 is a temporary area
            .res 1                  ; $D3       with different uses, like
            .res 1                  ; $D4       the switch and jump routine
            .res 1                  ; $D5
BYTRES:     .res 2                  ; $D6-$D7 (word) Result for GETBYTE function
TMPPTR:     .res 2                  ; $D8-$D9 (word) Temporary pointer
PCSAVE:     .res 2                  ; $DA-$DB (word) Program counter

            .export CURFINFO, BATP

CURFINFO:   .res FINFOLEN           ; $DC-$E8

BATP:       .res 2                  ; $E9-$EA (word) Pointer to Block Allocation Table for current drive

            .export CMDLIDX

CMDLIDX:    .res 1                  ; $EB  (byte) Current char position in command line
INTSVA:     .res 1                  ; $EC  (byte) Accumulator save during SVC or IRQ processing.
    
            ; $ED - $EF : Global RAM used by CODOS
 
            .exportzp ERRNUM, SVCENB

ERRNUM:     .res 1                  ; $ED  Error number for user-defined error recovery.
SVCENB:     .res 1                  ; $EE  ADDRESS OF SVC ENABLE FLAG
SAVEACC:    .res 1                  ; $EF  TODO: Unknown

            ; $F0 - $FF : Scratch RAM for console I-0. 

            .exportzp QLN

QLN:        .res 2                  ; $F0 Ptr to line-buffer used for INLINE and EDLINE 
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

            .export NFILES, TEMP4, SAVEY7, SAVEAX, SAVEY8

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
TEMP4:      .res 1                  ; $0290 (byte) Used as temporary space by command processor
TEMP3:      .res 1                  ; $0291
SAVEA2:     .res 1                  ; $0292
SAVEY7:     .res 1                  ; $0293
SAVEA3:     .res 1                  ; $0294
SAVEX7:     .res 1                  ; $0295
SAVEAX:     .res 1                  ; $0296
SAVEX8:     .res 1                  ; $0297
SAVEY8:     .res 1                  ; $0298
SAVEA4:     .res 1                  ; $0299
SAVEX9:     .res 1                  ; $029A
NFILES:     .res 1                  ; $029B Used to navigate through disk file entries
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


    YOUT            := $D27D        ; "Y" output (console and printer) entry point
                                    ; Must be set by hand at STARTUP.J with
                                    ; SET D27D=20 21 E6 (jsr JCOUT)
    PRTOUT          := $D280        ; Printer output entry point

            .segment "codos"
            
            ; Jump table (page 179)
            ;

            .export JINLINE

            jmp     COLDST
JWARMST:    jmp     WARMST
JGETKEY:    jmp     GETKEY          ; Console character input
JOUTCH:     jmp     OUTCH           ; Console character output
JTSTKEY:    jmp     TSTKEY          ; Console Key-depressed test subroutine
            jmp     NMIPROC
            jmp     IRQPROC
CPSEUDREGSP:jmp     CPSEUDREGS      ; Jump to copy pseudo-registers to scratch RAM
JERROR37:   jmp     ERROR37         ; Jump to "Required software package not loaded" error message
JINLINE:    jmp     INLINE          ; Jump to input an entire line from the keyboard
            jmp     CIN
JCOUT:      jmp     COUT            ; Jump to console-character-out routine with CTRL-S/Q (XON/XOFF)
            jmp     JERROR37        ; Required software package not loaded in memory
            jmp     JERROR37        ; Required software package not loaded in memory

            ; Device name table

            .export DNT

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

            .export IOCHTBL, CHANN1

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


; Table of FINFO structures for active files. There is room for up to 8,
; but the default is six, defined by TOPASSIGTB (6 * FINFOLEN) + 1
;
            .export FINFOTBL

FINFOTBL:   .byte   $06, $00, $00, $00, $00, $00, $00, $00, $00, $00
            .word   $E200           ; File buffer
            .byte   $88             ; $E200 K-1013 DMA encoded

            .byte   $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
            .word   $E100           ; File buffer
            .byte   $84             ; $E100 K-1013 DMA encoded

            .byte   $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
            .word   $E000           ; File buffer           
            .byte   $80             ; $E000 K-1013 DMA encoded

            .byte   $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
            .word   $D700           ; File buffer
            .byte   $5C             ; $D700 K-1013 DMA encoded

            .byte   $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
            .word   $D600           ; File buffer
            .byte   $58             ; $D600 K-1013 DMA encoded

            .byte   $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
            .word   $D500           ; File buffer
            .byte   $54             ; $D500 K-1013 DMA encoded

            .byte   $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
            .word   $D400           ; File buffer
            .byte   $50             ; $D400 K-1013 DMA encoded

            .byte   $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
            .word   $D300           ; File buffer
            .byte   $4C             ; $D300 K-1013 DMA encoded

            .byte   $00             ; ?? END OF TABLE MARKER ???

; K-1013 DMA encoded addresses for drive BATs
;
BATDMAT:    .byte   $90             ; $E400
            .byte   $8C             ; $E300
            .byte   $88             ; $E200
            .byte   $84             ; $E100

            .export STACKP, PROCST, YREG, XREG, ACCUM

; Processor registers at interrupt
;
STACKP:     .byte   $FF             ; Stack pointer
PROCST:     .byte   $04             ; Processor Status
YREG:       .byte   $00             ; Y
XREG:       .byte   $00             ; X
ACCUM:      .byte   $00             ; Accumulator

            .export PRGBANK, DATBANK, DSTBANK

PRGBANK:    .BYTE   $00             ; Current program bank
DATBANK:    .BYTE   $00             ; Current data bank
BNKCFG:     .BYTE   $00             ; Current bank configuration
SVCSTAT:    .BYTE   $00             ; SVC status (enabled/disables) at interrupt?
DSTBANK:    .BYTE   $00             ; Destination bank num for memory copy operations?
DSTBNKCFG:  .byte   $7F             ; Destination bank config

            .export NEWBNK, CHGBNKFLG, SVDFROMBNK, SVDDESTBNK

NEWBNK:     .byte   $00             ; Where GADDRBNK stores bank
CHGBNKFLG:  .byte   $00             ; If set, switches to NEWBNK
SVDFROMBNK: .byte   $00             ; Bank for saved file <from> bank
SVDDESTBNK: .byte   $00             ; Bank for saved file <dest> bank

            .export DEFBNKCFG, DEVICE

DEFBNKCFG:  .byte   $7F             ; Default bank configuration
CHANNEL:    .byte   $00             ; Current channel for I/O operations
DEVICE:     .byte   $00             ; Current device/file for I/O operations

            .export DIRPOINT, CURRDRV

DIRPOINT:   .byte   $00             ; Pointer to current directory entry
CURRDRV:    .byte   $00             ; Current disk drive number

; The following 64 bytes comprise the file header that prepends every file
; in the CODOS file system
;
FILEHDR:
; Directory entry
;
            .export FNAMBUF, TDATE

DIRENT:     .byte   $01             ; Always $01
FNAMBUF:    .byte   "NONAME.Z  ", $00, $00, ".", $00
BATPTR:     .byte   $00             ; Pointer to first block in BAT


            .byte   $80             ; Flag: $80 : Normal read/write file
                                    ;       $C0 : Locked file
            .byte   $40, $00, $00   ; File size (24 bits) includes header size ($40)

DRCTRYPNT:  .word   $00             ; Relative pointer to the directory entry

TDATE:      .byte   "*UNDATED*", $00 ; Today's date

            ; Reserved for future upgrades:
            ;
            .byte   $00, $00, $00, $00, $00, $00, $00, $00
            .byte   $20, $20, $20, $20, $20, $20, $20, $20
            .byte   $20, $20, $20, $20, $20, $20, $20, $20
            .byte   $20, $20, $20, $20, $19, $98, $FF, $E5
;
; End of file header

; Saved (loadable) files header
;
            .export SAVEDHDR

SAVEDHDR:   .byte   $58             ; Magic number for loadable (SAVEd) files
            .byte   $00             ; Overlay number
            .byte   $00             ; Memory bank number
            .byte   $00             ; Reserved (always $00)
            .byte   $00             ; Entry address (two bytes)
            .byte   $00             ;
            .byte   $00             ; Load address (two bytes)
            .byte   $00             ;
            .byte   $00             ; Size (two bytes)
            .byte   $00             ;


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
CMDTBL:
            ; SPECIFY

            .byte   $03             ; Command length 3
            .byte   $03             ; Specify
.if CODOS2_VER = 14
            .byte   $AF             ; Stepping Rate Time $A (6ms), Head Unload Time $F (240ms)
            .byte   $30             ; Head Load Time $30 (48ms)
.else
            .byte   $DF             ; Stepping Rate Time $D (3ms), Head Unload Time $F (240ms)
            .byte   $26             ; Head Load Time $26 (38ms)
.endif
            ; RECALIBRATE

            .byte   $02             ; Command length 2
            .byte   $07             ; Recalibrate
RECDRVHD:   .byte   $00             ; Drive and head:
                                    ;   XXXXX 0 00
                                    ;         | ||
                                    ;         | ++-> Drive (0-3)
                                    ;         +----> Head
            ; SEEK

            .byte   $03             ; Command length 3
            .byte   $0F             ; Seek
SEKDRVHD:   .byte   $00             ; Drive and head (see above)
SEKTRACK:   .byte   $00             ; Track

            ; SENSE INTERRUPT

            .byte   $01             ; Command length 1
            .byte   $08             ; Sense interrupt status

            ; READ/WRITE

            .byte   $09             ; Command length 9
                                    ; Read command. Same sequence is used for write,
RDWRD:      .byte   $46             ; storing $45 at this location
                                    ; MFM, no MT, no skip
RWRDRVHD:   .byte   $00             ; Disk and Head info
RWRTRACK:   .byte   $00             ; C- Cylinder
RWRHEADN:   .byte   $00             ; H - Head
RWRSECTR:   .byte   $00             ; R - Sector
            .byte   $01             ; N - 256 bytes/sector
RWREOSEC:   .byte   $00             ; EOT sector
            .byte   $0E             ; GPL
            .byte   $FF             ; DTL (ignored as N != 0)

            ; FORMAT (UNUSED?)

            .byte   $06             ; Command length 6
            .byte   $4D             ; Format command (MFM)
            .byte   $00             ; HD = 0, Drive = 0
            .byte   $01             ; 256 bytes sectors
            .byte   $1A             ; 26 sectors/track
            .byte   $34             ; Gap 3
            .byte   $00             ; Filler byte

            ; SENSE DRIVE

            .byte   $02             ; Command length 2
            .byte   $04             ; Sense drive status command   
SENDRVHD:   .byte   $00

; Disk status registers
;
DSKSTAT:
ST0:        .byte   $00
ST1:        .byte   $00
            .byte   $00
            .byte   $00
            .byte   $00
            .byte   $00
            .byte   $00

            .export NDRIVES

NDRIVES:    .byte   $02             ; Number of disk drives in system, 1 to 4

DRVNFO:     .byte   $00             ; Drive info table (one byte per drive)
            .byte   $00             ;     0b10000000 : Two sides
            .byte   $00
            .byte   $00

            ; Open drives flag table

ODRIVES:    .byte   $00
            .byte   $00
            .byte   $00
            .byte   $00

            ; BAT changes flag table

BATCHG:     .byte   $00             ; Bit 7 = 1 if BAT has been modified
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

            .export SECTNUM

RETRIES:    .byte   $00             ; uPD765 read/write retries
N765ECNT:   .byte   $00             ; uPD765 error count
CMDIDX:     .byte   $01             ; uPD765 command index
SAVEDRV:    .byte   $FF             ; Used for temporary storage
CBLOCK:     .byte   $00             ; Block of current file pointer
CTRACK:     .byte   $00             ; Track of current file pointer
CSECT:      .byte   $00             ; Sector of current file pointer
CHEAD:      .byte   $00             ; Current head number
SKDRIVE:    .byte   $00             ; Current drive in SEEK operations
RWDRIVE:    .byte   $00             ; Current drive in READ(WRITE operations
DSFLAG:     .byte   $00             ; Dual side flag for block calculations
                                    ; If Bit 7 == 1, then dual side
SCTBLKM1:   .byte   $07             ; Sectors/block minus 1 for block calculations
BLKSCTOF:   .byte   $00             ; Sector offset in the block
SECTNUM:    .byte   $00             ; Sector number (used in disk access functions)
DMADIR:     .byte   $00             ; DMA Dir:
                                    ;   0 == Memory -> Controller (read)
                                    ;   1 == Controller -> Memory (write)

            .export IGNORWRP, UNPROTFLG, SAVEOVERWR, SAVDESTPF, SAVENTRYPF

IGNORWRP:   .byte   $00             ; Flag. If bit 7 = 1, ignore memory write protection 
UNPROTFLG:  .byte   $00             ; Flag. If bit 7 = 1, unprotect SYSRAM
IGNORERR:   .byte   $00             ; Flag. If bit 7 = 1 then system will ignore (continue after)
                                    ;   irrecoverable disk read errors (use a last resort only).
SAVEOVERWR: .byte   $00             ; Flag. If bit 7 = 1 then permits save command to overwrite an
                                    ; existing file with the same name.
SAVDESTPF:  .byte   $00             ; Flag. If bit 7 = 1, then dest addr and bank is set
                                    ;    if, not, use starting address for the block
                                    ;    of memory
SAVENTRYPF: .byte   $00             ; Flag. If bit 7 = 1, then an entry point addr is set
                                    ;    if, not, use starting address for the block
                                    ;    of memory

; The following six flags are set by the command processor or the SVC proc. and
; are used during error recovery to display the right messages

            .export INTCMDERR, ISCMDFLG, PRBPREGS, SVC13FLG, PERRPFLG

INTCMDERR:  .byte   $00             ; Flag. If bit 7 = 1, then error was produced
                                    ;   during an internal command processing.
ISCMDFLG:   .byte   $00             ; Flag. If bit 7 = 1 then file to load is an external
                                    ;   command.
PRBPREGS:   .byte   $00             ; Flag. If bit 7 = 1 then print registers at BP, not at error
SVC13FLG:   .byte   $00             ; Flag. If bit 7 = 1 then program executing was invoked by SVC #13.
NOPRREGS:   .byte   $00             ; Flag. If bit 7 = 1 then don't print regs on error

PERRPFLG:   .byte   $00             ; Flag. If bit 7 = 1, we are here as part of the
                                    ; print error process. Cleared by the command processor.

            .export ASSIGNFLAG

IRQFLAG:    .byte   $00             ; Flag. If bit 7 = 1, interrupt is IRQ (0 is BRK)
NMIFLAG:    .byte   $00             ; Flag. If bit 7 = 1, interrupt is NMI
DEFSVCFLAG: .byte   $00             ; Flag. If bit 7 = 1, SVC enabled by default
ASSIGNFLAG: .byte   $00             ; Flag. If bit 6 = 1, it is a new file
                                    ;       If bit 7 = 1, it is an existing file
                                    ;       Clear: it is a device      
            .export SCOLON, COLON, QUOTE

SCANFLG:    .byte   $00             ; Flag for SVC #29 (FSCAN)
                                    ; If the name was a device name (Cy set) then:
                                    ;   Bit 7 is set to 1 if device does not exist 
                                    ;   Bits 6-0 contain the ASCII device name (1 character)
                                    ; If the name was a file name (Cy clear) then:
                                    ;   Bit 7 is set to 1 if illegal name/drive number
                                    ;   Bit 6 is set to 1 if the drive is not open
                                    ;   Bit 5 is set to 1 if the file exists
                                    ;   Bit 4 is set to 1 if the drive is write-protected
                                    ;   Bits 3 and 2 are not used
                                    ;   Bits 1 and 0 contain the drive number selected
KBDECHO:    .byte   $00             ; Keyboard echo flag for CODOS. Set to $80 to enable echo.
ETX:        .byte   $03             ; ETX (CTRL-C)
XOFF:       .byte   $13             ; XOFF
EOF:        .byte   $1A             ; End of file
ULINE:      .byte   "_"             ; _
SCOLON:     .byte   ";"             ; ;
PERIOD:     .byte   "."             ; .
DOLLAR:     .byte   "$"             ; $
COLON:      .byte   ":"             ; :
CARET:      .byte   "^"             ; ^
QUOTE:      .byte   $22             ; " May be modified to ' by the command processor
DEFAULTEXT: .byte   "C"             ; Current ASCII default file extension character ("C").
NUMOVL:     .byte   $11             ; Number of system overlays+1
CURROVL:    .byte   $00             ; Current overlay number

            .export DEFDRV, NUMFNAMES, DUMPBYTES, DUMPCHANN

DEFDRV:     .byte   $00             ; Current default drive number (Set by DRIVE command).
TOPASSIGTB: .byte   $4F             ; Top of active files table (6 active files max)
NUMFNAMES:  .byte   $05             ; Number of file names per line for FILES command (5 or less).
DUMPBYTES:  .byte   $10             ; Number of bytes to dump per display line.
DUMPCHANN:  .byte   $02             ; Default output channel for dump command

ARITHTBLLEN = 5
ARITHMOP:   .byte   $2B             ; "+"   (List of arithmetic operators)
            .byte   $2D             ; "-"
            .byte   $2A             ; "*"
            .byte   $2F             ; "/"
BACKSLASH:  .byte   $5C             ; ASCII character to be used in lieu of Backslash "\"

SYSERRMNAM: .byte   "SYSERRMSG.Z"
CMDPROCNAM: .byte   "COMDPROC.Z"
STARTUPNAM: .byte   "STARTUP.J"

            .export INPLBUF, INTSRVP, ERRRCVRYP, CMDFNP

INPLBUF:    .word   $0500           ; Pointer to start of system input line buffer.
OUTLBUF:    .word   $0600           ; Pointer to start of system output line buffer
LBUFADDR:   .word   $A000           ; Pointer to large transient buffer for COPYF, ETC.
LBUFSIZE:   .word   $1400           ; Size (NOT. final address) of large transient buffer.
INTSRVP:    .word   INTSRV          ; Pointer to user-defined interrupt service routine.
ERRRCVRYP:  .word   ERRRCVRY        ; Pointer to user-defined error recovery routine.     
SECSTRK:    .byte   NSECTS          ; Number of sects per track
CMDFNP:     .word   $0000           ; Pointer to internal command function
            .byte   $00
            .byte   $00
DRIVERP:    .word   $0000           ; Pointer to current device driver
SAVECH:     .byte   $00             ; Used for temporary save character in I/O functions

; Breakpoint table

            .export BPBANK, BPADDRLO, BPADDRHI, BPOP

BPBANK:     .byte   $FF             ; Breakpoint bank
            .byte   $FF             ;
            .byte   $FF             ;
BPADDRLO:   .byte   $00             ; Breakpoint address (LSB)
            .byte   $00             ;
            .byte   $00             ;
BPADDRHI:   .byte   $00             ; Breakpoint address (MSB)
            .byte   $00             ;
            .byte   $00             ;
BPOP:       .byte   $00             ; Opcode at breakpoint
            .byte   $00             ;
            .byte   $00             ;

; Jump table
;
JMPTBL:     jmp     NMIPROC         ; Jump to NMI processor               
            jmp     IRQPROC         ; Jump to IRQ and BRK processor
            jmp     CTRLCPROC       ; Jump to CNTRL/C processor
            jmp     JWARMST         ; Jump to OS warm reset entry


COLDST:     cld                     ; Clear decimal mode
            ldx     #$FF            ; Set stack pointer
            txs                     ;
            jsr     SYSINIT         ; Init variables, pointers and jump tables
            lda     #$BF            ; Set maximum record length for input line
            sta     YLNLIM          ;
            jsr     OPENDRV0        ; Open disk drive 0
            jsr     LDCMDPR         ; Load command processor

            ; Opens (assign) STARTUP.J if exists
            ;
            ldx     #$08            ; Copy file name to buffer 
@LOOP:      lda     STARTUPNAM,x    ;
            sta     FNAMBUF,x       ;
            dex                     ;
            bpl     @LOOP           ;
            ldx     #$00            ; Set drive 0
            stx     CURRDRV         ;
            jsr     FEXIST          ; Check if file exists
            bne     @CONT           ; No, continue to warm start
            ldx     #$01            ; Assigns channel 1 to file (fails if not found)
            jsr     FOPEN           ;
@CONT:      jmp     WARMST          ; Continue to warm start
            ; Not reached

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
            sta     DEFBNKCFG       ; 
            jsr     INIMMAP

            lda     #$EA            ; Init the JPOSTERR jump with NOPs
            ldx     #$02            ;
@LOOP1:     sta     JPOSTERR,x      ;
            dex                     ;
            bpl     @LOOP1          ;

            ; Copy jump table 
            ;
            ldx     #$0B            ; Table size - 1 
@LOOP2:     lda     JMPTBL,x        ;
            sta     NMIPRC,x        ;
            dex                     ;
            bpl     @LOOP2          ;

            lda     #'C'            ; Current ASCII default file extension character ("C")
            sta     DEFAULTEXT      ;
            ; Fall through

; SVC 26 - Reinstate normal error processing by CODOS
;
            .export RESTERRP

RESTERRP:   lda     #<ERRRCVRY      ; Set pointer to error recovery routine
            sta     ERRRCVRYP       ;
            lda     #>ERRRCVRY      ;
            sta     ERRRCVRYP+1     ;
            rts

; CTRL-C processor
;
CTRLCPROC:  cld
            ldx     #$FF            ; Is this a bug and it should be TXS?
            tsx                     ;
            jsr     SYSINIT         ; Init system variables, pointers and jump tables
            lda     #$00            ; Useless, gets overwritten below
            ldy     NDRIVES         ; Get number of drives in system
            dey                     ; Last drive
@LOOP:      tya                     ; Close drive
            jsr     CLDRIVE         ;
            dey                     ; Close next
            bpl     @LOOP           ;
            jsr     DEFSETOUTB      ; Assigns default output device and set output buffer
            jsr     INITIO          ; Clear screen and set default values of display
            jsr     EXSENSEINT      ; Serve any pending interrupt
            jsr     OPENDRV0        ; Open system drive
            jsr     OUTSTR          ; Print reset message
            .byte   "RESET.", $00
            jmp     WARMST          ; Do a warm start
            ; not reached

; NMI Processor
;
NMIPROC:    sta     INTSVA          ; Save accumulator
            lda     #$00            ; Unprotect K-1013 SYSRAM
            sta     HSRCW           ;
            sec                     ; Set NMI flag
            ror     NMIFLAG         ;
            jmp     INTPROC         ; Jump to interrupt processor
            ; Not reched

; IRQ Processor
;
IRQPROC:    sta     INTSVA          ; Save accumulator on entry
            pla                     ; Get and save back processor status register
            pha                     ;
            and     #$10            ; Check if BRK
            bne     SVCINT          ; Yes, could be an SVC
            lda     INTSVA          ; Recover accumulator
            jmp     (INTSRVP)       ; Jump to user-defined interrupt service routine
            ; Not reached

; Interrupt service routine
;
INTSRV:     lda     #$00            ; Unprotect K-1013 SYSRAM
            sta     HSRCW           ;
            sta     NMIFLAG         ; Clear NMI flag
            ; Fall through

; Interrupt processor
;
INTPROC:    sec                     ; Set IRQ flag
            ror     IRQFLAG         ;
            pla                     ; Get and save processor status register
            sta     PROCST          ;
            pla                     ; Get and save
            sta     PCSAVE          ; Program counter (low)
            pla                     ; Program counter (high) in A
            jmp     INTCONT         ; Jump to interrupt processor
            ; Not reached

; Service interrupt routine
;
SVCINT:     lda     #$00            ; Unprotect K-1013 SYSRAM
            sta     HSRCW           ;
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
            ; Fall through

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
            sta     DEFBNKCFG       ;
            jsr     INIMMAP         ;
            lda     INTSVA          ; 
            sta     ACCUM
            bit     IRQFLAG         ; Is it an IRQ?
            bmi     @DOIRQ          ; Yes, do it
                                    ; No, it is a BP
            sec                     ; Set the "print registers at BP, not at error"
            ror     PRBPREGS        ;   flag
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
            lda     PRGBANK         ; Get program bank
            eor     DEFBNKCFG       ;    What is it doing here?
            sta     BNKCTL          ;
            lda     BPOP,x          ; Get saved instruction byte at BP 
            sta     (PCSAVE),y      ; and restore it to the PC
            lda     DEFBNKCFG       ; TODO: Again
            sta     BNKCTL          ;    Why?
            lda     #$FF            ; Invalidate/clear BP
            sta     BPBANK,x        ;
            jsr     DEFSETINPB      ; Set input buffer
            jsr     DEFSETOUTB      ; Set output buffer
            jsr     OUTSTR
            .byte   $0d, "BP", $00
            jmp     @OUTSTAT        ; Print status (Registers, pointers) and warm-start
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

@DOIRQ:     jsr     DEFSETINPB      ; Set input buffer
            jsr     DEFSETOUTB      ; Set output buffer
            bit     IRQFLAG         ; Is it an IRQ?
            bpl     @DOBRK          ; No, then it is a BRK
            jsr     OUTSTR          ; Yes, print
            .byte   $0D, "INTERRUPT (", $00
            bit     NMIFLAG         ; Is it an NMI?
            bpl     @NOTNMI         ; No, print IRQ
            jsr     OUTSTR          ; Yes, print NMI
            .byte   "NMI)", $00     ;
            jmp     @OUTSTAT
            ; Not reached

@NOTNMI:    jsr     OUTSTR          ; Print IRQ
            .byte   "IRQ)", $00     ;
            jmp     @OUTSTAT

@DOBRK:     jsr     OUTSTR          ; Print BRK
            .byte   $0D, "BRK", $00

@OUTSTAT:   jsr     OUTSTR          ; Print registers at interrupt
            .byte   ", ", $00       ;
            jsr     OUTREGS         ;
            jmp     WARMST          ; And warm start
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

ERROR52:    inc     ERRNUM          ; Missing or illegal function key number
ERROR51:    inc     ERRNUM          ; Missing or illegal memory bank number
ERROR50:    inc     ERRNUM          ; System crash: Directory redundancy check failed
ERROR49:    inc     ERRNUM          ; System crash: NEC 765 chip result phase error
ERROR48:    inc     ERRNUM          ; System crash: NEC 765 chip command phase error
ERROR47:    inc     ERRNUM          ; System crash: illegal track on disk
ERROR46:    inc     ERRNUM          ; System crash: file ordinal check error
ERROR45:    inc     ERRNUM          ; System crash: directory/file table check error
ERROR44:    inc     ERRNUM          ; System crash: illegal sector on disk
ERROR43:  	inc     ERRNUM          ; System crash: illegal system overlay number
ERROR42:  	inc     ERRNUM          ; Unformatted diskette or hardware drive fault
ERROR41:  	inc     ERRNUM          ; Unformatted diskette or irrecoverable seek error
ERROR40:  	inc     ERRNUM          ; Unformatted diskette or drive went not-ready
ERROR39:  	inc     ERRNUM          ; Diskette is full; no room left in directory
ERROR38:  	inc     ERRNUM          ; Diskette is full; all blocks already allocated
ERROR37:  	inc     ERRNUM          ; Required software package not loaded in memory
ERROR36:  	inc     ERRNUM          ; Illegal entry into CODOS system
ERROR35:  	inc     ERRNUM          ; No CODOS on drive 0, or system overlay load error
ERROR34:  	inc     ERRNUM          ; Not enough channels are free for specified function
ERROR33:  	inc     ERRNUM          ; Input from output-only device, or vice-versa
ERROR32:  	inc     ERRNUM          ; Write-protected disk or formatting error
ERROR31:  	inc     ERRNUM          ; Breakpoint table full (3 BP's already set).
ERROR30:  	inc     ERRNUM          ; Unformatted disk or irrecoverable read/write error
ERROR29:  	inc     ERRNUM          ; All buffers in use (free a chan. assigned to a file)
ERROR28:  	inc     ERRNUM          ; Missing or illegal register name
ERROR27:  	inc     ERRNUM          ; <destinatiom address missing or illegal
ERROR26:  	inc     ERRNUM          ; Missing or illegal character string delimiter (' , ")
ERROR25:  	inc     ERRNUM          ; New file name is already on selected diskette
ERROR24:  	inc     ERRNUM          ; <value> missing or illegal
ERROR23:  	inc     ERRNUM          ; Memory verify failure during SET or FILL
ERROR22:  	inc     ERRNUM          ; Illegal or unimplemented SVC number
ERROR21:  	inc     ERRNUM          ; New file on write-protected diskette
ERROR20:  	inc     ERRNUM          ; <entry> address missing or illegal
ERROR19:  	inc     ERRNUM          ; Arithmetic overflow
ERROR18:  	inc     ERRNUM          ; <value> out of range (greater than $FF or less than 0)
ERROR17:  	inc     ERRNUM          ; Reserved or protected memory violation
ERROR16:  	inc     ERRNUM          ; <from> address greater than to address
ERROR15:  	inc     ERRNUM          ; <to> address missing or illegal
ERROR14:  	inc     ERRNUM          ; <from> address missing or illegal
ERROR13:  	inc     ERRNUM          ; Not a loadable ("SAVEd") file
ERROR12:  	inc     ERRNUM          ; Missing or illegal file name
ERROR11:  	inc     ERRNUM          ; Missing or illegal device or file name
ERROR10:  	inc     ERRNUM          ; Diskette is write-protected
ERROR09:  	inc     ERRNUM          ; Channel needed is not assigned
ERROR08:  	inc     ERRNUM          ; Missing or illegal channel number
ERROR07:  	inc     ERRNUM          ; Locked file violation
ERROR06:  	inc     ERRNUM          ; Drive needed is not ready
ERROR05:  	inc     ERRNUM          ; Missing or illegal disk drive number
ERROR04:  	inc     ERRNUM          ; Syntax error in command argument
ERROR03:  	inc     ERRNUM          ; Drive needed is not open
ERROR02:  	inc     ERRNUM          ; File not found
ERROR01:  	inc     ERRNUM          ; Command not found
            jmp     (ERRRCVRYP)

; Error recovery routine
;
ERRRCVRY:   pha
            lda     #$00            ; Unprotect K-1013 SYSRAM
            sta     HSRCW           ;
            cld
            jsr     INIMMAP         ; Set default memory config
            bit     PERRPFLG        ; Error was during print error processing?
            bpl     @OUTERR         ; No, go print error
            pla                     ; Yes, just do a warm start
            jmp     WARMST          ;

@OUTERR:    pla                     ; Save registers at error
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
            lda     #$80            ; Set flag: Print error processing, so errors beyond
            sta     PERRPFLG        ; this point won't cause a lock with a print error loop
            jsr     DEFSETINPB      ; Set default input device and set input buffer 
            jsr     DEFSETOUTB      ; Same for output
            jsr     OUTSTR          ; Print error message
            .byte   $0d, "CODOS ERROR #", $00
            lda     ERRNUM          ; Get error num
            jsr     HEXBYTE         ; Put into output buffer as HEX
            jsr     POUTBUFFCR02    ;
            bit     INTCMDERR       ; Error during internal command processing?
            bpl     @NOCMDERR       ; No, don't print command line info
@PRCMD:     lda     (INPBUFP),y     ; Yes, print the command line
            cmp     #$0D            ; End?
            beq     @GETCMDIDX      ; Yes, exit loop
            jsr     OUTCHAR         ; No, print char
            iny                     ; And go for the next one
            bne     @PRCMD          ;
@GETCMDIDX: jsr     OUTCR           ; New line
            ldy     CMDLIDX         ; Recover index in command line processing
            beq     @OUTARROW       ; If it is at the beginning, skip
            lda     #$20            ; Print spaces til the index position
@PSPACE:    jsr     OUTCHAR         ;
            dey                     ;
            bne     @PSPACE         ;
@OUTARROW:  lda     #'^'            ; Print an "arrow" pointing to the character of the
            jsr     OUTCHAR         ; command CODOS was going to examine before error
            jsr     OUTCR           ; 
            jmp     @OUTLONG        ; Print long error message

@NOCMDERR:  bit     PRBPREGS        ; Should we print registers at BP, not at error?
            bmi     @PRREGS         ; Yes, go for it
            bit     NOPRREGS        ; Should we print registers at all?
            bmi     @OUTLONG        ; No, go print human readable message
            lda     ERRADDR         ; Yes, prepare variables:
            sta     PCSAVE          ; Program counter at error
            lda     ERRADDR+1       ;
            sta     PCSAVE+1        ;
            ldx     #$04            ; Stack, Processor status, Y, X and A
@CPYREG:    lda     ERRORS,x        ;
            sta     STACKP,x        ;
            dex                     ;
            bpl     @CPYREG         ;

@PRREGS:    jsr     OUTREGSLB       ; Print registers

@OUTLONG:   bit     PERRPFLG        ; Are we here as part of the print error processing? 
            bpl     WARMST          ; No, just warm start
            ldx     #$0B            ; Yes, get file with error messages
@CPYFNAM:   lda     SYSERRMNAM,x    ;
            sta     FNAMBUF,x       ;
            dex                     ;
            bpl     @CPYFNAM        ;
            inx                     ; X == 0
            stx     CURRDRV         ; Set drive 0
            jsr     FEXIST          ; Check if file exists
            bne     WARMST          ; No, skip message display
            jsr     FOPEN0          ; Assigns channel 0 to file (fails if not found)
@GETMSG:    ldx     #$00            ; Get entire line from error message file
            jsr     GETLINE         ;   returns length in A
            bcs     @RETURN         ; If error, just free channel and return
            dec     ERRNUM          ; Repeat until we reach
            bne     @GETMSG         ; The error message correponding to ERRNUM
            tay                     ; Transfer line length to Y
            tax                     ;   and X
@CPYLINE:   lda     (INPBUFP),y     ; Copy the line (message) to the output buffer
            sta     (OUTBUFP),y     ;
            dey                     ;
            bpl     @CPYLINE        ;
            txa                     ; Restore line length in A
            tay                     ;   and transfer to Y
            ldx     #$02            ; Print new line to console
            jsr     OUTCR           ;
            jsr     POUTBUFFCR02    ; Print output buffer to console (length in Y)
@RETURN:    jsr     FREECH0         ; Free channel and return

; Post-error user routine
;
; May be used for jumping to an alternate warm start routine or do some additional
; error handling: NOTE: AUTOTERM.C inserts a jmp $2800 here
;
            .export JPOSTERR

JPOSTERR:   nop                     ;
            nop                     ;
            nop                     ;

            .export WARMST

WARMST:     cld
            lda     #$00            ; Unprotect K-1013 SYSRAM
            sta     HSRCW           ;
            jsr     CKCMDPR         ; Load command processor if not loaded yet
            jmp     CMDPROC         ; And jump to it
            ; Not reached

; Execute command, making sure that the command processor is loaded first
;
            .export CKCMDEXEC

CKCMDEXEC:  jsr     CKCMDPR         ; Make sure that command processor is loaded
            jmp     CMDEXEC         ; Execute command
            ; Not reached


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
@SKIP:      jmp     CONTBP          ; Continue with command execution


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
            ; Fall through

; Execute program in memory (common to GO and external commands)
;
            .export EXCMD

EXCMD:      ldx     #$7F
            stx     DEFBNKCFG
            ldx     DEFSVCFLAG      ; Get default SVC state (enabled or disabled)
            stx     SVCSTAT         ; And set it as current
            ldx     #$FF            ; Discard stack
            bit     SVC13FLG        ; Was the GO invoked by an SVC?
            bpl     NOSVCGO         ; No, skip
            ; Fall through

; Common code for NEXT and GO commands, continue after BP
;
            .export CONTBP

CONTBP:     jsr     RESTORE
            jmp     L00D2           ; Switch bank and jump

NOSVCGO:    stx     STACKP          ; Set stack (Discarded in case of GO)
            jsr     RESTORE
            jsr     BANKSW          ; TODO: Understand what it does
            php
            cld                     
            lda     #$00            ; Unprotect K-1013 SYSRAM
            sta     HSRCW           ;
.if  CODOS2_VER = 17
            lda     SAVEACC         ; Restore accumulator
.else
            lda     INTSVA          ; Restore accumulator
.endif
            sta     ACCUM           ; Save it to BP registers data
            stx     XREG            ;   also Y
            sty     YREG            ;   also X
            pla                     ;   and the processor status register
            sta     PROCST          ;
            tsx                     ;   and the stack pointer
            stx     STACKP          ;
            lda     #$7F            ; Set up the default memory map config
            sta     DEFBNKCFG       ;
            jmp     WARMST          ; And fo a warm start

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
            eor     DEFBNKCFG       ;
            eor     DATBANK         ;
            sta     BNKCFG          ;
            lda     #$7F            ;
            sta     SVIA1DIR        ;
            jsr     CPYSWNJMP       ; Copy switch and jump routine to page 0
            lda     #$00            ; Clear flags
            sta     INTCMDERR       ;   Error during internal command processing
            sta     PRBPREGS        ;   Print registers at BP instead of error
            lda     #$03            ; Protect SYS memory and DMA direction to read
            bit     UNPROTFLG       ; Unprotect SYSRAM?
            bpl     @SKIP2          ; No, skip
            lda     #$01            ; Set DMA direction byte to read, no memory protection
@SKIP2:     sta     HSRCW           ;
            lda     SVCSTAT         ; Restore SVC status
            sta     SVCENB          ;
            lda     ACCUM           ; Get and save A
.if  CODOS2_VER = 17
            sta     SAVEACC         ;
.else
            sta     INTSVA          ;
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
            jsr     FOPEN0          ; Assigns channel 0 to file (fails if not found)
            ldx     #$00
            txa
            jsr     LOADSVD
            bcc     @LDNEXT
            jsr     ERROR13         ; Not a loadable ("SAVEd") file
            ; Not reached
@LDNEXT:    ldx     #$00            ; Keep loading segments until end of file
            txa
            jsr     LOADSVD
            bcc     @LDNEXT
            jmp     FREECH0         ; Clse/free channel and return

; Init memory map config
;
            .export INIMMAP

INIMMAP:    sec
            ror     SEEIO           ; Set I/O space enable semaphore
            lda     #$00            ; Set destination bank for memory copy ops?
            sta     DSTBANK         ;
            lda     DEFBNKCFG       ; Set default bank config
            sta     BNKCTL          ;
            lda     #$7F            ;
            sta     SVIA1DIR        ;

            ; This clears the break flag by forcing an rti

            lda     #>@RETURN       ; Set return address to $EC11
            pha                     ;
            lda     #<@RETURN       ;
            pha                     ;
            php                     ;
            rti                     ; This will return just below
@RETURN:    rts


; Copy bank switch/restore routine to $0100-$0112

            .export CPYBNKSW

CPYBNKSW:   ldx     #$12
@LOOP:      lda     BANKSW_O,x
            sta     BANKSW,x
            dex
            bpl     @LOOP
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
            sta     IOENABLE        ; Enable I/O space from $BE00 to $BFFF
            lda     #$FF            ; Sets bank 0 and write enable $8000 to $BFFF
            sta     BNKCTL          ;
            sta     SVIA1DIR        ;
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
            jsr     ERROR48         ; System crash: NEC 765 chip command phase error
@CONT:      lda     CMDTBL+1,x      ; Write command byte
            sta     DATR            ;
            inx                     ; next command byte
            dey                     ;
            bne     @WAITRD         ; Until command length
            ldy     SAVEY1          ; Restore Y register
            rts                     ; And return

; Sense drive X status command
;
EXSENSEDRV: stx     SENDRVHD        ; Set drive into sense command
            ldx     #SENSEDRV       ; Send Sense drive command to the disk controller
            bne     EXSENSECMD      ;
            ; Always jump

; Sense interrupt command
;
EXSENSEINT: ldx     #SENSEINT       ; Send Sense interrupt command to the disk controller
            ; Fall through

; Execute SENSE type of command
;
EXSENSECMD: jsr     SNDCMD          ; Send command
            ; Fall through

;       Read result from uPD765 Data Register
;
RSLTPH:     ldx     #$00            ; Init disk status index
@WAITRD:    lda     MSTR            ; Read uPD765 Main Status Register
            bpl     @WAITRD         ; Wait until bit 7 is 1 (Ready)
            and     #$40            ; Check data direction
            bne     @CONT           ; Jump if data register is to be read
            jsr     ERROR49         ; System crash: NEC 765 chip result phase error
            ; Not reached
@CONT:      lda     DATR            ; Read data register
            sta     DSKSTAT,x       ;
            nop                     ; Give the controller some time
            nop                     ;
            inx                     ; Advance one pos
            lda     MSTR            ; Check if still busy
            and     #$10            ;
            bne     @WAITRD         ; Yes, go get next byte
            rts

; Send SEEK type command to uPD765 and process status
;
SNDSKCMDST: jsr     SNDCMD          ; Send command to controller

@WAITINT:   lda     HSRCW           ; Wait for interrupt ( Bit 7 of HSRCW is 0)
            bmi     @WAITINT        ;
            ; Fall through

; Execute a Sense interrupt command and return
; Carry set if error, carry clear otherwise
;
SNSINTST:   jsr     EXSENSEINT      ; Execute a sense interrupt command
            lda     ST0             ; Get status register 0
            cmp     #$C0            ; Return carry if error
            rts

; Init disk drive X
;
; Performs a complete init sequence of SPECIFY and RECALIBRATE and get
; disk information (single or dual side)
;
INITDRV:    jsr     DRVVALID        ; Ensure that drive is valid
            stx     RECDRVHD        ; Set drive into recalibrate command
@LOOP:      lda     HSRCW           ; Check if interrupt pending
            bmi     @CONT           ; No, continue
            jsr     EXSENSEINT      ; Serve interrupt
            jmp     @LOOP           ; And go check again

@CONT:      ldx     #SPECIFY        ; Send specify command
            jsr     SNDCMD          ;
            ldx     #RECALIBRATE    ; Send recalibrate command
            jsr     SNDSKCMDST      ;
            and     #$D8            ; Delete don't care bits from ST0
            beq     @GETST          ; No error, get status
            and     #$08            ; Fail: Check if ready
            bne     @NOTRDY         ; Not ready
            jsr     ERROR42         ; Unformatted diskette or hardware drive fault
            ; Not reached
@NOTRDY:    jsr     ERROR06         ; Drive not ready error
            ; Not reached
@GETST:     lda     ST1             ; Get status register 1
            beq     @GETNHD         ; All clear, go get disk sides
            jsr     ERROR41         ; Unformatted diskette or irrecoverable seek error
            ; Not reached
@GETNHD:    ldx     RECDRVHD        ; Get drive
.if  CODOS2_VER = 17
            jsr     EXSENSEDRV17    ; Sense drive
.else
            jsr     EXSENSEDRV      ; Sense drive
.endif
            ldx     RECDRVHD        ; Get drive
            lda     ST0             ; Get status register 0
            and     #$08            ; Filter out except Two Sides flag
            beq     @STORE          ; One side
            lda     #$80            ; Two sides
@STORE:     sta     DRVNFO,x        ; Store info for drive (Bit 7 = 1 -> 2 sides)
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
EXSEEK:     stx     SEKDRVHD        ; Set drive for seek command
            stx     SKDRIVE         ; Save as current
            cmp     #$4D            ; Check it is a valid track
            bcc     @VALID          ; 
            jsr     ERROR47         ; System crash: illegal track on disk
            ; Not reached
@VALID:     sta     SEKTRACK        ; Set track for seek command
            lda     CHEAD           ; Get single or dual side disk
            beq     DOSEEK          ; Single side, head 0 (no need to change DRVHD)
            lda     #$04            ; Dual sided, select head 1
            ora     SEKDRVHD        ; Combine with drive number
            sta     SEKDRVHD        ; Update drive and head
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
GETDRVTRK:  ldx     SKDRIVE         ; Drive
            lda     SEKTRACK        ; Track from seek command
DORTS:      rts

; Manage seek errors
;
SKERROR:    and     #$03            ; Is it our drive
            cmp     SKDRIVE         ;
            bne     @RETRY          ; No, close and retry
            jsr     ERROR06         ; Drive needed is not ready.
            ; Not reached

@RETRY:     jsr     CLDRIVE         ; Close drive
            jmp     DOSEEK          ; Seek again
            ; Not reached

; Execute READ or WRITE command
;
EXRDWR:     lda     CURFINFO+_DMABF ; Set DMA buffer
            sta     ADMA            ;
            lda     RWREOSEC        ; Get End Of Track sector
            cmp     #NSECTS         ; Compare with number of sectors
            bcc     @CONT           ; Is lower, continue
            jsr     ERROR44         ; System crash: illegal sector on disk
            ; Not reached
@CONT:      lda     DMADIR          ; Set DMA direction
            sta     HSRCW           ;
            ldx     #READWRITE      ; Execute READ or WRITE command
            jsr     SNDCMD          ;
@WAITINT:   lda     HSRCW           ; Wait for interrupt ( Bit 7 of HSRCW is 0)
            bmi     @WAITINT        ;
            jsr     RSLTPH          ; Get result
            lda     ST0             ; Get status register 0
            and     #$D8            ; Mask out don't care bits
            beq     @RETCC          ; No error bit set, return carry clear
            cmp     #$40            ; If abnormal termination
            beq     @CKEOC          ; go check if it is End of Cylinder
                                    ; Any other value is an error
            jsr     ERROR40         ; Unformatted diskette or drive went not-ready
            ; Not reached
@CKEOC:     lda     ST1             ; Get status register 1
            and     #$B7            ; Mask out don't care bits
            cmp     #$80            ; Is it "End Of Cylinder" (normal for the K-1013)
            beq     @RETCC          ; Yes, return with carry clear
            sec                     ; Any other bit, it is an error
            rts                     ;
            ; Not reached
@RETCC:     clc                     ; Clear carry and return
            rts                     ;

; Write sector A
;
WRITSECT:   sta     RWRSECTR        ; Set sector for write command
            lda     #$45            ; Set command to write
            sta     RDWRD           ;
            lda     #$00            ; Set DMA to read mode
            beq     RDWRSECT        ; Always jump
            ; Not reached

; Read sector of current file position
;
RDFPSECT:   jsr     GETFPSECT       ; Get physical sector coordinates
            ; Fall through

; Read sector A (with CHEAD, and CTRACK) sector also in CSECT
;
            .export READSECT

READSECT:   sta     RWRSECTR        ; Set sector for write command
            lda     #$46            ; Set command to read
            sta     RDWRD           ;
            lda     #$01            ; Set DMA to write mode
            ; Fall through

; Common code for READ and WRITE sector functions
;
RDWRSECT:   sta     DMADIR          ; Set DMA direction
            cmp     DMADIR          ; Does it make sense?
            bne     @SYSERR         ; Because this never jumps :/
            inc     DMADIR          ; Again, this make no sense
            cmp     DMADIR          ; Because this comparison is always NE
            bne     @CONT           ; so it always jumps
@SYSERR:    jsr     ERROR36         ; Illegal entry into CODOS system
            ; Not reached
@CONT:      sta     DMADIR          ; Set DMA direction into
            sta     HSRCW           ; the K-1013 register
            jsr     DRVVALIDO       ; Check that drive X is valid and open
            stx     RWRDRVHD        ; Store drive into command
            stx     RWDRIVE         ; And save as drive for R/W operations
            lda     #$00            ; Set head 0
            sta     CHEAD           ;
            sta     RWRHEADN        ; Set head in READ or WRITE command
            lda     RWRSECTR        ; Get sector from READ or WRITE command
            cmp     #NSECTS         ; Is it a valid sector number?
            bcc     @CNTRW          ; Yes, continue with read or write
            sbc     #NSECTS         ; Nom maybe a 2 side disk. Realculate sector
            sta     RWRSECTR        ;
            lda     DRVNFO,x        ; Check if one or two sides
            bmi     @S2RW           ; Two sides, modify read/write command
            jsr     ERROR44         ; System crash: illegal sector on disk
            ; Not reached
@S2RW:      lda     #$04            ; Add head number into command drive/head
            ora     RWRDRVHD        ;
            sta     RWRDRVHD        ;
            lda     #$01            ; Add header number into command head
            sta     RWRHEADN        ;
            lda     RWRSECTR        ; Get recalculated sector
@CNTRW:     sta     RWREOSEC        ; And save it into command EOT sector
            lda     SEKTRACK        ; Get track from last seek
            sta     RWRTRACK        ; And store into command
            jsr     EXRDWR          ; Execute command
            bcs     @FAILED         ; Failed? Retry.
@RETOK:     ldx     RWDRIVE         ; OK, return drive and sector in XA
            lda     RWRSECTR        ;
            rts

@FAILED:    lda     RDWRD           ; Check if read or write
            cmp     #$46            ; Is it a read command?
            bne     @ISWRT          ; No, it's write
            inc     RDERRCNT        ; Yes, increment read error count
@AGAIN:     jsr     EXRDWR          ; Execute command again
            bcc     @RETOK          ; Success, return
            inc     RCERRCNT        ; Failed again, try recalibrate
            ldx     RWDRIVE         ;
            jsr     INITDRV         ;
            lda     RWRTRACK        ; Recover and set track where error occurred
            sta     TRKERRNUM       ;
            jsr     SEEKTRK         ; Seek to it
            lda     RWREOSEC        ; Set sector of last error
            sta     SECERRNUM       ; causing a recalibrate
            lda     #$10            ; Set retries to 16
            sta     RETRIES
@RETRY:     jsr     EXRDWR          ; Execute command
            bcc     @RETOK          ; If ok, return
            dec     RETRIES         ; No, retry
            bne     @RETRY          ;
            bit     IGNORERR        ; No more retries. Ignore error set?
            bmi     @RETOK          ; Yes, return
            jsr     ERROR30         ; Unformatted disk or irrecoverable read/write error
            ; Not reached
            jmp     @RETOK          ; Dead code?

@ISWRT:     inc     WRERRCNT        ; Increment write error count
            lda     ST1             ; Check status register 1
            and     #$02            ; Was it a write protected error?
            beq     @AGAIN          ; No, retry
            jsr     ERROR10         ; Diskette is write-protected
            ; Not reached

; Gets FINFO for current file (DEVICE), copies it into CURFINFO in page zero
; and sets CURRDRV
;
            .export GETFINFO
    
GETFINFO:   jsr     CPYCFINFO       ; Copies file info structure to CURFINFO struct
                                    ; in page zero
            lda     CURFINFO+_DRIVE ; Get file drive
            sta     CURRDRV         ; Sets as current drive
            ; Fall through

; Set the BATP to the current drive's BAT
;
            .export SETBATP

SETBATP:    lda     #$00            ; BAT begins at page start 
            sta     BATP            ;
            lda     #$E4            ; Drive 0 BAT page
            sec                     ; Subsequent drive BATs are located <drivenum>
            sbc     CURRDRV         ; pages below
            sta     BATP+1          ;
            rts

;  Set next block A for block Y into the current BAT
;
SETNEXTBLK: stx     SAVEX7          ; Save X
            sta     (BATP),y        ; Store next block A for block Y
            lda     #$E4            ; Drive 0 BAT page
            sec                     ; Subsequent drive BATs are located <drivenum>
            sbc     BATP+1          ; So sugbtract current BAT page to get current
            tax                     ; drive and transfer to X
            lda     #$80            ; Flag that there are changes in the BAT
            sta     BATCHG,x        ; for this drive
            lda     (BATP),y        ; Recover next block in A
            ldx     SAVEX7          ; Restore X
            rts

; Read sector A from track 12
;    If sector == 0, loads BAT into BAT area
;    If sector != 0, into directory buffer
;
RDSECTATR12:
            sta     SECTNUM         ; Set sector from track 12 to read
            ; Fall through

; Read SECTNUM from track 12
;    If sector == 0, loads BAT into BAT area
;    If sector != 0, into directory buffer
;
            .export RDSECTNTR12

RDSECTNTR12:
            jsr     PREPRDTR12      ; Prepare read of track 12
            jsr     READSECT        ; And read sector
            rts

; Write BAT to current drive
;
WRTBAT:     lda     #$00
            sta     SECTNUM         ; BAT's sector number is 0
            ; Fall Through

; Write to sector A of TRACK 12
;
WRTRCK12:   jsr     PREPRDTR12      ; Prepare the current FINFO struct for writing
                                    ; to TRACK 12
            jsr     WRITSECT        ; Write to sector SECTNUM
            cmp     #$00            ; If sector 0 (BAT), write to the BAT copy
            bne     PTR12RET        ; If not, return
            lda     #$11            ; Now write the BAT copy at sector $11
            jsr     WRITSECT        ;
            ldx     CURRDRV
            lda     #$00            ; Clear the BAT changes flag for current drive
            sta     BATCHG,x        ;
            rts

; Prepare the current FINFO struct for reading/writing to track 12 of a disk
;   If SECTNUM == 0 : Read BAT
;      SECTNUM != 0 : Read DIR
;
PREPRDTR12: lda     #$00            ; Set head 0
            sta     CHEAD           ;
            ldx     CURRDRV         ; Ensure that current drive is opened
            jsr     DRVVALIDO       ; Check that drive X is valid and open
            lda     #$0C            ; Track $0C holds directory info
            jsr     SEEKTRK         ;
            lda     #$94            ; Set transfer buffer to $E500 (Directory buffer)
            sta     CURFINFO+_DMABF ;
            lda     SECTNUM         ; If this is non-zero
            bne     PTR12RET        ;   just return
            lda     BATDMAT,x       ; 
            sta     CURFINFO+_DMABF ; Set transfer buffer to drive's BAT
            lda     #$00
PTR12RET:   rts

; Copies file info to the current file structure in page zero
;
            .export CPYCFINFO

CPYCFINFO:  ldy     DEVICE          ; Get current device (file)
            ldx     #$00
@LOOP:      lda     FINFOTBL,y      ; From file's FINFO
            sta     CURFINFO,x      ; To current FINFO structure
            iny                     ;
            inx                     ;
            cpx     #FINFOLEN       ; Repeat until done
            bmi     @LOOP           ;
            rts

; Copies back current finfo structure to file's FINFO, except for the
;    immutable data (buffer info and ???)
;
UPDCFINFO:  ldy     DEVICE          ; Get current device (file)
            ldx     #$00
@LOOP:      lda     CURFINFO,x      ; From current FINFO structure
            sta     FINFOTBL,y      ; To file's FINFO
            iny
            inx
            cpx     #FINFOLEN-4     ; Exclude immutable data
            bmi     @LOOP
            rts

; Compares CURFINFO+_FPOS and CURFINFO+_FSIZE
; Carry set if $E2-$E4 < $DF-$E1
;
LEF05:      lda     CURFINFO+_FPOS      ;
            cmp     CURFINFO+_FSIZE     ;
            lda     CURFINFO+_FPOS+1    ;
            sbc     CURFINFO+_FSIZE+1   ;
            lda     CURFINFO+_FPOS+2    ;
            sbc     CURFINFO+_FSIZE+2   ;
            rts

; Calculate remaining file size from current file pos
;   (FSIZE-FPOS)
;
CALREMFSIZ: sec
            lda     CURFINFO+_FSIZE     ;
            sbc     CURFINFO+_FPOS      ;
            sta     FILEPOS             ;
            lda     CURFINFO+_FSIZE+1   ;
            sbc     CURFINFO+_FPOS+1    ;
            sta     FILEPOS+1           ;
            lda     CURFINFO+_FSIZE+2   ;
            sbc     CURFINFO+_FPOS+2    ;
            sta     FILEPOS+2           ;
            rts

; Sets file end at current position
;
SETEND:     lda     CURFINFO+_FPOS+2    ; Just copies current fpos to fsize
            sta     CURFINFO+_FSIZE+2   ;
            lda     CURFINFO+_FPOS+1    ;
            sta     CURFINFO+_FSIZE+1   ;
            lda     CURFINFO+_FPOS      ;
            sta     CURFINFO+_FSIZE     ;
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
            ldx     #$00            ; Set head 0
            stx     CHEAD           ;
            ldx     #$0C
            cmp     #$09
            bcc     @CONT
            inx
            bit     DRVNFO          ; Check if disk in drive 0 is one or two sides
            bpl     @CONT           ; One side
            dex                     ; Two sides
            lda     #$01            ; Set head 1
            sta     CHEAD           ;
@CONT:      txa
            ldx     #$00            ; Seek track on drive 0
            jsr     SEEKTRK         ;
            lda     #$F8            ; DMA address for overlays ( $FE00 ) 
            sta     CURFINFO+_DMABF ;
            lda     CURROVL
            clc
            adc     #$11
            cmp     #NSECTS
            bcc     LEF7E
            bit     DRVNFO          ; Check if one or two sides
            bmi     LEF7E           ; Two sides
            sec                     ; One side
            sbc     #NSECTS
LEF7E:      jsr     READSECT
            lda     CURROVL
            cmp     OVLORG
            beq     LEF8C
            jsr     ERROR35         ; No CODOS on drive 0, or system overlay load error
            ; Not reached
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
            ; Not reached
@RETURN:    pla
            rts

; Check if drive is valid
;
            .export DRVVALID

DRVVALID:   cpx     NDRIVES         ; Check drive number
            bcc     @RETURN         ; Drive between 0 - 3  
            jsr     ERROR05         ; Missing or illegal disk drive number
            ; Not reached
@RETURN:    rts


; Get device or file for channel X
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
; Saves device into DEVICE and returns device/file in X and A.
; Does not return on error
;
            .export ASSIGNED

ASSIGNED:   jsr     GETDEV          ; Get device for channel
            bne     @RET            ; Valid, return            
            jsr     ERROR09         ; Channel needed is not assigned
@RET:       tax
            rts


; Get current file (DEVICE) and checks if locked.
; Does not return if it is
;
GETCURCHKLK:
            jsr     GETFINFO
            ; Fall through

; Check if file in DEVICE is locked.
; Does not return if it is
;       
CHKLCK:     lda     CURFINFO+_FLAGS
            and     #FLLOCKED
            beq     @RET
            jsr     ERROR07         ; Locked file violation
@RET:       rts

; Calculate sector, track  and head of file position
;
; File pointer is a 24 bit value
; As sectors are 256 bytes, and blocks start in a sector, the LSB is the byte offset
; in the sector and the two most significant bytes are used to calculate the block
; number and the sector offset in that block
;
; Blocks are 2K of size for single side, 4K for dual side. So, to calculate the block
; offset from the first block of the file, we have to divide by 8 for single side
; disks and 16 for dual side. 
;
; Sets CBLOCK, CHEAD, CTRACK, CSECT and BLKSCTOF, and returns block sector in A
;
            .export GETFPSECT

GETFPSECT:  lda     #$00            ; Set head 0
            sta     CHEAD         ;
            lda     #$07            ; Sectors/block-1
            sta     SCTBLKM1        ;
            ldx     CURFINFO+_DRIVE ; Get drive number
            lda     DRVNFO,x        ; Get flag for drive. If Bit 7 == 1, then dual side
            sta     DSFLAG          ; Store it
            lda     CURFINFO+_FPOS+2 ; Get sector offset
            sta     TEMP1            ;
            lda     CURFINFO+_FPOS+1 ;
            lsr     TEMP1           ; And divide it by 8
            ror     a               ;
            lsr     TEMP1           ;
            ror     a               ;
            lsr     TEMP1           ;
            ror     a               ;
            bit     DSFLAG          ; Is if a dual side disk?
            bpl     @CONT           ; No, we get the block offset in TEMP1, remainder
                                    ; offset in block in A
            ldx     #$0F            ; Yes, update the sectors/block-1
            stx     SCTBLKM1        ;
            lsr     TEMP1           ; And divide again (total is sector offset / 16)
            ror     a               ;
@CONT:      tax                     ; Transfer block offset to X
            lda     CURFINFO+_BATPT ; Get first block of file
            inx                     ; 
            bne     @DECOFF         ; Past block?
@NXTBLK:    tay                     ; Get index of nex block
            lda     (BATP),y        ; And read block info
@DECOFF:    dex                     ; Decrement block offset
            bne     @NXTBLK         ; Continue until we arrive to block offset
            sta     CBLOCK          ; Save block
            ;
            ; First block is 1
            ;
            ; BS = Sectors per block (8 for single side, 16 for dual)
            ; TS = Sectors per track (NSECTS for single side, NSECTS*2 for dual)
            ; TRACK = ((BLOCK - 1) * BS) / TS
            ; SECTOR =((BLOCK - 1) * BS) % TS
            ;
            ; IF BLOCK > 39, add 18 to SECTOR
            ;
            sec
            sbc     #$01            ; A = CBLOCK-1
            ldx     #$FF            ; Start with X = -1
                                    ; (will increment to 0 in the loop)
            sec                     ; Set carry for subtraction
@DIVLP:     inx                     ;
            sbc     #NSECTS/2       ;
            bcs     @DIVLP          ; X = (CBLOCK-1) / (NSECTS/2)
            adc     #NSECTS/2       ; A = (CBLOCK-1) % (NSECTS/2)
            asl     a               ;
            asl     a               ;
            asl     a               ; A  = ((CBLOCK-1) % (NSECTS/2))*BS (Single side)
            bit     DSFLAG          ; Dual side disk?
            bpl     @SKIP           ; Nope, skip
            asl     a               ; A  = ((CBLOCK-1) % (NSECTS/2))*BS (Dual side)
@SKIP:      sta     TEMP1           ; Save in TEMP1
            txa                     ;
            asl     a               ;
            asl     a               ;
            tax                     ; X = (CBLOCK-1) / (NSECTS/2) * 8
            lda     #NSECTS         ; Get sectors per track
            sta     SECSTRK         ;
            lda     #$07            ; Mask for sector offset in the block
            bit     DSFLAG          ; Dual side disk?
            bpl     @SKIP2          ; No, skip
            lda     #NSECTS*2       ; Yes, double the sectors per track
            sta     SECSTRK         ;
            lda     #$0F            ; And adjust sector offset mask
@SKIP2:     and     CURFINFO+_FPOS+1 ; Calculate and store sector offset
            sta     BLKSCTOF        ; in the block
            clc                     ;
            adc     TEMP1           ; Add sector offset to sector calculation
            
            ; Calculate track and sector

            dex                     ; X = (CBLOCK-1) / (NSECTS/2) * 8 - 1 ( -0
@TRKSBC:    inx                     ;   entering the loop)
            sec                     ; Set carry for subtraction
            sbc     SECSTRK         ; First substraction
            bcs     @TRKSBC         ; Overflow? No, loop
            adc     SECSTRK         ; Yes, restore last substraction
            cpx     #$0C            ; Track 12?
            bcc     @GOTIT          ; No, we've got our track and sector
            adc     #$11            ; Yes, have to add 12 sectors (11 + Cy)
            cmp     SECSTRK         ; Are we past sector per trck?
            bcc     @GOTIT          ; No, we are done
            sbc     SECSTRK         ; Yes, get sectors % SECSTRK
            inx                     ; And increment track number
@GOTIT:     stx     CTRACK          ; Save track
            sta     CSECT           ; Save file sector for pointer
            ldx     CURFINFO+_DRIVE ; Get file drive
            lda     CSECT           ; Get sector (again?)
            cmp     #NSECTS         ; Is it bigger that sectors per track?
            bcc     @SEEK           ; No, skip to seek track
            inc     CHEAD           ; Switch to head 1
@SEEK:      lda     CTRACK          ; Seek track
            jsr     SEEKTRK         ;
            lda     CSECT           ; And return sector in A
            rts

;
;
LF081:      jsr     GETFINFO        ; Gets FINFO for current file
            ; Fall through

;
;
LF084:      bit     CURFINFO+_FLAGS ; Check flags
            bvc     WRFRET          ; If What the fuck flag is not set, return
            ; Fall through

; Write sector of current file position
;
            .export WRFPSECT

WRFPSECT:   jsr     GETFPSECT       ; 
            jsr     WRITSECT        ;
            lda     CURFINFO+_FLAGS ;
            and     #$BF            ; Clear the WTF flag
            sta     CURFINFO+_FLAGS ;
WRFRET:     rts

; SVC 18 - Set the file position for a channel to End-of-File
;
; Arguments:            X = Channel number 
;
; Arguments returned:   None
;
            .export FEND

FEND:       lda     #$FF            ; Set file pos bigger than maximum file size
            sta     FILEPOS+2       ;
            jmp     FSEEK           ; And do a seek


; SVC 17 - Set the file position for a channel to Beginning-of-Data
;
; Arguments:            X = Channel number 
;
; Arguments returned:   None
;
            .export FREWIND

FREWIND:    lda     #$00            ; Set file pos at 0
            sta     FILEPOS         ;
            sta     FILEPOS+1       ;
            sta     FILEPOS+2       ;
            ; Fall through

; SVC 19 - Specify the file position for a channel
;
; Arguments:            X = Channel number 
;                       U7 (FILEPOS)
;
; Arguments returned:   None
;
            .export FSEEK

FSEEK:      jsr     ASSIGNED        ; Get assigned device/file to channel X
            bmi     @RETURN         ; Return if device
            jsr     LF081
            lda     FILEPOS
            clc
            adc     #FHDRLEN
            sta     CURFINFO+_FPOS
            lda     FILEPOS+1
            adc     #$00
            sta     CURFINFO+_FPOS+1
            lda     FILEPOS+2
            adc     #$00
            sta     CURFINFO+_FPOS+2
            bcs     @LF0C6
            jsr     LEF05
            bcc     @LF0CF
@LF0C6:     ldx     #$02
@LF0C8:     lda     CURFINFO+_FSIZE,x
            sta     CURFINFO+_FPOS,x
            dex
            bpl     @LF0C8
@LF0CF:     jsr     RDFPSECT
            jsr     UPDCFINFO
@RETURN:    rts

; SVC 23 - Truncate a file at the present file position.
;
; Arguments:            X = Channel number 
;
; Arguments returned:   None
;
            .export FTRUNC

FTRUNC:     jsr     ASSIGNED            ; Make sure the channel is assigned
            bmi     @RETURN             ; If not, just return
            jsr     GETCURCHKLK         ; Get current file and ensures it's not locked
            jsr     GETFPSECT           ; Calculate sector, track and head of file position
            jsr     SETEND              ; Sets current position as file size
            ldy     CBLOCK              ; Get current block
            lda     (BATP),y            ; Get next block for current
            cmp     #$F9                ; Is a valid block? (meaning not last, mostly)
            bcs     @UFINFO             ; No, we're done
            tax                         ; Yes, save block in X
            lda     #BLKLAST            ; Mark block as last in the series
            jsr     SETNEXTBLK          ;
            txa                         ; Recover last block
            tay                         ; And free it's chain
            jsr     FREEBLK             ;
@UFINFO:    jsr     UPDCFINFO           ; Copies back current finfo structure to file's FINFO
@RETURN:    rts


; Get MEMCOUNT characters from channel X into (MEMBUFF)
;
            .export GETMBUFF
            
GETMBUFF:   jsr     ASSIGNED        ; Get assigned device/file
            bpl     @ISFILE         ; Check if a file
            jmp     @ISDEV          ; Jump if a device

@ISFILE:    jsr     LF221
            lda     CURFINFO+_BUFF
            sta     @LF146
            sta     @LF138
            sta     @LF186
            sta     @LF132
            lda     CURFINFO+_BUFF+1
            sta     @LF147
            sta     @LF139
            sta     @LF187
            sta     @LF133
@LF123:     lda     DSTBNKCFG
            sta     BNKCTL
            lda     L00D2+1
            beq     @LF17D
            ldy     CURFINFO+_FPOS
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
@LF141:     ldx     CURFINFO+_FPOS
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
@LF15A:     lda     CURFINFO+_FPOS
            beq     @LF165
            clc
            adc     L00D2
            sta     L00D2
            bcs     @LF167
@LF165:     dec     L00D2+1
@LF167:     inc     CURFINFO+_FPOS+1
            bne     @LF16D
            inc     CURFINFO+_FPOS+2
@LF16D:     lda     DEFBNKCFG
            sta     BNKCTL
            ldx     #$00
            stx     CURFINFO+_FPOS
            jsr     RDFPSECT
            jmp     @LF123

@LF17D:     lda     L00D2
            beq     @LF19F
            ldy     CURFINFO+_FPOS
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
            sty     CURFINFO+_FPOS
@LF19F:     lda     DEFBNKCFG
            sta     BNKCTL
            jsr     UPDCFINFO
            jmp     @LF1EB

@ISDEV:     and     #$7F            ; Clear device's higher bit
            tax                     ; and use it as an index
            lda     DDTI,x          ; to get the device's driver
            sta     DRIVERP         ;
            lda     DDTI+1,x        ;
            sta     DRIVERP+1       ;
            jsr     DSTMEMOK        ; Ensures 
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


LF1F7:      lda     DEFBNKCFG

LF1F9:      sta     BNKCTL
            jsr     JDRIVERP
            ldx     DSTBNKCFG
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

; Null device driver routines
;
NULDRVI:    lda     EOF             ; Just returns "End of file"
NULDRVO:    sec                     ; Error
            rts                     ;


LF221:      jsr     DSTMEMOK        ; Ensures that destination memory is OK
            jsr     LF081
            jsr     CALREMFSIZ      ; Calculate remaining file size from current pos
            bcs     @CONT           ; Cont if remaining size >= 0
            jsr     ERROR46         ; System crash: file ordinal check error
            ; Not reached
@CONT:      sec                     ; MEMCOUNT > remaining file size?
            lda     FILEPOS         ;
            sbc     MEMCOUNT        ;
            lda     FILEPOS+1       ;
            sbc     MEMCOUNT+1      ;
            lda     FILEPOS+2       ;
            sbc     #$00            ;
            bcs     LF246           ; No
            lda     FILEPOS         ; Yes, adjust MEMCOUNT to file size
            sta     MEMCOUNT        ;
            lda     FILEPOS+1       ;
            sta     MEMCOUNT+1      ;
LF246:      lda     MEMCOUNT        ; Set bytes to transfer
            sta     L00D2           ;
            lda     MEMCOUNT+1      ;
            sta     L00D2+1         ;
            rts

; Check that destination of a memory copy is permitted and sets
; the destination bank config
;
; Does not return if fails
;
DSTMEMOK:   lda     DSTBANK         ; Check destination bank
            bne     @DESTOK         ; Is not system bank, so don't check for
                                    ; protected memory
            lda     MEMBUFF+1
            bne     @NOTZP          ; Jump if dest not in ZP
            lda     MEMBUFF         ; Check if odestrig is in reserved ZP space
            cmp     #$B0            ;
            bcc     @DESTOK         ; Dest below $00B0 (OK)
            bcs     @DESTPRTCT      ; Dest gt or eq $00B0 (reserved CODOS space)
            ; Not reached

@NOTZP:     cmp     #$02            ; Check if dest below $0200 (reserved CODOS space)
            bcs     @DESTOK         ; Dest gt or eq $0200
@DESTPRTCT: bit     IGNORWRP        ; Is copy to CODOS space allowed?
            bmi     @DESTOK         ; Yes, go on
            jsr     ERROR17         ; No, reserved or protected memory violation
            ; Not reached
@DESTOK:    lda     MEMCOUNT        ; Check if <dest> + <count> > $FFFF
            clc
            adc     MEMBUFF
            lda     MEMCOUNT+1
            adc     MEMBUFF+1
            bcc     @DESTOK2        ; Dest beyond 0xFFFF?
            jsr     ERROR16         ; yes, <from> address greater than <to> address
            ; Not reached
@DESTOK2:   cmp     #>SYSRAM        ; Is it in SYSRAM (protected)
            bcc     @DESTOK3        ; No, go on
            lda     DSTBANK         ; Bank 0?
            bne     @DESTOK3        ; No, go on
            bit     IGNORWRP        ; Is copy to CODOS space allowed?
            bmi     @DESTOK3        ; Yes, go on
            jsr     ERROR17         ; No, reserved or protected memory violation
            ; Not reached
@DESTOK3:   nop
            ; Fall through

; Set destination bank config
;
SETDBNKCFG: lda     DSTBANK         ; Get destination bank
            and     #$03            ; Mask out non-bank bytes
            eor     DEFBNKCFG
            sta     DSTBNKCFG       ; Store destination bank config
            rts


; Output MEMCOUNT characters from (MEMBUFF) to channel X
;
            .export OUTMBUFF

OUTMBUFF:   jsr     ASSIGNED        ; Ensure that channel X is assigned
            bpl     @ISFILE         ; Check if it is a file
            jmp     DOUTMBUFF       ; Its a device, output (MEMBUFF) to channel
            ; Not reached

@ISFILE:    jsr     SETDBNKCFG
            jsr     GETCURCHKLK
            lda     CURFINFO+_BUFF
            sta     LF2EA
            sta     LF2D6
            sta     LF2DC
            sta     LF341
            lda     CURFINFO+_BUFF+1
            sta     LF2EB
            sta     LF2D7
            sta     LF2DD
            sta     LF342
LF2C2:      lda     DSTBNKCFG
            sta     BNKCTL
            lda     MEMCOUNT+1
            bne     LF2CF
            jmp     LF336

LF2CF:      ldy     CURFINFO+_FPOS
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
LF2E3:      ldx     CURFINFO+_FPOS
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
LF2FA:      lda     CURFINFO+_FPOS
            beq     LF305
            clc
            adc     MEMCOUNT
            sta     MEMCOUNT
            bcs     LF307
LF305:      dec     MEMCOUNT+1
LF307:      lda     DEFBNKCFG
            sta     BNKCTL
            jsr     WRFPSECT
            inc     CURFINFO+_FPOS+1
            bne     LF316
            inc     CURFINFO+_FPOS+2
LF316:      ldy     #$00
            sty     CURFINFO+_FPOS
            jsr     LEF05
            bcc     LF375
            jsr     SETEND
            lda     BLKSCTOF
            cmp     SCTBLKM1
            bne     LF2C2
            jsr     GETFREEB
            ldy     CBLOCK
            jsr     SETNEXTBLK
            jmp     LF2C2

LF336:      lda     MEMCOUNT
            beq     LF35E
            ldy     CURFINFO+_FPOS
            ldx     #$00
LF33E:      lda     (MEMBUFF,x)

            .byte   $99             ; sta $E000,y
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
            sty     CURFINFO+_FPOS
            lda     CURFINFO+_FLAGS
            ora     #$40            ; Set _WTFCK flag
            sta     CURFINFO+_FLAGS
LF35E:      lda     DEFBNKCFG
            sta     BNKCTL
            jsr     LEF05
            php
            bcc     LF36D
            jsr     SETEND
LF36D:      jsr     UPDCFINFO
            ldx     CHANNEL
            plp
            rts

LF375:      jsr     GETFPSECT
            lda     MEMCOUNT+1
            bne     LF382
            lda     CSECT
            jsr     READSECT
LF382:      jmp     LF2C2

; Output MEMCOUNT characters from (MEMBUFF) using device A
;
DOUTMBUFF:  and     #$7F            ; Get index
            tax                     ; To Device Driver Table for Output
            lda     DDTO,x          ;
            sta     DRIVERP         ; And set the driver pointer
            lda     DDTO+1,x        ;
            sta     DRIVERP+1       ;
            jsr     SETDBNKCFG      ; Set destination bank config
            lda     #$00            ; This is weird... This code is calculating the 2's 
            sec                     ; complement negation of MEMCOUNT (-MEMCOUNT) and
            sbc     MEMCOUNT        ; then uses INC instructions in the @LOOP until it
            sta     MEMCOUNT        ; reaches 0. I can't see any difference from
            lda     #$00            ; leaving MEMCOUNT be and using DEC instructions
            sbc     MEMCOUNT+1      ; until it reaches 0. The print routine does not
            sta     MEMCOUNT+1      ; uses MEMCOUNT as an index, which would be the
            ora     MEMCOUNT        ; only explanation.
            beq     @EMPTY
@LOOP:      jsr     DOUTMBCHAR
            inc     MEMCOUNT
            bne     @LOOP
            inc     MEMCOUNT+1
            bne     @LOOP
@EMPTY:     sec
            ldx     CHANNEL
            rts

; Outputs char at (MEMBUFF) to device using previously set JDRIVERP
;
DOUTMBCHAR: ldx     DEFBNKCFG
            stx     BNKCTL
            ldx     #$00
            lda     (MEMBUFF,x)
            jsr     JDRIVERP
            lda     DSTBNKCFG
            sta     BNKCTL
            inc     MEMBUFF
            bne     LF3D1
            inc     MEMBUFF+1
LF3D1:      rts

; Get the first free block available
; Marks  it as last block in the series, updates (BATP),_BLAST
; and returns block number in A
;
GETFREEB:   ldy     #_BLAST         ; Get last allocated block
            lda     (BATP),y        ;
            tay                     ; 
@NEXTB:     iny                     ; Get next block
            lda     (BATP),y        ; Check if free
            bne     @NEXTB          ; No, check next.
            cpy     #$F9            ; Are we past the allocated blocks space?
            bcc     @FOUND          ; No, then we found it
            ldy     #$00            ; Yes, start again from the beginning, looking
                                    ; for free blocks
@NEXTD:     iny                     ; Get next block
            lda     (BATP),y        ; Check if free
            bne     @NEXTD          ; No, check next
            cpy     #$F9            ; Are we past the allocated blocks space?
            bcc     @FOUND          ; No, then we found it
            lda     CURFINFO+_FPOS+1
            bne     @LF3F1
            dec     CURFINFO+_FPOS+2
@LF3F1:     dec     CURFINFO+_FPOS+1
            dec     CURFINFO+_FPOS
            jsr     SETEND
            jsr     UPDCFINFO       ; Updates file's FINFO structure
            jsr     ERROR38         ; Diskette is full; all blocks already allocated.
            ; Not reached
@FOUND:     lda     #BLKLAST        ; Mark block as last in the series
            jsr     SETNEXTBLK      ;
            tya
            ldy     #_BLAST
            sta     (BATP),y
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
            jmp     RDSECTATR12
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
            lda     FINFOTBL+_DRIVE,x ; Get drive of file
            cmp     CURRDRV         ; Is it ours?
            bne     @CNEXT          ; No, check next
            ldx     CHANNEL         ; Recover channel
            jsr     FREECH          ; And free it
@CNEXT:     ldx     CHANNEL         ; Get next channel
@NEXT:      dex                     ;
            bpl     @LOOP           ; Loop until no more 
            ldx     CURRDRV         ; Mark drive as closed
            lda     #$00            ;
            sta     ODRIVES,x       ;
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
            lda     FINFOTBL+_DRIVE,x       ; Get drive
            cmp     SAVEDRV         ; Is it our drive
            bne     @NEXT           ; No, check next
            lda     #FLUNUSED       ; 
            sta     FINFOTBL+_FLAGS,x ; Invalidate
            sta     IOCHTBL,y       ; Unassign channel
@NEXT:      dey
            bpl     @LOOP
            ldx     SAVEDRV         ;
            lda     #$00            ; Close drive
            sta     ODRIVES,x       ;
            ldy     SAVEY6          ; Restore Y
            rts

; ASSIGN channel 0 to file or device
;
            .export ASSIGN0

ASSIGN0:      ldx     #$00
            ; Fall through

; ASSIGN channel X to file or device
;
; CURRDRV contains the drive number if we are assigning a file and the device
;    name if it is a device
;
            .export ASSIGN

ASSIGN:     jsr     CLRASSIGNF      ; Clears assign flag and returns CURRDRV in X and A
            cmp     NDRIVES         ; Check if a file or a device
            bcc     @ISFILE         ; Valid drive, it is a file
            jmp     ASSIGNDEV       ; Not a drive number, so it is a device

@ISFILE:    sec
            ror     ASSIGNFLAG      ; Sets bit 7: It is an existing file
            jsr     FEXIST          ; Check if file exists
            bne     @NEWFIL         ; No, new file
            jmp     FASSIGN         ; Yes, assign file

@NEWFIL:    lsr     ASSIGNFLAG      ; Sets bit 6: It is a new file
            ldx     CURRDRV         ; Get current drive
            jsr     EXSENSEDRV      ; Sense drive X status command
            bit     ST0             ; Get status register 0
            bvc     @WRITABLE       ; Check if write protected
            jsr     ERROR21         ; New file on write-protected diskette
            ; Not reached
@WRITABLE:  ldy     #_BNENT         ; Get number of files on disk
            lda     (BATP),y        ;
            cmp     #MAXFILES+1     ; Have we reached the maximum?
            bcc     @AVAIL          ; No, still room for more
            jsr     ERROR39         ; Diskette is full; no room left in directory
            ; Not reached
@AVAIL:     jsr     GETFREEB        ; Get the first free block
            sta     BATPTR          ; Stores it into the directory entry
            jsr     GETAFTNTRY      ; Find a free entry in the active files table,
                                    ; assigns the DEVICE number and copy the entry
                                    ; to CURFINFO
            beq     @ISNEW          ; New entry? Yes, go on
            jsr     ERROR45         ; No, there shouldn't be an active entry for
                                    ; that drive/block!
                                    ; System crash: directory/file table check error
            ; Not reached
@ISNEW:     jsr     INITFILE        ; Init file size and file position
            lda     DRCTRYPNT+1     ; Get sector of first free entry
            jsr     RDSECTATR12     ; Read sector from track 12 (directory)
            ldy     DRCTRYPNT       ; Index to the entry in the sector
            dey                     ; Back one byte, as DRCTRYPNT points to the file
                                    ; name
            ldx     #$00            ; Copy the new DIRENT to the directory buffer
@LOOP:      lda     DIRENT,x        ;
            sta     DIRBUF,y        ;
            iny                     ;
            inx                     ;
            cpx     #$10            ; Last byte?
            bcc     @LOOP           ; No, next one
            jsr     WRTRCK12        ; Write changes to disk
            lda     #$80            ; Set flag to "Normal file"
            sta     CURFINFO+_FLAGS ;
            jsr     UPDFINCHAN      ; Update active file and I/O channel tables
            lda     #FHDRLEN        ; Header length
            sta     MEMCOUNT        ;
            lda     #$00            ;
            sta     MEMCOUNT+1      ;
            lda     #<FILEHDR       ; File header data address
            sta     MEMBUFF         ;
            lda     #>FILEHDR       ;
            sta     MEMBUFF+1       ;
            ldx     CHANNEL         ; Output to file channel
            jsr     OUTMBUFF        ;    (write to new file)
            jsr     WRFPSECT
            jsr     UPDCFINFO       ; Update FINFO entry in active files table
            ldy     #_BNENT         ; Get number of files on disk
            lda     (BATP),y        ;
            clc
            adc     #$01
            jsr     SETNEXTBLK
            jmp     WRTBAT

FASSIGN:    ldx     DIRPOINT
            lda     $E50E,x
            sta     BATPTR
            jsr     GETAFTNTRY
            beq     LF542
            jsr     LF084
LF542:      lda     #FHDRLEN        ; Inits file pointer to the first data byte
            jsr     SETFILEP        ;   (just past the 64 byte file header)
            jsr     RDFPSECT
            lda     CURFINFO+_FLAGS
            bne     LF560
            ldx     #$02
            ldy     #$13
LF552:      lda     (CURFINFO+_BUFF),y
            sta     CURFINFO+_FSIZE,x
            dey
            dex
            bpl     LF552
            ldy     #$10
            lda     (CURFINFO+_BUFF),y
            sta     CURFINFO+_FLAGS
LF560:      lda     CURFINFO+_FLAGS        ; Get flags
            ora     #$C0
            sta     ASSIGNFLAG
            ; Fall through

; Update active file and I/O channel tables
;
UPDFINCHAN: jsr     UPDCFINFO       ; Update FINFO entry
            ldx     CHANNEL         ; Update channel
            lda     DEVICE          ; With the device/file number
            sta     IOCHTBL,x       ;
            rts


; Assign device in CURRDRV to a channel
;
ASSIGNDEV:  ldy     #$08            ; Search for the device into the DNT
@LOOP:      lda     DNT,y
            cmp     CURRDRV
            beq     @FOUND
            dey
            bpl     @LOOP
            jsr     ERROR11         ; Missing or illegal device or file name
            ; Not reached
@FOUND:     tya                     ; Compose device number
            asl     a               ;
            ora     #$80            ;
            ldx     CHANNEL         ;
            sta     IOCHTBL,x       ; Assign the device to the CHANNEL
            sta     DEVICE          ; And set current device
            rts

; Assigns channel 0 to an existing file
; Fails if file does not exist
;
            .export FOPEN0

FOPEN0:     ldx     #$00
            ; Fall through

; Assigns channel X to an existing file
; Fails if file does not exist
;
            .export FOPEN

FOPEN:      jsr     CLRASSIGNF      ; Clears assign flag and returns CURRDRV in X and A
            jsr     DRVVALIDO       ; Check that drive X is valid and open
            jsr     FEXIST          ; Check if file exists
            beq     @OPEN           ; Yes, go open (assign) it
            bit     ISCMDFLG        ; Is it a command?
            bpl     @ISFIL          ;  no, display "File not found"
            jsr     ERROR01         ;  yes, display "Command not found"
            ; Not reached
@ISFIL:     jsr     ERROR02         ; File not found
            ; Not reached
@OPEN:      jmp     FASSIGN         ; Go and assign channel
            ; Not reached


; Clear ASSIGN flag and return current drive in A and X
;
CLRASSIGNF: lda     CURRDRV         ; Get current drive
            sta     SAVEA2          ; Save it
            jsr     FREECH          ; Free channel
            lda     #$00            ; Clear ASSIGN flag
            sta     ASSIGNFLAG      ;
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
            jsr     ZEROFILEP
            jsr     RDFPSECT
            lda     CURFINFO+_FSIZE+1
            ora     CURFINFO+_FSIZE+2
            bne     @LF5F9
            lda     CURFINFO+_FSIZE
            cmp     #$41
            bcs     @LF5F9
            jmp     LF639

@LF5F9:     ldy     #$13
            ldx     #$02
@LF5FD:     lda     CURFINFO+_FSIZE,x
            cmp     (CURFINFO+_BUFF),y
            beq     @LF60B
            sta     (CURFINFO+_BUFF),y
            lda     CURFINFO+_FLAGS
            ora     #$40            ; Set _WTFCK flag
            sta     CURFINFO+_FLAGS
@LF60B:     dey
            dex
            bpl     @LF5FD
            jsr     LF084
            ldx     CURFINFO+_DRIVE
            lda     BATCHG,x
            bpl     @LF61C
            jsr     WRTBAT
@LF61C:     lda     #FLUNUSED
            ldx     DEVICE
            sta     FINFOTBL+_FLAGS,x
@DOFREE:    ldx     CHANNEL         
            lda     #$00
            sta     IOCHTBL,x
@RETURN:    rts


; Delete file in FNAMBUF from drive X
;
            .export FDELETE

FDELETE:    jsr     DRVVALIDO       ; Check that drive X is valid and open
            stx     CURRDRV         ; Set as current drive
            jsr     FOPEN0          ; Assigns channel 0 to file (fails if not found)
            jsr     CHKLCK          ; Ensure it is not locked
LF639:      ldy     #$15
            lda     (CURFINFO+_BUFF),y
            jsr     RDSECTATR12
            ldy     #$14
            lda     (CURFINFO+_BUFF),y
            tax
            sta     DIRPOINT
            ldy     #$01
LF64A:      lda     DIRBUF,x
            cmp     #$2E
            beq     LF65C
            cmp     (CURFINFO+_BUFF),y
            beq     LF658
            jsr     ERROR50         ; System crash: Directory redundancy check failed.
LF658:      iny
            inx
            bne     LF64A
LF65C:      ldx     DIRPOINT
            lda     #$00
            sta     DIRBUF,x
            jsr     WRTRCK12
            ldy     #_BNENT         ; Get number of files on disk
            lda     (BATP),y        ;
            sec
            sbc     #$01
            jsr     SETNEXTBLK
            ldy     CURFINFO+_BATPT
            jsr     FREEBLK
            lda     #FLUNUSED
            ldx     DEVICE
            sta     FINFOTBL+_FLAGS,x
            ldx     #$09
LF680:      lda     IOCHTBL,x
            cmp     DEVICE
            bne     LF68D
            lda     #$00
            sta     IOCHTBL,x
LF68D:      dex
            bpl     LF680
            rts

; Free chain of blocks starting at block Y
;
FREEBLK:    lda     (BATP),y        ; Get next block
            tax                     ; Save it
            lda     #$00            ; Mark current as free
            jsr     SETNEXTBLK      ;
            cpx     #$F9            ; Is next a vaild block?
            bcs     @UPDBAT         ; No, we're done
            txa                     ; Yes, continue with that block 
            tay                     ;
            jmp     FREEBLK         ;
            ; Not reached
@UPDBAT:    jmp     WRTBAT          ; Update disk's BAT and return
            ; Not reached


; Check if the file in CURRDRV starting at BATPTR is already an active file
; If so, reuses the file (DEVICE) number to that entry and copies the latter to the
; CURFINFO structure in page zero.
;
; If not, assigns a free slot in the active files table, assigns a new file (DEVICE)
; number, copies the entry to CURFINFO and sets the flags to 0.
;
; Returns file flags in A
;
; If not found and no more free slots, raises ERROR 29 and does not return
;
GETAFTNTRY: lda     #$00            ; Inits file number
            sta     DEVICE          ;
            ldx     TOPASSIGTB      ; Get top of assigned files table

@LOOP:      txa                     ; Calculate start of this entry
            sec                     ;
            sbc     #FINFOLEN       ;
            bmi     @NOMORE         ; Start is negative, there are no more entries
            tax
            lda     FINFOTBL+_FLAGS,x ; Get flags
            beq     @NEXT             ; If unused, mark as candidate and get next entry
            lda     FINFOTBL+_DRIVE,x ; Active, get drive
            cmp     CURRDRV           ; In current drive?
            bne     @LOOP             ; No, get next entry
            lda     FINFOTBL+_BATPT,x ; Same file as current file?
            cmp     BATPTR            ;
            bne     @LOOP             ; No, get next entry
            stx     DEVICE            ; Yes, store DEVICE
            jsr     CPYCFINFO         ; Copy FINFO structure to CURINFO struct in page zero
            lda     CURFINFO+_FLAGS   ; Return flags in A
            rts                       ;

@NEXT:      stx     DEVICE          ; Save unused entry
            jmp     @LOOP           ; And go check next

@NOMORE:    ldx     DEVICE          ; Get file number
            bne     @FREE           ; If set, there is a free entry
            jsr     ERROR29         ; All buffers in use (free a chan. assigned to a file)
            ; Not reached

@FREE:      jsr     CPYCFINFO       ; Copy FINFO structure to CURINFO struct in page zero
            lda     CURRDRV         ; Get current drive
            sta     CURFINFO+_DRIVE ; Store into FINFO
            lda     BATPTR          ; Get first block of file
            sta     CURFINFO+_BATPT ; Store into FINFO
            lda     #FLUNUSED       ; Mark it as unused   
            sta     CURFINFO+_FLAGS ;
            rts


; FSCAN a file or device
;
; Arguments:            Y = Index to start of file or device name in buffer 
;                       TMPBUFP points to input buffer
;
            .export FSCAN

FSCAN:      lda     (TMPBUFP),y     ; Check if first char is alphabetic
            jsr     ISALPHA         ;
            bcs     @FILE           ; No, jump to set the flags
            tax                     ; Yes, save first char
            iny                     ; Advance to next char
            lda     (TMPBUFP),y     ; And check if it is a valid file name char
            jsr     VALFNCHR        ;
            bcc     @FILE0          ; Yes, go check the file flags
            txa                     ; Recover first char
            ldx     #$07            ; Check if in the Device Name Table
@LOOP:      cmp     DNT,x           ;
            beq     @DEVICE         ; Found, just return the device name
            dex                     ; Check next
            bpl     @LOOP           ;
            ora     #$80            ; Set device not found flag
@DEVICE:    sec                     ; Is device (carry set)
            rts

@FILE0:     dey
@FILE:      lda     DEFDRV          ; Set current drive to the default
            sta     CURRDRV         ;
            lda     #$00            ;
            sta     SCANFLG         ; inits SCANFLG
            jsr     FNAMFROMBUF     ; Copy file name from buffer pointed by (TMPBUFP),y
                                    ; to FNAMBUF
            bcs     NVALID          ; If not valid file name, jump
            lda     (TMPBUFP),y     ; Get drive number, if specified
            cmp     COLON           ; Drive separator?
            bne     @DEFDRV         ; No, default drive
@SKIP:      iny                     ; Yes, get dribe number
            lda     (TMPBUFP),y     ;
            cmp     #$20            ; Skip blanks
            beq     @SKIP           ;
            sec                     ; Convert to byte
            sbc     #$30            ;
            bcc     NVALID          ; If not a digit, not valid
            cmp     NDRIVES         ; Digit. Valid drive number?
            bcs     NVALID          ; Nope, jump
            sta     CURRDRV         ; Set as current drive
            iny                     ; And advance no next char position
@DEFDRV:    jsr     SRVINTX         ; Serve pending interrupt and get drive in X 
            lda     ODRIVES,x       ; Check if open
            bpl     NOPEN           ; No, jump
            sty     SAVEY5          ; Save Y
            jsr     EXSENSEDRV      ; Execute a Sense Drive command
            lda     ST0             ; Get status register
            and     #$40            ; Check if write protected
            lsr     a               ; and if so, sets bit 4 of flag 
            lsr     a               ;
            sta     SCANFLG         ;
            jsr     FEXIST          ; Check if file exists
            php                     ; Save flags
            lda     #$00            ; Init SCANFLG value
            ldy     SAVEY5          ; Recover Y
            plp                     ; And flags from FEXIST
            bne     SETFLG          ; Don't exist, do not set that flag
            lda     #$20            ; Set file exist bit
            bne     SETFLG          ; Always jump
NVALID:     lda     #$80            ; Set bit 7 (illegal file name)
            bne     SETFLG          ; Always jump
NOPEN:      lda     #$40            ; Set bit 6 (Drive not open)
SETFLG:     ora     SCANFLG         ; Set flag
            ora     CURRDRV         ; Add drive to bits 0 and 1
            sta     SCANFLG         ;
            clc                     ; Is file (carry clear)
            bit     SCANFLG         ; And set N and V flags with bit 7 and 6
            rts

; Search for FNAMBUF in the directory table
; Returns:
;    A == 0 if file exists
;    A != 0 if file does not exist, DRCTRYPNT point to the first empty
;           entry in the directory table
;
FEXIST:     jsr     SETBATP         ; Set BATP to the current drive's BAT
            ldx     #$00            ; Init
            stx     DRCTRYPNT+1     ;   sector and
            stx     DRCTRYPNT       ;   offset to first free entry
            inx
            stx     SECTNUM         ; Start from sector 1 (directory entries)
            stx     DIRPOINT        ; Points to filename of first entry
            ldy     #_BNENT         ; Get number of files on disk
            lda     (BATP),y        ;
            sta     NFILES          ; And save them
            bne     @NOEMPTY        ; Disk empty?. No, continue
            inc     DRCTRYPNT+1     ; Yes, first free entry is in the
            inc     DRCTRYPNT       ; first one in the first sector
@RETURN:    lda     DRCTRYPNT       ;
            rts                     ;

@NOEMPTY:   jsr     RDSECTNTR12     ; Read SECTNUM sector into DIR buffer
@ENTRYLP:   ldy     DIRPOINT        ; Get pointer to filename of entry
            ldx     #$00            ; Init FNAMBUF index
@CMPLP:     lda     DIRBUF,y        ; Get first char
            beq     @DELETED        ; If it is a NULL, it is deleted
            cmp     #'.'            ; Extension separator?
            beq     @CHKEXT         ;   yes, go compare extension
            cmp     FNAMBUF,x       ; Compare with our file name
            bne     @NEXT           ; Different, go check next entry
            inx                     ; Equal, go check next char
            iny                     ;
            jmp     @CMPLP          ;
            ; Not reached
@CHKEXT:    lda     FNAMBUF,x       ; Get char of our file name
            cmp     #'.'            ; Extension?
            bne     @NEXT           ; No, then different. Go check next entry
            lda     $E501,y         ; Compare the exension letter
            cmp     FNAMBUF+1,x     ;
            bne     @NEXT           ; Different, go check next entry
            lda     #$00            ; Return 0 and clear DRCTRYPNT
            sta     DRCTRYPNT       ;
            rts                     ; and done.

@NEXT:      dec     NFILES          ; Decrement file count
            bne     @NDIRE          ; Still files left, advance to next entry
            lda     DRCTRYPNT       ; No more files, were there any deleted?
            bne     @RETURN         ; Yes, return 
            beq     @NXTFREE        ; No, advance to next entry and report it
                                    ; as free
            ; Not reached
@NDIRE:     jsr     NXTDIRENT       ; Point DIRPOINT to next entry in buffer
            jmp     @ENTRYLP        ; And process it

@DELETED:   lda     DRCTRYPNT+1     ; First deleted entry?
            bne     @NDIRE          ; No, go get next entry
            lda     SECTNUM         ; Yes, store sector
            sta     DRCTRYPNT+1     ;
            lda     DIRPOINT        ;    and pointer
            sta     DRCTRYPNT       ;    of first deleted (free) entry 
            lda     NFILES          ; Any more files?
            bne     @NDIRE          ;    yes, go get next entry
            jmp     @RETURN         ;    no, return with deleted pointer in A

@NXTFREE:   jsr     NXTDIRENT
            jmp     @DELETED


; Point to next directory entry and loads sector into dir buffer
; if necessary. Updates DIRPOINT and SECTNUM.
;
NXTDIRENT:  clc                     ;
            lda     DIRPOINT        ; Get current position
            adc     #$10            ; Advance size of entry
            sta     DIRPOINT        ;
            bcc     @RETURN         ; If we are past the current sector
            inc     SECTNUM         ; advance to the next
            jsr     RDSECTNTR12     ; And load it into the buffer
@RETURN:    rts

; Inits file size (64, as it is the length of the file header) and file
; pointer for new files
;
INITFILE:   lda     #FHDRLEN            ; Set file size to 64
            sta     CURFINFO+_FSIZE     ;
            lda     #$00                ;
            sta     CURFINFO+_FSIZE+1   ;
            sta     CURFINFO+_FSIZE+2   ;
            ; Fall through

; Zeroes file pointer
;
            .export ZEROFILEP

ZEROFILEP:  lda     #$00
            ; Fall through

; Sets file pointer to value in A
;
SETFILEP:   sta     CURFINFO+_FPOS
            lda     #$00
            sta     CURFINFO+_FPOS+1
            sta     CURFINFO+_FPOS+2
            rts

; Copy file name from buffer pointed by (TMPBUFP) to FNAMBUF
;
            .export FNAMFROMBUF0

FNAMFROMBUF0:
            ldy     #$00
            ; Fall through    

; Copy file name from buffer pointed by (TMPBUFP),y to FNAMBUF
;
            .export FNAMFROMBUF

FNAMFROMBUF:
            ldx     #$00
@LOOP:      lda     (TMPBUFP),y
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
            lda     (TMPBUFP),y         ;
            iny                     ; Advance 1 pos
            bne     @STOREXT        ; And go to store extension (always jumps)
            ; Not reached

@NOEXT:     lda     #'.'            ; Store default extension
            sta     FNAMBUF,x       ;
            lda     DEFAULTEXT      ;
@STOREXT:   sta     FNAMBUF+1,x     ; Store extension
            lda     FNAMBUF         ; Check that the first char of file name
            jsr     ISALPHA         ; is a letter
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
            jsr     ISALPHANUM      ; Validate that it is alphanumeric
@RETURN:    rts


; Character validation routines.
; Character in A
; Return carry clear if vaild, carry set if not
;
; Check if char is alphanumeric
;
ISALPHANUM: jsr     ISNUM           ; Is it a number?
            bcc     RETVAL          ; Yes, return CC 
            ; Fall through


; Check if character is alphabetic
;
            .export ISALPHA

ISALPHA:    cmp     #'A'            ; Is it a letter
            bcs     CHKZ            ; Maybe, complete check
NOVAL:      sec                     ; Definitely not, return CS
            rts                     ;
CHKZ:       cmp     #'Z'+1          ; Is it 'Z' or lower?
RETVAL:     rts                     ; Yes, return CC; No, return CS

; Check if character is numeric
;
            .export ISNUM

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

; Converts word at P0SCRATCH into its 4-char ascii hex representation
; at  (OUTBUFP),y
;
            .export HEXWORD0

HEXWORD0:   ldx     #$00

; Converts word at P0SCRATCH,x into its 4-char ascii hex representation
; at  (OUTBUFP),y
;
            .export HEXWORD

HEXWORD:    lda     P0SCRATCH+1,x   ; Gets most significant byte
            jsr     HEXBYTE         ; Converts it
            lda     P0SCRATCH,x     ; Gets less significant byte
            ; Fall through

; Converts byte in A into its 2-char ascii hex representation
; at  (OUTBUFP),y
;
            .export HEXBYTE

HEXBYTE:    pha                     ; Save byte
            lsr     a               ; Get upper nibble
            lsr     a               ;
            lsr     a               ;
            lsr     a               ;
            jsr     NIBBLE          ; Convert it
            pla                     ; Recover byte
            ; Fall through

; Converts nibble in lower half of A into its 1-char ascii hex
; representation at  (OUTBUFP),y
;
            .export NIBBLE

NIBBLE:     and     #$0F            ; and get lower nibble
            clc
            adc     #$30            ; Adds "0"
            cmp     #$3A            ; Is it "9" or lower?
            bmi     @STORE          ; Yes, goto store it
            adc     #$06            ; Nope, add 7 (6 + carry) to get hex digit
@STORE:     sta     (OUTBUFP),y     ; And store it
            iny                     ; Next position
            rts                     ; and return


; Decode decimal ASCII string in (INPBUFP) to 16-bit value
;
            .export DECDECOD

DECDECOD:   lda     #$0A        
            jsr     LF915
LF8BF:      sec
            sbc     #$30
            bcc     LF8EB
            cmp     TMPPTR
            bcs     LF8EB
            jsr     LF8F1
            jmp     LF8BF

; Decode hexadecimal ASCII string in (INPBUFP) to 16-bit value ??
;
            .export HEXDECOD

HEXDECOD:   lda     #$10
            jsr     LF915

LF8D3:      sec                     ; Substract "0" from the char
            sbc     #'0'            ;
            bcc     LF8EB
            cmp     #$0A            ; 0 to 9
            bcc     @ISVAL          ; Yes, got valid digit

            sbc     #$07            ; Substract 7 and check if hexadecimal
            cmp     #$0A            ; A or higher?
            bcc     LF8EB
            cmp     #$10
            bcs     LF8EB

@ISVAL:     jsr     LF8F1
            bne     LF8D3
LF8EB:      rol     $029F           ; Clear flag
            jmp     GETNEXTCH       ; Return delimiter char in A and index to delimiter
                                    ; in Y
            ; Not reached

LF8F1:      pha
            stx     SAVEX1
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

LF90B:      dec     $029F
            ldx     SAVEX1
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
            beq     GETNEXTNB1      ;   get next char
@RETURN:    rts

; Get char from (INPBUFP),y+1 and return it in A
; If No more chars (NULL , ';' or EOL), zero flag is set
; Preserves carry flag
;
            .export GETNEXTCH1

GETNEXTCH1: iny
            ; Fall through

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


; Output program counter and registers to output line buffer
;
            .export OUTREGSLB

OUTREGSLB:  jsr     SETOUTBCH       ; Set output line buffer as destination
            ; Fall through

; Output program counter and registers to output buffer
;
OUTREGS:    jsr     OUTSTR
            .byte   "P=", $0
            ldx     #_PCSAVE        ; Print Program Counter as an HEX word
            jsr     HEXWORD         ;
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
            jsr     POUTBUFF02      ; Print output buffer to console
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
            jmp     POUTBUFF02      ; Print output buffer to console

LF9BA:      lda     PRGBANK
            eor     DEFBNKCFG
            sta     BNKCTL
            lda     (PCSAVE),y
            ldx     DEFBNKCFG
            stx     BNKCTL
            rts

; String of valid register names
;
            .export REGDESC

REGDESC:    .byte   "SFYXA"

; Output Y characters from (OUTBUFP) to channel 2 (console output)
; followed by a CR
;;
POUTBUFFCR02:
            .export POUTBUFFCR02

            ldx     #$02
            ; Fall through

; Output Y characters from (OUTBUFP) to channel X
; followed by a CR
;
            .export POUTBUFFCR

POUTBUFFCR: jsr     POUTBUFF
            ; Fall through

; Output a CR to channel X
;
            .export OUTCR

OUTCR:      lda     #$0D
            jmp     OUTCHAR
            ; Not reached

; Output Y characters from (OUTBUFP) to channel 2 (console output)
;
POUTBUFF02: ldx     #$02
            ; Fall through

; Output Y characters from (OUTBUFP) to channel X
;
            .export POUTBUFF

POUTBUFF:   sty     MEMCOUNT        ; Set length
            lda     #$00            ;
            sta     MEMCOUNT+1      ;
            lda     OUTBUFP         ; Set buffer pointer
            sta     MEMBUFF         ;
            lda     OUTBUFP+1       ;
            sta     MEMBUFF+1       ;
            jsr     OUTMBUFF
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
            sta     QLN             ;
            lda     INPBUFP+1       ;
            sta     QLN+1           ;
            jmp     JINLINE         ; Get entire line from keyboard and place it
                                    ; into (QLN) (which now it is INPBUFP)

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

@FROMDEV:   jsr     CHRRDPREP       ; Prepare transfer of SAVECH from device/file
            sec                     ; Set ignore memory write protection flag
            ror     IGNORWRP        ;
            jsr     GETMBUFF        ; Do the transfer
            php                     ; Save processor status
            clc                     ; Clear gnore memory write protection flag
            rol     IGNORWRP        ;

            ; Fall through

; Restore regs and processor status
;
RESTRREGS:  ldy     SAVEY3          ; Restore registers (saved by CHRRDPREP
            ldx     SAVEX3          ;  or CHRWRPREP)
            lda     SAVECH          ; Get transferred char
            plp                     ; Recover processor status
            rts

; Get one char from the console
;
JCINP:      jmp     (CINP)          ; Jump to console input routine


; Output string immediately following the JSR call
;
            .export OUTSTR

OUTSTR:     stx     $0287           ; Save X
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
            jsr     OUTCHAR         ; Print char
            jmp     LFA66           ; Loop

LFA78:      lda     TMPPTR+1        ; Push new PC to the stack
            pha                     ;
            lda     TMPPTR          ;
            pha                     ;
            ldy     $0289           ; Restore indexes
            lda     $0288           ;
            ldx     $0287           ;
            rts

; Output character in A to channel in X
;
            .export OUTCHAR

OUTCHAR:    sta     SAVECH          ; Save char
            lda     IOCHTBL,x       ; Get device/file for channel
            cmp     #$82            ; Is it console?
            bne     @TODEV          ; No, jump
            lda     SAVECH          ; Recover char
            jsr     JCOUTP          ; And output to console
            sec                     ; Set carry (Why?)
            rts
@TODEV:     jsr     CHRWRPREP       ; Prepare transfer to memory buffer
            jsr     OUTMBUFF        ; Outputs memory buffer to device
            php
            jmp     RESTRREGS       ; Restore regs (saved by CHRWRPREP) and processor
                                    ; status
            ; Not reached

; Prepare transfer of one character to SAVECH from MEMBUF
;  Note: Does not make sense, as this entry point is called just from GETCHAR
;        A contains the file/device and gets overwritten anyway with the transfer
; 
CHRRDPREP:  sta     SAVECH          ; Save char
            ; Fall through

; Prepare transfer of one character from SAVECH to MEMBUF
;
CHRWRPREP:  stx     SAVEX3          ; Save X
            sty     SAVEY3          ; Save Y
            lda     #$01            ; Just one char
            sta     MEMCOUNT        ;
            lda     #$00            ;
            sta     MEMCOUNT+1      ;
            lda     #<SAVECH        ; Set buffer address
            sta     MEMBUFF         ;
            lda     #>SAVECH        ;
            sta     MEMBUFF+1       ;
            rts

JCOUTP:     jmp     (COUTP)             ; Jump to console output routine

; Encode 16-bit value to decimal ASCII string.
;
            .export DECWORD

DECWORD:    stx     $028C
            lda     #$00
            sta     $029F
            ldx     #$06
LFACB:      lda     LFB05,x
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
LFAE8:      sec
            ror     $029F
LFAEC:      jsr     LFAFE
LFAEF:      jsr     LFBAA
            ldx     $028D
            dex
            dex
            bpl     LFACB
            ldx     $028C
            lda     P0SCRATCH
LFAFE:      clc
            adc     #$30
            sta     (OUTBUFP),y
            iny
            rts

LFB05:      .word   $000A
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

; Get a byte from the command line and returns in A
;
            .export GETBYTE

GETBYTE:    stx     SAVEX8          ; Save X
            ldx     #_BYTRES        ; Use BYTRES as result buffer
            jsr     EVALEXP         ; Evaluate expression
            lda     BYTRES+1        ; > $FF ?
            beq     @CONT           ; No, it is a valid byte
            jsr     ERROR18         ; <value> out of range (greater than $FF or less than 0).
@CONT:      lda     BYTRES          ; Get result
            ldx     SAVEX8          ; restore X
            rts


; X is the position at P0SCRATCH
;
; EValuate expression
;
            .export EVALEXP

EVALEXP:    lda     #$00
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
            jsr     GETNEXTNB       ; Get next non-blank
            cmp     PERIOD          ; Decimal mark?
            beq     @LFC59          ; Yes, get decinal value
            cmp     DOLLAR          ; Hexadecinal mark?
            bne     @LFC0A          ; No, but it is hexa anyways, as it is nor decimal
            jsr     GETNEXTNB1      ; Get next non-blank (first value digit)
@LFC0A:     jsr     HEXDECOD        ; Decode hex value
            bcc     @LFC61
@LFC0F:     jsr     LFBBC
            lda     $0291
            bne     @LFC32
            jsr     LFB0D
@LFC1A:     jsr     LFBBC
@LFC1D:     jsr     GETNEXTNB
            ldx     #ARITHTBLLEN-1  ; Check if it is
@NXOP:      cmp     ARITHMOP,x      ; an arithmetic operator
            beq     @LFBF3          ; Yes, go to ...
            dex                     ; Check next operator
            bpl     @NXOP           ;
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
            jsr     DECDECOD
            bcs     @LFC0F
@LFC61:     rts

; Save memory block with header into current file at current position
;
; Overlay in A
;
            .export SAVEBLK

SAVEBLK:    sta     SAVEDHDR+_OVLAY ; Set overlay
            lda     #$00            ; Reserved: always 0
            sta     SAVEDHDR+_RSRVD ;
            lda     SVDFROMBNK      ; Get <from> bank
            bit     SAVDESTPF       ; Check if <dest> is set
            bpl     @NODEST         ; No, use <from> info
            lda     SVDDESTBNK
@NODEST:    sta     SAVEDHDR+_MEMBK ; Set memory bank
            lda     #SVDMAGIC       ; Set saved files magic number
            sta     SAVEDHDR+_MAGIC ;
            lda     MEMCOUNT        ; Get <to> address
            sec                     ; And convert to bytes count
            sbc     MEMBUFF         ;
            sta     MEMCOUNT        ;
            lda     MEMCOUNT+1      ;
            sbc     MEMBUFF+1       ;
            sta     MEMCOUNT+1      ;
            bcs     @TOOK           ; If Cy clear, <from> > <to>
            jsr     ERROR16         ; <from> address greater than <to> address.
@TOOK:      inc     MEMCOUNT        ; MEMCOUNT = <to> - <from> + 1
            bne     @CONT           ;
            inc     MEMCOUNT+1      ;
@CONT:      bit     SAVDESTPF       ; Check if <dest> is set
            bpl     LFC9D
            jsr     SETDSTBUFF
LFC9D:      ldx     #$02
            bit     SAVENTRYPF
            bpl     LFCA6
            ldx     #$06
LFCA6:      jsr     LFBAA
            ldx     #$05
LFCAB:      lda     P0SCRATCH,x
            sta     SAVEDHDR+_PNTRS,x
            dex
            bpl     LFCAB
            jsr     SVDRWPREP       ; Prepare read/write of "saved file" header
            ldx     CHANNEL
            jsr     OUTMBUFF
            jsr     CPYPTRS
            bit     SAVDESTPF
            bpl     LFCC7
            jsr     SETDSTBUFF
LFCC7:      ldx     CHANNEL
            lda     SVDFROMBNK
            sta     DSTBANK
            jsr     OUTMBUFF
            lda     #$00
            sta     DSTBANK
            rts

; Prepare read/write of "saved file" header
;
SVDRWPREP:  lda     #SAVEDHDRLEN
            sta     MEMCOUNT
            lda     #$00
            sta     MEMCOUNT+1
            lda     #<SAVEDHDR
            sta     MEMBUFF
            lda     #>SAVEDHDR
            sta     MEMBUFF+1
            rts

; Copy pointers from "Saved file" header to page 0
;
CPYPTRS:    ldx     #$05
@LOOP:      lda     SAVEDHDR+_PNTRS,x
            sta     P0SCRATCH,x
            dex
            bpl     @LOOP
            rts

; Copy DESTBUFF pointer to MEMBUFF pointer 
;
SETDSTBUFF: ldx     #$01
@LOOP:      lda     DESTBUFF,x
            pha
            lda     MEMBUFF,x
            sta     DESTBUFF,x
            pla
            sta     MEMBUFF,x
            dex
            bpl     @LOOP
            rts


; Loads $58 block from file
;
;    Overlay in A
;
            .export LOADSVD

LOADSVD:    sta     SAVEA3              ; Save A in temporary storage
            jsr     LD58HDR             ; Load "saved file" hdr from file at current pos
            bcs     @RETCS              ; Return with error
            lda     SAVEDHDR+_OVLAY     ; Check if overlay is same as requested
            cmp     SAVEA3              ;
            bne     @RETCS              ; No, return with error
            jsr     CPYPTRS             ; Yes, copy pointers from header to page 0
            lda     SAVEDHDR+_MEMBK     ; Set destination bank from header
            sta     DSTBANK             ;
            bit     SAVDESTPF           ; Was a <dest> specified?
            bpl     @CONT               ; No, continue
            jsr     SETDSTBUFF          ; Set <dest> as loading address 
            lda     SVDDESTBNK          ; Set <dest> bank as loading bank
            sta     DSTBANK             ;
@CONT:      ldx     CHANNEL             ; Load block into memory
            jsr     GETMBUFF            ;
            rts

@RETCS:     sec
            rts

; Load "saved file" header from file at current position
;
LD58HDR:    jsr     SVDRWPREP       ; Prepare read/write of "saved file" header
            lda     #$00            ; Set destination bank
            sta     DSTBANK         ;
            sec                     ; Set ignore memory write protection flag
            ror     IGNORWRP        ;
            jsr     GETMBUFF        ; Get header from file into MEMBUFF
            bcs     @RETURN         ; If error, return
            rol     IGNORWRP        ; Clear ignore memory write protection flag
            lda     SAVEDHDR+_MAGIC ; Check that the magic number is correct
            cmp     #$58            ;
            beq     @RETOK          ; Yes, jump to return OK
            sec                     ; Return error
@RETURN:    rts
            ; Not reached
@RETOK:     clc                     ; Return OK
            rts

; Assigns default input device and set input buffer
;
DEFSETINPB: lda     #$00
            ldx     CHANN1          ; Get input channel device
            sta     CHANN1          ; And clears it
            jsr     UNASSIGN
            ; Fall through

; Set input buffer to input line buffer and sets input channel
; to console if not set
;
            .export SETINPBCH

SETINPBCH:  lda     CHANN1          ; Get input channel device
            bne     @CONT           ; If set, go on
            lda     #$82            ; If not, set default (console)
            sta     CHANN1          ;
@CONT:      ldy     #$00
            ; Fall through

; Set input buffer to input line buffer
;
            .export SETINPB

SETINPB:    lda     INPLBUF         ; Set input line buffer
            sta     INPBUFP         ;
            lda     INPLBUF+1       ;
            sta     INPBUFP+1       ;
            rts

; Set output buffer to output line buffer and sets output channel to
; console if not set
;
; Also clears Y
;
            .export SETOUTBCH

SETOUTBCH:  lda     CHANN2          ; Get output channel device
            bne     @CONT           ; If set, continue
            lda     #$82            ; If not, set to console
            sta     CHANN2          ;
@CONT:      ldy     #$00
            ; Fall through

; Set output buffer to output line buffer
;
            .export SETOUTB

SETOUTB:    lda     OUTLBUF         ; Set output line buffer
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
            jmp     SETOUTBCH         ; And set output buffer

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
            lda     #FLUNUSED       ; Close or free entry
            sta     FINFOTBL+_FLAGS,x ;
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
            ; Not reached

; Serve any K-1013 pending interrupts and returns current drive in X
;
SRVINTX:    jsr     SRVINT          ; Serve any k-1013 pending interrupt (if any)
            ldx     CURRDRV         ; Get drive
            rts

.if  CODOS2_VER = 17
EXSENSEDRV17:
            jsr     EXSENSEDRV
@LOOP:      jsr     NVALID
            inx
            bne     @LOOP
            rts
.endif

            .export OVLORG, SYSRAM, DIRBUF, BOOTP

OVLORG      := __OVERLAYS_START__   ; Origin of CODOS overlays

USRRAM      := __USRRAM_START__     ; K-1013 onboard user RAM
SYSRAM      := __SYSRAM_START__     ; K-1013 onboard system RAM

DIRBUF      := SYSRAM+$500          ; Directory buffer

BOOTP       := __BOOTPROM_START__+$0E ; Boot from PROM

            .end
