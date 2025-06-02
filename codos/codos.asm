; Disassembly of CODOS 2.0 by Eduardo Casino (mail@eduardocasino.es)
;

; da65 V2.18 - Ubuntu 2.19-1
; Created:    2023-11-25 21:28:31
; Input file: codos.bin
; Page:       1

.ifdef mtu
            .include "monomeg.inc"
.endif
            .include "codos.inc"

            .importzp QLN

.ifdef mtu

EXINBNK     := __EXINBNK            ; Location of the exec in bank routine

            .export SEEIO

SEEIO       := __SEEIO              ; I-O space enable semaphore

.endif ; MTU

            .export CNTRLC

NMIPRC      := __NMIPRC             ; Jump to NMI processor
IRQBRK      := __IRQBRK             ; Jump to IQR and BRK processor
WARMRS      := __WARMRS             ; Jump to operating system warm reset entry
CNTRLC      := __CNTRLC             ; Jump executed when CNTRL-C is entered from console

.ifdef mtu
            ; Relevant keyboard and text display driver entry points
            ;
            .export KEYSTR, LEGTBL

KEYSTR      := __KEYSTR             ; (256 bytes) Function key substitute string table
LEGTBL      := __LEGTBL             ; (64 bytes) Function key legend table

.endif ; MTU

            ;   Disk Controller Registers
            ;
            .export HSRCW, ADMA

HSRCW       := SYSRAM+$1FE8         ; Read  - Hardware Status Read
                                    ; Write - Hardware Control Write
ADMA        := SYSRAM+$1FEA         ; Write - Set DMA Address Register

            ;   uPD765 Registers
            ;
MSTR        := SYSRAM+$1FEE         ; Read  - uPD765 Main Status Register
DATR        := SYSRAM+$1FEF         ; R/W   - uPD765 Data Register

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
            .export DESTBUFF, L00D2, BYTRES, TMPPTR, PCSAVE, FILEPOS

P0SCRATCH:  .res 2                  ; $C1-$C2 (word)
MEMBUFF:    .res 2                  ; $C3-$C4 (word) Pointer to buffer for memory copy operations
MEMCOUNT:   .res 2                  ; $C5-$C6 (word) Counter for memory copy operations
TMPBUFP:    .res 2                  ; $C7-$C8 (word) Used in FNAMFROMBUF and as <entry> buffer
DESTBUFF:   .res 2                  ; $C9-$CA (word) <dest> buffer when specified in command
INPBUFP:    .res 2                  ; $CB-$CC (word) Pointer to input buffer
OUTBUFP:    .res 2                  ; $CD-$CE (word) Pointer to output buffer
FILEPOS:    .res 3                  ; $CF-$D1 (24 bit) File position. Also used to
                                    ;    calculate remaining file size from file pos

            ; This memory area is shared by the switch bank and exec routine and as
            ; a temporary storage space for different routines

.ifdef mtu
SWITCHNJMP:
.endif
L00D2:      .res 1                  ; $D2  $D2-$D9 is a temporary area
            .res 1                  ; $D3       with different uses, like
TMPVAL:     .res 2                  ; $D4-$D5       the switch and jump routine
BYTRES:     .res 2                  ; $D6-$D7 (word) Result for GETBYTE function
TMPPTR:     .res 2                  ; $D8-$D9 (word) Temporary pointer

            ; This is where the Switch and exec routine jumps after switching
            ; banks

PCSAVE:     .res 2                  ; $DA-$DB (word) Program counter

            .export CURFINFO, BATP

CURFINFO:   .tag FINFO

BATP:       .res 2                  ; $E9-$EA (word) Pointer to Block Allocation Table for current drive

            .export CMDLIDX

CMDLIDX:    .res 1                  ; $EB  (byte) Current char position in command line
INTSVA:     .res 1                  ; $EC  (byte) Accumulator save during SVC or IRQ processing.
    
            ; $ED - $EF : Global RAM used by CODOS
 
            .exportzp ERRNUM, SVCENB

ERRNUM:     .res 1                  ; $ED  Error number for user-defined error recovery.
SVCENB:     .res 1                  ; $EE  ADDRESS OF SVC ENABLE FLAG
SAVEACC:    .res 1                  ; $EF  TODO: Unknown

            .segment "scratch1"

            .export NFILES, TEMP2, TEMP4, SAVEY7, SAVEAX, SAVEA5, SAVEA6, SAVEY8

TEMP5:      .res 1                  ; $0283
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
SAVEA5:     .res 1                  ; $029D
SAVEA6:     .res 1                  ; $029E
PRLEADING0: .res 1                  ; $029F Print leading 0s flag. Used in number
                                    ;   to ASCII conversion routines.
                                    ;   If bit 7 = 1, print leading zeroes
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


YOUT        := USRRAM+$127D ; "Y" output (console and printer) entry point
                            ; Must be set by hand at STARTUP.J with
                            ; SET D27D=20 21 E6 (jsr JCOUT)
PRTOUT      := USRRAM+$1280 ; Printer output entry point

            .segment "codos"

            ; Loadable file data
            ;
            .export LDHEADER

LDHEADER:   .byte   $58             ; CODOS loadable file header byte
            .byte   $00             ; Memory overlay
            .byte   $00             ; Memory bank
            .byte   $00             ; Reserved
            .addr   JCOLDST         ; Entry point
            .addr   CODOS           ; Load address
            .word   CODOS_SIZE      ; Memory image size

CODOS:
            ; Copyright header
            ;
            .byte "MTU-130 CODOS 2.0", $0D
            .byte "COPYRIGHT (C) 1981, MICRO TECHNOLOGY UNLIMIMITED", $0D
            .byte "PO BOX 12106, 2806 HILLSBOROUGH ST.", $0D
            .byte "RALEIGH, NC 27605 USA", $0D
            .byte "Written by Bruce D. Carbrey", $0D
            .byte "ASM 1/18/82 patch 6/14/82", $0D, $0D, $0D, $20

            ; Jump table (page 179)
            ;

            .export JCOLDST, JOUTCH, JCPSEUDREGS, JERROR37, JINLINE

JCOLDST:    jmp     COLDST
JWARMST:    jmp     WARMST
JGETKEY:    jmp     GETKEY          ; Console character input
JOUTCH:     jmp     OUTCH           ; Console character output
JTSTKEY:    jmp     TSTKEY          ; Console Key-depressed test subroutine
            jmp     NMIPROC
            jmp     IRQPROC
JCPSEUDREGS:jmp     CPSEUDREGS      ; Jump to copy pseudo-registers to scratch RAM
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

FINFOTBL:   .byte   $06

            .byte   $00, $00, $00, $00, $00, $00, $00, $00, $00
            .word   SYSRAM+$200         ; File buffer
            dma     byte, SYSRAM+$200   ; $88 SYSRAM+$200 K-1013 DMA encoded
            .byte   $00

            .byte   $00, $00, $00, $00, $00, $00, $00, $00, $00
            .word   SYSRAM+$100         ; File buffer
            dma     byte, SYSRAM+$100   ; $84 SYSRAM+$100 K-1013 DMA encoded
            .byte   $00

            .byte   $00, $00, $00, $00, $00, $00, $00, $00, $00
            .word   SYSRAM              ; File buffer           
            dma     byte, SYSRAM        ; $80 SYSRAM K-1013 DMA encoded
            .byte   $00

            .byte   $00, $00, $00, $00, $00, $00, $00, $00, $00
            .word   USRRAM+$1700    ; File buffer
            dma     byte, USRRAM+$1700  ; $5C USRRAM+$1700 K-1013 DMA encoded
            .byte   $00

            .byte   $00, $00, $00, $00, $00, $00, $00, $00, $00
            .word   USRRAM+$1600        ; File buffer
            dma     byte, USRRAM+$1600  ; $58 USRRAM+$1600 K-1013 DMA encoded
            .byte   $00

            .byte   $00, $00, $00, $00, $00, $00, $00, $00, $00
            .word   USRRAM+$1500        ; File buffer
            dma     byte, USRRAM+$1500  ; $54 USRRAM+$1500 K-1013 DMA encoded
            .byte   $00

            .byte   $00, $00, $00, $00, $00, $00, $00, $00, $00
            .word   USRRAM+$1400    ; File buffer
            dma     byte, USRRAM+$1400  ; $50 USRRAM+$1400 K-1013 DMA encoded
            .byte   $00

            .byte   $00, $00, $00, $00, $00, $00, $00, $00, $00
            .word   USRRAM+$1300        ; File buffer
            dma     byte, USRRAM+$1300  ; $4C USRRAM+$1300 K-1013 DMA encoded
            .byte   $00

; K-1013 DMA encoded addresses for drive BATs
;
BATDMAT:    dma     byte, SYSRAM+$400   ; $90
            dma     byte, SYSRAM+$300   ; $8C
            dma     byte, SYSRAM+$200   ; $88
            dma     byte, SYSRAM+$100   ; $84

            .export STACKP, PROCST, YREG, XREG, ACCUM

; Processor registers at interrupt
;
STACKP:     .byte   $FF             ; Stack pointer
PROCST:     .byte   $04             ; Processor Status
YREG:       .byte   $00             ; Y
XREG:       .byte   $00             ; X
ACCUM:      .byte   $00             ; Accumulator

.ifdef mtu
            .export PRGBANK, DATBANK, DSTBANK

PRGBANK:    .byte   $00             ; Current program bank
DATBANK:    .byte   $00             ; Current data bank
BNKCFG:     .byte   $00             ; Current bank configuration
SVCSTAT:    .byte   $00             ; SVC status (enabled/disabled) at interrupt?
DSTBANK:    .byte   $00             ; Destination bank num for memory copy operations?
DSTBNKCFG:  .byte   $7F             ; Destination bank config

            .export NEWBNK, CHGBNKFLG, SVDFROMBNK, SVDDESTBNK

NEWBNK:     .byte   $00             ; Where GADDRBNK stores bank
CHGBNKFLG:  .byte   $00             ; If set, switches to NEWBNK
SVDFROMBNK: .byte   $00             ; Bank for saved file <from> bank
SVDDESTBNK: .byte   $00             ; Bank for saved file <dest> bank

            .export DEFBNKCFG

DEFBNKCFG:  .byte   $7F             ; Default bank configuration

.else ; !MTU

SVCSTAT:    .byte   $00             ; SVC status (enabled/disabled) at interrupt?

.endif
            .export DEVICE

CHANNEL:    .byte   $00             ; Current channel for I/O operations
DEVICE:     .byte   $00             ; Current device/file for I/O operations

            .export DIRPOINT, CURRDRV

DIRPOINT:   .byte   $00             ; Pointer to current directory entry
CURRDRV:    .byte   $00             ; Current disk drive number

; The following 64 bytes comprise the file header that prepends every file
; in the CODOS file system
;

            .export FILEHDR, DIRENT

FILEHDR:
; Directory entry
;
DIRENT:     .byte   $01             ; Always $01
            .byte   "NONAME.Z  ", $00, $00, ".", $00
            .byte   $00             ; Pointer to first block in BAT


            .byte   $80             ; Flag: $80 : Normal read/write file
                                    ;       $C0 : Locked file
            .byte   $40, $00, $00   ; File size (24 bits) includes header size ($40)

            .word   $00             ; Relative pointer to the directory entry

            .byte   "*UNDATED*", $00 ; Today's date

            ; Reserved for future upgrades:
            ;
            .byte   $00, $00, $00, $00, $00, $00, $00, $00
            .byte   $20, $20, $20, $20, $20, $20, $20, $20
            .byte   $20, $20, $20, $20, $20, $20, $20, $20
            .byte   $20, $20, $20, $20

            ; Boot sector data

            .byte   $19                 ; Last sector to load
            dma     byte, SYSRAM+$600   ; $98 DMA encoded address for loading the system
            .word   JCOLDST-1           ; Entry point - 1
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
            .export _FORMAT

_SPECIFY    = SPECIFY-CMDTBL
_RECAL      = RECAL-CMDTBL
_SEEK       = SEEK-CMDTBL
_SENSEINT   = SENSEINT-CMDTBL
_RDWR       = RDWR-CMDTBL
_FORMAT     = FORMAT-CMDTBL
_SENSEDRV   = SENSEDRV-CMDTBL


CMDTBL:

            .export HDSTEP, HDLOAD

SPECIFY:    .byte   $03             ; Command length 3
            .byte   $03             ; Specify
.if CODOS2_VER = 14
HDSTEP:     .byte   $AF             ; Stepping Rate Time $A (6ms), Head Unload Time $F (240ms)
HDLOAD:     .byte   $30             ; Head Load Time $30 (48ms)
.else
HDSTEP:     .byte   $DF             ; Stepping Rate Time $D (3ms), Head Unload Time $F (240ms)
HDLOAD:     .byte   $26             ; Head Load Time $26 (38ms)
.endif

RECAL:      .byte   $02             ; Command length 2
            .byte   $07             ; Recalibrate
RECDRVHD:   .byte   $00             ; Drive and head:
                                    ;   XXXXX 0 00
                                    ;         | ||
                                    ;         | ++-> Drive (0-3)
                                    ;         +----> Head

SEEK:       .byte   $03             ; Command length 3
            .byte   $0F             ; Seek
SEKDRVHD:   .byte   $00             ; Drive and head (see above)
SEKTRACK:   .byte   $00             ; Track

SENSEINT:   .byte   $01             ; Command length 1
            .byte   $08             ; Sense interrupt status

RDWR:       .byte   $09             ; Command length 9
                                    ; Read command. Same sequence is used for write,
RDWRCMD:    .byte   $46             ; storing $45 at this location
                                    ; MFM, no MT, no skip
RWRDRVHD:   .byte   $00             ; Disk and Head info
RWRTRACK:   .byte   $00             ; C- Cylinder
RWRHEADN:   .byte   $00             ; H - Head
RWRSECTR:   .byte   $00             ; R - Sector
            .byte   $01             ; N - 256 bytes/sector
RWREOSEC:   .byte   $00             ; EOT sector
            .byte   $0E             ; GPL
            .byte   $FF             ; DTL (ignored as N != 0)

            .export FMTDRVHD

FORMAT:     .byte   $06             ; Command length 6
            .byte   $4D             ; Format command (MFM)
FMTDRVHD:   .byte   $00             ; HD = 0, Drive = 0
            .byte   $01             ; 256 bytes sectors
            .byte   $1A             ; 26 sectors/track
            .byte   $34             ; Gap 3
            .byte   $00             ; Filler byte

SENSEDRV:   .byte   $02             ; Command length 2
            .byte   $04             ; Sense drive status command   
SENDRVHD:   .byte   $00             ;

; Disk status registers
;
            .export DSKSTAT

DSKSTAT:
ST0:        .byte   $00             ;
ST1:        .byte   $00             ;
            .byte   $00             ;
            .byte   $00             ;
            .byte   $00             ;
            .byte   $00             ;
            .byte   $00             ;

            .export NDRIVES, DRVNFO, ODRIVES

NDRIVES:    .byte   $02             ; Number of disk drives in system, 1 to 4

DRVNFO:     .byte   $00             ; Drive info table (one byte per drive)
            .byte   $00             ;     0b10000000 : Two sides
            .byte   $00             ;
            .byte   $00             ;

            ; Open drives flag table

ODRIVES:    .byte   $00             ;
            .byte   $00             ;
            .byte   $00             ;
            .byte   $00             ;

            ; BAT changes flag table

BATCHG:     .byte   $00             ; Bit 7 = 1 if BAT has been modified
            .byte   $00             ; Same for drive 1
            .byte   $00             ; Same for drive 2
            .byte   $00             ; Same for drive 3

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

            .export CSECT, CHEAD, DSFLAG, SECTNUM

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

            .export DEFSVCFLAG, ASSIGNFLAG

IRQFLAG:    .byte   $00             ; Flag. If bit 7 = 1, interrupt is IRQ (0 is BRK)
NMIFLAG:    .byte   $00             ; Flag. If bit 7 = 1, interrupt is NMI
DEFSVCFLAG: .byte   $00             ; Flag. If bit 7 = 1, SVC enabled by default
ASSIGNFLAG: .byte   $00             ; Flag. If bit 6 = 1, it is file
                                    ;       If bit 6 = 0, it is adevice
                                    ;       If bit 7 = 1, it is an existing file
  
            .export ULINE, SCOLON, COLON, QUOTE, CARET, DEFAULTEXT, NUMOVL

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

            .export DEFDRV, TOPASSIGTB, NUMFNAMES, DUMPBYTES, DUMPCHANN

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

            .export INPLBUF, LBUFADDR, LBUFSIZE, INTSRVP, ERRRCVRYP, SECSTRK, CMDFNP

INPLBUF:    .word   __INPLBUF       ; Pointer to start of system input line buffer.
OUTLBUF:    .word   __OUTLBUF       ; Pointer to start of system output line buffer
LBUFADDR:   .word   __LBUFADDR      ; Pointer to large transient buffer for COPYF, ETC.
LBUFSIZE:   .word   __LBUFSIZE      ; Size (NOT. final address) of large transient buffer.
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

BPBANK:     .byte   $FF             ; Breakpoint bank or BP set flag
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

; System cold start
;
.proc COLDST
            cld                     ; Clear decimal mode
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
LOOP:       lda     STARTUPNAM,x    ;
            sta     DIRENT+DIRE::FNAM,x
            dex                     ;
            bpl     LOOP            ;
            ldx     #$00            ; Set drive 0
            stx     CURRDRV         ;
            jsr     FEXIST          ; Check if file exists
            bne     CONT            ; No, continue to warm start
            ldx     #$01            ; Assigns channel 1 to file (fails if not found)
            jsr     FOPEN           ;
CONT:       jmp     WARMST          ; Continue to warm start
            ; Not reached
.endproc

; Init system variables, pointers and jump tables
;
.proc SYSINIT
            ldx     #$00            ; Set DMA direction bit to read
            stx     HSRCW           ;
.ifdef mtu
            stx     PRGBANK         ; Init current program bank
            stx     DATBANK         ; Init current data bank
.endif
            stx     ERRNUM          ; Init error number
            stx     OVLORG          ; Init overlays
            stx     DEFDRV          ; Init default drive
.ifdef mtu
            lda     #$7F            ; Bank 0, write disable $8000 to $BFFF 
            sta     DEFBNKCFG       ; 
            jsr     INIMMAP
.else
            jsr     CLEARBRK
.endif
            lda     #$EA            ; Init the JPOSTERR jump with NOPs
            ldx     #$02            ;
LOOP1:      sta     JPOSTERR,x      ;
            dex                     ;
            bpl     LOOP1           ;

            ; Copy jump table 
            ;
            ldx     #$0B            ; Table size - 1 
LOOP2:      lda     JMPTBL,x        ;
            sta     NMIPRC,x        ;
            dex                     ;
            bpl     LOOP2           ;

            lda     #'C'            ; Current ASCII default file extension character ("C")
            sta     DEFAULTEXT      ;
            ; Fall through
.endproc

; SVC 26 - Reinstate normal error processing by CODOS
;
            .export RESTERRP

.proc RESTERRP
            lda     #<ERRRCVRY      ; Set pointer to error recovery routine
            sta     ERRRCVRYP       ;
            lda     #>ERRRCVRY      ;
            sta     ERRRCVRYP+1     ;
            rts
.endproc

; CTRL-C processor
;
.proc CTRLCPROC
            cld
            ldx     #$FF            ; Is this a bug and it should be TXS?
            tsx                     ;
            jsr     SYSINIT         ; Init system variables, pointers and jump tables
            lda     #$00            ; Useless, gets overwritten below
            ldy     NDRIVES         ; Get number of drives in system
            dey                     ; Last drive
LOOP:       tya                     ; Close drive
            jsr     CLDRIVE         ;
            dey                     ; Close next
            bpl     LOOP            ;
            jsr     DEFSETOUTB      ; Assigns default output device and set output buffer
            jsr     INITIO          ; Clear screen and set default values of display
            jsr     EXSENSEINT      ; Serve any pending interrupt
            jsr     OPENDRV0        ; Open system drive
            jsr     OUTSTR          ; Print reset message
            .byte   "RESET.", $00
            jmp     WARMST          ; Do a warm start
            ; not reached
.endproc

; NMI Processor
;
.proc NMIPROC
            sta     INTSVA          ; Save accumulator
            lda     #$00            ; Unprotect K-1013 SYSRAM
            sta     HSRCW           ;
            sec                     ; Set NMI flag
            ror     NMIFLAG         ;
            jmp     INTPROC         ; Jump to interrupt processor
            ; Not reched
.endproc

; IRQ Processor
;
.proc IRQPROC
            sta     INTSVA          ; Save accumulator on entry
            pla                     ; Get and save back processor status register
            pha                     ;
            and     #$10            ; Check if BRK
            bne     SVCINT          ; Yes, could be an SVC
            lda     INTSVA          ; Recover accumulator
            jmp     (INTSRVP)       ; Jump to user-defined interrupt service routine
            ; Not reached
.endproc

; Interrupt service routine
;
.proc INTSRV
            lda     #$00            ; Unprotect K-1013 SYSRAM
            sta     HSRCW           ;
            sta     NMIFLAG         ; Clear NMI flag
            ; Fall through
.endproc

; Interrupt processor
;
.proc INTPROC
            sec                     ; Set IRQ flag
            ror     IRQFLAG         ;
            pla                     ; Get and save processor status register
            sta     PROCST          ;
            pla                     ; Get and save
            sta     PCSAVE          ; Program counter (low)
            pla                     ; Program counter (high) in A
            jmp     INTCONT         ; Jump to interrupt processor
            ; Not reached
.endproc

; Service interrupt routine
;
.proc SVCINT
            lda     #$00            ; Unprotect K-1013 SYSRAM
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
.endproc

; Interrupt service routine (continued)
;
.proc INTCONT
            sta     PCSAVE+1        ; Save program counter (high)
            stx     XREG            ; Save registers on entry
            sty     YREG            ;
            cld
            tsx                     ; Save stack pointer on entry
            stx     STACKP          ;
.ifdef mtu
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
.else ; MTU
            jsr     CLEARBRK
.endif ; ! MTU
            lda     INTSVA          ; 
            sta     ACCUM
            bit     IRQFLAG         ; Is it an IRQ?
            bmi     DOIRQ           ; Yes, do it
                                    ; No, it is a BP
            sec                     ; Set the "print registers at BP, not at error"
            ror     PRBPREGS        ;   flag
            ldx     #$02            ; Check which Break Point it is
LOOP:       
            lda     BPBANK,x        ; Is it the same program bank?
.ifdef mtu
            cmp     PRGBANK         ;
            bne     NEXT            ; No, check next BP
.else
            bmi     NEXT            ; BP not set
.endif
            lda     BPADDRLO,x      ; Is it the same address?
            cmp     PCSAVE          ;
            bne     NEXT            ; No, check next
            lda     BPADDRHI,x      ; Maybe,
            cmp     PCSAVE+1        ; lets see the MSB
            bne     NEXT            ; No, check next
            ldy     #$00            ; Yes
.ifdef mtu
            lda     PRGBANK         ; Get program bank
            eor     DEFBNKCFG       ;    What is it doing here?
            sta     BNKCTL          ;
.endif
            lda     BPOP,x          ; Get saved instruction byte at BP 
            sta     (PCSAVE),y      ; and restore it to the PC
.ifdef mtu
            lda     DEFBNKCFG       ; TODO: Again
            sta     BNKCTL          ;    Why?
.endif            
            lda     #$FF            ; Invalidate/clear BP
            sta     BPBANK,x        ;
            jsr     DEFSETINPB      ; Set input buffer
            jsr     DEFSETOUTB      ; Set output buffer
            jsr     OUTSTR
            .byte   $0d, "BP", $00
            jmp     OUTSTAT         ; Print status (Registers, pointers) and warm-start
            ; Not reached
NEXT:       dex                     ; Decrement BP
            bpl     LOOP            ; And repeat

            ; If we are here, either it is an SVC or just a BRK

.ifdef mtu
            lda     PRGBANK         ; SVC only available in bank 0
            bne     DOIRQ           ;
.endif
            lda     SVCENB          ; Are SVC enabled?
            sta     SVCSTAT         ; Save SVC status
            bpl     DOIRQ           ; No, should be BRK
            jmp     SVCPROC         ; Yes, serve it

DOIRQ:      jsr     DEFSETINPB      ; Set input buffer
            jsr     DEFSETOUTB      ; Set output buffer
            bit     IRQFLAG         ; Is it an IRQ?
            bpl     DOBRK           ; No, then it is a BRK
            jsr     OUTSTR          ; Yes, print
            .byte   $0D, "INTERRUPT (", $00
            bit     NMIFLAG         ; Is it an NMI?
            bpl     NOTNMI          ; No, print IRQ
            jsr     OUTSTR          ; Yes, print NMI
            .byte   "NMI)", $00     ;
            jmp     OUTSTAT         ; Go print status
            ; Not reached

NOTNMI:     jsr     OUTSTR          ; Print IRQ
            .byte   "IRQ)", $00     ;
            jmp     OUTSTAT         ; Go print status

DOBRK:      jsr     OUTSTR          ; Print BRK
            .byte   $0D, "BRK", $00 ;

OUTSTAT:    jsr     OUTSTR          ; Print registers at interrupt
            .byte   ", ", $00       ;
            jsr     OUTREGS         ;
            jmp     WARMST          ; And warm start
            ; Not reached
.endproc

; Error routines
;
            .export ERROR01, ERROR02, ERROR03, ERROR04, ERROR05, ERROR06, ERROR07, ERROR08
            .export ERROR09, ERROR10, ERROR11, ERROR12, ERROR13, ERROR14, ERROR15, ERROR16
            .export ERROR17, ERROR18, ERROR19, ERROR20, ERROR21, ERROR22, ERROR23, ERROR24
            .export ERROR25, ERROR26, ERROR27, ERROR28, ERROR29, ERROR30, ERROR31, ERROR32
            .export ERROR33, ERROR34, ERROR35, ERROR36, ERROR37, ERROR38, ERROR39, ERROR40
            .export ERROR41, ERROR42, ERROR43, ERROR44, ERROR45, ERROR46, ERROR47, ERROR48
            .export ERROR49, ERROR50
            
.ifdef mtu
            .export ERROR51, ERROR52

ERROR52:    inc     ERRNUM          ; Missing or illegal function key number
ERROR51:    inc     ERRNUM          ; Missing or illegal memory bank number

.endif

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
ERROR27:  	inc     ERRNUM          ; <destination> address missing or illegal
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
            jmp     (ERRRCVRYP)     ; And go to error recovery routine

; Error recovery routine
;
            .export ERRRCVRY

.proc ERRRCVRY
            pha                     ; Save accumulator
            lda     #$00            ; Unprotect K-1013 SYSRAM
            sta     HSRCW           ;
            cld
.ifdef mtu
            jsr     INIMMAP         ; Set default memory config
.else
            jsr     CLEARBRK
.endif
            bit     PERRPFLG        ; Error was during print error processing?
            bpl     OUTERR          ; No, go print error
            pla                     ; Yes, just do a warm start
            jmp     WARMST          ;

OUTERR:     pla                     ; Recover A. Save registers at error:
            sta     ERRORA          ; Save A
            stx     ERRORX          ; Save X
            sty     ERRORY          ; Save Y
            tsx                     ; Save SP
            stx     ERRORS          ;
            php                     ; Save processor status reg
            pla                     ;
            sta     ERRORP          ;
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
            bpl     NOCMDERR        ; No, don't print command line info
PRCMD:      lda     (INPBUFP),y     ; Yes, print the command line
            cmp     #$0D            ; End?
            beq     GETCMDIDX       ; Yes, exit loop
            jsr     OUTCHAR         ; No, print char
            iny                     ; And go for the next one
            bne     PRCMD           ;
GETCMDIDX:  jsr     OUTCR           ; New line
            ldy     CMDLIDX         ; Recover index in command line processing
            beq     OUTARROW        ; If it is at the beginning, skip
            lda     #' '            ; Print spaces til the index position
PSPACE:     jsr     OUTCHAR         ;
            dey                     ;
            bne     PSPACE          ;
OUTARROW:   lda     #'^'            ; Print an "arrow" pointing to the character of the
            jsr     OUTCHAR         ; command CODOS was going to examine before error
            jsr     OUTCR           ; 
            jmp     OUTLONG         ; Print long error message

NOCMDERR:   bit     PRBPREGS        ; Should we print registers at BP, not at error?
            bmi     PRREGS          ; Yes, go for it
            bit     NOPRREGS        ; Should we print registers at all?
            bmi     OUTLONG         ; No, go print human readable message
            lda     ERRADDR         ; Yes, prepare variables:
            sta     PCSAVE          ; Program counter at error
            lda     ERRADDR+1       ;
            sta     PCSAVE+1        ;
            ldx     #$04            ; Stack, Processor status, Y, X and A
CPYREG:     lda     ERRORS,x        ;
            sta     STACKP,x        ;
            dex                     ;
            bpl     CPYREG          ;

PRREGS:     jsr     OUTREGSLB       ; Print registers

OUTLONG:    bit     PERRPFLG        ; Are we here as part of the print error processing? 
            bpl     WARMST          ; No, just warm start
            ldx     #$0B            ; Yes, get file with error messages
CPYFNAM:    lda     SYSERRMNAM,x    ;
            sta     DIRENT+DIRE::FNAM,x
            dex                     ;
            bpl     CPYFNAM         ;
            inx                     ; X == 0
            stx     CURRDRV         ; Set drive 0
            jsr     FEXIST          ; Check if file exists
            bne     WARMST          ; No, skip message display
            jsr     FOPEN0          ; Assigns channel 0 to file (fails if not found)
GETMSG:     ldx     #$00            ; Get entire line from error message file
            jsr     GETLINE         ;   returns length in A
            bcs     RETURN          ; If error, just free channel and return
            dec     ERRNUM          ; Repeat until we reach
            bne     GETMSG          ; The error message correponding to ERRNUM
            tay                     ; Transfer line length to Y
            tax                     ;   and X
CPYLINE:    lda     (INPBUFP),y     ; Copy the line (message) to the output buffer
            sta     (OUTBUFP),y     ;
            dey                     ;
            bpl     CPYLINE         ;
            txa                     ; Restore line length in A
            tay                     ;   and transfer to Y
            ldx     #$02            ; Print new line to console
            jsr     OUTCR           ;
            jsr     POUTBUFFCR02    ; Print output buffer to console (length in Y)
RETURN:     jsr     FREECH0         ; Free channel and return
.endproc

; Post-error user routine
;
; May be used for jumping to an alternate warm start routine or do some additional
; error handling: NOTE: AUTOTERM.C inserts a jmp $2800 here
;
            .export JPOSTERR

.proc JPOSTERR
            nop                     ;
            nop                     ;
            nop                     ;
            ; Fall through
.endproc

; System warm start routine
;
            .export WARMST

.proc WARMST
            cld                     ;
            lda     #$00            ; Unprotect K-1013 SYSRAM
            sta     HSRCW           ;
            jsr     CKCMDPR         ; Load command processor if not loaded yet
            jmp     CMDPROC         ; And jump to it
            ; Not reached
.endproc

; Execute command, making sure that the command processor is loaded first
;
            .export CKCMDEXEC

.proc CKCMDEXEC
            jsr     CKCMDPR         ; Make sure that command processor is loaded
            jmp     CMDEXEC         ; Execute command
            ; Not reached
.endproc

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

.proc NEXT
            jsr     GETPC           ; Get program counter from command args
.ifdef mtu
            bit     CHGBNKFLG       ; Is there a bank switch
            bpl     SKIP            ; No, skip it
            lda     NEWBNK          ; Yes, switch to the new bank
            sta     PRGBANK         ;
            sta     DATBANK         ;
.endif
SKIP:       jmp     CONTBP          ; Continue with command execution
.endproc

; GO Command
;
; DESCRIPTION:  Begin execution of a machine-language program in memory.
; SYNTAX:       GO [<addr>]
; ARGUMENTS:    <addr>=starting address. Defaults to current value of the Program
;               Counter (P), as displayed by the REG command.
;
            .export GOCMD

.proc GOCMD
            jsr     GETPC           ; Get program counter from command args
.ifdef mtu
            lda     NEWBNK          ; Switches to new bank
            sta     PRGBANK         ;
            sta     DATBANK         ;
.endif
            ; Fall through
.endproc

; Execute program in memory (common to GO and external commands)
;
            .export EXCMD

.proc EXCMD
.ifdef mtu
            ldx     #$7F            ; Set default bank configuration
            stx     DEFBNKCFG       ;
.endif
            ldx     DEFSVCFLAG      ; Get default SVC state (enabled or disabled)
            stx     SVCSTAT         ; And set it as current
            ldx     #$FF            ; Discard stack
            bit     SVC13FLG        ; Was this invoked by SVC 13?
            bpl     EXECCMD         ; No, skip and go to normal command exec
            ; Fall through
.endproc

; Common code for NEXT and GO commands, continue after BP
;
            .export CONTBP

.proc CONTBP
            jsr     PREPEXEC        ; Sets stack pointer, memory bank config and
                                    ; memory protection, sets registers at invocation,
                                    ; copies the switch and jump routine to its page 0
                                    ; location.
.ifdef mtu
            jmp     SWITCHNJMP      ; Switch bank and jump to saved PC to continue
                                    ; execution
.else
            jmp     JUMP        ; Jump to saved PC to continue execution
.endif

.endproc

; Continuation of EXCMD, normal command execution
;
.proc EXECCMD
            stx     STACKP          ; Discard stack
            jsr     PREPEXEC        ; Sets stack pointer, memory bank config and
                                    ; memory protection, sets registers at invocation,
                                    ; copies the switch and jump routine to its page 0
                                    ; location.
.ifdef mtu
            jsr     EXINBNK         ; Switches to bank, execs code and restore bank
.else
            jsr     EXEC            ; Execs code
.endif
            php                     ; Save flags
            cld                     ;
            lda     #$00            ; Unprotect K-1013 SYSRAM
            sta     HSRCW           ;
.if  ::CODOS2_VER = 17
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
.ifdef mtu
            lda     #$7F            ; Set up the default memory map config
            sta     DEFBNKCFG       ;
.endif
            jmp     WARMST          ; And fo a warm start
.endproc

; Sets stack pointer, memory bank config and memory protection,
; sets registers at invocation, and copies the switch and jump routine to
; its page 0 location.
;
.proc PREPEXEC
            pla                     ; Get return address (low)
            tay                     ; and save it in Y
            pla                     ; Get return address (high)
            bit     SVC13FLG        ; Was this invoked by SVC 13?
            bmi     SKIP            ; Yes, skip
            ldx     STACKP          ; Set stack pointer
            txs                     ;
SKIP:       pha                     ; Push back the return address
            tya                     ;
            pha                     ;
.ifdef mtu
            lda     PRGBANK         ; Set new memory bank config
            asl     a               ;
            asl     a               ;
            eor     DEFBNKCFG       ;
            eor     DATBANK         ;
            sta     BNKCFG          ;
            lda     #$7F            ;
            sta     SVIA1DIR        ;
            jsr     CPYSWNJMP       ; Copy switch and jump routine to page 0
.endif
            lda     #$00            ; Clear flags
            sta     INTCMDERR       ;   Error during internal command processing
            sta     PRBPREGS        ;   Print registers at BP instead of error
            lda     #$03            ; Protect SYS memory and DMA direction to read
            bit     UNPROTFLG       ; Unprotect SYSRAM?
            bpl     SKIP2           ; No, skip
            lda     #$01            ; Set DMA direction byte to read, no memory protection
SKIP2:      sta     HSRCW           ;
            lda     SVCSTAT         ; Restore SVC status
            sta     SVCENB          ;
            lda     ACCUM           ; Get and save A at invocation
.if  ::CODOS2_VER = 17
            sta     SAVEACC         ;
.else
            sta     INTSVA          ;
.endif
            ldy     YREG            ; Get Y at invocation
            ldx     XREG            ; Get X at invocation
            lda     PROCST          ; Get Status register at invocation
            pha                     ; Save it (so next operation does not alter it)
.ifdef mtu
            lda     BNKCFG          ; Get memory bank configuration
.endif
            plp                     ; Recover flags and return
            rts                     ;
.endproc

; Check if Command Processor is loaded, load it if not
;
.proc CKCMDPR
            lda     CMDPROC         ; Check if Command Processor is loaded
            cmp     #$D8            ; First byte should be $D8 (CLD)
            bne     LDCMDPR         ; No, go ahead and load from disk
            rts                     ; Yes, return
.endproc

; Loads Command Processor from disk
;
.proc LDCMDPR
            ldx     #$09            ; Get file name
LOOP:       lda     CMDPROCNAM,x    ;
            sta     DIRENT+DIRE::FNAM,x
            dex                     ;
            bpl     LOOP            ;
            ldx     #$00            ; Set drive 0
            stx     CURRDRV         ;
            jsr     FOPEN0          ; Assigns channel 0 to file (fails if not found)
            ldx     #$00            ;
            txa                     ; Overlay 0
            jsr     LOADSVD         ; Loads $58 segment from file
            bcc     LDNEXT          ; If OK, load next
            jsr     ERROR13         ; Not a loadable ("SAVEd") file
            ; Not reached
 LDNEXT:    ldx     #$00            ; Keep loading segments until end of file
            txa                     ; Overlay 0
            jsr     LOADSVD         ; Loads $58 segment from file
            bcc     LDNEXT          ; Continue until no more blocks
            jmp     FREECH0         ; Clse/free channel and return
.endproc

.ifdef mtu

; Init memory map config
;
            .export INIMMAP

.proc INIMMAP
            sec
            ror     SEEIO           ; Set I/O space enable semaphore
            lda     #$00            ; Set destination bank for memory copy ops?
            sta     DSTBANK         ;
            lda     DEFBNKCFG       ; Set default bank config
            sta     BNKCTL          ;
            lda     #$7F            ;
            sta     SVIA1DIR        ;
            ; Fall through
.endproc

.endif ; MTU


; Clears the break flag by forcing an rti
;
            .export CLEARBRK

.proc CLEARBRK
            lda     #>RETURN        ; Set return address to $EC11
            pha                     ;
            lda     #<RETURN        ;
            pha                     ;
            php                     ;
            rti                     ; This will return just below
RETURN:     rts
.endproc

.ifndef mtu

; Restore accumulator and jump to (PCSAVE)
;
.proc JUMP
            php
.if ::CODOS2_VER = 17
            lda     SAVEACC         ; Get A at invocation
.else
            lda     INTSVA          ; Get A at invocation
.endif
            plp                     ; Restore flags
            jmp     (PCSAVE)        ; Continue execution at  address stored in PCSAVE
.endproc

; Exec code at (PCSAVE), then restores A at entry
;
.proc EXEC
            jsr     JUMP            ; Jumps to (PCSAVE)
.if ::CODOS2_VER = 17
            sta     SAVEACC         ; Restore A (was saved by SWITCHNJMP routine)
.else
            sta     INTSVA          ; Restore A (was saved by SWITCHNJMP routine)
.endif
            rts
.endproc

.else ; defined MTU

; Copy execute in bank routine to $0100-$0112
;
            .export CPYEXINBNK

.proc CPYEXINBNK
            ldx     #EXINBNKLEN     ; Routine length
LOOP:       lda     EXINBNK_O,x     ; Copy it backwards, byte by byte
            sta     EXINBNK,x       ;
            dex                     ;
            bpl     LOOP            ;
            rts                     ;
.endproc

; Copy switch bank and exec routine to its page 0 location
;
.proc CPYSWNJMP
            ldx     #$07            ; Routine length
LOOP:       lda     SWITCHNJMP_O,x  ; Copy it backwards, byte by byte
            sta     a:SWITCHNJMP,x  ;
            dex                     ;
            bpl     LOOP            ;
            rts                     ;
.endproc

; Switches bank and jumps to PC. Copied to $D2
;
SWITCHNJMP_O:
            php                     ; Save flags
            sta     BNKCTL          ; Switch bank
.if  CODOS2_VER = 17
            lda     SAVEACC         ; Get A at invocation
.else
            lda     INTSVA          ; Get A at invocation
.endif
            plp                     ; Restore flags
SWNJMPEND:  .byte   $4C             ; JMP Absolute. As SWITCHNJMP_O is copied to $D2, it
                                    ; means it jumps to address contained in
                                    ; $DA-$DB, which is PCSAVE (saved Program Counter)
                                    ; which was set by the interrupt service routine,
                                    ; command processor or SVC processor
SWITCHNJMPLEN = SWNJMPEND - SWITCHNJMP_O ; Calculate routine length

; Exec code at specified bank, then restores default memory configuration.
; Copied to 0100-0112
;
EXINBNK_O:  jsr     SWITCHNJMP      ; Switch bank and jumps to PC
            php                     ; Save flags on exit
.if  CODOS2_VER = 17
            sta     SAVEACC         ; Restore A (was saved by SWITCHNJMP routine)
.else
            sta     INTSVA          ; Restore A (was saved by SWITCHNJMP routine)
.endif
            sta     IOENABLE        ; Enable I/O space from $BE00 to $BFFF
                                    ; (A content is irrelevant)
            lda     #$FF            ; Sets bank 0 and write enable $8000 to $BFFF,
            sta     BNKCTL          ; restoring defaultbank configuration
            sta     SVIA1DIR        ;
            plp                     ; Restore flags and return
EXINBNKEND: rts                     ;
EXINBNKLEN = EXINBNKEND - EXINBNK_O ; Calculate routine length

.endif ; MTU


;       Send command to uPD765
;       X : Command index
;
            .export SNDCMD

.proc SNDCMD
            sty     SAVEY1          ; Save Y register (restored in COMMPH)
            stx     CMDIDX          ; Save command index
            ldy     CMDTBL,x        ; Get command length

CHKBSY:     lda     #$10            ; Check if uPD765 is busy processing a command (it shouldn't)
            and     MSTR            ;
            beq     COMMPH          ; No, send command
WAITRD:     bit     MSTR            ; Yes, wait until finished
            bpl     WAITRD          ;
            bvs     READST          ; Jump if data register needs to be read
            lda     #$00            ; Otherwise, try to complete command sequence
            sta     DATR            ;
RETRY:      nop                     ; Wait a couple of cycles
            inc     N765ECNT        ; Increment controller error count
            jmp     CHKBSY          ; And try again

READST:     lda     DATR            ; Read status register
            jmp     RETRY           ; And try again
            ; Not reached
.endproc

;       Write command to uPD765 Data Register
;       X -> index to command byte in command table
;       Y -> command length 
;
.proc COMMPH
WAITRD:     lda     MSTR            ; Read uPD765 Main Status Register
            bpl     WAITRD          ; Wait until bit 7 is 1 (Ready)
            and     #$40            ; Check data direction
            beq     CONT            ; Jump if data register is to be written
            jsr     ERROR48         ; System crash: NEC 765 chip command phase error
CONT:       lda     CMDTBL+1,x      ; Write command byte
            sta     DATR            ;
            inx                     ; next command byte
            dey                     ;
            bne     WAITRD          ; Until command length
            ldy     SAVEY1          ; Restore Y register
            rts                     ; And return
.endproc

; Sense drive X status command
;
.proc EXSENSEDRV
            stx     SENDRVHD        ; Set drive into sense command
            ldx     #_SENSEDRV      ; Send Sense drive command to the disk controller
            bne     EXSENSECMD      ;
            ; Always jump
.endproc

; Sense interrupt command
;
.proc EXSENSEINT
            ldx     #_SENSEINT      ; Send Sense interrupt command to the disk controller
            ; Fall through
.endproc

; Execute SENSE type of command
;
.proc EXSENSECMD
            jsr     SNDCMD          ; Send command
            ; Fall through
.endproc

;       Read result from uPD765 Data Register
;
            .export RSLTPH

.proc RSLTPH
            ldx     #$00            ; Init disk status index
WAITRD:     lda     MSTR            ; Read uPD765 Main Status Register
            bpl     WAITRD          ; Wait until bit 7 is 1 (Ready)
            and     #$40            ; Check data direction
            bne     CONT            ; Jump if data register is to be read
            jsr     ERROR49         ; System crash: NEC 765 chip result phase error
            ; Not reached
CONT:       lda     DATR            ; Read data register
            sta     DSKSTAT,x       ;
            nop                     ; Give the controller some time
            nop                     ;
            inx                     ; Advance one pos
            lda     MSTR            ; Check if still busy
            and     #$10            ;
            bne     WAITRD          ; Yes, go get next byte
            rts                     ;
.endproc

; Send SEEK type command to uPD765 and process status
;
.proc SNDSKCMDST
            jsr     SNDCMD          ; Send command to controller

WAITINT:    lda     HSRCW           ; Wait for interrupt ( Bit 7 of HSRCW is 0)
            bmi     WAITINT         ;
            ; Fall through
.endproc

; Execute a Sense interrupt command and return
; Carry set if error, carry clear otherwise
;
.proc SNSINTST
            jsr     EXSENSEINT      ; Execute a sense interrupt command
            lda     ST0             ; Get status register 0
            cmp     #$C0            ; Return carry if error
            rts                     ;
.endproc

; Init disk drive X
;
; Performs a complete init sequence of SPECIFY and RECALIBRATE and get
; disk information (single or dual side)
;
            .export INITDRV

.proc INITDRV
            jsr     DRVVALID        ; Ensure that drive is valid
            stx     RECDRVHD        ; Set drive into recalibrate command
LOOP:       lda     HSRCW           ; Check if interrupt pending
            bmi     CONT            ; No, continue
            jsr     EXSENSEINT      ; Serve interrupt
            jmp     LOOP            ; And go check again

CONT:       ldx     #_SPECIFY       ; Send specify command
            jsr     SNDCMD          ;
            ldx     #_RECAL         ; Send recalibrate command
            jsr     SNDSKCMDST      ;
            and     #$D8            ; Delete don't care bits from ST0
            beq     GETST           ; No error, get status
            and     #$08            ; Fail: Check if ready
            bne     NOTRDY          ; Not ready
            jsr     ERROR42         ; Unformatted diskette or hardware drive fault
            ; Not reached
NOTRDY:     jsr     ERROR06         ; Drive not ready error
            ; Not reached
GETST:      lda     ST1             ; Get status register 1
            beq     GETNHD          ; All clear, go get disk sides
            jsr     ERROR41         ; Unformatted diskette or irrecoverable seek error
            ; Not reached
GETNHD:     ldx     RECDRVHD        ; Get drive
.if  ::CODOS2_VER = 17
            jsr     EXSENSEDRV17    ; Sense drive
.else
            jsr     EXSENSEDRV      ; Sense drive
.endif
            ldx     RECDRVHD        ; Get drive
            lda     ST0             ; Get status register 0
            and     #$08            ; Filter out except Two Sides flag
            beq     STORE           ; One side
            lda     #$80            ; Two sides
STORE:      sta     DRVNFO,x        ; Store info for drive (Bit 7 = 1 -> 2 sides)
            rts                     ;
.endproc

; Seeks track A on drive X, checking that drive is valid and performing
; a retry
;
            .export CKSEEKTRK

.proc CKSEEKTRK
            jsr     DRVVALIDO       ; Verify drive X is valid and open
            ; Fall through
.endproc

; Seeks track A on drive X, performing retries
;
            .export SEEKTRK

.proc SEEKTRK
            jsr     EXSEEK          ; Execute seek command (X drive, A track)
            bcs     DORTS           ; Return if OK
            jsr     INITDRV         ; Reinit drive (SPECIFY + RECALIBRATE)
            jsr     GETDRVTRK       ; Recover Drive/Track info
            sta     TRKERRNUM       ; Store track that caused last error
            jsr     EXSEEK          ; And retry
            bcs     DORTS           ; Return if OK
            jsr     ERROR41         ; Unformatted diskette or irrecoverable seek error
            ; Not reached
.endproc

; Seek track A of drive X
;
; Returns resulting drive and track in XA
;
; NOTE: In this case Carry Set means success!!
;
.proc EXSEEK
            stx     SEKDRVHD        ; Set drive for seek command
            stx     SKDRIVE         ; Save as current
            cmp     #$4D            ; Check it is a valid track
            bcc     VALID              ; 
            jsr     ERROR47         ; System crash: illegal track on disk
            ; Not reached
VALID:      sta     SEKTRACK        ; Set track for seek command
            lda     CHEAD           ; Get single or dual side disk
            beq     DOSEEK          ; Single side, head 0 (no need to change DRVHD)
            lda     #$04            ; Dual sided, select head 1
            ora     SEKDRVHD        ; Combine with drive number
            sta     SEKDRVHD        ; Update drive and head
DOSEEK:     jsr     SRVINT          ; Serve any pending interrupt (if any)
            ldx     #_SEEK          ; Send SEEK command
            jsr     SNDSKCMDST      ;
            bcs     SKERROR         ; Jump if error
            and     #$F8            ; Mask out non important bits
            cmp     #$20            ; Check for SEEK end
            beq     GETDRVTRK       ; Yes, return with Cy clear
            clc                     ;
            ; Fall through
.endproc

; Get drive and trck in XA
;
.proc GETDRVTRK
            ldx     SKDRIVE         ; Drive
            lda     SEKTRACK        ; Track from seek command
            ; Fall through
.endproc

.proc DORTS
            rts
.endproc

; Manage seek errors
;
.proc SKERROR
            and     #$03            ; Is it our drive
            cmp     SKDRIVE         ;
            bne     RETRY           ; No, close and retry
            jsr     ERROR06         ; Drive needed is not ready.
            ; Not reached

RETRY:      jsr     CLDRIVE         ; Close drive
            jmp     EXSEEK::DOSEEK  ; Seek again
            ; Not reached
.endproc

; Execute READ or WRITE command
;
.proc EXRDWR
                                    ; Set DMA buffer
            lda     CURFINFO+FINFO::DMABF
            sta     ADMA            ;
            lda     RWREOSEC        ; Get End Of Track sector
            cmp     #NSECTS         ; Compare with number of sectors
            bcc     CONT            ; Is lower, continue
            jsr     ERROR44         ; System crash: illegal sector on disk
            ; Not reached
CONT:       lda     DMADIR          ; Set DMA direction
            sta     HSRCW           ;
            ldx     #_RDWR          ; Execute READ or WRITE command
            jsr     SNDCMD          ;
WAITINT:    lda     HSRCW           ; Wait for interrupt ( Bit 7 of HSRCW is 0)
            bmi     WAITINT         ;
            jsr     RSLTPH          ; Get result
            lda     ST0             ; Get status register 0
            and     #$D8            ; Mask out don't care bits
            beq     RETCC           ; No error bit set, return carry clear
            cmp     #$40            ; If abnormal termination
            beq     CKEOC           ; go check if it is End of Cylinder
                                    ; Any other value is an error
            jsr     ERROR40         ; Unformatted diskette or drive went not-ready
            ; Not reached
CKEOC:      lda     ST1             ; Get status register 1
            and     #$B7            ; Mask out don't care bits
            cmp     #$80            ; Is it "End Of Cylinder" (normal for the K-1013)
            beq     RETCC           ; Yes, return with carry clear
            sec                     ; Any other bit, it is an error
            rts                     ;
            ; Not reached
RETCC:      clc                     ; Clear carry and return
            rts                     ;
.endproc

; Write sector A
;
            .export WRITSECT

.proc WRITSECT
            sta     RWRSECTR        ; Set sector for write command
            lda     #$45            ; Set command to write
            sta     RDWRCMD         ;
            lda     #$00            ; Set DMA to read mode
            beq     RDWRSECT        ; Always jump
            ; Not reached
.endproc

; Read sector of current file position
;
.proc RDFPSECT
            jsr     GETFPSECT       ; Get physical sector coordinates
            ; Fall through
.endproc

; Read sector A (with CHEAD, and CTRACK) sector also in CSECT
;
            .export READSECT

.proc READSECT
            sta     RWRSECTR        ; Set sector for write command
            lda     #$46            ; Set command to read
            sta     RDWRCMD         ;
            lda     #$01            ; Set DMA to write mode
            ; Fall through
.endproc

; Common code for READ and WRITE sector functions
;
.proc RDWRSECT
            sta     DMADIR          ; Set DMA direction
            cmp     DMADIR          ; Does it make sense?
            bne     SYSERR          ; Because this never jumps :/
            inc     DMADIR          ; Again, this make no sense
            cmp     DMADIR          ; Because this comparison is always NE
            bne     CONT            ; so it always jumps
SYSERR:     jsr     ERROR36         ; Illegal entry into CODOS system
            ; Not reached
CONT:       sta     DMADIR          ; Set DMA direction into
            sta     HSRCW           ; the K-1013 register
            jsr     DRVVALIDO       ; Check that drive X is valid and open
            stx     RWRDRVHD        ; Store drive into command
            stx     RWDRIVE         ; And save as drive for R/W operations
            lda     #$00            ; Set head 0
            sta     CHEAD           ;
            sta     RWRHEADN        ; Set head in READ or WRITE command
            lda     RWRSECTR        ; Get sector from READ or WRITE command
            cmp     #NSECTS         ; Is it a valid sector number?
            bcc     CNTRW           ; Yes, continue with read or write
            sbc     #NSECTS         ; Nom maybe a 2 side disk. Realculate sector
            sta     RWRSECTR        ;
            lda     DRVNFO,x        ; Check if one or two sides
            bmi     S2RW            ; Two sides, modify read/write command
            jsr     ERROR44         ; System crash: illegal sector on disk
            ; Not reached
S2RW:       lda     #$04            ; Add head number into command drive/head
            ora     RWRDRVHD        ;
            sta     RWRDRVHD        ;
            lda     #$01            ; Add header number into command head
            sta     RWRHEADN        ;
            lda     RWRSECTR        ; Get recalculated sector
CNTRW:      sta     RWREOSEC        ; And save it into command EOT sector
            lda     SEKTRACK        ; Get track from last seek
            sta     RWRTRACK        ; And store into command
            jsr     EXRDWR          ; Execute command
            bcs     FAILED          ; Failed? Retry.
RETOK:      ldx     RWDRIVE         ; OK, return drive and sector in XA
            lda     RWRSECTR        ;
            rts                     ;

FAILED:     lda     RDWRCMD         ; Check if read or write
            cmp     #$46            ; Is it a read command?
            bne     ISWRT           ; No, it's write
            inc     RDERRCNT        ; Yes, increment read error count
AGAIN:      jsr     EXRDWR          ; Execute command again
            bcc     RETOK           ; Success, return
            inc     RCERRCNT        ; Failed again, try recalibrate
            ldx     RWDRIVE         ;
            jsr     INITDRV         ; Reinit drive (SPECIFY + RECALIBRATE)
            lda     RWRTRACK        ; Recover and set track where error occurred
            sta     TRKERRNUM       ;
            jsr     CKSEEKTRK       ; Seek to it
            lda     RWREOSEC        ; Set sector of last error
            sta     SECERRNUM       ; causing a recalibrate
            lda     #$10            ; Set retries to 16
            sta     RETRIES         ;
RETRY:      jsr     EXRDWR          ; Execute command
            bcc     RETOK           ; If ok, return
            dec     RETRIES         ; No, retry
            bne     RETRY           ;
            bit     IGNORERR        ; No more retries. Ignore error set?
            bmi     RETOK           ; Yes, return
            jsr     ERROR30         ; Unformatted disk or irrecoverable read/write error
            ; Not reached
            jmp     RETOK           ; Dead code?

ISWRT:      inc     WRERRCNT        ; Increment write error count
            lda     ST1             ; Check status register 1
            and     #$02            ; Was it a write protected error?
            beq     AGAIN           ; No, retry
            jsr     ERROR10         ; Diskette is write-protected
            ; Not reached
.endproc

; Gets FINFO for current file (DEVICE), copies it into CURFINFO in page zero
; and sets CURRDRV
;
            .export GETFINFO
    
.proc GETFINFO
            jsr     CPYCFINFO       ; Copies file info structure to CURFINFO struct
                                    ; in page zero
                                    ; Get file drive
            lda     CURFINFO+FINFO::DRIVE
            sta     CURRDRV         ; Sets as current drive
            ; Fall through
.endproc

; Set the BATP to the current drive's BAT
;
            .export SETBATP

.proc SETBATP
            lda     #$00            ; BAT begins at page start 
            sta     BATP            ;
            lda     #$E4            ; Drive 0 BAT page
            sec                     ; Subsequent drive BATs are located <drivenum>
            sbc     CURRDRV         ; pages below
            sta     BATP+1          ;
            rts                     ;
.endproc

;  Set next block A for block Y into the current BAT
;
            .export SETNEXTBLK

.proc SETNEXTBLK
            stx     SAVEX7          ; Save X
            sta     (BATP),y        ; Store next block A for block Y
            lda     #$E4            ; Drive 0 BAT page
            sec                     ; Subsequent drive BATs are located <drivenum>
            sbc     BATP+1          ; So substract current BAT page to get current
            tax                     ; drive and transfer to X
            lda     #$80            ; Flag that there are changes in the BAT
            sta     BATCHG,x        ; for this drive
            lda     (BATP),y        ; Recover next block in A
            ldx     SAVEX7          ; Restore X
            rts                     ;
.endproc

; Read sector A from track 12
;    If sector == 0, loads BAT into BAT area
;    If sector != 0, into directory buffer
;
            .export RDSECTATR12

.proc RDSECTATR12
            sta     SECTNUM         ; Set sector from track 12 to read
            ; Fall through
.endproc

; Read SECTNUM from track 12
;    If sector == 0, loads BAT into BAT area
;    If sector != 0, into directory buffer
;
            .export RDSECTNTR12

.proc RDSECTNTR12
            jsr     PREPRDTR12      ; Prepare read of track 12
            jsr     READSECT        ; And read sector SECTNUM
            rts                     ;
.endproc

; Write BAT to current drive
;
            .export WRTBAT

.proc WRTBAT
            lda     #$00            ;
            sta     SECTNUM         ; BAT's sector number is 0
            ; Fall Through
.endproc

; Write to sector A of TRACK 12
;
            .export WRTRCK12

.proc WRTRCK12
            jsr     PREPRDTR12      ; Prepare the current FINFO struct for writing
                                    ; to TRACK 12
            jsr     WRITSECT        ; Write to sector SECTNUM
            cmp     #$00            ; If sector 0 (BAT), write to the BAT copy
            bne     PTR12RET        ; If not, return
            lda     #$11            ; Now write the BAT copy at sector $11
            jsr     WRITSECT        ;
            ldx     CURRDRV         ;
            lda     #$00            ; Clear the BAT changes flag for current drive
            sta     BATCHG,x        ;
            rts                     ;
.endproc

; Prepare the current FINFO struct for reading/writing to track 12 of a disk
;   If SECTNUM == 0 : Read BAT
;      SECTNUM != 0 : Read DIR
;
.proc PREPRDTR12
            lda     #$00            ; Set head 0
            sta     CHEAD           ;
            ldx     CURRDRV         ; Ensure that current drive is opened
            jsr     DRVVALIDO       ; Check that drive X is valid and open
            lda     #$0C            ; Track $0C holds directory info
            jsr     CKSEEKTRK       ;
            dma     A, DIRBUF       ; Set DMA transfer buffer to $E500 (Directory buffer)
            sta     CURFINFO+FINFO::DMABF
            lda     SECTNUM         ; If this is non-zero
            bne     PTR12RET        ;   just return
            lda     BATDMAT,x       ; Get DMA address of current drive's BAT
                                    ; Set transfer buffer to drive's BAT
            sta     CURFINFO+FINFO::DMABF
            lda     #$00            ; Clears A and return
            ; Fallthrough
.endproc


.proc PTR12RET
            rts
.endproc

; Copies file info to the current file structure in page zero
;
            .export CPYCFINFO

.proc CPYCFINFO
            ldy     DEVICE          ; Get current device (file)
            ldx     #$00            ;
LOOP:       lda     FINFOTBL,y      ; From file's FINFO
            sta     CURFINFO,x      ; To current FINFO structure
            iny                     ;
            inx                     ;
            cpx     #FINFOLEN       ; Repeat until done
            bmi     LOOP            ;
            rts
.endproc

; Copies back current finfo structure to file's FINFO, except for the
;    immutable data (buffer info and ???)
;
            .export UPDCFINFO

.proc UPDCFINFO
            ldy     DEVICE          ; Get current device (file)
            ldx     #$00            ;
LOOP:       lda     CURFINFO,x      ; From current FINFO structure
            sta     FINFOTBL,y      ; To file's FINFO
            iny                     ;
            inx                     ;
            cpx     #FINFOLEN-4     ; Exclude immutable data
            bmi     LOOP            ;
            rts                     ;
.endproc

; Check if end-of-file has been reached. Cy set if so.
;
; Compares CURFINFO+FINFO::FPOS and CURFINFO+FINFO::FSIZE
; Carry set if CURFINFO+FINFO::FPOS > CURFINFO+FINFO::FSIZE
;
.proc FEOF
            lda     CURFINFO+FINFO::FPOS    ;
            cmp     CURFINFO+FINFO::FSIZE   ;
            lda     CURFINFO+FINFO::FPOS+1  ;
            sbc     CURFINFO+FINFO::FSIZE+1 ;
            lda     CURFINFO+FINFO::FPOS+2  ;
            sbc     CURFINFO+FINFO::FSIZE+2 ;
            rts                             ;
.endproc

; Calculate remaining file size from current file pos
;   (FSIZE-FPOS) and stores it into FILEPOS
;
.proc CALREMFSIZ
            sec                             ; Clear borrow for substraction
            lda     CURFINFO+FINFO::FSIZE   ; Substracts current position form file size
            sbc     CURFINFO+FINFO::FPOS    ;
            sta     FILEPOS                 ; And store it into FILEPOS
            lda     CURFINFO+FINFO::FSIZE+1 ;
            sbc     CURFINFO+FINFO::FPOS+1  ;
            sta     FILEPOS+1               ;
            lda     CURFINFO+FINFO::FSIZE+2 ;
            sbc     CURFINFO+FINFO::FPOS+2  ;
            sta     FILEPOS+2               ;
            rts                             ;
.endproc

; Sets file end at current position (truncates file)
;
.proc SETEND
            lda     CURFINFO+FINFO::FPOS+2  ; Just copies current fpos to fsize
            sta     CURFINFO+FINFO::FSIZE+2 ;
            lda     CURFINFO+FINFO::FPOS+1  ;
            sta     CURFINFO+FINFO::FSIZE+1 ;
            lda     CURFINFO+FINFO::FPOS    ;
            sta     CURFINFO+FINFO::FSIZE   ;
            rts                             ;
.endproc

; Load overlay A from disk
;
            .export OVERLAY

.proc OVERLAY
            cmp     #$00            ; Is it an overlay
            beq     RETURN          ; No, return
            cmp     OVLORG          ; Is it the overlay loaded
            bne     LOAD            ; No, go load it
RETURN:     rts                     ; Yes, return

LOAD:       stx     SAVEX9          ; Save X        
            cmp     NUMOVL          ; Validate overlay number
            bcc     OVLOK           ; Correct, go on
            jsr     ERROR43         ; System crash: illegal system overlay number
            ; Not reached

            ; Overlays are in block 40 for dual side disks and blocks 14 and 41 for
            ; single side. Block 50 starts at sector 18 of track 12, head 0
            ; Overlays are 256 bytes long each, so numbers 1 to 8 always fit in
            ; block 40 (2048K) and numbers 9 to 16 are in second half of block 40
            ; for dual side disks and in block 41 for single side.

OVLOK:      sta     CURROVL         ; Set current overlay
            ldx     #$00            ; Set head 0
            stx     CHEAD           ;
            ldx     #$0C            ; Track 12 
            cmp     #$09            ; Overlays 1 - 8 ?
            bcc     SEEK            ; Yes, those are in block 40 no matter if single
                                    ; or dual disk
                                    ; No, means:
                                    ;  If dual side, overlay is in track 12, head 1
                                    ;  If single side, overlay is in track 13
            inx                     ; Increment track for single side
            bit     DRVNFO          ; Check if disk in drive 0 is one or two sides
            bpl     SEEK            ; One side, then jum to seek
            dex                     ; Two sides, then go back to track 12
            lda     #$01            ; And set head 1
            sta     CHEAD           ;
SEEK:       txa                     ; Move track to A (where CKSEEKTRK expects it)
            ldx     #$00            ; Seek track A on drive 0
            jsr     CKSEEKTRK       ;
            dma     A, OVLORG       ; $F8 DMA address for overlays
            sta     CURFINFO+FINFO::DMABF
            lda     CURROVL         ; Recover current overlay. Overlays start in 1.
            clc                     ; As overlay 1 start at sector 18, add 17 
            adc     #$11            ; to get the overlay sector number
            cmp     #NSECTS         ; Is sector < NSECTS?
            bcc     READ            ; Yes, go read sector
            bit     DRVNFO          ; Check if one or two sides
            bmi     READ            ; Two sides, sector number is ok
            sec                     ; One side, sector number is sector MOD NSECTS
            sbc     #NSECTS         ;
READ:       jsr     READSECT        ; Read sector
            lda     CURROVL         ; Recover overlay number
            cmp     OVLORG          ; Compare to number of loaded overlay
            beq     RETX            ; If equal, return OK
            jsr     ERROR35         ; No CODOS on drive 0, or system overlay load error
            ; Not reached
RETX:       ldx     SAVEX9          ; Restore X and return
            rts                     ;
.endproc

; Check that drive X is valid and open
;
            .export DRVVALIDO

.proc DRVVALIDO
            jsr     DRVVALID        ; Check that drive is valid
            ; Fall through
.endproc

; Check if drive is open.
; Does not return in case of error
;
            .export ISDRVOPEN

.proc ISDRVOPEN
            pha                     ; Save A
            lda     ODRIVES,x       ; Check if drive X is open
            bmi     RETURN          ; It is, just return
            pla                     ; It isn't, recover A and error
            jsr     ERROR03         ; Drive needed is not open
            ; Not reached
RETURN:     pla                     ; Restore A and return
            rts                     ;
.endproc

; Check if drive is valid
;
            .export DRVVALID

.proc DRVVALID
            cpx     NDRIVES         ; Check drive number
            bcc     RETURN          ; Drive between 0 - 3  
            jsr     ERROR05         ; Missing or illegal disk drive number
            ; Not reached
RETURN:     rts                     ;
.endproc

; Get device or file for channel X
;
; Saves device into DEVICE and returns it into A
; Does not return on error
;
            .export GETDEV

.proc GETDEV
            stx     CHANNEL         ; Save channel
            cpx     #$0A            ; Is it valid?
            bcc     CONT            ; Yes, go on
            jsr     ERROR08         ; Nop, Missing or illegal channel number.
CONT:       lda     IOCHTBL,x       ; Get device or file
            sta     DEVICE          ; And save it
            rts                     ;
.endproc

; Get assigned device/file to channel in X
;
; Saves device into DEVICE and returns device/file in X and A.
; Does not return on error
;
            .export ASSIGNED

.proc ASSIGNED
            jsr     GETDEV          ; Get device for channel
            bne     RET             ; Valid, return            
            jsr     ERROR09         ; Channel needed is not assigned
RET:        tax                     ; Returns device in X
            rts                     ;
.endproc

; Get current file (DEVICE) and checks if locked.
; Does not return if it is
;
.proc GETCURCHKLK
            jsr     GETFINFO        ; Gets FINFO for current file
            ; Fall through
.endproc

; Check if file in DEVICE is locked.
; Does not return if it is
;       
.proc CHKLCK
            lda     CURFINFO+FINFO::FLAGS ; Get file flags
            and     #FLLOCKED             ; Is it locked?
            beq     RET                   ; No, just return
            jsr     ERROR07               ; Locked file violation
            ; Not reached
RET:        rts
.endproc

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

.proc GETFPSECT
            lda     #$00            ; Set head 0
            sta     CHEAD           ;
            lda     #$07            ; Sectors/block-1
            sta     SCTBLKM1        ;
                                    ; Get drive number
            ldx     CURFINFO+FINFO::DRIVE
            lda     DRVNFO,x        ; Get flag for drive. If Bit 7 == 1, then dual side
            sta     DSFLAG          ; Store it
                                    ; Get sector offset
            lda     CURFINFO+FINFO::FPOS+2
            sta     TEMP5           ;
            lda     CURFINFO+FINFO::FPOS+1
            lsr     TEMP5           ; And divide it by 8
            ror     a               ;
            lsr     TEMP5           ;
            ror     a               ;
            lsr     TEMP5           ;
            ror     a               ;
            bit     DSFLAG          ; Is if a dual side disk?
            bpl     CONT            ; No, we get the block offset in TEMP5, remainder
                                    ; offset in block in A
            ldx     #$0F            ; Yes, update the sectors/block-1
            stx     SCTBLKM1        ;
            lsr     TEMP5           ; And divide again (total is sector offset / 16)
            ror     a               ;
CONT:       tax                     ; Transfer block offset to X
                                    ; Get first block of file
            lda     CURFINFO+FINFO::BATPT
            inx                     ; 
            bne     DECOFF          ; Past block?
NXTBLK:     tay                     ; Get index of nex block
            lda     (BATP),y        ; And read block info
DECOFF:     dex                     ; Decrement block offset
            bne     NXTBLK          ; Continue until we arrive to block offset
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
            sec                     ;
            sbc     #$01            ; A = CBLOCK-1
            ldx     #$FF            ; Start with X = -1
                                    ; (will increment to 0 in the loop)
            sec                     ; Set carry for subtraction
DIVLP:      inx                     ;
            sbc     #NSECTS/2       ;
            bcs     DIVLP           ; X = (CBLOCK-1) / (NSECTS/2)
            adc     #NSECTS/2       ; A = (CBLOCK-1) % (NSECTS/2)
            asl     a               ;
            asl     a               ;
            asl     a               ; A  = ((CBLOCK-1) % (NSECTS/2))*BS (Single side)
            bit     DSFLAG          ; Dual side disk?
            bpl     SKIP            ; Nope, skip
            asl     a               ; A  = ((CBLOCK-1) % (NSECTS/2))*BS (Dual side)
SKIP:       sta     TEMP5           ; Save in TEMP5
            txa                     ;
            asl     a               ;
            asl     a               ;
            tax                     ; X = (CBLOCK-1) / (NSECTS/2) * 8
            lda     #NSECTS         ; Get sectors per track
            sta     SECSTRK         ;
            lda     #$07            ; Mask for sector offset in the block
            bit     DSFLAG          ; Dual side disk?
            bpl     SKIP2           ; No, skip
            lda     #NSECTS*2       ; Yes, double the sectors per track
            sta     SECSTRK         ;
            lda     #$0F            ; And adjust sector offset mask
                                    ; Calculate and store sector offset
SKIP2:      and     CURFINFO+FINFO::FPOS+1
            sta     BLKSCTOF        ; in the block
            clc                     ;
            adc     TEMP5           ; Add sector offset to sector calculation
            
            ; Calculate track and sector

            dex                     ; X = (CBLOCK-1) / (NSECTS/2) * 8 - 1 ( -0
TRKSBC:     inx                     ;   entering the loop)
            sec                     ; Set carry for subtraction
            sbc     SECSTRK         ; First substraction
            bcs     TRKSBC          ; Overflow? No, loop
            adc     SECSTRK         ; Yes, restore last substraction
            cpx     #$0C            ; Track 12?
            bcc     GOTIT           ; No, we've got our track and sector
            adc     #$11            ; Yes, have to add 12 sectors (11 + Cy)
            cmp     SECSTRK         ; Are we past sector per trck?
            bcc     GOTIT           ; No, we are done
            sbc     SECSTRK         ; Yes, get sectors % SECSTRK
            inx                     ; And increment track number
GOTIT:      stx     CTRACK          ; Save track
            sta     CSECT           ; Save file sector for pointer
                                    ; Get file drive
            ldx     CURFINFO+FINFO::DRIVE
            lda     CSECT           ; Get sector (again?)
            cmp     #NSECTS         ; Is it bigger that sectors per track?
            bcc     SEEK            ; No, skip to seek track
            inc     CHEAD           ; Switch to head 1
SEEK:       lda     CTRACK          ; Seek track
            jsr     CKSEEKTRK       ;
            lda     CSECT           ; And return sector in A
            rts                     ;
.endproc

; Write file buffer to disk if there are any pendinng changes for
; current file
;
.proc FFLUSH
            jsr     GETFINFO        ; Gets FINFO for current file
            ; Fall through
.endproc

; Write file buffer to disk if there are any pendinng changes for
; current FINFO
;
.proc FLUSH
            bit     CURFINFO+FINFO::FLAGS ; Check flags
            bvc     WRFRET                ; If there are no pending changes, return
            ; Fall through
.endproc

; Write sector of current file position
;
            .export WRFPSECT

.proc WRFPSECT
            jsr     GETFPSECT             ; Calc. sector, track and head of file pos
            jsr     WRITSECT              ; Write sector buffer to disk
            lda     CURFINFO+FINFO::FLAGS ; Get file flags
            and     #<(~FISDIRTY)         ; Clear the changes pending flag
            sta     CURFINFO+FINFO::FLAGS ;
            ; Fall through
.endproc

.proc WRFRET
            rts
.endproc

; SVC 18 - Set the file position for a channel to End-of-File
;
; Arguments:            X = Channel number 
;
; Arguments returned:   None
;
            .export FEND

.proc FEND
            lda     #$FF            ; Set file pos bigger than maximum file size
            sta     FILEPOS+2       ;
            jmp     FSEEK           ; And do a seek
.endproc

; SVC 17 - Set the file position for a channel to Beginning-of-Data
;
; Arguments:            X = Channel number 
;
; Arguments returned:   None
;
            .export FREWIND

.proc FREWIND
            lda     #$00            ; Set file pos at 0
            sta     FILEPOS         ;
            sta     FILEPOS+1       ;
            sta     FILEPOS+2       ;
            ; Fall through
.endproc

; SVC 19 - Specify the file position for a channel and reads file sector into buffer
;
; Arguments:            X = Channel number 
;                       U7 (FILEPOS)
;
; Arguments returned:   None
;
            .export FSEEK

.proc FSEEK
            jsr     ASSIGNED        ; Get assigned device/file to channel X
            bmi     RETURN          ; Return if device
            jsr     FFLUSH          ; Flush pending changes to disk
            lda     FILEPOS         ; Add file header length to get the position
            clc                     ; relative to the beginning of data
            adc     #FHDRLEN        ;
            sta     CURFINFO+FINFO::FPOS
            lda     FILEPOS+1       ;
            adc     #$00            ;
            sta     CURFINFO+FINFO::FPOS+1
            lda     FILEPOS+2       ;
            adc     #$00            ;
            sta     CURFINFO+FINFO::FPOS+2
            bcs     FEND            ; If overflow, set file pos to file size
            jsr     FEOF            ; Check if end of file (Cy set if so)
            bcc     READ            ; No, read sector into buffer
FEND:       ldx     #$02            ; Yes, set file pos to file size
CPYFSIZ:    lda     CURFINFO+FINFO::FSIZE,x
            sta     CURFINFO+FINFO::FPOS,x
            dex                     ;
            bpl     CPYFSIZ         ;
READ:       jsr     RDFPSECT        ; Read sector of current file position
            jsr     UPDCFINFO       ; Copies back current finfo structure to file's FINFO
RETURN:     rts
.endproc

; SVC 23 - Truncate a file at the present file position.
;
; Arguments:            X = Channel number 
;
; Arguments returned:   None
;
            .export FTRUNC

.proc FTRUNC
            jsr     ASSIGNED        ; Make sure the channel is assigned
            bmi     RETURN          ; If not, just return
            jsr     GETCURCHKLK     ; Get current file and ensures it's not locked
            jsr     GETFPSECT       ; Calculate sector, track and head of file position
            jsr     SETEND          ; Sets current position as file size
            ldy     CBLOCK          ; Get current block
            lda     (BATP),y        ; Get next block for current
            cmp     #$F9            ; Is a valid block? (meaning not last, mostly)
            bcs     UFINFO          ; No, we're done
            tax                     ; Yes, save block in X
            lda     #BLKLAST        ; Mark block as last in the series
            jsr     SETNEXTBLK      ;
            txa                     ; Recover last block
            tay                     ; And free it's chain
            jsr     FREEBLK         ;
UFINFO:     jsr     UPDCFINFO       ; Copies back current finfo structure to file's FINFO
RETURN:     rts                     ;
.endproc

; Get MEMCOUNT characters from channel X into (MEMBUFF)
;
; Returns: Cy clear if at least one byte transferred, Cy set otherwise
;          MEMCOUNT is updated to available data
;
            .export GETMBUFF
            
.proc GETMBUFF
            jsr     ASSIGNED        ; Get assigned device/file
            bpl     ISFILE          ; Check if a file
            jmp     ISDEV           ; Jump if a device

ISFILE:     jsr     PREPCPY         ; Check that dest memory is valid, flush any
                                    ; pending changes to disk, adjust MEMCOUNT to the
                                    ; available data in file (if MEMCOUNT is greater)
                                    ; and store the count into L00D2
                                    ; Get pointer to file buffer and copies
            lda     CURFINFO+FINFO::BUFF
            sta     LDALO3          ; to the lda insructions below
            sta     LDALO2          ;
            sta     LDALO4          ;
            sta     LDALO1          ;
            lda     CURFINFO+FINFO::BUFF+1
            sta     LDAHI3          ;
            sta     LDAHI2          ;
            sta     LDAHI4          ;
            sta     LDAHI1          ;

CPMEM:      
.ifdef mtu
            lda     DSTBNKCFG       ; Set destination bank 
            sta     BNKCTL          ;
.endif
            lda     L00D2+1         ; Current count > 256 (one page)
            beq     CPYREM          ; No, go copy remaining bytes
                                    ; Position at the beginning of a sector?
            ldy     CURFINFO+FINFO::FPOS
            bne     CPTOENDS        ; No, go copy bytes up to end of sector first

            ; Copy page loop. Does two lda,sta pairs per iteration to optimize for
            ; speed

CPPAGE:     .byte   $B9             ; lda SYSRAM, y
LDALO1:     .byte   <SYSRAM         ; Replaced with current file buffer
LDAHI1:     .byte   >SYSRAM         ;

            sta     (MEMBUFF), y    ; Store into memory
            iny                     ; Advance one pos in MEMBUFF

            .byte   $B9             ; lda SYSRAM, y
LDALO2:     .byte   <SYSRAM         ; Replaced with current file buffer
LDAHI2:     .byte   >SYSRAM         ;

            sta     (MEMBUFF), y    ; Store into memory
            iny                     ; Advance one pos in MEMBUFF

            bne     CPPAGE          ; Repeat until end of page
            beq     INCMPGE         ; Always jump to increment MEMBUFF page
            ; Not reached

            ; Copy to end of sector

                                    ; Get current position in the sector
CPTOENDS:   ldx     CURFINFO+FINFO::FPOS
            ldy     #$00            ; Init index to MEMBUFF

CPONE:      .byte   $BD             ; lda SYSRAM, x
LDALO3:     .byte   <SYSRAM         ; Replaced with current file buffer
LDAHI3:     .byte   >SYSRAM         ;

            sta     (MEMBUFF), y    ; Store to MEMBUFF
            iny                     ; Andvance pos in MEMBUFF

            inx                     ; Advance pos in sector
            bne     CPONE           ; While not end of sector, repeat
            tya                     ; Transfer MEMBUFF index to A
            beq     INCMPGE         ; If reached end of page, go increase MEMBUFF page
            clc                     ; If not, advance read bytes in MEMBUFF
            adc     MEMBUFF         ;
            sta     MEMBUFF         ;
            bcc     DECOUNT         ; Not end of page

INCMPGE:    inc     MEMBUFF+1       ; Increment dest page
                                    ; Did we started at the beginning of a sector?
DECOUNT:    lda     CURFINFO+FINFO::FPOS
            beq     DECCPGE         ; Yes, copied an entire page, decrement count page
            clc                     ; Calculate remaining count
            
            ; The trick is that the starting position in the sector is the 2's
            ; complement of the bytes trasferred, so adding the position to L00D2
            ; is the same as substracting the bytes transferred

            adc     L00D2           ; Add starting pos in sector
            sta     L00D2           ;
            bcs     INCFPOS         ; If no borrow, skip decrement
DECCPGE:    dec     L00D2+1         ; Decrement count page
                                    ; Increment second byte of file position
INCFPOS:    inc     CURFINFO+FINFO::FPOS+1
            bne     RESTBNK         ; Skip next if not overflow
                                    ; Increment third byte of file position
            inc     CURFINFO+FINFO::FPOS+2
RESTBNK:    
.ifdef mtu
            lda     DEFBNKCFG       ; Restore default bank
            sta     BNKCTL          ;
.endif
            ldx     #$00            ; Zeroes first byte of file position
                                    ; From now on, we are page aligned
            stx     CURFINFO+FINFO::FPOS
            jsr     RDFPSECT        ; Read sector for current file position
            jmp     CPMEM           ; Continue copying

            ; Copy remaining bytes ( < 256 )

CPYREM:     lda     L00D2           ; Get remainig bytes count
            beq     CPDONE          ; If none, we're done
                                    ; Get position in sector
            ldy     CURFINFO+FINFO::FPOS
            ldx     #$00            ; Init index in MEMBUFF

CPONEB:     .byte   $B9             ; lda SYSRAM, y
LDALO4:     .byte   <SYSRAM         ; Replaced with current file buffer
LDAHI4:     .byte   >SYSRAM         ;

            sta     (MEMBUFF,x)     ; Store into MEMBUFF 
            inc     MEMBUFF         ; Increment destination
            bne     DECNT           ;
            inc     MEMBUFF+1       ; 
DECNT:      dec     L00D2           ; Decrement count
            beq     ENDCPY          ; No more? Then we're done
            iny                     ; increment position in file buffer
            bne     CPONEB          ; Repeat until end of sector
            jmp     INCFPOS         ; Advance file position

ENDCPY:     iny                     ; Increment position in file buffer
            beq     INCFPOS         ; If overflow, advance file position
                                    ; If not, update file position
            sty     CURFINFO+FINFO::FPOS

CPDONE:     
.ifdef mtu
            lda     DEFBNKCFG       ; Restore default bank
            sta     BNKCTL          ;
.endif
            jsr     UPDCFINFO       ; Update FINFO entry in active files table
            jmp     ENDSUB          ; And end subroutine

            ; Is a device
            
ISDEV:      and     #$7F            ; Clear device's higher bit
            tax                     ; and use it as an index
            lda     DDTI,x          ; to get the device's driver
            sta     DRIVERP         ;
            lda     DDTI+1,x        ;
            sta     DRIVERP+1       ;
            jsr     DSTMEMOK        ; Ensures that destination memory is OK 
            jsr     SETCNT          ; Sets bytes to transfer
            lda     #$00            ;
            sec                     ; Clears borrow
            sbc     MEMCOUNT        ; Negate MEMCOUNT so we can do INCs until zero
            sta     MEMCOUNT        ;
            lda     #$00            ;
            sbc     MEMCOUNT+1      ;
            sta     MEMCOUNT+1      ;
            ora     MEMCOUNT        ; Check if anything to transfer
            beq     ENDSUB          ; If not, we're done
GETDBYT:    jsr     GETDRVBYTE      ; Get byte from device and store into MEMBUFF
            bcs     NOMORE          ; Error, no more bytes?
            inc     MEMCOUNT        ; Increment MEMCOUNT (remember, now it is negative)
            bne     GETDBYT         ;
            inc     MEMCOUNT+1      ;
            bne     GETDBYT         ; Repeat until no more bytes
NOMORE:     lda     L00D2           ; Update L00D2 with bytes transferred
            clc                     ; Add memcount (which now is negative)
            adc     MEMCOUNT        ;
            sta     MEMCOUNT        ;
            lda     L00D2+1         ;
            adc     MEMCOUNT+1      ;
            sta     MEMCOUNT+1      ;

ENDSUB:     ldx     CHANNEL         ; Restore X with channel number
            clc                     ; Clear carry
            lda     MEMCOUNT        ; Did we transferred any byte 
            ora     MEMCOUNT+1      ;
            bne     RET             ; Yes, return Cy clear
            sec                     ; No, return Cy set
RET:        rts                     ;
.endproc

; Inputs byte into (MEMBUFF) from device using previously set JDRIVERP
;
.proc GETDRVBYTE
.ifdef mtu
            lda     DEFBNKCFG       ; Switch to default bank
            sta     BNKCTL          ;
.endif
            jsr     JDRIVERP        ; Get byte from driver
.ifdef mtu
            ldx     DSTBNKCFG       ; Switch back to destination bank
            stx     BNKCTL          ;
.endif
            nop                     ; Give the device driver time?
            nop                     ;
            nop                     ;
            bcs     RETERR          ; There was an error calling the driver
            clc                     ; Clear Cy for success
            ldx     #$00            ; Store byte into MEMBUFF
            sta     (MEMBUFF,x)     ;
            inc     MEMBUFF         ; Advance one pos
            bne     RETURN          ;
            inc     MEMBUFF+1       ;
RETURN:     rts                     ;
RETERR:     sec                     ; Set Cy for error
            rts                     ;
.endproc

; Jumps to the current device driver's routine
;
JDRIVERP:   jmp     (DRIVERP)       ; Jumps to DRIVERP address

; Null device driver routines
;
.proc NULDRVI
            lda     EOF             ; Just returns "End of file"
            ; Fall through
.endproc

.proc NULDRVO
            sec                     ; Error
            rts                     ;
.endproc

; Check that dest memory is valid, flush any pending changes to disk,
; adjust MEMCOUT to the available data in file (if MEMCOUNT is greater)
; and store the count into L00D2
;
.proc PREPCPY
            jsr     DSTMEMOK        ; Ensures that destination memory is OK
            jsr     FFLUSH          ; Flush pending changes to disk
            jsr     CALREMFSIZ      ; Calculate remaining file size from current pos
                                    ; and store into FILEPOS
            bcs     CONT            ; Cont if remaining size >= 0
            jsr     ERROR46         ; System crash: file ordinal check error
            ; Not reached
CONT:       sec                     ; MEMCOUNT > remaining file size?
            lda     FILEPOS         ;
            sbc     MEMCOUNT        ;
            lda     FILEPOS+1       ;
            sbc     MEMCOUNT+1      ;
            lda     FILEPOS+2       ;
            sbc     #$00            ;
            bcs     SETCNT          ; No, set bytes to transfer
            lda     FILEPOS         ; Yes, adjust MEMCOUNT to remaining file size
            sta     MEMCOUNT        ;
            lda     FILEPOS+1       ;
            sta     MEMCOUNT+1      ;
            ; Fall through
.endproc

; Set bytes to transfer
;
.proc SETCNT
            lda     MEMCOUNT        ; Set bytes to transfer
            sta     L00D2           ;
            lda     MEMCOUNT+1      ;
            sta     L00D2+1         ;
            rts                     ;
.endproc

; Check that destination of a memory copy is permitted and sets
; the destination bank config
;
; Does not return if fails
;
.proc DSTMEMOK
.ifdef mtu
            lda     DSTBANK         ; Check destination bank
            bne     DESTOK          ; Is not system bank, so don't check for
                                    ; protected memory
.endif
            lda     MEMBUFF+1       ;
            bne     NOTZP           ; Jump if dest not in ZP
            lda     MEMBUFF         ; Check if odestrig is in reserved ZP space
            cmp     #$B0            ;
            bcc     DESTOK          ; Dest below $00B0 (OK)
            bcs     DESTPRTCT       ; Dest gt or eq $00B0 (reserved CODOS space)
            ; Not reached

NOTZP:      cmp     #$02            ; Check if dest below $0200 (reserved CODOS space)
            bcs     DESTOK          ; Dest gt or eq $0200
DESTPRTCT:  bit     IGNORWRP        ; Is copy to CODOS space allowed?
            bmi     DESTOK          ; Yes, go on
            jsr     ERROR17         ; No, reserved or protected memory violation
            ; Not reached
DESTOK:     lda     MEMCOUNT        ; Check if <dest> + <count> > $FFFF
            clc                     ;
            adc     MEMBUFF         ;
            lda     MEMCOUNT+1      ;
            adc     MEMBUFF+1       ;
            bcc     DESTOK2         ; Dest beyond 0xFFFF?
            jsr     ERROR16         ; yes, <from> address greater than <to> address
            ; Not reached
DESTOK2:    cmp     #>SYSRAM        ; Is it in SYSRAM (protected)
            bcc     DESTOK3         ; No, go on
.ifdef mtu
            lda     DSTBANK         ; Bank 0?
            bne     DESTOK3         ; No, go on
.endif
            bit     IGNORWRP        ; Is copy to CODOS space allowed?
            bmi     DESTOK3         ; Yes, go on
            jsr     ERROR17         ; No, reserved or protected memory violation
            ; Not reached
DESTOK3:    nop                     ;
            ; Fall through

.ifdef mtu

.endproc

; Set destination bank config
;
.proc SETDBNKCFG
            lda     DSTBANK         ; Get destination bank
            and     #$03            ; Mask out non-bank bytes
            eor     DEFBNKCFG       ;
            sta     DSTBNKCFG       ; Store destination bank config
.endif
            rts                     ;
.endproc


; Output MEMCOUNT characters from (MEMBUFF) to channel X
;
            .export OUTMBUFF

.proc OUTMBUFF
            jsr     ASSIGNED        ; Ensure that channel X is assigned
            bpl     ISFILE          ; Check if it is a file
            jmp     ISDEV           ; Jump if a device
            ; Not reached

ISFILE:     
.ifdef mtu
            jsr     SETDBNKCFG      ; Set destination bank config
.endif
            jsr     GETCURCHKLK     ; Get current file and ensures it's not locked
                                    ; Get pointer to file buffer and copies
            lda     CURFINFO+FINFO::BUFF
            sta     STALO3          ; to the sta insructions below
            sta     STALO1          ;
            sta     STALO2          ;
            sta     STALO4          ;
            lda     CURFINFO+FINFO::BUFF+1
            sta     STAHI3          ;
            sta     STAHI1          ;
            sta     STAHI2          ;
            sta     STAHI4          ;

CPMEM:      
.ifdef mtu
            lda     DSTBNKCFG       ;
            sta     BNKCTL          ;
.endif
            lda     MEMCOUNT+1      ; Current count > 256 (one page)
            bne     CHKBOS          ; Yes, check if file pos is at the start of sector
            jmp     CPYREM          ; No, go copy remaining bytes
                                    ; Is file pos at the beginning of a sector?
CHKBOS:     ldy     CURFINFO+FINFO::FPOS
            bne     CPTOENDS        ; No, go copy bytes up to end of sector first

            ; Copy page loop. Does two lda,sta pairs per iteration to optimize for
            ; speed

CPPAGE:     lda     (MEMBUFF),y     ; Get byte from MEMBUFF
        
            .byte   $99             ; sta SYSRAM, y
STALO1:     .byte   $00             ; Replaced with current file buffer
STAHI1:     .byte   $E0             ;

            iny                     ; Get next byte from MEMBUFF
            lda     (MEMBUFF),y     ;
        
            .byte   $99             ; sta SYSRAM, y
STALO2:     .byte   $00             ; Replaced with current file buffer
STAHI2:     .byte   $E0             ;
            
            iny                     ; Advance on epos in MEMBUFF
            bne     CPPAGE          ; Repeat until end of page
            beq     INCMPGE         ; Always jump to increment MEMBUFF page
            ; Not reached

            ; Copy to end of sector
                                    ; Get current position in the sector
CPTOENDS:   ldx     CURFINFO+FINFO::FPOS
            ldy     #$00            ; Init index to MEMBUFF

CPONE:      lda     (MEMBUFF),y     ; Get byte from membuff

            .byte   $9D             ; sta SYSRAM, x
STALO3:     .byte   $00             ; Replaced with current file buffer
STAHI3:     .byte   $E0             ;

            iny                     ; Advance pos in MEMBUFF
            inx                     ; Advance pos in sector
            bne     CPONE           ; Repeat while not end of sector
            tya                     ; Transfer num bytes to A
            clc                     ;
            adc     MEMBUFF         ; Advance num bytes positions into MEMBUF
            sta     MEMBUFF         ;
            bcc     DECOUNT         ;
INCMPGE:    inc     MEMBUFF+1       ;
                                    ; Did we started at the beginning of a sector?
DECOUNT:    lda     CURFINFO+FINFO::FPOS
            beq     DECCPGE         ; Yes, copied an entire page, decrement count page
            clc                     ; Calculate remaining count
 
            ; The trick is that the starting position in the sector is the 2's
            ; complement of the bytes trasferred, so adding the position to L00D2
            ; is the same as substracting the bytes transferred
 
            adc     MEMCOUNT        ; Add starting pos in sector
            sta     MEMCOUNT        ;
            bcs     WRSEC           ; If no borrow, skip page decrement

DECCPGE:    dec     MEMCOUNT+1
WRSEC:      
.ifdef mtu
            lda     DEFBNKCFG       ; Switch to default bank
            sta     BNKCTL          ;
.endif
            jsr     WRFPSECT        ; Write sector of current file pos
                                    ; Increment second byte of file position
            inc     CURFINFO+FINFO::FPOS+1
            bne     ZPOSLO          ; Skip next if no overflow
                                    ; Increment third byte of file position
            inc     CURFINFO+FINFO::FPOS+2
ZPOSLO:     ldy     #$00            ; Zeroes first byte of file position
                                    ; From now on, we are page aligned
            sty     CURFINFO+FINFO::FPOS
            jsr     FEOF            ; Check if end of file (Cy set if so)
            bcc     GETSECT         ; No, get sector into buffer
            jsr     SETEND          ; Yes, set new end of file
            lda     BLKSCTOF        ; Get sector offset in the block
            cmp     SCTBLKM1        ; Compare to sectors/block - 1
            bne     CPMEM           ; Not reached, continue copy
            jsr     GETFREEB        ; Yes, we need another block
            ldy     CBLOCK          ; Get block of current file pointer
            jsr     SETNEXTBLK      ; Set new block as next one for current block 
            jmp     CPMEM           ; And continue copy

            ; Copy remaining bytes ( < 256 )

CPYREM:     lda     MEMCOUNT        ; Get remainig bytes count
            beq     CPDONE          ; If none, we're done
                                    ; Get position in sector
            ldy     CURFINFO+FINFO::FPOS
            ldx     #$00            ; Init index in MEMBUFF

CPONEB:     lda     (MEMBUFF,x)     ; Get byte from MEMBUFF

            .byte   $99             ; sta SYSRAM,y
STALO4:     .byte   $00             ; Replaced with current file buffer
STAHI4:     .byte   $E0             ;

            inc     MEMBUFF         ; Increment pos in membuff
            bne     DECNT           ;
            inc     MEMBUFF+1       ;
DECNT:      dec     MEMCOUNT        ; Decrement count
            beq     ENDCPY          ; No more? Then we're done
            iny                     ; increment position in file buffer
            bne     CPONEB          ; Repeat until end of sector
            jmp     WRSEC           ; Write buffer to disk

ENDCPY:     iny                     ; Increment position in file buffer
            beq     WRSEC           ; If complete sector, write it to disk
                                    ; Update file position
            sty     CURFINFO+FINFO::FPOS
                                    ; Set the pending changes flag
            lda     CURFINFO+FINFO::FLAGS
            ora     #FISDIRTY       ;
            sta     CURFINFO+FINFO::FLAGS

CPDONE:     
.ifdef mtu
            lda     DEFBNKCFG       ; Switch to the default bank
            sta     BNKCTL          ;
.endif
            jsr     FEOF            ; Check if end of file (Cy set if so)
            php                     ; Save flags
            bcc     UPDFINFO        ; If not FEOF, go update FINFO
            jsr     SETEND          ; Yes, set current position as file size
UPDFINFO:   jsr     UPDCFINFO       ; Update FINFO entry in active files table
            ldx     CHANNEL         ; Recover channel into X
            plp                     ; Recover flags (result of FEOF)
            rts                     ; And return

            ; Note: this seems to do the same no matter the value of MEMCOUNT+1,
            ; As GETFPSECT updates CSECT

GETSECT:    jsr     GETFPSECT       ; Load sector of current pos into file buffer
            lda     MEMCOUNT+1      ; MEMCOUNT > 256 (sector size) ?
            bne     CONTCPY         ; Yes, continue copy
            lda     CSECT           ; No, read current sector
            jsr     READSECT        ;
CONTCPY:    jmp     CPMEM           ; And continue copy

            ; Is a device

ISDEV:      and     #$7F            ; Get index
            tax                     ; To Device Driver Table for Output
            lda     DDTO,x          ;
            sta     DRIVERP         ; And set the driver pointer
            lda     DDTO+1,x        ;
            sta     DRIVERP+1       ;
.ifdef mtu
            jsr     SETDBNKCFG      ; Set destination bank config
.endif
            lda     #$00            ; This is weird... This code is calculating the 2's 
            sec                     ; complement negation of MEMCOUNT (-MEMCOUNT) and
            sbc     MEMCOUNT        ; then uses INC instructions in the @LOOP until it
            sta     MEMCOUNT        ; reaches 0. I can't see any difference from
            lda     #$00            ; leaving MEMCOUNT be and using DEC instructions
            sbc     MEMCOUNT+1      ; until it reaches 0. The print routine does not
            sta     MEMCOUNT+1      ; uses MEMCOUNT as an index, which would be the
            ora     MEMCOUNT        ; only explanation.
            beq     NOMORE          ; Nothing to copy
OUTDBYT:    jsr     OUTDRVBYTE      ; Get byte from MEMBUFF and output to device
            inc     MEMCOUNT        ; Increment MEMCOUNT (remember, now it is negative)
            bne     OUTDBYT         ;
            inc     MEMCOUNT+1      ;
            bne     OUTDBYT         ; Repeat until no more bytes
NOMORE:     sec                     ; Set Cy
            ldx     CHANNEL         ; Recover channel into X
            rts                     ;
.endproc

; Outputs char at (MEMBUFF) to device using previously set JDRIVERP
;
.proc OUTDRVBYTE
.ifdef mtu
            ldx     DEFBNKCFG       ; Switch to default bank
            stx     BNKCTL          ;
.endif
            ldx     #$00            ; Get byte from MEMBUFF
            lda     (MEMBUFF,x)     ;
            jsr     JDRIVERP        ; And output to device drive
.ifdef mtu
            lda     DSTBNKCFG       ; Switch back to destination bank
            sta     BNKCTL          ;
.endif
            inc     MEMBUFF         ; Increment position in MEMBUFF
            bne     RETURN          ;
            inc     MEMBUFF+1       ;
RETURN:     rts
.endproc

; Get the first free block available
; Marks  it as last block in the series, updates (BATP),BAT::LAST
; and returns block number in A
;
.proc GETFREEB
            ldy     #BAT::LAST      ; Get last allocated block
            lda     (BATP),y        ;
            tay                     ; 
NEXTB:      iny                     ; Get next block
            lda     (BATP),y        ; Check if free
            bne     NEXTB           ; No, check next.
            cpy     #$F9            ; Are we past the allocated blocks space?
            bcc     FOUND           ; No, then we found it
            ldy     #$00            ; Yes, start again from the beginning, looking
                                    ; for free blocks
NEXTD:      iny                     ; Get next block
            lda     (BATP),y        ; Check if free
            bne     NEXTD           ; No, check next
            cpy     #$F9            ; Are we past the allocated blocks space?
            bcc     FOUND           ; No, then we found it
                                    ; Decrement file position as no block was found
            lda     CURFINFO+FINFO::FPOS+1
            bne     DECFP1          ;
            dec     CURFINFO+FINFO::FPOS+2
DECFP1:     dec     CURFINFO+FINFO::FPOS+1
            dec     CURFINFO+FINFO::FPOS
            jsr     SETEND          ; Truncate file
            jsr     UPDCFINFO       ; Updates file's FINFO structure
            jsr     ERROR38         ; Diskette is full; all blocks already allocated.
            ; Not reached
FOUND:      lda     #BLKLAST        ; Mark block as last in the series
            jsr     SETNEXTBLK      ;
            tya                     ; Transfer new block to A
            ldy     #BAT::LAST      ; Sets block as last allocated in BAT
            sta     (BATP),y        ;
            rts                     ;
.endproc

; Open drive 0
;
.proc OPENDRV0
            ldx     #$00            ; Sets drive 0
            ; Fall through
.endproc

; Open drive X
;
            .export OPENDRV

.proc OPENDRV
            stx     CURRDRV         ; Sets current drive
            jsr     DRVVALID        ; Check if valid (does not return if not)
            lda     ODRIVES,x       ; Check if open
            bpl     CONT            ; No, go on
            txa                     ; Transfer drive to A for calling CLDRIVE
            jsr     CLDRIVE         ; Close drive
CONT:       ldx     CURRDRV         ; Recover drive from CURRDRV
            jsr     INITDRV         ; Init drive (SPECIFY + RECALIBRATE)
            ldx     CURRDRV         ; Recover drive again
            lda     #$80            ; Mark drive as open
            sta     ODRIVES,x       ;
            lda     #$00            ; Read disk's BAT and return
            jmp     RDSECTATR12     ;
            ; Not reached
.endproc

; Close drive X
;
            .export CLOSEDRV

.proc CLOSEDRV
            jsr     DRVVALID        ; Check if valid (does not return if not)
            lda     ODRIVES,x       ; Is it open?
            bpl     RETURN          ; No, return
            stx     CURRDRV         ; Save to current drive
            ldx     #$09            ; Max channel number
LOOP:       jsr     GETDEV          ; Get device for channel
            beq     NEXT            ; Not assigned, check next
            bmi     NEXT            ; Not a file, check next
            tax
                                    ; Get drive of file
            lda     FINFOTBL+FINFO::DRIVE,x
            cmp     CURRDRV         ; Is it ours?
            bne     CNEXT           ; No, check next
            ldx     CHANNEL         ; Recover channel
            jsr     FREECH          ; And free it
CNEXT:      ldx     CHANNEL         ; Get next channel
NEXT:       dex                     ;
            bpl     LOOP            ; Loop until no more 
            ldx     CURRDRV         ; Mark drive as closed
            lda     #$00            ;
            sta     ODRIVES,x       ;
RETURN:     rts                     ;
.endproc

; Serve pending FDC interrupt (if any)
;
.proc SRVINT
CHECK:      bit     HSRCW           ;
            bmi     RETURN          ; No pending interrupt
            jsr     SNSINTST        ; Execute a Sense Interrupt command
            bcc     RETURN          ; If success, return
            jsr     CLDRIVE         ; If not, close drive
            jmp     CHECK           ; Repeat until no pending interrupt
            ; Not reached
RETURN:     rts                     ;
.endproc

; Close drive A (internal)
;
;
.proc CLDRIVE
            sty     SAVEY6          ; Save Y
            and     #$03            ; Mask out track
            sta     SAVEDRV         ; And save it
            ldy     #$09            ; Max channel number
LOOP:       ldx     IOCHTBL,y       ; Get channel's device or file
            bmi     NEXT            ; Check next if it is a device driver
            beq     NEXT            ; or not assigned
                                    ; Get drive
            lda     FINFOTBL+FINFO::DRIVE,x
            cmp     SAVEDRV         ; Is it our drive
            bne     NEXT            ; No, check next
            lda     #FLUNUSED       ;
                                    ; Invalidate
            sta     FINFOTBL+FINFO::FLAGS,x
            sta     IOCHTBL,y       ; Unassign channel
NEXT:       dey                     ; Repeat for next channel
            bpl     LOOP            ;
            ldx     SAVEDRV         ;
            lda     #$00            ; Close drive
            sta     ODRIVES,x       ;
            ldy     SAVEY6          ; Restore Y
            rts                     ;
.endproc

; ASSIGN channel 0 to file or device
;
            .export ASSIGN0

.proc ASSIGN0
            ldx     #$00          ; Sets channel 0
            ; Fall through
.endproc

; ASSIGN channel X to file or device
;
; CURRDRV contains the drive number if we are assigning a file and the device
;    name if it is a device
;
            .export ASSIGN

.proc ASSIGN
            jsr     CLRASSIGNF      ; Clears assign flag and returns CURRDRV in X and A
            cmp     NDRIVES         ; Check if a file or a device
            bcc     ISFILE          ; Valid drive, it is a file
            jmp     ASSIGNDEV       ; Not a drive number, so it is a device

ISFILE:     sec
            ror     ASSIGNFLAG      ; Sets bit 7: It is an existing file
            jsr     FEXIST          ; Check if file exists
            bne     NEWFIL          ; No, new file
            jmp     FASSIGN         ; Yes, assign file

NEWFIL:     lsr     ASSIGNFLAG      ; Sets bit 6: It is a file
            ldx     CURRDRV         ; Get current drive
            jsr     EXSENSEDRV      ; Sense drive X status command
            bit     ST0             ; Get status register 0
            bvc     WRITABLE        ; Check if write protected
            jsr     ERROR21         ; New file on write-protected diskette
            ; Not reached
WRITABLE:   ldy     #BAT::NENT      ; Get number of files on disk
            lda     (BATP),y        ;
            cmp     #MAXFILES+1     ; Have we reached the maximum?
            bcc     AVAIL           ; No, still room for more
            jsr     ERROR39         ; Diskette is full; no room left in directory
            ; Not reached
AVAIL:      jsr     GETFREEB        ; Get the first free block
                                    ; Stores it into the directory entry
            sta     DIRENT+DIRE::BATP
            jsr     GETAFTNTRY      ; Find a free entry in the active files table,
                                    ; assigns the DEVICE number and copy the entry
                                    ; to CURFINFO
            beq     ISNEW           ; New entry? Yes, go on
            jsr     ERROR45         ; No, there shouldn't be an active entry for
                                    ; that drive/block!
                                    ; System crash: directory/file table check error
            ; Not reached
ISNEW:      jsr     INITFILE        ; Init file size and file position
                                    ; Get sector of first free entry
            lda     FILEHDR+FHDR::NSEC
            jsr     RDSECTATR12     ; Read sector from track 12 (directory)
                                    ; Index to the entry in the sector
            ldy     FILEHDR+FHDR::NENT
            dey                     ; Back one byte, as FILEHDR+FHDR::NENT points to
                                    ; the file name
            ldx     #$00            ; Copy the new DIRENT to the directory buffer
LOOP:       lda     DIRENT,x        ;
            sta     DIRBUF,y        ;
            iny                     ;
            inx                     ;
            cpx     #$10            ; Last byte?
            bcc     LOOP            ; No, next one
            jsr     WRTRCK12        ; Write changes to disk
            lda     #$80            ; Set flag to "Normal file"
            sta     CURFINFO+FINFO::FLAGS
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
            jsr     WRFPSECT        ; Write buffer to disk
            jsr     UPDCFINFO       ; Update FINFO entry in active files table
            ldy     #BAT::NENT      ; Get number of files on disk
            lda     (BATP),y        ;
            clc                     ; Clear carry for addition
            adc     #$01            ; Increase nuber of files
                                    ; Now it uses SETNEXTBLK to update the number of
                                    ; files in the BAT (Y is BAT::NENT, A is the new
                                    ; file count)
            jsr     SETNEXTBLK      ; Set new file count A for offset Y into the current
            jmp     WRTBAT          ; BAT and write BAT to disk
.endproc

; Assign channel to file
;
.proc FASSIGN
            ldx     DIRPOINT        ; Get pointer to filename of directory entry
                                    ; Get pointer to firsrt block in bat (We use
                                    ; DIRE::BATP-1 because X points to the file name,
                                    ; not to the first byte)
            lda     DIRBUF+DIRE::BATP-1,x
                                    ; Stores it into the directory entry
            sta     DIRENT+DIRE::BATP
            jsr     GETAFTNTRY      ; Find a free entry in the active files table,
                                    ; assigns the DEVICE number and copy the entry
                                    ; to CURFINFO
            beq     INIFP           ; New entry? Yes, go on
            jsr     FLUSH           ; Reuse entry. Flush buffer to disk.
INIFP:      lda     #FHDRLEN        ; Inits file pointer to the first data byte
            jsr     SETFILEP        ;   (just past the 64 byte file header)
            jsr     RDFPSECT        ; Read sector pointer by current file pos
                                    ; Check file flags
            lda     CURFINFO+FINFO::FLAGS
            bne     UPDFLG          ; If set, (existing entry), skip to update flags
            ldx     #$02            ; Not set, copy file length to current FINFO
            ldy     #FHDR::FLEN+2   ; from the file header
CPSIZ:      lda     (CURFINFO+FINFO::BUFF),y
            sta     CURFINFO+FINFO::FSIZE,x
            dey                     ;
            dex                     ;
            bpl     CPSIZ           ; Repeat until done
            ldy     #FHDR::FLAG     ; Copy file flags from file header
            lda     (CURFINFO+FINFO::BUFF),y
            sta     CURFINFO+FINFO::FLAGS
                                    ; Get flags, again
UPDFLG:     lda     CURFINFO+FINFO::FLAGS
            ora     #$C0            ; Mark as an existing file (bits 7 and 6 set)
            sta     ASSIGNFLAG      ;
            ; Fall through
.endproc

; Update active file and I/O channel tables
;
.proc UPDFINCHAN
            jsr     UPDCFINFO       ; Update FINFO entry
            ldx     CHANNEL         ; Update channel
            lda     DEVICE          ; With the device/file number
            sta     IOCHTBL,x       ;
            rts                     ;
.endproc

; Assign device in CURRDRV to a channel
;
.proc ASSIGNDEV
            ldy     #$08            ; Search for the device into the DNT
LOOP:       lda     DNT,y           ; Check name in table
            cmp     CURRDRV         ; Compare to device name
            beq     FOUND           ; Match, go found
            dey                     ;
            bpl     LOOP            ; Repeat until no more entries
            jsr     ERROR11         ; Missing or illegal device or file name
            ; Not reached
FOUND:      tya                     ; Compose device number
            asl     a               ;
            ora     #$80            ;
            ldx     CHANNEL         ;
            sta     IOCHTBL,x       ; Assign the device to the CHANNEL
            sta     DEVICE          ; And set current device
            rts                     ;
.endproc

; Assigns channel 0 to an existing file
; Fails if file does not exist
;
            .export FOPEN0

.proc FOPEN0
            ldx     #$00            ; Sets channel number to 0
            ; Fall through
.endproc

; Assigns channel X to an existing file
; Fails if file does not exist
;
            .export FOPEN

.proc FOPEN
            jsr     CLRASSIGNF      ; Clears assign flag and returns CURRDRV in X and A
            jsr     DRVVALIDO       ; Check that drive X is valid and open
            jsr     FEXIST          ; Check if file exists
            beq     OPEN            ; Yes, go open (assign) it
            bit     ISCMDFLG        ; Is it a command?
            bpl     ISFIL           ;  no, display "File not found"
            jsr     ERROR01         ;  yes, display "Command not found"
            ; Not reached
ISFIL:      jsr     ERROR02         ; File not found
            ; Not reached
OPEN:       jmp     FASSIGN         ; Go and assign channel
            ; Not reached
.endproc

; Clear ASSIGN flag and return current drive in A and X
;
.proc CLRASSIGNF
            lda     CURRDRV         ; Get current drive
            sta     SAVEA2          ; Save it
            jsr     FREECH          ; Free channel
            lda     #$00            ; Clear ASSIGN flag
            sta     ASSIGNFLAG      ;
            lda     SAVEA2          ; Recover drive
            tax                     ; Stores into X
            sta     CURRDRV         ; And into CURRDRV
            rts                     ;
.endproc

; Free channel 0
;
            .export FREECH0

.proc FREECH0
            ldx     #$00            ; Sets channel 0
            ; Fall through
.endproc

; Free channel in X
;
            .export FREECH

.proc FREECH
            jsr     GETDEV          ; Get device or file for the channel
            beq     RETURN          ; If not assigned, return

            ldx     #$09            ; Number of entries in IOCHTBL
LOOP:       lda     IOCHTBL,x       ; Get file or device assigned to the channel
            cmp     DEVICE          ; Is it our device?
            bne     NEXT            ; No, check next
            cpx     CHANNEL         ; Is it our channel
            bne     DOFREE          ; No, free it
NEXT:       dex                     ; Yes, continue search
            bpl     LOOP            ;

            ldx     DEVICE          ; Device not assigned to any other channel
            bmi     DOFREE          ; If it is not a file, go to unassign channel
            jsr     FFLUSH          ; Flush pending changes to disk
            jsr     ZEROFILEP       ; Zeroes file pointer
            jsr     RDFPSECT        ; Reads first sector of file
                                    ; Check if file size >= 256
            lda     CURFINFO+FINFO::FSIZE+1
            ora     CURFINFO+FINFO::FSIZE+2
            bne     CONT            ; Yes, continue
                                    ; Check if file size is at least 1 byte
            lda     CURFINFO+FINFO::FSIZE
            cmp     #FHDRLEN+1      ; (excluding the file header)
            bcs     CONT            ; Yes, continue
            jmp     FDELCURR        ; No, delete file

CONT:       ldy     #FHDR::FLEN+2   ; Compare FINFO file length to header file length
            ldx     #$02            ;
CMPBYT:     lda     CURFINFO+FINFO::FSIZE,x
            cmp     (CURFINFO+FINFO::BUFF),y
            beq     NXTBYT          ; This byte is the same, continue to next one
                                    ; Different, update FINFO
            sta     (CURFINFO+FINFO::BUFF),y
                                    ; And pending changes flag
            lda     CURFINFO+FINFO::FLAGS
            ora     #FISDIRTY       ;
            sta     CURFINFO+FINFO::FLAGS
NXTBYT:     dey                     ; Advance no next byte
            dex                     ;
            bpl     CMPBYT          ; And continue until no more
            jsr     FLUSH           ; Flush buffer changes to disk
                                    ; Get drive
            ldx     CURFINFO+FINFO::DRIVE
            lda     BATCHG,x        ; Check if its BAT has changes
            bpl     SKIP            ; No, skip updating
            jsr     WRTBAT          ; Write BAT to disk
SKIP:       lda     #FLUNUSED       ; Mark entry for this deviceas unused
            ldx     DEVICE          ;
            sta     FINFOTBL+FINFO::FLAGS,x
DOFREE:     ldx     CHANNEL         ; 
            lda     #$00            ; Free channel and return
            sta     IOCHTBL,x       ;
RETURN:     rts                     ;
.endproc

; Delete file in DIRENT+DIRE::FNAM from drive X
;
            .export FDELETE

.proc FDELETE
            jsr     DRVVALIDO       ; Check that drive X is valid and open
            stx     CURRDRV         ; Set as current drive
            jsr     FOPEN0          ; Assigns channel 0 to file (fails if not found)
            jsr     CHKLCK          ; Ensure it is not locked
            ; Fall through
.endproc

; Delete current file
;
.proc FDELCURR
            ldy     #FHDR::NSEC     ; Get sector in track 12 of the directory
            lda     (CURFINFO+FINFO::BUFF),y
            jsr     RDSECTATR12     ; Read it
            ldy     #FHDR::NENT     ; Get offset to entry in directory sector
            lda     (CURFINFO+FINFO::BUFF),y
            tax                     ; Transfer to X to use it as index
            sta     DIRPOINT        ; Save it into DIRPOINT
            ldy     #DIRE::FNAM     ; Index to file name in the FINFO
                                    ; Check that file names match
CMPCHR:     lda     DIRBUF,x        ; Get char of file name
            cmp     #'.'            ; Is it the extension separator
            beq     DELETE          ; Yes, seems good, start deleting
                                    ; No, compare to file in directory entry
            cmp     (CURFINFO+FINFO::BUFF),y
            beq     CMPNXT          ; Match, go compare the next char
            jsr     ERROR50         ; System crash: Directory redundancy check failed.
CMPNXT:     iny                     ;
            inx                     ;
            bne     CMPCHR          ; Continue until the extension found or mismatch
DELETE:     ldx     DIRPOINT        ; Get offset to entry in directory sector
            lda     #$00            ; Mark entry as deletes
            sta     DIRBUF,x        ;
            jsr     WRTRCK12        ; Update directory on disk
            ldy     #BAT::NENT      ; Get number of files on disk
            lda     (BATP),y        ; 
            sec                     ; Clear borrow for substraction
            sbc     #$01            ; Decrease number
                                    ; Now it uses SETNEXTBLK to update the number of
                                    ; files in the BAT (Y is BAT::NENT, A is the new
                                    ; file count)
            jsr     SETNEXTBLK      ; Set new file count A for offset Y into the current
                                    ; Get index to first block of file
            ldy     CURFINFO+FINFO::BATPT
            jsr     FREEBLK         ; Free chain of blocks starting at block Y
            lda     #FLUNUSED       ; Mark as unused in active files table
            ldx     DEVICE          ;
            sta     FINFOTBL+FINFO::FLAGS,x
            ldx     #$09            ; Search for file's device in the IO Channel table
UNASSGN:    lda     IOCHTBL,x       ; Check if assigned to this channel
            cmp     DEVICE          ;
            bne     NXTCHN          ; No, check next
            lda     #$00            ; Yes, mark as not assigned
            sta     IOCHTBL,x       ;
NXTCHN:     dex                     ; Next channel
            bpl     UNASSGN         ; Repeat unti no more channels
            rts                     ;
.endproc

; Free chain of blocks starting at block Y
;
.proc FREEBLK
            lda     (BATP),y        ; Get next block
            tax                     ; Save it
            lda     #$00            ; Mark current as free
            jsr     SETNEXTBLK      ;
            cpx     #$F9            ; Is next a vaild block?
            bcs     UPDBAT          ; No, we're done
            txa                     ; Yes, continue with that block 
            tay                     ;
            jmp     FREEBLK         ;
            ; Not reached
UPDBAT:     jmp     WRTBAT          ; Update disk's BAT and return
            ; Not reached
.endproc

; Check if the file in CURRDRV starting at DIRENT+DIRE::BATP is already an active file
;
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
.proc GETAFTNTRY
            lda     #$00            ; Inits file number
            sta     DEVICE          ;
            ldx     TOPASSIGTB      ; Get top of assigned files table

LOOP:       txa                     ; Calculate start of this entry
            sec                     ;
            sbc     #FINFOLEN       ;
            bmi     NOMORE          ; Start is negative, there are no more entries
            tax
                                    ; Get flags
            lda     FINFOTBL+FINFO::FLAGS,x
            beq     NEXT            ; If unused, mark as candidate and get next entry
                                    ; Active, get drive
            lda     FINFOTBL+FINFO::DRIVE,x
            cmp     CURRDRV         ; In current drive?
            bne     LOOP            ; No, get next entry
                                    ; Same file as current file?
            lda     FINFOTBL+FINFO::BATPT,x
            cmp     DIRENT+DIRE::BATP
            bne     LOOP            ; No, get next entry
            stx     DEVICE          ; Yes, store DEVICE
            jsr     CPYCFINFO       ; Copy FINFO structure to CURINFO struct in page zero
                                    ; Return flags in A
            lda     CURFINFO+FINFO::FLAGS
            rts                     ;

NEXT:       stx     DEVICE          ; Save unused entry
            jmp     LOOP            ; And go check next

NOMORE:     ldx     DEVICE          ; Get file number
            bne     FREE            ; If set, there is a free entry
            jsr     ERROR29         ; All buffers in use (free a chan. assigned to a file)
            ; Not reached

FREE:       jsr     CPYCFINFO       ; Copy FINFO structure to CURINFO struct in page zero
            lda     CURRDRV         ; Get current drive
                                    ; Store into FINFO
            sta     CURFINFO+FINFO::DRIVE
                                    ; Get first block of file
            lda     DIRENT+DIRE::BATP
                                    ; Store into FINFO
            sta     CURFINFO+FINFO::BATPT
            lda     #FLUNUSED       ; Mark it as unused   
            sta     CURFINFO+FINFO::FLAGS
            rts                     ;
.endproc

; FSCAN a file or device
;
; Arguments:            Y = Index to start of file or device name in buffer 
;                       TMPBUFP points to input buffer
;
            .export FSCAN

.proc FSCAN
            lda     (TMPBUFP),y     ; Check if first char is alphabetic
            jsr     ISALPHA         ;
            bcs     FILE            ; No, jump to set the flags
            tax                     ; Yes, save first char
            iny                     ; Advance to next char
            lda     (TMPBUFP),y     ; And check if it is a valid file name char
            jsr     VALFNCHR        ;
            bcc     FILE0           ; Yes, go check the file flags
            txa                     ; Recover first char
            ldx     #$07            ; Check if in the Device Name Table
LOOP:       cmp     DNT,x           ;
            beq     DEVICE          ; Found, just return the device name
            dex                     ; Check next
            bpl     LOOP            ;
            ora     #$80            ; Set device not found flag
DEVICE:     sec                     ; Is device (carry set)
            rts                     ;

FILE0:      dey                     ;
FILE:       lda     DEFDRV          ; Set current drive to the default
            sta     CURRDRV         ;
            lda     #$00            ;
            sta     SCANFLG         ; inits SCANFLG
            jsr     FNAMFROMBUF     ; Copy file name from buffer pointed by (TMPBUFP),y
                                    ; to DIRENT+DIRE::FNAM
            bcs     NVALID          ; If not valid file name, jump
            lda     (TMPBUFP),y     ; Get drive number, if specified
            cmp     COLON           ; Drive separator?
            bne     DEFAULT         ; No, default drive
SKIP:       iny                     ; Yes, get dribe number
            lda     (TMPBUFP),y     ;
            cmp     #$20            ; Skip blanks
            beq     SKIP            ;
            sec                     ; Convert to byte
            sbc     #$30            ;
            bcc     NVALID          ; If not a digit, not valid
            cmp     NDRIVES         ; Digit. Valid drive number?
            bcs     NVALID          ; Nope, jump
            sta     CURRDRV         ; Set as current drive
            iny                     ; And advance no next char position
DEFAULT:    jsr     SRVINTX         ; Serve pending interrupt and get drive in X 
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
            ; Not reached
.endproc

; Set illegal file name in flags
; 
.proc NVALID
            lda     #$80            ; Set bit 7 (illegal file name)
            bne     SETFLG          ; Always jump
            ; Not reached
.endproc

; Set drive not open in flags
;
.proc NOPEN
            lda     #$40            ; Set bit 6 (Drive not open)
            ; Fall through
.endproc

; Set flag for FSCAN
;
.proc SETFLG
            ora     SCANFLG         ; Set flag
            ora     CURRDRV         ; Add drive to bits 0 and 1
            sta     SCANFLG         ;
            clc                     ; Is file (carry clear)
            bit     SCANFLG         ; And set N and V flags with bit 7 and 6
            rts                     ;
.endproc

; Search for DIRENT+DIRE::FNAM in the directory table
; Returns:
;    A == 0 if file exists
;    A != 0 if file does not exist, FILEHDR+FHDR::NENT point to the first empty
;           entry in the directory table
;
            .export FEXIST

.proc FEXIST
            jsr     SETBATP         ; Set BATP to the current drive's BAT
            ldx     #$00            ; Init
                                    ;   sector and
            stx     FILEHDR+FHDR::NSEC
                                    ;   offset to first free entry
            stx     FILEHDR+FHDR::NENT
            inx                     ;
            stx     SECTNUM         ; Start from sector 1 (directory entries)
            stx     DIRPOINT        ; Points to filename of first entry
            ldy     #BAT::NENT      ; Get number of files on disk
            lda     (BATP),y        ;
            sta     NFILES          ; And save them
            bne     NOEMPTY         ; Disk empty?. No, continue
                                    ; Yes, first free entry is in the
            inc     FILEHDR+FHDR::NSEC
                                    ; first one in the first sector
            inc     FILEHDR+FHDR::NENT
RETURN:     lda     FILEHDR+FHDR::NENT
            rts                     ;

NOEMPTY:    jsr     RDSECTNTR12     ; Read SECTNUM sector into DIR buffer
ENTRYLP:    ldy     DIRPOINT        ; Get pointer to filename of entry
            ldx     #$00            ; Init DIRENT+DIRE::FNAM index
CMPLP:      lda     DIRBUF,y        ; Get first char
            beq     DELETED         ; If it is a NULL, it is deleted
            cmp     #'.'            ; Extension separator?
            beq     CHKEXT          ;   yes, go compare extension
                                    ; Compare with our file name
            cmp     DIRENT+DIRE::FNAM,x
            bne     NEXT            ; Different, go check next entry
            inx                     ; Equal, go check next char
            iny                     ;
            jmp     CMPLP           ;
            ; Not reached
                                    ; Get char of our file name
CHKEXT:     lda     DIRENT+DIRE::FNAM,x
            cmp     #'.'            ; Extension?
            bne     NEXT            ; No, then different. Go check next entry
            lda     $E501,y         ; Compare the exension letter
            cmp     DIRENT+DIRE::FNAM+1,x
            bne     NEXT            ; Different, go check next entry
            lda     #$00            ; Return 0 and clear FILEHDR+FHDR::NENT
            sta     FILEHDR+FHDR::NENT
            rts                     ; and done.

NEXT:       dec     NFILES          ; Decrement file count
            bne     NDIRE           ; Still files left, advance to next entry
                                    ; No more files, were there any deleted?
            lda     FILEHDR+FHDR::NENT
            bne     RETURN          ; Yes, return 
            beq     NXTFREE         ; No, advance to next entry and report it
                                    ; as free
            ; Not reached
NDIRE:      jsr     NXTDIRENT       ; Point DIRPOINT to next entry in buffer
            jmp     ENTRYLP         ; And process it

                                    ; First deleted entry?
DELETED:    lda     FILEHDR+FHDR::NSEC
            bne     NDIRE           ; No, go get next entry
            lda     SECTNUM         ; Yes, store sector
            sta     FILEHDR+FHDR::NSEC
            lda     DIRPOINT        ;    and pointer
                                    ;    of first deleted (free) entry 
            sta     FILEHDR+FHDR::NENT 
            lda     NFILES          ; Any more files?
            bne     NDIRE           ;    yes, go get next entry
            jmp     RETURN          ;    no, return with deleted pointer in A

NXTFREE:    jsr     NXTDIRENT       ; Point DIRPOINT to next entry in buffer
            jmp     DELETED         ; Repeat until no more files
.endproc

; Point to next directory entry and loads sector into dir buffer
; if necessary. Updates DIRPOINT and SECTNUM.
;
            .export NXTDIRENT

.proc NXTDIRENT
            clc                     ;
            lda     DIRPOINT        ; Get current position
            adc     #$10            ; Advance size of entry
            sta     DIRPOINT        ;
            bcc     RETURN          ; If we are past the current sector
            inc     SECTNUM         ; advance to the next
            jsr     RDSECTNTR12     ; And load it into the buffer
RETURN:     rts                     ;
.endproc

; Inits file size (64, as it is the length of the file header) and file
; pointer for new files
;
            .export INITFILE

.proc INITFILE
            lda     #FHDRLEN                ; Set file size to 64
            sta     CURFINFO+FINFO::FSIZE   ;
            lda     #$00                    ;
            sta     CURFINFO+FINFO::FSIZE+1 ;
            sta     CURFINFO+FINFO::FSIZE+2 ;
            ; Fall through
.endproc

; Zeroes file pointer
;
            .export ZEROFILEP

.proc ZEROFILEP
            lda     #$00            ; Value to store in CURFINFO+FINFO::FPOS
            ; Fall through
.endproc

; Sets file pointer to value in A
;
.proc SETFILEP
            sta     CURFINFO+FINFO::FPOS   ; Set file pointer to A
            lda     #$00                   ; Set file pointer MSBs to 0
            sta     CURFINFO+FINFO::FPOS+1 ;
            sta     CURFINFO+FINFO::FPOS+2 ;
            rts                            ;
.endproc

; Copy file name from buffer pointed by (TMPBUFP) to DIRENT+DIRE::FNAM
;
            .export FNAMFROMBUF0

.proc FNAMFROMBUF0
            ldy     #$00            ; Set index to TMPBUFP to 0
            ; Fall through    
.endproc

; Copy file name from buffer pointed by (TMPBUFP),y to DIRENT+DIRE::FNAM
;
            .export FNAMFROMBUF

.proc FNAMFROMBUF
            ldx     #$00            ; Init file name index
LOOP:       lda     (TMPBUFP),y     ; Get char from buffer
            jsr     VALFNCHR        ; Is it a valid file name character?
            bcs     CHKEXT          ; No, check if extension
                                    ; Yes, store
            sta     DIRENT+DIRE::FNAM,x
            iny                     ; And advance
            inx                     ;
            cpx     #FNAMLEN-1      ; Have we reached max filename lenght?
            bcc     LOOP            ; No, copy next char
            bcs     RETURN          ; Yes, return with CS (error)
            ; Not reached

CHKEXT:     cmp     #'.'            ; Extension?
            bne     NOEXT           ; No, assume end and add default extension
                                    ; Yes, store the dot
            sta     DIRENT+DIRE::FNAM,x
            iny                     ; Get the extension char
            lda     (TMPBUFP),y     ;
            iny                     ; Advance 1 pos
            bne     STOREXT         ; And go to store extension (always jumps)
            ; Not reached

NOEXT:      lda     #'.'            ; Store default extension
            sta     DIRENT+DIRE::FNAM,x
            lda     DEFAULTEXT      ;
                                    ; Store extension
STOREXT:    sta     DIRENT+DIRE::FNAM+1,x
                                    ; Check that the first char of file name
            lda     DIRENT+DIRE::FNAM
            jsr     ISALPHA         ; is a letter
            bcs     RETURN          ; If not, return with CS (error)
            ldx     #$01            ; Advance to second char
                                    ; Get char
LOOP2:      lda     DIRENT+DIRE::FNAM,x
            cmp     #'.'            ; Is it the extension separator?
            beq     VALEXT          ; Yes, go validate extension
            jsr     VALFNCHR        ; No, check it is a valid file name char
            bcs     RETURN          ; If not, return with CS (error)
            inx                     ; Advance to next char
            cpx     #FNAMLEN-1      ; Have we reached the max filename lenght?
            bcc     LOOP2           ; No, continue with next char
            bcs     RETURN          ; Yesm return with CS (error)
            ; Not reached

VALEXT:     cpx     #$01            ; File name length too short?
            beq     RETURN          ; Yes, return (Shouldn't it set the carry flag?)
                                    ; Get the extension char
            lda     DIRENT+DIRE::FNAM+1,x
            jsr     ISALPHANUM      ; Validate that it is alphanumeric
RETURN:     rts                     ;
.endproc

; Character validation routines.
; Character in A
; Return carry clear if vaild, carry set if not
;
; Check if char is alphanumeric
;
            .export ISALPHANUM

.proc ISALPHANUM
            jsr     ISNUM           ; Is it a number?
            bcc     RETVAL          ; Yes, return CC 
            ; Fall through
.endproc

; Check if character is alphabetic
;
            .export ISALPHA

.proc ISALPHA
            cmp     #'A'            ; Is it a letter
            bcs     CHKZ            ; Maybe, complete check
NOVAL:      sec                     ; Definitely not, return CS
            rts                     ;
CHKZ:       cmp     #'Z'+1          ; Is it 'Z' or lower?
            ; Fall through
.endproc

.proc RETVAL
            rts                     ; Yes, return CC; No, return CS
.endproc

; Check if character is numeric
;
            .export ISNUM

.proc ISNUM
            cmp     #'0'            ; Is it a number?
            bcc     ISALPHA::NOVAL  ; No, return CS
            cmp     #'9'+1          ; Is it '9' or lower
            rts                     ; Yes, return CC; No, return CS
.endproc

; Check if A is a valid filename character
; Returns CC if char is '_' or alphanumeric, CS otherwise
;
            .export VALFNCHR

.proc VALFNCHR
            cmp     ULINE           ; Is underline?
            bne     ISALPHANUM      ; No, check alphanumeric
            clc                     ; Yes, return OK
            rts                     ;
.endproc

; SVC 10
;
; Encode 16-bit value to hexadecimal ASCII string. 
;
; Arguments:
;       Y = Index to byte in buffer to receive first character encoded. 
;       UO/P0SCRATCH = Value to be encoded.
;       U6/OUTBUFP = Pointer to buffer
;
; Returns:
;       Y = Index to byte after last character of hex number
;
            .export HEXENCOD0

.proc HEXENCOD0
            ldx     #$00            ; Set index to P0SCRATCH to 0
            ; Fall through
.endproc

; Encode  word at P0SCRATCH,x into its 4-char ascii hex representation
; at  (OUTBUFP),y
;
            .export HEXENCOD

.proc HEXENCOD
            lda     P0SCRATCH+1,x   ; Gets most significant byte
            jsr     HEXBYTE         ; Converts it
            lda     P0SCRATCH,x     ; Gets less significant byte
            ; Fall through
.endproc

; Converts byte in A into its 2-char ascii hex representation
; at  (OUTBUFP),y
;
            .export HEXBYTE

.proc HEXBYTE
            pha                     ; Save byte
            lsr     a               ; Get upper nibble
            lsr     a               ;
            lsr     a               ;
            lsr     a               ;
            jsr     NIBBLE          ; Convert it
            pla                     ; Recover byte
            ; Fall through
.endproc

; Converts nibble in lower half of A into its 1-char ascii hex
; representation at  (OUTBUFP),y
;
            .export NIBBLE

.proc NIBBLE
            and     #$0F            ; Get lower nibble
            clc                     ; Clear carry for addition
            adc     #'0'            ; Adds "0"
            cmp     #$3A            ; Is it "9" or lower?
            bmi     STORE           ; Yes, goto store it
            adc     #$06            ; Nope, add 7 (6 + carry) to get hex digit
STORE:      sta     (OUTBUFP),y     ; And store it
            iny                     ; Next position
            rts                     ; and return
.endproc

; SVC 9
;
; Decode decimal ASCII string in (INPBUFP) to 16-bit value
;
; Arguments:
;       Y = Index to first character of string to be decoded
;       U5/INPBUFP = Pointer to the string of ASCII characters
;
; Returns:
;       A Delimiting character encountered.
;       Y = Index to delimiting character.
;       Flags: Cy = 1 if at least one valid decimal digit was encountered prior to the
;       delimiter.
;       UO/P0SCRATCH = Value returned (in normal low-byte, high-byte order).
;
            .export DECDECOD

.proc DECDECOD
            lda     #$0A            ; Init base 10 decoding (stored in TMPPTR),
            jsr     INITDEC         ;   inits result in P0SCRATCH and returns first
                                    ;   ascii digit in A
DIGIT:      sec                     ; Clear borrow for substraction
            sbc     #'0'            ; Convert digit to value
            bcc     ENDDEC          ; If borrow, it was an invalid digit (terminator)
            cmp     TMPPTR          ; Compare to base
            bcs     ENDDEC          ; >= 10 ? Yes, invalid digit (terminator)
            jsr     DECDIGIT        ; No, decode digit
            jmp     DIGIT           ; Next digit
            ; Not reached
.endproc

; SVC 8
;
; Decode hexadecimal ASCII string in (INPBUFP) to 16-bit value
;
; Arguments:
;       Y = Index to first character of string to be decoded
;       U5/INPBUFP = Pointer to the string of ASCII characters
;
; Returns:
;       A Delimiting character encountered.
;       Y = Index to delimiting character.
;       Flags: Cy = 1 if at least one valid hex digit was encountered prior to the
;       delimiter.
;       UO/P0SCRATCH = Value returned (in normal low-byte, high-byte order).
;
            .export HEXDECOD

.proc HEXDECOD
            lda     #$10            ; Init base 16 decoding (stored in TMPPTR),
            jsr     INITDEC         ;   inits result in P0SCRATCH and returns first
                                    ;   ascii digit in A 
DIGIT:      sec                     ; Substract "0" from the char
            sbc     #'0'            ;
            bcc     ENDDEC          ; If borrow, it was an invalid digit (terminator)
            cmp     #$0A            ; 0 to 9
            bcc     ISVAL           ; Yes, got valid digit
            sbc     #$07            ; Substract 7 and check if hexadecimal
            cmp     #$0A            ; less than A?
            bcc     ENDDEC          ; If borrow, it was an invalid digit (terminator)
            cmp     #$10            ; Greater than F?
            bcs     ENDDEC          ; Yes, it was an invalid digit (terminator)
ISVAL:      jsr     DECDIGIT        ; No, decode digit
            bne     DIGIT           ; Next digit
            ; Fall through
.endproc

.proc ENDDEC
            rol     PRLEADING0      ; Set carry if at least one valid digit was found
            jmp     GETNEXTCH       ; Return delimiter char in A and index to delimiter
                                    ; in Y
            ; Not reached
.endproc

.proc DECDIGIT
            pha                     ; Save accumulator and X
            stx     SAVEX1          ;
            ldx     #_TMPPTR        ; Recover base
            jsr     MULT16_16       ; RESULT * BASE -> RESULT
            pla                     ; Restore A
            clc                     ; Clear carry for addition
            adc     P0SCRATCH       ; Add result to current digit value
            sta     P0SCRATCH       ;
            lda     P0SCRATCH+1     ;
            adc     #$00            ;
            sta     P0SCRATCH+1     ;
            bcc     CONT            ; If $FFFF or less, continue
            jsr     ERROR19         ; Arithmetic overflow.
            ; Not reached

CONT:       dec     PRLEADING0      ; Sets PRLEADING0
            ldx     SAVEX1          ; Restore X
            iny                     ; Get next digit
            lda     (INPBUFP),y     ;
            rts                     ; And return
.endproc

; Init decimal or hexadecimal string to 16-bit value decoding
;
; On entry, A contains the base
; On exit, A contains first string character
;
.proc INITDEC
            sta     TMPPTR          ; Save base in TMPPTR
            lda     #$00            ;
            sta     TMPPTR+1        ;
            sta     P0SCRATCH       ; Init result
            sta     P0SCRATCH+1     ;
            sta     PRLEADING0      ; Clear flag. In the decoding functions, this
                                    ; variable is used to return carry depending
                                    ; on the number of digits decoded: Cy clear
                                    ; if none, Cy set if at least one
            beq     GETNEXTNB       ; Always jump to get next non blank and returns
            ; Not reached
.endproc

; Get next non-blank character from buffer starting at current pos + 1
; Y contains current position at INPBUFP and it is updated at exit
;
            .export GETNEXTNB1

.proc GETNEXTNB1
            iny                     ; Advance position in command line buffer
            ; Fall through
.endproc

; Get next non-blank character from input buffer starting at current position
; Y contains current position at INPBUFP and it is updated at exit
;
            .export GETNEXTNB

.proc GETNEXTNB
            jsr     GETNEXTCH       ; Get character from command line
            beq     RETURN          ; If null or semicolon, return
            cmp     #$20            ; If blank,
            beq     GETNEXTNB1      ;   get next char
RETURN:     rts                     ;
.endproc

; Get char from (INPBUFP),y+1 and return it in A
; If No more chars (NULL , ';' or EOL), zero flag is set
; Preserves carry flag
;
            .export GETNEXTCH1

.proc GETNEXTCH1
            iny                     ; Advance one pos in command line buffer
            ; Fall through
.endproc

; Get char from (INPBUFP),y and return it in A
; If No more chars (NULL , ';' or EOL), zero flag is set
; Preserves carry flag
;
            .export GETNEXTCH

.proc GETNEXTCH
            lda     (INPBUFP),y     ; Get char from input buffer
            beq     RETURN          ; if null, return
            bcs     CSCONT          ; We come from carry set?
            cmp     #$0D            ; No, end of line?
            beq     CCRET           ; Yes, return success
            cmp     SCOLON          ; Set zero flag if semicolon
CCRET:      clc                     ; Clear carry
            rts                     ;

CSCONT:     cmp     #$0D            ; End of line
            beq     CSRET           ; Yes, return with carry set
            cmp     SCOLON          ; Set zero flag if semicolon
CSRET:      sec                     ;
RETURN:     rts                     ;
.endproc

; Output program counter and registers to output line buffer
;
            .export OUTREGSLB

.proc OUTREGSLB
            jsr     SETOUTBCH       ; Set output line buffer as destination
            ; Fall through
.endproc

; Output program counter and registers to output buffer
;
.proc OUTREGS
            jsr     OUTSTR          ; Print string
            .byte   "P=", $0        ;
            ldx     #_PCSAVE        ; Print Program Counter as an HEX word
            jsr     HEXENCOD        ; Print address
.ifdef mtu
            lda     #':'            ; Bank separator
            sta     (OUTBUFP),y     ;
            iny                     ;
            lda     PRGBANK         ; Load Current Program Bank
            clc                     ;
            adc     #'0'            ; Convert to ASCII
            sta     (OUTBUFP),y     ; Copy to output buffer
            iny                     ;
            lda     #'/'            ; program/data bank separator
            sta     (OUTBUFP),y     ;
            iny                     ;
            lda     DATBANK         ; Load Current Data Bank
            clc                     ;
            adc     #'0'            ; Convert to ASCII
            sta     (OUTBUFP),y     ;
            iny                     ;
.endif ; MTU
            lda     #' '            ;
            sta     (OUTBUFP),y     ;
            iny                     ;
            lda     #'('            ;
            sta     (OUTBUFP),y     ;
            iny                     ;
            jsr     POUTBUFF02      ; Print output buffer to console ( Sets Y = 0 )
.ifdef mtu
            jsr     GETPCCONT       ; Contents of memory at P through P+2 in hex
.else
            lda     (PCSAVE),y      ; Contents of memory at P through P+2 in hex
.endif
            jsr     HEXBYTE         ;
            dey                     ;
.ifdef mtu
            jsr     GETPCCONT       ;
.else
            lda     (PCSAVE),y
.endif
            iny                     ;
            jsr     HEXBYTE         ;
            dey                     ;
            dey                     ;
.ifdef mtu
            jsr     GETPCCONT       ;
.else
            lda     (PCSAVE),y
.endif
            iny                     ;
            iny                     ;
            jsr     HEXBYTE         ;
            lda     #')'            ;
            sta     (OUTBUFP),y     ;

            iny                     ; Print content of registers                     
            ldx     #$04            ; Number of registers - 1
PREG:       lda     #' '            ; Separator
            sta     (OUTBUFP),y     ;
            iny                     ;
            lda     REGDESC,x       ; Get register name from table
            sta     (OUTBUFP),y     ; and copy to output buffer
            iny                     ;
            lda     #'='            ;
            sta     (OUTBUFP),y     ;
            iny                     ;
            lda     STACKP,x        ; Get content of register
            jsr     HEXBYTE         ; to output buffer
            dex                     ; Next register
            bpl     PREG            ; Until no more
            jmp     POUTBUFF02      ; Print output buffer to console
.endproc

.ifdef mtu

; Get contents of memory at (PCSAVE),y 
; 
.proc GETPCCONT
            lda     PRGBANK         ; Switch to program bank
            eor     DEFBNKCFG       ;
            sta     BNKCTL          ;   
            lda     (PCSAVE),y      ; Get value at PC
            ldx     DEFBNKCFG       ; And switch to default bank
            stx     BNKCTL          ;
            rts                     ;
.endproc

.endif ; MTU

; String of valid register names
;
            .export REGDESC

REGDESC:    .byte   "SFYXA"         ;

; Output Y characters from (OUTBUFP) to channel 2 (console output)
; followed by a CR
;
            .export POUTBUFFCR02

.proc POUTBUFFCR02
            ldx     #$02            ; Set console
            ; Fall through
.endproc

; Output Y characters from (OUTBUFP) to channel X
; followed by a CR
;
            .export POUTBUFFCR

.proc POUTBUFFCR
            jsr     POUTBUFF        ; Output characters to channel X
            ; Fall through
.endproc

; Output a CR to channel X
;
            .export OUTCR

.proc OUTCR
            lda     #$0D            ; Load CR
            jmp     OUTCHAR         ; And output to channel
            ; Not reached
.endproc

; Output Y characters from (OUTBUFP) to channel 2 (console output)
;
            .export POUTBUFF02

.proc POUTBUFF02
            ldx     #$02            ; Set console
            ; Fall through
.endproc

; Output Y characters from (OUTBUFP) to channel X
;
            .export POUTBUFF

.proc POUTBUFF
            sty     MEMCOUNT        ; Set length
            lda     #$00            ;
            sta     MEMCOUNT+1      ;
            lda     OUTBUFP         ; Set buffer pointer
            sta     MEMBUFF         ;
            lda     OUTBUFP+1       ;
            sta     MEMBUFF+1       ;
            jsr     OUTMBUFF        ; Output to channel X
            ldy     #$00            ;
            rts                     ;
.endproc

; Get line from input channel in X and store into INPBUFP
; Returns carry clear if success, carry set on error (no input)
; Returns length (excluding end of line) in A
;
            .export GETLINE

.proc GETLINE
            ldy     #$00            ;
            lda     IOCHTBL,x       ; Get device/file for channel in X
            cmp     #$82            ; Is it the console?
            bne     FROMDEV         ; No, jump
            lda     INPBUFP         ; Yes, set console input buffer
            sta     QLN             ;
            lda     INPBUFP+1       ;
            sta     QLN+1           ;
            jmp     JINLINE         ; Get entire line from keyboard and place it
                                    ; into (QLN) (which now it is INPBUFP)

FROMDEV:    jsr     GETCHAR         ; Get character from device
            bcs     END             ; If none, end with carry set (error)
            cmp     #$0D            ; End of line?
            beq     CCEND           ; Yes, end with carry clear (success)
            sta     (INPBUFP),y     ; Store in buffer
            iny                     ; Increment buffer index
            cpy     YLNLIM          ; Buffer full?
            bcc     FROMDEV         ; No, get next character
CCEND:      clc                     ; Clear carry (success)
END:        lda     #$0D            ; Store end of line
            sta     (INPBUFP),y     ;
            tya                     ; Return length (excluding end of line)
            beq     RETURN          ; No input, return
            clc                     ; Success
RETURN:     ldy     #$00            ;
            rts                     ;
.endproc

; Get character from input channel in X and return it in A
; Carry clear on success, carry set if no input
;
            .export GETCHAR

.proc GETCHAR
            lda     IOCHTBL,x       ; Get devive/file for channel in X
            cmp     #$82            ; Is it the console?
            bne     FROMDEV         ; No, jump
            jsr     JCINP           ; Yes, get character from console
            cmp     EOF             ; No input?
            beq     RETURN          ; Return
            clc                     ; Clear carry (success)
RETURN:     rts                     ;

FROMDEV:    jsr     CHRRDPREP       ; Prepare transfer of SAVECH from device/file
            sec                     ; Set ignore memory write protection flag
            ror     IGNORWRP        ;
            jsr     GETMBUFF        ; Do the transfer
            php                     ; Save processor status
            clc                     ; Clear gnore memory write protection flag
            rol     IGNORWRP        ;
            ; Fall through
.endproc

; Restore regs and processor status
;
.proc RESTRREGS
            ldy     SAVEY3          ; Restore registers (saved by CHRRDPREP
            ldx     SAVEX3          ;  or CHRWRPREP)
            lda     SAVECH          ; Get transferred char
            plp                     ; Recover processor status
            rts                     ;
.endproc

; Get one char from the console
;
JCINP:      jmp     (CINP)          ; Jump to console input routine


; Output string immediately following the JSR call
;
            .export OUTSTR

.proc OUTSTR
            stx     SAVEX2          ; Save X
            ldx     #$02            ; X = 2, what for?
            bne     CONT            ; Always jump
            stx     SAVEX2          ; Dead code?
CONT:       sta     SAVEA1          ; Save A
            sty     SAVEY2          ; Save Y
            pla                     ; Get PC and save in TMPPTR. PC points
            sta     TMPPTR          ; to last opcode of instruction
            pla                     ;
            sta     TMPPTR+1        ;
NEXT:       inc     TMPPTR          ; Increment PC (points to first char of string)
            bne     GETC            ;
            inc     TMPPTR+1        ;
GETC:       ldy     #$00            ;
            lda     (TMPPTR),y      ; Get char
            beq     FINISH          ; If null, end of string
            jsr     OUTCHAR         ; Print char
            jmp     NEXT            ; Loop

FINISH:     lda     TMPPTR+1        ; Push new PC to the stack
            pha                     ;
            lda     TMPPTR          ;
            pha                     ;
            ldy     SAVEY2          ; Restore indexes
            lda     SAVEA1          ;
            ldx     SAVEX2          ;
            rts                     ;
.endproc

; SVC 4
;
; Output character in A to channel in X
;
; Arguments:
;       X = Channel 
;       A Byte to output
;
; Returns:
;       FLAGS: CY = 1 if at End-of-File after output operation
;
            .export OUTCHAR

.proc OUTCHAR
            sta     SAVECH          ; Save char
            lda     IOCHTBL,x       ; Get device/file for channel
            cmp     #$82            ; Is it console?
            bne     TODEV           ; No, jump
            lda     SAVECH          ; Recover char
            jsr     JCOUTP          ; And output to console
            sec                     ; Set carry (End-of-file)
            rts                     ; 
TODEV:      jsr     CHRWRPREP       ; Prepare transfer to memory buffer
            jsr     OUTMBUFF        ; Outputs memory buffer to device
            php                     ; Save flags
            jmp     RESTRREGS       ; Restore regs (saved by CHRWRPREP) and processor
                                    ; status
            ; Not reached
.endproc

; Prepare transfer of one character to SAVECH from MEMBUFF
;  Note: Does not make sense, as this entry point is called just from GETCHAR
;        A contains the file/device and gets overwritten anyway with the transfer
; 
.proc CHRRDPREP
            sta     SAVECH          ; Save char
            ; Fall through
.endproc

; Prepare transfer of one character from SAVECH to MEMBUFF
;
.proc CHRWRPREP
            stx     SAVEX3          ; Save X
            sty     SAVEY3          ; Save Y
            lda     #$01            ; Just one char
            sta     MEMCOUNT        ;
            lda     #$00            ;
            sta     MEMCOUNT+1      ;
            lda     #<SAVECH        ; Set buffer address
            sta     MEMBUFF         ;
            lda     #>SAVECH        ;
            sta     MEMBUFF+1       ;
            rts                     ;
.endproc

JCOUTP:     jmp     (COUTP)         ; Jump to console output routine


; SVC 11
;
; Encode 16-bit value to decimal ASCII string. 
;
; Arguments:
;       Y = Index to byte in buffer to receive first character encoded. 
;       UO/P0SCRATCH = Value to be encoded.
;       U6/OUTBUFP = Pointer to buffer
;
; Returns:
;       Y = Index to byte after last character of decimal number
;
            .export DECENCOD

.proc DECENCOD
            stx     SAVEX4          ; Save X
            lda     #$00            ; Clear the print leading 0s flag
            sta     PRLEADING0      ;
            ldx     #$06            ; Points to 10^4
DECDIG:     lda     POWERS,x        ; Store into TMPPTR
            sta     TMPPTR          ;
            lda     POWERS+1,x      ;
            sta     TMPPTR+1        ;
            stx     SAVEX5          ; Save index
            ldx     #_TMPPTR        ;
            jsr     UDIV            ; P0SCRATCH = P0SCRATCH / TMPPTR
            lda     P0SCRATCH       ; 
            bne     SETPR0          ; If non-0, set print zeroes flag and continue
            bit     PRLEADING0      ; 0->  Check if print leading zeroes flag set
            bmi     PRNDIG          ; Yes, print digit
            bpl     DONTPRN         ; No, don't print (Always jump)
            ; Not reached

SETPR0:     sec                     ; Set print leading zeroes flag
            ror     PRLEADING0      ;
PRNDIG:     jsr     OUTASCIID       ; Print digit ascii decimal
DONTPRN:    jsr     UNU0            ; Copy TMPPTR to P0SCRATCH
            ldx     SAVEX5          ; Recover index
            dex                     ; Advance to next power of 10
            dex                     ;
            bpl     DECDIG          ; Repeat decoding digits until last power
            ldx     SAVEX4          ; Recover X
            lda     P0SCRATCH       ; Get last digit
            ; Fall through
.endproc

; Outputs digit as ascii decimal
;
.proc OUTASCIID
            clc                     ; Clear carry for addition
            adc     #'0'            ; Convert value to ascii decimal digit
            sta     (OUTBUFP),y     ; Copy to output buffer
            iny                     ; Incrememt position and return
            rts                     ;
.endproc

POWERS:     .word   $000A           ; 10^1
            .word   $0064           ; 10^2
            .word   $03E8           ; 10^3
            .word   $2710           ; 10^4

; 16-bit add. UADD n for SVC 27
; 
; UO = UO + Un
;  
; X contains index to pseudo-reg ( X = n + 2 )
;
            .export UADD

.proc UADD
            clc                     ; Clears carry
            lda     P0SCRATCH,x     ; Get Un
            adc     P0SCRATCH       ; Add U0
            sta     P0SCRATCH       ; And store into U0
            lda     P0SCRATCH+1,x   ;
            adc     P0SCRATCH+1     ;
            sta     P0SCRATCH+1     ;
            rts                     ;   
.endproc

; 16-bit substract. USUB n for SVC 27
;
; U0 = U0 -Un
;
; X contains index to pseudo-reg ( X = n + 2 )
;
            .export USUB

.proc USUB
            sec                     ; Sets carry for sub
            lda     P0SCRATCH       ; Get U0
            sbc     P0SCRATCH,x     ; Substract Un
            sta     P0SCRATCH       ; Store into U0
            lda     P0SCRATCH+1     ;
            sbc     P0SCRATCH+1,x   ;
            sta     P0SCRATCH+1     ;
            rts                     ;
.endproc

; Do a 16-bit product and fails if result is > 16-bit
;
; P0SCRATCH * P0SCRATCH,x -> P0SCRATCH
;
.proc MULT16_16
            jsr     MULT16_32       ; P0SCRATCH * P0SCRATCH,x -> L00D2:P0SCRATCH
            lda     L00D2+1         ; Check if greater than $FFFF
            ora     L00D2           ;
            bne     ERR19           ; Yes, overflow
            lda     P0SCRATCH+1     ; Returns P0SCRATCH MSB
            rts                     ;
.endproc

; 
; Multiplies two 16 bit numbers using a shift-and-add algorithm
;   P0SCRATCH * P0SCRATCH,x -> L00D2:P0SCRATCH
;
            .export MULT16_32

.proc MULT16_32
            stx     SAVEX10         ; Save X
            lda     #$00            ; Init result
            sta     L00D2           ;
            sta     L00D2+1         ;
            lda     P0SCRATCH,x     ; Store mutiplier
            sta     TMPVAL          ;
            lda     P0SCRATCH+1,x   ;
            sta     TMPVAL+1        ;
            ldx     #$11            ; Init iteration counter (16bits+1 for carry)

            clc                     ; Clear carry
SUMLOOP:    ror     L00D2+1         ; Shift right result and multiplicand
            ror     L00D2           ;
            ror     P0SCRATCH+1     ;
            ror     P0SCRATCH       ;
            dex                     ;
            beq     RETURN          ; If last iteration, return
            bcc     SUMLOOP         ; If not carry, don't iterate
            lda     L00D2           ; Add multiplier to the result
            clc                     ;
            adc     TMPVAL          ;
            sta     L00D2           ;
            lda     L00D2+1         ;
            adc     TMPVAL+1        ;
            sta     L00D2+1         ;
            bcc     SUMLOOP         ;
RETURN:     ldx     SAVEX10         ; Restore X 
            rts                     ;
.endproc

; Check if divide by 0 attempt
;
.proc DIVBY0
            lda     P0SCRATCH+1,x   ; Is it also 0
            bne     UDIVCONT        ; No, continue
            ; Fall through
.endproc

.proc ERR19
            jsr     ERROR19         ; Arithmetic overflow
            ; Not reached
.endproc


; UDIV n for SVC 27
;
; 16-bit divide, using a bit-by-bit division using subtraction and rotation.
;
; U0 = U0 / Un.
; 
; X contains index to pseudo-reg ( X = n + 2 )
;
; Result: Quotient in U0, remainder in Un
;
            .export UDIV

.proc UDIV
            sty     SAVEY4          ; Save Y
            lda     P0SCRATCH,x     ; Get LSB of divisor
            sta     TMPVAL          ; Store into TMPVAL
            beq     DIVBY0          ; Check if attempting a div by 0
            lda     P0SCRATCH+1,x   ; Get MSB of divisor
            ; Fall through
.endproc

.proc UDIVCONT
            sta     TMPVAL+1        ; Store into TMPVAL+1
            lda     #$00            ; Init quotient
            sta     P0SCRATCH,x     ;
            sta     P0SCRATCH+1,x   ;
            ldy     #$11            ; Init iteration loop (16 bits plus carry)
            clc                     ;
            bcc     PREPDIV         ; Go to prepare dividend (Always jumps)
ITER:       rol     P0SCRATCH,x     ; Shift a bit from dividend into quotient
            rol     P0SCRATCH+1,x   ;
            lda     P0SCRATCH,x     ; Substract the LSB of the divisor from the
            sec                     ; LSB of the quotient
            sbc     TMPVAL          ; 
            sta     L00D2           ; And store into L00D2
            lda     P0SCRATCH+1,x   ; Substract the MSB of divisor from the
            sbc     TMPVAL+1        ; MSB of the quotient
            bcc     PREPDIV         ; If borrow, skip
            sta     P0SCRATCH+1,x   ; Updates quotient with result of 
            lda     L00D2           ; substraction
            sta     P0SCRATCH,x     ;
PREPDIV:    rol     P0SCRATCH       ; Rotates dividend left to prepare
            rol     P0SCRATCH+1     ; for next bit
            dey                     ;
            bne     ITER            ; If there are more bits, next iteration
            ldy     SAVEY4          ; No more, restore Y and return
            rts                     ;
.endproc

; 16-bit move, UN to U0. UNU0 n for SVC 27
;
            .export UNU0

.proc UNU0
            lda     P0SCRATCH,x     ; Just copy P0SCRATCH,x to P0SCRATCH
            sta     P0SCRATCH       ;
            lda     P0SCRATCH+1,x   ;
            sta     P0SCRATCH+1     ;
            rts                     ;
.endproc

; 16-bit move, U0 to UN. U0UN n for SVC 27
;
            .export U0UN

.proc U0UN
            lda     P0SCRATCH       ; Just copy P0SCRATCH to P0SCRATCH,x
            sta     P0SCRATCH,x     ;
            lda     P0SCRATCH+1     ;
            sta     P0SCRATCH+1,x   ;
            rts                     ;
.endproc

; 16-bit exchange. USWP n for SVC 27
;
; U0 exchanged with Un
;
            .export USWP

.proc USWP
            lda     P0SCRATCH+1,x   ; Save Un
            pha                     ;
            lda     P0SCRATCH,x     ;
            pha                     ;
            jsr     U0UN            ; Copy U0 to Un
            pla                     ; Restore old Un into U0
            sta     P0SCRATCH       ;
            pla                     ;
            sta     P0SCRATCH+1     ;
            rts                     ;
.endproc

; Get a byte from the command line and returns in A
;
            .export GETBYTE

.proc GETBYTE
            stx     SAVEX8          ; Save X
            ldx     #_BYTRES        ; Use BYTRES as result buffer
            jsr     EVALEXP         ; Evaluate expression
            lda     BYTRES+1        ; > $FF ?
            beq     CONT            ; No, it is a valid byte
            jsr     ERROR18         ; <value> out of range (greater than $FF or less than 0).
CONT:       lda     BYTRES          ; Get result
            ldx     SAVEX8          ; restore X
            rts                     ;
.endproc

; Evaluate expression from command line and store it into P0SCRATCH,X
;
; Y contains current index into command line

; Returns carry clear if no valid digit was converted, carry set otherwise
;
            .export EVALEXP

.proc EVALEXP
            lda     #$00            ;
            sta     P0SCRATCH,x     ; Init result
            sta     P0SCRATCH+1,x   ;
            stx     SAVEX6          ; Save X
            tax                     ; X = 0
            jsr     GETNEXTNB       ; Get next non-blank from command line
            cmp     #'-'            ; Is it a dash
            bne     NODASH          ; No, jump
            inx                     ; Set index to substraction operation
            ; Get operand
GETNUM:     iny                     ; Advance one pos in command line
NODASH:     stx     TEMP3           ; Store operator index
            ldx     SAVEX6          ; recover index to buffer 
            jsr     GETNEXTNB       ; Get next non-blank
            cmp     PERIOD          ; Decimal mark?
            beq     ISDEC           ; Yes, get decinal value
            cmp     DOLLAR          ; Hexadecinal mark?
            bne     ISHEX           ; No, but it is hexa anyways, as it is nor decimal
            jsr     GETNEXTNB1      ; Get next non-blank (first value digit)
ISHEX:      jsr     HEXDECOD        ; Decode hex value
            bcc     RETURN          ; No digit found, return
            ; Execute operation
OPERATE:    jsr     USWP            ; Exchange P0SCRATCH and P0SCRATCH,X
            lda     TEMP3           ; Get operator index
            bne     SUBS            ; If it is not addition, go check substraction
            jsr     UADD            ; Perform addition
NXTOP:      jsr     USWP            ; Swap P0SCRATCH and P0SCRATCH,X back
GETOP:      jsr     GETNEXTNB       ; Get next non-blank
            ldx     #ARITHTBLLEN-1  ; Check if it is an arithmetic operator
NXOP:       cmp     ARITHMOP,x      ;
            beq     GETNUM          ; Yes, go get operand
            dex                     ; Check next operator
            bpl     NXOP            ; Continue until found or no more operators
            ldx     SAVEX6          ; Unrecognised operator. Restore X
            jsr     GETNEXTCH       ; Get next char
            sec                     ; And return error
            rts                     ;

SUBS:       dec     TEMP3           ; Is it substraction?
            bne     MULT            ; No, check if multiplication
            jsr     USUB            ; Yes, substract
            jmp     NXTOP           ; Get next operator

MULT:       dec     TEMP3           ; Is it multiplication?
            bne     DIV             ; No, check division
            jsr     MULT16_16       ; Yes, so multiplication
            jmp     NXTOP           ; Get next operator

DIV:        dec     TEMP3           ; Is it division?
            bne     MOD             ; No, then it is modulo
            jsr     UDIV            ; Yes, divide
            jmp     NXTOP           ; Get next operator

MOD:        jsr     UDIV            ; Modulo, do a division and then
            jmp     GETOP           ;   jump and not swap, so get remainder instead
                                    ;   of quotient

ISDEC:      jsr     GETNEXTNB1      ; Get next non-blank
            jsr     DECDECOD        ; Decode decimal operand
            bcs     OPERATE         ; Do the operation if at least one valid digit
RETURN:     rts                     ; No more digits, return 
.endproc

; Save memory block with header into current file at current position
;
; Overlay in A
;
            .export SAVEBLK

.proc SAVEBLK
                                    ; Set overlay
            sta     SAVEDHDR+SHDR::OVLAY
            lda     #$00            ; Reserved: always 0
            sta     SAVEDHDR+SHDR::RSRVD
.ifdef mtu
            lda     SVDFROMBNK      ; Get <from> bank
            bit     SAVDESTPF       ; Check if <dest> is set
            bpl     NODEST          ; No, use <from> info
            lda     SVDDESTBNK
.endif
                                    ; Set memory bank (always 0 for non-MTU arch)
NODEST:     sta     SAVEDHDR+SHDR::MEMBK
            lda     #SVDMAGIC       ; Set saved files magic number
            sta     SAVEDHDR+SHDR::MAGIC
            lda     MEMCOUNT        ; Get <to> address
            sec                     ; And convert to bytes count
            sbc     MEMBUFF         ;
            sta     MEMCOUNT        ;
            lda     MEMCOUNT+1      ;
            sbc     MEMBUFF+1       ;
            sta     MEMCOUNT+1      ;
            bcs     TOOK            ; If Cy clear, <from> > <to>
            jsr     ERROR16         ; <from> address greater than <to> address.
            ; Not reached
TOOK:       inc     MEMCOUNT        ; MEMCOUNT = <to> - <from> + 1
            bne     CONT            ;
            inc     MEMCOUNT+1      ;
CONT:       bit     SAVDESTPF       ; Check if <dest> is set
            bpl     CHKENT          ; No, check if entry was set
            jsr     SETDSTBUFF      ; Set <dest> as loading address
CHKENT:     ldx     #_MEMBUFF       ;
            bit     SAVENTRYPF      ; Is an entry point addr set?
            bpl     NOENT           ; No, use use starting address (MEMBUFF)
            ldx     #_TMPBUFP       ; Yes, use it
NOENT:      jsr     UNU0            ; Copy P0SCRATCH,x to P0SCRATCH
                                    ;   (either MEMBUFF or TMPBUFP)
            ldx     #_MEMCOUNT+1    ; Copy starting and entry addresses
CPYADD:     lda     P0SCRATCH,x     ;
            sta     SAVEDHDR+SHDR::ENTRY,x
            dex                     ;
            bpl     CPYADD          ;
            jsr     SVDRWPREP       ; Prepare read/write of "saved file" header
            ldx     CHANNEL         ; Get channel
            jsr     OUTMBUFF        ; Output MEMCOUNT bytes from (MEMBUFF) to channel X
            jsr     CPYPTRS         ; Copy pointers from header to page 0
            bit     SAVDESTPF       ; Check if <dest> is set
            bpl     OUTB            ; No, skip 
            jsr     SETDSTBUFF      ; Set <dest> as loading address
OUTB:       ldx     CHANNEL         ; Get channel
.ifdef mtu
            lda     SVDFROMBNK      ; Get <from> bank
            sta     DSTBANK         ;
.endif
            jsr     OUTMBUFF        ; Output buffer to channel
.ifdef mtu
            lda     #$00            ; Set default bank
            sta     DSTBANK         ;
.endif
            rts                     ;
.endproc

; Prepare read/write of "saved file" header
;
.proc SVDRWPREP
            lda     #SAVEDHDRLEN    ; Read length of the header
            sta     MEMCOUNT        ;
            lda     #$00            ;
            sta     MEMCOUNT+1      ;
            lda     #<SAVEDHDR      ; Point buffer to header
            sta     MEMBUFF         ;
            lda     #>SAVEDHDR      ;
            sta     MEMBUFF+1       ;
            rts                     ;
.endproc

; Copy pointers from "Saved file" header to page 0
;
.proc CPYPTRS
            ldx     #_MEMCOUNT+1      ;
LOOP:       lda     SAVEDHDR+SHDR::ENTRY,x
            sta     P0SCRATCH,x       ;
            dex                       ;
            bpl     LOOP              ;
            rts                       ;
.endproc

; Swap DESTBUFF and MEMBUFF pointers 
;
.proc SETDSTBUFF
            ldx     #$01            ;
LOOP:       lda     DESTBUFF,x      ;
            pha                     ;
            lda     MEMBUFF,x       ;
            sta     DESTBUFF,x      ;
            pla                     ;
            sta     MEMBUFF,x       ;
            dex                     ;
            bpl     LOOP            ;
            rts                     ;
.endproc

; Loads $58 block from file
;
;    Overlay in A
;
            .export LOADSVD

.proc LOADSVD
            sta     SAVEA3          ; Save A in temporary storage
            jsr     LD58HDR         ; Load "saved file" hdr from file at current pos
            bcs     RETCS           ; Return with error
                                    ; Check if overlay is same as requested
            lda     SAVEDHDR+SHDR::OVLAY
            cmp     SAVEA3          ;
            bne     RETCS           ; No, return with error
            jsr     CPYPTRS         ; Yes, copy pointers from header to page 0
.ifdef mtu
                                    ; Set destination bank from header
            lda     SAVEDHDR+SHDR::MEMBK
            sta     DSTBANK         ;
.endif
            bit     SAVDESTPF       ; Was a <dest> specified?
            bpl     CONT            ; No, continue
            jsr     SETDSTBUFF      ; Set <dest> as loading address 
.ifdef mtu
            lda     SVDDESTBNK      ; Set <dest> bank as loading bank
            sta     DSTBANK         ;
.endif
CONT:       ldx     CHANNEL         ; Load block into memory
            jsr     GETMBUFF        ;
            rts                     ;

RETCS:      sec                     ; Return with error
            rts                     ;
.endproc

; Load "saved file" header from chanel X at current position
;
            .export LD58HDR

.proc LD58HDR
            jsr     SVDRWPREP       ; Prepare read/write of "saved file" header
.ifdef mtu
            lda     #$00            ; Set destination bank
            sta     DSTBANK         ;
.endif
            sec                     ; Set ignore memory write protection flag
            ror     IGNORWRP        ;
            jsr     GETMBUFF        ; Get header from file into MEMBUFF
            bcs     RETURN          ; If error, return
            rol     IGNORWRP        ; Clear ignore memory write protection flag
                                    ; Check that the magic number is correct
            lda     SAVEDHDR+SHDR::MAGIC
            cmp     #$58            ;
            beq     RETOK           ; Yes, jump to return OK
            sec                     ; Return error
RETURN:     rts                     ;
            ; Not reached
RETOK:      clc                     ; Return OK
            rts                     ;
.endproc

; Assigns default input device and set input buffer
;
            .export DEFSETINPB

.proc DEFSETINPB
            lda     #$00            ;
            ldx     CHANN1          ; Get input channel device
            sta     CHANN1          ; And clears it
            jsr     UNASSIGN        ;
            ; Fall through
.endproc

; Set input buffer to input line buffer and sets input channel
; to console if not set
;
            .export SETINPBCH

.proc SETINPBCH
            lda     CHANN1          ; Get input channel device
            bne     CONT            ; If set, go on
            lda     #$82            ; If not, set default (console)
            sta     CHANN1          ;
CONT:       ldy     #$00            ;
            ; Fall through
.endproc

; Set input buffer to input line buffer
;
            .export SETINPB

.proc SETINPB
            lda     INPLBUF         ; Set input line buffer
            sta     INPBUFP         ;
            lda     INPLBUF+1       ;
            sta     INPBUFP+1       ;
            rts                     ;
.endproc

; Set output buffer to output line buffer and sets output channel to
; console if not set
;
; Also clears Y
;
            .export SETOUTBCH

.proc SETOUTBCH
            lda     CHANN2          ; Get output channel device
            bne     CONT            ; If set, continue
            lda     #$82            ; If not, set to console
            sta     CHANN2          ;
CONT:       ldy     #$00            ;
            ; Fall through
.endproc

; Set output buffer to output line buffer
;
            .export SETOUTB

.proc SETOUTB
            lda     OUTLBUF         ; Set output line buffer
            sta     OUTBUFP         ;
            lda     OUTLBUF+1       ;
            sta     OUTBUFP+1       ;
            rts                     ;
.endproc

; Assigns default output device and set output buffer
;
            .export DEFSETOUTB

.proc DEFSETOUTB
            lda     #$00            ; Get and clear channel 2 device or file
            ldx     CHANN2          ; and save it in X
            sta     CHANN2          ;
            jsr     UNASSIGN        ; Unassign file (if it is a file)
            jmp     SETOUTBCH       ; And set output buffer
.endproc

; Unassign file
;
; Device or file number in X
;
.proc UNASSIGN
            bmi     RETURN          ; If it is a device or
            beq     RETURN          ; not assigned, return
            stx     DEVICE          ; Save device
            ldx     #$09            ;
LOOP:       lda     IOCHTBL,x       ; Search for file in the I/O channel table
            cmp     DEVICE          ; If it is assigned
            beq     RETURN          ; returns
            dex                     ;
            bpl     LOOP            ; Repeat until no more channels
            ldx     DEVICE          ; Not found in the device table
            lda     #FLUNUSED       ; Close or free entry
            sta     FINFOTBL+FINFO::FLAGS,x
RETURN:     rts                     ;
.endproc

; Console Input Routine
;
.proc CIN
            jsr     JGETKEY         ; Get key
            cmp     ETX             ; Is it CTRL-C?
            beq     JCNTRLC         ; Yes, process CTRL-C
            cmp     EOF             ; End of file?
            beq     RETURN          ; Yes, return
            bit     KBDECHO         ; No, is ECHO on?
            bpl     CCRET           ; No, return with carry clear
            jsr     JOUTCH          ; Yes, echo character
CCRET:      clc                     ;
RETURN:     rts                     ;
.endproc

; Console Output Routine
;
.proc COUT
            sta     SAVEA4          ; Save char
            jsr     JTSTKEY         ; Check if key pressed
            bcc     COUTC           ; No, output char
            ; Fall through
.endproc

.proc CHKCTLC
            cmp     ETX             ; Is it CTRL-C?
            bne     KEYPR           ; No, go on
            ; Fall through
.endproc

JCNTRLC:    jmp     CNTRLC          ; Process CTRL-C

.proc KEYPR
            cmp     XOFF            ; Is it XOFF?
            bne     COUTC           ; No, output char
            jsr     JGETKEY         ; Yes, get key
            bpl     CHKCTLC         ; If it is atandard ASCII, check again
            ; Fall through
.endproc

.proc COUTC
            lda     SAVEA4          ; Output char
            jmp     JOUTCH          ;
            ; Not reached
.endproc

; Serve any K-1013 pending interrupts and returns current drive in X
;
.proc SRVINTX
            jsr     SRVINT          ; Serve any k-1013 pending interrupt (if any)
            ldx     CURRDRV         ; Get drive
            rts                     ;
.endproc

.if  CODOS2_VER = 17
.proc EXSENSEDRV17
            jsr     EXSENSEDRV      ; Sense drive X
            ; TODO: Understand this :/
LOOP:       jsr     NVALID          ; Set invalid file name
            inx
            bne     LOOP
            rts
.endproc
.endif

CODOS_SIZE = * - CODOS

            .end
