; da65 V2.18 - Ubuntu 2.19-1
; Created:    2025-05-19 14:33:25
; Input file: format.bin
; Page:       1


            .setcpu "6502"

            .include "symbols.inc"

            .include "codos.inc"

            .code

DEFSKEW     = $0C                   ; Default skew if no T argument provided
DEFINTR     = $02                   ; Default interleave if no S argument provided

            ; Offsets into ARGTABLE

_INTL       = 0
_SKEW       = 1

.proc START
            ldx     #$00            ; Unprotects SYSRAM
            stx     HSRCW           ;
            cld                     ;
            ldy     CMDLIDX         ; Get command line index
            jsr     GETARGS         ; Get command line arguments
            bcc     ARGSOK          ; Arguments OK, continue
            jsr     ERROR04         ; Syntax error in command argument
            ; Not reached

ARGSOK:     lda     NDRIVES         ; Get number of drives in the system
            cmp     #$02            ; Is it 2 or more
            bcs     TWODRV          ; Yes, operate with 2 drives
            jmp     ONEDRIVE        ; No, operate with one drive

TWODRV:     jmp     TWODRIVES       ; Jump to 2 drives operation
.endproc

; Get arguments from command line
;
; Returns Cy clear if success, Cy set if error
;
.proc GETARGS
            lda     #DEFINTR<<2     ; Set default interleave
            sta     ARGTABLE+_INTL  ;   ( x4 to use it in the format data table)
            lda     #DEFSKEW<<2     ; Set default skew
            sta     ARGTABLE+_SKEW  ;   ( x4 to use it in the format data table)
GETARG:     jsr     GETNEXTNB       ; Get next non-blank from command line
            beq     RETOK           ; No more, return OK
            ldx     #$FF            ; Init index to arguments table
            cmp     #'S'            ; Interleave?
            beq     INTLV           ; Yes, set X = 0
            cmp     #'T'            ; Skew?
            bne     RETERR          ; No, return with error
            inx                     ; increment index to arg table ( 1 for T ) 
INTLV:      inx                     ; Increment index to arg table ( 0 for S )
            jsr     GETNEXTNB1      ; Get next non-blank from command line   
            beq     NOVAL           ; No more? Check if argument admits no value
            cmp     #'='            ; Value separator?
            bne     NOVAL           ; No, check if argument admits no value
            jsr     GETNEXTNB1      ; Advance one pos until next non-blank
            jsr     GETBYTE         ; Get parameter as a byte
            bcc     RETERR          ; No valid byte, error
            cmp     #NSECTS-1       ; Maximum interleave/skew is sectors-1
            bcs     RETERR          ; Error if bigger
STOR:       asl     a               ; Store as value << 2
            asl     a               ;
            beq     RETERR          ; 0 is no valid interleave/skew
            sta     ARGTABLE,x      ; Set value in arguments table
            jmp     GETARG          ; Get next argument
NOVAL:      cpx     #$00            ; Interleave (S) arg?
            bne     RETERR          ; No, skew value is mandatory 
            lda     #$03            ; Yes, use interleave = 3
            jmp     STOR            ; And store it
RETOK:      clc                     ; Return OK
            rts                     ;
RETERR:     sec                     ; Return with error
            rts                     ;
.endproc

; One drive operation
;
.proc ONEDRIVE
            bit     ODRIVES         ; Check if drive 0 is open
            bpl     CLOSED          ; No, continue
            ldx     #$00            ; Yes, close drive
            jsr     CLOSEDRV        ;
CLOSED:     jsr     OUTSTR
            .byte   $0D, "PUT IN DISK TO BE FORMATTED.", $00
            jsr     WARNING         ; Print warning message
            jsr     OUTSTR
            .byte   ".", $00
            jsr     CONFIRM         ; Get confirmation
            bcs     CONT            ; If Yes, go ahed
RETURN:     rts                     ; If No, return

CONT:       ldx     #$00            ; Set current drive
            stx     CURRDRV         ;
            jsr     FORMAT          ; And format it
            ldx     #$00            ; Open drive 0
            jsr     OPENDRV         ;
            ldx     #$00            ; Set current drive
            stx     CURRDRV         ;
            ldx     #$0C            ; Verify track 12
            jsr     VERIFY          ;
            bcs     RETURN          ; If error, return
            jsr     ASKTEST         ; Ask if test for bad sectors
            bcc     INITD           ; No, skip
            ldx     #$00            ; Init BAT of disk 0
            jsr     INITBAT         ;
            ldx     #$00            ; Set current drive
            stx     CURRDRV         ;
VRFYTRK:    jsr     VERIFY          ; Verify track
            bcs     RETURN          ; If error, return
            inx                     ; Next track
            cpx     #NCYLS          ; Are we past last track? 
            bcc     VRFYTRK         ; No, verify it
INITD:      ldx     #$00            ; Initt directory of disk 0
            jsr     INITDIR         ;
            jsr     GETVOLID        ; Get Volume ID from user
            ldx     #$00            ; Set Volume ID for disk 0
            jsr     SETVOLID        ;
            ldy     #$28            ; Mark overlay blocks (40 and, if SS, 41)
            lda     #BLKOVLY        ;
            sta     (BATP),y        ;
            iny                     ; Block 41
            bit     DRVNFO          ; Dual side?
            bmi     WBAT            ; Yes, skip
            sta     (BATP),y        ; No, mark block 41
WBAT:       jsr     WRTBAT          ; Update BAT on disk
            ldx     #$00            ; Close drive
            jsr     CLOSEDRV        ;
            jsr     OUTSTR          ; Print string
            .byte   $0D, "TO COPY SYSTEM,", $00
            jsr     COPYOVLS0       ; Copy overlays
            ldx     #$00            ; Init index to file copy table
            stx     FILEIDX         ;
FLOOP:      ldx     FILEIDX         ; Get index to file copy table
            lda     FILETBL,x       ; Get first char of name
            beq     DSKOPEN         ; Last. Print disk now open message and return
            jsr     CPYFNAM         ; Copy file name into current dir entry
            stx     FILEIDX         ; Update index to table
            jsr     COPYFILE0       ; Copy file to drive 0
            jmp     FLOOP           ; Loop until no more files
.endproc

.proc DSKOPEN
            jsr     OUTSTR
            .byte   $0D, "NEW DISK IS NOW OPEN.", $00
            rts
.endproc

.proc TWODRIVES
            bit     ODRIVES+1       ; Check if drive 1 is open
            bpl     CLOSED          ; No, continue
            ldx     #$01            ; Close drive 1
            jsr     CLOSEDRV        ;
CLOSED:     jsr     WARNING         ; Print warning message
            jsr     OUTSTR          ;
            .byte   "1.", $00
            jsr     CONFIRM         ; Get confirmation
            bcs     CONT            ; If it is a Yes, continue
RETURN:       rts                     ; If No, return

CONT:       ldx     #$01            ; Set current drive
            stx     CURRDRV         ;
            jsr     FORMAT          ; And format it
            ldx     #$01            ; Open drive 1
            jsr     OPENDRV         ;
            ldx     #$0C            ; Verify track 12
            jsr     VERIFY          ;
            bcs     RETURN          ; If error, return
            ldx     #$01            ; Init BAT of disk 1
            jsr     INITBAT         ;
            jsr     ASKTEST         ; Ask if test for bad sectors
            bcc     INITD           ; No, skip
            ldx     #$01            ; Set current drive
            stx     CURRDRV         ;
            ldx     #$00            ; Init track count
VRFYTRK:    jsr     VERIFY          ; Verify track
            bcs     RETURN          ; If error, return  
            inx                     ; Next track
            cpx     #NCYLS          ; Are we past last track?
            bcc     VRFYTRK         ; No, verify it
INITD:      ldx     #$01            ; Init directory of disk 1
            jsr     INITDIR         ;
            jsr     GETVOLID        ; Get Volume ID from user
            ldx     #$01            ; Set Volume ID for disk 1
            jsr     SETVOLID        ;
            jsr     WRTBAT          ; Update BAT on disk
            jsr     OUTSTR          ; Print string
            .byte   $0D, "WANT TO COPY DRIVE 0 SYSTEM", $00
            jsr     INPUTYN         ; Get Y/N answer
            sta     CPSYSRES        ; Save it for later
            bcs     MKOVLY          ; If it is a Yes, continue
            cmp     #'S'            ; Undocumented option: "S"?
            bne     DSKOPN          ; No, skip
MKOVLY:     ldx     #$01            ; Set current drive
            stx     CURRDRV         ;
            jsr     SETBATP         ; Set the BATP to the current drive's BAT
            ldy     #$28            ; Set Y to block 40 offset
            lda     #BLKOVLY        ; Mark it as overlay block
            sta     (BATP),y        ;
            iny                     ; Advance to block 41

            ; I believe the following to lines are a bug. They are marking
            ; an additional block as "overlay", rendering it unusable

            sta     (BATP),y        ; Mark it as overlay
            iny                     ; Advance to block 42
            bit     DRVNFO+1        ; Check if disk is dual side
            bmi     WBAT            ; Yes, skip
            sta     (BATP),y        ; No, mark block 42
WBAT:       jsr     WRTBAT          ; Update BAT on disk
            lda     CPSYSRES        ; Recover response
            cmp     #'S'            ; If undocumented option "S", skip system copy
            beq     DSKOPN          ; (Why? Was it a development test?)
            lda     #$01            ; Init overlay index
            sta     OVLINDEX        ;
REAOVL:     lda     OVLINDEX        ; Get overlay number
            cmp     NUMOVL          ; Are we past the last overlay?
            bcs     CPYFILES        ; Yes, break
            jsr     OVERLAY         ; Read overlay into memory
            jsr     WRTOVL1         ; Write overlay into disk 1
            inc     OVLINDEX        ; Increment overlay number
            jmp     REAOVL          ; And loop

CPYFILES:   ldx     #$00            ; Init index to system files table
            stx     FILEIDX         ;
CPYONE:     ldx     FILEIDX         ; Get file index
            lda     FILETBL,x       ; Load first file name char
            beq     DSKOPN          ; If null, we're done
            jsr     CPYFNAM         ; Copy file name into current dir entry
            stx     FILEIDX         ; Update index to table
            jsr     COPYFILE1       ; Copy file to drive 1
            jmp     CPYONE          ; Loop

DSKOPN:     jmp     DSKOPEN         ; Print disk now open message and return
.endproc

; Format drive CURRDRV
;
.proc FORMAT
            cld
            lda     #$00            ; Set DMA to read
            sta     HSRCW           ;
            sta     TRACK           ; Init track number
            sta     TEMP2           ; Init head number?
            ldx     CURRDRV         ; Get current drive
            stx     FMTDRVHD        ; Set drive/head into format command
            jsr     INITDRV         ; Init drive (SPECIFY + RECALIBRATE)
            lda     #$02            ; Init offset of physical sector number offset
            sta     PHYSECTOFF      ;   to first sector
SEEK:       ldx     CURRDRV         ; Get current drive
            lda     TRACK           ; Seek track
            jsr     SEEKTRK         ;

PREPFMT:    ldx     #(NSECTS*4)-1   ; Last byte of format data

            ; Prepare the format data into the buffer. Four bytes per sector:
            ;    00 -> Track number
            ;    01 -> Head number
            ;    02 -> Sector number
            ;    03 -> Bytes per sector ( 1 == 256 )
            ;
SECTDAT:    lda     #$01            ; Bytes per sector ( 1 == 256 )
            sta     DIRBUF,x        ;
            dex                     ;
            lda     #$FF            ; Init sector number ( will be updated later )
            sta     DIRBUF,x        ;
            dex                     ;
            lda     TEMP2           ; Head number 
            sta     DIRBUF,x        ;
            dex                     ;
            lda     TRACK           ; Track number
            sta     DIRBUF,x        ;
            dex                     ;
            bpl     SECTDAT         ; Repeat until last sector

            ; Update logical sector number information with interleave factor

            ldy     #$00            ; Init logical sector number
            lda     PHYSECTOFF      ; Get offset of current physical sector number
UPDLOG:     tax                     ; Transfer to X to use it as an index
SETSECT:    lda     DIRBUF,x        ; Get sector number
            cmp     #$FF            ; Uninitialized?
            beq     SETLOG          ; Yes, go set it
            inx                     ; Advance to next sector
            inx                     ;
            inx                     ;
            inx                     ;
            txa                     ;
            cmp     #NSECTS*4       ; Are we past the last sector
            bcc     SETSECT         ; No, repeat
            bcs     MOD             ; Go calculate the MOD (NSECTS*4)
            ; Not reached

SETLOG:     tya                     ; Set logical sector number
            sta     DIRBUF,x        ;
            iny                     ; Increment number
            cpy     #NSECTS         ; Are we past the last sector?
            beq     FMTTRCK         ; Yes, go format track
            txa                     ; Transfer sector 
            clc                     ;
            adc     ARGTABLE+_INTL  ; Adds interleave ( x4 )
            cmp     #NSECTS*4       ; Greater than last sector?
            bcc     UPDLOG          ; No, go update sector info

MOD:        sbc     #NSECTS*4       ; Yes, calculate MOD (NSECTS*4)
            jmp     UPDLOG          ;

            ; Format track with format data in DIRBUF

FMTTRCK:    dma     A, DIRBUF       ; A = DMA encoded address of DIRBUF
            stx     PHYSECTOFF      ; Update offset to physical sector
            sta     ADMA            ; Set DMA address
            lda     TEMP2           ; Get head
            beq     SEND            ; If head 0, skip
            lda     #$04            ; Update head in FORMAT command
            ora     FMTDRVHD        ;
            sta     FMTDRVHD        ;
SEND:       ldx     #_FORMAT        ; Send FORMAT command
            jsr     SNDCMD          ;
WAITINT:    lda     HSRCW           ; Wait for interrupt
            bmi     WAITINT         ;
            jsr     RSLTPH          ; Get results
            lda     DSKSTAT         ; Get status register
            and     #$D8            ; Mask-out don't care bits
            beq     WAIT            ; No errors
            jsr     ERROR32         ; Write-protected disk or formatting error
WAIT:       ldx     #$00            ; Wait at least 500uS after write to allow the write
WAITLP:     dex                     ; current to decay before seeking again
            bne     WAITLP          ;
            lda     FMTDRVHD        ; Get drive and header
            and     #$03            ; Mask-out header
            sta     FMTDRVHD        ;
            tax                     ; Transfer to X to use it as index
            lda     DRVNFO,x        ; Is it a two-side disk?
            bpl     NXTTRCK         ; No, next track
            lda     TEMP2           ; Yes, check head
            bne     NXTTRCKDH       ; If head == 1, next track
            inc     TEMP2           ; If head == 0, continue with head = 1
            lda     PHYSECTOFF      ; Get offset to physical sector info
            clc                     ; Add interleave
            adc     ARGTABLE+_INTL  ;
            cmp     #NSECTS*4       ;
            bcc     UPDINT          ;
            sbc     #NSECTS*4       ;
UPDINT:     sta     PHYSECTOFF      ;
            jmp     PREPFMT         ; Recalculate format data and format track

NXTTRCKDH:  dec     TEMP2           ; Set head = 0 again
NXTTRCK:    lda     PHYSECTOFF      ; Get offset to physical sector info
            clc                     ; Add skew
            adc     ARGTABLE+_SKEW  ;
            cmp     #NSECTS*4       ;
            bcc     UPDSKW          ;
            sbc     #NSECTS*4       ;
UPDSKW:     sta     PHYSECTOFF      ;
            inc     TRACK           ; Increment track number
            lda     TRACK           ; 
            cmp     #NCYLS          ; Are we past last track? 
            beq     RETURN          ; Yes, return
            jmp     SEEK            ; No, seek next track and format it
RETURN:     rts
.endproc

; Copy overlays to disk 0
;
.proc COPYOVLS0
            jsr     PUTSRCD         ; Ask for source disk in drive 0
            lda     LBUFADDR        ; Set dest buffer to large transient buffer
            sta     DESTBUFF        ;
            lda     LBUFADDR+1      ;
            sta     DESTBUFF+1      ;
            lda     #$01            ; Init overlay number
            sta     OVLINDEX        ;
REAOVL:     lda     OVLINDEX        ; Get current overlay index
            cmp     NUMOVL          ; Are we past last overlay
            bcs     WRITE           ; Yes, go write to disk 
            jsr     OVERLAY         ; Load overlay A from disk
            ldy     #$00            ; Init byte index for copy
REABYTE:    lda     OVLORG,y        ; Read one byte
            sta     (DESTBUFF),y    ;
            iny                     ; Next byte
            bne     REABYTE         ; Repeat until complete
            inc     DESTBUFF+1      ; Advance to next page in large transient buffer
            inc     OVLINDEX        ; Next overlay
            jmp     REAOVL          ; Loop

WRITE:      jsr     PUTDSTD         ; Ask for destination disk in drive 0
            lda     LBUFADDR        ; Set dest buffer to large transient buffer
            sta     DESTBUFF        ;
            lda     LBUFADDR+1      ;
            sta     DESTBUFF+1      ;
WRTOVL:     ldy     #$00            ; Init byte index for copy
WRTBYT:     lda     (DESTBUFF),y    ; Copy one byte
            sta     OVLORG,y        ;
            iny                     ; Next byte
            bne     WRTBYT          ; Repeat until complete
            jsr     WRTOVL0         ; Write overlay to disk 0
            ldx     OVLORG          ; Get current overlay number
            inx                     ; Next one
            txa                     ; Transfer to A
            cmp     NUMOVL          ; Are we past last overlay
            bcs     RETURN          ; Yes, return
            inc     DESTBUFF+1      ; Advance to next page in large transient buffer
            jmp     WRTOVL          ; Next overlay
RETURN:     rts
.endproc

; Copy system file to disk 1
;
.proc COPYFILE1
            ldx     #$00            ; Set current drive 0
            stx     CURRDRV         ;
            jsr     FOPEN0          ; Assigns channel 0 to file in DIRENT
            ldx     #$01            ; Set current drive to 1
            stx     CURRDRV         ;
            ldx     #$09            ; Look for a free channel
GETCHN:     lda     IOCHTBL,x       ; Get device/file
            beq     STCHN           ; Free, break
            dex                     ; If not, check next channel
            bne     GETCHN          ; Loop
            jsr     ERROR34         ; Not enough channels are free
STCHN:      stx     FILE1CHAN       ; Store channel
            jsr     ASSIGN          ; Assign to new file
            ldx     #$03            ; Set MEMBUFF, MEMCOUNT to LBUFADDR, LBUFSIZE
CPYPT:      lda     LBUFADDR,x      ;
            sta     MEMBUFF,x       ;
            dex                     ;
            bpl     CPYPT           ;
            ldx     #$00            ; Get MEMCOUNT bytes from channel 0
            jsr     GETMBUFF        ;
            bcs     RETURN          ; No more bytes, return
            lda     LBUFADDR        ; Update MEMBUFF
            sta     MEMBUFF         ;
            lda     LBUFADDR+1      ;
            sta     MEMBUFF+1       ;
            ldx     FILE1CHAN       ; Output to destination file's channel
            jsr     OUTMBUFF        ;
            jmp     CPYPT           ; Repeat until complete

RETURN:     ldx     FILE1CHAN       ; Get destination file channel
            jsr     FREECH          ; Free it
            jsr     FREECH0         ; Free channel 0
            rts
.endproc

; Copy system file to disk 0
;
.proc COPYFILE0
            lda     #$00            ; Init file size and file position
            ldx     #$05            ;
INIT:       sta     SFILPOS,x       ;
            dex                     ;
            bpl     INIT            ;
READ:       ldx     #$00            ; Clear the "More bytes to read" flag
            stx     MORETORD        ;
            stx     CURRDRV         ; Set drive to 0
            jsr     PUTSRCD         ; Ask for source disk in drive 0
            jsr     FOPEN0          ; Opendisk 0
            ldx     #$02
UPDPOS:     lda     SFILPOS,x       ; Update file position
            sta     FILEPOS,x       ;
            dex                     ;
            bpl     UPDPOS          ;
            ldx     #$00            ;
            jsr     FSEEK           ; Seek to FILEPOS
            ldx     #$03            ; Set MEMBUFF and MEMCOUNT to LBUFADDR and LBUFSIZE
SETMBUF:    lda     LBUFADDR,x      ;
            sta     MEMBUFF,x       ;
            dex                     ;
            bpl     SETMBUF         ;
            ldx     #$00            ; CHannel 0
            jsr     GETMBUFF        ; Get MEMCOUNT characters into (MEMBUFF)
            bcs     RETDST          ; If no bytes transferred, return
            lda     MEMCOUNT        ; Get bytes transferred
            sta     NBYTES          ; Save LSB into bytes count
            cmp     LBUFSIZE        ; Compare with LSB of LBUFSIZE (Generates borrow)
                                    ; if LBUFSIZE > MEMCOUNT) 
            lda     MEMCOUNT+1      ; Save MSB
            sta     NBYTES+1        ;
            sbc     LBUFSIZE+1      ; Substract LBUFSIZE+1 (plus borrow)
            bcc     GETFPOS         ; If read less than LBUFSIZE bytes, skip
            lda     #FLNORMAL       ; Set "More bytes to read" flag 
            sta     MORETORD        ;
GETFPOS:    lda     CURFINFO+_FPOS  ; Get position relative to of first data byte
            sec                     ; substracting the file header length
            sbc     #FHDRLEN        ;
            sta     SFILPOS         ;
            lda     CURFINFO+_FPOS+1 ;
            sbc     #$00            ;
            sta     SFILPOS+1       ;
            lda     CURFINFO+_FPOS+2 ;
            sbc     #$00            ;
            sta     SFILPOS+2       ;
            jsr     PUTDSTD         ; Ask for destination disk in drive 0
            jsr     ASSIGN0         ; Assign to channel 0
            ldx     #$02            ; Set destination file position to end
SETFPOS:    lda     SFILSIZ,x       ; of file
            sta     FILEPOS,x       ;
            dex                     ;
            bpl     SETFPOS         ;
            ldx     #$00            ; Channel 0
            jsr     FSEEK           ; Seek to FILEPOS
            lda     LBUFADDR        ; Point MEMBUFF to LBUFADDR
            sta     MEMBUFF         ;
            lda     LBUFADDR+1      ;
            sta     MEMBUFF+1       ;
            lda     NBYTES          ; Set MEMCOUNT to actual bytes read
            sta     MEMCOUNT        ;
            lda     NBYTES+1        ;
            sta     MEMCOUNT+1      ;
            ldx     #$00            ; Channel 0
            jsr     OUTMBUFF        ; Write buffer to disk file
            ldx     #$02            ; Update new file size
UPDSIZ:     lda     SFILPOS,x       ;
            sta     SFILSIZ,x       ;
            dex                     ;
            bpl     UPDSIZ          ;
            bit     MORETORD        ; More bytes to read?
            bpl     RETURN          ; No, return
            jmp     READ            ; Yes, loop

RETDST:     jsr     PUTDSTD         ; Ask for destination disk in drive 0
RETURN:     jsr     FREECH0         ; Free channel 0
            rts                     ;
.endproc

; Copy file name to FNAMBUF in current DIRENT
;
.proc CPYFNAM
            ldy     #$00            ; Init index to name in FNAMBUF
LOOP:       lda     FILETBL,x       ; Copy char
            sta     FNAMBUF,y       ;
            inx                     ; Advance to next char
            iny                     ;
            cmp     #'.'            ; Extension separator?
            bne     LOOP            ; No, continue
            lda     FILETBL,x       ; Copy extension
            sta     FNAMBUF,y       ;
            inx                     ; Advance index to file name table
            rts
.endproc

; Ask for source disk in drive 0
;
.proc PUTSRCD
            bit     ODRIVES         ; Check if drive 0 is open
            bpl     CONT            ; No, continue
            ldx     #$00            ; Yes, close drive 0
            jsr     CLOSEDRV        ;
CONT:       jsr     OUTSTR
            .byte   $0D, "PUT SOURCE DISK IN.", $00
            jsr     CONFIRM         ; Get confirmation
            bcs     RETURN          ; It is a Yes, open drive and return
            jmp     EXIT            ; It is a no, exit program
RETURN:     ldx     #$00            ; Open drive 0
            jsr     OPENDRV         ;
            rts
.endproc

; Exit program: Jump to system warm start
;
EXIT:       jmp     WARMST

; Ask for destination disk in drive 0
;
.proc PUTDSTD
            bit     ODRIVES         ; Check if drive 0 is open
            bpl     CONT            ; No, continue
            ldx     #$00            ; Yes, close drive
            jsr     CLOSEDRV        ;
CONT:       jsr     OUTSTR          ; Print string
            .byte   $0D, "PUT NEW DEST. DISK IN.", $00
            jsr     CONFIRM         ; Get confirmation
            bcc     EXIT            ; It's a No, exit program
            ldx     #$00            ; It's a Yes, open drive 0
            jsr     OPENDRV         ;
            rts
.endproc

.proc CONFIRM
            jsr     OUTSTR
            .byte   $0D, "ARE YOU READY", $00
            jsr     INPUTYN         ; Get Y/N answer
            rts
.endproc

.proc ASKTEST
            jsr     OUTSTR
            .byte   $0D, "WANT TO TEST FOR BAD SECTORS", $00
            jsr     INPUTYN         ; Get Y/N answer
            rts
.endproc

.proc WARNING
            jsr     OUTSTR
            .byte   $0D, "WARNING: FORMAT WILL IRREVOCABLY"
            .byte   $0D, "ERASE EVERYTHING ON DISK IN DRIVE ", $00
            rts
.endproc

; Get Y/N answer from console
;
; Returns Cy clear if No, Cy set if Yes
;
.proc INPUTYN
            jsr     OUTSTR
            .byte   " (Y/N)?= ", $00
            ldx     #$01
            jsr     GETLINE         ; Get line from console
            jsr     GETNEXTNB       ; Get first non-blank
            beq     RETCS           ; If none, assume it is a Yes (Bad UX decission!)
            cmp     #'Y'            ; Return with carry set if Yes
            beq     RETCS           ;
            clc                     ; It is a No, return with carry clear
            rts                     ;
RETCS:      sec                     ; It is a Yes, return with carry set
            rts                     ;
.endproc

; Ask user for the volume ID
;
.proc GETVOLID
            jsr     OUTSTR
            .byte   $0D, "DISK VOLUME SERIAL NO. (VSN)?= ", $00
            ldx     #$01            ; Get line from console
            jsr     GETLINE         ;
            jsr     GETNEXTNB       ; Get first non-blank
            ldx     #_MEMBUFF       ; Eval expression and store it into MEMBUFF
            jsr     EVALEXP         ;
            bcc     GETVOLID        ; If no valid number, insist
            lda     MEMBUFF         ; Store into VOLID
            sta     VOLID           ;
            lda     MEMBUFF+1       ;
            sta     VOLID+1         ;
            rts
.endproc

; Init BAT for drive X
;
.proc INITBAT
            stx     CURRDRV         ; Set current drive
            jsr     SETBATP         ; Set the BATP to the current drive's BAT
            ldy     #$00            ; Init BAT to all 0
            tya                     ;
LOOP:       sta     (BATP),y        ; Clean one byte
            iny                     ;
            bne     LOOP            ; Repeat until all done
            rts
.endproc

; Set Volume Id into disk's BAT
;
.proc SETVOLID
            stx     CURRDRV         ; Set current drive
            jsr     SETBATP         ; Set the BATP to the current drive's BAT
            ldy     #_BTVSN         ; Index to Disk Volume Number
            lda     VOLID           ; Get VOLID
            sta     (BATP),y        ; Save it into the BAT
            iny                     ;
            lda     VOLID+1         ;
            sta     (BATP),y        ;
            rts
.endproc

; Init directory info in drive X
;
.proc INITDIR
            stx     CURRDRV         ; Set current drive
            lda     #$00            ; Init index and filler value 
            tax                     ;
CLR1ST:     sta     DIRBUF,x        ; Clear first sector
            inx                     ;
            bne     CLR1ST          ;
            lda     #$11            ; Last directory/BAT sector in track 12 
            sta     SECTNUM         ; Set sector number
CLEAR:      ldx     CURRDRV         ; Get current drive
            jsr     WRTRCK12        ; Write sector into track 12
            dec     SECTNUM         ; Continue
            bne     CLEAR           ; Until sector 1
            rts
.endproc

; Write current overlay to drive 1
;
; Parameters: Overlay number in OVLORG
;
.proc WRTOVL1
            ldx     #$00            ; Init head
            stx     CHEAD           ;
            lda     OVLORG          ; Get loaded overlay
            ldx     #$0C            ; Init track of overlays
            cmp     #$09            ; Check if it is one of the lower half of overlays 
            bcc     SKTRACK         ; Yes, so it is in block 40, no matter what
            inx                     ; No, it is in track 13 (if SS)
            bit     DRVNFO+1        ; Is it dual side?
            bpl     SKTRACK         ; No, go seek track
            dex                     ; Yes, return to track 12
            lda     #$01            ; And head 1
            sta     CHEAD           ;
SKTRACK:    txa                     ; Transfer track to A for CKSEEKTRK
            ldx     #$01            ; Drive 1
            jsr     CKSEEKTRK       ; Checks drive valid and seeks track A on drive X
            dma     X, OVLORG       ; DMA address for overlays 
            stx     CURFINFO+_DMABF ; Set buffer in current FINFO
            ldx     #$01            ; Set drive 1
            lda     OVLORG          ; Get overlay number (base 1)
            clc                     ; Clear carry for addition
            adc     #$11            ; Calculate overlay sector (the first 18 sectors
                                    ; are used by BATs and directory)
            cmp     #NSECTS         ; Compare with sectors per track
            bcc     WRITE           ; If smaller, skip
            bit     DRVNFO+1        ; Is disk dual side?
            bmi     WRITE           ; Yes, skip
            sec                     ; No, use MOD NSECTS
            sbc     #NSECTS         ;
WRITE:      ldx     #$01            ; Set drive 1
            jsr     WRITSECT        ; Write sector
            rts
.endproc

; Write current overlay to drive 0
;
; Parameters: Overlay number in OVLORG
;
.proc WRTOVL0
            lda     #$00            ; Init head
            sta     CHEAD           ;
            lda     OVLORG          ; Get loaded overlay
            ldx     #$0C            ; Init track of overlays
            cmp     #$09            ; Check if it is one of the lower half of overlays 
            bcc     SKTRACK         ; Yes, so it is in block 40, no matter what
            inx                     ; No, it is in track 13 (if SS)
            bit     DRVNFO          ; Is it dual side?
            bpl     SKTRACK         ; Yes, so it is in block 40, no matter what
            dex                     ; Yes, return to track 12
            lda     #$01            ; And head 1
            sta     CHEAD     
SKTRACK:    txa                     ; Transfer track to A for CKSEEKTRK
            ldx     #$00            ; Drive 0
            jsr     CKSEEKTRK       ; Checks drive valid and seeks track A on drive X
            dma     X, OVLORG       ; DMA address for overlays 
            stx     CURFINFO+_DMABF ; Set buffer in current FINFO
            ldx     #$00            ; Set drive 0
            lda     OVLORG          ; Get overlay number (base 1)
            clc                     ; Clear carry for addition
            adc     #$11            ; Calculate overlay sector (the first 18 sectors
                                    ; are used by BATs and directory)
            cmp     #NSECTS         ; Compare with sectors per track
            bcc     WRITE           ; If smaller, skip
            bit     DRVNFO          ; Is disk dual side?
            bmi     WRITE           ; Yes, skip
            sec                     ; No, use MOD NSECTS
            sbc     #NSECTS         ;
WRITE:      ldx     #$00            ; Set drive 0
            jsr     WRITSECT        ; Write sector
            rts
.endproc

; Verify track X
;
.proc VERIFY
            stx     VERTRACK        ; Set track to verify
            cld                     ;
            ldx     CURRDRV         ; Get current drive
            lda     DRVNFO,x        ; Get dual-side info
            sta     DSFLAG          ; Store it
            bpl     SSIDE           ; Single side
            lda     #NSECTS*2       ; Double the sectors per track
            bne     STORE           ; Go store sectors per track
            ; Not reached

SSIDE:      lda     #NSECTS         ; 26 sectors per track
STORE:      sta     SECSTRK         ;
            lda     #<MYERRRCVRY    ; Set error recovery routine
            sta     ERRRCVRYP       ;
            lda     #>MYERRRCVRY    ;
            sta     ERRRCVRYP+1     ;
            lda     RAND            ; Init random number generator seed
            sta     SEED            ;
            lda     RAND+1          ;
            sta     SEED+1          ;
            lda     #$00            ; Init sector number
            sta     SECTOR          ;
WRITE:      jsr     WRTRAND         ; Fill sector with a pseudo-random series of bytes
            lda     SECTOR          ; 
            adc     #$03            ; Fill sectors in this order: 0, 3, 6 ..
            sta     SECTOR          ; .. 252, 255, .. 2, 5 .. 254, 1, 4, ..
            sec                     ; Clear borrow for substraction
            sbc     SECSTRK         ; Substract sectors per track
            beq     CHECK           ; If 0, then last sector reached
            bcc     WRITE           ; If SECTOR < SECSTRK, go fill sector
            sta     SECTOR          ; If overflow, SECTOR = SECTOR - SECSTRK
            jmp     WRITE           ; Loop until last sector

CHECK:      lda     SEED            ; Reset random number generator with same seed
            sta     RAND            ;
            lda     SEED+1          ;
            sta     RAND+1          ;
            lda     #$00            ; Init sector number
            sta     SECTOR
CHKSEC:     jsr     CHKRAND         ; Checks sector content
            bcc     NEXT            ; If OK, go check next sector
            jsr     HANDLERR        ; Not OK, handle error
            bcc     NEXT            ; Was recoverable, check next
            sec                     ; Why? It is already set...
            bcs     RETERR           ; Was unrecoverable, return with error
NEXT:       lda     SECTOR          ; Get curremt sector number
            clc                     ; Clear carry for addition
            adc     #$03            ; Check sectors in this order: 0, 3, 6 ..
            sta     SECTOR          ; .. 252, 255, .. 2, 5 .. 254, 1, 4, ..
            sec                     ; Clear borrow for substraction
            sbc     SECSTRK         ; Substract sectors per track
            beq     RETOK           ; If 0, then last sector reached
            bcc     CHKSEC          ; If SECTOR < SECSTRK, go check sector
            sta     SECTOR          ; If overflow, SECTOR = SECTOR - SECSTRK
            jmp     CHKSEC          ; Loop until last sector

RETOK:      clc
RETERR:     ldx     VERTRACK        ; Restore X
            lda     #<ERRRCVRY      ; Restore normal error recovery routine
            sta     ERRRCVRYP       ;
            lda     #>ERRRCVRY      ;
            sta     ERRRCVRYP+1     ;
            rts
.endproc

; Custom error recovery routine
;
.proc MYERRRCVRY
            sta     SAVEACC         ; Save accumulator
            lda     ERRNUM          ; Get error number
            cmp     #$28            ; Unformatted diskette or drive went not-ready?
            beq     HANDLE          ; Yes, handle error
            cmp     #$1E            ; Unformatted disk or unrecoverable read/write error?
            beq     HANDLE          ; Yes, handle error
            lda     #<ERRRCVRY      ; No, restore normal error recovery routine
            sta     ERRRCVRYP       ;
            lda     #>ERRRCVRY      ;
            sta     ERRRCVRYP+1     ;
            lda     SAVEACC         ; Recover accumulator
            jmp     ERRRCVRY        ; And jump to system error recovery

HANDLE:     sty     SAVEYRG         ; Save Y
            lda     #$00            ; Clear error number
            sta     ERRNUM          ;
            jsr     HANDLERR        ; Handle error
            php                     ; Save flags
            jsr     OUTSTR          ; Print if was a CRC error
            .byte   " (CRC)", $00
            ldy     SAVEYRG         ; Recover Y reg
            plp                     ; Recover flags
            bcs     EXIT            ; If unrecoverable, exit program
            rts                     ; Else, return
EXIT:       jmp     WARMST          ; Jumps to system's wart start
.endproc

; Handle format error. If just a bad sector in a data track, mark it into the BAT and
; returns carry clear. Any other problem is not recoverable. return carry set
; 
.proc HANDLERR
            jsr     OUTSTR
            .byte   $0D, "BAD TRACK $", $00
            ldy     #$00            ; Set index to output buffer
            lda     VERTRACK        ; Print track as hex to console 
            jsr     HEXBYTE         ;
            jsr     POUTBUFF02      ;
            jsr     OUTSTR
            .byte   " SECTOR $", $00
            lda     SECTOR          ; Print sector as hex to console
            ldy     #$00            ;
            jsr     HEXBYTE         ;
            jsr     POUTBUFF02      ;
            jsr     BLOCKNUM        ; Get block number in Y where error occurred
            bcs     UNUSABLE        ; System track, can't be bypassed
            jsr     SETBATP         ; Set the BATP to the current drive's BAT
            lda     #BLKUNUS        ; Mark block Y as unusable
            jsr     SETNEXTBLK      ;
            jsr     OUTSTR          ; Print string
            .byte   " BYPASSED.", $00
            clc                     ; Return OK
            rts                     ;
UNUSABLE:   jsr     OUTSTR          ; Print string
            .byte   " MAKES DISK UNUSABLE.", $00
            sec                     ; Return error
            rts                     ;
.endproc

; Gets block number where error occurred.
;
; Returns: SECTCOUNT contains block number
;          Cy clear if recoverable
;          Cy set if not
;
.proc BLOCKNUM
            lda     #$00            ; Init sector count
            sta     SECTCOUNT       ;
            sta     SECTCOUNT+1     ;

            ; Calculate sector number where error occurred

            ldx     VERTRACK        ; Get track
            inx                     ; Increase to prepare loop
LOOP:       dex                     ; Check if track 0
            beq     ADDCURR         ; Yes, break loop
            lda     SECSTRK         ; No, get sectors per track
            clc                     ; Clear carry to prepare for addition
            adc     SECTCOUNT       ; Adds sectors to sector count
            sta     SECTCOUNT       ;
            bcc     LOOP            ;
            inc     SECTCOUNT+1     ;
            jmp     LOOP            ;

ADDCURR:    lda     SECTCOUNT       ; Add current sector to sector count
            clc                     ; Clear carry to prepare for addition
            adc     SECTOR          ; Add sector
            sta     SECTCOUNT       ;
            bcc     GETTRK          ;
            inc     SECTCOUNT+1     ;

GETTRK:     lda     VERTRACK        ; Get current track
            cmp     #$0C            ; Is it track 12 (holds BATs and directory and
                                    ; first 8 overlays)?
            beq     RETERR          ; Yes, error is unrecoverable
            bcc     CALCBLK         ; If it is lower, continue to calculate block num 
            lda     SECTCOUNT       ; It is higher, substract the 18 sectors of the
            sbc     #$12            ; directory info from the sector count (borrow
            sta     SECTCOUNT       ; is already cleared)
            lda     SECTCOUNT+1     ;
            sbc     #$00            ;
            sta     SECTCOUNT+1     ;
            
            ; Calculate block number:
            ;   Block size for SS disks is 2048 bytes or 8 sectors
            ;                  DS disks is 4096 bytes or 16 sectors
            ; 
CALCBLK:    ldx     #$03            ; Init number of rotations (divisions by two)
                                    ; to calculate block number for SS disk
            bit     DSFLAG          ; Is it a dual side disk?
            bpl     DIVIDE          ; No, skip
            ldx     #$04            ; Yes, then add one division
DIVIDE:     lsr     SECTCOUNT+1     ; Divide by sects/block to calculate block number
            ror     SECTCOUNT       ;
            dex                     ;
            bne     DIVIDE          ;
                                    ; Now SECTCOUNT contains the block number in base 0
            inc     SECTCOUNT       ; Convert to base 1
            ldy     SECTCOUNT       ; And store it into Y
            bit     DSFLAG          ; Dual side disk?
            bmi     RETOK           ; Yes, return OK
            cpy     #$29            ; Block 41? (overlays)
            beq     RETERR          ; Yes, return error 
RETOK:      clc                     ; Return OK
            rts                     ;
RETERR:     sec                     ; Return error
            rts                     ;
.endproc

; Checks sector content against a pseudo-random series of bytes
;
; Parameters: Sector number in SECT, track in VERTRACK, drive in CURRDRV
; Returns: Cy clear if match, Cy set if not
;
.proc CHKRAND
            lda     #$00            ; Start with head 0
            sta     CHEAD           ;
            lda     SECTOR          ; Get SECTOR
            cmp     #NSECTS         ; Are we past NSECTS?
            bcc     GETDRV          ; No, skip
            inc     CHEAD           ; Yes, head 1
GETDRV:     ldx     CURRDRV         ; Get current drive
            lda     VERTRACK        ; Get current track
            jsr     CKSEEKTRK       ; Checks drive valid and seeks track A on drive X
            dma     A, DIRBUF       ; A = DMA encoded address of DIRBUF
            sta     CURFINFO+_DMABF ; Set buffer in current FINFO
            ldx     CURRDRV         ; Get current drive
            lda     SECTOR          ; Get sector
            jsr     READSECT        ; Read sector into DIRBUF
            ldx     #$00
LOOP:       jsr     RANDOM          ; Generate a pseudo-random byte in A
            cmp     DIRBUF,x        ; Compare against the content in DIRBUF
            bne     RETERR          ; It is not a match, error
            inx                     ; Go check next
            bne     LOOP            ; Loop until end of buffer
            clc                     ; Return OK
            rts                     ;
RAND:       jsr     RANDOM          ; Syncs the random number generator before exiting
RETERR:     inx                     ;
            bne     RAND            ;
            sec                     ; Return error
            rts
.endproc

; Fill sector with a pseudo-random series of bytes
;
; Parameters: Sector number in SECT, track in VERTRACK, drive in CURRDRV
;
.proc WRTRAND
            ldx     #$00
LOOP:       jsr     RANDOM          ; Generate a pseudo-random byte in A
            sta     DIRBUF,x        ; Store in DIRBUF,x
            inx                     ;
            bne     LOOP            ; Repeat until DIRBUF full
            lda     #$00            ; Start with head 0
            sta     CHEAD           ;
            lda     SECTOR          ; Get SECTOR
            cmp     #NSECTS         ; Are we past NSECTS?
            bcc     GETDRV          ; No, skip
            inc     CHEAD           ; Yes, head 1
GETDRV:     ldx     CURRDRV         ; Get current drive
            lda     VERTRACK        ; Get current track
            jsr     CKSEEKTRK       ; Checks drive valid and seeks track A on drive X
            dma     A, DIRBUF       ; A = DMA encoded address of DIRBUF
            sta     CURFINFO+_DMABF ; Set buffer in current FINFO
            ldx     CURRDRV         ; Get current drive
            lda     SECTOR          ; Get sector
            jsr     WRITSECT        ; Write sector
            rts
.endproc

; Generate a deterministic pseudo-random number and return it in A
;
; NOTE: LFSR?
;
.proc RANDOM
            lda     RAND
            lsr     a
            eor     RAND
            lsr     a
            lsr     a
            eor     RAND
            lsr     a
            eor     RAND+1
            lsr     a
            lsr     a
            lsr     a
            lsr     a
            rol     RAND+1
            rol     RAND
            lda     RAND
            rts
.endproc

            .bss

TRACK:      .res    1               ; Track number
            .res    1               ; Reserved
PHYSECTOFF: .res    1               ; Physical sector number offset in format data
FILEIDX:    .res    1               ; Index to file copy table
OVLINDEX:   .res    1               ; Current overlay index
FILE1CHAN:  .res    1
SFILPOS:    .res    3               ; Sistem file position
SFILSIZ:    .res    3               ; System file size
MORETORD:   .res    1               ; Set if there are more bytes to read from file
NBYTES:     .res    2               ; Bytes transferred
VOLID:      .res    2               ; Volume ID
CPSYSRES:   .res    1               ; Response to "want to copy drive 0 system?"
VERTRACK:   .res    1               ; Track to be verified
SECTOR:     .res    1               ; Sector number
SECTCOUNT:  .res    2               ; Holds calculated sect number where error occurred
RAND:       .res    2               ; Latest generated pseudo-random number
SEED:       .res    2               ; Pseudo-random number generator seed
SAVEACC:    .res    1               ; Save accumulator on error recovery
SAVEYRG:    .res    1               ; Save Y register on error recovery

            .data

ARGTABLE:   .byte   DEFINTR<<2      ; Interleave * 4 
            .byte   DEFSKEW<<2      ; Skew * 4

            ; File copy table
            ;
FILETBL:    .byte   "CODOS.Z"
            .byte   "COMDPROC.Z"
            .byte   "SYSERRMSG.Z"
            .byte   "SVCPROC.Z"
            .byte   "STARTUP.J"
            .byte   "DIR.C"
            .byte   "IODRIVER.Z"
.if CODOS2_VER <> 14
            .byte   "GRAPHDRIVER.Z"
            .byte   "PRINTDRIVER.Z"
.endif
            .byte   $00
