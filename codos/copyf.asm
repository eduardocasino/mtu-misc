; da65 V2.18 - Ubuntu 2.19-1
; Created:    2025-05-27 17:16:49
; Input file: copyf_14.bin
; Page:       1


            .setcpu "6502"

            .include "symbols.inc"

            .include "codos.inc"

            .code

            ; Loadable file data
            ;
            .byte   $58             ; CODOS loadable file header byte
            .byte   $00             ; Memory overlay
            .byte   $00             ; Memory bank
            .byte   $00             ; Reserved
            .addr   START           ; Entry point
            .addr   START           ; Load address
            .word   PROG_SIZE       ; Memory image size

.proc START
            lda     #$01            ; Unprotect SYSRAM
            sta     HSRCW           ;
            cld
            jsr     SETOUTBCH       ; Set output buffer
            ldy     CMDLIDX         ; Get command line index
            jsr     GETNEXTNB       ; Get next non-blank
            sty     CURINDEX        ; Save command line index to first arg
            bne     GETSPATTRN      ; Non-blank found, get source file pattern
            jsr     SETUPATTRN      ; No file name, set universal pattern for orig
            jmp     SETDRIVES       ; Yes, go set source and dest drives 

GETSPATTRN: jsr     SETSRCFNAM      ; Set source file name (or pattern) and drive
            bit     ISPATTERN       ; Is it a file pattern?
            bpl     GETSFNAM        ; No, go get normal source file name
            jmp     SETDRIVES       ; Yes, go set source and dest drives

GETSFNAM:   ldy     CURINDEX        ; Restore command line index to first arg
            jsr     GETFILNDRV      ; Get source file name and disk
            jsr     FOPEN0          ; Assign file to channel 0
.if ::CODOS2_VER <> 14
            ; CODOS 1.5 fix
            ;
            ; Gets and stores the terminator byte of the source file date

            ldy     #FHDR::DATE+DATELEN
            lda     (CURFINFO+FINFO::BUFF),y
            sta     DATETERM
.endif
            ldy     CMDLIDX         ; Get command line index (points to drive, if specified)
            jsr     GETNEXTNB       ; Get next non-blank
            beq     DEFDSTDRV       ; None, use default destination drive
            jsr     ISNUM           ; Is it a num?
            bcc     GETDSDRIVE      ; Yes, go get it
            jsr     GETFILNDRV      ; No, it is a file:drive type of arg
ASGNDSTCH:  jsr     GETFREECH       ; Get a free channel in X
            stx     DSTCHANN        ; And save it
            jsr     ASSIGN          ; Assign it to our dest file
.if ::CODOS2_VER <> 14
            lda     DATETERM        ; Updates the date terminator byte in dest file
            ldy     #FHDR::DATE+DATELEN
            sta     (CURFINFO+FINFO::BUFF),y
.endif
            bit     ASSIGNFLAG      ; Check if exists
            bpl     COPYF           ; No, go copy file
            bit     SAVEOVERWR      ; Yes, check if overwrite is allowed
            bmi     COPYF           ; Yes, go copy file
            ldx     DSTCHANN        ; No, free channel
            jsr     FREECH          ;
            jsr     ERROR25         ; New file name is already on selected diskette
            ; Not reached
COPYF:      jsr     COPYFILE        ; Copy file from source channel 0 to DSTCHANN
            rts                     ; And return

            ; NOTE 1: If source drive =...  then default dest drive =...
            ;           0                     1
            ;           1                     0
            ;           2                     3
            ;           3                     2
            ;
DEFDSTDRV:  lda     CURRDRV         ; Set dest drive depending on src drive
            eor     #$01            ;
            sta     CURRDRV         ;
            jmp     ASGNDSTCH       ; And continue to assign channel to dest file

GETDSDRIVE: jsr     GETDRIVEOPND    ; Get drive from command line and ensure it's open
            jmp     ASGNDSTCH

SETDRIVES:  ldx     CURRDRV         ; Get current drive
            stx     SRCDRIVE        ; Save it as source drive
            jsr     DRVVALIDO       ; Chech that it is valid and open it
            ldy     CMDLIDX         ; Get command line index
            jsr     GETNEXTNB       ; Get next non-blank
            beq     SETDEFDSTD      ; If none, set default destination drive
            jsr     GETDRIVEOPND    ; Get dest drive from command line and open it 
            stx     DSTDRIVE        ; Save it
            jmp     GETNFILES       ; And continue to get num of files on source disk

SETDEFDSTD: lda     SRCDRIVE        ; Get source drive and calculate dest (see NOTE 1)
            eor     #$01            ;
            sta     DSTDRIVE        ;

GETNFILES:  ldx     SRCDRIVE        ; Set source drive as current
            stx     CURRDRV         ;
            jsr     SETBATP         ; Set the BATP to the current drive's BAT
            ldy     #BAT::NENT      ; Get number of files on source disk
            lda     (BATP),y        ;
            sta     NUMFILES        ; And save them
            bne     SRCHMATCH       ; If any, go find matches against source pattern
RETURN:     rts                     ; None, abort to command processor

SRCHMATCH:  lda     #$01            ; Init DIRPOINT to first entry
            sta     DIRPOINT        ;
            sta     SECTNUM         ; First directory sector
            jsr     RDSECTNTR12     ; Read directory sector
CHKENTRY:   ldx     DIRPOINT        ; Get directory entry pointer
            lda     DIRBUF,x        ; Get first char of entry's filename
            beq     NEXTENTRY       ; If null (deleted entry), go to next entry
            jsr     ISMATCH         ; Is the entry's file name a match?
            bcs     DECNFILES       ; No match, decrement nfiles and check next one
            jsr     COPYIFNEW       ; Is a match, copy file if doesn't exist on dest
DECNFILES:  dec     NUMFILES        ; Decrement number of files
            beq     RETURN          ; No more files, return
NEXTENTRY:  jsr     NXTDIRENT       ; Point DIRPOINT to next entry in buffer
            jmp     CHKENTRY        ; And process entry
.endproc

; Copy file from source if it does not exist on dest
;
.proc   COPYIFNEW
            ldx     SECTNUM         ; Save current sector number
            stx     CURSECTN        ;
            jsr     COPYFNAM        ; Copy orig fname to new dirent and to output buffer
            ldx     #$02            ; Output to console
            jsr     OUTCR           ; First a CR
            jsr     POUTBUFF02      ; Then the name
            ldx     DSTDRIVE        ; Set dest drive as current
            stx     CURRDRV         ;
            jsr     FEXIST          ; Check if dest file exist
            beq     EXISTS          ; Yes, skip copying
            jsr     GETFREECH       ; No, assign a free channel to dest file
            stx     DSTCHANN        ;
.if ::CODOS2_VER = 17
            ldx     SRCDRIVE        ; Make source drive the current drive
            stx     CURRDRV         ;
            jsr     FOPEN0          ; Assign source file to channel 0
            
            ; CODOS 1.7 fix
            ;
            ; Gets and stores the terminator byte of the source file date
            
            ldy     #FHDR::DATE+DATELEN 
            lda     (CURFINFO+FINFO::BUFF),y
            sta     DATETERM

            lda     DSTDRIVE        ; Make destination file the current file
            sta     CURRDRV         ;
            ldx     DSTCHANN        ; Assign destination file to dest channel
            jsr     ASSIGN          ;

            lda     DATETERM        ; Updates the date terminator byte in dest file
            ldy     #FHDR::DATE+DATELEN
            sta     (CURFINFO+FINFO::BUFF),y
.else
            jsr     ASSIGN          ;
            ldx     SRCDRIVE        ; Set source file as current
            stx     CURRDRV         ;
            jsr     FOPEN0          ; Assign channel 0 to source file
.endif
            jsr     COPYFILE        ; Copy file from source channel 0 to DSTCHANN 
            jsr     OUTSTR          ; And inform user
            .byte   " COPIED.", $00
            jmp     DONE

EXISTS:     lda     #':'            ; Disk separator
            ldx     #$02            ; Console output
            jsr     OUTCHAR         ; Print the separator
            lda     DSTDRIVE        ; Get the destination drive
            clc                     ;
            adc     #'0'            ; Convert to ascii
            jsr     OUTCHAR         ; Output to console
            jsr     OUTSTR          ; And output message
            .byte   " ALREADY EXISTS.", $00

DONE:       ldx     CURDIREP        ; Recover current directory entry pointer
            stx     DIRPOINT        ;
            ldx     CURSECTN        ; Recover current directory sector
            stx     SECTNUM         ;
            ldx     SRCDRIVE        ; Recover current source drive
            stx     CURRDRV         ;
            jsr     SETBATP         ; Set BATP to current drive's BAT
            jsr     RDSECTNTR12     ; Read directory sector
            rts                     ; And return
.endproc

; Get a free channel
;
; Returns channel number in X
;
.proc GETFREECH
            ldx     #$09            ; Max channels
LOOP:       lda     IOCHTBL,x       ; Get device
            beq     RETURN          ; If 0, unasigned. We got it.
            dex                     ; Check next
            bne     LOOP            ; Until no more
            jsr     ERROR34         ; Not enough channels are free
            ; Not reached
RETURN:     rts
.endproc

; Copy file from channel 0 to DSTCHANN (destination file)
;
.proc COPYFILE
COPY:       lda     LBUFADDR        ; Set buff addr and length to large transient buffer
            sta     MEMBUFF         ;
            lda     LBUFADDR+1      ;
            sta     MEMBUFF+1       ;
            lda     LBUFSIZE        ;
            sta     MEMCOUNT        ;
            lda     LBUFSIZE+1      ;
            sta     MEMCOUNT+1      ;
            ldx     #$00            ; Read from channel 0 (source) into buffer
            jsr     GETMBUFF        ;
            bcs     FINISH          ; Nothing read, finish up
            lda     LBUFADDR        ; Set buff addr and length to large transient buffer
            sta     MEMBUFF         ;
            lda     LBUFADDR+1      ;
            sta     MEMBUFF+1       ;
            ldx     DSTCHANN        ; Set dest channel
            jsr     OUTMBUFF        ; Write to dest channel
            jmp     COPY            ; Repeat until no more data

FINISH:     ldx     DSTCHANN        ; Get dest channel  
.if ::CODOS2_VER <> 14
            jsr     FTRUNC          ; Truncate dest file
            ldx     DSTCHANN        ; Get dest channel again
.endif
            jsr     FREECH          ; And free it
            jsr     FREECH0         ; And source channel 0
            rts
.endproc

; Set source file name (or pattern) and drive from command line
;
.proc SETSRCFNAM
            ldx     DEFDRV          ; Get default drive (origin)
            stx     CURRDRV         ; Save it
            ldx     #$00            ; Init pattern flag
            stx     ISPATTERN       ;
            jsr     ISALPHA         ; Is the first char alphabetic?
            bcs     NAMPATERN       ; No, then it should be a pattern
LOOP:       cpx     #FNAMLEN-2      ; Have we reached file name length, minus extension? 
            bcs     ERR12           ; Yes, it is an invalid file name
            sta     SFILENAME,x     ; No, copy char and go for the
            inx                     ;   next one
            jsr     GETNEXTCH1      ; Advance one pos and get next char
            beq     SETDEFEXT       ; No more, then no extension provide. Set default.
            cmp     COLON           ; Is it the drive separator?
            beq     SETDEFEXT       ; Yes, then no extension provided. Set default.
            jsr     VALFNCHR        ; Is it a valid file name char?
            bcc     LOOP            ; Go store and continue
            cmp     #' '            ; Is it a blank?
            beq     SETDEFEXT       ; Again, no extension provided. Set default.
            cmp     #'.'            ; Extension separator?
            beq     COPYEXT         ; Yes, copy extension

NAMPATERN:  dec     ISPATTERN       ; Flag it is a pattern
            cmp     #'?'            ; If any valid pattern char, store and continue
            beq     LOOP            ;
            cmp     #'-'            ;
            beq     LOOP            ;
            cmp     #'*'            ;
            beq     LOOP            ;
ERR12:      jsr     ERROR12         ; Missing or illegal file name
            ; Not reached

COPYEXT:    sta     SFILENAME,x     ; Store separator
            inx                     ; Advance one pos in file name
            jsr     GETNEXTCH1      ; Advance one pos in commandline and get char
            jsr     ISALPHANUM      ; Check if alphanumeric
            bcs     EXTPATERN       ; No, then may be a pattern
STOREXT:    sta     SFILENAME,x     ; Yes, store it
            jsr     GETNEXTNB1      ; Get next non-blank
            cmp     COLON           ; Drive separator?
            bne     RETURN          ; No, update command line index and return
            iny                     ; Yes, andvance one pos in command line
            jsr     GETDRIVEOPND    ; Get drive from cmdline and check that it is opened
RETURN:     sty     CMDLIDX         ; Update command line index
            rts

SETDEFEXT:  lda     #'.'            ; Extension separatod
            sta     SFILENAME,x     ;
            inx                     ;
            lda     DEFAULTEXT      ; Get default extension
            dey                     ; Go back one pos in cmdline
            jmp     STOREXT         ; And go store the extension and continue

EXTPATERN:  dec     ISPATTERN       ; Flag that file name is a pattern
            cmp     #'?'            ; If not a valid wildcard for extension..
            beq     STOREXT         ;
            cmp     #'*'            ;
            bne     ERR12           ; ... invalid file name error
            lda     #'?'            ; Set wildcard
            jmp     STOREXT         ;
.endproc

; Check if directory entry's file name is a match for the file pattern
;
; Returns Cy clear if match, Cy set otherwise
;
.proc ISMATCH
            ldx     #$00            ; Init index to current pattern char
            ldy     DIRPOINT        ; Get pointer to current directory entry
LOOP:       lda     SFILENAME,x     ; Get char from pattern
            cmp     #'.'            ; Is it the extension separator?
            beq     CHKEXT          ; Yes, check extension
            cmp     #'*'            ; Is it a wildcard (anything but .)? 
            beq     CHKATRX         ; Yes, check asterix wildcard
            cmp     #'-'            ; Is it yhe prefix wildcard?
            beq     CHKPRFX         ; Yes, check it
            cmp     #'?'            ; Is it the one char except . wildcard?
            bne     CHKCHAR         ; No, check char against dir entry
            lda     DIRBUF,y        ; Check if  entry char is a dot
            cmp     #'.'            ;
            beq     NOMATCH         ; It is, no match
NXTCHR:     inx                     ; It is not, continue comparing
            iny                     ;
            jmp     LOOP            ;

CHKATRX1:   iny
CHKATRX:    lda     DIRBUF,y        ; Get entry's file name char 
            cmp     #'.'            ; While not the extension separator
            bne     CHKATRX1        ; Get next entry's file name char
            inx                     ; Increment index to source file pattern
            jmp     LOOP            ; And continue with the next char in pattern

CHKPRFX1:   iny
CHKPRFX:    lda     DIRBUF,y        ; Get entry's file name char 
            cmp     #'.'            ; Is itthe extension separator?
            beq     NOMATCH         ; Yes, then no match
            cmp     ULINE           ; Is it an underline?
            bne     CHKPRFX1        ; No, gt next entry's file name char
            jmp     NXTCHR          ; Yes, continue with the next char in pattern

CHKCHAR:    cmp     DIRBUF,y        ; Compare with entry's file name char
            beq     NXTCHR          ; Yes, continue with the next char in pattern
NOMATCH:    sec                     ; No, no match
            rts

CHKEXT:     cmp     DIRBUF,y        ; Compare to entry's file name
            bne     NOMATCH         ; If different, no match
            inx                     ; Increment indexes
            iny                     ;
            lda     SFILENAME,x     ; Get char from pattern
            cmp     #'?'            ; Is it a wildcard?
            beq     MATCH           ; Yes, we're done, match
            cmp     DIRBUF,y        ; Is it the same as in dir entry?
            bne     NOMATCH         ; No, no match
MATCH:      clc                     ; Yes, match
            rts
.endproc

; Set universal file pattern ("*.?") as source file name
; Also sets the default drive as the current drive 
;
.proc SETUPATTRN
            lda     #'*'
            sta     SFILENAME
            lda     #'.'
            sta     SFILENAME+1
            lda     #'?'
            sta     SFILENAME+2
            lda     DEFDRV
            sta     CURRDRV
            rts
.endproc

; Copy file name of origin to new directory entry and to output buffer
;
.proc COPYFNAM
            ldx     DIRPOINT        ; Get pointer to current directory entry
            stx     CURDIREP        ; And save it
            ldy     #$00
LOOP:       lda     DIRBUF,x        ; Copy file name to output buffer
            sta     DIRENT+DIRE::FNAM,y
            sta     (OUTBUFP),y     ;
            inx                     ;
            iny                     ;
            cmp     #'.'            ; Extension separator?
            bne     LOOP            ; No, repeat
            lda     DIRBUF,x        ; Yes, copy extension
            sta     DIRENT+DIRE::FNAM,y
            sta     (OUTBUFP),y     ;
            iny                     ;
            rts
.endproc

ISPATTERN:  .byte   0               ; Flag. If non-zero, file name is a pattern

PROG_SIZE = * - START

            .bss

.if CODOS2_VER <> 14
DATETERM:   .res    1               ; Date terminator in source file
.endif
DSTCHANN:   .res    1               ; Channel assigned to det file
CURDIREP:   .res    1               ; Pointer to current directory entry
CURSECTN:   .res    1               ; Sector of current directory entry
SRCDRIVE:   .res    1               ; Source drive number
DSTDRIVE:   .res    1               ; Destination drive number
CURINDEX:   .res    1               ; Current index to command line pos
NUMFILES:   .res    1               ; Number of files on source disk
UNUSED:     .res    1
SFILENAME:  .res    FNAMLEN         ; Source file name or pattern

            .end
