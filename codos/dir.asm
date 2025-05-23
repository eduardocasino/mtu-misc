; da65 V2.18 - Ubuntu 2.19-1
; Created:    2025-05-19 09:34:40
; Input file: dir.bin
; Page:       1


            .setcpu "6502"

            .include "symbols.inc"

            .include "codos.inc"

DIRLINELEN  = $27                   ; Length of dir listing lines
DRIVEPOS    = $0F
FLAGSPOS    = $12
DATEPOS     = $1D
FSIZEPOS    = $20

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
            lda     #$00            ; Unprotects SYSRAM
            sta     HSRCW           ;
            cld                     ;
            ldx     #$02            ; Output CR to console
            jsr     OUTCR           ;
            ldy     CMDLIDX         ; Get command line index
            jsr     GETNEXTNB       ; Get next non-blank from command line
            bne     GPATTERN        ; If any, go get pattern
            jsr     DEFPATTERN      ; No args, use default pattern "*.?"
            jmp     DIRSEARCH       ; And start searching
.endproc

.proc GPATTERN
            jsr     GETPATTERN      ; Get search pattern and disk from command line
            ; Fall through
.endproc

.proc DIRSEARCH
            jsr     SETOUTBCH       ; Set output buffer to output line buffer and sets
                                    ; output channel to console if not set
            ldx     CURRDRV         ; Get current drive
.if ::CODOS2_VER <> 14
            stx     DRIVE           ; And store it
.endif
            jsr     DRVVALIDO       ; Ensure valid and open
            jsr     SETBATP         ; Set BATP to the current drive's BAT
            ldy     #BAT::NENT      ; Get number of files on disk
            lda     (BATP),y        ;
            sta     NFILES          ; Save it
            bne     DIRLIST         ; If at least one, continue
NOMORE:     ldy     CMDLIDX         ; None, recover command line index
            jsr     GETNEXTNB       ; Get next non-blank from command line
            bne     GPATTERN        ; If any, go get next pattern
            rts                     ; If none, we are done

DIRLIST:    lda     #$01            ; Set pointer to first directory entry
            sta     DIRPOINT        ;
            sta     SECTNUM         ; Get first sector of the directory table
            jsr     RDSECTNTR12     ;
CHKENTRY:   ldx     DIRPOINT        ; Get pointer to directory entry
            lda     DIRBUF,x        ; Get first character of entry's file name
            beq     NEXT            ; If it is null, entry is deleted
            jsr     ISMATCH         ; Check if entry's name matches pattern
            bcs     NOMATCH         ; No, go check next entry
            jsr     PRINTFILE       ; Yes, print file info
NOMATCH:    dec     NFILES          ; Decrement number of files
            beq     NOMORE          ; Jump if no more files
NEXT:       jsr     NXTDIRENT       ; Point to next directory entry and loads sector
                                    ; into dir buffer if necessary. Updates DIRPOINT
                                    ; and SECTNUM.
            jmp     CHKENTRY        ; Check next entry
.endproc

; Prints file info for current entry
;
.proc PRINTFILE
            lda     #' '            ; Clear output buffer with blanks
            ldy     #DIRLINELEN     ; up to dir listing line length
CLRCHR:     sta     (OUTBUFP),y     ;
            dey                     ;
            bpl     CLRCHR          ; Repeat until completed
            ldx     DIRPOINT        ; Get pointer to directory entry
                                    ; Get pointer to first block
            lda     DIRBUF+DIRE::BATP-1, x
                                    ; And store into current FINFO
            sta     CURFINFO+FINFO::BATPT
            jsr     INITFILE        ; Init file size and file position
            lda     CURRDRV         ; Get current drive
                                    ; And store into current FINFO
            sta     CURFINFO+FINFO::DRIVE           
            lda     #$94            ; Set transfer buffer to $E500 (Directory buffer)
            sta     CURFINFO+FINFO::DMABF
            jsr     GETFPSECT       ; Get sector, track and head of file position
            jsr     READSECT        ; And read sector
            ldx     #$01            ; Init file entry index in file header
            ldy     #$00            ; Init output buffer index
GETCHR:     lda     DIRBUF,x        ; Copy file name to output buffer
            sta     (OUTBUFP),y     ;
            inx                     ; Next file name char
            cpx     #$11            ; Have we reached the end of file name?
            bcs     CPYEXT          ; Yes, copy that last char and continue processing
            iny                     ; No, advance to next pos in output buffer
            cmp     #'.'            ; Check if extension separator
            bne     GETCHR          ; No, get next file name char
CPYEXT:     lda     DIRBUF,x        ; Yes, copy the extension (or last char)
            sta     (OUTBUFP),y     ;
            ldy     #DRIVEPOS       ; Advance to position of drive separator
            lda     #':'            ; Copy the drive separator
            sta     (OUTBUFP),y     ;
            lda     CURRDRV         ; Get current drive
            iny                     ; Advance one pos
            jsr     NIBBLE          ; And output the drive number as char
                                    ; Get file flags
            lda     DIRBUF+FHDR::FLAG
            and     #FLLOCKED       ; Is file locked?
            beq     DASH            ; No, go print a dash
            lda     #'L'            ; Yes, print an 'L'
            bne     CPYFLAG         ; Always jumps
DASH:       lda     #'-'            ; Normal file, print '-' as flags
CPYFLAG:    ldy     #FLAGSPOS       ; Advance to position of flags
            sta     (OUTBUFP),y     ; And copy flag
            ldx     #DATELEN-1      ; Copy date to output buffer
            ldy     #DATEPOS        ; Advance to position of date
                                    ; Copy date char
CPYDAT:     lda     DIRBUF+FHDR::DATE,x
            sta     (OUTBUFP),y     ;
            dey                     ;
            dex                     ;
            bpl     CPYDAT          ; Repeat until completed
                                    ; Get file length
            lda     DIRBUF+FHDR::FLEN
            sec                     ; Clear borrow for substraction
            sbc     #FHDRLEN        ; Substract file header length
            sta     DIRBUF+FHDR::FLEN
            lda     DIRBUF+FHDR::FLEN+1
            sbc     #$00            ;
            sta     DIRBUF+FHDR::FLEN+1
            bcs     LB4DD           ;
            dec     DIRBUF+FHDR::FLEN+2
LB4DD:      ldy     #FSIZEPOS       ; Advance to position of file size
            ldx     #FHDR::FLEN+2   ; Points to MSB of file size in header
            jsr     COPYFSIZEH      ; Copy two MSBs of file size to output buffer
            ldx     #FHDR::FLEN     ; Points to LSB of file size in header
            jsr     COPYFSIZEL      ; Copy LSB of file to output buffer
            ldy     #DIRLINELEN     ; Print output buffer to console
            jsr     POUTBUFFCR02    ;
.if ::CODOS2_VER <> 14
            lda     DRIVE           ; Restore drive number on entry
            sta     CURRDRV         ;
.endif
            jsr     RDSECTNTR12     ; Reads sector of current directory entry into DIRBUF
            rts                     ; and returns
.endproc

; Copy two MSBs of file size to output buffer
;
; X contains the offset of file size's MSB into file header
; Y contains current output buffer position
;
.proc COPYFSIZEH
            lda     #'$'            ; Copy the hex indicator to output buffer
            sta     (OUTBUFP),y     ;
            iny                     ; Advance one pos
            lda     DIRBUF,x        ; Get file size byte
            jsr     HEXBYTE         ; And copy to the output buffer as ascii hex
            dex                     ; Next byte
            ; Fall through
.endproc

; Copy LSB of file size to output buffer
;
; X contains the offset of file size's LSB into file header
; Y contains current output buffer position
;
.proc COPYFSIZEL
            lda     DIRBUF,x        ; Get byte
            jsr     HEXBYTE         ; And copy to the output buffer as ascii hex
            rts
.endproc

; Get file pattern and drive from command line
;
.proc GETPATTERN
            ldx     DEFDRV          ; Set default drive as current drive
            stx     CURRDRV         ;
            ldx     #$00            ; Init unused variable
            stx     UNUSED          ;
            jsr     ISALPHA         ; Is it an alphabetic character
            bcs     WCARD           ; No, check if wildcard
STORE:      cpx     #$0C            ; Reached pos 12 and no extension separator found?
            bcs     ERR12           ; Yes, bad filename
            sta     FNAME,x         ; Store char 
            inx                     ; Advance pos in file name
            jsr     GETNEXTCH1      ; Advance one pos in command line and get char
            beq     ADDEXT          ; No more? Add default extension to file name
            cmp     COLON           ; Is it a the drive separator separator?
            beq     ADDEXT          ; Yes, add default extension to file name
            jsr     VALFNCHR        ; Is a valid file name character?
            bcc     STORE           ; Yes, store it
            cmp     #' '            ; Is it an space?
            beq     ADDEXT          ; Yes, add default extension to file name
            cmp     #'.'            ; Is it the extension separator?
            beq     GETEXT          ; Yes, go get the extension
WCARD:      dec     UNUSED          ; Decrement unused variable
            cmp     #'?'            ; Check if a valid wildcard and store it if so
            beq     STORE           ;
            cmp     #'-'            ;
            beq     STORE           ;
            cmp     #'*'            ;
            beq     STORE           ;
ERR12:      jsr     ERROR12         ; Missing or illegal file name
            ; Not reached

GETEXT:     sta     FNAME,x         ; Store '.'
            inx                     ; Advance one pos in file name
            jsr     GETNEXTCH1      ; Advance one pos in command line and get char
            jsr     ISALPHANUM      ; Check that it is a valid extension char
            bcs     EXTWCARD        ; No, go see if it is a wildcard

GETDRV:     sta     FNAME,x         ; Store char into file name
            jsr     GETNEXTNB1      ; Advance and get next non-blank from command line
            cmp     COLON           ; Is it the driveseparator?
            bne     RETURN          ; No, skip drive assignment
            iny                     ; Advance one pos to get the drive
            jsr     GETDRIVEOPND    ; Get drive from command line and ensure it's opened
RETURN:     sty     CMDLIDX         ; Update command line index and return
            rts                     ;

ADDEXT:     lda     #'.'            ; Add default extension to file name
            sta     FNAME,x         ;
            inx                     ;
            lda     DEFAULTEXT      ;
            dey                     ; Go back one pos in command line
            jmp     GETDRV          ; and continue processing

EXTWCARD:   dec     UNUSED          ; Decrement unused variable
            cmp     #'?'            ; Is it "match one char"?
            beq     GETDRV          ; Yes, continue to get drive
            cmp     #'*'            ; Is it "match any char but '.'"?
            bne     ERR12           ; No, invalid pattern
            lda     #'?'            ; Yes, use '?' instead and
            jmp     GETDRV          ; contiue to get drive
.endproc

; Compares pattern with file entry's name
;
; Returns Cy clear if matches, Cy set if not
;
.proc ISMATCH
            ldx     #$00            ; Init index to file name char position
            ldy     DIRPOINT        ; Get pointer to file entry
CMPCHR:     lda     FNAME,x         ; Get char of file name
            cmp     #'.'            ; Is it the extension separator?
            beq     CHKEXT          ; Yes, go check the extension
            cmp     #'*'            ; Is it "match any char but '.'"?
            beq     MATCHATRX       ; Yes, go see how many entry's name chars match
            cmp     #'-'            ; Is it "match string terminated by underline"?
            beq     MATCHDASH       ; Yes, go see how many entry's name chars match
            cmp     #'?'            ; Is it "match any single char"?
            bne     MATCHCHAR       ; No, then do a direct comparison
            lda     DIRBUF,y        ; Get char from entry's name
            cmp     #'.'            ; Is it the extension separator?
            beq     NOMATCH         ; Yes, then it is not a match
CMPNXT:     inx                     ; Compare next char
            iny                     ;
            jmp     CMPCHR          ;

MTCHATRX1:  iny                     ; Advance to next entry's name char

            ; Matches '*' (anything but '.')

MATCHATRX:  lda     DIRBUF,y        ; Get char from entry's name
            cmp     #'.'            ; Is it the extension separator?
            bne     MTCHATRX1       ; No, then it is match, go get next char 
            inx                     ; No more matches, go compare next entry's char
            jmp     CMPCHR          ;

MTCHDASH1:  iny                     ; Advance to next entry's name char

            ; Matches '-' (string terminated by and including '_')

MATCHDASH:  lda     DIRBUF,y        ; Get char from entry's name
            cmp     #'.'            ; Is it the extension separator?
            beq     NOMATCH         ; Yes, then it is not a match
            cmp     ULINE           ; Is it the underline terminator?
            bne     MTCHDASH1       ; No, then it is match, go get next char
            jmp     CMPNXT          ; Yes, it is a match, continue matching

            ; Direct character match

MATCHCHAR:  cmp     DIRBUF,y        ; Compare with char from entry's name
            beq     CMPNXT          ; Yes, it is a match, continue matching
NOMATCH:    sec                     ; Not a match, return with Cy set
            rts                     ;

CHKEXT:     cmp     DIRBUF,y        ; Compare with char from entry's name
            bne     NOMATCH         ; If not a '.', it is not a match
            inx                     ; Advance to next char (extension name)
            iny                     ;
            lda     FNAME,x         ; Get extension from pattern
            cmp     #'?'            ; Is it the wildcard?
            beq     MATCH           ; Yes, then it is a match, return with Cy clear
            cmp     DIRBUF,y        ; Is it the same as the entry's
            bne     NOMATCH         ; No, then it is not a match
MATCH:      clc                     ; Clear carry (is a match) and return
            rts                     ;
.endproc

; Sets the default pattern "*.?" (match anything) for the file search and
; sets default drive as current drive
;
.proc DEFPATTERN
            lda     #'*'            ; Store pattern into file name
            sta     FNAME           ;
            lda     #'.'            ;
            sta     FNAME+1         ;
            lda     #'?'            ;
            sta     FNAME+2         ;
            lda     DEFDRV          ;
            sta     CURRDRV         ;
            rts                     ;
.endproc

.if CODOS2_VER <> 14
DRIVE:      .byte   $00             ; Drive number on entry
.endif
UNUSED:     .byte   $00             ; Unused variable

PROG_SIZE = * - START


            ; Uninitialized data

            .bss

FNAME:      .res   14               ; FNAMLEN

            .end