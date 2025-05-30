; da65 V2.18 - Ubuntu 2.19-1
; Created:    2025-05-30 11:57:40
; Input file: kill.bin
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
            lda     #$00
            sta     HSRCW
            cld                     ; Clear the message printed flag
            sta     MSGDONE         ;
            ldy     CMDLIDX         ; Get command line index
            jsr     GETNEXTNB       ; Get next non-blank
            bne     GPATTERN        ; There is a pattern
            jsr     DEFPATTERN      ; No args, use default pattern "*.?"
            jmp     DELFILES        ; Start deleting
.endproc

.proc GPATTERN
            jsr     GETPATTERN      ; Get search pattern and disk from command line
            ; Fall through
.endproc

.proc DELFILES
            jsr     SETOUTBCH       ; Set output buffer to output line buffer and sets
                                    ; output channel to console if not set
            ldx     CURRDRV         ; Get current drive
            jsr     DRVVALIDO       ; Ensure valid and open
            jsr     SETBATP         ; Set BATP to the current drive's BAT
            ldy     #BAT::NENT      ; Get number of files on disk
            lda     (BATP),y        ;
            sta     NUMFILES        ; Save it
            bne     DIRLIST         ; If at least one, continue
NOMORE:     ldy     CMDLIDX
            rts

DIRLIST:    lda     #$01            ; Set pointer to first directory entry
            sta     DIRPOINT        ;
            sta     SECTNUM         ; Get first sector of the directory table
            jsr     RDSECTNTR12     ;
CHKENTRY:   ldx     DIRPOINT        ; Get pointer to directory entry
            lda     DIRBUF,x        ; Get first character of entry's file name
            beq     NEXT            ; If it is null, entry is deleted
            jsr     ISMATCH         ; Check if entry's name matches pattern
            bcs     NOMATCH         ; No, go check next entry
            jsr     DELONEF         ; Yes, delete file
NOMATCH:    dec     NUMFILES        ; Decrement number of files
            beq     NOMORE          ; Jump if no more files
NEXT:       jsr     NXTDIRENT       ; Point to next directory entry and loads sector
                                    ; into dir buffer if necessary. Updates DIRPOINT
                                    ; and SECTNUM.
            jmp     CHKENTRY        ; Check next entry
.endproc

; Delete current entry
;
.proc DELONEF
            lda     CURRDRV         ; Get and save current drive
            sta     DRIVE           ;
            lda     DIRPOINT        ; Get and save offset to directory entry
            sta     DPOINT          ;
            lda     SECTNUM         ; get and save current directory sector
            sta     DSECTOR         ;
            bit     MSGDONE         ; Is the info message already printed?
            bmi     SKIPMSG         ; Yes, SKIPMSG it
            sec                     ; No, set the flag
            ror     MSGDONE         ;
            jsr     OUTSTR          ; And print it
            .byte   $0D, "CR OR Y WILL DELETE, N WILL KEEP FILE:", $0D, $00
SKIPMSG:    jsr     GETFNAM         ; Copies file name to DIRENT and output buffer
            jsr     POUTBUFF02      ; Outputs file name to console
            lda     #':'            ; Outputs ':' to console 
            ldx     #$02            ;
            jsr     OUTCHAR         ;
            lda     DRIVE           ; Get drive
            clc                     ; Convert to ascii
            adc     #'0'            ;
            ldx     #$02            ; Output to console
            jsr     OUTCHAR         ;
            jsr     OUTSTR          ;
            .byte   " ?= ", $00     ;
            ldx     #$01            ; Get entire line from console
            jsr     GETLINE         ;
            jsr     GETNEXTNB       ; Get next non-blank
            beq     DOIT            ; If blank, assume 'Y'
            cmp     #'Y'            ; Is it a "Yes"
            bne     SKIPDEL         ; Anything different, don't delete
DOIT:       ldx     DRIVE           ; Set drive
            stx     CURRDRV         ;
            jsr     FDELETE         ; And delete current DIRENT
SKIPDEL:    ldx     DRIVE           ; Get drive and set as current
            stx     CURRDRV         ;
            lda     DPOINT          ; Recover pointer to dir
            sta     DIRPOINT        ;
            lda     DSECTOR         ; Recover directory sector
            sta     SECTNUM         ;
            jsr     SETBATP         ; Set the BATP to the current drive's BAT
            jsr     RDSECTNTR12     ; And read directory sector
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

; Copy entry's file name into current DIRENT and into the output buffer
;
; On extit, Y points to first pos after the file name in output buffer
;
.proc GETFNAM
            ldx     DIRPOINT        ; Get pointer to file entry
            stx     DPOINT          ; And store it
            ldy     #$00            ; Copy entry's file name into DIRENT
LOOP:       lda     DIRBUF,x        ;
            sta     DIRENT+DIRE::FNAM,y
            sta     (OUTBUFP),y     ;
            inx                     ;
            iny                     ;
            cmp     #'.'            ; Last was xtension separator?
            bne     LOOP            ; No, repeat
            lda     DIRBUF,x        ; Yes, so this is the extension. Store it and return
            sta     DIRENT+DIRE::FNAM,y
            sta     (OUTBUFP),y     ;
            iny                     ;
            rts
.endproc

UNUSED:     .byte   $00             ; Unused variable

PROG_SIZE = * - START

            ; Uninitialized data

            .bss

            .res    1
DPOINT:     .res    1               ; Directory offset into directory sector
DSECTOR:    .res    1               ; Sector of current directory entry
DRIVE:      .res    1               ; Current drive
            .res    1
            .res    1
NUMFILES:   .res    1               ; Number of files on disk
MSGDONE:    .res    1               ; Flag. Bit 7 = 1 if message already printed
FNAME:      .res   14               ; Current entry's file name

            .end
