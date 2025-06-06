; da65 V2.19 - Git cf0688fc5
; Created:    2025-05-05 12:22:14
; Input file: kk6.bin
; Page:       1


            .setcpu "6502"

            .include "symbols.inc"
            .include "codos.inc"

            .segment "overlays"

            .byte   $06             ; Overlay number

; LOCK Command
;
; DESCRIPTION:  Enable the software write-protect for a file
;
; SYNTAX:       LOCK <file>[:<drive>] ...
;
; ARGUMENTS:    <file> = file name
;               <drive> = disk drive. Defaults to current default drive, usually 0
;
.proc LOCK
NEXT:       jsr     ISNUM           ; Check if first char of args is numeric
            bcc     ISCHN           ; Yes, it is a channel
            jsr     GETFILNDRV      ; Get file and drive from command line
            jsr     FOPEN0          ; Assigns channel 0 to file
DOLOCK:     jsr     LOCKCH          ; Locks file assigned to channel
            ldy     CMDLIDX         ; Get command line index
            jsr     GETNEXTNB       ; Get next non blank
            bne     NEXT            ; Yes, go lock next
            jmp     FREECH0         ; Free channel 0 and return

ISCHN:      jsr     GETCHANN        ; Get channel from command line
            jmp     DOLOCK          ; And go lock it
.endproc

; UNLOCK Command
;
; DESCRIPTION:  Disable the software write-protect for a file
;
; SYNTAX:       UNLOCK <file>[:<drive>] ...
;
; ARGUMENTS:    <file> = file name
;               <drive> = disk drive. Defaults to current default drive, usually 0
;
.proc UNLOCK
NEXT:       jsr     ISNUM           ; Check if first char of args is numeric
            bcc     ISCHN           ; Yes, it is a channel
            jsr     GETFILNDRV      ; Get file and drive from command line
            jsr     FOPEN0          ; Assigns channel 0 to file
DOUNLCK:    jsr     UNLCKCH         ; Unocks file assigned to channel
            ldy     CMDLIDX         ; Get command line index
            jsr     GETNEXTNB       ; Get next non blank
            bne     NEXT            ; Yes, go unlock next
            jmp     FREECH0         ; Free channel 0 and return

ISCHN:      jsr     GETCHANN        ; Get channel from command line
            jmp     DOUNLCK         ; And go unlock it
.endproc

; Locks file assigned to channel in X
;
.proc LOCKCH
            jsr     ASSIGNED        ; Get assigned device/file to channel X
            bpl     ISFILE          ; It is a file, go to
            rts                     ; It is a device, return

ISFILE:     jsr     GETFINFO        ; Gets FINFO for current file (DEVICE),
                                    ; copies it into CURFINFO in page zero
                                    ; and sets CURRDRV
            jsr     ZEROFILEP       ; Zeroes file pointer
            dma     A, DIRBUF       ; Set transfer buffer to DIRBUF (Directory buffer)
            sta     CURFINFO+FINFO::DMABF
            jsr     GETFPSECT       ; Get sector of current file pointer
            jsr     READSECT        ; Read sector
            ldx     #FHDR::FLAG     ; Get file flag from buffer
            lda     DIRBUF,x        ;
            ora     #FLLOCKED       ; Set locked bit 
            sta     DIRBUF,x        ;
            lda     CSECT           ; Get sector of current file pointer
                                    ; Get drive
            ldx     CURFINFO+FINFO::DRIVE
            jsr     WRITSECT        ; Write sector back
            jsr     GETFINFO        ; Gets FINFO for file, copies it into CURFINFO
                                    ; Set locked bit in flags
            lda     CURFINFO+FINFO::FLAGS
            ora     #FLLOCKED       ;
            sta     CURFINFO+FINFO::FLAGS
            jsr     UPDCFINFO       ; Updates file's FINFO structure
            rts
.endproc

; Locks file assigned to channel in X
;
.proc UNLCKCH
            jsr     ASSIGNED        ; Get assigned device/file to channel X
            bpl     ISFILE          ; It is a file, go to
            rts

ISFILE:     jsr     GETFINFO        ; Gets FINFO for current file (DEVICE),
                                    ; copies it into CURFINFO in page zero
                                    ; and sets CURRDRV
            jsr     ZEROFILEP       ; Zeroes file pointer
            dma     A, DIRBUF       ; Set transfer buffer to DIRBUF (Directory buffer)
            sta     CURFINFO+FINFO::DMABF
            jsr     GETFPSECT       ; Get sector of current file pointer
            jsr     READSECT        ; Read sector
            ldx     #FHDR::FLAG     ; Get file flag from buffer
            lda     DIRBUF,x        ;
            and     #<(~FLLOCKED)   ; Clear locked bit
            sta     DIRBUF,x        ;
            lda     CSECT           ; Get sector of current file pointer
                                    ; Get drive
            ldx     CURFINFO+FINFO::DRIVE
            jsr     WRITSECT        ; Write sector back
            jsr     GETFINFO        ; Gets FINFO for file, copies it into CURFINFO
                                    ; Set locked bit in flags
            lda     CURFINFO+FINFO::FLAGS
            and     #<(~FLLOCKED)   ; Clear locked bit
            sta     CURFINFO+FINFO::FLAGS
            jsr     UPDCFINFO       ; Updates file's FINFO structure
            rts
.endproc

; SVC Command
;
; DESCRIPTION:  Enable or disable SVCs (upon subsequent entry to user program)
;
; SYNTAX:       SVC [<off>]
;
; ARGUMENTS:    <off> = any non-blank argument. Defaults to no argument. 
;
.proc SVC
            beq     ENAB            ; If no arguments, enable
            lda     #$00            ; Disable SVC
            beq     UPDF            ; Go update flag (always jumps)
ENAB:       lda     #$80            ; Enable SVC
UPDF:       sta     DEFSVCFLAG      ; Update flag
            rts
.endproc

; MSG Command
;
; DESCRIPTION:  Print a message over a specified channel.
;
; SYNTAX:       Single line form: MSG <chan> <text> CR 
;               Multi-line form:  MSG <chan>^CR
;                                 <text> CR ... 
;                                 <text>^CR 
;
; ARGUMENTS:    <chan> = channel number from 0 to 9
;               <text> = any printable ASCII text except the "^" (caret) character
;               CR = ASCII Carriage Return control character (not the letters "CR")
;
.proc MSG
            ldy     CMDLIDX         ; Get command line index
            lda     #$00            ; Unprotect SYSRAM
            sta     HSRCW           ;
            jsr     GETCHANN        ; Get channel from command line
            stx     MSGOUTC         ; Save it
            jsr     GETNEXTNB       ; Get next non-blank from command line
            bne     CONT            ; Yes, go get message
OUTC:       jmp     OUTCR           ; No, just output CR and return

CONT:       cmp     CARET           ; Character is caret?
            beq     MLINE           ; Yes, process multi-line
LOOP:       jsr     OUTCHAR         ; No, output to channel
            jsr     GETNEXTCH1      ; Advance and get next char
            beq     OUTC            ; If none, output CR and return
            bne     LOOP            ; Always jump to output char
            ; Not reached
MLINE:      ldx     #$01            ; Get line from console
            jsr     GETLINE         ;
            ldx     MSGOUTC         ; Get output channel
            bcs     OUTC            ; If no input, output CR and return
            ldy     #$00            ; Init index
GETCR:      lda     (INPBUFP),y     ; Get char from input buffer at pos Y
            cmp     #$0D            ; Is it carriage return?
            beq     NEXT            ; Yes, ouput CR and get next line
            cmp     CARET           ; Is it a caret (^)?
            beq     OUTC            ; Yes, output CR and return
            jsr     OUTCHAR         ; No, output char
            iny                     ; Advance to next pos in input buffer
            bne     GETCR           ; Always jump to get next char
            ; Not reached
NEXT:       jsr     OUTCHAR         ; Output char
            jmp     MLINE           ; And get next line
.endproc

MSGOUTC:    .byte   $00             ; Channel for msg out command

.ifdef mtu
            ; This block is just junk that was in the buffer when
            ; writing it to disk. I leave it to facilitate checksum
            ; comparisons with the original
            ;
            .byte   $FE, $60, $00, $00, $1E
.endif
            .end
