; da65 V2.19 - Git cf0688fc5
; Created:    2025-04-30 10:05:12
; Input file: overlay01.bin
; Page:       1


            .setcpu "6502"

            .include "symbols.inc"

            .segment "overlays"

            .byte $01               ; Overlay number

; SET Command
;
; DESCRIPTION:  Set the value of memory locations
;                          {"character" ...}
; SYNTAX:       SET <from>[=]{<value>        }
;                          {'character' ...}
; ARGUMENTS:    <from> = address at which to deposit the first value
;               <value> = numeric value to be deposited
;               <character> = an ASCII character to be deposited
;
.proc SETCMD
            jsr     GADDRBNKMB      ; Get Address and bank and store into MEMBUF
            jsr     GETNEXTNB       ; Advance to next non-blank
            bne     CONT            ; Any? Ok, continue
            jsr     ERROR24         ; <value> missing or illegal
            ; Not reached
CONT:       cmp     #'='            ; Is it the optional '='
            bne     CHRDEL          ; No, then go check the character delimiter
            jsr     GETNEXTNB1      ; Yes, get the next non blank
CHRDEL:     sty     CMDLIDX         ; Save pos
            cmp     #'''            ; Is it a single quote?
            beq     ISCHR           ; Yes, continue
            cmp     #'"'            ; Is it a double quote?
            bne     ISVAL           ; No, it is a value
ISCHR:      sta     QUOTE           ; Save delimiter
            sty     SAVEY8          ; Save current position
GNEXT:      jsr     GETNEXTCH1      ; Get char from (INPBUFP),y+1
            sty     CMDLIDX         ; Update command line index with pos after char
            bne     GOTCH           ; Got a char and is not a delimiter
            cmp     SCOLON          ; Is it a semicolon?
            beq     GNEXT           ; Get next char
            jsr     ERROR26         ; Missing or illegal character string delimiter (' , ")
GOTCH:      cmp     QUOTE           ; Is it the char delimiter?
            bne     GNEXT           ; No, get next
            dey                     ; Go to previous pos
            cpy     SAVEY8          ; Is it the one of the first delimiter
            bne     CHVAL           ; No, so we got a character value
            jsr     ERROR26         ; Missing or illegal character string delimiter (' , ")
CHVAL:      ldy     SAVEY8          ; Return to position of first delimiter
STCHAR:     jsr     GETNEXTCH1      ; Get char from next pos
            cpy     CMDLIDX         ; Are we at the pos after the last char?
            beq     ADNEXT          ; Yes, go advance and get the next argument
            jsr     SETBYTE         ; No, store the character
            jmp     STCHAR          ; And go store next
            ; Not reached

ADNEXT:     iny                     ; Advance pos
NXTARG:     jsr     GETNEXTNB       ; Get next non-blank
            bne     CHRDEL          ; Go check if delimiter
            rts                     ; No more, return

ISVAL:      jsr     GETBYTE         ; Get byte from command line
            bcs     STOR            ; Good, go store it
            jsr     ERROR24         ; <value> missing or illegal
            ; Not reached

STOR:       jsr     SETBYTE         ; Store byte
            jmp     NXTARG          ; And go get next char
            ; Not reached
.endproc

;   Store byte into destination
;
.proc SETBYTE
            sta     SAVEAX          ; Save byte
            lda     NEWBNK          ; Get <from> bank
            bne     CONT            ; It is not bank 0
            jsr     CHKVALID        ; Check if <from> is valid. Does not return if not.
            lda     NEWBNK          ; Get <from> bank again (which is 0)
CONT:       eor     DEFBNKCFG       ; Switch to NEWBNK
            sta     BNKCTL          ;
            lda     SAVEAX          ; Recover byte
            stx     SAVEAX          ; Save X
            ldx     #$00            ; Store byte to destination
            sta     (MEMBUFF,x)     ;
            cmp     (MEMBUFF,x)     ; And verify there was no error
            php                     ; Save comparison status
            lda     DEFBNKCFG       ; Restore bank settings
            sta     BNKCTL          ;
            plp                     ; Recover comparison status
            beq     CHKOK           ; Same value, check ok
            jsr     ERROR23         ; Memory verify failure during SET or FILL
CHKOK:      inc     MEMBUFF         ; Advance to next memory position
            bne     RETURN          ;
            inc     MEMBUFF+1       ;
RETURN:     ldx     SAVEAX          ; Recover X
            rts
.endproc

; Check if destination is a valid address.
; Does not return if not.
;
.proc CHKVALID
            lda     MEMBUFF+1       ; Is destination in SYSRAM?
            cmp     #>SYSRAM        ;
            bcc     CONT            ; No, continue
            jsr     CHKUNPROT         ; Yes, check if protected (does not return if not)
CONT:       cmp     #$02            ; Destination under $0200? FIXME: Address map dependent!
            bcs     RETURN          ; No, just return
            cmp     #$01            ; Destination in stack?
            bcc     PAGE0           ; No, then in page 0
            txa                     ; Save X
            tsx                     ; Get stack pointer
            cpx     MEMBUFF         ; Is destination inside current stack?
            bcs     XRET            ; No, recover X and return
            jsr     CHKUNPROT       ; Yes, check if unprotected and fail if not
XRET:       tax                     ; Recover X
            jmp     RETURN          ; and return

PAGE0:      lda     MEMBUFF         ; Check if destination is
            cmp     #P0SCRATCH      ; in the CODOS reserved page 0 area
            bcc     RETURN          ; No, return
            jsr     CHKUNPROT       ; Yes, check if unprotected and fail if not
RETURN:     rts
.endproc

; Check if IGNORWRP is set and fail if not
;
.proc CHKUNPROT
            bit     IGNORWRP        ; Check ignore write protection flag
            bmi     RETURN          ; If set, just return
            jsr     ERROR17         ; Reserved or protected memory violation
RETURN:     rts
.endproc

; UNPROTECT Command
;
; DESCRIPTION:  Disable the hardware write-protect on the top 8K of RAM on the disk
;               controller board (addresses SYSRAM-SYSRAM+$1FFF in bank 0) and disable
;               the system reserved memory checking for SET and FILL commands
; SYNTAX:       UNPROTECT
; ARGUMENTS:    None.
; 
.proc UNPROTECT
            nop
            lda     #$80            ;
            sta     UNPROTFLG       ; Set unprotect flag
            sta     IGNORWRP        ; Set ignore write protection flag
            rts
.endproc

; PROTECT Command
;
; DESCRIPTION:  Enable the memory-protect hardware on the upper 8k block of memory on
;               the disk controller board (addresses SYSRAM-SYSRAM+$1FFF in bank 0) and
;               enable the reserved-memory checking for SET and FILL commands
; SYNTAX:       PROTECT
; ARGUMENTS:    None.
; 
.proc PROTECT
            lda     #$00            ; Clear unprotect flag
            sta     UNPROTFLG       ;
            rts
.endproc

            ; This block is just junk that was in the buffer when
            ; writing it to disk. I leave it to facilitate checksum
            ; comparisons with the original
            ;
            .byte   $CD, $C8, $A9
            .byte   $3A, $91, $CD, $C8, $A5, $DD, $D8, $18 
            .byte   $69, $30, $91, $CD, $4C, $A7, $FE, $60 
            .byte   $20, $68, $D9, $A2, $01, $4C, $94, $F5 
            .byte   $D7, $FE, $00, $FE, $60, $00, $00, $1E
