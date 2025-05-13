; da65 V2.19 - Git cf0688fc5
; Created:    2025-05-05 12:21:23
; Input file: kk2.bin
; Page:       1


            .setcpu "6502"

            .include "symbols.inc"

            .segment "overlays"

            .byte   $02             ; Overlay number

; FILL Command
;
; DESCRIPTION:  Fill a block of memory with a constant. 
;                                   {"character" ...}
; SYNTAX:       FILL <from> <to> [=]{<value>        }
;                                   {'character' ...}
; ARGUMENTS:    <from> = address at which to deposit the first value
;               <to> = ending address for fill operation
;               <value> = numeric value to be deposited
;               <character> = an ASCII character to be deposited
;
FILL:       jsr     GADDRBNKMB      ; Get Address and bank and store into MEMBUF
            jsr     GETTOP          ; Get <to> from command line and store to MEMCOUNT
            cld                     ; Convert the <to> address into a byte count
            sec                     ; from MEMBUFF and store it back into MEMCOUNT
            lda     MEMCOUNT        ;
            sbc     MEMBUFF         ;
            sta     MEMCOUNT        ;
            lda     MEMCOUNT+1      ;
            sbc     MEMBUFF+1       ;
            sta     MEMCOUNT+1      ;
            bcs     @CONT           ; <to> is equal or greater than <from>
            jsr     ERROR14         ; <from> address missing or illegal
            ; Not reached

@CONT:      jsr     GETNEXTNB       ; Get next non-blank
            cmp     #'='            ; Is it the optional "="?
            bne     @CHKCHR         ; No, go check if character or value
            jsr     GETNEXTNB1      ; Advance and get next non blank (ignore)
@CHKCHR:    sty     CMDLIDX         ; Update command line index
            cmp     #$27            ; Is it a single quote?
            beq     @ISCHAR         ; Yes, it is a char string
            cmp     #'"'            ; Is it a double quote?
            bne     @ISVAL          ; No, it is a value
@ISCHAR:    sta     QUOTE           ; Save delimiter
            jsr     GETNEXTCH1      ; Advance and get next char
            cmp     #$00            ; No more?
            bne     @GETCH          ; There are more, continue
@ILLEG:     jsr     ERROR24         ; <value> missing or illegal
@GETCH:     tax                     ; Save char in X
            jsr     GETNEXTCH1      ; Advance and get next char
            cmp     QUOTE           ; Is it the string delimiter?
            bne     @ILLEG          ; No, illegal value
@STOR:      txa                     ; Recover char
            jsr     SETBYTE
            lda     MEMCOUNT
            sec
            sbc     #$01
            sta     MEMCOUNT
            lda     MEMCOUNT+1
            sbc     #$00
            sta     MEMCOUNT+1
            bcs     @STOR
            rts

@ISVAL:     jsr     LFBCC
            bcc     @ILLEG
            tax
            bcs     @STOR


;   Store byte into destination
;
SETBYTE:    sta     SAVEAX          ; Save byte
            lda     NEWBNK          ; Get <from> bank
            bne     @CONT           ; It is not bank 0
            jsr     CHKVALID        ; Check if <from> is valid. Does not return if not.
            lda     NEWBNK          ; Get <from> bank again (which is 0)
@CONT:      eor     DEFBNKCFG       ; Switch to NEWBNK
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
            beq     @CHKOK          ; Same value, check ok
            jsr     ERROR23         ; Memory verify failure during SET or FILL
@CHKOK:     inc     MEMBUFF         ; Advance to next memory position
            bne     @RETURN         ;
            inc     MEMBUFF+1       ;
@RETURN:    ldx     SAVEAX          ; Recover X
            rts


CHKVALID:   lda     MEMBUFF+1
            cmp     #>SYSRAM        ;
            bcc     @CONT           ; No, continue
            jsr     CHKUNPROT       ; Yes, check if protected (does not return if not)
@CONT:      cmp     #$02            ; Destination under $0200? FIXME: Address map dependent!
            bcs     @RETURN         ; No, just return
            cmp     #$01            ; Destination in stack?
            bcc     @PAGE0          ; No, then in page 0
            txa                     ; Save X
            tsx                     ; Get stack pointer
            cpx     MEMBUFF         ; Is destination inside current stack?
            bcs     @XRET           ; No, recover X and return
            jsr     CHKUNPROT       ; Yes, check if unprotected and fail if not
@XRET:      tax                     ; Recover X
            jmp     @RETURN         ; and return

@PAGE0:     lda     MEMBUFF         ; Check if destination is
            cmp     #P0SCRATCH      ; in the CODOS reserved page 0 area
            bcc     @RETURN         ; No, return
            jsr     CHKUNPROT       ; Yes, check if unprotected and fail if not
@RETURN:    rts

; Check if IGNORWRP is set and fail if not
;
CHKUNPROT:  bit     IGNORWRP        ; Check ignore write protection flag
            bmi     @RETURN         ; If set, just return
            jsr     ERROR17         ; Reserved or protected memory violation
@RETURN:    rts

            ; This block is just junk that was in the buffer when
            ; writing it to disk. I leave it to facilitate checksum
            ; comparisons with the original
            ;
            .byte   $03, $20, $F3, $E9, $60, $EA, $A9, $80
            .byte   $8D, $78, $E7, $8D, $77, $E7, $60, $A9
            .byte   $00, $8D, $78, $E7, $60, $CD, $C8, $A9
            .byte   $3A, $91, $CD, $C8, $A5, $DD, $D8, $18 
            .byte   $69, $30, $91, $CD, $4C, $A7, $FE, $60 
            .byte   $20, $68, $D9, $A2, $01, $4C, $94, $F5 
            .byte   $D7, $FE, $00, $FE, $60, $00, $00, $1E
