; da65 V2.19 - Git cf0688fc5
; Created:    2025-05-05 12:23:37
; Input file: kk15.bin
; Page:       1


            .setcpu "6502"

.ifdef mtu
            .include "monomeg.inc"
.endif
            .include "symbols.inc"
            .include "codos.inc"

            .segment "overlays"

            .byte   $0F             ; Overlay number

; COMPARE Command
;
; DESCRIPTION:  Determine if two blocks of memory are identical.
;
; SYNTAX:       OCOMPARE <from> <to> <dest>
;
; ARGUMENTS:    <from> = starting address for first block
;               <to> = ending address for first block
;               <dest> = starting address for second block
;
.proc COMPARE
            ldx     #_MEMBUFF       ; Redundant, GADDRBNKMB also sets it
            jsr     GADDRBNKMB      ; Get address and bank and store into MEMBUFF and NEWBNK
.ifdef ::mtu
            lda     NEWBNK          ; Get from bank
            sta     FROMBNK         ; Store it as from and dest bank
            sta     DESTBNK         ;
            eor     DEFBNKCFG       ; Default <from> memory config
            sta     FBNKCFG         ; Store as from and dest config
            sta     DBNKCFG         ;
.endif
            lda     MEMBUFF         ; Store <from> address into DESTBUFF
            sta     DESTBUFF        ;
            lda     MEMBUFF+1       ;
            sta     DESTBUFF+1      ;
            jsr     GETTOP          ; Get <to> from command line and store into MEMCOUNT
            ldx     #_TMPBUFP       ; Evaluate <dest> expression from command line and
            jsr     EVALEXP         ; store into TMPBUFP
            bcs     CHKADDR         ; If OK, continue
ERR27:      jsr     ERROR27         ; <destination> address missing or illegal
            ; Not reached

CHKADDR:    lda     MEMCOUNT        ; Check if <from> address is greater than <to>
            cmp     DESTBUFF        ;
            lda     MEMCOUNT+1      ;
            sbc     DESTBUFF+1      ;
            bcs     ADDROK          ; Not greater, that's OK
            jsr     ERROR16         ; <from> address greater than <to> address
            ; Not reached

ADDROK:     jsr     GETNEXTNB       ; Get next non-blank from command line
.ifdef ::mtu
            beq     COMP            ; If not, no bank specified, use <from> bank
            cmp     COLON           ; Is it the bank separator?
            bne     ERR27           ; No, error
            iny                     ; Advance one pos in command line
            jsr     GETBYTE         ; Get byte from command line
            bcc     ERR27           ; Not OK, error
            cmp     #$04            ; Is a valid bank number?
            bcs     ERR27           ; No, error
            sta     DESTBNK         ; Yes, store <dest> bank
            eor     DEFBNKCFG       ; and <dest> bank config
            sta     DBNKCFG         ;
.else
            bne     ERR27
.endif
COMP:       ldy     #$00            ; Init Y for comparison
CMPBYT:     
.ifdef ::mtu
            ldx     FBNKCFG         ; Set <from> bank
            stx     BNKCTL          ;
.endif
            lda     (DESTBUFF),y    ; Get byte from <from> block
.ifdef ::mtu
            ldx     DBNKCFG         ; Set <dest> bank
            stx     BNKCTL          ;
.endif
            cmp     (TMPBUFP),y     ; Compare with byte from <dest> bank 
.ifdef ::mtu
            php                     ; Save flags
            ldx     DEFBNKCFG       ; Switch to defaulk bank
            stx     BNKCTL          ;
            plp                     ; Recover flags
.endif
            bne     DIFFER          ; Not equal
            lda     DESTBUFF        ; Compare <from> address lower byte
            cmp     MEMCOUNT        ; with <to> address lower byte
            beq     CHKEND          ; Equal, check if we have reached the end
NXTADD:     inc     DESTBUFF        ; Different, increment <from> and <dest> addresses
            bne     INCDST          ;
            inc     DESTBUFF+1      ;
INCDST:     inc     TMPBUFP         ;
            bne     CMPBYT          ;
            inc     TMPBUFP+1       ;
            bne     CMPBYT          ;
CHKEND:     lda     DESTBUFF+1      ; Compare <from> address higher byte 
            cmp     MEMCOUNT+1      ; with <to> address higher byte
            bne     NXTADD          ; Not yet the end, increment <from> and <dest>
            jsr     OUTSTR          ; If we are here, blocks are identical
            .byte   "SAME.", $00    ;
            rts

DIFFER:     ldx     #_DESTBUFF      ; Print <from> address
            jsr     PRADDR          ;
.ifdef ::mtu
            ldx     FBNKCFG         ; Switch to <from> bank
            stx     BNKCTL          ;
.endif
            lda     (DESTBUFF),y    ; Get byte at <from> address
.ifdef ::mtu
            ldx     DEFBNKCFG       ; Switch to default bank
            stx     BNKCTL          ;
.endif
            jsr     HEXBYTE         ; Print byte as ascii hex into output buffer
            jsr     POUTBUFF02      ; Print buffer to console
            jsr     OUTSTR          ; Print a comma before printing <dest> content
            .byte   ", ", $00       ;
            ldx     #_TMPBUFP       ; Index to <dest> address
.ifdef ::mtu
            lda     DESTBNK         ; Copy DESTBNK into FROMBNK so PRADDR prints
            sta     FROMBNK         ; the correct bank
.endif
            jsr     PRADDR          ; Print <dest> address
.ifdef ::mtu
            ldx     DBNKCFG         ; Switch to <dest> bank
            stx     BNKCTL          ;
.endif
            lda     (TMPBUFP),y     ; Get byte at <dest> address
.ifdef ::mtu
            ldx     DEFBNKCFG       ; Switch to default bank
            stx     BNKCTL          ;
.endif
            jsr     HEXBYTE         ; Print byte as ascii hex into output buffer
            jmp     POUTBUFF02      ; Print buffer to console and return
.endproc

; Print address pointed at P0SCRATCH,x
;
.proc PRADDR
            jsr     HEXENCOD        ; Outputs word at P0SCRATCH,x as 4-char ascii hex
.ifdef ::mtu
            lda     #':'            ; Output the bank separator
            sta     (OUTBUFP),y     ;
            iny                     ; Advance one pos in output buffer
            lda     FROMBNK         ; Get bank
            clc                     ; Convert to ascii
            adc     #'0'            ;
            sta     (OUTBUFP),y     ; And store to output buffer
.endif
            iny                     ; Advance one pos
            jsr     POUTBUFF02      ; Output buffer to console
            jsr     OUTSTR          ; Print equal sign before byte
            .byte   " = ", $00
            rts
.endproc

.ifdef mtu
FROMBNK:    .byte   $00             ; Bank of the <from> address
DESTBNK:    .byte   $00             ; Bank of the <dest> address
FBNKCFG:    .byte   $7F             ; From bank config
DBNKCFG:    .byte   $7F             ; Dest bank config

            ; This block is just junk that was in the buffer when
            ; writing it to disk. I leave it to facilitate checksum
            ; comparisons with the original
            ;
            .byte                            $FE, $7B, $FE
            .byte   $88, $FE, $95, $FE, $B4, $FE, $E0, $FE
.endif
            .end
