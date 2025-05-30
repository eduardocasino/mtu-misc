; da65 V2.19 - Git cf0688fc5
; Created:    2025-05-05 12:23:00
; Input file: kk10.bin
; Page:       1


            .setcpu "6502"

.ifdef mtu
            .include "monomeg.inc"
.endif
            .include "symbols.inc"
            .include "codos.inc"

            .segment "overlays"

            .byte   $0A             ; Overlay number

; COPY Command
;
; DESCRIPTION:  Copy a block of memory to another memory location. 
;
; SYNTAX:       COPY <from><to><dest> 
;
; ARGUMENTS:    <from> = starting address of block to be copied
;               <to> = ending address of block to be copied
;               <dest> = starting address of destination of copy
;
.proc COPY
            jsr     GADDRBNKMB      ; Get address and bank and store into MEMBUFF and NEWBNK
            jsr     GETTOP          ; Get TO address and store into MEMCOUNT
            ldx     #_TMPBUFP       ; Get <dest> address and store into TMPBUFP
            jsr     EVALEXP         ;
            bcs     CONT            ; Expression OK, continue
BADDST:     jsr     ERROR27         ; <destination> address missing or illegal
            ; Not reached

CONT:       
.ifdef ::mtu
            lda     NEWBNK          ; Get bank of starting address
            eor     DEFBNKCFG       ;
            sta     FROMBANK        ; Save it
            sta     DESTBANK        ; And set also as default for destination
.endif
            jsr     GETNEXTNB       ; Get next non-blank from command line
.ifdef ::mtu
            beq     ARGSDONE        ; If no more, continue
            cmp     COLON           ; Is it the bank delimiter? 
            bne     BADDST          ; No, bad destination address
            iny                     ; Yes, get it
            jsr     GETBYTE         ;
            bcc     BADDST          ; Not a number, bad destination
            cmp     #$04            ; Check if a valid bank number
            bcs     BADDST          ; No, bad destination
            eor     DEFBNKCFG       ; Set as destination bank
            sta     DESTBANK        ;
.else
            bne     BADDST
.endif
ARGSDONE:   jsr     GETCOUNT        ; Get number of bytes to copy
            bcs     CDIST           ; If not negative, continue
            jsr     ERROR16         ; <from> address greater than <to> address
            ; Not reached

            ; Calculate <from> to <dest> distance

CDIST:      lda     TMPBUFP         ; Substract <from> address from <dest> address
            sec                     ;
            sbc     MEMBUFF         ;
            sta     DESTBUFF        ; and store into DESTBUFF
            lda     TMPBUFP+1       ;
            sbc     MEMBUFF+1       ;
            sta     DESTBUFF+1      ;
            bcc     CPFWD           ; Jump if result is negative, (dest is lower than from)

            ; Calculate <dest> + count and store into MEMBUFF

            lda     DESTBUFF        ; Add <to> address to DESTBUFF
            clc                     ;
            adc     MEMCOUNT        ;
            sta     MEMBUFF         ; And store into MEMBUFF
            lda     DESTBUFF+1      ;
            adc     MEMCOUNT+1      ;
            sta     MEMBUFF+1       ;

            ; Copy backwards from <to> to <dest>+count

            ldy     #$00            ; init index for indirect addressing in COPYBCK
            ldx     BYTCOUNT+1      ; Get byte count MSB
            beq     CHKLSB          ; If zero, go check LSB
CPPAGE:     jsr     COPYBCK         ; Not zero, copy one byte
            dey                     ; Decrement index (now $FF)
            dec     MEMCOUNT+1      ; Decrement source and dest pages
            dec     MEMBUFF+1       ;
BYTBK:      jsr     COPYBCK         ; Copy one page
            dey                     ;
            bne     BYTBK           ;
            dec     BYTCOUNT+1      ; Decrement 256 bytes of bytecount
            bne     CPPAGE          ; While more pages, repeat
CHKLSB:     ldx     BYTCOUNT        ; Any more bytes to copy?
            beq     CBRET           ; No, copy last and return
            jsr     COPYBCK         ; Yes, copy
            dey                     ; Decrement index (now $FF)
            dec     MEMCOUNT+1      ; Decrement source and dest pages
            dec     MEMBUFF+1       ;
            dec     BYTCOUNT        ; Any more bytes?
            beq     CBRET           ; No, copy last and return
BYTBK2:     jsr     COPYBCK         ; Yes, copy backwards until no more bytes
            dey                     ;
            dec     BYTCOUNT        ;
            bne     BYTBK2          ;
CBRET:      jsr     COPYBCK         ; Copy last byte
            rts

            ; Copy forwards from <from> to <dest>

CPFWD:      ldy     #$00
            ldx     BYTCOUNT+1      ; Get byte count MSB
            beq     CHKLSB2         ; If 0, go check LSB
CPPAGE2:    jsr     COPYFWD         ; Not zero, copy one page
            iny                     ;
            bne     CPPAGE2         ;
            inc     MEMBUFF+1       ; Advance to next page
            inc     TMPBUFP+1       ;
            dec     BYTCOUNT+1      ; Decrement 256 bytes of bytecount
            bne     CPPAGE2         ; While more pages, repeat
CHKLSB2:    ldx     BYTCOUNT        ; Any more bytes to copy;
            beq     CFRET           ; No, copy last and return
BYTFW:      jsr     COPYFWD         ; Yes, copy forwards until no more bytes
            iny                     ;
            dec     BYTCOUNT        ;
            bne     BYTFW           ;
CFRET:      jsr     COPYFWD         ; Copy last byte
            rts
.endproc

; Get number of bytes to copy
;
.proc GETCOUNT
            lda     MEMCOUNT        ; Get ending address
            sec                     ; Substract memory origin
            sbc     MEMBUFF         ;
            sta     BYTCOUNT        ; And store byte count
            lda     MEMCOUNT+1      ;
            sbc     MEMBUFF+1       ;
            sta     BYTCOUNT+1      ;
            rts
.endproc

; Copy byte backwards from <to> bank:address,y to <dest>+count bank:address,y
;
.proc COPYBCK
.ifdef ::mtu
            ldx     FROMBANK        ; Switch to source bank
            stx     BNKCTL          ;
.endif
            lda     (MEMCOUNT),y    ; Read byte from address,y
.ifdef ::mtu
            ldx     DESTBANK        ; Switch to dest bank
            stx     BNKCTL          ;
.endif
            sta     (MEMBUFF),y     ; Store byte into address,y
.ifdef ::mtu
            ldx     DEFBNKCFG       ; Switch to default bank
            stx     BNKCTL          ;
.endif
            rts
.endproc

; Copy byte forwards from <from> bank:address to dest bank:address
;
.proc COPYFWD
.ifdef ::mtu
            ldx     FROMBANK        ; Switch to source bank
            stx     BNKCTL          ;
.endif
            lda     (MEMBUFF),y     ; Read byte from source address,y
.ifdef ::mtu
            ldx     DESTBANK        ; Switch to dest bank
            stx     BNKCTL          ;
.endif
            sta     (TMPBUFP),y     ; Store byte into dest address,y
.ifdef ::mtu
            ldx     DEFBNKCFG       ; Switch to default bank
            stx     BNKCTL          ;
.endif
            rts
.endproc

.ifdef mtu
FROMBANK:      .byte   $7F
DESTBANK:      .byte   $7F
.endif
BYTCOUNT:      .word   $0060

.ifdef mtu
            ; These two bytes are just junk that was in the buffer when
            ; writing it to disk. I leave it to facilitate checksum
            ; comparisons with the original
            ;
            .byte   $00, $1E
.endif
            .end
