; da65 V2.19 - Git cf0688fc5
; Created:    2025-05-05 12:23:37
; Input file: kk15.bin
; Page:       1


        .setcpu "6502"
        .segment "overlays"

        .byte   $0F

L203D           := $203D
LD9B7           := $D9B7
LD9F1           := $D9F1
LE9DF           := $E9DF
LE9F5           := $E9F5
LF89B           := $F89B
LF8A2           := $F8A2
LF925           := $F925
LF9DB           := $F9DB
LFA50           := $FA50
LFBCC           := $FBCC
LFBE1           := $FBE1
        ldx     #$02
        jsr     LD9B7
        lda     $E6D4
        sta     LFEF1
        sta     LFEF2
        eor     $E6D8
        sta     LFEF3
        sta     LFEF4
        lda     $C3
        sta     $C9
        lda     $C4
        sta     $CA
        jsr     LD9F1
        ldx     #$06
        jsr     LFBE1
        bcs     LFE2D
LFE2A:  jsr     LE9DF
LFE2D:  lda     $C5
        cmp     $C9
        lda     $C6
        sbc     $CA
        bcs     LFE3A
        jsr     LE9F5
LFE3A:  jsr     LF925
        beq     LFE57
        cmp     $E790
        bne     LFE2A
        iny
        jsr     LFBCC
        bcc     LFE2A
        cmp     #$04
        bcs     LFE2A
        sta     LFEF2
        eor     $E6D8
        sta     LFEF4
LFE57:  ldy     #$00
LFE59:  ldx     LFEF3
        stx     $BFE0
        lda     ($C9),y
        ldx     LFEF4
        stx     $BFE0
        cmp     ($C7),y
        php
        ldx     $E6D8
        stx     $BFE0
        plp
        bne     LFE97
        lda     $C9
        cmp     $C5
        beq     LFE87
LFE79:  inc     $C9
LFE7B:  bne     LFE7F
        inc     $CA
LFE7F:  inc     $C7
        bne     LFE59
        inc     $C8
        bne     LFE59
LFE87:  lda     $CA
        cmp     $C6
        bne     LFE79
        jsr     LFA50
        .byte   $53
        eor     ($4D,x)
        eor     $2E
LFE95:  brk
        rts

LFE97:  ldx     #$08
        jsr     LFED5
        ldx     LFEF3
        stx     $BFE0
        lda     ($C9),y
        ldx     $E6D8
        stx     $BFE0
        jsr     LF8A2
        jsr     LF9DB
        jsr     LFA50
        bit     a:$20
        ldx     #$06
        lda     LFEF2
        sta     LFEF1
        jsr     LFED5
        ldx     LFEF4
        stx     $BFE0
        lda     ($C7),y
        ldx     $E6D8
        stx     $BFE0
        jsr     LF8A2
        jmp     LF9DB

LFED5:  jsr     LF89B
        lda     #$3A
        sta     ($CD),y
        iny
        lda     LFEF1
        clc
        adc     #$30
        sta     ($CD),y
        iny
        jsr     LF9DB
        jsr     LFA50
        jsr     L203D
        brk
        rts

LFEF1:  brk
LFEF2:  brk
LFEF3:  .byte   $7F
LFEF4:  .byte   $7F
        inc     LFE7B,x
        dey
        inc     LFE95,x
        ldy     $FE,x
        cpx     #$FE
