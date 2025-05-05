; da65 V2.19 - Git cf0688fc5
; Created:    2025-05-05 12:23:00
; Input file: kk10.bin
; Page:       1


        .setcpu "6502"
        .segment "overlays"

        .byte   $0A

LD9B7           := $D9B7
LD9F1           := $D9F1
LE9DF           := $E9DF
LE9F5           := $E9F5
LF925           := $F925
LFBCC           := $FBCC
LFBE1           := $FBE1
        jsr     LD9B7
        jsr     LD9F1
        ldx     #$06
        jsr     LFBE1
        bcs     LFE11
LFE0E:  jsr     LE9DF
LFE11:  lda     $E6D4
        eor     $E6D8
        sta     LFEFA
        sta     LFEFB
        jsr     LF925
        beq     LFE37
        cmp     $E790
        bne     LFE0E
        iny
        jsr     LFBCC
        bcc     LFE0E
        cmp     #$04
        bcs     LFE0E
        eor     $E6D8
        sta     LFEFB
LFE37:  jsr     LFEBC
        bcs     LFE3F
        jsr     LE9F5
LFE3F:  lda     $C7
        sec
        sbc     $C3
        sta     $C9
        lda     $C8
        sbc     $C4
        sta     $CA
        bcc     LFE94
        lda     $C9
        clc
        adc     $C5
        sta     $C3
        lda     $CA
        adc     $C6
        sta     $C4
        ldy     #$00
        ldx     LFEFD
        beq     LFE75
LFE62:  jsr     LFECC
        dey
        dec     $C6
        dec     $C4
LFE6A:  jsr     LFECC
        dey
        bne     LFE6A
        dec     LFEFD
        bne     LFE62
LFE75:  ldx     LFEFC
        beq     LFE90
        jsr     LFECC
        dey
        dec     $C6
        dec     $C4
        dec     LFEFC
        beq     LFE90
LFE87:  jsr     LFECC
        dey
        dec     LFEFC
        bne     LFE87
LFE90:  jsr     LFECC
        rts

LFE94:  ldy     #$00
        ldx     LFEFD
        beq     LFEAA
LFE9B:  jsr     LFEE3
        iny
        bne     LFE9B
        inc     $C4
        inc     $C8
        dec     LFEFD
        bne     LFE9B
LFEAA:  ldx     LFEFC
        beq     LFEB8
LFEAF:  jsr     LFEE3
        iny
        dec     LFEFC
        bne     LFEAF
LFEB8:  jsr     LFEE3
        rts

LFEBC:  lda     $C5
        sec
        sbc     $C3
        sta     LFEFC
        lda     $C6
        sbc     $C4
        sta     LFEFD
        rts

LFECC:  ldx     LFEFA
        stx     $BFE0
        lda     ($C5),y
        ldx     LFEFB
        stx     $BFE0
        sta     ($C3),y
        ldx     $E6D8
        stx     $BFE0
        rts

LFEE3:  ldx     LFEFA
        stx     $BFE0
        lda     ($C3),y
        ldx     LFEFB
        stx     $BFE0
        sta     ($C7),y
        ldx     $E6D8
        stx     $BFE0
        rts

LFEFA:  .byte   $7F
LFEFB:  .byte   $7F
LFEFC:  rts

LFEFD:  brk
        brk
        .byte   $1E
