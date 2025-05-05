; da65 V2.19 - Git cf0688fc5
; Created:    2025-05-05 13:32:25
; Input file: kk4.bin
; Page:       1


        .setcpu "6502"
        .segment "overlays"

        .byte   $04

LD939           := $D939
LD9B7           := $D9B7
LD9F1           := $D9F1
LE9F5           := $E9F5
LF4A0           := $F4A0
LF5C3           := $F5C3
LF89B           := $F89B
LF8A2           := $F8A2
LF8AB           := $F8AB
LF925           := $F925
LF9D3           := $F9D3
        ldx     #$00
        stx     $CA
        inx
        stx     $C9
        inx
        stx     $E79A
        jsr     LD9B7
        lda     $C3
        sta     $C7
        lda     $C4
        sta     $C8
        jsr     LF925
        beq     LFE4B
        jsr     LD9F1
        lda     $C5
        cld
        sec
        sbc     $C7
        sta     $C9
        lda     $C6
        sbc     $C8
        sta     $CA
        bcs     LFE32
        jsr     LE9F5
LFE32:  jsr     LF925
        beq     LFE4B
        cmp     #$41
        bcc     LFE45
        sta     $E6DC
        jsr     LF4A0
        ldx     #$00
        beq     LFE48
LFE45:  jsr     LD939
LFE48:  stx     $E79A
LFE4B:  jsr     LFEC4
        ldy     #$06
        lda     $C7
        and     #$0F
        tax
        dex
LFE56:  inx
        cpx     #$10
        bcc     LFE5D
        ldx     #$00
LFE5D:  txa
        jsr     LF8AB
        iny
        iny
        jsr     LFEF3
        bne     LFE56
        ldx     $E79A
        jsr     LF9D3
LFE6E:  jsr     LFEC4
        ldx     #$06
        jsr     LF89B
        iny
LFE77:  iny
        jsr     LFED9
        jsr     LF8A2
        jsr     LFEED
        bne     LFE77
        lda     $C7
        sec
        sbc     $E799
        sta     $C7
        lda     $C8
        sbc     #$00
        sta     $C8
        jsr     LFECE
        iny
        iny
LFE96:  jsr     LFED9
        cmp     #$21
        bcc     LFEA1
        cmp     #$7F
        bcc     LFEA3
LFEA1:  lda     #$2E
LFEA3:  sta     ($CD),y
        iny
        jsr     LFEED
        bne     LFE96
        ldx     $E79A
        jsr     LF9D3
        lda     $C9
        sec
        sbc     $E799
        sta     $C9
        lda     $CA
        sbc     #$00
        sta     $CA
        bcs     LFE6E
        jmp     LF5C3

LFEC4:  ldy     #$47
        lda     #$20
LFEC8:  sta     ($CD),y
        dey
        bpl     LFEC8
        iny
LFECE:  lda     $E799
        sta     LFEFD
        lsr     a
        sta     LFEFE
        rts

LFED9:  lda     $E6D4
        eor     $E6D8
        sta     $BFE0
        ldx     #$00
        lda     ($C7,x)
        ldx     $E6D8
        stx     $BFE0
        rts

LFEED:  inc     $C7
        bne     LFEF3
        inc     $C8
LFEF3:  dec     LFEFE
        bne     LFEF9
        iny
LFEF9:  dec     LFEFD
        rts

LFEFD:  brk
LFEFE:  brk
        .byte   $1E
