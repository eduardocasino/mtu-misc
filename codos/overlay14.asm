; da65 V2.19 - Git cf0688fc5
; Created:    2025-05-05 12:23:32
; Input file: kk14.bin
; Page:       1


        .setcpu "6502"
        .segment "overlays"

        .byte   $0E

L0315           := $0315
LE9AD           := $E9AD
LE9E1           := $E9E1
LF924           := $F924
LF925           := $F925
LFB35           := $FB35
LFBCC           := $FBCC
        jsr     LF925
        beq     LFE39
        jsr     LFBCC
        sec
        sbc     #$01
        cmp     #$08
        bcc     LFE13
        jsr     LE9AD
LFE13:  asl     a
        asl     a
        asl     a
        sta     $029D
        clc
        adc     #$08
        sta     $029E
        tax
        jsr     LFE4F
        lda     $029D
        asl     a
        asl     a
        sta     $029D
        clc
        adc     #$1F
        tax
        sta     $029E
        jsr     LFE8A
LFE35:  jsr     L0315
        rts

LFE39:  ldx     #$3F
        lda     #$20
LFE3D:  sta     $05C0,x
        dex
        bpl     LFE3D
        inx
        lda     #$80
LFE46:  sta     $0400,x
        dex
        bne     LFE46
        jmp     LFE35

LFE4F:  lda     #$20
LFE51:  dex
        sta     $05C0,x
        cpx     $029D
        bne     LFE51
        jsr     LF925
        cmp     #$0D
        beq     LFE89
        cmp     #$27
        beq     LFE6E
        cmp     #$22
        beq     LFE6E
LFE69:  sty     $EB
        jsr     LE9E1
LFE6E:  sta     $E792
LFE71:  iny
        lda     ($CB),y
        cmp     #$0D
        beq     LFE69
        cmp     $E792
        beq     LFE88
        cpx     $029E
        bcs     LFE71
        sta     $05C0,x
        inx
        bne     LFE71
LFE88:  iny
LFE89:  rts

LFE8A:  lda     #$80
        inx
LFE8D:  dex
        sta     $0400,x
        cpx     $029D
        bne     LFE8D
        jsr     LF925
        cmp     #$0D
        beq     LFED4
        cmp     #$27
        beq     LFEAA
        cmp     #$22
        beq     LFEAA
LFEA5:  sty     $EB
        jsr     LE9E1
LFEAA:  sta     $E792
LFEAD:  iny
        lda     ($CB),y
        cmp     #$0D
        beq     LFEA5
        cmp     $E792
        beq     LFEC4
        cpx     $029E
        bcs     LFEAD
        sta     $0400,x
        inx
        bne     LFEAD
LFEC4:  jsr     LF924
        beq     LFECF
        jsr     LFBCC
        jmp     LFED1

LFECF:  lda     #$0D
LFED1:  sta     $0400,x
LFED4:  rts

        rts

        jsr     LFB35
        lda     $D2
        sta     $C1,x
        lda     $D3
        sta     $C2,x
        rts

        .byte   $0C
        .byte   $FB
        .byte   $1A
        .byte   $FB
        cmp     $FE,x
        .byte   $6F
        .byte   $FB
        lda     #$FB
        .byte   $B2
        .byte   $FB
        .byte   $BB
        .byte   $FB
        lsr     $FE,x
        adc     ($FE,x)
        ror     $7BFE
        inc     LFE88,x
        sta     $FE,x
        ldy     $FE,x
        cpx     #$FE
