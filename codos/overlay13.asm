; da65 V2.19 - Git cf0688fc5
; Created:    2025-05-05 12:23:27
; Input file: kk13.bin
; Page:       1


        .setcpu "6502"
        .segment "overlays"

        .byte   $0D

LFB35           := $FB35
LFBB3           := $FBB3
        clc
        rol     $D6
        cld
        jsr     LFE4C
        tay
        and     #$07
        asl     a
        tax
        tya
        lsr     a
        lsr     a
        lsr     a
        and     #$1E
        beq     LFE2B
        tay
        lda     #$FE
        pha
        lda     #$01
        cpy     #$06
        bcc     LFE21
        lda     #$03
LFE21:  pha
        lda     LFEE1,y
        pha
        lda     LFEE0,y
        pha
        rts

LFE2B:  lda     $C1,x
        ora     $C2,x
        php
        pla
        asl     a
        clc
        ldy     $C2,x
        bpl     LFE38
        sec
LFE38:  ror     a
        lsr     a
        ror     $D6
        rol     a
        sta     $E6CA
LFE40:  ldy     #$11
LFE42:  lda     $C0,y
        sta     $AF,y
        dey
        bne     LFE42
        rts

LFE4C:  ldy     #$02
        lda     ($DA),y
        inc     $DA
        bne     LFE56
        inc     $DB
LFE56:  rts

        jsr     LFE4C
        sta     $C1,x
        jsr     LFE4C
        sta     $C2,x
        rts

        jsr     LFE9F
        lda     ($D2),y
        sta     $C1,x
        iny
        lda     ($D2),y
        sta     $C2,x
        rts

        jsr     LFEAA
        lda     ($D2),y
        sta     $C1
        iny
        lda     ($D2),y
        sta     $C2
        rts

        jsr     LFE9F
        lda     $C1,x
        sta     ($D2),y
        iny
        lda     $C2,x
        sta     ($D2),y
LFE88:  rts

        jsr     LFEAA
        lda     $C1
        sta     ($D2),y
        iny
        lda     $C2
        sta     ($D2),y
        rts

        lda     $C1,x
        sta     $D1
        ldx     #$0E
        jmp     LFBB3

LFE9F:  jsr     LFE4C
        sta     $D2
        jsr     LFE4C
        jmp     LFEB0

LFEAA:  lda     $C1,x
        sta     $D2
        lda     $C2,x
LFEB0:  sta     $D3
        ldy     #$00
        rts

        jsr     LFE40
        lda     #$E6
        pha
        lda     #$14
        pha
        lda     $C1,x
        sec
        sbc     #$01
        sta     $D7
        lda     $C2,x
        sbc     #$00
        pha
        lda     $D7
        pha
        lsr     $D6
        php
        rol     $D6
        plp
        lda     $C1
        rts

        jsr     LFB35
        lda     $D2
        sta     $C1,x
        lda     $D3
        .byte   $95
LFEE0:  .byte   $C2
LFEE1:  rts

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
