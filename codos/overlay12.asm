; da65 V2.19 - Git cf0688fc5
; Created:    2025-05-05 12:23:21
; Input file: kk12.bin
; Page:       1


        .setcpu "6502"
        .segment "overlays"

        .byte   $0C

LD9B7           := $D9B7
LD9F1           := $D9F1
LE9E1           := $E9E1
LE9E5           := $E9E5
LE9F5           := $E9F5
LF89B           := $F89B
LF924           := $F924
LF925           := $F925
LF9D6           := $F9D6
LF9DB           := $F9DB
LFBCC           := $FBCC
        jsr     LD9B7
        lda     $E6D4
        eor     $E6D8
        sta     LFEF2
        jsr     LD9F1
        lda     $C5
        sec
        sbc     $C3
        sta     $C7
        lda     $C6
        sbc     $C4
        sta     $C8
        bcs     LFE22
        jsr     LE9F5
LFE22:  lda     #$FF
        sta     LFEF4
        lda     $C3
        sta     $C9
        lda     $C4
        sta     $CA
        jsr     LF925
        bne     LFE37
LFE34:  jsr     LE9E5
LFE37:  ldx     #$00
        cmp     #$3D
        bne     LFE42
        jsr     LF924
        beq     LFE34
LFE42:  cmp     #$3F
        beq     LFE34
LFE46:  cmp     #$27
        beq     LFE73
        cmp     #$22
        beq     LFE73
        cmp     #$3F
        bne     LFE61
        sty     $EB
        bit     LFEF4
        bpl     LFE34
        stx     LFEF4
        inx
        iny
        jmp     LFE6C

LFE61:  jsr     LFBCC
        sta     LFEF5,x
        inx
        cpx     #$0B
        bcs     LFE34
LFE6C:  jsr     LF925
        bne     LFE46
        beq     LFE94
LFE73:  sta     $E792
LFE76:  iny
        lda     ($CB),y
        bne     LFE7E
        jsr     LE9E1
LFE7E:  cmp     $E792
        beq     LFE8E
        sta     LFEF5,x
        inx
        cpx     #$0B
        bcs     LFE34
        jmp     LFE76

LFE8E:  iny
        sty     $EB
        jmp     LFE6C

LFE94:  stx     LFEF3
        cpx     #$00
        beq     LFE34
LFE9B:  ldx     LFEF2
        stx     $BFE0
        ldy     #$00
        ldx     #$00
LFEA5:  lda     ($C9),y
        cmp     LFEF5,x
        bne     LFED1
LFEAC:  iny
        inx
        cpx     LFEF3
        bne     LFEA5
        lda     $CA
        cmp     #$FE
        beq     LFED1
        ldx     $E6D8
        stx     $BFE0
        ldy     #$00
        ldx     #$08
        jsr     LF89B
        ldx     #$02
        jsr     LF9DB
        jsr     LF9D6
        jmp     LFED6

LFED1:  cpx     LFEF4
        beq     LFEAC
LFED6:  inc     $C9
        bne     LFEDC
        inc     $CA
LFEDC:  lda     $C7
        bne     LFEE6
        lda     $C8
        beq     LFEEB
        dec     $C8
LFEE6:  dec     $C7
        jmp     LFE9B

LFEEB:  ldx     $E6D8
        stx     $BFE0
        rts

LFEF2:  .byte   $C7
LFEF3:  .byte   $AE
LFEF4:  cld
LFEF5:  inc     $8E
        cpx     #$BF
        rts

        .byte   $7F
        .byte   $7F
        rts

        brk
        brk
        .byte   $1E
