; da65 V2.19 - Git cf0688fc5
; Created:    2025-05-05 12:21:30
; Input file: kk3.bin
; Page:       1


        .setcpu "6502"
        .segment "overlays"

        .byte   $03

L2820           := $2820
L2E32           := $2E32
L372D           := $372D
LE61B           := $E61B
LE9DD           := $E9DD
LE9E5           := $E9E5
LF924           := $F924
LF925           := $F925
LF92F           := $F92F
LF948           := $F948
LFA50           := $FA50
LFBCC           := $FBCC
LFBE1           := $FBE1
LFD6B           := $FD6B
        bne     LFE06
        jmp     LF948

LFE06:  cmp     #$50
        bne     LFE21
        jsr     LFE5D
        ldx     #$19
        sty     $EB
        jsr     LFBE1
        bcs     LFE19
LFE16:  jsr     LE9E5
LFE19:  jsr     LF925
        sty     $EB
        bne     LFE06
        rts

LFE21:  ldx     #$04
LFE23:  cmp     $F9CC,x
        beq     LFE2E
        dex
        bpl     LFE23
        jsr     LE9DD
LFE2E:  jsr     LFE5D
        cmp     #$27
        beq     LFE39
        cmp     #$22
        bne     LFE52
LFE39:  sta     $E792
        jsr     LF92F
        cmp     #$0D
        beq     LFE16
        sta     $E6C9,x
        jsr     LF92F
        cmp     $E792
        bne     LFE16
        iny
        jmp     LFE19

LFE52:  jsr     LFBCC
        bcc     LFE16
        sta     $E6C9,x
        jmp     LFE19

LFE5D:  jsr     LF924
        cmp     #$3D
        bne     LFE67
        jsr     LF924
LFE67:  sty     $EB
        rts

        bne     LFED5
        jsr     LFD6B
        lda     $CB
        sta     $F0
        lda     $CC
        sta     $F1
        jsr     LFA50
        ora     $4F43
        .byte   $44
        .byte   $4F
        .byte   $53
        jsr     L2E32
        bmi     LFEA5
        jsr     L2820
        .byte   $43
        and     #$20
        and     ($39),y
        sec
        and     ($20),y
        eor     $5554
        brk
        jsr     LFA50
        ora     $4C50
        eor     $41
        .byte   $53
        eor     $20
        eor     $4E
        .byte   $54
        eor     $52
        .byte   $20
LFEA5:  .byte   $44
        eor     ($54,x)
        eor     $20
        plp
        eor     $58
        eor     ($4D,x)
        bvc     LFEFD
        eor     $3A
        bmi     LFEE9
        and     $554A
        jmp     L372D

        rol     $29,x
        .byte   $3F
        and     a:$20,x
        jsr     LE61B
        jsr     LF925
        bne     LFED5
        ldx     #$08
LFECB:  lda     LFEEF,x
        sta     $E6F3,x
        dex
        bpl     LFECB
        rts

LFED5:  ldx     #$00
LFED7:  sta     $E6F3,x
        inx
        cpx     #$09
        beq     LFEEE
        jsr     LF92F
        bne     LFED7
LFEE4:  lda     #$20
        sta     $E6F3,x
LFEE9:  inx
        cpx     #$09
        bcc     LFEE4
LFEEE:  rts

LFEEF:  rol     a
        eor     $4E,x
        .byte   $44
        eor     ($54,x)
        eor     $44
        rol     a
        .byte   $D7
        inc     $FE00,x
        rts

LFEFD:  brk
        brk
        .byte   $1E
