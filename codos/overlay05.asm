; da65 V2.19 - Git cf0688fc5
; Created:    2025-05-05 12:21:41
; Input file: kk5.bin
; Page:       1


        .setcpu "6502"
        .segment "overlays"

        .byte   $05

L4328           := $4328
L4554           := $4554
LD939           := $D939
LD995           := $D995
LE9D1           := $E9D1
LF095           := $F095
LF0D6           := $F0D6
LF4A0           := $F4A0
LF4A2           := $F4A2
LF592           := $F592
LF5C5           := $F5C5
LF88B           := $F88B
LF925           := $F925
LF9D3           := $F9D3
LF9F1           := $F9F1
        jsr     LF88B
        bcs     LFE0F
        jsr     LD939
        stx     LFE8E
        jmp     LFE28

LFE0F:  jsr     LD995
        ldx     #$00
        stx     LFE8E
        lda     $E6DC
        cmp     $E74F
        bcs     LFE25
        jsr     LF592
        jmp     LFE28

LFE25:  jsr     LF4A0
LFE28:  ldy     $EB
        jsr     LF925
        bne     LFE37
        lda     #$43
        sta     $E6DC
        jmp     LFE48

LFE37:  jsr     LF88B
        bcs     LFE45
        jsr     LD939
        stx     LFE8F
        jmp     LFE5B

LFE45:  jsr     LD995
LFE48:  ldx     #$09
LFE4A:  lda     $E652,x
        beq     LFE55
        dex
        bpl     LFE4A
        jsr     LE9D1
LFE55:  stx     LFE8F
        jsr     LF4A2
LFE5B:  ldx     LFE8E
        jsr     LF9F1
        bcs     LFE7C
        sta     LFE90
LFE66:  lda     ($CB),y
        sta     ($CD),y
LFE6A:  iny
        cpy     LFE90
        bcc     LFE66
        ldy     LFE90
        ldx     LFE8F
        jsr     LF9D3
        jmp     LFE5B

LFE7C:  ldx     LFE8F
        jsr     LF0D6
        ldx     LFE8F
        jsr     LF5C5
        ldx     LFE8E
        jmp     LF5C5

LFE8E:  brk
LFE8F:  brk
LFE90:  brk
        .byte   $43
        .byte   $4F
        bvc     LFEEE
        .byte   $52
        eor     #$47
        pha
        .byte   $54
        jsr     L4328
        and     #$20
        and     ($39),y
        sec
        bmi     LFEC4
        eor     $4349
        .byte   $52
        .byte   $4F
        jsr     L4554
        .byte   $43
        pha
        lsr     $4C4F
        .byte   $4F
        .byte   $47
        eor     $5520,y
        lsr     $494C
        eor     $5449
        eor     $44
        .byte   $2E
LFEBF:  jsr     LD939
        .byte   $20
        .byte   $9C
LFEC4:  beq     LFE6A
        .byte   $EB
        jsr     LF925
        bne     LFEBF
        rts

LFECD:  jsr     LD939
        jsr     LF095
        ldy     $EB
        jsr     LF925
        bne     LFECD
        rts

        inc     $4D
        cld
        inc     $8D
        cpx     #$BF
        ldx     #$00
        lda     ($C7,x)
        ldx     $E6D8
        stx     $BFE0
        rts

        .byte   $E6
LFEEE:  .byte   $C7
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
