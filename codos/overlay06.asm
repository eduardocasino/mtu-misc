; da65 V2.19 - Git cf0688fc5
; Created:    2025-05-05 12:22:14
; Input file: kk6.bin
; Page:       1


        .setcpu "6502"
        .segment "overlays"

        .byte   $06

LD939           := $D939
LD968           := $D968
LEDA6           := $EDA6
LEDB5           := $EDB5
LEE70           := $EE70
LEEF4           := $EEF4
LEFB9           := $EFB9
LEFD0           := $EFD0
LF592           := $F592
LF5C3           := $F5C3
LF81C           := $F81C
LF88B           := $F88B
LF925           := $F925
LF92F           := $F92F
LF9D6           := $F9D6
LF9F1           := $F9F1
LFA88           := $FA88
LFE01:  jsr     LF88B
        bcc     LFE19
        jsr     LD968
        jsr     LF592
LFE0C:  jsr     LFE3D
        ldy     $EB
        jsr     LF925
        bne     LFE01
        jmp     LF5C3

LFE19:  jsr     LD939
        jmp     LFE0C

LFE1F:  jsr     LF88B
        bcc     LFE37
        jsr     LD968
        jsr     LF592
LFE2A:  jsr     LFE72
        ldy     $EB
        jsr     LF925
        bne     LFE1F
        jmp     LF5C3

LFE37:  jsr     LD939
        jmp     LFE2A

LFE3D:  jsr     LEFB9
        bpl     LFE43
        rts

LFE43:  jsr     LEE70
        jsr     LF81C
        lda     #$94
        sta     $E7
        jsr     LEFD0
        jsr     LEDB5
        ldx     #$10
        lda     $E500,x
        ora     #$20
        sta     $E500,x
        lda     $E76E
        ldx     $DD
        jsr     LEDA6
        jsr     LEE70
        lda     $DC
        ora     #$20
        sta     $DC
        jsr     LEEF4
        rts

LFE72:  jsr     LEFB9
        bpl     LFE78
        rts

LFE78:  jsr     LEE70
        jsr     LF81C
        lda     #$94
        sta     $E7
        jsr     LEFD0
        jsr     LEDB5
        ldx     #$10
        lda     $E500,x
        and     #$DF
        sta     $E500,x
        lda     $E76E
        ldx     $DD
        jsr     LEDA6
        jsr     LEE70
        lda     $DC
        and     #$DF
        sta     $DC
        jsr     LEEF4
        rts

        beq     LFEAD
        lda     #$00
        beq     LFEAF
LFEAD:  lda     #$80
LFEAF:  sta     $E785
        rts

        ldy     $EB
        lda     #$00
        sta     $FFE8
        jsr     LD939
        stx     LFEFA
        jsr     LF925
        bne     LFEC8
LFEC5:  jmp     LF9D6

LFEC8:  cmp     $E791
        beq     LFED7
LFECD:  jsr     LFA88
        jsr     LF92F
        beq     LFEC5
        bne     LFECD
LFED7:  ldx     #$01
        jsr     LF9F1
        ldx     LFEFA
        bcs     LFEC5
        ldy     #$00
LFEE3:  lda     ($CB),y
        cmp     #$0D
        beq     LFEF4
        cmp     $E791
        beq     LFEC5
        jsr     LFA88
        iny
        bne     LFEE3
LFEF4:  jsr     LFA88
        jmp     LFED7

LFEFA:  brk
        inc     a:$60,x
        brk
        .byte   $1E
