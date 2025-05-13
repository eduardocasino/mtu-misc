; da65 V2.19 - Git cf0688fc5
; Created:    2025-05-05 12:22:48
; Input file: kk9.bin
; Page:       1


            .setcpu "6502"

            .include "codos.inc"
            .include "symbols.inc"

            .segment "overlays"

            .byte   $09

LD968           := $D968
LE9FB           := $E9FB
LF0A4           := $F0A4
LF592           := $F592
LF594           := $F594
LF5C3           := $F5C3
LF899           := $F899
LF89B           := $F89B
LF925           := $F925
LF9D1           := $F9D1
LFD35           := $FD35
LFE01:  jsr     LD968
        jsr     LF592
        ldx     #$00
        jsr     LFD35
        bcc     LFE11
LFE0E:  jsr     LE9FB
LFE11:  lda     $E71E
        bne     LFE0E
        ldy     #$2C
        lda     #$20
LFE1A:  sta     ($CD),y
        dey
        bpl     LFE1A
        ldx     #$00
        ldy     #$00
LFE23:  lda     $E6DE,x
        sta     ($CD),y
        inx
        iny
        cmp     #$2E
        bne     LFE23
        lda     $E6DE,x
        sta     ($CD),y
        iny
        iny
        lda     #$3D
        sta     ($CD),y
        iny
        ldx     #$05
LFE3C:  lda     SAVEDHDR+_PNTRS,x
        sta     $C1,x
        dex
        bpl     LFE3C
        jsr     LF899
        iny
        iny
        iny
        sty     LFEE7
LFE4D:  ldx     #$02
        jsr     LF89B
        iny
        iny
        lda     $E71F
        beq     LFE69
        dey
        dey
        lda     #$3A
        sta     ($CD),y
        iny
        lda     $E71F
        clc
        adc     #$30
        sta     ($CD),y
        iny
LFE69:  iny
        ldx     $C3
        bne     LFE70
        dec     $C4
LFE70:  dex
        txa
        clc
        adc     $C5
        sta     $C1
        lda     $C4
        adc     $C6
        sta     $C2
        jsr     LF899
        lda     $E2
        clc
        adc     $C5
        sta     $CF
        lda     $E3
        adc     $C6
        sta     $D0
        lda     $E4
        adc     #$00
        sta     $D1
        lda     $CF
        sec
        sbc     #$40
        sta     $CF
        lda     $D0
        sbc     #$00
        sta     $D0
        lda     $D1
        sbc     #$00
        sta     $D1
        .byte   $8C
LFEA7:  inx
        inc     a:$A2,x
        jsr     LF0A4
        ldy     LFEE8
        jsr     LF9D1
        ldx     #$00
        jsr     LFD35
        bcc     LFEC8
LFEBB:  jsr     LF5C3
        ldy     $EB
        jsr     LF925
        beq     LFEE6
        jmp     LFE01

LFEC8:  lda     $E71E
        bne     LFEBB
        ldy     #$27
        lda     #$20
LFED1:  sta     ($CD),y
        dey
        bpl     LFED1
        ldx     #$03
LFED8:  lda     $E723,x
        sta     $C3,x
        dex
        bpl     LFED8
        ldy     LFEE7
        jmp     LFE4D

LFEE6:  rts

LFEE7:  .byte   $01
LFEE8:  ora     ($30,x)
        sta     ($CD),y
        jmp     LFEA7

        rts

        jsr     LD968
        ldx     #$01
        jmp     LF594

        .byte   $D7
        inc     $FE00,x
        rts

        brk
        brk
        .byte   $1E
