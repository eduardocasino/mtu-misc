; da65 V2.19 - Git cf0688fc5
; Created:    2025-05-05 12:21:23
; Input file: kk2.bin
; Page:       1


        .setcpu "6502"
        .segment "overlays"

        .byte   $02

LD968           := $D968
LD9B7           := $D9B7
LD9F1           := $D9F1
LE9E5           := $E9E5
LE9E7           := $E9E7
LE9F3           := $E9F3
LE9F9           := $E9F9
LF594           := $F594
LF924           := $F924
LF925           := $F925
LF92F           := $F92F
LFBCC           := $FBCC
        jsr     LD9B7
        jsr     LD9F1
        cld
        sec
        lda     $C5
        sbc     $C3
        sta     $C5
        lda     $C6
        sbc     $C4
        sta     $C6
        bcs     LFE1A
        jsr     LE9F9
LFE1A:  jsr     LF925
        cmp     #$3D
        bne     LFE24
        jsr     LF924
LFE24:  sty     $EB
        cmp     #$27
        beq     LFE2E
        cmp     #$22
        bne     LFE58
LFE2E:  sta     $E792
        jsr     LF92F
        cmp     #$00
        bne     LFE3B
LFE38:  jsr     LE9E5
LFE3B:  tax
        jsr     LF92F
        cmp     $E792
        bne     LFE38
LFE44:  txa
        jsr     LFE60
        lda     $C5
        sec
        sbc     #$01
        sta     $C5
        lda     $C6
        sbc     #$00
        sta     $C6
        bcs     LFE44
        rts

LFE58:  jsr     LFBCC
        bcc     LFE38
        tax
        bcs     LFE44
LFE60:  sta     $0296
        lda     $E6D4
        bne     LFE6E
        jsr     LFE97
        lda     $E6D4
LFE6E:  eor     $E6D8
        sta     $BFE0
        lda     $0296
        stx     $0296
        ldx     #$00
        sta     ($C3,x)
        cmp     ($C3,x)
        php
        lda     $E6D8
        sta     $BFE0
        plp
        beq     LFE8D
        jsr     LE9E7
LFE8D:  inc     $C3
        bne     LFE93
        inc     $C4
LFE93:  ldx     $0296
        rts

LFE97:  lda     $C4
        cmp     #$E0
        bcc     LFEA0
        jsr     LFEBF
LFEA0:  cmp     #$02
        bcs     LFEBE
        cmp     #$01
        .byte   $90
LFEA7:  ora     $BA8A
        cpx     $C3
        bcs     LFEB1
        jsr     LFEBF
LFEB1:  tax
        jmp     LFEBE

LFEB5:  lda     $C3
        cmp     #$C1
        bcc     LFEBE
        jsr     LFEBF
LFEBE:  rts

LFEBF:  bit     $E777
        bmi     LFEC7
        jsr     LE9F3
LFEC7:  rts

        .byte   $03
        jsr     LE9F3
        rts

        nop
        lda     #$80
        sta     $E778
        sta     $E777
        rts

        lda     #$00
        sta     $E778
        rts

        cmp     $A9C8
        .byte   $3A
        sta     ($CD),y
        iny
        lda     $DD
        cld
        clc
        adc     #$30
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
