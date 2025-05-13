; da65 V2.19 - Git cf0688fc5
; Created:    2025-05-05 12:23:08
; Input file: kk11.bin
; Page:       1


        .setcpu "6502"

        .include "symbols.inc"

        .segment "overlays"

        .byte   $0B

LD968           := $D968
LE9B1           := $E9B1
LE9E3           := $E9E3
LEA07           := $EA07
LEE9B           := $EE9B
LEEAA           := $EEAA
LEEE3           := $EEE3
LF09C           := $F09C
LF592           := $F592
LF5C3           := $F5C3
LF77A           := $F77A
        jsr     LD968
        ldx     $E6DC
        stx     LFE7E
        jsr     LF592
        lda     $DC
        and     #$20
        beq     LFE16
        jsr     LEA07
LFE16:  ldy     $EB
        jsr     LD968
        ldx     LFE7E
        stx     $E6DC
        jsr     LF77A
        bne     LFE29
        jsr     LE9E3
LFE29:  ldx     #$00
        jsr     LF09C
        ldy     #$15
        lda     ($E5),y
        jsr     LEE9B
        ldy     #$14
        lda     ($E5),y
        tax
        sta     $E6DB
        ldy     #$01
LFE3F:  lda     $E500,x
        cmp     #$2E
        beq     LFE52
        cmp     ($E5),y
        beq     LFE4D
        jsr     LE9B1
LFE4D:  iny
        inx
        jmp     LFE3F

LFE52:  ldx     $E6DB
        ldy     #$00
LFE57:  lda     $E6DE,y
        sta     $E500,x
        inx
        iny
        cpy     #$0E
        bne     LFE57
        ldx     #$00
        ldy     #$01
LFE67:  lda     $E6DE,x
        sta     ($E5),y
        iny
        inx
        cpx     #$0E
        bne     LFE67
        jsr     LEEE3
        jsr     WRFPSECT
        jsr     LEEAA
        jmp     LF5C3

LFE7E:  brk
        dec     $C6
        cpy     $CE
        .byte   $FC
        inc     $09F0,x
LFE87:  jsr     LFECC
        dey
        dec     LFEFC
        bne     LFE87
        jsr     LFECC
        rts

        ldy     #$00
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

        lda     $C5
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
