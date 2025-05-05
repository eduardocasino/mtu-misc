; da65 V2.19 - Git cf0688fc5
; Created:    2025-05-05 12:23:49
; Input file: kk16.bin
; Page:       1


        .setcpu "6502"
        .segment "overlays"

        .byte   $10

L2000           := $2000
L203D           := $203D
L4946           := $4946
L5246           := $5246
L5345           := $5345
L5628           := $5628
LEE78           := $EE78
LEF90           := $EF90
LF899           := $F899
LF89B           := $F89B
LF8A2           := $F8A2
LF8AB           := $F8AB
LF9DB           := $F9DB
LFA50           := $FA50
LFAC1           := $FAC1
LFD76           := $FD76
        ldx     #$00
LFE03:  lda     $E754,x
        bpl     LFE11
        stx     LFEA8
        jsr     LFE18
        ldx     LFEA8
LFE11:  inx
        cpx     $E74F
        bcc     LFE03
        rts

LFE18:  stx     $E6DC
        jsr     LEF90
        jsr     LFD76
        jsr     LEE78
        ldy     #$FD
        lda     ($E9),y
        sta     $C1
        lda     #$00
        sta     $C2
        ldy     #$00
        jsr     LFAC1
        jsr     LF9DB
        jsr     LFA50
        jsr     L4946
        jmp     L5345

        .byte   $3A
        brk
        lda     $E6DC
        jsr     LF8AB
        jsr     LF9DB
        jsr     LFA50
        jsr     L5628
        .byte   $53
        lsr     a:$3D
        ldy     #$FA
        lda     ($E9),y
        sta     $C1
        iny
        lda     ($E9),y
        sta     $C2
        ldy     #$00
        jsr     LF899
        jsr     LF9DB
        jsr     LFA50
        and     #$2C
        jsr     L2000
        .byte   $9B
        inc     a:$A9,x
        sta     $C2
        txa
        asl     a
        rol     $C2
        sta     $C1
LFE7B:  ldx     $E6DC
        lda     $E750,x
        bpl     LFE87
        asl     $C1
        rol     $C2
LFE87:  ldy     #$00
        jsr     LFAC1
        jsr     LF9DB
        jsr     LFA50
        .byte   $4B
        .byte   $20
        .byte   $46
LFE95:  .byte   $52
        eor     $45
        ora     $6000
        ldx     #$00
        ldy     #$F8
LFE9F:  lda     ($E9),y
        bne     LFEA4
        inx
LFEA4:  dey
        bne     LFE9F
        rts

LFEA8:  brk
        .byte   $BF
        jsr     LF8A2
        jsr     LF9DB
        jsr     LFA50
        bit     a:$20
        ldx     #$06
        lda     LFEF2
        sta     LFEF1
        jsr     LFED5
        ldx     LFEF4
        stx     $BFE0
        lda     ($C7),y
        ldx     $E6D8
        stx     $BFE0
        jsr     LF8A2
        jmp     LF9DB

LFED5:  jsr     LF89B
        lda     #$3A
        sta     ($CD),y
        iny
        lda     LFEF1
        clc
        adc     #$30
        sta     ($CD),y
        iny
        jsr     LF9DB
        jsr     LFA50
        jsr     L203D
        brk
        rts

LFEF1:  brk
LFEF2:  brk
        .byte   $7F
LFEF4:  .byte   $7F
        inc     LFE7B,x
        dey
        inc     LFE95,x
        ldy     $FE,x
        cpx     #$FE
