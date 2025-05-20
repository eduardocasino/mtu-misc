; da65 V2.18 - Ubuntu 2.19-1
; Created:    2025-05-19 14:33:25
; Input file: format.bin
; Page:       1


            .setcpu "6502"

            .include "symbols.inc"

            .include "codos.inc"

L0024           := $0024
L0D59           := $0D59
L204C           := $204C
L2E45           := $2E45
L4142           := $4142
L414D           := $414D
L4328           := $4328
L4542           := $4542
L4544           := $4544
L454E           := $454E
L4552           := $4552
L4553           := $4553
L4554           := $4554
L4944           := $4944
L4957           := $4957
L4D55           := $4D55
L4E20           := $4E20
L4E49           := $4E49
L4E4F           := $4E4F
L4E55           := $4E55
L4F43           := $4F43
L4F46           := $4F46
L4F4E           := $4F4E
L4F53           := $4F53
L4F54           := $4F54
L4F56           := $4F56
L504F           := $504F
L5244           := $5244
L5254           := $5254
L5349           := $5349
L5928           := $5928
L5942           := $5942
L5953           := $5953
LBD1D           := $BD1D
LEA18           := $EA18
LEC45           := $EC45
LEC94           := $EC94
LECC4           := $ECC4
LED0B           := $ED0B
LED0E           := $ED0E
LEE85           := $EE85
LEEA5           := $EEA5

            .code

START:      ldx     #$00            ; Unprotects SYSRAM
            stx     HSRCW           ;
            cld                     ;
            ldy     CMDLIDX         ; Get command line index
            jsr     LB41D
            bcc     LB410
            jsr     ERROR04         ; Syntax error in command argument
            ; Not reached

LB410:      lda     NDRIVES         ; Get number of drives in the system
            cmp     #$02            ; Is it 2 or more
            bcs     LB41A           ; Yes, operate with 2 drives
            jmp     LB464           ; No, operate with one drive

LB41A:      jmp     LB53D           ; Jump to 2 drives operation


LB41D:      lda     #$08
            sta     LBCBA
            lda     #$30
            sta     LBCBB
LB427:      jsr     GETNEXTNB       ; Get next non-blank from command line
            beq     RETOK           ; No more, return OK
            ldx     #$FF            ; Init index to arguments table
            cmp     #'S'            ; Interleave?
            beq     LB437           ; Yes, go get interleave factor from command line
            cmp     #'T'            ; Skew?
            bne     RETERR          ; No, return with error
            inx                     ; increment index to arg table ( 1 for T ) 
LB437:      inx                     ; Increment index to arg table ( 0 f0r S )
            jsr     GETNEXTNB1      
            beq     LB457
            cmp     #$3D
            bne     LB457
            jsr     GETNEXTNB1
            jsr     GETBYTE
            bcc     RETERR
            cmp     #$19
            bcs     RETERR
LB44D:      asl     a
            asl     a
            beq     RETERR
            sta     LBCBA,x
            jmp     LB427

LB457:      cpx     #$00
            bne     RETERR
            lda     #$03
            jmp     LB44D

RETOK:      clc
            rts

RETERR:      sec
            rts

LB464:  bit     ODRIVES
        bpl     LB46E
        ldx     #$00
        jsr     CLOSEDRV
LB46E:  jsr     OUTSTR
        .byte   $0D, "PUT IN DISK TO BE FORMATTED.", $00
        jsr     LB90C
        jsr     OUTSTR
        .byte   ".", $00
        jsr     LB8D1
        bcs     LB49D
LB49C:  rts

LB49D:  ldx     #$00
        stx     CURRDRV
        jsr     LB619
        ldx     #$00
        jsr     OPENDRV
        ldx     #$00
        stx     CURRDRV
        ldx     #$0C
        jsr     LBA6E
        bcs     LB49C
        jsr     LB8E7
        bcc     LB4CF
        ldx     #$00
        jsr     LB9B2
        ldx     #$00
        stx     CURRDRV
LB4C5:  jsr     LBA6E
        bcs     LB49C
        inx
        cpx     #$4D
        bcc     LB4C5
LB4CF:  ldx     #$00
        jsr     LB9D5
        jsr     LB974
        ldx     #$00
        jsr     LB9C1
        ldy     #$28
        lda     #$FE
        sta     (BATP),y
        iny
        bit     DRVNFO
        bmi     LB4EA
        sta     (BATP),y
LB4EA:  jsr     LEEA5
        ldx     #$00
        jsr     CLOSEDRV
        jsr     OUTSTR
        .byte   $0D, "TO COPY SYSTEM,", $00
        jsr     LB704
        ldx     #$00
        stx     LBCA1
LB50E:  ldx     LBCA1
        lda     LBCBC,x
        beq     LB522
        jsr     LB858
        stx     LBCA1
        jsr     LB7AB
        jmp     LB50E

LB522:  jsr     OUTSTR
        .byte   $0D, "NEW DISK IS NOW OPEN.", $00
        rts

LB53D:  bit     ODRIVES+1
        bpl     LB547
        ldx     #$01
        jsr     CLOSEDRV
LB547:  jsr     LB90C
        jsr     OUTSTR
        .byte   "1.", $00
        jsr     LB8D1
        bcs     LB556
LB555:  rts

LB556:  ldx     #$01
        stx     CURRDRV
        jsr     LB619
        ldx     #$01
        jsr     OPENDRV
        ldx     #$0C
        jsr     LBA6E
        bcs     LB555
        ldx     #$01
        jsr     LB9B2
        jsr     LB8E7
        bcc     LB585
        ldx     #$01
        stx     CURRDRV
        ldx     #$00
LB57B:  jsr     LBA6E
        bcs     LB555
        inx
        cpx     #$4D
        bcc     LB57B
LB585:  ldx     #$01
        jsr     LB9D5
        jsr     LB974
        ldx     #$01
        jsr     LB9C1
        jsr     LEEA5
        jsr     OUTSTR
        .byte   $0D, "WANT TO COPY DRIVE 0 SYSTEM", $00
        jsr     LB955
        sta     LBCAF
        bcs     LB5C1
        cmp     #$53
        bne     LB616
LB5C1:  ldx     #$01
        stx     CURRDRV
        jsr     SETBATP
        ldy     #$28
        lda     #$FE
        .byte   $91
LB5CE:  sbc     #$C8
        sta     (BATP),y
        iny
        bit     DRVNFO+1
        bmi     LB5DA
        sta     (BATP),y
LB5DA:  jsr     LEEA5
        lda     LBCAF
        cmp     #$53
        beq     LB616
        lda     #$01
        sta     LBCA2
LB5E9:  lda     LBCA2
        cmp     NUMOVL
        bcs     LB5FD
        jsr     OVERLAY
        jsr     LB9F2
        inc     LBCA2
        jmp     LB5E9

LB5FD:  .byte   $A2
LB5FE:  brk
        stx     LBCA1
LB602:  ldx     LBCA1
        lda     LBCBC,x
        beq     LB616
        jsr     LB858
        stx     LBCA1
        jsr     LB75D
        jmp     LB602

LB616:  jmp     LB522

LB619:  cld
        lda     #$00
        sta     HSRCW
        sta     LBC9E
        sta     $0284
        ldx     CURRDRV
        stx     $E740
        jsr     LECC4
        lda     #$02
        sta     LBCA0
LB633:  ldx     CURRDRV
        lda     LBC9E
        jsr     LED0E
LB63C:  ldx     #$67
LB63E:  lda     #$01
        sta     DIRBUF,x
        dex
        lda     #$FF
        sta     DIRBUF,x
        dex
        lda     $0284
        sta     DIRBUF,x
        dex
        lda     LBC9E
        sta     DIRBUF,x
        dex
        bpl     LB63E
        ldy     #$00
        lda     LBCA0
LB65F:  tax
LB660:  lda     DIRBUF,x
        cmp     #$FF
        beq     LB672
        inx
        inx
        inx
        inx
        txa
        cmp     #$68
        bcc     LB660
        bcs     LB684
LB672:  tya
        sta     DIRBUF,x
        iny
        cpy     #$1A
        beq     LB689
        txa
        clc
        adc     LBCBA
        cmp     #$68
        bcc     LB65F
LB684:  sbc     #$68
        jmp     LB65F

LB689:  lda     #$94
        stx     LBCA0
        sta     $FFEA
        lda     $0284
        beq     LB69E
        lda     #$04
        ora     $E740
        sta     $E740
LB69E:  ldx     #$17
        jsr     LEC45
LB6A3:  lda     HSRCW
        bmi     LB6A3
        jsr     LEC94
        lda     $E748
        and     #$D8
        beq     LB6B5
        jsr     ERROR32
LB6B5:  ldx     #$00
LB6B7:  dex
        bne     LB6B7
        lda     $E740
        and     #$03
        sta     $E740
        tax
        lda     DRVNFO,x
        bpl     LB6E6
        lda     $0284
        bne     LB6E3
        inc     $0284
        lda     LBCA0
        clc
        adc     LBCBA
        cmp     #$68
        bcc     LB6DD
        sbc     #$68
LB6DD:  sta     LBCA0
        jmp     LB63C

LB6E3:  dec     $0284
LB6E6:  lda     LBCA0
        clc
        adc     LBCBB
        cmp     #$68
        bcc     LB6F3
        sbc     #$68
LB6F3:  sta     LBCA0
        inc     LBC9E
        lda     LBC9E
        cmp     #$4D
        beq     LB703
        jmp     LB633

LB703:  rts

LB704:  jsr     LB86E
        lda     $E7C2
        sta     DESTBUFF
        lda     $E7C3
        sta     $CA
        lda     #$01
        sta     LBCA2
LB716:  lda     LBCA2
        cmp     $E794
        bcs     LB733
        jsr     OVERLAY
        ldy     #$00
LB723:  lda     OVLORG,y
        sta     (DESTBUFF),y
        iny
        bne     LB723
        inc     $CA
        inc     LBCA2
        jmp     LB716

LB733:  jsr     LB8A1
        lda     $E7C2
        sta     DESTBUFF
        lda     $E7C3
        sta     $CA
LB740:  ldy     #$00
LB742:  lda     (DESTBUFF),y
        sta     OVLORG,y
        iny
        bne     LB742
        jsr     LBA30
        ldx     OVLORG
        inx
        txa
        cmp     $E794
        bcs     LB75C
        inc     $CA
        jmp     LB740

LB75C:  rts

LB75D:  ldx     #$00
        stx     CURRDRV
        jsr     FOPEN0
        ldx     #$01
        stx     CURRDRV
        ldx     #$09
LB76C:  lda     IOCHTBL,x
        beq     LB777
        dex
        bne     LB76C
        jsr     ERROR34
LB777:  stx     LBCA3
        jsr     ASSIGN
        ldx     #$03
LB77F:  lda     $E7C2,x
        sta     MEMBUFF,x
        dex
        bpl     LB77F
        ldx     #$00
        jsr     GETMBUFF
        bcs     LB7A1
        lda     $E7C2
        sta     MEMBUFF
        lda     $E7C3
        sta     $C4
        ldx     LBCA3
        jsr     OUTMBUFF
        jmp     LB77F

LB7A1:  ldx     LBCA3
        jsr     FREECH
        jsr     FREECH0
        rts

LB7AB:  lda     #$00
        ldx     #$05
LB7AF:  sta     LBCA4,x
        dex
        bpl     LB7AF
LB7B5:  ldx     #$00
        stx     LBCAA
        stx     CURRDRV
        jsr     LB86E
        jsr     FOPEN0
        ldx     #$02
LB7C5:  lda     LBCA4,x
        sta     FILEPOS,x
        dex
        bpl     LB7C5
        ldx     #$00
        jsr     FSEEK
        ldx     #$03
LB7D4:  lda     $E7C2,x
        sta     MEMBUFF,x
        dex
        bpl     LB7D4
        ldx     #$00
        jsr     GETMBUFF
        bcs     LB851
        lda     MEMCOUNT
        sta     LBCAB
        cmp     $E7C4
        lda     $C6
        sta     LBCAC
        sbc     $E7C5
        bcc     LB7FA
        lda     #$80
        sta     LBCAA
LB7FA:  lda     $E2
        sec
        sbc     #$40
        sta     LBCA4
        lda     $E3
        sbc     #$00
        sta     LBCA5
        lda     $E4
        sbc     #$00
        sta     LBCA6
        jsr     LB8A1
        jsr     ASSIGN0
        ldx     #$02
LB818:  lda     LBCA7,x
        sta     FILEPOS,x
        dex
        bpl     LB818
        ldx     #$00
        jsr     FSEEK
        lda     $E7C2
        sta     MEMBUFF
        lda     $E7C3
        sta     $C4
        lda     LBCAB
        sta     MEMCOUNT
        lda     LBCAC
        sta     $C6
        ldx     #$00
        jsr     OUTMBUFF
        ldx     #$02
LB840:  lda     LBCA4,x
        sta     LBCA7,x
        dex
        bpl     LB840
        bit     LBCAA
        bpl     LB854
        jmp     LB7B5

LB851:  jsr     LB8A1
LB854:  jsr     FREECH0
        rts

LB858:  ldy     #$00
LB85A:  lda     LBCBC,x
        sta     FNAMBUF,y
        inx
        iny
        cmp     #$2E
        bne     LB85A
        lda     LBCBC,x
        sta     FNAMBUF,y
        inx
        rts

LB86E:  bit     ODRIVES
        bpl     LB878
        ldx     #$00
        jsr     CLOSEDRV
LB878:  jsr     OUTSTR
        .byte   $0D, "PUT SOURCE DISK IN.", $00
        jsr     LB8D1
        bcs     LB898
        jmp     LB89E

LB898:  ldx     #$00
        jsr     OPENDRV
        rts

LB89E:  jmp     WARMST

LB8A1:  bit     ODRIVES
        bpl     LB8AB
        ldx     #$00
        jsr     CLOSEDRV
LB8AB:  jsr     OUTSTR
        .byte   $0D, "PUT NEW DEST. DISK IN.", $00
        jsr     LB8D1
        bcc     LB89E
        ldx     #$00
        jsr     OPENDRV
        rts

LB8D1:  jsr     OUTSTR
        .byte   $0D, "ARE YOU READY", $00
        jsr     LB955
        rts

LB8E7:  jsr     OUTSTR
        .byte   $0D, "WANT TO TEST FOR BAD SECTORS", $00
        jsr     LB955
        rts

LB90C:  jsr     OUTSTR
        .byte   $0D, "WARNING: FORMAT WILL IRREVOCABLY"
        .byte   $0D, "ERASE EVERYTHING ON DISK IN DRIVE ", $00
        rts

LB955:  jsr     OUTSTR
        .byte   " (Y/N)?= ", $00
        ldx     #$01
        jsr     GETLINE
        jsr     GETNEXTNB
        beq     LB972
        cmp     #$59
        beq     LB972
        clc
        rts

LB972:  sec
        rts

LB974:  jsr     OUTSTR
        .byte   $0D, "DISK VOLUME SERIAL NO. (VSN)?= ", $00
        ldx     #$01
        jsr     GETLINE
        jsr     GETNEXTNB
        ldx     #$02
        jsr     EVALEXP
        bcc     LB974
        lda     MEMBUFF
        sta     LBCAD
        lda     $C4
        sta     LBCAE
        rts

LB9B2:  stx     CURRDRV
        jsr     SETBATP
        ldy     #$00
        tya
LB9BB:  sta     (BATP),y
        iny
        bne     LB9BB
        rts

LB9C1:  stx     CURRDRV
        jsr     SETBATP
        ldy     #$FA
        lda     LBCAD
        sta     (BATP),y
        iny
        lda     LBCAE
        sta     (BATP),y
        rts

LB9D5:  stx     CURRDRV
        lda     #$00
        tax
LB9DB:  sta     DIRBUF,x
        inx
        bne     LB9DB
        lda     #$11
        sta     SECTNUM
LB9E6:  ldx     CURRDRV
        jsr     WRTRCK12
        dec     SECTNUM
        bne     LB9E6
        rts

LB9F2:  ldx     #$00
        stx     CHEAD
        lda     OVLORG
        ldx     #$0C
        cmp     #$09
        bcc     LBA0C
        inx
        bit     $E751
        bpl     LBA0C
        dex
        lda     #$01
        sta     CHEAD
LBA0C:  txa
        ldx     #$01
        jsr     LED0B
        ldx     #$F8
        stx     $E7
        ldx     #$01
        lda     OVLORG
        clc
        adc     #$11
        cmp     #$1A
        bcc     LBA2A
        bit     $E751
        bmi     LBA2A
        sec
        sbc     #$1A
LBA2A:  ldx     #$01
        jsr     WRITSECT
        rts

LBA30:  lda     #$00
        sta     CHEAD
        lda     OVLORG
        ldx     #$0C
        cmp     #$09
        bcc     LBA4A
        inx
        bit     DRVNFO
        bpl     LBA4A
        dex
        lda     #$01
        sta     CHEAD
LBA4A:  txa
        ldx     #$00
        jsr     LED0B
        ldx     #$F8
        stx     $E7
        ldx     #$00
        lda     OVLORG
        clc
        adc     #$11
        cmp     #$1A
        bcc     LBA68
        bit     DRVNFO
        bmi     LBA68
        sec
        sbc     #$1A
LBA68:  ldx     #$00
        jsr     WRITSECT
        rts

LBA6E:  stx     LBCB0
        cld
        ldx     CURRDRV
        lda     DRVNFO,x
        sta     $E772
        bpl     LBA81
        lda     #$34
        bne     LBA83
LBA81:  lda     #$1A
LBA83:  sta     $E7CA
        lda     #$FE
        sta     ERRRCVRYP
        lda     #$BA
        sta     $E7C9
        lda     LBCB4
        sta     LBCB6
        lda     LBCB5
        sta     LBCB7
        lda     #$00
        sta     LBCB1
LBAA1:  jsr     LBC4F
        lda     LBCB1
        adc     #$03
        sta     LBCB1
        sec
        sbc     $E7CA
        beq     LBABA
        bcc     LBAA1
        sta     LBCB1
        jmp     LBAA1

LBABA:  lda     LBCB6
        sta     LBCB4
        lda     LBCB7
        sta     LBCB5
        lda     #$00
        sta     LBCB1
LBACB:  jsr     LBC13
        bcc     LBAD8
        jsr     LBB3A
        bcc     LBAD8
        sec
        bcs     LBAF0
LBAD8:  lda     LBCB1
        clc
        adc     #$03
        sta     LBCB1
        sec
        sbc     $E7CA
        beq     LBAEF
        bcc     LBACB
        sta     LBCB1
        jmp     LBACB

LBAEF:  clc
LBAF0:  ldx     LBCB0
        lda     #$18
        sta     ERRRCVRYP
        lda     #$EA
        sta     $E7C9
        rts

        sta     LBCB8
        lda     ERRNUM
        cmp     #$28
        beq     LBB1B
        cmp     #$1E
        beq     LBB1B
        lda     #$18
        sta     ERRRCVRYP
        lda     #$EA
        sta     $E7C9
        lda     LBCB8
        jmp     LEA18

LBB1B:  sty     LBCB9
        lda     #$00
        sta     ERRNUM
        jsr     LBB3A
        php
        jsr     OUTSTR
        .byte   " (CRC)", $00
        ldy     LBCB9
        plp
        bcs     LBB37
        rts

LBB37:  jmp     WARMST

LBB3A:  jsr     OUTSTR
        .byte   $0D, "BAD TRACK $", $00
        ldy     #$00
        lda     LBCB0
        jsr     HEXBYTE
        jsr     POUTBUFF02
        jsr     OUTSTR
        .byte   " SECTOR $", $00
        lda     LBCB1
        ldy     #$00
        jsr     HEXBYTE
        jsr     POUTBUFF02
        jsr     LBBA5
        bcs     LBB8A
        jsr     SETBATP
        lda     #$FF
        jsr     LEE85
        jsr     OUTSTR
        .byte   " BYPASSED.", $00
        clc
        rts

LBB8A:  jsr     OUTSTR
        .byte   " MAKES DISK UNUSABLE.", $00
        sec
        rts

LBBA5:  lda     #$00
        sta     LBCB2
        sta     LBCB3
        ldx     LBCB0
        inx
LBBB1:  dex
        beq     LBBC6
        lda     $E7CA
        clc
        adc     LBCB2
        sta     LBCB2
        bcc     LBBB1
        inc     LBCB3
LBBC3:  jmp     LBBB1

LBBC6:  lda     LBCB2
        clc
        adc     LBCB1
        sta     LBCB2
        bcc     LBBD5
        inc     LBCB3
LBBD5:  lda     LBCB0
        cmp     #$0C
        beq     LBC11
        bcc     LBBEE
        lda     LBCB2
        sbc     #$12
        sta     LBCB2
        lda     LBCB3
        sbc     #$00
        sta     LBCB3
LBBEE:  ldx     #$03
        bit     $E772
        bpl     LBBF7
        ldx     #$04
LBBF7:  lsr     LBCB3
        ror     LBCB2
        dex
        bne     LBBF7
        inc     LBCB2
        ldy     LBCB2
        bit     $E772
        bmi     LBC0F
        cpy     #$29
        beq     LBC11
LBC0F:  clc
        rts

LBC11:  sec
        rts

LBC13:  lda     #$00
        sta     CHEAD
        lda     LBCB1
        cmp     #$1A
        bcc     LBC22
        inc     CHEAD
LBC22:  ldx     CURRDRV
        lda     LBCB0
        jsr     LED0B
        lda     #$94
        sta     $E7
        ldx     CURRDRV
        lda     LBCB1
        jsr     READSECT
        ldx     #$00
LBC3A:  jsr     LBC80
        cmp     DIRBUF,x
        bne     LBC4A
        inx
        bne     LBC3A
        clc
        rts

LBC47:  jsr     LBC80
LBC4A:  inx
        bne     LBC47
        sec
        rts

LBC4F:  ldx     #$00
LBC51:  jsr     LBC80
        sta     DIRBUF,x
        inx
        bne     LBC51
        lda     #$00
        sta     CHEAD
        lda     LBCB1
        cmp     #$1A
        bcc     LBC69
        inc     CHEAD
LBC69:  ldx     CURRDRV
        lda     LBCB0
        jsr     LED0B
        lda     #$94
        sta     $E7
        ldx     CURRDRV
        lda     LBCB1
        jsr     WRITSECT
        rts

LBC80:  lda     LBCB4
        lsr     a
        eor     LBCB4
        lsr     a
        lsr     a
        eor     LBCB4
        lsr     a
        eor     LBCB5
        lsr     a
        lsr     a
        lsr     a
        lsr     a
        rol     LBCB5
        rol     LBCB4
        lda     LBCB4
        rts

        .bss

LBC9E:  .res    1
        .res    1
LBCA0:  .res    1
LBCA1:  .res    1
LBCA2:  .res    1
LBCA3:  .res    1
LBCA4:  .res    1
LBCA5:  .res    1
LBCA6:  .res    1
LBCA7:  .res    3

LBCAA:  .res    1
LBCAB:  .res    1
LBCAC:  .res    1
LBCAD:  .res    1
LBCAE:  .res    1
LBCAF:  .res    1
LBCB0:  .res    1
LBCB1:  .res    1
LBCB2:  .res    1
LBCB3:  .res    1
LBCB4:  .res    1
LBCB5:  .res    1
LBCB6:  .res    1
LBCB7:  .res    1
LBCB8:  .res    1
LBCB9:  .res    1

        .data

LBCBA:  .byte   $08            
LBCBB:  .byte   $30       
LBCBC:  .byte   "CODOS.Z"
        .byte   "COMDPROC.Z"
        .byte   "SYSERRMSG.Z"
        .byte   "SVCPROC.Z"
        .byte   "STARTUP.J"
        .byte   "DIR.C"
        .byte   "IODRIVER.Z"
.if CODOS2_VER <> 14
        .byte   "GRAPHDRIVER.Z"
        .byte   "PRINTDRIVER.Z"
.endif
        .byte   $00
