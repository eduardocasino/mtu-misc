; da65 V2.18 - Ubuntu 2.19-1
; Created:    2025-06-11 10:53:02
; Input file: graphdriver2.bin
; Page:       1


            .setcpu "6502"

            .include "symbols.inc"
            .include "monomeg.inc"

            .segment "graphdrvjmp"

            ; Loadable file data
            ;
            .byte   $58             ; CODOS loadable file header byte
            .byte   $00             ; Memory overlay
            .byte   $00             ; Memory bank
            .byte   $00             ; Reserved
            .addr   SMOVE           ; Entry point
            .addr   JMPTBL          ; Load address
            .word   JMPTBL_SIZE     ; Memory image size

JMPTBL:     jmp     _SDRAW
            jmp     _SMOVE
            jmp     _SDRAWR
            jmp     _SMOVER
            jmp     _SVEC
            jmp     _SVECR
            jmp     _SDOT
            jmp     _SDOTR
            jmp     _SGRIN
            jmp     _SLTPEN
            jmp     ERR37
            jmp     _SDRWCH
            jmp     _SISDOT
            jmp     _SOFFGC
            jmp     _SONGC
            jmp     _SINTLP
            jmp     _STSTLP

JMPTBL_SIZE = * - JMPTBL

            .segment "graphdriver"

            ; Loadable file data
            ;
            .byte   $58             ; CODOS loadable file header byte
            .byte   $00             ; Memory overlay
            .byte   $00             ; Memory bank
            .byte   $00             ; Reserved
            .addr   SMOVE           ; Entry point
            .addr   _SMOVER         ; Load address
            .word   DRIVER_SIZE     ; Memory image size

_SMOVER:    pha
            jsr     LC01F
            pla
_SMOVE:     pha
            txa
            pha
            jsr     LC32D
            pla
            tax
            pla
            rts

_SDRAWR:    pha
            jsr     LC01F
            pla
_SDRAW:     sta     $02C7
            lda     #$80
            sta     $020A
            jmp     LC063

LC01F:      cld
            lda     #$00
            sta     $0207
            sta     $0209
            txa
            bpl     LC031
            lda     #$FF
            sta     $0207
            txa
LC031:      clc
            adc     $0202
            sta     $0206
            lda     $0203
            adc     $0207
            sta     $0207
            tya
            bpl     LC04A
            lda     #$FF
            sta     $0209
            tya
LC04A:      clc
            adc     $0204
            sta     $0208
            lda     $0205
            adc     $0209
            sta     $0209
            rts

_SVECR:     pha
            jsr     LC01F
            pla
_SVEC:      sta     $02C7
LC063:      stx     $02C8
            sty     $02C9
            bit     $020A
            bmi     LC079
            bvs     LC079
            jsr     LC326
            jsr     LC32D
            jmp     LC287

LC079:      jsr     LC2E2
            jsr     LC326
            ldx     #$03
            stx     $02D1
            lda     $020A
            asl     a
            asl     a
            sta     $02D2
LC08C:      lda     $0206
            sec
            sbc     $0202
            sta     $F6
            lda     $0207
            sbc     $0203
            bcs     LC0B2
LC09D:      ldy     $0206,x
            lda     $0202,x
            sta     $0206,x
            tya
            sta     $0202,x
            dex
            bpl     LC09D
            stx     $02D1
            bmi     LC08C
LC0B2:      sta     $F7
            jsr     LC2BE
            jsr     SWTBANK1
            jsr     LC1AC
            ldx     #$80
            lda     $0208
            sec
            sbc     $0204
            bcs     LC0D1
            ldx     #$00
            lda     $0204
            sec
            sbc     $0208
LC0D1:      stx     $FB
            bne     LC0D8
            jmp     LC224

LC0D8:      sta     $FA
            lda     $F7
            bne     LC0E6
            lda     $F6
            tax
            bne     LC13E
            jmp     LC209

LC0E6:      lda     $F6
            tax
            sec
            sbc     $FA
            sta     $F2
            lda     $F7
            sbc     #$00
            sta     $F3
            lda     $F7
            lsr     a
            sta     $F9
            txa
            ror     a
            sta     $F8
LC0FD:      lda     $F9
            bne     LC112
            lda     $F8
            sec
            sbc     $FA
            bcc     LC11F
            beq     LC11E
LC10A:      sta     $F8
            jsr     LC1DE
            jmp     LC12E

LC112:      lda     $F8
            sec
            sbc     $FA
            bcs     LC10A
            dec     $F9
            jmp     LC10A

LC11E:      clc
LC11F:      lda     $F8
            adc     $F2
            sta     $F8
            lda     $F9
            adc     $F3
            sta     $F9
            jsr     LC17E
LC12E:      cpx     #$00
            bne     LC134
            dec     $F7
LC134:      dex
            bne     LC0FD
            dec     $F7
            bpl     LC0FD
            jmp     LC269

LC13E:      sec
            sbc     $FA
            bcs     LC154
            lda     $FA
            stx     $FA
            sta     $F6
            tax
            lda     $FB
            ora     #$40
            sta     $FB
            txa
            sec
            sbc     $FA
LC154:      sta     $F2
            txa
            lsr     a
            sta     $F8
LC15A:      lda     $F8
            sec
            sbc     $FA
            bcc     LC16F
            beq     LC16E
            sta     $F8
            jsr     LC1DE
            dex
            bne     LC15A
            jmp     LC269

LC16E:      clc
LC16F:      lda     $F8
            adc     $F2
            sta     $F8
            jsr     LC17E
            dex
            bne     LC15A
            jmp     LC269

LC17E:      bit     $FB
            bmi     LC18F
            lda     $F4
            clc
            adc     #$3C
            sta     $F4
            bcc     LC19A
            inc     $F5
            bcs     LC19A
LC18F:      lda     $F4
            sec
            sbc     #$3C
            sta     $F4
            bcs     LC19A
            dec     $F5
LC19A:      inc     $FC
            ldy     $FC
            cpy     #$08
            bcc     LC1AC
            ldy     #$00
            sty     $FC
            inc     $F4
            bne     LC1AC
            inc     $F5
LC1AC:      bit     $02D2
            bpl     LC1C1
            asl     $020C
            rol     $020B
            bcc     LC1DD
            lda     #$01
            ora     $020C
            sta     $020C
LC1C1:      lda     LC201,y
            ldy     #$00
            bit     $020A
            bvs     LC1D0
            ora     ($F4),y
            sta     ($F4),y
            rts

LC1D0:      bmi     LC1D9
            eor     #$FF
            and     ($F4),y
            sta     ($F4),y
            rts

LC1D9:      eor     ($F4),y
            sta     ($F4),y
LC1DD:      rts

LC1DE:      lda     $FB
            asl     a
            bpl     LC19A
            bcc     LC1F3
LC1E5:      ldy     $FC
            lda     $F4
            sbc     #$3C
            sta     $F4
            bcs     LC1AC
            dec     $F5
            bcc     LC1AC
LC1F3:      ldy     $FC
            lda     $F4
            adc     #$3C
            sta     $F4
            bcc     LC1AC
            inc     $F5
            bcs     LC1AC           ; Always jump
            ; Not reached

LC201:      .byte   $80, $40, $20, $10, $08, $04, $02, $01

LC209:      ldx     $FA
            beq     LC269
            bit     $FB
            bpl     LC21B
LC211:      sec
            jsr     LC1E5
            dex
            bne     LC211
            jmp     LC269

LC21B:      clc
            jsr     LC1F3
            dex
            bne     LC21B
            beq     LC269
LC224:      ldx     $F6
            beq     LC234
LC228:      ldy     $FC
            cpy     #$07
            beq     LC23B
LC22E:      jsr     LC19A
            dex
            bne     LC228
LC234:      dec     $F7
            bpl     LC228
            jmp     LC269

LC23B:      cpx     #$08
            bcc     LC22E
            bit     $02D2
            bmi     LC22E
            inc     $F4
            bne     LC24A
            inc     $F5
LC24A:      ldy     #$00
            lda     #$FF
            bit     $020A
            bvs     LC25E
LC253:      sta     ($F4),y
            txa
            sec
            sbc     #$08
            tax
            bne     LC23B
            beq     LC234
LC25E:      bmi     LC264
            lda     #$00
            beq     LC253
LC264:      eor     ($F4),y
            jmp     LC253

LC269:      jsr     RSTBANK0
            ldx     #$03
            bit     $02D1
            bmi     LC27E
LC273:      lda     $0206,x
            sta     $0202,x
            dex
            bpl     LC273
            bmi     LC287
LC27E:      lda     $0202,x
            sta     $0206,x
            dex
            bpl     LC27E
LC287:      ldy     $02C9
            lda     $02C7
            ldx     $02C8
            rts

_SDOTR:     pha
            jsr     LC01F
            pla
_SDOT:      sta     $02C7
            stx     $02C8
            sty     $02C9
            jsr     LC326
            jsr     LC32D
            bit     $020A
            bmi     LC2AC
            bvc     LC2BB
LC2AC:      lsr     $02D2
            jsr     LC2BE
            jsr     SWTBANK1
            jsr     LC1AC
            jsr     RSTBANK0
LC2BB:      jmp     LC287

LC2BE:      lda     $0204
            jsr     NTLINSTART
LC2C4:      lda     $0203
            lsr     a
            lda     $0202
            ror     a
            lsr     a
            lsr     a
            clc
            adc     $F2
            sta     $F4
            lda     $F3
            adc     #$00
            sta     $F5
            lda     $0202
            and     #$07
            sta     $FC
            tay
            rts

LC2E2:      stx     $02CF
            ldx     #$00
LC2E7:      cld
            lda     $0202,x
            cmp     #$E0
            lda     $0203,x
            bmi     LC302
            sbc     #$01
            bcc     LC30A
            lda     #$DF
            sta     $0202,x
            lda     #$01
            sta     $0203,x
            bne     LC30A
LC302:      lda     #$00
            sta     $0202,x
            sta     $0203,x
LC30A:      lda     $0205,x
            beq     LC322
            bmi     LC318
            lda     #$FF
            sta     $0204,x
            bne     LC31D
LC318:      lda     #$00
            sta     $0204,x
LC31D:      lda     #$00
            sta     $0205,x
LC322:      ldx     $02CF
            rts

LC326:      stx     $02CF
            ldx     #$04
            bne     LC2E7
LC32D:      ldx     #$03
LC32F:      lda     $0206,x
            sta     $0202,x
            dex
            bpl     LC32F
            rts

_SDRWCH:    sta     $02C7
            stx     $02C8
            sty     $02C9
            pha
            jsr     LC32D
            pla
            jsr     LC35B
            lda     $0206
            clc
            adc     #$06
            sta     $0206
            bcc     LC358
            inc     $0207
LC358:      jmp     LC287

LC35B:      cld
            cmp     #$80
            bcs     LC386
            sec
            sbc     #$20
            bcc     LC386
            sta     $02CC
            lda     $0202
            cmp     #$DB
            lda     $0203
            sbc     #$01
            bcs     LC386
            lda     $0205
            bne     LC386
            cmp     #$F7
            bcs     LC386
            jsr     LC2BE
            lda     $02CC
            jsr     PRNCHR
LC386:      rts

_SISDOT:    stx     $02C8
            sty     $02C9
            jsr     LC32D
            jsr     LC2E2
            jsr     LC2BE
            jsr     SWTBANK1
            lda     LC201,y
            ldy     #$00
            and     ($F4),y
            tax
            jsr     RSTBANK0
            txa
            pha
            ldx     $02C8
            ldy     $02C9
            pla
            rts

_SGRIN:     txa
            pha
            tya
            pha
            sec
            ror     $0213
            ldx     #$03
LC3B8:      lda     $0202,x
            sta     $0206,x
            dex
            bpl     LC3B8
            bmi     LC3C8
LC3C3:      lda     #$00
            sta     $020D
LC3C8:      jsr     _SONGC
            jsr     TSTKEY
            php
            jsr     _SOFFGC
            plp
            bcc     LC3C8
            ldy     #$01
            ldx     #$00
            cmp     #$A0
            bcc     LC421
            cmp     #$A4
            bcc     LC3EC
            cmp     #$B0
            bcc     LC421
            cmp     #$B4
            bcs     LC421
            ldy     $0223
LC3EC:      sty     $FA
            and     #$03
            beq     LC419
            cmp     #$02
            beq     LC409
            bcs     LC41D
LC3F8:      sec
            lda     $0202,x
            sbc     $FA
            sta     $0202,x
            bcs     LC3C3
            dec     $0203,x
            jmp     LC3C3

LC409:      tya
            clc
            adc     $0202,x
            sta     $0202,x
            bcc     LC3C3
            inc     $0203,x
            jmp     LC3C3

LC419:      ldx     #$02
            bne     LC409
LC41D:      ldx     #$02
            bne     LC3F8
LC421:      sta     $FA
            ldx     #$03
LC425:      lda     $0206,x
            tay
            lda     $0202,x
            sta     $0206,x
            tya
            sta     $0202,x
            dex
            bpl     LC425
            asl     $0213
            jsr     CLICK
            pla
            tay
            pla
            tax
            lda     $FA
            rts

_SOFFGC:    asl     $021C
            bcs     LC44D
            rts

_SONGC:     sec
            ror     $021C
LC44D:      pha
            txa
            pha
            tya
            pha
            jsr     LC2E2
            lda     $0204
            jsr     NTLINSTART
            jsr     SWTBANK1
            lda     $BFFC
            and     #$F1
            ora     #$04
            sta     $BFFC
            lda     $BFF1
LC46B:      lda     $BFFD
            and     #$01
            beq     LC46B
            ldy     #$3B
LC474:      lda     ($F2),y
            eor     #$FF
            sta     ($F2),y
            dey
            bpl     LC474
            lda     #$C4
            sta     $F2
            lda     #$FB
            sta     $F3
            jsr     LC2C4
            lda     LC201,y
            sta     $FA
            ldx     #$00
            ldy     #$00
LC491:      lda     ($F4),y
            eor     $FA
            sta     ($F4),y
            lda     $F4
            sec
            sbc     #$3C
            sta     $F4
            bcs     LC4A2
            dec     $F5
LC4A2:      inx
            bne     LC491
            jsr     RSTBANK0
            pla
            tay
            pla
            tax
            pla
            rts

_SINTLP:    jsr     _TIOON
            lda     $BFFC
            and     #$F1
            sta     $BFFC
            lda     $BFF1
LC4BC:      lda     $BFFD
            and     #$01
            beq     LC4BC
            sta     $BFC4
            jmp      _IORES

_SLTPEN:    jsr     _SINTLP
            jsr     _TIOON
            lda     $BFFC
            ora     #$04
            sta     $BFFC
            lda     $BFF1
LC4DA:      jsr     _STSTLP
            bcs     LC4E7
            lda     $BFFD
            and     #$01
            beq     LC4DA
            clc
LC4E7:      jmp      _IORES

_STSTLP:    jsr     _TIOON
            lda     $BFC0
            and     #$08
            bne     LC4F6
            clc
            rts

LC4F6:      lda     $BFC0
            and     #$07
            sta     $0209
            lda     $BFC1
            sec
            sbc     #$02
            sta     $0208
            lda     $BFC2
            and     #$3F
            sbc     #$00
            ldy     #$08
            asl     $0208
LC513:      rol     a
            cmp     #$3C
            bcc     LC51A
            sbc     #$3C
LC51A:      rol     $0208
            dey
            bne     LC513
            sta     $0206
LC523:      cmp     #$0A
            bcc     LC52B
            sbc     #$0A
            bne     LC523
LC52B:      asl     a
            tax
            bit     $BFC2
            bpl     LC533
            inx
LC533:      ldy     $0209
            lda     LC578,x
            and     LC5A0,y
            beq     LC541
            inc     $0206
LC541:      lda     LC58C,x
            and     LC5A0,y
            beq     LC54C
            dec     $0206
LC54C:      lda     #$00
            sta     $0207
            lda     $0206
            asl     a
            asl     a
            asl     a
            rol     $0207
            ora     $0209
            sbc     #$05
            sta     $0206
            bcs     LC567
            dec     $0207
LC567:      lda     $0208
            eor     #$FF
            sta     $0208
            lda     #$00
            sta     $0209
            sec
            jmp      _IORES

LC578:      .byte   $00, $00, $00, $03, $0F, $00, $00, $3F
            .byte   $00, $00, $00, $00, $0F, $00, $00, $0F
            .byte   $FF, $0F, $00, $00
LC58C:      .byte   $C0, $00, $00, $00, $00, $00, $00, $00
            .byte   $00, $00, $00, $C0, $00, $00, $00, $00
            .byte   $00, $00, $C0, $F1
            
LC5A0:      .byte   $01, $02

.if CODOS2_VER <> 14
            .byte   $04, $08, $10, $20, $40, $80
.endif

DRIVER_SIZE = * - _SMOVER
