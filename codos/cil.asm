; da65 V2.18 - Ubuntu 2.19-1
; Created:    2025-06-20 09:54:14
; Input file: cilb.bin
; Page:       1


            .setcpu "6502"

            .include "macros.inc"
            .include "symbols.inc"
            .include "codos.inc"
            .include "basic.inc"

            .scope  cilb

            .segment "HEADER"

            ; Loadable file data
            ;
            .byte   $58             ; CODOS loadable file header byte
            .byte   $00             ; Memory overlay
            .byte   $00             ; Memory bank
            .byte   $00             ; Reserved
            .addr   INIT            ; $977E Entry point
            .addr   LNKINFO         ; Load address
            .word   SIZE            ; Memory image size

            .code

LNKINFO:    .byte   $80+'L'
            .byte   $D0
            .word   TOKEN_NAME_TABLE
            .word   TOKEN_ADDRESS_TABLE
            .word   FUNHND
            .byte   TOKEN_POS
            .word   LIBNAM

LIBNAM:
.ifdef cilb     
            .byte   "CILB.Z", $00
.else
            .byte   "CIL.Z", $00
.endif
            init_token_tables

            keyword_rts "PRINT@", PRINT
            keyword_rts "INPUT@", INPUT
            keyword_rts "BLKRD@", BLKRD
            keyword_rts "BLKWRT@", BLKWRT
            keyword_rts "GET@", GET
            keyword_rts "PUT@", PUT
            keyword_rts "SYSTEM", SYSTEM
            keyword_rts "TRUNC@", TRUNC
            keyword_rts "POSITION@", POSITION
            keyword_rts "LIST@", LIST
            keyword_rts "ENTER@", ENTER
            keyword_rts "APPEND", APPEND
            keyword_rts "ONERR", ONERR
            
            keyword "POSN", TOKEN_POS

            .segment "KEYWORDS"
            
            .byte   $00, $00

            .segment "VECTORS"

INIT:       lda     #$00
            sta     STATUS
            sta     L9FC0
            sta     L0780
            rts

FUNHND:     cmp     #TOKEN_POS
            bne     L9793
            jsr     CHRGET
            jmp     L9CCC
L9793:      jmp     L97EA

ERROR:      lda     ERRNUM
            sta     L9FBB
            cld
            lda     #$00
            sta     PERRPFLG
            sta     ERRNUM
            jsr     _PROTECT
            lsr     Z14
            jsr     CLEAR2
            jsr     STXTPT
            lda     L9FBC
            sta     LINNUM
            lda     L9FBC+1
            sta     LINNUM+1
            jsr     GOTO2
            jmp     NEWSTT

L97BE:      stx     L9FBF
            jsr     _UNPROTECT
            ldy     #$00
            jsr     _SETERRRCVRY
            jsr     _PROTECT
            lda     #<ERRTBL
            ldy     #>ERRTBL
            ldx     L9FBF
            jmp     ERREXIT

L97D6:      ldx     #$39
            bne     L97BE
L97DA:      ldx     #VALTYP
            bne     L97BE
L97DE:      ldx     #$2B
            bne     L97BE
L97E2:      ldx     #INDEX+1
            bne     L97BE
L97E6:      ldx     #$00
            beq     L97BE
L97EA:      ldx     #$47
            bne     L97BE

            init_error_table

ERRTBL:     define_error ERR_OVERLAP, "LINE # OVERLAP"
            define_error ERR_TOOLONG, "ARGUMENT TOO LONG"
            define_error ERR_OVERLAY, "OVERLAY LOAD"
            define_error ERR_TOOLARGE, "FILE TOO LARGE"
            define_error ERR_BLOKARG, "BLOCK ARGUMENT"
            define_error ERR_ILLEGAL, "ILLEGAL CIL TOKEN"


L9846:      jsr     _PROTECT
            jmp     SYNERR

L984C:      jsr     _PROTECT
            jmp     IQLERR

L9852:      jsr     L9860
            jsr     L9928
            bcc     L985D
            jmp     L9846

L985D:      jmp     CHRGET

L9860:      jsr     L990A
            cpx     #$00
            beq     L986A
L9867:      jsr     ERROR08
L986A:      cmp     #$0A
            bcs     L9867
            sta     L9FB8
            lda     #$00
            sta     L9FB9
            sta     L9FBA
            sta     STATUS
            rts

L987C:      lda     #$E0
            sta     FCHTYP
            lda     #$20
            sta     RETTYP
            jmp     UFPARM

L9889:      lda     INPLBUF
            sta     TMPBUFP
            lda     INPLBUF+1
            sta     TMPBUFP+1
            lda     #$00
            sta     L9FC2
L9898:      jsr     L987C
            jsr     FRETPS
            sta     L9FC3
            jsr     L98B7
            jsr     L9928
            bcs     L98B1
            jsr     CHRGET
            bne     L9898
            jmp     L9846

L98B1:      jsr     SETINPB
            ldy     #$00
            rts

L98B7:      tax
            beq     L98D9
            ldy     L9FC2
L98BD:      cpy     MAXILEN
            bne     L98C5
            jmp     L97DA

L98C5:      jsr     L98DA
            beq     L98CF
            sta     (TMPBUFP),y
            iny
            bne     L98BD
L98CF:      sta     (TMPBUFP),y
            iny
            lda     #$0D
            sta     (TMPBUFP),y
            sty     L9FC2
L98D9:      rts

L98DA:      ldx     #$00
            lda     (INDEX,x)
            inc     INDEX
            bne     L98E4
            inc     INDEX+1
L98E4:      dec     L9FC3
            rts

L98E8:      ldy     #$00
            lda     (FLTACC+3),y    ; Get length of string
            beq     L9909
            sty     MEMCOUNT+1
            sta     MEMCOUNT
            clc
            adc     L9FBA
            sta     L9FBA
L98F9:      iny
            lda     (FLTACC+3),y
            sta     MEMBUFF-1,y
            cpy     #$02
            bne     L98F9
            ldx     L9FB8
            jsr     _OUTMBUFF
L9909:      rts

L990A:      lda     #$C0
            sta     FCHTYP
            lda     #$40
            sta     RETTYP
            jmp     UFPARM

L9917:      clc
            lda     MEMBUFF
            adc     MEMCOUNT
            lda     MEMBUFF+1
            adc     MEMCOUNT+1
            cmp     MEMTOP+1
            bcc     L9927
            jmp     L97DE

L9927:      rts

L9928:      jsr     CHRGOT
            beq     L9933
            cmp     #$2C
            bne     L9933
            clc
            rts

L9933:      sec
            rts

L9935:      inc     L9FB9
L9938:      lda     #$00
            ror     a
            ora     L9FB9
            sta     STATUS
            rts

SYSTEM:     jsr     CHRGOT
            beq     L9969
            jsr     L9889
            lda     L9FC2
            beq     L9969
            jsr     _UNPROTECT
            sec
            ror     SVC13FLG
            jsr     SETOUTB
.ifdef     mtu
            jsr     CPYEXINBNK
.else
            jsr     CLEARBRK
.endif
            jsr     CKCMDEXEC
            jsr     _GETASSGNFLG
            sta     STATUS
            asl     SVC13FLG
            jmp     _PROTECT
L9969:      jmp     L9846

TRUNC:      jsr     L9860
            jsr     _UNPROTECT
            ldx     L9FB8
            jsr     FTRUNC
            lda     #$80
            sta     STATUS
            jmp     _PROTECT

PRINT:      jsr     L9860
            jsr     _UNPROTECT
            jsr     CHRGOT
            beq     L9996
            cmp     #$2C
            bne     L9993
            jsr     CHRGET
            bne     L99A7
L9993:      jmp     L9846

L9996:      ldx     L9FB8
            jsr     OUTCR
            jsr     L9938
            jmp     _PROTECT

L99A2:      jsr     CHRGET
            beq     L9996
L99A7:      cmp     #$A6
            beq     L99E7
            cmp     #$A9
            beq     L99EA
            jsr     L987C
            jsr     FRETPS
            jsr     L98E8
L99B8:      jsr     L9935
            jsr     CHRGOT
            beq     L9996
            cmp     #$3B
            beq     L99DF
            cmp     #$2C
            bne     L99A2
            lda     L9FBA
            sec
L99CC:      sbc     #$0A
            bcs     L99CC
            eor     #$FF
            adc     #$01
            tax
            adc     L9FBA
            sta     L9FBA
            txa
            jsr     L9A27
L99DF:      jsr     CHRGET
            bne     L99A7
            jmp     _PROTECT

L99E7:      sec
            bcs     L99EB
L99EA:      clc
L99EB:      php
            jsr     CHRGET
            jsr     L990A
            beq     L99F7
            jmp     L984C

L99F7:      plp
            bcc     L9A0A
            tax
            cmp     L9FBA
            bcc     L9A15
            beq     L9A15
            sbc     L9FBA
            stx     L9FBA
            bcs     L9A12
L9A0A:      tax
            adc     L9FBA
            sta     L9FBA
            txa
L9A12:      jsr     L9A27
L9A15:      jsr     CHRGOT
            cmp     #$29
            beq     L9A1F
            jmp     L9846

L9A1F:      jsr     CHRGET
            rol     STATUS
            jmp     L99B8

L9A27:      cmp     #$0A
            bcc     L9A37
            sbc     #$0A
            pha
            lda     #$0A
            jsr     L9A37
            pla
            bne     L9A27
            rts

L9A37:      sta     MEMCOUNT
            lda     #<L9A4E
            sta     MEMBUFF
            lda     #>L9A4E
            sta     MEMBUFF+1
            lda     #$00
            sta     MEMCOUNT+1
            ldx     L9FB8
            jsr     _OUTMBUFF
            jmp     L9938

L9A4E:      .byte   $20, $20, $20, $20, $20, $20, $20, $20, $20, $20 

ONERR:      lda     L9FBB
            beq     L9A6A
            sta     STATUS
            lda     #$00
            sta     L9FBB
            jsr     CHRGOT
            jmp     EXECUTE_STATEMENT

L9A6A:      ldy     #$00
L9A6C:      lda     (CHRPTR),y
            iny
            cmp     #$20
            beq     L9A6C
            cmp     #$4F
            bne     L9A9A
            lda     (CHRPTR),y
            iny
            cmp     #$46
            bne     L9A9A
            lda     (CHRPTR),y
            iny
            cmp     #$46
            bne     L9A9A
            clc
            tya
            adc     CHRPTR
            sta     CHRPTR
            bcc     L9A8F
            inc     $80
L9A8F:      jsr     _UNPROTECT          ; On exit, X = $00
            ldy     #$00
            jsr     _SETERRRCVRY        ; Set error recovery routine at $0000 (warm start)
            jmp     _PROTECT

L9A9A:      jsr     ERRDIR
            lda     CURLIN
            sta     L9FBC
            lda     CURLIN+1
            sta     L9FBC+1
            jsr     _UNPROTECT
            lda     #<ERROR
            ldy     #>ERROR
            jsr     _SETERRRCVRY
            jsr     _PROTECT
            jmp     REM

INPUT:      jsr     L9852
            jsr     _UNPROTECT
L9ABD:      lsr     L9FBE
            jsr     GETVAR
            lda     VALTYP
            and     L0780
            sta     L9FBF
            ldx     L9FB8
            ldy     #$00
            bit     L9FC0
            bpl     L9ADF
            lda     L9FC1
            sta     INPUTBUFFER
            sty     L9FC0
            iny
L9ADF:      jsr     _GETCHAR
            sta     INPUTBUFFER,y
            php
            bcs     L9B26
            cmp     #$0D
            beq     L9B26
            cmp     #$2C
            bne     L9AF5
            bit     L9FBF
            bpl     L9B26
L9AF5:      bit     VALTYP
            bmi     L9B08
            cmp     #$20
            bne     L9B04
            bit     L9FBE
            bpl     L9B08
            bmi     L9B26
L9B04:      sec
            ror     L9FBE
L9B08:      cpy     #$C0
            beq     L9B11
            plp
            iny
            jmp     L9ADF

L9B11:      sta     L9FC1
            lda     #$0D
            sta     INPUTBUFFER,y
            lda     #$80
            sta     L9FC0
            lda     L9FB9
            ora     #$40
            sta     L9FB9
L9B26:      plp
            jsr     L9935
            sty     L9FBA
            tya
            ldx     #<INPUTBUFFER
            ldy     #>INPUTBUFFER
            bit     VALTYP
            bpl     L9B44
            stx     FLTACC+3
            sty     FLTACC+4
            stx     STRNG1
            sty     STRNG1+1
            jsr     L276F
            jmp     L9B4B

L9B44:      stx     INDEX
            sty     INDEX+1
            jsr     VAL2
L9B4B:      lda     VALTYP+1
            ldx     VALTYP
            jsr     STORVAR
            lda     CHRPTR+1
            cmp     #$08
            beq     L9B6C
            jsr     CHRGOT
            beq     L9B76
            cmp     #$2C
            bne     L9B69
            jsr     CHRGET
            beq     L9B69
            jmp     L9ABD

L9B69:      jmp     L9846

L9B6C:      lda     #$00
            tay
L9B6F:      sta     (CHRPTR),y
            iny
            cpy     #$03
            bne     L9B6F
L9B76:      jmp     _PROTECT

APPEND:     jsr     FILOPEN
            jsr     L1618
            jsr     _GETMBUFF
            jsr     L9BC4
            lda     FRSTLIN+1
            cmp     MEMBUFF+1
            bcc     L9BBE
            bne     L9B97
            lda     FRSTLIN
            cmp     MEMBUFF
            bcc     L9BBE
            beq     L9BBE
L9B97:      jsr     L162C
            sec
            lda     SMPVAR
            sbc     #$02
            sta     MEMBUFF
            lda     SMPVAR+1
            sbc     #$00
            sta     MEMBUFF+1
            jsr     L9917
            jsr     _GETMBUFF
            bcc     L9BB5
            jsr     _FREECH
            jmp     L97E2

L9BB5:      jsr     _FREECH
            jsr     _PROTECT
            jmp     LOAD2

L9BBE:      jsr     _FREECH
            jmp     L97E6

L9BC4:      lda     #$00
            sta     MEMBUFF
            sta     MEMBUFF+1
            lda     PRGTXT
            sta     P0SCRATCH
            lda     PRGTXT+1
            sta     P0SCRATCH+1
L9BD2:      ldy     #$01
            lda     (P0SCRATCH),y
            beq     L9BF2
            iny
            lda     (P0SCRATCH),y
            sta     MEMBUFF
            iny
            lda     (P0SCRATCH),y
            sta     MEMBUFF+1
            ldy     #$00
            lda     (P0SCRATCH),y
            pha
            iny
            lda     (P0SCRATCH),y
            sta     P0SCRATCH+1
            pla
            sta     P0SCRATCH
            jmp     L9BD2

L9BF2:      rts

PUT:        jsr     L9852
            jsr     _UNPROTECT
L9BF9:      jsr     L990A
            cpx     #$00
            beq     L9C03
            jmp     L984C

L9C03:      ldx     L9FB8
            jsr     _OUTCHAR
            jsr     L9935
            jsr     L9928
            bcs     L9C19
            jsr     CHRGET
            bne     L9BF9
L9C16:      jmp     L9846

L9C19:      jsr     _PROTECT
            jsr     CHRGOT
            bne     L9C16
            rts

GET:        jsr     L9852
            jsr     _UNPROTECT
L9C28:      jsr     GETVAR
            ldx     L9FB8
            jsr     _GETCHAR
            bcc     L9C38
            dec     L9FB9
            lda     #$00
L9C38:      tay
            jsr     L9935
            bit     VALTYP
            bpl     L9C57
            sty     INPUTBUFFER
            ldx     #<INPUTBUFFER
            ldy     #>INPUTBUFFER
            stx     FLTACC+3
            sty     FLTACC+4
            stx     STRNG1
            sty     STRNG1+1
            lda     #$01
            jsr     L276F
            jmp     L9C5C

L9C57:      lda     #$00
            jsr     GIVAYF
L9C5C:      lda     VALTYP+1
            ldx     VALTYP
            jsr     STORVAR
            jsr     L9928
            bcs     L9C70
            jsr     CHRGET
            bne     L9C28
L9C6D:      jmp     L9846

L9C70:      jsr     _PROTECT
            jsr     CHRGOT
            bne     L9C6D
            rts

POSITION:
            jsr     L9852
            lda     #$C0
            sta     FCHTYP
            lda     #$80
            sta     RETTYP
            jsr     UFPARM
            lda     FACSIGN
            bmi     L9C9B
            lda     FLTACC
            bne     L9C99
            sta     FILEPOS
            sta     FILEPOS+1
            sta     FILEPOS+2
            beq     L9CBD
L9C99:      bmi     L9C9E
L9C9B:      jmp     L984C

L9C9E:      cmp     #$99
            bcs     L9C9B
            ldx     FLTACC+1
            stx     FILEPOS+2
            ldx     FLTACC+2
            stx     FILEPOS+1
            ldx     FLTACC+3
            stx     FILEPOS
L9CAE:      cmp     #$98
            beq     L9CBD
            lsr     FILEPOS+2
            ror     FILEPOS+1
            ror     FILEPOS
            clc
            adc     #$01
            bne     L9CAE
L9CBD:      jsr     _UNPROTECT
            ldx     L9FB8
            jsr     _FSEEK
            jsr     L9938
            jmp     _PROTECT

L9CCC:      lda     #$28
            jsr     CHRCHK
            jsr     L9860
            lda     #$29
            jsr     CHRCHK
            jsr     _UNPROTECT
            ldx     L9FB8
            jsr     ASSIGNED
            bmi     L9D10
            jsr     GETFINFO
            lda     CURFINFO+FINFO::FPOS
            sec
            sbc     #$40
            sta     FLTACC+3
            lda     CURFINFO+FINFO::FPOS+1
            sbc     #$00
            sta     FLTACC+2
            lda     CURFINFO+FINFO::FPOS+2
            sbc     #$00
            sta     FLTACC+1
            lda     #$00
            sta     FLTACC+4
            sta     FACSIGN
            lda     #$98
            sta     FLTACC
L9D04:      jsr     FEOF
            jsr     L9938
            jsr     _PROTECT
            jmp     NORMALIZE_FAC2

L9D10:      lda     #$00
            sta     FLTACC
            sta     FACSIGN
            beq     L9D04
LIST:       jsr     L9860
            jsr     CHRGOT
            beq     L9D25
            jsr     L9928
            bcs     L9D31
L9D25:      ldx     L9FB8
            stx     OUTCHN
            jsr     CHRGET
            jmp     LIST2

L9D31:      jmp     L9846
ENTER:      jsr     L9860
            jsr     CHRGOT
            bne     L9D42
            ldx     L9FB8
            jmp     ENTER2

L9D42:      jmp     L9846

L9D45:      lda     STREND
            pha
            lda     STREND+1
            pha
            jsr     GETVAR
            tax
            pla
            cmp     STREND+1
            bne     L9D69
            pla
            cmp     STREND
            bne     L9D69
            cpy     ARYVAR+1
            bcc     L9D66
            bne     L9D63
            cpx     ARYVAR
            bcc     L9D66
L9D63:      lda     #$80
            .byte   $2C
L9D66:      lda     #$00
            rts

L9D69:      jmp     L97D6

L9D6C:      jsr     L9D45
            sta     L9FC4
            stx     L9FC6
            sty     L9FC7
            lda     VALTYP+1
            asl     VALTYP
            ror     a
            and     #$C0
            sta     L9FC5
            bit     L9FC4
            bpl     L9D91
            lda     VARNAM
            sta     L9FCA
            lda     VARNAM+1
            sta     L9FCB
L9D91:      jsr     CHRGOT
            cmp     #$A7
            bne     L9DB2
            jsr     CHRGET
            jsr     L9D45
            cmp     L9FC4
            bne     L9DAF
            cpy     L9FC7
            bcc     L9DAF
            bne     L9DB5
            cpx     L9FC6
            bcs     L9DB5
L9DAF:      jmp     L97D6

L9DB2:      lda     L9FC4
L9DB5:      stx     L9FC8
            sty     L9FC9
            cmp     L9FC4
            bne     L9DAF
            tax
            bpl     L9DD1
            lda     L9FCA
            cmp     VARNAM
            bne     L9DAF
            lda     L9FCB
            cmp     VARNAM+1
            bne     L9DAF
L9DD1:      lda     L9FC6
            sta     VARLOC
            lda     L9FC7
            sta     VARLOC+1
            rts

L9DDC:      lda     L9FC7
            cmp     L9FC9
            bcc     L9DEC
            bne     L9DEC
            lda     L9FC6
            cmp     L9FC8
L9DEC:      rts

L9DED:      bit     L9FC5
            bmi     L9E11
            bvs     L9DF7
            lda     #$05
            .byte   $2C
L9DF7:      lda     #$02
            clc
            adc     L9FC8
            php
            sec
            sbc     L9FC6
            sta     MEMCOUNT
            lda     L9FC9
            sbc     L9FC7
            plp
            adc     #$00
            sta     MEMCOUNT+1
            clc
            rts

L9E11:      ldy     #$01
            sty     MEMCOUNT
            dey
            sty     MEMCOUNT+1
            sec
            rts

L9E1A:      ldy     #$06
            lda     (VARLOC),y
            and     #$80
            sta     L9FC5
            beq     L9E2D
            dey
            lda     (VARLOC),y
            bpl     L9E2D
            lsr     L9FC5
L9E2D:      clc
            lda     L9FC6
            adc     #$07
L9E33:      sta     L9FC6
            sta     VARLOC
            lda     L9FC7
            adc     #$00
            sta     L9FC7
            sta     VARLOC+1
            rts

L9E43:      lda     L9FC6
            sta     MEMBUFF
            lda     L9FC7
            sta     MEMBUFF+1
            ldx     L9FB8
            jsr     _OUTMBUFF
            bit     L9FC5
            bpl     L9E6F
            ldy     #$00
            lda     (VARLOC),y
            beq     L9E6F
            sta     MEMCOUNT
            sty     MEMCOUNT+1
            iny
            lda     (VARLOC),y
            sta     MEMBUFF
            iny
            lda     (VARLOC),y
            sta     MEMBUFF+1
            jsr     _OUTMBUFF
L9E6F:      rts

L9E70:      lda     L9FC6
            sta     MEMBUFF
            lda     L9FC7
            sta     MEMBUFF+1
            ldx     L9FB8
            jsr     _GETMBUFF
            bit     L9FC5
            bpl     L9EBE
            ldy     #$00
            lda     (VARLOC),y
            beq     L9EBE
            pha
            eor     #$FF
            sec
            adc     STRBTM
            ldx     STRBTM+1
            bcs     L9E96
            dex
L9E96:      cpx     STREND+1
            bcc     L9EBF
            bne     L9EA0
            cmp     STREND
            bcc     L9EBF
L9EA0:      sta     STRBTM
            stx     STRBTM+1
            sta     FRESPC
            stx     FRESPC+1
L9EA8:      sta     MEMBUFF
            stx     MEMBUFF+1
            sty     MEMCOUNT+1
            iny
            sta     (VARLOC),y
            txa
            iny
            sta     (VARLOC),y
            pla
            sta     MEMCOUNT
            ldx     L9FB8
            jsr     _GETMBUFF
L9EBE:      rts

L9EBF:      pla
            jsr     GETSPA
            pha
            lda     STRBTM
            ldx     STRBTM+1
            ldy     #$00
            beq     L9EA8
BLKWRT:     jsr     L9852
            jsr     _UNPROTECT
L9ED2:      jsr     L9D6C
            bit     L9FC4
            bmi     L9F1A
L9EDA:      bit     L9FC5
            bmi     L9EE9
            bvs     L9EE5
            lda     #$05
            bne     L9EEB
L9EE5:      lda     #$02
            bne     L9EEB
L9EE9:      lda     #$01
L9EEB:      sta     MEMCOUNT
            lda     #$00
            sta     MEMCOUNT+1
            jsr     L9E43
            php
            jsr     L9DDC
            bcs     L9F01
            plp
            jsr     L9E1A
            jmp     L9EDA

L9F01:      plp
L9F02:      jsr     L9935
            jsr     L9928
            bcs     L9F12
            jsr     CHRGET
            bne     L9ED2
L9F0F:      jmp     L9846

L9F12:      jsr     CHRGOT
            bne     L9F0F
            jmp     _PROTECT

L9F1A:      jsr     L9DED
            bcs     L9F2C
            jsr     L9E43
            jmp     L9F02

L9F25:      ldy     #$01
            sty     MEMCOUNT
            dey
            sty     MEMCOUNT+1
L9F2C:      jsr     L9E43
            php
            jsr     L9DDC
            bcs     L9F01
            plp
            clc
            lda     L9FC6
            adc     #$03
            jsr     L9E33
            jmp     L9F25

BLKRD:      jsr     L9852
            jsr     _UNPROTECT
L9F48:      jsr     L9D6C
            bit     L9FC4
            bmi     L9F90
L9F50:      bit     L9FC5
            bmi     L9F5F
            bvs     L9F5B
            lda     #$05
            bne     L9F61
L9F5B:      lda     #$02
            bne     L9F61
L9F5F:      lda     #$01
L9F61:      sta     MEMCOUNT
            lda     #$00
            sta     MEMCOUNT+1
            jsr     L9E70
            php
            jsr     L9DDC
            bcs     L9F77
            plp
            jsr     L9E1A
            jmp     L9F50

L9F77:      plp
L9F78:      jsr     L9935
            jsr     L9928
            bcs     L9F88
            jsr     CHRGET
            bne     L9F48
L9F85:      jmp     L9846

L9F88:      jsr     CHRGOT
            bne     L9F85
            jmp     _PROTECT

L9F90:      jsr     L9DED
            bcs     L9FA2
            jsr     L9E70
            jmp     L9F78

L9F9B:      ldy     #$01
            sty     MEMCOUNT
            dey
            sty     MEMCOUNT+1
L9FA2:      jsr     L9E70
            php
            jsr     L9DDC
            bcs     L9F77
            plp
            clc
            lda     L9FC6
            adc     #$03
            jsr     L9E33
            jmp     L9F9B

L9FB8:      .byte   $00
L9FB9:      .byte   $00
L9FBA:      .byte   $00
L9FBB:      .byte   $00
L9FBC:      .word   $0000
L9FBE:      .byte   $00
L9FBF:      .byte   $00
L9FC0:      .byte   $00
L9FC1:      .byte   $00
L9FC2:      .byte   $00
L9FC3:      .byte   $00
L9FC4:      .byte   $00
L9FC5:      .byte   $00
L9FC6:      .byte   $00
L9FC7:      .byte   $00
L9FC8:      .byte   $00
L9FC9:      .byte   $00
L9FCA:      .byte   $00
L9FCB:      .byte   $00

            .endscope

            .end