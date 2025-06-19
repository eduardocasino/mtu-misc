; da65 V2.18 - Ubuntu 2.19-1
; Created:    2025-06-03 19:01:18
; Input file: basic1.bin
; Page:       1


; Credit to Michael Steil mist64@mac.com of the
; Mist64 project
;
            .setcpu "6502"
            .feature string_escapes
            .feature force_range
            .feature org_per_seg

            .include "macros.inc"
            .include "symbols.inc"
.ifdef mtu
            .include "monomeg.inc"

MEMORY_TOP  = $BE00
CHECKSUM    = $006C
.else
MEMORY_TOP  = $C000
CHECKSUM    = $003E
.endif

            .scope msbasic
; constants
;
STACK_TOP		    := $FE
SPACE_FOR_GOSUB     := $3E
CRLF_1              := $0A
CRLF_2              := $0D


BYTES_FP		    := 5
BYTES_PER_ELEMENT   := BYTES_FP
BYTES_PER_VARIABLE  := BYTES_FP+2
MANTISSA_BYTES	    := BYTES_FP-1
BYTES_PER_FRAME     := 2*BYTES_FP+8
FOR_STACK1		    := 2*BYTES_FP+5
FOR_STACK2		    := BYTES_FP+4

MAX_EXPON           = 10


; zero page
;
ZP_START4 = $1A
ZP_START5 = $99

            .zeropage

GORESTART:	.res    3               ; $00
GOSTROUT:   .res    3               ; $03
GOAYINT:    .res    2               ; $06
GOGIVEAYF:  .res    2               ; $08
CHARAC:     .res    1               ; $0A
ENDCHR:     .res    1               ; $0B
EOLPNTR:    .res    1               ; $0C
DIMFLG:     .res    1               ; $0D
VALTYP:     .res    2               ; $0E-$0F
DATAFLG:    .res    1               ; $10
SUBFLG:     .res    1               ; $11
INPUTFLG:   .res    1               ; $12
CPRMASK:    .res    1               ; $13
Z14:        .res    1               ; $14
Z15:        .res    1               ; $15
POSX:       .res    1               ; $16
Z17:        .res    1               ; $17
LINNUM:
TXPSV:      .res    2               ; $18-$19

            .org    ZP_START4

TEMPPT:     .res    1               ; $1A
LASTPT:     .res    2               ; $1B
TEMPST:     .res    9               ; $1D
INDEX:      .res    2               ; $26
DEST:       .res    2               ; $28
RESULT:     .res    BYTES_FP        ; $2A
RESULT_LAST = RESULT + BYTES_FP-1
TXTTAB:     .res    2               ; $2F
VARTAB:     .res    2               ; $31
ARYTAB:     .res    2               ; $33
STREND:     .res    2               ; $35
FRETOP:     .res    2               ; $37
FRESPC:     .res    2               ; $39
MEMSIZ:     .res    2               ; $3B
CURLIN:     .res    2               ; $3D
OLDLIN:     .res    2               ; $3F
OLDTEXT:    .res    2               ; $41
Z43:        .res    2               ; $43
DATPTR:     .res    2               ; $45
INPTR:      .res    2               ; $47
VARNAM:     .res    2               ; $49
VARPNT:     .res    2               ; $4B
FORPNT:     .res    2               ; $4D
LASTOP:     .res    2               ; $4F
CPRTYP:     .res    1               ; $51
FNCNAM:
TEMP3:      .res    2               ; $52
DSCPTR:     .res    3               ; $54
DSCLEN:     .res    2               ; $57
JMPADRS     := DSCLEN + 1
Z59:        .res    1               ; $59
ARGEXTENSION:
            .res    1               ; $5A
TEMP1 :     .res    1               ; $5B
HIGHDS:     .res    2               ; $5C
HIGHTR:     .res    2               ; $5E
TEMP2:      .res    1               ; $60
INDX:
TMPEXP:     .res    1               ; $61
EXPON:      .res    1               ; $62
LOWTR:
LOWTRX:     .res    1               ; $63
EXPSGN:     .res    1               ; $64
FAC:        .res    BYTES_FP        ; $65
FAC_LAST = FAC + BYTES_FP-1
FACSIGN:    .res    1               ; $6A
SERLEN:     .res    1               ; $6B
SHIFTSIGNEXT:
            .res    1               ; $6C
ARG:        .res    BYTES_FP        ; $6D
ARG_LAST = ARG + BYTES_FP-1
ARGSIGN:    .res    1               ; $72
STRNG1:     .res    2               ; $73
SGNCPR = STRNG1
FACEXTENSION = STRNG1+1
STRNG2:     .res    2               ; $75
Z77:        .res    1               ; $77

CHRGET:                             ; $78
TXTPTR  = <(GENERIC_TXTPTR-GENERIC_CHRGET + CHRGET)
CHRGOT  = <(GENERIC_CHRGOT-GENERIC_CHRGET + CHRGET)
CHRGOT2 = <(GENERIC_CHRGOT2-GENERIC_CHRGET + CHRGET)
RNDSEED = <(GENERIC_RNDSEED-GENERIC_CHRGET + CHRGET)

            .org    ZP_START5

Z99:        .res    2               ; $99-$9A
Z9B:        .res    1               ; $9B
Z9C:        .res    2               ; $9C-$9D
Z9E:        .res    1               ; $9E

STACK       := $100
STACK2      := STACK

            .bss

L0800:      .res    1
L0801:      .res    1
L0802:      .res    1
ICHANNEL:   .res    1
SCHANNEL:   .res    1
OCHANNEL:   .res    1
L0806:      .res    1
L0807:      .res    1
TOPMEM:     .res    2               ; Top of memory
L080A:      .res    1
L080B:      .res    3
L080E:      .res    1
L080F:      .res    3
L0812:      .res    1
L0813:      .res    1
L0814:      .res    2               ; Points to buffer below
            .res    2               ; Size of buffer below (10)
L0818:      .res    8          
L0820:      .res    2
L0822:      .res    1
            .res    20
L0837:      .res    1
L0838:      .res    1
L0839:      .res    1
L083A:      .res    1
L083B:      .res    1
INPUTBUFFER:
L083C:      .res    1
L083D:      .res    1

            .segment "BSS2"

L0900:      .res    8
L0908:      .res    16
L0918:      .res    16
L0928:      .res    16
L0938:      .res    8
L0940:      .res    16
L0950:      .res    $60         
L09B0:      .res    2               ; Points to buffer below
            .res    2               ; Size of buffer below (21)
L09B4:      .res    4
L09B8:      .res    2
L09BA:      .res    1
L09BB:      .res    1
L09BC:      .res    1
L09BD:      .res    1
L09BE:      .res    1
L09BF:      .res    1
L09C0:      .res    2
            .res    2
            .res    2
L09C6:      .res    1
L09C7:      .res    1
            .res    1

L09C9:      .res    1
L09CA:      .res    1
L09CB:      .res    1
L09CC:      .res    1
L09CD:      .res    1
L09CE:      .res    1
L09CF:      .res    1
L09D0:      .res    1
            .res    $1C
L09ED:      .res    1
L09EE:      .res    1


            .code

            ; Loadable file data
            ;
            .byte   $58             ; CODOS loadable file header byte
            .byte   $00             ; Memory overlay
            .byte   $00             ; Memory bank
            .byte   $00             ; Reserved
            .addr   ENTRY           ; Entry point
            .addr   START           ; Load address
            .word   CODE_SIZE       ; Memory image size

; 0x09F9
START:

L09F9:      sec
            ror     L09D0

L09FD:      jmp     L18FA

ENTRY:      jmp     INIT
L0A03:      jmp     L0E65
L0A06:      jmp     L2792+1         ; Seems not valid
L0A09:      jmp     L3133
L0A0C:      jmp     L1D09+2         ; Seems not valid...

            init_token_tables

            keyword_rts "END", END
            keyword_rts "FOR", FOR
            keyword_rts "NEXT", NEXT
            keyword_rts "DATA", DATA
            keyword_rts "INPUT", INPUT
            keyword_rts "DIM", DIM
            keyword_rts "READ", READ
            keyword_rts "LET", LET
            keyword_rts "GOTO", GOTO, TOKEN_GOTO
            keyword_rts "RUN", RUN
            keyword_rts "IF", IF
            keyword_rts "RESTORE", RESTORE
            keyword_rts "GOSUB", GOSUB, TOKEN_GOSUB
            keyword_rts "RETURN", POP
            keyword_rts "REM", REM
            keyword_rts "STOP", STOP
            keyword_rts "ON", ON
            keyword_rts "WAIT", WAIT
            keyword_rts "LOAD", LOAD
            keyword_rts "ENTER", ENTER
            keyword_rts "SAVE", SAVE
            keyword_rts "DEF", DEF
            keyword_rts "POKE", POKE
            keyword_rts "PRINT", PRINT
            keyword_rts "CONT", CONT
            keyword_rts "LIST", LIST
            keyword_rts "CLEAR", CLEAR
            keyword_rts "MCALL", MCALL
            keyword_rts "OUTCHAN", OUTCHAN
            keyword_rts "BYE", BYE
.ifdef mtu
            keyword_rts "LEGEND", LEGEND
.endif
            keyword_rts "GET", GET
            keyword_rts "LIB", LIB
            keyword_rts "FRELIB", FRELIB
            keyword_rts "EDIT", EDIT
.ifdef mtu
            keyword_rts "TONE", TONE
.endif
            keyword_rts "NEW", NEW
            keyword_rts "\"\"\"\"\"\"", ERRUNDEF    ; """""" RESERVED

            count_tokens

            keyword "TAB(", TOKEN_TAB
            keyword "TO", TOKEN_TO
            keyword "FN", TOKEN_FN  ; A8
            keyword "SPC(", TOKEN_SPC
            keyword "THEN", TOKEN_THEN
            keyword "NOT", TOKEN_NOT
            keyword "STEP", TOKEN_STEP
            keyword "+", TOKEN_PLUS
            keyword "-", TOKEN_MINUS
            keyword "*"
            keyword "/"
            keyword "^"
            keyword "AND"
            keyword "OR"
            keyword ">", TOKEN_GREATER
            keyword "=", TOKEN_EQUAL
            keyword "<"

            .segment "VECTORS"
UNFNC:
            keyword_addr "SGN", SGN, TOKEN_SGN
            keyword_addr "INT", INT
            keyword_addr "ABS", ABS
            keyword_addr "USR", USR, TOKEN_USR
            keyword_addr "FRE", FRE
            keyword_addr "POS", POS
            keyword_addr "SQR", SQR
            keyword_addr "RND", RND
            keyword_addr "LOG", LOG
            keyword_addr "EXP", EXP

            .segment "VECTORS"
UNFNC_COS:
            keyword_addr "COS", COS

            .segment "VECTORS"
UNFNC_SIN:
            keyword_addr "SIN", SIN

            .segment "VECTORS"
UNFNC_TAN:
            keyword_addr "TAN", TAN

            .segment "VECTORS"
UNFNC_ATN:
            keyword_addr "ATN", ATN
            keyword_addr "PEEK", PEEK
            keyword_addr "LEN", LEN
            keyword_addr "STR$", STR
            keyword_addr "VAL", VAL
            keyword_addr "ASC", ASC

            .segment "KEYWORDS"

            .byte   $00

            keyword_addr "CHR$", CHRSTR
            keyword_addr "LEFT$", LEFTSTR, TOKEN_LEFTSTR
            keyword_addr "RIGHT$", RIGHTSTR
            keyword_addr "MID$", MIDSTR
            keyword "GO", TOKEN_GO

            .segment "KEYWORDS"

            .byte   $00
            .byte   $00

            .segment "VECTORS"

MATHTBL:    .byte   $79
            .addr   FADDT-1
            .byte   $79
            .addr   FSUBT-1
            .byte   $7B
            .addr   FMULTT-1
            .byte   $7B
            .addr   FDIVT-1
            .byte   $7F
            .addr   FPWRT-1
            .byte   $50
            .addr   TAND-1
            .byte   $46
            .addr   OR-1
            .byte   $7D
            .addr   NEGOP-1
            .byte   $5A
            .addr   EQUOP-1
            .byte   $64
            .addr   RELOPS-1

            init_error_table

            define_error ERR_NOFOR, "NEXT WITHOUT FOR"
            define_error ERR_SYNTAX, "SYNTAX"
            define_error ERR_NOGOSUB, "RETURN WITHOUT GOSUB"
            define_error ERR_NODATA, "OUT OF DATA"
            define_error ERR_ILLQTY, "ILLEGAL QUANTITY"
            define_error ERR_OVERFLOW, "OVERFLOW"
            define_error ERR_MEMFULL, "OUT OF MEMORY"
            define_error ERR_UNDEFSTAT, "UNDEF'D STATEMENT"
            define_error ERR_BADSUBS, "BAD SUBSCRIPT"
            define_error ERR_REDIMD, "REDIM'D ARRAY"
            define_error ERR_ZERODIV, "DIVISION BY ZERO"
            define_error ERR_ILLDIR, "ILLEGAL DIRECT"
            define_error ERR_BADTYPE, "TYPE MISMATCH"
            define_error ERR_STRLONG, "STRING TOO LONG"
            define_error ERR_BADDATA, "FILE DATA"
            define_error ERR_FRMCPX, "FORMULA TOO COMPLEX"
            define_error ERR_CANTCONT, "CAN'T CONTINUE"
            define_error ERR_UNDEFFN, "UNDEF'D FUNCTIONN"       ; <-Typo in original

            init_codos_error_table

            define_codos_error ERR_CODOS_LOAD, "LOAD"
            define_codos_error ERR_CODOS_FEXISTS, "FILE EXISTS"
            define_codos_error ERR_CODOS_NFOUND, "FILE NOT FOUND"
            define_codos_error ERR_CODOS_LARGE, "FILE TOO LARGE"
            define_codos_error ERR_CODOS_NBASIC, "NOT A BASIC FILE"
            define_codos_error ERR_CODOS_NLOADED, "NOT LOADED"
            define_codos_error ERR_CODOS_LIST, "LIST"
            define_codos_error ERR_CODOS_MNYLIBS, "TOO MANY LIBRARIES"
            define_codos_error ERR_CODOS_LIBLOAD, "CAN'T LOAD LIBRARY"
            define_codos_error ERR_CODOS_NOTLIB, "NOT A LIBRARY FILE"
            define_codos_error ERR_CODOS_INTEGRITY, "INTEGRITY"

L0D3D:      .byte   $0B

QT_ERROR:   .byte   " ERROR", $00
QT_IN:      .byte   " IN ", $00
QT_OK:      .byte   $0D, "READY.", $0D, $00
QT_BREAK:   .byte   $0D, " BREAK", $00
            .byte   $81

; ----------------------------------------------------------------------------
; CALLED BY "NEXT" AND "FOR" TO SCAN THROUGH
; THE STACK FOR A FRAME WITH THE SAME VARIABLE.
;
; (FORPNT) = ADDRESS OF VARIABLE IF "FOR" OR "NEXT"
; 	= $XXFF IF CALLED FROM "RETURN"
; 	<<< BUG: SHOULD BE $FFXX >>>
;
;	RETURNS .NE. IF VARIABLE NOT FOUND,
;	(X) = STACK PNTR AFTER SKIPPING ALL FRAMES
;
;	.EQ. IF FOUND
;	(X) = STACK PNTR OF FRAME FOUND
; ----------------------------------------------------------------------------
GTFORPNT:   tsx
            inx
            inx
            inx
            inx

L0D61:      lda     STACK+1,x
            cmp     #$81
            bne     L0D89
            lda     FORPNT+1
            bne     L0D76
            lda     STACK+2,x
            sta     FORPNT
            lda     STACK+3,x
            sta     FORPNT+1
L0D76:      cmp     STACK+3,x
            bne     L0D82
            lda     FORPNT
            cmp     STACK+2,x
            beq     L0D89
L0D82:      txa
            clc
            adc     #BYTES_PER_FRAME
            tax
            bne     L0D61
L0D89:      rts

; ----------------------------------------------------------------------------
; MOVE BLOCK OF MEMORY UP
;
; ON ENTRY:
;	(Y,A) = (HIGHDS) = DESTINATION END+1
;	(LOWTR) = LOWEST ADDRESS OF SOURCE
;	(HIGHTR) = HIGHEST SOURCE ADDRESS+1
; ----------------------------------------------------------------------------
BLTU:       jsr     REASON
            sta     STREND
            sty     STREND+1
BLTU2:      sec
            lda     HIGHTR
            sbc     LOWTR
            sta     INDEX
            tay
            lda     HIGHTR+1
            sbc     LOWTR+1
            tax
            inx
            tya
            beq     L0DC5
            lda     HIGHTR
            sec
            sbc     INDEX
            sta     HIGHTR
            bcs     L0DAE
            dec     HIGHTR+1
            sec
L0DAE:      lda     HIGHDS
            sbc     INDEX
            sta     HIGHDS
            bcs     L0DBE
            dec     HIGHDS+1
            bcc     L0DBE
L0DBA:      lda     (HIGHTR),y
            sta     (HIGHDS),y
L0DBE:      dey
            bne     L0DBA
            lda     (HIGHTR),y
            sta     (HIGHDS),y
L0DC5:      dec     HIGHTR+1
            dec     HIGHDS+1
            dex
            bne     L0DBE
            rts

; ----------------------------------------------------------------------------
; CHECK IF ENOUGH ROOM LEFT ON STACK
; FOR "FOR", "GOSUB", OR EXPRESSION EVALUATION
; ----------------------------------------------------------------------------
CHKMEM:     asl     a
            adc     #SPACE_FOR_GOSUB
            bcs     MEMERR
            sta     INDEX
            tsx
            cpx     INDEX
            bcc     MEMERR
            rts

            .byte   $91             ; Dead code

; ----------------------------------------------------------------------------
; CHECK IF ENOUGH ROOM BETWEEN ARRAYS AND STRINGS
; (Y,A) = ADDR ARRAYS NEED TO GROW TO
; ----------------------------------------------------------------------------
REASON:     cpy     FRETOP+1
            bcc     L0E07
            bne     L0DE5
            cmp     FRETOP
            bcc     L0E07
L0DE5:      pha
            ldx     #FAC-TEMP1-1
            tya
L0DE9:      pha
            lda     TEMP1,x
            dex
            bpl     L0DE9
            jsr     GARBAG
            ldx     #TEMP1-FAC+1
L0DF4:      pla
            sta     FAC,x
            inx
            bmi     L0DF4
            pla
            tay
            pla
            cpy     FRETOP+1
            bcc     L0E07
            bne     MEMERR
            cmp     FRETOP
            bcs     MEMERR
L0E07:      rts

            .byte   $C2             ; Dead code?
            .byte   $A1             ;

; ----------------------------------------------------------------------------
; CODOS: HANDLE A CODOS EXTENSION ERROR
;
; Loads an alternate error table and continues in normal error routine
;
; (X)=OFFSET IN ERROR MESSAGE TABLE
; (ERRFLG) > 128 IF "ON ERR" TURNED ON
; (CURLIN+1) = $FF IF IN DIRECT MODE
; ----------------------------------------------------------------------------
CODOS_ERROR:
            lda     #<CODOS_ERROR_MESSAGES
            ldy     #>CODOS_ERROR_MESSAGES
            bne     ERROR2          ; Always jump

MEMERR:     ldx     #ERR_MEMFULL

; ----------------------------------------------------------------------------
; HANDLE AN ERROR
;
; (X)=OFFSET IN ERROR MESSAGE TABLE
; (ERRFLG) > 128 IF "ON ERR" TURNED ON
; (CURLIN+1) = $FF IF IN DIRECT MODE
; ----------------------------------------------------------------------------
ERROR:      lda     #<ERROR_MESSAGES
            ldy     #>ERROR_MESSAGES
ERROR2:     sta     LOWTR
            sty     LOWTR+1
            lsr     Z14             ; Clear the no output flag
            lda     #$01
            sta     SCHANNEL
            lda     #$02
            sta     OCHANNEL
            jsr     CRDO
            jsr     OUTQUES
            txa
            tay
L0E2E:      lda     (LOWTR),y
            pha
            and     #$7F
            jsr     OUTDO
            iny
            pla
            bpl     L0E2E

L0E3A:      
.ifdef mtu
            jsr     TONE2
.endif
            jsr     L03E9
            jsr     L1129
            lda     #<QT_ERROR
            ldy     #>QT_ERROR

; ----------------------------------------------------------------------------
; PRINT STRING AT (Y,A)
; PRINT CURRENT LINE # UNLESS IN DIRECT MODE
; FALL INTO WARM RESTART
; ----------------------------------------------------------------------------
PRINT_ERROR_LINNUM:
            ldx     #$01
            stx     SCHANNEL
            ldx     #$02
            stx     OCHANNEL
            jsr     STROUT
            ldy     CURLIN+1
            iny
            beq     RESTART
            jsr     INPRT

; ----------------------------------------------------------------------------
; WARM RESTART ENTRY
; ----------------------------------------------------------------------------
RESTART:    lsr     Z14             ; Clear the no output flag
            lda     #<QT_OK
            ldy     #>QT_OK
            jsr     GOSTROUT
L0E65:      jsr     INLIN
L0E68:      stx     TXTPTR
            sty     TXTPTR+1
            jsr     CHRGET
            tax
            beq     L0E65
            ldx     #$FF
            stx     CURLIN+1
            bcc     NUMBERED_LINE
            jsr     PARSE_INPUT_LINE
            jmp     NEWSTT2

; ----------------------------------------------------------------------------
; HANDLE NUMBERED LINE
; ----------------------------------------------------------------------------
NUMBERED_LINE:
            jsr     LINGET
            jsr     PARSE_INPUT_LINE
            sty     EOLPNTR
            jsr     FNDLIN
            bcc     PUT_NEW_LINE
            ldy     #$01
            lda     (LOWTR),y
            sta     INDEX+1
            lda     VARTAB
            sta     INDEX
            lda     LOWTR+1
            sta     DEST+1
            lda     LOWTR
            dey
            sbc     (LOWTR),y
            clc
            adc     VARTAB
            sta     VARTAB
            sta     DEST
            lda     VARTAB+1
            adc     #$FF
            sta     VARTAB+1
            sbc     LOWTR+1
            tax
            sec
            lda     LOWTR
            sbc     VARTAB
            tay
            bcs     L0EB9
            inx
            dec     DEST+1
L0EB9:      clc
            adc     INDEX
            bcc     L0EC1
            dec     INDEX+1
            clc
L0EC1:      lda     (INDEX),y
            sta     (DEST),y
            iny
            bne     L0EC1
            inc     INDEX+1
            inc     DEST+1
            dex
            bne     L0EC1
; ----------------------------------------------------------------------------
PUT_NEW_LINE:
            jsr     SETPTRS
            jsr     L0F30
            lda     INPUTBUFFER
            beq     L0E65
            clc
            lda     VARTAB
            sta     HIGHTR
            adc     EOLPNTR
            sta     HIGHDS
            ldy     VARTAB+1
            sty     HIGHTR+1
            bcc     L0EEA
            iny
L0EEA:      sty     HIGHDS+1
            jsr     BLTU

            lda     LINNUM
            ldy     LINNUM+1
            sta     INPUTBUFFER-2
            sty     INPUTBUFFER-1
            lda     STREND
            ldy     STREND+1
            sta     VARTAB
            sty     VARTAB+1
            ldy     EOLPNTR
            dey
; ---COPY LINE INTO PROGRAM-------
L0F04:      lda     INPUTBUFFER-4,y
            sta     (LOWTR),y
            dey
            cpy     #$FF
            bne     L0F04

; ----------------------------------------------------------------------------
; CLEAR ALL VARIABLES
; RE-ESTABLISH ALL FORWARD LINKS
; ----------------------------------------------------------------------------
FIX_LINKS:
            jsr     SETPTRS
            jsr     L0F30
            ldx     SCHANNEL
            cpx     #$01
            beq     L0F21
            jsr     ISCNTC
L0F1E:      jmp     L0E65

L0F21:      bit     L080A
            bpl     L0F1E
            jsr     L080B
L0F29:      jsr     L0FA3
            jmp     L0E68

            .byte   $AD

L0F30:      lda     TXTTAB
            ldy     TXTTAB+1
            sta     INDEX
            sty     INDEX+1
            clc
L0F39:      ldy     #$01
            lda     (INDEX),y
            beq     RET3
            ldy     #$04
L0F41:      iny
            lda     (INDEX),y
            bne     L0F41
            iny
            tya
            adc     INDEX
            tax
            ldy     #$00
            sta     (INDEX),y
            lda     INDEX+1
            adc     #$00
            iny
            sta     (INDEX),y
            stx     INDEX
            sta     INDEX+1
            bcc     L0F39
RET3:       rts

L0F5D:      jsr     _SETINPBCH
INLIN:      lda     #<INPUTBUFFER
            sta     INPBUFP
            lda     #>INPUTBUFFER
            sta     INPBUFP+1
            jsr     L03D1
            ldx     SCHANNEL
            jsr     _GETLINE
            bcc     L0F91
            jsr     _FREECH
            ldx     SCHANNEL
            cpx     #$01
            beq     L0F5D
            lda     #$01
            sta     SCHANNEL
            cpx     ICHANNEL
            bne     INLIN
            lda     #<QT_OK
            ldy     #>QT_OK
            jsr     CONSOLE_STROUT
            jmp     INLIN

L0F91:      tay
            lda     #$00
            sta     (INPBUFP),y
            lda     L03DF
            sta     INPBUFP
            lda     L03DF+1
            sta     INPBUFP+1
            jsr     L03CE
L0FA3:      ldx     #<L083B
            ldy     #>L083B
            rts

L0FA8:      stx     Z99
            sty     Z99+1
            jsr     L03D1
            ldx     SCHANNEL
            jsr     _GETCHAR
            bcc     L0FC4
            jsr     _FREECH
            jsr     _SETINPBCH
            ldx     #$01
            stx     SCHANNEL
            lda     #$00
L0FC4:      jsr     L03CE
            ldx     Z99
            ldy     Z99+1
            rts

            .byte   $DE

; ----------------------------------------------------------------------------
; TOKENIZE THE INPUT LINE
; ----------------------------------------------------------------------------
PARSE_INPUT_LINE:
            ldy     #$04
            sty     DATAFLG
            ldx     TXTPTR
L0FD3:      lda     #$08
            sta     Z9E
            lda     L0800,x
            cmp     #$20
            beq     L1028
            sta     ENDCHR
            cmp     #$22
            beq     L1055
            bit     DATAFLG
            bvs     L1028
            cmp     #$3F
            bne     L0FF0
            lda     #$97
            bne     L1028
L0FF0:      cmp     #$30
            bcc     L0FF8
            cmp     #$3C
            bcc     L1028
L0FF8:      sty     STRNG2
            jsr     L1098
            stx     TXTPTR
            ldy     #$00
            sty     EOLPNTR
            beq     L1007
L1005:      iny
            inx
L1007:      lda     L0800,x
            sec
            sbc     (Z9C),y
            beq     L1005
            cmp     #$80
            bne     L105C
            ora     EOLPNTR
            ldy     Z9E
            bmi     L1026
            pha
            lda     L0900,y
            ldy     STRNG2
            iny
            sta     L0837,y
            pla
            bne     L1028
L1026:      ldy     STRNG2
L1028:      inx
            iny
            sta     L0837,y
            lda     L0837,y
            beq     L108F
            sec
            sbc     #$3A
            beq     L103F
            cmp     #$49
            bne     L1041
            bit     Z9E
            bpl     L0FD3
L103F:      sta     DATAFLG
L1041:      sec
            sbc     #$54
            bne     L0FD3
            bit     Z9E
            bpl     L0FD3
            sta     ENDCHR
L104C:      lda     L0800,x
            beq     L1028
            cmp     ENDCHR
            beq     L1028
L1055:      iny
            sta     L0837,y
            inx
            bne     L104C
L105C:      ldx     TXTPTR
            inc     EOLPNTR
            dey
L1061:      iny
            lda     (Z9C),y
            bpl     L1061
            iny
            lda     (Z9C),y
            bne     L1007
            iny
            lda     (Z9C),y
            beq     L107E
            tya
            clc
            adc     Z9C
            sta     Z9C
            ldy     #$00
            bcc     L1007
            inc     Z9C+1
            bne     L1007
L107E:      jsr     L1098
            bcs     L108A
            ldy     #$00
            sty     EOLPNTR
            jmp     L1007

L108A:      lda     L0800,x
            bpl     L1026
L108F:      sta     L0839,y
            lda     #$3B
            sta     TXTPTR
            rts

            .byte   $B1

L1098:      ldy     Z9E
            bpl     L109E
            sec
            rts

L109E:      dey
            sty     Z9E
            bmi     L10BB
            lda     L0900,y
            bpl     L109E
            tya
            asl     a
            tay
            lda     L0908,y
            sta     Z9C
            lda     L0908+1,y
            sta     Z9C+1
L10B5:      ldy     #$00
            lda     (Z9C),y
            clc
            rts

L10BB:      lda     #<TOKEN_NAME_TABLE
            sta     Z9C
            lda     #>TOKEN_NAME_TABLE
            sta     Z9C+1
            bne     L10B5

; ----------------------------------------------------------------------------
; SEARCH FOR LINE
;
; (LINNUM) = LINE # TO FIND
; IF NOT FOUND:  CARRY = 0
;	LOWTR POINTS AT NEXT LINE
; IF FOUND:      CARRY = 1
;	LOWTR POINTS AT LINE
; ----------------------------------------------------------------------------
FNDLIN:     lda     TXTTAB
            ldx     TXTTAB+1
FL1:        ldy     #$01
            sta     LOWTR
            stx     LOWTR+1
            lda     (LOWTR),y
            beq     L10F2
            iny
            iny
            lda     LINNUM+1
            cmp     (LOWTR),y
            bcc     L10F3
            beq     L10E0
            dey
            bne     L10E9
L10E0:      lda     LINNUM
            dey
            cmp     (LOWTR),y
            bcc     L10F3
            beq     L10F3
L10E9:      dey
            lda     (LOWTR),y
            tax
            dey
            lda     (LOWTR),y
            bcs     FL1
L10F2:      clc
L10F3:      rts

; ----------------------------------------------------------------------------
; "NEW" STATEMENT
; ----------------------------------------------------------------------------
NEW:        bne     L10F3
SCRTCH:     lda     #$00
            tay
            sta     (TXTTAB),y
            iny
            sta     (TXTTAB),y
            lda     TXTTAB
            clc
            adc     #$02
            sta     VARTAB
            lda     TXTTAB+1
            adc     #$00
            sta     VARTAB+1
; ----------------------------------------------------------------------------
SETPTRS:    jsr     STXTPT
            lda     #$00

; ----------------------------------------------------------------------------
; "CLEAR" STATEMENT
; ----------------------------------------------------------------------------
CLEAR:      bne     L113C
CLEAR1:     lda     MEMSIZ
            ldy     MEMSIZ+1
            sta     FRETOP
            sty     FRETOP+1
            lda     VARTAB
            ldy     VARTAB+1
            sta     ARYTAB
            sty     ARYTAB+1
            sta     STREND
            sty     STREND+1
L1126:      jsr     RESTORE2
; ----------------------------------------------------------------------------
L1129:      ldx     #TEMPST
            stx     TEMPPT
            pla
            tay
            pla
            ldx     #STACK_TOP
            txs
            pha
            tya
            pha
            lda     #$00
            sta     OLDTEXT+1
            sta     SUBFLG
L113C:      rts

; ----------------------------------------------------------------------------
; SET TXTPTR TO BEGINNING OF PROGRAM
; ----------------------------------------------------------------------------
STXTPT:     clc
            lda     TXTTAB
            adc     #$FF
            sta     TXTPTR
            lda     TXTTAB+1
            adc     #$FF
            sta     TXTPTR+1
            rts

            .byte   "66713"

; Codos
L1150:      lda     TXTTAB
            sta     LOWTR
            lda     TXTTAB+1
            sta     LOWTR+1
            jsr     CHRGOT
            beq     L1178
            cmp     #$AE
            beq     L1170
            jsr     LINGET
            jsr     FNDLIN
            jsr     CHRGOT
            beq     L117E
            cmp     #$AE
            bne     L117F
L1170:      jsr     CHRGET
            beq     L1178
            jmp     LINGET

L1178:      lda     #$FF
            sta     LINNUM
            sta     LINNUM+1
L117E:      rts

L117F:      jmp     SYNERR

; ----------------------------------------------------------------------------
; "LIST" STATEMENT
; ----------------------------------------------------------------------------
LIST:       beq     L11A5
            bcc     L11A5
            cmp     #TOKEN_MINUS
            beq     L11A5
            jsr     L15E0
            ldx     ICHANNEL
            stx     OCHANNEL
            jsr     CHRGOT
            beq     L11A5
            cmp     #$2C
            beq     L11A2
            jsr     L1735
            jmp     SYNERR

L11A2:      jsr     CHRGET
L11A5:      jsr     L1150
            pla
            pla
L11AA:      ldy     #$01
            lda     (LOWTR),y
            beq     L11E9
            jsr     ISCNTC
            jsr     CRDO
            iny
            lda     (LOWTR),y
            tax
            iny
            lda     (LOWTR),y
            cmp     LINNUM+1
            bne     L11C5
            cpx     LINNUM
            beq     L11C7
L11C5:      bcs     L11E9
L11C7:      sty     FORPNT
            jsr     LINPRT
            lda     #$20
L11CE:      ldy     FORPNT
            and     #$7F
L11D2:      jsr     OUTDO
            iny
            beq     L1213
            lda     (LOWTR),y
            bne     L123C
            tay
            lda     (LOWTR),y
            tax
            iny
            lda     (LOWTR),y
            stx     LOWTR
            sta     LOWTR+1
            bne     L11AA
L11E9:      ldx     OCHANNEL
            cpx     #$02
            beq     L1207
            jsr     CRDO
            jsr     L03D1
            ldx     OCHANNEL
            jsr     L03F3
            ldx     OCHANNEL
            jsr     _FREECH
            ldx     #$02
            stx     OCHANNEL
L1207:      lda     LOWTR+1
            beq     L120E
            jmp     RESTART

L120E:      ldx     LOWTR
            jmp     CODOS_ERROR

L1213:      lda     #>DATPTR
            sta     LOWTR+1
            lda     #<DATPTR
            sta     LOWTR
            bne     L11E9
L121D:      jmp     L1BCA

            .byte   $A0                 ; Dead code

L1221:      ldy     L0A0C+1
            and     #$25
L1226:      ldx     #$E5
            eor     $BEDE,x
            ora     #$C6
            dey
            bne     L1226
            rts

L1231:      pha
            lda     #<TOKEN_NAME_TABLE
            sta     Z9C
            lda     #>TOKEN_NAME_TABLE
            sta     Z9C+1
            pla
            rts

L123C:      bpl     L11D2
            jsr     L1251
            bcs     L121D
            dey
L1244:      iny
            lda     (Z9C),y
            bpl     L124C
            jmp     L11CE

L124C:      jsr     OUTDO
            bne     L1244
L1251:      cmp     #$D0
            bcs     L125A
            jsr     L1231
            bne     L126A
L125A:      sty     FORPNT
            jsr     L1324
            bcs     L1272
            ldy     FORPNT
            iny
            beq     L1273
            lda     (LOWTR),y
            bpl     L1273
L126A:      sty     FORPNT
            jsr     L1276
            lda     FORPNT
            clc
L1272:      rts

L1273:      jmp     L1932

L1276:      sec
            sbc     #$7F
            tax
            ldy     #$FF
L127C:      dex
            bne     L1295
            iny
            lda     (Z9C),y
            bne     L1291
            iny
L1285:      tya
            ldy     #$00
            clc
            adc     Z9C
            sta     Z9C
            bcc     L1291
            inc     Z9C+1
L1291:      rts

L1292:      jsr     L1285
L1295:      iny
            lda     (Z9C),y
            beq     L1292
            bpl     L1295
            bmi     L127C

; ----------------------------------------------------------------------------
; "FOR" STATEMENT
;
; FOR PUSHES 18 BYTES ON THE STACK:
; 2 -- TXTPTR
; 2 -- LINE NUMBER
; 5 -- INITIAL (CURRENT)  FOR VARIABLE VALUE
; 1 -- STEP SIGN
; 5 -- STEP VALUE
; 2 -- ADDRESS OF FOR VARIABLE IN VARTAB
; 1 -- FOR TOKEN ($81)
; ----------------------------------------------------------------------------
FOR:        lda     #$80
            sta     SUBFLG
            lda     #$20            ; $20 = JSR
            cmp     L16D1           ; It is always $20, so comparison is always true!
            beq     L12AF           ; Always jump
            ldx     #<L31E2         ; Never executed
            txa                     ;
            jsr     L270C           ;
            
L12AF:      jsr     LET
            jsr     GTFORPNT
            bne     L12BC
            txa
            adc     #FOR_STACK1
            tax
            txs
L12BC:      pla
            pla
            lda     #FOR_STACK2
            jsr     CHKMEM
            jsr     DATAN
            clc
            tya
            adc     TXTPTR
            pha
            lda     TXTPTR+1
            adc     #$00
            pha
            lda     CURLIN+1
            pha
            lda     CURLIN
            pha
            lda     #TOKEN_TO
            jsr     SYNCHR
            jsr     CHKNUM
            jsr     FRMNUM
            lda     FACSIGN
            ora     #$7F
            and     FAC+1
            sta     FAC+1
            lda     #<STEP
            ldy     #>STEP
            sta     INDEX
            sty     INDEX+1
            jmp     FRM_STACK3

; ----------------------------------------------------------------------------
; "STEP" PHRASE OF "FOR" STATEMENT
; ----------------------------------------------------------------------------
STEP:       lda     #<CON_ONE
            ldy     #>CON_ONE
            jsr     LOAD_FAC_FROM_YA
            jsr     CHRGOT
            cmp     #TOKEN_STEP
            bne     L1308
            jsr     CHRGET
            jsr     FRMNUM
L1308:      jsr     SIGN
            jsr     FRM_STACK2
            lda     FORPNT+1
            pha
            lda     FORPNT
            pha
            lda     #$81
            pha
            bne     NEWSTT  ; Always jump

L1319:      ldy     #$07
L131B:      cmp     L0900,y
            beq     L1323
            dey
            bpl     L131B
L1323:      rts

L1324:      jsr     L1319
            bne     L1338
            tya
            asl     a
            tay
            lda     L0908,y
            sta     Z9C
            lda     L0908+1,y
            sta     Z9C+1
            clc
            rts

L1338:      sec
            rts

L133A:      jsr     L1319
            beq     L1342
            jmp     L1BCA

L1342:      lda     L0938,y
            sta     Z9E
            tya
            asl     a
            tay
            lda     L0918,y
            sta     Z9C
            lda     L0918+1,y
            sta     Z9C+1
            jmp     CHRGET

; ----------------------------------------------------------------------------
; PERFORM NEXT STATEMENT
; ----------------------------------------------------------------------------
NEWSTT:     jsr     ISCNTC
            lda     TXTPTR
            ldy     TXTPTR+1
            cpy     #>INPUTBUFFER
            beq     L1366
            sta     OLDTEXT
            sty     OLDTEXT+1
L1366:      ldy     #$00
            lda     (TXTPTR),y
            bne     COLON
            ldy     #$02
            lda     (TXTPTR),y
            clc
            bne     L1376
            jmp     L144B

L1376:      iny
            lda     (TXTPTR),y
            sta     CURLIN
            iny
            lda     (TXTPTR),y
            sta     CURLIN+1
L1380:      tya
            adc     TXTPTR
            sta     TXTPTR
            bcc     NEWSTT2
            inc     TXTPTR+1

NEWSTT2:    jsr     CHRGET
            jsr     EXECUTE_STATEMENT
            bit     L080E
            bpl     L1397
            jsr     L080F
L1397:      jmp     NEWSTT

; ----------------------------------------------------------------------------
; EXECUTE A STATEMENT
;
; (A) IS FIRST CHAR OF STATEMENT
; CARRY IS SET
; ----------------------------------------------------------------------------
EXECUTE_STATEMENT:
            bne     EXECUTE_STATEMENT1
            jmp     L1415

EXECUTE_STATEMENT1:
            cmp     #$80            ; Check if a token
            bcc     LET1            ; No, it should be a variable assignment
            cmp     #$D0            ; Check if over last token
            bcs     L13B8           ; Yes, maybe that is a library command?
            ldy     #$A5
            sty     Z9E
.ifdef mtu
            sta     SPREGREN-$A5,y  ; $A5+$22 = $C7
.endif

                                    ; Search for token in token table
L13AE:      ldy     #<TOKEN_ADDRESS_TABLE
            sty     Z9C
            ldy     #>TOKEN_ADDRESS_TABLE
            sty     Z9C+1
            bne     L13BB           ; Always jump

L13B8:      jsr     L133A

L13BB:      cmp     #$80            ; Check if a token
            bcc     SYNERR1         ; Should be, so syntax error
            cmp     Z9E 
            bcs     L13DD
            sec
            sbc     #$80
            asl     a
            tay
            iny
            lda     (Z9C),y
            pha
            dey
            lda     (Z9C),y
            pha
            jmp     CHRGET

LET1:       jmp     LET

COLON:      cmp     #$3A
            beq     NEWSTT2

SYNERR1:    jmp     SYNERR

L13DD:      ldy     #>TOKEN_ADDRESS_TABLE       ; May be $0A directly...
            cpy     Z9C+1
            bne     SYNERR1
            cmp     #TOKEN_GO
            bne     SYNERR1
            jsr     CHRGET
            lda     #TOKEN_TO
            jsr     SYNCHR
            jmp     GOTO

.ifdef kim1

CODE_SIZE = * - START

            .segment "CODE2"

            ; Loadable file data
            ;
            .byte   $58             ; CODOS loadable file header byte
            .byte   $00             ; Memory overlay
            .byte   $00             ; Memory bank
            .byte   $00             ; Reserved
            .addr   ENTRY           ; Entry point
            .addr   RESTORE         ; Load address
            .word   CODE2_SIZE      ; Memory image size

.endif

; ----------------------------------------------------------------------------
; "RESTORE" STATEMENT
; ----------------------------------------------------------------------------
RESTORE:    jsr     LINGET
            jsr     FNDLIN
            bcs     L1400
            lda     LINNUM
            ora     LINNUM+1
            bne     L1416
L1400:      sec
            lda     LOWTR
            ldy     LOWTR+1
            bne     L140C
RESTORE2:   sec
            lda     TXTTAB
            ldy     TXTTAB+1
L140C:      sbc     #$01
            bcs     SETDA
            dey
SETDA:      sta     DATPTR
            sty     DATPTR+1
L1415:      rts

L1416:      jmp     UNDERR

ISCNTC:     jsr     L352B
            bcc     L1422
            beq     L142F
            bcs     L1427
L1422:      lda     LSTKEY
            bne     L1429
L1427:      sta     Z9B
L1429:      rts

; ----------------------------------------------------------------------------
; "STOP" STATEMENT
; ----------------------------------------------------------------------------
STOP:       bcs     END2

; ----------------------------------------------------------------------------
; "END" STATEMENT
; ----------------------------------------------------------------------------
END:        clc

END2:       bne     RET1
L142F:      lda     TXTPTR
            ldy     TXTPTR+1
            php
            cpy     #$08
            bne     L143C
            plp
            jmp     END4

L143C:      plp
            sta     OLDTEXT
            sty     OLDTEXT+1
CONTROL_C_TYPED:      lda     CURLIN
            ldy     CURLIN+1
            sta     OLDLIN
            sty     OLDLIN+1

END4:       pla
            pla

L144B:      
.ifdef mtu
            jsr     TONE2
.endif
            lda     #<QT_BREAK
            ldy     #>QT_BREAK
            ldx     #$00
            stx     Z14
            bcc     L145B
            jmp     PRINT_ERROR_LINNUM

L145B:      jmp     RESTART

; ----------------------------------------------------------------------------
; "CONT" COMMAND
; ----------------------------------------------------------------------------
CONT:       bne     RET1
            ldx     #ERR_CANTCONT
            ldy     OLDTEXT+1
            bne     L1469
            jmp     ERROR

L1469:      lda     OLDTEXT
            sta     TXTPTR
            sty     TXTPTR+1
            lda     OLDLIN
            ldy     OLDLIN+1
            sta     CURLIN
            sty     CURLIN+1
RET1:       rts

; ----------------------------------------------------------------------------
; CODOS "EDIT" STATEMENT
; ----------------------------------------------------------------------------
EDIT:       jsr     LINGET
            jsr     CHRGOT
            beq     L1483
            jmp     SYNERR

L1483:      jsr     FNDLIN
            bcs     L148B
            jmp     UNDERR

L148B:      ldy     #$03
            lda     (LOWTR),y
            tax
            dey
            lda     (LOWTR),y
            tay
            txa
            jsr     L25FE
            ldy     #$01
            jsr     FOUT1
            ldy     #$01
            ldx     #$00
L14A1:      lda     STACK,y
            beq     L14AD
            sta     L083C,x
            iny
            inx
            bne     L14A1
L14AD:      lda     #$20
            sta     L083C,x
            inx
            ldy     #$04
L14B5:      lda     (LOWTR),y
            beq     L14F0
            bmi     L14C7
            sta     L083C,x
            iny
            inx
            cpx     L0813
            bne     L14B5
            beq     L14F0
L14C7:      stx     INDX
            jsr     L1251
            bcs     L1506
            sta     EOLPNTR
            ldx     INDX
L14D2:      lda     (Z9C),y
            bmi     L14E2
            sta     L083C,x
            iny
            inx
            cpx     L0813
            bne     L14D2
            beq     L14F0
L14E2:      and     #$7F
            sta     L083C,x
            ldy     EOLPNTR
            iny
            inx
            cpx     L0813
            bne     L14B5
L14F0:      txa
            tay
            lda     #<L083C
            sta     QLN
            lda     #>L083C
            sta     QLN+1
            jsr     EDLINE
            tay
            lda     #$00
            sta     L083C,y
            jmp     L0F29

L1506:      jmp     L1BCA

; ----------------------------------------------------------------------------
; CODOS "LOAD" STATEMENT
; ----------------------------------------------------------------------------
LOAD:       jsr     L1607
            ldy     #$00
            jsr     L03E6
            jsr     L1717
            lda     TXTPTR+1
            cmp     #$08
            bne     L1526
L151A:      jsr     L1592
            jsr     SETPTRS
            jsr     L0F30
            jmp     RESTART

L1526:      lda     VARTAB+1
            cmp     MEMBUFF+1
            bcc     L151A
            bne     L1534
            lda     VARTAB
            cmp     MEMBUFF
            bcc     L151A
L1534:      jsr     L0F30
            jsr     STXTPT
            jmp     L1126

; ----------------------------------------------------------------------------
; CODOS "SAVE" STATEMENT
; ----------------------------------------------------------------------------
SAVE:       sty     Z9C+1
            sty     Z9C
            jsr     L15E0
            bit     L0822
            bpl     L154C
            jmp     _ERROR12

L154C:      jsr     L1659
            jsr     L1624
            ldx     ICHANNEL
            jsr     _OUTMBUFF
            lda     L09BD
            beq     L157E
            clc
            lda     Z9C+1
            adc     #$2B
            sta     Z9C+1
            ldy     #$2F
L1566:      adc     (Z9C),y
            iny
            dec     Z9C
            cpy     #$3A
            bne     L1566
            ldx     ICHANNEL
L1572:      lda     (Z9C),y
            jsr     _OUTCHAR
            iny
            dec     Z9C
            cpy     #$3F
            bne     L1572
L157E:      ldx     ICHANNEL
            jsr     L03F3
            jmp     L1735

L1587:      lda     L03DF
            sta     INPBUFP
            lda     L03DF+1
            sta     INPBUFP+1
            rts

L1592:      lda     MEMBUFF
            sta     VARTAB
            lda     MEMBUFF+1
            sta     VARTAB+1
            rts

; ----------------------------------------------------------------------------
; CODOS "ENTER" STATEMENT
; ----------------------------------------------------------------------------
ENTER:      jsr     L1607
            ldx     ICHANNEL
            stx     SCHANNEL
            ldx     TXTPTR+1
            cpx     #$08
            beq     L15AB
            rts

L15AB:      pla
            pla
            jmp     L0E65

L15B0:      jsr     FRMEVL
            jsr     FRESTR
            jsr     L1637
L15B9:      lda     INPBUFP
            sta     TMPBUFP
            lda     INPBUFP+1
            sta     TMPBUFP+1
            jsr     L03D1
            jsr     _FSCAN
            sta     L0822
            bit     L0822
            bcs     L15D4
            bmi     L15D7
            bvs     L15DA
            rts

L15D4:      bmi     L15DD
            rts

L15D7:      jmp     _ERROR12

L15DA:      jmp     _ERROR03

L15DD:      jmp     _ERROR11

L15E0:      jsr     L15B0
            bcs     L15FB
            asl     a
            asl     a
            bpl     L15F6
            bit     L0812
            bmi     L15F6
            jsr     L03CE
            ldx     #ERR_CODOS_FEXISTS
            jmp     CODOS_ERROR

L15F6:      lda     L0822
            and     #$03
L15FB:      rol     L0822
            jsr     L03DB
            ldx     ICHANNEL
            jmp     _ASSIGN

L1607:      jsr     L15B0
            bcs     L15D7
            asl     a
            asl     a
            bmi     L15F6
            jsr     L03CE
            ldx     #ERR_CODOS_NFOUND
            jmp     CODOS_ERROR

L1618:      ldy     #$03
L161A:      lda     L0814,y
            sta     MEMBUFF,y
            dey
            bpl     L161A
            rts

L1624:      lda     TXTTAB
            sta     MEMBUFF
            lda     TXTTAB+1
            sta     MEMBUFF+1
            lda     L0820
            sta     MEMCOUNT
            lda     L0820+1
            sta     MEMCOUNT+1
            rts

L1637:      tay
            beq     L1656
            jsr     L1587
            lda     #$0D
            sta     (INPBUFP),y
            dey
L1642:      lda     (INDEX),y
            sta     (INPBUFP),y
            dey
            bpl     L1642
            iny
            lda     #$20
L164C:      cmp     (INPBUFP),y
            bne     L1655
            inc     INPBUFP
            jmp     L164C

L1655:      rts

L1656:      jmp     SYNERR

L1659:      lda     #$42
            sta     L0818
            lda     #$00
            sta     L0818+1
            sta     L0818+2
            sta     L0818+3
            ldy     #$02
            lda     (TXTTAB),y
            sta     L0818+4
            iny
            lda     (TXTTAB),y
            sta     L0818+5
            lda     TXTTAB
            sta     L0818+6
            lda     TXTTAB+1
            sta     L0818+7
            sec
            lda     VARTAB
            sbc     TXTTAB
            sta     L0818+8
            lda     VARTAB+1
            sbc     TXTTAB+1
            sta     L0818+9
            jsr     L1618
            ldx     ICHANNEL
            jmp     _OUTMBUFF

L1698:      lda     L0818
            cmp     #$42
            beq     L16A7
            jsr     L1735
            ldx     #ERR_CODOS_NBASIC
            jmp     CODOS_ERROR

L16A7:      clc
            lda     TXTTAB
            adc     L0820
            tay
            lda     TXTTAB+1
            adc     L0820+1
            cmp     MEMSIZ+1
            bcc     L16BF
            jsr     L1735
            ldx     #ERR_CODOS_LARGE
            jmp     CODOS_ERROR

L16BF:      rts

; ----------------------------------------------------------------------------
; "RUN" COMMAND
; ----------------------------------------------------------------------------
RUN:        bne     L16D7

L16C2:      jsr     L1E38
            ldy     #$00
            ldx     #$00
L16C9:      adc     (LOWTR),y
            inx
            cpx     L0D3D
            bne     L16C9

L16D1:      jsr     INTEGRITY_CHK
            jmp     SETPTRS

L16D7:      bcc     L1711
            jsr     L1607
            ldy     #$00
            jsr     L03E6
            jsr     CHRGOT
            beq     L16F3
            cmp     #$2C
            beq     L16F0
            jsr     L1735
            jmp     SYNERR

L16F0:      jsr     CHRGET
L16F3:      jsr     LINGET
            jsr     L1717
            jsr     L1735
            jsr     L1592
            jsr     L0F30
            lda     LINNUM
            ora     LINNUM+1
            beq     L16C2
            jsr     CLEAR1
            jsr     L177F
            jmp     NEWSTT

L1711:      jsr     CLEAR1
            jmp     L175F

L1717:      jsr     L1618
            ldx     ICHANNEL
            jsr     _GETMBUFF
            bcs     L172D
            jsr     L1698
            jsr     L1624
            jsr     _GETMBUFF
            bcc     L1735
L172D:      jsr     L1735
            ldx     #ERR_CODOS_LOAD
            jmp     CODOS_ERROR

L1735:      ldx     ICHANNEL
            jsr     _FREECH
            jmp     L03CE

; ----------------------------------------------------------------------------
; "GOSUB" STATEMENT
;
; LEAVES 7 BYTES ON STACK:
; 2 -- RETURN ADDRESS (NEWSTT)
; 2 -- TXTPTR
; 2 -- LINE #
; 1 -- GOSUB TOKEN
; ----------------------------------------------------------------------------
GOSUB:      lda     #$03
            jsr     CHKMEM
            lda     #$20            ; What??? L2197 always contains JSR, so this
            cmp     L2197           ; comparison is always true!
            beq     L1750           ;
            ldy     #<L31E2
            tya
            jsr     L270C
L1750:      lda     TXTPTR+1
            pha
            lda     TXTPTR
            pha
            lda     CURLIN+1
            pha
            lda     CURLIN
            pha
            lda     #TOKEN_GOSUB
            pha
L175F:      jsr     CHRGOT
            jsr     GOTO
            jmp     NEWSTT

; ----------------------------------------------------------------------------
; "GOTO" STATEMENT
; ALSO USED BY "RUN" AND "GOSUB"
; ----------------------------------------------------------------------------
GOTO:       jsr     LINGET
            jsr     REMN
            lda     CURLIN+1
            cmp     LINNUM+1
            bcs     L177F
            tya
            sec
            adc     TXTPTR
            ldx     TXTPTR+1
            bcc     L1783
            inx
            bcs     L1783
L177F:      lda     TXTTAB
            ldx     TXTTAB+1
L1783:      jsr     FL1
            bcc     UNDERR
            lda     LOWTR
            sbc     #$01
            sta     TXTPTR
            lda     LOWTRX+1
            sbc     #$00
            sta     TXTPTR+1
L1794:      rts

; ----------------------------------------------------------------------------
; "POP" AND "RETURN" STATEMENTS
; ----------------------------------------------------------------------------
POP:        bne     L1794
            lda     #$FF
            sta     FORPNT+1
            jsr     GTFORPNT
            txs
            cmp     #TOKEN_GOSUB    ; $8C
            beq     RETURN
            ldx     #ERR_NOGOSUB
            .byte   $2C
UNDERR:     ldx     #ERR_UNDEFSTAT
            jmp     ERROR

; ----------------------------------------------------------------------------
SYNERR2:    jmp     SYNERR

; ----------------------------------------------------------------------------
RETURN:     pla
            pla
            sta     CURLIN
            pla
            sta     CURLIN+1
            pla
            sta     TXTPTR
            pla
            sta     TXTPTR+1

; ----------------------------------------------------------------------------
; "DATA" STATEMENT
; EXECUTED BY SKIPPING TO NEXT COLON OR EOL
; ----------------------------------------------------------------------------
DATA:       jsr     DATAN

; ----------------------------------------------------------------------------
; ADD (Y) TO TXTPTR
; ----------------------------------------------------------------------------
ADDON:      tya
            clc
            adc     TXTPTR
            sta     TXTPTR
            bcc     L17C8
            inc     TXTPTR+1
L17C8:      rts

; ----------------------------------------------------------------------------
; SCAN AHEAD TO NEXT ":" OR EOL
; ----------------------------------------------------------------------------
DATAN:      ldx     #$3A
            .byte   $2C

REMN:       ldx     #$00
            stx     CHARAC
            ldy     #$00
            sty     ENDCHR
L17D4:      lda     ENDCHR
            ldx     CHARAC
            sta     CHARAC
            stx     ENDCHR
L17DC:      lda     (TXTPTR),y
            beq     L17C8
            cmp     ENDCHR
            beq     L17C8
            iny
            cmp     #$22
            bne     L17DC
            beq     L17D4
            ; Not reached

            .byte   $C1             ; Dead code

; ----------------------------------------------------------------------------
; "IF" STATEMENT
; ----------------------------------------------------------------------------
IF:         jsr     FRMEVL
            jsr     CHRGOT
            cmp     #TOKEN_GOTO     ; $88
            beq     L17FB
            lda     #TOKEN_THEN     ; $AA
            jsr     SYNCHR
L17FB:      lda     FAC
            bne     L1804

; ----------------------------------------------------------------------------
; "REM" STATEMENT, OR FALSE "IF" STATEMENT
; ----------------------------------------------------------------------------
REM:        jsr     REMN
            beq     ADDON
L1804:      jsr     CHRGOT
            bcs     L180C
            jmp     GOTO

L180C:      jmp     EXECUTE_STATEMENT

; ----------------------------------------------------------------------------
; "ON" STATEMENT
;
; ON <EXP> GOTO <LIST>
; ON <EXP> GOSUB <LIST>
; ----------------------------------------------------------------------------
ON:         jsr     GETBYT
            pha
            cmp     #TOKEN_GOSUB    ; $8C
            beq     L181B
L1817:      cmp     #TOKEN_GOTO     ; $88
            bne     SYNERR2
L181B:      dec     FAC_LAST
            bne     L1823
            pla
            jmp     EXECUTE_STATEMENT1

L1823:      jsr     CHRGET
            jsr     LINGET
            cmp     #$2C
            beq     L181B
            pla
L182E:      rts

; ----------------------------------------------------------------------------
; CONVERT LINE NUMBER
; ----------------------------------------------------------------------------
LINGET:     ldx     #$00
            stx     LINNUM
            stx     LINNUM+1
L1835:      bcs     L182E
            sbc     #$2F
            sta     CHARAC
            lda     LINNUM+1
            sta     INDEX
            cmp     #$19
            bcs     L1817
; <<<<<DANGEROUS CODE>>>>>
; NOTE THAT IF (A) = $AB ON THE LINE ABOVE,
; ON.1 WILL COMPARE = AND CAUSE A CATASTROPHIC
; JUMP TO $22D9 (FOR GOTO), OR OTHER LOCATIONS
; FOR OTHER CALLS TO LINGET.
;
; YOU CAN SEE THIS IS YOU FIRST PUT "BRK" IN $22D9,
; THEN TYPE "GO TO 437761".
;
; ANY VALUE FROM 437760 THROUGH 440319 WILL CAUSE
; THE PROBLEM.  ($AB00 - $ABFF)
; <<<<<DANGEROUS CODE>>>>>
            lda     LINNUM
            asl     a
            rol     INDEX
            asl     a
            rol     INDEX
            adc     LINNUM
            sta     LINNUM
            lda     INDEX
            adc     LINNUM+1
            sta     LINNUM+1
            asl     LINNUM
            rol     LINNUM+1
            lda     LINNUM
            adc     CHARAC
            sta     LINNUM
            bcc     L1863
            inc     LINNUM+1
L1863:      jsr     CHRGET
            jmp     L1835


L1869:      pha
            txa
            pha
            jmp     L1885

L186F:  .byte   $0F

; ----------------------------------------------------------------------------
; "LET" STATEMENT
;
; LET <VAR> = <EXP>
; <VAR> = <EXP>
; ----------------------------------------------------------------------------
LET:        jsr     PTRGET
            sta     FORPNT
            sty     FORPNT+1
            lda     #TOKEN_EQUAL
            jsr     SYNCHR
            lda     VALTYP+1
            pha
            lda     VALTYP
            pha
            jsr     FRMEVL
L1885:      pla
            rol     a
            jsr     CHKVAL
            bne     LETSTRING
            pla
LET2:       bpl     L18A1
            jsr     ROUND_FAC
            jsr     AYINT
            ldy     #$00
            lda     FAC+3
            sta     (FORPNT),y
            iny
            lda     FAC+4
            sta     (FORPNT),y
            rts

; ----------------------------------------------------------------------------
; REAL VARIABLE = EXPRESSION
; ----------------------------------------------------------------------------
L18A1:      jmp     SETFOR

LETSTRING:  pla

; ----------------------------------------------------------------------------
; INSTALL STRING, DESCRIPTOR ADDRESS IS AT FAC+3,4
; ----------------------------------------------------------------------------
PUTSTR:     ldy     #$02
            lda     (FAC_LAST-1),y
            cmp     FRETOP+1
            bcc     L18C4
            bne     L18B6
            dey
            lda     (FAC_LAST-1),y
            cmp     FRETOP
            bcc     L18C4
L18B6:      ldy     FAC_LAST
            cpy     VARTAB+1
            bcc     L18C4
            bne     L18CB
            lda     FAC+3
            cmp     VARTAB
            bcs     L18CB
L18C4:      lda     FAC_LAST-1
            ldy     FAC_LAST
            jmp     L18E1

L18CB:      ldy     #$00
            lda     (FAC_LAST-1),y
            jsr     STRINI
            lda     DSCPTR
            ldy     DSCPTR+1
            sta     SGNCPR
            sty     FACEXTENSION
            jsr     MOVINS
            lda     #FAC
            ldy     #$00
L18E1:      sta     DSCPTR
            sty     DSCPTR+1
            jsr     FRETMS
            ldy     #$00
            lda     (DSCPTR),y
            sta     (FORPNT),y
            iny
            lda     (DSCPTR),y
            sta     (FORPNT),y
            iny
            lda     (DSCPTR),y
            sta     (FORPNT),y
            rts

; CODOS specific stuff

L18F9:      .byte   $0A

L18FA:      ldx     #$00
            lda     VARTAB+1
            sec
            sbc     L09C9
            bcc     L1918
            adc     #$00
            sta     L09CC
            cmp     #$61
            bcs     L1932

            lda     #$80
L190F:      sta     L0950,x
            inx
            cpx     L09CC
            bne     L190F
L1918:      clc
            lda     Z9E
            adc     #$1A
            sta     Z9C+1
            adc     #$04
            sta     Z9C
            lda     #$80
L1925:      cmp     L0950,x
            bne     L1937
            asl     L0950,x
            inx
            cpx     #$61
            bcc     L1925

L1932:      ldx     #ERR_CODOS_INTEGRITY
            jmp     CODOS_ERROR

L1937:      rts

L1938:      jsr     L03D1
            lda     #'Z'
            jsr     SETDEFEXT
            jsr     L1607
            lda     #'B'
            jsr     SETDEFEXT
            lda     #$00
            sta     L09CD
            sta     L09CE
            sta     L09CF
            rts

L1954:      ldx     #$03
L1956:      lda     L09B0,x
            sta     MEMBUFF,x
            dex
            bpl     L1956
            rts

L195F:      ldx     #$00
L1961:      lda     L0900,x
            bpl     L1975
            cmp     L09BF
            beq     L1977
            inx
            cpx     #$08
            bne     L1961
            ldx     #ERR_CODOS_MNYLIBS
            jmp     CODOS_ERROR

L1975:      clc
            rts

L1977:      sec
            rts

L1979:      lda     L09BB
            sec
            sbc     L09C9
            bcc     L19B7
            cmp     #$61
            bcs     L19B7
            sta     L09CA
            tax
            lda     L09BA
            adc     L09BC
            php
            sec
            sbc     #$01
            lda     L09BB
            sbc     #$00
            plp
            adc     L09BD
            sec
            sbc     L09C9
            cmp     #$61
            bcs     L19B7
            adc     #$01
            sta     L09CB
L19AA:      lda     L0950,x
            bmi     L19B7
            inx
            cpx     L09CB
            bne     L19AA
            clc
            rts

L19B7:      sec
            rts

L19B9:      ldx     L09CA
            lda     L09BF
L19BF:      sta     L0950,x
            inx
            cpx     L09CB
            bne     L19BF
            rts

L19C9:      lda     L09BF
            sta     L0900,x
            lda     L09C6
            sta     L0938,x
            txa
            asl     a
            tax
            clc
            ldy     #$00
L19DB:      lda     L09C0,y
            sta     L0908,x
            iny
            lda     L09C0,y
            sta     L0908+1,x
            iny
            cpy     #$06
            beq     L19F5
            bcs     L19FC
            txa
            adc     #$10
            tax
            bne     L19DB
L19F5:      iny
            txa
            adc     #$17
            tax
            bne     L19DB
L19FC:      rts

L19FD:      clc
            lda     L09CD
            adc     MEMCOUNT
            sta     L09CD
            lda     L09CE
            adc     MEMCOUNT+1
            sta     L09CE
            bcc     L1A13
            inc     L09CF
L1A13:      rts

L1A14:      clc
            lda     L09BA
            adc     #$0B
            sta     L09BA
            bcc     L1A22
            inc     L09BB
L1A22:      sec
            lda     L09BC
            sbc     #$0B
            sta     L09BC
            bcs     L1A30
            dec     L09BD
L1A30:      rts

L1A31:      clc
            lda     L09BC
            adc     L09CD
            sta     L09CD
            sta     FILEPOS
            lda     L09BD
            adc     L09CE
            sta     L09CE
            sta     FILEPOS+1
            lda     L09CF
            adc     #$00
            sta     L09CF
            sta     FILEPOS+2
            ldx     ICHANNEL
            jmp     _FSEEK

L1A58:      jsr     L1A14
            jsr     L1979
            bcc     L1A7C
            jsr     L1A31
            jsr     L1954
            ldx     ICHANNEL
            jsr     _GETMBUFF
            bcs     L1A74
            jsr     L19FD
            jmp     L1A58

L1A74:      jsr     L1735
            ldx     #ERR_CODOS_LIBLOAD
            jmp     CODOS_ERROR

L1A7C:      lda     L09BA
            sta     MEMBUFF
            lda     L09BB
            sta     MEMBUFF+1
            cmp     MEMSIZ+1
            bcc     L1A94
            bne     L1A9A
            lda     MEMBUFF
            cmp     MEMSIZ
            bcs     L1A9A
            lda     MEMBUFF+1
L1A94:      sta     MEMSIZ+1
            lda     MEMBUFF
            sta     MEMSIZ
L1A9A:      ror     a
            and     L09D0
            sta     L09D0
            lda     L09BC
            sta     MEMCOUNT
            lda     L09BD
            sta     MEMCOUNT+1
            ldx     ICHANNEL
            jsr     _GETMBUFF
            bcs     L1ACF
            jsr     L19B9
            jsr     L1735
            ldx     #$12
L1ABB:      asl     L09BF,x
            lda     GETARY2+1,x
            eor     #$80
            cmp     L09BF,x
            bne     L1AD4
            inx
            cpx     L241D
            bne     L1ABB
            rts

L1ACF:      ldx     #ERR_CODOS_LOAD
            jmp     CODOS_ERROR

L1AD4:      lda     #$31
            sta     CHRGOT2+6
            eor     #$FF
            tax
            inx
            stx     CHRGOT2+9
L1ADE:      rts

; ----------------------------------------------------------------------------
; CODOS "LIB" STATEMENT
; ----------------------------------------------------------------------------
LIB:        beq     L1B4D
            jsr     L09F9
L1AE4:      jsr     L1938
            jsr     L1221
            jsr     L1954
            ldx     ICHANNEL
            jsr     _GETMBUFF
            bcs     L1ACF
            lda     L09BE
            cmp     #INPBUFP+1
            bne     L1B48
            jsr     L19FD
            jsr     L195F
            bcs     L1B28
            stx     Z9E
            ldx     L18F9
            ldy     #$00
L1B0B:      lda     CHRGOT2+2       ; Does it makes any sense? $87
            ora     (Z9C),y
            sta     L09C7,x
            cmp     L2C8E,x
            bne     L1B3B
            inx
            cpx     L186F
            bne     L1B0B
L1B1D:      jsr     L1A58
            ldx     Z9E
            jsr     L19C9
            jsr     L1B45
L1B28:      jsr     CHRGOT
            beq     L1B35
            jsr     CHKCOM
            sta     (Z9C),y
            jmp     L1AE4

L1B35:      bit     L09D0
            bpl     L1B42
            rts

L1B3B:      lda     #$17
            sta     L13AE+1
            bne     L1B1D
L1B42:      jmp     CLEAR1

L1B45:      jmp     (L09B8)

L1B48:      ldx     #ERR_CODOS_NOTLIB
            jmp     CODOS_ERROR

L1B4D:      ldx     #$07
L1B4F:      lda     L0900,x
            bmi     L1B58
            dex
            bpl     L1B4F
            rts

L1B58:      stx     Z9E
            jsr     CRDO
L1B5D:      lda     Z9E
            asl     a
            tax
            lda     L0940,x
            ldy     L0940+1,x
            sta     INDEX
            sty     INDEX+1
            ldy     #$00
L1B6D:      lda     (INDEX),y
            beq     L1B77
            jsr     OUTDO
            iny
            bne     L1B6D
L1B77:      jsr     CRDO
            dec     Z9E
            bpl     L1B5D
            rts

; ----------------------------------------------------------------------------
L1B7F:      ldx     #$5F
            lda     #$00
L1B83:      sta     L0950,x         ; Init variables from L0950 to L09AF
            dex
            bpl     L1B83

            ldx     #$07            ; Same for variables from $0900 to $0907
L1B8B:      sta     L0900,x
            dex
            bpl     L1B8B

            lda     TOPMEM+1
            sec
            sbc     #$60            ; Substract 24K bytes
            sta     L09C9
            rts

; ----------------------------------------------------------------------------
; CODOS "FRELIB" STATEMENT
; ----------------------------------------------------------------------------
FRELIB:     beq     L1BA0
            jmp     SYNERR

L1BA0:      jsr     L1B7F
            lda     #$00
            sta     L080A
            sta     L080E
            lda     TXTPTR+1
            cmp     #$08
            beq     L1BB2
            rts

L1BB2:      lda     TOPMEM
            sta     MEMSIZ
            lda     TOPMEM+1
            sta     MEMSIZ+1
            jsr     L03D1
            ldy     #$00
            jsr     L03E6
            jsr     L03CE
            jmp     CLEAR1

; ----------------------------------------------------------------------------
L1BCA:      sec
            sbc     #$D0
            sta     Z9E
            jsr     L1C0F
            bcs     L1BF9
L1BD4:      jsr     _GETLINE
            bcs     L1BF9
            dec     Z9E
            bpl     L1BD4
            tay
            lda     #$20
            sta     (INPBUFP),y
            iny
            lda     #$00
            sta     (INPBUFP),y
            lda     INPBUFP
            ldy     INPBUFP+1
            jsr     STROUT
L1BEE:      jsr     L03D1
            jsr     L1735
            ldx     #ERR_CODOS_NLOADED
            jmp     CODOS_ERROR

L1BF9:      jsr     CHRGOT
            and     #$7F
            sta     Z77
            jmp     L1BEE

L1C03:  .byte   "SYSLIBNAM.Z", $0D

L1C0F:      jsr     L1C2A
            jsr     L03D1
            jsr     L15B9
            bcs     L1C28
            bvs     L1C28
            asl     a
            asl     a
            bpl     L1C28
            ldx     ICHANNEL
            jsr     _ASSIGN
            clc
            rts

L1C28:      sec
            rts

L1C2A:      jsr     L1587
            ldy     #$0C
L1C2F:      lda     L1C03,y
            sta     (INPBUFP),y
            dey
            bpl     L1C2F
            iny
            rts

.ifdef mtu
; ----------------------------------------------------------------------------
; CODOS "LEGEND" STATEMENT
; ----------------------------------------------------------------------------
LEGEND:     jsr     GETBYT
            cpx     #$00
            bne     L1C43
L1C40:      jmp     IQERR

L1C43:      cpx     #$09
            bcs     L1C40
            dex
            stx     Z9E
            jsr     CHKCOM
L1C4D:      jsr     FRMEVL
            jsr     FRESTR
            cmp     #$09
            bcc     L1C59
            lda     #$08
L1C59:      sta     LOWTR
            jsr     L1C77
            bcs     L1C6E
            jsr     CHRGOT
            beq     L1C74
            cmp     #$2C
            bne     L1C6E
            jsr     CHRGET
            bne     L1C4D
L1C6E:      jsr     DRWLEG
            jmp     SYNERR

L1C74:      jmp     DRWLEG

L1C77:      lda     Z9E
            cmp     #$08
            bcs     L1C9E
            asl     a
            asl     a
            asl     a
            sta     LOWTR+1
            tax
            ldy     #$00
            jsr     L1C9F
            lda     LOWTR
            beq     L1C9B
            ldx     LOWTR+1
            ldy     #$00
L1C90:      lda     (INDEX),y
            sta     LEGTBL,x
            inx
            iny
            cpy     LOWTR
            bne     L1C90
L1C9B:      inc     Z9E
            clc
L1C9E:      rts

L1C9F:      lda     #$20
L1CA1:      sta     LEGTBL,x
            inx
            iny
            cpy     #$08
            bne     L1CA1
            rts
.endif

; ----------------------------------------------------------------------------
; CODOS "OUTCHAN" STATEMENT
; ----------------------------------------------------------------------------
OUTCHAN:    jsr     GETBYT
            stx     OCHANNEL
            rts

; ----------------------------------------------------------------------------
PRSTRING:   jsr     STRPRT
L1CB5:      jsr     CHRGOT
            beq     CRDO
            cmp     #$2C
            beq     L1D03
            cmp     #$3B
            beq     L1D32
            jmp     SYNERR

; ----------------------------------------------------------------------------
; "PRINT" STATEMENT
; ----------------------------------------------------------------------------
PRINT:      beq     CRDO
PRINT2:     beq     L1D02
            cmp     #TOKEN_TAB
            beq     L1D1A
            cmp     #TOKEN_SPC
            clc
            beq     L1D1A
            cmp     #','
            beq     L1D03
            cmp     #$3B
            beq     L1D32
            jsr     FRMEVL
            bit     VALTYP
            bmi     PRSTRING
            jsr     FOUT
            jsr     STRLIT
            ldy     #$00
            lda     (FAC_LAST-1),y
            clc
            adc     Z15
            cmp     POSX
            bcc     L1CF5
            jsr     CRDO
L1CF5:      jsr     STRPRT
            jsr     OUTSP
            bne     L1CB5

; Output CR
;
CRDO:       lda     #CRLF_2
            jsr     OUTDO
L1D02:      rts

L1D03:      lda     Z15
            cmp     Z17
            bcc     L1D0F
L1D09:      jsr     CRDO
            jmp     L1D32

L1D0F:      sec
L1D10:      sbc     #$0A
            bcs     L1D10
            eor     #$FF
            adc     #$01
            bne     L1D2D
L1D1A:      php
            jsr     GTBYTC
            cmp     #')'
            beq     L1D25
            jmp     SYNERR

L1D25:      plp
            bcc     L1D2E
            txa
            sbc     Z15
            bcc     L1D32
L1D2D:      tax
L1D2E:      inx
L1D2F:      dex
            bne     L1D38
L1D32:      jsr     CHRGET
            jmp     PRINT2

L1D38:      jsr     OUTSP
            bne     L1D2F

; ----------------------------------------------------------------------------
; CODOS STRCONS PRINT STRING AT (Y,A) IF OUTPUT CHANNEL IS CONSOLE
; ----------------------------------------------------------------------------
CONSOLE_STROUT:
            ldx     #$02
            cpx     OCHANNEL
            beq     STROUT
            rts

; ----------------------------------------------------------------------------
; PRINT STRING AT (Y,A)
; ----------------------------------------------------------------------------
STROUT:     jsr     STRLIT

; ----------------------------------------------------------------------------
; PRINT STRING AT (FACMO,FACLO)
; ----------------------------------------------------------------------------
STRPRT:     jsr     FREFAC
            tax
            ldy     #$00
            inx
L1D4F:      dex
            beq     L1D02
            lda     (INDEX),y
            jsr     OUTDO
            iny
            bne     L1D4F

; ----------------------------------------------------------------------------
OUTSP:      lda     #$20
            .byte   $2C                 ; Skip next instruction
OUTQUES:    lda     #$3F

; ----------------------------------------------------------------------------
; PRINT CHAR FROM (A)
; ----------------------------------------------------------------------------
OUTDO:      bit     Z14
            bmi     L1D88           ; If set, no output
            pha
            cmp     #$20
            bcc     L1D8D
            lda     Z15
            cmp     POSX
            bne     L1D71
            jsr     CRDO
L1D71:      inc     Z15
L1D73:      pla
            stx     Z99
            sty     Z99+1
            jsr     L03D1
            ldx     OCHANNEL
            jsr     _OUTCHAR
            jsr     L03CE
            ldy     Z99+1
            ldx     Z99
L1D88:      and     #$FF
            rts


INTEGRITY_CHKSUM:
            .word   CHECKSUM

L1D8D:      cmp     BSPACE
            beq     L1DAF
            cmp     #$7F
            beq     L1DAF
            cmp     #$0D
            beq     L1DA9
            cmp     #$0C
            beq     L1DA9
            cmp     CANCEL
            beq     L1DA9
            cmp     #$09
            beq     L1D71
            bne     L1D73
L1DA9:      lda     #$00
            sta     Z15
            beq     L1D73
L1DAF:      lda     Z15
            bne     L1DB6
            pla
            bne     L1D88
L1DB6:      dec     Z15
            jmp     L1D73

; ----------------------------------------------------------------------------
; INPUT CONVERSION ERROR:  ILLEGAL CHARACTER
; IN NUMERIC FIELD.  MUST DISTINGUISH
; BETWEEN INPUT, READ, AND GET
; ----------------------------------------------------------------------------
INPUTERR:   lda     INPUTFLG
            beq     RESPERR
            bmi     L1DC5
            ldy     #$FF
            bne     L1DC9
L1DC5:      lda     Z43
            ldy     Z43+1
L1DC9:      sta     CURLIN
            sty     CURLIN+1
            jmp     SYNERR

RESPERR:    lda     SCHANNEL
            cmp     #$01
            beq     L1DDC
            ldx     #ERR_BADDATA
            jmp     ERROR

L1DDC:      lda     #<ERRREENTRY
            ldy     #>ERRREENTRY
            jsr     STROUT
            lda     OLDTEXT
            ldy     OLDTEXT+1
            sta     TXTPTR
            sty     TXTPTR+1
            rts

; ----------------------------------------------------------------------------
; "GET" STATEMENT
; ----------------------------------------------------------------------------
GET:        jsr     ERRDIR
            ldx     #<(INPUTBUFFER+1)
            ldy     #>(INPUTBUFFER+1)
            lda     #$00
            sta     INPUTBUFFER+1
            lda     #$40
            jmp     PROCESS_INPUT_LIST

            .byte   $EF             ; Dead code

; ----------------------------------------------------------------------------
; "INPUT" STATEMENT
; ----------------------------------------------------------------------------
INPUT:      lsr     Z14             ; Clear no output flag
            cmp     #$22
            bne     L1E0F
            jsr     STRTXT
            lda     #$3B
            jsr     SYNCHR
            jsr     STRPRT
L1E0F:      jsr     ERRDIR
            lda     #$2C
            sta     INPUTBUFFER-1
            jsr     NXIN
            lda     INPUTBUFFER
            bne     L1E53
            bit     L0800
            bmi     L1E53
            clc
            jmp     CONTROL_C_TYPED

NXIN:       lda     SCHANNEL
            cmp     #$01
            bne     L1E35
            jsr     OUTQUES
            jsr     OUTSP
L1E35:      jmp     INLIN

; ----------------------------------------------------------------------------
L1E38:      clc
            lda     Z9E
            adc     #$1A
            sta     LOWTR+1
            eor     #$7F
            adc     #$03
            sta     LOWTR
            lda     #$00
            sta     Z9C
            sta     Z9C+1
            rts

; ----------------------------------------------------------------------------
; "READ" STATEMENT
; ----------------------------------------------------------------------------
READ:       ldx     DATPTR
            ldy     DATPTR+1
            lda     #$98    ; READ
            .byte   $2C
L1E53:      lda     #$00    ; INPUT

; ----------------------------------------------------------------------------
; PROCESS INPUT LIST
;
; (Y,X) IS ADDRESS OF INPUT DATA STRING
; (A) = VALUE FOR INPUTFLG:  $00 FOR INPUT
; 				$40 FOR GET
;				$98 FOR READ
; ----------------------------------------------------------------------------
PROCESS_INPUT_LIST:
            sta     INPUTFLG
            stx     INPTR
            sty     INPTR+1
PROCESS_INPUT_ITEM:
            jsr     PTRGET
            sta     FORPNT
            sty     FORPNT+1
            lda     TXTPTR
            ldy     TXTPTR+1
            sta     LASTOP
            sty     LASTOP+1
            ldx     INPTR
            ldy     INPTR+1
            stx     TXTPTR
            sty     TXTPTR+1
            jsr     CHRGOT
            bne     INSTART
            bit     INPUTFLG
            bvc     L1E87
            jsr     L0FA8
            sta     INPUTBUFFER
            ldx     #<(INPUTBUFFER-1)
            ldy     #>(INPUTBUFFER-1)
            bne     L1E96
L1E87:      bmi     FINDATA
            lda     SCHANNEL
            cmp     #$01
            bne     L1E93
            jsr     OUTQUES
L1E93:      jsr     NXIN
L1E96:      stx     TXTPTR
            sty     TXTPTR+1

; ----------------------------------------------------------------------------
INSTART:    jsr     CHRGET
            bit     VALTYP
            bpl     L1ED2
            bit     INPUTFLG
            bvc     L1EAE
            inx
            stx     TXTPTR
            lda     #$00
            sta     CHARAC
            beq     L1EBA
L1EAE:      sta     CHARAC
            cmp     #$22
            beq     L1EBB
            lda     #$3A
            sta     CHARAC
            lda     #$2C
L1EBA:      clc
L1EBB:      sta     ENDCHR
            lda     TXTPTR
            ldy     TXTPTR+1
            adc     #$00
            bcc     L1EC6
            iny
L1EC6:      jsr     STRLT2
            jsr     POINT
            jsr     PUTSTR
            jmp     INPUT_MORE
; ----------------------------------------------------------------------------
L1ED2:      jsr     FIN
            lda     VALTYP+1
            jsr     LET2
; ----------------------------------------------------------------------------
INPUT_MORE: jsr     CHRGOT
            beq     L1EE6
            cmp     #$2C
            beq     L1EE6
            jmp     INPUTERR

L1EE6:      lda     TXTPTR
            ldy     TXTPTR+1
            sta     INPTR
            sty     INPTR+1
            lda     LASTOP
            ldy     LASTOP+1
            sta     TXTPTR
            sty     TXTPTR+1
            jsr     CHRGOT
            beq     INPDONE
            jsr     CHKCOM
            jmp     PROCESS_INPUT_ITEM

; ----------------------------------------------------------------------------
FINDATA:    jsr     DATAN
            iny
            tax
            bne     L1F1A
            ldx     #ERR_NODATA
            iny
            lda     (TXTPTR),y
            beq     GERR
            iny
            lda     (TXTPTR),y
            sta     Z43
            iny
            lda     (TXTPTR),y
            iny
            sta     Z43+1
L1F1A:      lda     (TXTPTR),y
            tax
            jsr     ADDON
            cpx     #$83
            bne     FINDATA
            jmp     INSTART
; ---NO MORE INPUT REQUESTED------
INPDONE:    lda     INPTR
            ldy     INPTR+1
            ldx     INPUTFLG
            bpl     L1F32   ; INPUT or GET
            jmp     SETDA
L1F32:      ldy     #$00
            lda     (INPTR),y
            beq     L1F46
            lda     SCHANNEL
            cmp     #$01
            bne     L1F46
            lda     #<ERREXTRA
            ldy     #>ERREXTRA
            jmp     STROUT

L1F46:      rts

; ----------------------------------------------------------------------------
ERREXTRA:   .byte   "?EXTRA IGNORED", $0D, $00
ERRREENTRY: .byte   "?REDO FROM START", $0D, $00

; ----------------------------------------------------------------------------
; "NEXT" STATEMENT
; ----------------------------------------------------------------------------
NEXT:       bne     NEXT1
            ldy     #$00
            beq     NEXT2

NEXT1:      jsr     PTRGET

NEXT2:      sta     FORPNT
            sty     FORPNT+1
            jsr     GTFORPNT
            beq     NEXT3
            ldx     #$00
GERR:       beq     JERROR
NEXT3:      txs
            txa
            clc
            adc     #$04
            pha
            adc     #BYTES_FP+1
            sta     DEST
            pla
            ldy     #>STACK
            jsr     LOAD_FAC_FROM_YA
            tsx
            lda     STACK+BYTES_FP+4,x
            sta     FACSIGN
            lda     FORPNT
            ldy     FORPNT+1
            jsr     FADD
            jsr     SETFOR
            ldy     #>STACK
            jsr     FCOMP2
            tsx
            sec
            sbc     STACK+BYTES_FP+4,x
            beq     L1FC2
            lda     STACK+2*BYTES_FP+5,x
            sta     CURLIN
            lda     STACK+2*BYTES_FP+6,x
            sta     CURLIN+1
            lda     STACK+2*BYTES_FP+8,x
            sta     TXTPTR
            lda     STACK+2*BYTES_FP+7,x
            sta     TXTPTR+1
L1FBF:      jmp     NEWSTT

L1FC2:      txa
            adc     #2*BYTES_FP+7
            tax
            txs
            jsr     CHRGOT
            cmp     #$2C
            bne     L1FBF
            jsr     CHRGET
            jsr     NEXT1

; ----------------------------------------------------------------------------
; EVALUATE EXPRESSION, MAKE SURE IT IS NUMERIC
; ----------------------------------------------------------------------------
FRMNUM:     jsr     FRMEVL

; ----------------------------------------------------------------------------
; MAKE SURE (FAC) IS NUMERIC
; ----------------------------------------------------------------------------
CHKNUM:     clc
            .byte   $24             ; BIT zpg -> Skip next one byte

; ----------------------------------------------------------------------------
; MAKE SURE (FAC) IS STRING
; ----------------------------------------------------------------------------
CHKSTR:     sec

; ----------------------------------------------------------------------------
; MAKE SURE (FAC) IS CORRECT TYPE
; IF C=0, TYPE MUST BE NUMERIC
; IF C=1, TYPE MUST BE STRING
; ----------------------------------------------------------------------------
CHKVAL:     bit     VALTYP
            bmi     L1FE1
            bcs     L1FE3
L1FE0:      rts

L1FE1:      bcs     L1FE0
L1FE3:      ldx     #ERR_BADTYPE

JERROR:     jmp     ERROR

; ----------------------------------------------------------------------------
; EVALUATE THE EXPRESSION AT TXTPTR, LEAVING THE
; RESULT IN FAC.  WORKS FOR BOTH STRING AND NUMERIC
; EXPRESSIONS.
; ----------------------------------------------------------------------------
FRMEVL:     ldx     TXTPTR
            bne     L1FEE
            dec     TXTPTR+1
L1FEE:      dec     TXTPTR
            ldx     #$00
            .byte   $24             ; BIT zpg -> Skip next one byte
FRMEVL1:    pha
            txa
            pha
            lda     #$01
            jsr     CHKMEM
            jsr     FRM_ELEMENT
            lda     #$00
            sta     CPRTYP
FRMEVL2:    jsr     CHRGOT
L2005:      sec
            sbc     #TOKEN_GREATER
            bcc     L2021
            cmp     #$03
            bcs     L2021
            cmp     #$01
            rol     a
            eor     #$01
            eor     CPRTYP
            cmp     CPRTYP
            bcc     SNTXERR
            sta     CPRTYP
            jsr     CHRGET
            jmp     L2005

L2021:      ldx     CPRTYP
            bne     FRM_RELATIONAL
            bcs     L20A6
            adc     #$07
            bcc     L20A6
            adc     VALTYP
            bne     L2032
            jmp     CAT

L2032:      adc     #$FF
            sta     INDEX
            asl     a
            adc     INDEX
            tay
FRM_PRECEDENCE_TEST:
            pla
            cmp     MATHTBL,y
            bcs     FRM_PERFORM1
            jsr     CHKNUM
L2043:      pha
L2044:      jsr     FRM_RECURSE
            pla
            ldy     LASTOP
            bpl     PREFNC
            tax
            beq     GOEX
            bne     FRM_PERFORM2

; ----------------------------------------------------------------------------
; FOUND ONE OR MORE RELATIONAL OPERATORS <,=,>
; ----------------------------------------------------------------------------
FRM_RELATIONAL:
            lsr     VALTYP
            txa
            rol     a
            ldx     TXTPTR
            bne     L205B
L2059:      dec     TXTPTR+1
L205B:      dec     TXTPTR
            ldy     #$1B
            sta     CPRTYP
            bne     FRM_PRECEDENCE_TEST
PREFNC:     cmp     MATHTBL,y
            bcs     FRM_PERFORM2
            bcc     L2043

; ----------------------------------------------------------------------------
; STACK THIS OPERATION AND CALL FRMEVL FOR
; ANOTHER ONE
; ----------------------------------------------------------------------------
FRM_RECURSE:
            lda     MATHTBL+2,y
            pha
            lda     MATHTBL+1,y
            pha
            jsr     FRM_STACK1
            lda     CPRTYP
            jmp     FRMEVL1

SNTXERR:    jmp     SYNERR

; ----------------------------------------------------------------------------
; STACK (FAC)
; THREE ENTRY POINTS:
; 	1, FROM FRMEVL
;	2, FROM "STEP"
;	3, FROM "FOR"
; ----------------------------------------------------------------------------
FRM_STACK1: lda     FACSIGN
            ldx     MATHTBL,y

; ----------------------------------------------------------------------------
; ENTER HERE FROM "STEP", TO PUSH STEP SIGN AND VALUE
; ----------------------------------------------------------------------------
FRM_STACK2: tay
            pla
            sta     INDEX
            pla
            sta     INDEX+1
            inc     INDEX
            bne     L208F
            inc     INDEX+1
L208F:      tya
            pha

; ----------------------------------------------------------------------------
; ENTER HERE FROM "FOR", WITH (INDEX) = STEP,
; TO PUSH INITIAL VALUE OF "FOR" VARIABLE
; ----------------------------------------------------------------------------
FRM_STACK3: jsr     ROUND_FAC
            lda     FAC+4
            pha
            lda     FAC+3
            pha
            lda     FAC+2
            pha
            lda     FAC+1
            pha
            lda     FAC
            pha
            jmp     (INDEX)

L20A6:      ldy     #$FF
            pla
GOEX:       beq     EXIT

; ----------------------------------------------------------------------------
; PERFORM STACKED OPERATION
;
; (A) = PRECEDENCE BYTE
; STACK:  1 -- CPRMASK
;	5 -- (ARG)
;	2 -- ADDR OF PERFORMER
; ----------------------------------------------------------------------------
FRM_PERFORM1:
            cmp     #$64
            beq     L20B2
            jsr     CHKNUM
L20B2:      sty     LASTOP

FRM_PERFORM2:
            pla
            lsr     a
            sta     CPRMASK
            pla
            sta     ARG
            pla
            sta     ARG+1
            pla
            sta     ARG+2
            pla
            sta     ARG+3
            pla
            sta     ARG+4
            pla
            sta     ARGSIGN
            eor     FACSIGN
            sta     SGNCPR
EXIT:       lda     FAC
            rts

; ----------------------------------------------------------------------------
; GET ELEMENT IN EXPRESSION
;
; GET VALUE OF VARIABLE OR NUMBER AT TXTPNT, OR POINT
; TO STRING DESCRIPTOR IF A STRING, AND PUT IN FAC.
; ----------------------------------------------------------------------------
FRM_ELEMENT:
            lda     #$00
            sta     VALTYP
L20D5:      jsr     CHRGET
            bcs     L20DD
L20DA:      jmp     FIN

L20DD:      jsr     ISLETC
            bcs     FRM_VARIABLE
            cmp     #$2E
            beq     L20DA
            cmp     #TOKEN_MINUS
            beq     MIN
            cmp     #TOKEN_PLUS
            beq     L20D5
            cmp     #$22
            bne     NOT_

; ----------------------------------------------------------------------------
; STRING CONSTANT ELEMENT
;
; SET Y,A = (TXTPTR)+CARRY
; ----------------------------------------------------------------------------
STRTXT:     lda     TXTPTR
            ldy     TXTPTR+1
            adc     #$00
            bcc     L20FB
            iny
L20FB:      jsr     STRLIT
            jmp     POINT

; ----------------------------------------------------------------------------
; "NOT" FUNCTION
; IF FAC=0, RETURN FAC=1
; IF FAC<>0, RETURN FAC=0
; ----------------------------------------------------------------------------
NOT_:       cmp     #TOKEN_NOT
            bne     L2118
            ldy     #$18
            bne     EQUL

; ----------------------------------------------------------------------------
; COMPARISON FOR EQUALITY (= OPERATOR)
; ALSO USED TO EVALUATE "NOT" FUNCTION
; ----------------------------------------------------------------------------
EQUOP:      jsr     AYINT
            lda     FAC_LAST
            eor     #$FF
            tay
            lda     FAC_LAST-1
            eor     #$FF
            jmp     GIVAYF

L2118:      cmp     #TOKEN_FN
            bne     L211F
            jmp     L266D

L211F:      cmp     #TOKEN_SGN
            bcc     PARCHK
            jmp     L21C6

; ----------------------------------------------------------------------------
; EVALUATE "(EXPRESSION)"
; ----------------------------------------------------------------------------
PARCHK:     jsr     CHKOPN
            jsr     FRMEVL
CHKCLS:     lda     #')'
            .byte   $2C
CHKOPN:     lda     #'('
            .byte   $2C
CHKCOM:     lda     #','

; ----------------------------------------------------------------------------
; UNLESS CHAR AT TXTPTR = (A), SYNTAX ERROR
; ----------------------------------------------------------------------------
SYNCHR:     ldy     #$00
            cmp     (TXTPTR),y
            bne     SYNERR
            jmp     CHRGET
; ----------------------------------------------------------------------------
SYNERR:     ldx     #ERR_SYNTAX
            jmp     ERROR
; ----------------------------------------------------------------------------
MIN:        ldy     #$15
EQUL:       pla
            pla
            jmp     L2044
; ----------------------------------------------------------------------------
FRM_VARIABLE:
            jsr     PTRGET
FRM_VARIABLE_CALL	= *-1
            sta     FAC_LAST-1
            sty     FAC_LAST
            lda     VARNAM
            ldy     VARNAM+1
            ldx     VALTYP
            beq     L215D
            ldx     #$00
            stx     STRNG1+1
            rts

L215D:      ldx     VALTYP+1
            bpl     L216E
            ldy     #$00
            lda     (FAC+3),y
            tax
            iny
            lda     (FAC+3),y
            tay
            txa
            jmp     GIVAYF

L216E:      cmp     #$53
            bne     L218F
            cpy     #$54
            bne     L21BF
            jsr     L217E
            lda     Z77
            jmp     FLOAT

L217E:      lda     #$A5
            sta     Z9E
            jsr     L1E38
            ldy     #$04
            sta     (LOWTR),y
            jsr     L1221
            jmp     INTEGRITY_CHK

L218F:      cmp     #$4B
            bne     L21BF
            cpy     #$45
            bne     L21BF
L2197:      jsr     L217E
            lda     Z9B
            beq     L21BC
            cmp     #$FF
            beq     L21BA
            cmp     L0806
            bcc     L21BA
            cmp     L0807
            beq     L21AE
            bcs     L21BA
L21AE:      sec
            sbc     L0806
            adc     #$00
            ldx     #$FF
            stx     Z9B
            bne     L21BC
L21BA:      lda     #$00
L21BC:      jmp     FLOAT

L21BF:      lda     FAC_LAST-1
            ldy     FAC_LAST
            jmp     LOAD_FAC_FROM_YA

L21C6:      cmp     #$D0
            bcc     UNARY
            jsr     L1319
            beq     L21D2
            jmp     L1BCA

L21D2:      tya
            asl     a
            tay
            lda     L0928,y
            sta     Z9C
            lda     L0928+1,y
            sta     Z9C+1
            jsr     CHRGET
            jmp     (Z9C)
; ----------------------------------------------------------------------------
UNARY:      asl     a
            pha
            tax
            jsr     CHRGET
            cpx     #<(TOKEN_USR*2)
            beq     USR
            cpx     #<(TOKEN_LEFTSTR*2-1)
            bcc     L2213
            jsr     CHKOPN
            jsr     FRMEVL
            jsr     CHKCOM
            jsr     CHKSTR
            pla
            tax
            lda     FAC_LAST
            pha
            lda     FAC_LAST-1
            pha
            txa
            pha
            jsr     GETBYT
            pla
            tay
            txa
            pha
            jmp     L2218

L2213:      jsr     PARCHK
            pla
            tay

L2218:      lda     L09ED,y
            sta     JMPADRS+1
            lda     L09EE,y
            sta     JMPADRS+2
            jsr     JMPADRS
            jmp     CHKNUM

; ----------------------------------------------------------------------------
USR:        pla
            jsr     CHKOPN
            jsr     FRMNUM
            jsr     GETADDR
            jsr     L2238
            jmp     CHKCLS

L2238:      jsr     L2AA8
            jmp     CHKNUM

; ----------------------------------------------------------------------------
OR:         ldy     #$FF
            .byte   $2C

; ----------------------------------------------------------------------------
TAND:       ldy     #$00
            sty     EOLPNTR
            jsr     AYINT
            lda     FAC_LAST-1
            eor     EOLPNTR
            sta     CHARAC
            lda     FAC_LAST
            eor     EOLPNTR
            sta     ENDCHR
            jsr     COPY_ARG_TO_FAC
            jsr     AYINT
            lda     FAC_LAST
            eor     EOLPNTR
            and     ENDCHR
            eor     EOLPNTR
            tay
            lda     FAC_LAST-1
            eor     EOLPNTR
            and     CHARAC
            eor     EOLPNTR
            jmp     GIVAYF

; ----------------------------------------------------------------------------
; PERFORM RELATIONAL OPERATIONS
; ----------------------------------------------------------------------------
RELOPS:     jsr     CHKVAL
            bcs     STRCMP
            lda     ARGSIGN
            ora     #TXTPTR
            and     ARG+1
            sta     ARG+1
            lda     #<ARG
            ldy     #>ARG
            jsr     FCOMP
            tax
            jmp     NUMCMP

; ----------------------------------------------------------------------------
; STRING COMPARISON
; ----------------------------------------------------------------------------
STRCMP:     lda     #$00
            sta     VALTYP
            dec     CPRTYP
            jsr     FREFAC
            sta     FAC
            stx     FAC+1
            sty     FAC+2
            lda     ARG_LAST-1
            ldy     ARG_LAST
            jsr     FRETMP
            stx     ARG_LAST-1
            sty     ARG_LAST
            tax
            sec
            sbc     FAC
            beq     L22AE
            lda     #$01
            bcc     L22AE
            ldx     FAC
            lda     #$FF
L22AE:      sta     FACSIGN
            ldy     #$FF
            inx
STRCMP1:    iny
            dex
            bne     L22BE
            ldx     FACSIGN

NUMCMP:     bmi     CMPDONE
            clc
            bcc     CMPDONE
L22BE:      lda     (ARG_LAST-1),y
            cmp     (FAC+1),y
            beq     STRCMP1
            ldx     #$FF
            bcs     CMPDONE
            ldx     #$01
CMPDONE:    inx
            txa
            rol     a
            and     CPRMASK
            beq     L22D3
            lda     #$FF
L22D3:      jmp     FLOAT

; ----------------------------------------------------------------------------
; "DIM" STATEMENT
; ----------------------------------------------------------------------------
NXDIM:      jsr     CHKCOM

DIM:        tax
            jsr     PTRGET2
            jsr     CHRGOT
            bne     NXDIM
            rts

; CODOS: Alternate PTRGET
APTRGET:    jsr     PTRGET
            sta     FORPNT
            sty     FORPNT+1
            rts

; ----------------------------------------------------------------------------
; PTRGET -- GENERAL VARIABLE SCAN
;
; SCANS VARIABLE NAME AT TXTPTR, AND SEARCHES THE
; VARTAB AND ARYTAB FOR THE NAME.
; IF NOT FOUND, CREATE VARIABLE OF APPROPRIATE TYPE.
; RETURN WITH ADDRESS IN VARPNT AND Y,A
;
; ACTUAL ACTIVITY CONTROLLED SOMEWHAT BY TWO FLAGS:
;	DIMFLG -- NONZERO IF CALLED FROM "DIM"
;		ELSE = 0
;
;	SUBFLG -- = $00
;		= $40 IF CALLED FROM "GETARYPT"
; ----------------------------------------------------------------------------
PTRGET:     ldx     #$00
            jsr     CHRGOT
PTRGET2:    stx     DIMFLG
PTRGET3:    sta     VARNAM
            jsr     CHRGOT
            jsr     ISLETC
            bcs     NAMOK
SYNERR3:    jmp     SYNERR

NAMOK:      ldx     #$00
            stx     VALTYP
            stx     VALTYP+1
            jsr     CHRGET
            bcc     L230F
            jsr     ISLETC
            bcc     L231A
L230F:      tax
L2310:      jsr     CHRGET
            bcc     L2310
            jsr     ISLETC
            bcs     L2310
L231A:      cmp     #$24
            bne     L2324
            lda     #$FF
            sta     VALTYP
            bne     L2334
L2324:      cmp     #$25
            bne     L233B
            lda     SUBFLG
            bne     SYNERR3
            lda     #$80
            sta     VALTYP+1
            ora     VARNAM
            sta     VARNAM
L2334:      txa
            ora     #$80
            tax
            jsr     CHRGET
L233B:      stx     VARNAM+1
            sec
            ora     SUBFLG
            sbc     #$28
            bne     L2347
            jmp     ARRAY

L2347:      lda     #$00
            sta     SUBFLG
            lda     VARTAB
            ldx     VARTAB+1
            ldy     #$00
L2351:      stx     LOWTR+1
L2353:      sta     LOWTR
            cpx     ARYTAB+1
            bne     L235D
            cmp     ARYTAB
            beq     NAMENOTFOUND
L235D:      lda     VARNAM
            cmp     (LOWTR),y
            bne     L236E
            lda     VARNAM+1
            iny
            cmp     (LOWTR),y
            bne     L236D
            jmp     SET_VARPNT_AND_YA

L236D:      dey
L236E:      clc
            lda     LOWTR
            adc     #BYTES_PER_VARIABLE
            bcc     L2353
            inx
            bne     L2351

; ----------------------------------------------------------------------------
; CHECK IF (A) IS ASCII LETTER A-Z
;
; RETURN CARRY = 1 IF A-Z
;	= 0 IF NOT
; ----------------------------------------------------------------------------
ISLETC:     cmp     #$41
            bcc     L2381
            sbc     #$5B
            sec
            sbc     #$A5
L2381:      rts

; ----------------------------------------------------------------------------
; VARIABLE NOT FOUND, SO MAKE ONE
; ----------------------------------------------------------------------------
NAMENOTFOUND:
            pla
            pha
            cmp     #<FRM_VARIABLE_CALL
            bne     MAKENEWVARIABLE
            tsx
            lda     STACK+2,x
            cmp     #>FRM_VARIABLE_CALL
            bne     MAKENEWVARIABLE
            lda     #<(CON_HALF+2)
            ldy     #>(CON_HALF+2)
            rts

; ----------------------------------------------------------------------------
; MAKE A NEW SIMPLE VARIABLE
;
; MOVE ARRAYS UP 7 BYTES TO MAKE ROOM FOR NEW VARIABLE
; ENTER 7-BYTE VARIABLE DATA IN THE HOLE
; ----------------------------------------------------------------------------
MAKENEWVARIABLE:
            lda     VARNAM
            ldy     VARNAM+1
            jmp     L239F

L239C:      jmp     SYNERR

L239F:      cmp     #$53
            bne     L23A9
            cpy     #$54
            bne     L23B1
            beq     L239C
L23A9:      cmp     #$4B
            bne     L23B1
            cpy     #$45
            beq     L239C
L23B1:      lda     ARYTAB
            ldy     ARYTAB+1
            sta     LOWTR
            sty     LOWTR+1
            lda     STREND
            ldy     STREND+1
            sta     HIGHTR
            sty     HIGHTR+1
            clc
            adc     #BYTES_PER_VARIABLE
            bcc     L23C7
            iny
L23C7:      sta     HIGHDS
            sty     HIGHDS+1
            jsr     BLTU
            lda     HIGHDS
            ldy     HIGHDS+1
            iny
            sta     ARYTAB
            sty     ARYTAB+1
            ldy     #$00
            lda     VARNAM
            sta     (LOWTR),y
            iny
            lda     VARNAM+1
            sta     (LOWTR),y
            lda     #$00
            iny
            sta     (LOWTR),y
            iny
            sta     (LOWTR),y
            iny
            sta     (LOWTR),y
            iny
            sta     (LOWTR),y
            iny
            sta     (LOWTR),y

; ----------------------------------------------------------------------------
; PUT ADDRESS OF VALUE OF VARIABLE IN VARPNT AND Y,A
; ----------------------------------------------------------------------------
SET_VARPNT_AND_YA:
            lda     LOWTR
            clc
            adc     #$02
            ldy     LOWTR+1
            bcc     L23FD
            iny
L23FD:      sta     VARPNT
            sty     VARPNT+1
            rts

; ----------------------------------------------------------------------------
; COMPUTE ADDRESS OF FIRST VALUE IN ARRAY
; ARYPNT = (LOWTR) + #DIMS*2 + 5
; ----------------------------------------------------------------------------
GETARY:     lda     EOLPNTR
            asl     a
GETARY2:    adc     #$05
            adc     LOWTR
            ldy     LOWTR+1
            bcc     L240E
            iny
L240E:      sta     HIGHDS
            sty     HIGHDS+1
            rts

; ----------------------------------------------------------------------------
NEG32768:
            .byte   $90, $80, $00, $00, $00

            .byte   $60, $60, $62, $6E, $6A


L241D:      .byte   $17

; ----------------------------------------------------------------------------
; EVALUATE NUMERIC FORMULA AT TXTPTR
; CONVERTING RESULT TO INTEGER 0 <= X <= 32767
; IN FAC+3,4
; ----------------------------------------------------------------------------
MAKINT:     jsr     CHRGET
            jsr     FRMEVL

; ----------------------------------------------------------------------------
; CONVERT FAC TO INTEGER
; MUST BE POSITIVE AND LESS THAN 32768
; ----------------------------------------------------------------------------
MKINT:      jsr     CHKNUM
            lda     FACSIGN
            bmi     MI1

; ----------------------------------------------------------------------------
; CONVERT FAC TO INTEGER
; MUST BE -32767 <= FAC <= 32767
; ----------------------------------------------------------------------------
AYINT:      lda     FAC
            cmp     #$90
            bcc     MI2
            lda     #<NEG32768
            ldy     #>NEG32768
            jsr     FCOMP
MI1:        bne     IQERR
MI2:        jmp     QINT

; ----------------------------------------------------------------------------
; LOCATE ARRAY ELEMENT OR CREATE AN ARRAY
; ----------------------------------------------------------------------------
ARRAY:      lda     DIMFLG
            ora     VALTYP+1
            pha
            lda     VALTYP
            pha
            ldy     #$00
L2447:      tya
            pha
            lda     VARNAM+1
            pha
            lda     VARNAM
            pha
            jsr     MAKINT
            pla
            sta     VARNAM
            pla
            sta     VARNAM+1
            pla
            tay
            tsx
            lda     STACK+2,x
            pha
            lda     STACK+1,x
            pha
            lda     FAC_LAST-1
            sta     STACK+2,x
            lda     FAC_LAST
            sta     STACK+1,x
            iny
            jsr     CHRGOT
            cmp     #$2C
            beq     L2447
            sty     EOLPNTR
            jsr     CHKCLS
            pla
            sta     VALTYP
            pla
            sta     VALTYP+1
            and     #$7F
            sta     DIMFLG
; ----------------------------------------------------------------------------
; SEARCH ARRAY TABLE FOR THIS ARRAY NAME
; ----------------------------------------------------------------------------
            ldx     ARYTAB
            lda     ARYTAB+1
L2488:      stx     LOWTR
            sta     LOWTR+1
            cmp     STREND+1
            bne     L2494
            cpx     STREND
            beq     MAKE_NEW_ARRAY
L2494:      ldy     #$00
            lda     (LOWTR),y
            iny
            cmp     VARNAM
            bne     L24A3
            lda     VARNAM+1
            cmp     (LOWTR),y
            beq     USE_OLD_ARRAY
L24A3:      iny
            lda     (LOWTR),y
            clc
            adc     LOWTR
            tax
            iny
            lda     (LOWTR),y
            adc     LOWTR+1
            bcc     L2488

; ----------------------------------------------------------------------------
; ERROR:  BAD SUBSCRIPTS
; ----------------------------------------------------------------------------
SUBERR:     ldx     #ERR_BADSUBS
            .byte   $2C

; ----------------------------------------------------------------------------
; ERROR:  ILLEGAL QUANTITY
; ----------------------------------------------------------------------------
IQERR:      ldx     #ERR_ILLQTY
JER:        jmp     ERROR

; ----------------------------------------------------------------------------
; FOUND THE ARRAY
; ----------------------------------------------------------------------------
USE_OLD_ARRAY:
            ldx     #ERR_REDIMD
            lda     DIMFLG
            bne     JER
            jsr     GETARY
            lda     EOLPNTR
            ldy     #$04
            cmp     (LOWTR),y
            bne     SUBERR
            jmp     FIND_ARRAY_ELEMENT

; ----------------------------------------------------------------------------
; CREATE A NEW ARRAY, UNLESS CALLED FROM GETARYPT
; ----------------------------------------------------------------------------
MAKE_NEW_ARRAY:
            jsr     GETARY
            jsr     REASON
            lda     #$00
            tay
            sta     STRNG2+1
            ldx     #BYTES_PER_ELEMENT
            lda     VARNAM
            sta     (LOWTR),y
            bpl     L24E1
            dex
L24E1:      iny
            lda     VARNAM+1
            sta     (LOWTR),y
            bpl     L24EA
            dex
            dex
L24EA:      stx     STRNG2
            lda     EOLPNTR
            iny
            iny
            iny
            sta     (LOWTR),y
L24F3:      ldx     #$0B
            lda     #$00
            bit     DIMFLG
            bvc     L2503
            pla
            clc
            adc     #$01
            tax
            pla
            adc     #$00
L2503:      iny
            sta     (LOWTR),y
            iny
            txa
            sta     (LOWTR),y
            jsr     MULTIPLY_SUBSCRIPT
            stx     STRNG2
            sta     STRNG2+1
            ldy     INDEX
            dec     EOLPNTR
            bne     L24F3
            adc     HIGHDS+1
            bcs     GME
            sta     HIGHDS+1
            tay
            txa
            adc     HIGHDS
            bcc     L2526
            iny
            beq     GME
L2526:      jsr     REASON
            sta     STREND
            sty     STREND+1
            lda     #$00
            inc     STRNG2+1
            ldy     STRNG2
            beq     L253A
L2535:      dey
            sta     (HIGHDS),y
            bne     L2535
L253A:      dec     HIGHDS+1
            dec     STRNG2+1
            bne     L2535
            inc     HIGHDS+1
            sec
            lda     STREND
            sbc     LOWTR
            ldy     #$02
            sta     (LOWTR),y
            lda     STREND+1
            iny
            sbc     LOWTR+1
            sta     (LOWTR),y
            lda     DIMFLG
            bne     RTS9
            iny

; ----------------------------------------------------------------------------
; FIND SPECIFIED ARRAY ELEMENT
;
; (LOWTR),Y POINTS AT # OF DIMS IN ARRAY DESCRIPTOR
; THE SUBSCRIPTS ARE ALL ON THE STACK AS INTEGERS
; ----------------------------------------------------------------------------
FIND_ARRAY_ELEMENT:
            lda     (LOWTR),y
            sta     EOLPNTR
            lda     #$00
            sta     STRNG2
L255F:      sta     STRNG2+1
            iny
            pla
            tax
            sta     FAC_LAST-1
            pla
            sta     FAC_LAST
            cmp     (LOWTR),y
            bcc     FAE2
            bne     GSE
            iny
            txa
            cmp     (LOWTR),y
            bcc     FAE3
; ----------------------------------------------------------------------------
GSE:        jmp     SUBERR

GME:        jmp     MEMERR
; ----------------------------------------------------------------------------
FAE2:       iny
FAE3:       lda     STRNG2+1
            ora     STRNG2
            clc
            beq     L258D
            jsr     MULTIPLY_SUBSCRIPT
            txa
            adc     FAC_LAST-1
            tax
            tya
            ldy     INDEX
L258D:      adc     FAC_LAST
            stx     STRNG2
            dec     EOLPNTR
            bne     L255F
            sta     STRNG2+1
            ldx     #BYTES_FP
            lda     VARNAM
            bpl     L259E
            dex
L259E:      lda     VARNAM+1
            bpl     L25A4
            dex
            dex
L25A4:      stx     RESULT+2
            lda     #$00
            jsr     MULTIPLY_SUBS1
            txa
            adc     HIGHDS
            sta     VARPNT
            tya
            adc     HIGHDS+1
            sta     VARPNT+1
            tay
            lda     VARPNT
RTS9:       rts

; ----------------------------------------------------------------------------
; MULTIPLY (STRNG2) BY ((LOWTR),Y)
; LEAVING PRODUCT IN A,X.  (HI-BYTE ALSO IN Y.)
; USED ONLY BY ARRAY SUBSCRIPT ROUTINES
; ----------------------------------------------------------------------------
MULTIPLY_SUBSCRIPT:
            sty     INDEX
            lda     (LOWTR),y
            sta     RESULT+2
            dey
            lda     (LOWTR),y

MULTIPLY_SUBS1:  sta     RESULT+3
            lda     #$10
            sta     INDX
            ldx     #$00
            ldy     #$00
L25CC:      txa
            asl     a
            tax
            tya
            rol     a
            tay
            bcs     GME
            asl     STRNG2
            rol     STRNG2+1
            bcc     L25E5
            clc
            txa
            adc     RESULT+2
            tax
            tya
            adc     RESULT+3
            tay
            bcs     GME
L25E5:      dec     INDX
            bne     L25CC
            rts

; ----------------------------------------------------------------------------
; "FRE" FUNCTION
;
; COLLECTS GARBAGE AND RETURNS # BYTES OF MEMORY LEFT
; ----------------------------------------------------------------------------
FRE:        lda     VALTYP
            beq     L25F1
            jsr     FREFAC
L25F1:      jsr     GARBAG
            sec
            lda     FRETOP
            sbc     STREND
            tay
            lda     FRETOP+1
            sbc     STREND+1

; CODOS
L25FE:      sta     FAC+1
            sty     FAC+2
            sec
            ldx     #$00
            stx     VALTYP
            ldx     #$90
            jmp     FLOAT2

; ----------------------------------------------------------------------------
; FLOAT THE SIGNED INTEGER IN A,Y
; ----------------------------------------------------------------------------
GIVAYF:     ldx     #$00
            stx     VALTYP
            sta     FAC+1
            sty     FAC+2
            ldx     #$90
            jmp     FLOAT1

POS:        ldy     Z15

; ----------------------------------------------------------------------------
; FLOAT (Y) INTO FAC, GIVING VALUE 0-255
; ----------------------------------------------------------------------------
SNGFLT:     lda     #$00
            beq     GIVAYF

; ----------------------------------------------------------------------------
; CHECK FOR DIRECT OR RUNNING MODE
; GIVING ERROR IF DIRECT MODE
; ----------------------------------------------------------------------------
ERRDIR:     ldx     CURLIN+1
            inx
            bne     RTS9
            ldx     #ERR_ILLDIR
            .byte   $2C

ERRUNDEF:   ldx     #ERR_UNDEFFN
            jmp     ERROR

DEF:        jsr     FNC
            jsr     ERRDIR
            jsr     CHKOPN
            lda     #$80
            sta     SUBFLG
            jsr     PTRGET
            jsr     CHKNUM
            jsr     CHKCLS
            lda     #TOKEN_EQUAL ; $B5
            jsr     SYNCHR
            pha
            lda     VARPNT+1
            pha
            lda     VARPNT
            pha
            lda     TXTPTR+1
            pha
            lda     TXTPTR
            pha
            jsr     DATA
            jmp     L26C8

FNC:        lda     #TOKEN_FN   ; $A8
            jsr     SYNCHR
            ora     #$80
            sta     SUBFLG
            jsr     PTRGET3
            sta     FNCNAM
            sty     FNCNAM+1
            jmp     CHKNUM

L266D:      jsr     FNC
            lda     FNCNAM+1
            pha
            lda     FNCNAM
            pha
            jsr     PARCHK
            jsr     CHKNUM
            pla
            sta     FNCNAM
L267F:      pla
            sta     FNCNAM+1
            ldy     #$02
            lda     (FNCNAM),y
            sta     VARPNT
            tax
            iny
            lda     (FNCNAM),y
            beq     ERRUNDEF
            sta     VARPNT+1
            iny
L2691:      lda     (VARPNT),y
            pha
            dey
            bpl     L2691
            ldy     VARPNT+1
            jsr     STORE_FAC_AT_YX_ROUNDED
            lda     TXTPTR+1
            pha
            lda     TXTPTR
            pha
            lda     (FNCNAM),y
            sta     TXTPTR
            iny
            lda     (FNCNAM),y
            sta     TXTPTR+1
            lda     VARPNT+1
            pha
            lda     VARPNT
            pha
            jsr     FRMNUM
            pla
            sta     FNCNAM
            pla
            sta     FNCNAM+1
            jsr     CHRGOT
            beq     L26C2
            jmp     SYNERR

L26C2:      pla
            sta     TXTPTR
            pla
            sta     TXTPTR+1
L26C8:      ldy     #$00
            pla
            sta     (FNCNAM),y
            pla
            iny
            sta     (FNCNAM),y
            pla
            iny
            sta     (FNCNAM),y
            pla
            iny
            sta     (FNCNAM),y
            pla
            iny
            sta     (FNCNAM),y
            rts

; CODOS -------------------- 
; Integrity check
;
INTEGRITY_CHK:
            ldx     #$00
            clc
            ldy     #$00
L26E3:      lda     (LOWTR),y
L26E5:      adc     #$10
            bcc     L26E5
            adc     Z9C             ; On entry, Z9C = 0
            sta     Z9C
            bcc     L26F1
            inc     Z9C+1
L26F1:      asl     Z9C
            rol     Z9C+1
            inx
            cpx     L2713
            bne     L26E3
            lda     Z9C             ; Here, X = 5
            cmp     INTEGRITY_CHKSUM-5,x
            bne     L270A
            lda     Z9C+1
            cmp     INTEGRITY_CHKSUM-4,x
            bne     L270A
            rts

L270A:      lda     #<L31E2
L270C:      ldy     #>L31E2
            sta     GOSTROUT+1
            sty     GOSTROUT+2
            rts

L2713:      .byte   $05

; ----------------------------------------------------------------------------
; "STR$" FUNCTION
; ----------------------------------------------------------------------------
STR:        jsr     CHKNUM
            ldy     #$00
            jsr     FOUT1
            pla
            pla
            lda     #<(STACK2-1)
            ldy     #>(STACK2-1)
            beq     STRLIT

; ----------------------------------------------------------------------------
; GET SPACE AND MAKE DESCRIPTOR FOR STRING WHOSE
; ADDRESS IS IN FAC+3,4 AND WHOSE LENGTH IS IN A-REG
; ----------------------------------------------------------------------------
STRINI:     ldx     FAC_LAST-1
            ldy     FAC_LAST
            stx     DSCPTR
            sty     DSCPTR+1

; ----------------------------------------------------------------------------
; GET SPACE AND MAKE DESCRIPTOR FOR STRING WHOSE
; ADDRESS IS IN Y,X AND WHOSE LENGTH IS IN A-REG
; ----------------------------------------------------------------------------
STRSPA:     jsr     GETSPA
            stx     FAC+1
            sty     FAC+2
            sta     FAC
            rts

; ----------------------------------------------------------------------------
; BUILD A DESCRIPTOR FOR STRING STARTING AT Y,A
; AND TERMINATED BY $00 OR QUOTATION MARK
; RETURN WITH DESCRIPTOR IN A TEMPORARY
; AND ADDRESS OF DESCRIPTOR IN FAC+3,4
; ----------------------------------------------------------------------------
STRLIT:     ldx     #$22
            stx     CHARAC
            stx     ENDCHR

; ----------------------------------------------------------------------------
; BUILD A DESCRIPTOR FOR STRING STARTING AT Y,A
; AND TERMINATED BY $00, (CHARAC), OR (ENDCHR)
;
; RETURN WITH DESCRIPTOR IN A TEMPORARY
; AND ADDRESS OF DESCRIPTOR IN FAC+3,4
; ----------------------------------------------------------------------------
STRLT2:     sta     STRNG1
            sty     STRNG1+1
            sta     FAC+1
            sty     FAC+2
            ldy     #$FF
L2746:      iny
            lda     (STRNG1),y
            beq     L2757
            cmp     CHARAC
            beq     L2753
            cmp     ENDCHR
            bne     L2746
L2753:      cmp     #$22
            beq     L2758
L2757:      clc
L2758:      sty     FAC
            tya
            adc     STRNG1
            sta     STRNG2
            ldx     STRNG1+1
            bcc     L2764
            inx
L2764:      stx     STRNG2+1
            lda     STRNG1+1
            beq     L276E
            cmp     #>INPUTBUFFER
            bne     PUTNEW
L276E:      tya
            jsr     STRINI
            ldx     STRNG1
            ldy     STRNG1+1
            jsr     MOVSTR

; ----------------------------------------------------------------------------
; STORE DESCRIPTOR IN TEMPORARY DESCRIPTOR STACK
;
; THE DESCRIPTOR IS NOW IN FAC, FAC+1, FAC+2
; PUT ADDRESS OF TEMP DESCRIPTOR IN FAC+3,4
; ----------------------------------------------------------------------------
PUTNEW:     ldx     TEMPPT
            cpx     #TEMPST+9
            bne     PUTEMP
            ldx     #ERR_FRMCPX
JERR:       jmp     ERROR

PUTEMP:     lda     FAC
            sta     GORESTART,x
            lda     FAC+1
            sta     GORESTART+1,x
            lda     FAC+2
            sta     GORESTART+2,x
            ldy     #$00
L2792:      stx     FAC_LAST-1
            sty     FAC_LAST
            sty     FACEXTENSION
            dey

            sty     VALTYP
            stx     LASTPT
            inx
            inx
            inx
            stx     TEMPPT
            rts

L27A3:      sta     SGNCPR
            stx     FACEXTENSION
            jmp     L276E

; ----------------------------------------------------------------------------
; MAKE SPACE FOR STRING AT BOTTOM OF STRING SPACE
; (A)=# BYTES SPACE TO MAKE
;
; RETURN WITH (A) SAME,
;	AND Y,X = ADDRESS OF SPACE ALLOCATED
; ----------------------------------------------------------------------------
GETSPA:     lsr     DATAFLG
L27AC:      pha
            eor     #$FF
            sec
            adc     FRETOP
            ldy     FRETOP+1
            bcs     L27B7
            dey
L27B7:      cpy     STREND+1
            bcc     L27CC
            bne     L27C1
            cmp     STREND
            bcc     L27CC
L27C1:      sta     FRETOP
            sty     FRETOP+1
            sta     FRESPC
            sty     FRESPC+1
            tax
            pla
            rts

L27CC:      ldx     #ERR_MEMFULL
            lda     DATAFLG
            bmi     JERR
            jsr     GARBAG
            lda     #$80
            sta     DATAFLG
            pla
            bne     L27AC

; ----------------------------------------------------------------------------
; SHOVE ALL REFERENCED STRINGS AS HIGH AS POSSIBLE
; IN MEMORY (AGAINST HIMEM), FREEING UP SPACE
; BELOW STRING AREA DOWN TO STREND.
; ----------------------------------------------------------------------------
GARBAG:     ldx     MEMSIZ
            lda     MEMSIZ+1
FINDHIGHESTSTRING:
            stx     FRETOP
            sta     FRETOP+1
            ldy     #$00
            sty     FNCNAM+1
            sty     FNCNAM
            lda     STREND
            ldx     STREND+1
            sta     LOWTR
            stx     LOWTR+1
            lda     #TEMPST
            ldx     #$00
            sta     INDEX
            stx     INDEX+1
L27FA:      cmp     TEMPPT
            beq     L2803
            jsr     CHECK_VARIABLE
            beq     L27FA
L2803:      lda     #BYTES_PER_VARIABLE
            sta     DSCLEN
            lda     VARTAB
            ldx     VARTAB+1
            sta     INDEX
            stx     INDEX+1
L280F:      cpx     ARYTAB+1
            bne     L2817
            cmp     ARYTAB
            beq     L281C
L2817:      jsr     CHECK_SIMPLE_VARIABLE
            beq     L280F
L281C:      sta     HIGHDS
            stx     HIGHDS+1
L2820:      lda     #$03	; OSI GC bugfix -> $04 ???
            sta     DSCLEN
L2824:      lda     HIGHDS
            ldx     HIGHDS+1
L2828:      cpx     STREND+1
            bne     L2833
            cmp     STREND
            bne     L2833
            jmp     MOVE_HIGHEST_STRING_TO_TOP

L2833:      sta     INDEX
            stx     INDEX+1
            ldy     #$00
            lda     (INDEX),y
            tax
            iny
            lda     (INDEX),y
            php
            iny
            lda     (INDEX),y
            adc     HIGHDS
            sta     HIGHDS
            iny
            lda     (INDEX),y
            adc     HIGHDS+1
            sta     HIGHDS+1
            plp
            bpl     L2824
            txa
            bmi     L2824
            iny
            lda     (INDEX),y
            ldy     #$00	; GC bugfix
            asl     a
            adc     #$05
            adc     INDEX
            sta     INDEX
            bcc     L2864
            inc     INDEX+1
L2864:      ldx     INDEX+1
L2866:      cpx     HIGHDS+1
            bne     L286E
            cmp     HIGHDS
            beq     L2828
L286E:      jsr     CHECK_VARIABLE
            beq     L2866

; ----------------------------------------------------------------------------
; PROCESS A SIMPLE VARIABLE
; ----------------------------------------------------------------------------
CHECK_SIMPLE_VARIABLE:
            lda     (INDEX),y
            bmi     CHECK_BUMP
            iny
            lda     (INDEX),y
            bpl     CHECK_BUMP
            iny

; ----------------------------------------------------------------------------
; IF STRING IS NOT EMPTY, CHECK IF IT IS HIGHEST
; ----------------------------------------------------------------------------
CHECK_VARIABLE:
            lda     (INDEX),y
            beq     CHECK_BUMP
            iny
            lda     (INDEX),y
            tax
            iny
            lda     (INDEX),y
            cmp     FRETOP+1
            bcc     L2892
            bne     CHECK_BUMP
            cpx     FRETOP
            bcs     CHECK_BUMP
L2892:      cmp     LOWTR+1
            bcc     CHECK_BUMP
            bne     L289C
            cpx     LOWTR
            bcc     CHECK_BUMP
L289C:      stx     LOWTR
            sta     LOWTR+1
            lda     INDEX
            ldx     INDEX+1
            sta     FNCNAM
            stx     FNCNAM+1
            lda     DSCLEN
            sta     Z59

; ----------------------------------------------------------------------------
; ADD (DSCLEN) TO PNTR IN INDEX
; RETURN WITH Y=0, PNTR ALSO IN X,A
; ----------------------------------------------------------------------------
CHECK_BUMP:
            lda     DSCLEN
            clc
            adc     INDEX
            sta     INDEX
            bcc     L28B7
            inc     INDEX+1
L28B7:      ldx     INDEX+1
            ldy     #$00
            rts

; ----------------------------------------------------------------------------
; FOUND HIGHEST NON-EMPTY STRING, SO MOVE IT
; TO TOP AND GO BACK FOR ANOTHER
; ----------------------------------------------------------------------------
MOVE_HIGHEST_STRING_TO_TOP:
            lda     FNCNAM+1
            ora     FNCNAM
            beq     L28B7
            lda     Z59
            and     #$04
            lsr     a
            tay
            sta     Z59
            lda     (FNCNAM),y
            adc     LOWTR
            sta     HIGHTR
            lda     LOWTR+1
            adc     #$00
            sta     HIGHTR+1
            lda     FRETOP
            ldx     FRETOP+1
            sta     HIGHDS
            stx     HIGHDS+1
            jsr     BLTU2
            ldy     Z59
            iny
            lda     HIGHDS
            sta     (FNCNAM),y
            tax
            inc     HIGHDS+1
            lda     HIGHDS+1
            iny
            sta     (FNCNAM),y
            jmp     FINDHIGHESTSTRING

; ----------------------------------------------------------------------------
; CONCATENATE TWO STRINGS
; ----------------------------------------------------------------------------
CAT:        lda     FAC_LAST
            pha
            lda     FAC_LAST-1
            pha
            jsr     FRM_ELEMENT
            jsr     CHKSTR
            pla
            sta     STRNG1
            pla
            sta     STRNG1+1
            ldy     #$00
            lda     (STRNG1),y
            clc
L290A:      adc     (FAC_LAST-1),y
            bcc     L2913
            ldx     #ERR_STRLONG
            jmp     ERROR

L2913:      jsr     STRINI
            jsr     MOVINS
            lda     DSCPTR
            ldy     DSCPTR+1
            jsr     FRETMP
            jsr     MOVSTR1
            lda     STRNG1
            ldy     STRNG1+1
            jsr     FRETMP
            jsr     PUTNEW
            jmp     FRMEVL2

; ----------------------------------------------------------------------------
; GET STRING DESCRIPTOR POINTED AT BY (STRNG1)
; AND MOVE DESCRIBED STRING TO (FRESPC)
; ----------------------------------------------------------------------------
MOVINS:     ldy     #$00
            lda     (STRNG1),y
            pha
            iny
            lda     (STRNG1),y
            tax
            iny
            lda     (STRNG1),y
            tay
            pla

; ----------------------------------------------------------------------------
; MOVE STRING AT (Y,X) WITH LENGTH (A)
; TO DESTINATION WHOSE ADDRESS IS IN FRESPC,FRESPC+1
; ----------------------------------------------------------------------------
MOVSTR:     stx     INDEX
            sty     INDEX+1
MOVSTR1:    tay
            beq     L294F
            pha
L2946:      dey
            lda     (INDEX),y
            sta     (FRESPC),y
            tya
            bne     L2946
            pla
L294F:      clc
            adc     FRESPC
            sta     FRESPC
            bcc     L2958
            inc     FRESPC+1
L2958:      rts

; ----------------------------------------------------------------------------
; IF (FAC) IS A TEMPORARY STRING, RELEASE DESCRIPTOR
; ----------------------------------------------------------------------------
FRESTR:     jsr     CHKSTR

; ----------------------------------------------------------------------------
; IF STRING DESCRIPTOR POINTED TO BY FAC+3,4 IS
; A TEMPORARY STRING, RELEASE IT.
; ----------------------------------------------------------------------------
FREFAC:     lda     FAC_LAST-1
            ldy     FAC_LAST

; ----------------------------------------------------------------------------
; IF STRING DESCRIPTOR WHOSE ADDRESS IS IN Y,A IS
; A TEMPORARY STRING, RELEASE IT.
; ----------------------------------------------------------------------------
FRETMP:     sta     INDEX
            sty     INDEX+1
            jsr     FRETMS
            php
            ldy     #$00
            lda     (INDEX),y
            pha
            iny
            lda     (INDEX),y
            tax
            iny
            lda     (INDEX),y
            tay
            pla
            plp
            bne     L298C
            cpy     FRETOP+1
            bne     L298C
            cpx     FRETOP
            bne     L298C
            pha
            clc
            adc     FRETOP
            sta     FRETOP
            bcc     L298B
            inc     FRETOP+1
L298B:      pla
L298C:      stx     INDEX
            sty     INDEX+1
            rts

; ----------------------------------------------------------------------------
; RELEASE TEMPORARY DESCRIPTOR IF Y,A = LASTPT
; ----------------------------------------------------------------------------
FRETMS:     cpy     LASTPT+1
            bne     L29A1
            cmp     LASTPT
            bne     L29A1
            sta     TEMPPT
            sbc     #$03
            sta     LASTPT
            ldy     #$00
L29A1:      rts

; ----------------------------------------------------------------------------
; "CHR$" FUNCTION
; ----------------------------------------------------------------------------
CHRSTR:     jsr     CONINT
            txa
            pha
            lda     #$01
            jsr     STRSPA
            pla
            ldy     #$00
            sta     (FAC+1),y
            pla
            pla
            jmp     PUTNEW

; ----------------------------------------------------------------------------
; "LEFT$" FUNCTION
; ----------------------------------------------------------------------------
LEFTSTR:    jsr     SUBSTRING_SETUP
            cmp     (DSCPTR),y
            tya
SUBSTRNG1:  bcc     L29C2
            lda     (DSCPTR),y
            tax
            tya
L29C2:      pha
SUBSTRING2: txa
SUBSTRING3: pha
            jsr     STRSPA
            lda     DSCPTR
            ldy     DSCPTR+1
            jsr     FRETMP
            pla
            tay
            pla
            clc
            adc     INDEX
            sta     INDEX
            bcc     L29DB
            inc     INDEX+1
L29DB:      tya
            jsr     MOVSTR1
            jmp     PUTNEW

; ----------------------------------------------------------------------------
; "RIGHT$" FUNCTION
; ----------------------------------------------------------------------------
RIGHTSTR:   jsr     SUBSTRING_SETUP
            clc
            sbc     (DSCPTR),y
            eor     #$FF
            jmp     SUBSTRNG1

; ----------------------------------------------------------------------------
; "MID$" FUNCTION
; ----------------------------------------------------------------------------
MIDSTR:     lda     #$FF
            sta     FAC_LAST
            jsr     CHRGOT
            cmp     #$29
            beq     L29FE
            jsr     CHKCOM
            jsr     GETBYT
L29FE:      jsr     SUBSTRING_SETUP
            beq     GOIQ
            dex
            txa
            pha
            clc
            ldx     #$00
            sbc     (DSCPTR),y
            bcs     SUBSTRING2
            eor     #$FF
            cmp     FAC_LAST
            bcc     SUBSTRING3
            lda     FAC_LAST
            bcs     SUBSTRING3

; ----------------------------------------------------------------------------
; COMMON SETUP ROUTINE FOR LEFT$, RIGHT$, MID$:
; REQUIRE ")"; POP RETURN ADRS, GET DESCRIPTOR
; ADDRESS, GET 1ST PARAMETER OF COMMAND
; ----------------------------------------------------------------------------
SUBSTRING_SETUP:
            jsr     CHKCLS
            pla
            tay
            pla
            sta     Z59
            pla
            pla
            pla
            tax
            pla
            sta     DSCPTR
            pla
            sta     DSCPTR+1
            lda     Z59
            pha
            tya
            pha
            ldy     #$00
            txa
            rts

; ----------------------------------------------------------------------------
; "LEN" FUNCTION
; ----------------------------------------------------------------------------
LEN:        jsr     GETSTR
            jmp     SNGFLT

GETSTR:     jsr     FRESTR
            ldx     #$00
            stx     VALTYP
            tay
            rts

; ----------------------------------------------------------------------------
; "ASC" FUNCTION
; ----------------------------------------------------------------------------
ASC:        jsr     GETSTR
            beq     GOIQ
            ldy     #$00
            lda     (INDEX),y
            tay
            jmp     SNGFLT

; ----------------------------------------------------------------------------
GOIQ:       jmp     IQERR

; ----------------------------------------------------------------------------
; SCAN TO NEXT CHARACTER AND CONVERT EXPRESSION
; TO SINGLE BYTE IN X-REG
; ----------------------------------------------------------------------------
GTBYTC:     jsr     CHRGET

; ----------------------------------------------------------------------------
; EVALUATE EXPRESSION AT TXTPTR, AND
; CONVERT IT TO SINGLE BYTE IN X-REG
; ----------------------------------------------------------------------------
GETBYT:     jsr     FRMNUM

; ----------------------------------------------------------------------------
; CONVERT (FAC) TO SINGLE BYTE INTEGER IN X-REG
; ----------------------------------------------------------------------------
CONINT:     jsr     MKINT
            ldx     FAC_LAST-1
            bne     GOIQ
            ldx     FAC_LAST
            jmp     CHRGOT

; ----------------------------------------------------------------------------
; "VAL" FUNCTION
; ----------------------------------------------------------------------------
VAL:        jsr     GETSTR
            bne     L2A6B
            jmp     ZERO_FAC

L2A6B:      ldx     TXTPTR
            ldy     TXTPTR+1
            stx     STRNG2
            sty     STRNG2+1
            ldx     INDEX
            stx     TXTPTR
            clc
            adc     INDEX
            sta     DEST
            ldx     INDEX+1
            stx     TXTPTR+1
            bcc     L2A83
            inx
L2A83:      stx     DEST+1
            ldy     #$00
            lda     (DEST),y
            pha
            lda     #$00
            sta     (DEST),y
            jsr     CHRGOT
            jsr     FIN
            pla
            ldy     #$00
            sta     (DEST),y

; ----------------------------------------------------------------------------
; COPY STRNG2 INTO TXTPTR
; ----------------------------------------------------------------------------
POINT:      ldx     STRNG2
            ldy     STRNG2+1
            stx     TXTPTR
            sty     TXTPTR+1
            rts

; ----------------------------------------------------------------------------
; CODOS "MCALL" FUNCTION
; ----------------------------------------------------------------------------
MCALL:      jsr     FRMNUM
            jsr     GETADDR
L2AA8:      jmp     (LINNUM)

; ----------------------------------------------------------------------------
; EVALUATE "EXP1,EXP2"
;
; CONVERT EXP1 TO 16-BIT NUMBER IN LINNUM
; CONVERT EXP2 TO 8-BIT NUMBER IN X-REG
; ----------------------------------------------------------------------------
GTNUM:      jsr     FRMNUM
            jsr     GETADDR

; ----------------------------------------------------------------------------
; EVALUATE ",EXPRESSION"
; CONVERT EXPRESSION TO SINGLE BYTE IN X-REG
; ----------------------------------------------------------------------------
COMBYTE:    jsr     CHKCOM
            jmp     GETBYT

; ----------------------------------------------------------------------------
; CONVERT (FAC) TO A 16-BIT VALUE IN LINNUM
; ----------------------------------------------------------------------------
GETADDR:    lda     FACSIGN
            bmi     GOIQ
            lda     FAC
            cmp     #$91
            bcs     GOIQ
            jsr     QINT
            lda     FAC_LAST-1
            ldy     FAC_LAST
            sty     LINNUM
            sta     LINNUM+1
            rts

; ----------------------------------------------------------------------------
; "PEEK" FUNCTION
; ----------------------------------------------------------------------------
PEEK:       lda     LINNUM+1
            pha
            lda     LINNUM
            pha
            jsr     GETADDR
            ldy     #$00
            lda     (LINNUM),y
            tay
            pla
            sta     LINNUM
            pla
            sta     LINNUM+1
            jmp     SNGFLT

; ----------------------------------------------------------------------------
; "POKE" STATEMENT
; ----------------------------------------------------------------------------
POKE:       jsr     GTNUM
            txa
            ldy     #$00
            sta     (LINNUM),y
            rts

; ----------------------------------------------------------------------------
; "WAIT" STATEMENT
; ----------------------------------------------------------------------------
WAIT:       jsr     GTNUM
            stx     FORPNT
            ldx     #$00
            jsr     CHRGOT
            beq     L2AFC
            jsr     COMBYTE
L2AFC:      stx     FORPNT+1
            ldy     #$00
L2B00:      lda     (LINNUM),y
            eor     FORPNT+1
            and     FORPNT
            beq     L2B00
RTS3:       rts

TEMP1X = TEMP1+(5-BYTES_FP)

; ----------------------------------------------------------------------------
; ADD 0.5 TO FAC
; ----------------------------------------------------------------------------
FADDH:      lda     #<CON_HALF
            ldy     #>CON_HALF
            jmp     FADD

; ----------------------------------------------------------------------------
; FAC = (Y,A) - FAC
; ----------------------------------------------------------------------------
FSUB:       jsr     LOAD_ARG_FROM_YA

; ----------------------------------------------------------------------------
; FAC = ARG - FAC
; ----------------------------------------------------------------------------
FSUBT:      lda     FACSIGN
            eor     #$FF
            sta     FACSIGN
            eor     ARGSIGN
            sta     SGNCPR
            lda     FAC
            jmp     FADDT

; ----------------------------------------------------------------------------
; SHIFT SMALLER ARGUMENT MORE THAN 7 BITS
; ----------------------------------------------------------------------------
FADD1:      jsr     SHIFT_RIGHT
            bcc     FADD3

; ----------------------------------------------------------------------------
; FAC = (Y,A) + FAC
; ----------------------------------------------------------------------------
FADD:       jsr     LOAD_ARG_FROM_YA

; CODOS: Alternate FADDT
AFADDT:     lda     FAC

; ----------------------------------------------------------------------------
; FAC = ARG + FAC
; ----------------------------------------------------------------------------
FADDT:      bne     L2B31
            jmp     COPY_ARG_TO_FAC

L2B31:      ldx     FACEXTENSION
            stx     ARGEXTENSION
            ldx     #ARG
            lda     ARG
FADD2:      tay
            beq     RTS3
            sec
            sbc     FAC
            beq     FADD3
            bcc     L2B55
            sty     FAC
            ldy     ARGSIGN
            sty     FACSIGN
            eor     #$FF
            adc     #$00
            ldy     #$00
            sty     ARGEXTENSION
            ldx     #FAC
            bne     L2B59
L2B55:      ldy     #$00
            sty     FACEXTENSION
L2B59:      cmp     #$F9
            bmi     FADD1
            tay
            lda     FACEXTENSION
            lsr     GORESTART+1,x
            jsr     SHIFT_RIGHT4
FADD3:      bit     SGNCPR
            bpl     FADD4
            ldy     #FAC
            cpx     #ARG
            beq     L2B71
            ldy     #ARG
L2B71:      sec
            eor     #$FF
            adc     ARGEXTENSION
            sta     FACEXTENSION
            lda     GOSTROUT+1,y
            sbc     GOSTROUT+1,x
            sta     FAC+4
            lda     GOSTROUT,y
            sbc     GOSTROUT,x
            sta     FAC+3
            lda     GORESTART+2,y
            sbc     GORESTART+2,x
            sta     FAC+2
            lda     GORESTART+1,y
            sbc     GORESTART+1,x
            sta     FAC+1
; ----------------------------------------------------------------------------
; NORMALIZE VALUE IN FAC
; ----------------------------------------------------------------------------
NORMALIZE_FAC1:
            bcs     NORMALIZE_FAC2
            jsr     COMPLEMENT_FAC
NORMALIZE_FAC2:
            ldy     #$00
            tya
            clc
L2B9D:      ldx     FAC+1
            bne     NORMALIZE_FAC4
            ldx     FAC+2
            stx     FAC+1
            ldx     FAC+3
            stx     FAC+2
            ldx     FAC+4
            stx     FAC+3
            ldx     FACEXTENSION
            stx     FAC+4
            sty     FACEXTENSION
            adc     #$08
            cmp     #(MANTISSA_BYTES+1)*8
            bne     L2B9D

; ----------------------------------------------------------------------------
; SET FAC = 0
; (ONLY NECESSARY TO ZERO EXPONENT AND SIGN CELLS)
; ----------------------------------------------------------------------------
ZERO_FAC:
            lda     #$00
STA_IN_FAC_SIGN_AND_EXP:
            sta     FAC
STA_IN_FAC_SIGN:
            sta     FACSIGN
            rts

; ----------------------------------------------------------------------------
; ADD MANTISSAS OF FAC AND ARG INTO FAC
; ----------------------------------------------------------------------------
FADD4:      adc     ARGEXTENSION
            sta     FACEXTENSION
            lda     FAC+4
            adc     ARG+4
            sta     FAC+4
            lda     FAC+3
            adc     ARG+3
            sta     FAC+3
            lda     FAC+2
            adc     ARG+2
            sta     FAC+2
            lda     FAC+1
            adc     ARG+1
            sta     FAC+1
            jmp     NORMALIZE_FAC5

; ----------------------------------------------------------------------------
; FINISH NORMALIZING FAC
; ----------------------------------------------------------------------------
NORMALIZE_FAC3:
            adc     #$01
            asl     FACEXTENSION
            rol     FAC+4
            rol     FAC+3
            rol     FAC+2
            rol     FAC+1
NORMALIZE_FAC4:
            bpl     NORMALIZE_FAC3
            sec
            sbc     FAC
            bcs     ZERO_FAC
            eor     #$FF
            adc     #$01
            sta     FAC
NORMALIZE_FAC5:
            bcc     L2C08
NORMALIZE_FAC6:
            inc     FAC
            beq     OVERFLOW
            ror     FAC+1
            ror     FAC+2
            ror     FAC+3
            ror     FAC+4
            ror     FACEXTENSION
L2C08:      rts

; ----------------------------------------------------------------------------
; 2'S COMPLEMENT OF FAC
; ----------------------------------------------------------------------------
COMPLEMENT_FAC:
            lda     FACSIGN
            eor     #$FF
            sta     FACSIGN

; ----------------------------------------------------------------------------
; 2'S COMPLEMENT OF FAC MANTISSA ONLY
; ----------------------------------------------------------------------------
COMPLEMENT_FAC_MANTISSA:
            lda     FAC+1
            eor     #$FF
            sta     FAC+1
            lda     FAC+2
            eor     #$FF
            sta     FAC+2
            lda     FAC+3
            eor     #$FF
            sta     FAC+3
            lda     FAC+4
            eor     #$FF
            sta     FAC+4
            lda     FACEXTENSION
            eor     #$FF
            sta     FACEXTENSION
            inc     FACEXTENSION
            bne     RTS12

; ----------------------------------------------------------------------------
; INCREMENT FAC MANTISSA
; ----------------------------------------------------------------------------
INCREMENT_FAC_MANTISSA:
            inc     FAC+4
            bne     RTS12
            inc     FAC+3
            bne     RTS12
            inc     FAC+2
            bne     RTS12
            inc     FAC+1
RTS12:      rts

OVERFLOW:   ldx     #ERR_OVERFLOW
            jmp     ERROR

; ----------------------------------------------------------------------------
; SHIFT 1,X THRU 5,X RIGHT
; (A) = NEGATIVE OF SHIFT COUNT
; (X) = POINTER TO BYTES TO BE SHIFTED
;
; RETURN WITH (Y)=0, CARRY=0, EXTENSION BITS IN A-REG
; ----------------------------------------------------------------------------
SHIFT_RIGHT1:
            ldx     #RESULT-1
SHIFT_RIGHT2:
            ldy     GOSTROUT+1,x
            sty     FACEXTENSION
            ldy     GOSTROUT,x
            sty     GOSTROUT+1,x
            ldy     GORESTART+2,x
            sty     GOSTROUT,x
            ldy     GORESTART+1,x
            sty     GORESTART+2,x
            ldy     SHIFTSIGNEXT
            sty     GORESTART+1,x

; ----------------------------------------------------------------------------
; MAIN ENTRY TO RIGHT SHIFT SUBROUTINE
; ----------------------------------------------------------------------------
SHIFT_RIGHT:
            adc     #$08
            bmi     SHIFT_RIGHT2
            beq     SHIFT_RIGHT2
            sbc     #$08
            tay
            lda     FACEXTENSION
            bcs     SHIFT_RIGHT5
L2C68:      asl     GORESTART+1,x
            bcc     L2C6E
            inc     GORESTART+1,x
L2C6E:      ror     GORESTART+1,x
            ror     GORESTART+1,x

; ----------------------------------------------------------------------------
; ENTER HERE FOR SHORT SHIFTS WITH NO SIGN EXTENSION
; ----------------------------------------------------------------------------
SHIFT_RIGHT4:
            ror     GORESTART+2,x
            ror     GOSTROUT,x
            ror     GOSTROUT+1,x
            ror     a
            iny
            bne     L2C68
SHIFT_RIGHT5:
            clc
            rts

; ----------------------------------------------------------------------------
CON_ONE:    .byte   $81, $00, $00, $00, $00
POLY_LOG:   .byte   $03
            .byte   $7F, $5E, $56, $CB, $79
            .byte   $80, $13, $9B, $0B, $64
L2C8E:      .byte   $80, $76, $38, $93, $16
            .byte   $82, $38, $AA, $3B, $20
            .byte   $F0, $F0, $F1, $F7, $F5
CON_SQR_HALF:
            .byte   $80, $35, $04, $F3, $34
CON_SQR_TWO:
            .byte   $81, $35, $04, $F3, $34
CON_NEG_HALF:
            .byte   $80, $80, $00, $00, $00
CON_LOG_TWO:
            .byte   $80, $31, $72, $17, $F8

; ----------------------------------------------------------------------------
; "LOG" FUNCTION
; ----------------------------------------------------------------------------
LOG:        jsr     SIGN
            beq     GIQ
            bpl     LOG2

GIQ:        jmp     IQERR

LOG2:       lda     FAC
            sbc     #$7F
            pha
            lda     #$80
            sta     FAC
            lda     #<CON_SQR_HALF
            ldy     #>CON_SQR_HALF
            jsr     FADD
            lda     #<CON_SQR_TWO
            ldy     #>CON_SQR_TWO
            jsr     FDIV
            lda     #<CON_ONE
            ldy     #>CON_ONE
            jsr     FSUB
            lda     #<POLY_LOG
            ldy     #>POLY_LOG
            jsr     POLYNOMIAL_ODD
            lda     #<CON_NEG_HALF
            ldy     #>CON_NEG_HALF
            jsr     FADD
            pla
            jsr     ADDACC
            lda     #<CON_LOG_TWO
            ldy     #>CON_LOG_TWO

; ----------------------------------------------------------------------------
; FAC = (Y,A) * FAC
; ----------------------------------------------------------------------------
FMULT:      jsr     LOAD_ARG_FROM_YA

; CODOS: Alternate FMULTT
AFMULTT:    lda     FAC

; ----------------------------------------------------------------------------
; FAC = ARG * FAC
; ----------------------------------------------------------------------------
FMULTT:     bne     L2CF9
            jmp     L2D54

L2CF9:      jsr     ADD_EXPONENTS
            lda     #$00
            sta     RESULT
            sta     RESULT+1
            sta     RESULT+2
            sta     RESULT+3
            lda     FACEXTENSION
            jsr     MULTIPLY1
            lda     FAC+4
            jsr     MULTIPLY1
            lda     FAC+3
            jsr     MULTIPLY1
            lda     FAC+2
            jsr     MULTIPLY1
            lda     FAC+1
            jsr     MULTIPLY2
            jmp     COPY_RESULT_INTO_FAC

; ----------------------------------------------------------------------------
; MULTIPLY ARG BY (A) INTO RESULT
; ----------------------------------------------------------------------------
MULTIPLY1:  bne     MULTIPLY2
            jmp     SHIFT_RIGHT1

MULTIPLY2:  lsr     a
            ora     #$80
L2D2A:      tay
            bcc     L2D46
            clc
            lda     RESULT+3
            adc     ARG+4
            sta     RESULT+3
            lda     RESULT+2
            adc     ARG+3
            sta     RESULT+2
            lda     RESULT+1
            adc     ARG+2
            sta     RESULT+1
            lda     RESULT
            adc     ARG+1
            sta     RESULT
L2D46:      ror     RESULT
            ror     RESULT+1
            ror     RESULT+2
            ror     RESULT+3
            ror     FACEXTENSION
            tya
            lsr     a
            bne     L2D2A
L2D54:      rts

; ----------------------------------------------------------------------------
; UNPACK NUMBER AT (Y,A) INTO ARG
; ----------------------------------------------------------------------------
LOAD_ARG_FROM_YA:
            sta     INDEX
            sty     INDEX+1
            ldy     #BYTES_FP-1
            lda     (INDEX),y
            sta     ARG+4
            dey
            lda     (INDEX),y
            sta     ARG+3
            dey
            lda     (INDEX),y
            sta     ARG+2
            dey
            lda     (INDEX),y
            sta     ARGSIGN
            eor     FACSIGN
            sta     SGNCPR
            lda     ARGSIGN
            ora     #$80
            sta     ARG+1
            dey
            lda     (INDEX),y
            sta     ARG
            rts

; ----------------------------------------------------------------------------
; ADD EXPONENTS OF ARG AND FAC
; (CALLED BY FMULT AND FDIV)
;
; ALSO CHECK FOR OVERFLOW, AND SET RESULT SIGN
; ----------------------------------------------------------------------------
ADD_EXPONENTS:
            lda     ARG

ADD_EXPONENTS1:
            beq     ZERO
            clc
            adc     FAC
            bcc     L2D8B
            bmi     JOV
            clc
            .byte   $2C
L2D8B:      bpl     ZERO
            adc     #$80
            sta     FAC
            bne     L2D96
            jmp     STA_IN_FAC_SIGN

L2D96:      lda     SGNCPR
            sta     FACSIGN
            rts

; ----------------------------------------------------------------------------
; IF (FAC) IS POSITIVE, GIVE "OVERFLOW" ERROR
; IF (FAC) IS NEGATIVE, SET FAC=0, POP ONE RETURN, AND RTS
; CALLED FROM "EXP" FUNCTION
; ----------------------------------------------------------------------------
OUTOFRNG:   lda     FACSIGN
            eor     #$FF
            bmi     JOV

; ----------------------------------------------------------------------------
; POP RETURN ADDRESS AND SET FAC=0
; ----------------------------------------------------------------------------
ZERO:       pla
            pla
            jmp     ZERO_FAC

JOV:        jmp     OVERFLOW

; ----------------------------------------------------------------------------
; MULTIPLY FAC BY 10
; ----------------------------------------------------------------------------
MUL10:      jsr     COPY_FAC_TO_ARG_ROUNDED
            tax
            beq     L2DBF
            clc
            adc     #$02
            bcs     JOV
            ldx     #$00
            stx     SGNCPR
            jsr     FADD2
            inc     FAC
            beq     JOV
L2DBF:      rts

; ----------------------------------------------------------------------------
CONTEN:     .byte   $84, $20, $00, $00, $00

; ----------------------------------------------------------------------------
; DIVIDE FAC BY 10
; ----------------------------------------------------------------------------
DIV10:      jsr     COPY_FAC_TO_ARG_ROUNDED
            lda     #<CONTEN
            ldy     #>CONTEN
            ldx     #$00

; ----------------------------------------------------------------------------
; FAC = ARG / (Y,A)
; ----------------------------------------------------------------------------
DIV:        stx     SGNCPR
            jsr     LOAD_FAC_FROM_YA
            jmp     FDIVT

; ----------------------------------------------------------------------------
; FAC = (Y,A) / FAC
; ----------------------------------------------------------------------------
FDIV:       jsr     LOAD_ARG_FROM_YA

; CODOS: Alternate FDIVT
AFDIVT:     lda     FAC

; ----------------------------------------------------------------------------
; FAC = ARG / FAC
; ----------------------------------------------------------------------------
FDIVT:      beq     L2E53
            jsr     ROUND_FAC
            lda     #$00
            sec
            sbc     FAC
            sta     FAC
            jsr     ADD_EXPONENTS
            inc     FAC
            beq     JOV
            ldx     #-MANTISSA_BYTES
            lda     #$01
L2DF2:      ldy     ARG+1
            cpy     FAC+1
            bne     L2E08
            ldy     ARG+2
            cpy     FAC+2
            bne     L2E08
            ldy     ARG+3
            cpy     FAC+3
            bne     L2E08
            ldy     ARG+4
            cpy     FAC+4
L2E08:      php
            rol     a
            bcc     L2E15
            inx
            sta     RESULT_LAST-1,x
            beq     L2E43
            bpl     L2E47
            lda     #$01
L2E15:      plp
            bcs     L2E26
L2E18:      asl     ARG_LAST
            rol     ARG+3
            rol     ARG+2
            rol     ARG+1
            bcs     L2E08
            bmi     L2DF2
            bpl     L2E08
L2E26:      tay
            lda     ARG+4
            sbc     FAC+4
            sta     ARG+4
            lda     ARG+3
            sbc     FAC+3
            sta     ARG+3
            lda     ARG+2
            sbc     FAC+2
            sta     ARG+2
            lda     ARG+1
            sbc     FAC+1
            sta     ARG+1
            tya
            jmp     L2E18

L2E43:      lda     #$40
            bne     L2E15
L2E47:      asl     a
            asl     a
            asl     a
            asl     a
            asl     a
            asl     a
            sta     FACEXTENSION
            plp
            jmp     COPY_RESULT_INTO_FAC

L2E53:      ldx     #ERR_ZERODIV
            jmp     ERROR

; ----------------------------------------------------------------------------
; COPY RESULT INTO FAC MANTISSA, AND NORMALIZE
; ----------------------------------------------------------------------------
COPY_RESULT_INTO_FAC:
            lda     RESULT
            sta     FAC+1
            lda     RESULT+1
            sta     FAC+2
            lda     RESULT+2
            sta     FAC+3
            lda     RESULT+3
            sta     FAC+4
            jmp     NORMALIZE_FAC2

; CODOS: Alternate LOAD_FAC_FROM_YA
ALOAD_FAC_FROM_YA:
            jsr     LOAD_FAC_FROM_YA
            lda     FACSIGN
            eor     ARGSIGN
            sta     SGNCPR
            rts

; ----------------------------------------------------------------------------
; UNPACK (Y,A) INTO FAC
; ----------------------------------------------------------------------------
LOAD_FAC_FROM_YA:
            sta     INDEX
            sty     INDEX+1
            ldy     #MANTISSA_BYTES
            lda     (INDEX),y
            sta     FAC+4
            dey
            lda     (INDEX),y
            sta     FAC+3
            dey
            lda     (INDEX),y
            sta     FAC+2
            dey
            lda     (INDEX),y
            sta     FACSIGN
            ora     #$80
            sta     FAC+1
            dey
            lda     (INDEX),y
            sta     FAC
            sty     FACEXTENSION
            rts

; ----------------------------------------------------------------------------
; ROUND FAC, STORE IN TEMP2
; ----------------------------------------------------------------------------
STORE_FAC_IN_TEMP2_ROUNDED:
            ldx     #TEMP2
            .byte   $2C

; ----------------------------------------------------------------------------
; ROUND FAC, STORE IN TEMP1
; ----------------------------------------------------------------------------
STORE_FAC_IN_TEMP1_ROUNDED:
            ldx     #TEMP1X
            ldy     #$00
            beq     STORE_FAC_AT_YX_ROUNDED

; ----------------------------------------------------------------------------
; ROUND FAC, AND STORE WHERE FORPNT POINTS
; ----------------------------------------------------------------------------
SETFOR:     ldx     FORPNT
            ldy     FORPNT+1
            .byte   $24

; CODOS: Alternate STORE_FAC_AT_YX_ROUNDED
ASTORE_FAC_AT_YX_ROUNDED:
            tax

; ----------------------------------------------------------------------------
; ROUND FAC, AND STORE AT (Y,X)
; ----------------------------------------------------------------------------
STORE_FAC_AT_YX_ROUNDED:
            jsr     ROUND_FAC
            stx     INDEX
            sty     INDEX+1
            ldy     #MANTISSA_BYTES
            lda     FAC+4
            sta     (INDEX),y
            dey
            lda     FAC+3
            sta     (INDEX),y
            dey
            lda     FAC+2
            sta     (INDEX),y
            dey
            lda     FACSIGN
            ora     #$7F
            and     FAC+1
            sta     (INDEX),y
            dey
            lda     FAC
            sta     (INDEX),y
            sty     FACEXTENSION
            rts

; ----------------------------------------------------------------------------
; COPY ARG INTO FAC
; ----------------------------------------------------------------------------
COPY_ARG_TO_FAC:
            lda     ARGSIGN

MFA:        sta     FACSIGN
            ldx     #BYTES_FP

L2ED7:      lda     SHIFTSIGNEXT,x
            sta     EXPSGN,x
            dex
            bne     L2ED7
            stx     FACEXTENSION
            rts

; ----------------------------------------------------------------------------
; ROUND FAC AND COPY TO ARG
; ----------------------------------------------------------------------------
COPY_FAC_TO_ARG_ROUNDED:
            jsr     ROUND_FAC
MAF:        ldx     #BYTES_FP+1
L2EE6:      lda     EXPSGN,x
            sta     SHIFTSIGNEXT,x
            dex
            bne     L2EE6
            stx     FACEXTENSION
RTS14:      rts

; ----------------------------------------------------------------------------
; ROUND FAC USING EXTENSION BYTE
; ----------------------------------------------------------------------------
ROUND_FAC:
            lda     FAC
            beq     RTS14
            asl     FACEXTENSION
            bcc     RTS14

; ----------------------------------------------------------------------------
; INCREMENT MANTISSA AND RE-NORMALIZE IF CARRY
; ----------------------------------------------------------------------------
INCREMENT_MANTISSA:
            jsr     INCREMENT_FAC_MANTISSA
            bne     RTS14
            jmp     NORMALIZE_FAC6

; ----------------------------------------------------------------------------
; TEST FAC FOR ZERO AND SIGN
;
; FAC > 0, RETURN +1
; FAC = 0, RETURN  0
; FAC < 0, RETURN -1
; ----------------------------------------------------------------------------
SIGN:       lda     FAC
            beq     RTS15
L2F04:      lda     FACSIGN
SIGN2:      rol     a
            lda     #$FF
            bcs     RTS15
            lda     #$01
RTS15:      rts

; ----------------------------------------------------------------------------
; "SGN" FUNCTION
; ----------------------------------------------------------------------------
SGN:        jsr     SIGN

; ----------------------------------------------------------------------------
; CONVERT (A) INTO FAC, AS SIGNED VALUE -128 TO +127
; ----------------------------------------------------------------------------
FLOAT:      sta     FAC+1
            lda     #$00
            sta     FAC+2
            ldx     #$88

; ----------------------------------------------------------------------------
; FLOAT UNSIGNED VALUE IN FAC+1,2
; (X) = EXPONENT
; ----------------------------------------------------------------------------
FLOAT1:     lda     FAC+1
            eor     #$FF
            rol     a

; ----------------------------------------------------------------------------
; FLOAT UNSIGNED VALUE IN FAC+1,2
; (X) = EXPONENT
; C=0 TO MAKE VALUE NEGATIVE
; C=1 TO MAKE VALUE POSITIVE
; ----------------------------------------------------------------------------
FLOAT2:     lda     #$00
            sta     FAC+4
            sta     FAC+3
            stx     FAC
            sta     FACEXTENSION
            sta     FACSIGN
            jmp     NORMALIZE_FAC1

; ----------------------------------------------------------------------------
; "ABS" FUNCTION
; ----------------------------------------------------------------------------
ABS:        lsr     FACSIGN
            rts

; ----------------------------------------------------------------------------
; COMPARE FAC WITH PACKED # AT (Y,A)
; RETURN A=1,0,-1 AS (Y,A) IS <,=,> FAC
; ----------------------------------------------------------------------------
FCOMP:      sta     DEST

; ----------------------------------------------------------------------------
; SPECIAL ENTRY FROM "NEXT" PROCESSOR
; "DEST" ALREADY SET UP
; ----------------------------------------------------------------------------
FCOMP2:     sty     DEST+1
            ldy     #$00
            lda     (DEST),y
            iny
            tax
            beq     SIGN
            lda     (DEST),y
            eor     FACSIGN
            bmi     L2F04
            cpx     FAC
            bne     L2F67
            lda     (DEST),y
            ora     #$80
            cmp     FAC+1
            bne     L2F67
            iny
            lda     (DEST),y
            cmp     FAC+2
            bne     L2F67
            iny
            lda     (DEST),y
            cmp     FAC+3
            bne     L2F67
            iny
            lda     #$7F
            cmp     FACEXTENSION
            lda     (DEST),y
            sbc     FAC_LAST
            beq     L2F8F
L2F67:      lda     FACSIGN
            bcc     L2F6D
            eor     #$FF
L2F6D:      jmp     SIGN2

; ----------------------------------------------------------------------------
; QUICK INTEGER FUNCTION
;
; CONVERTS FP VALUE IN FAC TO INTEGER VALUE
; IN FAC+1...FAC+4, BY SHIFTING RIGHT WITH SIGN
; EXTENSION UNTIL FRACTIONAL BITS ARE OUT.
;
; THIS SUBROUTINE ASSUMES THE EXPONENT < 32.
; ----------------------------------------------------------------------------
QINT:       lda     FAC
            beq     QINT3
            sec
            sbc     #120+8*BYTES_FP
            bit     FACSIGN
            bpl     L2F84
            tax
            lda     #$FF
            sta     SHIFTSIGNEXT
            jsr     COMPLEMENT_FAC_MANTISSA
            txa
L2F84:      ldx     #FAC
            cmp     #$F9
            bpl     QINT2
            jsr     SHIFT_RIGHT
            sty     SHIFTSIGNEXT
L2F8F:      rts

QINT2:      tay
            lda     FACSIGN
            and     #$80
            lsr     FAC+1
            ora     FAC+1
            sta     FAC+1
            jsr     SHIFT_RIGHT4
            sty     SHIFTSIGNEXT
            rts

; ----------------------------------------------------------------------------
; "INT" FUNCTION
;
; USES QINT TO CONVERT (FAC) TO INTEGER FORM,
; AND THEN REFLOATS THE INTEGER.
; ----------------------------------------------------------------------------
INT:        lda     FAC
            cmp     #120+8*BYTES_FP
            bcs     RTS17
            jsr     QINT
            sty     FACEXTENSION
            lda     FACSIGN
            sty     FACSIGN
            eor     #$80
            rol     a
            lda     #120+8*BYTES_FP
            sta     FAC
            lda     FAC_LAST
            sta     CHARAC
            jmp     NORMALIZE_FAC1

QINT3:      sta     FAC+1
            sta     FAC+2
            sta     FAC+3
            sta     FAC+4
            tay
RTS17:      rts

; ----------------------------------------------------------------------------
; CONVERT STRING TO FP VALUE IN FAC
;
; STRING POINTED TO BY TXTPTR
; FIRST CHAR ALREADY SCANNED BY CHRGET
; (A) = FIRST CHAR, C=0 IF DIGIT.
; ----------------------------------------------------------------------------
FIN:        ldy     #$00
            ldx     #SERLEN-TMPEXP
L2FCC:      sty     TMPEXP,x
            dex
            bpl     L2FCC
            bcc     FIN2
            cmp     #$2D
            bne     L2FDB
            stx     SERLEN
            beq     FIN1
L2FDB:      cmp     #$2B
            bne     FIN3
FIN1:       jsr     CHRGET
FIN2:       bcc     FIN9
FIN3:       cmp     #$2E
            beq     FIN10
            cmp     #$45
            bne     FIN7
            jsr     CHRGET
            bcc     FIN5
            cmp     #TOKEN_MINUS
            beq     L3003
            cmp     #$2D
            beq     L3003
            cmp     #TOKEN_PLUS
            beq     FIN4
            cmp     #$2B
            beq     FIN4
            bne     FIN6

L3003:      ror     EXPSGN
FIN4:       jsr     CHRGET

FIN5:       bcc     GETEXP
FIN6:       bit     EXPSGN
            bpl     FIN7
            lda     #$00
            sec
            sbc     EXPON
            jmp     FIN8

; ----------------------------------------------------------------------------
; FOUND A DECIMAL POINT
; ----------------------------------------------------------------------------
FIN10:      ror     LOWTR
            bit     LOWTR
            bvc     FIN1

; ----------------------------------------------------------------------------
; NUMBER TERMINATED, ADJUST EXPONENT NOW
; ----------------------------------------------------------------------------
FIN7:       lda     EXPON
FIN8:       sec
            sbc     INDX
            sta     EXPON
            beq     L3037
            bpl     L3030
L3027:      jsr     DIV10
            inc     EXPON
            bne     L3027
            beq     L3037
L3030:      jsr     MUL10
            dec     EXPON
            bne     L3030
L3037:      lda     SERLEN
            bmi     L303C
            rts

L303C:      jmp     NEGOP

; ----------------------------------------------------------------------------
; ACCUMULATE A DIGIT INTO FAC
; ----------------------------------------------------------------------------
FIN9:       pha
            bit     LOWTR
            bpl     L3046
            inc     INDX
L3046:      jsr     MUL10
            pla
            sec
            sbc     #$30
            jsr     ADDACC
            jmp     FIN1

; ----------------------------------------------------------------------------
; ADD (A) TO FAC
; ----------------------------------------------------------------------------
ADDACC:     pha
            jsr     COPY_FAC_TO_ARG_ROUNDED
            pla
            jsr     FLOAT
            lda     ARGSIGN
            eor     FACSIGN
            sta     SGNCPR
            ldx     FAC
L3063:      jmp     FADDT

; ----------------------------------------------------------------------------
; ACCUMULATE DIGIT OF EXPONENT
; ----------------------------------------------------------------------------
GETEXP:     lda     EXPON
            cmp     #MAX_EXPON
            bcc     L3075
            lda     #$64
            bit     EXPSGN
            bmi     L3083
            jmp     OVERFLOW

L3075:      asl     a
            asl     a
            clc
            adc     EXPON
            asl     a
            clc
            ldy     #$00
            adc     (TXTPTR),y
            sec
            sbc     #$30
L3083:      sta     EXPON
            jmp     FIN4

; ----------------------------------------------------------------------------
CON_99999999_9:
            .byte   $9B, $3E, $BC, $1F, $FD

CON_999999999:
            .byte   $9E, $6E, $6B, $27, $FD

CON_BILLION:
            .byte   $9E, $6E, $6B, $28, $00

; ----------------------------------------------------------------------------
; PRINT "IN <LINE #>"
; ----------------------------------------------------------------------------
INPRT:      lda     #<QT_IN
            ldy     #>QT_IN
            jsr     GOSTROUT2
            lda     CURLIN+1
            ldx     CURLIN

; ----------------------------------------------------------------------------
; PRINT A,X AS DECIMAL INTEGER
; ----------------------------------------------------------------------------
LINPRT:     sta     FAC+1
            stx     FAC+2
            ldx     #$90
            sec
            jsr     FLOAT2
            jsr     FOUT
GOSTROUT2:  jmp     STROUT

; ----------------------------------------------------------------------------
; CONVERT (FAC) TO STRING STARTING AT STACK
; RETURN WITH (Y,A) POINTING AT STRING
; ----------------------------------------------------------------------------
FOUT:       ldy     #$01

; ----------------------------------------------------------------------------
; "STR$" FUNCTION ENTERS HERE, WITH (Y)=0
; SO THAT RESULT STRING STARTS AT STACK-1
; (THIS IS USED AS A FLAG)
; ----------------------------------------------------------------------------
FOUT1:      lda     #$20
            bit     FACSIGN
            bpl     L30BC
            lda     #$2D
L30BC:      sta     STACK2-1,y
            sta     FACSIGN
            sty     STRNG2
            iny
            lda     #$30
            ldx     FAC
            bne     L30CD
            jmp     FOUT4

L30CD:      lda     #$00
            cpx     #$80
            beq     L30D5
            bcs     L30DE
L30D5:      lda     #<CON_BILLION
            ldy     #>CON_BILLION
            jsr     FMULT

            lda     #-9
L30DE:      sta     INDX

; ----------------------------------------------------------------------------
; ADJUST UNTIL 1E8 <= (FAC) <1E9
; ----------------------------------------------------------------------------
L30E0:      lda     #<CON_999999999
            ldy     #>CON_999999999
            jsr     FCOMP
            beq     L3107
            bpl     L30FD
L30EB:      lda     #<CON_99999999_9
            ldy     #>CON_99999999_9
            jsr     FCOMP
            beq     L30F6
            bpl     L3104
L30F6:      jsr     MUL10
            dec     INDX
            bne     L30EB
L30FD:      jsr     DIV10
            inc     INDX
            bne     L30E0
L3104:      jsr     FADDH
L3107:      jsr     QINT
; ----------------------------------------------------------------------------
; FAC+1...FAC+4 IS NOW IN INTEGER FORM
; WITH POWER OF TEN ADJUSTMENT IN INDX
;
; IF -10 < INDX > 1, PRINT IN DECIMAL FORM
; OTHERWISE, PRINT IN EXPONENTIAL FORM
; ----------------------------------------------------------------------------
            ldx     #$01
            lda     INDX
            clc
            adc     #3*BYTES_FP-5
            bmi     L311C
            cmp     #3*BYTES_FP-4
            bcs     L311D
            adc     #$FF
            tax
            lda     #$02
L311C:      sec
L311D:      sbc     #$02
            sta     EXPON
            stx     INDX
            txa
            beq     L3128
            bpl     L313B
L3128:      ldy     STRNG2
            lda     #$2E
            iny
            sta     STACK2-1,y
            txa
            beq     L3139
L3133:      lda     #$30
            iny
            sta     STACK2-1,y
L3139:      sty     STRNG2
; ----------------------------------------------------------------------------
; NOW DIVIDE BY POWERS OF TEN TO GET SUCCESSIVE DIGITS
; ----------------------------------------------------------------------------
L313B:      ldy     #$00
            ldx     #$80
L313F:      lda     FAC_LAST
            clc
            adc     DECTBL+3,y
            sta     FAC+4
            lda     FAC+3
            adc     DECTBL+2,y
            sta     FAC+3
            lda     FAC+2
            adc     DECTBL+1,y
            sta     FAC+2
            lda     FAC+1
            adc     DECTBL,y
            sta     FAC+1
            inx
            bcs     L3163
            bpl     L313F
            bmi     L3165
L3163:      bmi     L313F
L3165:      txa
            bcc     L316C
            eor     #$FF
            adc     #$0A
L316C:      adc     #$2F
            iny
            iny
            iny
            iny
            sty     VARPNT
            ldy     STRNG2
            iny
            tax
            and     #$7F
            sta     STACK2-1,y
            dec     INDX
            bne     L3187
            lda     #$2E
            iny
            sta     STACK2-1,y
L3187:      sty     STRNG2
            ldy     VARPNT
            txa
            eor     #$FF
            and     #$80
            tax
            cpy     #DECTBL_END-DECTBL
            bne     L313F
; ----------------------------------------------------------------------------
; NINE DIGITS HAVE BEEN STORED IN STRING.  NOW LOOK
; BACK AND LOP OFF TRAILING ZEROES AND A TRAILING
; DECIMAL POINT.
; ----------------------------------------------------------------------------
            ldy     STRNG2
L3197:      lda     STACK2-1,y
            dey
            cmp     #$30
            beq     L3197
            cmp     #$2E
            beq     L31A4
            iny
L31A4:      lda     #$2B
            ldx     EXPON
            beq     L31D8
            bpl     L31B4
            lda     #$00
            sec
            sbc     EXPON
            tax
            lda     #$2D
L31B4:      sta     STACK2+1,y
            lda     #$45
            sta     STACK2,y
            txa
            ldx     #$2F
            sec
L31C0:      inx
L31C1:      sbc     #$0A
            bcs     L31C0
            adc     #$3A
            sta     STACK2+3,y
            txa
            sta     STACK2+2,y
            lda     #$00
            sta     STACK2+4,y
            beq     L31DD
FOUT4:      sta     STACK2-1,y
L31D8:      lda     #$00
            sta     STACK2,y
L31DD:      lda     #<STACK2
            ldy     #>STACK2
            rts

; CODOS
L31E2:      ldx     #GOSTROUT+2
            lda     #>(CONSOLE_STROUT+1)
            sta     GORESTART,x
            dex
            lda     #<(CONSOLE_STROUT+1)
            sta     GORESTART,x
            lda     #$FE            ; ERR_CODOS_INTEGRITY * 2
            lsr     a
            tax
            jmp     CODOS_ERROR

; ----------------------------------------------------------------------------
CON_HALF:   .byte   $80, $00, $00, $00, $00

; ----------------------------------------------------------------------------
; POWERS OF 10 FROM 1E8 DOWN TO 1,
; AS 32-BIT INTEGERS, WITH ALTERNATING SIGNS
; ----------------------------------------------------------------------------
DECTBL:     .byte   $FA, $0A, $1F, $00  ; -100000000
            .byte   $00, $98, $96, $80  ; 10000000
            .byte   $FF, $F0, $BD, $C0  ; -1000000
            .byte   $00, $01, $86, $A0  ; 100000
            .byte   $FF, $FF, $D8, $F0  ; -10000
            .byte   $00, $00, $03, $E8  ; 1000
            .byte   $FF, $FF, $FF, $9C  ; -100
            .byte   $00, $00, $00, $0A  ; 10
            .byte   $FF, $FF, $FF, $FF  ; -1
DECTBL_END:

; ----------------------------------------------------------------------------
; "SQR" FUNCTION
; ----------------------------------------------------------------------------
SQR:    jsr     COPY_FAC_TO_ARG_ROUNDED
        lda     #<CON_HALF
        ldy     #>CON_HALF
        jsr     LOAD_FAC_FROM_YA

; ----------------------------------------------------------------------------
; EXPONENTIATION OPERATION
;
; ARG ^ FAC  =  EXP( LOG(ARG) * FAC )
; ----------------------------------------------------------------------------
FPWRT:  beq     EXP
        lda     ARG
        bne     L3230
        jmp     STA_IN_FAC_SIGN_AND_EXP

L3230:  ldx     #<TEMP3
        ldy     #>TEMP3
        jsr     STORE_FAC_AT_YX_ROUNDED
        lda     ARGSIGN
        bpl     L324A
        jsr     INT
        lda     #<TEMP3
        ldy     #>TEMP3
        jsr     FCOMP
        bne     L324A
        tya
        ldy     CHARAC
L324A:  jsr     MFA
        tya
        pha
        jsr     LOG
        lda     #<TEMP3
        ldy     #>TEMP3
        jsr     FMULT
        jsr     EXP
        pla
        lsr     a
        bcc     L326A

; ----------------------------------------------------------------------------
; NEGATE VALUE IN FAC
; ----------------------------------------------------------------------------
NEGOP:  lda     FAC
        beq     L326A
        lda     FACSIGN
        eor     #$FF
        sta     FACSIGN
L326A:  rts

; ----------------------------------------------------------------------------
CON_LOG_E:  .byte   $81, $38, $AA, $3B, $29
POLY_EXP:   .byte   $07
            .byte   $71, $34, $58, $3E, $56
            .byte   $74, $16, $7E, $B3, $1B
            .byte   $77, $2F, $EE, $E3, $85
            .byte   $7A, $1D, $84, $1C, $2A
            .byte   $7C, $63, $59, $58, $0A
            .byte   $7E, $75, $FD, $E7, $C6
            .byte   $80, $31, $72, $18, $10
            .byte   $81, $00, $00, $00, $00

; ----------------------------------------------------------------------------
; "EXP" FUNCTION
;
; FAC = E ^ FAC
; ----------------------------------------------------------------------------
EXP:        lda     #<CON_LOG_E
            ldy     #>CON_LOG_E
            jsr     FMULT
            lda     FACEXTENSION
            adc     #$50
            bcc     L32A9
            jsr     INCREMENT_MANTISSA

L32A9:      sta     ARGEXTENSION
            jsr     MAF
            lda     FAC
            cmp     #$88
            bcc     L32B7

L32B4:      jsr     OUTOFRNG

L32B7:      jsr     INT
            lda     CHARAC
            clc
            adc     #$81
            beq     L32B4
            sec
            sbc     #$01
            pha
            ldx     #BYTES_FP

L32C7:      lda     ARG,x
            ldy     FAC,x
            sta     FAC,x
            sty     ARG,x
            dex
            bpl     L32C7
            lda     ARGEXTENSION
            sta     FACEXTENSION
            jsr     FSUBT
            jsr     NEGOP
            lda     #<POLY_EXP
            ldy     #>POLY_EXP
            jsr     POLINOMIAL
            lda     #$00
            sta     SGNCPR
            pla
            jsr     ADD_EXPONENTS1
            rts

; ----------------------------------------------------------------------------
; ODD POLYNOMIAL SUBROUTINE
;
; F(X) = X * P(X^2)
;
; WHERE:  X IS VALUE IN FAC
;	Y,A POINTS AT COEFFICIENT TABLE
;	FIRST BYTE OF COEFF. TABLE IS N
;	COEFFICIENTS FOLLOW, HIGHEST POWER FIRST
;
; P(X^2) COMPUTED USING NORMAL POLYNOMIAL SUBROUTINE
; ----------------------------------------------------------------------------
POLYNOMIAL_ODD:
            sta     STRNG2
            sty     STRNG2+1
            jsr     STORE_FAC_IN_TEMP1_ROUNDED
            lda     #TEMP1X
            jsr     FMULT
            jsr     SERMAIN
            lda     #<TEMP1X
            ldy     #>TEMP1X
            jmp     FMULT

; ----------------------------------------------------------------------------
; NORMAL POLYNOMIAL SUBROUTINE
;
; P(X) = C(0)*X^N + C(1)*X^(N-1) + ... + C(N)
;
; WHERE:  X IS VALUE IN FAC
;	Y,A POINTS AT COEFFICIENT TABLE
;	FIRST BYTE OF COEFF. TABLE IS N
;	COEFFICIENTS FOLLOW, HIGHEST POWER FIRST
; ----------------------------------------------------------------------------
POLINOMIAL: sta     STRNG2
            sty     STRNG2+1

SERMAIN:    jsr     STORE_FAC_IN_TEMP2_ROUNDED
            lda     (STRNG2),y
            sta     SERLEN
            ldy     STRNG2
            iny
            tya
            bne     L3315
            inc     STRNG2+1
L3315:      sta     STRNG2
            ldy     STRNG2+1
L3319:      jsr     FMULT
            lda     STRNG2
            ldy     STRNG2+1
            clc
            adc     #BYTES_FP
            bcc     L3326
            iny
L3326:      sta     STRNG2
            sty     STRNG2+1
            jsr     FADD
            lda     #TEMP2
            ldy     #$00
            dec     SERLEN
            bne     L3319
RTS19:      rts

; ----------------------------------------------------------------------------
; "RND" FUNCTION
; ----------------------------------------------------------------------------

; <<< THESE ARE MISSING ONE BYTE FOR FP VALUES >>>
; (non CONFIG_SMALL)
CONRND1:    .byte   $98, $35, $44, $7A
CONRND2:    .byte   $68, $28, $B1, $46

RND:        jsr     SIGN
            tax
            bmi     L335C
            lda     #<(RNDSEED+3)   ; Bug?
            ldy     #>(RNDSEED+3)
            jsr     LOAD_FAC_FROM_YA
            txa
            beq     RTS19
            lda     #<CONRND1
            ldy     #>CONRND1
            jsr     FMULT
            lda     #<CONRND2
            ldy     #>CONRND2
            jsr     FADD
L335C:      ldx     FAC_LAST
            lda     FAC+1
            sta     FAC_LAST
            stx     FAC+1
            lda     #$00
            sta     FACSIGN
            lda     FAC
            sta     FACEXTENSION
            lda     #$80
            sta     FAC
            jsr     NORMALIZE_FAC2
            ldx     #<(RNDSEED+3)     ; <-- FIXME: Seems like a bug, should be
                                    ; RNDSEED. Have to investigate it further
            ldy     #>(RNDSEED+3)
GOMOVMF:    jmp     STORE_FAC_AT_YX_ROUNDED

SIN_COS_TAN_ATN:
; ----------------------------------------------------------------------------
; "COS" FUNCTION
; ----------------------------------------------------------------------------
COS:        lda     #<CON_PI_HALF
            ldy     #>CON_PI_HALF
            jsr     FADD

; ----------------------------------------------------------------------------
; "SIN" FUNCTION
; ----------------------------------------------------------------------------
SIN:        jsr     COPY_FAC_TO_ARG_ROUNDED
            lda     #<CON_PI_DOUB
            ldy     #>CON_PI_DOUB
            ldx     ARGSIGN
            jsr     DIV
            jsr     COPY_FAC_TO_ARG_ROUNDED
            jsr     INT
            lda     #$00
            sta     STRNG1
            jsr     FSUBT
; ----------------------------------------------------------------------------
; (FAC) = ANGLE AS A FRACTION OF A FULL CIRCLE
;
; NOW FOLD THE RANGE INTO A QUARTER CIRCLE
;
; <<< THERE ARE MUCH SIMPLER WAYS TO DO THIS >>>
; ----------------------------------------------------------------------------
            lda     #<QUARTER
            ldy     #>QUARTER
            jsr     FSUB
            lda     FACSIGN
            pha
            bpl     SIN1
            jsr     FADDH
            lda     FACSIGN
            bmi     L33B6
            lda     CPRMASK
            eor     #$FF
            sta     CPRMASK
; ----------------------------------------------------------------------------
; IF FALL THRU, RANGE IS 0...1/2
; IF BRANCH HERE, RANGE IS 0...1/4
; ----------------------------------------------------------------------------
SIN1:       jsr     NEGOP
; ----------------------------------------------------------------------------
; IF FALL THRU, RANGE IS -1/2...0
; IF BRANCH HERE, RANGE IS -1/4...0
; ----------------------------------------------------------------------------
L33B6:      lda     #<QUARTER
            ldy     #>QUARTER
            jsr     FADD
            pla
            bpl     L33C3
            jsr     NEGOP
L33C3:      lda     #<POLY_SIN
            ldy     #>POLY_SIN
            jmp     POLYNOMIAL_ODD

; ----------------------------------------------------------------------------
; "TAN" FUNCTION
;
; COMPUTE TAN(X) = SIN(X) / COS(X)
; ----------------------------------------------------------------------------
TAN:        jsr     STORE_FAC_IN_TEMP1_ROUNDED
            lda     #$00
            sta     CPRMASK
            jsr     SIN
            ldx     #TEMP3
            ldy     #$00
            jsr     GOMOVMF
            lda     #TEMP1+(5-BYTES_FP)
            ldy     #$00
            jsr     LOAD_FAC_FROM_YA
            lda     #$00
            sta     FACSIGN
            lda     CPRMASK
            jsr     TAN1
            lda     #TEMP3
            ldy     #$00
            jmp     FDIV

TAN1:       pha
            jmp     SIN1

; ----------------------------------------------------------------------------
CON_PI_HALF:
            .byte   $81, $49, $0F, $DA, $A2
CON_PI_DOUB:
            .byte   $83, $49, $0F, $DA, $A2
QUARTER:    .byte   $7F, $00, $00, $00, $00
POLY_SIN:   .byte   $05, $84, $E6, $1A, $2D, $1B, $86, $28
            .byte   $07, $FB, $F8, $87, $99, $68, $89, $01
            .byte   $87, $23, $35, $DF, $E1, $86, $A5, $5D
            .byte   $E7, $28, $83, $49, $0F, $DA, $A2

; PET encoded easter egg text since CBM2
MICROSOFT:  .byte   $A1, $54, $46, $8F, $13, $8F, $52, $43
            .byte   $89, $CD

; ----------------------------------------------------------------------------
; "ATN" FUNCTION
; ----------------------------------------------------------------------------
ATN:        lda     FACSIGN
            pha
            bpl     L3436
            jsr     NEGOP
L3436:      lda     FAC
            pha
            cmp     #$81
            bcc     L3444
            lda     #<CON_ONE
            ldy     #>CON_ONE
            jsr     FDIV
; ----------------------------------------------------------------------------
; 0 <= X <= 1
; 0 <= ATN(X) <= PI/8
; ----------------------------------------------------------------------------
L3444:      lda     #<POLY_ATN
            ldy     #>POLY_ATN
            jsr     POLYNOMIAL_ODD
            pla
            cmp     #$81
            bcc     L3457
            lda     #<CON_PI_HALF
            ldy     #>CON_PI_HALF
            jsr     FSUB
L3457:      pla
            bpl     L345D
            jmp     NEGOP
L345D:      rts

; ----------------------------------------------------------------------------
POLY_ATN:
            .byte   $0B
            .byte	$76, $B3, $83, $BD, $D3
            .byte	$79, $1E, $F4, $A6, $F5
            .byte	$7B, $83, $FC, $B0, $10
            .byte   $7C, $0C, $1F, $67, $CA
            .byte	$7C, $DE, $53, $CB, $C1
            .byte	$7D, $14, $64, $70, $4C
            .byte	$7D, $B7, $EA, $51, $7A
            .byte	$7D, $63, $30, $88, $7E
            .byte	$7E, $92, $44, $99, $3A
            .byte	$7E, $4C, $CC, $91, $C7
            .byte	$7F, $AA, $AA, $AA, $13
            .byte   $81, $00, $00, $00, $00

; CODOS
L349B:      lda     #$FF
            bit     L0802
            bne     L34D7
            rts

L34A3:      lda     #$00
            sta     VALTYP+1
            jsr     FRMEVL
            bit     VALTYP
            bmi     L34BA
            bit     VALTYP+1
            bmi     L34B6
            lda     #$80
            bne     L34BC

L34B6:      lda     #$40
            bne     L34BC

L34BA:      lda     #$20
L34BC:      bit     L0801
            bne     L34C6
            ldx     #ERR_BADTYPE    
            jmp     ERROR

L34C6:      sta     L0801
L34C9:      lda     #$00
            sta     VALTYP+1
            lda     #$FF
            bit     L0802
            bne     L34D7
            bit     L0801
L34D7:      bmi     L34FD
            bvs     L350F
            bit     VALTYP
            bmi     L34EE
            jsr     L3501
            ldy     #$00
            jsr     FOUT1
            lda     #$FF
            ldy     #$00
            jsr     STRLIT
L34EE:      ldy     #$02
            lda     (FAC+3),y
            tax
            dey
            lda     (FAC+3),y
            pha
            dey
            lda     (FAC+3),y
            tay
            pla
            rts

L34FD:      bit     VALTYP
            bmi     L350C
L3501:      bit     VALTYP+1
            bpl     L3527
            lda     FAC_LAST-1
            ldy     FAC_LAST
            jmp     GIVAYF

L350C:      jmp     VAL

L350F:      bit     VALTYP
            bpl     L3516
            jsr     L34FD
L3516:      bit     VALTYP+1
            bmi     L3523
            lda     FAC
            cmp     #$91
            bcs     L3528
            jsr     QINT
L3523:      lda     FAC_LAST
            ldx     FAC_LAST-1
L3527:      rts

L3528:      jmp     IQERR

L352B:      jsr     _TSTKEY
            bcs     L3531
            rts

L3531:      cmp     #$03
            beq     L3542
            cmp     XOFF
            bne     L3542
            lda     #$00
            sta     LSTKEY
            lda     XOFF
L3542:      sec
            rts

L3544:      jsr     L1129
            lda     #$00
            sta     TXTPTR+1
            sec
            jsr     L142F

SETDEFEXT:  sta     DEFAULTEXT
            rts

; ----------------------------------------------------------------------------
; CODOS "BYE" STATEMENT
; ----------------------------------------------------------------------------
BYE:        jsr     L03D1
            lda     #$00
            sta     NOPRREGS
            jsr     L03E3
            jmp     _WARMST

; ----------------------------------------------------------------------------
; CODOS: Set error recovery routine
;        YA - Pointer tor routine
;        If Y = 0, set default
; ----------------------------------------------------------------------------
SETERRRCVRY:
            cpy     #$00
            bne     L3569
            lda     #<ERRRCVRY
            ldy     #>ERRRCVRY
L3569:      sta     ERRRCVRYP
            sty     ERRRCVRYP+1
            rts

.ifdef mtu
; ----------------------------------------------------------------------------
; CODOS "TONE" STATEMENT
; ----------------------------------------------------------------------------
TONE:       bne     L3580
TONE2:      lda     $BFDB           ; User 6522 auxiliary control register

            and     #$E3
            sta     $BFDB
            lda     #$0F
            sta     $BFDA           ; $BFDA User 6522 shift register
            rts

L3580:      jsr     GETBYT
            stx     $BFD8           ; User 6522 timer 2
            jsr     CHRGOT
            beq     L35A4
            cmp     #$2C
            bne     JSYNE
            jsr     GTBYTC
            stx     $BFF1           ; System 2 6522 Audio DAC port data register
            jsr     CHRGOT
            beq     L35A4
            cmp     #$2C
            bne     JSYNE
            jsr     GTBYTC
            stx     $BFDA           ; $BFDA User 6522 shift register
L35A4:      lda     $BFDB           ; User 6522 timer 2
            and     #$E3
            ora     #$10
            sta     $BFDB           ; User 6522 timer 2
            lda     $BFDA           ; $BFDA User 6522 shift register
            rts

JSYNE:      jmp     SYNERR
.endif

; ----------------------------------------------------------------------------
; CODOS: WARM RESTART ENTRY
; ----------------------------------------------------------------------------
CODOS_RESTART:
            jsr     L03D1
            lda     #<L0E3A
            ldy     #>L0E3A
            sta     JPOSTERR+1
            sty     JPOSTERR+2
            lda     #$4C
            sta     JPOSTERR
            lda     #<L3544
            ldy     #>L3544
            sta     CNTRLC+1
            sty     CNTRLC+2
            lda     #'B'
            sta     DEFAULTEXT
            lda     #$80
            sta     NOPRREGS
            jsr     L03CE
            jmp     RESTART

; Everything below this point is freed for programs memory
; after initialization

RAMSTART2:

GENERIC_CHRGET:
            inc     TXTPTR
            bne     GENERIC_CHRGOT
            inc     TXTPTR+1

            ; Self-modifying code, EA60 is replaced

GENERIC_CHRGOT:
GENERIC_TXTPTR = GENERIC_CHRGOT + 1
            lda     $EA60               ;
            cmp     #$3A                ; Is it a number?
            bcs     L35F8               ; No, return
GENERIC_CHRGOT2:
            cmp     #$20                ; Is it a space?
            beq     GENERIC_CHRGET      ; Yes, jump
            sec                         ; Clear borrow for substraction
            sbc     #$30
            sec
            sbc     #$D0
L35F8:      rts

; ----------------------------------------------------------------------------
; INITIAL VALUE FOR RANDOM NUMBER, ALSO COPIED
; IN ALONG WITH CHRGET, BUT ERRONEOUSLY:
; <<< THE LAST BYTE IS NOT COPIED >>>
; (on all non-CONFIG_SMALL)
; ----------------------------------------------------------------------------
GENERIC_RNDSEED:
            .byte   $80
            .byte   $4F
            .byte   $C7
            .byte   $52
            .byte   $58
GENERIC_CHRGET_END:

.ifdef mtu
; This is some sort of primitive "User lock" system
;
; The MTU-130 has some special registers with the "Vendor number", "Group number" and "User number"
; CODOS Basic checks that the User number matches with the code below and fails with an "Integrity"
; Error if don't
;
USRNPOS:    .byte   $0B             ; Position of user number code in special registers
USRNCODE:   .byte   $00             ; Expected User Number
            .byte   $00             ;
            .byte   $01             ;
            .byte   $07             ;
            .byte   $05             ;
.endif

INIT:       ldy     CMDLIDX
LOOP:       lda     (INPBUFP),y
            cmp     #$0D
            beq     L3612
            iny
            bne     LOOP
            jmp     _WARMST

L3612:      lda     #$00
            sta     (INPBUFP),y
            ldx     #$FF
            stx     CURLIN+1
            txs
            stx     L0838
            stx     L0839
            lda     #<INIT
            ldy     #>INIT
            sta     GORESTART+1
            sty     GORESTART+2
            sta     GOSTROUT+1
            sty     GOSTROUT+2
            lda     #<AYINT
            ldy     #>AYINT
            sta     GOAYINT
            sty     GOAYINT+1
            lda     #<GIVAYF
            ldy     #>GIVAYF
            sta     GOGIVEAYF
            sty     GOGIVEAYF+1
            lda     #$4C
            sta     GORESTART
            sta     GOSTROUT
            sta     JMPADRS
            lda     YLNLIM
            sta     POSX
            sta     L0813
            lda     #$B4
            sta     Z17
            ldx     #GENERIC_CHRGET_END-GENERIC_CHRGET+2
L3653:      lda     GENERIC_CHRGET-1,x
            sta     CHRGET-1,x
            dex
            bne     L3653
            lda     #$03
            sta     DSCLEN
            txa
            sta     SHIFTSIGNEXT
            sta     LASTPT+1
            pha
            sta     Z14
            sta     L0800
            sta     Z77
            sta     Z9B
            sta     L080A
            sta     L080E
            sta     L0812
            lda     #$60
            sta     L080B
            sta     L080F

            ldx     #$06
L3681:      lda     L37F9,x
            sta     ICHANNEL,x
            dex
            bpl     L3681

            ldx     #$03
L368C:      lda     L3800,x
            sta     L0814,x
            lda     L3804,x
            sta     L09B0,x
            dex
            bpl     L368C

            jsr     CRDO
            ldx     #$1D
            stx     TEMPPT
            clc
            lda     CMDLIDX
            adc     INPBUFP
            sta     TXTPTR
            lda     INPBUFP+1
            adc     #$00
            sta     TXTPTR+1
            jsr     CHRGOT
            tay
            bne     L36BD
L36B5:      lda     TOPMEM
            ldy     TOPMEM+1
            bne     L36E3
L36BD:      jsr     CHRGOT
            jsr     LINGET
            ldy     LINNUM+1
            beq     L36B5
            lda     LINNUM
            cpy     L3808+1
            beq     L36D2
            bcs     L36D7
            bcc     L36DD
L36D2:      cmp     L3808
            bcc     L36DD
L36D7:      lda     L3808
            ldy     L3808+1
L36DD:      sta     TOPMEM
            sty     TOPMEM+1
L36E3:      sta     MEMSIZ
            sty     MEMSIZ+1
            jsr     L1B7F
            jsr     CHRGOT
            tay
            beq     L371D
            cmp     #$2C
            beq     L36F7
L36F4:      jmp     _WARMST

L36F7:      jsr     CHRGET
            tay
            beq     L371D
            jsr     LINGET
            lda     LINNUM+1
            bne     L36F4
            lda     LINNUM
            cmp     #$10
            bcc     L36F4
            sta     POSX
L370C:      sbc     L380A
            bcs     L370C
            eor     #$FF
            sbc     L380A
            sbc     #$02
            clc
            adc     POSX
            sta     Z17
L371D:      ldx     #<RAMSTART2
            ldy     #>RAMSTART2
            stx     TXTTAB
            sty     TXTTAB+1
            ldy     #$00
            tya
            sta     (TXTTAB),y
            inc     TXTTAB
            bne     L3730
            inc     TXTTAB+1
L3730:

; CODOS - User lock routine --------------------------------------------------
.ifdef mtu
            ldx     USRNPOS         ; Position of the User Number in the special registers
            sta     SPREGREN        ; Prepares reading of special registers
L3736:      lda     SPREGREAD       ; Read byte from special registers
            dex                     ; and discard it
            bne     L3736           ; Until we arrive to the User number

L373C:      lda     SPREGREAD       ; Read register value
            and     #$0F            ; Mask out upper nibble
            cmp     USRNCODE,y      ; Compare to registered user code
            bne     USRNMISM        ; Not equal, integrity fail, you pirate!
            iny                     ; Next byte
            cpy     #$05            ; Continue until all 5 digits are read
            bne     L373C           ;
            beq     L3767           ; And (always) jump

; Usr Number mismatch
;
USRNMISM:   lda     #<WARMST
            sta     GOSTROUT+1
            lda     #>WARMST
            sta     GOSTROUT+2
            lda     #$4C
            sta     GOSTROUT
            jsr     L03D1
            jsr     L03E3
            jsr     L03CE
            ldx     #ERR_CODOS_INTEGRITY
            jmp     CODOS_ERROR
.endif
; ----------------------------------------------------------------------------

L3767:      jsr     SCRTCH
            lda     TXTTAB
            ldy     TXTTAB+1
            jsr     REASON
            jsr     CRDO
            lda     MEMSIZ
            sec
            sbc     TXTTAB
            tax
            lda     MEMSIZ+1
            sbc     TXTTAB+1
            jsr     LINPRT
            lda     #<QT_BYTES_FREE
            ldy     #>QT_BYTES_FREE
            jsr     STROUT
            lda     #<CONSOLE_STROUT
            ldy     #>CONSOLE_STROUT
            sta     GOSTROUT+1
            sty     GOSTROUT+2
            lda     #<CODOS_RESTART
            ldy     #>CODOS_RESTART
            sta     GORESTART+1
            sty     GORESTART+2
            jmp     (GORESTART+1)

L379B:
QT_BYTES_FREE:
            .byte   " BYTES FREE", $0D, $0D
L37A8:
QT_BASIC:
            .byte   "MTU-130 BASIC  V1.0   (C) 1981 MTU", $0D, $00
            .byte   "COPYRIGHT 1981, MICRO TECHNOLOGY UNLIMITED", $0D, $00, $00

; Initialization data
L37F9:      .byte   $00             ; System channel
            .byte   $01             ; Input channel (console)
            .byte   $02             ; Output channel (console)
            .byte   $80             ; Minimum what???
            .byte   $87             ; Maximum what???

            .addr   MEMORY_TOP      ; Top of memory

L3800:      .addr   L0818           ; Buffer for???
            .word   $000A           ; Buffer count (10)

L3804:      .addr   L09B4           ; Buffer for??
            .word   $0015           ; Buffer count (21)

L3808:      .addr   MEMORY_TOP

L380A:      .byte   $0A

.ifdef mtu
CODE_SIZE = * - START
.else
CODE2_SIZE = * - RESTORE
.endif


; CODOS INTERFACE JUMP TABLE
;
            .segment "systemif"

            ; Loadable file data
            ;
            .byte   $58             ; CODOS loadable file header byte
            .byte   $00             ; Memory overlay
            .byte   $00             ; Memory bank
            .byte   $00             ; Reserved
            .addr   ENTRY           ; Entry point
            .addr   _GETCHAR        ; Load address
            .word   SYSIF_SIZE      ; Memory image size

_GETCHAR:   jmp     GETCHAR
_OUTCHAR:   jmp     OUTCHAR
_TSTKEY:    jmp     TSTKEY
_GETLINE:   jmp     GETLINE
_FSCAN:     jmp     FSCAN
_ASSIGN:    jmp     ASSIGN
_OUTMBUFF:  jmp     OUTMBUFF
_GETMBUFF:  jmp     GETMBUFF
_FSEEK:     jmp     FSEEK
_FREECH:    jmp     FREECH
_SETINPBCH: jmp     SETINPBCH
_WARMST:    jmp     WARMST
_ERROR12:   jmp     ERROR12
_ERROR11:   jmp     ERROR11
_ERROR03:   jmp     ERROR03

L03CE:      ldx     #$02
            .byte   $2C             ; BIT abs (Old trick to skip next two bytes)

L03D1:      ldx     #$00
            stx     HSRCW
            rts

L03D7:      lda     ASSIGNFLAG
            rts

L03DB:      sta     CURRDRV
            rts

L03DF:      .byte   $00, $05, $00, $06

L03E3:      jmp     INIJMPTBL       ; E834

L03E6:      jmp     SETERRRCVRY

L03E9:      lda     #$00
            sta     PERRPFLG
            rts

L03EF:      sta     IGNORWRP
            rts

L03F3:      jmp     FTRUNC

SYSIF_SIZE = * - _GETCHAR

; USER INTERFACE JUMP TABLE
;
            .segment "jmptable"

            ; Loadable file data
            ;
            .byte   $58             ; CODOS loadable file header byte
            .byte   $00             ; Memory overlay
            .byte   $00             ; Memory bank
            .byte   $00             ; Reserved
            .addr   ENTRY           ; Entry point
            .addr   L0700           ; Load address
            .word   JMPTBL_SIZE     ; Memory image size

L0700:      jmp     L34A3
            jmp     L349B
            jmp     ERROR2
            jmp     CHKCOM
            jmp     SYNCHR
            jmp     SYNERR
            jmp     IQERR
            jmp     FRMEVL
            jmp     PARCHK
            jmp     APTRGET
            jmp     L1869
            jmp     FRESTR
            jmp     L27A3
            jmp     STROUT
            jmp     ALOAD_FAC_FROM_YA
            jmp     LOAD_ARG_FROM_YA
            jmp     ASTORE_FAC_AT_YX_ROUNDED
            jmp     MAF
            jmp     COPY_ARG_TO_FAC
            jmp     ROUND_FAC
            jmp     AFADDT
            jmp     FSUBT
            jmp     AFMULTT
            jmp     AFDIVT
            jmp     FCOMP
            jmp     COS
            jmp     SIN
            jmp     TAN
            jmp     ATN
            jmp     LOG
            jmp     EXP
            jmp     FPWRT
            jmp     CLEAR1
            jmp     L0F30
            jmp     LINGET
            jmp     L1150
            jmp     FNDLIN
            jmp     SAVE
            jmp     L25FE
            jmp     L1251
            jmp     PARSE_INPUT_LINE
            jmp     ERRUNDEF

JMPTBL_SIZE = * - L0700

            .endscope

            .end