

            .include "monomeg.inc"

            .importzp DESTBUFF, MEMBUFF, MEMCOUNT, P0SCRATCH, PCSAVE
            .importzp TMPBUFP, TMPPTR, BYTRES


            .segment "ioscratch0" : zeropage

; $F0 - $FF : Zero-page Scratch RAM for console I-0
;
            .exportzp QLN

QLN:        .res    2              ; $F0 Ptr to line-buffer used for INLINE and EDLINE
VRAMDST:    .res    2              ; $F2 - $F3 Video ram dest for graphic funcs
VRAMORG:    .res    2              ; $F4 - $F5 Video ram origin for graphic funcs
CHARFNTP:   .res    2              ; $F6 - $F7 Pointer to character font
VRAMCNT:    .res    2              ; $F8 - $F9 Video ram count for graphic funcs
TEMP1:      .res    1              ; $FA Temporary storage for keyboard routine
TEMP2:      .res    1              ; $FB Temporary storage for screen routines
CHRDISPL:   .res    1              ; $FC Horizontal displacement from the 8x5 char matrix
NUMCHRS:    .res    1              ; $FD Number of chars in input buffer
UNKNWN16:   .res    1              ; $FE
UNKNWN17:   .res    1              ; $FF

            .segment "ioscratch"

; Scratch ram used by Console I-O and graphics drivers
;
ASVBP:      .res    1               ; $02B0 - Temporary storage for A in BEEP proc.
TMPMASK:    .res    2               ; $02B1-$02B2 - Temporary storage for video masks
L02B3:      .res    1               ; $02B3
L02B4:      .res    2               ; $02B4
L02B6:      .res    7               ; $02B6
L02BD:      .res    1               ; $02BD
L02BE:      .res    2               ; $02BE
L02C0:      .res    7               ; $02C0
L02C7:      .res    1               ; $02C7
L02C8:      .res    1               ; $02C8
L02C9:      .res    1               ; $02C9
TCURS:      .res    2               ; $02CA
L02CC:      .res    1               ; $02CC
XSVKB:      .res    1               ; $02CD
YSVKB:      .res    1               ; $02CE
L02CF:      .res    1               ; $02CF
FNBNK:      .res    1               ; $02D0 Bank of current character table
L02D1:      .res    1               ; $02D1
L02D2:      .res    1               ; $02D2
L02D3:      .res    1               ; $02D3
TMPIDX:     .res    1               ; $02D4 Temporary storage for index caalculations
CURPOS:     .res    1               ; $02D5 Cursor position in line buffer
XSAVLI:     .res    1               ; $02D6 Save X in line editing functions
INSFLAG:    .res    1               ; $02D7 Flag. It bit 7 = 1, insert enabled
L02D8:      .res    1               ; $02D8
L02D9:      .res    1               ; $02D9
L02DA:      .res    1               ; $02DA
L02DB:      .res    1               ; $02DB
FNJUMP:     .res    2               ; $02DC Jump to special key function
L02DE:      .res    1               ; $02DE
L02DF:      .res    1               ; $02DF

            .segment "iodata"

; Loadable file data
;
            .byte   $58             ; CODOS loadable file header byte
            .byte   $00             ; Memory overlay
            .byte   $00             ; Memory bank
            .byte   $00             ; Reserved
            .addr   INITIO          ; Entry point
            .addr   COL             ; Load address
            .word   IODATA_SIZE     ; Memory image size

            .export YLNLIM

COL:        .byte   $01             ; $0200 CURRENT COLUMN LOCATION OF TEXT CURSOR 1-80.
LINE:       .byte   $01             ; $0201 CURRENT LINE NUMBER OF TEXT CURSOR. 1-NLINET.
UNK0:       .byte   $00
UNK1:       .byte   $00
UNK2:       .byte   $00
UNK3:       .byte   $00
UNK4:       .byte   $00
UNK5:       .byte   $00
UNK6:       .byte   $00
UNK7:       .byte   $00
UNK8:       .byte   $80             ; $020A
UNK9:       .byte   $F0             ; $020B
UNK10:      .byte   $F0             ; $020C
LSTKEY:     .byte   $00             ; $020D KEYBOARD KEY LAST DOWN
RPTFLG:     .byte   $00             ; $020E FLAG USED BY AUTO REPEAT ALGORITHM
KBECHO:     .byte   $00             ; $020F IF BIT 7=1 THEN "ECHO" EACH KEY TO THE DISPLAY.
NOLFCR:     .byte   $00             ; $0210    IF BIT 7=1 THEN NO AUTOMATIC LINE FEED AFTER CR.
NOSCRL:     .byte   $00             ; $0211 IF BIT 7=1 THEN INSTEAD OF SCROLLING, THE TEXT WINDOW IS CLEARED AND THE CURSOR IS HOMED WHEN TEXT GOES BEYOND THE BOOTOM LINE.
UNDRLN:     .byte   $00             ; $0212    IF BIT 7=1 THEN ALL CHARACTERS UNDERLINED WHEN DRAWN.
NOCLIK:     .byte   $00             ; $0213    IF BIT 7=1 THEN NO CLICK WHEN A KEY IS PRESSED.
NOBELL:     .byte   $00             ; $0214 IF BIT 7=1 THEN BEL CHARACTER IS IGNORED.
RVIDEO:     .byte   $00             ; $0215 IF BIT 7=1 THEN CHARACTERS ARE DRAWN IN REVERSE VIDEO.
SHODEL:     .byte   $00             ; $0216    IF BIT 7=1 THEN DISPLAY DEL (ROBOUT) AS A CHARACTER SHAPE
SHOUL:      .byte   $00             ; $0217    IF BIT 7=1 THEN CHARACTER CELL IS ERASED BEFORE THE UNDERLINE CHARACTER IS DRAWN.
EXCCP:      .byte   $00             ; $0218 IF BIT 7=1 THEN CALL USER CONTROL CHARACTER PROCESSOR.
EXTHI:      .byte   $00             ; $0219 IF BIT 7=1 THEN CALL USER RUTINE TO PROCESS ALL CHARACTERS WHEN BIT 7 SET.
EXFONT:     .byte   $00             ; $021A    IF BIT 7=1 THEN USE EXTERNAL FONT TABLE.
CURVIS:     .byte   $00             ; $021B FLAG INDICATING PRESENT STATE OF CURSOR
UNK15:      .byte   $00             ; $021C
CRSRWRAP:   .byte   $00             ; $021D FLAG INDICATING IF A CURSOR WRAP OCCURRED
NLINET:     .byte   $18             ; $021E NUMBER OF TEXT LINES IN THE TEXT WINDOW.
YTDOWN:     .byte   $00             ; $021F 255-(Y COORDINATE OF TOP OF THE TEXT WINDOW).
DBCDLA:     .byte   $05             ; $0220 WAIT TIME IN MILLISECONDS ALLOWED FOR CONTACT BOUNCE.
RPTRAT:     .byte   $C3             ; $0221 INTERCHARACTER REPEAT DELAY IN 256uS UNITS.
CURDLA:     .byte   $06             ; $0222 DETERMINES CURSOR BLINK SPEED, 0=NO BLINK.
UNK17:      .byte   $05             ; $0223
CLKPER:     .byte   $05             ; $0224 CLICK WAVEFORM PERIOD IN UNITS OF 200 MICROSECONDS.
CLKVOL:     .byte   $20             ; $0225 CLICK VOLUME, $00 = MINIMUM, $7F = MAXIMUM.
CLKCY:      .byte   $02             ; $0226 CLICK DURATION IN UNITS OF COMPLETE WAVEFORM CYCLES
BELPER:     .byte   $05             ; $0227 BELL SOUND WAVEFORM PERIOD IN UNITS OF 200 MICROSECONDS.
BELVOL:     .byte   $40             ; $0228    BELL SOUND VOLUME, $00 = MINIMUM, $7F MAXIMUM.
BELCY:      .byte   $0C             ; $0229    BELL SOUND DURATION IN UNITS OF COMPLETE WAVEFORM CYCLES.
UNK18:      .byte   $07             ; $022A
UNK19:      .byte   $08             ; $022B
UNK20:      .byte   $09             ; $022C
UNK21:      .byte   $0C             ; $022D
UNK22:      .byte   $18             ; $022E
QEXCC:      .word   ERR37           ; $022F ADDRESS OF EXTERNAL CONTROL CHARACTER PROCESSOR IF USED.
QEXFNT:     .word   ERR37           ; $0231 ADDRESS OF EXTERNAL FONT TABLE IF USED.
QEXHI7:     .word   ERR37           ; $0233    ADDRESS OF EXTERNAL PROCESSOR FOR CHARACTERS WITH BIT 7=1
FNTTBL:     .word   CHTB            ; $0235 CHARACTER FONT TABLE
EXFTBK:     .byte   $00             ; $0237    MEMORY BANK NUMBER CONTAINING EXTERNAL FONT TABLE.
YLNLIM:     .byte   $C0             ; $0238 LINE SIZE LIMIT FOR INLINE AND ENDLINE ENTRY POINTS
NOLEKO:     .byte   $00             ; $0239    ECHO FLAG NORMALLY 0 BUT IF SET TO 80 WILL DISABLE KEYBOARD ECHO
UKINLN:     .byte   $00             ; $023A IF BIT 7=1 THEN IRRECOGNIZED KEYS ARE ACCEPTED FOR ENTRY POINTS INLINE AND ENDLINE.
SPKTBL:     .word   _SPKTBL         ; $023B KEYBOARD SPECIAL KEYS TABLE
UNK23:      .byte   $00             ; $023D

IODATA_SIZE = * - COL

            .segment "iodrvjmp"

            ; Loadable file data
            ;
            .byte   $58             ; CODOS loadable file header byte
            .byte   $00             ; Memory overlay
            .byte   $00             ; Memory bank
            .byte   $00             ; Reserved
            .addr   INITIO          ; Entry point
            .addr   GETKEY          ; Load address
            .word   JMPTBL_SIZE     ; Memory image size

            .export GETKEY, OUTCH, TSTKEY, INITIO, CLRDSP, DRWLEG, INLINE, EDLINE
            .export SDRAW, SMOVE, SDRAWR, SMOVER, SVEC, SVECR, SDOT, SDOTR, SGRIN
            .export SLTPEN, SDRWCH, SISDOT, SOFFGC, SONGC, SINTLP, STSTLP, IFKEY
            .export INITTW, DEFTW, CLRHTW, HOMETW, CRLF, CLRTW, CLRLEG, CLRTLN
            .export LINEFD, OFFTCR, ONTCR, FLPTCR, TIOON, IORES, BEEP

GETKEY:     jmp     _GETKEY     ; Wait until a keyboard key is struck and return character in A
OUTCH:      jmp     _OUTCH      ; Display printable character or interpret control character
TSTKEY:     jmp     _TSTKEY     ; Test if a key is pressed
INITIO:     jmp     _INITIO     ; Clear screen and set default values of display parameters

CLRDSP:     jmp     _CLRDSP     ; Clear the entire VIDHRES by VIDVRES screen
DRWLEG:     jmp     _DRWLEG     ; Draw legend boxes
            jmp     ERR37
ERR37:      jmp     JERROR37    ; Skips graphic routines if no graphic driver was loaded.
                                ; If so appropriate JMPs are automatically updated.
INLINE:     jmp     _INLINE     ; Input an entire line from the keyboard
EDLINE:     jmp     _EDLINE     ; EDIT AN ENTIRE LINE USING THE KEYBOARD
SDRAW:      jmp     ERR37       ; DRAW A SOLID VECTOR FROM THE CURSOR TO (XX,YY)
SMOVE:      jmp     ERR37       ; MOVE GRAPHIC CURSOR TO (XX,YY) WITHOUT DRAWING
SDRAWR:     jmp     ERR37       ; DRAW A SOLID WHITE VECTOR RELATIVE TO THE CURSOR
SMOVER:     jmp     ERR37       ; MOVE THE GRAPHIC CURSOR RELATIVE TO ITS PRESENT POSITION
SVEC:       jmp     ERR37       ; DRAW A VECTOR FROM THE CURSOR TO (XX,YY) ACCORDING TO GMODE and DSHPAT
SVECR:      jmp     ERR37       ; DRAW A VECTOR RELATIVE TO THE CURSOR ACCORDING TO GMODE and DSHPAT
SDOT:       jmp     ERR37       ; DRAW A SINGLE DOT (PIXEL) AT (XX,YY) ACCORDING TO GMODE
SDOTR:      jmp     ERR37       ; DRAW A SINGLE DOT (PIXEL) AT A POSITION RELATIVE TO THE CURSOR ACCORDING TO GMODE
SGRIN:      jmp     ERR37       ; ALLOW USER COORDINATE INPUT BY MANEUVERING A CURSOR WITH THE KEYBOAR CURSOR CONTROL KEYS
SLTPEN:     jmp     ERR37       ; ACTIVATE LIGHT PEN FFOR ONE FRAME and RETURN COORDINATES OF HIT, IF ANY
            jmp     ERR37
SDRWCH:     jmp     ERR37       ; DRAW A SINGLE CHARACTER AT (XX,YY)
SISDOT:     jmp     ERR37       ; DETERMINE PIXEL AT (XX,YY) IS ON OR OFF
SOFFGC:     jmp     ERR37       ; TURN OFF THE GRAPHIC CROSSHAIR CURSOR
SONGC:      jmp     ERR37       ; TURN ON THE GRAPHIC CROSSHAIR CURSOR
SINTLP:     jmp     ERR37       ; WAIT FOR END OF FRAME and THEN ACTIVATE THE LIGHT PEN
STSTLP:     jmp     ERR37       ; TEST FOR LIGHT PEN HIT and RETURN COORDINATES IF A HIT
            jmp     ERR37
            jmp     ERR37
            jmp     ERR37
IFKEY:      jmp     _IFKEY      ; IFKEY - TEST IF A KEY IS PRESSED WITHOUT MULTIPLE RECOGNITION LOCKOUT
INITTW:     jmp     _INITTW     ; INITTW - INITIALIZE THE TEXT WINDOW TO 24 LINES and CLEAR THE TEXT WINDOW ONLY
DEFTW:      jmp     _DEFTW      ; DEFTW - SET THE POSITION and SIZE OF THE TEXT WINDOW
CLRHTW:     jmp     _CLRHTW     ; CLRHTW - CLEAR THE TEXT WINDOW and HOME THE CURSOR.
HOMETW:     jmp     _HOMETW     ; HOMETW - PLACE THE CURSOR IN THE HOME POSITION (COL=1, LINE=1)
CRLF:       jmp     _CRLF       ; CRLF - MOVE CURSOR TO THE LEFT SCREEN EDGE and DOWN ONE LINE
CLRTW:      jmp     _CLRTW      ; CLRTW - CLEAR THE TEXT WINDOW WITHOUT MOVING THE CURSOR
CLRLEG:     jmp     _CLRLEG     ; CLRLEG - CLEAR THE LEGEND DISPLAY AREA (BOTTOMMOST 16 SCAN LINES)
CLRTLN:     jmp     _CLRTLN     ; CLRTLN - CLEAR A SPECIFIED TEXT LINE
LINEFD:     jmp     _LINEFD     ; LINEFD - MOVE CURSOR DOWN  ONE TEXT LINE
OFFTCR:     jmp     _OFFTCR     ; OFFTCR - TURN THE TEXT CURSOR OFF IF IT IS ON
ONTCR:      jmp     _ONTCR      ; ONTCR - TURN THE TEXT CURSOR ON
FLPTCR:     jmp     _FLPTCR     ; FLPTCR - FLIP THE VIDEO SENSE OF THE CURSOR AT THE CURSOR POSITION

TIOON:      jmp     _TIOON      ; SYSTEM ROUTINE TO FORCE I/O SELECTION
IORES:      jmp     _IORES      ; SYSTEM ROUTINE TO RESTORE I-O/RAM SETTING
BEEP:       jmp     _BEEP       ; SOUND AN AUDIBLE BEEP
            jmp     ERR37
            jmp     ERR37

JMPTBL_SIZE = * - GETKEY

            .segment "tabtbl"

            ; Loadable file data
            ;
            .byte   $58             ; CODOS loadable file header byte
            .byte   $00             ; Memory overlay
            .byte   $00             ; Memory bank
            .byte   $00             ; Reserved
            .addr   INITIO          ; Entry point
            .addr   TABTBL          ; Load address
            .word   TABTBL_SIZE     ; Memory image size

            .export TABTBL

TABTBL:     .byte   $09, $11, $19, $21, $29, $31, $39, $41
            .byte   $49, $00, $00, $00, $00, $00, $00, $00
            .byte   $00, $00, $00, $00, $00, $00, $00, $00
            .byte   $00, $00, $00, $00, $00, $00, $00, $00

TABTBL_SIZE = * - TABTBL

            .segment "iodriver"

            ; Loadable file data
            ;
            .byte   $58             ; CODOS loadable file header byte
            .byte   $00             ; Memory overlay
            .byte   $00             ; Memory bank
            .byte   $00             ; Reserved
            .addr   INITIO          ; Entry point
            .addr   SPKTRL          ; Load address
            .word   IODRIVER_SIZE   ; Memory image size

; Keyboard special keys with translation
;
SPKTRL:     .byte   $8A             ; '*' MULTIPLY
            .byte   $2A             ; '*' MULTIPLY
            .byte   $8B             ; '/' DIVIDE
            .byte   $2F             ; '/' DIVIDE
            .byte   $8C             ; '-' MINUS
            .byte   $2D             ; '-' MINUS
            .byte   $8D             ; '+' PLUS
            .byte   $2B             ; '+' PLUS
            .byte   $FF
            .byte   $FF
            .byte   $FF
            .byte   $FF

; Keyboard special keys
;
_SPKTBL:    .byte   $02             ; 'STX'
            .byte   $03             ; 'ETX' (^C)
            .byte   $05             ; 'ENQ' (^E)
            .byte   $07             ; 'BEL'
            .byte   $08             ; 'BS'  (^H)
            .byte   $09             ; 'HT'  (^I)
            .byte   $0A             ; 'LF'
            .byte   $0B             ; 'VT'
            .byte   $0C             ; 'FF'
            .byte   $0D             ; 'RETURN'
            .byte   $12             ; 'DC2'
            .byte   $17             ; 'ETB'
            .byte   $18             ; 'CAN'
            .byte   $1A             ; 'SUB'
            .byte   $1B             ; 'ESC'
            .byte   $7F             ; 'DEL'
            .byte   $8E             ; 'ENTER'
            .byte   $A0             ; 'CURSOR UP'
            .byte   $A1             ; 'CURSOR LEFT'
            .byte   $A2             ; 'CURSOR RIGHT'
            .byte   $A3             ; 'CURSOR DOWN'
            .byte   $A4             ; 'HOME'
            .byte   $A5             ; 'DELETE'
            .byte   $A6             ; 'INSERT'
            .byte   $B0             ; 'SHIFT/CURSOR UP'
            .byte   $B1             ; 'SHIFT/CURSOR LEFT'
            .byte   $B2             ; 'SHIFT/CURSOR RIGHT'
            .byte   $B3             ; 'SHIFT/CURSOR DOWN'
            .byte   $B4             ; 'SHIFT/ HOME
            .byte   $FF
            .byte   $FF
SPKTBLSIZ = * - _SPKTBL

; Special keys jump table
;
SPKJMP:     .addr   SKSTX           ; STX
            .addr   CNTRLC          ; CTRL-C
            .addr   SKENQ           ; ENQ (^E)
            .addr   SKBEL           ; BEL
            .addr   SKBS            ; BS  (^H)
            .addr   SKHT            ; HT  (^I)
            .addr   SKLF            ; LF
            .addr   SKVT            ; VT
            .addr   SKFF            ; FF
            .addr   SKRET           ; RETURN
            .addr   SKDC2           ; DC2
            .addr   SKETB           ; ETB
            .addr   SKCAN           ; CAN
            .addr   SKSUB           ; SUB
            .addr   SKESC           ; ESC
            .addr   SKSUP           ; DEL
            .addr   SKRET           ; ENTER
            .addr   SKVT            ; CURSOR UP
            .addr   SKBS            ; CURSOR LEFT
            .addr   SKCSRR          ; CURSOR RIGHT
            .addr   SKLF            ; CURSOR DOWN
            .addr   SKHOME          ; HOME
            .addr   SKDEL           ; DELETE
            .addr   SKINS           ; INSERT
            .addr   SKVT            ; SHIFT CURSOR UP
            .addr   SKSCSRL         ; SHIFT CURSOR LEFT
            .addr   SKSCSRR         ; SHIFT CURSOR RIGHT
            .addr   SKLF            ; SHIFT CURSOR DOWN
            .addr   SKFF            ; SHIFT HOME
            .addr   $0000
            .addr   $0000

; INLINE - input an entire line from the keyboard, with editing permitted
;
; Arguments: None (QLN must be set)
;
; Arguments returned: A = number of characters in the line, Y = 0, X preserved
;                     QLN points to the complted line
;
.proc _INLINE
            ldy     #$00            ; Init line index
            ; Fall through
.endproc

; EDLINE - Edit an entire line using the keyboard
;
; Arguments: Y=indexes the implied CR at the end of the line to be edited
;            QLN (address $00F) points to start of line to be edited
;
; Arguments returned: A = number of characters in the line, Y = 0, X preserved
;                     QLN points to the complted line
;
.proc _EDLINE
            sty     NUMCHRS         ; Update number of chars
            cld
            lda     UNK23
            sta     L02DA
            stx     XSAVLI          ; Preserve X
UPDSCRN:    jsr     OUTLBUF         ; Output line buffer to screen
CLRINSFLG:  lda     #$00            ; Clear insert flag
            sta     INSFLAG         ;
GKLOOP:     jsr     _GETKEY         ; Get key
            cmp     #$7F            ; Printable?
            bcs      SPECIAL        ; No, it is an special or function key
            cmp     #' '            ; Maybe:
            bcc     CHKSPCL         ; Character below space, go check special keys
KNORMAL:    jsr     NKEYMNG         ; Normal character key
            jmp     GKLOOP          ; And continue processing the input line

SPECIAL:    beq     CHKSPCL         ; Char is $7F (DEL)
            cmp     #$88            ; Check id it is a function key
            bcs     CHKSPCL         ; No, check if special key
            jsr     FNKEY           ; Yes, manage it
            cmp     #$0D            
            bne     GKLOOP          ; And continue processing the input line
CHKSPCL:    ldx     #SPKTBLSIZ-1    ; Search for the char into the SPKTBL
CHKSPNX:    cmp     _SPKTBL,x       ; Found?
            beq     FJMP            ; Yes, go jump to handler function
            dex                     ; No, check next
            bpl     CHKSPNX         ; Repeat until no more
            ldx     #$0A            ; Now check if it is an special key with translation
SPTLOOP:    cmp     SPKTRL,x        ; Match?
            beq     TRNSLTE         ; Yes, translate to normal equivalent
            dex                     ; No, advance to next entry
            dex                     ;
            bpl     SPTLOOP         ; Repeat until no more or match
            bit     UKINLN          ; Unrecognized keys allowed?
            bmi     KNORMAL         ; Yes, continue as normal key
            bpl     GKLOOP          ; No, continue processing the input line
FJMP:       txa                     ; Multiply by two for use in jump table
            asl     a               ;
            tax                     ;
            lda     SPKJMP,x        ; Get function address
            sta     FNJUMP          ;
            lda     SPKJMP+1,x      ;
            sta     FNJUMP+1        ;
            jmp     (FNJUMP)        ; And jump
TRNSLTE:    inx                     ; Advance to next pos in table (key equivalent)
            lda     SPKTRL,x        ; Get the equivalent
            jmp     KNORMAL         ; And continue as normal key
.endproc

; Internal procedure: Handle ^B command (Recall a previously typed line)
;
.proc SKSTX
            jsr     LC817
            jsr     LC926
            jmp     _EDLINE::UPDSCRN
.endproc

; Internal procedure: Handle ^E command (Turn off/on echo of keyboard characters to CRT)
;
.proc SKENQ
            lda     NOLEKO          ; Get "No keyboard echo" flag
            eor     #$FF            ; Flip it
            sta     NOLEKO          ; Store it back
            jmp     _EDLINE::GKLOOP ; And continue processing the input line
.endproc

; Internal procedure: Handle BEL special character
;
.proc SKBEL
            jsr     RNGBEL          ; Ring the bell
            jmp     _EDLINE::GKLOOP ; And continue processing the input line
.endproc

; Internal procedure: Handle ^H (BS) special character
;
.proc SKBS
            cpy     CURPOS
            beq     LC6A8
            dey                     ; Decrement line index
UPDC:       jsr     BSCOORD         ; Updates text window coordinates for a BS
                                    ; Clear insert flag and continue processing
            jmp     _EDLINE::CLRINSFLG

LC6A8:      cpy     NUMCHRS         ; Are we at the end of the line?
            beq     UPDC            ; Yes, update coordinates and continue processing
            bne     BSHTRET         ; No, continue without updating coordinates
            ; Fall through
.endproc

; Internal procedure: Handle ^I (horizontal tabulator)
;
.proc SKHT
            jsr     HORIZTAB
            ; Fall through
.endproc

; Common return for SKBS and SKHT
;
BSHTRET:                            ; Clear insert flag and continue processing
            jmp     _EDLINE::CLRINSFLG

; Internal procedure - Manage line feed key
;
.proc SKLF
            tya                     ; Transfer cursor position to A
            clc                     ; Clear Cy for addition
            adc     #TXTHRES        ; Add line length
            bcs     LC6C5           ; If overflows
            cmp     NUMCHRS         ; Or past or equal last char in buffer
            bcs     LC6C5           ; Yes, go update
            tay                     ; Update line buffer index
LC6BF:      jsr     _LINEFD         ; Line feed to screen
            jmp     _EDLINE::CLRINSFLG ; Clear insert flag and continue processing

LC6C5:      cpy     NUMCHRS         ; Last char of line?
            bne     LC70D           ; No, it is past it
            sty     CURPOS          ; Yes
            jmp     LC6BF
.endproc

SKVT:       tya
            sec
            sbc     #$50
            bcc     LC6E1
            cmp     CURPOS
            bcc     LC6E1
            tay
LC6DB:      jsr     LCF08
            jmp     _EDLINE::CLRINSFLG ; Clear insert flag and continue processing

LC6E1:      cpy     NUMCHRS
            bne     LC70D
            sty     CURPOS
            jmp     LC6DB

SKFF:       jsr     _CLRHTW
LC6EE:      sty     CURPOS
            jmp     _EDLINE::CLRINSFLG ; Clear insert flag and continue processing

SKRET:      clc
            ldy     NUMCHRS
LC6F7:      lda     #$0D
            sta     (QLN),y
            php
            jsr     OUTCIFEON
            jsr     LC8F7
            plp
            tya
            ldy     #$00
            ldx     XSAVLI
            rts

SKDC2:      jsr     OUTLBUF
LC70D:      jmp     _EDLINE::CLRINSFLG ; Clear insert flag and continue processing

; Internal procedure: Manage ^W (delete to end-of-line) command
;
.proc SKETB
            jsr     DELTOEOL
            jmp     _EDLINE::CLRINSFLG ; Clear insert flag and continue processing
.endproc

SKCAN:      jsr     LC817
            jmp     _EDLINE::CLRINSFLG ; Clear insert flag and continue processing

SKSUB:      cpy     #$00
            bne     LC70D
            beq     LC6F7
SKESC:      jsr     _GETKEY
            jmp     _EDLINE::KNORMAL

; Internal procedure - Manage SUP key
;
.proc SKSUP
            jsr     KSUPR
            jmp     _EDLINE::GKLOOP
.endproc

; Unused, it is exactly the same as SKDEL
;
.proc SKDEL2
            jsr     LC849
            jmp     _EDLINE::GKLOOP
.endproc

SKCSRR:     cpy     NUMCHRS
            bcs     LC73F
            iny
LC739:      jsr     CURSORR
            jmp     _EDLINE::CLRINSFLG ; Clear insert flag and continue processing

LC73F:      cpy     CURPOS
            beq     LC739
            bne     LC70D
SKHOME:     jsr     _HOMETW
            jmp     LC6EE

SKDEL:      jsr     LC849
            jmp     _EDLINE::GKLOOP

; Internal procedure: Manage INS key. Set the insert flag.
;
SKINS:      sec                     ; Set insert flag
            ror     INSFLAG         ;
            jmp     _EDLINE::GKLOOP ; And continue processing the input line

SKSCSRL:    lda     CURPOS
            sta     TMPIDX
            jsr     LC8A1
            jmp     _EDLINE::CLRINSFLG ; Clear insert flag and continue processing

SKSCSRR:    cpy     NUMCHRS
            beq     LC70D
            jsr     CURSORR
            iny
            bne     SKSCSRR
            ; Fall through

; Internal procedure - Manage normal keys
;
.proc NKEYMNG
            cpy     YLNLIM          ; Reached line limit?
            bcc     NOEND           ; No, continue
            jmp     RNGBEL          ; Yes, ring the bell and return

NOEND:      bit     INSFLAG         ; Check insert flag
            bpl     NOINS           ; No, continue
            jmp     INSCHAR         ; Yes, jump to insert

NOINS:      sta     (QLN),y         ; Save char into input line buffer
            cpy     NUMCHRS         ; Are we past the last char in buffer?
            bcc     ADVANCE         ; No, go to advance pos
            inc     NUMCHRS         ; Yes, increment number of chars
ADVANCE:    iny                     ;
            ; Fall through
.endproc

; Internal procedure - Output char to screen if echo is not disabled
;
.proc OUTCIFEON
            bit     NOLEKO          ; Check keyboard echo flag
            bmi     NOECHO          ; If no echo, skip
            jmp     OUTCH           ; Echo on, output the character
NOECHO:     jmp     CURSORR         ; Advance cursor to the right and return
.endproc

; Internal procedure - del character at cursor position
;
.proc KSUPR
            cpy     CURPOS
            beq     RETURN
            bit     INSFLAG
            bpl     LC7A4
            jsr     BSCOORD         ; Updates text window coordinates for a BS
            dey
            jmp     LC849

LC7A4:      cpy     NUMCHRS
            bcc     LC7AA
            dec     NUMCHRS
LC7AA:      dey
            jsr     BSCOORD         ; Updates text window coordinates for a BS
            lda     #$20
            sta     (QLN),y
            jsr     OUTCIFEON
            jsr     BSCOORD         ; Updates text window coordinates for a BS
RETURN:     rts
.endproc

.proc DELTOEOL
            sty     TMPIDX          ; STore current index to temp variable (never used?)
LOOP:       cpy     NUMCHRS         ; Have we reached end of line?
            bcs     LC7C8           ; Yes, ...
            lda     #' '            ; No, overwrite with blank
            jsr     OUTCIFEON       ;
            iny                     ; Advance position
            bne     LOOP            ; And repeat (will always jump)
LC7C8:      jsr     LC8A1
            sty     NUMCHRS
            rts
.endproc

; Internal procedure: Advance to next tabulator stop
;
.proc HORIZTAB
            sty     TMPIDX          ; Save index to temporary var
            ldx     #$00            ; Find next tab stop position
LOOP:       lda     TABTBL,x        ; Get tab stop
            beq     UPDIDRET        ; If 0, no more
            tay                     ; Transfer to Y
            dey                     ; And decrement to compare with current position
            cpy     TMPIDX          ; If current pos >= tab stop, check next tabstop
            beq     NEXT            ;
            bcc     NEXT            ;
            cpy     YLNLIM          ; Is tab stop past the line limit?
            bcs     UPDIDRET        ; Go update index and return
            tya                     ; Get current index into Y
            ldy     TMPIDX          ;  then update TMPIDX with tab stop
            sta     TMPIDX          ;  position
ADVPOS:     cpy     NUMCHRS         ; Compare current index with number of chars
            bcs     PRTAB           ;   at or beyond number of chars, go print spaces
            cpy     TMPIDX          ; Compare current index with tab stop position
            bcs     RETURN          ;   at or beyond tab stop, just return
            jsr     CURSORR         ;   before, advance 1 pos right
            iny                     ; Increment current index
            bne     ADVPOS          ; And repeat (will always jump)
PRTAB:      lda     #' '            ; Print spaces until tab stop
            cpy     TMPIDX          ; Have we reached the tab stop position?
            bcs     UPDNCRET        ; Go update number of chars and return
            jsr     OUTCIFEON       ; Output to screen if echo enable
            sta     (QLN),y         ; Store into buffer
            iny                     ; Advance pos
            bne     PRTAB           ; And repeat (will always jump)
UPDNCRET:   sty     NUMCHRS         ; Uppate number of chars
RETURN:     rts

NEXT:       inx                     ; Next tab stop
            cpx     #TABTBL_SIZE    ; Repeat until end of table
            bcc     LOOP            ;
UPDIDRET:   ldy     TMPIDX          ; Set index to new value and return
            rts
.endproc

.proc LC817
            lda     CURPOS          ; Get current char position
            sta     TMPIDX          ; and store it
            jsr     LC8A1
            lda     #' '
LOOP:       cpy     NUMCHRS
            bcs     LC82C           ; If we've reached the end of line
            jsr     OUTCIFEON
            iny
            bne     LOOP
LC82C:      jsr     LC8A1
            ldy     #$00
            sty     CURPOS
            sty     NUMCHRS
            rts
.endproc

; Internal procedure - Output line buffer to screen and init current
;
.proc OUTLBUF
            ldy     #$00            ; Reset cursor position
            sty     CURPOS          ;
LOOP:       cpy     NUMCHRS         ; Have we reached the number of chars?
            beq     RETURN          ; Yes, return
            lda     (QLN),y         ; No, get char from line buffer
            jsr     OUTCIFEON       ; Echo (if set) and advance cursor right
            iny                     ; Next char
            bne     LOOP            ; Loop until current pos
RETURN:     rts
.endproc

LC849:      cpy     NUMCHRS
            bcs     LC86B
            sty     TMPIDX
LC850:      iny
            cpy     NUMCHRS
            bcs     LC861
            lda     (QLN),y
            dey
            sta     (QLN),y
            iny
            jsr     OUTCIFEON
            jmp     LC850

LC861:      lda     #$20
            jsr     OUTCIFEON
            dec     NUMCHRS
            jsr     LC8A1
LC86B:      rts

INSCHAR:    sta     L02D3           ; Save char
            sty     TMPIDX           ; And current index
            ldy     NUMCHRS         ; Get number of chars in buffer
            cpy     YLNLIM          ; Line full?
            bcc     LC87F           ; No, continue
            jsr     RNGBEL          ; Yes, ring the bell
            jmp     LC8AF           ; Restore index and return

LC87F:      cpy     TMPIDX
            beq     LC88D
            dey
            lda     (QLN),y
            iny
            sta     (QLN),y
            dey
            bne     LC87F
LC88D:      inc     NUMCHRS
            lda     L02D3
            sta     (QLN),y
LC894:      lda     (QLN),y
            jsr     OUTCIFEON
            iny
            cpy     NUMCHRS
            bcc     LC894
            inc     TMPIDX
            ; Fall through

.proc LC8A1
            tya                     ; Transfer current index to A
            sec                     ; Clear borrow for substraction
            sbc     TMPIDX          ; Substract initial index pos
            tax                     ; Transfer to X
            beq     LC8AF           ; If same pos, go to restore index and return
LC8A9:      jsr     BSCOORD         ; Updates text window coordinates for a BS
            dex
            bne     LC8A9
            ; Fall through
.endproc

.proc LC8AF
            ldy     TMPIDX
            rts
.endproc

; Internal procedure - Manage function key
;
FNKEY:      and     #$7F
            asl     a
            asl     a
            asl     a
            asl     a
            asl     a
            tax
            stx     L02D8
            clc
            adc     #$20
            sta     L02D9
LC8C4:      lda     KEYSTR,x
            cmp     #$20
            bcc     LC8D5
            cmp     #$80
            bcs     LC8D5
            inx
            cpx     L02D9
            bne     LC8C4
LC8D5:      ror     NOLEKO
            stx     L02D9
            ldx     L02D8
LC8DE:      cpx     L02D9
            bcs     LC8EC
            lda     KEYSTR,x
            jsr     NKEYMNG
            inx
            bne     LC8DE
LC8EC:      lda     KEYSTR,x
            and     #$7F
            cmp     #$0D
            asl     NOLEKO
            rts

LC8F7:      jsr     LC91B
            ldy     #$FF
LC8FC:      iny
            lda     (QLN),y
            tax
            sty     TMPIDX
            ldy     UNK23
            iny
            jsr     SWTBANK1        ; Turns on I-O address space and switches to bank 1
            txa
            sta     (VRAMDST),y
            jsr     RSTBANK0        ; Restores Bank 0 and turns on RAM at BE00-BFFF
            sty     UNK23
            ldy     TMPIDX
            cpy     NUMCHRS
            bne     LC8FC
            rts

LC91B:      lda     #$50
            sta     VRAMDST
            lda     #$FC
            sta     VRAMDST+1
            ldy     #$FF
            rts

LC926:      sty     TMPIDX
            jsr     LC91B
            ldy     L02DA
LC92F:      dey
            cpy     UNK23
            beq     LC92F
            jsr     SWTBANK1        ; Turns on I-O address space and switches to bank 1
            lda     (VRAMDST),y
            cmp     #$FF
            bne     LC947
            lda     UNK23
            sta     L02DA
            jmp     LC94E

LC947:      cmp     #$0D
            bne     LC92F
            sty     L02DA
LC94E:      sty     L02DB
LC951:      iny
            jsr     SWTBANK1        ; Turns on I-O address space and switches to bank 1
            lda     (VRAMDST),y
            tax
            jsr     RSTBANK0        ; Restores Bank 0 and turns on RAM at BE00-BFFF
            sty     L02DB
            ldy     TMPIDX
            txa
            sta     (QLN),y
            cmp     #$0D
            beq     LC972
            iny
            sty     TMPIDX
            ldy     L02DB
            jmp     LC951

LC972:      sty     NUMCHRS
            rts

; SUBROUTINE _GETKEY: WAIT FOR KEYBOARD KEY DEPRESSION, RETURN
; ASCII CODE FOR KEY.
;
; ON ENTRY: NO ARGUMENTS.
;
; ON RETURN: A = ASCII CODE FOR DEPRESSED KEY (OR SPECIAL KEY
; CODE FOR NON-ASCII KEYS); X, Y PRESERVED.
;
; NOTES:
; THIS SUBROUTINE SCANS AN UNENCODED KEYBOARD MATRIX CONNECTED
; TO THE MONOMEG KEYBOARD INTERFACE PORT.
; ENTRY POINT GETKEY SITS IN A LOOP WAITING FOR A KEY TO
; BE PRESSED.  WHEN A KEY IS PRESSED (EXCEPTING SHIFT, CONTROL,
; REPEAT), THE ROUTINE RETURNS WITH KEY CODE IN ACCUMULATOR.
;
; THIS ROUTINE IMPLEMENTS TRUE 2-KEY ROLLOVER, KEY DEBOUNCING,
; and REPEAT TIMING.
; CAPS LOCK IS SUPPORTED; IT ONLY AFFECTS LETTERS.
;
; DEBOUNCE DELAY and THE TIME BETWEEN REPEATS IS AN ALTERABLE
; PARAMETER.  THE DEBOUNCE DELAY IS DBCDLA+5 MILLISECONDS.  THE
; REPEAT INTERVAL IS RPTRAT+50 MILLISECONDS.  BOTH PARAMETERS
; ARE SIGNED INTEGERS.  THEIR DEFAULT VALUE IS ASSUMED TO BE 0.
;
; CONTINUOUSLY SCAN KEYBOARD UNTIL A KEY IS PRESSED OR THE
; CONDITIONS FOR REPEAT ARE MET.
;
; NOTE: THE ENTRY POINT FOR "_GETKEY" IS IN THE JUMP TABLE.
;
_GETKEY:    stx     XSVKB           ; Save registers
            sty     YSVKB
            jsr     _ONTCR          ; DISPLAY CURSOR
            jsr     _TIOON          ; TURN ON I-O ADDRESS SPACE
            jsr     KSETUP          ; SET UP CA1 and CA2
            lda     #$40            ; DISABLE T1 INTERRUPT
            sta     KBIER
            lda     KBACR           ; SET T1 FOR SINGLE-SHOT MODE and LEAVE PB7
            and     #$3F            ; ALONE
            sta     KBACR           ; (ALL THIS SHOULDN'T DISTURB T1 ITSELF)

            ldy     #0              ; SET COUNT FOR AUTO REPEAT TIMEOUT DELAY
GETK1:      ldx     DBCDLA          ; LOAD DEBOUNCE DELAY INTO X
GETK2:      jsr     WA1TST          ; WAIT 1 MILLISECOND & TEST LSTKEY
            bne     GETK4           ; JUMP IF LSTKEY IS UP
            lda     #0              ; TEST staTE OF REPEAT KEY
            jsr     KEYTST
            beq     GETK3           ; SKIP AHEAD IF REPEAT KEY IS DOWN
            ldx     #7              ; IF UP, SEE IF LSTKEY IS IN LIST OF AUTO
            lda     LSTKEY          ; REPEAT KEYS
GETK25:     cmp     AUTORL,X
            beq     GETK26
            dex
            bpl     GETK25
            bmi     GETK1           ; LOOP BACK IF LSTKEY NOT IN LIST
GETK26:     bit     RPTFLG          ; IF IN LIST, TEST IF DELAY NEEDED
            bmi     GETK3           ; SKIP AHEAD IF DELAY NOT NEEDED
            dey                     ; DECREMENT DELAY BEFORE REPEAT
            bne     GETK1           ; LOOP BACK IF DELAY NOT EXPIRED
            sec                     ; SET REPEAT FLAG TO AVOID DELAY NEXT TIME
            ror     RPTFLG
GETK3:      lda     KBIFR           ; TEST IF REPEAT TIMER (T1) EXPIRED
            and     #$40
            beq     GETK1           ; LOOP BACK IF REPEAT DELAY UNEXPIRED
            bne     GETK10          ; OTHERWISE GO OUTPUT THE REPEATED CODE
GETK4:      dex                     ; DECREMENT DEBOUNCE DELAY
            bne     GETK2           ; GO TEST KEY AGAIN IF NOT EXPIRED
            stx     RPTFLG          ; OTHERWISE CLEAR REPEAT FLAG
                                    ; and staRT SCANNING FOR NEW KEY
            ; TURN ON BLINKING CURSOR AS PROMPT FOR KEYBOARD INPUT
            ;
GETK4A:     jsr     _ONTCR          ; DISPLAY CURSOR
GETK4B:     lda     #0
            sta     TCURS           ; CLEAR LOW BYTE OF COUNTER
            lda     CURDLA          ; RECALL CURSOR DELAY DESIRED
            lsr     a               ; DIVIDE BY 2
            ror     TCURS
            sta     TCURS+1         ; INITIALIZE COUNT

            ; PREVIOUS KEY IS NOW RELEASED, RESUME SCAN OF KEYBOARD
            ;
GETK5:      jsr     KYSCAN          ; SCAN THE KEYBOARD ONCE
            bne     GETK7           ; JUMP OUT IF A KEY SEEN DOWN
            lda     TCURS           ; OTHERWISE HandLE CURSOR FLASH
            bne     GETK6A          ; BRANCH IF NO BORROW ON CLOCK REMAINING
            dec     TCURS+1         ; DECREMENT HI BYTE OF CLOCK REMAINING
            bpl     GETK6A          ; BRANCH IF TIME REMAINS ON CLOCK
            bit     CURVIS          ; TEST "CURSOR VISIBLE" FLAG
            bpl     GETK4A          ; IF NOT VISIBLE, GO MAKE IT VISIBLE
            lda     CURDLA
            beq     GETK5           ; IF NO BLINKING WANTED, BRANCH
            jsr     _OFFTCR         ; ELSE FLIP IT TO INVISIBLE
            jmp     GETK4B          ; and REstaRT BLINK TIMER
GETK6A:     dec     TCURS           ; DECREMENT TIME LEFT TILL CURSOR "BLINKS"
            jmp     GETK5           ; GO SCAN THE ARRAY AGAIN

GETK7:      jsr     _OFFTCR         ; ENSURE THAT CURSOR IS OFF
            jsr     CLCMXA          ; CALCULATE MATRIX ADDRESS OF KEY
                                    ; CLCMXA SETS DEBOUNCE DELAY IN X
GETK9:      jsr     WA1TST          ; WAIT FOR 1 MILLISECOND THEN TEST AGAIN
            bne     GETK5           ; GO REstaRT SCAN IF NOT STILL DOWN
            dex                     ; CHECK DEBOUNCE DELAY
            bne     GETK9           ; CONTINUE VERIFICATION
GETK10:     lda     RPTRAT          ; staRT THE REPEAT TIMER
            sta     KBT1CH
            jsr     _OFFTCR         ; ENSURE CURSOR IS OFF
            jsr     ASCII           ; CONVERT MATRIX ADDRESS IN LSTKEY TO
                                    ; ASCII CODE IN A
            jsr     CLICK           ; SOUND AUDIO "CLICK" TO ACKNOWLEDGE KEY
            bit     KBECHO          ; TEST "KEYBOARD ECHO" FLAG
            bpl     GETK12          ; BRANCH IF NO ECHO WANTED
            jsr     _OUTCH          ; ELSE ECHO KEY
GETK12:     ldx     XSVKB           ; RESTORE REGS
            ldy     YSVKB
            jmp     _IORES          ; RESTORE I-O OR RAM ADDRESS SPACE,
                                    ; RETURN TO CALLER WITH ASCII CODE IN A

; SUBROUTINE IFKEY: TEST KEY WITHOUT ROLLOVER
;
; THIS ROUTINE IS SIMILAR TO TSTKEY BELOW IN ALL RESPECTS EXCEPT
; THAT IF THE PREVIOUS KEY IS STILL DOWN, IT IS RETURNED WITH
; THE CARRY FLAG SET.
;
_IFKEY:     lda     #$00
            sta     LSTKEY          ; Clear last key
            ; Fall through

; SUBROUTINE TSTKEY: TEST FOR KEY DEPRESSION (WITH ROLLOVER).
;
; ON ENTRY: NO ARGUMENTS.
;
; ON RETURN: CY FLAG IS "KEY IS PRESSED" FLAG (CY CLEAR IF NO
; KEY IS DOWN OR IF THE SAME KEY IS STILL DOWN); IF CY SET, THEN
; CHARACTER IN A; X, Y PRESERVED.
;
; NOTE: SCANS THE KEYBOARD ONCE.
;
; NOTE: ENTRY POINT "TSTKEY" IS IN THE JUMP TABLE.
;
_TSTKEY:    stx     XSVKB
            sty     YSVKB
            jsr     _TIOON
            jsr     KSETUP
            lda     LSTKEY
            beq     LCA5D
            jsr     KEYTST
            bne     LCA4F
LCA4C:      clc
            bcc     LCA7C
LCA4F:      ldx     DBCDLA
LCA52:      jsr     WA1TST
            beq     LCA4C
            dex
            bne     LCA52
            stx     LSTKEY
LCA5D:      jsr     KYSCAN
            beq     LCA7B
            jsr     CLCMXA
LCA65:      jsr     WA1TST
            bne     LCA76
            dex
            bne     LCA65
            jsr     ASCII
            jsr     CLICK
            sec
            bcs     LCA7C
LCA76:      lda     #$00
            sta     LSTKEY
LCA7B:      clc
LCA7C:      ldx     XSVKB
            ldy     YSVKB
            jmp     _IORES

; LOCAL SUBROUTINE WA1TST: WAIT FOR 1 MS THEN TEST LSTKEY
;
; ON ENTRY: NO ARGUMENTS.
;
; ON RETURN: X, Y PRESERVED, Z=KEY DOWN, NZ=KEY UP
;
WA1TST:     jsr     WA1MS           ; First wait for 1 millisecond
            lda     LSTKEY          ; Now test state of LSTKEY
            ; Fall through

; SUBROUTINE KEYTST: TEST KEY WHOSE MATRIX ADDRESS IS IN A.
;
; ON ENTRY: A = DESIRED MATRIX ADDRESS OF KEYBOARD.
;
; ON RETURN: Z FLAG IS "KEY PRESSED" FLAG (USE BEQ TO BRANCH IF
; KEY IS PRESSED); X AND Y REGISTERS PRESERVED.
;
KEYTST:     jsr     _TIOON
            sta     $BFC5
            stx     TEMP1
LCA93:      bit     COLMKS
            beq     LCAA0
            sta     $BFE1
            sec
            sbc     #$01
            bne     LCA93
LCAA0:      lsr     a
            lsr     a
            lsr     a
            lsr     a
            tax
            lda     ROWMSK,x
            ldx     TEMP1
            and     $BFE1
            jmp     _IORES

; MASK TABLE TO ISOLATE ROW BITS
;
ROWMSK:     .byte   %00000100       ; keyboard ROW
            .byte   %00001000
            .byte   %00010000
            .byte   %00100000
            .byte   %01000000
            .byte   %10000000

;        SUBROUTINE WA1MS: WAIT ONE MILLISECOND.
;
;        ON ENTRY: NO ARGUMENTS.
;
;        ON RETURN: X, Y PRESERVED. #$C5
;
WA1MS:      lda    #(1000-15)/5     ; Set count
WA1MS1:     sbc    #$01             ; Always sets carry for next iteration
            bne    WA1MS1
            rts

;        LOCAL SUBROUTINE CLCMXA: CALCULATE MATRIX ADDRESS FROM SCAN
;                                 RESULTS
;        ARGUMENTS: X IS COMPLEMENT OF COLUMN NUMBER
;                   A IS COMPLEMENT OF ROW DATA AT THAT COLUMN
;
;        ON RETURN: RESULT IN LSTKEY, Y PRESERVED, TEMP1 DESTROYED
;                   DEBOUNCE DELAY LOADED INTO X
;
CLCMXA:     ldx     #$05            ; CALCULATE MATRIX ADDRESS OF KEY
CLCMX1:     asl     a               ; FIRST CALCULATE ROW ADDRESS IN X
            bcs     CLCMX2
            dex
            bpl     CLCMX1
CLCMX2:     txa                     ; MULTIPLY ROW ADDRESS BY 16
            asl     a
            asl     a
            asl     a
            asl     a
            sty     TEMP1
            ora     TEMP1           ; ADD IN COMPLEMENT COLUMN ADDRESS
            eor     #$0F            ; UNCOMPLEMENT COLUMN ADDRESS
            sta     LSTKEY          ; PUT RESULT IN LSTKEY
            ldx     DBCDLA          ; LOAD DEBOUNCE DELAY INTO X
            rts

; LOCAL SUBROUTINE KSETUP: SETUP CA1 and CA2 FOR KEYBOARD SCAN
;
; ON ENTRY: NO ARGUMENTS.
;
; ON RETURN: X, Y PRESERVED
;
KSETUP:     lda     KBPCR           ; Set up CA1 and CA2
            ora     #$0B            ; Interrupt on CA1 + EDGE, - Pulse on CA2
            sta     KBPCR           ; Whenever read PA1
            rts

; LOCAL SUBROUTINE KYSCAN: SCAN THE KEYBOARD ONCE AND RETURN
;
; ON ENTRY: NO ARGUMENTS.
;
; ON RETURN: Z=NO KEY FOUND DOWN, NZ=KEY FOUND DOWN
;            IF NZ, Y WILL BE COMPLEMENT OF ROW NUMBER AND A WILL
;            BE COMPLEMENT OF ROW DATA AT THAT COLUMN.
;            X PRESERVED
;
KYSCAN:     sta     KSCCLR          ; CLEAR KEYBOARD SCAN COUNTER (DATA NOT IMPORTANT)
            lda     KBDATA          ; SKIP COLUMN 0 = ALL SHIFT & MODE KEYS
            ldy     #$0E            ; Y IS COMPLEMENT OF ROW COUNTER
KYSCN1:     lda     KBDATA          ; LOOK AT CURRENT COLUMN THEN INCREMENT SCAN COUTER
            and     #$FC            ; ISOLATE KEYBOARD ROW DATA
            eor     #$FC            ; FLIP KEY SENSE & TEST IF ANY PRESSED
            bne     KYSCN2          ; JUMP OUT IF A KEY SEEN DOWN
            dey
            bpl     KYSCN1          ; LOOP UNTIL WHOLE KEYBOARD SCANNED
            iny
KYSCN2:     rts

; LOCAL SUBROUTINE ASCII: CONVERT MATRIX ADDRESS IN LSTKEY TO
;
; ASCII CODE IN A.
;
ASCII:      ldx     LSTKEY          ; LOOKUP UNSHIFTED ASCII EQUIVALENT
            lda     USCODE,X
            sta     TEMP1           ; SAVE RESULT
            sta     KSCCLR          ; ADDRESS KEYBOARD COLUMN 0
            lda     KBDATA          ; JUMP IF EITHER SHIFT KEY IS DOWN
            bit     LSHIFT
            beq     ASCII2
            bit     RSHIFT          ; JUMP IF EITHER SHIFT KEY IS DOWN
            beq     ASCII2
            and     #$10            ; TEST CAPS LOCK KEY
            beq     ASCII0          ; SKIP IF DOWN
            lda     TEMP1           ; GET UNSHIFTED ASCII
            bne     ASCII7          ; GO TO FINAL OUTPUT
ASCII0:     lda     TEMP1           ; TEST IF ASCII IS A LETTER
            cmp     #$61
            bcc     ASCII7          ; JUMP IF NOT
            cmp     #$7B
            bcs     ASCII7          ; JUMP IF NOT
ASCII1:     sec                     ; CONVERT LOWER CASE TO UPPER CASE
            sbc     #$20
            bne     ASCII7          ; GO TO FINAL OUTPUT  (UNCOND JUMP)
ASCII2:     cpx     #$3D            ; TEST IF KEY IS IN AUXILIARY KEYPADS
            lda     TEMP1           ;    GET ASCII CODE
            bcc     ASCII3          ; JUMP IF NOT
            ora     #$10            ; OR IN bit 4 IF SO
            bcs     ASCII7          ; GO TO FINAL OUTPUT (UNCOND JUMP)
                                    ; SCRUNCH THE UNSHIFTED ASCII CODE FOR
                                    ; COMPACT TABLE LOOKUP OF SHIFTED EQUIV.
ASCII3:     cmp     #$61            ; TEST IF ASCII IS BELOW $61
            bcc     ASCII4          ; JUMP AHEAD IF SO
            cmp     #$7B            ; TEST IF ASCII IS ABOVE $7A
            bcc     ASCII1          ; JUMP IF NOT, IS A LETTER
            sbc     #$1A            ; SCRUNCH CODES BETWEEN 61 and 7A IF SO
ASCII4:     cmp     #$5B            ; TEST IF ASCII IS ABOVE $5A
            bcc     ASCII5          ; JUMP AHEAD IF NOT
            sbc     #$1D            ; SCRUNCH CODES BETWEEN 3E and 5A IF SO
ASCII5:     sec                     ; SUBTRACT $27
            sbc     #$27            ; SO THAT CHARACTERS NOT AFFECTED BY SHIFT
            bpl     ASCII6          ; ARE NEGATIVE, ALL OTHERS POSITIVE
            lda     TEMP1           ; IS A CONTROL CHARACTER, RETRIEVE IT
            rts                     ; and RETURN WITH NO FURTHER PROCESSING
ASCII6:     tax                     ; LOOKUP SHIFTED SPECIAL CHARACTER
            lda     SHCODE,X
ASCII7:     pha                     ; SAVE CODE FOR CNTL KEY TESTING
            sta     KSCCLR          ; ADDRESS KEYBOARD COLUMN 0
            lda     KBDATA          ; LOOK AT CNTL KEY
            and     #$08
            bne     ASCII8          ; SKIP IF NOT DOWN
            pla                     ; RESTORE ASCII CODE
            and     #$1F            ; ZERO UPPER 3 BITS LEAVING CONTROL CODES
            rts                     ; and RETURN
ASCII8:     pla                     ; RESTORE ASCII CODE
            rts                     ; and RETURN

; UNSHIFTED KEY CODE TRANSLATE TABLE
;
USCODE:     .byte   $00             ; 00 (NO CODE)
            .byte   $1B             ; 01 ESC
            .byte   $31             ; 02 1!
            .byte   $32             ; 03 2@
            .byte   $33             ; 04 3#
            .byte   $34             ; 05 4$
            .byte   $35             ; 06 5%
            .byte   $36             ; 07 6 CARRET
            .byte   $37             ; 08 7&
            .byte   $38             ; 09 8*
            .byte   $39             ; 0A 9(
            .byte   $30             ; 0B 0)
            .byte   $2D             ; 0C -_
            .byte   $3D             ; 0D =+
            .byte   $60             ; 0E GRAVE ACCENT TILDE
            .byte   $08             ; 0F BACKSPACE
COLMKS:     .byte   $0F             ; 10 (NO CODE) MASK TO ISOLATE COL ADDR
            .byte   $09             ; 11 TAB
            .byte   $71             ; 12 Q
            .byte   $77             ; 13 W
            .byte   $65             ; 14 E
            .byte   $72             ; 15 R
            .byte   $74             ; 16 T
            .byte   $79             ; 17 Y
            .byte   $75             ; 18 U
            .byte   $69             ; 19 I
            .byte   $6F             ; 1A O
            .byte   $70             ; 1B P
            .byte   $5B             ; 1C LEFT BRACKET RIGHT BRACKET
            .byte   $5C             ; 1D BACKSLASH VERTICAL BAR
            .byte   $0A             ; 1E LINE FEED
            .byte   $7F             ; 1F RUBOUT
LSHIFT:     .byte   $20             ; 20 (NO CODE) bit MASK FOR LEFT SHIFT
RSHIFT:     .byte   $40             ; 21 (NO KEY) bit MASK FOR RIGHT SHIFT
            .byte   $61             ; 22 A
            .byte   $73             ; 23 S
            .byte   $64             ; 24 D
            .byte   $66             ; 25 F
            .byte   $67             ; 26 G
            .byte   $68             ; 27 H
            .byte   $6A             ; 28 J
            .byte   $6B             ; 29 K
            .byte   $6C             ; 2A L
            .byte   $3B             ; 2B ;:
            .byte   $27             ; 2C '"
            .byte   $7B             ; 2D LEFT BRACE RIGHT BRACE
            .byte   $0D             ; 2E RETURN
            .byte   $00             ; 2F (NO KEY)
            .byte   $00             ; 30 (NO CODE)
            .byte   $00             ; 31 (NO KEY)
            .byte   $20             ; 32 SPACE
            .byte   $7A             ; 33 Z
            .byte   $78             ; 34 X
            .byte   $63             ; 35 C
            .byte   $76             ; 36 V
            .byte   $62             ; 37 B
            .byte   $6E             ; 38 N
            .byte   $6D             ; 39 M
            .byte   $2C             ; 3A , LESS THAN
            .byte   $2E             ; 3B . GREATER THAN
            .byte   $2F             ; 3C /?
            .byte   $A5             ; 3D DELETE
            .byte   $A6             ; 3E INSERT
            .byte   $2E             ; 3F .
            .byte   $00             ; 40 (NO CODE)
            .byte   $80             ; 41 F1
            .byte   $81             ; 42 F2
            .byte   $82             ; 43 F3
            .byte   $83             ; 44 F4
            .byte   $84             ; 45 F5
            .byte   $85             ; 46 F6
            .byte   $86             ; 47 F7
            .byte   $87             ; 48 F8
            .byte   $A3             ; 49 CURSOR DOWN
            .byte   $A2             ; 4A CURSOR RIGHT
            .byte   $8E             ; 4B ENTER
            .byte   $A4             ; 4C HOME
            .byte   $A1             ; 4D CURSOR LEFT
            .byte   $A0             ; 3E CURSOR UP
            .byte   $30             ; 4F 0
            .byte   $00             ; 50 (NO KEY)
            .byte   $8C             ; 51 SUBTRACT
            .byte   $36             ; 52 6
            .byte   $8D             ; 53 ADD
            .byte   $8B             ; 54 DIVIDE
            .byte   $35             ; 55 5
            .byte   $8A             ; 56 MULTIPLY
            .byte   $39             ; 57 9
            .byte   $89             ; 58 PF2
            .byte   $31             ; 59 1
            .byte   $34             ; 5A 4
            .byte   $33             ; 5B 3
            .byte   $37             ; 5C 7
            .byte   $88             ; 5D PF1
            .byte   $38             ; 5E 8
            .byte   $32             ; 5F 2

; SHIFTED KEY CODE TRANSLATE TABLE
; TRANSLATES SCRUNCHED UNSHIFTED ASCII TO SHIFTED ASCII
; CAN'T OCCUR MEANS THAT THE CODE DOES NOR APPERAR IN THE
; UNSHIFTED ASCII TABLE
;
;           00-26 EITHER CAN'T OCCUR OR
;           SHIFTED = UNSHIFTED
;
SHCODE:     .byte   $22             ; 27 "
            .byte   $00             ; 28 (CAN'T OCCUR)
            .byte   $00             ; 29 (CAN'T OCCUR)
            .byte   $00             ; 2A (CAN'T OCCUR)
            .byte   $00             ; 2B (CAN'T OCCUR)
            .byte   $3C             ; 2C LESS THAN
            .byte   $5F             ; 2D UNDERLINE
            .byte   $3E             ; 2E GREATER THAN
            .byte   $3F             ; 2F ?
            .byte   $29             ; 30 )
            .byte   $21             ; 31 !
            .byte   $40             ; 32 @
            .byte   $23             ; 33 #
            .byte   $24             ; 34 $
            .byte   $25             ; 35 %
            .byte   $5E             ; 36 CARRET ^
            .byte   $26             ; 37 &
            .byte   $2A             ; 38 *
            .byte   $28             ; 39 (
            .byte   $00             ; 3A (CAN'T OCCUR)
            .byte   $3A             ; 3B :
            .byte   $00             ; 3C (CAN'T OCCUR)
            .byte   $2B             ; 3D +

            ; 3E-5A (CAN'T OCCUR)
            ;
            .byte   $5D             ; 5B RIGHT BRACKET ]
            .byte   $7C             ; 5C VERTICAL BAR |
            .byte   $00             ; 5D (CAN'T OCCUR)
            .byte   $00             ; 5E (CAN'T OCCUR)
            .byte   $00             ; 5F (CAN'T OCCUR)
            .byte   $7E             ; 60 TILDE ~

            ;61-7A (CAN'T OCCUR)
            ;
            .byte   $7D             ; 7B RIGHT BRACE }
            .byte   $00             ; 7C (CAN'T OCCUR)
            .byte   $00             ; 7D (CAN'T OCCUR)
            .byte   $00             ; 7E (CAN'T OCCUR)
            .byte   $7F             ; 7F RUBOUT

; TABLE OF MATRIX ADDRESSES OF AUTO REPEAT KEYS
;
AUTORL:     .byte   $0F             ; BACKSPACE
            .byte   $1F             ; ROBOUT
            .byte   $32             ; SPACE
            .byte   $3D             ; DELETE CHARACTER
            .byte   $49             ; CURSOR DOWN
            .byte   $4A             ; CURSOR RIGHT
            .byte   $4D             ; CURSOR LEFT
            .byte   $4E             ; CURSOR UP

; DISPLAY CHARACTER THEN MOVE CURSOR
;
_OUTCH:     cld
            sta     L02C7
            stx     L02C8
            sty     L02C9
            cmp     #$00
            bpl     LCBFC
            bit     EXTHI
            bpl     LCC52
            jmp     (QEXHI7)

LCBFC:      sec
            sbc     #$20
            bcs     LCC04
            jmp     LCC80

LCC04:      pha
            asl     CRSRWRAP
            jsr     LD13B
            pla
            cmp     #$5F
            bne     LCC23
            bit     SHODEL
            bmi     LCC32
            jsr     BSCOORD         ; Updates text window coordinates for a BS
            jsr     LD13B
            lda     #$00
            jsr     PRNCHR
            jmp     LCC48

LCC23:      cmp     #$3F
            bne     LCC32
            bit     SHOUL
            bmi     LCC32
            jsr     LD0D7
            jmp     LCC45

LCC32:      jsr     PRNCHR
            bit     UNDRLN
            bpl     LCC3D
            jsr     LD0D7
LCC3D:      bit     RVIDEO
            bpl     LCC45
            jsr     LD0A7
LCC45:      jsr     CURSORR
LCC48:      ldy     L02C9
            ldx     L02C8
            lda     L02C7
            rts

LCC52:      ldx     #$08
LCC54:      cmp     LCCFF,x
            beq     LCC7A
            dex
            bpl     LCC54
            cmp     #$A0
            bne     LCC66
            jsr     LCF08
            jmp     LCC48

LCC66:      cmp     #$A2
            bne     LCC70
            jsr     CURSORR
            jmp     LCC48

LCC70:      cmp     #$A4
            bne     LCC48
            jsr     _HOMETW
            jmp     LCC48

LCC7A:      lda     LCD08,x
            jmp     LCBFC

LCC80:      clc
            adc     #$20
            bit     EXCCP
            bpl     LCC8B
            jmp     (QEXCC)

LCC8B:      cmp     #$0D
            bne     LCC9A
            asl     CRSRWRAP
            bcs     LCC48
            jsr     _CRLF
            jmp     LCC48

LCC9A:      cmp     #$0A
            bne     LCCA4
            jsr     _LINEFD
            jmp     LCC48

LCCA4:      cmp     UNK19
            bne     LCCAF
            jsr     BSCOORD         ; Updates text window coordinates for a BS
            jmp     LCC48

LCCAF:      cmp     UNK22
            bne     LCCC2
            lda     LINE
            jsr     _CLRTLN
            lda     #$01
            sta     COL
            jmp     LCC48

LCCC2:      cmp     UNK21
            bne     LCCCD
            jsr     _CLRHTW
            jmp     LCC48

LCCCD:      cmp     UNK18
            bne     LCCD8
            jsr     RNGBEL
            jmp     LCC48

LCCD8:      cmp     UNK20
            beq     LCCE0
LCCDD:      jmp     LCC48

LCCE0:      ldx     #$00
LCCE2:      lda     TABTBL,x
            beq     LCCFC
            cmp     COL
            beq     LCCF7
            bcc     LCCF7
            cmp     #$51
            bcs     LCCFC
            sta     COL
            bcc     LCCDD
LCCF7:      inx
            cpx     #$20
            bne     LCCE2
LCCFC:      jmp     LCC48

LCCFF:      .byte   $00
            .byte   $8A
            .byte   $8B
            .byte   $8C
            .byte   $8D
            .byte   $8E
            .byte   $A1
            .byte   $A3
            .byte   $B4

LCD08:      .byte   $00
            .byte   $2A
            .byte   $2F
            .byte   $2D
            .byte   $2B
            .byte   $0D
            .byte   $08
            .byte   $0A
            .byte   $0C

; INITIO - Clear screen and set default values of display parameters
;
_INITIO:    jsr     _INITTW         ; Init text window
            lda     #<__INPLBUF     ; Inits input line buffer
            sta     QLN             ;
            lda     #>__INPLBUF     ;
            sta     QLN+1           ;
            ldx     #$40            ; Clears last 64 bytes of input buffer to make
            lda     #' '            ; room for the 8 key legends
INITIO1:    sta     __INPLBUF+__INPBSIZ-1,x
            dex                     ;
            bne     INITIO1         ; Loop until done
            lda     #$80            ; Mark KEYSTR table as empty
INITIO2:    sta     KEYSTR,x        ;
            dex                     ;
            bne     INITIO2         ; Repeat until done
            lda     #$00            ; Clear last key pressed
            sta     LSTKEY          ;
            jmp     _DRWLEG         ; Go to draw legends and return

; INITTW - INITIALIZE THE TEXT WINDOW TO 24 LINES and CLEAR THE TEXT WINDOW ONLY
;
_INITTW:    cld
            jsr     _CLRDSP         ; Clear the entire VIDHRES by VIDVRES screen
            lda     #$00            ; Enable keyboard echo
            sta     NOLEKO          ;
            ldx     #<(CRSRWRAP-KBECHO+1)
INITTW1:    sta     KBECHO,x        ; Clear flags (from KBECHO to CRSRWRAP)
            dex                     ;
            bpl     INITTW1         ;
            lda     #$01            ; Make a short beep
            tax                     ;
            tay                     ;
            jsr     _BEEP           ;
            jsr     LC91B
            ldy     #$00
            jsr     SWTBANK1        ; Turns on I-O address space and switches to bank 1
            lda     #$FF
            sta     (VRAMDST),y
            iny
            lda     #$0D
            sta     (VRAMDST),y
            sty     UNK23
            jsr     RSTBANK0        ; Restores Bank 0 and turns on RAM at BE00-BFFF
            lda     #$18
            ldy     #$00
            ; Fall through

; DEFTW - SET THE POSITION AND SIZE OF THE TEXT WINDOW
;
; Arguments: A=Number of text lines
;            Y=Y coordinate of the top line
;
_DEFTW:     sta     NLINET          ; Set number of text lines
            sty     YTDOWN          ; Set position of text window
            ; Fall through

; CLRHTW - CLEAR THE TEXT WINDOW and HOME THE CURSOR.
;
_CLRHTW:    jsr     _CLRTW          ; Clear the text window without moving the cursor
            ; Fall through

; HOMETW - PLACE THE CURSOR IN THE HOME POSITION (COL=1, LINE=1)
;
_HOMETW:    pha                     ; Preserve A
            lda     #$01            ; Set LINE and COL
            sta     LINE            ;
            sta     COL             ;
            pla                     ; Restore A
            rts

_CRLF:      pha
            lda     #$01
            sta     COL
            bit     NOLFCR
            bmi     LCD93
            txa
            pha
            tya
            pha
            jsr     _LINEFD
            pla
            tay
            pla
            tax
LCD93:      pla
            rts

_CLRDSP:    pha
            txa
            pha
            lda     #$00
            sta     VRAMORG
            lda     #$C0
            sta     VRAMORG+1
            lda     #$00
            sta     VRAMCNT
            lda     #$3C
            sta     VRAMCNT+1
            jsr     CLRVRAM
            pla
            tax
            pla
            rts


; CLRTW - CLEAR THE TEXT WINDOW WITHOUT MOVING THE CURSOR
;
.proc _CLRTW
            pha                     ; Preserve A and X
            txa                     ;
            pha                     ;
            lda     YTDOWN          ; Get coordinate of top of text window
            jsr     VLINSTART       ; Calculate where it starts at video memory
            jsr     VDST2ORG        ; Move it into VRAMORG
            lda     NLINET          ; Get number of text lines in the text window
            ldx     #$50
            jsr     LCF36
            lda     VRAMDST
            sec
            sbc     VRAMORG
            sta     VRAMCNT
            lda     VRAMDST+1
            sbc     VRAMORG+1
            sta     VRAMCNT+1
            inc     VRAMCNT
            bne     LCDD6
            inc     VRAMCNT+1
LCDD6:      jsr     CLRVRAM
            pla
            tax
            pla
            rts
.endproc

; CLRLEG - CLEAR THE LEGEND DISPLAY AREA (BOTTOMMOST 16 SCAN LINES)
;
; Monomeg display is VIDHRESxVIDVRES bits
;
; Last 16 lines start at VIDHRES*(VIDVRES-16)/8 = VIDEORAM+$3840
; and occupy 16*VIDHRES/8 = $3C0 bytes
;
_CLRLEG:    lda     #<(VIDEORAM+VIDHRES*(VIDVRES-16)/8)
            sta     VRAMORG
            lda     #>(VIDEORAM+VIDHRES*(VIDVRES-16)/8)
            sta     VRAMORG+1
            lda     #<(16*VIDHRES/8)
            sta     VRAMCNT
            lda     #>(16*VIDHRES/8)
            sta     VRAMCNT+1
            jmp     CLRVRAM         ; Jump to clear the video ram and return

LCDF0:      lda     NLINET
            ; Fall through

_CLRTLN:    ldx     #$50
            cmp     NLINET
            bcc     LCDFD
            lda     NLINET
LCDFD:      jsr     LCF36
            lda     VRAMDST
            sec
            sbc     #$57
            sta     VRAMORG
            lda     VRAMDST+1
            sbc     #$02
            sta     VRAMORG+1
            lda     #$58
            sta     VRAMCNT
            lda     #$02
            sta     VRAMCNT+1
            ; Fall through

; Internal procedure: Clears VRAMCNT bytes of video RAM starting at VRAMORG
;
CLRVRAM:    jsr     SWTBANK1        ; Turns on I-O address space and switches to bank 1
            jsr     _CLRVRAM        ; Do the actual clearing
            jmp     RSTBANK0        ; Restores Bank 0 and turns on RAM at BE00-BFFF

_CLRVRAM:   tya                     ; Saves Y register
            pha                     ;
            lda     #$00            ; Set for clear
            ldy     VRAMCNT+1       ; Get count MSB
            beq     CLRVREM         ; Count is less than a page, go copy remainder
            tay                     ; Init index
CLRVRPAG:   sta     (VRAMORG),y     ; Clears two bytes per iteration for speed
            iny                     ;
            sta     (VRAMORG),y     ;
            iny                     ;
            bne     CLRVRPAG        ; While not end of page, continue
            inc     VRAMORG+1       ; Increment page origin
            dec     VRAMCNT+1       ; Decrement page count
            bne     CLRVRPAG        ; Repeat until last page
CLRVREM:    ldy     VRAMCNT         ; Get remainder bytes
            beq     CLRVDONE        ; None, we're finished
CLRVREML:   dey
            sta     (VRAMORG),y
            bne     CLRVREML        ; Loop until done
CLRVDONE:   pla                     ; Restore Y and return
            tay                     ;
            rts


_LINEFD:    lda     LINE
            cmp     NLINET
            bcc     LCE56
            bit     NOSCRL
            bpl     LCE5A
            jsr     _CLRTW
            lda     #$00
            sta     LINE
LCE56:      inc     LINE
            rts

LCE5A:      lda     NLINET
            cmp     #$02
            bcc     LCE9A
            ldx     #$50
            jsr     LCF36
            lda     VRAMDST
            sta     VRAMCNT
            lda     VRAMDST+1
            sta     VRAMCNT+1
            lda     YTDOWN
            jsr     VLINSTART
            jsr     VDST2ORG
            lda     VRAMORG
            clc
            adc     #$58
            sta     VRAMDST
            lda     VRAMORG+1
            adc     #$02
            sta     VRAMDST+1
            lda     VRAMCNT
            sec
            sbc     VRAMDST
            sta     VRAMCNT
            lda     VRAMCNT+1
            sbc     VRAMDST+1
            sta     VRAMCNT+1
            inc     VRAMCNT
            bne     LCE97
            inc     VRAMCNT+1
LCE97:      jsr     LCE9D
LCE9A:      jmp     LCDF0

LCE9D:      jsr     SWTBANK1        ; Turns on I-O address space and switches to bank 1
            jsr     LCEA6
            jmp     RSTBANK0        ; Restores Bank 0 and turns on RAM at BE00-BFFF

LCEA6:      tya
            pha
            ldy     #$00
            ldx     VRAMCNT+1
            beq     LCECB
LCEAE:      lda     (VRAMDST),y
            sta     (VRAMORG),y
            iny
            lda     (VRAMDST),y
            sta     (VRAMORG),y
            iny
            lda     (VRAMDST),y
            sta     (VRAMORG),y
            iny
            lda     (VRAMDST),y
            sta     (VRAMORG),y
            iny
            bne     LCEAE
            inc     VRAMDST+1
            inc     VRAMORG+1
            dex
            bne     LCEAE
LCECB:      ldx     VRAMCNT
            beq     LCED7
LCECF:      lda     (VRAMDST),y
            sta     (VRAMORG),y
            iny
            dex
            bne     LCECF
LCED7:      pla
            tay
            rts

; Internal procedure: Advance cursor position to the right and manage wrap
;
CURSORR:    pha                     ; Save accumulator
            lda     COL             ; Get column
            cmp     #$50            ; Have we reached pos 80?
            bcs     WRAP            ; Yes,
            inc     COL             ; No, increment position
            pla                     ; Recover accumulator and return
            rts

WRAP:       jsr     _CRLF           ; Cursor wrap (first col and down one line)
            sec                     ; Set the curor wrap flag
            ror     CRSRWRAP        ;
            pla                     ; Recover accumulator and return
            rts

; Internal procedure: Updates text window coordinates for a BS
;
.proc BSCOORD
            dec     COL             ; Decrement column number
            bne     RETURN          ; If 1 or greater, we're done
            pha                     ; Nope, preserve A
            lda     #TXTHRES        ; New col is the last one
            sta     COL             ;
            dec     LINE            ; Up one line
            bne     DONE            ; If 1 or more, we're done
            lda     NLINET          ; If 0, wrap to the bottom line
            sta     LINE            ;
DONE:       pla                     ; Restore A and return
RETURN:     rts
.endproc

LCF08:      dec     LINE
            beq     LCF0E
            rts

LCF0E:      pha
            lda     NLINET
            sta     LINE
            pla
            rts

LCF17:      ldx     COL
            beq     LCF20
            cpx     #$51
            bcc     LCF25
LCF20:      ldx     #$01
            stx     COL
LCF25:      lda     LINE
            beq     LCF31
            cmp     NLINET
            bcc     LCF36
            beq     LCF36
LCF31:      lda     #$01
            sta     LINE
            ; Fall through

; X=Characters per line?
; A=Number of text lines
;
.proc LCF36
            sta     TEMP1           ; Save number of text lines to temporary variable
            asl     a               
            asl     a
            adc     TEMP1
            asl     a
            adc     YTDOWN
            sec
            sbc     #$01
            jsr     VLINSTART
            dex
            txa
            inx
            sta     TEMP1
            asl     a
            adc     TEMP1
            pha
            asl     a
            and     #$07
            sta     CHRDISPL
            pla
            lsr     a
            lsr     a
            clc
            adc     VRAMDST
            sta     VRAMDST
            bcc     LCF60
            inc     VRAMDST+1
LCF60:      rts
.endproc

            eor     #$FF            ; 0xDEAD 0xC0DE?

; Internal procedure: Calculates a video line start address
; Video res is 480x256 bits, or 60x256 bytes
; This routine multiplies the line number x 60 and then adds the base address to
; the result and stores it in VRAMDST
; 
; A = Y coordinate of the start of the text window
;
; Returns strat address into VRAMDST
;
.proc VLINSTART
            pha                     ; Preserve Y Coordinate
            lda     #$00            ; Init result variable
            sta     TEMP2           ;
            pla                     ; Recover Y coordinate
            asl     a               ; Multiply by 4, result goes into TEMP2:TEMP1
            rol     TEMP2           ;
            asl     a               ;
            rol     TEMP2           ;
            sta     TEMP1           ;
            lda     TEMP2           ; Now, multiply by 16 (64 in total)
            sta     VRAMDST+1       ; and store result in VRAMDST
            lda     TEMP1           ;
            asl     a               ;
            rol     VRAMDST+1       ;
            asl     a               ;
            rol     VRAMDST+1       ;
            asl     a               ;
            rol     VRAMDST+1       ;
            asl     a               ;
            rol     VRAMDST+1       ;
            sec                     ; Substract the stored x4 nultiplication
            sbc     TEMP1           ;
            sta     VRAMDST         ;
            lda     VRAMDST+1       ;
            sbc     TEMP2           ;
            clc                     ; 
            adc     #>VIDEORAM      ; And adds video ram origin
            sta     VRAMDST+1       ;
            rts
.endproc

LCF92:      jsr     LCFFD
            jsr     _TIOON
            lda     BNKCTL
            eor     FNBNK
            sta     BNKCTL
            ldy     #$06
            ldx     #$00
LCFA5:      stx     TEMP1
            lda     (CHARFNTP),y
            and     #$F8
            ldx     CHRDISPL
            beq     LCFB5
LCFAF:      lsr     a
            ror     TEMP1
            dex
            bne     LCFAF
LCFB5:      sta     L02B4,y
            lda     TEMP1
            sta     L02BE,y
            dey
            bpl     LCFA5
            iny
            lda     (CHARFNTP),y
            ror     a
            bcc     LCFF2
            ldx     #$06
LCFC8:      lda     L02B4,x
            sta     L02B6,x
            lda     L02BE,x
            sta     L02C0,x
            dex
            bpl     LCFC8
            lda     #$00
            ldx     #$02
            jsr     LD001
            lda     (CHARFNTP),y
            ror     a
            ror     a
            bcc     LCFF2
            lda     #$20
            ldx     CHRDISPL
LCFE8:      lsr     a
            ror     L02BE
            dex
            bpl     LCFE8
            sta     L02B4
LCFF2:      lda     BNKCTL
            ora     #$03
            sta     BNKCTL
            jmp     _IORES

LCFFD:      lda     #$00
            ldx     #$09
LD001:      sta     L02B3,x
            sta     L02BD,x
            dex
            bpl     LD001
            rts

;
; Internal procedure: Print character to screen at address VRAMORG
; using the internal or external font, depending on the value of EXFONT
;
; A contains character index to char table (ascii code - $20)
; VRAMORG is start location in video memory
;
; On exit,
;
.proc PRNCHR
            sta     TEMP1           ; Save character index
            ldx     #$00            ; Init TEMP2
            stx     TEMP2           ;

            ; Multiply character index by 7, result into TEMP2:A
            ; First, multiply by 8. Then, if result is not 0, substract
            ; character index to obtain the result.

            asl     a               ; Bigger char pos id 5F, so first shift never
                                    ; Generates carry
            asl     a               ; Second shift, insert carry into TEMP2
            rol     TEMP2           ;
            asl     a               ; Third shift, insert carry into TEMP2
            rol     TEMP2           ;
            sec                     ; Clear borrow
            sbc     TEMP1           ; Substract char pos from accumulator to obtain x7
            bcs     SKIP            ; No borrow skip
            dec     TEMP2           ; Borrow, decrement MSB
SKIP:       clc                     ; Clear carry for next addition
            bit     EXFONT          ; Use xsternal font table?
            bpl     INTERNAL        ; No, go set internal
            adc     QEXFNT          ; Add char index to external table addr
            sta     CHARFNTP        ; and set the address of char in font table
            lda     QEXFNT+1        ;
            adc     TEMP2           ;
            sta     CHARFNTP+1      ;
            lda     EXFTBK          ; Get bank of external font table
            jmp     PRINT           ; And continue to print character

INTERNAL:   adc     #<__CHARTBL     ; Set pointer to character into internal char table
            sta     CHARFNTP        ;
            lda     #>__CHARTBL     ;
            adc     TEMP2           ;
            sta     CHARFNTP+1      ;
            lda     #$01            ; Bank of internal character table

PRINT:      sta     FNBNK           ; Set bank of current character table
            jsr     LCF92
            jsr     SWTBANK1        ; Turns on I-O address space and switches to bank 1
            ldx     CHRDISPL        ; Get window mask for the char displacement
            lda     WMASKTHI,x      ;
            sta     TMPMASK+1       ; And store it
            lda     WMASKTLO,x      ;
            sta     TMPMASK         ;
            ldx     #$09
LD05C:      ldy     #$01
            lda     TMPMASK+1
            and     (VRAMORG),y
            ora     L02BD,x
            sta     (VRAMORG),y
            dey
            lda     TMPMASK
            and     (VRAMORG),y
            ora     L02B3,x
            sta     (VRAMORG),y
            jsr     RSTRLNUP
            dex
            bpl     LD05C
            jmp     RSTBANK0        ; Restores Bank 0 and turns on RAM at BE00-BFFF
.endproc

; TABLE OF MASKS FOR OPENING UP A 5 BIT WINDOW ANYWHERE IN GRAPHIC MEMORY
;
WMASKTLO:   .byte   $03
            .byte   $81
            .byte   $C0
            .byte   $E0
            .byte   $F0
            .byte   $F8
            .byte   $FC
            .byte   $FE
;HIGH BYTE
WMASKTHI:   .byte   $FF
            .byte   $FF
            .byte   $FF
            .byte   $7F
            .byte   $3F
            .byte   $1F
            .byte   $0F
            .byte   $07

_OFFTCR:    asl     CURVIS
            bcs     _FLPTCR
            rts

_ONTCR:     sec
            ror     CURVIS
            ; Fall through

_FLPTCR:    pha
            txa
            pha
            tya
            pha
            jsr     LD13B
            jsr     LD0A7
            pla
            tay
            pla
            tax
            pla
            rts

LD0A7:      jsr     VDST2ORG
            jsr     SWTBANK1        ; Turns on I-O address space and switches to bank 1
            ldx     CHRDISPL
            lda     LD0FE,x
            sta     TMPMASK+1
            lda     LD0F6,x
            sta     TMPMASK
            ldx     #$09
LD0BD:      ldy     #$01
            lda     TMPMASK+1
            eor     (VRAMORG),y
            sta     (VRAMORG),y
            dey
            lda     TMPMASK
            eor     (VRAMORG),y
            sta     (VRAMORG),y
            jsr     RSTRLNUP
            dex
            bpl     LD0BD
            jmp     RSTBANK0        ; Restores Bank 0 and turns on RAM at BE00-BFFF

LD0D7:      jsr     VDST2ORG
            jsr     RSTRLNUP
            jsr     SWTBANK1        ; Turns on I-O address space and switches to bank 1
            ldx     CHRDISPL
            lda     LD0FE,x
            ldy     #$01
            eor     (VRAMORG),y
            sta     (VRAMORG),y
            dey
            lda     LD0F6,x
            eor     (VRAMORG),y
            sta     (VRAMORG),y
            jmp     RSTBANK0        ; Restores Bank 0 and turns on RAM at BE00-BFFF

; EQUATES FOR CURSOR CELL POSITIONS
;
LD0F6:      .byte   $FC
            .byte   $7E
            .byte   $3F
            .byte   $1F
            .byte   $0F
            .byte   $07
            .byte   $03
            .byte   $01
LD0FE:      .byte   $00
            .byte   $00
            .byte   $00
            .byte   $80
            .byte   $C0
            .byte   $E0
            .byte   $F0
            .byte   $F8

; Turns on I-O address space and switches to bank 1
;
SWTBANK1:   jsr     _TIOON          ; Turn on I-O address space
            lda     BNKCTL          ; Switch to bank 1
            ora     #$03            ;
            eor     #$01            ;
            sta     BNKCTL          ;
            lda     SVIA1DIR        ; Set port direction
            ora     #$03            ;
            sta     SVIA1DIR        ;
            rts

; Turn on I-O address space
;
_TIOON:     php                     ; Save IRQ enable flag
            sec
            sei
            ror     SEEIO           ; Set I-O enable flag
            sta     IOENABLE        ; Enable I/O space from $BE00 to $BFFF
            plp                     ; Restore IRQ enable flag
            rts

; Switches back to Bank 0 and turns on RAM at BE00-BFFF
;
.proc RSTBANK0
            lda     BNKCTL          ; Switch to bank 0
            ora     #$03
            sta     BNKCTL
            ; Fall through
.endproc

; Turn RAM on at BEOO-BFFF
;
.proc _IORES
            php                     ; Save IRQ enable flag
            sei
            asl     SEEIO           ; Test I-O enable flag
            bmi     RETURN          ; Don't enable if flag still set
            sta     IODISABLE
RETURN:     plp
            rts
.endproc


.proc LD13B
            jsr     LCF17
            ; Fall through
.endproc

; Internal procedure: Sets new video RAM origin at last video RAM destination
;
.proc VDST2ORG
            pha                     ; Preserves A
            lda     VRAMDST         ; Get VRAMDST and store into VRAMORG
            sta     VRAMORG         ;
            lda     VRAMDST+1       ;
            sta     VRAMORG+1       ;
            pla                     ; Restore A
            rts
.endproc

; Internal procedure: Go up one raster line, mantaining X position
;
.proc RSTRLNUP   
            lda     VRAMORG         ; Get current coordinates
            sec                     ; Substract number of bytes per line
            sbc     #VIDHRES/8      ;
            sta     VRAMORG         ; Update coordinates
            bcs     RETURN          ;
            dec     VRAMORG+1       ;
RETURN:     rts
.endproc

; Internal procedure: Ring the bell
;
; Preserves registers
;
.proc RNGBEL
            bit     NOBELL          ; Check if bell is on
            bmi     SNDONE          ; No, just return
            pha                     ; Save A onto the stack
            txa                     ; Save X
            pha                     ;
            tya                     ; Save Y
            pha                     ;
            ldy     BELPER          ; Get period
            ldx     BELCY           ;   volume
            lda     BELVOL          ;   and duration
            jmp     MKSOUND         ; Make sound, restore registers and return
.endproc

; GENERATE A CLICK
;
.proc CLICK
            bit     NOCLIK          ; If no click is set
            bmi     SNDONE          ; Just return
            pha                     ; Preserve A, X, Y
            txa                     ;
            pha                     ;
            tya                     ;
            pha                     ;
            ldy     CLKPER          ; Set period
            ldx     CLKCY           ; Set duration
            lda     CLKVOL          ; Set volume
            ; Fall through
.endproc

; Internal procedure: Generates an audible beep of period Y, volume X and duration A
;
; On entry, Y, X and A must be pushed into stack
; On exit, restores A, X and Y from the stack
;
.proc MKSOUND
            jsr     _BEEP           ; Make the sound
            pla                     ; Restore Y, X, A
            tay                     ;
            pla                     ;
            tax                     ;
            pla                     ;
            ; Fall through
.endproc

SNDONE:     rts                     ; Common return point for sound procedures

; BEEP - Sound an audible beep. Generates a square wave of 2 x A amplitude
;
; Arguments: A = volume in range of $00 (silence) to $7F (maximum), $40 is normal
;            X = duration in complete waveform cycles, 1-255, 0=256
;            Y = waveform period in units of 200 microseconds
;
; Arguments returned: None, X and Y registers preserved (Manual says ALL, but A
;                     is adjusted if was > $7F)
;
.proc _BEEP
            cmp     #$00            ; Check if volume is within range
            bpl     CONT            ; Under $80, continue
            lsr     a               ; Over $80, adjust it into range
CONT:       sta     ASVBP           ; Store volume temporarily
            txa                     ; Preserve X
            pha                     ;
            jsr     _TIOON          ; Turn on I/O address space
            lda     #$FF            ; Set audio port direction to OUT
            sta     SVIA2ADIR       ;
LOOP:       lda     ASVBP           ; Recover volume
            clc                     ; Turn bit 7 on
            adc     #$80            ;
            sta     SVIA2APORT      ; Set level for higher half of the square wave
            jsr     WAITHALF        ; Wait half waveform cycle
            lda     #$80            ; Set level for lower half of the square wave
            sec                     ;
            sbc     ASVBP           ;
            sta     SVIA2APORT      ;
            jsr     WAITHALF        ; Wait the other half wafeform cycle
            dex                     ; Repeat until duration is completed
            bpl     LOOP            ;
            lda     #$80            ; Mute
            sta     SVIA2APORT      ;
            jsr     _IORES          ; Restore RAM address space
            pla                     ; Restore X
            tax                     ;
            lda     ASVBP           ; And restore adjusted A
            rts
.endproc

; Internal procedure: Wait de duration of half a waveform period
;
; Arguments: Y contains the waveform period in units of 200 microseconds, so each
;            loop must take 100 microseconds (as we are waiting the duration of
;            half period)
;
.proc WAITHALF
            tya                     ; Preserve Y (Period)
            pha                     ;
LOOP:       lda     #$12            ; Init 90 microseccons loop
            sec
WAIT90:     sbc     #$01            ; 90 microseconds loop (approx., could be up to
                                    ; 108 if jump occurs between pages)
            bne     WAIT90
            dey                     ; Decrement count
            bne     LOOP            ; And repeat until complete
            pla                     ; Restore Y
            tay                     ;
            rts
.endproc

; DRWLEG - Draw legend boxes at the bottom 16 raster lines
;
; Monomeg display is VIDHRESxVIDVRES bits = $3C00 bytes
;
; Last 16 lines start at VIDHRES*(VIDVRES-16)/8 = $3840
; and occupy 16*VIDHRES/8 = $3C0 bytes
;
; Origin of char legends is VIDHRES*(VIDVRES-2)/8 = $3B88
;
.proc _DRWLEG
            jsr     _CLRLEG         ; Clear the legend boxes area
            ldy     #$00            ; Init index to legends table
            lda     #$05            ; Horizontal displacement from the origin byte
            sta     CHRDISPL        ;

            ; Set origin of first legend (using dest because entering
            ; the loop, orig is set as last dest)
            ;
            lda     #>(VIDEORAM+VIDHRES*(VIDVRES-2)/8)
            sta     VRAMDST+1
            lda     #<(VIDEORAM+VIDHRES*(VIDVRES-2)/8)
SETPOS:     sta     VRAMDST
LGNDLOOP:   lda     #$08            ; Legends are 8 bytes long
            sta     VRAMCNT         ; Use VRAMCNT LSB to store char count
PRCHAR:     jsr     VDST2ORG        ; Make last dest the new orig
            lda     LEGTBL,y        ; Get char of legend
            sec                     ;
            sbc     #' '            ; Char table starts with space, so rebase index
            bcc     DRBLNK          ; If it is not printable, print a space instead
            cmp     #$5F            ; Is it over the last printable char?
            bcc     CONT            ; No, continue
DRBLNK:     lda     #$00            ; Yes, print a space instead
CONT:       sty     VRAMCNT+1       ; Use VRAMCNT MSB to preserve legend table index
            jsr     PRNCHR          ; Print char
            lda     CHRDISPL        ; Recover displacement of last char
            clc                     ;
            adc     #$06            ; Calculate next displacement adding char width
            cmp     #$08            ; If within the byte length
            bcc     STDISP          ;    continue
            and     #$07            ; MOD 8
            inc     VRAMDST         ; And increment char origin
STDISP:     sta     CHRDISPL        ; Store new displacement
            ldy     VRAMCNT+1       ; Recover legend table index
            iny                     ; Next char in table
            dec     VRAMCNT         ; One less char of current legend left
            bne     PRCHAR          ; Go print char until no more left
            inc     VRAMDST         ; Legend complete. Advance to next box
            
            ; There are 8 legends, each legend is 8 bytes long, so half of the
            ; table is at 4 * 8 = 32 ($20) 
            ;
            cpy     #LEGTSIZ/2      ; Half of the table?
            bne     CHKEND          ; No, check if end

            ; Beginning of raster line for legends was $FB88. There are 60 ($3C)
            ; bytes per line, so $FB88+($3C/2) = $FBA6
            ;
            lda     #$A6            ; Position of the second legend group (half line)
            bne     SETPOS          ; Always jump
            ; Not reached

CHKEND:     cpy     #LEGTSIZ        ; End of the table?
            bcc     LGNDLOOP        ; No, continue printing

            ; Beginning of last scanline: VIDEORAM+VIDHRES*(VIDVRES-1)/8 = $FBC4
            ;
            lda     #<(VIDEORAM+VIDHRES*(VIDVRES-1)/8)
            sta     VRAMDST
            lda     #>(VIDEORAM+VIDHRES*(VIDVRES-1)/8)
            sta     VRAMDST+1
            jsr     DRBOXGRP        ; Draw box group
            lda     #$E2            ; Position of the second box group (half line)
            sta     VRAMDST         ;
            ; Fall through to print the second box group
.endproc

; Internal procedure: Draws group of four boxes starting at VRAMDST
;
.proc DRBOXGRP
            jsr     SWTBANK1        ; Turns on I-O address space and switches to bank 1
            lda     #$04            ; Each group has 4 boxes
            sta     VRAMCNT         ; Store the box count
LOOP:       jsr     DRWBOX          ; Draw box
            lda     VRAMDST         ; Get origin of last box
            clc                     ; Adds box length (7 bytes or 56 pixels)
            adc     #$07            ;
            sta     VRAMDST         ;
            dec     VRAMCNT         ; One less box to go
            bne     LOOP            ; Loop until no more
            jsr     VDST2ORG        ; Move VRAMDST to VRAMORG
            jsr     DRVRBOXLN       ; Draws rigth vertical line of last box group
            jmp     RSTBANK0        ; Restores Bank 0 and turns on RAM at BE00-BFFF
.endproc

; Internal procedure: Draw legend box. Draws top and bottom horizontal lines
; and left vertical line
;
.proc DRWBOX
            jsr     VDST2ORG        ; Move VRAMDST to VRAMORG
            jsr     DRHRBOXLN       ; Draw a horizontal 48 pixel long line
            jsr     DRVRBOXLN       ; Draw a vertical 13 pixel line going up
                                    ; At this point, VRAMORG is 13 lines up, so
                                    ; next procedure will print the top line 
            ; Fall through
.endproc

; Internal procedure: Draw a 56 pixel horizontal line (7 bytes) starting
; at VRAMORG
;
.proc DRHRBOXLN
            ldy     #$06            ; Byte count minus one
            lda     #$FF            ; Solid line
LOOP:       sta     (VRAMORG),y     ; Draw segment
            dey                     ; Next byte segment
            bpl     LOOP            ; Until no more
            rts
.endproc

; Internal procedure: Draw a 13 pixel vertical line starting at VRAMORG and
; going upwards
;
.proc DRVRBOXLN
            ldy     #$00
            ldx     #$0D            ; Line length
            bne     START           ; Always jump
LOOP:       jsr     RSTRLNUP        ; Go up one raster line
START:      lda     (VRAMORG),y     ; Get current byte value
            ora     #$80            ; Turns on first pixel (most significant bit)
            sta     (VRAMORG),y     ; 
            dex                     ; Next pixel
            bne     LOOP            ; Loop until no more
            rts
.endproc

IODRIVER_SIZE = * - SPKTRL


            .segment "chartbl"

            .byte   $58             ; CODOS loadable file header byte
            .byte   $00             ; Memory overlay
            .byte   $01             ; Memory bank
            .byte   $00             ; Reserved
            .addr   INITIO          ; Entry point
            .addr   CHTB            ; Load address
            .word   CHTB_SIZE       ; Memory image size


; CHARACTER FONT TABLE
; ENTRIES IN ORDER staRTING AT ASCII BLANK
; 96 ENTRIES
; EACH ENTRY CONTAINS 7 BYTES
; 7 BYTES ARE CHARACTER MATRIX, TOP ROW FIRST, LEFTMOST DOT
; IS LEFTMOST IN BYTE
; LOWER CASE FONT IS SMALL UPPER CASE, 5 BY 5 MATRIX
;
CHTB:       .byte        $00, $00, $00  ; BLANK
            .byte   $00, $00, $00, $00
            .byte        $20, $20, $20  ; !
            .byte   $20, $20, $00, $20
            .byte        $50, $50, $50  ; "
            .byte   $00, $00, $00, $00
            .byte        $50, $50, $F8  ; #
            .byte   $50, $F8, $50, $50
            .byte        $20, $78, $A0  ; $
            .byte   $70, $28, $F0, $20
            .byte        $C8, $C8, $10  ; %
            .byte   $20, $40, $98, $98
            .byte        $40, $A0, $A0  ; &
            .byte   $40, $A8, $90, $68
            .byte        $30, $30, $30  ; '
            .byte   $00, $00, $00, $00
            .byte        $20, $40, $40  ; (
            .byte   $40, $40, $40, $20
            .byte        $20, $10, $10  ; )
            .byte   $10, $10, $10, $20
            .byte        $20, $A8, $70  ; *
            .byte   $20, $70, $A8, $20
            .byte        $00, $20, $20  ; +
            .byte   $F8, $20, $20, $00
            .byte        $01, $00, $00  ; ,
            .byte   $30, $30, $10, $20
            .byte        $00, $00, $00  ; -
            .byte   $F8, $00, $00, $00
            .byte        $00, $00, $00  ; .
            .byte   $00, $00, $30, $30
            .byte        $08, $08, $10  ; /
            .byte   $20, $40, $80, $80
            .byte        $60, $90, $90  ; 0
            .byte   $90, $90, $90, $60
            .byte        $20, $60, $20  ; 1
            .byte   $20, $20, $20, $70
            .byte        $70, $88, $10  ; 2
            .byte   $20, $40, $80, $F8
            .byte        $70, $88, $08  ; 3
            .byte   $30, $08, $88, $70
            .byte        $10, $30, $50  ; 4
            .byte   $90, $F8, $10, $10
            .byte        $F8 ,$80, $F0  ; 5
            .byte   $08, $08, $08, $F0
            .byte        $70, $80, $80  ; 6
            .byte   $F0, $88, $88, $70
            .byte        $F8, $08, $10  ; 7
            .byte   $20, $40, $80, $80
            .byte        $70, $88, $88  ; 8
            .byte   $70, $88, $88, $70
            .byte        $70, $88, $88  ; 9
            .byte   $78, $08, $08, $70
            .byte        $00, $00, $30  ; :
            .byte   $30, $00, $30, $30
            .byte        $31, $30, $00  ; ;
            .byte   $30, $30, $10, $20
            .byte        $10, $20, $40  ; LESS THAN
            .byte   $80, $40, $20, $10
            .byte        $00, $00, $F8  ; =
            .byte   $00, $F8, $00, $00
            .byte        $40, $20, $10  ; GREATER THAN
            .byte   $08, $10, $20, $40
            .byte        $70, $88, $08  ; ?
            .byte   $10, $20, $00, $20
            .byte        $70, $88, $08  ; @
            .byte   $68, $A8, $A8, $D0
            .byte        $20, $50, $88  ; A
            .byte   $88, $F8, $88, $88
            .byte        $F0, $48, $48  ; B
            .byte   $70, $48, $48, $F0
            .byte        $70, $88, $80  ; C
            .byte   $80, $80, $88, $70
            .byte        $F0, $48, $48  ; D
            .byte   $48, $48, $48, $F0
            .byte        $F8, $80, $80  ; E
            .byte   $F0, $80, $80, $F8
            .byte        $F8, $80, $80  ; F
            .byte   $F0, $80, $80, $80
            .byte        $70, $88, $80  ; G
            .byte   $B8, $88, $88, $70
            .byte        $88, $88, $88  ; H
            .byte   $F8, $88, $88, $88
            .byte        $70, $20, $20  ; I
            .byte   $20, $20, $20, $70
            .byte        $38, $10, $10  ; J
            .byte   $10, $10, $90, $60
            .byte        $88, $90, $A0  ; K
            .byte   $C0, $A0, $90, $88
            .byte        $80, $80, $80  ; L
            .byte   $80, $80, $80, $F8
            .byte        $88, $D8, $A8  ; M
            .byte   $A8, $88, $88, $88
            .byte        $88, $88, $C8  ; N
            .byte   $A8, $98, $88, $88
            .byte        $70, $88, $88  ; O
            .byte   $88, $88, $88, $70
            .byte        $F0, $88, $88  ; P
            .byte   $F0, $80, $80, $80
            .byte        $70, $88, $88  ; Q
            .byte   $88, $A8, $90, $68
            .byte        $F0, $88, $88  ; R
            .byte   $F0, $A0, $90, $88
            .byte        $78, $80, $80  ; S
            .byte   $70, $08, $08, $F0
            .byte        $F8, $20, $20  ; T
            .byte   $20, $20, $20, $20
            .byte        $88, $88, $88  ; U
            .byte   $88, $88, $88, $70
            .byte        $88, $88, $88  ; V
            .byte   $50, $50, $20, $20
            .byte        $88, $88, $88  ; W
            .byte   $A8, $A8, $D8, $88
            .byte        $88, $88, $50  ; X
            .byte   $20, $50, $88, $88
            .byte        $88, $88, $50  ; Y
            .byte   $20, $20, $20, $20
            .byte        $F8, $08, $10  ; Z
            .byte   $20, $40, $80, $F8
            .byte        $70, $40, $40  ; LEFT BRACKET
            .byte   $40, $40, $40, $70
            .byte        $80, $80, $40  ; BACKSLASH
            .byte   $20, $10, $08, $08
            .byte        $70, $10, $10  ; RIGHT BRACKET
            .byte   $10, $10, $10, $70
            .byte        $20, $50, $88  ; CARET
            .byte   $00, $00, $00, $00
            .byte        $00, $00, $00  ; UNDERLINE
            .byte   $00, $00, $00, $F8
            .byte        $C0, $60, $30  ; GRAVE ACCENT
            .byte   $00, $00, $00, $00
            .byte        $00, $60, $10  ; A (LC)
            .byte   $70, $90, $90, $68
            .byte        $80, $80, $F0  ; B (LC)
            .byte   $88, $88, $88, $F0
            .byte        $00, $00, $78  ; C (LC)
            .byte   $80, $80, $80, $78
            .byte        $08, $08, $78  ; D (LC)
            .byte   $88, $88, $88, $78
            .byte        $00, $00, $70  ; E (LC)
            .byte   $88, $F0, $80, $78
            .byte        $30, $40, $40  ; F (LC)
            .byte   $E0, $40, $40, $40
            .byte        $71, $88, $88  ; G (LC)
            .byte   $98, $68, $08, $70
            .byte        $80, $80, $B0  ; H (LC)
            .byte   $C8, $88, $88, $88
            .byte        $20, $00, $60  ; I (LC)
            .byte   $20, $20, $20, $70
            .byte        $73, $10, $10  ; J (LC)
            .byte   $10, $10, $90, $60
            .byte        $80, $80, $90  ; K (LC)
            .byte   $A0, $C0, $A0, $90
            .byte        $60, $20, $20  ; L (LC)
            .byte   $20, $20, $20, $20
            .byte        $00, $00, $D0  ; M (LC)
            .byte   $A8, $A8, $A8, $A8
            .byte        $00, $00, $B0  ; N (LC)
            .byte   $C8, $88, $88, $88
            .byte        $00, $00, $70  ; O (LC)
            .byte   $88, $88, $88, $70
            .byte        $F1, $88, $88  ; P (LC)
            .byte   $88, $F0, $80, $80
            .byte        $79, $88, $88  ; Q (LC)
            .byte   $88, $78, $08, $08
            .byte        $00, $00, $B0  ; R (LC)
            .byte   $C8, $80, $80, $80
            .byte        $00, $00, $78  ; S (LC)
            .byte   $80, $70, $08, $F0
            .byte        $40, $40, $E0  ; T (LC)
            .byte   $40, $40, $50, $20
            .byte        $00, $00, $90  ; U (LC)
            .byte   $90, $90, $90, $68
            .byte        $00, $00, $88  ; V (LC)
            .byte   $88, $50, $50, $20
            .byte        $00, $00, $A8  ; W (LC)
            .byte   $A8, $A8, $A8, $50
            .byte        $00, $00, $88  ; X (LC)
            .byte   $50, $20, $50, $88
            .byte        $89, $88, $88  ; Y (LC)
            .byte   $50, $20, $40, $80
            .byte        $00, $00, $F8  ; Z (LC)
            .byte   $10, $20, $40, $F8
            .byte        $10, $20, $20 ; LEFT BRACE
            .byte   $60, $20, $20, $10
            .byte        $20, $20, $20 ; VERTICAL BAR
            .byte   $00, $20, $20, $20
            .byte        $40, $20, $20 ; RIGHT BRACE
            .byte   $30, $20, $20, $40
            .byte        $10, $A8, $40 ; CURLY
            .byte   $00, $00, $00, $00
            .byte        $A8, $50, $A8 ; RUBOUT
            .byte   $50, $A8, $50, $A8

CHTB_SIZE = * - CHTB

            .end
