


            .include "codos.inc"

            .importzp QLN, BYTRES, DESTBUFF, MEMBUFF, MEMCOUNT, P0SCRATCH, PCSAVE
            .importzp TMPBUFP, TMPPTR 

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

            .export GETKEY, OUTCH, TSTKEY, INITIO, DRWLEG, INLINE

; TODO: Make this address independent
;
KSCCLR      = $BFC5             ; PORT TO CLEAR KEYBOARD SCAN COUNTER
KBDATA      = $BFE1             ; DATA REGISTER FOR KEYBOARD PORT
KBT1CH      = $BFE5             ; TIMER 1 COUNT HIGH REGISTER FOR KEYBOARD 6522
KBPCR       = $BFEC             ; PERIPHERAL CONTROL REGISTER FOR KEYBOARD 6522
SYS1IER     = $BFEE             ; SYS1 I/O CHIP INTERRUPT ENABLE REGISTER
KBIFR       = $BFED             ; INTERRUPT FLAG REGISTER FOR KEYBOARD 6522
KBIER       = $BFEE             ; INTERRUPT ENABLE REGISTER FOR KEYBOARD 6522
KBACR       = $BFEB             ; AUXILIARY CONTROL REGISTER FOR KEYBOARD 6522


TEMP1       := $00FA            ; TEMPORARY STORAGE FOR KEYBOARD ROUTINE

LSTKEY      := $020D            ; KEYBOARD KEY LAST DOWN
RPTFLG      := $020E            ; FLAG USED BY AUTO REPEAT ALGORITHM

CURVIS      := $021B            ; FLAG INDICATING PRESENT STATE OF CURSOR
DBCDLA      := $0220            ; DEBOUNCE DELAY SYSTEM PARAMETER

TCURS       := $02CA 
XSVKB       := $02CD
YSVKB       := $02CE

L02DC       := $02DC

GETKEY:     jmp     _GETKEY     ; Wait until a keyboard key is struck and return character in A
OUTCH:      jmp     _OUTCH      ; Display printable character or interpret control character
TSTKEY:     jmp     _TSTKEY     ; Test if a key is pressed
INITIO:     jmp     _INITIO     ; Clear screen and set default values of display parameters

CLRDSP:     jmp     _CLRDSP     ; Clear the entire 480 by 256 screen
DRWLEG:     jmp     _DRWLEG     ; Draw legends
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
            .addr   LC5B0           ; Load address
            .word   IODRIVER_SIZE   ; Memory image size

;KEYBOARD ENTRY
LC5B0:      .byte  $8A ;'*' MULTIPLY
            .byte  $2A ;'*' MULTIPLY
            .byte  $8B ;'/' DIVIDE
            .byte  $2F ;'/' DIVIDE
            .byte  $8C ;'-' MINUS
            .byte  $2D ;'-' MINUS
            .byte  $8D ;'+' PLUS
            .byte  $2B ;'+' PLUS
            .byte  $FF 
            .byte  $FF 
            .byte  $FF 
            .byte  $FF 

;KEYBOARD SPECIAL KEYS
LC5BC:      .byte  $02 ;'stx'
            .byte  $03 ;'ETX'
            .byte  $05 ;'ENG'
            .byte  $07 ;'BEL'
            .byte  $08 ;'BS'      
            .byte  $09 ;'HT'
            .byte  $0A ;'LF'      
            .byte  $0B ;'VT'      
            .byte  $0C ;'FF'     
            .byte  $0D ;'RETURN'
            .byte  $12 ;'DC2'
            .byte  $17 ;'ETB'      
            .byte  $18 ;'CAN'      
            .byte  $1A ;'SUB'      
            .byte  $1B ;'ESC'      
            .byte  $7F ;'DEL'      
            .byte  $8E ;'ENTER'
            .byte  $A0 ;'CURSOR UP'
            .byte  $A1 ;'CURSOR LEFT'     
            .byte  $A2 ;'CURSOR RIGHT'
            .byte  $A3 ;'CURSOR DOWN'     
            .byte  $A4 ;'HOME'
            .byte  $A5 ;'DELETE'      
            .byte  $A6 ;'INSERT'
            .byte  $B0 ;'SHIFT/CURSOR UP'   
            .byte  $B1 ;'SHIFT/CURSOR LEFT' 
            .byte  $B2 ;'SHIFT/CURSOR RIGHT' 
            .byte  $B3 ;'SHIFT/CURSOR DOWN' 
            .byte  $B4 ;'SHIFT/ HOME 
            .byte  $FF      
            .byte  $FF      

;JUMP TABLE
LC5DB:      .addr  LC682       
            .addr  $0303 ; $0303 JUMP EXECUTED WHEN CTRL-C IS ENTERED FROM CONSOLE   
            .addr  $C68B            
            .addr  $C696             
            .addr  $C69C             
            .addr  $C6AE             
            .addr  $C6B4             
            .addr  $C6CF             
            .addr  $C6EB             
            .addr  $C6F4             
            .addr  $C70A             
            .addr  $C710 
            .addr  $C716 
            .addr  $C71C             
            .addr  $C722             
            .addr  $C728             
            .addr  $C6F4             
            .addr  $C6CF             
            .addr  $C69C             
            .addr  $C734             
            .addr  $C6B4 
            .addr  $C746 
            .addr  $C74C 
            .addr  $C752             
            .addr  $C6CF             
            .addr  $C759             
            .addr  $C765 
            .addr  $C6B4 
            .addr  $C6EB             
            .addr  $0000             
            .addr  $0000             


_INLINE:    ldy     #$00
            ; Fall through

_EDLINE:    sty     $FD
            cld
            lda     $023D
            sta     $02DA
            stx     $02D6
LC627:      jsr     LC837
LC62A:      lda     #$00
            sta     $02D7
LC62F:      jsr     _GETKEY
            cmp     #$7F
            bcs     LC640
            cmp     #$20
            bcc     LC64D
LC63A:      jsr     LC76F
            jmp     LC62F

LC640:      beq     LC64D
            cmp     #$88
            bcs     LC64D
            jsr     LC8B3
            cmp     #$0D
            bne     LC62F
LC64D:      ldx     #$1E
LC64F:      cmp     LC5BC,x
            beq     LC669
            dex
            bpl     LC64F
            ldx     #$0A
LC659:      cmp     LC5B0,x
            beq     LC67B
            dex
            dex
            bpl     LC659
            bit     UKINLN
            bmi     LC63A
            bpl     LC62F
LC669:      txa
            asl     a
            tax
            lda     LC5DB,x
            sta     L02DC
            lda     LC5DB+1,x
            sta     $02DD
            jmp     (L02DC)

LC67B:      inx
            lda     LC5B0,x
            jmp     LC63A


LC682:      jsr     LC817
            jsr     LC926
            jmp     LC627

            lda     NOLEKO
            eor     #$FF
            sta     NOLEKO
            jmp     LC62F

            jsr     LD155
            jmp     LC62F

            cpy     $02D5
            beq     LC6A8
            dey
LC6A2:      jsr     LCEF0
            jmp     LC62A

LC6A8:      cpy     $FD
            beq     LC6A2
            bne     LC6B1
            jsr     LC7CE
LC6B1:      jmp     LC62A

            tya
            clc
            adc     #$50
            bcs     LC6C5
            cmp     $FD
            bcs     LC6C5
            tay
LC6BF:      jsr     _LINEFD
            jmp     LC62A

LC6C5:      cpy     $FD
            bne     LC70D
            sty     $02D5
            jmp     LC6BF

            tya
            sec
            sbc     #$50
            bcc     LC6E1
            cmp     $02D5
            bcc     LC6E1
            tay
LC6DB:      jsr     LCF08
            jmp     LC62A

LC6E1:      cpy     $FD
            bne     LC70D
            sty     $02D5
            jmp     LC6DB

            jsr     _CLRHTW
LC6EE:      sty     $02D5
            jmp     LC62A

            clc
            ldy     $FD
LC6F7:      lda     #$0D
            sta     (QLN),y
            php
            jsr     LC788
            jsr     LC8F7
            plp
            tya
            ldy     #$00
            ldx     $02D6
            rts

            jsr     LC837
LC70D:      jmp     LC62A

            jsr     LC7B9
            jmp     LC62A

            jsr     LC817
            jmp     LC62A

            cpy     #$00
            bne     LC70D
            beq     LC6F7
            jsr     _GETKEY
            jmp     LC63A

            jsr     LC793
            jmp     LC62F

            jsr     LC849
            jmp     LC62F

            cpy     $FD
            bcs     LC73F
            iny
LC739:      jsr     LCEDA
            jmp     LC62A

LC73F:      cpy     $02D5
            beq     LC739
            bne     LC70D
            jsr     _HOMETW
            jmp     LC6EE

            jsr     LC849
            jmp     LC62F

            sec
            ror     $02D7
            jmp     LC62F

            lda     $02D5
            sta     $02D4
            jsr     LC8A1
            jmp     LC62A

LC765:      cpy     $FD
            beq     LC70D
            jsr     LCEDA
            iny
            bne     LC765
LC76F:      cpy     YLNLIM
            bcc     LC777
            jmp     LD155

LC777:      bit     $02D7
            bpl     LC77F
            jmp     LC86C

LC77F:      sta     (QLN),y
            cpy     $FD
            bcc     LC787
            inc     $FD
LC787:      iny
LC788:      bit     NOLEKO
            bmi     LC790
            jmp     OUTCH

LC790:      jmp     LCEDA

LC793:      cpy     $02D5
            beq     LC7B8
            bit     $02D7
            bpl     LC7A4
            jsr     LCEF0
            dey
            jmp     LC849

LC7A4:      cpy     $FD
            bcc     LC7AA
            dec     $FD
LC7AA:      dey
            jsr     LCEF0
            lda     #$20
            sta     (QLN),y
            jsr     LC788
            jsr     LCEF0
LC7B8:      rts

LC7B9:      sty     $02D4
LC7BC:      cpy     $FD
            bcs     LC7C8
            lda     #$20
            jsr     LC788
            iny
            bne     LC7BC
LC7C8:      jsr     LC8A1
            sty     $FD
            rts

LC7CE:      sty     $02D4
            ldx     #$00
LC7D3:      lda     $06E0,x
            beq     LC813
            tay
            dey
            cpy     $02D4
            beq     LC80E
            bcc     LC80E
            cpy     YLNLIM
            bcs     LC813
            tya
            ldy     $02D4
            sta     $02D4
LC7ED:      cpy     $FD
            bcs     LC7FC
            cpy     $02D4
            bcs     LC80D
            jsr     LCEDA
            iny
            bne     LC7ED
LC7FC:      lda     #$20
            cpy     $02D4
            bcs     LC80B
            jsr     LC788
            sta     (QLN),y
            iny
            bne     LC7FC
LC80B:      sty     $FD
LC80D:      rts

LC80E:      inx
            cpx     #$20
            bcc     LC7D3
LC813:      ldy     $02D4
            rts

LC817:      lda     $02D5
            sta     $02D4
            jsr     LC8A1
            lda     #$20
LC822:      cpy     $FD
            bcs     LC82C
            jsr     LC788
            iny
            bne     LC822
LC82C:      jsr     LC8A1
            ldy     #$00
            sty     $02D5
            sty     $FD
            rts

LC837:      ldy     #$00
            sty     $02D5
LC83C:      cpy     $FD
            beq     LC848
            lda     (QLN),y
            jsr     LC788
            iny
            bne     LC83C
LC848:      rts

LC849:      cpy     $FD
            bcs     LC86B
            sty     $02D4
LC850:      iny
            cpy     $FD
            bcs     LC861
            lda     (QLN),y
            dey
            sta     (QLN),y
            iny
            jsr     LC788
            jmp     LC850

LC861:      lda     #$20
            jsr     LC788
            dec     $FD
            jsr     LC8A1
LC86B:      rts

LC86C:      sta     $02D3
            sty     $02D4
            ldy     $FD
            cpy     YLNLIM
            bcc     LC87F
            jsr     LD155
            jmp     LC8AF

LC87F:      cpy     $02D4
            beq     LC88D
            dey
            lda     (QLN),y
            iny
            sta     (QLN),y
            dey
            bne     LC87F
LC88D:      inc     $FD
            lda     $02D3
            sta     (QLN),y
LC894:      lda     (QLN),y
            jsr     LC788
            iny
            cpy     $FD
            bcc     LC894
            inc     $02D4
LC8A1:      tya
            sec
            sbc     $02D4
            tax
            beq     LC8AF
LC8A9:      jsr     LCEF0
            dex
            bne     LC8A9
LC8AF:      ldy     $02D4
            rts

LC8B3:      and     #$7F
            asl     a
            asl     a
            asl     a
            asl     a
            asl     a
            tax
            stx     $02D8
            clc
            adc     #$20
            sta     $02D9
LC8C4:      lda     KEYSTR,x
            cmp     #$20
            bcc     LC8D5
            cmp     #$80
            bcs     LC8D5
            inx
            cpx     $02D9
            bne     LC8C4
LC8D5:      ror     NOLEKO
            stx     $02D9
            ldx     $02D8
LC8DE:      cpx     $02D9
            bcs     LC8EC
            lda     KEYSTR,x
            jsr     LC76F
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
            sty     $02D4
            ldy     $023D
            iny
            jsr     LD106
            txa
            sta     ($F2),y
            jsr     LD127
            sty     $023D
            ldy     $02D4
            cpy     $FD
            bne     LC8FC
            rts

LC91B:      lda     #$50
            sta     $F2
            lda     #$FC
            sta     $F3
            ldy     #$FF
            rts

LC926:      sty     $02D4
            jsr     LC91B
            ldy     $02DA
LC92F:      dey
            cpy     $023D
            beq     LC92F
            jsr     LD106
            lda     ($F2),y
            cmp     #$FF
            bne     LC947
            lda     $023D
            sta     $02DA
            jmp     LC94E

LC947:      cmp     #$0D
            bne     LC92F
            sty     $02DA
LC94E:      sty     $02DB
LC951:      iny
            jsr     LD106
            lda     ($F2),y
            tax
            jsr     LD127
            sty     $02DB
            ldy     $02D4
            txa
            sta     (QLN),y
            cmp     #$0D
            beq     LC972
            iny
            sty     $02D4
            ldy     $02DB
            jmp     LC951

LC972:      sty     $FD
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
            jsr     _OFFTCR          ; ENSURE CURSOR IS OFF
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
            stx     $FA
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
            ldx     $FA
            and     $BFE1
            jmp     _IORES

; MASK TABLE TO ISOLATE ROW BITS
;
ROWMSK:     .byte %00000100         ; keyboard ROW
            .byte %00001000
            .byte %00010000
            .byte %00100000
            .byte %01000000
            .byte %10000000

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
            sta     $02C7
            stx     $02C8
            sty     $02C9
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
            asl     $021D
            jsr     LD13B
            pla
            cmp     #$5F
            bne     LCC23
            bit     SHODEL
            bmi     LCC32
            jsr     LCEF0
            jsr     LD13B
            lda     #$00
            jsr     LD00B
            jmp     LCC48

LCC23:      cmp     #$3F
            bne     LCC32
            bit     SHOUL
            bmi     LCC32
            jsr     LD0D7
            jmp     LCC45

LCC32:      jsr     LD00B
            bit     UNDRLN
            bpl     LCC3D
            jsr     LD0D7
LCC3D:      bit     RVIDEO
            bpl     LCC45
            jsr     LD0A7
LCC45:      jsr     LCEDA
LCC48:      ldy     $02C9
            ldx     $02C8
            lda     $02C7
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
            jsr     LCEDA
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
            asl     $021D
            bcs     LCC48
            jsr     _CRLF
            jmp     LCC48

LCC9A:      cmp     #$0A
            bne     LCCA4
            jsr     _LINEFD
            jmp     LCC48

LCCA4:      cmp     $022B
            bne     LCCAF
            jsr     LCEF0
            jmp     LCC48

LCCAF:      cmp     $022E
            bne     LCCC2
            lda     LINE
            jsr     _CLRTLN
            lda     #$01
            sta     COL
            jmp     LCC48

LCCC2:      cmp     $022D
            bne     LCCCD
            jsr     _CLRHTW
            jmp     LCC48

LCCCD:      cmp     $022A
            bne     LCCD8
            jsr     LD155
            jmp     LCC48

LCCD8:      cmp     $022C
            beq     LCCE0
LCCDD:      jmp     LCC48

LCCE0:      ldx     #$00
LCCE2:      lda     $06E0,x
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

_INITIO:    jsr     _INITTW
            lda     #$00
            sta     QLN
            lda     #$05
            sta     $F1
            ldx     #$40
            lda     #$20
LCD20:      sta     $05BF,x
            dex
            bne     LCD20
            lda     #$80
LCD28:      sta     KEYSTR,x
            dex
            bne     LCD28
            lda     #$00
            sta     LSTKEY
            jmp     _DRWLEG

_INITTW:    cld
            jsr     _CLRDSP
            lda     #$00
            sta     NOLEKO
            ldx     #$0F
LCD41:      sta     KBECHO,x
            dex
            bpl     LCD41
            lda     #$01
            tax
            tay
            jsr     _BEEP
            jsr     LC91B
            ldy     #$00
            jsr     LD106
            lda     #$FF
            sta     ($F2),y
            iny
            lda     #$0D
            sta     ($F2),y
            sty     $023D
            jsr     LD127
            lda     #$18
            ldy     #$00
            ; Fall through

_DEFTW:     sta     NLINET
            sty     YTDOWN
            ; Fall through

_CLRHTW:    jsr     _CLRTW
            ; Fall through

_HOMETW:    pha
            lda     #$01
            sta     LINE
            sta     COL
            pla
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
            sta     $F4
            lda     #$C0
            sta     $F5
            lda     #$00
            sta     $F8
            lda     #$3C
            sta     $F9
            jsr     LCE15
            pla
            tax
            pla
            rts

_CLRTW:     pha
            txa
            pha
            lda     YTDOWN
            jsr     LCF63
            jsr     LD13E
            lda     NLINET
            ldx     #$50
            jsr     LCF36
            lda     $F2
            sec
            sbc     $F4
            sta     $F8
            lda     $F3
            sbc     $F5
            sta     $F9
            inc     $F8
            bne     LCDD6
            inc     $F9
LCDD6:      jsr     LCE15
            pla
            tax
            pla
            rts

_CLRLEG:    lda     #$40
            sta     $F4
            lda     #$F8
            sta     $F5
            lda     #$C0
            sta     $F8
            lda     #$03
            sta     $F9
            jmp     LCE15

LCDF0:      lda     NLINET
            ; Fall through

_CLRTLN:    ldx     #$50
            cmp     NLINET
            bcc     LCDFD
            lda     NLINET
LCDFD:      jsr     LCF36
            lda     $F2
            sec
            sbc     #$57
            sta     $F4
            lda     $F3
            sbc     #$02
            sta     $F5
            lda     #$58
            sta     $F8
            lda     #$02
            sta     $F9
            ; Fall through

LCE15:      jsr     LD106
            jsr     LCE1E
            jmp     LD127

LCE1E:      tya
            pha
            lda     #$00
            ldy     $F9
            beq     LCE35
            tay
LCE27:      sta     ($F4),y
            iny
            sta     ($F4),y
            iny
            bne     LCE27
            inc     $F5
            dec     $F9
            bne     LCE27
LCE35:      ldy     $F8
            beq     LCE3E
LCE39:      dey
            sta     ($F4),y
            bne     LCE39
LCE3E:      pla
            tay
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
            lda     $F2
            sta     $F8
            lda     $F3
            sta     $F9
            lda     YTDOWN
            jsr     LCF63
            jsr     LD13E
            lda     $F4
            clc
            adc     #$58
            sta     $F2
            lda     $F5
            adc     #$02
            sta     $F3
            lda     $F8
            sec
            sbc     $F2
            sta     $F8
            lda     $F9
            sbc     $F3
            sta     $F9
            inc     $F8
            bne     LCE97
            inc     $F9
LCE97:      jsr     LCE9D
LCE9A:      jmp     LCDF0

LCE9D:      jsr     LD106
            jsr     LCEA6
            jmp     LD127

LCEA6:      tya
            pha
            ldy     #$00
            ldx     $F9
            beq     LCECB
LCEAE:      lda     ($F2),y
            sta     ($F4),y
            iny
            lda     ($F2),y
            sta     ($F4),y
            iny
            lda     ($F2),y
            sta     ($F4),y
            iny
            lda     ($F2),y
            sta     ($F4),y
            iny
            bne     LCEAE
            inc     $F3
            inc     $F5
            dex
            bne     LCEAE
LCECB:      ldx     $F8
            beq     LCED7
LCECF:      lda     ($F2),y
            sta     ($F4),y
            iny
            dex
            bne     LCECF
LCED7:      pla
            tay
            rts

LCEDA:      pha
            lda     COL
            cmp     #$50
            bcs     LCEE7
            inc     COL
            pla
            rts

LCEE7:      jsr     _CRLF
            sec
            ror     $021D
            pla
            rts

LCEF0:      dec     COL
            bne     LCF07
            pha
            lda     #$50
            sta     COL
            dec     LINE
            bne     LCF06
            lda     NLINET
            sta     LINE
LCF06:      pla
LCF07:      rts

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
LCF36:      sta     $FA
            asl     a
            asl     a
            adc     $FA
            asl     a
            adc     YTDOWN
            sec
            sbc     #$01
            jsr     LCF63
            dex
            txa
            inx
            sta     $FA
            asl     a
            adc     $FA
            pha
            asl     a
            and     #$07
            sta     $FC
            pla
            lsr     a
            lsr     a
            clc
            adc     $F2
            sta     $F2
            bcc     LCF60
            inc     $F3
LCF60:      rts

            eor     #$FF
LCF63:      pha
            lda     #$00
            sta     $FB
            pla
            asl     a
            rol     $FB
            asl     a
            rol     $FB
            sta     $FA
            lda     $FB
            sta     $F3
            lda     $FA
            asl     a
            rol     $F3
            asl     a
            rol     $F3
            asl     a
            rol     $F3
            asl     a
            rol     $F3
            sec
            sbc     $FA
            sta     $F2
            lda     $F3
            sbc     $FB
            clc
            adc     #$C0
            sta     $F3
            rts

LCF92:      jsr     LCFFD
            jsr     _TIOON
            lda     BNKCTL
            eor     $02D0
            sta     BNKCTL
            ldy     #$06
            ldx     #$00
LCFA5:      stx     $FA
            lda     ($F6),y
            and     #$F8
            ldx     $FC
            beq     LCFB5
LCFAF:      lsr     a
            ror     $FA
            dex
            bne     LCFAF
LCFB5:      sta     $02B4,y
            lda     $FA
            sta     $02BE,y
            dey
            bpl     LCFA5
            iny
            lda     ($F6),y
            ror     a
            bcc     LCFF2
            ldx     #$06
LCFC8:      lda     $02B4,x
            sta     $02B6,x
            lda     $02BE,x
            sta     $02C0,x
            dex
            bpl     LCFC8
            lda     #$00
            ldx     #$02
            jsr     LD001
            lda     ($F6),y
            ror     a
            ror     a
            bcc     LCFF2
            lda     #$20
            ldx     $FC
LCFE8:      lsr     a
            ror     $02BE
            dex
            bpl     LCFE8
            sta     $02B4
LCFF2:      lda     BNKCTL
            ora     #$03
            sta     BNKCTL
            jmp     _IORES

LCFFD:      lda     #$00
            ldx     #$09
LD001:      sta     $02B3,x
            sta     $02BD,x
            dex
            bpl     LD001
            rts

LD00B:      sta     $FA
            ldx     #$00
            stx     $FB
            asl     a
            asl     a
            rol     $FB
            asl     a
            rol     $FB
            sec
            sbc     $FA
            bcs     LD01F
            dec     $FB
LD01F:      clc
            bit     EXFONT
            bpl     LD037
            adc     QEXFNT
            sta     $F6
            lda     $0232
            adc     $FB
            sta     $F7
            lda     EXFTBK
            jmp     LD043

LD037:      adc     #$50
            sta     $F6
            lda     #$FD
            adc     $FB
            sta     $F7
            lda     #$01
LD043:      sta     $02D0
            jsr     LCF92
            jsr     LD106
            ldx     $FC
            lda     LD084,x
            sta     $02B2
            lda     LD07C,x
            sta     $02B1
            ldx     #$09
LD05C:      ldy     #$01
            lda     $02B2
            and     ($F4),y
            ora     $02BD,x
            sta     ($F4),y
            dey
            lda     $02B1
            and     ($F4),y
            ora     $02B3,x
            sta     ($F4),y
            jsr     LD149
            dex
            bpl     LD05C
            jmp     LD127

; TABLE OF MASKS FOR OPENING UP A 5 BIT WINDOW ANYWHERE IN GRAPHIC MEMORY
;
LD07C:      .byte   $03       
			.byte   $81 
			.byte   $C0       
			.byte   $E0
			.byte   $F0       
			.byte   $F8       
			.byte   $FC       
			.byte   $FE
;HIGH BYTE
LD084:      .byte   $FF 
			.byte   $FF       
			.byte   $FF       
			.byte   $7F       
			.byte   $3F       
			.byte   $1F       
			.byte   $0F       
			.byte   $07  

_OFFTCR:    asl     $021B
            bcs     _FLPTCR
            rts

_ONTCR:     sec
            ror     $021B
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

LD0A7:      jsr     LD13E
            jsr     LD106
            ldx     $FC
            lda     LD0FE,x
            sta     $02B2
            lda     LD0F6,x
            sta     $02B1
            ldx     #$09
LD0BD:      ldy     #$01
            lda     $02B2
            eor     ($F4),y
            sta     ($F4),y
            dey
            lda     $02B1
            eor     ($F4),y
            sta     ($F4),y
            jsr     LD149
            dex
            bpl     LD0BD
            jmp     LD127

LD0D7:      jsr     LD13E
            jsr     LD149
            jsr     LD106
            ldx     $FC
            lda     LD0FE,x
            ldy     #$01
            eor     ($F4),y
            sta     ($F4),y
            dey
            lda     LD0F6,x
            eor     ($F4),y
            sta     ($F4),y
            jmp     LD127

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

LD106:      jsr     _TIOON
            lda     BNKCTL
            ora     #$03
            eor     #$01
            sta     BNKCTL
            lda     SVIA1DIR
            ora     #$03
            sta     SVIA1DIR
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

LD127:      lda     BNKCTL
            ora     #$03
            sta     BNKCTL
            ; Fall through

; Turn RAM on at BEOO-BFFF
;
_IORES:     php                     ; Save IRQ enable flag
            sei
            asl     SEEIO           ; Test I-O enable flag
            bmi     RETURN          ; Don't enable if flag still set
            sta     IODISABLE
RETURN:     plp
            rts


LD13B:      jsr     LCF17
LD13E:      pha
            lda     $F2
            sta     $F4
            lda     $F3
            sta     $F5
            pla
            rts

LD149:      lda     $F4
            sec
            sbc     #$3C
            sta     $F4
            bcs     LD154
            dec     $F5
LD154:      rts

LD155:      bit     NOBELL
            bmi     LD186
            pha
            txa
            pha
            tya
            pha
            ldy     BELPER
            ldx     BELCY
            lda     BELVOL
            jmp     LD17E

; GENERATE A CLICK
;
CLICK:      bit     NOCLIK
            bmi     LD186
            pha
            txa
            pha
            tya
            pha
            ldy     CLKPER
            ldx     CLKCY
            lda     CLKVOL
            ; Fall through

LD17E:      jsr     _BEEP
            pla
            tay
            pla
            tax
            pla
LD186:      rts

_BEEP:      cmp     #$00
            bpl     LD18C
            lsr     a
LD18C:      sta     $02B0
            txa
            pha
            jsr     _TIOON
            lda     #$FF
            sta     $BFF3
LD199:      lda     $02B0
            clc
            adc     #$80
            sta     $BFF1
            jsr     LD1C2
            lda     #$80
            sec
            sbc     $02B0
            sta     $BFF1
            jsr     LD1C2
            dex
            bpl     LD199
            lda     #$80
            sta     $BFF1
            jsr     _IORES
            pla
            tax
            lda     $02B0
            rts

LD1C2:      tya
            pha
LD1C4:      lda     #$12
            sec
LD1C7:      sbc     #$01
            bne     LD1C7
            dey
            bne     LD1C4
            pla
            tay
            rts

_DRWLEG:    jsr     _CLRLEG
            ldy     #$00
            lda     #$05
            sta     $FC
            lda     #$FB
            sta     $F3
            lda     #$88
LD1E0:      sta     $F2
LD1E2:      lda     #$08
            sta     $F8
LD1E6:      jsr     LD13E
            lda     LEGTBL,y
            sec
            sbc     #$20
            bcc     LD1F5
            cmp     #$5F
            bcc     LD1F7
LD1F5:      lda     #$00
LD1F7:      sty     $F9
            jsr     LD00B
            lda     $FC
            clc
            adc     #$06
            cmp     #$08
            bcc     LD209
            and     #$07
            inc     $F2
LD209:      sta     $FC
            ldy     $F9
            iny
            dec     $F8
            bne     LD1E6
            inc     $F2
            cpy     #$20
            bne     LD21C
            lda     #$A6
            bne     LD1E0
LD21C:      cpy     #$40
            bcc     LD1E2
            lda     #$C4
            sta     $F2
            lda     #$FB
            sta     $F3
            jsr     LD22F
            lda     #$E2
            sta     $F2
LD22F:      jsr     LD106
            lda     #$04
            sta     $F8
LD236:      jsr     LD24D
            lda     $F2
            clc
            adc     #$07
            sta     $F2
            dec     $F8
            bne     LD236
            jsr     LD13E
            jsr     LD260
            jmp     LD127

LD24D:      jsr     LD13E
            jsr     LD256
            jsr     LD260
LD256:      ldy     #$06
            lda     #$FF
LD25A:      sta     ($F4),y
            dey
            bpl     LD25A
            rts

LD260:      ldy     #$00
            ldx     #$0D
            bne     LD269
LD266:      jsr     LD149
LD269:      lda     ($F4),y
            ora     #$80
            sta     ($F4),y
            dex
            bne     LD266
            rts

IODRIVER_SIZE = * - LC5B0


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
