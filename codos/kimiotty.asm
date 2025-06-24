

            .include "kim1.inc"

            .importzp DESTBUFF, MEMBUFF, MEMCOUNT, P0SCRATCH, PCSAVE
            .importzp TMPBUFP, TMPPTR, BYTRES


            .segment "ioscratch0" : zeropage

; $F0 - $FF : Zero-page Scratch RAM for console I-0
;
            .exportzp QLN

QLN:        .res    2               ; $F0 Ptr to line-buffer used for INLINE and EDLINE 
TPTR:       .res    2               ; Temporary pointer     
UNKNWN6:    .res    2               ; $F4 - $F5
UNKNWN8:    .res    2               ; $F6 - $F7
UNKNWN10:   .res    2               ; $F8 - $F9
TEMP1:      .res    1               ; $FA Temporary storeage for keyboard routine
UNKNWN13:   .res    1               ; $FB
UNKNWN14:   .res    1               ; $FC
NUMCHRS:    .res    1               ; $FD Number of chars in input buffer
UNKNWN16:   .res    1               ; $FE
UNKNWN17:   .res    1               ; $FF


            .segment "ioscratch"

; Scratch ram used by Console I-O and graphics drivers 
;
ASVKB:      .res    1               ; Saved A reg
XSVKB:      .res    1               ; Saved X reg
YSVKB:      .res    1               ; Saved Y reg
CURPOS:     .res    1               ; Cursor position in line buffer


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

            .export LSTKEY, YLNLIM, BSPACE, CANCEL

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
UNK16:      .byte   $00             ; $021D
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
BSPACE:     .byte   $08             ; $022B CTRL-H
UNK20:      .byte   $09             ; $022C
UNK21:      .byte   $0C             ; $022D
CANCEL:     .byte   $18             ; $022E CTRL-X
QEXCC:      .word   ERR37           ; $022F ADDRESS OF EXTERNAL CONTROL CHARACTER PROCESSOR IF USED.
QEXFNT:     .word   ERR37           ; $0231 ADDRESS OF EXTERNAL FONT TABLE IF USED.
QEXHI7:     .word   ERR37           ; $0233    ADDRESS OF EXTERNAL PROCESSOR FOR CHARACTERS WITH BIT 7=1
FNTTBL:     .word   $0000           ; $0235 CHARACTER FONT TABLE
EXFTBK:     .byte   $00             ; $0237    MEMORY BANK NUMBER CONTAINING EXTERNAL FONT TABLE.
YLNLIM:     .byte   $C0             ; $0238 LINE SIZE LIMIT FOR INLINE AND ENDLINE ENTRY POINTS
NOLEKO:     .byte   $00             ; $0239    ECHO FLAG NORMALLY 0 BUT IF SET TO 80 WILL DISABLE KEYBOARD ECHO
UKINLN:     .byte   $00             ; $023A IF BIT 7=1 THEN IRRECOGNIZED KEYS ARE ACCEPTED FOR ENTRY POINTS INLINE AND ENDLINE.
SPKTBL:     .word   $0000           ; $023B KEYBOARD SPECIAL KEYS TABLE
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
INITIO:     jmp     _INITIO     ; Set default values of display parameters
CLRDSP:     jmp     ERR37       ; Clear the entire 480 by 256 screen
DRWLEG:     jmp     ERR37       ; Draw legends
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
INITTW:     jmp     ERR37       ; INITTW - INITIALIZE THE TEXT WINDOW TO 24 LINES and CLEAR THE TEXT WINDOW ONLY
DEFTW:      jmp     ERR37       ; DEFTW - SET THE POSITION and SIZE OF THE TEXT WINDOW
CLRHTW:     jmp     _CLRHTW     ; CLRHTW - CLEAR THE TEXT WINDOW and HOME THE CURSOR.
HOMETW:     jmp     _HOMETW     ; HOMETW - PLACE THE CURSOR IN THE HOME POSITION (COL=1, LINE=1)
CRLF:       jmp     ERR37       ; CRLF - MOVE CURSOR TO THE LEFT SCREEN EDGE and DOWN ONE LINE
CLRTW:      jmp     _CLRTW      ; CLRTW - CLEAR THE TEXT WINDOW WITHOUT MOVING THE CURSOR
CLRLEG:     jmp     ERR37       ; CLRLEG - CLEAR THE LEGEND DISPLAY AREA (BOTTOMMOST 16 SCAN LINES)
CLRTLN:     jmp     ERR37       ; CLRTLN - CLEAR A SPECIFIED TEXT LINE
LINEFD:     jmp     ERR37       ; LINEFD - MOVE CURSOR DOWN  ONE TEXT LINE
OFFTCR:     jmp     ERR37       ; OFFTCR - TURN THE TEXT CURSOR OFF IF IT IS ON
ONTCR:      jmp     ERR37       ; ONTCR - TURN THE TEXT CURSOR ON
FLPTCR:     jmp     ERR37       ; FLPTCR - FLIP THE VIDEO SENSE OF THE CURSOR AT THE CURSOR POSITION

TIOON:      jmp     ERR37       ; SYSTEM ROUTINE TO FORCE I/O SELECTION
IORES:      jmp     ERR37       ; SYSTEM ROUTINE TO RESTORE I-O/RAM SETTING
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
            .addr   _INITIO         ; Load address
            .word   IODRIVER_SIZE   ; Memory image size

; INITIO - Clear screen and set default values of display parameters
;
.proc _INITIO
            lda     #<__INPLBUF     ; Inits input line buffer
            sta     QLN             ;
            lda     #>__INPLBUF     ;
            sta     QLN+1           ;
            lda     #$00            ; Clear last key pressed
            sta     LSTKEY          ;

            jsr     _CLRHTW         ; Clear the text window and homes cursor
            
            rts
.endproc


; SUBROUTINE _GETKEY: WAIT FOR KEYBOARD KEY DEPRESSION, RETURN
; ASCII CODE FOR KEY.
;
; ON ENTRY: NO ARGUMENTS.
;
; ON RETURN: A = ASCII CODE FOR DEPRESSED KEY (OR SPECIAL KEY
; CODE FOR NON-ASCII KEYS); X, Y PRESERVED.
;
; NOTE: THE ENTRY POINT FOR "_GETKEY" IS IN THE JUMP TABLE.
;
.proc _GETKEY
            lda     LSTKEY          ; Is there a pending key?
            bne     RETLAST

            bit     KBECHO          ; Test "KEYBOARD ECHO" flag
            bmi     INCH            ; Skip supress echo if echo wanted 

            lda     SBD             ; Supress echo
            and     #$FE
            sta     SBD

INCH:       jsr     TTYBGETCH

            bit     KBECHO          ; Test "KEYBOARD ECHO" flag
            bmi     RETLAST         ; Skip restore echo if set 

            pha                     ; Restore echo
            lda     SBD
            ora     #$01
            sta     SBD
            pla

RETLAST:    stx     XSVKB           ; Preserve X
            ldx     #0
            stx     LSTKEY
            ldx     XSVKB

            rts
.endproc

; SUBROUTINE IFKEY: TEST KEY WITHOUT ROLLOVER
;
; THIS ROUTINE IS SIMILAR TO TSTKEY BELOW IN ALL RESPECTS EXCEPT
; THAT IF THE PREVIOUS KEY IS STILL DOWN, IT IS RETURNED WITH
; THE CARRY FLAG SET.
;
.proc _IFKEY
            lda     #$00
            sta     LSTKEY          ; Clear last key
            ; Fall through
.endproc

; SUBROUTINE TSTKEY: TEST FOR KEY DEPRESSION (WITH ROLLOVER).
;
; ON ENTRY: NO ARGUMENTS.
;
; ON RETURN: CY FLAG IS "KEY IS PRESSED" FLAG (CY CLEAR IF NO
; KEY IS DOWN OR IF THE SAME KEY IS STILL DOWN); IF CY SET, THEN
; CHARACTER IN A; X, Y PRESERVED.
;
; NOTE: ENTRY POINT "TSTKEY" IS IN THE JUMP TABLE.
;
.proc _TSTKEY
            ; No need to save registers here, TTYNBGETCH preserves them

            jsr     TTYNBGETCH      ; Get key, non blocking
            beq     RETCLC          ; No key, return carry clear
            cmp     LSTKEY
            beq     RETCLC          ; Same key? Return carry clear

            sta     LSTKEY          ; Save key and return with Cy set
            sec
            rts

RETCLC:     clc
            rts
       
.endproc

; Local subroutine TTYBGETCH
;
; Blocking tty read. Wait until char ready, then
; put it into A
;
.proc TTYBGETCH
            lda     #1
LOOP:       bit     SAD             ; Check start bit
            bne     NOKEY           ; Nokey
            bmi     LOOP            ; Wait until ready
            bpl     TTYGETCH        ; Always jump
NOKEY:      lda #0
            rts
.endproc

; Local subroutine TTYNBGETCH
;
; Non-blocking tty read. If there is a char ready,
; put it into A. Otherwise, return 0x00. Adapted
; from the KIM-1 ROM
;
.proc TTYNBGETCH
            lda     #1
            bit     SAD             ; Check start bit
            bne     TTYBGETCH::NOKEY
            bmi     TTYBGETCH::NOKEY
            ; Fall through
.endproc

.proc TTYGETCH
            stx     XSVKB
            sty     YSVKB

            ldx     #$08            ; Set up 8 bit count
            lda     #$01
            jsr     DELAY           ; Delay 1 bit
            jsr     DEHALF          ; Delay 1/2 bit time
LOOP:       lda     SAD             ; Get 8 bits loop
            and     #$80            ; Mask off low order bits
            lsr     CHAR            ; Shift bit into CHAR
            ora     CHAR
            sta     CHAR
            jsr     DELAY
            dex
            bne     LOOP

            jsr     DEHALF
            lda     CHAR
            rol     a               ; Shift off parity
            lsr     a

            ldx     XSVKB
            ldy     YSVKB

            rts
.endproc


; Make an audible beep
;
.proc _BEEP
            lda     #$07
            ; Fall through
.endproc


; OUTCH - Display character
;
.proc _OUTCH
            sta     ASVKB
            stx     XSVKB
            sty     YSVKB

            cmp     #$0D            ; If CR, echo an additional LF
            bne     SKIP
            jsr     KOUTCH
            lda     #$0A
SKIP:       jsr     KOUTCH

            ldx     XSVKB
            ldy     YSVKB
            lda     ASVKB

            rts
.endproc


;  CLRTW - CLEAR THE TEXT WINDOW WITHOUT MOVING THE CURSOR
;
.proc _CLRTW

            jsr     OUTSEQ
            .byte   $1B, "[2J", $00
            rts
.endproc


; CLRHTW - CLEAR THE TEXT WINDOW and HOME THE CURSOR
;
.proc _CLRHTW
            jsr     _CLRTW
            ; Fall through
.endproc


; HOMETW - PLACE THE CURSOR IN THE HOME POSITION (COL=1, LINE=1)
.proc _HOMETW
            jsr     OUTSEQ
            .byte   $1B, "[2H", $00
            rts
.endproc


; Internal procedure: print sequence of characters immediately following the subroutine call
; (Ripped-off from OUTSTR)
;
.proc OUTSEQ

            pla                     ; Get PC and save in TMPPTR. PC points
            sta     TPTR            ; to last opcode of instruction
            pla                     ;
            sta     TPTR+1          ;
NEXT:       inc     TPTR            ; Increment PC (points to first char of string)
            bne     GETC            ;
            inc     TPTR+1          ;
GETC:       ldy     #$00            ;
            lda     (TPTR),y        ; Get char
            beq     FINISH          ; If null, end of sequence
            jsr     _OUTCH          ; Print char
            jmp     NEXT            ; Loop

FINISH:     lda     TPTR+1          ; Push new PC to the stack
            pha                     ;
            lda     TPTR            ;
            pha                     ;
            rts                     ;
.endproc

; INLINE - input an entire line from the keyboard
;
; Arguments: None (QLN must be set)
;
; Arguments returned: A = number of characters in the line, Y = 0, X preserved
;                     QLN points to the completed line
;                     Cy clear
;
.proc _INLINE
            ldy     #$00            ; Init character count
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
UPDSCRN:    jsr     OUTLBUF         ; Output line buffer to screen

GKLOOP:     jsr     _GETKEY         ; Get key
            cmp     #$7F            ; Printable?
            bcs     NONPRNT         ; Nope
            cmp     #' '            ; Maybe...
            bcc     CHKSPCL         ; Character below space? Go check special keys

KNORMAL:    jsr     NORMALKEY       ; Normal character key
            jmp     GKLOOP          ; And continue processing the input line

CHKSPCL:    cmp     BSPACE          ; BS?
            bne     CHKCR           ; No, check if CR
            cpy     #$00            ; Beginning of line?
            bne     DECCNT          ; No, continue
            jsr     RNGBEL          ; Yes, ring the bell
            jmp     GKLOOP          ; And continue processing the input line

DECCNT:     dey                     ; Decrement count
            jsr     _OUTCH          ; Move cursor left and delete char
            lda     #' '            ;
            jsr     _OUTCH          ;
            lda     #$08            ;
            jsr     _OUTCH          ;
            jmp     GKLOOP          ; Continue processing the input line

CHKCR:      cmp     #$0D            ; Is it CR?
            bne     NONPRNT         ; Nope, go process non-prontable
            sta     (QLN),y         ; Save char into input line buffer
            jsr     CHARECHO
            tya                     ;    return number of characters in A
            ldy     #$00            ;    and make Y = 0
            rts

NONPRNT:    bit     UKINLN          ; Unrecognized keys allowed?
            bmi     KNORMAL         ; Yes, continue as normal key
            bpl     GKLOOP          ; No, continue processing the input line
            ; Not reached
.endproc

; Internal procedure - Process normal key
;
.proc NORMALKEY
            cpy     YLNLIM          ; Reached line limit?
            bcc     CONT            ; No, continue
            jmp     RNGBEL          ; Yes, ring the bell and return

CONT:       sta     (QLN),y         ; Save char into input line buffer
            iny                     ; and increment character count
            ; Fall through
.endproc

; Internal procedure. Echo char depending on NOLEKO flag
;
.proc CHARECHO
            bit     NOLEKO          ; Check keyboard echo flag
            bpl     ECHO            ; If echo, skip
            lda     #' '            ; Advance cursor to the right and return
ECHO:       jmp     _OUTCH          ; Output the character and return  
.endproc

; Local procedure: Ring the bell
;
; Preserves registers
;
.proc RNGBEL
            bit     NOBELL          ; Check if bell is on
            bmi     DONE            ; No, just return
            pha                     ; Save A onto the stack
            txa                     ; Save X
            pha                     ;
            tya                     ; Save Y
            pha                     ;
            jsr     _BEEP           ; Make sound
            pla                     ; Restore Y
            tay                     ;
            pla                     ; Restore X
            tax                     ;
            pla                     ; Restore A
DONE:     rts
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

; Internal procedure - Output char to screen if echo is not disabled
;
.proc OUTCIFEON
            bit     NOLEKO          ; Check keyboard echo flag
            bmi     NOECHO          ; If no echo, skip
            jmp     OUTCH           ; Echo on, output the character
NOECHO:     rts
            ; jmp     CURSORR         ; Advance cursor to the right and return
.endproc

IODRIVER_SIZE = * - _INITIO

            .segment "chartbl"

            .end