; da65 V2.18 - Ubuntu 2.19-1
; Created:    2025-06-11 10:53:02
; Input file: graphdriver2.bin
; Page:       1


            .setcpu "6502"

            .include "symbols.inc"
            .include "monomeg.inc"

            .segment "parameters"

            ; Arguments to the graphics routines 
            ;
XC:         .res    2               ; ($0202) - X coordinate of the graphic cursor pos.
YC:         .res    2               ; ($0204) - Y coordinate of the graphic cursor pos.
XX:         .res    2               ; ($0206) - X graphic coordinate "register"
YY:         .res    2               ; ($0208) - Y graphic coordinate "register"
GMODE:      .res    1               ; ($020A) - Graphic drawing mode, $00=move, $40=erase,
                                    ;           $80=draw, $C0=flip. Add $20 for dashed lines
DSHPAT:     .res    2               ; ($020B) - Recirculating dashed line pattern,
                                    ;           each 1 bit=dot on

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

JMPTBL:     jmp     _SDRAW          ; DRAW A SOLID VECTOR FROM THE CURSOR TO (XX,YY)
            jmp     _SMOVE          ; MOVE GRAPHIC CURSOR TO (XX,YY) WITHOUT DRAWING
            jmp     _SDRAWR         ; DRAW A SOLID WHITE VECTOR RELATIVE TO THE CURSOR
            jmp     _SMOVER         ; MOVE THE GRAPHIC CURSOR RELATIVE TO ITS PRESENT POSITION
            jmp     _SVEC           ; DRAW A VECTOR FROM THE CURSOR TO (XX,YY) ACCORDING TO GMODE AND DSHPAT
            jmp     _SVECR          ; DRAW A VECTOR RELATIVE TO THE CURSOR ACCORDING TO GMODE AND DSHPAT
            jmp     _SDOT           ; DRAW A SINGLE DOT (PIXEL) AT (XX,YY) ACCORDING TO GMODE
            jmp     _SDOTR          ; DRAW A SINGLE DOT (PIXEL) AT A POSITION RELATIVE TO THE CURSOR ACCORDING TO GMODE
            jmp     _SGRIN          ; ALLOW USER COORDINATE INPUT BY MANEUVERING A CURSOR WITH THE KEYBOAR CURSOR CONTROL KEYS
            jmp     _SLTPEN         ; ACTIVATE LIGHT PEN FFOR ONE FRAME AND RETURN COORDINATES OF HIT, IF ANY
            jmp     ERR37
            jmp     _SDRWCH         ; DRAW A SINGLE CHARACTER AT (XX,YY)
            jmp     _SISDOT         ; DETERMINE PIXEL AT (XX,YY) IS ON OR OFF
            jmp     _SOFFGC         ; TTURN OFF THE GRAPHIC CROSSHAIR CURSOR
            jmp     _SONGC          ; TURN ON THE GRAPHIC CROSSHAIR CURSOR
            jmp     _SINTLP         ; WAIT FOR END OF FRAME AND THEN ACTIVATE THE LIGHT PEN
            jmp     _STSTLP         ; TEST FOR LIGHT PEN HIT AND RETURN COORDINATES IF A HIT

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

; SMOVER - Move the graphic cursor relative to its present position
;
; ARGUMENTS: XC,YC = coordinates of initial position graphic cursor
;            X     = Signed X displacement of final position from initial positon
;            Y     = Signed Y displacement of final position from initial position
;
; ARGUMENTS RETURNED: XC,YC and XX,YY are set to the absolute coordinates of the
;                         final position
;                     A, X, and Y are preserved
;
.proc _SMOVER
            pha
            jsr     REL2ABS         ; Calculate XX,YY from XC,YC and X,Y
            pla
            ; Fall through
.endproc

; SMOVE - Move the graphic cursor to (XX,YY) without drawing
;
; ARGUMENTS: XX,YY = coordinates of point to move to.
;            X     = Signed X displacement of final position from initial positon
;            Y     = Signed Y displacement of final position from initial position
;
; ARGUMENTS RETURNED: XC,YC contain a copy of XX,YY
;                     A, X, and Y are preserved
;
.proc _SMOVE
            pha                     ; Preserve A and X
            txa                     ;
            pha                     ;
            jsr     MOVECSR         ; Move graphic cursor to (XX,YY)
            pla                     ; Restore X and A
            tax                     ;
            pla                     ;
            rts
.endproc

; SDRAWR - Draw a solid white vector relative to the cursor
;
; ARGUMENTS: XC,YC = coordinates of initial endpoint of the vector
;            X     = Signed X displacement of final endpoint from XC (-128 to +127)
;            Y     = Signed Y displacement of final endpoint from YC (-128 to +127)
;
; ARGUMENTS RETURNED: XC,YC and XX,YY are set to the absolute coordinates of the
;                         final endpoint
;                     A, X, and Y are preserved
;
.proc _SDRAWR
            pha
            jsr     REL2ABS         ; Calculate XX,YY from XC,YC and X,Y
            pla
            ; Fall through
.endproc

; SDRAW - Draw a solid vector from the cursor to (XX,YY)
;
; ARGUMENTS: XC,YC = coordinates of initial endpoint 
;            XX,YY = coordinates of final endpoint
;
; ARGUMENTS RETURNED: XC,YC are set to the coordinates of the final endpoint
;                     GMODE is set to $80
;                     A, X, and Y are preserved
;
.proc _SDRAW
            sta     ASVGR           ; Preserve A
            lda     #$80            ; Set GMODE to $80 (draw)
            sta     GMODE           ;
            jmp     DRAWVECT        ; Go to draw the vector
.endproc

; Internal procedure: Calculate absolute dest coordinates from relative coordinates
;
; ARGUMENTS: XC,YC = coordinates of initial position graphic cursor
;            X     = Signed X displacement of final position from initial positon
;            Y     = Signed Y displacement of final position from initial position
;
; RETURNED ARGUMENTS: XX = XC + X
;                     YY = YC + Y
;                     X and Y preserved
.proc REL2ABS
            cld
            lda     #$00            ; Init MSB of XX,YY
            sta     XX+1            ;
            sta     YY+1            ;
            txa                     ; Get X displacement
            bpl     XCALC           ; If positive, skip
            lda     #$FF            ; Negative, init XX MSB with $FF
            sta     XX+1            ;
            txa                     ; Get X displacement again
XCALC:      clc                     ; Clear carry for addition
            adc     XC              ; Add displacement to XC
            sta     XX              ; And store into XX
            lda     XC+1            ;
            adc     XX+1            ;
            sta     XX+1            ;
            tya                     ; Get Y displacement
            bpl     YCALC           ; If positive, skip
            lda     #$FF            ; Negative, init YY MSB with $FF
            sta     YY+1            ;
            tya                     ; Get Y displacement again
YCALC:      clc                     ; Clear carry for addition
            adc     YC              ; Add displacement to YC
            sta     YY              ; And store into YY
            lda     YC+1            ;
            adc     YY+1            ;
            sta     YY+1            ;
            rts
.endproc

; SVECR - Draw a vector relative to the cursor according to GMODE and DSHPAT
;
; ARGUMENTS: XC,YC = coordinates of initial endpoint of the vector 
;            X = Signed X displacement of final endpoint from XC
;            Y = Signed Y displacement of final endpoint from YC
;            GMODE = Type of line to be drawn
;            DSHPAT = Dashing pattern if GMODE specifies a dashed line
;
; ARGUMENTS RETURNED: XC,YC and XX,YY are set to the absolute coordinates of the
;                         final endpoint
;                     A, X, and Y are preserved
;
.proc _SVECR
            pha
            jsr     REL2ABS         ; Calculate XX,YY from XC,YC and X,Y
            pla
            ; Fall through
.endproc

; SVEC - Draw a vector from the cursor to (XX,YY) according to GMODE and DSHPAT
;
; ARGUMENTS: XC,YC = coordinates of initial endpoint 
;            XX,YY = coordinates of final endpoint
;            GMODE = Type of line to be drawn
;            DSHPAT = Dashing pattern if GMODE specifies a dashed line
;
; ARGUMENTS RETURNED: XC,YC are set to the coordinates of the final endpoint
;                     A, X, and Y are preserved
;
.proc _SVEC
            sta     ASVGR
            ; Fall through
.endproc

; Internal procedure: Draw a vector. Same arguments as above, assumes that A
;                     was preserved in ASVGR
;
.proc DRAWVECT
            stx     XSVGR
            sty     YSVGR
            bit     GMODE
            bmi     LC079
            bvs     LC079
            jsr     VALXXYY
            jsr     MOVECSR         ; Move graphic cursor to (XX,YY)
            jmp     REGSRET         ; Restore registers and return

LC079:      jsr     VALXCYC
            jsr     VALXXYY
            ldx     #$03
            stx     $02D1
            lda     GMODE
            asl     a
            asl     a
            sta     $02D2
LC08C:      lda     XX
            sec
            sbc     XC
            sta     CHARFNTP
            lda     XX+1
            sbc     XC+1
            bcs     LC0B2
LC09D:      ldy     XX,x
            lda     XC,x
            sta     XX,x
            tya
            sta     XC,x
            dex
            bpl     LC09D
            stx     $02D1
            bmi     LC08C
LC0B2:      sta     CHARFNTP+1
            jsr     PIXLADDR        ; Gets address in video memory of (XC,YC)
            jsr     SWTBANK1        ; Turns on I-O address space and switches to bank 1
            jsr     LC1AC
            ldx     #$80
            lda     YY
            sec
            sbc     YC
            bcs     LC0D1
            ldx     #$00
            lda     YC
            sec
            sbc     YY
LC0D1:      stx     IOTEMP2
            bne     LC0D8
            jmp     LC224

LC0D8:      sta     IOTEMP1
            lda     CHARFNTP+1
            bne     LC0E6
            lda     CHARFNTP
            tax
            bne     LC13E
            jmp     LC209

LC0E6:      lda     CHARFNTP
            tax
            sec
            sbc     IOTEMP1
            sta     VRAMDST
            lda     CHARFNTP+1
            sbc     #$00
            sta     VRAMDST+1
            lda     CHARFNTP+1
            lsr     a
            sta     VRAMCNT+1
            txa
            ror     a
            sta     VRAMCNT
LC0FD:      lda     VRAMCNT+1
            bne     LC112
            lda     VRAMCNT
            sec
            sbc     IOTEMP1
            bcc     LC11F
            beq     LC11E
LC10A:      sta     VRAMCNT
            jsr     LC1DE
            jmp     LC12E

LC112:      lda     VRAMCNT
            sec
            sbc     IOTEMP1
            bcs     LC10A
            dec     VRAMCNT+1
            jmp     LC10A

LC11E:      clc
LC11F:      lda     VRAMCNT
            adc     VRAMDST
            sta     VRAMCNT
            lda     VRAMCNT+1
            adc     VRAMDST+1
            sta     VRAMCNT+1
            jsr     LC17E
LC12E:      cpx     #$00
            bne     LC134
            dec     CHARFNTP+1
LC134:      dex
            bne     LC0FD
            dec     CHARFNTP+1
            bpl     LC0FD
            jmp     LC269

LC13E:      sec
            sbc     IOTEMP1
            bcs     LC154
            lda     IOTEMP1
            stx     IOTEMP1
            sta     CHARFNTP
            tax
            lda     IOTEMP2
            ora     #$40
            sta     IOTEMP2
            txa
            sec
            sbc     IOTEMP1
LC154:      sta     VRAMDST
            txa
            lsr     a
            sta     VRAMCNT
LC15A:      lda     VRAMCNT
            sec
            sbc     IOTEMP1
            bcc     LC16F
            beq     LC16E
            sta     VRAMCNT
            jsr     LC1DE
            dex
            bne     LC15A
            jmp     LC269

LC16E:      clc
LC16F:      lda     VRAMCNT
            adc     VRAMDST
            sta     VRAMCNT
            jsr     LC17E
            dex
            bne     LC15A
            jmp     LC269

LC17E:      bit     IOTEMP2
            bmi     LC18F
            lda     VRAMORG
            clc
            adc     #$3C
            sta     VRAMORG
            bcc     LC19A
            inc     VRAMORG+1
            bcs     LC19A
LC18F:      lda     VRAMORG
            sec
            sbc     #$3C
            sta     VRAMORG
            bcs     LC19A
            dec     VRAMORG+1
            ; Fall through
.endproc

.proc LC19A
            inc     BITDISPL
            ldy     BITDISPL
            cpy     #$08
            bcc     LC1AC
            ldy     #$00
            sty     BITDISPL
            inc     VRAMORG
            bne     LC1AC
            inc     VRAMORG+1
            ; Fall through
.endproc

.proc LC1AC
            bit     $02D2
            bpl     LC1C1
            asl     DSHPAT+1
            rol     DSHPAT
            bcc     LC1DD
            lda     #$01
            ora     DSHPAT+1
            sta     DSHPAT+1
LC1C1:      lda     DISPTBL,y
            ldy     #$00
            bit     GMODE
            bvs     LC1D0
            ora     (VRAMORG),y
            sta     (VRAMORG),y
            rts

LC1D0:      bmi     LC1D9
            eor     #$FF
            and     (VRAMORG),y
            sta     (VRAMORG),y
            rts

LC1D9:      eor     (VRAMORG),y
            sta     (VRAMORG),y
LC1DD:      rts
.endproc

.proc LC1DE
            lda     IOTEMP2
            asl     a
            bpl     LC19A
            bcc     LC1F3
            ; Fall through
.endproc

.proc LC1E5
            ldy     BITDISPL
            lda     VRAMORG
            sbc     #$3C
            sta     VRAMORG
            bcs     LC1AC
            dec     VRAMORG+1
            bcc     LC1AC
            ; Fall through
.endproc

.proc LC1F3
            ldy     BITDISPL
            lda     VRAMORG
            adc     #$3C
            sta     VRAMORG
            bcc     LC1AC
            inc     VRAMORG+1
            bcs     LC1AC           ; Always jump
            ; Not reached
.endproc

            ; This table contains byte masks for the bit displacements of pixels
            ;
DISPTBL:    .byte   $80, $40, $20, $10, $08, $04, $02, $01

.proc LC209
            ldx     IOTEMP1
            beq     LC269
            bit     IOTEMP2
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
            ; Fall through
.endproc

.proc LC224
            ldx     CHARFNTP
            beq     LC234
LC228:      ldy     BITDISPL
            cpy     #$07
            beq     LC23B
LC22E:      jsr     LC19A
            dex
            bne     LC228
LC234:      dec     CHARFNTP+1
            bpl     LC228
            jmp     LC269

LC23B:      cpx     #$08
            bcc     LC22E
            bit     $02D2
            bmi     LC22E
            inc     VRAMORG
            bne     LC24A
            inc     VRAMORG+1
LC24A:      ldy     #$00
            lda     #$FF
            bit     GMODE
            bvs     LC25E
LC253:      sta     (VRAMORG),y
            txa
            sec
            sbc     #$08
            tax
            bne     LC23B
            beq     LC234
LC25E:      bmi     LC264
            lda     #$00
            beq     LC253
LC264:      eor     (VRAMORG),y
            jmp     LC253
.endproc

.proc LC269
            jsr     RSTBANK0        ; Restores Bank 0 and turns on RAM at BE00-BFFF
            ldx     #$03
            bit     $02D1
            bmi     LC27E
LC273:      lda     XX,x
            sta     XC,x
            dex
            bpl     LC273
            bmi     REGSRET         ; Restore registers and return
LC27E:      lda     XC,x
            sta     XX,x
            dex
            bpl     LC27E
            ; Fall through
.endproc

; Restore registers and return
;
.proc REGSRET
            ldy     YSVGR
            lda     ASVGR
            ldx     XSVGR
            rts
.endproc

; SDOTR - Draw a single dot (pixel) at a position relative to the cursor according
;         to GMODE
;
; ARGUMENTS: XC,YC = Present cursor position 
;            X     = Offset of point from cursor position in X direction
;            Y     = Offset of point from cursor position in Y direction
;            GMODE = Type of line to be drawn
;
; ARGUMENTS RETURNED: XC,YC and XX,YY are set to the coordinates of the dot
;                     A, X, and Y are preserved
;
.proc _SDOTR
            pha
            jsr     REL2ABS         ; Calculate XX,YY from XC,YC and X,Y
            pla
            ; Fall through
.endproc

; SDOT - Draw a single dot (pixel) at (XX,YY) according to GMODE
;
; ARGUMENTS: XX,YY = coordinates of dot to draw
;            GMODE = Type of line to be drawn
;
; ARGUMENTS RETURNED: XC,YC are set to the coordinates of the dot
;                     A, X, and Y are preserved
;
.proc _SDOT
            sta     ASVGR           ; Preserve registers
            stx     XSVGR           ;
            sty     YSVGR           ;
            jsr     VALXXYY         ; Validate and correct XX,YY coordinates
            jsr     MOVECSR         ; Move graphic cursor to (XX,YY)
            bit     GMODE           ; Check graphic mode
            bmi     LC2AC           ; Draw or flip
            bvc     RETURN          ; Not erase (so no valid mode)
LC2AC:      lsr     $02D2
            jsr     PIXLADDR        ; Gets address in video memory of (XC,YC)
            jsr     SWTBANK1        ; Turns on I-O address space and switches to bank 1
            jsr     LC1AC
            jsr     RSTBANK0        ; Restores Bank 0 and turns on RAM at BE00-BFFF
RETURN:     jmp     REGSRET         ; Restore registers and return
.endproc

; Internal procedure: Gets address in video memory of graphical cursor coords (XC,YC)
;
; ARGUMENTS: XC,YC - Graphical cursor coordinates
;
; ARGUMENTS RETURNED: VRAMORG - Address of pixel byte
;                     BITDISPL - Displacement of pixel in byte
;                     Y = BITDISPL
;
.proc PIXLADDR
            lda     YC
            jsr     NTLINSTART      ; Invert raster line num and calculates start address
            ; Fall through
.endproc

; Internal procedure: Gets address in video memory of a pixel in a raster line
;
; ARGUMENTS: XC - X coordinate of the graphical cursor
;            VRAMDST - Start address of the raster line
;
; ARGUMENTS RETURNED: VRAMORG - Address of pixel byte
;                     BITDISPL - Displacement of pixel in byte
;                     Y = BITDISPL
;                     
.proc HPIXLADDR
            lda     XC+1            ; Divide XC by 8 (8 pixels in each byte)
            lsr     a               ; XC+1 is 0 or 1, so only lsr once
            lda     XC              ;
            ror     a               ;
            lsr     a               ;
            lsr     a               ;
            clc                     ; Clear carry for addition
            adc     VRAMDST         ; Add to start of raster line
            sta     VRAMORG         ; And store
            lda     VRAMDST+1       ;
            adc     #$00            ;
            sta     VRAMORG+1       ;
            lda     XC              ; Get XC coordinate again
            and     #$07            ; And calculate modulo 8 for displacement
            sta     BITDISPL        ;
            tay                     ; Displacement into Y
            rts
.endproc

; Internal procedure: Validate that XC and YC coordinates are valid <($1E0,$100)
;
; ARGUMENTS: XC,YC - Graphical coordinates to validate
;
; RETURNED ARGUMENTS: (XC,YC) updated
;                     X and Y preserved
;
.proc VALXCYC
            stx     XSVGR2          ; Preserve X
            ldx     #$00            ; Offset to XC
            ; Fall through
.endproc

; Internal procedure: Validate that X and Y coordinates are valid <($1E0,$100)
;
; ARGUMENTS: X = Offset to coordinate set:
;               0 - Validate XC,YC
;               4 - Validate XX,YY
;
; RETURNED ARGUMENTS: (XC,YC) or (XX,YY) updated
;
.proc VALCOORD
            cld                     ; Clear decimal mode
            lda     XC,x            ; Get X coordinate
            cmp     #<VIDHRES       ; Compare to horizontal resolution
                                    ; Sets Cy = 1 if XC >= $E0
            lda     XC+1,x          ;
            bmi     ZEROX           ; Negative, go zero it
            sbc     #>VIDHRES       ;
            bcc     CHKYC           ; X coord within limits, go check Y coord
            lda     #(<VIDHRES)-1   ; Set X coord to the maximum
            sta     XC,x            ;
            lda     #>VIDHRES       ;
            sta     XC+1,x          ;
            bne     CHKYC           ; Always jump
            ; Not reached
ZEROX:      lda     #$00            ; Set X coord to zero
            sta     XC,x            ;
            sta     XC+1,x          ;
CHKYC:      lda     YC+1,x          ; Get MSB of Y coord
            beq     RETURN          ; If zero, it is within limits
            bmi     ZEROY           ; If negative, go zero it
            lda     #$FF            ; If not, set Y coordinate to maximum
            sta     YC,x            ;
            bne     ZEROYH          ; Always jump
            ; Not reached
ZEROY:      lda     #$00            ; Set Y coordinate to zero
            sta     YC,x            ;
ZEROYH:     lda     #$00            ;
            sta     YC+1,x          ;
RETURN:     ldx     XSVGR2          ; Restore X
            rts
.endproc

; Internal procedure: Validate that XX and YY coordinates are valid <($1E0,$100)
;
; ARGUMENTS: XX,YY - Graphical coordinates to validate
;
; RETURNED ARGUMENTS: (XX,YY) updated
;                     X and Y preserved
;
.proc VALXXYY
            stx     XSVGR2          ; Preserve X
            ldx     #$04            ; Offset to XX
            bne     VALCOORD        ; Always jump
            ; Not reached
.endproc

; Internal procedure: Move graphic cursor to (XX,YY)
;
; Copy the value of the coordinate "registers" to the graphical cursor coordinates
;
.proc MOVECSR
            ldx     #$03            ; Init index (Four bytes per set)
LOOP:       lda     XX,x            ; Copy
            sta     XC,x            ;
            dex                     ;
            bpl     LOOP            ; Repeat until done
            rts
.endproc

; SDRWCH - Draw a single character at (XX,YY)
;
; ARGUMENTS: XX,YY = Coordinates of lower left corner of 6 by 10 character matrix
;            A     = ASCII character code in range of $20 - $7F
;
; ARGUMENTS RETURNED: XC,YC position of character just drawn (copy of XX,YY)
;                     XX,YY position of next character to draw (XX=XX+6, YY=YY)
;                     A, X, and Y are preserved
;
.proc _SDRWCH
            sta     ASVGR           ; Preserve registers
            stx     XSVGR           ;
            sty     YSVGR           ;
            pha
            jsr     MOVECSR         ; Move graphic cursor to (XX,YY)
            pla
            jsr     PRINTC          ; Print the char
            lda     XX              ; Get current XX position
            clc                     ;
            adc     #$06            ; Advance to next char (chars are 6 pixels wide)
            sta     XX              ;
            bcc     RETURN          ;
            inc     XX+1            ;
RETURN:     jmp     REGSRET         ; Restore registers and return
.endproc

; Internal procedure: Validate char and coordinates. If OK, print char
;
; ARGUMENTS: XX,YY = Coordinates of lower left corner of 6 by 10 character matrix
;            A     = ASCII character code in range of $20 - $7F
;
.proc PRINTC
            cld
            cmp     #$80            ; Check char number
            bcs     RETURN          ; If bigger than $7F, just return
            sec                     ; Clear borrow for substraction
            sbc     #$20            ; Rebase char number from 0 to $5F
            bcc     RETURN          ; Was lower that $20, do nothing
            sta     ASVGR2          ; Store char temporarily
            lda     XC              ; Get X coordinate
            cmp     #<(VIDHRES-6+1) ; Check if past coordinate of last char
            lda     XC+1            ;    
            sbc     #>(VIDHRES-6+1) ;
            bcs     RETURN          ; Yes, do nothing
            lda     YC+1            ; Check if Y coordinate > 255
            bne     RETURN          ; Yes, do nothing
            cmp     #<(VIDVRES-10+1); Check if past coordinate of last char
            bcs     RETURN          ; Yes, do nothing
            jsr     PIXLADDR        ; Gets address in video memory of (XC,YC)
            lda     ASVGR2          ; Recover char
            jsr     PRNCHR          ; And print it
RETURN:     rts
.endproc

; SISDOT - Determine whether pixel at (XX,YY) is on or off
;
; ARGUMENTS: XX,YY = Position of pixel to test
;
; ARGUMENTS RETURNED: A=0 if pixel is off, nonzero if on
;                     XC,YC set equal to XX,YY
;                     X and Y are preserved
;
.proc _SISDOT
            stx     XSVGR           ; Preserve registers
            sty     YSVGR           ;
            jsr     MOVECSR         ; Move graphic cursor to (XX,YY)
            jsr     VALXCYC         ; Validate coordinates
            jsr     PIXLADDR        ; Gets address in video memory of (XC,YC)
                                    ; At this point, Y = BITDISPL
            jsr     SWTBANK1        ; Turns on I-O address space and switches to bank 1
            lda     DISPTBL,y       ; Get mask for bit displacement
            ldy     #$00            ;
            and     (VRAMORG),y     ; Check if pixel is on
            tax                     ; Save A
            jsr     RSTBANK0        ; Restores Bank 0 and turns on RAM at BE00-BFFF
            txa                     ; Restore A
            pha                     ; Save A (Why?)
            ldx     XSVGR           ; Restore X and Y
            ldy     YSVGR           ;
            pla                     ; Recover A (0 if pixel off, non zero if on)
            rts
.endproc

; SGRIN - Allow user coordinate input by maneuvering a cursor with the keyboard
;         cursor control keys. 
;
; ARGUMENTS: XC,YC = Initial position of graphic cursor
;
; ARGUMENTS RETURNED: XX,YY = user selected position of cursor
;                     A = ASCII code of key pressed to terminate the input
;                     XC, YC, X and Y are preserved
;
.proc _SGRIN
            txa
            pha
            tya
            pha
            sec
            ror     NOCLIK
            ldx     #$03
LC3B8:      lda     XC,x
            sta     XX,x
            dex
            bpl     LC3B8
            bmi     LC3C8
LC3C3:      lda     #$00
            sta     LSTKEY
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
            ldy     UNK17
LC3EC:      sty     IOTEMP1
            and     #$03
            beq     LC419
            cmp     #$02
            beq     LC409
            bcs     LC41D
LC3F8:      sec
            lda     XC,x
            sbc     IOTEMP1
            sta     XC,x
            bcs     LC3C3
            dec     XC+1,x
            jmp     LC3C3

LC409:      tya
            clc
            adc     XC,x
            sta     XC,x
            bcc     LC3C3
            inc     XC+1,x
            jmp     LC3C3

LC419:      ldx     #$02
            bne     LC409
LC41D:      ldx     #$02
            bne     LC3F8
LC421:      sta     IOTEMP1
            ldx     #$03
LC425:      lda     XX,x
            tay
            lda     XC,x
            sta     XX,x
            tya
            sta     XC,x
            dex
            bpl     LC425
            asl     NOCLIK
            jsr     CLICK
            pla
            tay
            pla
            tax
            lda     IOTEMP1
            rts
.endproc

; SOFFGC - Turn off the graphic crosshair cursor 
;
; ARGUMENTS: XC,YC = Position of graphic cursor
;
; ARGUMENTS RETURNED: None, A, X, and Y are preserved
;
.proc _SOFFGC
            asl     UNK15
            bcs     LC44D
            rts
.endproc

; SONGC - Turn on the graphic crosshair cursor
;
; ARGUMENTS: XC,YC = Position of graphic cursor
;
; ARGUMENTS RETURNED: None, A, X, and Y are preserved
;
.proc _SONGC
            sec
            ror     UNK15
            ; Fall through
.endproc

.proc LC44D
            pha
            txa
            pha
            tya
            pha
            jsr     VALXCYC
            lda     YC
            jsr     NTLINSTART      ; Invert raster line num and calculates start address
            jsr     SWTBANK1        ; Turns on I-O address space and switches to bank 1
            lda     $BFFC
            and     #$F1
            ora     #$04
            sta     $BFFC
            lda     $BFF1
LC46B:      lda     $BFFD
            and     #$01
            beq     LC46B
            ldy     #$3B
LC474:      lda     (VRAMDST),y
            eor     #$FF
            sta     (VRAMDST),y
            dey
            bpl     LC474
            lda     #$C4
            sta     VRAMDST
            lda     #$FB
            sta     VRAMDST+1
            jsr     HPIXLADDR       ; Gets address of pixel in raster line
            lda     DISPTBL,y
            sta     IOTEMP1
            ldx     #$00
            ldy     #$00
LC491:      lda     (VRAMORG),y
            eor     IOTEMP1
            sta     (VRAMORG),y
            lda     VRAMORG
            sec
            sbc     #$3C
            sta     VRAMORG
            bcs     LC4A2
            dec     VRAMORG+1
LC4A2:      inx
            bne     LC491
            jsr     RSTBANK0        ; Restores Bank 0 and turns on RAM at BE00-BFFF
            pla
            tay
            pla
            tax
            pla
            rts
.endproc

; SINTLP - Wait for end of frame and then activate the light pen
;
; ARGUMENTS: None
;
; ARGUMENTS RETURNED: None, X and Y are preserved
;
.proc _SINTLP
            jsr     _TIOON
            lda     $BFFC
            and     #$F1
            sta     $BFFC
            lda     $BFF1
LC4BC:      lda     $BFFD
            and     #$01
            beq     LC4BC
            sta     $BFC4
            jmp      _IORES
.endproc

; SLTPEN - Activate light pen for one frame and return coordinates of hit, if any
;
; ARGUMENTS: None
;
; ARGUMENTS RETURNED: Cy set if pen saw light, cleared if not
;                     XX,YY = Coordinates of hit, if any
;                     XX, YY, X and Y are preserved only if no hit
;
.proc _SLTPEN
            jsr     _SINTLP
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
.endproc

; STSTLP - Test for light pen hit and return coordinates if a hit
;
; ARGUMENTS: None
;
; ARGUMENTS RETURNED: Cy set if pen saw light previously, cleared if not
;                     XX,YY = Coordinates of hit, if any
;                     XX, YY, X and Y are preserved only if no hit
;
.proc _STSTLP
            jsr     _TIOON
            lda     $BFC0
            and     #$08
            bne     LC4F6
            clc
            rts

LC4F6:      lda     $BFC0
            and     #$07
            sta     YY+1
            lda     $BFC1
            sec
            sbc     #$02
            sta     YY
            lda     $BFC2
            and     #$3F
            sbc     #$00
            ldy     #$08
            asl     YY
LC513:      rol     a
            cmp     #$3C
            bcc     LC51A
            sbc     #$3C
LC51A:      rol     YY
            dey
            bne     LC513
            sta     XX
LC523:      cmp     #$0A
            bcc     LC52B
            sbc     #$0A
            bne     LC523
LC52B:      asl     a
            tax
            bit     $BFC2
            bpl     LC533
            inx
LC533:      ldy     YY+1
            lda     LC578,x
            and     LC5A0,y
            beq     LC541
            inc     XX
LC541:      lda     LC58C,x
            and     LC5A0,y
            beq     LC54C
            dec     XX
LC54C:      lda     #$00
            sta     XX+1
            lda     XX
            asl     a
            asl     a
            asl     a
            rol     XX+1
            ora     YY+1
            sbc     #$05
            sta     XX
            bcs     LC567
            dec     XX+1
LC567:      lda     YY
            eor     #$FF
            sta     YY
            lda     #$00
            sta     YY+1
            sec
            jmp      _IORES
.endproc

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
