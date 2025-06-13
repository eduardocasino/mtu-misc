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
            jmp     _SOFFGC         ; TURN OFF THE GRAPHIC CROSSHAIR CURSOR
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
; NOTE: Uses CHARFNTP as storage for distance between XC and XX
;
.proc DRAWVECT
            stx     XSVGR           ; Preserve X and Y
            sty     YSVGR           ;
            bit     GMODE           ; Chech graphic mode
            bmi     DRAW            ; Draw or flip
            bvs     DRAW            ; Erase
            jsr     VALXXYY         ; Just move. Validate dest coordinates
            jsr     MOVECSR         ; Move graphic cursor to (XX,YY)
            jmp     REGSRET         ; Restore registers and return

DRAW:       jsr     VALXCYC         ; Validate orig
            jsr     VALXXYY         ;  and dest coordinates
            ldx     #$03            ; Init index for coordinate swap
            stx     XHNGED          ; Use also as a flag: If bit 7 = 1, coords are xchanged
            lda     GMODE           ; Get mode
            asl     a               ; Shift the "dashed" flag bit to the msb
            asl     a               ;
            sta     DASHED          ; Store flag
XDIST:      lda     XX              ; Calculate horizontal distance form orig
            sec                     ; to dest coordinates
            sbc     XC              ;
            sta     CHARFNTP        ;   and store into CHARFNTP
            lda     XX+1            ;
            sbc     XC+1            ;
            bcs     CONT            ; If positive or 0, skip coord exchange
XCHNG:      ldy     XX,x            ; Exchange orig and dest X,Y coordinates
            lda     XC,x            ;
            sta     XX,x            ;
            tya                     ;
            sta     XC,x            ;
            dex                     ;
            bpl     XCHNG           ;
            stx     XHNGED          ; X is now $FF, so flag is true
            bmi     XDIST           ; Try again (always jump)
            ; Not reached

CONT:       sta     CHARFNTP+1      ; Finish store horiz distance into CHARFNTP
            jsr     PIXLADDR        ; Gets address in video memory of (XC,YC)
            jsr     SWTBANK1        ; Turns on I-O address space and switches to bank 1
            jsr     PIXEL           ; Draw pixel at XC,YC
            ldx     #$80            ; Flag we are going bottom to top
            lda     YY              ; Get vertical distance in pixels
            sec                     ;
            sbc     YC              ;
            bcs     STDIR           ; If positive or zero, skip
            ldx     #$00            ; Flag we are going top to bottom
            lda     YC              ; Recalculate Y distance from dest to orig
            sec                     ;
            sbc     YY              ;
STDIR:      stx     IOTEMP2         ; Store direction flag
            bne     CKDIAGNL        ; Jump if there is Y displacement
            jmp     HORIZ           ; No Y displacement, draw a horizontal line

CKDIAGNL:   sta     IOTEMP1         ; Store Y distance between orig and dest
            lda     CHARFNTP+1      ; Get horizontal distance MSB
            bne     LC0E6           ; If non-zero, more than one byte apart
            lda     CHARFNTP        ; Get LSB
            tax                     ; Transfer to X
            bne     LC13E           ; If non-zero, less than one byte apart
            jmp     VERTICAL        ; No horizontal displacement, draw vertical line

LC0E6:      lda     CHARFNTP        ; Get horizontal distance
            tax                     ; Save it into X
            sec                     ; Cear borrow for substract
            sbc     IOTEMP1         ; Substract vertical distance between orig and dest
            sta     VRAMDST         ; And store it into VRAMDST
            lda     CHARFNTP+1      ;
            sbc     #$00            ;
            sta     VRAMDST+1       ;
            lda     CHARFNTP+1      ; Divide horizontal distance by 2
            lsr     a               ;
            sta     VRAMCNT+1       ; And store into VRAMCNT
            txa                     ; Recover LSB
            ror     a               ;
            sta     VRAMCNT         ;
LC0FD:      lda     VRAMCNT+1       ;
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
            jmp     UPDCRD          ; Update coordinates and return

LC13E:      sec                     ; Clear borrow for substraction
            sbc     IOTEMP1         ;
            bcs     LC154
            lda     IOTEMP1
            stx     IOTEMP1
            sta     CHARFNTP
            tax
            lda     IOTEMP2
            ora     #$40            ; Set horizotal direction???
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
            jmp     UPDCRD          ; Update coordinates and return

LC16E:      clc
LC16F:      lda     VRAMCNT
            adc     VRAMDST
            sta     VRAMCNT
            jsr     LC17E
            dex
            bne     LC15A
            jmp     UPDCRD          ; Update coordinates and return

LC17E:      bit     IOTEMP2
            bmi     LC18F
            lda     VRAMORG
            clc
            adc     #VIDHRES/8
            sta     VRAMORG
            bcc     NXTPIXEL
            inc     VRAMORG+1
            bcs     NXTPIXEL
LC18F:      lda     VRAMORG
            sec
            sbc     #VIDHRES/8
            sta     VRAMORG
            bcs     NXTPIXEL
            dec     VRAMORG+1
            ; Fall through
.endproc

; Internal procedure: Advance horizontal position and draw pixel
;
; ARGUMENTS: DASHED  = Flag. If bit 7 = 1, follow the dash pattern in DSHPAT
;            GMODE   = Graphics mode
;            VRAMORG = Visual memory address of the pixel 
;            BITDISPL = bit displacement at VRAMORG
;
; ARGUMENTS RETURNED: DSHPAT, VRAMORG and BITDISPL are updated
;
.proc NXTPIXEL
            inc     BITDISPL        ; Increment and get bit displacement
            ldy     BITDISPL        ;
            cpy     #$08            ; Past last bit?
            bcc     PIXEL           ; No, go draw pixel
            ldy     #$00            ; Yes, zero it, increment video address and draw
            sty     BITDISPL        ;
            inc     VRAMORG         ;
            bne     PIXEL           ;
            inc     VRAMORG+1       ;
            ; Fall through
.endproc

; Internal procedure: Draw pixel at VRAMORG + BITDISPL
;
; ARGUMENTS: DASHED  = Flag. If bit 7 = 1, follow the dash pattern in DSHPAT
;            GMODE   = Graphics mode
;            VRAMORG = Visual memory address of the pixel 
;            Y       = BITDISPL (bit displacement at VRAMORG)
;
; ARGUMENTS RETURNED: DSHPAT is updated
;
.proc PIXEL
            bit     DASHED          ; Check if dashed mode
            bpl     CONT            ; Skip if not set
            asl     DSHPAT+1        ; Get bit from pattern
            rol     DSHPAT          ;
            bcc     RETURN          ; If 0, nothing to draw
            lda     #$01            ; Re-inject bit into the pattern
            ora     DSHPAT+1        ;
            sta     DSHPAT+1        ;
CONT:       lda     DISPTBL,y       ; Get mask for bit displacement
            ldy     #$00
            bit     GMODE           ; Check graphics mode
            bvs     ERASE           ; If erase or flip, skip
            ora     (VRAMORG),y     ; Draw it
            sta     (VRAMORG),y     ;
            rts

ERASE:      bmi     FLIP            ; If flip, skip
            eor     #$FF            ; Invert the mask
            and     (VRAMORG),y     ; And erase bit
            sta     (VRAMORG),y     ;
            rts

FLIP:       eor     (VRAMORG),y     ; Flip bit
            sta     (VRAMORG),y     ;
RETURN:     rts
.endproc

.proc LC1DE
            lda     IOTEMP2         ; Clear direction flag (set top to bottom) and
            asl     a               ;    move flag to carry flag
            bpl     NXTPIXEL        ; If set the XXX bit, go right and draw
            bcc     DNPIXEL         ; If not and vert direction was clear, go down and draw
                                    ; If yes, go up and draw
            ; Fall through
.endproc

; Internal procedure: Go up one raster line and draw pixel
;
.proc UPPIXEL
            ldy     BITDISPL        ; Get bit displacement
            lda     VRAMORG         ; Get current position
            sbc     #VIDHRES/8      ; Up one raster line
            sta     VRAMORG         ; Store it
            bcs     PIXEL           ; No borrow, go print pixel
            dec     VRAMORG+1       ; Borrow, decrement MSB
            bcc     PIXEL           ; and print pixel (always jump)
            ; Not reached
.endproc

; Internal procedure: Go down one raster line and draw pixel
;
.proc DNPIXEL
            ldy     BITDISPL        ; Get bit displacement
            lda     VRAMORG         ; Get current position
            adc     #VIDHRES/8      ; Down one raster line
            sta     VRAMORG         ; Store it
            bcc     PIXEL           ; No carry, go print pixel
            inc     VRAMORG+1       ; Carry, increment MSB
            bcs     PIXEL           ; and print pixel (always jump)
            ; Not reached
.endproc

            ; This table contains byte masks for the bit displacements of pixels
            ;
DISPTBL:    .byte   $80, $40, $20, $10, $08, $04, $02, $01

; Internal procedure: Draw an certical line of IOTEMP1 pixels from current
;                     coordinates +/- 1 (depending on direction flag IOTEMP2)
;
.proc VERTICAL
            ldx     IOTEMP1         ; Get vertical distance
            beq     UPDCRD          ; If none, update coordinates and return
            bit     IOTEMP2         ; Check direction flag
            bpl     TOP2BTM         ; Top to bottom

BTM2TOP:    sec                     ; Clear borrow
            jsr     UPPIXEL         ; Go up one raster line and draw pixel
            dex                     ; Decrement count
            bne     BTM2TOP         ; Repeat until done
            jmp     UPDCRD          ; Update coordinates and return

TOP2BTM:    clc                     ; Clear carry
            jsr     DNPIXEL         ; Go down one raster line and draw pixel
            dex                     ; Decrement count
            bne     TOP2BTM         ; Repeat until done
            beq     UPDCRD          ; Update coordinates and return
            ; Not reached
.endproc

; Internal procedure: Draw an horizontal line of CHARFNTP pixels from current
;                     coordinates + 1
;
.proc HORIZ
            ldx     CHARFNTP        ; Get count
            beq     DCMSB           ; If LSB is 0, go decrement MSB
LOOP:       ldy     BITDISPL        ; Get displacement
            cpy     #$07            ; Last bit?
            beq     ATBIT0          ; Yes, check if we can go byte by byte
NEXTP:      jsr     NXTPIXEL        ; Draw next pixel
            dex                     ; Decrement count
            bne     LOOP            ; And repeat until no more
DCMSB:      dec     CHARFNTP+1      ; 
            bpl     LOOP            ;
            jmp     UPDCRD          ; Update coordinates and return

ATBIT0:     cpx     #$08            ; If count < 8   
            bcc     NEXTP           ; Draw next pixel
            bit     DASHED          ; Count >= 8. Dashed line?
            bmi     NEXTP           ; Yes, draw next pixel

            ; Count is > 8 and we are at bit 0 and no dashed mode, so
            ; we can go faster byte by byte

            inc     VRAMORG         ; Increment vram position
            bne     SETBYT          ;
            inc     VRAMORG+1       ;
SETBYT:     ldy     #$00
            lda     #$FF            ; Solid line
            bit     GMODE           ; Check mode
            bvs     ERASE           ; If flip or erase, skip
DRBYT:      sta     (VRAMORG),y     ; Draw entire byte
            txa                     ; Get count
            sec                     ; Clear borrow for substraction
            sbc     #$08            ; Advance one byte
            tax                     ;
            bne     ATBIT0          ; Repeat until no more bytes
            beq     DCMSB           ; Always jump to decrement the count MSB
            ; Not reached

ERASE:      bmi     FLIP            ; If flip, skip
            lda     #$00            ; Set to erase
            beq     DRBYT           ; Always jump to draw byte
            ; Not reached

FLIP:       eor     (VRAMORG),y     ; Get flipped value
            jmp     DRBYT           ; And jump to draw byte
.endproc

; Internal procedure: Update coordinate, restore registers and return
;
.proc UPDCRD
            jsr     RSTBANK0        ; Restores Bank 0 and turns on RAM at BE00-BFFF
            ldx     #$03            ; 4 bytes per register set
            bit     XHNGED          ; Check if orig and dest were exchanged
            bmi     UPDDST          ; Yes, jump to update destinations
UPDORG:     lda     XX,x            ; Set orig with dest value
            sta     XC,x            ;
            dex                     ;
            bpl     UPDORG          ;
            bmi     REGSRET         ; Restore registers and return (always jump)
            ; Not reached

UPDDST:     lda     XC,x            ; Set dest with orig value
            sta     XX,x            ;
            dex                     ;
            bpl     UPDDST          ;
            ; Fall through
.endproc

; Restore registers and return
;
.proc REGSRET
            ldy     YSVGR           ; Restore Y, A, X
            lda     ASVGR           ;
            ldx     XSVGR           ;
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
            bmi     DRAW            ; Draw or flip
            bvc     RETURN          ; Not erase (so no valid mode)
DRAW:       lsr     DASHED          ; Disable dashed mode, if active
            jsr     PIXLADDR        ; Gets address in video memory of (XC,YC)
            jsr     SWTBANK1        ; Turns on I-O address space and switches to bank 1
            jsr     PIXEL           ; Draw pixel
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
            txa                     ; Preserve X and Y
            pha                     ;
            tya                     ;
            pha                     ;
            sec                     ; Disable audible click
            ror     NOCLIK          ;
            ldx     #$03            ; Copy XC,YC to XX,YY
CPYCRD:     lda     XC,x            ;
            sta     XX,x            ;
            dex                     ;
            bpl     CPYCRD          ; Repeat until done
            bmi     WAITKEY         ; Always jump
            ; Not reached

CLRKEY:     lda     #$00            ; Clear last key
            sta     LSTKEY          ;

WAITKEY:    jsr     _SONGC          ; Turn on the graphic crosshair cursor
            jsr     TSTKEY          ; Test if a key is pressed
            php                     ; Preserve flags
            jsr     _SOFFGC         ; Turn off the graphic crosshair cursor
            plp                     ; Restore flags
            bcc     WAITKEY         ; Repeat until key presssed
            ldy     #$01            ; Init cursor mivement step to 1 pixel
            ldx     #$00            ; Init coordinate index to XC
            cmp     #$A0            ; 'CURSOR UP'
            bcc     TRMNT           ; Key is below cursor keys, terminate input
            cmp     #$A4            ; 'HOME'
            bcc     NORMAL          ; Key is one of cursor jeys
            cmp     #$B0            ; 'SHIFT/CURSOR UP'
            bcc     TRMNT           ; Key is betwwen cursor and shift cursor, terminate
            cmp     #$B4            ; 'SHIFT/ HOME
            bcs     TRMNT           ; Key equal or above shift-home, terminate
            ldy     SCSTEP          ; It is a shift+cursor, get step number
NORMAL:     sty     IOTEMP1         ; Store step into IOTEMP1
            and     #$03            ; Mask-out higer bits
            beq     UP              ; 0 = Cursor up
            cmp     #$02            ; 2 = Cursor right
            beq     FRWRD           ; Advance forward (X=0, horizontal move)
            bcs     DOWN            ; 3 = Cursor down 
                                    ; 1 = Cursor left
BACK:       sec                     ; Clear borrow for substraction
            lda     XC,x            ; Get coordinate
            sbc     IOTEMP1         ; Substract step
            sta     XC,x            ; Update it, clear last key and get next
            bcs     CLRKEY          ;
            dec     XC+1,x          ;
            jmp     CLRKEY          ;

FRWRD:      tya                     ; Transfer step to A
            clc                     ; Clear carry for addition
            adc     XC,x            ; Add step to coordinate, clear last key and get next
            sta     XC,x            ;
            bcc     CLRKEY          ;
            inc     XC+1,x          ;
            jmp     CLRKEY          ;

UP:         ldx     #$02            ; Index to vertial coordinates
            bne     FRWRD           ; And advance forward (always jump)
            ; Not reached

DOWN:       ldx     #$02            ; Index to vertical coordinates
            bne     BACK            ; And advance backwards (always jump)
            ; Not reached

TRMNT:      sta     IOTEMP1         ; Save pressed key
            ldx     #$03            ; Exchange XX,YY and XC,YC
XCHGREG:    lda     XX,x            ;
            tay                     ;
            lda     XC,x            ;
            sta     XX,x            ;
            tya                     ;
            sta     XC,x            ;
            dex                     ;
            bpl     XCHGREG         ; Repeat until done
            asl     NOCLIK          ; Enable audible click
            jsr     CLICK           ; And make it
            pla                     ; Restore Y and X
            tay                     ;
            pla                     ;
            tax                     ;
            lda     IOTEMP1         ; Return key in A
            rts
.endproc

; SOFFGC - Turn off the graphic crosshair cursor 
;
; ARGUMENTS: XC,YC = Position of graphic cursor
;
; ARGUMENTS RETURNED: None, A, X, and Y are preserved
;
.proc _SOFFGC
            asl     CRSHAIR         ; Clear crosshair flag
            bcs     DRAWGC          ; If it was activated, delete from screen
            rts
.endproc

; SONGC - Turn on the graphic crosshair cursor
;
; ARGUMENTS: XC,YC = Position of graphic cursor
;
; ARGUMENTS RETURNED: None, A, X, and Y are preserved
;
.proc _SONGC
            sec                     ; Enable crosshair flag
            ror     CRSHAIR         ;
            ; Fall through
.endproc

; Internal procedure: draw/delete graphic cursor (crosshair) into screen
;
.proc DRAWGC
            pha                     ; Preserve registers
            txa                     ;
            pha                     ;
            tya                     ;
            pha                     ;
            jsr     VALXCYC         ; Validate coordinates
            lda     YC              ; Get current Y coordinate
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
            lda     DISPTBL,y       ; Get mask for bit displacement
            sta     IOTEMP1         ; Save it temporarily 
            ldx     #$00
            ldy     #$00
LC491:      lda     (VRAMORG),y
            eor     IOTEMP1
            sta     (VRAMORG),y
            lda     VRAMORG
            sec
            sbc     #VIDHRES/8
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
            jsr     _TIOON          ; Turn on I/O address space
            lda     $BFFC
            and     #$F1
            sta     $BFFC
            lda     $BFF1
LC4BC:      lda     $BFFD
            and     #$01
            beq     LC4BC
            sta     $BFC4
            jmp     _IORES          ; Restore RAM address space
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
            jsr     _SINTLP         ; Wait for end of frame and activate the light pen
            jsr     _TIOON          ; Turn on I/O address space
            lda     $BFFC
            ora     #$04
            sta     $BFFC
            lda     $BFF1
LC4DA:      jsr     _STSTLP         ; Test for light pen hit and return coords if a hit
            bcs     LC4E7
            lda     $BFFD
            and     #$01
            beq     LC4DA
            clc
LC4E7:      jmp     _IORES          ; Restore RAM address space
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
            jsr     _TIOON          ; Turn on I/O address space
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
            cmp     #VIDHRES/8
            bcc     LC51A
            sbc     #VIDHRES/8
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
            jmp     _IORES          ; Restore RAM address space
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
