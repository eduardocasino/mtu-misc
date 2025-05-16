; da65 V2.19 - Git cf0688fc5
; Created:    2025-05-05 12:23:32
; Input file: kk14.bin
; Page:       1


            .setcpu "6502"

            .include "symbols.inc"
            .include "codos.inc"

            .segment "overlays"

            .byte   $0E             ; Overlay number

; ONKEY Command
;
; DESCRIPTION:  Define a function key legend and associated substitution string.
;
; SYNTAX:       ONKEY [<key #>[<legend>[<string>[<term>]]]]
;
; ARGUMENTS:    <key #> = a function key number between 1 and 8 inclusive
;               <legend> = a string of 8 or fewer characters enclosed in quotes
;                          which is to be displayed in the specified legend box. 
;               <string> = a string of 31 or fewer characters in quotes which is
;                          to be entered into the input line buffer when the
;                          specified function key is pressed. 
;               <term> = the numeric value of the termination character to be
;                        entered into the input line buffer following the {string}.
;                        If omitted, a carriage return will be entered. If bit 7 of
;                        the character is set, the {string} will not be echoed to
;                        the console.
;
ONKEY:      jsr     GETNEXTNB       ; Get next non-blank from command line
            beq     @CLRALL         ; If no args, clear all legends and strings
            jsr     GETBYTE         ; Get key number
            sec                     ; Convert to base 0
            sbc     #$01            ;
            cmp     #$08            ; Is it a valid key number?
            bcc     @KEYOK          ; Yes, continue
            jsr     ERROR52         ; Missing or illegal function key number
            ; Not reached

@KEYOK:     asl     a               ; Get the index to the legend table by
            asl     a               ; multiplying by 8 (each legend entry is 8
            asl     a               ; chars long)
            sta     SAVEA5          ; Save it temporarily
            clc                     ; Clear carry for addition
            adc     #$08            ; Adds 8 to point to next entry
            sta     SAVEA6          ; And save it
            tax                     ; Transfer to X as index to the legends table 
            jsr     UPDLENTRY       ; Update legend entry
            lda     SAVEA5          ; Recover index to legend table
            asl     a               ; Multiply by 4 to get the index to the
            asl     a               ; KEYSTR table
            sta     SAVEA5          ; And save it
            clc                     ; Clear carry for addition
            adc     #$1F            ; Calculate the end of the entry
            tax                     ; Transfer to X as index to the strings table
            sta     SAVEA6          ; And save it
            jsr     UPDSENTRY       ; Update string entry
@DRAW:      jsr     DRWLEG          ; Draw legends
            rts                     ; and return

@CLRALL:    ldx     #$3F            ; Index to end of table
            lda     #' '            ; Clear with spaces
@CLRCHR:    sta     LEGTBL,x        ; Clear char
            dex                     ; Go next
            bpl     @CLRCHR         ; Finished?. No, go clear next
            inx                     ; Yes. X = 0
            lda     #$80            ; Mark KEYSTR table as empty
@EMPTY:     sta     KEYSTR,x        ; From 255 backwards
            dex                     ;
            bne     @EMPTY          ; Until 0
            jmp     @DRAW           ; Draw legends and return


; Update entry in function key legend table
;
; X contains index to next entry, SAVEA5 contains index to target entry
;
UPDLENTRY:  lda     #' '            ; Clear legend (stores)
@CLCHR:     dex                     ; Clears legend char backwards
            sta     LEGTBL,x        ;
            cpx     SAVEA5          ; Check if we have reached the first char
            bne     @CLCHR          ; No, loop
            jsr     GETNEXTNB       ; Get next non-blank from the command line
            cmp     #$0D            ; End of line?
            beq     @RETURN         ; Yes, return
            cmp     #'''            ; String delimiter?
            beq     @ISSTR          ; Yes, continue
            cmp     #'"'            ; The other string delimiter?
            beq     @ISSTR          ; Yes, continue
@ERR26:     sty     CMDLIDX         ; No, update command line index
            jsr     ERROR26         ; Missing or illegal character string delimiter (' , ")
            ; Not reached

@ISSTR:     sta     QUOTE           ; Save the delimiter
@CPYLEG:    iny                     ; Advance one position
            lda     (INPBUFP),y     ; Get char
            cmp     #$0D            ; End of line?
            beq     @ERR26          ; Yes, error
            cmp     QUOTE           ; Is it the string delimiter?
            beq     @INYRET         ; Yes, advance on pos and return
            cpx     SAVEA6          ; Are we past the last char of the legend?
            bcs     @CPYLEG         ; Yes, loop looking for QUOTE
            sta     LEGTBL,x        ; No, store char in the table entry
            inx                     ; Advance one pos in the table entry
            bne     @CPYLEG         ; Always jump
            ; Not reached

@INYRET:    iny
@RETURN:    rts


; Update entry in function key substitute string table
;
; X contains index to end of entry, SAVEA5 contains index to target entry
;
UPDSENTRY:  lda     #$80            ; Use no-echo mark to blank table
            inx                     ; Increment to prepare for loop
@CLCHR:     dex                     ; Blank char at table index
            sta     KEYSTR,x        ;
            cpx     SAVEA5          ; Have we reached the beginning of the entry?
            bne     @CLCHR          ; No, loop
            jsr     GETNEXTNB       ; Get next non-blank from command line
            cmp     #$0D            ; End of line?
            beq     @RETURN         ; Yes, return
            cmp     #'''            ; String delimiter?
            beq     @ISSTR          ; Yes, continue
            cmp     #'"'            ; The other string delimiter?
            beq     @ISSTR          ; Yes, continue
@ERR26:     sty     CMDLIDX         ; No, update command line index
            jsr     ERROR26         ; Missing or illegal character string delimiter (' , ")
            ; Not reached

@ISSTR:     sta     QUOTE           ; Save the delimiter
@CPYSTR:    iny                     ; Advance one position
            lda     (INPBUFP),y     ; Get char
            cmp     #$0D            ; End of line?
            beq     @ERR26          ; Yes, error
            cmp     QUOTE           ; Is it the string delimiter?
            beq     @GETTERM        ; Yes, advance on pos and return
            cpx     SAVEA6          ; Are we past the last char of the legend?
            bcs     @CPYSTR         ; Yes, loop looking for QUOTE
            sta     KEYSTR,x        ; No, store char in the table entry
            inx                     ; Advance one pos in the table entry
            bne     @CPYSTR         ; Always jump
            ; Not reached

@GETTERM:   jsr     GETNEXTNB1      ; Get next non-blank from command line
            beq     @SETDEF         ; If no more, set the default terminator character
            jsr     GETBYTE         ; Get terminator byte from command line
            jmp     @SETTERM        ; And jump to set it

@SETDEF:    lda     #$0D            ; Set CR as the string terminator character
@SETTERM:   sta     KEYSTR,x        ; And store it
@RETURN:    rts

            ; This block is just junk that was in the buffer when
            ; writing it to disk. I leave it to facilitate checksum
            ; comparisons with the original
            ;
            .byte                            $60, $20, $35
            .byte   $FB, $A5, $D2, $95, $C1, $A5, $D3, $95
            .byte   $C2, $60, $0C, $FB, $1A, $FB, $D5, $FE
            .byte   $6F, $FB, $A9, $FB, $B2, $FB, $BB, $FB
            .byte   $56, $FE, $61, $FE, $6E, $FE, $7B, $FE
            .byte   $88, $FE, $95, $FE, $B4, $FE, $E0, $FE
