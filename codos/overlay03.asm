; da65 V2.19 - Git cf0688fc5
; Created:    2025-05-05 12:21:30
; Input file: kk3.bin
; Page:       1


            .setcpu "6502"

            .include "symbols.inc"
            .include "codos.inc"

            .segment "overlays"

            .byte   $03             ; Overlay number

; REG Command
;
; DESCRIPTION:  Display or alter the contents of the user's 6502 registers
;                   [             {"character" ...}]
; SYNTAX:       REG [<register>[=]{<value>        }]
;                   [             {'character' ...}]
; ARGUMENTS:    <register>  = register name to be altered, A, X, Y, F, S, or P
;               <value>     = numeric value or numeric expression
;               <character> = an ASCII character to be deposited
;
REG:        bne     @PRINT          ; Any argument, go alter regs
            jmp     OUTREGSLB       ; Print'em and return
            ; Not reached

@PRINT:     cmp     #'P'            ; Status register?
            bne     @CHKVLD         ; No, go check if it is another valid reg
            jsr     SKIPEQ          ; Skip optional '='
            ldx     #_PCSAVE        ; Store result of expression in PCSAVE
            sty     CMDLIDX         ; Update command line index
            jsr     EVALEXP         ; Eval expression
            bcs     @NEXT           ; if ok, go check if there are more resg to alter
@ERROR:     jsr     ERROR24         ; <value> missing or illegal
            ; Not reached

@NEXT:      jsr     GETNEXTNB       ; Get next non-blank from command line
            sty     CMDLIDX         ; Update command line index
            bne     @PRINT          ; If any, process it
            rts                     ; else, return

@CHKVLD:    ldx     #$04            ; Check if valid register name
@CMPVLD:    cmp     REGDESC,x       ; Compare against string of valid register names
            beq     @VALID          ; Found, it is a valid register name (x contains offset)
            dex                     ; Check next
            bpl     @CMPVLD         ;
            jsr     ERROR28         ; Missing or illegal register name
            ; Not reached

@VALID:     jsr     SKIPEQ          ; Skip '='
            cmp     #'''            ; Is it a single quote?
            beq     @ISCHR          ; Yes, continue
            cmp     #'"'            ; Is it a double quote?
            bne     @ISVAL          ; No, it is a value
@ISCHR:     sta     QUOTE           ; Save delimiter
            jsr     GETNEXTCH1      ; Advance one pos and get next character
            cmp     #$0D            ; End of line?
            beq     @ERROR          ; Missing value
            sta     STACKP,x        ; Save value in register
            jsr     GETNEXTCH1      ; Advance and get next char
            cmp     QUOTE           ; Is it the delimiter?
            bne     @ERROR          ; No, invalid value
            iny                     ; Advance one pos
            jmp     @NEXT           ; And go check if there are more regs to alter

@ISVAL:     jsr     GETBYTE         ; Get byte from command line
            bcc     @ERROR          ; Error, not a valid number
            sta     STACKP,x        ; Store register
            jmp     @NEXT           ; Go check if there are more registers to alter

; Skip optional '=' in command line
;
SKIPEQ:     jsr     GETNEXTNB1      ; Advance one and get next non-blank
            cmp     #'='            ; Is it the optional '='?
            bne     @CONT           ; No, go on
            jsr     GETNEXTNB1      ; Yes, skip it
@CONT:      sty     CMDLIDX         ; Update command line index
            rts


; DATE Command
;
; DESCRIPTION:  Set the creation date for any new files generated
;
; SYNTAX:       DATE [<dd-mmm-yy>]
;
; ARGUMENTS:    <dd-mmm-yy> = desired date
;
; NOTE: If no date is passed, prompt the user
;
DATE:       bne     @SET            ; There are arguments, go set date
            jsr     SETINPB         ; No arguments. Set input buffer to input line buffer
            lda     INPBUFP         ; And set line-buffer used for INLINE and EDLINE
            sta     QLN             ;
            lda     INPBUFP+1       ;
            sta     QLN+1           ;
            jsr     OUTSTR          ; Print prompt
            .byte   $0D, "CODOS 2.0   (C) 1981 MTU", $00
            jsr     OUTSTR
            .byte   $0D, "PLEASE ENTER DATE (EXAMPLE:04-JUL-76)?= ", $00
            jsr     JINLINE         ; Input an entire line from the keyboard
            jsr     GETNEXTNB       ; Get next non blank from input buffer
            bne     @SET            ; There are arguments, go set the date
            ldx     #$08            ; No arguments, set "*UNDATED*"
@PRNC:      lda     UNDATED,x       ;
            sta     TDATE,x         ;
            dex                     ;
            bpl     @PRNC           ;
            rts                     ; And return

@SET:       ldx     #$00            ; Just blindly copy what's in the buffer
@CPYC:      sta     TDATE,x         ;
            inx                     ;
            cpx     #$09            ; Max length reached?
            beq     @RETURN         ; Yes, return
            jsr     GETNEXTCH1      ; No, advance and get next char from input buffer
            bne     @CPYC           ; If any, copy it
@CPSP:      lda     #' '            ; If not, fill with spaces
            sta     TDATE,x         ;
            inx                     ;
            cpx     #$09            ;
            bcc     @CPSP           ; Until length is reached
@RETURN:    rts

UNDATED:    .byte   "*UNDATED*"
        
            ; This block is just junk that was in the buffer when
            ; writing it to disk. I leave it to facilitate checksum
            ; comparisons with the original
            ;
            .byte   $D7, $FE, $00, $FE, $60, $00, $00, $1E
