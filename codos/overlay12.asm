; da65 V2.19 - Git cf0688fc5
; Created:    2025-05-05 12:23:21
; Input file: kk12.bin
; Page:       1


            .setcpu "6502"

            .include "symbols.inc"
            .include "codos.inc"

            .segment "overlays"

            .byte   $0C             ; Overlay number

MAXSTRLEN = $0B

; HUNT Command
;
; DESCRIPTION:  Search a block of memory for a string of bytes
;                                 {"<char>..."     }
; SYNTAX:       HUNT <from><to>[=]{<value>      ...}
;                                 {'<char>...'     }
; ARGUMENTS:    <from> = starting address for the search
;               <to> = final address for the search
;               <char> = an ASCII character
;               <value> = a numeric value, 0 to $FF. In a string of values, one
;                         (and only one) value can be replaced by the wildcard, "?",
;                         which matches any single byte. 
;
HUNT:       jsr     GADDRBNKMB      ; Get <from> address:bank and store into MEMBUFF and NEWBNK
            lda     NEWBNK          ; Get <from> bank
            eor     DEFBNKCFG       ;
            sta     FROMBANK        ; And save it
            jsr     GETTOP          ; Get <to> from command line and store into MEMCOUNT
            lda     MEMCOUNT        ; Convert MEMCOUNT to offset and store into TMPBUFP
            sec                     ;
            sbc     MEMBUFF         ;
            sta     TMPBUFP         ;
            lda     MEMCOUNT+1      ;
            sbc     MEMBUFF+1       ;
            sta     TMPBUFP+1       ;
            bcs     @CONT           ; If offset is positive, ok
            jsr     ERROR16         ; <from> address greater than <to> address
            ; Not reached
    
@CONT:      lda     #$FF            ; Init wildcard position to "none"
            sta     WILDCARDP       ;
            lda     MEMBUFF         ; Move <from> address to DESTBUFF
            sta     DESTBUFF        ;
            lda     MEMBUFF+1       ;
            sta     DESTBUFF+1      ;
            jsr     GETNEXTNB       ; Get next non-blank from command line
            bne     @ISEQ           ; Go check if it is the optional '='
@MISERR:    jsr     ERROR24         ; <value> missing or illegal
            ; Not reached

@ISEQ:      ldx     #$00            ; Init search string length
            cmp     #'='            ; Is it an '='?
            bne     @ISQM           ; No, just skip it
            jsr     GETNEXTNB1      ; Advance one pos and get next non-blank
            beq     @MISERR         ; If none, error
@ISQM:      cmp     #'?'            ; Is it a '?'?
            beq     @MISERR         ; Yes, it should not be here
@GINP:      cmp     #'''            ; Is it a char delimiter?
            beq     @ISCHAR         ; Yes, go process string
            cmp     #'"'            ; Is it the other char delimiter¿
            beq     @ISCHAR         ; Yes, go process string
            cmp     #'?'            ; Is it the wildcard
            bne     @ISVAL          ; No, then is a numeric value
            sty     CMDLIDX         ; Save command line index
            bit     WILDCARDP       ; Is wildcard already set?
            bpl     @MISERR         ; Yes, then error
            stx     WILDCARDP       ; Set wildcard position
            inx                     ; Increment string length
            iny                     ; Advance one pos in command line
            jmp     @GNEXT          ; And go get next

@ISVAL:     jsr     GETBYTE         ; Get number from command line
            sta     SRCHSTR,x       ; Store into the search string
            inx                     ; Advance pos in the string
            cpx     #MAXSTRLEN      ; If maximum search string length reached
            bcs     @MISERR         ; Go error
            ; Not reached

@GNEXT:     jsr     GETNEXTNB       ; Get next non-blank
            bne     @GINP           ; If any, go get next char from command line
            beq     @GSRCH          ; If not, go search (always jump)
            ; Not reached

@ISCHAR:    sta     QUOTE           ; Save the string delimiter
@GCHAR:     iny                     ; Get next char
            lda     (INPBUFP),y     ;
            bne     @CHKQTE         ; there are more
            jsr     ERROR26         ; Missing or illegal character string delimiter (' , ")
            ; Not reached

@CHKQTE:    cmp     QUOTE           ; Is it the closing quote?
            beq     @ENDQTE         ; Yes, no more chars in string
            sta     SRCHSTR,x       ; No, store char in search string
            inx                     ; Advance one pos
            cpx     #MAXSTRLEN      ; Have we reached the string length?
            bcs     @MISERR         ; Yes, invalid argument
            jmp     @GCHAR          ; No, get next char

@ENDQTE:    iny                     ; Advance pos in command line
            sty     CMDLIDX         ; Store index
            jmp     @GNEXT          ; And go get next char from command line

@GSRCH:     stx     SRCHSTRLEN      ; Store current string length
            cpx     #$00            ; If empty
            beq     @MISERR         ; Missing arg

@SRCHPG:    ldx     FROMBANK        ; Set memory bank
            stx     BNKCTL          ;
            ldy     #$00            ; Init buffer index
            ldx     #$00            ; Init search string index
@CPBYTE:    lda     (DESTBUFF),y    ; Compare bytes
            cmp     SRCHSTR,x       ;
            bne     @CHKWILD        ; Different, check if wildcard
@BMATCH:    iny                     ; Match, advance to next pos
            inx                     ;
            cpx     SRCHSTRLEN      ; Have we reached the string length? 
            bne     @CPBYTE         ; No, compare next byte

            lda     DESTBUFF+1      ; Have we reached page $FE?
            cmp     #$FE            ;
            beq     @CHKWILD        ; Dont print matches from $FE00 and above?

            ; Print match

            ldx     DEFBNKCFG       ; Restore default bank
            stx     BNKCTL          ;
            ldy     #$00            ; Set index to (OUTBUFP) to 0
            ldx     #_DESTBUFF      ; Set HEXENCOD to DESTBUFF
            jsr     HEXENCOD        ; Converts word at P0SCRATCH,x into 4-char ascii hex
            ldx     #$02            ; Redundant (POUTBUFF02 sets X = console)
            jsr     POUTBUFF02      ; Print output buffer to console
            jsr     OUTCR           ; Print return
            jmp     @CSRCH          ; Continue search

@CHKWILD:   cpx     WILDCARDP       ; Wildcard position?
            beq     @BMATCH         ; Yes, go to byte is a match
@CSRCH:     inc     DESTBUFF        ; Increment search buf position
            bne     @ISENDP         ;
            inc     DESTBUFF+1      ;
@ISENDP:    lda     TMPBUFP         ; Check if we've reached the end of page
            bne     @DCOUNT         ; No, go decrement count
            lda     TMPBUFP+1       ; Check if we are reached the last page
            beq     @RETURN         ; Yes, finish
            dec     TMPBUFP+1       ; No, decrement count
@DCOUNT:    dec     TMPBUFP         ;
            jmp     @SRCHPG         ; Search in new page

@RETURN:    ldx     DEFBNKCFG       ; Restore default bank
            stx     BNKCTL          ;
            rts                     ; And return

            ; Values are irrelevant, they are just junk that was in the buffer when
            ; writing it to disk. They should really be .res directives, but I leave
            ; them here to facilitate checksum comparisons with the original
            ;
FROMBANK:   .byte   $C7             ; .res 1
SRCHSTRLEN: .byte   $AE             ; .res 1 - Serach string length
WILDCARDP:  .byte   $D8             ; .res 1 - Position of the wildcard in the string
SRCHSTR:    .byte   $E6, $8E, $E0, $BF, $60, $7F, $7F, $60, $00, $00, $1E
                                    ; .res 11 - Search string

