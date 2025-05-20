; da65 V2.19 - Git cf0688fc5
; Created:    2025-05-05 13:32:25
; Input file: kk4.bin
; Page:       1


            .setcpu "6502"

            .include "symbols.inc"
            .include "codos.inc"

            .segment "overlays"

            .byte   $04             ; Overlay number

DUMPLINELEN = $47                   ; Length of dump lines    

; DUMP Command
;
; DESCRIPTION:  Display the contents of a block of memory in hexadecimal and
;               as ASCII characters. 
;
; SYNTAX:       DUMP <from> [<to> [<device> ]]
;                           [     [<channel>]]
; ARGUMENTS:    <from> = desired starting address
;               <to> = desired ending address. Default is from +15
;               <device> = desired device on which to display the output
;                          Defaults to the console
;               <channel> = desired channel on which to display the output
;
DUMP:       ldx     #$00            ;
            stx     DESTBUFF+1      ; Set default count 
            inx                     ; Init to 1, but a line with 16 bytes is
            stx     DESTBUFF        ; printed anyways
            inx                     ; 
            stx     DUMPCHANN       ; Output channel (default 2)
            jsr     GADDRBNKMB      ; Get Address:bank from command line and
                                    ; store into MEMBUFF and NEWBNK
            lda     MEMBUFF         ; Store addr in TMPBUF
            sta     TMPBUFP         ;
            lda     MEMBUFF+1       ;
            sta     TMPBUFP+1       ;
            jsr     GETNEXTNB       ; Get next non-blank from command line
            beq     @DODUMP         ; No more, exec with default args 
            jsr     GETTOP          ; Get TO address and store into MEMCOUNT
            lda     MEMCOUNT        ; Convert the <to> address into a byte count
            cld                     ; from TMPBUFF and store it back into DESTBUFF
            sec                     ;
            sbc     TMPBUFP         ;
            sta     DESTBUFF        ;
            lda     MEMCOUNT+1      ;
            sbc     TMPBUFP+1       ;
            sta     DESTBUFF+1      ;
            bcs     @DSTOK          ; <to> is greater or equal than <from>
            jsr     ERROR16         ; <from> address greater than to address
            ; Not reached

@DSTOK:     jsr     GETNEXTNB       ; Get next non-blank from command line
            beq     @DODUMP         ; No more arguments, exec to default channel
            cmp     #'A'            ; Devices are letters, channels are numbers
            bcc     @ISCHAN         ; Not a device
            sta     CURRDRV         ; Store device name in CURRDRV
            jsr     ASSIGN0         ; Assign to channel 0
            ldx     #$00            ; Set channel to 0
            beq     @SETCH          ; Always jump to set channel number
@ISCHAN:    jsr     GETCHANN        ; Get channel number from command line
@SETCH:     stx     DUMPCHANN       ;
@DODUMP:    jsr     CLRINITLN       ; Clear output buffer and init dump line

            ; Print first line with byte index

            ldy     #$06            ; Skip 6 positions (address + 2 spaces)
            lda     TMPBUFP         ; Get low byte of address to dump
            and     #$0F            ; Get low nibble (modulo 16)
            tax                     ; Set print index         
            dex                     ; Decrement X as it is incremented entering the loop
@DUMPN:     inx                     ; Next byte index
            cpx     #$10            ; Past $F?
            bcc     @OUTN           ; No, go print it
            ldx     #$00
@OUTN:      txa                     ; Transfer to A (where NIBBLE expects it)
            jsr     NIBBLE          ; And print to (OUTBUFP),y
            iny                     ; Advance two positions (spaces)
            iny                     ;
            jsr     ONELESS         ; Decrement bytes to print
            bne     @DUMPN          ; Continue if more bytes left
            ldx     DUMPCHANN       ; Get output channel
            jsr     POUTBUFFCR      ; Output Y chars + CR from (OUTBUFP) to channel X
@DUMPL:     jsr     CLRINITLN       ; Clear and init next dump line
            ldx     #_TMPBUFP       ; Write hex address of current byte to (OUTBUFP),y
            jsr     HEXENCOD        ;
            iny                     ; Advance position in the output buffer
@DUMPB:     iny                     ; And another one in the loop
            jsr     GETDUMPBYT      ; Get byte to dump
            jsr     HEXBYTE         ; Converts into 2-byte ascii hex at (OUTBUFP),y 
            jsr     NXTBYTE         ; Advance to next byte and decrement bytes left
            bne     @DUMPB          ; While there are bytes to print, loop
            lda     TMPBUFP         ; Go back to the start address
            sec                     ; to print the ascii chars
            sbc     DUMPBYTES       ;
            sta     TMPBUFP         ;
            lda     TMPBUFP+1       ;
            sbc     #$00            ;
            sta     TMPBUFP+1       ;
            jsr     INITLN          ; Init bytes left and bytes til space
            iny                     ; A couple of spaces
            iny                     ;
@DUMPA:     jsr     GETDUMPBYT      ; Get byte to dump
            cmp     #'!'            ; Check printable
            bcc     @PDOT           ; No, go print a dot
            cmp     #'~'+1          ; Maybe, check against last printable
            bcc     @OUTC           ; Yes, print to the output buffer
@PDOT:      lda     #'.'            ; Not printable, print a dot instead
@OUTC:      sta     (OUTBUFP),y     ; Output char
            iny                     ; Advance one position in the buffer
            jsr     NXTBYTE         ; Advance to next byte and decrement bytes left
            bne     @DUMPA          ; While there are chars to print, loop
            ldx     DUMPCHANN       ; Get output channel
            jsr     POUTBUFFCR      ; Output Y chars + CR from (OUTBUFP) to channel X
            lda     DESTBUFF        ; Get bytes left to dump and decrement
            sec                     ; by the dumped bytes number
            sbc     DUMPBYTES       ;
            sta     DESTBUFF        ;
            lda     DESTBUFF+1      ;
            sbc     #$00            ;
            sta     DESTBUFF+1      ;
            bcs     @DUMPL          ; If there are bytes, left, go print next line
            jmp     FREECH0         ; No more, free channel and return

; Clear and init dump line
;
CLRINITLN:  ldy     #DUMPLINELEN    ; Clear output buffer
            lda     #' '            ; with spaces
@LOOP:      sta     (OUTBUFP),y     ;
            dey                     ;
            bpl     @LOOP           ;
            iny                     ; Y now is 0
            ; Fall through

; Init dump line
;
INITLN:     lda     DUMPBYTES       ; Init number of bytes to dump per display line
            sta     BYTESLEFT       ;
            lsr     a               ; Init bytes left until extra space
            sta     BYTESPACE       ;
            rts

; Get byte from address (TMPBUFP) at bank NEWBANK
;
; Returns byte in A and restores bank
;
GETDUMPBYT: lda     NEWBNK          ; Switch to <from> bank
            eor     DEFBNKCFG       ;
            sta     BNKCTL          ;
            ldx     #$00            ; Get byte
            lda     (TMPBUFP,x)     ;
            ldx     DEFBNKCFG       ; Switch back to default bank
            stx     BNKCTL          ;
            rts

; Advance to next byte to dump and decrement bytes left
;
NXTBYTE:    inc     TMPBUFP         ; Go to address of next byte to dump
            bne     @CONT           ;
            inc     TMPBUFP+1       ;
@CONT:
            ; Fall through

; Decrement bytes left. Adds extra space if needed.
;
ONELESS:    dec     BYTESPACE       ; Check if an extra space is needed
            bne     @NOEXTRA        ; Not yet
            iny                     ; Yes, increment pos in output buffer
@NOEXTRA:   dec     BYTESLEFT       ; Decrement bytes left
            rts

BYTESLEFT:  .byte   $00             ; Bytes left to print in current line
BYTESPACE:  .byte   $00             ; Bytes left until extra space

            ; This byte is just junk that was in the buffer when
            ; writing it to disk. I leave it to facilitate checksum
            ; comparisons with the original
            ;
            .byte   $1E
