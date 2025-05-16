; da65 V2.19 - Git cf0688fc5
; Created:    2025-05-05 12:23:49
; Input file: kk16.bin
; Page:       1


            .setcpu "6502"

            .include "symbols.inc"
            .include "codos.inc"

            .segment "overlays"

            .byte   $10             ; Overlay number

; DISK Command
;
; DESCRIPTION:  Display the number of files, remaining space, and volume
;               serial number on all open disk drives. 
;
; SYNTAX:       DISK
;
; ARGUMENTS:    None.
;
DISK:       ldx     #$00            ; Init drives loop
@CHKOPN:    lda     ODRIVES,x       ; Check if drive is open
            bpl     @NXTDRV         ; No, go check next
            stx     DRIVE           ; Yes, store drive
            jsr     @PRINFO         ; Go print info
            ldx     DRIVE           ; Recover drive
@NXTDRV:    inx                     ; Next drive
            cpx     NDRIVES         ; Have we reached the maximum drive?
            bcc     @CHKOPN         ; No, loop
            rts                     ; Yes, return

@PRINFO:    stx     CURRDRV         ; Sets drive as current
            jsr     DRVVALIDO       ; Ensure it is open
            jsr     SETOUTBCH       ; Set output buffer to output line buffer
            jsr     SETBATP         ; Set BATP to the current drive's BAT
            ldy     #_BNENT         ; Get number of entries
            lda     (BATP),y        ;
            sta     P0SCRATCH       ; Store into P0SCRATCH
            lda     #$00            ;
            sta     P0SCRATCH+1     ; High byte is always 0
            ldy     #$00            ; Init index to output buffer
            jsr     DECWORD         ; Encode as ascii decimal number
            jsr     POUTBUFF02      ; Print output buffer to console
            jsr     OUTSTR          ; Print string
            .byte    " FILES:", $00
            lda     CURRDRV         ; Get current drive
            jsr     NIBBLE          ; Convert lower nibble to ascii number
            jsr     POUTBUFF02      ; Print output buffer to console
            jsr     OUTSTR          ; Print string
            .byte   " (VSN=", $00
            ldy     #_BTVSN         ; Get VSN
            lda     (BATP),y        ;
            sta     P0SCRATCH       ; And store into P0SCRATCH
            iny                     ;
            lda     (BATP),y        ;
            sta     P0SCRATCH+1     ;
            ldy     #$00            ; Init output buffer index
            jsr     HEXWORD0        ; Convert VSN to ascii hex
            jsr     POUTBUFF02      ; Print output buffer to console
            jsr     OUTSTR          ; Print string
            .byte   "), ", $00
            jsr     GETFREE         ; Get free blocks
            lda     #$00            ; Convert blocks to kilobytes. Init result.
            sta     P0SCRATCH+1     ; 
            txa                     ; Transfer free blocks to A
            asl     a               ; Multiply by 2 ( blocks are 2K for SS, 4K for DS)
            rol     P0SCRATCH+1     ; Inject carry into high byte
            sta     P0SCRATCH       ; store low byte
            ldx     CURRDRV         ; Recover current drive
            lda     DRVNFO,x        ; Check if two sides
            bpl     @PRFREE         ; No, skip
            asl     P0SCRATCH       ; Yes, block size is twice the size
            rol     P0SCRATCH+1     ;
@PRFREE:    ldy     #$00            ; Set index to output buffer
            jsr     DECWORD         ; Convert bytes free to decimal ascii into output buf
            jsr     POUTBUFF02      ; Print output buffer to console
            jsr     OUTSTR          ; Print string
            .byte   "K FREE", $0D, $00
            rts                     ; And return

; Get free blocks
;
; Returns number in X
;
GETFREE:    ldx     #$00            ; Inits number of free blocks
            ldy     #_BTOPB         ; Y points to last block slot
@LOOP:      lda     (BATP),y        ; Get block info
            bne     @NEXT           ; If not free, go check the next
            inx                     ; Increment free block count
@NEXT:      dey                     ; Decrement block slot
            bne     @LOOP           ; If there are more, loop
            rts

DRIVE:      .byte   $00             ; Drive we are processing

            ; This block is just junk that was in the buffer when
            ; writing it to disk. I leave it to facilitate checksum
            ; comparisons with the original
            ;
            .byte        $BF, $20, $A2, $F8, $20, $DB, $F9
            .byte   $20, $50, $FA, $2C, $20, $00, $A2, $06
            .byte   $AD, $F2, $FE, $8D, $F1, $FE, $20, $D5
            .byte   $FE, $AE, $F4, $FE, $8E, $E0, $BF, $B1
            .byte   $C7, $AE, $D8, $E6, $8E, $E0, $BF, $20
            .byte   $A2, $F8, $4C, $DB, $F9, $20, $9B, $F8
            .byte   $A9, $3A, $91, $CD, $C8, $AD, $F1, $FE
            .byte   $18, $69, $30, $91, $CD, $C8, $20, $DB
            .byte   $F9, $20, $50, $FA, $20, $3D, $20, $00
            .byte   $60, $00, $00, $7F, $7F, $FE, $7B, $FE
            .byte   $88, $FE, $95, $FE, $B4, $FE, $E0, $FE
