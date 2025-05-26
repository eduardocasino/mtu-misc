; da65 V2.18 - Ubuntu 2.19-1
; Created:    2025-05-26 16:19:58
; Input file: sysgendisk.bin
; Page:       1

            .setcpu "6502"

            .include "symbols.inc"
            .include "codos.inc"

            .code

            ; Loadable file data
            ;
            .byte   $58             ; CODOS loadable file header byte
            .byte   $00             ; Memory overlay
            .byte   $00             ; Memory bank
            .byte   $00             ; Reserved
            .addr   START           ; Entry point
            .addr   START           ; Load address
            .word   PROG_SIZE       ; Memory image size

.proc START
            lda     #$00
            sta     HSRCW
            jsr     OUTSTR
            .byte   $0D, "THIS UTILITY MODIFIES DISK ATTRIBUTES"
            .byte   $0D, "FOR CODOS ON DRIVE 0 DISK."
            .byte   $0D, "DEFAULTS SHOWN IN ( )."
            .byte   $0D, "WANT TO PROCEED (Y)?=", $00

            ldx     #$01            ; Get line from console
            jsr     GETLINE         ;
            jsr     GETNEXTNB       ; Get next non-blank
            beq     PROCEED         ; If blank, continue (wow, dangerous)
            cmp     #'Y'            ; If 'Y', continue
            beq     PROCEED         ;
            rts                     ; Else, return

PROCEED:    cld
            jsr     READCODOS       ; Read CODOS relevant part from file
            jsr     SETNDRIVES      ; Set number of installed drives
            jsr     SETNBUFFS       ; Set number of system buffers
            jsr     SETSTPNLOAD
            jsr     WRITECODOS      ; Write changes back to file
            ldx     #$00
            jsr     FREECH
            jsr     OUTSTR
            .byte   $0D, "SYSTEM MODIFIED."
            .byte   $0D, "CHANGES WILL BE ACTIVATED ON NEXT POWER-UP."
            .byte   $0D, "SUGGEST YOU LOCK CODOS.Z.", $00

            rts
.endproc


.proc SETNDRIVES
START:      jsr     OUTSTR          ; Print messages
            .byte   $0D, "# OF DRIVES (", $00

            lda     NDRIVES - LDHEADER + BUFFER
            sta     SYSNDRIVES      ; Get current NDRIVES and store locally
            clc                     ; Convert to ascii
            adc     #'0'            ;
            ldx     #$02            ; Output to console
            jsr     OUTCHAR         ;
            jsr     OUTSTR          ; And print the rest of the message
            .byte   ")?=", $00      
        
            jsr     GETNUMBER       ; Get number from console and store into TMPNUM
            bcc     RETURN          ; If no valid drive, just return (leave default)
            lda     TMPNUM+1        ; Get MSB
            beq     GETLSB          ; If 0, get LSB
BADNUM:     jsr     ILLEGAL         ; If not, too large
            jmp     START           ; And start again

GETLSB:     lda     TMPNUM          ; Get LSB
            beq     BADNUM          ; If 0, too small
            cmp     #MAXDRIVES+1    ; Over the maximum?
            bcs     BADNUM          ; Yes, error
                                    ; No, store into buffer
            sta     NDRIVES - LDHEADER + BUFFER
            cmp     #(MAXDRIVES/2)+1
            bcc     RETURN          ; If drives <= 2, return
            beq     REDUCEONE       ; If drives = 3, reduce 1 buffer
            jsr     REDUCEBUFS      ; If drives = 4, reduce 2 buffers
REDUCEONE:  jsr     REDUCEBUFS      ;
RETURN:     rts

.endproc


; Set number of system buffers
;
.proc SETNBUFFS
                                    ; Get Top of active files table
START:      lda     TOPASSIGTB - LDHEADER + BUFFER  ; $B96D
            ldx     #$00            ; Divide by size to get the number
ITERATE:    sec                     ; 
            sbc     #FINFOLEN       ;
            bcc     DIVDONE         ; Finished
            inx
            bne     ITERATE
DIVDONE:    txa                     ; Ad number of system drives
            clc                     ;
            adc     SYSNDRIVES      ;
            sec                     ; Minus two
            sbc     #$02            ;
            sta     TMPNUM          ; And store into TMPNUM
            lda     #$00            ;
            sta     TMPNUM+1        ;
            jsr     OUTSTR          ; Print current # of disk buffers
            .byte   $0D, "# DISK BUFFERS (", $00
            lda     TMPNUM          ; Get number (LSB)
            clc                     ;
            adc     #'0'            ; Convert to ascii
            ldx     #$02            ; And output to console
            jsr     OUTCHAR         ;
            jsr     OUTSTR          ; Complete the string
            .byte   ")?=", $00      ;
            jsr     GETNUMBER       ; Get number from console
            lda     TMPNUM+1        ; Get MSB
            beq     CONT            ; If 0, continue
BADNUM:     jsr     ILLEGAL         ; To big
            jmp     START           ; Start again

CONT:       lda     TMPNUM          ; 
            cmp     #MAXBUFFERS+1   ; Bigger than max?
            bcs     BADNUM          ; Yes, error
            adc     #$02            ; Increment in two
            sec                     ; And substract number of drives
            sbc     NDRIVES - LDHEADER + BUFFER
            bcc     BADNUM          ; Not enough buffers
            cmp     #$02            ; Compare to bare minimum
            bcc     BADNUM          ; Not enough
            tax                     ; Transfer to X
            lda     #$01            ; Calculate new top of table
            clc                     ;
CALC:       adc     #FINFOLEN       ;
            dex                     ;
            bne     CALC            ; Repeat until no more buffers
            sta     TOPASSIGTB - LDHEADER + BUFFER
            rts                     ; Update and return
.endproc


; Set track step and head load times
;
.proc SETSTPNLOAD
START:      jsr     OUTSTR
            .byte   $0D, "TRACK STEP TIME ($", $00
            
            ; Get track stepping time for the SPECIFY command:
            ; 
            ; Command W 0  0  0  0  0  0  1  1
            ; W         <-  SRT ->  <-  HUT ->
            ; W         <-      HLT      ->  ND
            ;
            lda     HDSTEP - LDHEADER + BUFFER
            lsr     a               ; It is stored into the high nibble of 
            lsr     a               ; the byte, so rotate right
            lsr     a               ;
            lsr     a               ;
            sta     TMPNUM          ; And store into TMPNUM
            lda     #$00            ;
            sta     TMPNUM+1        ;
            lda     #$10            ; Valid values are 1 to 16 (stored as 0 to 15)
            sec                     ; Convert to 1 to 16
            sbc     TMPNUM          ;
            sta     TMPNUM          ;
            ldy     #$00            ; 
            jsr     HEXBYTE         ; Convert to ascii hex and output to console
            jsr     POUTBUFF02      ;
            jsr     OUTSTR          ;
            .byte   " MS)?=", $00   ; Output rest of the string
            jsr     GETNUMBER       ; Get number form console
            bcc     SETHLT          ; If empty, use default
            lda     TMPNUM+1        ; Check MSB
            beq     CONT            ; 0, continue
BADNUM:     jsr     ILLEGAL         ; Too big
            jmp     START           ; STart again

CONT:       lda     TMPNUM          ; Get LSB
            beq     BADNUM          ; Minimun is 1
            cmp     #$10            ; Maximum is 16
            bcs     BADNUM          ;
            lda     #$00            ; Convert to 0 - 15 and move it to the high nibble
            sec                     ;
            sbc     TMPNUM          ;
            asl     a               ;
            asl     a               ;
            asl     a               ;
            asl     a               ;
            sta     TMPNUM          ; Store it
            lda     HDSTEP - LDHEADER + BUFFER
            and     #$0F            ; Clear default
            ora     TMPNUM          ; Add the other nibble (HUT) and update
            sta     HDSTEP - LDHEADER + BUFFER
SETHLT:     jsr     OUTSTR
            .byte   $0D, "HEAD LOAD TIME ($", $00
            lda     #$00            ; Init HLT (Head Load Time)
            sta     TMPNUM+1        ;
            lda     HDLOAD - LDHEADER + BUFFER
            and     #$FE            ; Get current value and clear last bit
            clc                     ; Values are 2 to 254ms in 2ms increments).
            adc     #$02            ; Why? I think it is a bug
            sta     TMPNUM          ; Store it
            ldy     #$00            ; Output to console as ascii hex
            jsr     HEXBYTE         ;
            jsr     POUTBUFF02      ;
            jsr     OUTSTR          ;
            .byte   " MS)?=", $00   ; Prit the rest of the string
            jsr     GETNUMBER       ; Get number from console
            bcc     RETURN          ; If none, go with default
            lda     TMPNUM+1        ; Get MSB
            beq     CONT2           ; If 0, check LSB
BADNUM2:    jsr     ILLEGAL         ; Too big
            jmp     SETHLT          ; Start again

CONT2:      lda     TMPNUM          ; Get LSB
            beq     BADNUM2         ; Too small
            sec                     ;
            sbc     #$01            ; Again, I think it is a bug
            bcc     BADNUM2         ; Too small
            and     #$FE            ; Clear last bit
            sta     TMPNUM          ;
            lda     HDLOAD - LDHEADER + BUFFER
            and     #$01            ; Get current, set new value and store back
            ora     TMPNUM          ;
            sta     HDLOAD - LDHEADER + BUFFER
RETURN:     rts
.endproc

; Reduce the number of simultaneous FINFO buffers
;
; Moves the info structure one entry upwards
;
.proc REDUCEBUFS
            ldx     #$00
LB690:      lda     FINFOTBL + 1 + FINFOLEN - LDHEADER + BUFFER,x
            sta     FINFOTBL + 1 - LDHEADER + BUFFER,x
            inx
            cpx     #$5B
            bcc     LB690
            rts
.endproc

; Get numeric response from console and stores into TMPNUM
;
; EVALEXP returtns carry clear if no valid digit was converted
;
.proc GETNUMBER
            ldx     #$01            ; Console
            jsr     GETLINE         ; Get entire line
            ldx     #_TMPBUFP       ; Eval expression and store result
            jsr     EVALEXP         ; into TMPBUFP
            bcc     RETURN          ; No valid value, return
            lda     TMPBUFP         ; Store result in TMPNUM
            sta     TMPNUM          ;
            lda     TMPBUFP+1       ;
            sta     TMPNUM+1        ;
RETURN:     rts
.endproc

; Print illegal value message
;
.proc ILLEGAL
            jsr     OUTSTR
            .byte   $0D, "ILLEGAL VALUE.", $00
            rts
.endproc

.proc READCODOS
            ldx     #$07            ; Store CODOS file name into the file header
LOOP:       lda     CODOSFILE,x     ;
            sta     FILEHDR+FHDR::DIRE+DIRE::FNAM,x
            dex                     ;
            bpl     LOOP            ;
            ldx     #$00            ;
            stx     CURRDRV         ; Assign to channel 0
            jsr     FOPEN0          ;
            lda     CURFINFO+FINFO::FLAGS
            and     #FLLOCKED       ; Check if file locked
            beq     CONT            ; No, read it
            jsr     ERROR07         ; Locked file violation
            ; Not reached
CONT:       jsr     SETFBUFFER      ; Sets input/output buffer. Also sets channel X = 0
            jsr     GETMBUFF        ; Read it
            rts

CODOSFILE:  .byte   "CODOS.Z", $00

.endproc

; Write changes from BUFFER to CODOS.Z
;
.proc WRITECODOS
            ldx     #$00            ; Channel 0
            jsr     FREWIND         ; Rewindo to beginning of file
            jsr     SETFBUFFER       ; Sets input/output buffer. Also sets channel X = 0
            jsr     OUTMBUFF        ; Write over the changes
            rts
.endproc

; Sets file buffer and file buffer size
;
; Also sets X = 0 for the channel
;
.proc SETFBUFFER
            lda     #<BUFFER
            sta     MEMBUFF
            lda     #>BUFFER
            sta     MEMBUFF+1
            lda     #<BUFFER_SIZE
            sta     MEMCOUNT
            lda     #>BUFFER_SIZE
            sta     MEMCOUNT+1
            ldx     #$00
            rts
.endproc

            .data

TMPNUM:     .word   $0000           ; Storage for numbers read from console
            .byte   $00, $00        ; Unused
SYSNDRIVES: .byte   $02             ; Number of installed floppy drives

PROG_SIZE = * - START

            .bss

BUFFER_SIZE = $02A1                 ; Why this size?

BUFFER:     .res    BUFFER_SIZE

            .end
