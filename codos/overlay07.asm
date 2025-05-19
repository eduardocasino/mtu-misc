; da65 V2.19 - Git cf0688fc5
; Created:    2025-04-30 10:52:35
; Input file: overlay07.bin
; Page:       1


            .setcpu "6502"

            .include "codos.inc"
            .include "symbols.inc"

            .segment "overlays"

            .byte   $07             ; Overlay number

; ASSIGN Command
;
; DESCRIPTION:  Assign an input-output channel to a file or device, or to display all
;               current channel assignments. 
;                      [         {<device>        }    ]
; SYNTAX:       ASSIGN [<channel>{<file>[:<drive>]} ...]
; ARGUMENTS:    <channel> = channel number, 0 to 9
;               <device> = single character device name
;               <file> = file name
;               <drive> = disk drive number, 0 to 3. Defaults to current default drive,
;               usually 0
; 
ASSIGNCMD:  bne     @CONT
            jmp     DISPLAYCH       ; No arguments, display current channel assignments
            ; Not reached
@CONT:      jsr     GETCHANN        ; Get channel number from command line into CHANNEL
                                    ; (in fact, from A, which contains first NN
                                    ;  char after the command name)
            jsr     GETDEVORFIL     ; Get device or file from command line. If it
                                    ; is a file, sets CURRDRV, FNAMBUF
                                    ; If it is a device, CURRDRV contains the device name
            ldx     CHANNEL         ; Assign channel
            jsr     ASSIGN          ;   to file/device
            bit     ASSIGNFLAG      ; Check flag:
            bvc     @ISDEV          ;    If bit 6 is clear: it is a device
            bmi     @FEXIST         ;    If bit 7 is set: file exist
            jsr     OUTSTR          ; Print "NEW FILE"
            .byte   "NEW", 00       ;
            jmp     @SKIP           ;
            ; Not reached

@FEXIST:    jsr     OUTSTR          ; Print "OLD FILE "
            .byte   "OLD", $00      ;
                                    ;
@SKIP:      jsr     OUTSTR          ;
            .byte   " FILE ", $00   ;

            ldy     #$00            ; Copy file name to output buffer
@LOOP:      lda     FNAMBUF,y       ;  copy chars until "." found (inclusive)
            sta     (OUTBUFP),y     ;
            iny                     ;
            cmp     #'.'            ;
            bne     @LOOP           ;

            lda     FNAMBUF,y       ; Copy the extension
            sta     (OUTBUFP),y     ;
            iny
            lda     COLON           ; Copy a colon
            sta     (OUTBUFP),y     ;
            iny                     ;
            lda     CURRDRV         ; And the current drive
            clc                     ;
            adc     #$30            ; (converted to ascii)
            sta     (OUTBUFP),y     ;
            iny                     ; Y now contains the buffer length
            ldx     #$02            ; Output to channel 2 (console)
            jsr     POUTBUFFCR      ; Print output buffer with a CR
@ISDEV:     ldy     CMDLIDX         ; Recover command line index
            jsr     GETNEXTNB       ; Get next non-blank from command line
            beq     @RETURN         ; No more channels, return
            jmp     ASSIGNCMD       ; Assign next channel
            ; Not reached
@RETURN:    rts

; Display current channel assignments
;
DISPLAYCH:  jsr     SETOUTBCH       ; Set output buffe rand output channel to
                                    ; console if not set
            ldx     #$00            ; Init channel number
            beq     @FIRST          ; Always jump
@NEXTCH:    ldx     TEMP4           ; Get current channel
            inx                     ; And go for next
            cpx     #$0A            ; Are we past the last one?
            bcs     @RETURN         ; Yes, just return
@FIRST:     stx     TEMP4           ; Save current channel
            lda     IOCHTBL,x       ; Get channel's device
            beq     @NEXTCH         ; If not assigned, go check next
            sta     DEVICE          ; Store file/device
            jsr     OUTSTR          ; Print assignment info:
            .byte   "CHAN. ", $00   ;

            lda     TEMP4           ; Get channel
            ldy     #$00            ; Convert to hex at the begining of the output buffer    
            jsr     HEXBYTE         ;
            lda     #$20            ; Add a space next
            sta     (OUTBUFP),y     ;
            iny                     ; Advance one pos
            ldx     TEMP4           ;   Get channel's device (again)
            lda     IOCHTBL,x       ; Why? it is already in DEVICE...
            bpl     @ISFILE         ; If it is a file, go display file name
            and     #$7F            ; It is a device, clear device flag
            lsr     a               ; and get device number
            tax                     ; And from it, the device name
            lda     DNT,x           ;  (which is a letter)
            sta     (OUTBUFP),y     ; And place into the output buffer
@PRINT:     iny                     ; Advance one pos and
            jsr     POUTBUFFCR02    ;   print output buffer to console, followed by a CR
            jmp     @NEXTCH

@ISFILE:    sty     SAVEY7          ; Save output buffer index
            ldx     TEMP4           ; Get channel number
            jsr     GETDEV          ; Get device or file from channel and store in DEVICE
            jsr     CPYCFINFO       ; Fills current FINFO structure for DEVICE
            jsr     ZEROFILEP       ; Zeroes file pointer
            dma     A, DIRBUF       ; Set transfer buffer to DIRBUF (Directory buffer)
            sta     CURFINFO+_DMABF ;
            jsr     GETFPSECT       ; Get sector of current file pointer
            jsr     READSECT        ; Read sector
            ldy     SAVEY7          ; Recover output buffer position
            ldx     #$00            ; Init offset to file names in directory
@GETCH:     lda     DIRBUF+1,x      ; Get file name char 
            sta     (OUTBUFP),y     ; and store to output buffer
            iny                     ; continue
            cmp     #'.'            ; until extension separator
            beq     @FEXT           ; then get the extension
            inx                     ; Else go get next char
            bne     @GETCH          ;
@FEXT:      lda     DIRBUF+2,x      ; Store extension name
            sta     (OUTBUFP),y     ;
            iny                     ; Advance to next pos
            lda     #':'            ; Print drive separator
            sta     (OUTBUFP),y     ;
            iny                     ; Advance
            lda     CURFINFO+_DRIVE ; Get drive number 
            cld                     ; Convert to ASCII
            clc                     ;
            adc     #'0'            ;
            sta     (OUTBUFP),y     ; Save to the buffer
            jmp     @PRINT          ; and print

@RETURN:    rts

            ; This block is just junk that was in the buffer when
            ; writing it to disk. I leave it to facilitate checksum
            ; comparisons with the original
            ;
            .byte   $20, $68, $D9, $A2, $01, $4C, $94, $F5
            .byte   $D7, $FE, $00, $FE, $60, $00, $00, $1E
