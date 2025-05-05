; da65 V2.19 - Git cf0688fc5
; Created:    2025-04-30 10:52:35
; Input file: overlay07.bin
; Page:       1


            .setcpu "6502"

            .include "codos.inc"
            .include "symbols.inc"

; CODOS symbols
LEFD0           := $EFD0
LF594           := $F594
LF81C           := $F81C
LF8A2           := $F8A2
LFD76           := $FD76

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
            jmp     DISPLAYCH       ; No arguments, diaplay current channel assignments

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
            jsr     PRNSTR          ; Print "NEW FILE"
            .byte   "NEW", 00       ;
            jmp     @SKIP           ;
            ; Not reached

@FEXIST:    jsr     PRNSTR          ; Print "OLD FILE "
            .byte   "OLD", $00      ;
                                    ;
@SKIP:      jsr     PRNSTR          ;
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

DISPLAYCH:  jsr     LFD76
            ldx     #$00
            beq     LFE74
LFE6C:      ldx     TEMP4
            inx
            cpx     #$0A
            bcs     LFEEF
LFE74:      stx     TEMP4
            lda     $E652,x
            beq     LFE6C
            sta     DEVICE
            jsr     PRNSTR
            .byte   "CHAN. ", $00

            lda     TEMP4
            ldy     #$00

            jsr     LF8A2
            lda     #$20
            sta     (OUTBUFP),y
            iny
            ldx     TEMP4
            lda     $E652,x
            bpl     LFEAE
            and     #$7F
            lsr     a
            tax
            lda     $E62A,x
            sta     (OUTBUFP),y
LFEA7:      iny
            jsr     POUTBUFFCR02    ; Print output buffer to console, followed by a CR
            jmp     LFE6C

LFEAE:      sty     $0293
            ldx     TEMP4
            jsr     GETDEV          ; Get device or file from channel and store in DEVICE
            jsr     CPYCFINFO       ; Fills current FINFO structure for DEVICE
            jsr     LF81C
            lda     #$94
            sta     $E7
            jsr     LEFD0
            jsr     READSECT
            ldy     $0293
            ldx     #$00
LFECC:      lda     $E501,x
            sta     (OUTBUFP),y
            iny
            cmp     #$2E
            beq     LFED9
            inx
            bne     LFECC
LFED9:      lda     $E502,x
            sta     (OUTBUFP),y
            iny
            lda     #$3A
            sta     (OUTBUFP),y
            iny
            lda     $DD
            cld
            clc
            adc     #$30
            sta     (OUTBUFP),y
            jmp     LFEA7

LFEEF:      rts

            ; This block is just junk that was in the buffer when
            ; writing it to disk. I leave it to facilitate checksum
            ; comparisons with the original
            ;
            .byte   $20, $68, $D9, $A2, $01, $4C, $94, $F5
            .byte   $D7, $FE, $00, $FE, $60, $00, $00, $1E
