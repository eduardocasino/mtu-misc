; da65 V2.19 - Git cf0688fc5
; Created:    2025-05-05 12:21:41
; Input file: kk5.bin
; Page:       1


            .setcpu "6502"

            .include "symbols.inc"
            .include "codos.inc"

            .segment "overlays"

            .byte   $05             ; Overlay number

; TYPE Command
;
; DESCRIPTION:  Display, print, or create a text file 
;                    <device>        [<dest device>       ]
; SYNTAX:       TYPE <file> :<drive> [<dest file> :<drive>]
;                    <channel>       [<dest channel>      ]
; ARGUMENTS:    <device> = single character source device name
;               <file> = file name to type
;               <drive> = disk drive, 0 to 3. Defaults to the current default drive
;               <channel> = pre-assigned source channel number, 0 to 9
;               <dest device> = output device name. Defaults to Console ("C")
;               <dest file> = file to receive output from TYPE
;               <dest channel> = pre-assigned channel to recieve output from TYPE
;
.proc TYPE
            jsr     ISNUM           ; Check if first char of args is numeric
            bcs     NOTNUM          ; No, it is not
            jsr     GETCHANN        ; Get channel from command line
            stx     SCHANNEL        ; And store it
            jmp     GDEST           ; Go get destination

NOTNUM:     jsr     GETDEVORFIL     ; Get device or file from command line. If it
                                    ; is a file, sets CURRDRV, FNAMBUF
                                    ; If it is a device, CURRDRV contains the device name
            ldx     #$00            ; Assign channel 0
            stx     SCHANNEL        ;
            lda     CURRDRV         ; Check if a valid drive number
            cmp     NDRIVES         ;
            bcs     ASGN            ; No, then it is a device
            jsr     FOPEN0          ; Yes, Assigns channel 0 to file (fails if not found)
            jmp     GDEST           ; Go get destination

ASGN:       jsr     ASSIGN0         ; Assign channel 0 to device

GDEST:      ldy     CMDLIDX         ; Get command line index
            jsr     GETNEXTNB       ; Get next non-blank
            bne     CNTDST          ; Are there any? Then continue to get dest
            lda     #'C'            ; No, then dest is Console
            sta     CURRDRV         ;
            jmp     GETCH           ; Go get destination channel

CNTDST:     jsr     ISNUM           ; Check if argument is numeric
            bcs     DNOTNUM         ; No, it's not
            jsr     GETCHANN        ; Get channel from command line
            stx     DCHANNEL        ; And store it
            jmp     PRNT            ; And print source to dest

DNOTNUM:    jsr     GETDEVORFIL     ; Get device or file from command line. If it
                                    ; is a file, sets CURRDRV, FNAMBUF
                                    ; If it is a device, CURRDRV contains the device name

GETCH:      ldx     #$09            ; Search for an empty slot in IOCHTBL
LOOP:       lda     IOCHTBL,x       ;
            beq     ASGND           ; Found, go and assign it
            dex                     ; Loop until no more entries
            bpl     LOOP            ;
            jsr     ERROR34         ; Not enough channels are free for specified function
            ; Not reached

ASGND:      stx     DCHANNEL        ; Assign channel X to file or device
            jsr     ASSIGN          ;

PRNT:       ldx     SCHANNEL        ; Get source channel
            jsr     GETLINE         ; Input line of text from channel
            bcs     FINISH          ; If no more lines, finish
            sta     LINELEN         ; Save line length
COPY:       lda     (INPBUFP),y     ; Copy char by char from input to output buffer
            sta     (OUTBUFP),y     ;
            iny                     ;
            cpy     LINELEN         ; Repeat until line length
            bcc     COPY            ;
            ldy     LINELEN         ; Print output buffer to dest channel,
            ldx     DCHANNEL        ;   followed by a CR
            jsr     POUTBUFFCR      ;
            jmp     PRNT            ; Print next line

FINISH:     ldx     DCHANNEL        ; Update destination file size
            jsr     FTRUNC          ;
            ldx     DCHANNEL        ; Free destination channel
            jsr     FREECH          ;
            ldx     SCHANNEL        ; Free source channel
            jmp     FREECH          ;
.endproc

SCHANNEL:   .byte   $00             ; Source channel for TYPE command
DCHANNEL:   .byte   $00             ; Destination channel for TYPE command
LINELEN:    .byte   $00             ; Lenght of current input line

            .byte   "COPYRIGHT (C) 1980 MICRO TECHNOLOGY UNLIMITED."


; BEGINOF Command
;
; DESCRIPTION:  Position a file associated with a given channel to beginning-of-data
;
; SYNTAX:       BEGINOF <channel> ...
;
; ARGUMENTS:    <channel> = channel number, previously assigned to a file
;
.proc BEGINOF
            jsr     GETCHANN        ; Get channel from command line
            jsr     FREWIND         ; Set file pos at 0
            ldy     CMDLIDX         ; Update command line ndex
            jsr     GETNEXTNB       ; Get next non-blank
            bne     BEGINOF         ; If more arguments, start again
            rts
.endproc

; ENDOF Command
;
; DESCRIPTION:  Position a file associated with a given channel to end-of-file
;
; SYNTAX:       ENDOF <channel> ...
;
; ARGUMENTS:    <channel> = channel number, previously assigned to a file
;
.proc ENDOF
            jsr     GETCHANN        ; Get channel from command line
            jsr     FEND            ; Set file pos bigger than maximum file size
            ldy     CMDLIDX         ; Update command line ndex
            jsr     GETNEXTNB       ; Get next non-blank
            bne     ENDOF           ; If more arguments, start again
            rts
.endproc

            ; This block is just junk that was in the buffer when
            ; writing it to disk. I leave it to facilitate checksum
            ; comparisons with the original
            ;
            .byte                  $E6, $4D, $D8, $E6, $8D
            .byte   $E0, $BF, $A2, $00, $A1, $C7, $AE, $D8
            .byte   $E6, $8E, $E0, $BF, $60, $E6, $C7, $D0
            .byte   $02, $E6, $C8, $CE, $FE, $FE, $D0, $01
            .byte   $C8, $CE, $FD, $FE, $60, $00, $00, $1E
