; da65 V2.19 - Git cf0688fc5
; Created:    2025-05-05 01:10:38
; Input file: overlay08.bin
; Page:       1


            .setcpu "6502"

            .include "codos.inc"
            .include "symbols.inc"

            .segment "overlays"

            .byte   $08             ; Overlay number

; FILES Command
;
; DESCRIPTION:  Display the name of every file on a disk
; SYNTAX:       FILES [<drive> ...]
; ARGUMENTS:    <drive> = selected disk drive number, 0 to 3. Defaults to 0.
; 
.proc FILES
GETDRV:     ldx     #$00            ; Get first non-blank character in the command line
            jsr     GETNEXTNB       ;
            beq     DEFDRV          ; null or semicolon, use default drive
            jsr     GETDRIVEOPND    ; Get drive from command line and check that it is opened
DEFDRV:     jsr     FILESDRV        ; Display files for drive X
            ldy     CMDLIDX         ; Get command line index
            jsr     GETNEXTNB       ; More arguments?
            beq     RETURN          ; No, we're done
            ldx     #$02            ; Yes, prints CR to console
            jsr     OUTCR           ;
            jmp     GETDRV          ; And get next drive

RETURN:     rts
.endproc

; Display files for drive X
;
.proc FILESDRV
.if ::CODOS2_VER <> 14
            jmp     FIX15_1         ; V 1.5 mod, sets CURRDRV and saves X
            ; FIX15_1 jumps back to CONT
.else
            stx     CURRDRV         ; Sets CURRDRV
.endif

CONT:       jsr     INILINE         ; Inits line. Clears line and sets FPERLIN
            jsr     DRVVALIDO       ; Checks that it is valid and open
            jsr     SETBATP         ; Set the BATP to the current drive's BAT
            ldy     #BAT::NENT      ; Offset in BAT to number of files
            lda     (BATP),y        ; Get it
            sta     NFILES          ; And store
            bne     SEARCH          ; If not empty, init search
            jsr     OUTSTR          ; Empty, just say so
            .byte   "(NONE)", $00
            ldx     CURRDRV         ; 
            rts

SEARCH:     lda     #$00            ; Init sector for search
            sta     SECTNUM         ;
            ldy     #$00            ; and diretory index

GETENTRY:
.if ::CODOS2_VER <> 14
            jmp     FIX15_2         ; V 1.5 mod, restores X, sets CURRDRV and incs SECTNUM
            ; FIX15_2 jumps back to CONT2
.else
            inc     SECTNUM         ; Directory starts at sector 1 of track 12
.endif

CONT2:      jsr     RDSECTNTR12     ; Read sector
            lda     #$01            ; Init pointer to current directory entry name
            bne     UPDP            ; Always jump
NEXT:       lda     DIRPOINT        ; Gets pointer to current directory entry
            clc                     ; Advance to next entry (dir entries are
            adc     #$10            ; 16 bytes long)
            bcs     GETENTRY        ; If not in sector, get next one
UPDP:       sta     DIRPOINT        ; Updates directory pointer
            tax                     ; 
CPCHAR:     lda     DIRBUF,x        ; Get first char of file name
            beq     NEXT            ; If null, deleted, go get next
            sta     (OUTBUFP),y     ; No, store in output buffer
            inx                     ; Advance one pos
            iny                     ;
            cmp     #'.'            ; Was last char the extension separator
            bne     CPCHAR          ; No, go copy into buffer

            lda     DIRBUF,x        ; Get extension char
            sta     (OUTBUFP),y     ; And store it in buffer
            iny                     ; Advance to next pos in output buffer
            dec     NFILES          ; More files left?
            beq     RETURN          ; No, print buffer and return
            jsr     ADD2LINE        ; Yes, adds it to line
            jmp     NEXT            ; And go for next extry
            ; Not reached

RETURN:     jsr     OUTCLEAR        ; Prints output buffer and clears it
            rts
.endproc

; Add file entry to the output buffer
;
.proc ADD2LINE
            tya                     ; Tabulate entry to 16 positions
            and     #$F0            ;
            clc                     ;
            adc     #$10            ;
            dec     FPERLIN         ; Room for more files on this line?
            beq     OUTCLEAR        ; No, rints output buffer and clears it
            ; Not reached
            tay                     ; Yes, update index
            rts                     ; and return
.endproc

; Prints output buffer and clears it
;
.proc OUTCLEAR
            jsr     POUTBUFFCR02    ; Print output buffer to console (length in Y)
            ; Fall through
.endproc

; Init line.
;
; Clears line and init FPERLIN
;
.proc INILINE
            ldy     #$47            ; Prints 72 spaces
            lda     #' '            ;
PRSP:       sta     (OUTBUFP),y     ;
            dey                     ;
            bpl     PRSP            ;
            iny                     ; Y back to 0
            lda     NUMFNAMES       ; Number of file names per line for FILES command
            sta     FPERLIN         ;
            rts
.endproc

FPERLIN:    .byte   $00             ; Files per line

.if CODOS2_VER <> 14
SAVEX:      .byte   $00

.proc FIX15_1
            stx     CURRDRV         ; Sets current drive    
            stx     SAVEX           ; And saves X
            jmp     FILESDRV::CONT  ; Continue where we left
            ; Not reached
.endproc

.proc FIX15_2
            inc     SECTNUM         ; Increments directory sector
            ldx     SAVEX           ; Recovers X
            stx     CURRDRV         ; Sets current drive
            jmp     FILESDRV::CONT2 ; And continue where we left
            ; Not reached
.endproc

            ; This block is just junk that was in the buffer when
            ; writing it to disk. I leave it to facilitate checksum
            ; comparisons with the original
            ;
.else
.ifdef mtu
            .byte             $BD, $2A, $E6, $91, $CD, $C8
            .byte   $20, $D1, $F9, $4C, $6C, $FE, $8C, $93
            .byte   $02, $AE, $90, $02, $20, $A8, $EF, $20
.endif
.endif

.ifdef mtu
            .byte   $E3, $EE, $20, $1C, $F8, $A9, $94, $85
            .byte   $E7, $20, $D0, $EF, $20, $B5, $ED, $AC
            .byte   $93, $02, $A2, $00, $BD, $01, $E5, $91
            .byte   $CD, $C8, $C9, $2E, $F0, $03, $E8, $D0
            .byte   $F3, $BD, $02, $E5, $91, $CD, $C8, $A9
            .byte   $3A, $91, $CD, $C8, $A5, $DD, $D8, $18
            .byte   $69, $30, $91, $CD, $4C, $A7, $FE, $60
            .byte   $20, $68, $D9, $A2, $01, $4C, $94, $F5
            .byte   $D7, $FE, $00, $FE, $60, $00, $00, $1E
.endif
            .end
