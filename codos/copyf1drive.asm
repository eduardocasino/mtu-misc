; da65 V2.18 - Ubuntu 2.19-1
; Created:    2025-05-27 13:06:07
; Input file: copyf1drive_15.bin
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
            lda     #$00            ; Unprotect SYSRAM
            sta     HSRCW           ;
            cld
            sta     MORETORD        ; Clear the "More bytes to read" flag
            jsr     DEFSETINPB      ; Set input buffer
            jsr     DEFSETOUTB      ; Set output buffer
            lda     ODRIVES         ; Check if drive 0 is open
            bpl     NOTOPEN         ; No, continue

CPYCHUNK:   ldx     #$00            ; Close drive 0
            jsr     CLOSEDRV        ;
NOTOPEN:    jsr     ASKSOURCE       ; Print message asking for source disk
            bit     MORETORD        ; Check if more bytes to read from file
            bmi     READCHUNK       ; Yes, we already were here. Go read chunk from file        
            jsr     OUTSTR          ; No, ask for file name
            .byte   $0D, "FILE (OR CR IF DONE)?= ", $00
            ldx     #$01            ; Read line from console
            jsr     GETLINE         ;
            jsr     GETNEXTNB       ; Get next non-blank
            bne     GETFIL          ; If some, go get file name
            ldx     #$00            ; None, open drive 0 and return to cmd proessor
            jsr     OPENDRV         ;
            rts

GETFIL:     ldx     #$00            ; Open drive 0
            jsr     OPENDRV         ;
            ldy     #$00            ; Ge file and drive from command line
            jsr     GETFILNDRV      ;
            jsr     FOPEN0          ; Assign file to channel 0
.if ::CODOS2_VER <> 14
            ; CODOS 1.5 fix
            ;
            ; Gets and stores the terminator byte of the source file date

            ldy     #FHDR::DATE+DATELEN
            lda     (CURFINFO+FINFO::BUFF),y
            sta     DATETERM
.endif
            jmp     SETBUFF         ; Jump to set copy file buffer

READCHUNK:  jsr     OUTSTR
            .byte   $0D, "CR TO CONTINUE.", $00
            ldx     #01             ; Get char from command line
            jsr     GETCHAR         ;
            cmp     #$0D            ; Is it a CR
            beq     CNTREAD         ; Yes, continue
            rts                     ; No, abort

CNTREAD:    ldx     #$00            ; Open drive 0
            jsr     OPENDRV         ;
            jsr     ASSIGN0         ; Assign file to channel 0
            ldx     #$02            ; Update file position
UPDPOS:     lda     SFILEPOS,x      ;
            sta     FILEPOS,x       ;
            dex                     ;
            bpl     UPDPOS          ;
            ldx     #$00            ; Advance to the file position
            jsr     FSEEK           ;

SETBUFF:    ldx     #$03            ; Set buffer and buffer size to large buff address
CPYBYTE:    lda     LBUFADDR,x      ;
            sta     MEMBUFF,x       ;
            dex                     ;
            bpl     CPYBYTE         ;
            lda     MORETORD        ; Copy "more bytes to read flag"
            sta     NOT1STCHUNK     ;
            ldx     #$00            ; Clear "more bytes to read" flag
            stx     MORETORD        ;
            jsr     GETMBUFF        ; Read file chunk into large transient buffer
            bcc     BYTESREAD       ; Some bytes read, 
            jmp     CPYCHUNK        ; No bytes read, check if more files

BYTESREAD:  lda     MEMCOUNT        ; Get number of bytes read
            sta     BYTREAD         ; Save it
            cmp     LBUFSIZE        ; And check if it has filled the whole buffer
            lda     MEMCOUNT+1      ;
            sta     BYTREAD+1       ;
            sbc     LBUFSIZE+1      ;
            bcc     CHKCOMPLTD      ; If read less than LBUFSIZE bytes, check if file
                                    ; completed
            lda     #$80            ; Set "More bytes to read" flag 
            sta     MORETORD        ;
            lda     CURFINFO+FINFO::FPOS
            sec                     ; Store current position
            sbc     #FHDRLEN        ; Discard file header
            sta     SFILEPOS        ;
            lda     CURFINFO+FINFO::FPOS+1
            sbc     #$00            ;
            sta     SFILEPOS+1      ;
            lda     CURFINFO+FINFO::FPOS+2
            sbc     #$00            ;
            sta     SFILEPOS+2      ;

CHKCOMPLTD: ldx     #$00            ; Close drive 0
            jsr     CLOSEDRV        ;
            jsr     ASKDEST         ; Ask for detination disk
            ldx     #$00            ; Open drive 0
            jsr     OPENDRV         ;
            jsr     ASSIGN0         ; Assign file to channel 0
            bit     NOT1STCHUNK     ; Check if it is the first file chunk
.if ::CODOS2_VER = 14
            bpl     WRTECHUNK       ; Yes, go write data to destination
.else
            bmi     UPDPOSD         ; No, go update destination file pos
            bit     ASSIGNFLAG      ; Yes, check if an existing file
            bpl     CNTWRITE        ; No, continue
            bit     SAVEOVERWR      ; Check if overwrite allowed
            bmi     CNTWRITE        ; Yes, continue
            jmp     FEXISTS         ; No, print error and continue to next file

CNTWRITE:   lda     DATETERM        ; Updates the date terminator byte in dest file
            ldy     #FHDR::DATE+DATELEN
            sta     (CURFINFO+FINFO::BUFF),y
            jmp     WRTECHUNK
.endif

UPDPOSD:    ldx     #$02            ; Update destination file position
CPPOSBYT:   lda     DFILEPOS,x      ;
            sta     FILEPOS,x       ;
            dex                     ;
            bpl     CPPOSBYT        ;
            ldx     #$00            ; And advance to new pos
            jsr     FSEEK           ;

WRTECHUNK:  lda     LBUFADDR        ; Set buffer to large transient buffer
            sta     MEMBUFF         ;
            lda     LBUFADDR+1      ;
            sta     MEMBUFF+1       ;
            lda     BYTREAD         ; Set count to bytes read
            sta     MEMCOUNT        ;
            lda     BYTREAD+1       ;
            sta     MEMCOUNT+1      ;
            ldx     #$00            ; Output data to dest file (channel 0)
            jsr     OUTMBUFF        ;
            lda     CURFINFO+FINFO::FPOS
            sec                     ; Update dest file pos and store it 
            sbc     #FHDRLEN        ; Discard file header
            sta     DFILEPOS        ;
            lda     CURFINFO+FINFO::FPOS+1
            sbc     #$00            ;
            sta     DFILEPOS+1      ;
            lda     CURFINFO+FINFO::FPOS+2
            sbc     #$00            ;
            sta     DFILEPOS+2      ;
.if ::CODOS2_VER <> 14
            bit     MORETORD        ; Check if more bytes to read
            bmi     GCPYCHUNK       ; Yes, go get them
            ldx     #$00            ; No, truncate destination file
            jsr     FTRUNC          ;

GCPYCHUNK:  jmp     CPYCHUNK        ; Jump to copy chunk  

FEXISTS:    jsr     OUTSTR
            .byte   $0D, "***ERROR, NEW FILE NAME IS ALREADY ON DESTINATION DISK."
            .byte   $0D, $00
.endif
            jmp     CPYCHUNK        ; Jump for next file
.endproc

; Print message asking for source disk
;
.proc ASKSOURCE
            jsr     OUTSTR
            .byte   $0D, "PUT SOURCE DISK IN.", $00
            rts
.endproc

; Print message asking for destination disk and wait for input
;
.proc ASKDEST
            jsr     OUTSTR
            .byte   $0D, "PUT DEST. DISK IN,"
            .byte   $0D, "CR WHEN READY.?=", $00

            ldx     #01
            jsr     GETCHAR
            rts
.endproc

PROG_SIZE = * - START

            .bss

.if CODOS2_VER <> 14
DATETERM:   .res    1               ; Date terminator in source file
.endif
MORETORD:   .res    1               ; Set if there are more bytes to read from file
NOT1STCHUNK:.res    1               ; Set if not the first chunk written
SFILEPOS:   .res    3               ; Source file position (relative to data start)
DFILEPOS:   .res    3               ; Destination file position (relative to data start)
BYTREAD:    .res    2               ; Bytes read

            .end
