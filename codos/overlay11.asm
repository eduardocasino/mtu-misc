; da65 V2.19 - Git cf0688fc5
; Created:    2025-05-05 12:23:08
; Input file: kk11.bin
; Page:       1


            .setcpu "6502"

            .include "symbols.inc"
            .include "codos.inc"

            .segment "overlays"

            .byte   $0B             ; Overlay number

; RENAME Command
;
; DESCRIPTION:  Change the name of an existing file
;
; SYNTAX:       RENAME <file>[:<drive>] <newfile> 
;
; ARGUMENTS:    <file> = the existing file name
;               <drive> = disk drive number for the file. Defaults to current
;                         default drive, usually 0
;               <newfile> = new file name. 
;
.proc RENAME
            jsr     GETFILNDRV      ; Get file and drive from the command line
            ldx     CURRDRV         ; Get and store the current drive
            stx     RENAMEDRV       ;
            jsr     FOPEN0          ; Assign channel 0 to file
                                    ; Get file flags
            lda     CURFINFO+FINFO::FLAGS
            and     #FLLOCKED       ; Check if locked
            beq     CONT            ; No, continue
            jsr     ERROR07         ; Locked file violation
            ; Not reached

CONT:       ldy     CMDLIDX         ; Get command line index
            jsr     GETFILNDRV      ; Get destination file and drive
            ldx     RENAMEDRV       ; Set current drive to the source file's
            stx     CURRDRV         ;
            jsr     FEXIST          ; Check if destination file exists
            bne     NEXIST          ; No, continue
            jsr     ERROR25         ; New file name is already on selected diskette
            ; Not reached

NEXIST:     ldx     #$00            ; Rewind source file
            jsr     FREWIND         ;
            ldy     #FHDR::NSEC     ; Get the sector on track 12 of the directory entry
            lda     (CURFINFO+FINFO::BUFF),y
            jsr     RDSECTATR12     ; Read sector A from track 12
            ldy     #FHDR::NENT     ; Get the entry offset in the sector
            lda     (CURFINFO+FINFO::BUFF),y
            tax                     ;
            sta     DIRPOINT        ; Store it into DIRPOINT
            ldy     #DIRE::FNAM     ; Compare names
CMPLOOP:    lda     DIRBUF,x        ; 
            cmp     #'.'            ; Have we reached the extension?
            beq     RENAM           ; Yes, jump
                                    ; No, char is thr same?
            cmp     (CURFINFO+FINFO::BUFF),y
            beq     SAME            ; Yes, continue
            jsr     ERROR50         ; System crash: Directory redundancy check failed
            ; Not reached

SAME:       iny                     ; Advance to next char
            inx                     ;
            jmp     CMPLOOP         ; And continue loop

            ; Rename dir entry

RENAM:      ldx     DIRPOINT        ; Get pointer to entry in sector
            ldy     #$00            ; Init DIRENT+DIRE::FNAM index
                                    ; Copy new name
RNLOOP:     lda     DIRENT+DIRE::FNAM,y
            sta     DIRBUF,x        ;
            inx                     ;
            iny                     ;
            cpy     #FNAMLEN        ; Have we reached the file name length?
            bne     RNLOOP          ; No, continue copying

            ; Rename dir entry copy in file header

            ldx     #$00            ;
            ldy     #DIRE::FNAM     ;
RNLOOP2:    lda     DIRENT+DIRE::FNAM,x
            sta     (CURFINFO+FINFO::BUFF),y
            iny                     ;
            inx                     ;
            cpx     #FNAMLEN        ; Have we reached the file name length?
            bne     RNLOOP2         ; No, continue copying
            jsr     CPYCFINFO       ; Copies file info structure to CURFINFO struct
                                    ; in page zero
            jsr     WRFPSECT        ; Update file header info on disk
            jsr     WRTRCK12        ; Update dir entry on disk
            jmp     FREECH0         ; Free channel and return
.endproc

RENAMEDRV:  .byte   $00

            ; This block is just junk that was in the buffer when
            ; writing it to disk. I leave it to facilitate checksum
            ; comparisons with the original
            ;
            .byte                                      $C6 
            .byte   $C6, $C4, $CE, $FC, $FE, $F0, $09, $20
            .byte   $CC, $FE, $88, $CE, $FC, $FE, $D0, $F7
            .byte   $20, $CC, $FE, $60, $A0, $00, $AE, $FD
            .byte   $FE, $F0, $0F, $20, $E3, $FE, $C8, $D0
            .byte   $FA, $E6, $C4, $E6, $C8, $CE, $FD, $FE
            .byte   $D0, $F1, $AE, $FC, $FE, $F0, $09, $20
            .byte   $E3, $FE, $C8, $CE, $FC, $FE, $D0, $F7
            .byte   $20, $E3, $FE, $60, $A5, $C5, $38, $E5
            .byte   $C3, $8D, $FC, $FE, $A5, $C6, $E5, $C4
            .byte   $8D, $FD, $FE, $60, $AE, $FA, $FE, $8E
            .byte   $E0, $BF, $B1, $C5, $AE, $FB, $FE, $8E
            .byte   $E0, $BF, $91, $C3, $AE, $D8, $E6, $8E
            .byte   $E0, $BF, $60, $AE, $FA, $FE, $8E, $E0
            .byte   $BF, $B1, $C3, $AE, $FB, $FE, $8E, $E0
            .byte   $BF, $91, $C7, $AE, $D8, $E6, $8E, $E0
            .byte   $BF, $60, $7F, $7F, $60, $00, $00, $1E
