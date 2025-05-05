; da65 V2.19 - Git cf0688fc5
; Created:    2025-04-30 10:05:12
; Input file: overlay01.bin
; Page:       1


            .setcpu "6502"

            .include "symbols.inc"

; TODO: Move to include files
;
LD968       := $D968
LD9B7       := $D9B7
LF594       := $F594

            .segment "overlays"

            .byte $01               ; Overlay number

; SET Command
;
; DESCRIPTION:  Set the value of memory locations
;                          {"character" ...}
; SYNTAX:       SET <from>={<value>        }
;                          {'character' ...}
; ARGUMENTS:    <from> = address at which to deposit the first value
;               <value> = numeric value to be deposited
;               <character> = an ASCII character to be deposited
;
SETCMD:     jsr     LD9B7
            jsr     GETNEXTNB
            bne     @CONT
            jsr     ERROR24         ; <value> missing or illegal
            ; Not reached
@CONT:      cmp     #$3D
            bne     LFE13
            jsr     GETNEXTNB1
LFE13:      sty     $EB
            cmp     #$27
            beq     LFE1D
            cmp     #$22
            bne     LFE57
LFE1D:      sta     $E792
            sty     $0298
LFE23:      jsr     GETNEXTCH1
            sty     $EB
            bne     LFE32
            cmp     $E78D
            beq     LFE23
            jsr     ERROR26         ; Missing or illegal character string delimiter (' , ")
LFE32:      cmp     $E792
            bne     LFE23
            dey
            cpy     $0298
            bne     LFE40
            jsr     ERROR26         ; Missing or illegal character string delimiter (' , ")
LFE40:      ldy     $0298
LFE43:      jsr     GETNEXTCH1
            cpy     $EB
            beq     LFE50
            jsr     LFE65
            jmp     LFE43

LFE50:      iny
LFE51:      jsr     GETNEXTNB
            bne     LFE13
            rts

LFE57:      jsr     LFBCC
            bcs     LFE5F
            jsr     ERROR24
            ; Not reached

LFE5F:      jsr     LFE65
            jmp     LFE51

LFE65:      sta     $0296
            lda     $E6D4
            bne     LFE73
            jsr     LFE9C
            lda     $E6D4
LFE73:      eor     $E6D8
            sta     $BFE0
            lda     $0296
            stx     $0296
            ldx     #$00
            sta     ($C3,x)
            cmp     ($C3,x)
            php
            lda     $E6D8
            sta     $BFE0
            plp
            beq     LFE92
            jsr     ERROR23         ; Memory verify failure during SET or FILL
LFE92:      inc     $C3
            bne     LFE98
            inc     $C4
LFE98:      ldx     $0296
            rts

LFE9C:      lda     $C4
            cmp     #$E0
            bcc     LFEA5
            jsr     LFEC4
LFEA5:      cmp     #$02
            bcs     LFEC3
            cmp     #$01
            bcc     LFEBA
            txa
            tsx
            cpx     $C3
            bcs     LFEB6
            jsr     LFEC4
LFEB6:      tax
            jmp     LFEC3

LFEBA:      lda     $C3
            cmp     #$C1
            bcc     LFEC3
            jsr     LFEC4
LFEC3:      rts

LFEC4:      bit     IGNORWRP
            bmi     LFECC
            jsr     ERROR17         ; Reserved or protected memory violation
LFECC:      rts


; UNPROTECT Command
;
; DESCRIPTION:  Disable the hardware write-protect on the top 8K of RAM on the disk
;               controller board (addresses $E000-$FFFF in bank 0) and disable the
;               system reserved memory checking for SET and FILL commands
; SYNTAX:       UNPROTECT
; ARGUMENTS:    None.
; 
UNPROTECT:  nop
            lda     #$80            ;
            sta     UNPROTFLG       ; Set unprotect flag
            sta     IGNORWRP        ; Set ignore write protection flag
            rts


; PROTECT Command
;
; DESCRIPTION:  Enable the memory-protect hardware on the upper 8k block of memory on
;               the disk controller board (addresses $E000-$FFFF in bank 0) and enable
;               the reserved-memory checking for SET and FILL commands
; SYNTAX:       PROTECT
; ARGUMENTS:    None.
; 
PROTECT:    lda     #$00            ; Clear unprotect flag
            sta     UNPROTFLG       ;
            rts

            ; This block is just junk that was in the buffer when
            ; writing it to disk. I leave it to facilitate checksum
            ; comparisons with the original
            ;
            .byte   $CD, $C8, $A9
            .byte   $3A, $91, $CD, $C8, $A5, $DD, $D8, $18 
            .byte   $69, $30, $91, $CD, $4C, $A7, $FE, $60 
            .byte   $20, $68, $D9, $A2, $01, $4C, $94, $F5 
            .byte   $D7, $FE, $00, $FE, $60, $00, $00, $1E
