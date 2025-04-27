; da65 V2.19 - Git cf0688fc5
; Created:    2025-04-23 17:47:56
; Input file: command.bin
; Page:       1


; TODO: Move this to an include file
;
SVIA1PORT       = $BFE0         ; System 1 6522 System control port data register
SVIA1DIR        = $BFE2         ; System 1 6522 System control port direction register
BNKCTL          = SVIA1PORT     ; System 1 6522 (Bank control data register)

HSRCW           = $FFE8



LE77D           := $E77D
LE7CB           := $E7CB
LE9AF           := $E9AF
LE9D7           := $E9D7
LE9E3           := $E9E3
LE9ED           := $E9ED
LE9F7           := $E9F7
LE9F9           := $E9F9
LE9FB           := $E9FB
LE9FD           := $E9FD
LE9FF           := $E9FF
LEA05           := $EA05
LEA0B           := $EA0B
LEB2F           := $EB2F
LEF33           := $EF33
LEF90           := $EF90
LEF93           := $EF93
LEF9F           := $EF9F
LF0D6           := $F0D6
LF40B           := $F40B
LF42D           := $F42D
LF4A0           := $F4A0
LF592           := $F592
LF5C3           := $F5C3
LF5C5           := $F5C5
LF62D           := $F62D
LF829           := $F829
LF882           := $F882
LF892           := $F892
LF8CE           := $F8CE
LF924           := $F924
LF930           := $F930
LFBE1           := $FBE1
LFC62           := $FC62
LFD05           := $FD05
LFD5F           := $FD5F
LFD76           := $FD76
LFF0E           := $FF0E

            .importzp MEMBUFF, BUFFERP1, ERRNUM, SVCENB

            .segment "cmdproc"

            .export CMDPROC

CMDPROC:    cld
            lda     #$00
            sta     HSRCW

            ; Init flags

            ldx     #$05
@LOOP:      sta     LE77D,x
            dex
            bpl     @LOOP
        
            sta     ERRNUM
            sta     SVCENB
        
            jsr     INIMMAP
            jsr     CPYBNKSW
            lda     $E778
            sta     $E777
            jsr     LFD76
            jsr     LFD5F
            lda     $E653
            cmp     #$82            ; Console output?
            bne     LD837           ; Nope
            jsr     PRNSTR          ; Yes, print CODOS prompt
            .byte   $0D, "CODOS> ", $00

LD837:      ldx     #$01
            jsr     GETLINE
            bcc     LD846
            ldx     #$01
            jsr     LF5C5
            jmp     CMDPROC

            ; Command processor entry

.export LD846
LD846:      jsr     LF925
            beq     CMDPROC
            sec
            ror     LE77D
            sty     $EB
            jsr     LF882
            bcs     LD870
LD856:      iny
            jsr     LF930
            beq     LD870
            jsr     LF892
            bcc     LD856
            cmp     #$21
            beq     LD870
            cmp     #$20
            bne     LD86C
            jsr     LF925
LD86C:      cmp     #$3A
            beq     LD89F
LD870:      ldy     $EB
            jsr     LD8E8
            beq     LD89F
            jsr     LEF33
            txa
            asl     a
            tax
            lda     LDC99,x
            sta     LE7CB
            lda     LDC99+1,x
            sta     LE7CB+1
            jsr     LF925
            php
            sty     $EB
            bit     $E780
            bmi     LD89B
            plp
            jsr     LD89C
            jmp     CMDPROC

LD89B:      plp
LD89C:      jmp     (LE7CB)

LD89F:  ldy     #$00
        jsr     LD968
        jsr     LF925
        sty     $EB
        sec
        ror     $E77E
        jsr     LF592
        clc
        ror     $E77E
        lda     #$00
        sta     $E77B
        sta     $E77C
        ldx     #$00
        txa
        jsr     LFD05
        bcc     LD8C7
        jsr     LE9FB
LD8C7:  lda     $E721
        sta     $DA
        lda     $E722
        sta     $DB
        lda     $E6D2
        sta     PRGBANK
        sta     $E6CF
LD8DA:  ldx     #$00
        txa
        jsr     LFD05
        bcc     LD8DA
        jsr     LF5C3
        jmp     LEB2F

LD8E8:  sty     $EB
        ldx     #$00
        stx     $0282
        beq     LD8FD
LD8F1:  ldx     $0281
LD8F4:  inx
        lda     LDBB9,x
        cmp     #$20
        bcs     LD8F4
        inx
LD8FD:  stx     $0281
        lda     LDBB9,x
        beq     LD936
        inc     $0282
        ldy     $EB
LD90A:  lda     ($CB),y
        jsr     LF892
        bcs     LD91A
        cmp     LDBB9,x
        bne     LD8F1
        inx
        iny
        bne     LD90A
LD91A:  cmp     #$21
        bne     LD92D
        iny
LD91F:  lda     LDBB9,x
        cmp     #$20
        bcc     LD929
        inx
        bne     LD91F
LD929:  ldx     $0282
        rts

LD92D:  lda     LDBB9,x
        cmp     #$20
        bcc     LD929
        bcs     LD8F1
LD936:  ldx     #$00
        rts

LD939:  jsr     LFBCC
        bcs     LD941
        jsr     LEA05
LD941:  sta     $0280
        tax
        sty     $EB
        cmp     #$0A
        bcc     LD94E
        jsr     LEA05
LD94E:  rts

LD94F:  jsr     LD955
        jmp     LEF93

LD955:  jsr     LF8CE
        bcs     LD95D
        jsr     LEA0B
LD95D:  lda     BUFFERP1
        tax
        stx     $E6DC
        sty     $EB
        jmp     LEF9F

LD968:  lda     $CB
        sta     $C7
        lda     $CC
        sta     $C8
        jsr     LF925
        jsr     LF829
        bcc     LD97B
        jsr     LE9FD
LD97B:  jsr     LF925
        cmp     $E790
        bne     LD989
        jsr     LF924
        jmp     LD94F

LD989:  ldx     $E796
        jsr     LEF90
        stx     $E6DC
        sty     $EB
        rts

        jsr     LF925
        jsr     LF882
        bcc     LD9A0
        jsr     LE9FF
LD9A0:  iny
        jsr     LF930
        jsr     LF892
        dey
        bcs     LD9AD
        jmp     LD968

LD9AD:  jsr     LF930
        sta     $E6DC
        iny
        sty     $EB
        rts

; Get Address and bank from command line + 2
;
LD9B7:      ldx     #$02
            ; Fall through

; Get Address and bank from command line + X
; Returns addres in MEMBUFF, bank in NEWBNK
;
LD9B9:      jsr     LFBE1
            bcs     @LD9C1
            jsr     LE9F9
@LD9C1:     sty     $EB
            lda     #$00            ; Some initialization
            sta     NEWBNK          ;
            sta     CHGBNKFLG       ; Clears change bank flag
            jsr     LF925
            beq     @RETURN
            cmp     $E790
            bne     @RETURN
            jsr     LF924
            sty     $EB
            beq     @LD9E6
            iny
            sec
            sbc     #$30
            bcc     @LD9E6
            cmp     #$04
            bcc     @LD9E9
@LD9E6:     jsr     LE9AF
@LD9E9:     sta     NEWBNK          ; New bank
            sec                     ; And sets flag
            ror     CHGBNKFLG       ;
@RETURN:    rts

LD9F1:  ldx     #$04
        jsr     LFBE1
        bcs     LD9FB
        jsr     LE9F7
LD9FB:  sty     $EB
        rts

            .export LD9FE

LD9FE:      lda     #$00
            sta     CHGBNKFLG
            sta     NEWBNK
            jsr     LF925
            beq     LDA16
            jsr     LD9B7           ; Get address and bank
            lda     MEMBUFF
            sta     $DA
            lda     MEMBUFF+1
            sta     $DB
LDA16:      rts

LDA17:  lda     #$00
        sta     $E77B
        jsr     LF925
        cmp     #$3D
        bne     LDA2D
        iny
        ldx     #$08
        jsr     LD9B9
        sec
        ror     $E77B
LDA2D:  sty     $EB
        rts

LDA30:  lda     #$00
        sta     $E77C
        jsr     LF925
        cmp     #$3D
        bne     LDA4B
        iny
        ldx     #$06
        jsr     LFBE1
        bcs     LDA47
        jsr     LE9ED
LDA47:  sec
        ror     $E77C
LDA4B:  sty     $EB
        rts

; BP Command
;
; DESCRIPTION:  Set a program breakpoint for machine language debugging.
; SYNTAX:       BP [<addr>]
; ARGUMENTS:    <addr>=address of first byte of instruction at which breakpoint is
;               to be set
;
; If no arguments are provided, clears all break points
; 
BREAKP:     jsr     LF925           ; Get next char from command line       
            bne     @GETARGS        ; Jump if there are any arguments

            ldx     #$02            ; No arguments, clear all break points
@LOOP:      lda     BPBANK,x        ; Get bank
            bmi     @NEXT           ; If bit 7 set, it means "not set"
            lda     BPADDRLO,x      ; Get BP address
            sta     MEMBUFF         ;
            lda     BPADDRHI,x      ;
            sta     MEMBUFF+1       ;
            ldy     #$00
            lda     BPBANK,x        ; Get bank
            eor     DEFBNK          ; And switch to it 
            sta     BNKCTL          ;
            lda     (MEMBUFF),y     ; Get OP at BP address
            bne     @CONT           ; If not a BRK, do nothing
            lda     BPOP,x          ; Restore original OP
            sta     (MEMBUFF),y     ;
@CONT:      lda     DEFBNK          ; Switch back to current bank
            sta     BNKCTL          ;
            lda     #$FF            ; Invalidate/clear BP
            sta     BPBANK,x        ;
@NEXT:      dex
            bpl     @LOOP
            rts

@GETARGS:   jsr     LD9B7
            ldy     #$00

            ldx     #$02            ; Search for a free BP slot
@LOOP2:     lda     BPBANK,x        ; Get bank
            bpl     @NOTFREE        ; This BP slot is not free

@SETBP:     lda     MEMBUFF         ; Set BP address
            sta     BPADDRLO,x      ;
            lda     MEMBUFF+1       ;
            sta     BPADDRHI,x      ;
            lda     NEWBNK          ; Set BP memory bank
            sta     BPBANK,x        ;
            eor     DEFBNK          ; And switch to it
            sta     BNKCTL          ;
            lda     (MEMBUFF),y     ; Get current opcode at BP address
            sta     BPOP,x          ; and save it
            tya                     ; Y is currently 0 (BRK)
            sta     (MEMBUFF),y     ; Set BRK at BP address
@RSTBNK:    lda     DEFBNK          ; Switch bank bank
            sta     BNKCTL          ;
            rts

@NOTFREE:   cmp     PRGBANK         ; Is it in the current program bank?
            bne     @NEXT2          ; No, go try next slot
            lda     BPADDRLO,x      ; Get address
            cmp     MEMBUFF         ; Is it in the desired address?
            bne     @NEXT2          ; No, go try next slot
            lda     BPADDRHI,x      ; Maybe, get MSB
            cmp     MEMBUFF+1       ;
            bne     @NEXT2          ; No, go try next slot
            lda     NEWBNK          ; Definitely, switch to BP bank
            eor     DEFBNK          ;
            sta     BNKCTL          ;
            lda     (MEMBUFF),y     ; Get opcode at BP address
            bne     @SETBP          ; Not a BRK, set BP
            beq     @RSTBNK         ; Already a BRK, just restore bank and return
@NEXT2:     dex
            bpl     @LOOP2
            jsr     ERROR31         ; Breakpoint table full (3 BP's already set)
            ; Not reached


; FREE Command
;
; DESCRIPTION:  Disassociate an Input-Output channel from a device or file.
; SYNTAX:       FREE <channel>
; ARGUMENTS:    <channel> = channel number to free, 0 to 9.
; 
FREE:       jsr     LD939
            jsr     LF5C5
            ldy     $EB
            jsr     LF925
            bne     FREE
            rts


LDAEE:  beq     LDAFC
LDAF0:  jsr     LD955
LDAF3:  jsr     LF40B
        jsr     LF925
        bne     LDAF0
        rts

LDAFC:  ldx     #$00
        beq     LDAF3

LDB00:  beq     LDB10
LDB02:  jsr     LD955
LDB05:  jsr     LF42D
        ldy     $EB
        jsr     LF925
        bne     LDB02
        rts

LDB10:  ldx     #$00
        beq     LDB05
        rts

SAVE:   jsr     LD968
        jsr     LF4A0
        bit     $E786
        bpl     LDB28
        bit     $E77A
        bmi     LDB28
        jsr     LE9E3
LDB28:  ldy     $EB
        jsr     LDA30
LDB2D:  jsr     LD9B7
        lda     NEWBNK
        sta     $E6D6
        jsr     LDA17
        lda     NEWBNK
        sta     $E6D7
        jsr     LD9F1
        lda     #$00
        tax
        jsr     LFC62
        ldy     $EB
        jsr     LF925
        bne     LDB2D
        jsr     LF0D6
        jmp     LF5C3

LDB55:  jsr     LD968
        jsr     LF592
        ldy     $EB
        jsr     LDA17
        lda     NEWBNK
        sta     $E6D7
        ldx     #$00
        txa
        jsr     LFD05
        bcc     LDB71
        jsr     LE9FB
LDB71:  lda     BUFFERP1
        sta     $DA
        lda     BUFFERP1+1
        sta     $DB
        lda     $E6D2
        sta     PRGBANK
        sta     $E6CF
LDB82:  ldy     $EB
        jsr     LDA17
        ldx     #$00
        txa
        jsr     LFD05
        bcc     LDB82
        jmp     LF5C3

RESAVE:     sec
            ror     $E77A
            jsr     SAVE
            asl     $E77A
            rts

LDB9D:  jsr     LD94F
        sta     $E796
        rts

LDBA4:  jsr     LD968
        jsr     LF62D
        ldy     $EB
        jsr     LF925
        bne     LDBA4
        rts

BOOT:       ldx     #$FF            ; Init stack pointer
            txs                     ;
            cld
            jmp     LFF0E           ; Boot from PROM

LDBB9:      .byte   "ASSIGN", $07
            .byte   "FREE", $00
            .byte   "OPEN", $00
            .byte   "CLOSE", $00
            .byte   "SAVE", $00
            .byte   "GET", $00
            .byte   "DUMP", $04
            .byte   "DRIVE", $00
            .byte   "DELETE", $00
            .byte   "LOCK", $06
            .byte   "UNLOCK", $06
            .byte   "FILES", $08
            .byte   "DISK", $10
            .byte   "SET", $01
            .byte   "UNPROTECT", $01
            .byte   "BEGINOF", $05
            .byte   "ENDOF", $05
            .byte   "GO", $00
            .byte   "REG", $03
            .byte   "PROTECT", $01
            .byte   "FILL", $02
            .byte   "NEXT", $00
            .byte   "TYPE", $05
            .byte   "DATE", $03
            .byte   "GETLOC", $09
            .byte   "COPY", $0A
            .byte   "RENAME", $0B
            .byte   "SVC", $06
            .byte   "BOOT", $00
            .byte   "DO", $07
            .byte   "HUNT", $0C
            .byte   "COMPARE", $0F
            .byte   "ONKEY", $0E
            .byte   "BP", $00
            .byte   "RESAVE", $00
            .byte   "MSG", $06

            .byte   $00, $00, $00, $00, $00, $00, $00, $00
            .byte   $00, $00, $00, $00, $00, $00, $00, $00
            .byte   $00, $00, $00, $00, $00, $00, $00, $00

LDC99:      .word   $0000
            .word   $FE01           ; Assign
            .word   FREE
            .word   LDAEE
            .word   LDB00
            .word   SAVE
            .word   LDB55
            .word   $FE01           ; Dump
            .word   LDB9D
            .word   LDBA4
            .word   $FE01           ; Lock
            .word   $FE1F           ; Unlock
            .word   $FE01           ; Files
            .word   $FE01           ; Disk
            .word   $FE01           ; Set
            .word   $FECD           ; Unprotect
            .word   $FEBF           ; Beginof
            .word   $FECD           ; Endof
            .word   LEB23
            .word   $FE01           ; Reg
            .word   $FED7           ; Protect
            .word   $FE01           ; Fill
            .word   $EB0F
            .word   $FE01
            .word   $FE6A
            .word   $FE01
            .word   $FE01
            .word   $FE01
            .word   $FEA7
            .word   BOOT
            .word   $FEF0
            .word   $FE01
            .word   $FE01
            .word   $FE01
            .word   BREAKP
            .word   RESAVE
            .word   $FEB3
            .word   $0000
            .word   $0000
            .word   $0000
            .word   $0000