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



LEB2F           := $EB2F
LF0D6           := $F0D6
LF592           := $F592
LF62D           := $F62D
LF8CE           := $F8CE
LFBE1           := $FBE1
LFC62           := $FC62
LFD05           := $FD05
LFD5F           := $FD5F
LFF0E           := $FF0E

            .importzp MEMBUFF, TMPBUFP, INPBUFP, P0SCRATCH, ERRNUM, SVCENB
            .importzp PCSAVE, CMDLIDX

            .segment "scratch0"

CHANNEL:    .res 1                  ; $0280 
CMDIDX:     .res 1                  ; $0281 Command index in tables
CMDNUM:     .res 1                  ; $0282 Command number

            .segment "cmdproc"

            .export CMDPROC

CMDPROC:    cld
            lda     #$00            ; Unprotect SYSRAM
            sta     HSRCW           ;

            ldx     #$05            ; Clear error flags
@LOOP:      sta     INTCMDERR,x     ;
            dex                     ;
            bpl     @LOOP           ;
        
            sta     ERRNUM          ; Clear error
            sta     SVCENB          ; Disable SVCs
        
            jsr     INIMMAP         ; Set default memory config
            jsr     CPYBNKSW        ; Copy bank switching routine to page zero
            lda     UNPROTFLG       ; Get status of memory protection flag
            sta     IGNORWRP        ; And copy to the ignore protection flag
            jsr     SETOUTB         ; Set output buffer to output line buffer
            jsr     SETINPB         ; Set input buffer to input line buffer
            lda     CHANN1
            cmp     #$82            ; Console input?
            bne     @CONT           ; Nope
            jsr     PRNSTR          ; Yes, print CODOS prompt
            .byte   $0D, "CODOS> ", $00

@CONT:      ldx     #$01            ;
            jsr     GETLINE         ; Get entire line
            bcc     CMDEXEC         ; If OK, go execute the command
            ldx     #$01            ; Not OK, free channel 1 (console )
            jsr     FREECH          ;   (SETINPB will set it again to default $82)
            jmp     CMDPROC         ; And restart command processor

            ; Command processor entry

            .export CMDEXEC

CMDEXEC:    jsr     GETNEXTNB       ; Get next non-blank from input buffer
            beq     CMDPROC         ; Nothing? Restart command processor
            sec                     ; Set flag: Error produced during
            ror     INTCMDERR       ;    command processing
            sty     CMDLIDX         ; Save command line index
            jsr     ISALPHA         ; Check that char is alphabetic
            bcs     @SEARCH
@LOOP:      iny
            jsr     GETNEXTCH       ; Get next char
            beq     @SEARCH         ; Nothing? We got the command
            jsr     VALFNCHR        ; Check it is a valid character
            bcc     @LOOP           ; Yes, get next
            cmp     #'!'            ; Abbreviation?
            beq     @SEARCH         ; Yes, we got the command
            cmp     #' '            ; Is it a blank?
            bne     @CHKCOL         ; No, go check if it is a colon
            jsr     GETNEXTNB       ; Yes, advance until next non-blank
@CHKCOL:    cmp     #':'            ; Is it a colon? (???)
            beq     EXTERNAL        ; Yes, it is an external in an specified drive
@SEARCH:    ldy     CMDLIDX         ; We've got a command. Recover position
            jsr     SEARCHCMD       ; See if in the command name table. Returns overlay
                                    ; in A (zero if none) and cmd number in X
            beq     EXTERNAL        ; It is not found, so try external
            jsr     OVERLAY         ; It is found, loads the required overlay (if any)
            txa                     ; Get the command number
            asl     a               ; Calculate offset in command function table
            tax                     ; Get the pointer to the function
            lda     CMDFUNTBL,x     ;
            sta     CMDFNP          ; And save to the jump to command pointer
            lda     CMDFUNTBL+1,x   ;
            sta     CMDFNP+1        ;
            jsr     GETNEXTNB       ; Advance to the first argument in command line
            php
            sty     CMDLIDX         ; Save command line index
            bit     SVC13FLG
            bmi     @JCMD           ; If comes from SVC, jumps to the command function
            plp
            jsr     JCMDFNP         ; Execute command function
            jmp     CMDPROC         ; And restart command processor
            ; Not reached

@JCMD:      plp
            ; Fall through

JCMDFNP:    jmp     (CMDFNP)        ; Jump to the command function pointer      

; Execute external command
;
EXTERNAL:   ldy     #$00
            jsr     GETFILNDRV      ; Get file and drive from beginning of command line
            jsr     GETNEXTNB       ; Advance to first non blank (first argument)
            sty     CMDLIDX         ; And save where it is
            sec                     ; Mark that file to load is a command
            ror     ISCMDFLG        ;  (just used to display the correct error message)
            jsr     LF592
            clc                     ; Clear the command flag
            ror     ISCMDFLG        ;
            lda     #$00
            sta     $E77B
            sta     $E77C
            ldx     #$00
            txa
            jsr     LFD05
            bcc     LD8C7
            jsr     ERROR13         ; Not a loadable ("SAVEd") file.
LD8C7:      lda     $E721
            sta     PCSAVE
            lda     $E722
            sta     PCSAVE+1
            lda     $E6D2
            sta     PRGBANK
            sta     DATBANK
LD8DA:      ldx     #$00
            txa
            jsr     LFD05
            bcc     LD8DA
            jsr     FREECH0
            jmp     LEB2F

; Search typed command in command name table
;
; Returns overlay in A (zero if none) and cmd number in X (zero if not found)
;
SEARCHCMD:  sty     CMDLIDX         ; Save command position
            ldx     #$00            ; Init command name char index
            stx     CMDNUM          ;
            beq     @CMPNAM         ; And jump to compare command names
            ; Not reached

@NEXTCMD:   ldx     CMDIDX          ; Get current command index
@SKIP:      inx                     ; Go to next
            lda     CMDNAMTBL,x     ; Get first char of command name
            cmp     #' '            ; Is it valid?
            bcs     @SKIP           ; Yes, advance char
            inx                     ; No, we've got the command typr, advance past it

@CMPNAM:    stx     CMDIDX          ; Save command index
            lda     CMDNAMTBL,x     ; Get command's char
            beq     @ENDOFTBL       ; If null, end of table
            inc     CMDNUM          ; Increment CMDNUM (command name char index?)
            ldy     CMDLIDX         ; Get typed command position
@CMPCH:     lda     (INPBUFP),y     ; Get char from input buffer
            jsr     VALFNCHR        ; Check it is a valid command name char
            bcs     @CHKABB         ; Nope, go check if it is a '!'
            cmp     CMDNAMTBL,x     ; Yes, compare to current command in table
            bne     @NEXTCMD        ; Not equal, try next command in table
            inx                     ; Compare next name char
            iny                     ;
            bne     @CMPCH          ; Always jump
            ; Not reached

@CHKABB:    cmp     #'!'            ; Is it an abbreviation
            bne     @CHKTYP         ; No, then are different or reached command overlay
            iny                     ; Yes, advance in the command line
@SKIP2:     lda     CMDNAMTBL,x     ; Skip rest of the name until command overlay
            cmp     #' '            ; Printable? 
            bcc     @RETURN         ; Nope, got command overlay
            inx                     ; Skip char
            bne     @SKIP2          ; Always jump
            ; Not reached

@RETURN:    ldx     CMDNUM          ; Load command number
            rts                     ; And return

@CHKTYP:    lda     CMDNAMTBL,x     ; Get command char
            cmp     #' '            ; Printable
            bcc     @RETURN         ; Nope then it is the command overlay
            bcs     @NEXTCMD        ; Yes, then the command does not match. Go next.
            ; Not reached

@ENDOFTBL:  ldx     #$00
            rts

; Get channel number from A and store it in CHANNEL
; Returns channel number in X
;
GETCHANN:   jsr     LFBCC           ; Get channel number
            bcs     @CONT
            jsr     ERROR08         ; Missing or illegal channel number
            ; Not reached

@CONT:      sta     CHANNEL         ; Save channel
            tax                     ; ANd also return it in X
            sty     CMDLIDX         ; Save command line index
            cmp     #$0A            ; Valid channel numbers are 0-9
            bcc     @RETURN         ; OK
            jsr     ERROR08         ; Missing or illegal channel number
            ; Not reached

@RETURN:    rts

; Get drive from command line and check that it is opened
; Does not return on error
;
GETDRIVEOPND:  
            jsr     GETDRIVE
            jmp     ISDRVOPEN

; Get drive from command line and sets CURRDRV
; Returns drive in X. Sets CURRDRV and updates CMDLIDX.
; Does not return on error
;
GETDRIVE:   jsr     LF8CE
            bcs     LD95D
            jsr     ERROR05             ; Missing or illegal disk drive number
            ; Not reached
LD95D:      lda     P0SCRATCH
            tax
            stx     CURRDRV
            sty     CMDLIDX             ; Save command line position
            jmp     DRVVALID            ; Verify that drive is valid
            ; Not reached

; Get file and drive from command line and ensures that drive is open
; Returns drive in X. Sets CURRDRV, FNAMBUF and updates CMDLIDX.
;
GETFILNDRV: lda     INPBUFP         ; Copy command line pointer to
            sta     TMPBUFP         ; TMPBUF so we can use FNAMFROMBUF
            lda     INPBUFP+1       ;
            sta     TMPBUFP+1       ;
            jsr     GETNEXTNB       ; Get next non blank from command line
                                    ; This sets Y to the first char of the file name
            jsr     FNAMFROMBUF     ; Copy file name from (TMPBUFP),y to FNAMBUF
            bcc     @CONT
            jsr     ERROR12         ; Missing or illegal file name
@CONT:      jsr     GETNEXTNB       ; Get next non blank from command line
            cmp     COLON           ; Drive separator?
            bne     @USEDEF         ; No, use default drive
            jsr     GETNEXTNB1      ; Yes, get next non blank from command line
            jmp     GETDRIVEOPND    ; Get drive and ensure that it is open.
                                    ; Sets CURRDRV and updates CMDLIDX.
            ; Not reached

@USEDEF:    ldx     DEFDRV          ; Get default drive
            jsr     DRVVALIDO       ; Ensure that drive is valid and open
            stx     CURRDRV
            sty     CMDLIDX         ; Updates command line index
            rts

; Gets <device> or <file>:<drive> from command line
; If it is a file, uses GETFILNDRV and CURRDRV contains a valid drive.
; If it is a device, CURRDRV contains the device name
;
GETDEVORFIL:
            jsr     GETNEXTNB       ; Get next non-blank
            jsr     ISALPHA         ; Verify it is alphanumeric
            bcc     @CONT
            jsr     ERROR11         ; Missing or illegal device or file name
            ; Not reached
@CONT:      iny                     ; Advance and
            jsr     GETNEXTCH       ; Get next char
            jsr     VALFNCHR        ; Validate it is a valid file name char
            dey                     ; Go back
            bcs     @INVALID        ; Not valid
            jmp     GETFILNDRV
            ; Not reached
@INVALID:   jsr     GETNEXTCH       ; Get char
            sta     CURRDRV         ; Save into CURRDRV to force an invalid?
                                    ;   NOTE: seems that later on it will force a
                                    ;   "Missing or illegal device or file name" error
            iny                     ; Increment and save the command line index
            sty     CMDLIDX         ;
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
            jsr     ERROR14         ; <from> address missing or illegal
@LD9C1:     sty     CMDLIDX
            lda     #$00            ; Some initialization
            sta     NEWBNK          ;
            sta     CHGBNKFLG       ; Clears change bank flag
            jsr     GETNEXTNB
            beq     @RETURN
            cmp     COLON
            bne     @RETURN
            jsr     GETNEXTNB1
            sty     CMDLIDX
            beq     @LD9E6
            iny
            sec
            sbc     #$30
            bcc     @LD9E6
            cmp     #$04
            bcc     @LD9E9
@LD9E6:     jsr     ERROR51         ; Missing or illegal memory bank number
@LD9E9:     sta     NEWBNK          ; New bank
            sec                     ; And sets flag
            ror     CHGBNKFLG       ;
@RETURN:    rts

LD9F1:      ldx     #$04
            jsr     LFBE1
            bcs     LD9FB
            jsr     ERROR15         ; <to> address missing or illegal
LD9FB:      sty     CMDLIDX
            rts

            .export GETPC

GETPC:      lda     #$00
            sta     CHGBNKFLG       ; Clears bank switching flag
            sta     NEWBNK
            jsr     GETNEXTNB       ; Get next char from command line
            beq     @RETURN         ; If none, returns
            jsr     LD9B7           ; Get address and bank
            lda     MEMBUFF         ; Place address into program counter
            sta     PCSAVE          ;
            lda     MEMBUFF+1       ;
            sta     PCSAVE+1        ;
@RETURN:    rts

LDA17:  lda     #$00
        sta     $E77B
        jsr     GETNEXTNB
        cmp     #$3D
        bne     LDA2D
        iny
        ldx     #$08
        jsr     LD9B9
        sec
        ror     $E77B
LDA2D:  sty     CMDLIDX
        rts

LDA30:  lda     #$00
        sta     $E77C
        jsr     GETNEXTNB
        cmp     #$3D
        bne     LDA4B
        iny
        ldx     #$06
        jsr     LFBE1
        bcs     LDA47
        jsr     ERROR20         ; <entry> address missing or illegal
LDA47:  sec
        ror     $E77C
LDA4B:  sty     CMDLIDX
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
BREAKP:     jsr     GETNEXTNB       ; Get next char from command line       
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
            eor     DEFBNKCFG       ; And switch to it 
            sta     BNKCTL          ;
            lda     (MEMBUFF),y     ; Get OP at BP address
            bne     @CONT           ; If not a BRK, do nothing
            lda     BPOP,x          ; Restore original OP
            sta     (MEMBUFF),y     ;
@CONT:      lda     DEFBNKCFG       ; Switch back to default bank
            sta     BNKCTL          ;
            lda     #$FF            ; Invalidate/clear BP
            sta     BPBANK,x        ;
@NEXT:      dex
            bpl     @LOOP           ; Continue until last BP
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
            eor     DEFBNKCFG       ; And switch to it
            sta     BNKCTL          ;
            lda     (MEMBUFF),y     ; Get current opcode at BP address
            sta     BPOP,x          ; and save it
            tya                     ; Y is currently 0 (BRK)
            sta     (MEMBUFF),y     ; Set BRK at BP address
@RSTBNK:    lda     DEFBNKCFG       ; Switch bank bank
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
            eor     DEFBNKCFG       ;
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
; SYNTAX:       FREE <channel> ...
; ARGUMENTS:    <channel> = channel number to free, 0 to 9.
; 
FREE:       jsr     GETCHANN        ; Get channel from command line
                                    ; (in fact, from A, which contains first NN
                                    ;  char after the command name)
            jsr     FREECH
            ldy     CMDLIDX         ; Get command line index
            jsr     GETNEXTNB       ; Get next channel
            bne     FREE            ; Repeat until no more arguments
            rts

; OPEN Command
;
; DESCRIPTION:  Declare a disk ready for access by the system.
; SYNTAX:       OPEN [<drive> ...]
; ARGUMENTS:    <drive> = disk drive number to be opened, 0 to 3.
;               Defaults to drive 0.
; 
OPEN:       beq     @NOPARAMS
@GETDRV:    jsr     GETDRIVE        ; Get drive number in X and CURRDRV
@OPENX:     jsr     OPENDRV         ; Open it
            jsr     GETNEXTNB
            bne     @GETDRV         ; Repeat until no more arguments
            rts

@NOPARAMS:  ldx     #$00            ; Set default drive (0)
            beq     @OPENX          ; Always jumps
            ; Not reached

; CLOSE Command
;
; DESCRIPTION:  Conclude operations on a disk in preparation for
;               removing it from the drive or powering-down the system
; SYNTAX:       CLOSE [<drive> ...]
; ARGUMENTS:    <drive> = disk drive number to be closed, 0 to 3.
;               Defaults to drive 0.
; 
CLOSE:      beq     @NOARGS
@LOOP:      jsr     GETDRIVE
@CLOSEONE:  jsr     CLOSEDRV
            ldy     CMDLIDX         ; Get command line index
            jsr     GETNEXTNB
            bne     @LOOP           ; Repeat until no more arguments
            rts

@NOARGS:    ldx     #$00
            beq     @CLOSEONE
            rts

; SAVE Command
;
; DESCRIPTION:  Save one or more blocks of memory on a file.
; SYNTAX:       SAVE <file>[:<drive>][=<entry>]<from>[=<dest>] <to> ... 
; ARGUMENTS:
;               <file>  = file name.
;               <drive> = drive name, 0 to 3. Defaults to the default drive,
;                         usually 0.
;               <entry> = entry point. Defaults to <from>
;               <from>  = starting address for the block of memory.
;               <dest>  = address at which the block is to be loaded into memory on
;                         subsequent GET commands. Defaults to <from>.
;               <to>    = final address of the memory block. 
; 
SAVE:       jsr     GETFILNDRV      ; Get file and drive from command line
            jsr     ASSIGN0         ; Assign to channel 0
            bit     $E786
            bpl     @LDB28          ; Bit 7 clear
            bit     $E77A
            bmi     @LDB28
            jsr     ERROR25         ; New file name is already on selected diskette 
@LDB28:     ldy     CMDLIDX
            jsr     LDA30
@LDB2D:     jsr     LD9B7
            lda     NEWBNK
            sta     $E6D6
            jsr     LDA17
            lda     NEWBNK
            sta     $E6D7
            jsr     LD9F1
            lda     #$00
            tax
            jsr     LFC62
            ldy     CMDLIDX         ; Get cmd line index
            jsr     GETNEXTNB
            bne     @LDB2D
            jsr     LF0D6
            jmp     FREECH0

; GET Command
;
; DESCRIPTION:  Load a memory image from a disk file.
; SYNTAX:       GET <file>[:<drive>][=<dest>...] 
; ARGUMENTS:
;               <file>  = file name to be loaded into memory.
;               <drive> = drive number, 0 to 3. Defaults to the default drive,
;                         usually 0.
;               <dest>  = destination starting address for load to be used in
;                         lieu of the from address which was specified when the
;                         file was saved. 
; 
GETCMD:     jsr     GETFILNDRV
            jsr     LF592
            ldy     CMDLIDX         ; Get command line index
            jsr     LDA17
            lda     NEWBNK
            sta     $E6D7
            ldx     #$00
            txa
            jsr     LFD05
            bcc     @LDB71
            jsr     ERROR13         ; Not a loadable ("SAVEd") file
@LDB71:     lda     P0SCRATCH
            sta     PCSAVE
            lda     P0SCRATCH+1
            sta     PCSAVE+1
            lda     $E6D2
            sta     PRGBANK
            sta     DATBANK
@LDB82:     ldy     CMDLIDX
            jsr     LDA17
            ldx     #$00
            txa
            jsr     LFD05
            bcc     @LDB82
            jmp     FREECH0
            ; Not reached

; RESAVE Command
;
; DESCRIPTION:  Replace an existing file with a program or memory image(s).
; SYNTAX:       RESAVE <file>[:<drive>][=<entry>]<from>[=<dest>] <to> ... 
; ARGUMENTS:
;               <file>  = file name.
;               <drive> = disk drive, 0 to 3. Defaults to the default drive,
;                         usually 0.
;               <entry> = entry point. Defaults to <from>
;               <from>  = starting address for the block of memory.
;               <dest>> = address at which the block is to be loaded into memory on
;                         subsequent GET commands. Defaults to <from>.
;               <to>    = final address of the memory block. 
; 
RESAVE:     sec
            ror     $E77A
            jsr     SAVE
            asl     $E77A
            rts

; DRIVE Command
;
; DESCRIPTION:  Designate the default disk drive number to be used when files
;               are referenced without a drive number.
; SYNTAX:       DRIVE <drive>
; ARGUMENTS:    <drive> = disk drive number, 0 to 3.
; 
DRIVE:      jsr     GETDRIVEOPND
            sta     DEFDRV
            rts

; DELETE Command
;
; DESCRIPTION:  Remove a file from disk
; SYNTAX:       DELETE <file>[:<drive>] ...
; ARGUMENTS:    None.
;               <file>  = file name.
;               <drive> = disk drive, 0 to 3. Defaults to the default drive,
;                         usually 0.
; 
DELETE:     jsr     GETFILNDRV
            jsr     LF62D
            ldy     CMDLIDX         ; Get command line index
            jsr     GETNEXTNB
            bne     DELETE          ; Repeat until last argument
            rts

; BOOT Command
;
; DESCRIPTION:  Re-boot the CODOS operating sytem from the disk in drive 0
; SYNTAX:       BOOT
; ARGUMENTS:    None.
; 
BOOT:       ldx     #$FF            ; Init stack pointer
            txs                     ;
            cld
            jmp     LFF0E           ; Boot from PROM


            ; Command name table

CMDNAMTBL:  .byte   "ASSIGN", $07
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

            ; Reserved for new commands

            .byte   $00, $00, $00, $00, $00, $00, $00, $00
            .byte   $00, $00, $00, $00, $00, $00, $00, $00
            .byte   $00, $00, $00, $00, $00, $00, $00, $00

            ; Command function table

CMDFUNTBL:  .word   $0000
            .word   $FE01           ; Assign
            .word   FREE
            .word   OPEN
            .word   CLOSE
            .word   SAVE
            .word   GETCMD
            .word   $FE01           ; Dump
            .word   DRIVE
            .word   DELETE
            .word   $FE01           ; Lock
            .word   $FE1F           ; Unlock
            .word   $FE01           ; Files
            .word   $FE01           ; Disk
            .word   $FE01           ; Set
            .word   $FECD           ; Unprotect
            .word   $FEBF           ; Beginof
            .word   $FECD           ; Endof
            .word   GOCMD
            .word   $FE01           ; Reg
            .word   $FED7           ; Protect
            .word   $FE01           ; Fill
            .word   NEXT
            .word   $FE01           ; Type
            .word   $FE6A           ; Date
            .word   $FE01           ; Getloc
            .word   $FE01           ; Copy
            .word   $FE01           ; Rename
            .word   $FEA7           ; SCV
            .word   BOOT
            .word   $FEF0           ; Do
            .word   $FE01           ; Hunt
            .word   $FE01           ; Compare
            .word   $FE01           ; Onkey
            .word   BREAKP
            .word   RESAVE
            .word   $FEB3           ; Msg
            .word   $0000           ; Reserved
            .word   $0000           ; Reserved
            .word   $0000           ; Reserved
            .word   $0000           ; Reserved