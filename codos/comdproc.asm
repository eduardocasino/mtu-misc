; da65 V2.19 - Git cf0688fc5
; Created:    2025-04-23 17:47:56
; Input file: command.bin
; Page:       1

.ifdef mtu
            .include "monomeg.inc"
.endif

.ifdef  kim1
            .include "kim1.inc"
.endif

            .include "codos.inc"

            .importzp MEMBUFF, MEMCOUNT, TMPBUFP, INPBUFP, P0SCRATCH, DESTBUFF
            .importzp ERRNUM, SVCENB, PCSAVE, CMDLIDX, BYTRES, TMPPTR

            .segment "scratch0"

            .export CHANNEL, CMDIDX

CHANNEL:    .res 1                  ; $0280 
CMDIDX:     .res 1                  ; $0281 Command index in tables
CMDNUM:     .res 1                  ; $0282 Command number

            .segment "cmdproc"

            ; Loadable file data
            ;
            .byte   $58             ; CODOS loadable file header byte
            .byte   $00             ; Memory overlay
            .byte   $00             ; Memory bank
            .byte   $00             ; Reserved
            .addr   CMDPROC         ; Entry point
            .addr   CMDPROC         ; Load address
            .word   CMDPROC_SIZE    ; Memory image size

            .export CMDPROC

.proc CMDPROC
            cld
            lda     #$00            ; Unprotect SYSRAM
            sta     HSRCW           ;

            ldx     #$05            ; Clear error flags
LOOP:       sta     INTCMDERR,x     ;
            dex                     ;
            bpl     LOOP            ;
        
            sta     ERRNUM          ; Clear error
            sta     SVCENB          ; Disable SVCs
.ifdef mtu        
            jsr     INIMMAP         ; Set default memory config
            jsr     CPYEXINBNK      ; Copy execute in bank routine to page zero
.else
            jsr     CLEARBRK
.endif
            lda     UNPROTFLG       ; Get status of memory protection flag
            sta     IGNORWRP        ; And copy to the ignore protection flag
            jsr     SETOUTBCH       ; Set output buffer to output line buffer
            jsr     SETINPBCH       ; Set input buffer to input line buffer
            lda     CHANN1
            cmp     #$82            ; Console input?
            bne     CONT            ; Nope
            jsr     OUTSTR          ; Yes, print CODOS prompt
.ifdef mtu
            .byte   $0D, "CODOS> ", $00
.else
            .byte   $0D, "> ", $00
.endif

CONT:       ldx     #$01            ;
            jsr     GETLINE         ; Get entire line
            bcc     CMDEXEC         ; If OK, go execute the command
            ldx     #$01            ; Not OK, free channel 1 (console )
            jsr     FREECH          ;   (SETINPBCH will set it again to default $82)
            jmp     CMDPROC         ; And restart command processor
.endproc

            ; Command processor entry

            .export CMDEXEC

.proc CMDEXEC
            jsr     GETNEXTNB       ; Get next non-blank from input buffer
            beq     CMDPROC         ; Nothing? Restart command processor
            sec                     ; Set flag: Error produced during
            ror     INTCMDERR       ;    command processing
            sty     CMDLIDX         ; Save command line index
            jsr     ISALPHA         ; Check that char is alphabetic
            bcs     SEARCH          ; No, must be a separator
LOOP:       iny
            jsr     GETNEXTCH       ; Get next char
            beq     SEARCH          ; Nothing? We got the command
            jsr     VALFNCHR        ; Check it is a valid character
            bcc     LOOP            ; Yes, get next
            cmp     #'!'            ; Abbreviation?
            beq     SEARCH          ; Yes, we got the command
            cmp     #' '            ; Is it a blank?
            bne     CHKCOL          ; No, go check if it is a colon
            jsr     GETNEXTNB       ; Yes, advance until next non-blank
CHKCOL:     cmp     #':'            ; Is it a colon? (???)
            beq     EXTERNAL        ; Yes, it is an external in an specified drive
SEARCH:     ldy     CMDLIDX         ; We've got a command. Recover position
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
            bmi     JCMD            ; If comes from SVC, jumps to the command function
            plp
            jsr     JCMDFNP         ; Execute command function
            jmp     CMDPROC         ; And restart command processor
            ; Not reached

JCMD:       plp
            ; Fall through
.endproc

JCMDFNP:    jmp     (CMDFNP)        ; Jump to the command function pointer      

; Execute external command
;
.proc EXTERNAL
            ldy     #$00
            jsr     GETFILNDRV      ; Get file and drive from beginning of command line
            jsr     GETNEXTNB       ; Advance to first non blank (first argument)
            sty     CMDLIDX         ; And save where it is
            sec                     ; Mark that file to load is a command
            ror     ISCMDFLG        ;  (just used to display the correct error message)
            jsr     FOPEN0          ; Assigns channel 0 to file (fails if not found)
            clc                     ; Clear the command flag
            ror     ISCMDFLG        ;
            lda     #$00            ; Inits flags:
            sta     SAVDESTPF       ;   No <dest> specified
            sta     SAVENTRYPF      ;   No <entry> specified
            ldx     #$00            ; Set channel 0
            txa                     ; Set overlay 0
            jsr     LOADSVD         ; Load first block
            bcc     CONT            ; OK, continue
            jsr     ERROR13         ; Not a loadable ("SAVEd") file
                                    ; Store entry point into PCSAVE
CONT:       lda     SAVEDHDR+SHDR::ENTRY
            sta     PCSAVE          ;
            lda     SAVEDHDR+SHDR::ENTRY+1
            sta     PCSAVE+1        ;
.ifdef mtu
            lda     DSTBANK         ; Sets program and data banks
            sta     PRGBANK         ;
            sta     DATBANK         ;
.endif
NEXT:       ldx     #$00            ; Get next block
            txa                     ;
            jsr     LOADSVD         ;
            bcc     NEXT            ; Go get next one while OK
            jsr     FREECH0         ; Free channel
            jmp     EXCMD           ; Execute program in memory
            ; Not reached
.endproc

; Search typed command in command name table
;
; Returns overlay in A (zero if none) and cmd number in X (zero if not found)
;
.proc SEARCHCMD
            sty     CMDLIDX         ; Save command position
            ldx     #$00            ; Init command name char index
            stx     CMDNUM          ;
            beq     CMPNAM          ; And jump to compare command names
            ; Not reached

NEXTCMD:    ldx     CMDIDX          ; Get current command index
SKIP:       inx                     ; Go to next
            lda     CMDNAMTBL,x     ; Get first char of command name
            cmp     #' '            ; Is it valid?
            bcs     SKIP            ; Yes, advance char
            inx                     ; No, we've got the command typr, advance past it

CMPNAM:     stx     CMDIDX          ; Save command index
            lda     CMDNAMTBL,x     ; Get command's char
            beq     ENDOFTBL        ; If null, end of table
            inc     CMDNUM          ; Increment CMDNUM (command name char index?)
            ldy     CMDLIDX         ; Get typed command position
CMPCH:      lda     (INPBUFP),y     ; Get char from input buffer
            jsr     VALFNCHR        ; Check it is a valid command name char
            bcs     CHKABB          ; Nope, go check if it is a '!'
            cmp     CMDNAMTBL,x     ; Yes, compare to current command in table
            bne     NEXTCMD         ; Not equal, try next command in table
            inx                     ; Compare next name char
            iny                     ;
            bne     CMPCH           ; Always jump
            ; Not reached

CHKABB:     cmp     #'!'            ; Is it an abbreviation
            bne     CHKTYP          ; No, then are different or reached command overlay
            iny                     ; Yes, advance in the command line
SKIP2:      lda     CMDNAMTBL,x     ; Skip rest of the name until command overlay
            cmp     #' '            ; Printable? 
            bcc     RETURN          ; Nope, got command overlay
            inx                     ; Skip char
            bne     SKIP2           ; Always jump
            ; Not reached

RETURN:     ldx     CMDNUM          ; Load command number
            rts                     ; And return

CHKTYP:     lda     CMDNAMTBL,x     ; Get command char
            cmp     #' '            ; Printable
            bcc     RETURN          ; Nope then it is the command overlay
            bcs     NEXTCMD         ; Yes, then the command does not match. Go next.
            ; Not reached

ENDOFTBL:   ldx     #$00            ; Not found
            rts
.endproc

; Get channel number from command line and store it in CHANNEL
;
; Returns channel number in X
;
            .export GETCHANN

.proc GETCHANN
            jsr     GETBYTE         ; Get byte from command line
            bcs     CONT            ; Valid byte? Yes, continue
            jsr     ERROR08         ; Missing or illegal channel number
            ; Not reached

CONT:       sta     CHANNEL         ; Save channel
            tax                     ; And also return it in X
            sty     CMDLIDX         ; Update command line index
            cmp     #$0A            ; Valid channel numbers are 0-9
            bcc     RETURN          ; OK
            jsr     ERROR08         ; Missing or illegal channel number
            ; Not reached

RETURN:     rts
.endproc

; Get drive from command line and check that it is opened
; Does not return on error
;
                .export GETDRIVEOPND

.proc GETDRIVEOPND  
            jsr     GETDRIVE        ; Get drive from command line
            jmp     ISDRVOPEN       ; Checks if open and returns (or fails)
.endproc

; Get drive from command line and sets CURRDRV
; Returns drive in X. Sets CURRDRV and updates CMDLIDX.
; Does not return on error
;
.proc GETDRIVE
            jsr     HEXDECOD        ; Converts drive number from ascii
            bcs     CONT            ; Continue if valid number
            jsr     ERROR05         ; Missing or illegal disk drive number
            ; Not reached
CONT:       lda     P0SCRATCH       ; Get result number
            tax                     ; Store into CURRDRV (will be validated
            stx     CURRDRV         ; later on)
            sty     CMDLIDX         ; Save command line position
            jmp     DRVVALID        ; Verify that drive is valid and returns
            ; Not reached
.endproc

; Get file and drive from command line and ensures that drive is open
; Returns drive in X. Sets CURRDRV, DIRENT+DIRE::FNAM and updates CMDLIDX.
;
            .export GETFILNDRV

.proc GETFILNDRV
            lda     INPBUFP         ; Copy command line pointer to
            sta     TMPBUFP         ; TMPBUF so we can use FNAMFROMBUF
            lda     INPBUFP+1       ;
            sta     TMPBUFP+1       ;
            jsr     GETNEXTNB       ; Get next non blank from command line
                                    ; This sets Y to the first char of the file name
            jsr     FNAMFROMBUF     ; Copy file name from (TMPBUFP),y to DIRENT+DIRE::FNAM
            bcc     CONT 
            jsr     ERROR12         ; Missing or illegal file name
CONT:       jsr     GETNEXTNB       ; Get next non blank from command line
            cmp     COLON           ; Drive separator?
            bne     USEDEF          ; No, use default drive
            jsr     GETNEXTNB1      ; Yes, advance one pos and get next non blank
            jmp     GETDRIVEOPND    ; Get drive and ensure that it is open.
                                    ; Sets CURRDRV and updates CMDLIDX.
            ; Not reached

USEDEF:     ldx     DEFDRV          ; Get default drive
            jsr     DRVVALIDO       ; Ensure that drive is valid and open
            stx     CURRDRV         ; Set curret drive
            sty     CMDLIDX         ; Updates command line index
            rts
.endproc

; Gets <device> or <file>:<drive> from command line
; If it is a file, uses GETFILNDRV and CURRDRV contains a valid drive.
; If it is a device, CURRDRV contains the device name
;
            .export GETDEVORFIL

.proc GETDEVORFIL
            jsr     GETNEXTNB       ; Get next non-blank
            jsr     ISALPHA         ; Verify it is alphanumeric
            bcc     CONT 
            jsr     ERROR11         ; Missing or illegal device or file name
            ; Not reached
CONT:       iny                     ; Advance and
            jsr     GETNEXTCH       ; Get next char
            jsr     VALFNCHR        ; Validate it is a valid file name char
            dey                     ; Go back
            bcs     INVALID         ; Not valid
            jmp     GETFILNDRV      ; Valis, get file and drive and returns
            ; Not reached
INVALID:    jsr     GETNEXTCH       ; Get char
            sta     CURRDRV         ; Save into CURRDRV to force an invalid?
                                    ;   NOTE: seems that later on it will force a
                                    ;   "Missing or illegal device or file name" error
            iny                     ; Increment and save the command line index
            sty     CMDLIDX         ;
            rts
.endproc

; Get Address:bank from command line and store into MEMBUFF and NEWBNK
;
            .export GADDRBNKMB

.proc GADDRBNKMB
            ldx     #_MEMBUFF
            ; Fall through
.endproc

; Get Address:bank from command line and store it into
; P0SCRATCH + X and NEWBNK
;
.proc GADDRBNK
            jsr     EVALEXP         ; Evaluate expression
            bcs     CONT            ; If OK, continue
            jsr     ERROR14         ; <from> address missing or illegal
CONT:       sty     CMDLIDX         ; Update command line index
.ifdef mtu
            lda     #$00            ; Some initialization
            sta     NEWBNK          ; Inits NEWBNK to bank 0
            sta     CHGBNKFLG       ; Clears change bank flag
            jsr     GETNEXTNB       ; Get next non-blank
            beq     RETURN          ; Nothing? Then we're done
            cmp     COLON           ; Is it the bank separator?
            bne     RETURN          ; No, we're done
            jsr     GETNEXTNB1      ; Yes, advance one pos and get next non blank
            sty     CMDLIDX         ; Update command line index
            beq     ERROR           ; Oops, we were expecting the bank number
            iny                     ; Advance one pos
            sec                     ; Check that the channel is a number from 0 to 3
            sbc     #'0'            ; Convert from ascii to number
            bcc     ERROR           ; Invalid number          
            cmp     #$04            ; Check if 0-3
            bcc     SETBNK          ; Yes, set new bank
ERROR:      jsr     ERROR51         ; Missing or illegal memory bank number
SETBNK:     sta     NEWBNK          ; New bank
            sec                     ; And sets change bank flag
            ror     CHGBNKFLG       ;
.endif
RETURN:     rts
.endproc

; Get <to> address from command line and store to MEMCOUNT
;
            .export GETTOP

.proc GETTOP
            ldx     #_MEMCOUNT      ; Eval expression and store into MEMCOUNT
            jsr     EVALEXP         ;
            bcs     CONT            ; If correct, return
            jsr     ERROR15         ; <to> address missing or illegal
CONT:       sty     CMDLIDX         ; Recover command line index
            rts
.endproc

; Get program counter from command args
;
            .export GETPC

.proc GETPC
.ifdef mtu
            lda     #$00            ; Clears bank switching flag
            sta     CHGBNKFLG       ;
            sta     NEWBNK          ; And sets bank 0 as new bank
.endif
            jsr     GETNEXTNB       ; Get next char from command line
            beq     RETURN          ; If none, returns
            jsr     GADDRBNKMB      ; Get address and bank and store into MEMBUFF and NEWBNK
            lda     MEMBUFF         ; Place address into program counter
            sta     PCSAVE          ;
            lda     MEMBUFF+1       ;
            sta     PCSAVE+1        ;
RETURN:     rts
.endproc

; Get the <dest> address and bank from the command line
;
; Looks for pattern "=<address>"
;
; Y has the command line index on entry
; 
.proc GETDESTP
            lda     #$00            ; Inits flag: no dest specified
            sta     SAVDESTPF       ;
            jsr     GETNEXTNB       ; Get next non blank
            cmp     #'='            ;
            bne     NODEST          ; No '=', no dest specified
            iny                     ; Advance one position
            ldx     #_DESTBUFF      ; Evaluate address expression and store it
            jsr     GADDRBNK        ; into TMPBU2. Bank into NEWBANK
            sec                     ; Set flag, dest address specified
            ror     SAVDESTPF       ;
NODEST:     sty     CMDLIDX         ; Update command line index
            rts
.endproc

; Get the entry point address from the command line
;
; Looks for pattern "=<address>"
;
; Y has the command line index on entry
; 
.proc GETENTRYP
            lda     #$00            ; Inits flag: no entry point specified
            sta     SAVENTRYPF      ;
            jsr     GETNEXTNB       ; Get next non blank
            cmp     #'='            ; 
            bne     NOENT           ; No '=', no entry point specified
            iny                     ; Advance one position
            ldx     #_TMPBUFP       ; Evaluate address expression and store it
            jsr     EVALEXP         ; into TMPBUF
            bcs     CONT            ; Continue if OK
            jsr     ERROR20         ; <entry> address missing or illegal
CONT:       sec                     ; Set flag, entry point address specified
            ror     SAVENTRYPF      ;
NOENT:      sty     CMDLIDX         ; Update command line index
            rts
.endproc

; BP Command
;
; DESCRIPTION:  Set a program breakpoint for machine language debugging.
; SYNTAX:       BP [<addr>]
; ARGUMENTS:    <addr>=address of first byte of instruction at which breakpoint is
;               to be set
;
; If no arguments are provided, clears all break points
; 
.proc BREAKP
            jsr     GETNEXTNB       ; Get next char from command line       
            bne     GETARGS         ; Jump if there are any arguments

            ldx     #$02            ; No arguments, clear all break points
LOOP:       lda     BPBANK,x        ; Get bank
            bmi     NEXT            ; If bit 7 set, it means "not set"
            lda     BPADDRLO,x      ; Get BP address
            sta     MEMBUFF         ;
            lda     BPADDRHI,x      ;
            sta     MEMBUFF+1       ;
            ldy     #$00
.ifdef mtu
            lda     BPBANK,x        ; Get bank
            eor     DEFBNKCFG       ; And switch to it 
            sta     BNKCTL          ;
.endif
            lda     (MEMBUFF),y     ; Get OP at BP address
            bne     CONT            ; If not a BRK, do nothing
            lda     BPOP,x          ; Restore original OP
            sta     (MEMBUFF),y     ;
CONT:
.ifdef mtu
            lda     DEFBNKCFG       ; Switch back to default bank
            sta     BNKCTL          ;
.endif
            lda     #$FF            ; Invalidate/clear BP
            sta     BPBANK,x        ;
NEXT:       dex                     ; Advance to next BP
            bpl     LOOP            ; Continue until last
            rts

GETARGS:    jsr     GADDRBNKMB      ; Get <from> address:bank and store into MEMBUFF and NEWBNK
            ldy     #$00

            ldx     #$02            ; Search for a free BP slot
LOOP2:      lda     BPBANK,x        ; Get bank (or BP set flag)
            bpl     NOTFREE         ; This BP slot is not free

SETBP:      lda     MEMBUFF         ; Set BP address
            sta     BPADDRLO,x      ;
            lda     MEMBUFF+1       ;
            sta     BPADDRHI,x      ;
.ifdef mtu
            lda     NEWBNK          ; Set BP memory bank
            sta     BPBANK,x        ;
            eor     DEFBNKCFG       ; And switch to it
            sta     BNKCTL          ;
.else
            lda     #$00
            sta     BPBANK,x        ; Set "BP set"
.endif
            lda     (MEMBUFF),y     ; Get current opcode at BP address
            sta     BPOP,x          ; and save it
            tya                     ; Y is currently 0 (BRK)
            sta     (MEMBUFF),y     ; Set BRK at BP address
RSTBNK:
.ifdef mtu
            lda     DEFBNKCFG       ; Switch to default bank
            sta     BNKCTL          ;
.endif
            rts

NOTFREE:    
.ifdef mtu
            cmp     PRGBANK         ; Is it in the current program bank?
            bne     NEXT2           ; No, go try next slot
.endif
            lda     BPADDRLO,x      ; Get address
            cmp     MEMBUFF         ; Is it in the desired address?
            bne     NEXT2           ; No, go try next slot
            lda     BPADDRHI,x      ; Maybe, get MSB
            cmp     MEMBUFF+1       ;
            bne     NEXT2           ; No, go try next slot
.ifdef mtu
            lda     NEWBNK          ; Definitely, switch to BP bank
            eor     DEFBNKCFG       ;
            sta     BNKCTL          ;
.endif
            lda     (MEMBUFF),y     ; Get opcode at BP address
            bne     SETBP           ; Not a BRK, set BP
            beq     RSTBNK          ; Already a BRK, just restore bank and return
NEXT2:      dex                     ; Advance to next slot
            bpl     LOOP2           ; And go check it
            jsr     ERROR31         ; Breakpoint table full (3 BP's already set)
            ; Not reached
.endproc

; FREE Command
;
; DESCRIPTION:  Disassociate an Input-Output channel from a device or file.
; SYNTAX:       FREE <channel> ...
; ARGUMENTS:    <channel> = channel number to free, 0 to 9.
; 
.proc FREE
            jsr     GETCHANN        ; Get channel from command line
            jsr     FREECH          ; Free it
            ldy     CMDLIDX         ; Get command line index
            jsr     GETNEXTNB       ; Get next channel
            bne     FREE            ; Repeat until no more arguments
            rts
.endproc

; OPEN Command
;
; DESCRIPTION:  Declare a disk ready for access by the system.
; SYNTAX:       OPEN [<drive> ...]
; ARGUMENTS:    <drive> = disk drive number to be opened, 0 to 3.
;               Defaults to drive 0.
; 
.proc OPEN
            beq     OPEN0           ; No parameters, open drive 0
GETDRV:     jsr     GETDRIVE        ; Get drive number in X and CURRDRV
OPENX:      jsr     OPENDRV         ; Open it
            jsr     GETNEXTNB       ; Get next non-blank
            bne     GETDRV          ; Repeat until no more arguments
            rts

OPEN0:      ldx     #$00            ; Set default drive (0)
            beq     OPENX           ; Always jumps
            ; Not reached
.endproc

; CLOSE Command
;
; DESCRIPTION:  Conclude operations on a disk in preparation for
;               removing it from the drive or powering-down the system
; SYNTAX:       CLOSE [<drive> ...]
; ARGUMENTS:    <drive> = disk drive number to be closed, 0 to 3.
;               Defaults to drive 0.
; 
.proc CLOSE
            beq     CLOSE0          ; No args, close disk 0
LOOP:       jsr     GETDRIVE        ; Get drive from command line (into X)
CLOSEX:     jsr     CLOSEDRV        ; And close it
            ldy     CMDLIDX         ; Get command line index
            jsr     GETNEXTNB       ; Get next non-blank
            bne     LOOP            ; Repeat until no more arguments
            rts

CLOSE0:     ldx     #$00            ; Set drive 0
            beq     CLOSEX          ; And go close it (always jumps)
            ; Not reached
            rts                     ; Dead code
.endproc

; SAVE Command
;
; DESCRIPTION:  Save one or more blocks of memory on a file.
; SYNTAX:       SAVE <file>[:<drive>] [=<entry>] <from> [=<dest>] <to> ... 
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
.proc SAVE
            jsr     GETFILNDRV      ; Get file and drive from command line
            jsr     ASSIGN0         ; Assign to channel 0
            bit     ASSIGNFLAG      ; Flag. If bit 6 = 1, it is a file
                                    ;       If bit 6 = 0, it is a device
                                    ;       If bit 7 = 1, it is an existing file
            bpl     OVERWR          ; Bit 7 clear, it is new or a device
            bit     SAVEOVERWR      ; Flag. If bit 7 = 1 then permits overwrite 
            bmi     OVERWR          ;
            jsr     ERROR25         ; New file name is already on selected diskette 
OVERWR:     ldy     CMDLIDX         ; Recover command line index
            jsr     GETENTRYP       ; Get =<entry> address
MEMBLK:     jsr     GADDRBNKMB      ; Get <from> address:bank and store into MEMBUFF and NEWBNK
.ifdef mtu
            lda     NEWBNK          ; Store bank for saved file <from>
            sta     SVDFROMBNK      ;
.endif
            jsr     GETDESTP        ; Get =<dest> address
.ifdef mtu
            lda     NEWBNK          ; Store bank for <dest>
            sta     SVDDESTBNK      ;
.endif
            jsr     GETTOP          ; Get TO address and store into MEMCOUNT
            lda     #$00            ; Set overlay as 0
            tax                     ; This seems useless, as X is modified before use
            jsr     SAVEBLK         ; Save memory block into file's current position
            ldy     CMDLIDX         ; Get cmd line index
            jsr     GETNEXTNB       ; Next non-blank
            bne     MEMBLK          ; Any? Get next memory block
            jsr     FTRUNC          ; No more, just truncates output file at current pos
            jmp     FREECH0         ; Frees channel and returns
            ; Not reached
.endproc

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
.proc GETCMD
            jsr     GETFILNDRV      ; Get file and drive from command line
            jsr     FOPEN0          ; Assigns channel 0 to file (fails if not found)
            ldy     CMDLIDX         ; Get command line index
            jsr     GETDESTP        ; Get =<dest> address (if specified)
.ifdef mtu
            lda     NEWBNK          ; Store bank for <dest>
            sta     SVDDESTBNK      ;
.endif
            ldx     #$00            ; Set channel 0
            txa                     ; Set overlay 0
            jsr     LOADSVD         ; Loads $58 segment from file
            bcc     CONT            ; Continue if no error
            jsr     ERROR13         ; Not a loadable ("SAVEd") file
CONT:       lda     P0SCRATCH       ; Store entry address into PCSAVE
            sta     PCSAVE          ;
            lda     P0SCRATCH+1     ;
            sta     PCSAVE+1        ;
.ifdef mtu
            lda     DSTBANK         ; Set DATBANK and PRGBANK from header (or <dest> if set)
            sta     PRGBANK         ;
            sta     DATBANK         ;
.endif
NEXT:       ldy     CMDLIDX         ; Recover command line index
            jsr     GETDESTP        ; Get <dest>, if set
            ldx     #$00            ; Set channel 0
            txa                     ; Set overlay 0
            jsr     LOADSVD         ; Loads $58 segment from file
            bcc     NEXT            ; If OK, go load next
            jmp     FREECH0         ; No more, free channel 0 and return
            ; Not reached
.endproc

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
.proc RESAVE
            sec                     ; Allow overwrite
            ror     SAVEOVERWR      ;
            jsr     SAVE            ; Execute SAVE
            asl     SAVEOVERWR      ; Clean flag
            rts
.endproc

; DRIVE Command
;
; DESCRIPTION:  Designate the default disk drive number to be used when files
;               are referenced without a drive number.
; SYNTAX:       DRIVE <drive>
; ARGUMENTS:    <drive> = disk drive number, 0 to 3.
; 
.proc DRIVE
            jsr     GETDRIVEOPND    ; Get drive from command line and ensures it's open
            sta     DEFDRV          ; Set as default
            rts
.endproc

; DELETE Command
;
; DESCRIPTION:  Remove a file from disk
; SYNTAX:       DELETE <file>[:<drive>] ...
; ARGUMENTS:    None.
;               <file>  = file name.
;               <drive> = disk drive, 0 to 3. Defaults to the default drive,
;                         usually 0.
; 
.proc DELETE
            jsr     GETFILNDRV      ; Get file and drive from command line
            jsr     FDELETE         ; Delete file in DIRENT+DIRE::FNAM from drive X
            ldy     CMDLIDX         ; Get command line index
            jsr     GETNEXTNB       ; Get next non-blank
            bne     DELETE          ; Repeat until last argument
            rts
.endproc

; BOOT Command
;
; DESCRIPTION:  Re-boot the CODOS operating sytem from the disk in drive 0
; SYNTAX:       BOOT
; ARGUMENTS:    None.
; 
.proc BOOT
            ldx     #$FF            ; Init stack pointer
            txs                     ;
            cld
            jmp     BOOTP           ; Boot from PROM
.endproc

.ifdef kim1

; KIM Command
;
; DESCRIPTION:  Exit the CODOS monitor and enter the KIM-1 Monitor
;
; SYNTAX:       KIM
;
; ARGUMENTS:    None
;
.proc KIMMON
            jsr     MONITOR
.endproc

.endif

            ; Command name table
            ;
            ;       Name:       Overlay:
            ;
CMDNAMTBL:  .byte   "ASSIGN",   $07
            .byte   "FREE",     $00
            .byte   "OPEN",     $00
            .byte   "CLOSE",    $00
            .byte   "SAVE",     $00
            .byte   "GET",      $00
            .byte   "DUMP",     $04
            .byte   "DRIVE",    $00
            .byte   "DELETE",   $00
            .byte   "LOCK",     $06
            .byte   "UNLOCK",   $06
            .byte   "FILES",    $08
            .byte   "DISK",     $10
            .byte   "SET",      $01
            .byte   "UNPROTECT",$01
            .byte   "BEGINOF",  $05
            .byte   "ENDOF",    $05
            .byte   "GO",       $00
            .byte   "REG",      $03
            .byte   "PROTECT",  $01
            .byte   "FILL",     $02
            .byte   "NEXT",     $00
            .byte   "TYPE",     $05
            .byte   "DATE",     $03
            .byte   "GETLOC",   $09
            .byte   "COPY",     $0A
            .byte   "RENAME",   $0B
            .byte   "SVC",      $06
            .byte   "BOOT",     $00
            .byte   "DO",       $07
            .byte   "HUNT",     $0C
            .byte   "COMPARE",  $0F
.ifdef mtu
            .byte   "ONKEY",    $0E
.endif
            .byte   "BP",       $00
            .byte   "RESAVE",   $00
            .byte   "MSG",      $06
.ifdef kim1
            .byte   "KIM",      $00
.endif
            ; Reserved for new commands

.ifdef mtu  
            .byte   $00, $00, $00, $00, $00, $00, $00, $00
            .byte   $00, $00, $00, $00, $00, $00, $00, $00
            .byte   $00, $00, $00, $00, $00, $00, $00, $00
.endif

            ; Command function table
            ;
            ;       Entry point:
CMDFUNTBL:  .word   $0000
            .word   OVLORG+$01      ; Assign
            .word   FREE
            .word   OPEN
            .word   CLOSE
            .word   SAVE
            .word   GETCMD
            .word   OVLORG+$01      ; Dump
            .word   DRIVE
            .word   DELETE
            .word   OVLORG+$01      ; Lock
            .word   OVLORG+$1F      ; Unlock
            .word   OVLORG+$01      ; Files
            .word   OVLORG+$01      ; Disk
            .word   OVLORG+$01      ; Set
            .word   OVLORG+$CD      ; Unprotect
            .word   OVLORG+$BF      ; Beginof
            .word   OVLORG+$CD      ; Endof
            .word   GOCMD
            .word   OVLORG+$01      ; Reg
            .word   OVLORG+$D7      ; Protect
            .word   OVLORG+$01      ; Fill
            .word   NEXT
            .word   OVLORG+$01      ; Type
            .word   OVLORG+$6A      ; Date
            .word   OVLORG+$01      ; Getloc
            .word   OVLORG+$01      ; Copy
            .word   OVLORG+$01      ; Rename
            .word   OVLORG+$A7      ; SCV
            .word   BOOT
            .word   OVLORG+$F0      ; Do
            .word   OVLORG+$01      ; Hunt
            .word   OVLORG+$01      ; Compare
.ifdef mtu
            .word   OVLORG+$01      ; Onkey
.endif
            .word   BREAKP
            .word   RESAVE
            .word   OVLORG+$B3      ; Msg
.ifdef kim1
            .word   KIMMON          ; Kim
.endif
.if .not (.defined(mtu) .or .defined(kim1))
            .word   $0000           ; Reserved
.endif
            .word   $0000           ; Reserved
            .word   $0000           ; Reserved
            .word   $0000           ; Reserved
            .word   $0000           ; Reserved

CMDPROC_SIZE = * - CMDPROC