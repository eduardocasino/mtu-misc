; da65 V2.19 - Git cf0688fc5
; Created:    2025-04-23 19:06:25
; Input file: svcproc.bin
; Page:       1


            .setcpu "6502"

            .importzp PCSAVE, INPBUFP, OUTBUFP, U0, U3, U5, U7, P0SCRATCH, CMDLIDX
            .importzp CURFINFO, TMPBUFP, FILEPOS, BYTRES, DESTBUFF, MEMBUFF, MEMCOUNT

            .include "codos.inc"

            .segment "svcproc"

            .export SVCPROC

SVCPROC:    jmp     ENTRYP          ; SVC processor entry point

; Copy pseudo-registers to scratch RAM in Page 0
;
            .export CPSEUDREGS

CPSEUDREGS: ldy     #$11
@CPREG:     lda     U0-1,y
            sta     P0SCRATCH-1,y
            dey
            bne     @CPREG
            rts

; SVC function pointer table
;
FNPTRT:     .word   PRCODOS         ; Show registers, enter CODOS monitor
            .word   CODOS           ; Enter CODOS monitor
            .word   OUTMSG          ; Output inline message
            .word   GETCHAR         ; Input byte from channel
            .word   OUTCHAR         ; Output byte over channel
            .word   GETLINE         ; Input line of text from channel
            .word   POUTBUFFCR      ; Same as POUTBUFF, followed by a CR
            .word   POUTBUFF        ; Print Y characters from (OUTBUFP) to channel X
            .word   HEXDECOD        ; Decode hexadecimal ASCII string to 16-bit value
            .word   DECDECOD        ; Decode decimal ASCII string to 16-bit value
            .word   HEXWORD0        ; Encode 16-bit value to hexadecimal ASCII string
            .word   DECWORD         ; Encode 16-bit value to decimal ASCII string
            .word   GETARGS         ; Obtain line buffers and argument index
            .word   CMDEXEC         ; Execute any CODOS Monitor command
            .word   ISASSIGNED      ; Determine if channel is assigned or available
.ifndef KIM1
            .word   RDRECORD        ; Read a record from a channel
            .word   WRRECORD        ; Write a record to a channel
.else
            .word   GETMBUFF        ; Read a record from a channel
            .word   OUTMBUFF        ; Write a record to a channel
.endif
            .word   FREWIND         ; Set file position for a channel to Beginning-of-Data
            .word   FEND            ; Set file position for a channel to End-of-File
            .word   FSEEK           ; Specify the file position for a channel
            .word   FTELL           ; Get position of a file assigned to a channel
            .word   ASSIGNSVC       ; Assign a channel to a device or file
            .word   FREECH          ; Free a channel
            .word   FTRUNC          ; Truncate a file at the present file position
            .word   USERISR         ; define address of an interrupt service routine
            .word   SETERREC        ; Sets a user-defined error recovery routine
            .word   RESTERRP        ; Reinstate normal error processing by CODOS
            .word   CODOS16         ; Enter the CODOS 16-bit Pseudo-processor
            .word   VERSION         ; Return CODOS version
            .word   SCANDEV         ; FSCAN a device or file name/drive
            .word   DATE            ; Get current date
            .word   $0000           ; Unused
            .word   $0000           ; Unused
            .word   $0000           ; Unused

; Return values
;
UPDNONE     = %0000000              ; None
UPDPSTA     = %00000001             ; Update PROCST (Flags)
UPDYREG     = %00000010             ; Update Y
UPDXREG     = %00000100             ; Update X
UPDACCU     = %00001000             ; Update A
UPDPSU0     = %00010000             ; Update U0 (P0SCRATCH)
UPDPSU1     = %00100000             ; Update U1 (MEMBUFF)
UPDPSU2     = %01000000             ; Update U2 (MEMCOUNT)


; Return value table
;
RETVALT:    .byte   UPDNONE
            .byte   UPDNONE
            .byte   UPDNONE
            .byte   UPDACCU | UPDPSTA
            .byte   UPDPSTA
            .byte   UPDACCU | UPDYREG | UPDPSTA
            .byte   UPDNONE
            .byte   UPDNONE
            .byte   UPDPSU0 | UPDACCU | UPDYREG | UPDPSTA
            .byte   UPDPSU0 | UPDACCU | UPDYREG | UPDPSTA
            .byte   UPDYREG
            .byte   UPDYREG
            .byte   UPDYREG
            .byte   UPDNONE
            .byte   UPDACCU | UPDPSTA
            .byte   UPDPSU1 | UPDPSU2 | UPDPSTA
            .byte   UPDPSTA
            .byte   UPDNONE
            .byte   UPDNONE
            .byte   UPDNONE
            .byte   UPDNONE
            .byte   UPDACCU | UPDPSTA
            .byte   UPDNONE
            .byte   UPDNONE
            .byte   UPDNONE
            .byte   UPDNONE
            .byte   UPDNONE
            .byte   UPDNONE
            .byte   UPDACCU | UPDXREG | UPDYREG
            .byte   UPDACCU | UPDYREG | UPDPSTA
            .byte   UPDYREG
            .byte   $00             ; Unused
            .byte   $00             ; Unused
            .byte   $00             ; Unused

; SVC processor entry point
;
ENTRYP:     ldy     #$01            ; Get SVC number
            lda     (PCSAVE),y      ;
            sta     SVCNUM          ;
            cmp     #NUMSVCS        ; Check it is a valid SVC
            bcc     @ISVALID
            jsr     ERROR22         ; Illegal or unimplemented SVC number
            ; Not reached
@ISVALID:   asl     a               ; Calculate offset in SVC function pointer table
            tax                     ;
            lda     FNPTRT,x        ; Get function pointer
            sta     FNPTR           ;
            lda     FNPTRT+1,x      ;
            sta     FNPTR+1         ;
            jsr     CPSEUDREGS      ; Copy pseudo-registers to scratch RAM in Page 0
            ldy     YREG            ; Load processor registers at interrupt
            ldx     XREG            ;
            lda     PROCST          ;
            and     #$F3            ; Set status register with decimal and interrupt
            pha                     ; flags cleared
            lda     ACCUM           ;
            plp                     ;
            jsr     JFNPTR          ; Execute SVC function
            sta     SAVEA           ; Save registers at exit
            stx     SAVEX           ;
            sty     SAVEY           ;
            php                     ;
            pla                     ;
            sta     SAVEP           ;
            ldx     SVCNUM          ; Get SVC number in X
            lda     RETVALT,x       ; Get return value flag 
            rol     a               ; Check bit 7
            sta     RETVALF         ; And save it
            bcc     @CHKPREG        ; Bit 7 set? No, go check pseudo-regs
            ldx     XREG            ; This one does not seem to be used
            lda     P0SCRATCH,x     ; Returns preudo register number X/2
            sta     U0,x            ;
            lda     P0SCRATCH+1,x   ;
            sta     U0+1,x          ;
@CHKPREG:   ldx     #$05            ; Init offset to pseudo-registers to U2
@CHKBIT:    rol     RETVALF         ; Check bit 6,5,4 (update U2, U1, U0)
            bcc     @NEXT           ; No, skip
            lda     P0SCRATCH,x     ; Yes, update
            sta     U0,x            ;
            lda     P0SCRATCH-1,x   ;
            sta     U0-1,x          ;
@NEXT:      dex                     ; Next register
            dex                     ;
            bpl     @CHKBIT         ; Jump if offset is not negative
            ldx     #$03            ; Init offset to processor registers to A
@CHKBIT2:   rol     RETVALF         ; Check bit 3,2,1,0 (Update A, X, Y, PROCST)
            bcc     @NEXT2          ; Not set, skip
            lda     SAVEP,x         ; Yes, update
            sta     PROCST,x        ;
@NEXT2:     dex                     ; Next register
            bpl     @CHKBIT2        ; Jump if offset is not negative
            lda     PCSAVE          ; Advance PC past the SVC args          
            clc                     ; (BRK + num), each SVC takes care of its
            adc     #$02            ;   specific args
            sta     PCSAVE          ;
            bcc     @RETURN         ; If no page change, return
            inc     PCSAVE+1        ;
@RETURN:    clc                     ; Clear flag
            ror     PRBPREGS        ;
            jmp     CONTBP          ; Continue execution after BP
            ; Not reached

JFNPTR:     jmp     (FNPTR)         ; Jump to command function

; SVC 02 - Output inline message over channel
;
; Arguments:            First Byte after SVC 2 = channel number
;                       Second through Nth byte = desired ASCII message text,
;                       terminated by a zero byte ($00). 
; Arguments returned:   None
;
OUTMSG:     lda     PCSAVE          ; Get program counter at BRK
            clc                     ;
            adc     #$03            ; Advance to output message (pasr BRK, SVC num
                                    ; and channel)
            sta     OUTBUFP         ; And point OUTBUFP to it
            lda     PCSAVE+1        ;
            adc     #$00            ;
            sta     OUTBUFP+1       ;
            ldy     #$02            ; Get channel number
            lda     (PCSAVE),y      ;
            tax                     ; POUTBUFF expect channel number in X
            ldy     #$FF            ; Calculate message length
@NCHAR:     iny                     ;
            lda     (OUTBUFP),y     ;
            bne     @NCHAR          ;
            dey                     ; Decrement one to calculate the PC offset, as
                                    ; it is calculated with OUTBUFP (which is 3
                                    ; pos past the PC) and then the processor
                                    ; adds 2 more pos on exit to skip BRK + SVC num
            sty     PCOFFS          ; Save the offset
            iny
            jsr     POUTBUFF        ; Print Y characters from (OUTBUFP) to channel X
            lda     PCOFFS          ; Recover offset
            clc                     ; and calculate new PC
            adc     OUTBUFP         ;
            sta     PCSAVE          ;
            lda     OUTBUFP+1       ;
            adc     #$00            ;
            sta     PCSAVE+1        ;
            rts

; SVC 00 - Show registers, enter CODOS Monitor
;
; Arguments:            None
; Arguments returned:   None
;
PRCODOS:    jsr     OUTREGSLB
            ; Fall through

; SVC 01 - Enter CODOS monitor
;
; Arguments:            None
; Arguments returned:   None
;
CODOS:      jmp     WARMST
            ; Not reached


; SVC 12 - Obtain location of system input line buffer, output line buffer,
;          and arguments passed to user-defined command
;
; Arguments:            None
;
; Arguments returned:   Y  = Index to first argument passed
;                       U5 = Pointer to System Input-line buffer
;                       U6 = Pointer to System Output-line buffer
;
GETARGS:    ldx     #$03            ; Copy INPLBUF and OUTLBUF
@COPY:      lda     INPLBUF,x       ;
            sta     U5,x            ; to U5 and U6
            dex                     ;
            bpl     @COPY           ;
            ldy     CMDLIDX         ; Return index to first argument
            rts

.ifndef KIM1
; SVC 15 - Read a record from a channel.
;
; Arguments:            X = Channel number desired, 0 to 9. Must already be
;                           assigned to a valid file or input device
;                       U1 = Starting address to receive contents of record in
;                           the currently selected data bank
;                       U2 = Size of record to be read, in bytes
;
; Arguments returned:   U1 = Address of last byte read, plus one
;                       U2 = Actual number of bytes read
;                       Flags: If Carry is set then End-of-File was encountered
;                           before any bytes could be read. All other flags are
;                           not meaningfully returned. 
;
; NOTE: At SVC entry, U1 and U2 are copied to MEMBUFF and MEMCOUNT
;
RDRECORD:   lda     DATBANK         ; Set data bank as destination bank
            sta     DSTBANK         ;
            jmp     GETMBUFF        ; Get MEMCOUNT characters from file/device A into (MEMBUFF)
            ; Not reached

; SVC 16 - Write a record to a channel.
;
; Arguments:            X = Channel number desired, 0 to 9. Must already be
;                           assigned to a valid file or output device
;                       U1 = Starting address to receive contents of record in
;                           the currently selected data bank
;                       U2 = Size of record to be read, in bytes
;
; Arguments returned:   U1 = Address of last byte read, plus one
;                       U2 = Actual number of bytes read
;                       Flags: If Carry is set then the channel was positioned at
;                           End-of-File after completing the write 
;
; NOTE: At SVC entry, U1 and U2 are copied to MEMBUFF and MEMCOUNT
;
WRRECORD:   lda     DATBANK         ; Set data bank as destination bank
            sta     DSTBANK         ;
            jmp     OUTMBUFF        ; Print MEMCOUNT characters from (MEMBUFF) to channel X
            ; Not reached
.endif


; SVC 21 - Assign a channel to a device or file
;
; Arguments:            A = Disk drive number, 0 to 3, or single-character device name
;                           to be assigned.
;                       X = Channel number, 0 to 9
;                       U3 = Pointer to file name in memory (applies to assignment to
;                            file only).
;
; Arguments returned:   A = status byte as follows:
;                           Bit 6 = File flag. If not set, then channel assigned
;                                   to device, not file.
;                           Bit 7 = Old flag. If set, then file already exists
;                           Bit 5 = Locked flag. If set, then file is locked (read-only) 
;
;                       Flags: Sign flag and Overflow flag reflect value of bits 7
;                               and 6 respectively of status byte as described above 
; NOTE: At SVC entry, U3 is copied to TMPBUFP
;
ASSIGNSVC:  sta     CURRDRV         ; Set drive as current
            cmp     #$04            ; Check it is a valid one
            bcs     @ISDEV          ; No, then it is a device name
            txa                     ; Save X (channel)
            pha                     ;
            jsr     FNAMFROMBUF0    ; Copy file name from (TMPBUFP) to FNAMBUF
            bcs     @ERROR          ; If failed, go to error
            lda     (TMPBUFP),y     ; Get first char after file name
            cmp     COLON           ; Drive separator?
            bne     @CONT           ; No continue with current drive
            iny                     ; Advance and
            lda     (TMPBUFP),y     ;   get drive num
            sec                     ; Convert to number
            sbc     #'0'            ;
            sta     CURRDRV         ; And set as current drive
            iny                     ; Advance to next character
@CONT:      pla                     ; Recover X (channel)
            tax                     ;
            jsr     ASSIGN          ; Assign it
            lda     ASSIGNFLAG      ; Get assign flags
            bit     ASSIGNFLAG      ; And set processor flags on return 
            rts                     ;
@ISDEV:     jsr     ASSIGN          ; Assign it
            lda     #$00            ; And return all flags cleared
            rts                     ;

@ERROR:     jsr     ERROR12         ; Missing or illegal file name
            ; Not reached


; SVC 14 - Determine the channel assignment for a selected channel
;
; Arguments:            X = Channel number
;
; Arguments returned:   Flags: Carry is set if the channel is assigned
;                       A = disk drive number if returned as 0 to 3; otherwise
;                           returned as the single character device name.
;                           Not meaningfully returned if CY is clear. 
;
ISASSIGNED: jsr     GETDEV          ; Get device or file for channel X
            clc                     ; Start with unassigned
            beq     @RETURN         ; If device is null, then unassigned
            bpl     @ISFILE         ; It is a file
            and     #$7F            ; It is a device. Clear device bit mark,
            lsr     a               ; calculate index to the
            tax                     ; device name table
            lda     DNT,x           ; and get device name
            sec                     ; Set assigned (Carry set)
@RETURN:    rts                     ;
@ISFILE:    tax                     ; Get the drive number from the table of active files
            lda     FINFOTBL+_DRIVE,x
            sec                     ; Set assigned (Carry set)
            rts                     ;


; SVC 20 - Determine the position of a file assigned to a channel
;
; Arguments:            X = Channel number
;
; Arguments returned:   U7 = File position (3 bytes). 
;
FTELL:      jsr     ASSIGNED        ; Get assigned device/file to channel
            bmi     @ISDEV          ; Jump if it is a device
            jsr     GETFINFO        ; Gets FINFO for file, copies it into CURFINFO
                                    ; in page zero and sets CURRDRV
            lda     CURFINFO+_FPOS  ; Get file pos from FINFO
            sec                     ; Substract the file header length
            sbc     #$40            ;
            sta     U7              ; And save it into U7
            lda     CURFINFO+_FPOS+1
            sbc     #$00            ;
            sta     U7+1            ;
            lda     CURFINFO+_FPOS+2
            sbc     #$00            ;
            sta     U7+2            ;
            rts                     ;
@ISDEV:     lda     #$00            ; It is a device,
            sta     U7              ;   return $000000
            sta     U7+1            ;
            sta     U7+2            ;
            rts                     ;


; UNUSED. Seems that it was the initial idea for the SVC 19, but then realized
;         that the registers were already copied at SVC entry
;
_FSEEK:     lda     U7
            sta     FILEPOS
            lda     U7+1
            sta     FILEPOS+1
            lda     U7+2
            sta     FILEPOS+2
            jmp     FSEEK


; SVC 13 - Execute any CODOS Monitor command
;
; Arguments:            U5 = pointer to command in memory
;                       Command must be terminated by CR ($0D). 
;
; Arguments returned:   All registers and Pseudo-registers are returned as set
;                       by the Monitor command executed  
;
CMDEXEC:    jsr     SETINPB         ; Set input buffer
            jsr     SETOUTB         ; Set output buffer
            ldy     #$00            ; Copy command to Input Buffer
@CPYC:      lda     (U5),y          ;
            sta     (INPBUFP),y     ;
            beq     @CPEND          ; If NULL, terminate copy
            cmp     #$0D            ; If CR, terminate copy
            beq     @CPEND          ;
            iny                     ; Next char
            cpy     YLNLIM          ; Check line size limit
            bcc     @CPYC           ; Not reached, continue; else, terminate copy
@CPEND:     lda     PCSAVE+1        ; Save PCSAVE and SCVNUM, that
            pha                     ; may be modified by the execution
            lda     PCSAVE          ; of the command
            pha                     ;
            lda     SVCNUM          ;
            pha                     ;
            sec                     ; Flag that command is executed throug SVC
            ror     SVC13FLG        ;
            ldy     #$00            ; Execute command at the beginning of the input buffer,
            jsr     CKCMDEXEC       ; making sure that the command processor is loaded first
            asl     SVC13FLG        ; Restore flag
            pla                     ; And recover saved values
            sta     SVCNUM          ;
            pla                     ;
            sta     PCSAVE          ;
            pla                     ;
.if CODOS2_VER = 14
            sta     PCSAVE+1
            rts
.else
            jmp     ENDSVC23        ; Continue there,  because there were
            ; Not reached
.endif                              ; modifications to this SVC and wanted to mantain
                                    ; the entry point addresses of next commands


; SVC 24 - Define the address of an interrupt service routine
;
; Arguments:            UO = pointer to interrupt service routine for IRQ
;
; Arguments returned:   None
;
USERISR:    lda     P0SCRATCH       ; Set new interrupt pointer (U0 copied to P0SCRATCH)
            sta     INTSRVP         ;
            lda     P0SCRATCH+1     ;
            sta     INTSRVP+1       ;
            rts


; SVC 25 - Define the address of a user-defined error recovery routine
;
; Arguments:            UO = pointer to error recovery procedure
;
; Arguments returned:   None
;
SETERREC:   lda     P0SCRATCH       ; Set new error recovery pointer
            sta     ERRRCVRYP       ;
            lda     P0SCRATCH+1     ;
            sta     ERRRCVRYP+1     ;
            rts


; SVC 27 - Enter the CODOS 16-bit Pseudo-processor
;
; Arguments:            First through n-th bytes following the SVC are instructions
;                       for the 16-bit pseudo processor. A zero instruction
;                       (not zero byte) terminates the string. 
;
; Arguments returned:   Flags
;
CODOS16:    lda     #$0D            ; This SVC is in overlay $0D
            jsr     OVERLAY         ; so load it

            jmp     OVLORG+1        ; And execute it
            ; Not reached


; SVC 28 - Return information about the version of CODOS which is running
;
; Arguments:            None
;
; Arguments returned:   A = High address byte of the System 8K RAM on the K-1013
;                       disk controller
;                       X = Release level of CODOS, expressed as two hex digits
;                       Y = Code number indicating the kind of system on which CODOS
;                           is running. 
;                           Presently defined codes are:
;                               1 = MTU-130 with 8-inch floppy disk(s)
;                               2 = KIM-1 with 8-inch floppy disk(s) 
;                               3 = AIM-65 with 8-inch floppy disk(s)
;                               4 = PET or CBM with 8-inch floppy disk
;                               5 = SYM-1 with 8-inch floppy disk(s) 
;
VERSION:    ldx     #RELEASE
            ldy     #SYSTEM
            lda     #>SYSRAM
            rts


; SVC 29 - Scan a device or file name/drive in preparation for ASSIGNment to a
;          channel, or to ascertain a file's status. 
;
; Arguments:            Y = Index to start of file or device name in buffer 
;                       U5 = Pointer to input buffer 
;
; Arguments returned:   Y = Index to first character after file name/drive or device
;                           name in buffer.
;                       flags: Cy set if parsed name was a device name; Cy clear if
;                              was a file name
;                       A = status information
;                           If the name was a device name (Cy set) then:
;                             Bit 7 is set to 1 if the specified device does not exist 
;                             Bits 6-0 contain the ASCII device name (1 character)
;                           If the name was a file name (Cy clear) then:
;                             Bit 7 is set to 1 if illegal name/drive number
;                             Bit 6 is set to 1 if the drive is not open
;                             Bit 5 is set to 1 if the file exists
;                             Bit 4 is set to 1 if the drive is write-protected
;                             Bits 3 and 2 are not used
;                             Bits 1 and 0 contain the drive number selected
;
;                       U3 = Points to first character of file/device name in the buffer
;
;
SCANDEV:    lda     INPBUFP         ; Set TMPBUFP
            sta     TMPBUFP         ;
            lda     INPBUFP+1       ;
            sta     TMPBUFP+1       ;
            sty     CINDEX          ; Index to start device/file name
            jsr     FSCAN           ; Call to CODOS FSCAN function
            php                     ; Save processor flags
            pha                     ; and return flags
            lda     INPBUFP         ;
            clc                     ; Update U3 with pointer to first char of file/device
            adc     CINDEX          ;
            sta     U3              ;
            lda     INPBUFP+1       ;
            adc     #$00            ;
            sta     U3+1            ;
            pla                     ; Recover return flags
            plp                     ; and processor flags
            rts


; SVC 30 - Obtain current date
;
; Arguments:            Y = Index to start of nine character date field in buffer
;                       U6 = Pointer to buffer 
;
; Arguments returned:   Y = Index to next available character after the date
;
DATE:       ldx     #$00            ; Copy current system date
@LOOP:      lda     TDATE,x         ; to index at output buffer
            sta     (OUTBUFP),y     ; (U6 is copied to OUTBUFP at entry)
            iny                     ; Next char
            inx                     ;
            cpx     #$09            ; Are we past last character?
            bne     @LOOP           ; No, get it
            rts


SAVEP:      .byte $00               ; Saved registers on entry
SAVEY:      .byte $00               ;
SAVEX:      .byte $00               ;
SAVEA:      .byte $00               ;
FNPTR:      .word $0000             ; Function pointer of current SVC
SVCNUM:     .byte $00               ; Invoked SVC number
RETVALF:    .byte $00               ; Used to save the return values flag for SVC
PCOFFS:     .byte $00               ; Offset to calculate PC after message of SVC 2
CINDEX:     .byte $00               ; Index in command line


.if CODOS2_VER <> 14
; Continuation of the SVC 13
;
ENDSVC23:   sta     PCSAVE+1        ; Finish to recover PCSAVE
.ifndef KIM1
            lda     #$00            ; Set default program bank
            sta     PRGBANK         ;
.if CODOS2_VER = 17
            sta     DATBANK
.endif
.endif
            rts
.endif
