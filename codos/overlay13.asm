; da65 V2.19 - Git cf0688fc5
; Created:    2025-05-05 12:23:27
; Input file: kk13.bin
; Page:       1


            .setcpu "6502"

            .include "symbols.inc"
            .include "codos.inc"

            .segment "overlays"

            .byte   $0D             ; Overlay number

; SVC 27 - Enter the CODOS 16-bit Pseudo-processor
;
; Arguments:            First through n-th bytes following the SVC are instructions
;                       for the 16-bit pseudo processor. A zero instruction
;                       (not zero byte) terminates the string. 
;
; Arguments returned:   Flags
;
CODOS16:    clc                     ; Clears bit 0 of BYTRES

SETCYST:    rol     BYTRES          ; Set carry status in bit 1 of BYTRES
DEFRET:     cld                     ; Clear decimal mode
            jsr     GETARG          ; Get arg, which should be a pseudo-op
            tay                     ; Save it in Y
            and     #$07            ; Get pseudo-reg
            asl     a               ; Get U0 based index
            tax                     ; And store it into X
            tya                     ; Recover arg
            lsr     a               ; Convert instruction into an index to the
            lsr     a               ; instruction table by shifting it to the
            lsr     a               ; right.
            and     #$1E            ; Mask out any other bits
            beq     UEXT            ; If it is instruction 0 (UEXT), jump
            tay                     ; Instruction index into Y
            lda     #>SETCYST       ; Set return address after instruction
            pha                     ;
            lda     #<(SETCYST-1)   ; Return point for ops 1 and 2
            cpy     #3*2            ; Is op < 3 ?
            bcc     @CONT           ; Yes, continue
            lda     #<(DEFRET-1)    ; No, set default return point
@CONT:      pha                     ;
            lda     OPTABLE-1,y     ; Set op entry point
            pha                     ;
            lda     OPTABLE-2,y     ;
            pha                     ;
            rts                     ; And "jump"

; UEXT n
;
; Exit PP mode. Set the Z and N flags to reflect the value in register Un,
; and return to normal 6502 execution mode.
; The Carry flag reflects the last UADD or USUB result.
;
UEXT:       lda     P0SCRATCH,x     ; Check if Un = 0
            ora     P0SCRATCH+1,x   ;
            php                     ; Transfer flags to A
            pla                     ;
            asl     a               ; Shift left to expunge N flag

            clc                     ; clears carry for next check
            ldy     P0SCRATCH+1,x   ; Check if Un is negative
            bpl     @SETN           ; It is positive
            sec                     ; It is negative, set carry
@SETN:      ror     a               ; Injects N flag into A
            lsr     a               ; SHift right to expunge C flag
            ror     BYTRES          ; Moves op carry result to Cy
            rol     a               ; Injects C flag into A
            sta     PROCST          ; And store flags
            ; Fall through

; Update SVC pseudo registers from working copy
;
; At exit, Y = 0
;
UPDATEREGS: ldy     #$11            ; Update pseudo registers
@LOOP:      lda     P0SCRATCH-1,y   ;
            sta     U0-1,y          ;
            dey                     ;
            bne     @LOOP           ;
            rts

; Get first argument in (PCSAVE) and advance one position
;
GETARG:     ldy     #$02            ; Get SVC arg (byte)
            lda     (PCSAVE),y      ;
            inc     PCSAVE          ; And advance one pos
            bne     @RETURN         ;
            inc     PCSAVE+1        ;
@RETURN:    rts

; ULDI n,val
;
; Un = val. 16-bit load immediate. First byte of val is low-order byte,
; second is high-order byte
;
ULDI:       jsr     GETARG          ; Get low byte
            sta     P0SCRATCH,x     ; Store into low byte of Un
            jsr     GETARG          ; Get high byte
            sta     P0SCRATCH+1,x   ; And store it
            rts

; ULDA n,addr
;
; Un = (addr). 16-bit load. The data at address addr is placed in the
; low-order byte of Un and the data at addr+1 is placed in the
; high-order byte
;
ULDA:       jsr     GETWORD         ; Get addr from SVC byte args into L00D2
                                    ; (clears Y)
            lda     (L00D2),y       ; Get (addr)
            sta     P0SCRATCH,x     ; And store it into Un
            iny                     ;
            lda     (L00D2),y       ;
            sta     P0SCRATCH+1,x   ;
            rts

; U0LD n
;
; U0 = (Un). 16-bit load U0 register-indirect. The data pointed to by
; register Un is loaded into the low byte of U0, and the next byte is
; loaded into the high byte of U0
;
U0LD:       jsr     UNL00D2         ; Copies UN to L00D2 and clears Y
            lda     (L00D2),y       ; Get (Un)
            sta     P0SCRATCH       ; And copy into U0
            iny                     ;
            lda     (L00D2),y       ;
            sta     P0SCRATCH+1     ;
            rts


; USTA n,addr
;
; (addr) = Un. 16-bit store. The low byte of Un is stored in memory at
; address addr, and the high byte is stored at addr+1.
;
USTA:       jsr     GETWORD         ; Get addr from SVC byte args into L00D2
                                    ; (clears Y)
            lda     P0SCRATCH,x     ; Get Un and store it into (L00D2) 
            sta     (L00D2),y       ;
            iny                     ;
            lda     P0SCRATCH+1,x   ;
            sta     (L00D2),y       ;
            rts

; U0ST n
;
; (Un) = U0. 16-bit store U0 register-indirect. The low byte of U0 is
; stored at the address in Un, and the high byte is stored at the next
; higher address
; 
U0ST:       jsr     UNL00D2         ; Copies UN to L00D2 and clears Y
            lda     P0SCRATCH       ; Get U0       
            sta     (L00D2),y       ; And store into (L00D2)
            iny                     ;
            lda     P0SCRATCH+1     ;
            sta     (L00D2),y       ;
            rts

; UNU7 n
;
; 24-bit move. Register U0 is moved to the low-order 16 bits of U7,
; and the low-order 8 bits of register Un is moved to the most
; significant (3rd byte)
;
UNU7:       lda     P0SCRATCH,x     ; Copy Lower 8 bits of Un to 
            sta     FILEPOS+2       ; MSB of U7
            ldx     #7*2            ; Copy U0 to U7 (lower 16 bits)
            jmp     U0UN            ; and return

; Get word from SVC arguments and clears Y
;
; Returns word L00D2
;
GETWORD:    jsr     GETARG          ; Get next byte arg (low)
            sta     L00D2           ; store it in L00D2
            jsr     GETARG          ; Get next byte arg (high)
            jmp     STHCLY          ; Go store high and clear Y

; UNL00D2
;
; Copies UN to L00D2 and clears Y
;
UNL00D2:    lda     P0SCRATCH,x
            sta     L00D2
            lda     P0SCRATCH+1,x
            ; Fall through

; Stores A in high byte of L00D2 and clears Y
;
STHCLY:     sta     L00D2+1
            ldy     #$00
            rts

; UJSR n
;
; Call subroutine at (Un). Execute 6502-subroutine whose address is
; in register Un
;
; On entry to the subroutine, register A will contain the low-order byte of U0,
; register X will contain the index needed to address register Un relative to
; register U0 (e.g., 0 if n= 0, 2 if n=1, 8 if s=4, etc.), and Y will be 0.
; The carry flag will reflect the status of the last UADD or USUB operation;
; the N and Z flags will reflect the value of the low order byte of U0.
; The decimal mode flag will be clear. 
;
UJSR:       jsr     UPDATEREGS      ; Update pseudo registers from working copy
                                    ; also, set Y = 0
            lda     #>(JCPSEUDREGS-1) ; Set the return entry point
            pha                     ;
            lda     #<(JCPSEUDREGS-1) ;
            pha                     ;

            ; Decrement Un as we are using a RTS to jump

            lda     P0SCRATCH,x     ; Get low byte of Un
            sec                     ; Clear borrow
            sbc     #$01            ; Decrement low byte
            sta     BYTRES+1        ; Store temporarily into BYTRES+1
            lda     P0SCRATCH+1,x   ; Decrement high byte if borrow
            sbc     #$00            ;
            pha                     ; Set high byte of jump address for the rts
            lda     BYTRES+1        ; Set low byte of jump address
            pha                     ;
            lsr     BYTRES          ; Transfer status of the last UADD or USUB to Cy
            php                     ; Save flags
            rol     BYTRES          ; Restore status
            plp                     ; Recover flags
            lda     P0SCRATCH       ; Load low-order byte of U0 into A
            rts                     ; And jump

; UMUL n
;
; 16-bit multiply
;
; U0 = U0 * Un
; 
; X contains index to pseudo-reg ( X = n + 2 )
;
; Result: Low 16-bits of product in UO, high-order 16 bits in Un 
;
UMUL:       jsr     MULT16          ; U0 * Un -> L00D2:U0
            lda     L00D2           ; Copy L00D2 to Un
            sta     P0SCRATCH,x     ;
            lda     L00D2+1         ;
            sta     P0SCRATCH+1,x   ;
UNOP:       rts

            ; Operations entry point table
            ;
            ; As we arre using an RTS to jump, pointer is dest address minus one
            ;
OPTABLE:    .word   UADD-1
            .word   USUB-1
            .word   UMUL-1
            .word   UDIV-1
            .word   UNU0-1
            .word   U0UN-1
            .word   USWP-1
            .word   ULDI-1
            .word   ULDA-1
            .word   U0LD-1
            .word   USTA-1
            .word   U0ST-1
            .word   UNU7-1
            .word   UJSR-1
            .word   UNOP-1
