; da65 V2.18 - Ubuntu 2.19-1
; Created:    2025-06-18 13:23:27
; Input file: cpuid.bin
; Page:       1


            .setcpu "6502"

            .include "symbols.inc"
            .include "monomeg.inc"

.macro svc number
		brk
        .byte number
.endmacro

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
            cld
            sec                     ; Enable SVCs
            ror     SVCENB          ;
            lda     #$FF
            sta     SEEIO           ; Set I-O enable flag
            sta     IOENABLE        ; Enable I/O space from $BE00 to $BFFF

            svc     $0C             ; SVC #12 : Obtain location of system input line buffer,
                                    ; output line buffer, and arguments passed to user-defined command.
                                    ; (NOTE: Seems unnecessary)

            sta     SPREGREN        ; Prepares reading of special registers
            lda     SPREGREAD       ; Skip first byte

            svc     $02             ; SVC #2 : Output message to channel
            .byte   $02             ; Channel 2 : Console
            .byte   $0D, "VENDOR NUMBER = ", $00

            jsr     PRGROUP

            svc     $02             ; SVC #2 : Output message to channel
            .byte   $02             ; Channel 2 : Console
            .byte   $0D, $0D, "GROUP NUMBER = ", $00

            jsr     PRGROUP

            svc     $02             ; SVC #2 : Output message to channel
            .byte   $02             ; Channel 2 : Console
            .byte   $0D, $0D, "USER NUMBER = ", $00

            jsr     PRGROUP

            svc     $02             ; SVC #2 : Output message to channel
            .byte   $02             ; Channel 2 : Console
            .byte   $0D, $00        ; Just a CR

            rts
.endproc

.proc PRGROUP
            ldy     #$05            ; Length of each group
LOOP:       lda     SPREGREAD       ; Get number
            and     #$0F            ; Mask out upper nibble
            clc                     ; Convert to ascii
            adc     #$30            ;
            cmp     #$3A            ; Check if decimal
            bcc     PRNUM           ; Yes, skip
            adc     #$06            ; No, convert to letter
PRNUM:      ldx     #$02            ; Channel 2 (console) for SVC #4
            svc     $04             ; SVC #4 : Output byte in A over channel in X
            dey                     ; Next byte
            bne     LOOP            ; Repeat until done
            rts
.endproc

PROG_SIZE = * - START

            .end
