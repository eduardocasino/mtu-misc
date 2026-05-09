; =============================================================================
; DISASM.ASM  –  MTU 6502 DISASSEMBLER
; =============================================================================
; Companion utility to MACASM.ASM (MTU 6502 Macro Assembler).
; Disassembles a range of 6502 machine code, formatting each instruction
; as: <address>  <hex bytes>  <mnemonic> <operand>
;
; Target CPU  : MOS Technology 6502
; Host OS     : CODOS (MTU Disk Operating System)
;
; Usage (at the DISASM> prompt):
;   <start> [<end>] [L<lines>] [P<channel>]
;
;   start    Hex start address (required)
;   end      Hex end address   (optional; takes priority over line count)
;   L<n>     Decimal number of lines to disassemble (default: 22)
;   P<ch>    Output channel number (default: 2 = console)
;
; =============================================================================

            .setcpu "6502"

            .include "symbols.inc"

.macro svc number
            brk
            .byte   number
.endmacro

; =============================================================================
; ZERO-PAGE ALIASES
;
; Reuses CODOS 16-bit pseudo-registers for page-zero data and pointers
; =============================================================================
ZP_DISASM       = U1        ; Current disassembly pointer
ZP_END          = U2        ; End-address limit
LINE_COUNT      = U3        ; Lines per page
REMAINING_LINES = U3+1      ; Remaining lines counter (counts down)
END_MODE_FLAG   = U4        ; $00 = line-count mode; $80 = end-address mode
OUT_CHANNEL     = U4+1      ; Active output channel number
OPFMT_BYTE      = U7        ; Operand format byte (suffix mask + arg count)
ARG_BYTE_COUNT  = U7+1      ; Argument byte count (0, 1, or 2)

            .code

; =============================================================================
; CODOS LOADABLE FILE HEADER
; =============================================================================
            .byte   $58             ; CODOS loadable file signature byte
            .byte   $00             ; Memory overlay flag
            .byte   $00             ; Memory bank
            .byte   $00             ; Reserved
            .word   START           ; Entry point address
            .word   START           ; Load address
            .word   PROG_SIZE       ; Memory image size in bytes

START:      jmp     MAIN_ENTRY      ; Normal entry point

CONTINUE_CHAR:
            .byte   $11             ; ^Q (DC1/XON) Continue character
DEFAULT_LINE_COUNT:
            .byte   $16             ; Default number of lines to disassemble (22)

; =============================================================================
; MAIN_ENTRY
;
; Assembler start-up. Enables the SVC handler, obtains the CODOS system
; buffer addresses, and falls through to the main prompt/parse loop.
; =============================================================================
MAIN_ENTRY:
            sec                     ; Set carry
            ror     SVCENB          ; Enable SVC
            svc     $0c             ; Obtain system buffer addresses (cmd line, args)
            jmp     PARSE_ARGS      ; Jump to argument parser

; =============================================================================
; PROMPT_AND_INPUT
;
; Prints "DISASM>" on channel 2, reads a line of text from channel 1,
; and returns.  If the user enters a bare CR the SVC handler is disabled
; and control returns to CODOS.
; =============================================================================
PROMPT_AND_INPUT:
            svc     $02             ; Output inline message on channel
            .byte   $02             ; -> channel 2 (console)
            .byte   $0d, "DISASM>", $00

            ldx     #$01            ; Channel 1 (console input)
            svc     $05             ; Input line of text from channel
            lda     (U5),Y          ; Read first character of input
            cmp     #$0d            ; Empty line?
            bne     PARSE_ARGS      ; No: parse the input line
            asl     SVCENB          ; Yes: Disable SVC handler and
            rts                     ;  return to CODOS

; =============================================================================
; PARSE_ARGS
;
; Parses the command-line arguments left by SVC $0C / $05:
;   1. Decodes the start address (hex) via SVC $08.
;   2. If only CR follows, jumps to DISASM_LOOP using last start address.
;   3. Decodes optional end address (hex).
;   4. Recognises 'L' (line count, decimal) and 'P' (output channel) options.
;   5. On any parse error jumps to the appropriate error handler.
;
; On successful parse, sets up:
;   ZP_DISASM        = start address
;   ZP_END           = end address (bit7 set if in use)
;   LINE_COUNT       = number of lines
;   OUT_CHANNEL      = output channel
; =============================================================================
PARSE_ARGS:
            jsr     INIT_DEFAULTS   ; Restore default line count and channel

            svc     $08             ; Decode hex ASCII string -> U0
            bcs     PA_GOT_START    ; Carry set = valid hex value decoded
            jmp     ERR_START_ADDR  ; No valid start address -> error

PA_GOT_START:
            pha                     ; Save the character that terminated the decode
            lda     U0              ; Copy decoded address lo
            sta     ZP_DISASM       ; Save start address
            lda     U0+1            ; Decoded address hi
            sta     ZP_DISASM+1
            pla
            cmp     #$0d            ; Only CR after start address?
            bne     PA_TRY_END      ; No: try to parse end address / options
PA_JMP_DISASM_LOOP:
            jmp     DISASM_LOOP     ; Yes: disassemble from start address

PA_TRY_END:
            svc     $08             ; Decode next hex field -> U0
            bcs     PA_GOT_END      ; Carry set = valid end address
            cmp     #$0d            ; CR (no end address, no options)?
            beq     PA_JMP_DISASM_LOOP ; Yes: start disassembly
            cmp     #'L'            ; 'L' = line-count option?
            beq     PA_LINES
            cmp     #'P'            ; 'P' = channel option?
            beq     PA_CHANNEL_OPT
            jmp     ERR_END_ADDR    ; Unexpected character -> error

PA_LINES:
            iny                     ; Skip 'L'
            svc     $09             ; Decode decimal ASCII string -> U0
            bcs     PA_GOT_LINES    ; Carry set = valid decimal value
            cmp     #$0d            ; End of line?
            beq     PA_JMP_DISASM_LOOP ; Yes: start disassembly
            bne     PA_TRY_CHANNEL  ; Extra characters -> error

PA_HEX_LINES:
            svc     $08             ; Decode hex ASCII string -> U0
            bcc     PA_HEX_LINE_CHK ; Carry clear = no hex digits
            jmp     ERR_TOO_MANY    ; Carry set = extra hex parameter -> error

PA_HEX_LINE_CHK:
            cmp     #$0d            ; End of line?
            beq     PA_JMP_DISASM_LOOP ; Yes: start disassembly
            jmp     ERR_TOO_MANY    ; Extra content -> error

PA_GOT_LINES:
            pha                     ; Save terminator
            lda     U0+1            ; High byte of line count
            beq     PA_STORE_LINES  ; Zero -> fits in one byte, OK
            pla
            jmp     ERR_LINES_LARGE ; Count too large -> error

PA_STORE_LINES:
            lda     U0              ; Line count low byte
            sta     LINE_COUNT      ; Store as active line count
            pla                     ; Restore terminator
            cmp     #$0d            ; End of line?
            beq     PA_JMP_DISASM_LOOP ; Yes: start disassembly
            bne     PA_TRY_CHANNEL  ; More options follow

PA_GOT_END:
            pha                     ; Save terminator
            lda     U0              ; End address lo
            sta     ZP_END
            lda     U0+1            ; End address hi
            sta     ZP_END+1
            lda     #$80
            sta     END_MODE_FLAG   ; Set end-address mode flag
            pla                     ; Restore terminator
            cmp     #$0d            ; End of line?
            bne     PA_TRY_CHANNEL  ; No: try to parse channel option
PA_JMP_DISASM_LOOP2:
            jmp     DISASM_LOOP     ; Yes: start disassembly

PA_TRY_CHANNEL:
            svc     $08             ; Decode next hex field
            bcs     PA_GOT_CHAN_HEX ; Carry set = decoded a value
            cmp     #'P'            ; 'P' = channel option?
            bne     PA_AFTER_CHAN

PA_CHANNEL_OPT:
            ldx     #$06            ; Default channel 6 for file output
            stx     OUT_CHANNEL
            svc     $15             ; Assign channel to device or file
            iny                     ; Advance past device specifier
            jmp     PA_HEX_LINES    ; Continue parsing

PA_AFTER_CHAN:
            cmp     #$0d            ; End of line?
            beq     PA_JMP_DISASM_LOOP2 ; Yes: start disassembly
            jmp     ERR_ILLEGAL_CHAN ; Extra content -> error

PA_GOT_CHAN_HEX:
            pha                     ; Save accumulator
            lda     U0+1            ; High byte of channel
            beq     PA_CHK_CHAN_VAL
PA_BAD_CHAN:
            pla                     ; Restore accumulator
            jmp     ERR_ILLEGAL_CHAN

PA_CHK_CHAN_VAL:
            lda     U0              ; Channel number
            cmp     #$0a            ; Must be < 10
            bcs     PA_BAD_CHAN     ; No: error
            sta     OUT_CHANNEL     ; Store channel number
            pla                     ; Restore accumulator
            cmp     #$0d            ; End of line?
            beq     PA_JMP_DISASM_LOOP2 ; Yes: start disassembly
            jmp     PA_HEX_LINES    ; No: continue parsing

; =============================================================================
; INIT_DEFAULTS
;
; Resets the line count, end-mode flag, and output channel to their defaults
; before each new disassembly command is parsed.
; =============================================================================
INIT_DEFAULTS:
            lda     DEFAULT_LINE_COUNT  ; Default lines per screen
            sta     LINE_COUNT
            lda     #$00
            sta     END_MODE_FLAG       ; Line-count mode (not end-address)
            lda     #$02
            sta     OUT_CHANNEL         ; Default output channel: 2 (console)
            rts

; =============================================================================
; DISASM_LOOP
;
; Main disassembly loop.  Disassembles one instruction per iteration.
;
; In line-count mode  (END_MODE_FLAG = $00): decrements LINE_COUNT; when it reaches
;   zero, pauses and waits for a keypress:
;     DC1/XON ($11)  -> continue from current address
;     anything else  -> reprint the prompt
;
; In end-address mode (END_MODE_FLAG = $80): continues until the current address
;   exceeds ZP_END, then returns to the prompt.
; =============================================================================
DISASM_LOOP:
            lda     LINE_COUNT          ; Reset the 
            sta     REMAINING_LINES     ;   remaining lines counter

PAGE_LOOP:
            jsr     DISASM_ONE_INSTR    ; Disassemble one instruction
            jsr     ADVANCE_PC          ; Advance PC past instruction bytes
            sta     ZP_DISASM           ; Save next address lo
            sty     ZP_DISASM+1         ; Save next address hi

            ldx     #$02
            cpx     OUT_CHANNEL         ; Are we outputting to the console (ch 2)?
            beq     PL_CHECK_MODE       ; Yes: check pause condition
            lda     #$00
            svc     $04                 ; Output null byte on non-console channel

PL_CHECK_MODE:
            bit     END_MODE_FLAG       ; End-address mode?
            bmi     PL_END_CHECK        ; Yes: compare against end address

            dec     REMAINING_LINES     ; Line-count mode: decrement counter
            bne     PAGE_LOOP           ; Not zero: continue

            ; Counter reached zero: pause and wait for keypress
            ldx     #$01                ; Channel 1 (keyboard)
            svc     $03                 ; Input one character from channel
            cmp     CONTINUE_CHAR       ; DC1/XON ($11) = "continue" key?
            beq     DISASM_LOOP         ; Yes: re-fill page from current address
            lda     #$0d                ; Send CR before returning to prompt
            jsr     OUTPUT_BYTE
            jmp     PROMPT_AND_INPUT    ; Back to prompt

PL_END_CHECK:
            sec
            lda     ZP_END              ; Compare current PC against end address
            sbc     ZP_DISASM
            lda     ZP_END+1
            sbc     ZP_DISASM+1
            bcs     PAGE_LOOP           ; Still before end: continue
            lda     #$0d
            jsr     OUTPUT_BYTE
            jmp     PROMPT_AND_INPUT    ; Past end: back to prompt

; =============================================================================
; DISASM_ONE_INSTR
;
; Disassembles the single instruction at (ZP_DISASM) and writes a
; formatted listing line to OUT_CHANNEL.
;
; Output format:
;   AAAA  BB [BB [BB]]  MMM [operand]
;   where AAAA = address (4 hex digits), BB = raw bytes, MMM = mnemonic.
;
; Steps:
;   1. Call EMIT_LINE_PREFIX to output CR + address…
;   2. Output the address (AAAA) and raw bytes.
;   3. Pad to the mnemonic column.
;   4. Decode and output the 3-character mnemonic from MNEMONIC.
;   5. Output the operand (with addressing-mode suffixes).
; =============================================================================
DISASM_ONE_INSTR:
            jsr EMIT_LINE_PREFIX ; Output CR + address to start the listing line
            ldx     #$00
            lda     (ZP_DISASM,X)   ; Read opcode byte
            tay                     ; Save opcode in Y

            ; Determine operand byte count from opcode bits
            lsr     a
            bcc     DOI_SMALL       ; Bit 0 clear: use short path
            lsr     a
            bcs     DOI_SPECIAL     ; Bits 1:0 = 11 -> special
            cmp     #$22            ; $22 (after 1 shift) -> special
            beq     DOI_SPECIAL
            and     #$07
            ora     #$80
DOI_SMALL:
            lsr     a               ; Shift to get byte-count index
            tax
            lda     OPFMT_TABLE,x   ; Look up operand format byte
            bcs     DOI_USE_HI
            lsr     a               ; Use low nibble
            lsr     a
            lsr     a
            lsr     a
DOI_USE_HI:
            and     #$0f            ; Isolate nibble
            bne     DOI_STORE_FMT
DOI_SPECIAL:
            ldy     #$80            ; Special/unknown opcode
            lda     #$00
DOI_STORE_FMT:
            tax                     ; X = format table index
            lda     ADDR_FMT_TABLE,x ; Load address-mode format byte
            sta     OPFMT_BYTE      ; Suffix-mask + arg-byte-count
            and     #$03
            sta     ARG_BYTE_COUNT  ; Argument byte count (0/1/2)

            ; Reconstruct mnemonic index in Y (from saved opcode)
            tya                     ; Restore opcode
            and     #$8f            ; Mask to relevant bits
            tax
            tya
            ldy     #$03            ; Prepare loop counter for bit extraction

            cpx     #$8a            ; Special case: opcode = $8A (TXA)?
            beq     DOI_SKIP_SHIFT
DOI_SHIFT_LOOP:
            lsr     a
            bcc     DOI_SKIP_SHIFT  ; Carry clear: stop shifting
            lsr     a
DOI_SHIFT2: lsr     a
            ora     #$20
            dey
            bne     DOI_SHIFT2
            iny
DOI_SKIP_SHIFT:
            dey
            bne     DOI_SHIFT_LOOP

            pha                     ; Save mnemonic sub-index

DOI_RAW_LOOP:
            lda     (ZP_DISASM),y   ; Read instruction byte at offset Y
            jsr     EMIT_HEX_BYTE   ; Output as 2 hex digits
            ldx     #$01
DOI_COL_LOOP:
            jsr     EMIT_SPACES     ; Pad with spaces between fields
            cpy     ARG_BYTE_COUNT
            iny
            bcc     DOI_RAW_LOOP    ; More bytes to output

            ldx     #$03
            cpy     #$03
            bcc     DOI_COL_LOOP    ; Pad remaining columns

            pla                     ; Restore mnemonic sub-index
            tay

            ; Decode and output the 3-character mnemonic
            lda     MNEM_HI,y       ; High byte of mnemonic word pair
            sta     MNEMONIC        ; Temp storage for bit extraction
            lda     MNEM_LO,y       ; Low byte
            sta     MNEMONIC+1

            ; Each mnemonic character is encoded in 5 bits; add $3F for ASCII
DOI_MNEM_LOOP:
            lda     #$00
            ldy     #$05
DOI_MNEM_SHIFT:
            asl     MNEMONIC+1      ; Shift mnemonic bits left
            rol     MNEMONIC
            rol     a
            dey
            bne     DOI_MNEM_SHIFT
            adc     #$3f            ; Bias to ASCII printable range
            jsr     OUTPUT_BYTE     ; Output mnemonic character
            dex
            bne     DOI_MNEM_LOOP   ; 3 characters total

            ; Output space(s) between mnemonic and operand
            jsr     EMIT_1_SPACE

            ; Output operand: hex value with addressing-mode suffixes
            ldx     #$06            ; Suffix loop counter (6 bits in mask)
DOI_SUFFIX_LOOP:
            cpx     #$03            ; At the operand-value position?
            bne     DOI_NOT_VALUE
            ldy     ARG_BYTE_COUNT  ; Any argument bytes?
            beq     DOI_NOT_VALUE   ; Zero: no operand to print
            lda     #'$'
            jsr     OUTPUT_BYTE     ; Output '$' prefix

DOI_EMIT_OPERAND:
            lda     OPFMT_BYTE      ; Check operand size flag
            cmp     #$e8            ; Flag for 2-byte (absolute) operand
            lda     (ZP_DISASM),y   ; Read operand byte
            bcs     DOI_EMIT_ABS_OPERAND ; 2-byte: output high then low
            jsr     EMIT_HEX_BYTE   ; 1-byte: output single hex pair
            dey
            bne     DOI_EMIT_OPERAND ; More bytes

DOI_NOT_VALUE:
            asl     OPFMT_BYTE      ; Shift out next suffix bit
            bcc     DOI_NEXT_SUFFIX ; Bit was 0: no suffix
            lda     SUFFIX_CHARS,x  ; Primary suffix character
            jsr     OUTPUT_BYTE
            lda     SUFFIX_CHARS2,x ; Secondary suffix character (0 = none)
            beq     DOI_NEXT_SUFFIX
            jsr     OUTPUT_BYTE

DOI_NEXT_SUFFIX:
            dex
            bne     DOI_SUFFIX_LOOP
            rts

; =============================================================================
; DOI_EMIT_ABS_OPERAND
;
; Emits a 2-byte (absolute) operand in big-endian order:
;   1. Calls ADVANCE_PC_BYTE to step past the low operand byte and retrieve it.
;   2. Saves the low byte in X and increments it by 1 to step to the high byte.
;      If the low byte wraps ($FF -> $00) Y is incremented to carry into the
;      high address byte.
;   3. Falls through to DOI_EMIT_HI_BYTE to output the high byte from Y.
;
; On entry: Y = offset of the low operand byte within (ZP_DISASM)
;           A = low operand byte (already read by caller)
; On exit : A = high operand byte (passed to EMIT_ADDR_HEX)
;           X = low  operand byte + 1  (passed to EMIT_ADDR_HEX)
; =============================================================================
DOI_EMIT_ABS_OPERAND:
            jsr     ADVANCE_PC_BYTE ; Advance PC past low byte; result in A/Y
            tax                     ; X = low operand byte
            inx                     ; Step to high-byte offset
            bne     DOI_EMIT_HI_BYTE ; No page cross
            iny                     ; Page cross: increment high address byte

; =============================================================================
; DOI_EMIT_HI_BYTE
;
; Transfers the high byte of the 2-byte operand (held in Y) to A and falls
; through to EMIT_ADDR_HEX, which outputs the full 16-bit value as 4 hex
; digits (high byte first, then low byte in X).
; =============================================================================
DOI_EMIT_HI_BYTE:
            tya                     ; A = high operand byte
            ; Fall through to EMIT_ADDR_HEX

; =============================================================================
; EMIT_ADDR_HEX
;
; Outputs a 16-bit address (hi byte in A, lo byte in X) as 4 uppercase hex
; digits to OUT_CHANNEL.  Calls EMIT_HEX_BYTE twice.
; =============================================================================
EMIT_ADDR_HEX:
            stx     SAVEX           ; Save lo byte
            jsr     EMIT_HEX_BYTE   ; Emit hi byte (A)
            lda     SAVEX           ; Restore lo byte
            jmp     EMIT_HEX_BYTE   ; Emit lo byte and return

; =============================================================================
; EMIT_LINE_PREFIX
;
; Starts a new disassembly output line by writing a carriage return followed
; by the current disassembly address (4 uppercase hex digits) to OUT_CHANNEL.
; Called once per instruction at the top of DISASM_ONE_INSTR.
; =============================================================================
EMIT_LINE_PREFIX:
            ldx     OUT_CHANNEL     ; Output channel
            lda     #$0d            ; Carriage return
            svc     $04             ; Output byte on channel
            lda     ZP_DISASM+1     ; Address hi byte
            ldx     ZP_DISASM       ; Address lo byte
            jsr     EMIT_ADDR_HEX   ; Output 4-digit hex address

; =============================================================================
; EMIT_1_SPACE
;
; Outputs one space character to OUT_CHANNEL.
; =============================================================================
EMIT_1_SPACE:
            ldx     #$01
; Falls through

; =============================================================================
; EMIT_SPACES
;
; Outputs X space characters to OUT_CHANNEL.
; =============================================================================
EMIT_SPACES:
            lda     #$20            ; Space character
            jsr     OUTPUT_BYTE
            dex
            bne     EMIT_SPACES
            rts

; =============================================================================
; ADVANCE_PC
;
; Advances the disassembly pointer (ZP_DISASM) by ARG_BYTE_COUNT + 1
; (the opcode byte plus operand bytes).  Returns the new address in A (lo)
; and Y (hi), with carry set for page-cross detection.
; =============================================================================
ADVANCE_PC:
            lda     ARG_BYTE_COUNT  ; Number of operand bytes
            sec                     ; Include the opcode itself (+1)
; Falls through

; =============================================================================
; ADVANCE_PC_BYTE
;
; Adds A + carry to ZP_DISASM, returning the result in A/Y.
; =============================================================================
ADVANCE_PC_BYTE:
            ldy     ZP_DISASM+1     ; Load current PC hi
            tax
            bpl     APC_POS
            dey                     ; Handle page boundary for negative offset
APC_POS:
            adc     ZP_DISASM       ; Add offset to lo byte
            bcc     APC_DONE
            iny                     ; Carry: increment hi byte
APC_DONE:
            rts

; =============================================================================
; OUTPUT_BYTE
;
; Outputs the byte in A to the channel in OUT_CHANNEL via SVC $04.
; Preserves X.
; =============================================================================
OUTPUT_BYTE:
            stx     SAVEX           ; Save X
            ldx     OUT_CHANNEL     ; Load output channel
            svc     $04             ; Output byte over channel
            ldx     SAVEX           ; Restore X
            rts

; =============================================================================
; EMIT_HEX_BYTE
;
; Encodes the byte in A as a 2-digit uppercase hexadecimal ASCII string and
; outputs it to OUT_CHANNEL via SVC $0A + SVC $07.
; Preserves X and Y.
; =============================================================================
EMIT_HEX_BYTE:
            sty     SAVEY           ; Save Y
            stx     SAVEX           ; Save X
            ldy     #$00
            sty     U0              ; Zero hi byte of value
            sta     U0+1            ; Store byte to encode in lo
            svc     $0a             ; Encode 16-bit value to hex ASCII string
            ldx     OUT_CHANNEL     ; Output channel
            ldy     #$02            ; 2 hex digits to output
            svc     $07             ; Output string of text on channel
            ldy     SAVEY           ; Restore Y
            ldx     SAVEX           ; Restore X
            rts

; =============================================================================
; OPFMT_TABLE
;
; 64-byte intermediate look-up table.
; A 6-bit key derived from the opcode is used to index this table.
; Each byte encodes two packed 4-bit fields:
;   High nibble -> index into ADDR_FMT_TABLE (addressing/operand format)
;   Low nibble  -> index into MNEM_HI / MNEM_LO (mnemonic identifier)
;
; The 6-bit key is constructed from bits of the opcode as follows:
;   key = (opcode >> 1) & $3F   for most opcodes
; with special handling for implied/accumulator modes.
; =============================================================================
OPFMT_TABLE:
            .byte   $40, $02, $45, $03, $D0, $08, $40, $09
            .byte   $30, $22, $45, $33, $D0, $08, $40, $09
            .byte   $40, $02, $45, $33, $D0, $08, $40, $09
            .byte   $40, $02, $45, $B3, $D0, $08, $40, $09
            .byte   $00, $22, $44, $33, $D0, $8C, $44, $00
            .byte   $11, $22, $44, $33, $D0, $8C, $44, $9A
            .byte   $10, $22, $44, $33, $D0, $08, $40, $09
            .byte   $10, $22, $44, $33, $D0, $08, $40, $09
            .byte   $62, $13, $78, $A9

; =============================================================================
; ADDR_FMT_TABLE
;
; 13-byte addressing-mode / operand-format table.
; Each byte encodes:
;   Bits 7..2  Suffix-enable mask (6 bits, one per possible suffix in LB689/LB68F)
;   Bits 1..0  Argument byte count:
;                $00 = no argument   (1-byte instruction)
;                $01 = 1-byte arg    (zero-page / immediate / branch)
;                $02 = 2-byte arg    (absolute address)
;                $03 = special
;
; Index  Encoded  Arg  Suffixes active
;   0    $00       0   none              (implied / accumulator: no operand)
;   1    $21       1   (#)               (immediate:  #$xx)
;   2    $01       1   none              (zero-page:   $xx)
;   3    $02       2   none              (absolute:    $xxxx)
;   4    $00       0   none              (accumulator: A)
;   5    $80       0   (A)               (accumulator explicit: ASL A)
;   6    $59       1   (,Y)(              (zero-page indirect Y: ($xx),Y)
;   7    $4D       1   (,X)(             (zero-page indirect X: ($xx,X))
;   8    $11       1   (,X)              (zero-page,X: $xx,X)
;   9    $12       1   (,Y)              (zero-page,Y: $xx,Y)
;  10    $06       2   (,X)              (absolute,X: $xxxx,X)
;  11    $4A       2   (,Y)(             (absolute indirect: ($xxxx))
;  12    $05       2   (,Y)              (absolute,Y: $xxxx,Y)
; =============================================================================
ADDR_FMT_TABLE:
            .byte   $00             ;  0  implied / accumulator
            .byte   $21             ;  1  immediate
            .byte   $01             ;  2  zero-page
            .byte   $02             ;  3  absolute
            .byte   $00             ;  4  accumulator (explicit A)
            .byte   $80             ;  5  accumulator (print "A")
            .byte   $59             ;  6  (zp),Y
            .byte   $4D             ;  7  (zp,X)
            .byte   $11             ;  8  zp,X
            .byte   $12             ;  9  zp,Y
            .byte   $06             ; 10  abs,X
            .byte   $4A             ; 11  abs indirect
            .byte   $05             ; 12  abs,Y

; =============================================================================
; SUFFIX_CHARS / SUFFIX_CHARS2
;
; Two 7-byte arrays providing the one or two characters to print for each
; addressing-mode suffix position (indexed by X = 6 down to 1 in the
; suffix output loop).
;
; X   SUFFIX_CHARS[X]  SUFFIX_CHARS2[X]  Output
; 6   $1D (sentinel)   $41 'A'           A           (accumulator)
; 5   $2C ','          $00               (none)       -> just ','
; ...  wait, see original data. Let's use the original byte values as-is.
;
; Corrected from the actual data:
; X   SUFFIX_CHARS   SUFFIX_CHARS2   Displayed
; 6     $1D (unused)   $41 'A'         A
; 5     $2C ','        $59 'Y'         ,Y
; 4     $29 ')'        $00             )
; 3     $2C ','        $58 'X'         ,X
; 2     $23 '#'        $00             #
; 1     $28 '('        $00             (
; =============================================================================
SUFFIX_CHARS:
            .byte   $1D             ; X=6: (unused primary; 'A' comes from SUFFIX_CHARS2)
            .byte   $2C             ; X=5: ','
            .byte   $29             ; X=4: ')'
            .byte   $2C             ; X=3: ','
            .byte   $23             ; X=2: '#'
            .byte   $28             ; X=1: '('

SUFFIX_CHARS2:
            .byte   $41             ; X=6: 'A'
            .byte   $59             ; X=5: 'Y'
            .byte   $00             ; X=4: (none)
            .byte   $58             ; X=3: 'X'
            .byte   $00             ; X=2: (none)
            .byte   $00             ; X=1: (none)
            .byte   $00             ; (pad)

; =============================================================================
; MNEMONIC ENCODING TABLES
;
; Two parallel 64-byte arrays, indexed by the mnemonic index (0..63).
; Together they form a 16-bit value per mnemonic (MNEM_HI[i] << 8 | MNEM_LO[i]).
;
; Each mnemonic character is stored as a 5-bit biased value:
;   char_value = ASCII_char - $3F
;   E.g. 'B' = $42 -> $42 - $3F = 3
;
; The three characters of a mnemonic are packed into 15 bits (bits 15..1);
; bit 0 is unused.  The 16-bit value is split across MNEM_HI (bits 15..8)
; and MNEM_LO (bits 7..0), and the three 5-bit groups are extracted by
; shifting left 5 bits at a time in DISASM_ONE_INSTR.
;
; Example — BRK ($1CD8):
;   Bits 15..11 = 00011 ($03) -> $03 + $3F = $42 = 'B'
;   Bits 10.. 6 = 10011 ($13) -> $13 + $3F = $52 = 'R'
;   Bits  5.. 1 = 01100 ($0C) -> $0C + $3F = $4B = 'K'
;
; Index  MNEM_HI  MNEM_LO  Mnemonic
;    0    $1C      $D8      BRK
;    1    $8A      $62      PHP
;    2    $1C      $5A      BPL
;    3    $23      $48      CLC
;    4    $5D      $26      JSR
;    5    $8B      $62      PLP
;    6    $1B      $94      BMI
;    7    $A1      $88      SEC
;    8    $9D      $54      RTI
;    9    $8A      $44      PHA
;   10    $1D      $C8      BVC
;   11    $23      $54      CLI
;   12    $9D      $68      RTS
;   13    $8B      $44      PLA
;   14    $1D      $E8      BVS
;   15    $A1      $94      SEI
;   16    $00      $00      ???
;   17    $29      $B4      DEY
;   18    $19      $08      BCC
;   19    $AE      $84      TYA
;   20    $69      $74      LDY
;   21    $A8      $B4      TAY
;   22    $19      $28      BCS
;   23    $23      $6E      CLV
;   24    $24      $74      CPY
;   25    $53      $F4      INY
;   26    $1B      $CC      BNE
;   27    $23      $4A      CLD
;   28    $24      $72      CPX
;   29    $53      $F2      INX
;   30    $19      $A4      BEQ
;   31    $A1      $8A      SED
;   32    $00      $00      ???
;   33    $1A      $AA      BIT
;   34    $5B      $A2      JMP
;   35    $5B      $A2      JMP (indirect)
;   36    $A5      $74      STY
;   37    $69      $74      LDY
;   38    $24      $74      CPY
;   39    $24      $72      CPX
;   40    $AE      $44      TXA
;   41    $AE      $68      TXS
;   42    $A8      $B2      TAX
;   43    $AD      $32      TSX
;   44    $29      $B2      DEX
;   45    $00      $00      ???
;   46    $7C      $22      NOP
;   47    $00      $00      ???
;   48    $15      $1A      ASL
;   49    $9C      $1A      ROL
;   50    $6D      $26      LSR
;   51    $9C      $26      ROR
;   52    $A5      $72      STX
;   53    $69      $72      LDX
;   54    $29      $88      DEC
;   55    $53      $C8      INC
;   56    $84      $C4      ORA
;   57    $13      $CA      AND
;   58    $34      $26      EOR
;   59    $11      $48      ADC
;   60    $A5      $44      STA
;   61    $69      $44      LDA
;   62    $23      $A2      CMP
;   63    $A0      $C8      SBC
; =============================================================================
MNEM_HI:
            .byte   $1C, $8A, $1C, $23, $5D, $8B, $1B, $A1
            .byte   $9D, $8A, $1D, $23, $9D, $8B, $1D, $A1
            .byte   $00, $29, $19, $AE, $69, $A8, $19, $23
            .byte   $24, $53, $1B, $23, $24, $53, $19, $A1
            .byte   $00, $1A, $5B, $5B, $A5, $69, $24, $24
            .byte   $AE, $AE, $A8, $AD, $29, $00, $7C, $00
            .byte   $15, $9C, $6D, $9C, $A5, $69, $29, $53
            .byte   $84, $13, $34, $11, $A5, $69, $23, $A0

MNEM_LO:
            .byte   $D8, $62, $5A, $48, $26, $62, $94, $88
            .byte   $54, $44, $C8, $54, $68, $44, $E8, $94
            .byte   $00, $B4, $08, $84, $74, $B4, $28, $6E
            .byte   $74, $F4, $CC, $4A, $72, $F2, $A4, $8A
            .byte   $00, $AA, $A2, $A2, $74, $74, $74, $72
            .byte   $44, $68, $B2, $32, $B2, $00, $22, $00
            .byte   $1A, $1A, $26, $26, $72, $72, $88, $C8
            .byte   $C4, $CA, $26, $48, $44, $44, $A2, $C8

; =============================================================================
; ERROR HANDLERS
; =============================================================================

; "START ADDRESS ERROR"
ERR_START_ADDR:
            svc     $02
            .byte   $02
            .byte   "START ADDRESS ERROR", $0d, $00
            jmp     PROMPT_AND_INPUT

; "END ADDRESS ERROR"
ERR_END_ADDR:
            svc     $02
            .byte   $02
            .byte   "END ADDRESS ERROR", $0d, $00
            jmp     PROMPT_AND_INPUT

; "ERROR, TOO MANY PARAMETERS"
ERR_PARAMS:
ERR_TOO_MANY:
            svc     $02
            .byte   $02
            .byte   "ERROR, TOO MANY PARAMETERS", $0d, $00
            jmp     PROMPT_AND_INPUT

; "# OF LINES TOO LARGE"
ERR_LINES_LARGE:
            svc     $02
            .byte   $02
            .byte   "# OF LINES TOO LARGE", $0d, $00
            jmp     PROMPT_AND_INPUT

; "ILLEGAL CHANNEL NUMBER"
ERR_ILLEGAL_CHAN:
            svc     $02
            .byte   $02
            .byte   "ILLEGAL CHANNEL NUMBER", $0d, $00
            jmp     PROMPT_AND_INPUT

; =============================================================================
; SCRATCH / WORK BYTES
; =============================================================================
SAVEY:      .byte   $00             ; Saved Y scratch (EMIT_HEX_BYTE)
SAVEX:      .byte   $00             ; Saved X scratch / lo-byte temp
MNEMONIC:   .word   $0000           ; Mnemonic shift register

PROG_SIZE = * - START

            .end