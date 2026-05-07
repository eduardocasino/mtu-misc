; =============================================================================
; MACASM.ASM  –  MTU 6502 MACRO ASSEMBLER  Version 1.1
; =============================================================================
; Copyright 1981-1982 Micro Technology Unlimited (MTU)
; Author: Bruce D. Carbrey
; Date:   21-Oct-1982
;
; Target CPU  : MOS Technology 6502
; Host OS     : CODOS (MTU Disk Operating System)
; Assembler   : ca65
;
; Two-pass assembler
; ------------------
;   Pass 1 (PASS1_MAIN_LOOP):  Scans every source line, builds the symbol
;     table with preliminary values, does NOT emit object code.  Advances
;     the location counter for each instruction/directive to establish
;     addresses.
;   Pass 2 (PASS2_MAIN_LOOP):  Re-reads every source line, resolves all
;     operands, emits object code via EMIT_BYTE_PASS2/OBJBUF_WRITE_BYTE,
;     and writes the assembly listing.
;
; Known bugs / issues
; -------------------
;   BUG-1  INIT_LINE_WS:  "sta LISTING_BUF" should be "sta LISTING_BUF,X".
;          As written, every iteration of the clearing loop overwrites the
;          same byte (LISTING_BUF[0]) instead of sweeping the whole buffer.
;          This bug is already annotated with "FIXME" in the original source.
;
;   BUG-2  EMIT_BYTE_PASS2:  When PASSFLG = $00 (pass 1) the code
;          executes "brk / rts", which CODOS interprets as SVC $60 (opcode
;          of RTS = $60).  SVC $60 is undocumented; the code appears to
;          rely on the OS returning normally to act as a pass-1 no-op.
;
;   BUG-3  @UNREACH:  Unreachable dead code ("bit URESFLG / bmi @ERROR3") placed
;          immediately after a "jsr ERROR_RANGE" (error handler that never returns).
;          The original comment suggests the branch at @ALT_EMIT ("bcs @CHK_ABS")
;          should instead branch here.
;
; =============================================================================

            .setcpu "6502"

            .include "symbols.inc"
.ifdef mtu
            .include "monomeg.inc"
.endif

.macro svc number
            brk
            .byte   number
.endmacro

; Unofficial HLT (JAM, KIL) instruction
; Freezes the CPU, reset required
;
.macro hlt
            .byte   $02
.endmacro

; =============================================================================
; ZERO PAGE SCRATCH VARIABLES  ($40–$73)
; Defined in the .zeropage segment; actual runtime addresses are $40–$73.
; =============================================================================

            .zeropage

SYMTBLP:    .res    2               ; Symbol table entry pointer ($40)
SYMVALP:    .res    2               ; Symbol value-store pointer – next free byte in heap ($42)
HASHTBLP:   .res    2               ; Hash-table bucket probe pointer ($44)
MATCHP:     .res    2               ; Matched symbol entry pointer (result of hash lookup) ($46)
SEGHEADP:   .res    2               ; Object segment header write pointer ($48)
MACTBLP:    .res    2               ; Macro table base pointer ($4A)
OBJBUFP:    .res    2               ; Object-code output buffer pointer ($4C)
SYMNAMP:    .res    2               ; Symbol name pointer – sort comparator A ($4E)
SYMNAMP2:   .res    2               ; Symbol name pointer – sort comparator B ($50)
TEMPP:      .res    2               ; Generic temporary pointer ($52)
LSTOUTP:    .res    2               ; Listing output write pointer ($54)
SYMVAL:     .res    3               ; 24-bit assembled/evaluated value (lo, mid, hi) ($56)
TMPVAL:     .res    3               ; 24-bit scratch A (multiply/divide) ($59)
TMPVAL2:    .res    3               ; 24-bit scratch B (multiply/divide remainder) ($5C)
URESFLG:    .res    1               ; Unresolved-reference flag: $C0 = unresolved ($5F)
PASSFLG:    .res    1               ; Pass flag: $00 = pass 1, $80 = pass 2 ($60)
LOCCNT:     .res    2               ; Location counter (current assembly address) ($61)
LOCCNT2:    .res    2               ; Location counter snapshot at start of current line ($63)
SAVEX:      .res    1               ; Saved X register scratch ($65)
MACBODYP:   .res    2               ; Macro body start address ($66)
OPCTBLP:    .res    2               ; Opcode table sequential read pointer ($68)
MARGSTRP:   .res    2               ; Macro parameter string read pointer ($6A)
MARGWRTP:   .res    2               ; Macro argument write pointer (into MACSTACK) ($6C)
RETADDRP:   .res    2               ; PRTSTR: inline string pointer (return address scratch) ($6E)
HEAP2PRP:   .res    2               ; Secondary-heap memory probe pointer ($70)
HEAP2WRP:   .res    2               ; Secondary-heap (expansion RAM) write pointer ($72)

            .code

; =============================================================================
; CODOS LOADABLE FILE HEADER
; First 8 bytes of the code segment are the CODOS load descriptor.
; =============================================================================
            .byte   $58             ; CODOS loadable file signature byte
            .byte   $00             ; Memory overlay flag
            .byte   $00             ; Memory bank
            .byte   $00             ; Reserved
            .addr   START           ; Entry point address
            .addr   START           ; Load address
            .word   PROG_SIZE       ; Total memory image size in bytes

; =============================================================================
; START  –  Jump table
; =============================================================================
START:      jmp     MAIN_ENTRY      ; Normal entry: initialise and assemble
            jmp     ABORT_ASSEMBLY  ; Error exit: flush + return to CODOS

; =============================================================================
; CONFIGURATION / RELOCATABLE POINTER TABLE
;
; These words are patched by the command-line parser and memory probe to
; define the layout of the five heap/buffer regions.  All pointers must be
; page-aligned.
;
;   SRCST   / SRCEND   Source program read buffer
;   LSTST   / LSTEND   Listing output accumulation buffer
;   HEAPST  / HEAPEND  Symbol-table primary heap (names and values)
;   HASHTST / HASHTEND Open-address hash table (2-byte bucket entries)
;   MACSST  / MACSEND  Macro invocation argument stack
;   HEAP2ST / HEAP2END Secondary heap in expansion-RAM bank 3
; =============================================================================
SRCST:      .addr   SRCPRBUF        ; Start of source buffer  (relocatable) ($0706)
SRCEND:     .addr   LSTNGBUF        ; End+1 of source buffer  (page-aligned) ($0708)
LSTST:      .addr   LSTNGBUF        ; Start of listing buffer (relocatable) ($070A)
LSTEND:     .addr   TABLE           ; End+1 of listing buffer (page-aligned) ($070C)
HEAPST:     .addr   TABLE           ; Start of symbol-table primary heap ($070E)
HEAPEND:    .addr   SYMHASHT        ; End+1 of primary heap   (page-aligned) ($0710)
HASHTST:    .addr   SYMHASHT        ; Start of hash table     (relocatable) ($0712)
HASHTEND:   .addr   MACSTACK        ; End+1 of hash table ($0714)
HEAP2ST:    .addr   SCNDHEAP+4      ; Start of secondary heap (bank 3) ($0716)
HEAP2END:   .addr   SCNDHEAP+$ff00  ; End+1 of secondary heap (bank 3) ($0718)
MACSST:     .addr   MACSTACK        ; Start of macro argument stack (relocatable) ($071A)
MACSPTR:    .addr   MACSTACK        ; Current write position in macro arg stack ($071C)
MACSEND:    .addr   MACSTACK+$200   ; End+1 of macro argument stack ($071E)

; =============================================================================
; HASH TABLE SIZE CODE  ($0720)
; Encodes the AND mask applied to the hash sum to select the bucket index.
;   $0F -> 4 K-bucket table (8 KB)
;   $07 -> 2 K-bucket table (4 KB)
;   $03 -> 1 K-bucket table (2 KB)
; HASHSIZEC is the active size code; HASHSIZEC+1 to HASHSIZEC+3 are alternative
; codes selectable via the command-line (currently unused at runtime).
; =============================================================================
HASHSIZEC:  .byte   $0f             ; Active hash mask: $0F -> 4 K buckets ($0720)
            .byte   $09
            .byte   $11
            .byte   $19

; Substitution value for an undefined / unresolved operand (24-bit, all bits set)
UNDEFSUBST: .word   $ffff           ; Low and middle bytes of substitution value ($0724)
            .byte   $00             ; High byte

; =============================================================================
; LISTING / CROSS-REFERENCE FORMATTING PARAMETERS  ($0727–$072A)
; =============================================================================
LISTLINESPP:.byte   $39             ; Lines per page in listing output (57) ($0727)
XRNAMCOLMAX:.byte   $3d             ; Maximum column for symbol name in xref listing (61) ($0728)
LISTTABSPC: .byte   $14             ; Tab width between columns in symbol table listing (20) ($0729)
XRNUMCOLMAX:.byte   $4b             ; Maximum column for a cross-reference number (75) ($072A)

; =============================================================================
; CONFIGURABLE OPERATOR / SYNTAX CHARACTERS  ($072B–$073E)
; All of these can be changed to suit national keyboards or user preference.
; =============================================================================
ARITHOPS:   .byte   '~'             ; Exclusive-OR operator ($072B)
            .byte   '|'             ; OR operator ($072C)
            .byte   '&'             ; AND operator ($072D)
            .byte   '\'             ; Remainder (modulo) operator ($072E)
            .byte   '/'             ; Division operator ($072F)
            .byte   '*'             ; Multiplication operator ($0730)
SUBSOP:     .byte   '-'             ; Subtraction operator ($0731)
ADDOP:      .byte   '+'             ; Addition operator ($0732)

LBRACKET:   .byte   '['             ; Left grouping bracket ($0733)
RBRACKET:   .byte   ']'             ; Right grouping bracket ($0734)
LOBYTE:     .byte   '<'             ; Low-byte extraction operator ($0735)
HIBYTE:     .byte   '>'             ; High-byte extraction operator ($0736)
HEXPREFIX:  .byte   '$'             ; Hexadecimal literal prefix ($0737)
OCTPREFIX:  .byte   '@'             ; Octal literal prefix ($0738)
BINPREFIX:  .byte   '%'             ; Binary literal prefix ($0739)
APOSTROPHE: .byte   $27             ; Character literal quote (apostrophe) ($073A)
TABCHAR:    .byte   $09             ; Tab character (used in line-continuation handling) ($073B)
ARGLEADIN:  .byte   '!'             ; Macro argument lead-in character ($073C)
ARGLEFT:    .byte   '{'             ; Macro argument string left delimiter ($073D)
ARGRIGHT:   .byte   '}'             ; Macro argument string right delimiter ($073E)
ARGCOUNT:   .byte   '#'             ; "!#" in a macro body -> replaced by actual arg count ($073F)

; Characters permitted inside identifiers in addition to A-Z and 0-9
DOT:        .byte   '.'             ; Embedded identifier chars: '.', '?', '_' ($0740)
QMARK:      .byte   '?'             ;
UNDERSCORE: .byte   '_'             ;

PCSYMBOL:   .byte   '*'             ; Program-counter symbol (current address) ($0743)
COMMENT:    .byte   ';'             ; Comment delimiter character ($0744)

; =============================================================================
; ASSEMBLER FLAGS AND VARIABLES
; =============================================================================

FIRST_FF_FLAG:      .byte   $00   ; ($0745) $00 = emit a form-feed before the first page
                                  ;         $80 = suppress it (save paper if printer already at top-of-form)
HASH_PROBE_WRAP:    .byte   $00   ; ($0746) Hash collision wrap counter
HEX_DIGIT_FLAG:     .byte   $00   ; ($0747) Non-zero while parsing hex literal
ERROR_LISTING_RUN:  .byte   $00   ; ($0748) $80 = pass 1 is being re-run to produce the
                                  ;         error listing to file (set only on errors)
XREF_MODE:          .byte   $80   ; ($0749) $80 = cross-reference mode (emit xref records)
LISTING_ACTIVE:     .byte   $00   ; ($074A) $80 = listing is being produced on this line
LISTING_FILE_OPEN:  .byte   $80   ; ($074B) $80 = listing output file/channel is open
LINE_LIST_FLAGS:    .byte   $80   ; ($074C) Per-line listing control bits
LISTING_ON:         .byte   $00   ; ($074D) Current effective listing-on/off state
SYMTBL_INIT:        .byte   $00   ; ($074E) $80 = symbol table initialisation in progress
                                  ;         (suppresses normal hash-table name writes)
LINE_CONTINUE:      .byte   $00   ; ($074F) $80 = currently reading a continued (tab-split) line
XREF_ENABLED:       .byte   $80   ; ($0750) $80 = print cross-reference table after assembly
END_SEEN:           .byte   $00   ; ($0751) $80 = .END directive has been processed
INCLUDE_POP:        .byte   $00   ; ($0752) $80 = return from .READ / include stack requested
OBJSEG_OPEN:        .byte   $00   ; ($0753) $80 = an object segment header is open/pending
COND_DEPTH:         .byte   $00   ; ($0754) Bit mask tracking .IF nesting depth
COND_ELSE:          .byte   $00   ; ($0755) Bit mask tracking .ELSE state per nesting level
IFCMP_ZERO:         .byte   $00   ; ($0756) $FF = both .IFxx operands were equal
IFCMP_NEG:          .byte   $00   ; ($0757) $FF = first .IFxx operand was less than the second
ENTRY_DEFINED:      .byte   $00   ; ($0758) $80 = .ENTRY directive has been seen
DIRECT_DEFS_ACT:    .byte   $00   ; ($0759) $80 = "==" command-line redirect active
LIST_SUPPRESS:      .byte   $00   ; ($075A) $80 = suppress line-number increment (.LIST OFF)
ENDIANNESS:         .byte   $00   ; ($075B) $80 = Emit bytes BE for DBYTE
RELADDR_FLAG:       .byte   $00   ; ($075C) Relative-address mode flag for branch instructions
EXPANSION_BANK:     .byte   $FD   ; ($075D) $80 = expansion RAM (bank 3) is present and usable
VALUE_OUTPUT:       .byte   $00   ; ($075E) $80 = value digits have already been written to listing
VALUE_SUPPRESS:     .byte   $00   ; ($075F) $80 = suppress value digit output in listing
IN_STRING:          .byte   $00   ; ($0760) In-string flag for comment-character scanning
MACRO_EXPANDING:    .byte   $00   ; ($0761) $80 = currently inside a macro expansion
LIST_SUPPRESS2:     .byte   $00   ; ($0762) Secondary listing-suppress flag
ENTRY_TYPE_FLAGS:   .byte   $00   ; ($0763) Type/flag byte of the current symbol-table entry
INSTR_FLAGS:        .byte   $00   ; ($0764) Instruction flags for current opcode
MACRO_FLAGS:        .byte   $00   ; ($0765) Macro-expansion state flags
LINE_NUM:           .word   $0000 ; ($0766) Current source line number (16-bit)
PAGE_LINE_CNT:      .byte   $00   ; ($0768) Line counter on the current listing page
                    .byte   $00   ; ($0769) Unused
ADDR_MODE:          .byte   $00   ; ($076A) Resolved addressing mode index (0–$0A)
FATAL_ERR_CODE:     .byte   $00   ; ($076B) Fatal error code (1–6) encoded by ERR_CNT_LADDER
ERR_TYPE_CODE:      .byte   $00   ; ($076C) Error type code (0–11) encoded by ERR_TYPE_LADDER
                    .byte   $00   ; ($076D) Unused
ERR_CNT:            .word   $0000 ; ($076E) Line-error count (16-bit)
                    .byte   $00   ; ($0770) Unused
                    .byte   $00   ; ($0771) Unused
SAVED_SP:           .byte   $00   ; ($0772) Stack pointer saved for error longjmp
SAVED_RET:          .word   $0000 ; ($0773) Saved return address (16-bit)
SYMBOL_COUNT:       .word   $0000 ; ($0775) Number of symbols defined (16-bit)
REF_COUNT:          .word   $0000 ; ($0777) Total cross-reference entries (16-bit)
SORTED_END:         .word   $0000 ; ($0779) Sorted hash-table end ptr (16-bit)
SORT_GAP:           .word   $0000 ; ($077B) Shell-sort gap (16-bit)
SORT_LIMIT:         .word   $0000 ; ($077D) Shell-sort upper bound (16-bit)
XREF_COL1:          .byte   $00   ; ($077F) Xref listing column 1 (next tab stop)
XREF_COL2:          .byte   $00   ; ($0780) Xref listing symbol-value column
DEC_DIGIT:          .byte   $00   ; ($0781) ASCII digit scratch for FORMAT_DEC16
SIGN_FLAG:          .byte   $00   ; ($0782) $80 = product/quotient is negative
VALUE_STACK_TOP:    .byte   $00   ; ($0783) Expression evaluator value stack top
OPER_STACK_TOP:     .byte   $00   ; ($0784) Expression evaluator operator stack top
IFCMP_VAL:          .byte   $00   ; ($0785) IFCMP_VAL[0]: .IFxx first operand, byte 0
                    .byte   $00   ;         IFCMP_VAL[1]: .IFxx first operand, byte 1
                    .byte   $00   ;         IFCMP_VAL[2]: .IFxx first operand, byte 2
OVL_NUM:            .byte   $00   ; ($0788) Overlay number from .OVL
BANK_NUM:           .byte   $00   ; ($0789) Bank number from .BANK
SYMVALP_SAVE:       .word   $0000 ; ($078A) Saved SYMVALP (16-bit)
ENTRY_ADDR:         .word   $0000 ; ($078C) .ENTRY address  (16-bit)
LOCCNT_BASE:        .word   $0000 ; ($078E) Last segment checkpoint (16-bit) 
SEG_SIZE:           .word   $0000 ; ($0790) Current segment size (16-bit)
DDEFS_END:          .word   $0000 ; ($0792) End ptr of direct-defs buffer
HASH_LAST:          .word   MACSTACK-2 ; ($0794) last hash bucket ptr
OP_HANDLER:         .word   $0000 ; ($0796) Current operator-handler vector
MACRO_BODY_VEC:     .word   $0000 ; ($0798) Macro body expansion vector
ERRFNP:             .word   $0000 ; ($079A) Error-recovery dispatch vector (pass-dependent)
                    .byte   $03   ; ($079C) Unused
BANK_CTL_BITS:      .byte   $00   ; ($079D) 2-bit bank selector for BNKCTL
FILL_COUNT_NEG:     .word   $0000 ; ($079E) Negated .FILL count (16-bit)
MACRO_ARG_CNT:      .byte   $00   ; ($07A0) Actual argument count in macro call
STR_DELIM:          .byte   $00   ; ($07A1) Current string delimiter char
PARAM_IDX_SAVE:     .byte   $00   ; ($07A2) Saved parameter index
OPCODE_TBL_PTR:     .word   $0000 ; ($07A3) Pointer to current opcode table entry (16-bit)
NEXT_LOCAL:         .byte   "000 " ; ($07A5) NEXT_LOCAL[4]: "next" local label ASCII counter
PREV_LOCAL:         .byte   "000 " ; ($07A9) PREV_LOCAL[4]: "previous" local label ASCII counter
.ifdef mtu
HW_CHECK_LEN:       .byte   $0b   ; ($07AD) Hardware check data length
                    .byte   $c9   ; ($07AE) Unused
HW_CHECK_DATA:      .byte   $00   ; ($07AE) HW_CHECK_DATA[0]: expected check nibbles, byte 0
                    .byte   $00   ;         HW_CHECK_DATA[1]: expected check nibbles, byte 1
                    .byte   $01   ;         HW_CHECK_DATA[2]: expected check nibbles, byte 2
                    .byte   $07   ;         HW_CHECK_DATA[3]: expected check nibbles, byte 3
                    .byte   $05   ;         HW_CHECK_DATA[4]: expected check nibbles, byte 4
.endif
CMD_Y_SAVE:         .byte   $00   ; ($07B4) Saved Y during command-line parsing
SYM_NAME_OFF:       .byte   $00   ; ($07B5) Byte offset to end of symbol name
LINE_POS:           .byte   $00   ; ($07B6) Current Y index into LINE_BUF
LINE_POS_SAVE:      .byte   $00   ; ($07B7)
LINE_POS_SAVE2:     .byte   $00   ; ($07B8)
UNRES_LINE_POS:     .byte   $00   ; ($07B9) Line pos when unresolved ref seen
UNRES_POS:          .byte   $00   ; ($07BA) Set with position in buffer if current data value is unresolved
SRC_BUF_OFF:        .byte   $00   ; ($07BB) Offset within loaded source buffer
                    .byte   $00   ; ($07BC) Unused
IDENT_LEN:          .byte   $00   ; ($07BD) Length of current identifier in IDENT_BUF
SYM_MATCH_OFF:      .byte   $00   ; ($07BE) Offset of match in SYMTBLP
OBJBUF_POS:         .byte   $00   ; ($07BF) Write index into OBJCOBUF
FILL_LINE_POS:      .byte   $00   ; ($07C0) Line pos saved at start of .FILL
OUT_CHANNEL:        .byte   $00   ; ($07C1) Output channel for PRINT_FINAL_SUM
                    .byte   $00   ; ($07C2) Unused
SRC_REMAINING:      .word   $0000 ; ($07C3) Source bytes remaining (16-bit)
DIV_TEMP:           .word   $0000 ; ($07C5) Division scratch (16-bit)
                    .byte   $00   ; ($07C7) Unused
                    .byte   $00   ; ($07C8) Unused
                    .byte   $00   ; ($07C9) Unused
                    .byte   $00   ; ($07CA) Unused
OPT_START_Y:        .byte   $00   ; ($07CB) Option token start position in line
EVAL_X_SAVE:        .byte   $00   ; ($07CC) Saved X in EVAL_EXPRESSION
EVAL_Y_SAVE:        .byte   $00   ; ($07CD) Saved Y in formatting routines
MACDEF_Y_SAVE:      .byte   $00   ; ($07CE) Saved Y in DEFINE_MACRO
SYMTBL_CHKSUM:      .word   $0000 ; ($07CF) Symbol-table checksum (16-bit)
READNB_X_SAVE:      .byte   $00   ; ($07D1) Saved X scratch for READ_NEXT_BLOCK
READBI_X_SAVE:      .byte   $00   ; ($07D2) Saved X scratch for READ_BLOCK_INNER
OUTPUTEL_Y_SAVE:    .byte   $00   ; ($07D3) Saved Y scratch for OUTPUT_ERROR_LINE
FDEC16_Y_SAVE:      .byte   $00   ; ($07D4) Saved Y scratch for FORMAT_DEC16
EBPASS2_X_SAVE:     .byte   $00   ; ($07D5) Saved X scratch for EMIT_BYTE_PASS2
LIST_LINE_LEN:      .byte   $00   ; ($07D6) Listing line byte count
CMD_END_POS:        .byte   $00   ; ($07D7) End of filename token in command buffer
CMD_CUR_POS:        .byte   $00   ; ($07D8) Current position in command buffer
LIST_DRIVE_POS:     .byte   $00   ; ($07D9) Drive number position for listing
TAB_COL:            .byte   $00   ; ($07DA) Column being expanded to tab stop
READMB_X_SAVE:      .byte   $00   ; ($07DB) Saved X scratch for READ_MACRO_BODY_BYTE
RWARG_X_SAVE:       .byte   $00   ; ($07DC) Saved X scratch for WRITE_ARG_BYTE / READ_ARG_BYTE
RWARG_Y_SAVE:       .byte   $00   ; ($07DD) Saved Y scratch for WRITE_ARG_BYTE / READ_ARG_BYTE
PRTSTR_Y_SAVE:      .byte   $00   ; ($07DF) Saved Y for PRTSTR
OUTPGB_Y_SAVE:      .byte   $00   ; ($07DE) Saved Y for OUTPUT_LINE_PG_BRK routine
OBJ_BUF_LIMIT:      .byte   $0a   ; ($07E0) Object buffer size in pages
CHKSUM_PASS1:       .word   $0067 ; ($07E1) Pass-1 checksum reference (16-bit)
                    .byte   $d9   ; ($07E3) Unused
SRC_CHANNEL:        .byte   $05   ; ($07E4) CODOS channel number for source file
LST_CHANNEL:        .byte   $06   ; ($07E5) CODOS channel for listing file
OBJ_CHANNEL:        .byte   $07   ; ($07E6) CODOS channel for object file
CON_CHANNEL:        .byte   $04   ; ($07E7) CODOS channel for console/error
                    .byte   $05   ; ($07E8) Unused
DEF_CHANNEL:        .byte   $08   ; ($07E9) CODOS channel for definitions file
                    .byte   $00   ; ($07EA) Unused
EXT_OPT_CHARS:      .byte   "LOEXD" ; ($07EB) valid single-letter option chars

; Default drive/device assignments for each file (all 0 = default drive)
SRC_DRIVE:          .byte   $00   ; ($07F0) Drive for source file
LST_DRIVE:          .byte   $00   ; ($07F1) Drive for listing file
OBJ_DRIVE:          .byte   $00   ; ($07F2) Drive for object file
ERR_DEVICE:         .byte   'C'   ; ($07F3) Error device: 'C' = console, 'N' = none
OBJ_DEVICE:         .byte   'N'   ; ($07F4) Object device (initial='N') 
DEF_DEVICE:         .byte   'N'   ; ($07F5) Definitions device: 'N' = no definitions file

; Per-channel table index values

SRC_FNAME_OFFSET=SRC_FNAME_SLOT-FILENAME_SLOTS
LST_FNAME_OFFSET=LST_FNAME_SLOT-FILENAME_SLOTS
OBJ_FNAME_OFFSET=OBJ_FNAME_SLOT-FILENAME_SLOTS
ERR_FNAME_OFFSET=ERR_FNAME_SLOT-FILENAME_SLOTS
EXT_FNAME_OFFSET=EXT_FNAME_SLOT-FILENAME_SLOTS
DEF_FNAME_OFFSET=DEF_FNAME_SLOT-FILENAME_SLOTS

CHAN_ASSIGN_TBL:    .byte   SRC_FNAME_OFFSET
                    .byte   LST_FNAME_OFFSET
                    .byte   OBJ_FNAME_OFFSET    
                    .byte   ERR_FNAME_OFFSET
                    .byte   EXT_FNAME_OFFSET
                    .byte   DEF_FNAME_OFFSET

; Default file extensions: source=A, listing=L, object=C, errors=E, ext=A, defs=A
DEFAULT_EXTS:       .byte   'A'   ; Default extension for source file ($07FC)
                    .byte   'L'   ; Default extension for listing file ($07FD)
                    .byte   'C'   ; Default extension for object file ($07FE)
                    .byte   'E'   ; Default extension for error file ($07FF)
                    .byte   'A'   ; Default extension for externals file ($0800)
                    .byte   'A'   ; Default extension for definitions file ($0801)

; =============================================================================
; FILE NAME RECORD BUFFERS  ($0802–$086B)
;
; Each record is 14 bytes (13 chars + NUL) in the CODOS filename format.
; The assembler keeps one record per channel.  COPY_FILENAME_TO_SLOT
; fills these from the parsed command line.
;
; Layout of each 14-byte slot:
;   bytes 0–12  filename characters (A-Z, 0-9, '.', drive digit)
;   byte  13    NUL ($00) terminator
; =============================================================================
FILENAME_SLOTS:
SRC_FNAME_SLOT:     .byte   "            . " ; Channel 0 filename record ($0802, 14 bytes)
LST_FNAME_SLOT:     .byte   "            . " ; Channel 1 filename record ($0810)
OBJ_FNAME_SLOT:     .byte   "            . " ; Channel 2 filename record ($081E)
ERR_FNAME_SLOT:     .byte   "            . " ; Channel 3 filename record ($082C)
EXT_FNAME_SLOT:     .byte   "            . " ; Channel 4 (object file) filename record ($083A)
DEF_FNAME_SLOT:     .byte   "            . " ; Channel 5 filename record ($0848)
FNAME_BUFFER:       .byte   "            . " ; Scratch 14-byte parse buffer for PARSE_FILENAME ($0856)

; Miscellaneous layout bytes following the filename records
EXTENSION_POS:      .byte   $00     ; ($0864) Filename parse: index to extension position
CURRENT_OPT:        .byte   $20     ; ($0865) Scratch: current option letter being processed
OPT_INDEX:          .byte   $00     ; ($0866) Option index (0–5) into EXT_OPT_CHARS
DEFAULT_EXT_CHAR:   .byte   $20     ; ($0867) Current default extension character
PARSED_DRIVE:       .byte   $00     ; ($0868) Current drive number from filename parse
LINE_BUF_LEN:       .byte   $00     ; ($0869) Length of source line (byte count in LINE_BUF)
LINE_BUF_END:       .word   $0000   ; ($086A) Source buffer end offset, low byte

; =============================================================================
; INCLUDE FILE STACK
;
; 37-entry push-down stack for nested .READ (include) files.
; Each frame is 18 bytes:
;   byte  0       drive / device code
;   bytes 1–3     CODOS file position (3 bytes)
;   bytes 4–17    14-byte filename record
;
; INCL_STACK_TOP holds the byte index of the top-of-stack (incremented by 18 per push).
; Maximum nesting depth = $25 / 18 = 2 levels — error if exceeded.
; =============================================================================
INCL_FILE_STACK:    .byte   $ff              ; ($086C) Include stack entry 0, drive byte
                    .byte   $00
                    .byte   $00
                    .byte   $00
                    .byte   "            . " ; filename
                    .byte   $ff              ; Include stack entry 1
                    .byte   $00
                    .byte   $00
                    .byte   $00
                    .byte   "            . "
                    .byte   $ff              ; Include stack entry 2
                    .byte   $00
                    .byte   $00
                    .byte   $00
                    .byte   "            . "
INCL_STACK_TOP:     .byte   $00              ; ($08A2) Index of top-of-include stack

; =============================================================================
; LISTING LINE OUTPUT BUFFER
;
; This is the fixed-format listing line template.  Fields are written in-place
; by the assembly engine.
; =============================================================================
LIST_BUF:           .byte   $0d             ; Listing line prefix (CR)
ADDRESS_FIELD:      .byte   "0000 "         ; Address field: 4 hex digits + space
VALUE_FIELD:        .byte   "0000 "         ; Value field:   4 hex digits + space

SEPARATOR_SP:       .byte   ' '             ; Separator space
EQUATE_CHAR:        .byte   ' '             ; Equate char 1 (.e.g. '=' for EQU)
SECOND_EQUATE:      .byte   ' '             ; Second equate char for listings
                    .byte   ' '             ; Separator
SEPARATOR_DOT_1:    .byte   ' '             ; Separator dot 0 (set to '.' by certain directives)
SEPARATOR_DOT_2:    .byte   ' '             ; Separator dot 1

LINE_BUF0:          .byte   ' '             ; Filled with CR at runtime
LINE_BUF:           .byte   "                                        "
                    .byte   "                                        ", 0
                    ; Line buffer: 80 spaces + NUL

; =============================================================================
; ERROR LINE BUFFER
; Used by REPORT_LINE_ERROR to output the error indicator line.
; =============================================================================
ERR_PREFIX:         .byte   $0d, "*ERROR "  ; Error line prefix
ERR_COUNT:          .byte   "0000 "         ; Error count field (4 decimal digits + space)
ERR_SEPARATOR:      .byte   "****************************************"
                    .byte   "****************************************"
                    ; 80 asterisks, used as visual separator after fatal error

; =============================================================================
; PASS SUMMARY LINE BUFFER
; Used by FORMAT_PASS_SUMMARY to format "NNNN ERRORS IN PASS n".
; =============================================================================
SUMMARY_BUF:        .byte   $0d             ; CR
SUMMARY_TEMPLATE:   .byte   "0000 ERRORS IN PASS " ; Pass summary template
PASS_NUMBER:        .byte   "1 "            ; Pass number digit ("1" or "2")
                    .byte   $0d

; =============================================================================
; PAGE HEADER / LISTING BANNER BUFFER
; Emitted at the top of each new page in the listing output.
; =============================================================================
PAGE_HEADER:        .byte   $0d             ; CR
PAGE_FF:            .byte   $0c             ; ASCII form-feed ($097C; set to $00 to suppress)
LISTING_BUF:        .byte   "                                              "
                    .byte   "MTU 6502 MACASM 1.1 " ; 46 spaces + title string
CURDATE:            .byte   "          "    ; 10-char date field, filled by SVC $1E
PAGE_NUMBER:        .byte   "   "           ; Page number field
                    .byte   $0d
                    .byte   "BY BRUCE D. CARBREY"

; =============================================================================
; CROSS-REFERENCE TABLE HEADER STRINGS
; TABLE_HEADER contains the two possible header strings:
;   "SYMBOL TABLE WITH CROSS REFERENCES" (with xref, 33 chars)
;   "SYMBOL TABLE             " (without xref, shorter)
; Indexed by X from PRINT_SYMTBL.
; NOTE: Last three chars double as the 3-byte arithmetic accumulator
; buffer used by the expression evaluator.
; =============================================================================
TABLE_HEADER:       .byte   "SYMBOL TABLE WITH CROSS REFEREN"
ACCUM24:            .byte   "CES"           ; Also: 24-bit arithmetic accumulator

; =============================================================================
; ARITHMETIC OPERATIONS EVALUATION STACKS
; VALUE_STACK:    Up to eight 24-bit operands
; OPERATOR_STACK: Up to 16 operators
; =============================================================================

VALUE_STACK:        .byte   "   "
                    .byte   "   "
                    .byte   "   "
                    .byte   "   "
                    .byte   "   "
                    .byte   "   "
                    .byte   "   "
                    .byte   "   "
OPERATOR_STACK:     .byte   "                "

; Identifier buffer. The current token is assembled here (up to 32 chars + NUL) before
; hash lookup.
;
IDENT_BUF:  .byte   "                                 "

; =============================================================================
; ERR_CNT_LADDER
; ============================================================================
; Fatal error encoder.  Each label is an entry point; entering at an earlier
; label causes more INC instructions to execute before falling through to the
; "ASSEMBLY ABORTED" message.  The number of increments applied to
; FATAL_ERR_CODE encodes the specific fatal error:
;
;   Entry     INC count   FATAL_ERR_CODE   Meaning
;   FATAL_MACROS_OVERNESTED         6      Macros over-nested
;   FATAL_OUTPUT_FILE_EXISTS        5      Output file already exists
;   FATAL_NO_SOURCE                 4      No source file specified
;   FATAL_COMMAND_SYNTAX            3      Command syntax error
;   FATAL_HASH_FULL                 2      Symbol table overflowed hash table
;   FATAL_HEAP_FULL                 1      Symbol table overflowed heap
; =============================================================================
FATAL_MACROS_OVERNESTED:
            inc     FATAL_ERR_CODE  ; (entry: macros over-nested)
FATAL_OUTPUT_FILE_EXISTS:
            inc     FATAL_ERR_CODE  ; (entry: output file exists)
FATAL_NO_SOURCE:
            inc     FATAL_ERR_CODE  ; (entry: no source)
FATAL_COMMAND_SYNTAX:
            inc     FATAL_ERR_CODE  ; (entry: command syntax)
FATAL_HASH_FULL:
            inc     FATAL_ERR_CODE  ; (entry: hash full)
FATAL_HEAP_FULL:
            inc     FATAL_ERR_CODE  ; (entry: heap full)
            svc     2               ; Output inline message on channel
            .byte   $02             ; -> channel 2 (console)
            .byte   $0d, "ASSEMBLY ABORTED: ", 0

            ; Dispatch on FATAL_ERR_CODE to print the specific reason
            lda     FATAL_ERR_CODE  ; Load fatal error code
            cmp     #$01
            bne     ERR_CODE_2
            ; -- error 1: symbol table heap full --
            jsr     PRINT_SYMTAB_FULL_PREFIX ; Print "SYMBOL TABLE FULL ("
            svc     2               ; Output inline message on channel
            .byte   $02
            .byte   "HEAP).", 0
            jmp     ABORT_ASSEMBLY

; -- error 2: symbol table hash table full --
ERR_CODE_2: cmp     #$02
            bne     ERR_CODE_3
            jsr     PRINT_SYMTAB_FULL_PREFIX ; Print "SYMBOL TABLE FULL ("
            svc     2               ; Output inline message on channel
            .byte   $02
            .byte   "(HASH).", 0
            jmp     ABORT_ASSEMBLY

; -- error 3: command syntax error --
ERR_CODE_3: cmp     #$03
            bne     ERR_CODE_4
            svc     2               ; Output inline message on channel
            .byte   $02
            .byte "COMMAND SYNTAX.", 0
            jmp     ABORT_ASSEMBLY

; -- error 4: no source file specified --
ERR_CODE_4: cmp     #$04
            bne     ERR_CODE_5
            svc     2               ; Output inline message on channel
            .byte   $02
            .byte   "NO SOURCE.", 0
            jmp     ABORT_ASSEMBLY

; -- error 5: output file already exists --
ERR_CODE_5: cmp     #$05
            bne     ERR_CODE_6
            svc     2               ; Output inline message on channel
            .byte   $02
            .byte   "OUTPUT FILE EXISTS.", 0
            jmp     ABORT_ASSEMBLY

; -- error 6: macros nested too deeply --
; NOTE (BUG-NOTE): if FATAL_ERR_CODE is anything other than 1–6 (should not
; happen), the "bne ERR_CODE_6" below creates an infinite loop.
ERR_CODE_6: cmp     #$06
            bne     ERR_CODE_6     ; Should not occur
            svc     2               ; Output inline message on channel
            .byte   $02
            .byte   "MACROS OVERNESTED.", 0
            jmp     ABORT_ASSEMBLY

; ============================================================================
; ABORT_ASSEMBLY
; Called after any fatal error.  Flushes the listing output buffer, frees all
; I/O channels, then issues SVC #01 to return control to CODOS.
; ============================================================================
ABORT_ASSEMBLY:
            jsr     FLUSH_OBJ_BUF   ; Flush any pending listing data
            jsr     CLOSE_ALL_CH    ; Close all open channels
            svc     1               ; Return to CODOS monitor

; ============================================================================
; PRINT_SYMTAB_FULL_PREFIX
; Outputs the partial message "SYMBOL TABLE FULL (" to channel 2.
; Called before the HEAP or HASH suffix messages above.
; ============================================================================
PRINT_SYMTAB_FULL_PREFIX:
            svc     2               ; Output inline message on channel
            .byte   $02
            .byte   "SYMBOL TABLE FULL (", 0
            rts

; ============================================================================
; ERR_TYPE_LADDER  (ERROR_MACRO – ERROR_SYNTAX)
; ============================================================================
; Error-type encoder, analogous to ERR_CNT_LADDER above.
; Each label is an entry point.  The number of INC operations applied to
; ERR_TYPE_CODE encodes the error class
;
; After the last INC, the ladder:
;  1. Saves Y to LINE_POS.
;  2. Pulls the two JSR return-address bytes off the stack into
;     SAVED_RET – this discards the intermediate return frame
;     implementing a non-local goto.
;  3. Restores the stack pointer from SAVED_SP – this discards
;     all intervening stack frames, implementing a longjmp.
;  4. Dispatches through (ERRFNP) – the pass-dependent error handler.
; ============================================================================
ERROR_MACRO:
            inc     ERR_TYPE_CODE   ; (entry: 12 increments)
            inc     ERR_TYPE_CODE   ; Error 11 is set directly by REPORT_ERROR
ERROR_CONDITIONAL:
            inc     ERR_TYPE_CODE   ; entry: 10 increments
ERROR_RANGE:
            inc     ERR_TYPE_CODE   ; entry:  9 increments
ERROR_ADDRESSING:
            inc     ERR_TYPE_CODE   ; entry:  8 increments
ERROR_TOO_COMPLEX:
            inc     ERR_TYPE_CODE   ; entry:  7 increments
ERROR_UNDEFINED:
            inc     ERR_TYPE_CODE   ; entry:  6 increments
ERROR_DUPLICATE:
            inc     ERR_TYPE_CODE   ; entry:  5 increments
ERROR_ZERO_PAGE:
            inc     ERR_TYPE_CODE   ; entry:  4 increments
ERROR_IDENTIFIER:
            inc     ERR_TYPE_CODE   ; entry:  3 increments
ERROR_ARITHMETIC:
            inc     ERR_TYPE_CODE   ; entry:  2 increments
ERROR_SYNTAX:
            inc     ERR_TYPE_CODE   ; entry:  1 increment – least-severe error
            sty     LINE_POS        ; Save current line position
            pla                     ; Discard JSR return address (high byte)
            sta     SAVED_RET+1
            pla                     ; Discard JSR return address (low byte)
            sta     SAVED_RET
            ldx     SAVED_SP        ; Restore stack pointer (longjmp)
            txs
            jmp     (ERRFNP)        ; Dispatch to pass-dependent error handler

; ============================================================================
; REPORT_ERROR
; Sets ERR_TYPE_CODE = 11 (File I/O Error), calls FORMAT_ERR_LINE
; to emit the error annotation, then jumps to FATAL_NO_SOURCE to increment the
; fatal error counter (code 4 = no source -> aborts the run).
; Called on internal / unrecoverable errors detected outside normal assembly.
; ============================================================================
REPORT_ERROR:
            lda     #$0b            ; Set error type to 11 (File I/O error)
            sta     ERR_TYPE_CODE   ; ERR_TYPE_CODE = 11
            jsr     REPORT_LINE_ERROR ; Format and emit error line
            jmp     FATAL_NO_SOURCE

; ============================================================================
; MAIN_ENTRY
; Assembler start-up routine, entered from the CODOS loader via START above.
;
; Actions (in order):
;  1. Clear decimal mode (CLD – precaution after CODOS entry).
;  2. Enable SVC handler (SVCENB = $80).
;  3. Print version banner via SVC #$02 (inline message on channel 2).
;  4. Fetch the current date from CODOS via SVC #$1E into CURDATE.
;  5. Parse command-line options (PARSE_CMD_LINE / PARSE_ALL_OPTS / PROBE_MEMORY).
;  6. Determine pass-1 behaviour:
;       - If LST_DRIVE = 'N' (no listing), clear LISTING_FILE_OPEN and XREF_MODE.
;       - Otherwise open the listing channel.
;  7. Run pass 1 (PASS1_MAIN_LOOP).
;  8. If errors exist but listing is active:
;       - Set flags for pass-2 listing and re-run pass 1 as a second pass.
;  9. If no errors: run pass 2 (PASS2_MAIN_LOOP).
; 10. Print cross-reference table if enabled.
; 11. Print summary stats, flush listing, close channels, return to CODOS.
; ============================================================================
MAIN_ENTRY:
            cld                     ; Clear decimal mode
            lda     #$80
            sta     SVCENB          ; Enable SVC (BRK) handler
            svc     2               ; Output inline message on channel
            .byte   $02             ; -> channel 2 (console)
            .byte   $0d, "MTU 6502 MACRO ASSEMBLER VERSION 1.1 (21-OCT-82)"
            .byte   $0d, "Copyright 1982 Micro Technology Unlimited", 0

            ; Fetch current date into CURDATE via SVC $1E
            lda     #<CURDATE
            sta     U6
            lda     #>CURDATE
            sta     U6+1
            ldy     #$00
            svc     $1e             ; Obtain current date into buffer at U6

            lda     #$fa
            sta     PAGE_LINE_CNT   ; Initialise PAGE_LINE_CNT = $FA (triggers page break)
            lda     #$00
            jsr     PARSE_CMD_LINE  ; Parse source/output filenames
            jsr     OPEN_ERR_CHANNEL ; Open error file channel
            jsr     PROBE_MEMORY    ; Determine symbol-table ceiling

            ; Check for no-listing option
            lda     LST_DRIVE       ; Listing drive
            cmp     #'N'            ; 'N' = no listing file
            bne     @NOLIST
            lsr     LISTING_FILE_OPEN ; Clear LISTING_FILE_OPEN (shift MSB out)
            lsr     XREF_MODE       ; Clear cross-reference mode
            jmp     @CONT

@NOLIST:    lda     #$10            ; Open listing channel with mode $10
            jsr     OPEN_LST_CHANNEL ; Assign listing channel
            lda     #$80
            sta     LISTING_FILE_OPEN ; Set LISTING_FILE_OPEN

@CONT:      lda     #$00
            sta     LISTING_ON      ; Listing off initially
            sta     ERROR_LISTING_RUN ; Error listing off initially
            jsr     PASS1_MAIN_LOOP ; Perform pass 1

            ; Check for undefined symbols (ERR_CNT = error count)
            lda     ERR_CNT
            ora     ERR_CNT+1
            beq     @SKIP           ; No errors: skip to pass 2 setup

            ; Errors exist: check if listing is enabled
            bit     LISTING_FILE_OPEN ; Test LISTING_FILE_OPEN
            bpl     @DONE           ; No listing: go straight to cleanup

            ; Re-run pass 1 as a second scan to produce the error listing
            sec
            ror     LISTING_ON      ; Enable listing
            sec
            ror     ERROR_LISTING_RUN ; Enable error listing
            jsr     PASS1_MAIN_LOOP ; Second pass-1 scan with listing
            jmp     @CONT2

@SKIP:      jsr     PRINT_PASS_SUMMARY_CONSOLE ; Print pass-1 summary to console
            jsr     OPEN_OBJ_CHANNEL ; Open object output channel
            jsr     PASS2_MAIN_LOOP ; Perform pass 2 (emit object code)

@CONT2:     bit     LISTING_FILE_OPEN ; Test LISTING_FILE_OPEN
            bpl     @DONE
            jsr     PRINT_SYMTBL    ; Print cross-reference table

@DONE:      jsr     PRINT_PASS_SUMMARY ; Print pass-1 summary to console and file
            jsr     FLUSH_OBJ_BUF   ; Flush listing buffer
            jsr     PRINT_FINAL_SUM ; Print final statistics
            jsr     CLOSE_ALL_CH    ; Close all channels
            nop
            nop
            rts                     ; Return to CODOS (caller from loader)

; ============================================================================
; CLOSE_ALL_CH
; Frees CODOS I/O channels SRC_CHANNEL, LST_CHANNEL, OBJ_CHANNEL, and
; CON_CHANNEL via SVC #$16.  If DEF_DEVICE ≠ 'N', also frees DEF_CHANNEL.
; ============================================================================
CLOSE_ALL_CH:
            ldx     SRC_CHANNEL     ; Source channel
            svc     $16             ; Free (close) a channel
            ldx     LST_CHANNEL     ; List channel
            svc     $16
            ldx     OBJ_CHANNEL     ; Object channel
            svc     $16
            ldx     CON_CHANNEL     ; Console channel
            svc     $16
            lda     DEF_DEVICE      ; Definitions device
            cmp     #'N'            ; 'N' = no definitions file -> skip
            beq     @RETURN
            ldx     DEF_CHANNEL     ; Definitions channel
            svc     $16
@RETURN:    rts

.ifdef mtu
; ============================================================================
; DISABLE_IO_PAGE
; Disables the MTU I/O page at $BE00–$BFFF by writing to address $FFFE
; (hardware bank-select register).  Sets the high bit of SEEIO as a flag.
; The byte written to $FFFE is whatever is in A on entry (caller's context).
; ============================================================================
DISABLE_IO_PAGE:
            sec
            ror     SEEIO           ; Set bit 7 of SEEIO (I/O page active flag)
            sta     $fffe           ; Write to hardware: enable I/O page
            rts

; ============================================================================
; ENABLE_IO_PAGE
; Re-enables normal RAM at $BE00–$BFFF by writing to $FFFF.  Clears the
; SEEIO flag if it was set by DISABLE_IO_PAGE.
; ============================================================================
ENABLE_IO_PAGE:
            asl     SEEIO           ; Shift out bit 7 (clears SEEIO flag)
            bmi     @RETURN         ; If still set, skip write (already enabled)
            sta     $ffff           ; Write to hardware: disable I/O page (enable RAM)
@RETURN:    rts
.endif

; ============================================================================
; PARSE_CMD_LINE
; Reads the CODOS command-line buffer (obtained via SVC #$0C) and extracts:
;   1. Source filename -> LINE_BUF, then parsed by PARSE_FILENAME.
;   2. Three output filename slots (listing, object, definitions) filled via
;      COPY_FILENAME_TO_SLOT into SRC_FNAME_SLOT.
;   3. Option flags (=M, =S, =E, =L, =N, ==) extracted by PARSE_OPTIONS.
;
; The command-line buffer pointer is at U5 (obtained from SVC $0C).
; Source line text is also copied into LINE_BUF.
; ============================================================================
PARSE_CMD_LINE:
            svc     $0c             ; Obtain system buffer addresses -> U5 = cmd line ptr
            sty     CMD_Y_SAVE      ; CMD_Y_SAVE = Y (start of command line)
            ldx     CON_CHANNEL     ; Console channel
            lda     #'C'            ; Assign console ('C' device) to error channel
            svc     $15             ; Assign channel to device or file
            ldy     #$00
            sty     SRC_DRIVE       ; Source drive = 0 (default)
            sty     DIRECT_DEFS_ACT ; Clear DIRECT_DEFS_ACT

            ; Copy command line to line buffer until CR ($0D) or 256 chars
@LOOP:      lda     (U5),Y
            cmp     #$0d            ; CR = end of command line
            beq     @CONT
            sta     LINE_BUF,Y      ; Store in line buffer
            iny
            bne     @LOOP

@CONT:      lda     #$00
            sta     LINE_BUF,Y      ; NUL-terminate line buffer
            iny
            sty     LINE_BUF_LEN    ; Line buffer length
            ldy     CMD_Y_SAVE      ; Restore Y to start of command line
            jsr     SKIP_LEADING_SPACES ; Skip spaces -> Y points to first non-space
            bne     @NONBLANK
@NOFILE:    jsr     REPORT_ERROR    ; No filename found -> fatal error
@NONBLANK:  jsr     IS_UPPER        ; Check: is first char A-Z?
            bcs     @NOFILE         ; Not a valid identifier start -> error
            lda     #'A'
            sta     DEFAULT_EXT_CHAR ; Set default extension 'A'
            jsr     PARSE_FILENAME  ; Parse source filename into FNAME_BUFFER record
            bcc     @GETDRV
            jsr     REPORT_ERROR    ; Parse error -> fatal

@GETDRV:    sty     CMD_CUR_POS     ; CMD_CUR_POS = Y (after source filename)
            lda     PARSED_DRIVE    ; Parsed drive number
            sta     SRC_DRIVE       ; Source drive
            sta     LST_DRIVE       ; Listing drive (same by default)
            sta     OBJ_DRIVE       ; Object drive (same by default)

            ; Copy parsed source filename into channel slot 0
            ldx     #SRC_FNAME_OFFSET
            jsr     COPY_FILENAME_TO_SLOT

            ; Set up listing filename: change extension to 'L'
            ldx     EXTENSION_POS   ; Extension position in FNAME_BUFFER
            lda     #'L'            ; Listing file extension
            sta     FNAME_BUFFER,X
            ldx     #LST_FNAME_OFFSET ; Slot 1 offset (LST_FNAME_SLOT)
            jsr     COPY_FILENAME_TO_SLOT

            ; Set up object filename: change extension to 'C'
            ldx     EXTENSION_POS
            lda     #'C'            ; Object file extension
            sta     FNAME_BUFFER,X
            ldx     #OBJ_FNAME_OFFSET ; Slot 2 offset (OBJ_FNAME_SLOT)
            jsr     COPY_FILENAME_TO_SLOT

            ldy     CMD_Y_SAVE      ; Restore Y to command-line start
            jsr     SKIP_LEADING_SPACES ; Skip spaces
            bne     PARSE_OPTIONS   ; More tokens: parse options
            rts                     ; End of command line

; ============================================================================
; PARSE_OPTIONS
; Scans remaining command-line tokens looking for "=option" pairs.
; Recognises:
;   =<char><value>   named option: delegates to APPLY_NAMED_OPTION
;   ==<filename>     listing redirect: delegates to PARSE_LISTING_REDIR
; ============================================================================
PARSE_OPTIONS:
            sty     OPT_START_Y     ; Start of option
            jsr     IS_UPPER        ; Is current char A-Z?
            bcc     @NEXT           ; Yes: might be an option keyword
@FATAL:     jsr     FATAL_SYNTAX_ERR ; Not a valid option syntax -> fatal
            ; Not reached

            .byte   $fe             ; Dead code

@NEXT:      iny                     ; Advance past current char
            lda     LINE_BUF,Y
            beq     @FATAL          ; EOL without '=' -> error
            cmp     COMMENT         ; Comment char?
            beq     @FATAL          ; -> error
            cmp     #'='
            beq     @EQUATE         ; Found '=' -> process option
            cmp     #' '
            bne     @NEXT           ; Not space: keep scanning for '='
            jsr     SKIP_LEADING_SPACES ; Skip spaces
            cmp     #'='
            bne     @FATAL           ; No '=' after name -> error

@EQUATE:    iny                     ; Advance past '='
            lda     LINE_BUF,Y
            cmp     #'='            ; Double '==' -> listing redirect
            bne     @SINGLE
            jsr     PARSE_LIST_REDIR
            jmp     @CONT

@SINGLE:    jsr     APPLY_NAMED_OPT ; Process named option

@CONT:      jsr     SKIP_LEADING_SPACES ; Skip to next token
            bne     PARSE_OPTIONS    ; More tokens: loop
            rts

; ============================================================================
; APPLY_NAMED_OPT
; Reads the option letter preceding the '=' and dispatches:
;   M  -> macro cross-reference flag (XREF_MODE / LINE_LIST_FLAGS)
;   S  -> suppress flag (LIST_SUPPRESS or ERROR_LISTING_RUN)
;   L  -> listing drive/device (LST_DRIVE)
;   N  -> "no listing" flag (ERR_DRIVE = ERR_DEVICE)
;   E  -> error listing flag
;   O/X/D  -> other extension options (indexed via EXT_OPT_CHARS)
; ============================================================================
APPLY_NAMED_OPT:
            ldy     OPT_START_Y     ; Start of option
            lda     LINE_BUF,Y      ; Option letter
            sta     CURRENT_OPT     ; Save as current option letter
            cmp     #'M'
            beq     @MOPT
            cmp     #'S'
            bne     @NOTS

@MOPT:      jsr     ADVANCE_SKIP_LEADING_SPACES ; Skip to char after option letter
            jsr     ADVANCE_SKIP_LEADING_SPACES ; Skip '='
            cmp     #'N'            ; =N means disable this option
            bne     @FATAL
            ldx     #$00            ; Value = 0 (disable)
            lda     CURRENT_OPT
            cmp     #'M'            ; Was it 'M'?
            beq     @SKIP
            stx     XREF_ENABLED    ; Disable xref for 'S'
@SKIP:      stx     XREF_MODE       ; XREF_MODE = 0
            iny
            rts

@NOTS:      ldx     #$06            ; Search EXT_OPT_CHARS backwards for valid option letter
@LOOP:      lda     EXT_OPT_CHARS-1,X
            cmp     CURRENT_OPT     ; Compare with option letter
            beq     @FOUND          ; Found
            dex
            bne     @LOOP
@FATAL:     jsr     FATAL_SYNTAX_ERR ; Unknown option -> fatal

@FOUND:     stx     OPT_INDEX       ; Save option index
            lda     DEFAULT_EXTS,X  ; Get default extension for this option
            sta     DEFAULT_EXT_CHAR ; Store as default extension
            iny
            jsr     ADVANCE_SKIP_LEADING_SPACES ; Skip spaces to option value
            beq     @FATAL          ; Empty value -> error
            cmp     #$22            ; '"' = explicit filename follows
            beq     @FNAME
            jsr     ADVANCE_PEEK_BLANK ; Check that next is not blank (Null/Space/EOL)
            beq     @NONBLNK        ; Non blank -> use single-char value
            dey

@FNAME:     jsr     PARSE_FILENAME  ; Parse filename value
            bcs     @FATAL          ; Parse error -> fatal option error
            ldx     OPT_INDEX       ; Option index
            cpx     #$04            ; Option 4 = listing?
            bne     @NOLST
            sty     LIST_DRIVE_POS  ; LIST_DRIVE_POS = Y (listing drive position)
@NOLST:     lda     PARSED_DRIVE    ; Parsed drive number
            sta     SRC_DRIVE,X     ; Store in appropriate drive variable
            lda     CHAN_ASSIGN_TBL,X ; Channel assignment table index
            tax
            jsr     COPY_FILENAME_TO_SLOT ; Copy to filename slot
            rts

@NONBLNK:   dey
            lda     LINE_BUF,Y      ; Single-char value (device letter)
            iny
            cmp     #'N'            ; 'N' = none/null
            bne     @NOSRCDRIVE
            ldx     OPT_INDEX
            sta     SRC_DRIVE,X     ; Store 'N' as drive = none
            rts

@NOSRCDRIVE:
            ldx     CURRENT_OPT     ; Option letter
            cpx     #'L'            ; 'L' option -> listing drive
            bne     @NOLSTDRIVE
            sta     LST_DRIVE       ; LST_DRIVE = value
            rts

@NOLSTDRIVE:
            cpx     #'E'            ; 'E' option -> error drive
            bne     @FATAL
            sta     ERR_DEVICE      ; ERR_DEVICE = value
            rts

; ============================================================================
; PARSE_LIST_REDIR
; Handles the "==" double-equals syntax for redirecting listing/definitions
; output to an inline filename in the command buffer.
; Copies the filename text to the DDEFSBUF buffer and stores the end pointer
; in DDEFS_END
; ============================================================================
PARSE_LIST_REDIR:
            bit     DIRECT_DEFS_ACT ; Test if redirect already active
            bmi     @SKIP           ; Already active: skip pointer init
            lda     #<DDEFSBUF      ; Point TEMPP at direct-definitions buffer
            sta     TEMPP
            lda     #>DDEFSBUF
            sta     TEMPP+1

@SKIP:      sec
            ror     DIRECT_DEFS_ACT ; Enable output redirect
            jsr     ADVANCE_SKIP_LEADING_SPACES ; Skip past '='
            bne     @CONT
            jsr     FATAL_SYNTAX_ERR ; No filename after '==' -> fatal

@CONT:      jsr     ADVANCE_PEEK_BLANK ; Skip non-blanks
            bne     @CONT           ; Keep skipping until space/EOL
            sty     CMD_END_POS     ; End of filename token
            ldy     OPT_START_Y     ; Restore Y to start of option
            ldx     #$00

            ; Copy filename text from LINE_BUF into DDEFSBUF via (TEMPP)
@COPY:      lda     LINE_BUF,Y      ; Read char from command line
            sta     (TEMPP,X)       ; Store via (TEMPP + X = TEMPP since X=0)
            inc     TEMPP           ; Advance destination pointer
            bne     @SKIP2
            inc     TEMPP+1
@SKIP2:     iny
            cpy     CMD_END_POS     ; Past end of filename?
            bcc     @COPY           ; No: keep copying

            lda     #$0d            ; Append CR to terminate the buffer entry
            sta     (TEMPP,X)
            inc     TEMPP
            bne     @SKIP3
            inc     TEMPP+1
@SKIP3:     lda     TEMPP           ; Save end pointer to DDEFS_END
            sta     DDEFS_END
            lda     TEMPP+1
            sta     DDEFS_END+1
            rts

; ============================================================================
; PARSE_FILENAME
; Parses a CODOS filename from LINE_BUF at (LINE_BUF,Y).
; Accepts: [drive_digit ':'] name ['.' extension]
; The parsed 14-byte record is written to the FNAME_BUFFER scratch buffer.
; Drive digit (if present) is stored in PARSED_DRIVE.
; Returns:  C clear = success,  C set = parse error.
; ============================================================================
PARSE_FILENAME:
            lda     SRC_DRIVE       ; Source drive (current default drive)
            sta     PARSED_DRIVE    ; Set parsed drive with default
            sty     LINE_POS        ; Save current line position

            lda     LINE_BUF,Y
            cmp     #$22            ; '"' = use current default filename (no parse)
            bne     @GETFNAME

            ; Copy current default filename from SRC_FNAME_SLOT to FNAME_BUFFER
            ldx     #$00
@COPY:      lda     SRC_FNAME_SLOT,X
            cmp     #'.'            ; Extension delimiter found -> stop
            beq     @CHKEXT
            sta     FNAME_BUFFER,X
            inx
            bne     @COPY

@GETFNAME:  jsr     IS_UPPER        ; Is first char A-Z?
            bcs     @RETERR         ; No -> error
            sta     FNAME_BUFFER    ; First char into name record
            ldx     #$01            ; Index into FNAME_BUFFER (already stored char 0)

@CHKEXT:    iny                     ; Advance to next char
            jsr     PEEK_BLANK_CHAR ; Check if end-of-name char (space/NUL/comment)
            beq     @DEFEXT         ; End: no extension -> use default
            cmp     #':'            ; Drive separator?
            beq     @DEFEXT         ; -> process drive digit
            cmp     #'.'            ; Extension delimiter?
            beq     @PARSEEXT       ; -> parse extension
            jsr     IS_LOCAL        ; Valid filename char?
            bcs     @RETERR         ; No -> error
            sta     FNAME_BUFFER,X  ; Accumulate char
            inx
            cpx     #$0d            ; Max 13 chars in name
            bcs     @RETERR         ; Too long -> error
            bcc     @CHKEXT         ; Continue

@PARSEEXT:  sta     FNAME_BUFFER,X  ; Store '.'
            inx
            iny                     ; Skip to extension char
            lda     LINE_BUF,Y
            jsr     IS_ALNUM        ; Must be alphanumeric
            bcs     @RETERR         ; Not valid -> error
            iny
            bne     @CONT           ; Continue with explicit extension char

@DEFEXT:    lda     #'.'            ; Insert default extension delimiter
            sta     FNAME_BUFFER,X
            inx
            lda     DEFAULT_EXT_CHAR ; Get default extension char

@CONT:      sta     FNAME_BUFFER,X  ; Store extension char
            cpx     #$03            ; Need at least 3 chars for a valid name
            stx     EXTENSION_POS   ; Save extension position
            bcc     @RETERR         ; Too short -> error

            jsr     SKIP_LEADING_SPACES ; Skip spaces
            beq     @DONE           ; EOL: done
            cmp     #':'            ; Drive separator?
            bne     @DONE           ; No: done without drive

            ; Parse drive digit after ':'
            jsr     ADVANCE_SKIP_LEADING_SPACES ; Skip ':'
            beq     @RETERR         ; Nothing after ':' -> error
            jsr     IS_DIGIT        ; Must be digit
            bcs     @RETERR         ; Not a digit -> error
            sec
            sbc     #'0'            ; Convert ASCII digit to drive number
            sta     PARSED_DRIVE    ; Set parsed drive = digit value
            iny

@DONE:      clc                     ; C clear = success
            sty     CMD_Y_SAVE      ; CMD_Y_SAVE = Y (after filename)
            rts
@RETERR:    sec                     ; C set = parse error
            rts

; ============================================================================
; COPY_FILENAME_TO_SLOT
; Copies 14 bytes from the FNAME_BUFFER scratch buffer to the filename record at
; FILENAME_SLOTS+X (channel name table).  X = byte offset of the slot (0, $0E, $1C…).
; Preserves Y register.
; ============================================================================
COPY_FILENAME_TO_SLOT:
            tya
            pha                     ; Save Y
            ldy     #$00
@COPY:      lda     FNAME_BUFFER,Y
            sta     FILENAME_SLOTS,X ; Copy to filename slot
            inx
            iny
            cpy     #$0e            ; 14 bytes
            bcc     @COPY
            pla
            tay                     ; Restore Y
            rts

; ============================================================================
; FATAL_SYNTAX_ERR
; Outputs the current source line to channel 2 (console), followed by the
; 80-asterisk separator line, then jumps to FATAL_COMMAND_SYNTAX
; to abort with FATAL_ERR_CODE = 3 ("COMMAND SYNTAX").
; ============================================================================
FATAL_SYNTAX_ERR:
            sty     LINE_POS        ; Save current line position
            lda     #<LINE_BUF0
            sta     U6
            lda     #>LINE_BUF0
            sta     U6+1
            lda     #$0d
            sta     LINE_BUF0       ; Prepend CR to source line
            ldy     LINE_BUF_LEN    ; LINE_BUF length
            ldx     #$02            ; Channel 2 (console)
            svc     6               ; Output line of text on channel
            lda     #<ERR_SEPARATOR ; Asterisk separator line
            sta     U6
            lda     #>ERR_SEPARATOR
            sta     U6+1
            ldy     LINE_POS        ; Restore line position
            svc     6               ; Output line of text on channel
            jmp     FATAL_COMMAND_SYNTAX
            ; Does not return

; ============================================================================
; PROBE_MEMORY
; Determines the top of available RAM by writing and verifying test patterns
; page by page, starting from address 0 and working up to HEAP2END (the
; configured end of secondary heap).  The probe pointer is HEAP2PRP.
;
; Also configures the MTU bank-select register BNKCTL: low 2 bits are set
; from BANK_CTL_BITS to select the working bank.
;
; On exit:
;   HEAP2END/HEAP2END+1 = highest reachable page address
;   EXPANSION_BANK bit 7 set if any secondary heap pages respond
;   BNKCTL is left pointing to bank 3 (the data bank)
; ============================================================================
PROBE_MEMORY:
            ldx     #$00
            stx     EXPANSION_BANK  ; EXPANSION_BANK = 0 initially
            stx     HEAP2PRP        ; Probe pointer low = 0
            stx     HEAP2PRP+1      ; Probe pointer high = 0
            lda     BNKCTL          ; Read current bank-control register
            and     #<~$03          ; Bank 3
            ora     BANK_CTL_BITS   ; Apply BANK_CTL_BITS
            sta     BNKCTL          ; Write bank select

@LOOP:      lda     HEAP2PRP+1      ; Current page number
            cmp     HEAP2END+1      ; Reached end of secondary heap?
            beq     @BREAK          ; Yes: stop

            ; Write-back test: read, increment, write, compare
            lda     (HEAP2PRP,X)    ; Read byte at current page
            clc
            adc     #$01
            sta     (HEAP2PRP,X)    ; Write incremented value
            cmp     (HEAP2PRP,X)    ; Did it stick?
            bne     @BREAK          ; No: RAM not present here -> stop
            sec
            sbc     #$01
            sta     (HEAP2PRP,X)    ; Restore original value
            inc     HEAP2PRP+1      ; Advance to next page
            jmp     @LOOP

@BREAK:     ; Save probe result as actual secondary heap end
            lda     HEAP2PRP
            sta     HEAP2END        ; Update secondary heap end low
            lda     HEAP2PRP+1
            sta     HEAP2END+1      ; Update secondary heap end high
            beq     @DONE           ; Zero high byte -> no pages found
            sec
            ror     EXPANSION_BANK  ; EXPANSION_BANK bit 7 = 1 (RAM found)

@DONE:      ; Switch back to bank 0 (data bank)
            lda     BNKCTL
            and     #$fc            ; Mask out other bits
            ora     #$03            ; Restore bank 0
            sta     BNKCTL
            rts

; ============================================================================
; CHARACTER CLASSIFICATION ROUTINES
;
; IS_ALNUM   C clear if A is '0'–'9' or 'A'–'Z';  C set otherwise
; IS_UPPER   C clear if A is 'A'–'Z';             C set otherwise
; SET_CARRY  Always sets carry (shared "reject" return)
; IS_DIGIT   C clear if A is '0'–'9';             C set otherwise
; IS_LOCAL   C clear if A equals the local-label char (UNDERSCORE = '_')
;
; All routines preserve A.
; ============================================================================
IS_ALNUM:
            jsr     IS_DIGIT
            bcc     RET_ALNUM       ; It's a digit -> C clear, return
IS_UPPER:
            cmp     #'A'
            bcs     CHK_UPPER       ; >= 'A': might be uppercase letter
SET_CARRY:
            sec
            rts
CHK_UPPER:  cmp     #'Z'+1          ; > 'Z'?
RET_ALNUM:      rts                     ; Returns C clear if in range, C set otherwise

IS_DIGIT:
            cmp     #'0'
            bcc     SET_CARRY       ; < '0': not a digit
            cmp     #'9'+1          ; C clear if <= '9', set if > '9'
            rts

IS_LOCAL:
            cmp     UNDERSCORE           ; Compare with local-label char ('_')
            bne     IS_ALNUM        ; Not '_': fall through to IS_ALNUM
            clc
            rts

; ============================================================================
; PASS1_MAIN_LOOP
; Performs assembler pass 1: reads every source line, processes labels into
; the symbol table, and advances the location counter to establish addresses.
; No object code is emitted to the output file.
;
; Entry sequence:
;  1. INIT_LINE_WS clears per-line scratch.
;  2. Hardware copy-protection check: reads specific nibbles from the MTU
;     special-register port and compares against HW_CHECK_DATA.
;     Aborts with "SYS. ERROR LEVEL 1" if they don't match.
;  3. Initialises the symbol table (INIT_SYMTBL).
;  4. Opens the source channel via OPEN_CHANNEL / READ_NEXT_BLOCK.
;  5. Loops: for each source line via READ_NEXT_LINE, processes
;     labels (EVAL_EXPRESSION), looks up the opcode (LOOKUP_OPCODE),
;     and advances LOCCNT by the instruction size.
;  6. ERRFNP is set to PASS1_ERROR_HANDLER (pass-1 error recovery vector).
; ============================================================================
PASS1_MAIN_LOOP:
            jsr     INIT_LINE_WS    ; Clear per-line workspace
            lda     #$00
            sta     ENTRY_DEFINED   ; Clear ENTRY_DEFINED
            sta     PASSFLG         ; Clear PASSFLG (pass 1)
            lda     #$80
            sta     LISTING_ACTIVE  ; Set LISTING_ACTIVE
            lda     LSTST           ; Initialise LSTOUTP (listing write ptr)
            sta     LSTOUTP
            lda     LSTST+1
            sta     LSTOUTP+1
            jsr     INIT_SYMTBL     ; Zero hash table and install built-in macros

.ifdef mtu
            ; Hardware copy-protection check
            jsr     DISABLE_IO_PAGE ; Enable hardware I/O
            sta     SPREGREN        ; Activate special-register read
            ldx     HW_CHECK_LEN    ; Number of nibbles to consume
DISCARD_NIBBLE:
            lda     SPREGREAD       ; Read and discard (synchronise)
            dex
            bne     DISCARD_NIBBLE
READ_NIBBLE:
            lda     SPREGREAD       ; Read a hardware check nibble
            and     #$0f            ; Keep low nibble
            cmp     HW_CHECK_DATA,X ; Compare with expected HW_CHECK_DATA[X]
            beq     CHK_MATCH       ; Match: continue
            ; Checksum failed -> print error and return to monitor
            svc     2               ; Output inline message on channel
            .byte   $02
            .byte   $0d, "SYS. ERROR LEVEL 1, PLEASE CALL MTU.", 0
            svc     1               ; Return to CODOS monitor
            ; Not reached

CHK_MATCH:  inx
            cpx     #$05            ; All 4 nibbles checked?
            bcc     READ_NIBBLE     ; No: check next nibble
            jsr     ENABLE_IO_PAGE  ; Restore RAM
.endif
            ; Initialise symbol value pointer and save it
            lda     #$00
            sta     SYMVAL
            sta     SYMVAL+1
            sta     SYMVAL+2
            lda     SYMVALP         ; Save SYMVALP for pass-2 restore
            sta     SEGHEADP
            sta     SYMVALP_SAVE
            lda     SYMVALP+1
            sta     SEGHEADP+1
            sta     SYMVALP_SAVE+1

            jsr     WRITE_PADDING   ; Write four zero bytes to object segment header

            ; Save stack pointer for error longjmp (ERR_TYPE_LADDER)
            tsx
            stx     SAVED_SP

            ; Set pass-1 error handler
            lda     #<PASS1_ERROR_HANDLER
            sta     ERRFNP
            lda     #>PASS1_ERROR_HANDLER
            sta     ERRFNP+1

            ; Open source file channel and read first block
            ldy     CMD_CUR_POS     ; Get current cursor position
            jsr     OPEN_CHANNEL    ; Open source channel
            jsr     READ_NEXT_BLOCK ; Read next block from channel
            bit     DIRECT_DEFS_ACT ; Ouput redirect active?
            bpl     @NEXTB
            jsr     SETUP_DDEF_READ_MODE ; Set up direct-defs read mode
            jmp     PASS1_LINE_LOOP

@NEXTB:     jsr     READ_BLOCK_SVC  ; Read next block from channel
            jmp     PASS1_LINE_LOOP

; ============================================================================
; PASS1_ERROR_HANDLER
;
; Pass 1 error handler: prints error annotation, resumes at next line
; ============================================================================
PASS1_ERROR_HANDLER:
            jsr     REPORT_LINE_ERROR ; Format error line
            jmp     PASS1_END_CHECK

; ============================================================================
; PASS1_ERROR_CONT
;
; Per-line continuation point for pass 1.  Jumped to at the end of every
; source-line handler (instruction, directive, macro, label-only, empty line,
; comment line, or after an inline error recovery).
;
; Actions:
;   1. Clears VALUE_OUTPUT ($00) so the next line starts with no pending
;      object-byte value for the listing.
;   2. Tests LISTING_ON: if listing is disabled (N clear) jumps directly to
;      PASS1_END_CHECK without emitting any output.
;   3. Tests MACRO_EXPANDING: if not currently expanding a macro goes to
;      @EMITL unconditionally; otherwise additionally guards on LIST_SUPPRESS2
;      and skips to PASS1_END_CHECK when macro-body listing is suppressed.
;   4. Calls OUTPUT_ERR_LINE to append the formatted source / error line to
;      the listing buffer.
;   5. Falls through to PASS1_END_CHECK.
; ============================================================================
PASS1_ERROR_CONT:
            lda     #$00
            sta     VALUE_OUTPUT    ; Clear VALUE_OUTPUT
            bit     LISTING_ON      ; LISTING_ON?
            bpl     PASS1_END_CHECK ; Not listing: skip output
            bit     MACRO_EXPANDING ; MACRO_EXPANDING?
            bpl     @EMITL
            bit     LIST_SUPPRESS2  ; LIST_SUPPRESS2?
            bpl     PASS1_END_CHECK
@EMITL:     jsr     OUTPUT_ERR_LINE ; Emit error indicator line

; ============================================================================
; PASS1_END_CHECK
;
; End-of-line convergence point for pass 1.  Reached after every successfully
; processed or skipped source line and after PASS1_ERROR_CONT finishes any
; optional listing output.
;
; Checks END_SEEN:
;   - If clear (.END not yet encountered) -> fall through to PASS1_LINE_LOOP
;     to fetch and process the next source line.
;   - If set (.END directive was assembled) -> jump to END_OF_PASS1 to
;     finalise the pass (flush the segment header, check .IF nesting, return).
; ============================================================================
PASS1_END_CHECK:
            bit     END_SEEN        ; .END directive found?
            bpl     PASS1_LINE_LOOP
            jmp     END_OF_PASS1    ; Yes: .END seen -> finish pass

; ============================================================================
; PASS1_LINE_LOOP
;
; Main per-line processing loop for assembler pass 1.
; Iterates over every source line of the current file (or .READ include),
; building the symbol table and advancing the location counter without
; emitting any object code.
;
; On each iteration:
;   1. Checks INCLUDE_POP: if set, calls POP_INCL_FRAME to return from a
;      .READ-included file; if the include stack is empty, jumps to
;      END_OF_PASS1 (same effect as .END).
;   2. Calls READ_NEXT_LINE to load the next source line into LINE_BUF.
;   3. Snapshots LOCCNT -> LOCCNT2 and formats the address digits for the
;      listing via OUTPUT_ADDR_DIGITS.
;   4. Dispatches on the first character of the line:
;        NUL / comment char    -> PASS1_ERROR_CONT  (empty or comment line)
;        Space                 -> @CKEQU             (no label field)
;        Anything else         -> EVAL_EXPRESSION    (define label symbol)
;   5. After optional label processing, examines the mnemonic/directive field:
;        '='                   -> PROCESS_EQU_DIR   (EQU assignment)
;        PCSYMBOL ('*')        -> PROCESS_ORG_DIR   (origin assignment)
;        '.' + .MACRO keyword  -> macro definition path
;        Other                 -> ASSEMBLE_ZP_ABS + LOOKUP_OPCODE:
;             opcode found     -> ASSEMBLE_INSTR    (count bytes, advance LC)
;             directive        -> HANDLE_DIRECTIVE
;             macro reference  -> DISPATCH_MACRO
;   All paths end with jmp PASS1_ERROR_CONT.
; ============================================================================
PASS1_LINE_LOOP:
            bit     INCLUDE_POP     ; INCLUDE_POP? (.READ return pending)
            bpl     @CONT
            jsr     POP_INCL_FRAME  ; Pop include file stack
            bcc     @CONT           ; Stack not empty: continue
            jmp     END_OF_PASS1    ; Stack empty: same as .END

            ; Read next source line
@CONT:      jsr     READ_NEXT_LINE
            lda     LOCCNT
            sta     LOCCNT2         ; Snapshot location counter for this line
            lda     LOCCNT+1
            sta     LOCCNT2+1
            jsr     OUTPUT_ADDR_DIGITS ; Format address in listing buffer

            ldy     #$00
            lda     LINE_BUF        ; First char of source line
            beq     PASS1_ERROR_CONT ; Empty line
            cmp     COMMENT         ; Comment char?
            beq     PASS1_ERROR_CONT
            cmp     #' '            ; Space = no label field
            beq     @CKEQU
            jsr     EVAL_EXPRESSION ; Evaluate label (define symbol)

@CKEQU:     jsr     SKIP_LEADING_SPACES ; Skip to mnemonic/directive field
            cmp     #'='
            bne     @CKPC
            jsr     PROCESS_EQU_DIR ; Handle '=' EQU
            jmp     PASS1_ERROR_CONT

@CKPC:      cmp     PCSYMBOL        ; '*' = program-counter assign?
            bne     @CKDIR
            jsr     PROCESS_ORG_DIR ; Handle '*=' ORG
            jmp     PASS1_ERROR_CONT

@CKDIR:     cmp     #'.'            ; Directive (starts with '.')?
            bne     @CKOC
            jsr     DETECT_MACRO    ; Check for .MACRO keyword
            bcs     @MACRO           ; Yes: handle macro definition

@CKOC:      jsr     ASSEMBLE_ZP_ABS ; Default instruction form
            lda     LINE_BUF,Y
            beq     PASS1_ERROR_CONT ; EOL after label
            cmp     COMMENT
            beq     PASS1_ERROR_CONT

@MACRO:     jsr     LOOKUP_OPCODE   ; Look up mnemonic/opcode
            bpl     @FOUND          ; Found as regular opcode: assemble
            jsr     HANDLE_DIRECTIVE ; Handle directive or macro expansion
            jmp     PASS1_ERROR_CONT

@FOUND:     bvc     @ASSEMBLE       ; V clear: real instruction
            jsr     DISPATCH_MACRO  ; Macro invocation
            jmp     PASS1_ERROR_CONT

@ASSEMBLE:  jsr     ASSEMBLE_INSTR  ; Assemble instruction (pass 1: count bytes)
            jmp     PASS1_ERROR_CONT

; ============================================================================
; END_OF_PASS1  –  End-of-pass finalisation for pass 1.
; Called when END_SEEN or INCLUDE_POP finds the include stack empty.
; Calls ADVANCE_LOCCNT to emit the final segment header, then
; performs some addressing arithmetic on OBJBUFP and writes to a computed
; location.  The conditional-flag check at the end handles mismatched
; .IF/.ENDIF nesting.
; NOTE: The exact semantics of the OBJBUFP shifting are unclear.
; ============================================================================
END_OF_PASS1:
            jsr     ADVANCE_LOCCNT  ; Flush pending segment header
            asl     OBJBUFP         ; Shift OBJBUFP left (see NOTE above)
            rol     OBJBUFP+1       ; Complete 16-bit left shift; carry -> RUNNING_EOR_SUM inc
            bcc     @SKIP
            inc     RUNNING_EOR_SUM
@SKIP:      ldy     #$e5            ; Fixed offset for checksum
            lda     RUNNING_EOR_SUM
            sta     (OBJBUFP),Y     ; Write RUNNING_EOR_SUM at computed OBJBUFP+$E5
            bit     COND_DEPTH      ; COND_DEPTH: any open .IF blocks?
            bpl     @DONE
            asl     COND_DEPTH      ; Shift depth flag
            jsr     ERROR_CONDITIONAL ; Error: unclosed conditional
@DONE:      rts

; ============================================================================
; PASS2_MAIN_LOOP
; Performs assembler pass 2: re-reads every source line, resolves all symbol
; references, emits object code to the output file via EMIT_BYTE_PASS2
; /OBJBUF_WRITE_BYTE, and writes the assembly listing.
;
; Before the main loop, verifies that the symbol table was not corrupted
; between passes by checking SYMTBL_CHKSUM against the values stored at
; the end of pass 1.  If they differ, a crash at HALT is triggered.
;
; ERRFNP is set to PASS2_ERROR_HANDLER (pass-2 error recovery vector).
; ============================================================================
PASS2_MAIN_LOOP:
            jsr     INIT_LINE_WS    ; Clear per-line workspace
            lda     #$00
            sta     OBJBUF_POS      ; Empty object output buffer
            sta     OBJSEG_OPEN     ; Clear OBJSEG_OPEN
            sta     LISTING_ACTIVE  ; Clear LISTING_ACTIVE
            lda     #$80
            sta     PASSFLG         ; PASSFLG = $80 (pass 2)
            sta     SVCENB          ; SVCENB = $80 (SVC handler enabled)
            sec
            bit     LISTING_FILE_OPEN ; LISTING_FILE_OPEN?
            bmi     @SETFLG
            clc

@SETFLG:    ror     LISTING_ON      ; LISTING_ON: bit 7 = carry (listing if file open)

            ; Restore SYMVALP save from end of pass 1
            lda     SYMVALP_SAVE
            sta     SEGHEADP
            lda     SYMVALP_SAVE+1
            sta     SEGHEADP+1

            ; Rewind source channel to beginning of file
            ldx     SRC_CHANNEL
            svc     $11             ; Set channel position to beginning of data

            ; Compute symbol-table checksum for integrity verification
            ; Reads OBJ_BUF_LIMIT+1 bytes from (OBJBUFP)+$E1 and XOR-folds them
            ; against RUNNING_EOR_SUM (running EOR sum) and CUMULATIVE_XOR
            ; (cumulative XOR byte)
            ldy     #$e1
            ldx     OBJ_BUF_LIMIT
            inx
@LOOP:      lda     (OBJBUFP),Y
            eor     RUNNING_EOR_SUM
            sta     CUMULATIVE_XOR  ; Accumulate XOR into CUMULATIVE_XOR
            dex
            bne     @LOOP

            ; Second checksum pass: nibble-based scramble into SYMTBL_CHKSUM
            lda     SYMTBL_CHKSUM
            ldx     #$05
            clc
@LOOP2:     lda     (OBJBUFP),Y
            and     #$0f            ; Low nibble only
            cpx     #$02
            beq     @SKIP2
            cpx     #$03
            bne     @SKIP
            rol     A               ; Rotate nibble for index 3
            rol     A
@SKIP:      rol     A               ; Rotate for all non-special indices
            rol     A
            rol     A
@SKIP2:     clc
            adc     SYMTBL_CHKSUM
            sta     SYMTBL_CHKSUM   ; Accumulate into SYMTBL_CHKSUM
            bcc     @SKIP3
            eor     SYMTBL_CHKSUM+1
            sta     SYMTBL_CHKSUM+1

@SKIP3:     dex
            bne     @LOOP2

            ; If definitions file is needed, assign the channel
            lda     #<DEF_FNAME_SLOT
            sta     U3
            lda     #>DEF_FNAME_SLOT
            sta     U3+1
            lda     DEF_DEVICE      ; Definitions device
            cmp     #'N'            ; 'N' = no definitions file
            beq     @NODEF
            ldx     DEF_CHANNEL     ; Set definitions channel for SVC
            svc     $15             ; Assign channel to device or file
            svc     $12             ; Set channel position to end of file

@NODEF:     jsr     READ_NEXT_BLOCK ; Read next block from channel
            ; Verify symbol-table checksum against pass-1 reference values
            lda     SYMTBL_CHKSUM+1
            eor     CHKSUM_PASS1+1
            bne     CRASH           ; Mismatch -> crash
            bit     DIRECT_DEFS_ACT ; Redirect active?
            bpl     @NOREDIR
            jsr     SETUP_DDEF_READ_MODE
            jmp     @SKIP4

@NOREDIR:   jsr     READ_BLOCK_SVC  ; Read next block from channel

@SKIP4:     lda     SYMTBL_CHKSUM
            eor     CHKSUM_PASS1
            beq     PASS2_DONE      ; Match: proceed

CRASH:      jmp     DO_HALT         ; Checksum mismatch -> crash

PASS2_DONE: tsx
            stx     SAVED_SP        ; Save SP for error longjmp
            lda     #<PASS2_ERROR_HANDLER
            sta     ERRFNP
            lda     #>PASS2_ERROR_HANDLER
            sta     ERRFNP+1
            jmp     PASS2_FETCH_LINE

; ============================================================================
; PASS2_ERROR_HANDLER
;
; Pass 2 error recovery handler.  Installed in ERRFNP so that any error
; routine that performs a longjmp (restoring the stack pointer from SAVED_SP
; and jumping through ERRFNP) lands here instead of crashing.
;
; Actions:
;   1. Calls REPORT_LINE_ERROR to format and record the error annotation for
;      the current source line (increments error counter, fills ERR_COUNT
;      digits in the listing buffer).
;   2. Jumps unconditionally to PASS2_LINE_LOOP to resume processing from the
;      next source line, skipping any remaining work for the faulting line.
;
; Note: PASS2_ERROR_HANDLER does NOT call OUTPUT_ERR_LINE itself; the listing
; output for the erroneous line is handled by PASS2_ERROR_CONT after control
; reaches PASS2_LINE_LOOP.
; ============================================================================
PASS2_ERROR_HANDLER:
            jsr     REPORT_LINE_ERROR
            jmp     PASS2_LINE_LOOP

; ============================================================================
; PASS2_ERROR_CONT
;
; Per-line continuation point for pass 2.  Structurally mirrors
; PASS1_ERROR_CONT; jumped to at the end of every pass-2 line handler.
;
; Actions:
;   1. Clears VALUE_OUTPUT ($00).
;   2. Tests LISTING_ON: if listing is disabled (N clear) jumps directly to
;      PASS2_LINE_LOOP.
;   3. Tests MACRO_EXPANDING: if not inside a macro expansion goes to @EMIT
;      unconditionally to emit the listing line; otherwise guards on
;      LIST_SUPPRESS2 and jumps to PASS2_LINE_LOOP when suppressed.
;   4. Calls OUTPUT_ERR_LINE to write the formatted line (with any error
;      annotation and object bytes) to the listing output buffer.
;   5. Falls through to PASS2_LINE_LOOP.
; ============================================================================
PASS2_ERROR_CONT:
            lda     #$00
            sta     VALUE_OUTPUT    ; Clear VALUE_OUTPUT
            bit     LISTING_ON      ; LISTING_ON?
            bpl     PASS2_LINE_LOOP
            bit     MACRO_EXPANDING ; MACRO_EXPANDING?
            bpl     @EMIT
            bit     LIST_SUPPRESS2  ; LIST_SUPPRESS2?
            bpl     PASS2_LINE_LOOP
@EMIT:      jsr     OUTPUT_ERR_LINE

; ============================================================================
; PASS2_LINE_LOOP
;
; Top-of-loop convergence point for pass 2.  Reached from PASS2_ERROR_CONT
; (after optional listing output) and from PASS2_ERROR_HANDLER (direct jump).
;
; Checks END_SEEN:
;   - If clear (.END not yet seen) -> fall through to PASS2_FETCH_LINE to
;     handle any pending .READ stack operation and read the next source line.
;   - If set (.END directive assembled) -> jump to PASS2_END_FLUSH (pass 2 end sequence:
;     flushes the object output buffer if non-empty, then returns).
; ============================================================================
PASS2_LINE_LOOP:
            bit     END_SEEN            ; .END. directive found??
            bpl     PASS2_FETCH_LINE
            jmp     PASS2_END_FLUSH     ; Pass 2 end sequence

; ============================================================================
; PASS2_FETCH_LINE
;
; Entry point for reading the next source line in pass 2.  Also reached from
; the initialisation sequence immediately after the stack pointer is
; saved and ERRFNP is aimed at PASS2_ERROR_HANDLER.
;
; Checks INCLUDE_POP:
;   - If clear -> jump to @NEXT to read the next line directly.
;   - If set   -> call POP_INCL_FRAME to close the current .READ-included file
;     and restore the parent file context.
;       * If the include stack is now non-empty (C clear) -> continue at @NEXT.
;       * If the stack is empty (C set) -> the outermost file ended; jump to
;         PASS2_END_FLUSH (pass 2 end sequence / FLUSH_OBJECT + rts).
; ============================================================================
PASS2_FETCH_LINE:
            bit     INCLUDE_POP
            bpl     @NEXT
            jsr     POP_INCL_FRAME
            bcc     @NEXT
            jmp     PASS2_END_FLUSH

@NEXT:      jsr     READ_NEXT_LINE
            lda     LOCCNT
            sta     LOCCNT2
            lda     LOCCNT+1
            sta     LOCCNT2+1
            jsr     OUTPUT_ADDR_DIGITS

            ldy     #$00
            lda     LINE_BUF        ; First char
            beq     PASS2_ERROR_CONT
            cmp     COMMENT         ; Comment?
            beq     PASS2_ERROR_CONT
            cmp     #' '
            beq     @ISSPACE
            jsr     EVAL_EXPRESSION

@ISSPACE:   jsr     SKIP_LEADING_SPACES
            cmp     #'='
            bne     @CHKORG
            jsr     PROCESS_EQU_DIR
            jmp     PASS2_ERROR_CONT

@CHKORG:    cmp     PCSYMBOL        ; '*=' ORG?
            bne     @CHKCMT
            jsr     PROCESS_ORG_DIR
            jmp     PASS2_ERROR_CONT

@CHKCMT:    lda     LINE_BUF,Y
            beq     PASS2_ERROR_CONT
            cmp     COMMENT
            beq     PASS2_ERROR_CONT
            jsr     LOOKUP_OPCODE
            bpl     @CHKMAC
            jsr     HANDLE_DIRECTIVE
            jmp     PASS2_ERROR_CONT

@CHKMAC:    bvc     @ASSEMBLE
            jsr     DISPATCH_MACRO
            jmp     PASS2_ERROR_CONT

@ASSEMBLE:  jsr     ASSEMBLE_INSTR  ; Assemble and emit instruction bytes
            jmp     PASS2_ERROR_CONT

; ============================================================================
; PASS2_END_FLUSH
;
; Pass 2 end-of-assembly sequence.  Jumped to from PASS2_LINE_LOOP when
; END_SEEN is set, and from PASS2_FETCH_LINE when the .READ include stack
; is found empty (outermost source file exhausted).
;
; ============================================================================
PASS2_END_FLUSH:
            lda     OBJBUF_POS      ; Bytes left in output buffer?
            beq     @RETURN         ; Buffer already empty, return
            jsr     FLUSH_OBJECT    ; Flush object output buffer

@RETURN:    rts

; ============================================================================
; INIT_LINE_WS
; Clears and resets all per-line assembly state.  Called at the start of
; every source line in both passes and also once at the beginning of each pass.
;
; Actions:
;  - Resets digit fields: ERR_COUNT, ADDRESS_FIELD, PREV_LOCAL, NEXT_LOCAL to '0'.
;  - Fills the listing line buffer (LISTING_BUF, 46 bytes) with spaces.
;    BUG-1: The store uses "sta LISTING_BUF" (no index X) instead of "sta LISTING_BUF,X",
;    so only LISTING_BUF[0] is cleared per iteration; the rest of the buffer
;    is NOT zeroed.  (See BUG-1 in the file header.)
;  - Clears many per-line flag bytes to 0.
;  - Sets LINE_LIST_FLAGS = $80 (default: listing enabled).
; ============================================================================
INIT_LINE_WS:
            cld
            lda     #'0'
            ldx     #$03
@RESET1:    sta     ERR_COUNT,X     ; Reset error-count digits to '0'
            dex
            bpl     @RESET1
            ldx     #$03
@RESET2:    sta     ADDRESS_FIELD,X ; Reset address digits to '0'
            sta     PREV_LOCAL,X    ; Reset PREV_LOCAL digits
            sta     NEXT_LOCAL,X    ; Reset NEXT_LOCAL digits
            dex
            bpl     @RESET2
            ldx     #$2d            ; 46 iterations ($2D+1)
            lda     #' '
; BUG-1: Should be "sta LISTING_BUF,X" to sweep the whole buffer.
;        As written, every iteration stores to LISTING_BUF[0].
;        The listing line header is NOT cleared – the previous line's
;        title string / form-feed byte is left in place.
@RESET3:    sta     LISTING_BUF     ; *** BUG-1: missing ",X" index register ***
            lda     #' '
            dex
            bpl     @RESET3
            sta     PAGE_NUMBER     ; Clear page-number chars
            sta     PAGE_NUMBER+1
            sta     PAGE_NUMBER+2
            sta     PREV_LOCAL+3    ; Clear PREV_LOCAL[3]
            sta     NEXT_LOCAL+3    ; Clear NEXT_LOCAL[3]

            ; Reset macro argument stack pointer to base of MACSTACK
            lda     MACSST          ; MACSTACK start low
            sta     MARGSTRP        ; Macro read pointer = base
            sta     MARGWRTP        ; Macro write pointer = base
            sta     MACSPTR         ; MACSTACK current pos = base
            lda     MACSST+1
            sta     MARGSTRP+1
            sta     MARGWRTP+1
            sta     MACSPTR+1

            ; Clear all per-line flags
            lda     #$00
            sta     VALUE_OUTPUT    ; VALUE_OUTPUT = 0
            sta     OVL_NUM         ; OVL_NUM = 0
            sta     MACRO_EXPANDING ; MACRO_EXPANDING = 0
            sta     BANK_NUM        ; BANK_NUM = 0
            sta     INCL_STACK_TOP  ; INCL_STACK_TOP = 0
            sta     COND_DEPTH      ; COND_DEPTH = 0
            sta     COND_ELSE       ; COND_ELSE = 0
            sta     LOCCNT          ; Location counter = 0
            sta     LOCCNT+1
            sta     ERR_TYPE_CODE   ; ERR_TYPE_CODE = 0
            sta     LINE_NUM        ; LINE_NUM lo = 0
            sta     LINE_NUM+1      ; LINE_NUM hi = 0
            sta     INCLUDE_POP     ; INCLUDE_POP = 0
            sta     END_SEEN        ; END_SEEN = 0
            sta     ERR_CNT         ; ERR_CNT LO = 0
            sta     ERR_CNT+1       ; ERR_CNT HI = 0
            sta     LIST_SUPPRESS2  ; LIST_SUPPRESS2 = 0
            sta     VALUE_SUPPRESS  ; VALUE_SUPPRESS = 0
            nop
            nop
            nop
            lda     #$80
            sta     LINE_LIST_FLAGS ; Listing on by default
            nop
            nop
            nop
            rts

; ============================================================================
; DISPATCH_MACRO
; Dispatches a macro invocation that was matched by LOOKUP_OPCODE with the
; V flag set.  Reads the macro body pointer from the symbol-table entry at
; (MATCHP,Y) and jumps indirectly through MACRO_BODY_VEC.
; ============================================================================
DISPATCH_MACRO:
            ldy     SYM_MATCH_OFF   ; Offset of match in entry
            iny
            lda     (MATCHP),Y      ; Read macro body pointer
            sta     MACRO_BODY_VEC 
            iny
            lda     (MATCHP),Y
            sta     MACRO_BODY_VEC+1
            ldy     LINE_POS        ; Restore line position
            jsr     SKIP_LEADING_SPACES ; Skip spaces
            jmp     (MACRO_BODY_VEC) ; Dispatch to macro body reader

; ============================================================================
; HANDLE_DIRECTIVE
; Called when LOOKUP_OPCODE returns with N flag set (pseudo-op / directive)
; or when a macro invocation is to be expanded.  Checks MACRO_FLAGS
; to determine the directive type:
;
;   Bit 6 (V) of MACRO_FLAGS set -> recursive macro call (finish current expansion)
;   Bit 5 (MACRO_FLAGS & $20) set -> alternate addressing
;   Otherwise: push a new expansion frame and process the directive operand.
; ============================================================================
HANDLE_DIRECTIVE:
            bit     MACRO_FLAGS     ; MACRO_FLAGS: test V bit (recursive call)
            bvs     @ENDEXP         ; Bit 6 set -> end current expansion
            ldx     #$0b            ; Default: ERR_TYPE_CODE target = 11
            lda     MACRO_FLAGS
            and     #$20            ; Test bit 5 (alternate mode)
            beq     @CONT
            inx                     ; Alternate: target = 12
            ldy     LINE_POS
            jsr     SKIP_LEADING_SPACES ; Skip spaces to operand
            beq     @ERROR          ; No operand -> error
            jsr     EVAL_FULL_EXPR  ; Evaluate expression
@CONT:      ldy     SYM_MATCH_OFF
            iny
            iny
            iny
            jmp     @SIZE           ; Jump to size-table dispatch

; -- Recursive/tail case --
@ENDEXP:    ldy     LINE_POS
            jsr     SKIP_LEADING_SPACES ; Skip spaces
            bne     @CONT2
@ERROR:     jsr     ERROR_SYNTAX
            ; Not reached

@CONT2:     jsr     PARSE_ADDRMODE  ; Parse addressing-mode prefix
            sty     LINE_POS

@LOOP:      txa
            clc
            adc     SYM_MATCH_OFF   ; Compute entry offset: SYM_MATCH_OFF + X
            tay
            iny
            iny
            iny
            lda     (MATCHP),Y      ; Read size table entry
            cmp     #$ff            ; $FF = no more entries
            bne     @SIZE
            cpx     #$02
            bcc     @ERROR2         ; X < 2: range error
            cpx     #$05
            bcs     @ERROR2         ; X >= 5: range error
            txa
            clc
            adc     #$05            ; Advance by 5 positions
            sta     ADDR_MODE       ; ADDR_MODE = new index
            tax
            bne     @LOOP           ; Try again with new index

@SIZE:      lda     INST_SIZE,X     ; Size increment from INST_SIZE table
            clc
            adc     LOCCNT          ; Advance location counter by instruction size
            sta     LOCCNT
            bcc     @SKIP
            inc     LOCCNT+1
@SKIP:      bit     PASSFLG         ; Pass 2?
            bmi     @ASSEMBLE       ; Yes: assemble and emit
            rts                     ; Pass 1: just advance LOCCNT, done

@ERROR2:    jsr     ERROR_ADDRESSING ; Addressing range error
            ; Not reached

@ASSEMBLE:  lda     (MATCHP),Y      ; Re-read symbol table entry byte
            cpx     #$07
            bcs     @ALT_EMIT       ; X >= 7: different dispatch path
            cpx     #$00
            beq     @EMIT_ABS       ; X = 0: emit absolute address
            jsr     EMIT_ONE_BYTE_OPERAND ; Emit single-byte value
            bit     URESFLG         ; Unresolved?
            bmi     @ERROR3         ; Yes: error
            rts

@EMIT_ABS:  jmp     EMIT_OPCODE_AND_SETUP_LISTING ; Emit 16-bit absolute address

@ALT_EMIT:  cpx     #$0b
            bcs     @CHK_ABS
            jsr     EMIT_TWO_BYTE_OPERAND ; Emit 16-bit value (two bytes)
            ldx     ADDR_MODE
            cpx     #$0a
            bne     @CHKRES
            ldx     SYMVAL
            inx
            bne     @CHKRES
            ldy     LINE_POS_SAVE   ; Saved line position
            jsr     ERROR_RANGE     ; ERR_TYPE_LADDER (branch range error)
            ; Not reached

; BUG-3: @UNREACH is dead code (unreachable).
; The preceding "jsr ERROR_RANGE" never returns.  Possibly, the "bcs @CHK_ABS"
; at @ALT_EMIT should instead be "bcs @UNREACH".
@UNREACH:   bit     URESFLG         ; (Dead code) test unresolved flag
            bmi     @ERROR3         ; (Dead code) -> error

@CHK_ABS:   beq     @EMIT_ABS        ; X = 0 -> emit absolute
            bit     URESFLG
            bpl     @EMIT_REL
            lda     LOCCNT          ; Unresolved: use current PC as value
            sta     SYMVAL
            lda     LOCCNT+1
            sta     SYMVAL+1
@EMIT_REL:  jsr     EMIT_BRANCH_INSTR ; Emit relative byte (branch offset)

@CHKRES:    bit     URESFLG
            bmi     @ERROR3           ; Unresolved -> error
            rts

@ERROR3:    ldy     UNRES_LINE_POS  ; Position of unresolved reference
            jsr     ERROR_UNDEFINED ; Undefined reference
            ; Not reached

; ============================================================================
; PROCESS_EQU_DIR
; Handles the "=" (EQU) label assignment.
;   Single "=": evaluates the right-hand expression and stores the result in
;     the current symbol-table entry; outputs value digits to listing.
;   Double "==": two-pass equate; value must be known in pass 1.
; ============================================================================
PROCESS_EQU_DIR:
            iny                     ; Skip '='
            lda     LINE_BUF,Y
            cmp     #'='            ; Double '==' ?
            beq     @DOUBLE_EQ
            jsr     EVAL_FULL_EXPR  ; Evaluate RHS expression
            bpl     @KNOWN          ; N clear: value known
            bit     PASSFLG         ; Pass 2?
            bmi     @ERROR2         ; Pass 2 with unresolved RHS -> error
@KNOWN:     jsr     PEEK_BLANK_CHAR ; Check if end-of-name char (space/NUL/comment)
            bne     @ERROR1         ; Unexpected char after expression -> error
            jsr     EMIT_INSTR_REC  ; Write instruction record
            lda     ENTRY_TYPE_FLAGS
            and     #$20            ; External symbol?
            bne     @EXTERNAL
            jsr     OUTPUT_VAL_DIGITS ; Format value in listing
            lda     #'='
            sta     EQUATE_CHAR     ; Show '=' in listing addressing field
            rts

            ; External symbol: show dashes instead of value
@EXTERNAL:  ldx     #$03
            lda     #'-'
@FILL:      sta     VALUE_FIELD,X   ; Fill value field with '----'
            dex
            bpl     @FILL
            lda     #'='
            sta     EQUATE_CHAR
            rts

@ERROR1:      jsr     ERROR_SYNTAX
            ; Not reached

            ; Double '==' equate
@DOUBLE_EQ: bit     PASSFLG         ; Pass 2?
            bmi     @DONE           ; Pass 2: skip (== defined in pass 1)
            jsr     EVAL_EXPR_ADVANCE ; Advance past second '='
            bmi     @ERROR2         ; Evaluate: unresolved -> error
            jsr     PEEK_BLANK_CHAR ; Check if end-of-name char (space/NUL/comment)
            bne     @ERROR1
            jsr     EMIT_INSTR_REC2 ; Write instruction record (== form)
            jsr     OUTPUT_VAL_DIGITS
            lda     #'='
            sta     EQUATE_CHAR
            sta     SECOND_EQUATE   ; Show '==' in listing fields
@DONE:      rts

@ERROR2:    jsr     ERROR_UNDEFINED
            ; Not reached

; ============================================================================
; PROCESS_ORG_DIR
; Handles the "*=" (ORG / origin) assignment directive.
; Evaluates the address expression and sets LOCCNT/LOCCNT+1.
; Also calls ADVANCE_LOCCNT to close any pending segment header.
; ============================================================================
PROCESS_ORG_DIR:
            iny
            lda     LINE_BUF,Y
            cmp     #'='            ; Must have '=' after '*'
            bne     ERR_SYNTAX      ; No '=' -> syntax error
            jsr     EVAL_EXPR_ADVANCE ; Evaluate ORG expression
            bmi     ERR_UNREF       ; Unresolved -> error
            jsr     PEEK_BLANK_CHAR ; Check if end-of-name char (space/NUL/comment)
            bne     ERR_SYNTAX      ; Unexpected chars -> error
            jsr     ADVANCE_LOCCNT  ; Close pending segment
            bit     PASSFLG
            bmi     @RETURN         ; Pass 2: don't re-evaluate label
            jsr     ASSEMBLE_ZP_ABS ; Set LOCCNT from SYMVAL
@RETURN:    rts

ERR_UNREF:  jsr     ERROR_UNDEFINED
            ; Not reached
ERR_SYNTAX: jsr     ERROR_SYNTAX
            ; Not reached

; ============================================================================
; HANDLE_OVL
; Handles the ".OVL" (overlay) directive.
; Evaluates the overlay number expression and stores it in OVL_NUM.
; Overlay numbers must fit in 8 bits (SYMVAL+1 and SYMVAL+2 must be zero).
; ============================================================================
HANDLE_OVL: jsr     SKIP_LEADING_SPACES ; Skip spaces
            beq     ERR_SYNTAX      ; No operand -> syntax error
            jsr     EVAL_FULL_EXPR  ; Evaluate overlay number expression
            bmi     ERR_UNREF       ; Unresolved -> error
            jsr     PEEK_BLANK_CHAR ; Check if end-of-name char (space/NUL/comment)
            bne     ERR_SYNTAX
            lda     SYMVAL+1        ; High bytes must be zero (8-bit value)
            ora     SYMVAL+2
            bne     ERR_RANGE       ; > 255 -> range error
            lda     SYMVAL
            sta     OVL_NUM         ; Evaluated overlay number
            rts

ERR_RANGE:  jsr     ERROR_RANGE           ; ERR_TYPE_LADDER (range error)
            ; Not reached

; ============================================================================
; HANDLE_BANK
; Handles the ".BANK" directive.
; Evaluates the bank number and stores it in BANK_NUM.
; Bank numbers must be 0–3 (< 4).
; ============================================================================
HANDLE_BANK:
            jsr     SKIP_LEADING_SPACES
            beq     ERR_SYNTAX
            jsr     EVAL_FULL_EXPR
            bmi     ERR_UNREF
            jsr     PEEK_BLANK_CHAR ; Check if end-of-name char (space/NUL/comment)
            bne     ERR_SYNTAX
            lda     SYMVAL+1
            ora     SYMVAL+2
            bne     ERR_RANGE           ; > 255 -> range error
            lda     SYMVAL
            cmp     #$04            ; Bank must be 0–3
            bcs     ERR_RANGE       ; >= 4 -> range error
            sta     BANK_NUM        ; Evaluated bank number
            rts

; ============================================================================
; ADVANCE_LOCCNT
; Writes an object-code segment header when the location counter has advanced
; from its last checkpoint (LOCCNT_BASE).
;
; A segment header is 4 bytes at (SEGHEADP):
;   word   SYMVALP           next free heap address (new segment start ptr)
;   word   (LOCCNT – BASE)   segment byte count (SEG_SIZE)
;
; After writing the header, four zero bytes of padding are appended via
; WRITE_TO_PRIMARY_HEAP (one call per byte via WRITE_PADDING), and LOCCNT_BASE is
; updated.
;
; Called by HANDLE_OVL, PROCESS_ORG_DIR, HANDLE_ENTRY, end-of-pass, etc.
; ============================================================================
ADVANCE_LOCCNT:
            lda     LOCCNT
            sec
            sbc     LOCCNT_BASE     ; SEG_SIZE = LOCCNT − LOCCNT_BASE
            sta     SEG_SIZE
            lda     LOCCNT+1
            sbc     LOCCNT_BASE+1
            sta     SEG_SIZE+1
            ora     SEG_SIZE
            beq     UPDATE_LOCCNT   ; Segment size = 0: nothing to write

            lda     #$00
            sta     OBJSEG_OPEN     ; OBJSEG_OPEN = 0 (close any pending segment)
            bit     PASSFLG
            bmi     UPDATE_LOCCNT   ; Pass 2: skip header write (already done in pass 1)

            ; Write 4-byte segment header into heap at (SEGHEADP)
            ldy     #$00
            lda     SYMVALP         ; Segment start pointer, low
            sta     (SEGHEADP),Y
            iny
            lda     SYMVALP+1       ; Segment start pointer, high
            sta     (SEGHEADP),Y
            iny
            lda     SEG_SIZE
            sta     (SEGHEADP),Y
            iny
            lda     SEG_SIZE+1
            sta     (SEGHEADP),Y

            ; Advance SEGHEADP to next header slot
            lda     SYMVALP
            sta     SEGHEADP
            lda     SYMVALP+1
            sta     SEGHEADP+1

; Write four padding zero bytes to heap
;
WRITE_PADDING:
            lda     #$00
            jsr     WRITE_TO_PRIMARY_HEAP
            jsr     WRITE_TO_PRIMARY_HEAP
            jsr     WRITE_TO_PRIMARY_HEAP
            jsr     WRITE_TO_PRIMARY_HEAP

; Update location counter and checkpoint from SYMVAL (ORG value)
;
UPDATE_LOCCNT:
            lda     SYMVAL
            sta     LOCCNT          ; LOCCNT = new ORG value
            sta     LOCCNT_BASE     ; LOCCNT_BASE = LOCCNT
            lda     SYMVAL+1
            sta     LOCCNT+1
            sta     LOCCNT_BASE+1
            rts

; ============================================================================
; HANDLE_ENTRY
; Handles the ".ENTRY" directive.
; Records the current location counter as the program entry point address in
; ENTRY_ADDR.  The entry-point address is written to the object
; file header once (on first .ENTRY; duplicate .ENTRY -> error).
; ============================================================================
HANDLE_ENTRY:
            bit     PASSFLG
            bmi     @SKIP           ; Pass 2: already recorded -> skip
            bit     ENTRY_DEFINED   ; already seen a .ENTRY?
            bmi     @ERROR          ; Yes -> error (duplicate .ENTRY)
            sec
            ror     ENTRY_DEFINED   ; Mark as seen
            lda     LOCCNT
            sta     ENTRY_ADDR      ; ENTRY_ADDR = current LOCCNT
            lda     LOCCNT+1
            sta     ENTRY_ADDR+1
@SKIP:      rts
@ERROR:     jsr     ERROR_DUPLICATE           ; ERR_TYPE_LADDER (duplicate symbol)
            ; Not reached

; ============================================================================
; HANDLE_DATE
; Handles the ".DATE" directive.
; Emits 9 bytes from CURDATE (the date string fetched at startup) into the
; object stream.  The location counter is advanced by 9.
; In pass 1 only: increments LOCCNT by 9 without emitting.
; ============================================================================
HANDLE_DATE:
            ldx     #$0b            ; Size index (unused after this?)
            bit     PASSFLG
            lda     LOCCNT          ; Advance LOCCNT by 9 (date string length)
            clc
            adc     #$09
            sta     LOCCNT
            lda     LOCCNT+1
            adc     #$00
            sta     LOCCNT+1
            bit     PASSFLG
            bpl     @DONE           ; Pass 1: just advance, done
            ldy     #$00
@LOOP:      lda     CURDATE,Y       ; Read one byte of the date string
            jsr     EMIT_DATA_BYTE_WITH_LISTING ; Emit to object stream
            iny
            cpy     #$09            ; 9 bytes emitted?
            bne     @LOOP
@DONE:      rts

; ============================================================================
; HANDLE_FILL
; Handles the ".FILL count, value" directive.
; Emits 'count' copies of 'value' (a .BYTE-type expression) into the object
; stream.  Uses the negated count (FILL_COUNT_NEG) and counts up to zero.
; ============================================================================
HANDLE_FILL:
            ldx     #$00
            stx     UNRES_POS       ; UNRES_POS = 0
            jsr     EVAL_FULL_EXPR  ; Evaluate count expression
            bit     URESFLG
            bpl     @STOR
            jsr     ERROR_UNDEFINED ; Unresolved count -> error
            ; Not reached

@STOR:      ; Store negated count in FILL_COUNT_NEG (counts up from neg to zero)
            lda     #$00
            sec
            sbc     SYMVAL          ; -count low
            sta     FILL_COUNT_NEG
            lda     #$00
            sbc     SYMVAL+1        ; -count high
            sta     FILL_COUNT_NEG+1
            lda     SYMVAL+2
            beq     @CONT
            jsr     ERROR_ARITHMETIC ; Count too large (> 16-bit) -> range error
@CONT:      ora     SYMVAL+1
            ora     SYMVAL
            beq     @DONE           ; Count = 0: nothing to fill
            jsr     SKIP_LEADING_SPACES ; Skip to ','
            cmp     #','
            beq     @ADVANCE
            jsr     ERROR_SYNTAX    ; No comma -> syntax error
            ; Not reached

@ADVANCE:   jsr     ADVANCE_SKIP_LEADING_SPACES ; Skip ','
            sty     FILL_LINE_POS   ; Start of value expression

            ; Emit 'count' bytes in a loop (counting up from -count to 0)
@LOOP:      ldy     FILL_LINE_POS   ; Restore Y to value expression
            jsr     EMIT_BYTE_ENTRY ; Emit one .BYTE value
            inc     FILL_COUNT_NEG  ; FILL_COUNT_NEG++
            bne     @CHKLST
            inc     FILL_COUNT_NEG+1
            beq     @DONE           ; Reached 0: done
@CHKLST:    bit     LISTING_ON      ; LISTING_ON?
            bpl     @LOOP           ; Not listing: tight loop
            jsr     EMIT_LISTING_LINE_CONTINUATION ; Flush listing line
            jmp     @LOOP

@DONE:      rts

; ============================================================================
; EMIT_BYTE_ENTRY
; Emits one or more bytes from a comma-separated list of expressions.
; Handles:
;   - Numeric expressions: evaluated and emitted via EMIT_DATA_BYTE_WITH_LISTING
;   - Quoted strings: each character emitted individually
;   - Multiple comma-separated values on one line
; UNRES_POS is set to the line position if an unresolved value is seen.
; ============================================================================
EMIT_BYTE_ENTRY:
            ldx     #$00
            stx     UNRES_POS       ; UNRES_POS = 0

@LOOP:      ldx     #$0b            ; Default error type = 11
            jsr     SKIP_LEADING_SPACES ; Skip spaces
            cmp     APOSTROPHE      ; Quote char?
            beq     @STRING         ; Yes: string literal

@LOOP2:     jsr     EVAL_FULL_EXPR  ; Evaluate numeric expression
            bit     PASSFLG
            bpl     @ADVANCE        ; Pass 1: just advance LOCCNT
            lda     SYMVAL
            jsr     EMIT_DATA_BYTE_WITH_LISTING ; Emit low byte of value
            bit     URESFLG
            bpl     @ADVANCE
            lda     UNRES_POS       ; Was UNRES_POS already set?
            bne     @ADVANCE
            lda     UNRES_LINE_POS  ; Position of unresolved reference
            sta     UNRES_POS       ; UNRES_POS = position for error reporting

@ADVANCE:   inc     LOCCNT          ; Advance location counter by 1
            bne     @CHKSEP
            inc     LOCCNT+1

@CHKSEP:    lda     LINE_BUF,Y      ; Check separator after value
            beq     @DONE           ; EOL -> done
            cmp     COMMENT         ; Comment?
            beq     @DONE
            cmp     #' '
            beq     @DONE
            cmp     #','            ; Comma -> next value in list
            bne     @ERROR          ; Unexpected char -> error
            jsr     EMIT_LISTING_LINE_CONTINUATION ; Flush/update listing
            iny                     ; Skip ','
            jmp     @LOOP           ; Process next value

            ; -- String literal handling --
@STRING:    iny                     ; Skip opening quote
            lda     LINE_BUF,Y
            bne     @NEXTC
            dey                     ; End of line inside string
            jmp     @LOOP2

@NEXTC:     iny
            lda     LINE_BUF,Y
            beq     @CLOSE          ; EOL inside string
            cmp     APOSTROPHE      ; Closing quote?
            bne     @NOCLOSE

@CLOSE:     dey                     ; Back up before closing quote / EOL
            dey
            jmp     @LOOP2          ; Re-evaluate (handles empty string)

@NOCLOSE:   dey
@SCAN:      lda     LINE_BUF,Y      ; Scan string char by char
            beq     @ERROR          ; EOL without closing quote -> error
            iny
            cmp     APOSTROPHE      ; Quote?
            beq     @CHKSEP         ; Yes: end of string -> check separator
            inc     LOCCNT          ; Advance LOCCNT for each char
            bne     @SKIP
            inc     LOCCNT+1
@SKIP:      bit     PASSFLG
            bpl     @SCAN           ; Pass 1: advance only
            jsr     EMIT_DATA_BYTE_WITH_LISTING ; Emit character byte
            jmp     @SCAN

@DONE:      bit     PASSFLG
            bpl     @RETURN
            ldy     UNRES_POS       ; Any unresolved byte?
            beq     @RETURN
            jsr     ERROR_UNDEFINED ; Yes -> undefined error
            ; Not reached

@RETURN:    rts
@ERROR:     jsr     ERROR_SYNTAX
            ; Not reached

            .byte   $86             ; Dead code / safety padding

; ============================================================================
; HANDLE_WORD
; Handles the ".WORD" directive.
; Sets endianness to little-endian (LSB first) and falls through to the common
; WORD/DBYTE handler (WORD_DBYTE_COMMON).
; ============================================================================
HANDLE_WORD:
            lsr     ENDIANNESS      ; Clear ENDIANNESS  (0 -> little-endian)
            ; Fallthrough to common handler

; ============================================================================
; WORD_DBYTE_COMMON
; Common handler for .WORD (little-endian) and .DBYTE (big-endian).
; Processes a comma-separated list of 16-bit values, emitting each as two
; bytes in the order determined by BENDIANFLG:
;   BENDIANFLG = 0:   lo, hi  (little-endian, for .WORD)
;   BENDIANFLG = $80: hi, lo  (big-endian,    for .DBYTE)
; The location counter is advanced by 2 per value.
; ============================================================================
WORD_DBYTE_COMMON:
            ldx     #$00
            stx     UNRES_POS       ; UNRES_POS = 0

@NEXT:      ldx     #$0b            ; Default error type = 11
            jsr     EVAL_FULL_EXPR  ; Evaluate 16-bit expression
            bit     PASSFLG
            bpl     @ADVANCE        ; Pass 1: advance LOCCNT only
            bit     ENDIANNESS      ; Check endianness flag
            bmi     @EMIT_BE        ; $80 = big-endian -> jump to BE emit
            ; Little-endian: emit lo then hi
            lda     SYMVAL
            jsr     EMIT_DATA_BYTE_WITH_LISTING ; Emit low byte
            lda     SYMVAL+1
            jsr     EMIT_DATA_BYTE_WITH_LISTING ; Emit high byte

@COMMON:    bit     URESFLG         ; Unresolved?
            bpl     @ADVANCE
            lda     UNRES_POS
            bne     @ADVANCE
            lda     UNRES_LINE_POS  ; Position of unresolved reference
            sta     UNRES_POS

@ADVANCE:   lda     LOCCNT          ; Advance LOCCNT by 2
            clc
            adc     #$02
            sta     LOCCNT
            bcc     @CHKSEP
            inc     LOCCNT+1

@CHKSEP:    lda     LINE_BUF,Y      ; Check separator
            beq     @IS_SEP
            cmp     COMMENT
            beq     @IS_SEP
            cmp     #' '
            beq     @IS_SEP
            cmp     #','
            bne     @ERROR          ; Unexpected char -> error
            jsr     EMIT_LISTING_LINE_CONTINUATION
            iny
            jmp     @NEXT

@IS_SEP:    bit     PASSFLG
            bpl     @DONE
            ldy     UNRES_POS
            beq     @DONE
            jsr     ERROR_UNDEFINED ; Unresolved -> error
            ; Not reached

@DONE:      rts

@EMIT_BE:   ; Big-endian: emit hi then lo (for .DBYTE)
            lda     SYMVAL+1
            jsr     EMIT_DATA_BYTE_WITH_LISTING ; Emit high byte first
            lda     SYMVAL
            jsr     EMIT_DATA_BYTE_WITH_LISTING ; Emit low byte
            jmp     @COMMON

@ERROR:     jsr     ERROR_SYNTAX
            ; Not reached

; ============================================================================
; HANDLE_DBYTE
; Handles the ".DBYTE" directive.
; Sets endianness to big-endian (MSB first) and falls through to WORD_DBYTE_COMMON.
; ============================================================================
HANDLE_DBYTE:
            sec
            ror     ENDIANNESS      ; Set ENDIANNESS flag -> big-endian
            jmp     WORD_DBYTE_COMMON

; ============================================================================
; HANDLE_END
; Handles the ".END" directive.  Sets END_SEEN or INCLUDE_POP
; depending on whether we are inside an include file.
; ============================================================================
HANDLE_END: sec
            lda     INCL_STACK_TOP  ; Inside .READ?
            bne     H_POP_INC       ; Yes: pop include
            ror     END_SEEN        ; No, end detecxted
RETURN:     rts                     ; <- Handler for ".OPT" points here
H_POP_INC:  ror     INCLUDE_POP     ; Set INCLUDE_POP
            rts

; ============================================================================
; HANDLE_PAGE
; Handles the ".PAGE ['title']" directive.
; In pass 2 only: clears the listing line template, optionally copies a
; title string from the source line, and calls OUTPUT_LINE_PG_BRK
; to force a new page in the listing.
; ============================================================================
HANDLE_PAGE:
            bit     PASSFLG
            bmi     @CONT           ; Pass 2: process
            rts                     ; Pass 1: ignore

@CONT:      jsr     SKIP_LEADING_SPACES ; Skip spaces
            beq     @BREAK          ; No title: just break page
            cmp     APOSTROPHE      ; Quote?
            bne     @ERROR          ; Not a quote -> syntax error
            ; Clear listing line title area
            ldx     #$2d
            lda     #' '
@CLEAR:     sta     LISTING_BUF,X   ; Clear title field
            dex
            bpl     @CLEAR
            ldx     #$00
            ; Copy title string from source line to listing template
@COPY:      iny
            lda     LINE_BUF,Y
            beq     @BREAK          ; EOL -> done
            cmp     APOSTROPHE      ; Closing quote -> done
            beq     @BREAK
            sta     LISTING_BUF,X   ; Store title char
            inx
            cpx     #$2d            ; Max 45 title chars
            bne     @COPY

@BREAK:     jmp     OUTPUT_LINE_PG_BRK ; Force page break in listing

@ERROR:     jsr     ERROR_SYNTAX    ; Syntax error (non-quote after .PAGE)
            ; Not reached

; ============================================================================
; HANDLE_IFLT / HANDLE_IFLE / HANDLE_IFEQ / HANDLE_IFNE / HANDLE_IFGE / HANDLE_IFGT
; Conditional assembly directives: compare two expressions.
;
; All handlers call COMPARE_IFXX which evaluates both operands and
; sets IFCMP_ZERO and IFCMP_NEG:
;   IFCMP_ZERO = $FF when operand1 == operand2
;   IFCMP_NEG  = $FF when operand1 < operand2
;
; Each handler then tests these flags and, if the condition is FALSE,
; calls COND_ASSM_SKIP to scan forward to .ELSE or .ENDIF.
; ============================================================================

; .IFLT: skip if NOT (operand1 < operand2)
HANDLE_IFLT:                        ; (original: HANDLE_IFLT)
            jsr     COMPARE_IFXX    ; Evaluate and compare
            lda     IFCMP_ZERO      ; Both operators were equal
            ora     IFCMP_NEG       ; Operand 1 < operand2
            bpl     IFLT_DONE       ; Neither zero nor negative -> skip block
H_CND_SKIP: jmp     COND_ASSM_SKIP  ; Condition false -> skip
IFLT_DONE:  rts

; .IFLE: skip if NOT (operand1 <= operand2)
HANDLE_IFLE:                        ; (original: HANDLE_IFLE)
            jsr     COMPARE_IFXX
            bit     IFCMP_NEG       ; operand1 < operand2
            bmi     H_CND_SKIP      ; Negative -> condition true, skip block
            rts

; .IFEQ: skip if NOT (operand1 == operand2)
HANDLE_IFEQ:                        ; (original: HANDLE_IFEQ)
            jsr     COMPARE_IFXX
            bit     IFCMP_ZERO      ; Both operands equal?
            bpl     H_CND_SKIP
            rts

; .IFNE: skip if NOT (operand1 != operand2)
HANDLE_IFNE:                        ; (original: HANDLE_IFNE)
            jsr     COMPARE_IFXX
            bit     IFCMP_ZERO
            bmi     H_CND_SKIP
            rts

; .IFGE: skip if NOT (operand1 >= operand2)
HANDLE_IFGE:                        ; (original: HANDLE_IFGE)
            jsr     COMPARE_IFXX
            lda     IFCMP_ZERO
            ora     IFCMP_NEG
            bpl     H_CND_SKIP
            rts

; .IFGT: skip if NOT (operand1 > operand2)
HANDLE_IFGT:                        ; (original: HANDLE_IFGT)
            jsr     COMPARE_IFXX
            bit     IFCMP_NEG
            bpl     H_CND_SKIP
            rts

; ============================================================================
; HANDLE_IFNULL
; Handles ".IFNULL" – skip the block if the immediately following token is
; non-empty (i.e. the "null" condition is: no operand present).
; ============================================================================
HANDLE_IFNULL:                      ; (original: HANDLE_IFNULL)
            sec
            ror     COND_DEPTH      ; Mark .IF nesting: COND_DEPTH bit 7 set
            lsr     COND_ELSE       ; Clear COND_ELSE bit for this level
            jsr     SKIP_LEADING_SPACES ; Skip spaces
            beq     @DONE           ; EOL: operand is null -> condition true
            jmp     COND_ASSM_SKIP  ; Non-empty: condition false -> skip
@DONE:      rts

; ============================================================================
; COMPARE_IFXX
; Common setup and comparison for all .IFxx directives.
; Sets COND_DEPTH/COND_ELSE flags, evaluates the first expression into a
; temporary (IFCMP_VAL), then evaluates the optional second expression
; (SYMVAL).  Computes the 24-bit signed comparison result in IFCMP_ZERO
; and IFCMP_NEG.
; ============================================================================
COMPARE_IFXX:
            sec
            ror     COND_DEPTH      ; COND_DEPTH: mark new .IF level
            lsr     COND_ELSE       ; COND_ELSE: clear for this level
            jsr     SKIP_LEADING_SPACES ; Skip spaces
            jsr     EVAL_FULL_EXPR  ; Evaluate first operand
            bpl     @CONT           ; N clear (resolved)
@ERROR:     jsr     ERROR_UNDEFINED ; Unresolved -> error
            ; Not reached

@CONT:      ; Save first operand in IFCMP_VAL
            lda     SYMVAL
            sta     IFCMP_VAL
            lda     SYMVAL+1
            sta     IFCMP_VAL+1
            lda     SYMVAL+2
            sta     IFCMP_VAL+2
            ; Clear second operand (default = 0 for single-operand forms)
            lda     #$00
            sta     SYMVAL
            sta     SYMVAL+1
            sta     SYMVAL+2
            jsr     SKIP_LEADING_SPACES ; Check for optional second operand
            beq     @COMPARE        ; EOL: compare against 0
            cmp     #','            ; Comma separator?
            beq     @IS_COMMA
            jsr     ERROR_SYNTAX    ; Not comma -> syntax error
            ; Not reached

@IS_COMMA:  jsr     ADVANCE_SKIP_LEADING_SPACES ; Skip ','
            jsr     EVAL_FULL_EXPR  ; Evaluate second operand
            bmi     @ERROR          ; Unresolved -> error

            ; Perform 24-bit signed comparison: IFCMP_VAL − SYMVAL
@COMPARE:   lda     #$00
            sta     IFCMP_ZERO      ; Clear IFCMP_ZERO
            sta     IFCMP_NEG       ; Clear IFCMP_NEG
            lda     IFCMP_VAL
            cmp     SYMVAL
            bne     @LOWB           ; Low bytes differ: compute sign
            lda     IFCMP_VAL+1
            sbc     SYMVAL+1
            bne     @MIDB           ; Mid bytes differ
            lda     IFCMP_VAL+2
            sbc     SYMVAL+2
            bne     @HIGHB          ; High bytes differ
            dec     IFCMP_ZERO      ; All equal: IFCMP_ZERO = $FF
            rts

@LOWB:      lda     IFCMP_VAL+1
            sbc     SYMVAL+1
@MIDB:      lda     IFCMP_VAL+2
            sbc     SYMVAL+2
@HIGHB:     bmi    @DONE           ; operand2 < operand1
            dec     IFCMP_NEG       ; operand1 < operand2: IFCMP_NEG = $FF
@DONE:      rts

; ============================================================================
; HANDLE_IF
; Handles the ".IF expression" directive.
; Evaluates the expression; if it is non-zero the block is assembled,
; otherwise COND_ASSM_SKIP is called to skip to .ELSE or .ENDIF.
; ============================================================================
HANDLE_IF:  jsr     SKIP_LEADING_SPACES ; Skip to expression
            jsr     EVAL_FULL_EXPR  ; Evaluate
            bpl     @CONT
            jsr     ERROR_UNDEFINED ; Unresolved -> error
            ; Not reached

@CONT:      sec
            ror     COND_DEPTH      ; COND_DEPTH: mark .IF level
            lsr     COND_ELSE       ; COND_ELSE = 0 for this level
            lda     SYMVAL
            ora     SYMVAL+1
            ora     SYMVAL+2
            beq     COND_ASSM_SKIP  ; Expression = 0: skip block
            rts                     ; Expression != 0: assemble block

; ============================================================================
; COND_ASSM_SKIP
; Implements the "condition is false" path for .IF* directives.
; Scans source lines (via READ_NEXT_LINE) until a matching .ELSE or .ENDIF
; keyword is found at the same nesting depth.
; Handles nested .IF/.ENDIF by tracking depth through COND_DEPTH / COND_ELSE.
; ============================================================================
COND_ASSM_SKIP:
            bit     LISTING_ON      ; LISTING_ON?
            bpl     @NOLIST
            bit     MACRO_EXPANDING ; MACRO_EXPANDING?
            bpl     @OUTERR
            bit     LIST_SUPPRESS2  ; LIST_SUPPRESS2?
            bpl     @NOLIST
@OUTERR:    jsr     OUTPUT_ERR_LINE ; Emit listing line while skipping

@NOLIST:    bit     END_SEEN        ; .END directive seen?
            bmi     ERR_COND        ; Yes -> error (premature end)
            bit     INCLUDE_POP     ; INCLUDE_POP?
            bmi     ERR_COND
            jsr     READ_NEXT_LINE  ; Read next source line while skipping
            ldy     #$00
            jsr     SKIP_LEADING_SPACES ; Skip spaces
            cmp     #'.'            ; Potential directive?
            bne     @NOLIST

            ; Check for .ELSE
            iny
            lda     LINE_BUF,Y
            cmp     #'E'
            bne     @NOLIST
            iny
            lda     LINE_BUF,Y
            cmp     #'L'
            bne     @CHK_ENDIF      ; Not .ELSE: check for .ENDIF
            iny
            lda     LINE_BUF,Y
            cmp     #'S'
            bne     @NOLIST
            iny
            lda     LINE_BUF,Y
            cmp     #'E'
            bne     @NOLIST
            ; Found .ELSE at this level
            asl     COND_ELSE       ; Shift COND_ELSE: check if already in else
            bcs     ERR_COND        ; Was already in .ELSE -> error (two .ELSEs)
            sec
            ror     COND_ELSE       ; COND_ELSE = $80 (entered .ELSE)
            rts                     ; Resume assembly at .ELSE body

            ; Check for .ENDIF
@CHK_ENDIF: cmp     #'N'
            bne     @NOLIST
            iny
            lda     LINE_BUF,Y
            cmp     #'D'
            bne     @NOLIST
            iny
            lda     LINE_BUF,Y
            cmp     #'I'
            bne     @NOLIST
            iny
            lda     LINE_BUF,Y
            cmp     #'F'
            bne     @NOLIST
            ; Found .ENDIF at this level
            asl     COND_DEPTH      ; COND_DEPTH: pop one level
            asl     COND_ELSE
            rts                     ; Resume assembly after .ENDIF

ERR_COND:   jsr     ERROR_CONDITIONAL ; Conditional nesting error

; ============================================================================
; HANDLE_ELSE
; Handles the ".ELSE" directive when encountered during normal assembly
; (i.e. the corresponding .IF was true and we now need to skip the else block).
; ============================================================================
HANDLE_ELSE:
            bit     COND_DEPTH      ; COND_DEPTH: inside .IF?
            bpl     ERR_COND        ; Not inside .IF -> error
            asl     COND_ELSE       ; Were we in .ELSE already?
            bcs     ERR_COND        ; Yes -> duplicate .ELSE -> error
            sec
            ror     COND_ELSE       ; COND_ELSE = $80 (now in else block)
            jmp     COND_ASSM_SKIP  ; Skip the else body

; ============================================================================
; HANDLE_ENDIF
; Handles ".ENDIF": pops one conditional nesting level.
; ============================================================================
HANDLE_ENDIF:
            asl     COND_DEPTH      ; COND_DEPTH: pop level
            bcc     ERR_COND        ; Was not inside .IF -> error
            asl     COND_ELSE       ; Pop COND_ELSE level
            rts

; ============================================================================
; HANDLE_READ
; Handles the ".READ filename" include directive.
; Saves the current file position onto the include stack (PUSH_INCL_FRAME),
; assigns a new source channel to the specified file, and starts reading
; from it.
; ============================================================================
HANDLE_READ:
            lda     #'A'            ; Set default extension for .READ files
            sta     DEFAULT_EXT_CHAR
            jsr     SKIP_LEADING_SPACES ; Skip spaces
            beq     @ERROR          ; No filename -> error
            jsr     PARSE_FILENAME  ; Parse include filename
            bcs     @ERROR          ; Parse error
            jsr     SAVE_FILE_POS   ; Save current position
            ldx     #SRC_FNAME_OFFSET
            jsr     COPY_FILENAME_TO_SLOT ; Set up new filename slot
            lda     PARSED_DRIVE    ; Parsed drive
            sta     SRC_DRIVE
            jsr     OPEN_CHANNEL    ; Open include file
            jsr     READ_BLOCK_SVC  ; Read block from channel
            rts
@ERROR:     jsr     REPORT_ERROR

; ============================================================================
; READ_NEXT_BLOCK
;
; If OBJ_DEVICE contains a valid drive number (< 4, i.e., a real disk
; drive), initializes the object code output channel:
;   1. Clears the file position accumulator U7/U7+1/U7+2.
;   2. Pushes a frame onto the include stack (PUSH_INCLUDE_FRAME) to be
;      able to restore the active channel state.
;   3. Copies the object filename register (EXT_FNAME_SLOT, 14 bytes) into the
;      active register FILENAME_SLOTS.
;   4. Saves the object drive number in SRC_DRIVE (current drive).
;   5. Calls OPEN_CHANNEL to assign the channel to the object file.
;
; If OBJ_DEVICE >= 4 (e.g. 'N' = $4E = no file output), returns without doing
; anything.
; ============================================================================
READ_NEXT_BLOCK:
            lda     OBJ_DEVICE          ; Object file device
            cmp     #$04
            bcs     OBJ_OPEN_DONE       ; >= 4 -> no object file: exit
            lda     #$00
            sta     U7                  ; Clear the 24-bit file position
            sta     U7+1                ;  accumulator (for PUSH_INCL_FRAME)
            sta     U7+2
            jsr     PUSH_INCLUDE_FRAME  ; Save current state
            ldx     #$0d                ; 14 bytes (indices 13..0)
OBJ_FNAME_COPY_LOOP:
            lda     EXT_FNAME_SLOT,X    ; Source: object filename register
            sta     FILENAME_SLOTS,X    ; Destination: active filename register
            dex
            bpl     OBJ_FNAME_COPY_LOOP
            lda     OBJ_DEVICE          ; Retrieve object drive number
            sta     SRC_DRIVE           ; Set as active source drive
            ldy     LIST_DRIVE_POS      ; LIST_DRIVE_POS (for OPEN_CHANNEL)
            jsr     OPEN_CHANNEL        ; Assign channel to object file
OBJ_OPEN_DONE:
            rts

            .byte   $7e                 ; Dead bytes (unreachable code)
            .byte   $7c

; ============================================================================
; HANDLE_DEF
;
; Implements the ".DEF label[,label...]" directive.
; Only active in pass 2 (returns immediately in pass 1).
; For each symbol in the list:
;   1. Evaluates the expression (symbol name).
;   2. If the value is resolved, writes it to the DEF channel (DEF_CHANNEL) as
;      a text line of the form:  "NAME== $XXXX"
;      using SVC $06 (output line) and SVC $0a (hex encode).
;   3. If the DEF device is 'N' (none), terminates silently.
;   4. Continues with the next comma-separated symbol.
; ============================================================================
HANDLE_DEF: bit     PASSFLG
            bpl     HDEF_DONE           ; Pass 1: do nothing
HDEF_NEXT_SYMBOL:
            jsr     SKIP_LEADING_SPACES ; Skip spaces; is there an operand?
            beq     HDEF_SYNTAX_ERR     ; Empty line -> syntax error
            lda     #$00
            sta     URESFLG             ; Clear unresolved reference flag
            jsr     EVAL_SYMBOL_REFERENCE ; EVAL_SYMBOL_REF: evaluate name/expression
            bit     URESFLG
            bmi     HDEF_UNRES_ERR      ; Unresolved value -> error
            lda     DEF_DEVICE          ; Definitions device
            cmp     #'N'
            beq     HDEF_DONE           ; 'N' = no output -> terminate
            svc     $0c                 ; SVC: get system buffer addresses
            ldy     #$00
HDEF_NAME_COPY_LOOP:
            lda     IDENT_BUF,Y         ; Name character
            beq     HDEF_WRITE_VALUE    ; End of name (NUL) -> write value
            sta     (U6),Y              ; Copy to system output buffer
            iny
            bne     HDEF_NAME_COPY_LOOP
HDEF_WRITE_VALUE:
            lda     #'='
            sta     (U6),Y              ; First '='
            iny
            sta     (U6),Y              ; Second '='
            iny
            lda     #'$'
            sta     (U6),Y              ; Hexadecimal prefix
            iny
            lda     SYMVAL
            sta     U0                  ; Value to encode (16 bits)
            lda     SYMVAL+1
            sta     U0+1
            svc     $0a                 ; SVC: encode U0 as ASCII hex in the buffer
            ldx     DEF_CHANNEL         ; Definitions channel for SVC
            svc     6                   ; SVC: emit line to channel
            ldy     LINE_POS
            jsr     PEEK_BLANK_CHAR     ; Any more significant chars?
            beq     HDEF_DONE           ; End of line -> terminate
            cmp     #','
            bne     HDEF_SYNTAX_ERR     ; Not a comma -> error
            iny                         ; Skip the comma
            bne     HDEF_NEXT_SYMBOL    ; Process next symbol
HDEF_DONE:
            rts
HDEF_SYNTAX_ERR:
            jsr     ERROR_SYNTAX
            ; Does not return
HDEF_UNRES_ERR:
            ldy     LINE_POS
            jsr     ERROR_UNDEFINED     ; Unresolved reference
            ; Does not return

; ============================================================================
; HANDLE_LIST
;
; Implements the ".LIST [expression]" directive.
; Controls the source code listing output.
; The operand is a 3-bit mask encoded in SYMVAL:
;   bit 0 (lsb): enable/disable listing in LINE_LIST_FLAGS
;   bit 1:       enable/disable VALUE_SUPPRESS
;   bit 2:       enable/disable LIST_SUPPRESS2
; If there is no operand, uses the default value 1 (enable listing).
; ============================================================================
HANDLE_LIST:
            lda     #$01
            sta     SYMVAL              ; Default value: 1 (enable listing)
            jsr     SKIP_LEADING_SPACES ; Skip spaces; is there an operand?
            beq     HLIST_APPLY_VALUE   ; No operand: use default value
            jsr     EVAL_FULL_EXPR      ; Evaluate control expression
            bpl     HLIST_APPLY_VALUE   ; N=0 -> OK
            jsr     ERROR_UNDEFINED     ; N=1 -> unresolved reference: error
            ; Does not return
HLIST_APPLY_VALUE:
            lda     SYMVAL
            lsr     A                   ; Bit 0 -> carry, to rotate into LINE_LIST_FLAGS
            php                         ; Save carry
            ror     LINE_LIST_FLAGS     ; LINE_LIST_FLAGS: bit 7 = listing state
            plp
            bit     LISTING_FILE_OPEN   ; LISTING_FILE_OPEN?
            bpl     HLIST_SET_FLAGS     ; Listing channel closed -> update flags only
            bit     PASSFLG
            bmi     HLIST_CHK_CURRENT_STATE ; Pass 2: apply immediately
            bit     ERROR_LISTING_RUN   ; Check if we are on error listing pass
            bpl     HLIST_SET_FLAGS     ; Listing not enabled in pass 1: flags only
HLIST_CHK_CURRENT_STATE:
            bit     LISTING_ON          ; LISTING_ON currently active?
            bpl     HLIST_ENABLE        ; Not active -> enable
            ror     LISTING_ON          ; Disable listing (rotate bit 7 from carry)
            bit     LISTING_ON          ; Did it stay active?
            bmi     HLIST_SET_FLAGS     ; Yes -> do not emit transition line
            jsr     OUTPUT_ERR_LINE     ; Emit last line before disabling
            jmp     HLIST_SET_FLAGS
HLIST_ENABLE:
            ror     LISTING_ON          ; Enable listing
HLIST_SET_FLAGS:
            lda     SYMVAL
            lsr     A
            lsr     A                   ; Bit 1 of original value -> carry
            ror     VALUE_SUPPRESS      ; VALUE_SUPPRESS: bit 7 = suppress values
            lsr     A                   ; Bit 2 of original value -> carry
            ror     LIST_SUPPRESS2      ; LIST_SUPPRESS2
            rts

; Stub: empty return
;
EMPTY_RETURN_STUB:
            rts
            nop
            nop

; ============================================================================
; HANDLE_MACRO
;
; Implements the ".MACRO name [param,...]" directive.
; Pass 1: calls DEFINE_MACRO to register the name in the symbol table
;         and write the body header to the heap. Then reads and stores the
;         macro body line by line (via STORE_MACRO_LINE) until .ENDMAC
;         is found.
; Pass 2: only reads lines until .ENDMAC without storing them (they are
;         already in pass 1 memory); optionally emits each line to the listing.
; In both passes: if INCLUDE_POP is detected before .ENDMAC, it's an error
; (premature end of macro).
; ============================================================================
HANDLE_MACRO:
            bit     PASSFLG
            bmi     HMAC_P2_SCAN_LOOP   ; Pass 2: go to pass 2 loop
            jsr     DEFINE_MACRO        ; Register name and header
HMAC_P1_SCAN_LOOP:
            bit     LISTING_ON          ; LISTING_ON?
            bpl     HMAC_P1_CHK_INCLUDE_POP
            jsr     OUTPUT_ERR_LINE     ; Emit line to listing
HMAC_P1_CHK_INCLUDE_POP:
            bit     INCLUDE_POP         ; INCLUDE_POP: requested return from .READ?
            bpl     HMAC_P1_READ_LINE   ; No -> read next line
HMAC_ABORT:
            jsr     ERROR_MACRO         ; Syntax error (premature)
            ; Does not return
HMAC_P1_READ_LINE:
            jsr     READ_NEXT_LINE      ; Read the next source line
            jsr     DETECT_ENDMAC       ; Is it .ENDMAC?
            bcs     HMAC_P1_WRITE_TERM  ; Yes -> write terminator and exit
            jsr     STORE_MACRO_LINE    ; Store line in the macro body
            jmp     HMAC_P1_SCAN_LOOP
HMAC_P1_WRITE_TERM:
            lda     #$00
            jsr     WRITE_TO_HEAP       ; Write $00 as final body terminator
            rts

HMAC_P2_SCAN_LOOP:
            bit     LISTING_ON          ; LISTING_ON?
            bpl     HMAC_P2_CHK_INCLUDE_POP
            jsr     OUTPUT_ERR_LINE     ; Emit line to listing
HMAC_P2_CHK_INCLUDE_POP:
            bit     INCLUDE_POP         ; INCLUDE_POP?
            bmi     HMAC_ABORT          ; Yes -> error (premature end)
            jsr     READ_NEXT_LINE      ; Read next line
            jsr     DETECT_ENDMAC       ; Is it .ENDMAC?
            bcc     HMAC_P2_SCAN_LOOP   ; No -> continue reading
            rts                         ; Yes -> end

; ============================================================================
; HANDLE_ENDMAC
;
; Handler for the ".ENDMAC" directive when found outside an active
; .MACRO block. This is always an error (reached only if .ENDMAC
; appears without its corresponding .MACRO).
; ============================================================================
 HANDLE_ENDMAC:
            jsr     ERROR_MACRO         ; Syntax error
            ; Does not return

; ============================================================================
; INIT_SYMTBL
;
; Symbol table initialization before each pass.
; Steps:
;   1. Calculates the pointer to the last hash bucket (HASH_LAST =
;      HASHTEND − 2) for wrap-around searches.
;   2. Copies HASHTST/HASHTST+1 into MACTBLP (macro table base).
;   3. Sweeps the entire hash area (HASHTST..HASHTEND) with zeros.
;   4. Activates the SYMTBL_INIT flag to inhibit normal name writing
;      during predefined macro registration.
;   5. Initializes MACTBLP and the secondary heap pointers (HEAP2PRP/HEAP2WRP)
;      with the internal macro base (HEAP2ST).
;   6. Traverses the linked list of predefined macros (at HEAPST):
;      copies each macro name to IDENT_BUF and calls SYMTBL_HASH_LKP (hash insert).
;   7. Configures the SYMTBLP[12]/[13] pointers with the values $71/$5F
;      (pointing to the opcode operand table).
;   8. Advances SYMVALP one byte past MACTBLP (start of free heap).
;   9. Clears SYMTBL_INIT.
; ============================================================================
INIT_SYMTBL:
            lda     HASHTEND            ; End of hash table, low byte
            sec
            sbc     #$02
            sta     HASH_LAST           ; HASH_LAST = end − 2 (last bucket)
            lda     HASHTEND+1
            sbc     #$00
            sta     HASH_LAST+1
            lda     HASHTST             ; Hash table base = macro base
            sta     MACTBLP
            lda     HASHTST+1
            sta     MACTBLP+1
            ldy     #$00
            sty     SYMBOL_COUNT        ; SYMBOL_COUNT = 0
            sty     SYMBOL_COUNT+1
            ; Sweep the hash table with zeros (page by page)
ISYM_CLEAR_PAGE:
            tya
ISYM_ZERO_LOOP:
            sta     (MACTBLP),Y         ; Clear each byte to zero
            iny
            bne     ISYM_ZERO_LOOP
            inc     MACTBLP+1           ; Advance to the next 256-byte block
            lda     MACTBLP+1
            cmp     HASHTEND+1          ; Did we reach the end?
            bne     ISYM_CLEAR_PAGE
            ; Activate initialization flag (inhibit name writing)
            lda     #$80
            sta     SYMTBL_INIT         ; SYMTBL_INIT = active
            ; Point MACTBLP to the internal macro base (HEAPST)
            lda     HEAPST
            sta     MACTBLP
            lda     HEAPST+1
            sta     MACTBLP+1
            ; Initialize secondary heap pointers with HEAP2ST
            lda     HEAP2ST
            sta     HEAP2WRP
            sta     HEAP2PRP
            lda     HEAP2ST+1
            sta     HEAP2WRP+1
            sta     HEAP2PRP+1
            ; Predefined macro registration loop
ISYM_MACRO_PREDEF_LOOP:
            lda     MACTBLP
            sta     SYMVALP             ; SYMVALP points to current entry
            lda     MACTBLP+1
            sta     SYMVALP+1
            ldx     #$00
            ldy     #$01                ; Y=1: skip the length byte at the start
ISYM_COPY_NAME_LOOP:
            lda     (MACTBLP),Y         ; Read character from name
            sta     IDENT_BUF,X         ; Copy to IDENT_BUF
            bmi     ISYM_REGISTER_MACRO ; Bit 7 = 1 -> last char of name (MSB terminated)
            inx
            iny
            bne     ISYM_COPY_NAME_LOOP
ISYM_REGISTER_MACRO:
            lda     #$00
            sta     IDENT_BUF,X         ; Add NUL terminator to name
            inc     SYMVALP             ; Point SYMVALP to the byte following the name
            bne     ISYM_HASH_INSERT
            inc     SYMVALP+1
ISYM_HASH_INSERT:
            jsr     SYMTBL_HASH_LKP     ; Insert into hash table
            ldy     #$00
            lda     (MACTBLP),Y         ; Read entry length byte
            clc
            adc     MACTBLP             ; Advance MACTBLP to the next macro
            sta     MACTBLP
            bcc     ISYM_NEXT_MACRO
            inc     MACTBLP+1
ISYM_NEXT_MACRO:
            lda     (MACTBLP),Y         ; Any more entries? (0 = end of list)
            bne     ISYM_MACRO_PREDEF_LOOP
            ; Configure SYMTBLP+12/+13 pointing to opcodes
            ldx     #$0c
            lda     #$71
            sta     SYMTBLP,X           ; SYMTBLP[12] = $71 (low address)
            inx
            lda     #$5f
            sta     SYMTBLP,X           ; SYMTBLP[13] = $5F (high address)
            lda     #$00
            sta     SYMTBL_CHKSUM
            sta     SYMTBL_CHKSUM+1
            ; SYMVALP = MACTBLP + 1 (start of free heap)
            lda     MACTBLP
            clc
            adc     #$01
            sta     SYMVALP
            lda     MACTBLP+1
            adc     #$00
            sta     SYMVALP+1
            asl     SYMTBL_INIT         ; SYMTBL_INIT = $00 (disable init flag)
            rts

; ============================================================================
; LOOKUP_OPCODE
;
; Looks up the current token (from IDENT_BUF) in the mnemonics table.
; First appends the terminator character '*' ($2A) to the name to match
; the table format, then calls SYMTBL_HASH_LKP (hash lookup).
; Saves the pointer to the found entry in MATCHP/MATCHP+1.
;
; Return codes (in processor flags):
;   C clr          -> not found (unknown symbol)
;   C set, N=1     -> assembler directive (pseudo-op)
;   C set, V=1     -> macro invocation
;   C set, N=0,V=0 -> normal 6502 instruction
; ============================================================================
LOOKUP_OPCODE:
            sty     LINE_POS_SAVE2      ; Save line position (never recovered)
            jsr     EVAL_COLLECT_IDENT  ; Tokenize the current mnemonic
            ldx     IDENT_LEN           ; Token length
            lda     #'*'                ; Add '*' terminator (table format)
            sta     IDENT_BUF,X
            inx
            lda     #$00
            sta     IDENT_BUF,X         ; NUL after '*'
            stx     IDENT_LEN           ; Update length
            jsr     SYMTBL_HASH_LKP     ; Look up in table
            sty     SYM_MATCH_OFF       ; Offset of found field
            lda     SYMTBLP
            sta     MATCHP              ; Save pointer to found bucket
            lda     SYMTBLP+1
            sta     MATCHP+1
            bcc     LOP_NOT_FOUND       ; C=0 -> not found
            bit     ENTRY_TYPE_FLAGS    ; ENTRY_TYPE_FLAGS: V=1 -> macro
            bvc     LOP_IS_OPCODE       ; V=0 -> normal instruction or directive
            lda     ENTRY_TYPE_FLAGS
            asl     A
            asl     A
            sta     MACRO_FLAGS         ; MACRO_FLAGS: copy type bits
            jsr     EMPTY_RETURN_STUB   ; Empty return (stub)
            bit     MACRO_FLAGS         ; Return macro type flags in N/V
            rts
LOP_NOT_FOUND:
            ; Not found: write empty entry to heap (xref)
            lda     #$c0
            jsr     WRITE_TO_PRIMARY_HEAP ; Write $C0 ("not found" flag)
            lda     #$00
            jsr     WRITE_TO_PRIMARY_HEAP
            jsr     WRITE_TO_PRIMARY_HEAP
            jsr     WRITE_TO_PRIMARY_HEAP
            bit     XREF_MODE           ; XREF_MODE active?
            bpl     LOP_UNDEFINED_ERR   ; No -> go to error
            jsr     WRITE_TO_PRIMARY_HEAP ; Yes -> write 4 additional xref bytes
            jsr     WRITE_TO_PRIMARY_HEAP
            lda     LINE_NUM
            jsr     WRITE_TO_PRIMARY_HEAP
            lda     LINE_NUM+1
            jsr     WRITE_TO_PRIMARY_HEAP
            jmp     LOP_UNDEFINED_ERR
LOP_IS_OPCODE:
            jsr     RECORD_XREF         ; Register cross reference
LOP_UNDEFINED_ERR:
            ldy     LINE_POS
            jsr     ERROR_UNDEFINED
            ; Does not return

; ============================================================================
; ASSEMBLE_INSTR
;
; Core of instruction assembly in pass 2.
; Steps:
;   1. RECORD_XREF: register mnemonic xref.
;   2. Read the pointer to the opcodes subtable from the mnemonics table
;      entry (offset SYM_MATCH_OFF+1,+2) -> OPCODE_TBL_PTR.
;   3. Call RESOLVE_OPERAND_MODE to parse the operand and
;      determine the addressing mode.
;   4. Emit the listing line if applicable.
;   5. If assembly produced an error (bit 7 of MACRO_EXPANDING set), increment
;      the error counter (FATAL_MACROS_OVERNESTED).
;   6. Copy OPCODE_TBL_PTR -> OPCTBLP (opcode table read pointer).
;   7. Call INCREMENT_LOCAL_LABEL to advance the local label
;      counter.
; ============================================================================
ASSEMBLE_INSTR:
            jsr     RECORD_XREF         ; Register xref
            ldy     SYM_MATCH_OFF       ; Offset of found field
            iny
            lda     (SYMTBLP),Y         ; Read pointer to opcodes table, low byte
            sta     OPCODE_TBL_PTR
            iny
            lda     (SYMTBLP),Y         ; High byte
            sta     OPCODE_TBL_PTR+1
            jsr     RESOLVE_OPERAND_MODE ; Parse operand and mode
            bit     LISTING_ON          ; LISTING_ON?
            bpl     AINSTR_LISTING_DONE
            bit     MACRO_EXPANDING     ; MACRO_EXPANDING?
            bmi     AINSTR_LISTING_DONE ; Inside macro: do not emit listing here
            bit     LIST_SUPPRESS2      ; LIST_SUPPRESS2?
            bmi     AINSTR_LISTING_DONE
            jsr     OUTPUT_ERR_LINE     ; Emit listing line
AINSTR_LISTING_DONE:
            sec
            ror     MACRO_EXPANDING     ; MACRO_EXPANDING: read error bit and clear
            bcc     AINSTR_SET_PTR      ; No error -> continue
            jsr     FATAL_MACROS_OVERNESTED
            ; Does not return
AINSTR_SET_PTR:
            lda     OPCODE_TBL_PTR       ; Pointer to opcodes table
            sta     OPCTBLP
            lda     OPCODE_TBL_PTR+1
            sta     OPCTBLP+1
            jsr     INCREMENT_LOCAL_LABEL ; Advance local label
            rts

; ============================================================================
; INCREMENT_LOCAL_LABEL
;
; Increments the local label ASCII counter stored at PREV_LOCAL (4 chars,
; format "000 " or "0001" .. "9999"). Carry propagates from digit to digit
; (BCD in ASCII). After incrementing, copies the updated counter to NEXT_LOCAL
; (used in local label substitution).
; ============================================================================
INCREMENT_LOCAL_LABEL:
            ldx     #$03
            lda     PREV_LOCAL,X        ; Is the last digit a space?
            cmp     #' '
            bne     ILL_INC_LOOP_ENTRY
            dex                         ; Yes -> skip the space; start at the previous one
ILL_INC_LOOP_ENTRY:
            clc
ILL_INC_LOOP:
            lda     PREV_LOCAL,X
            clc
            adc     #$01                ; Increment ASCII digit
            sta     PREV_LOCAL,X
            cmp     #'9'+1
            bcc     ILL_COPY_TO_NEXT    ; No overflow -> copy and exit
            lda     #'0'                ; Overflow: reset to '0'
            sta     PREV_LOCAL,X
            dex
            bpl     ILL_INC_LOOP        ; Propagate carry to previous digit
            lda     #'1'
            sta     PREV_LOCAL          ; Prepend digit '1' (total overflow)
            lda     #'0'
            sta     PREV_LOCAL+3
ILL_COPY_TO_NEXT:
            ldx     #$03
ILL_COPY_LOOP:
            lda     PREV_LOCAL,X        ; Copy PREV_LOCAL -> NEXT_LOCAL
            sta     NEXT_LOCAL,X
            dex
            bpl     ILL_COPY_LOOP
            rts

; ============================================================================
; RESOLVE_OPERAND_MODE
;
; Parses the argument list of a macro invocation or instruction.
; Reads the comma-separated tokens from the operand field (LINE_BUF,Y) and
; writes them sequentially to the argument buffer (via WRITE_ARG_BYTE),
; separated by NUL bytes. Counts the number of arguments in MACRO_ARG_CNT.
; Handles strings delimited by single quotes or curly braces {}.
; Upon completion, writes an extra NUL as a sentinel and returns the current
; char in A and updated Y.
; ============================================================================
RESOLVE_OPERAND_MODE:
            jsr     PUSH_MACRO_CALL_FRAME ; Save call frame
            lda     #$00
            sta     MACRO_ARG_CNT       ; MACRO_ARG_CNT = 0
            ldy     LINE_POS
ROM_SCAN_LOOP:
            jsr     SKIP_LEADING_SPACES ; Skip spaces
            beq     ROM_DONE            ; End of line -> terminate
            cmp     COMMENT             ; Comment character?
            beq     ROM_DONE
ROM_CHK_COMMA:
            cmp     #','
            bne     ROM_CHK_STRING      ; Not a comma -> check string
            lda     #$00
            jsr     WRITE_ARG_BYTE      ; Write NUL as argument separator
            inc     MACRO_ARG_CNT       ; Increment argument counter
            iny
            jmp     ROM_SCAN_LOOP       ; Process next argument
ROM_CHK_STRING:
            cmp     APOSTROPHE          ; Single quote (char literal)?
            beq     ROM_PARSE_STRING
            cmp     ARGLEFT             ; String delimiter '{'?
            bne     ROM_CHK_COMMENT_CHR
ROM_PARSE_STRING:
            jsr     SCAN_STRING_TOKEN   ; Scan and copy the string
            jsr     SKIP_LEADING_SPACES ; Skip spaces after the string
            beq     ROM_CLOSE_ARG       ; End of line -> close argument
            jmp     ROM_CHK_COMMA       ; Check if comma comes next
ROM_CHK_COMMENT_CHR:
            cmp     COMMENT             ; Comment character?
            beq     ROM_CLOSE_ARG
            jsr     WRITE_ARG_BYTE      ; Copy char to args buffer
            iny
            lda     LINE_BUF,Y          ; Read next char
            beq     ROM_CLOSE_ARG       ; End of line
            cmp     #' '
            bne     ROM_CHK_COMMA       ; Not space -> check again
ROM_CLOSE_ARG:
            inc     MACRO_ARG_CNT       ; Close argument: increment counter
ROM_DONE:
            lda     #$00
            jsr     WRITE_ARG_BYTE      ; Write final sentinel NUL
            lda     LINE_BUF,Y          ; Return the current character
            rts

; ============================================================================
; SCAN_STRING_TOKEN
;
; Scans and copies the content of a delimited string to the arguments buffer.
; If the opening delimiter is the single quote (APOSTROPHE), copies until the
; next single quote. If it is '{' (ARGLEFT), the closing delimiter is
; '}' (ARGRIGHT). It is written byte by byte via WRITE_ARG_BYTE.
; ============================================================================
SCAN_STRING_TOKEN:
            sta     STR_DELIM           ; Save open delimiter
            cmp     APOSTROPHE          ; Single quote?
            beq     SST_COPY_CHAR       ; Yes: closing delimiter is the same quote
            lda     ARGRIGHT            ; No: closing delimiter is '}'
            sta     STR_DELIM
SST_SCAN_LOOP:
            iny
            lda     LINE_BUF,Y          ; Read next character of the token
            beq     SST_FINAL_CHAR      ; End of line -> return
            cmp     STR_DELIM           ; Is it the closing delimiter?
            beq     SST_CLOSING_DELIM
SST_COPY_CHAR:
            jsr     WRITE_ARG_BYTE      ; Copy character to buffer
            jmp     SST_SCAN_LOOP
SST_CLOSING_DELIM:
            cmp     ARGRIGHT            ; Is the closing delimiter '}'?
            beq     SST_PAST_DELIM      ; Yes: consume without copying
            jsr     WRITE_ARG_BYTE      ; No (single quote): copy the closing
SST_PAST_DELIM:
            iny                         ; Advance Y past the closing delimiter
SST_FINAL_CHAR:
            lda     LINE_BUF,Y          ; Return next character after the string
            rts

; ============================================================================
; READ_MACRO_BODY_BYTE
;
; Reads a byte of the macro body from the position indicated by OPCTBLP
; (opcodes table read pointer / macro body) and increments the pointer.
; Manages access to expansion RAM (bank 3) if active (bit 7 of EXPANSION_BANK).
; Returns: A = read byte, Z flags = (byte == 0) (to detect end of line
; or end of body).
; ============================================================================
READ_MACRO_BODY_BYTE:
            stx     READMB_X_SAVE       ; Save X
            ldx     #$00
            bit     EXPANSION_BANK      ; EXPANSION_BANK: secondary RAM active?
            bpl     RMBB_PRIMARY_READ   ; No -> read from primary heap
            ; Read from expansion RAM (bank BANK_CTL_BITS)
            lda     BNKCTL
            and     #<~$03              ; Bank 3
            ora     BANK_CTL_BITS       ; Select expansion bank
            sta     BNKCTL
            lda     (OPCTBLP,X)         ; Read body byte
            pha
            lda     BNKCTL
            and     #$fc                ; Mask out other bits
            ora     #$03                ; Restore data bank
            sta     BNKCTL
            pla
            jmp     RMBB_INC_PTR
RMBB_PRIMARY_READ:
            lda     (OPCTBLP,X)         ; Read byte from primary heap
RMBB_INC_PTR:
            inc     OPCTBLP             ; Advance read pointer
            bne     RMBB_DONE
            inc     OPCTBLP+1
RMBB_DONE:
            ldx     READMB_X_SAVE       ; Restore X
            cmp     #$00                ; Update Z flag (0 = end of block)
            rts

; ============================================================================
; WRITE_ARG_BYTE
;
; Writes the byte in A to the macro argument buffer (pointer MARGWRTP)
; and increments MARGWRTP. Checks that the buffer limit (MACSEND) is not exceeded;
; if it overflows, calls FATAL_MACROS_OVERNESTED (complexity error).
; ============================================================================
WRITE_ARG_BYTE:
            stx     RWARG_X_SAVE        ; Save X
            ldx     #$00
            sta     (MARGWRTP,X)        ; Write byte to the argument buffer
            inc     MARGWRTP            ; Advance write pointer
            bne     WAB_CHK_LIMIT
            inc     MARGWRTP+1
WAB_CHK_LIMIT:
            lda     MARGWRTP            ; Buffer limit exceeded?
            cmp     MACSEND
            lda     MARGWRTP+1
            sbc     MACSEND+1
            bcc     WAB_DONE            ; No -> OK
            jsr     FATAL_MACROS_OVERNESTED ; Yes -> complexity error
            ; Does not return
WAB_DONE:
            ldx     RWARG_X_SAVE        ; Restore X
            rts

; ============================================================================
; READ_ARG_BYTE
;
; Reads a byte from the argument strings buffer at position MARGSTRP
; and increments MARGSTRP. Returns: A = read byte, Z flags = (byte == 0).
; ============================================================================
READ_ARG_BYTE:
            stx     RWARG_X_SAVE        ; Save X
            ldx     #$00
            lda     (MARGSTRP,X)        ; Read byte from the argument buffer
            inc     MARGSTRP            ; Advance read pointer
            bne     RAB_DONE
            inc     MARGSTRP+1
RAB_DONE:
            ldx     RWARG_X_SAVE        ; Restore X
            cmp     #$00                ; Update Z flag
            rts

; ============================================================================
; PUSH_MACRO_CALL_FRAME
;
; Saves the state of the current macro invocation in the argument buffer
; before processing a new expansion level. Writes 9 bytes with the
; current frame: {MACSPTR lo, MACSPTR hi, MACRO_ARG_CNT (arg count), OPCTBLP lo, OPCTBLP hi,
; NEXT_LOCAL[4]}. Updates MACSPTR to the current write pointer MARGWRTP.
; This allows nesting macro expansions.
; ============================================================================
PUSH_MACRO_CALL_FRAME:
            lda     MACSPTR             ; Previous frame pointer (lo)
            jsr     WRITE_ARG_BYTE
            lda     MACSPTR+1           ; Previous frame pointer (hi)
            jsr     WRITE_ARG_BYTE
            lda     MACRO_ARG_CNT       ; Current number of arguments
            jsr     WRITE_ARG_BYTE
            lda     OPCTBLP             ; Macro body read pointer (lo)
            jsr     WRITE_ARG_BYTE
            lda     OPCTBLP+1
            jsr     WRITE_ARG_BYTE
            lda     NEXT_LOCAL          ; NEXT_LOCAL[0]
            jsr     WRITE_ARG_BYTE
            lda     NEXT_LOCAL+1        ; NEXT_LOCAL[1]
            jsr     WRITE_ARG_BYTE
            lda     NEXT_LOCAL+2        ; NEXT_LOCAL[2]
            jsr     WRITE_ARG_BYTE
            lda     NEXT_LOCAL+3        ; NEXT_LOCAL[3]
            jsr     WRITE_ARG_BYTE
            lda     MARGWRTP            ; Update frame pointer with current position
            sta     MACSPTR
            lda     MARGWRTP+1
            sta     MACSPTR+1
            rts

; ============================================================================
; POP_MACRO_CALL_FRAME
;
; Restores the macro invocation state from the previous level (undoes
; what PUSH_MACRO_CALL_FRAME did).
; If MACSPTR == MACSST (argument stack base), the stack is empty:
; returns C set (no more frames to restore -> end of expansion).
; If there is a previous frame: restores MACSPTR, MACRO_ARG_CNT, OPCTBLP,
; NEXT_LOCAL from the9 saved bytes and returns C clear.
; ============================================================================
POP_MACRO_CALL_FRAME:
            lda     MACSPTR             ; Are we at the base of the stack?
            cmp     MACSST
            bne     PMCF_RESTORE
            lda     MACSPTR+1
            cmp     MACSST+1
            bne     PMCF_RESTORE
            sec                         ; Empty stack -> return C set
            rts
PMCF_RESTORE:
            ; Calculate saved frame address: MARGWRTP = MACSPTR − 9
            lda     MACSPTR
            sec
            sbc     #$09
            sta     MARGWRTP
            lda     MACSPTR+1
            sbc     #$00
            sta     MARGWRTP+1
            sty     RWARG_Y_SAVE        ; Save Y
            ; Read the 9 frame fields (reverse order of saving)
            ldy     #$08
            lda     (MARGWRTP),Y        ; NEXT_LOCAL[3]
            sta     NEXT_LOCAL+3
            dey
            lda     (MARGWRTP),Y        ; NEXT_LOCAL[2]
            sta     NEXT_LOCAL+2
            dey
            lda     (MARGWRTP),Y        ; NEXT_LOCAL[1]
            sta     NEXT_LOCAL+1
            dey
            lda     (MARGWRTP),Y        ; NEXT_LOCAL[0]
            sta     NEXT_LOCAL
            dey
            lda     (MARGWRTP),Y        ; Macro body pointer (hi)
            sta     OPCTBLP+1
            dey
            lda     (MARGWRTP),Y        ; Macro body pointer (lo)
            sta     OPCTBLP
            dey
            lda     (MARGWRTP),Y        ; Macro argument count
            sta     MACRO_ARG_CNT
            dey
            lda     (MARGWRTP),Y        ; Previous frame pointer (hi)
            sta     MACSPTR+1
            dey
            lda     (MARGWRTP),Y        ; Previous frame pointer (lo)
            sta     MACSPTR
            ldy     RWARG_Y_SAVE        ; Restore Y
            clc                         ; Return C clear (previous frame exists)
            rts

; ============================================================================
; LOOKUP_MACRO_PARAM
;
; Returns a pointer to the Nth argument of the current macro (1-based).
; The index is passed in A. Reads the argument list from the current frame
; (MACSPTR -> argument buffer) and advances pointer MARGSTRP to the Nth arg.
; If the index exceeds the number of arguments (MACRO_ARG_CNT), points to the
; final NUL (empty argument) and returns A=0 (Z set).
; If A=0 -> SVC 0 (unrecoverable error: invalid index).
; ============================================================================
LOOKUP_MACRO_PARAM:
            sta     PARAM_IDX_SAVE      ; Save requested index (Seems that it is never recovered)
            stx     RWARG_X_SAVE
            tax                         ; X = index (1-based)
            bne     LMP_CHECK_COUNT     ; Index 0 -> fatal error
            svc     0                   ; SVC $00: show registers / return to monitor
            ; Does not return
LMP_CHECK_COUNT:
            cmp     MACRO_ARG_CNT       ; Index > number of arguments?
            beq     LMP_FIND_PARAM      ; =  -> within range
            bcc     LMP_FIND_PARAM      ; <  -> within range
            ; Index out of range: point to NUL at the end of the last arg
            lda     MARGWRTP
            sec
            sbc     #$01
            sta     MARGSTRP
            lda     MARGWRTP+1
            sbc     #$00
            sta     MARGSTRP+1
            ldx     RWARG_X_SAVE
            lda     #$00                ; Return NUL (empty argument)
            rts
LMP_FIND_PARAM:
            sty     RWARG_Y_SAVE        ; Save Y
            lda     MACSPTR             ; Start of the frame arguments area
            sta     MARGSTRP
            lda     MACSPTR+1
            sta     MARGSTRP+1
LMP_SCAN_TO_NTH:
            dex
            beq     LMP_RETURN_PTR      ; X=0 -> reached the desired argument
            ldy     #$ff
LMP_SKIP_ARG:
            iny
            lda     (MARGSTRP),Y
            bne     LMP_SKIP_ARG        ; Continue until NUL is found
            tya                         ; Advance MARGSTRP past the NUL
            sec
            adc     MARGSTRP
            sta     MARGSTRP
            lda     MARGSTRP+1
            adc     #$00
            sta     MARGSTRP+1
            jmp     LMP_SCAN_TO_NTH
LMP_RETURN_PTR:
            ldy     #$00
            lda     (MARGSTRP),Y        ; First byte of the argument
            ldy     RWARG_Y_SAVE        ; Restore Y
            ldx     RWARG_X_SAVE        ; Restore X
            cmp     #$00                ; Update Z (0 = empty argument)
            rts

; ============================================================================
; DETECT_MACRO
;
; Checks if the token at (LINE_BUF,Y) is ".MACRO" (followed by space, NUL, or
; comment). Returns C set if it is ".MACRO", C clear otherwise.
; In both cases, returns A = LINE_BUF[Y] (first character of the token).
; ============================================================================
DETECT_MACRO:
            cmp     #'.'
            bne     DM_NOT_MACRO
            lda     LINE_BUF+1,Y
            cmp     #'M'
            bne     DM_NOT_MACRO
            lda     LINE_BUF+2,Y
            cmp     #'A'
            bne     DM_NOT_MACRO
            lda     LINE_BUF+3,Y
            cmp     #'C'
            bne     DM_NOT_MACRO
            lda     LINE_BUF+4,Y
            cmp     #'R'
            bne     DM_NOT_MACRO
            lda     LINE_BUF+5,Y
            cmp     #'O'
            bne     DM_NOT_MACRO
            lda     LINE_BUF+6,Y        ; Character after ".MACRO": delimiter?
            beq     DM_IS_MACRO         ; NUL
            cmp     COMMENT             ; Comment
            beq     DM_IS_MACRO
            cmp     #' '                ; Space
            beq     DM_IS_MACRO
DM_NOT_MACRO:
            clc
            lda     LINE_BUF,Y
            rts
DM_IS_MACRO:
            sec
            lda     LINE_BUF,Y
            rts

; ============================================================================
; DETECT_ENDMAC
;
; Scans the current line (LINE_BUF) for the ".ENDMAC" directive.
; Allows ".ENDMAC" to appear after a label or leading space.
; Returns C set if ".ENDMAC" is detected (followed by space, NUL, or comment),
; C clear otherwise. A = LINE_BUF[Y] (char at start position).
; ============================================================================
DETECT_ENDMAC:
            ldy     #$00
            lda     LINE_BUF            ; First character of the line
            beq     DEM_NOT_ENDMAC      ; Empty line
            cmp     COMMENT             ; Comment?
            beq     DEM_NOT_ENDMAC
            cmp     #' '                ; Space (mnemonic field)?
            beq     DEM_CHECK_KEYWORD
DEM_SKIP_LABEL:
            iny
            lda     LINE_BUF,Y
            beq     DEM_NOT_ENDMAC
            cmp     COMMENT
            beq     DEM_NOT_ENDMAC
            cmp     #' '
            bne     DEM_SKIP_LABEL      ; Continue until first space
DEM_CHECK_KEYWORD:
            jsr     SKIP_LEADING_SPACES ; Skip spaces: get first word
            cmp     #'.'
            bne     DEM_NOT_ENDMAC
            lda     LINE_BUF+1,Y
            cmp     #'E'
            bne     DEM_NOT_ENDMAC
            lda     LINE_BUF+2,Y
            cmp     #'N'
            bne     DEM_NOT_ENDMAC
            lda     LINE_BUF+3,Y
            cmp     #'D'
            bne     DEM_NOT_ENDMAC
            lda     LINE_BUF+4,Y
            cmp     #'M'
            bne     DEM_NOT_ENDMAC
            lda     LINE_BUF+5,Y
            cmp     #'A'
            bne     DEM_NOT_ENDMAC
            lda     LINE_BUF+6,Y
            cmp     #'C'
            bne     DEM_NOT_ENDMAC
            lda     LINE_BUF+7,Y        ; Character after ".ENDMAC"
            beq     DEM_IS_ENDMAC       ; NUL
            cmp     COMMENT             ; Comment
            beq     DEM_IS_ENDMAC
            cmp     #' '
            beq     DEM_IS_ENDMAC
DEM_NOT_ENDMAC:
            clc
            lda     LINE_BUF,Y
            rts
DEM_IS_ENDMAC:
            sec
            lda     LINE_BUF,Y
            rts

; ============================================================================
; DEFINE_MACRO
;
; Processes the header of a ".MACRO name" definition in pass 1.
; Steps:
;   1. Parses the macro name (EVAL_EXPRESSION -> IDENT_BUF).
;   2. Appends '*' as terminator and calls SYMTBL_HASH_LKP.
;   3. If new (C clear): writes the type byte $E0 to the heap,
;      calculates and saves the body start address (MACBODYP),
;      writes the body pointer (2 bytes) and initial count (0).
;   4. If it already exists as a macro (C set, N=1, V=1): updates only the
;      body pointer in the existing entry (redefinition).
;   5. If it already exists as a normal symbol (C set, N=0): duplicate error.
;   6. If XREF_MODE active: writes 4 additional xref bytes.
;   7. Increments SYMBOL_COUNT.
; ============================================================================
DEFINE_MACRO:
            sty     MACDEF_Y_SAVE       ; Save Y
            jsr     EVAL_EXPRESSION     ; Tokenize macro name -> IDENT_BUF
            stx     SAVEX
            ldx     IDENT_LEN
            lda     #'*'
            sta     IDENT_BUF,X         ; Add terminator '*'
            inx
            lda     #$00
            sta     IDENT_BUF,X         ; NUL
            stx     IDENT_LEN
            ldx     SAVEX
            jsr     SYMTBL_HASH_LKP     ; Look up / insert
            bcc     DMAC_NEW_MACRO      ; C=0 -> new symbol
            bpl     DMAC_DUPLICATE_ERR  ; C=1, N=0 -> already exists as symbol: error
            bvc     DMAC_REDEF_MACRO    ; C=1, N=1, V=0 -> existing macro: update
DMAC_DUPLICATE_ERR:
            ldy     #$00
            jsr     ERROR_DUPLICATE               ; ERR_TYPE_LADDER: duplicate error
            ; Does not return
DMAC_NEW_MACRO:
            lda     #$e0
            jsr     WRITE_TO_PRIMARY_HEAP ; Write type $E0 (macro)
            asl     A
            sta     ENTRY_TYPE_FLAGS    ; ENTRY_TYPE_FLAGS = $C0
            bit     EXPANSION_BANK      ; EXPANSION_BANK available?
            bpl     DMAC_CALC_BODY_ADDR_PRI
            ; Body in expansion RAM
            lda     HEAP2WRP
            sta     MACBODYP
            lda     HEAP2WRP+1
            sta     MACBODYP+1
            jmp     DMAC_WRITE_BODY_PTR
DMAC_CALC_BODY_ADDR_PRI:
            lda     #$03                ; Default offset (no xref)
            bit     XREF_MODE           ; XREF_MODE active?
            bpl     DMAC_CALC_BODY_OFFSET
            lda     #$07                ; Larger offset with xref fields
DMAC_CALC_BODY_OFFSET:
            clc
            adc     SYMVALP             ; MACBODYP = SYMVALP + offset (space for header)
            sta     MACBODYP
            lda     SYMVALP+1
            adc     #$00
            sta     MACBODYP+1
DMAC_WRITE_BODY_PTR:
            lda     MACBODYP            ; Write body start address
            jsr     WRITE_TO_PRIMARY_HEAP
            lda     MACBODYP+1
            jsr     WRITE_TO_PRIMARY_HEAP
            lda     #$00                ; Initial reference count = 0
            jsr     WRITE_TO_PRIMARY_HEAP
            inc     REF_COUNT
            bne     DMAC_WRITE_XREF
            inc     REF_COUNT+1
DMAC_WRITE_XREF:
            bit     XREF_MODE           ; XREF_MODE?
            bpl     DMAC_DONE           ; No -> skip xref fields
            lda     #$00                ; xref ptr lo = 0
            jsr     WRITE_TO_PRIMARY_HEAP
            lda     #$00                ; xref ptr hi = 0
            jsr     WRITE_TO_PRIMARY_HEAP
            lda     LINE_NUM            ; LINE_NUM (definition on current line)
            jsr     WRITE_TO_PRIMARY_HEAP
            lda     LINE_NUM+1
            jsr     WRITE_TO_PRIMARY_HEAP
            jmp     DMAC_DONE
DMAC_REDEF_MACRO:
            lda     #$e0
            sta     (SYMTBLP),Y         ; Update type byte in place
            iny
            bit     EXPANSION_BANK      ; EXPANSION_BANK available?
            bpl     DMAC_REDEF_BODY_ADDR_PRI
            lda     HEAP2WRP
            sta     MACBODYP
            lda     HEAP2WRP+1
            sta     MACBODYP+1
            jmp     DMAC_WRITE_REDEF_PTR
DMAC_REDEF_BODY_ADDR_PRI:
            lda     SYMVALP
            sta     MACBODYP
            lda     SYMVALP+1
            sta     MACBODYP+1
DMAC_WRITE_REDEF_PTR:
            lda     MACBODYP
            sta     (SYMTBLP),Y         ; Write new body pointer (lo)
            iny
            lda     MACBODYP+1
            sta     (SYMTBLP),Y         ; Body pointer (hi)
            iny
            lda     #$00
            sta     (SYMTBLP),Y         ; Reference count = 0
            jsr     RECORD_XREF         ; Register xref
DMAC_DONE:
            ldy     MACDEF_Y_SAVE       ; Restore Y
            rts

; ============================================================================
; ASSEMBLE_ZP_ABS
;
; Entry point for instructions that use the current location counter
; value as an implicit operand (first byte of a .BYTE/.WORD list).
; If the line starts with space (empty mnemonic field), returns.
; Otherwise, loads LOCCNT2 into SYMVAL and falls through to EMIT_INSTR_REC
; with opcode $A0 (zero-page/absolute mode).
; ============================================================================
ASSEMBLE_ZP_ABS:
            lda     LINE_BUF            ; First character of the line
            cmp     #' '
            bne     AZPA_OPERAND
            rts
AZPA_OPERAND:
            lda     LOCCNT2             ; Current PC value at start of line
            sta     SYMVAL
            lda     LOCCNT2+1
            sta     SYMVAL+1
            lda     #$00
            sta     SYMVAL+2
            clc
            lda     #$a0                ; Opcode flags: absolute/zp mode
            jmp     EINR_WRITE_OPFLAGS  ; Fall through to EMIT_INSTR_REC

; ============================================================================
; EMIT_INSTR_REC
;
; Writes a 4-byte instruction record to the symbol heap:
;   { flags_opcode, symval_lo, symval_mid, symval_hi }
; where flags_opcode is the addressing mode control byte.
; There are three entry points:
;   EMIT_INSTR_REC: opcode = $A0, C set  (relative/branch mode)
;   EMIT_INSTR_REC2: opcode = $B0, C clear
;   EINR_WRITE_OPFLAGS: opcode in A, C according to caller
; If the symbol already exists in the table (hash lookup returns C set),
; updates the corresponding fields in place (in the existing entry).
; ============================================================================
EMIT_INSTR_REC:
            lda     #$a0
            sec
            jmp     EINR_WRITE_OPFLAGS
EMIT_INSTR_REC2:
            lda     #$b0
            clc
EINR_WRITE_OPFLAGS:
            sta     INSTR_FLAGS         ; INSTR_FLAGS: save opcode/mode byte
            ror     RELADDR_FLAG        ; RELADDR_FLAG: absorb carry (relative mode)
            sty     MACDEF_Y_SAVE       ; Save Y
            jsr     EVAL_EXPRESSION     ; Parse operand expression
            jsr     SYMTBL_HASH_LKP     ; Look up symbol
            bcs     EINR_EXISTING_SYM   ; Already exists -> update in place
            ; New symbol: check if 16-bit mode needed
EINR_NEW_SYM:
            lda     SYMVAL+1            ; Has upper bytes?
            ora     SYMVAL+2
            beq     EINR_BUILD_OPFLAGS  ; Only 8 bits -> do not mark 16-bit mode
            lda     #$08
EINR_BUILD_OPFLAGS:
            ora     INSTR_FLAGS         ; Combine with mode flags
            jsr     WRITE_TO_PRIMARY_HEAP ; Write control byte
            asl     A
            sta     ENTRY_TYPE_FLAGS
            lda     SYMVAL
            jsr     WRITE_TO_PRIMARY_HEAP ; Write value lo
            lda     SYMVAL+1
            jsr     WRITE_TO_PRIMARY_HEAP ; Value mid
            lda     SYMVAL+2
            jsr     WRITE_TO_PRIMARY_HEAP ; Value hi
            inc     REF_COUNT           ; REF_COUNT++
            bne     EINR_WRITE_XREF
            inc     REF_COUNT+1
EINR_WRITE_XREF:
            bit     XREF_MODE           ; XREF_MODE?
            bpl     EINR_DONE
            lda     #$00
            jsr     WRITE_TO_PRIMARY_HEAP ; xref ptr lo
            jsr     WRITE_TO_PRIMARY_HEAP ; xref ptr hi
            lda     LINE_NUM
            jsr     WRITE_TO_PRIMARY_HEAP
            lda     LINE_NUM+1
            jsr     WRITE_TO_PRIMARY_HEAP
            jmp     EINR_DONE
EINR_EXISTING_SYM:
            bmi     EINR_TYPE_ERR       ; N=1 -> incompatible type -> error
            bvs     EINR_MACRO_REF      ; V=1 -> is macro -> special treatment
            jsr     RECORD_XREF         ; Register symbol xref
            lda     INSTR_FLAGS
            ora     #$08                ; Set "16-bit" bit in flags
            sta     (SYMTBLP),Y         ; Update control byte in place
EINR_WRITE_VALUE:
            iny
            lda     SYMVAL
            sta     (SYMTBLP),Y         ; Update value lo
            iny
            lda     SYMVAL+1
            sta     (SYMTBLP),Y         ; Value mid
            iny
            lda     SYMVAL+2
            sta     (SYMTBLP),Y         ; Value hi
EINR_DONE:
            ldy     MACDEF_Y_SAVE       ; Restore Y
            rts
EINR_MACRO_REF:
            jsr     RECORD_XREF
            bit     RELADDR_FLAG        ; RELADDR_FLAG?
            bpl     EINR_TYPE_ERR       ; Not relative -> type error
            lda     ENTRY_TYPE_FLAGS
            and     #$20                ; "Already resolved" bit?
            bne     EINR_DONE           ; Yes -> OK
            bit     PASSFLG
            bmi     EINR_WRITE_VALUE    ; Pass 2 -> write value
EINR_TYPE_ERR:
            ldy     #$00
            jsr     ERROR_DUPLICATE
            ; Does not return

; ============================================================================
; PARSE_ADDRMODE
;
; Reads the token at (LINE_BUF,Y) and determines the 6502 addressing mode,
; storing the index in ADDR_MODE. Calls EVAL_FULL_EXPR to evaluate
; the operand when necessary.
;
; Detected modes and their indices:
;   $00  accumulator/implied     (A only)
;   $01  immediate               (#expr)
;   $02  zero-page / absolute    (expr)
;   $05  indexed indirect X      ((expr,X))
;   $06  indirect indexed Y      ((expr),Y)
;   $07  absolute,X
;   $08  absolute,Y
;   $0A  indirect pure           ((expr))
; ============================================================================
PARSE_ADDRMODE:
            cmp     #'#'                ; Immediate (#expr)?
            bne     PADDR_CHK_ACC
            ldx     #$01                ; Mode 1 = immediate
            jsr     EVAL_EXPR_ADVANCE   ; Advance Y and evaluate expression
            jmp     PADDR_CHECK_END
PADDR_SYNTAX_ERR:
            jsr     ERROR_SYNTAX
            ; Does not return
PADDR_CHK_ACC:
            cmp     #'A'                ; Accumulator (A)?
            bne     PADDR_CHK_PAREN
            jsr     ADVANCE_PEEK_BLANK  ; Advance Y and check if blank
            bne     PADDR_CHK_OPT_REG   ; Not end -> might be identifier, not A mode
            ldx     #$00                ; Yes end -> mode 0 (accumulator)
            beq     PADDR_STORE_MODE
PADDR_CHK_PAREN:
            cmp     #'('                ; Open parenthesis (indirect mode)?
            bne     PADDR_EVAL_EXPR
            jsr     EVAL_EXPR_ADVANCE   ; Evaluate expression inside ()
            cmp     #')'                ; Immediate close -> (expr)?
            bne     PADDR_CHK_INDIRECT_IDX
            jsr     ADVANCE_PEEK_BLANK  ; Check what follows ')' 
            bne     PADDR_CHK_INDY      ; Is there ',Y'?
            ldx     #$0a                ; No -> mode $0A (pure indirect)
            bne     PADDR_STORE_MODE
PADDR_CHK_INDIRECT_IDX:
            cmp     #','
            bne     PADDR_SYNTAX_ERR
            iny
            lda     LINE_BUF,Y
            cmp     #'X'
            bne     PADDR_SYNTAX_ERR
            iny
            lda     LINE_BUF,Y
            cmp     #')'
            bne     PADDR_SYNTAX_ERR
            ldx     #$05                ; Mode $05 = (zp,X)
PADDR_INDIRECT_SET_MODE:
            bit     URESFLG             ; Unresolved reference?
            bvc     PADDR_ADV_CHAR
            jsr     ERROR_ZERO_PAGE     ; Error: reference in indirect mode
PADDR_ADV_CHAR:
            iny                         ; Advance past ')'
PADDR_CHECK_END:
            jsr     PEEK_BLANK_CHAR     ; Check that nothing is left
            bne     PADDR_SYNTAX_ERR    ; More text exists -> error
PADDR_STORE_MODE:
            stx     ADDR_MODE           ; ADDR_MODE = detected mode
            rts
PADDR_CHK_INDY:
            cmp     #','
            bne     PADDR_SYNTAX_ERR
            iny
            lda     LINE_BUF,Y
            ldx     #$06                ; Mode $06 = (zp),Y
            cmp     #'Y'
            beq     PADDR_INDIRECT_SET_MODE
            bne     PADDR_SYNTAX_ERR
PADDR_CHK_OPT_REG:
            dey                         ; Rewind: 'A' belongs to an identifier
PADDR_EVAL_EXPR:
            jsr     EVAL_FULL_EXPR      ; Evaluate operand expression
            ldx     #$02                ; Assume zero-page/absolute mode
            bit     URESFLG             ; Unresolved (external reference)?
            bvc     PADDR_CHK_INDEX     ; V=0 -> resolved: keep mode 2
            ldx     #$07                ; V=1 -> force absolute mode ($07)
PADDR_CHK_INDEX:
            jsr     PEEK_BLANK_CHAR     ; is there more?
            beq     PADDR_STORE_MODE    ; No -> direct mode without index
            inx                         ; x: 2->3 (zpX) or 7->8 (absX)
            cmp     #','
            bne     PADDR_SYNTAX_ERR
            iny
            lda     LINE_BUF,Y
            cmp     #'X'               ; ',X'?
            beq     PADDR_ADV_CHAR
            inx                         ; x: 3->4 (zpY) or 8->9 (absY)
            cmp     #'Y'               ; ',Y'?
            beq     PADDR_ADV_CHAR
            jmp     PADDR_SYNTAX_ERR

; ============================================================================
; READ_NEXT_LINE
;
; Main source line reader. In normal mode, reads bytes from the channel
; buffer loaded in TEMPP (via READ_BLOCK_SVC) and copies them into the
; line buffer LINE_BUF. When inside a macro expansion (bit 7 of
; MACRO_EXPANDING set), delegates to the macro body reader.
; Handles:
;   - Line continuation (TAB character = TABCHAR) via EXPAND_TAB.
;   - Overly long lines (> $50 chars): activates LINE_CONTINUE.
;   - Line number increment and listing header clearing.
;   - Updating the buffer pointer SRC_BUF_OFF.
; ============================================================================
READ_NEXT_LINE:
            bit     MACRO_EXPANDING     ; MACRO_EXPANDING?
            bpl     RNL_NORMAL_READ     ; No -> read from source file
            jmp     READ_MACRO_BODY_LINE ; Read from macro body in memory
RNL_NORMAL_READ:
            ldy     SRC_BUF_OFF         ; Current buffer position
            bit     LINE_CONTINUE       ; Are we part of a continued line?
            bpl     RNL_NEW_LINE_READ   ; No -> new normal line
            ; Continuation: prepend comment character as header
            lda     COMMENT             ; Comment character (';')
            sta     LINE_BUF            ; First char = continuation mark
            jsr     CLEAR_LISTING_HEADER ; Clear listing header
            ldx     #$01                ; Start filling LINE_BUF from index 1
            jmp     RNL_CHAR_LOOP
RNL_NEW_LINE_READ:
            jsr     INCREMENT_LINE_COUNTER ; Increment line number in listing
            ldx     #$00                ; Start filling LINE_BUF from index 0
RNL_CHAR_LOOP:
            lda     (TEMPP),Y           ; Read byte from source buffer
            cmp     #' '                ; Control char (< space)?
            bcc     RNL_CHK_CR          ; Yes -> check if it is CR
            sta     LINE_BUF,X          ; Store in LINE_BUF
            inx
            cpx     #$51                ; Line too long (> 80 chars)?
            bcs     RNL_LINE_TOO_LONG
RNL_CHAR_ADVANCE:
            iny
            bne     RNL_CHK_BUF_END
            inc     TEMPP+1             ; Cross page boundary
RNL_CHK_BUF_END:
            cpy     LINE_BUF_END        ; Reached end of loaded buffer (low byte)?
            bne     RNL_CHAR_LOOP
            lda     TEMPP+1
            cmp     LINE_BUF_END+1      ; End of buffer (high byte)?
            bne     RNL_CHAR_LOOP
            jsr     READ_BLOCK_SVC      ; Load next block from channel
            bit     INCLUDE_POP         ; INCLUDE_POP: return signal from include?
            bpl     RNL_CHAR_LOOP       ; No -> continue
            bmi     RNL_TERMINATE_LINE  ; Yes -> terminate line
RNL_CHK_CR:
            cmp     #$0d                ; Carriage return?
            bne     RNL_CHK_TAB         ; No -> check tab
            lda     #$00
            sta     LINE_CONTINUE       ; End of continuation
RNL_ADVANCE_PAST_CR:
            iny                         ; Advance past CR
            sty     SRC_BUF_OFF         ; Update SRC_BUF_OFF with current position
            bne     RNL_CHK_BUF_AFTER_CR
            inc     TEMPP+1
RNL_CHK_BUF_AFTER_CR:
            cpy     LINE_BUF_END        ; Buffer depleted after CR?
            bne     RNL_TERMINATE_LINE
            lda     TEMPP+1
            cmp     LINE_BUF_END+1
            bne     RNL_TERMINATE_LINE
            jsr     READ_BLOCK_SVC      ; Load more data from channel
RNL_TERMINATE_LINE:
            lda     #$00
            sta     LINE_BUF,X          ; NUL at end of line in LINE_BUF
            stx     LINE_BUF_LEN        ; LINE_BUF_LEN = number of copied chars
            rts
RNL_CHK_TAB:
            cmp     TABCHAR             ; TAB character (line continuation)?
            bne     RNL_CHAR_ADVANCE    ; No -> advance normally
            jsr     EXPAND_TAB          ; Expand TAB to spaces in LINE_BUF
            jmp     RNL_CHAR_ADVANCE
RNL_LINE_TOO_LONG:
            sec
            ror     LINE_CONTINUE       ; Line too long
            jmp     RNL_ADVANCE_PAST_CR ; Terminate as if there was a CR

; ============================================================================
; READ_MACRO_BODY_LINE
;
; Line reader for macro expansion. Reads macro body bytes via
; READ_MACRO_BODY_BYTE and copies them to LINE_BUF.
; When it finds the body CR ($0D) terminator, terminates the line.
; The parameter escape character (ARGLEADIN, usually '!') invokes
; SUBSTITUTE_MACRO_PARAM to substitute the argument.
; If READ_MACRO_BODY_BYTE returns 0 (end of block), calls
; POP_MACRO_CALL_FRAME to restore the previous frame:
;   - If C set (empty stack): return to file reading mode.
;   - If C clear: parent frame exists -> continue reading parent macro.
; ============================================================================
READ_MACRO_BODY_LINE:
            jsr     CLEAR_LISTING_HEADER ; Clear listing header
            ldx     #$00
RMBL_CHAR_LOOP:
            jsr     READ_MACRO_BODY_BYTE ; Read body byte
            bne     RMBL_CHK_CR         ; Not NUL -> process char
            jsr     POP_MACRO_CALL_FRAME ; End of block -> restore frame
            asl     MACRO_EXPANDING     ; MACRO_EXPANDING: rotate to clear/detect
            bmi     RMBL_CHAR_LOOP      ; C=1 (more frames exist) -> continue
            jmp     RNL_NORMAL_READ     ; C=0 (empty stack) -> return to normal read
RMBL_CHK_CR:
            cmp     #$0d                ; CR (end of line in body)?
            beq     RMBL_END_LINE
            cmp     ARGLEADIN           ; Parameter escape ('!')?
            bne     RMBL_CHK_PRINTABLE
            jsr     SUBSTITUTE_MACRO_PARAM ; Substitute argument
            jmp     RMBL_NEXT_CHAR
RMBL_CHK_PRINTABLE:
            cmp     #' '                ; Printable char?
            bcs     RMBL_STORE_CHAR     ; Yes -> store
            cmp     TABCHAR             ; TAB?
            bne     RMBL_NEXT_CHAR
            jsr     EXPAND_TAB          ; Expand TAB
            jmp     RMBL_NEXT_CHAR
RMBL_STORE_CHAR:
            sta     LINE_BUF,X          ; Store char in LINE_BUF
            inx
            cpx     #$51                ; Line too long?
            bcs     RMBL_SKIP_OVERFLOW
RMBL_NEXT_CHAR:
            jsr     READ_MACRO_BODY_BYTE ; Read next byte
            jmp     RMBL_CHK_CR
RMBL_END_LINE:
            stx     LINE_BUF_LEN        ; LINE_BUF_LEN = copied chars
            lda     #$00
            sta     LINE_BUF,X          ; NUL at end
            rts
RMBL_SKIP_OVERFLOW:
            jsr     READ_MACRO_BODY_BYTE ; Keep reading until CR
            cmp     #$0d
            bne     RMBL_SKIP_OVERFLOW
            jmp     RMBL_END_LINE

; ============================================================================
; SUBSTITUTE_MACRO_PARAM
;
; Called when the parameter escape character (ARGLEADIN, '!') is found
; while reading a macro body.
; The following character determines the substitution type:
;   '1'..'9' : argument 1..9 -> LOOKUP_MACRO_PARAM + copy
;   'A'..'Z' : argument 10..35 (A=10, B=11, ...)
;   '!!'     : a single literal '!'
;   '!#'     : number of arguments of current call (ASCII decimal)
;   '!@'     : current local label counter (NEXT_LOCAL, 4 chars)
; If the character is none of the above, emits literal '!'.
; ============================================================================
SUBSTITUTE_MACRO_PARAM:
            jsr     READ_MACRO_BODY_BYTE ; Read the character after '!'
            beq     SMPARAM_LITERAL_ESC ; NUL -> emit literal '!'
            cmp     #'1'
            bcc     SMPARAM_CHK_SPECIAL ; < '1' -> might be '!' or other specials
            cmp     #'9'+1
            bcs     SMPARAM_CHK_ALPHA   ; > '9' -> check if it is A-Z
            sec
            sbc     #'0'                ; Convert '1'..'9' to index 1..9
            jmp     SMPARAM_FOUND_IDX
SMPARAM_CHK_ALPHA:
            cmp     #'A'
            bcc     SMPARAM_LITERAL_ESC ; < 'A' -> literal
            cmp     #'Z'+1
            bcs     SMPARAM_LITERAL_ESC ; > 'Z' -> literal
            sec
            sbc     #$37                ; Convert 'A'=10, 'B'=11, ..., 'Z'=35
SMPARAM_FOUND_IDX:
            jsr     LOOKUP_MACRO_PARAM  ; Get pointer to Nth argument
            beq     SMPARAM_DONE        ; Empty argument -> do not copy anything
            jsr     READ_ARG_BYTE       ; Read first byte of argument
SMPARAM_COPY_LOOP:
            sta     LINE_BUF,X          ; Copy byte to LINE_BUF
            inx
            cpx     #$51                ; LINE_BUF full?
            bcs     SMPARAM_DONE
            jsr     READ_ARG_BYTE       ; Read next byte
            bne     SMPARAM_COPY_LOOP   ; Not NUL -> continue copying
SMPARAM_DONE:
            rts
SMPARAM_LITERAL_ESC:
            lda     ARGLEADIN           ; Escape character ('!')
            sta     LINE_BUF,X
            inx
            rts
SMPARAM_CHK_SPECIAL:
            cmp     ARGLEADIN           ; Second '!'?
            bne     SMPARAM_CHK_ARG_COUNT
            jsr     READ_MACRO_BODY_BYTE ; Read char after '!!'
            cmp     ARGLEADIN           ; '!!@' -> retract cursor?
            beq     SMPARAM_EMIT_LOCAL  ; Yes -> emit local label counter
            ; '!!' -> retract pointer by 1 (read less)
            lda     OPCTBLP
            bne     SMPARAM_RETRACT_PTR
            dec     OPCTBLP+1
SMPARAM_RETRACT_PTR:
            dec     OPCTBLP             ; Decrement body read pointer
            jmp     SMPARAM_LITERAL_ESC ; Emit literal '!'
SMPARAM_EMIT_LOCAL:
            lda     NEXT_LOCAL          ; NEXT_LOCAL[0]
            sta     LINE_BUF,X
            inx
            lda     NEXT_LOCAL+1        ; NEXT_LOCAL[1]
SMPARAM_EMIT_LOCAL_MID:
            sta     LINE_BUF,X
SMPARAM_EMIT_LOCAL_LO3:
            inx
            lda     NEXT_LOCAL+2        ; NEXT_LOCAL[2]
            sta     LINE_BUF,X
            inx
            lda     NEXT_LOCAL+3        ; NEXT_LOCAL[3]
            cmp     #' '                ; Space? (padding field, do not emit)
            beq     SMPARAM_EMIT_DONE
            sta     LINE_BUF,X
            inx
SMPARAM_EMIT_DONE:
            rts
SMPARAM_CHK_ARG_COUNT:
            cmp     ARGCOUNT            ; '#'?
            bne     SMPARAM_LITERAL_ESC ; No -> literal '!'
            lda     MACRO_ARG_CNT
            cmp     #$0a                ; >= 10?
            bcc     SMPARAM_UNITS_DIGIT ; No -> a single digit
            lda     #'1'                ; Tens: start with '1'
            sta     LINE_BUF,X
            lda     MACRO_ARG_CNT
SMPARAM_TENS_LOOP:
            sec
            sbc     #$0a                ; Subtract 10 until negative
            bcc     SMPARAM_UNITS_DIGIT2
            inc     LINE_BUF,X          ; Increment tens digit
            jmp     SMPARAM_TENS_LOOP
SMPARAM_UNITS_DIGIT2:
            inx                         ; Advance to units digit
SMPARAM_UNITS_DIGIT:
            clc
            adc     #'0'                ; Convert remainder to ASCII
            sta     LINE_BUF,X
            inx
            rts

; ============================================================================
; INCREMENT_LINE_COUNTER
;
; Increments the 4-digit ASCII line number counter at ADDRESS_FIELD
; (listing field). Each digit is incremented with carry propagation.
; Also increments the binary counter LINE_NUM/LINE_NUM+1.
; If the LIST_SUPPRESS flag is active, skips the increment but
; still executes the listing header clearing.
; After incrementing, falls through to CLEAR_LISTING_HEADER to initialize
; the value/bytes fields of the listing line.
; ============================================================================
INCREMENT_LINE_COUNTER:
            bit     LIST_SUPPRESS       ; LIST_SUPPRESS: do not increment line number?
            bmi     CLEAR_LISTING_HEADER ; Yes -> skip increment, clear only
            ldx     #$03
ILC_DIGIT_LOOP:
            lda     #'9'
            inc     ADDRESS_FIELD,X     ; Increment ASCII digit
            cmp     ADDRESS_FIELD,X     ; Overflow (>'9')?
            bcs     ILC_LINE_DONE       ; No -> ready
            lda     #'0'
            sta     ADDRESS_FIELD,X     ; Reset digit and propagate carry
            dex
            bpl     ILC_DIGIT_LOOP
ILC_LINE_DONE:
            inc     LINE_NUM
            bne     CLEAR_LISTING_HEADER
            inc     LINE_NUM+1

; ============================================================================
; CLEAR_LISTING_HEADER
;
; Initializes with spaces the 7 bytes of the address/value field in the
; listing line header (VALUE_FIELD+4 .. VALUE_FIELD+4+6). This clears the hex
; digits from the previous line before assembling the new one.
; ============================================================================
CLEAR_LISTING_HEADER:
            lda     #' '
            ldx     #$06
CLH_CLEAR_LOOP:
            sta     VALUE_FIELD+4,X     ; Fill with spaces
            dex
            bpl     CLH_CLEAR_LOOP
            rts

; ============================================================================
; EXPAND_TAB
;
; Expands the TAB character in LINE_BUF. Uses the global tabulation
; table (TABTBL) to determine the next tab stop.
; Substitutes the TAB with spaces from the current column (X) to the
; next stop. If there is no valid entry in the table, adds one space.
; ============================================================================
EXPAND_TAB:
            inx                         ; Advance column (discard the TAB)
            stx     TAB_COL               ; TAB_COL = current column
            ldx     #$00
TAB_SCAN_TABLE:
            lda     TABTBL,X            ; Read entry from tabulation table
            beq     TAB_FALLBACK        ; 0 = end of table -> use one space
            cmp     TAB_COL               ; Stop <= current column?
            beq     TAB_ADVANCE_STOP    ; Yes (equal) -> search next
            bcc     TAB_ADVANCE_STOP    ; Yes (less)
            cmp     #$50                ; Stop > max column ($50)?
            bcs     TAB_FALLBACK
            tax                         ; X = next stop
            dex
            lda     TAB_COL
            stx     TAB_COL             ; TAB_COL = next stop − 1
            tax
            dex
TAB_FILL_SPACES:
            lda     #' '
            sta     LINE_BUF,X          ; Fill with spaces up to the stop
            inx
            cpx     TAB_COL
            bne     TAB_FILL_SPACES
            beq     TAB_DONE
TAB_ADVANCE_STOP:
            inx
            cpx     #$20                ; Table limit exceeded?
            bne     TAB_SCAN_TABLE
TAB_FALLBACK:
            ldx     TAB_COL             ; Current column
            dex
            lda     #' '
            sta     LINE_BUF,X          ; At least one space
            inx
TAB_DONE:
            rts

; ============================================================================
; OPEN_CHANNEL
;
; Assigns a CODOS channel to the file described by the name register at
; SRC_FNAME_SLOT (14 bytes) via SVC #$15. If assignment fails (bit 7 set),
; calls the error handler REPORT_ERROR.
; After assignment, prints the message "SOURCE FILE IS " followed by the
; filename (up to and including the extension dot) on channel 2.
; ============================================================================
OPEN_CHANNEL:
            lda     #<SRC_FNAME_SLOT
            sta     U3                  ; U3 = pointer to name register
            sta     U6
            lda     #>SRC_FNAME_SLOT
            sta     U3+1
            sta     U6+1
            lda     SRC_DRIVE           ; Source drive number
            ldx     SRC_CHANNEL         ; Source channel number
            svc     $15                 ; SVC: assign channel to file
            bmi     OCH_PRINT_MSG       ; Error (bit 7) -> print error message
            jsr     REPORT_ERROR        ; Channel error handler
OCH_PRINT_MSG:
            svc     2                   ; SVC: inline message on channel 2
            .byte   $02                 ; Channel 2
            .byte   $0d, "SOURCE FILE IS ", 0
            ldy     #$00
OCH_FIND_EXT:
            lda     SRC_FNAME_SLOT,Y
            iny
            cmp     #'.'
            bne     OCH_FIND_EXT
            iny                         ; Advance past the extension as well
            ldx     #$02                ; Channel 2
            svc     7                   ; SVC: emit string on channel
            rts

; ============================================================================
; OPEN_OBJ_CHANNEL
;
; Assigns the object code output channel (OBJ_CHANNEL) to the
; object file whose name is at OBJ_FNAME_SLOT (channel 2 register),
; using the drive OBJ_DRIVE.
; Falls through to ASSIGN_CHANNEL to execute SVC $15.
; ============================================================================
OPEN_OBJ_CHANNEL:
            lda     #<OBJ_FNAME_SLOT             ; Object filename register
            sta     U3
            lda     #>OBJ_FNAME_SLOT
            sta     U3+1
            ldx     OBJ_CHANNEL         ; Object channel for SVC (use in ASSIGN_CHANNEL)
            lda     OBJ_DRIVE           ; Object drive for SVC
            jmp     ASSIGN_CHANNEL      ; Execute assignment

; ============================================================================
; OPEN_LST_CHANNEL
;
; Assigns the listing channel (LST_CHANNEL) to the listing file
; whose name is at LST_FNAME_SLOT (channel 1 register), using LST_DRIVE.
; ============================================================================
OPEN_LST_CHANNEL:
            lda     #<LST_FNAME_SLOT    ; Listing filename register
            sta     U3
            lda     #>LST_FNAME_SLOT
            sta     U3+1
            ldx     LST_CHANNEL         ; Listing channel for SVC
            lda     LST_DRIVE           ; Listing drive for SVC
ASSIGN_CHANNEL:
            svc     $15                 ; SVC: assign channel to file
            bpl     ASSIGN_DONE         ; Success
            jsr     FATAL_OUTPUT_FILE_EXISTS
            ; Does not return
ASSIGN_DONE:
            rts

; ============================================================================
; OPEN_ERR_CHANNEL
;
; Assigns the error/console channel (CON_CHANNEL) to the error
; file whose name is at ERR_FNAME_SLOT (channel 3 register), with drive ERR_DEVICE.
; Only does so if ERR_DEVICE < 4 (is a disk drive, not a device).
; If ERR_DEVICE >= 4, returns without assigning (errors will go to the default
; device).
; ============================================================================
OPEN_ERR_CHANNEL:
            lda     #<ERR_FNAME_SLOT    ; Error filename register
            sta     U3
            lda     #>ERR_FNAME_SLOT
            sta     U3+1
            ldx     CON_CHANNEL         ; Conole channel
            lda     ERR_DEVICE          ; Error device
            cmp     #$04                ; Is it a disk drive (< 4)?
            bcs     ASSIGN_DONE         ; No (device) -> do not assign channel
            svc     $15                 ; SVC: assign channel to file
            bpl     ASSIGN_DONE
            jsr     FATAL_OUTPUT_FILE_EXISTS
            ; Does not return

; ============================================================================
; READ_BLOCK_SVC
;
; Fills the source buffer (SRCPRBUF) with the next block of data from the
; source input channel (SRC_CHANNEL) via SVC $0F.
;
; Entry:  X = preserved (saved and restored internally)
; Exit:   TEMPP         -> start of source buffer (= SRCST)
;         LINE_BUF_END  -> one-past-last byte actually read into buffer
;         SRC_BUF_OFF   = 0 (reset to start of new block)
;         LIST_SUPPRESS = 0 (reset)
;         INCLUDE_POP bit 7: set if end-of-file was signalled by SVC (carry in)
; ============================================================================
READ_BLOCK_SVC:
            stx     READNB_X_SAVE       ; Save X (will be restored at exit)
            lda     SRCST               ; Source buffer base address (low)
            sta     TEMPP               ; TEMPP -> start of source buffer
            sta     U1                  ; U1 also -> start (SVC $0F in-parameter)
            lda     SRCST+1
            sta     TEMPP+1
            sta     U1+1
            cld                         ; Ensure binary arithmetic mode
            lda     SRCEND              ; Compute available buffer size:
            sec                         ;   SRCEND − SRCST = max bytes to read
            sbc     SRCST
            sta     U2                  ; U2 (lo) = buffer size in bytes
            lda     SRCEND+1
            sbc     SRCST+1
            sta     U2+1                ; U2 (hi)
            jsr     DO_SVC_READ         ; Perform the actual SVC $0F read;
                                        ;   on return U1 points past last byte read
                                        ;   carry set = end of file reached
            ror     INCLUDE_POP         ; Shift carry into bit 7 of INCLUDE_POP:
                                        ;   $80 = EOF reached -> caller must pop include frame
            lda     U1                  ; SVC updates U1 to one-past-end of data read
            sta     LINE_BUF_END        ; Store as the source buffer end pointer
            lda     U1+1
            sta     LINE_BUF_END+1
            ldy     #$00
            sty     LIST_SUPPRESS       ; Clear listing-suppress flag for new block
            sty     SRC_BUF_OFF         ; Reset SRC_BUF_OFF (byte offset within block)
            ldx     READNB_X_SAVE       ; Restore X
            rts

; ============================================================================
; DO_SVC_READ
;
; Wrapper around SVC $0F ("Read a record from a channel") that preserves the
; X register.  Loads the source channel number from SRC_CHANNEL into
; X for the SVC call, then restores the caller's X and relays the processor
; status flags (in particular carry = EOF) back to the caller.
;
; Entry:  X = caller's X (saved)
;         U1 = destination buffer base
;         U2 = buffer size
; Exit:   U1 = one-past-last byte read
;         carry set if end-of-file
; ============================================================================
DO_SVC_READ:
            stx     READBI_X_SAVE       ; Save caller's X
            ldx     SRC_CHANNEL         ; Load source channel number for SVC
            svc     $0f                 ; SVC $0F: read record from channel into (U1), size U2
            php                         ; Preserve flags (carry = EOF indicator)
            ldx     READBI_X_SAVE       ; Restore caller's X
            plp                         ; Restore flags
            rts

; ============================================================================
; SETUP_DDEF_READ_MODE
;
; Redirects the source-line read pointers so that the assembler's inner loop
; reads from the direct-definitions buffer (DDEFSBUF) instead of the normal
; source program buffer (SRCPRBUF).
;
; This is called when the command-line "==" direct-definition option is active
; (DIRECT_DEFS_ACT = $80), so the directly-defined symbols are processed as
; if they were the first lines of the source file.
;
; Entry: DDEFS_END = one-past-last byte in DDEFSBUF
; Exit:  LINE_BUF_END  -> DDEFSBUF end pointer
;        TEMPP         -> start of DDEFSBUF
;        SRC_BUF_OFF   = 0
;        LIST_SUPPRESS = $80 (suppress listing output for these synthetic lines)
; ============================================================================
SETUP_DDEF_READ_MODE:
            lda     DDEFS_END           ; End of direct-definitions buffer
            sta     LINE_BUF_END        ; Set source end pointer (lo) to DDEFSBUF end
            lda     DDEFS_END+1
            sta     LINE_BUF_END+1
            lda     #<DDEFSBUF          ; Point TEMPP at the start of DDEFSBUF
            sta     TEMPP
            lda     #>DDEFSBUF
            sta     TEMPP+1
            lda     #$00
            sta     SRC_BUF_OFF         ; Reset SRC_BUF_OFF to beginning of buffer
            sec
            ror     LIST_SUPPRESS       ; Set LIST_SUPPRESS ($80): suppress listing for defs
            rts

; ============================================================================
; EMIT_LISTING_LINE_PG_CHK
;
; Entry point for listing-line output that first checks whether the current
; listing page is full.  If PAGE_LINE_CNT has reached LISTLINESPP (lines per
; page), calls OUTPUT_LINE_PG_BRK to emit a page break and header before
; falling through to OUTPUT_LISTING_LINE.
;
; Passing through to OUTPUT_LISTING_LINE when LISTLINESPP = 0 disables
; automatic pagination entirely.
;
; Entry:  Y = line length (number of bytes to output from (U6))
;         U6 = pointer to the listing line buffer
; ============================================================================
EMIT_LISTING_LINE_PG_CHK:
            lda     LISTLINESPP         ; Lines-per-page limit (0 = unlimited)
            beq     OUTPUT_LISTING_LINE ; 0 -> no pagination; go straight to output
            lda     PAGE_LINE_CNT       ; Current line count on this page
            cmp     LISTLINESPP         ; Full?
            bcc     OUTPUT_LISTING_LINE ; No: output line directly
            jsr     OUTPUT_LINE_PG_BRK  ; Yes: emit page break + new page header
            ; Fall through to OUTPUT_LISTING_LINE

; ============================================================================
; OUTPUT_LISTING_LINE
;
; Emits Y bytes starting at (U6) as one listing line.
;
; Two output paths depending on the listing device type (LST_DRIVE):
;   Disk-based listing (LST_DRIVE < 4):
;     Bytes are copied one by one into the in-memory listing buffer
;     (LSTOUTP..LSTEND).  When the buffer is full, FLUSH_OBJ_BUF is called
;     to drain it to the listing channel via SVC $10.
;   Device-based listing (LST_DRIVE >= 4):
;     The entire line is sent directly via SVC #7 on channel LST_CHANNEL.
;
; After output, PAGE_LINE_CNT is incremented by one.
; If Y = 0 on entry the routine returns immediately (nothing to output).
;
; Entry:  Y = byte count to write
;         U6 = pointer to data to write
;         LST_CHANNEL = listing channel number
;         LST_DRIVE = listing drive/device code
; ============================================================================
OUTPUT_LISTING_LINE:
            cpy     #$00                ; Nothing to output?
            beq     @DONE               ; Yes: return immediately
            lda     LST_DRIVE           ; Listing drive number
            cmp     #$04                ; Is it a disk drive (< 4)?
            bcc     @ISDRIVE            ; Yes: use in-memory buffer path
            ldx     LST_CHANNEL         ; No: use direct SVC output
            svc     7                   ; SVC $07: output Y bytes from (U6) on channel X
            jmp     @INCLINE            ; Skip in-memory path; go increment line counter
@ISDRIVE:   ldx     #$00                ; X = 0 (used as index-register zero for indirect)
            sty     LIST_LINE_LEN       ; Save byte count
            ldy     #$00
@READB:     lda     (U6),Y              ; Read byte from source listing line
            sta     (LSTOUTP,X)         ; Store into listing output buffer at LSTOUTP
            inc     LSTOUTP             ; Advance buffer write pointer
            bne     @CONT               ; No page boundary: continue
            inc     LSTOUTP+1           ; Crossed page boundary: increment high byte
            lda     LSTOUTP+1
            cmp     LSTEND+1            ; Has LSTOUTP reached LSTEND?
            bne     @CONT               ; No: continue filling
            jsr     FLUSH_OBJ_BUF       ; Buffer full: flush LSTST..LSTOUTP to disk
            ldx     #$00                ; Re-initialise X after call (may be clobbered)
@CONT:      iny                         ; Advance source pointer
            cpy     LIST_LINE_LEN       ; Reached byte count?
            bne     @READB              ; No: continue
@INCLINE:   inc     PAGE_LINE_CNT       ; One more line on this page
@DONE:      rts

; ============================================================================
; OUTPUT_LINE_PG_BRK
;
; Issues a page break before starting a new listing page.  Called by
; EMIT_LISTING_LINE_PG_CHK when PAGE_LINE_CNT >= LISTLINESPP.
;
; Actions:
;   1. Saves U6 and Y (via OUTPGB_Y_SAVE) so the caller's state is preserved.
;   2. Calls INCREMENT_PAGE_COUNTER to advance the 3-digit page number
;      in the page header at PAGE_NUMBER.
;   3. Resets PAGE_LINE_CNT to 1.
;   4. Points U6 at the page header buffer PAGE_HEADER (length $52 = 82 bytes).
;   5. If LISTING_ON is set, emits the header via OUTPUT_LISTING_LINE:
;       - On the very first page (FIRST_FF_FLAG = 0) the form-feed byte at
;         PAGE_FF is temporarily cleared so no FF is emitted before the header.
;       - For subsequent pages, PAGE_FF holds $0C (form-feed) which goes out
;         as part of the header and is restored for next time.
;   6. Restores U6 and Y and returns.
; ============================================================================
OUTPUT_LINE_PG_BRK:
            sty     OUTPGB_Y_SAVE       ; Save Y (listing line length)
            lda     U6                  ; Save current listing line pointer (lo)
            pha
            lda     U6+1
            pha
            jsr     INCREMENT_PAGE_COUNTER ; Increment the 3-digit page counter at PAGE_NUMBER
            lda     #$01
            sta     PAGE_LINE_CNT       ; Reset line counter to 1 for new page
            lda     #<PAGE_HEADER       ; Point U6 at the page header buffer
            sta     U6
            lda     #>PAGE_HEADER
            sta     U6+1
            ldy     #$52                ; Page header is $52 (82) bytes long
            bit     LISTING_ON          ; Listing active?
            bpl     @DONE               ; No: skip output; just restore registers
            bit     FIRST_FF_FLAG       ; First page flag: $00 = suppress initial FF
            bpl     @OUT_LINE           ; $00 -> not first page: skip
            lda     #$00                ; first page: Clear the FF byte
            sta     PAGE_FF             ; Suppress form-feed for the very first page
@OUT_LINE:  jsr     OUTPUT_LISTING_LINE ; Output the page header line
            bit     FIRST_FF_FLAG       ; Re-check: were we on the first page?
            bpl     @DONE               ; Yes: done (FF already suppressed)
            lda     #$0c                ; Restore form-feed ($0C) at PAGE_FF for next page
            sta     PAGE_FF
            lda     #$00
            sta     FIRST_FF_FLAG       ; Mark: first page has been emitted; future pages get FF
@DONE:      pla                         ; Restore U6 (hi)
            sta     U6+1
            pla                         ; Restore U6 (lo)
            sta     U6
            ldy     OUTPGB_Y_SAVE       ; Restore Y
            rts

; ============================================================================
; INCREMENT_PAGE_COUNTER
;
; Increments the 3-character decimal page number stored at PAGE_NUMBER (3 chars,
; least-significant digit at highest address).  Treats space ($20) as '0'
; so blank placeholders are promoted on the first increment.
; Handles carry propagation across all three digits.
;
; Entry: PAGE_NUMBER[0..2] = ASCII page number characters
; Exit:  PAGE_NUMBER[0..2] updated in place
; ============================================================================
INCREMENT_PAGE_COUNTER:
            ldx     #$02                ; Start from the units digit (index 2)
@LOOP:      lda     PAGE_NUMBER,X       ; Load current digit
            cmp     #' '                ; Blank placeholder?
            bne     @CONT
            lda     #'0'                ; Treat space as '0' before incrementing
@CONT:      clc
            adc     #$01                ; Increment digit
            sta     PAGE_NUMBER,X
            cmp     #'9'+1              ; Did it overflow past '9'?
            bcc     @DONE               ; No: done
            lda     #'0'                ; Yes: wrap digit back to '0'
            sta     PAGE_NUMBER,X
            clc
            dex                         ; Move to next more-significant digit
            bpl     @LOOP               ; Loop while digits remain
@DONE:      rts

; ============================================================================
; FLUSH_OBJ_BUF
;
; Flushes the in-memory listing line accumulation buffer (LSTST..LSTOUTP-1)
; to the listing output channel (LST_CHANNEL) via SVC $10, then resets
; LSTOUTP back to LSTST ready for the next batch.
;
; The flush is skipped if:
;   - SRC_DRIVE >= 4  (no disk-based source, so listing is not file-based)
;   - The listing channel (LST_CHANNEL) is not currently assigned to a file  (SVC $0E
;     returns carry clear)
;   - The channel's device code is >= 4  (it is a non-disk device)
;
; Called from OUTPUT_LISTING_LINE when the in-memory buffer is full, and
; during shutdown to drain any remaining bytes.
; ============================================================================
FLUSH_OBJ_BUF:
            lda     SRC_DRIVE           ; Source drive: current active drive number
            cmp     #$04                ; Is it a disk drive (< 4)?
            bcs     @RETURN             ; No: skip flush (not file-based listing)
            ldx     LST_CHANNEL         ; Listing channel for SVC
            svc     $0e                 ; SVC $0E: query channel assignment;
                                        ;   carry set = channel assigned to a file,
                                        ;   A = device/drive code of that file
            bcc     @RETURN             ; Not assigned: skip
            cmp     #$04                ; Is the listing channel on a disk drive?
            bcs     @RETURN             ; No: skip
            lda     LSTST               ; Set U1 = start of listing buffer
            sta     U1
            lda     LSTST+1
            sta     U1+1
            lda     LSTOUTP             ; Compute byte count = LSTOUTP − LSTST
            sec
            sbc     LSTST
            sta     U2                  ; U2 (lo) = number of bytes to write
            lda     LSTOUTP+1
            sbc     LSTST+1
            sta     U2+1                ; U2 (hi)
            ldx     LST_CHANNEL         ; Listing channel for SVC
            svc     $10                 ; SVC $10: write U2 bytes from (U1) to channel X
            lda     LSTST               ; Reset LSTOUTP to start of buffer
            sta     LSTOUTP
            lda     LSTST+1
            sta     LSTOUTP+1
@RETURN:    rts

; ============================================================================
; SAVE_FILE_POS
;
; Computes the logical file position corresponding to the first unread byte
; in the current source buffer and stores it in U7 (3 bytes).  The result
; is the physical channel position (from SVC $14) minus the number of bytes
; still buffered but not yet consumed.
;
; The adjusted position is the position to which the channel must be seeked
; (via SVC $13) to resume reading from the current point when the include
; frame is later popped.
;
; After computing the adjusted position in U7, execution falls through into
; PUSH_INCLUDE_FRAME to save U7, the current filename, and the drive
; code onto the include stack.
;
; Uses:  SRC_REMAINING = scratch for "bytes remaining in buffer" computation
; ============================================================================
SAVE_FILE_POS:
            ldx     SRC_CHANNEL         ; Los source channel number for SVC
            svc     $14                 ; SVC $14: query current file position -> U7 (3 bytes)
            lda     LINE_BUF_END        ; One-past-end of loaded buffer data
            sec
            sbc     TEMPP               ; Subtract TEMPP (= start of buffer)
            sta     SRC_REMAINING       ; Ttotal bytes loaded in buffer
            lda     LINE_BUF_END+1
            sbc     TEMPP+1
            sta     SRC_REMAINING+1
            lda     SRC_REMAINING       ; Now subtract SRC_BUF_OFF (bytes already consumed)
            sec
            sbc     SRC_BUF_OFF         ; -> bytes remaining unread in buffer
            sta     SRC_REMAINING
            bcs     @SKIP
            dec     SRC_REMAINING+1
@SKIP:      lda     U7                  ; Subtract bytes_remaining from physical file position
            sec
            sbc     SRC_REMAINING       ; -> logical "resume" position
            sta     U7
            lda     U7+1
            sbc     SRC_REMAINING+1
            sta     U7+1
            lda     U7+2
            sbc     #$00                ; Propagate borrow into high byte
            sta     U7+2
            ; Fall through into PUSH_INCLUDE_FRAME

; ============================================================================
; PUSH_INCLUDE_FRAME
;
; Pushes an 18-byte include-file frame onto the stack at INCL_FILE_STACK.
; Frame layout:
;   byte  0     drive/device code (SRC_DRIVE)
;   bytes 1–3   3-byte logical file position (U7, U7+1, U7+2)
;   bytes 4–17  14-byte filename record (SRC_FNAME_SLOT)
;
; INCL_STACK_TOP (INCL_STACK_TOP) holds the byte index of the next free slot.
; Maximum depth: $25 bytes / 18 = 2 frames; calls REPORT_ERROR (fatal error)
; if the stack overflows.
;
; Entry: U7[0..2] = file position to save; SRC_FNAME_SLOT[0..13] = current filename
; ============================================================================
PUSH_INCLUDE_FRAME:
            ldx     INCL_STACK_TOP      ; Current stack write index
            cpx     #$25                ; Stack full (maximum 2 frames × 18 bytes)?
            bcc     @CONT               ; No: there is room
            jsr     REPORT_ERROR        ; Yes: fatal error (include nesting too deep)
@CONT:      lda     SRC_DRIVE           ; Current source drive code
            sta     INCL_FILE_STACK,X   ; Frame byte 0: drive
            inx
            ldy     #$00
@CPY_POS:   lda     U7,Y                ; Copy 3-byte file position (bytes 1–3)
            sta     INCL_FILE_STACK,X
            inx
            iny
            cpy     #$03
            bne     @CPY_POS
            ldy     #$00
@CPY_REC:   lda     SRC_FNAME_SLOT,Y    ; Copy 14-byte filename record (bytes 4–17)
            sta     INCL_FILE_STACK,X
            inx
            iny
            cpy     #$0e
            bne     @CPY_REC
            stx     INCL_STACK_TOP      ; Update stack top index
            rts

; ============================================================================
; POP_INCL_FRAME
;
; Pops the topmost include-file frame from the stack (INCL_FILE_STACK) and resumes
; reading from the saved file position.
;
; Steps:
;   1. Checks whether the stack is empty (INCL_STACK_TOP = 0); if so returns C set.
;   2. Reads the frame in reverse order (top-down), restoring:
;       - SRC_FNAME_SLOT (14-byte filename)
;       - U7[0..2]       (3-byte file position)
;       - SRC_DRIVE      (drive code)
;   3. Decrements INCL_STACK_TOP (stack top pointer) by the frame size.
;   4. Calls OPEN_CHANNEL to re-open the parent file.
;   5. Calls SVC $13 on SRC_CHANNEL to seek to the saved position.
;   6. Calls READ_BLOCK_SVC to fill the source buffer from that position.
;   7. Checks INCLUDE_POP (bit 7): if set (EOF reached), returns C set.
;      Otherwise returns C clear to signal "continue with parent file".
; ============================================================================
POP_INCL_FRAME:
            ldx     INCL_STACK_TOP      ; Get stack top pointer
            beq     @RETC               ; Stack empty: return C set
            dex
            ldy     #$0d
@RES_FNAME: lda     INCL_FILE_STACK,X   ; Restore 14-byte filename (bytes 4–17, reversed)
            sta     SRC_FNAME_SLOT,Y
            dex
            dey
            bpl     @RES_FNAME
            ldy     #$02
@RES_FPOS:  lda     INCL_FILE_STACK,X   ; Restore 3-byte file position into U7[0..2]
            sta     U7,Y
            dex
            dey
            bpl     @RES_FPOS
            lda     INCL_FILE_STACK,X   ; Restore drive code into SRC_DRIVE
            sta     SRC_DRIVE
            stx     INCL_STACK_TOP      ; Update stack top pointer (decremented)
            jsr     OPEN_CHANNEL        ; Re-open the parent source channel
            ldx     SRC_CHANNEL         ; Load source channel number for SVC
            svc     $13                 ; SVC $13: seek channel to position in U7[0..2]
            jsr     READ_BLOCK_SVC      ; Refill source buffer from resumed position
            bit     INCLUDE_POP         ; Did READ_BLOCK_SVC signal EOF?
            bmi     @RETC               ; Yes: return C set (stack empty / EOF)
            clc                         ; No: parent file successfully resumed
            rts
@RETC:      sec                         ; Signal: stack empty or EOF in parent
            rts

; ============================================================================
; OUTPUT_ADDR_DIGITS
;
; Converts the 16-bit location counter (LOCCNT2 / LOCCNT2+1) to four
; uppercase hexadecimal ASCII digits and writes them into the listing
; output buffer at LIST_BUF+X, advancing X by 4.
;
; Calls HEX_BYTE_TO_ASCII twice: first for the high byte (producing
; the two most-significant hex digits), then for the low byte.
;
; Entry:  LOCCNT2/LOCCNT2+1 = 16-bit address to convert
;         X = current write index into LIST_BUF[]
; Exit:   X advanced by 4 (two hex digits per byte × 2 bytes)
; ============================================================================
OUTPUT_ADDR_DIGITS:
            ldx     #$06                ; X = 6: write starting at LIST_BUF[6] (address field)
            lda     LOCCNT2+1           ; High byte of address
            jsr     HEX_BYTE_TO_ASCII   ; Convert to 2 hex digits at LIST_BUF+X, X += 2
            lda     LOCCNT2             ; Low byte of address
            jmp     HEX_BYTE_TO_ASCII   ; Convert to 2 hex digits at LIST_BUF+X, X += 2 and return

; ============================================================================
; OUTPUT_VAL_DIGITS
;
; Converts the 24-bit assembled value in SYMVAL[0..2] to up to six uppercase
; hexadecimal ASCII digits and writes them into the listing output buffer at
; LIST_BUF+X.  Leading zero bytes are suppressed: if SYMVAL+2 is $00 or $FF
; (sign extension) the top byte is not output; the middle and low bytes are
; always output.
;
; Entry:  SYMVAL[0..2] = 24-bit value (lo, mid, hi)
;         X = current write index into LIST_BUF[]
; Exit:   X advanced by 2, 4, or 6 depending on value magnitude
; ============================================================================
OUTPUT_VAL_DIGITS:
            ldx     #$06                ; Reset X: value field starts at LIST_BUF[6]
OUTPUT_VAL_DIGITS2:
            lda     SYMVAL+2            ; High byte
            beq     @SKIP               ; $00 -> suppress high byte output
            cmp     #$ff
            beq     @SKIP               ; $FF -> suppress (sign extension)
            jsr     HEX_BYTE_TO_ASCII   ; Non-trivial high byte: emit 2 hex digits
@SKIP:      lda     SYMVAL+1            ; Middle byte: always emit
            jsr     HEX_BYTE_TO_ASCII
            lda     SYMVAL              ; Low byte: fall through to HEX_BYTE_TO_ASCII

; ============================================================================
; HEX_BYTE_TO_ASCII
;
; Converts the byte in A to two uppercase hexadecimal ASCII digits ('0'..'9',
; 'A'..'F') and stores them at LIST_BUF[X] and LIST_BUF[X+1], then increments X
; by 2.
;
; The VALUE_OUTPUT and VALUE_SUPPRESS flags gate the output:
;   If VALUE_OUTPUT is clear ($00): output is always written.
;   If VALUE_OUTPUT is set ($80) and VALUE_SUPPRESS is set ($80): skip output.
;
; Entry:  A = byte to convert
;         X = write index into LIST_BUF[]
; Exit:   LIST_BUF[X], LIST_BUF[X+1] = two hex digit characters
;         X += 2
; ============================================================================
HEX_BYTE_TO_ASCII:
            bit     VALUE_OUTPUT        ; Has any value already been output on this line?
            bpl     @CONT               ; No (clear): always write
            bit     VALUE_SUPPRESS      ; Is value output suppressed?
            bmi     @CONT               ; Set: fall through to write anyway (see note)
            rts                         ; Unset: skip this byte
@CONT:      pha                         ; Save original byte
            lsr     A                   ; Shift high nibble to low position
            lsr     A
            lsr     A
            lsr     A
            jsr     @EMIT               ; Emit high nibble as hex digit
            pla                         ; Restore original byte (low nibble in bits 3..0)
@EMIT:      and     #$0f                ; Isolate low nibble
            clc
            adc     #'0'                ; Map 0–9 -> '0'–'9'
            cmp     #'9'+1              ; Is it > '9' (i.e., A–F)?
            bmi     @STOR
            adc     #$06                ; Yes: add 7 (carry already 0, so +6 effective)
                                        ;   maps $3A->'A', $3B->'B', …, $3F->'F'
@STOR:      sta     LIST_BUF,X          ; Store ASCII digit in listing buffer
            inx                         ; Advance write index
            rts

; ============================================================================
; OUTPUT_ERR_LINE
;
; Emits the formatted listing line currently in the listing output buffer
; (starting at LIST_BUF) to the listing file channel (LST_CHANNEL).
;
; The line length is computed as LINE_BUF_LEN (source line length) + $12 (18 bytes
; for the fixed-width header: CR + address + value + flags + separators +
; leading space).
;
; Calls EMIT_LISTING_LINE_PG_CHK which handles automatic page breaks
; before delegating to OUTPUT_LISTING_LINE.
;
; Entry:  LINE_BUF_LEN = number of source text bytes in the listing line
; ============================================================================
OUTPUT_ERR_LINE:
            lda     LINE_BUF_LEN        ; Source line byte count
            clc
            adc     #$12                ; Add 18 bytes for fixed listing header
            tay                         ; Y = total line length
FLUSH_LINE: ldx     LST_CHANNEL         ; Set channel for SVC use inside EMIT_LISTING_LINE_PG_CHK
            lda     #<LIST_BUF          ; Point U6 at listing buffer (starts with CR)
            sta     U6
            lda     #>LIST_BUF
            sta     U6+1
            jmp     EMIT_LISTING_LINE_PG_CHK ; Emit with page-break check

; ============================================================================
; EMIT_LISTING_LINE_TO_CONSOLE
;
; Sends the formatted listing line from LIST_BUF to the console output channel
; (CON_CHANNEL) via SVC $7.  If ERROR_LISTING_RUN is set ($80), the
; line is also emitted to the listing file via EMIT_LISTING_LINE_PG_CHK.
; Called by REPORT_LINE_ERROR to display error annotations on the
; console during assembly.
;
; Entry:  LINE_BUF_LEN = source line byte count (used to compute total line length)
; ============================================================================
EMIT_LISTING_LINE_TO_CONSOLE:
            lda     LINE_BUF_LEN        ; Source line byte count
            clc
            adc     #$12                ; Add 18-byte header
            tay                         ; Y = total line length
            ldx     CON_CHANNEL         ; Console/error output channel
            bit     ERROR_LISTING_RUN   ; Are we in an error listing pass?
            bmi     @DONE               ; Yes: skip console direct output
                                        ;   (listing file output below covers it)
            lda     #<LIST_BUF          ; Point U6 at listing buffer
            sta     U6
            lda     #>LIST_BUF
            sta     U6+1
            svc     7                   ; SVC $07: output Y bytes from (U6) on channel X
@DONE:      rts

; ============================================================================
; EMIT_LISTING_LINE_CONTINUATION
;
; Called after each byte emitted by .FILL or .BYTE to update the listing
; output for multi-byte directives.
;
; Behaviour:
;   - Always sets VALUE_OUTPUT ($80) to record that at least one byte has
;     been written to the listing address/value field.
;   - If LISTING_ON is clear: returns immediately (no listing output).
;   - If VALUE_SUPPRESS is clear: places '.' markers in the separator field
;     (SEPARATOR_DOT_1/SEPARATOR_DOT_2) and returns.  The current line will
;     be finished later.
;   - If VALUE_SUPPRESS is set ($80): flushes the current listing line via
;     OUTPUT_ERR_LINE, then clears the addressing-mode and flag fields
;     (SEPARATOR_SP..SEPARATOR_DOT_2), writes the updated LOCCNT as the new address in
;     the listing header, and resets LINE_BUF_LEN to 0 so the
;     next byte starts a fresh listing line.
;
; Entry:  Y = current line position (preserved across the call)
; ============================================================================
EMIT_LISTING_LINE_CONTINUATION:
            sec
            ror     VALUE_OUTPUT        ; Set bit 7 of VALUE_OUTPUT ($80)
            bit     LISTING_ON          ; Listing active?
            bpl     @DONE               ; No: nothing to update
            bit     VALUE_SUPPRESS      ; Output new address line per byte?
            bmi     @FLUSH              ; Yes: full line flush
            lda     #'.'
            sta     SEPARATOR_DOT_1     ; Mark separator field byte 0 with '.'
            sta     SEPARATOR_DOT_2     ; Mark separator field byte 1 with '.'
            jmp     @DONE               ; Done: dots placed, line not yet flushed
@FLUSH:     sty     OUTPUTEL_Y_SAVE     ; Save Y
            jsr     OUTPUT_ERR_LINE     ; Flush current line to listing channel
            ldx     #$05
            lda     #' '
@CLEAR:     sta     SEPARATOR_SP,X      ; Clear 6 bytes: SEPARATOR_SP..SEPARATOR_DOT_2 (mode, flags, sep)
            dex
            bpl     @CLEAR
            ldx     #$06                ; Start hex-digit output at listing position 6
            lda     LOCCNT+1            ; Write updated location counter (high byte first)
            jsr     HEX_BYTE_TO_ASCII
            lda     LOCCNT              ; Then low byte
            jsr     HEX_BYTE_TO_ASCII
            lda     #$00
            sta     LINE_BUF_LEN        ; Reset source-line byte count for next continuation
            ldy     OUTPUTEL_Y_SAVE     ; Restore Y
@DONE:      rts

; ============================================================================
; REPORT_LINE_ERROR
;
; Records one assembly error and outputs error annotation lines.
;
; Steps:
;   1. Increments ERR_CNT (16-bit error counter).
;   2. Calls EMIT_LISTING_LINE_TO_CONSOLE to send the faulty source
;      line to the console (and optionally to the listing file).
;   3. If LISTING_ON is set, additionally sends the line via OUTPUT_ERR_LINE.
;   4. Increments the 4-digit error count field at ERR_COUNT in BCD-style ASCII,
;      rolling from '9' back to '0' with carry propagation.
;   5. Computes the caret-indicator line length (LINE_POS + 18) and outputs
;      it from ERR_PREFIX (the "*ERROR NNNN" prefix buffer) to both the console
;      channel and the listing channel (when enabled).
;   6. Looks up the error-type message pointer from the dispatch table
;      ERRROR_DISPATCH_TABLE using ERR_TYPE_CODE × 2, outputs the message string
;      to the console, and optionally to the listing channel.
;   7. Clears ERR_TYPE_CODE to 0 ready for the next line.
; ============================================================================
REPORT_LINE_ERROR:
            inc     ERR_CNT             ; Increment 16-bit error counter (lo)
            bne     @SKIP
            inc     ERR_CNT+1           ; Carry into high byte
@SKIP:      jsr     EMIT_LISTING_LINE_TO_CONSOLE ; Emit faulty source line to console/listing
            bit     LISTING_ON
            bpl     @NOLIST
            jsr     OUTPUT_ERR_LINE     ; Also emit to listing file
@NOLIST:    ldx     #$03                ; Increment 4-digit ASCII error count at ERR_COUNT
@ISDIG:     lda     #'9'                ; Load '9' to test if digit wrapped
            inc     ERR_COUNT,X         ; Increment digit
            cmp     ERR_COUNT,X         ; Did it exceed '9'?
            bcs     @GETPOS             ; No: done (digit in range)
            lda     #'0'                ; Yes: wrap back to '0'
            sta     ERR_COUNT,X
            dex                         ; Carry to next more-significant digit
            bpl     @ISDIG
@GETPOS:    lda     LINE_POS            ; Current position in source line
            clc
            adc     #$12                ; Add 18-byte header to get caret line length
            tay
            lda     #<ERR_PREFIX        ; Point U6 at error prefix buffer ("*ERROR NNNN ")
            sta     U6
            lda     #>ERR_PREFIX
            sta     U6+1
            bit     ERROR_LISTING_RUN   ; Are we in an error listing pass?
            bmi     @NOCONSOLE          ; Yes: skip direct console output (listing covers it)
            ldx     CON_CHANNEL         ; Console channel
            svc     7                   ; SVC $07: output caret line to console
@NOCONSOLE: bit     LISTING_ON
            bpl     @NOLIST2
            jsr     EMIT_LISTING_LINE_PG_CHK ; Also emit caret line to listing file
@NOLIST2:   lda     ERR_TYPE_CODE       ; Error type (0–12)
            asl     A                   ; × 2 to index the pointer table
            tax
            lda     ERRROR_DISPATCH_TABLE,X ; Load error message pointer
            sta     U6
            lda     ERRROR_DISPATCH_TABLE+1,X
            sta     U6+1
            ldx     CON_CHANNEL         ; Console channel
            ldy     #$00
@SCAN:      iny                         ; Scan for null terminator to determine length
            lda     (U6),Y
            bne     @SCAN
            bit     ERROR_LISTING_RUN   ; Are we in an error listing pass?
            bmi     @CHKLST             ; Yes: skip console direct output
            svc     7                   ; Output error type string to console
@CHKLST:    bit     LISTING_ON
            bpl     @NOLST3
            jsr     EMIT_LISTING_LINE_PG_CHK ; Output error type string to listing
@NOLST3:    lda     #$00
            sta     ERR_TYPE_CODE       ; Reset error type for next line
            rts

; ============================================================================
; ERROR TYPE MESSAGE DISPATCH TABLE
;
; 13 two-byte address entries (indexed by ERR_TYPE_CODE × 2).
; Entry 0 ($0000) is a sentinel used when there is no type-specific message.
; Entries 1–12 point to null-terminated error strings printed after the
; "^" caret indicator line.
;
;   Index  Constant  String
;     0    (none)    $0000  — no message
;     1    SYNTAX    "^ SYNTAX."
;     2    ARITH     "^ ARITHMETIC."
;     3    IDENT     "^ IDENTIFIER."
;     4    ZPPAGE    "^ 0-PAGE."
;     5    DUPL      "^ DUPLICATE."
;     6    UNDEF     "^ UNDEFINED."
;     7    COMPLEX   "^ COMPLEXITY."
;     8    ADDR      "^ ADDRESSING."
;     9    RANGE     "^ RANGE."
;    10    COND      "^ CONDITIONAL."
;    11    FILE      "^ FILE."
;    12    MACRO     "^ MACRO."
; ============================================================================
ERRROR_DISPATCH_TABLE:
            .addr   $0000                   ;  0: no error-type annotation
            .addr   SYNTAX_ERROR_STR        ;  1: syntax error
            .addr   ARITHMETIC_ERROR_STR    ;  2: arithmetic error
            .addr   IDENTIFIER_ERROR_STR    ;  3: identifier error
            .addr   ZEROPAGE_ERROR_STR      ;  4: zero-page error
            .addr   DUPLICATE_ERROR_STR     ;  5: duplicate definition
            .addr   UNDEFINED_ERROR_STR     ;  6: undefined symbol
            .addr   TOOCOMPLEX_ERROR_STR    ;  7: expression too complex
            .addr   ADDRESSING_ERROR_STR    ;  8: addressing mode error
            .addr   RANGE_ERROR_STR         ;  9: value out of range
            .addr   CONDITIONAL_ERROR_STR   ; 10: conditional assembly error
            .addr   FILEIO_ERROR_STR        ; 11: file I/O error
            .addr   MACRO_ERROR_STR         ; 12: macro error

; ============================================================================
; ERROR TYPE MESSAGE STRINGS
; Each string begins with "^ " (caret + space), which visually points up at
; the offending token in the source line above, followed by a category label
; and a period.  All strings are null-terminated.
; ============================================================================
SYNTAX_ERROR_STR:       .byte   "^ SYNTAX.", 0
ARITHMETIC_ERROR_STR:   .byte   "^ ARITHMETIC.", 0
IDENTIFIER_ERROR_STR:   .byte   "^ IDENTIFIER.", 0
ZEROPAGE_ERROR_STR:     .byte   "^ 0-PAGE.", 0
DUPLICATE_ERROR_STR:    .byte   "^ DUPLICATE.", 0
UNDEFINED_ERROR_STR:    .byte   "^ UNDEFINED.", 0
TOOCOMPLEX_ERROR_STR:   .byte   "^ COMPLEXITY.", 0
ADDRESSING_ERROR_STR:   .byte   "^ ADDRESSING.", 0
RANGE_ERROR_STR:        .byte   "^ RANGE.", 0
CONDITIONAL_ERROR_STR:  .byte   "^ CONDITIONAL.", 0
FILEIO_ERROR_STR:       .byte   "^ FILE.", 0
MACRO_ERROR_STR:        .byte   "^ MACRO.", 0

; ============================================================================
; PRINT_PASS_SUMMARY
;
; Sends the pass summary to the console (via PRINT_PASS_SUMMARY_CONSOLE)
; and, if a listing file is open (LISTING_FILE_OPEN = $80), also sends
; it to the listing channel via EMIT_LISTING_LINE_PG_CHK.
;
; The summary line format is: "<NNNN> ERRORS IN PASS <n> " (24 bytes),
; where NNNN is the current pass error count and n is '1' or '2'.
; ============================================================================
PRINT_PASS_SUMMARY:
            jsr     PRINT_PASS_SUMMARY_CONSOLE ; Output pass summary to console
PRINT_PASS_SUMMARY_FILE:
            bit     LISTING_FILE_OPEN   ; Is a listing file open?
            bpl     @DONE               ; No: skip listing output
            jsr     FORMAT_PASS_SUMMARY_LINE ; Re-format summary (U6 setup)
            jsr     EMIT_LISTING_LINE_PG_CHK ; Output to listing with page-break check
@DONE:      rts

; ============================================================================
; PRINT_PASS_SUMMARY_CONSOLE
;
; Sends the pass summary only to the console channel (CON_CHANNEL)
; via SVC $7.  The summary is formatted by FORMAT_PASS_SUMMARY_LINE
; into U6 before the SVC call.
;
; The summary line format is: "<NNNN> ERRORS IN PASS <n> " (24 bytes),
; where NNNN is the current pass error count and n is '1' or '2'.
; ============================================================================
PRINT_PASS_SUMMARY_CONSOLE:
            jsr     FORMAT_PASS_SUMMARY_LINE ; Format pass summary line into U6
            ldx     CON_CHANNEL         ; Set console chgannel for SVC
            svc     7                   ; SVC $07: output Y bytes from (U6) to console
            rts

; ============================================================================
; FORMAT_PASS_SUMMARY_LINE
;
; Fills in the pass-summary template at SUMMARY_BUF and points U6 at it.
;
; Actions:
;   1. Copies the 4 ASCII error-count digits from ERR_COUNT[0..3] into
;      SUMMARY_TEMPLATE[0..3], suppressing leading zeros (replacing '0' with ' '
;      for digits before the last non-zero or the tens position).
;   2. Sets PASS_NUMBER to '1' or '2' based on bit 7 of PASSFLG.
;   3. Sets U6 = SUMMARY_BUF, Y = $18 (24 bytes) for the subsequent SVC/output.
;
; Entry: ERR_COUNT[0..3] = 4-digit ASCII error count; PASSFLG bit 7 = pass (0=1, 1=2)
; Exit:  U6 -> SUMMARY_BUF (pass summary buffer); Y = $18 (line length)
; ============================================================================
FORMAT_PASS_SUMMARY_LINE:
            ldx     #$03
@COPY:      lda     ERR_COUNT,X         ; Copy raw error count digits
            sta     SUMMARY_TEMPLATE,X
            dex
            bpl     @COPY
@SUPPRESS:  inx                         ; Suppress leading zeros (left-to-right scan)
            lda     SUMMARY_TEMPLATE,X
            cmp     #'0'                ; Is this digit '0'?
            bne     @SETPASS            ; No: stop suppressing
            lda     #' '
            sta     SUMMARY_TEMPLATE,X  ; Replace leading '0' with space
            cpx     #$02                ; Stop at the tens digit (keep at least units)
            bcc     @SUPPRESS
@SETPASS:   lda     #'1'                ; Default: pass 1
            bit     PASSFLG             ; Bit 7 set -> pass 2
            bpl     @SKIP
            lda     #'2'
@SKIP:      sta     PASS_NUMBER         ; Set pass number character
            lda     #<SUMMARY_BUF       ; Point U6 at summary buffer
            sta     U6
            lda     #>SUMMARY_BUF
            sta     U6+1
            ldy     #$18                ; Line length = 24 bytes
            rts

; ============================================================================
; PRINT_FINAL_SUM
;
; Outputs the end-of-assembly statistics to the output channel stored in
; OUT_CHANNEL (set to LST_CHANNEL or CON_CHANNEL depending on whether listing is
; active).  Uses SVC $0B (decimal encode) and SVC $0A (hex encode) to
; format numbers, and SVC $7 to output each string.
;
; Lines produced:
;   "<lines> LINES, <symbols> SYMBOLS, <references> REFERENCES."
;   "SYMBOL TABLE USED $xxxx BYTES OF $xxxx BYTES AVAILABLE (BANK 0)"
;   "AND $xxxx OF $xxxx IN EXPANSION RAM BANK." (only if EXPANSION_BANK set)
;   "* END OF ASSEMBLY. *"
;
; The symbol count output is (SYMVALP − HEAPST) bytes used out of
; (HEAPEND − HEAPST) available.  EXPANSION_BANK flag ($80) indicates that
; an additional secondary heap in bank 3 is present; if so, HEAP2WRP and
; HEAP2END are also reported.
; ============================================================================
PRINT_FINAL_SUM:
            bit     LISTING_ON          ; Listing active?
            bpl     @COUT               ; No: go straight to console output
            ldx     LST_CHANNEL         ; Listing channel (for use in PRTRSTR)
            jsr     PRTSTR              ; Output "SUMMARY:\r" header to listing
            .byte   $0d, "SUMMARY:", $0d, 0
            jsr     @STATS              ; Now output stats to listing channel
@COUT:      ldx     #$02                ; Channel 2 = console
@STATS:     stx     OUT_CHANNEL         ; Save output channel number
            svc     $0c                 ; SVC $0C: obtain system string buffer addresses -> U0
            ldy     #$00
            lda     LINE_NUM            ; 16-bit source line count
            sta     U0
            lda     LINE_NUM+1
            sta     U0+1
            svc     $0b                 ; SVC $0B: encode U0 as decimal ASCII string
            ldx     OUT_CHANNEL
            svc     7                   ; Output "<lines>"
            jsr     PRTSTR
            .byte   " LINES, ", 0
            ldy     #$00
            lda     SYMBOL_COUNT
            sec
            sbc     #$55                ; Subtract $55 = 85: first $55 hash slots are reserved
            sta     U0
            lda     SYMBOL_COUNT+1
            sbc     #$00
            sta     U0+1
            svc     $0b                 ; Encode symbol count as decimal
            ldx     OUT_CHANNEL
            svc     7                   ; Output "<symbols>"
            jsr     PRTSTR
            .byte   " SYMBOLS, ", 0
            lda     REF_COUNT
            sta     U0
            lda     REF_COUNT+1
            sta     U0+1
            ldx     OUT_CHANNEL
            ldy     #$00
            svc     $0b                 ; Encode reference count as decimal
            svc     7                   ; Output "<references>"
            jsr     PRTSTR
            .byte   " REFERENCES.", $0d
            .byte   "SYMBOL TABLE USED $", 0
            lda     SYMVALP             ; Heap bytes used = SYMVALP − HEAPST
            sec
            sbc     HEAPST
            sta     U0
            lda     SYMVALP+1
            sbc     HEAPST+1
            sta     U0+1
            ldy     #$00
            svc     $0a                 ; SVC $0A: encode U0 as hex ASCII
            ldx     OUT_CHANNEL
            svc     7                   ; Output "$xxxx" used bytes
            jsr     PRTSTR
            .byte   " OF $", 0
            lda     HEAPEND             ; Total heap = HEAPEND − HEAPST
            sec
            sbc     HEAPST
            sta     U0
            lda     HEAPEND+1
            sbc     HEAPST+1
            sta     U0+1
            ldx     OUT_CHANNEL
            ldy     #$00
            svc     $0a                 ; Encode total heap size as hex ASCII
            svc     7                   ; Output "$xxxx" total
            jsr     PRTSTR
            .byte   " BYTES AVAILABLE (BANK 0)", 0
            bit     EXPANSION_BANK      ; Expansion RAM bank present?
            bpl     @DONE               ; No: skip secondary heap report
            jsr     PRTSTR              ; Yes: output expansion RAM stats
            .byte   $0d, "AND $", 0
            lda     HEAP2WRP            ; Secondary heap bytes used
            sta     U0
            lda     HEAP2WRP+1
            sta     U0+1
            ldy     #$00
            svc     $0a
            ldx     OUT_CHANNEL
            svc     7
            jsr     PRTSTR
            .byte   " OF $", 0
            lda     HEAP2END            ; Secondary heap total
            sta     U0
            lda     HEAP2END+1
            sta     U0+1
            ldy     #$00
            svc     $0a
            svc     7
            jsr     PRTSTR
            .byte   " IN EXPANSION RAM BANK.", 0
@DONE:      jsr     PRTSTR
            .byte   $0d, "* END OF ASSEMBLY. *", $0d, 0
            rts

; ============================================================================
; PRTSTR
;
; Outputs a null-terminated string that immediately follows the JSR PRTSTR
; instruction in the caller's code stream, then returns to the instruction
; after the string's null terminator.
;
; Mechanism:
;   1. Pops the 16-bit return address from the stack into RETADDRP; this
;      points to the byte immediately after the JSR instruction (i.e., the
;      first byte of the inline string).
;   2. Reads bytes from (RETADDRP), advancing RETADDRP after each one,
;      and outputs each via SVC $4 until a NUL byte ($00) is found.
;   3. Pushes the updated RETADDRP (now pointing at the byte after the NUL)
;      back onto the stack as the new return address.
;   4. Restores Y and returns.
;
; PRTSTR_Y_SAVE is used to preserve Y across the call.
; ============================================================================
PRTSTR:     sty     PRTSTR_Y_SAVE
            pla                         ; Pop return address low byte
            sta     RETADDRP
            pla                         ; Pop return address high byte
            sta     RETADDRP+1
@NEXT:      inc     RETADDRP            ; Advance to next string byte
            bne     @SKIP
            inc     RETADDRP+1
@SKIP:      ldy     #$00
            lda     (RETADDRP),Y        ; Read one string character
            beq     @DONE               ; NUL terminator: done
            svc     4                   ; SVC $04: output byte A to current channel
            jmp     @NEXT
@DONE:      lda     RETADDRP+1          ; Push updated return address (past NUL)
            pha
            lda     RETADDRP
            pha
            ldy     PRTSTR_Y_SAVE       ; Restore Y
            rts

; ============================================================================
; EMIT_OPCODE_AND_SETUP_LISTING
;
; Emits the opcode byte accumulated in the current instruction record to the
; object-code output stream via EMIT_BYTE_PASS2, then sets X to $0B (11)
; and falls into HEX_BYTE_TO_ASCII to write the opcode's hex
; representation at listing buffer position LIST_BUF[$0B] (the value field).
;
; Called as the first step of all instruction-emission helpers below.
; ============================================================================
EMIT_OPCODE_AND_SETUP_LISTING:
            jsr     EMIT_BYTE_PASS2     ; Write opcode byte to object-code stream
            ldx     #$0b                ; X = 11: start writing hex value digits at LIST_BUF[11]
            jmp     HEX_BYTE_TO_ASCII   ; Convert opcode byte to hex in listing buffer

; ============================================================================
; EMIT_BRANCH_INSTR
;
; Emits a branch instruction (opcode + 1-byte PC-relative offset) during
; pass 2.
;
; Steps:
;   1. Calls EMIT_OPCODE_AND_SETUP_LISTING to write the opcode and its hex
;      listing digits.
;   2. Computes the signed 8-bit branch offset:
;        offset = SYMVAL[0..1] − LOCCNT
;      where LOCCNT is the address of the byte that follows the branch
;      instruction.
;   3. Validates the offset: the high byte must be $00 (positive offset,
;      branch forward) or $FF (negative offset, branch backward); otherwise
;      calls ERROR_RANGE (range error).
;   4. Emits the offset byte via EMIT_BYTE_PASS2 and writes its hex
;      representation to the listing buffer.
; ============================================================================
EMIT_BRANCH_INSTR:
            jsr     EMIT_OPCODE_AND_SETUP_LISTING ; Emit opcode; X = $0B after return
            lda     SYMVAL              ; Subtract LOCCNT from target address:
            sec                         ; ffset = SYMVAL − LOCCNT
            sbc     LOCCNT
            sta     SYMVAL
            lda     SYMVAL+1
            sbc     LOCCNT+1
            sta     SYMVAL+1
            beq     @FORWARD            ; High byte = $00 -> forward branch, check sign
            cmp     #$ff
            beq     @BACKWARD           ; High byte = $FF -> backward branch, check sign
@ERROR:     jsr     ERROR_RANGE         ; Neither: out-of-range branch offset -> error
            ; Not reached
@FORWARD:   lda     SYMVAL              ; Forward branch: offset byte must be 0..$7F
            bmi     @ERROR              ; Negative -> out of range
            bpl     @EMIT               ; Non-negative -> valid
@BACKWARD:  lda     SYMVAL              ; Backward branch: offset byte must be $80..$FF
            bpl     @ERROR              ; Positive -> out of range
@EMIT:      jsr     EMIT_BYTE_PASS2     ; Emit the validated 8-bit offset byte
            jmp     HEX_BYTE_TO_ASCII   ; Write offset hex digits to listing; X still = $0D

; ============================================================================
; EMIT_ONE_BYTE_OPERAND
;
; Emits a single-byte (zero-page or immediate) operand instruction during
; pass 2: opcode followed by one operand byte.
;
; Steps:
;   1. EMIT_OPCODE_AND_SETUP_LISTING: emit opcode, set X = $0B.
;   2. Emit SYMVAL[0] (low byte of operand) via EMIT_BYTE_PASS2.
;   3. Write operand hex digits to listing at LIST_BUF[X].
; ============================================================================
EMIT_ONE_BYTE_OPERAND:
            jsr     EMIT_OPCODE_AND_SETUP_LISTING ; Emit opcode; X = $0B
            lda     SYMVAL              ; Low byte of operand
            jsr     EMIT_BYTE_PASS2     ; Emit to object stream
            jmp     HEX_BYTE_TO_ASCII   ; Write hex digits to listing; return

; ============================================================================
; EMIT_TWO_BYTE_OPERAND
;
; Emits a two-byte (absolute) operand instruction during pass 2: opcode
; followed by two operand bytes in little-endian order (low byte first).
;
; Steps:
;   1. EMIT_OPCODE_AND_SETUP_LISTING: emit opcode, set X = $0B.
;   2. Emit SYMVAL[0] (low byte) then SYMVAL[1] (high byte) via EMIT_BYTE_PASS2.
;   3. Write both operand bytes' hex digits to listing.
; ============================================================================
EMIT_TWO_BYTE_OPERAND:
            jsr     EMIT_OPCODE_AND_SETUP_LISTING ; Emit opcode; X = $0B
            lda     SYMVAL              ; Low byte of 16-bit absolute address
            jsr     EMIT_BYTE_PASS2
            jsr     HEX_BYTE_TO_ASCII   ; Hex digits for low byte at LIST_BUF[X], X += 2
            lda     SYMVAL+1            ; High byte
            jsr     EMIT_BYTE_PASS2
            jmp     HEX_BYTE_TO_ASCII   ; Hex digits for high byte at LIST_BUF[X]; return

; ============================================================================
; EMIT_DATA_BYTE_WITH_LISTING
;
; Emits one data byte (from .BYTE / .FILL directives) to the object-code
; stream and writes its hex representation to the listing buffer.
;
; Differs from the instruction-emission helpers in that no opcode is
; emitted first: the byte in A (passed via EMIT_BYTE_PASS2's context) is
; a raw data value.
;
; Listing output behaviour based on X (listing column position):
;   X < $0F ($15): write hex digits normally via HEX_BYTE_TO_ASCII.
;   $0F ≤ X < $11 ($17): the value field is full; write ".." as a placeholder
;     to show that more bytes follow on this line.
;   X >= $11: silently skip (already past the listing value area).
; ============================================================================
EMIT_DATA_BYTE_WITH_LISTING:
            jsr     EMIT_BYTE_PASS2     ; Emit byte to object-code stream
            cpx     #$11                ; X >= $11: past listing value area?
            bcs     @DONE               ; Yes: skip hex output
            cpx     #$0f                ; X >= $0F: value field is full?
            bcc     @EMIT               ; No: emit hex digits normally
            lda     #'.'                ; Yes: write ".." overflow indicator
            sta     LIST_BUF,X
            inx
            sta     LIST_BUF,X
            inx
            rts
@EMIT:      jmp     HEX_BYTE_TO_ASCII   ; Write hex digits at LIST_BUF[X], X += 2; return
@DONE:      rts

; ============================================================================
; EMIT_BYTE_PASS2
;
; Core routine that writes one byte from the accumulated instruction/data
; context to the object-code output buffer (OBJCOBUF).  Only active during
; pass 2 (PASSFLG bit 7 set); a call during pass 1 triggers a BRK (BUG-2).
;
; If OBJSEG_OPEN is clear (no segment header yet written), a new segment
; header record is automatically opened first.  The header format is:
;   byte 0: $58 ('X') — segment marker
;   byte 1: OVL_NUM — overlay number
;   byte 2: BANK_NUM — memory bank
;   byte 3: $00 — reserved
;   bytes 4–5: load address (LOCCNT2 or ENTRY_ADDR if .ENTRY was seen)
;   bytes 6–7: load address again (base address for this segment)
;   bytes 8–9: segment size from old header (SEG_SIZE)
; After writing the header, OBJSEG_OPEN is set ($80).
;
; The actual byte is then written to OBJCOBUF via OBJBUF_WRITE_BYTE.
;
; Entry: A = byte to emit
; ============================================================================
EMIT_BYTE_PASS2:
            bit    PASSFLG             ; Pass 2 active?
            bmi    @CONT               ; Yes: proceed
            brk                        ; Pass 1: should not be called (BUG-2)
            rts
@CONT:      bit     OBJSEG_OPEN         ; Is a segment header already open?
            bmi     OBJBUF_WRITE_BYTE   ; Yes: write byte directly
            pha                         ; Save byte to emit
            tya                         ; Save Y
            pha
            ldy     #$03                ; Read 4-byte previous segment header from SEGHEADP
            lda     (SEGHEADP),Y
            sta     SEG_SIZE+1          ; Previous segment size
            dey
            lda     (SEGHEADP),Y
            sta     SEG_SIZE
            dey
            lda     (SEGHEADP),Y        ; SEGHEADP+1 = new segment header address high
            pha
            dey
            lda     (SEGHEADP),Y        ; SEGHEADP+0 = new segment header address low
            sta     SEGHEADP
            pla
            sta     SEGHEADP+1
            pla                         ; Restore Y
            tay
            lda     #$58                ; Segment marker byte 'X'
            jsr     OBJBUF_WRITE_BYTE   ; Write to OBJCOBUF
            lda     OVL_NUM             ; Overlay number
            jsr     OBJBUF_WRITE_BYTE
            lda     BANK_NUM            ; Bank number
            jsr     OBJBUF_WRITE_BYTE
            lda     #$00                ; Reserved byte
            jsr     OBJBUF_WRITE_BYTE
            bit     ENTRY_DEFINED       ; Was .ENTRY seen?
            bmi     @USE_ENTRY          ; Yes: use ENTRY_ADDR as load address
            lda     LOCCNT2             ; No: use current location counter
            jsr     OBJBUF_WRITE_BYTE
            lda     LOCCNT2+1
            jsr     OBJBUF_WRITE_BYTE
            jmp     @SKIP
@USE_ENTRY: lda     ENTRY_ADDR
            jsr     OBJBUF_WRITE_BYTE
            lda     ENTRY_ADDR+1
            jsr     OBJBUF_WRITE_BYTE
@SKIP:      lda     LOCCNT2             ; Base address of this segment (lo)
            jsr     OBJBUF_WRITE_BYTE
            lda     LOCCNT2+1           ; (hi)
            jsr     OBJBUF_WRITE_BYTE
            lda     SEG_SIZE            ; Previous segment size — closes old segment
            jsr     OBJBUF_WRITE_BYTE
            lda     SEG_SIZE+1
            jsr     OBJBUF_WRITE_BYTE
            sec
            ror     OBJSEG_OPEN         ; Set OBJSEG_OPEN ($80): segment header now open
            pla                         ; Restore data byte to emit
            ; Fall through

; ============================================================================
; OBJBUF_WRITE_BYTE
;
; Low-level primitive that appends one byte to the object-code output buffer
; (OBJCOBUF) and flushes the buffer to disk when it is full.
;
; Called directly by EMIT_BYTE_PASS2 (both for the segment-header bytes it
; synthesises and for the final payload byte), so it must preserve X across
; the call because EMIT_BYTE_PASS2 uses X for its own bookkeeping.
;
; Algorithm:
;   1. Save X in EBPASS2_X_SAVE.
;   2. Write A to OBJCOBUF[OBJBUF_POS] and increment OBJBUF_POS.
;   3. If OBJBUF_POS did not wrap to $00, the buffer is not full — restore
;      X and return.
;   4. If OBJBUF_POS wrapped to $00 the 256-byte buffer is full: flush it
;      to the object-code file channel (OBJ_CHANNEL) via SVC $10, writing
;      exactly 256 bytes from OBJCOBUF.
;
; Entry:  A = byte to store
;         X = caller's value (preserved across the call)
; Exit:   A clobbered; X restored; Y unchanged
; ============================================================================
OBJBUF_WRITE_BYTE:
            stx     EBPASS2_X_SAVE      ; Save X
            ldx     OBJBUF_POS          ; Current write index in OBJCOBUF
            sta     OBJCOBUF,X          ; Store byte in object-code buffer
            inc     OBJBUF_POS          ; Advance write index
            bne     OBJBUF_WB_DONE      ; No overflow: done

FLUSH_OBJECT:
            pha                         ; Buffer full ($100 bytes): flush to disk
            lda     #<OBJCOBUF          ; U1 = start of object-code buffer
            sta     U1
            lda     #>OBJCOBUF
            sta     U1+1
            lda     OBJBUF_POS          ; Byte count (0 after overflow = 256)
            sta     U2
            beq     @WR256              ; Zero: write 256 bytes
            lda     #$00
            beq     @SKIP               ; Always jump
@WR256:     lda     #$01                ; U2+1 = 1 when count == 0 (full 256-byte buffer)
@SKIP:      sta     U2+1
            ldx     OBJ_CHANNEL         ; Object channel for SVC
            svc     $10                 ; SVC $10: write U2 bytes from (U1) to channel X
            pla                         ; Restore byte
OBJBUF_WB_DONE:
            ldx     EBPASS2_X_SAVE      ; Restore X
            rts

; ============================================================================
; ADVANCE_PEEK_BLANK
;
; Advances Y by 1 (skipping one character in the source line buffer LINE_BUF),
; then falls through into PEEK_BLANK_CHAR to test the new character.
; ============================================================================
ADVANCE_PEEK_BLANK:
            iny
            ; Fall through

; ============================================================================
; PEEK_BLANK_CHAR
;
; Tests the character at LINE_BUF[Y] without consuming it (Y is not modified).
; Sets the CPU flags so the caller can branch on whether the character is a
; "blank" (terminates an operand):
;   Z set  (BEQ branches)  if the character is NUL ($00), space ($20), or
;          the comment delimiter (COMMENT = ';').
;   Z clear otherwise.
;
; Entry:  Y = index into source line buffer LINE_BUF
; Exit:   Z flag set ↔ character is blank/end-of-field; A = character; Y unchanged
; ============================================================================
PEEK_BLANK_CHAR:
            lda     LINE_BUF,Y          ; Read character at current line position
            beq     @RETURN             ; NUL: blank
            cmp     #' '                ; Space: blank
            beq     @RETURN
            cmp     COMMENT             ; Comment start: blank
@RETURN:    rts                         ; Z flag reflects comparison result


ADVANCE_SKIP_LEADING_SPACES:
            iny                         ; Advance before scanning

; ============================================================================
; SKIP_LEADING_SPACES
;
; Skips space characters in LINE_BUF from the current Y position forward,
; stopping at the first non-space character, NUL, or comment delimiter.
; Sets Z flag to indicate the stopping character was blank/end-of-field.
;
; Entry:  Y = starting index into LINE_BUF
; Exit:   Y = index of first non-space (or NUL/comment) character
;         A = stopping character; Z set ↔ stopping at NUL or comment delimiter
; ============================================================================
SKIP_LEADING_SPACES:
            lda     LINE_BUF,Y          ; Read character
            cmp     #' '                ; Space?
            beq     ADVANCE_SKIP_LEADING_SPACES ; Yes: skip and continue
            cmp     #$00                ; NUL?
            beq     @DONE               ; Yes: stop (Z set)
            cmp     COMMENT             ; Comment delimiter?
@DONE:      rts                         ; Return; Z set ↔ end-of-field reached

; ============================================================================
; STORE_MACRO_LINE
;
; Copies one source line from LINE_BUF into the heap (via WRITE_TO_HEAP),
; respecting string literals.
;
; Behaviour:
;   - Scans LINE_BUF starting at Y = 0, character by character.
;   - Tracks whether the scan is inside a single-quoted string using
;     IN_STRING (bit 7 set = inside string).
;   - A comment character (';') found outside a string terminates the copy
;     (the comment is not stored).
;   - Single-quote characters (' = $27) toggle the IN_STRING flag.
;   - NUL ($00) terminates the scan.
;   - Each non-terminating character is written to the heap.
;   - A carriage return ($0D) is written as the line terminator.
;
; This is called during macro body recording (.MACRO ... .ENDMAC) to store
; each source line verbatim (minus comments) in the heap for later expansion.
; ============================================================================
STORE_MACRO_LINE:
            ldy     #$00
            sty     IN_STRING           ; Clear "inside string" flag
            dey                         ; Y = $FF (will be incremented to 0 on first loop)
@NEXT:      iny
            lda     LINE_BUF,Y          ; Read next character
            beq     @TERM               ; NUL: end of line
            cmp     COMMENT             ; Comment delimiter?
            bne     @CHKQUOT            ; No: check for quote
            bit     IN_STRING           ; Are we inside a string?
            bpl     @TERM               ; No: treat ';' as comment terminator, stop
            bmi     @STORE               ; Yes: store it as a literal character
@CHKQUOT:   cmp     #$27                ; Single-quote (')?
            bne     @STORE              ; No: store character as-is
            asl     IN_STRING           ; Toggle IN_STRING flag:
            bcs     @STORE              ;   was set -> now clear (closing quote)
            sec
            ror     IN_STRING           ;   was clear -> now set (opening quote)
@STORE:     jsr     WRITE_TO_HEAP       ; Store character in heap
            jmp     @NEXT               ; Next character
@TERM:      lda     #$0d                ; Store carriage return as line terminator
            jsr     WRITE_TO_HEAP
            rts

; ============================================================================
; RECORD_XREF
;
; Records one cross-reference entry for the current symbol.
;
; Steps:
;   1. Returns immediately if LISTING_ACTIVE is clear (the current line is not
;      being listed), since cross-references only apply to listed lines.
;   2. Increments the global reference counter (REF_COUNT, 16-bit).
;   3. If XREF_MODE is clear, returns (no cross-reference records needed).
;   4. Otherwise writes a 4-byte cross-reference record to the heap:
;        bytes 0–1: pointer to the next xref record for this symbol (chained
;                   linked list; initially $0000 for the first reference)
;        bytes 2–3: current LINE_NUM (the line number where this reference
;                   appears)
;      Updates the "previous xref pointer" field in the symbol table entry
;      (via SYMTBLP+Y) to point to the newly written record, building a
;      backwards-linked chain.
;
; Entry:  SYMTBLP -> current symbol table entry
;         LINE_NUM = current source line number (16-bit)
;         SYM_NAME_OFF = byte offset to the end of the name in entry
; ============================================================================
RECORD_XREF:
            bit     LISTING_ACTIVE      ; Only cross-reference listed lines
            bmi     @CONT
            rts
@CONT:      inc     REF_COUNT           ; REF_COUNT++
            bne     @SKIP
            inc     REF_COUNT+1
@SKIP:      bit     XREF_MODE           ; Xref mode active?
            bmi     @XREF
            rts                         ; No: just count, don't record location
@XREF:      sty     EVAL_Y_SAVE         ; Save Y (caller's line position)
            ldy     SYM_NAME_OFF        ; offset of type byte in symbol entry
            iny                         ; Skip to byte after name terminus
            iny
            iny
            iny                         ; Now Y points to the xref-chain link field (2 bytes)
            lda     (SYMTBLP),Y         ; Read current chain-link pointer (lo)
            jsr     WRITE_TO_HEAP       ; Write as first 2 bytes of new xref record
            iny
            lda     (SYMTBLP),Y         ; Chain-link hi byte
            jsr     WRITE_TO_HEAP
            lda     LINE_NUM            ; Current line number (lo)
            jsr     WRITE_TO_HEAP       ; Write as bytes 2–3 of xref record
            lda     LINE_NUM+1
            jsr     WRITE_TO_HEAP
            lda     SYMVALP+1           ; SYMVALP = next write position in primary heap
            bit     EXPANSION_BANK      ; Is expansion RAM bank available?
            bpl     @NOEXP              ; No: use SYMVALP
            lda     HEAP2WRP+1          ; Yes: use secondary heap write pointer (hi)
@NOEXP:     sta     (SYMTBLP),Y         ; Update symbol entry chain-link hi -> new record
            dey
            lda     SYMVALP             ; Primary heap (lo)
            bit     EXPANSION_BANK
            bpl     @NOEXP2
            lda     HEAP2WRP            ; Secondary heap write pointer (lo)
@NOEXP2:    sta     (SYMTBLP),Y         ; Update symbol entry chain-link lo -> new record
            ldy     EVAL_Y_SAVE         ; Restore Y
            rts

; ============================================================================
; WRITE_TO_HEAP
;
; Writes the byte in A to the heap, choosing the secondary heap (expansion
; RAM bank 3) if EXPANSION_BANK is set ($80), or the primary heap otherwise.
;
; Secondary heap path:
;   1. Saves X.
;   2. Saves the current bank-control register (BNKCTL).
;   3. Switches BNKCTL to the working bank (low 2 bits = BANK_CTL_BITS).
;   4. Writes A at (HEAP2WRP,X) (X=0 -> write via zero-page indirect).
;   5. Advances HEAP2WRP; calls FATAL_HEAP_FULL if it reaches HEAP2END.
;   6. Restores BNKCTL to bank 0
;   7. Restores X and returns.
;
; Falls through to WRITE_TO_PRIMARY_HEAP if EXPANSION_BANK is clear.
; ============================================================================
WRITE_TO_HEAP:
            bit     EXPANSION_BANK      ; Use secondary heap (expansion RAM)?
            bpl     WRITE_TO_PRIMARY_HEAP
            stx     SAVEX               ; Save X
            pha                         ; Save byte
            lda     BNKCTL              ; Save bank-control register
            and     #<~$03              ; Clear bank-select bits (select bank 3)
            ora     BANK_CTL_BITS       ; Set working bank
            sta     BNKCTL              ; Switch to working bank
            pla                         ; Restore byte
            ldx     #$00
            sta     (HEAP2WRP,X)        ; Write byte at secondary heap pointer
            inc     HEAP2WRP            ; Advance pointer (lo)
            bne     @SKIP
            inc     HEAP2WRP+1          ; Carry into hi byte
@SKIP:      ldx     HEAP2END+1          ; Check if HEAP2WRP == HEAP2END
            cpx     HEAP2WRP+1
            bne     @DONE
            ldx     HEAP2END
            cpx     HEAP2WRP
            bne     @DONE
            jsr     FATAL_HEAP_FULL     ; Fatal: secondary heap full
            ; Does not return
@DONE:      pha
            lda     BNKCTL              ; Switch back to bank 0
            and     #$fc
            ora     #$03
            sta     BNKCTL
            pla
            ldx     SAVEX               ; Restore X
            rts

; ============================================================================
; WRITE_TO_PRIMARY_HEAP
;
; Writes the byte in A at the current primary heap write position (SYMVALP),
; advances SYMVALP, and checks for heap overflow (SYMVALP == HEAPEND -> fatal
; error via FATAL_HEAP_FULL).  Uses X = 0 for the zero-page indirect write.
; ============================================================================
WRITE_TO_PRIMARY_HEAP:
            stx     SAVEX
            ldx     #$00
            sta     (SYMVALP,X)         ; Write byte at SYMVALP using ZP indirect
            inc     SYMVALP             ; Advance heap pointer (lo)
            bne     @SKIP
            inc     SYMVALP+1           ; Carry into hi byte
@SKIP:      ldx     HEAPEND+1           ; Check SYMVALP == HEAPEND
            cpx     SYMVALP+1
            bne     @RETURN
            ldx     HEAPEND
            cpx     SYMVALP
            bne     @RETURN
            jsr     FATAL_HEAP_FULL     ; Fatal: primary heap full
            ; Does not return
@RETURN:    ldx     SAVEX
            rts

; ============================================================================
; EVAL_EXPR_ADVANCE
;
; Advances Y by 1 then falls through into EVAL_FULL_EXPR to parse and
; evaluate a complete arithmetic expression starting at the new Y position.
; ============================================================================
EVAL_EXPR_ADVANCE:
            iny
            ; Fall through

; ============================================================================
; EVAL_FULL_EXPR
;
; Parses and evaluates a full arithmetic expression from the source line
; buffer LINE_BUF, starting at the current Y position (after skipping leading
; spaces via SKIP_LEADING_SPACES).
;
; Expression grammar (simplified):
;   expr  ::= ['<' | '>'] term { op term }
;   term  ::= symbol | decimal | '$' hex | '@' octal | '%' binary
;             | '\'' char | '*' | '[' expr ']'
;   op    ::= '+' | '-' | '*' | '/' | '\' | '&' | '|' | '~'
;
; All arithmetic is 24-bit.  Operators are dispatched through a precedence
; stack: EVAL_FULL_EXPR pushes operands and operators onto the evaluation
; stacks at OPERATOR_STACK (operator) and ACCUM24, then resolves them
; left-to-right.  Bracket groups '[' ']' are handled recursively.
;
; Results:
;   SYMVAL[0..2]  = 24-bit evaluated result
;   URESFLG       = $00 if fully resolved; $C0 if a symbol was undefined;
;                   $40 if result needs further resolution (e.g., forward ref)
;
; Entry:  Y = position in LINE_BUF (before skipping spaces)
;         X = caller's X (preserved via EVAL_X_SAVE)
; Exit:   SYMVAL[0..2] = result; Y = first unconsumed character position
;         A = character at Y; flags reflect URESFLG
; ============================================================================
EVAL_FULL_EXPR:
            stx     EVAL_X_SAVE         ; Save caller's X
            jsr     SKIP_LEADING_SPACES ; Skip leading spaces; Y -> first non-space
            sty     LINE_POS_SAVE       ; Save starting position (expression start)
            lda     LINE_BUF,Y          ; Read first character
            beq     @ERR                ; NUL: empty expression -> error
            ldx     #$00
            stx     URESFLG             ; Clear unresolved flag
            stx     VALUE_STACK_TOP     ; Clear value-stack depth
            stx     OPER_STACK_TOP      ; Clear operator-stack depth
@LOOP:      cmp     LOBYTE              ; '<' -> low-byte extract prefix
            beq     @PUSH
            cmp     HIBYTE              ; '>' -> high-byte extract prefix
            beq     @PUSH
            dey                         ; No prefix: back up one and push implicit '=' op
            lda     #$3d                ; '=' = "load as-is"
@PUSH:      jsr     PUSH_OPERATOR       ; Push operator character onto operator stack
            lda     #$00                ; Push a zero 24-bit value onto value stack
            ldx     VALUE_STACK_TOP
            sta     VALUE_STACK,X
            sta     VALUE_STACK+1,X
            sta     VALUE_STACK+2,X
            inx
            inx
            inx
            stx     VALUE_STACK_TOP     ; Update value-stack depth
            cpx     #$18                ; Stack overflow (8 entries × 3 bytes)?
            bcc     @CONT               ; No: continue
            jsr     ERROR_TOO_COMPLEX   ; Yes: complexity error
@ERR:       jsr     ERROR_SYNTAX        ; Syntax error (empty expression)
            ; Not reached
@CONT:      iny                         ; Advance to next character
            lda     LINE_BUF,Y
            cmp     ADDOP               ; '+' as unary prefix?
            beq     @PUSH2              ; Yes: push it as initial operator
            cmp     SUBSOP              ; '-' as unary prefix?
            beq     @PUSH2
            dey                         ; No unary prefix: back up
            lda     ADDOP               ; Use '+' as implicit initial operator
@PUSH2:     jsr     PUSH_OPERATOR       ; Push this unary-prefix operator
            jsr     ADVANCE_SKIP_LEADING_SPACES ; Skip one char and then spaces
            cmp     LBRACKET            ; '[' -> start grouped sub-expression?
            bne     @EVAL               ; No: evaluate primary term
            jsr     PUSH_OPERATOR       ; Push '[' as operator (bracket nesting)
            iny
            lda     LINE_BUF,Y
            jmp     @LOOP               ; Recurse into grouped sub-expression
@EVAL:      jsr     DISPATCH_PRIMARY_TERM ; Evaluate primary term (symbol, literal, etc.)
@RESOLVE:   jsr     POP_OPERATOR        ; Pop one operator from operator stack
            ldx     #$08
@SEARCH:    dex                         ; Search operator table ARITHOPS for match
            cmp     ARITHOPS,X
            bne     @SEARCH
            txa                         ; X = index of matched operator
            asl     A                   ; × 2 for word-size dispatch table
            tax
            lda     OP_DISPATCH,X       ; Load handler address
            sta     OP_HANDLER
            lda     OP_DISPATCH+1,X
            sta     OP_HANDLER+1
            ldx     VALUE_STACK_TOP
            jsr     @DISPATCH           ; Dispatch: jmp (OP_HANDLER) to execute the operator
            jsr     PEEK_BLANK_CHAR     ; What follows the result?
            beq     @DONE               ; Blank/NUL: expression done
            cmp     RBRACKET            ; ']' closing bracket?
            beq     @CLOSE              ; Yes: close grouped sub-expression
            ldx     #$07
@INFIX:     cmp     ARITHOPS,X          ; Another infix operator?
            beq     @PUSH2              ; Yes: push it and get next operand
            dex
            bpl     @INFIX
@DONE:      jsr     LOAD_OPERATOR_RESULT ; Finalise: apply lo/hi-byte extraction if needed
            ldx     OPER_STACK_TOP      ; Operator stack empty?
            bne     @SYNTAX             ; No -> syntax error (unmatched bracket?)
            lda     SYMVAL+1            ; Check if result fits in 8 bits
            ora     SYMVAL+2
            beq     @IS8BIT             ; Zero high bytes: 8-bit value; OK
            lda     #$40
            ora     URESFLG             ; Set "needs wider value" hint bit
            sta     URESFLG
@IS8BIT:    bit     URESFLG             ; Any unresolved reference?
            bpl     @RETVAL             ; No: return value as-is
            ldx     #$02
@SUBST:     lda     UNDEFSUBST,X        ; Substitute the undefined-op placeholder value
            sta     SYMVAL,X
            dex
            bpl     @SUBST
@RETVAL:    ldx     EVAL_X_SAVE         ; Restore caller's X
            lda     LINE_BUF,Y          ; A = current (stopping) character
            bit     URESFLG             ; Flags reflect URESFLG bit 7
            rts
@CLOSE:     jsr     LOAD_OPERATOR_RESULT ; Finalise sub-expression result
            iny                         ; Skip ']'
            jsr     POP_OPERATOR        ; Pop the matching '[' operator
            cmp     LBRACKET            ; Was it indeed '['?
            bne     @SYNTAX             ; No: stray ']'
            dec     VALUE_STACK_TOP     ; Remove the bracketed operand's stack entry
            dec     VALUE_STACK_TOP
            dec     VALUE_STACK_TOP
            jmp     @RESOLVE            ; Continue resolving operators
@SYNTAX:    jsr     ERROR_SYNTAX        ; Syntax error (unbalanced brackets)
            ; Not reached
@DISPATCH:  jmp     (OP_HANDLER)        ; Indirect dispatch to current operator handler

; ============================================================================
; INFIX OPERATOR DISPATCH TABLE
;
; Eight two-byte entries, indexed by operator order within ARITHOPS string:
;   Index  Char  Handler   Operation
;     0     '~'  XOR24     Exclusive OR (24-bit)
;     1     '|'  OR24      Bitwise OR (24-bit)
;     2     '&'  AND24     Bitwise AND (24-bit)
;     3     '\'  REM24     Remainder (modulo), 24-bit signed
;     4     '/'  DIV24     Division, 24-bit signed
;     5     '*'  MUL24     Multiplication, 24-bit signed
;     6     '-'  SUB24     Subtraction (24-bit)
;     7     '+'  ADD24     Addition (24-bit)
; ============================================================================
OP_DISPATCH:
            .addr   XOR24               ; '~' — XOR
            .addr   OR24                ; '|' — OR
            .addr   AND24               ; '&' — AND
            .addr   REM24               ; '\' — Remainder
            .addr   DIV24               ; '/' — Division
            .addr   MUL24               ; '*' — Multiplication
            .addr   SUB24               ; '-' — Subtraction
            .addr   ADD24               ; '+' — Addition

; ============================================================================
; EVAL_SYMBOL_REFERENCE
;
; Looks up the identifier in IDENT_BUF (collected by EVAL_EXPRESSION) in the
; symbol table via SYMTBL_HASH_LKP.
;
; Results:
;   - Symbol found with known value (C set, N clear):
;       Calls RECORD_XREF to count/log this reference.
;       Loads SYMVAL[0..2] from the symbol table entry.
;       If the symbol is flagged as forward-relative (V bit set in
;       ENTRY_TYPE_FLAGS), sets bit $40 in URESFLG.
;   - Symbol found but value unknown (C set, N set, i.e. byte $80+):
;       Calls ERROR_DUPLICATE (type-mismatch error).
;   - Symbol not found (C clear):
;       Sets SYMVAL = $FFFF/$FF (undefined placeholder).
;       Sets bit 7 of INSTR_FLAGS ($80) to flag "new undefined symbol".
;       Calls EINR_NEW_SYM to add the symbol to the table.
;       Records the line position in UNRES_LINE_POS.
;       Sets URESFLG = $C0 (unresolved + needs retry in pass 2).
;
; Entry:  IDENT_BUF = null-terminated identifier string
; Exit:   SYMVAL[0..2] = symbol value (or $FFFFFF if undefined)
;         URESFLG updated as described
; ============================================================================
EVAL_SYMBOL_REFERENCE:
            jsr     EVAL_EXPRESSION2    ; Pre-scan: collect identifier characters
            jsr     SYMTBL_HASH_LKP     ; Hash-table lookup
            bcc     @NOTFOUND           ; C clear: not found
            bmi     @ERROR              ; N set: type error (macro used as value?)
            jsr     RECORD_XREF         ; Record this usage in the cross-reference
            iny
            lda     (SYMTBLP),Y         ; Load value from symbol table (lo byte)
            sta     SYMVAL
            iny
            lda     (SYMTBLP),Y         ; (mid byte)
            sta     SYMVAL+1
            iny
            lda     (SYMTBLP),Y         ; (hi byte)
            sta     SYMVAL+2
            bit     ENTRY_TYPE_FLAGS    ; V flag in type byte: forward-reference?
            bvc     @UNRESOLV           ; No: unresolved
            lda     ENTRY_TYPE_FLAGS
            and     #$10                ; Bit $10: needs second-pass re-evaluation?
            beq     @RETURN
            lda     URESFLG
            ora     #$40                ; Set "soft-unresolved" bit
            sta     URESFLG
@RETURN:    rts
@UNRESOLV:  lda     #$ff                ; Undefined: load placeholder value $FFFF
            sta     SYMVAL
            sta     SYMVAL+1
            lda     #$00
            sta     SYMVAL+2
            lda     LINE_POS
            sta     UNRES_LINE_POS      ; Record position of unresolved reference
            lda     #$c0
            ora     URESFLG             ; Set URESFLG = $C0 (hard-unresolved)
            sta     URESFLG
            rts

@ERROR:     ldy     LINE_POS
            jsr     ERROR_DUPLICATE
            ; Not reached

@NOTFOUND:  lda     #$ff                ; Not found: placeholder value
            sta     SYMVAL
            sta     SYMVAL+1
            lda     #$00
            sta     SYMVAL+2
            lda     #$80
            sta     INSTR_FLAGS         ; Flag: new symbol (auto-add)
            jsr     EINR_NEW_SYM        ; Add symbol to table
            jmp     @UNRESOLV           ; Record as unresolved

; ============================================================================
; PUSH_OPERATOR
;
; Pushes the operator character in A onto the operator stack at
; OPERATOR_STACK[OPER_STACK_TOP], then increments OPER_STACK_TOP
; (operator stack depth).  Calls ERROR_TOO_COMPLEX (complexity error) if the stack is full
; (depth >= $10 = 16 operators).
; ============================================================================
PUSH_OPERATOR:
            stx     SAVEX
            ldx     OPER_STACK_TOP      ; Operator stack depth
            sta     OPERATOR_STACK,X    ; Push operator at stack top
            inx
            cpx     #$10                ; Stack full?
            bcs     @ERROR              ; Yes: complexity error
            stx     OPER_STACK_TOP      ; Update depth
            ldx     SAVEX
            rts
@ERROR:     jsr     ERROR_TOO_COMPLEX   ; Complexity error (expression too deep)

; ============================================================================
; POP_OPERATOR
;
; Pops the top operator character from the operator stack
; OPERATOR_STACK[OPER_STACK_TOP-1] into A, decrements OPER_STACK_TOP.
; Calls ERROR_SYNTAX (syntax error) if the stack is empty.
; ============================================================================
POP_OPERATOR:
            stx     SAVEX
            ldx     OPER_STACK_TOP      ; Operator stack depth
            bmi     @ERROR              ; Depth negative? -> error
            beq     @ERROR              ; Depth zero? -> error (stack empty)
            lda     OPERATOR_STACK-1,X  ; Read top entry (OPERATOR_STACK-1+depth)
            dec     OPER_STACK_TOP      ; Decrement depth
            ldx     SAVEX
            rts
@ERROR:     jsr     ERROR_SYNTAX        ; Syntax error (unbalanced operators)
            ; Not reached

; ============================================================================
; DISPATCH_PRIMARY_TERM
;
; Dispatches evaluation of a single primary expression term based on the
; leading character in A (already read from LINE_BUF[Y]):
;
;   'A'..'Z'   -> EVAL_SYMBOL_REFERENCE: look up symbol in hash table
;   '0'..'9'   -> PARSE_DECIMAL_LITERAL
;   '$'        -> PARSE_HEX_LITERAL
;   '\'' (')   -> Character literal: next char value stored in SYMVAL
;   '@'        -> PARSE_OCTAL_LITERAL
;   '%'        -> PARSE_BINARY_LITERAL
;   '*'        -> Current program counter (LOCCNT2) -> SYMVAL
;   Anything else -> Syntax error
; ============================================================================
DISPATCH_PRIMARY_TERM:
            cmp     #'A'                ; Below 'A'?
            bcc     @CHKDIG             ; Yes: not a letter
            cmp     #'Z'+1              ; Above 'Z'?
            bcs     @CHKHEX             ; Yes: not a letter
            jsr     EVAL_SYMBOL_REFERENCE ; Identifier: look up symbol
            ldy     LINE_POS            ; Restore Y from symbol lookup
            lda     LINE_BUF,Y          ; A = char after identifier
            rts
@CHKDIG:    cmp     #'0'                ; Below '0'?
            bcc     @CHKHEX             ; Yes: not a digit
            cmp     #'9'+1
            bcs     @CHKHEX             ; Above '9': not a digit
            jmp     PARSE_DECIMAL_LITERAL ; Decimal literal
@CHKHEX:    cmp     HEXPREFIX           ; '$'?
            bne     @CHKQUOT
            jmp     PARSE_HEX_LITERAL   ; Hex literal
@CHKQUOT:   cmp     APOSTROPHE          ; '\'' (apostrophe)?
            bne     @CHKOCT
            iny                         ; Advance past the quote
            lda     LINE_BUF,Y          ; Read the character
            bne     @STORE
            lda     #' '                ; NUL after quote -> treat as space
            dey
@STORE:    sta     SYMVAL              ; Store character code in SYMVAL[0]
            lda     #$00
            sta     SYMVAL+1            ; High bytes = 0
            sta     SYMVAL+2
            iny                         ; Advance past character
            lda     LINE_BUF,Y
            cmp     APOSTROPHE          ; Closing quote?
            bne     @DONE               ; No: single-char form
            iny                         ; Yes: skip closing quote too
            lda     LINE_BUF,Y
@DONE:      rts
@CHKOCT:    cmp     OCTPREFIX           ; '@'?
            bne     @CHKBIN
            jmp     PARSE_OCTAL_LITERAL ; Octal literal
@CHKBIN:    cmp     BINPREFIX           ; '%'?
            bne     @CHKPC
            jmp     PARSE_BINARY_LITERAL ; Binary literal
@CHKPC:     cmp     PCSYMBOL            ; '*' (current PC)?
            bne     @ERROR
            lda     LOCCNT2             ; Load current location counter
            sta     SYMVAL
            lda     LOCCNT2+1
            sta     SYMVAL+1
            lda     #$00
            sta     SYMVAL+2
            iny                         ; Advance past '*'
            lda     LINE_BUF,Y
            rts
@ERROR:     jsr     ERROR_SYNTAX        ; Syntax error: unrecognised term
            ; Not reached

; ============================================================================
; EVAL_EXPRESSION / EVAL_EXPRESSION2
;
; Top-level identifier scanner called from EVAL_SYMBOL_REFERENCE.
;
; EVAL_EXPRESSION: resets Y to 0 before scanning.
; EVAL_EXPRESSION2: scans from the current Y position.
;
; Reads consecutive valid identifier characters (letters 'A'..'Z', digits
; '0'..'9', '_', '?', '.') from LINE_BUF[Y] into IDENT_BUF, null-terminates the
; string, saves its length in IDENT_LEN and advances LINE_POS to
; the character following the identifier.
;
; On return A = character that terminated the scan (not part of identifier).
; Calls ERROR_IDENTIFIER (identifier error) if the first character is not a letter.
; ============================================================================
EVAL_EXPRESSION:
            ldy     #$00                ; Start from beginning of line buffer
EVAL_EXPRESSION2:
            lda     LINE_BUF,Y          ; First character
            cmp     #'A'
            bcc     ERR_IDENT           ; Below 'A': not a valid identifier start
            cmp     #'Z'+1
            bcs     ERR_IDENT           ; Above 'Z': not valid

; ============================================================================
; EVAL_COLLECT_IDENT
;
; Tokenises (collects) an identifier from LINE_BUF starting at the character
; already loaded in A (first character, already validated as 'A'..'Z') and
; at the offset in Y.
;
; Called from EVAL_EXPRESSION / EVAL_EXPRESSION2 immediately after the first
; character passes the 'A'..'Z' range check.  Also called directly by
; ASSEMBLE_ZP_ABS and similar sites that need to tokenise a mnemonic.
;
; Algorithm:
;   Appends characters to IDENT_BUF[0..X] while each successive character is:
;     'A'..'Z'   upper-case letter
;     '0'..'9'   decimal digit
;     '_'        underscore  (UNDERSCORE)
;     '?'        question-mark (QMARK)
;     '.'        period (DOT)
;   Stops on NUL or any other character.
;   If the accumulated length reaches 32 (IDENT_BUF full), calls
;   ERROR_IDENTIFIER and does not return normally.
;
; On return:
;   IDENT_BUF[0..IDENT_LEN-1]  null-terminated identifier string
;   IDENT_LEN                  number of characters stored
;   LINE_POS                   index of the first character after the identifier
;   X                          restored from SAVEX
;   Y                          points one past the last identifier character
;   A                          first character that did not belong to the identifier
; ============================================================================
EVAL_COLLECT_IDENT:
            stx     SAVEX
            ldx     #$00
@STORE:     sta     IDENT_BUF,X         ; Store character in identifier buffer
            inx
            cpx     #$20                ; Buffer full (32 chars)?
            bcc     @NEXT
            jsr     ERROR_IDENTIFIER    ; Identifier too long: error
@NEXT:      iny
            lda     LINE_BUF,Y          ; Next character
            beq     @ISNULL             ; NUL: end of identifier
            cmp     #'A'
            bcc     @CHKDEC             ; Below 'A': might be digit or embedded char
            cmp     #'Z'+1
            bcc     @STORE              ; 'A'..'Z': continue identifier
@CHKEMBED:  cmp     UNDERSCORE          ; '_'?
            beq     @STORE
            cmp     QMARK               ; '?'?
            beq     @STORE
            cmp     DOT                 ; '.'?
            beq     @STORE
@ISNULL:    lda     #$00
            sta     IDENT_BUF,X         ; Null-terminate identifier
            sty     LINE_POS            ; Save position after identifier
            stx     IDENT_LEN           ; Save identifier length
            ldx     SAVEX
            rts
@CHKDEC:    cmp     #'0'                ; '0'..'9'?
            bcc     @CHKEMBED           ; No: check embedded chars
            cmp     #'9'+1
            bcc     @STORE              ; Yes: valid within identifier
            bcs     @CHKEMBED           ; Above '9': check embedded chars
ERR_IDENT:  jsr     ERROR_IDENTIFIER    ; Identifier error (invalid start character)

; ============================================================================
; ARITHMETIC OPERATIONS  (ADD24 – PARSE_HEX_LITERAL)
;
; Each handler is called via the dispatch table at OP_DISPATCH with X pointing to
; the current value-stack frame ACCUM24[0..2] and SYMVAL[0..2] holding the
; second operand.  The result is written back to ACCUM24,X.
;
; Overflow calls ERROR_ARITHMETIC (arithmetic error).
;
; ADD24  ACCUM24,X[24] += SYMVAL[24]
; SUB24  ACCUM24,X[24] -= SYMVAL[24]
; MUL24  ACCUM24,X[24] *= SYMVAL[24]  (signed 24-bit via PREP_SIGN + UMUL24)
; DIV24  ACCUM24,X[24] /= SYMVAL[24]  (signed 24-bit via PREP_SIGN + UDIV24)
; REM24  ACCUM24,X[24] %= SYMVAL[24]  (signed 24-bit, remainder)
; AND24  ACCUM24,X[24] &= SYMVAL[24]
; OR24   ACCUM24,X[24] |= SYMVAL[24]
; XOR24  ACCUM24,X[24] ^= SYMVAL[24]
; ============================================================================

; Addition handler
;
ADD24:      lda     ACCUM24,X           ; Accumulator lo byte
            clc
            adc     SYMVAL              ; + operand lo
            sta     ACCUM24,X
            lda     ACCUM24+1,X         ; Accumulator mid byte
            adc     SYMVAL+1            ; + operand mid (with carry)
            sta     ACCUM24+1,X
            lda     ACCUM24+2,X         ; Accumulator hi byte
            adc     SYMVAL+2            ; + operand hi (with carry)
            sta     ACCUM24+2,X
            bvc     @RETURN             ; Signed overflow?
            jmp     ERR_ARITH           ; Yes: arithmetic error
@RETURN:    rts

; Subtraction handler
;
SUB24:      lda     ACCUM24,X           ; Accumulator lo
            sec
            sbc     SYMVAL              ; - operand lo
            sta     ACCUM24,X
            lda     ACCUM24+1,X         ; Accumulator mid
            sbc     SYMVAL+1            ; - operand mid
            sta     ACCUM24+1,X
            lda     ACCUM24+2,X         ; Accumulator hi
            sbc     SYMVAL+2            ; - operand hi
            sta     ACCUM24+2,X
            bvc     @RETURN             ; Signed overflow?
            jmp     ERR_ARITH           ; Yes: arithmetic error
@RETURN:    rts

; Multiplication handler
;
MUL24:      jsr     PREP_SIGN           ; Prepare: make both operands positive, record sign
            jsr     UMUL24              ; Unsigned 24×24-bit multiply -> SYMVAL[24]
            ; Fall through to APPLY_SIGN (negate result if operand signs differed)

; ============================================================================
; APPLY_SIGN
;
; If bit 7 of SIGN_FLAG (the sign flag set by PREP_MULTIPLY) is set, the result
; in SYMVAL[0..2] is negated (two's complement) and stored back to ACCUM24,X.
; If clear, the result is stored as-is.  Overflow detection included.
; ============================================================================
APPLY_SIGN: bit     SIGN_FLAG           ; Bit 7 set: result should be negative?
            bpl     @POSITIVE           ; No: store result positive
            lda     #$00                ; Negate: 0 - SYMVAL
            sec
            sbc     SYMVAL
            sta     ACCUM24,X
            lda     #$00
            sbc     SYMVAL+1
            sta     ACCUM24+1,X
            lda     #$00
            sbc     SYMVAL+2
            sta     ACCUM24+2,X
            bvc     @RETURN
            jmp     ERR_ARITH           ; Overflow: arithmetic error
@RETURN:    rts
@POSITIVE:  lda     SYMVAL              ; Store positive result
            sta     ACCUM24,X
            lda     SYMVAL+1
            sta     ACCUM24+1,X
            lda     SYMVAL+2
            sta     ACCUM24+2,X
            rts

; Division handler
;
DIV24:      jsr     PREP_SIGN           ; Prepare sign, make operands absolute
            jsr     UDIV24              ; Unsigned 24÷24-bit divide -> TMPVAL (quotient)
            lda     TMPVAL              ; Quotient -> SYMVAL
            sta     SYMVAL
            lda     TMPVAL+1
            sta     SYMVAL+1
            lda     TMPVAL+2
            sta     SYMVAL+2
            jmp     APPLY_SIGN          ; Apply sign to result

; Remainder handler
;
REM24:      jsr     PREP_SIGN           ; Prepare sign, make operands absolute
            jsr     UDIV24              ; Divide -> TMPVAL2 = remainder
            lda     TMPVAL2             ; Remainder -> SYMVAL
            sta     SYMVAL
            lda     TMPVAL2+1
            sta     SYMVAL+1
            lda     TMPVAL2+2
            sta     SYMVAL+2
            jmp     APPLY_SIGN          ; Apply sign to result

; ============================================================================
; PREP_SIGN
;
; Prepares both operands for signed multiplication or division:
;   1. XORs the sign bits (bit 7 of hi bytes) of accumulator (ACCUM24+2,X) and
;      SYMVAL+2 and stores the result in SIGN_FLAG:
;      SIGN_FLAG bit 7 set -> result should be negative.
;   2. If the accumulator is negative (bit 7 of ACCUM24+2,X set), computes its
;      two's complement and stores in TMPVAL[0..2].
;      Otherwise copies the accumulator to TMPVAL.
;   3. If SYMVAL+2 is negative, negates SYMVAL in place.
;
; Entry:  ACCUM24[0..2] = left operand (24-bit signed)
;         SYMVAL[0..2]  = right operand (24-bit signed)
; Exit:   TMPVAL[0..2]  = |left operand|
;         SYMVAL[0..2]  = |right operand|
;         SIGN_FLAG = result sign flag (bit 7 set -> negate result)
; ============================================================================
PREP_SIGN:  lda     ACCUM24+2,X         ; High byte of accumulator
            php                         ; Save flags (N bit = sign of accumulator)
            eor     SYMVAL+2            ; XOR with high byte of right operand
            sta     SIGN_FLAG           ; SIGN_FLAG bit 7 = result sign
            plp                         ; Restore accumulator flags
            bpl     @COPY               ; Accumulator non-negative: copy as-is
            lda     #$00                ; Accumulator negative: negate it -> TMPVAL
            sec
            sbc     ACCUM24,X
            sta     TMPVAL
            lda     #$00
            sbc     ACCUM24+1,X
            sta     TMPVAL+1
            lda     #$00
            sbc     ACCUM24+2,X
            sta     TMPVAL+2
            jmp     @CONT
@COPY:      lda     ACCUM24,X           ; Copy accumulator to TMPVAL (absolute value)
            sta     TMPVAL
            lda     ACCUM24+1,X
            sta     TMPVAL+1
            lda     ACCUM24+2,X
            sta     TMPVAL+2
@CONT:      lda     SYMVAL+2            ; High byte of right operand
            bpl     @DONE               ; Non-negative: already absolute
            lda     #$00                ; Negative: negate in place
            sec
            sbc     SYMVAL
            sta     SYMVAL
            lda     #$00
            sbc     SYMVAL+1
            sta     SYMVAL+1
            lda     #$00
            sbc     SYMVAL+2
            sta     SYMVAL+2
@DONE:      rts

; ============================================================================
; UMUL24
;
; Unsigned 24×24-bit multiplication using the shift-and-add algorithm.
; Computes TMPVAL × SYMVAL -> SYMVAL (low 24 bits of product).
; TMPVAL2 is used as the partial-product accumulator.
; Calls ERROR_ARITHMETIC (overflow error) if the 24-bit product overflows.
;
; Entry:  TMPVAL[0..2] = multiplicand
;         SYMVAL[0..2] = multiplier (shifted out during computation)
; Exit:   SYMVAL[0..2] = product (low 24 bits)
; ============================================================================
UMUL24:     stx     SAVEX
            lda     #$00                ; Initialise accumulator to zero
            sta     TMPVAL2
            sta     TMPVAL2+1
            sta     TMPVAL2+2
            ldx     #$19                ; 25 iterations (24 bits + sentinel)
            clc
@SHIFT:     ror     TMPVAL2+2           ; Right-shift accumulator (absorb carry)
            ror     TMPVAL2+1
            ror     TMPVAL2
            ror     SYMVAL+2            ; Shift multiplier right (LSB -> carry)
            ror     SYMVAL+1
            ror     SYMVAL
            dex
            beq     @RETVAL             ; All bits processed: done
            bcc     @SHIFT              ; Carry clear: current multiplier bit = 0, skip add
            lda     TMPVAL2             ; Carry set: current bit = 1; add multiplicand
            clc
            adc     TMPVAL
            sta     TMPVAL2
            lda     TMPVAL2+1
            adc     TMPVAL+1
            sta     TMPVAL2+1
            lda     TMPVAL2+2
            adc     TMPVAL+2
            sta     TMPVAL2+2
            jmp     @SHIFT
@RETVAL:    ldx     SAVEX
            lda     TMPVAL2             ; Move partial product into SYMVAL
            ora     TMPVAL2+1
            ora     TMPVAL2+2
            bne     ERR_ARITH           ; Non-zero overflow portion: error
            rts

ERR_ARITH:  jsr     ERROR_ARITHMETIC    ; Arithmetic overflow error

; ============================================================================
; UDIV24
;
; Unsigned 24÷24-bit long division using the subtract-and-shift algorithm.
; Computes TMPVAL / SYMVAL -> TMPVAL (quotient), remainder -> TMPVAL2.
; Calls ERR_ARITH -> ERROR_ARITHMETIC (error) if SYMVAL (divisor) is zero.
;
; Entry:  TMPVAL[0..2] = dividend
;         SYMVAL[0..2] = divisor
; Exit:   TMPVAL[0..2] = quotient
;         TMPVAL2[0..2] = remainder
; ============================================================================
UDIV24:     stx     SAVEX
            lda     SYMVAL              ; Check divisor ≠ 0
            ora     SYMVAL+1
            ora     SYMVAL+2
            beq     ERR_ARITH           ; Zero divisor: error
            lda     #$00                ; Initialise remainder to zero
            sta     TMPVAL2
            sta     TMPVAL2+1
            sta     TMPVAL2+2
            ldx     #$19                ; 25 iterations
            clc
            bcc     @RESULT             ; (always branch to avoid entering mid-loop)
@SHIFT:     rol     TMPVAL2             ; Shift remainder left, shift in dividend MSB
            rol     TMPVAL2+1
            rol     TMPVAL2+2
            lda     TMPVAL2             ; Tentative subtract: remainder - divisor
            sec
            sbc     SYMVAL
            sta     DIV_TEMP            ; Scratch for tentative result
            lda     TMPVAL2+1
            sbc     SYMVAL+1
            sta     DIV_TEMP+1
            lda     TMPVAL2+2
            sbc     SYMVAL+2
            bcc     @RESULT             ; Borrow: divisor > remainder; don't subtract
            sta     TMPVAL2+2           ; Accept subtraction: update remainder
            lda     DIV_TEMP+1
            sta     TMPVAL2+1
            lda     DIV_TEMP
            sta     TMPVAL2
@RESULT:    rol     TMPVAL              ; Shift quotient left, bring in result bit
            rol     TMPVAL+1
            rol     TMPVAL+2
            dex
            bne     @SHIFT              ; Repeat for all bits
            ldx     SAVEX
            rts

; ============================================================================
; LOAD_OPERATOR_RESULT
;
; Pops the top operator from the operator stack (via POP_OPERATOR) and applies
; it as an extraction/assignment operation on the value stack:
;
;   '='  -> Copy accumulator value ACCUM24,X -> SYMVAL (no-op assign)
;   '<'  -> Low-byte extract: SYMVAL = ACCUM24[0], high bytes zeroed
;   '>'  -> High-byte extract: SYMVAL = ACCUM24[1], high bytes zeroed
;   Other -> Syntax error
;
; Called after evaluating each primary term to finalise the pending prefix
; operator ('=', '<', or '>') that was pushed at the start of the operand.
; ============================================================================
LOAD_OPERATOR_RESULT:
            jsr     POP_OPERATOR        ; Pop top operator -> A
            stx     SAVEX
            ldx     VALUE_STACK_TOP     ; Value-stack depth
            cmp     #'='                ; '=' assign?
            bne     @CHKLOW
@COPY_RES:  lda     ACCUM24,X           ; Copy accumulator -> SYMVAL
            sta     SYMVAL
            lda     ACCUM24+1,X
            sta     SYMVAL+1
            lda     ACCUM24+2,X
            sta     SYMVAL+2
            ldx     SAVEX
            rts
@CHKLOW:    cmp     LOBYTE              ; '<' low-byte extract?
            bne     @CHKHI
@ZERO:      lda     #$00                ; Zero the upper bytes
            sta     ACCUM24+1,X
            sta     ACCUM24+2,X
            beq     @COPY_RES           ; Copy result (only lo byte matters)
@CHKHI:     cmp     HIBYTE              ; '>' high-byte extract?
            bne     @ERROR
            lda     ACCUM24+1,X         ; Shift mid byte into lo position
            sta     ACCUM24,X
            jmp     @ZERO               ; Zero upper bytes and copy
@ERROR:     jsr     ERROR_SYNTAX        ; Syntax error: unexpected operator
            ; Not reached

; ============================================================================
; Symbol-table integrity checksum
; ============================================================================
CUMULATIVE_XOR:     .byte   $00
RUNNING_EOR_SUM:    .byte   $00

; AND handler
;
AND24:      lda     ACCUM24,X           ; Accumulator lo
            and     SYMVAL              ; AND operand lo
            sta     ACCUM24,X
            lda     ACCUM24+1,X         ; Accumulator mid
            and     SYMVAL+1
            sta     ACCUM24+1,X
            lda     ACCUM24+2,X         ; Accumulator hi
            and     SYMVAL+2
            sta     ACCUM24+2,X
            rts

; OR handler
;
OR24:       lda     ACCUM24,X           ; Accumulator lo
            ora     SYMVAL              ; OR operand lo
            sta     ACCUM24,X
            lda     ACCUM24+1,X         ; Accumulator mid
            ora     SYMVAL+1
            sta     ACCUM24+1,X
            lda     ACCUM24+2,X         ; Accumulator hi
            ora     SYMVAL+2
            sta     ACCUM24+2,X
            rts

; XOR handler
;
XOR24:      lda     ACCUM24,X           ; Accumulator lo
            eor     SYMVAL              ; XOR operand lo
            sta     ACCUM24,X
            lda     ACCUM24+1,X         ; Accumulator mid
            eor     SYMVAL+1
            sta     ACCUM24+1,X
            lda     ACCUM24+2,X         ; Accumulator hi
            eor     SYMVAL+2
            sta     ACCUM24+2,X
            rts

; ============================================================================
; PARSE_HEX_LITERAL
;
; Parses a hexadecimal integer literal from LINE_BUF[Y+1..] (the leading '$'
; is already consumed, Y points past it before the call).
;
; Reads successive hex digits ('0'..'9', 'A'..'F') into SYMVAL[0..2] by
; calling SHIFT_LEFT_HEX for each valid digit.  Stops at the first
; non-hex character and returns it in A.
;
; Sets HEX_DIGIT_FLAG ($FF) as each digit is processed; calls ERROR_SYNTAX (syntax
; error) if no valid digit is found at all.
; ============================================================================
PARSE_HEX_LITERAL:
            jsr     CLEAR_VALUE         ; Clear SYMVAL and HEX_DIGIT_FLAG
@PARSE:     iny                         ; Advance to next character
            lda     LINE_BUF,Y
            sec
            sbc     #'0'                ; Normalise: '0'=0, '9'=9, 'A'->10 etc.
            bcc     RETURN_VALUE        ; < '0': not hex -> done
            cmp     #$0a
            bcc     @DECIMAL           ; 0..9: valid decimal digit
            sbc     #$07                ; A..F: subtract 7 more ($41 - $30 - 7 = 10)
            cmp     #$0a
            bcc     RETURN_VALUE        ; < 10 after correction: gap character, done
            cmp     #$10
            bcs     RETURN_VALUE        ; >= 16: not a hex digit
@DECIMAL:   jsr     SHIFT_LEFT_4        ; Shift SYMVAL left by 4 bits (×16) via SHIFT_LEFT_2
            jsr     SHIFT_LEFT_HEX      ; Add current nibble into SYMVAL, set flag
            jmp     @PARSE              ; Continue with next character

; ============================================================================
; RETURN_VALUE
;
; Common return for value parse routines
; ============================================================================
RETURN_VALUE:
            bit     HEX_DIGIT_FLAG      ; At least one valid digit seen?
            bmi     @RETCHAR            ; Yes: return stopping character
            jsr     ERROR_SYNTAX        ; No: syntax error (bare '$')
            ; Not reached
@RETCHAR:   lda     LINE_BUF,Y          ; Return stopping character in A
            rts

; ============================================================================
; CLEAR_VALUE
;
; Zeroes SYMVAL[0..2] and HEX_DIGIT_FLAG, ready for accumulation of a new
; numeric literal.
; ============================================================================
CLEAR_VALUE:
            lda     #$00
            sta     HEX_DIGIT_FLAG      ; No digits seen yet
            lda     #$00                ; Zero the 24-bit accumulator
            sta     SYMVAL
            sta     SYMVAL+1
            sta     SYMVAL+2
            rts

; ============================================================================
; SHIFT_LEFT_HEX
;
; Shifts SYMVAL[0..2] left by 4 bits (equivalent to ×16 for a new hex digit)
; using SHIFT_LEFT_4 (four successive left-shifts), then adds the low nibble in A
; to SYMVAL[0].  Sets HEX_DIGIT_FLAG to $FF (at least one nibble seen).
; Calls ERROR_ARITHMETIC (overflow) if the carry bit is set after shifting.
; ============================================================================
SHIFT_LEFT_HEX:
            dec     HEX_DIGIT_FLAG      ; $00 -> $FF: mark "digit seen"
            clc
            adc     SYMVAL              ; SYMVAL[0] += nibble value
            sta     SYMVAL
            lda     SYMVAL+1
            adc     #$00                ; Propagate carry
            sta     SYMVAL+1
            lda     SYMVAL+2
            adc     #$00
            sta     SYMVAL+2
            bcc     @RETURN
            jsr     ERROR_ARITHMETIC    ; Overflow: hex literal too large
@RETURN:    rts

; ============================================================================
; PARSE_DECIMAL_LITERAL
;
; Parses an unsigned decimal integer from LINE_BUF[Y].
; Clears SYMVAL via CLEAR_VALUE, then for each '0'..'9' digit:
;   SYMVAL = SYMVAL * 10 + digit  (via MULTIPLY_BY_10 + SHIFT_LEFT_HEX add)
; Stops at the first non-digit; calls ERROR_ARITHMETIC if overflow occurs.
; ============================================================================
PARSE_DECIMAL_LITERAL:
            jsr     CLEAR_VALUE         ; Clear SYMVAL and HEX_DIGIT_FLAG
            beq     @PARSE              ; (always; zero set by jsr CLEAR_VALUE)
@NEXT:      iny                         ; Advance to next character
@PARSE:     lda     LINE_BUF,Y
            sec
            sbc     #'0'                ; Subtract '0' to get digit value
            bcc     @RETCHAR            ; < 0: not a digit
            cmp     #$0a
            bcs     @RETCHAR            ; >= 10: not a decimal digit
            pha                         ; Save digit
            jsr     MULTIPLY_BY_10      ; Multiply current SYMVAL by 10
            pla                         ; Restore digit
            jsr     SHIFT_LEFT_HEX      ; Add digit into SYMVAL (low nibble path)
            jmp     @NEXT               ; Next digit
@RETCHAR:   lda     LINE_BUF,Y          ; Return stopping character
            rts

; ============================================================================
; PARSE_OCTAL_LITERAL
;
; Parses an octal integer from LINE_BUF[Y+1..] (the leading '@' is already
; consumed).  For each '0'..'7' digit, shifts SYMVAL left by 3 bits
; (via SHIFT_LEFT_1 three times) and adds the digit.
; ============================================================================
PARSE_OCTAL_LITERAL:
            jsr     CLEAR_VALUE         ; Clear SYMVAL
@PARSE:     iny
            lda     LINE_BUF,Y
            sec
            sbc     #'0'
            bcc     RETURN_VALUE        ; < '0': done
            cmp     #$08
            bcs     RETURN_VALUE        ; >= 8: not octal
            jsr     SHIFT_LEFT_3        ; Shift SYMVAL left 3 bits (×8)
            jsr     SHIFT_LEFT_HEX      ; Add octal digit
            jmp     @PARSE

; ============================================================================
; PARSE_BINARY_LITERAL
;
; Parses a binary integer from LINE_BUF[Y+1..] (the leading '%' is already
; consumed).  For each '0' or '1' digit, shifts SYMVAL left by 1 bit via
; ROL and ORs in the new bit.  Uses HEX_DIGIT_FLAG to track digit count.
; ============================================================================
PARSE_BINARY_LITERAL:
            jsr     CLEAR_VALUE         ; Clear SYMVAL
@PARSE:     iny
            lda     LINE_BUF,Y
            cmp     #'1'                ; '1'?
            bne     @CHK_ZERO
@SEEN:      dec     HEX_DIGIT_FLAG      ; Mark digit seen ($FF)
            rol     SYMVAL              ; Shift '1' bit into SYMVAL (carry was 1)
            rol     SYMVAL+1
            rol     SYMVAL+2
            bcc     @PARSE              ; No overflow: continue
            jsr     ERROR_ARITHMETIC    ; Carry: binary literal too wide
@CHK_ZERO:  cmp     #'0'                ; '0'?
            beq     @ZERO               ; Yes: shift in a 0 bit (carry = 0)
            jmp     RETURN_VALUE        ; Neither '0' nor '1': done
@ZERO:      clc                         ; Carry = 0 for a zero bit
            bcc     @SEEN               ; Shift 0 bit into SYMVAL

; ============================================================================
; MULTIPLY_BY_10
;
; Multiplies the 24-bit value in SYMVAL[0..2] by 10 in place.
;
; Algorithm  (shift-and-add, avoids a general multiply):
;   1. Copy SYMVAL -> TMPVAL                  (save original value X)
;   2. Shift SYMVAL left 2 bits via SHIFT_LEFT_2    (SYMVAL = X × 4)
;   3. Add TMPVAL back to SYMVAL             (SYMVAL = X×4 + X = X × 5)
;   4. If the 24-bit addition overflowed: call ERROR_ARITHMETIC (arithmetic error,
;      does not return)
;   5. Shift SYMVAL left 1 bit via SHIFT_LEFT_1     (SYMVAL = X×5 × 2 = X × 10)
;
; Entry:  SYMVAL[0..2] = 24-bit multiplicand
; Exit:   SYMVAL[0..2] = SYMVAL × 10
; Uses:   TMPVAL[0..2] (scratch)
; Errors: ERROR_ARITHMETIC (arithmetic overflow) — does not return
;
; Called from: PARSE_DECIMAL_LITERAL once per decimal digit.
; ============================================================================
MULTIPLY_BY_10:
            lda     SYMVAL              ; Save a copy of SYMVAL in TMPVAL (= original X)
            sta     TMPVAL
            lda     SYMVAL+1
            sta     TMPVAL+1
            lda     SYMVAL+2
            sta     TMPVAL+2
            jsr     SHIFT_LEFT_2        ; SYMVAL = SYMVAL × 4 (two left shifts)
            clc
            lda     SYMVAL              ; SYMVAL = SYMVAL×4 + original (= original × 5)
            adc     TMPVAL
            sta     SYMVAL
            lda     SYMVAL+1
            adc     TMPVAL+1
            sta     SYMVAL+1
            lda     SYMVAL+2
            adc     TMPVAL+2
            sta     SYMVAL+2
            bcc     SHIFT_LEFT_1        ; No carry -> safely shift ×2 -> ×10 and return
            jsr     ERROR_ARITHMETIC    ; Carry: 24-bit overflow -> arithmetic error

; ============================================================================
; SHIFT_LEFT_3
;
; Shifts SYMVAL[0..2] left by 3 bits (multiplies by 8) with overflow check.
; Used by PARSE_OCTAL_LITERAL to shift the accumulator by one octal digit
; position before adding the new digit.
;
; Implementation: calls SHIFT_LEFT_1 once, then jumps into SHIFT_LEFT_2
; which performs two more shifts, for a total of 3.
; ============================================================================
SHIFT_LEFT_3:
            jsr     SHIFT_LEFT_1        ; Shift ×2 (1 left shift, overflow checked)
            jmp     SHIFT_LEFT_2        ; Shift ×4 more (2 left shifts) -> total ×8

; ============================================================================
; SHIFT_LEFT_4
;
; Shifts SYMVAL[0..2] left by 4 bits (multiplies by 16) with overflow check.
; Used by PARSE_HEX_LITERAL to shift the accumulator by one hex digit
; position before adding the new nibble.
;
; Implementation: calls SHIFT_LEFT_2 for 2 shifts, then falls
; through into SHIFT_LEFT_2 again for 2 more shifts, totalling 4 shifts.
; ============================================================================
SHIFT_LEFT_4:
            jsr     SHIFT_LEFT_2        ; Shift ×4 (2 left shifts), returns here
            ; Falls through into SHIFT_LEFT_2 for 2 more shifts -> total ×16

; ============================================================================
; SHIFT_LEFT_2
;
; Shifts SYMVAL[0..2] left by 2 bits (multiplies by 4) with overflow check.
; Used directly by MULTIPLY_BY_10 (first step of the ×10 algorithm).
;
; Implementation: calls SHIFT_LEFT_1 for 1 shift, then falls through into
; SHIFT_LEFT_1 for 1 more shift, totalling 2 shifts.
; ============================================================================
SHIFT_LEFT_2:
            jsr     SHIFT_LEFT_1        ; Shift ×2 (1 left shift), returns here
            ; Falls through into SHIFT_LEFT_1 for 1 more shift -> total ×4

; ============================================================================
; SHIFT_LEFT_1
;
; Shifts SYMVAL[0..2] left by 1 bit (multiplies by 2) with overflow check.
; This is the atomic left-shift primitive called by all the helpers above
; and by the ×10 final step.
;
; Entry:  SYMVAL[0..2] = 24-bit value to shift
; Exit:   SYMVAL[0..2] = SYMVAL × 2
; Errors: ERROR_ARITHMETIC if carry is set after the shift (overflow)
; ============================================================================
SHIFT_LEFT_1:
            asl     SYMVAL              ; Shift SYMVAL left 1 bit (lo byte)
            rol     SYMVAL+1            ; Rotate carry into mid byte
            rol     SYMVAL+2            ; Rotate carry into hi byte
            bcc     @RETURN             ; No overflow: done
            jsr     ERROR_ARITHMETIC    ; Carry set: 24-bit overflow -> arithmetic error
@RETURN:    rts

; Copyright notice string
;
COPYRIGHT:  .byte   $0d, "*COPYRIGHT 1981, M.T.U."
            .byte   $0d, "BY B. CARBREY"
            .byte   $0d

; ============================================================================
; SYMTBL_HASH_LKP
;
; Open-address hash table lookup and insert for the symbol table.
; Searches for the null-terminated name in IDENT_BUF among the 2-byte bucket
; entries in the hash table (HASHTST..HASHTEND).  Each bucket holds a
; 2-byte pointer into the primary heap (SYMVALP) where the corresponding
; symbol record is stored.  The name is stored verbatim in the heap starting
; at the pointed-to address, with the last character's bit 7 set as the
; end-of-name marker.
;
; Hash computation:
;   - Initialise with the first character of the name (shifted left 3 bits
;     into HASHTBLP lo, and the raw character into HASHTBLP hi).
;   - Fold remaining characters in pairs:
;       odd  chars -> add to HASHTBLP lo
;       even chars -> XOR into HASHTBLP hi, then shift-XOR into HASHTBLP lo
;   - Final step: add the last character to HASHTBLP hi, mask with HASHSIZEC,
;     and multiply by 2 (since each bucket is 2 bytes) to get the bucket offset.
;
; Collision resolution: linear probing backward (subtract 2 per step),
; wrapping from HASHTST to HASH_LAST at most once.  A full
; wrap-around without a free slot is a fatal error (FATAL_HASH_FULL).
;
; Return codes (in processor flags, on return):
;   C clear                 New entry was created; SYMTBLP -> new heap record.
;   C set, N clear, V clear Symbol found; SYMTBLP -> existing heap record.
;   C set, N set            Entry type mismatch (e.g., macro used as value).
;   C set, V set            Entry is a macro definition.
;
; Side effects on match:
;   ENTRY_TYPE_FLAGS <- type byte (from the MSB-terminated last-name character,
;                       shifted left twice to expose N and V bits).
;   SYM_NAME_OFF <- Y offset to the end-of-name byte within the entry.
;   SYMTBLP <- pointer to the matched heap entry.
;
; Side effects on new entry:
;   SYMBOL_COUNT incremented.
;   If SYMTBL_INIT is clear: name bytes written to primary heap via
;     WRITE_TO_PRIMARY_HEAP; SYM_NAME_OFF set to name length.
;   Bucket at (HASHTBLP) filled with SYMVALP (heap write pointer).
; ============================================================================
SYMTBL_HASH_LKP:
            cld
            lda     #$00
            sta     HASH_PROBE_WRAP     ; Clear collision-wrap sentinel

            ; Initialise hash state from first character of name
            lda     IDENT_BUF           ; First character of name in IDENT_BUF
            sta     HASHTBLP+1          ; Hash hi component = first char
            asl     A                   ; Shift left 3 times -> first_char × 8
            asl     A
            asl     A
            sta     HASHTBLP            ; Hash lo component = first_char × 8

            ldy     #$01                ; Start scanning from second character
            ldy     #$01                ; (duplicate ldy — redundant, no effect)

            ; Fold remaining characters into hash in pairs
@FOLD:      lda     IDENT_BUF,Y         ; Read next character
            beq     @LAST               ; NUL: end of name
            adc     HASHTBLP            ; Odd char: add to hash lo
            sta     HASHTBLP
            iny
            lda     IDENT_BUF,Y         ; Read next character
            beq     @LAST               ; NUL: end of name
            eor     HASHTBLP+1          ; Even char: XOR into hash hi
            sta     HASHTBLP+1
            asl     A                   ; Shift the even char
            eor     HASHTBLP            ; XOR shifted char into hash lo
            sta     HASHTBLP
            jmp     @FOLD               ; Continue with next pair

            ; Finalise hash: fold last character into hi, mask, and scale
@LAST:      dey                         ; Back to the last processed character
            lda     IDENT_BUF,Y
            adc     HASHTBLP+1          ; Add last char to hash hi
            and     HASHSIZEC           ; Mask to table size (e.g. $0F for 4 K buckets)

            ; Compute bucket address: HASHTBLP = HASHTST + hash_index × 2
            sta     HASHTBLP+1          ; HASHTBLP+1 = masked hash (bucket index)
            lda     HASHTBLP            ; HASHTBLP lo: hash accumulator (used as temporary)
            asl     A                   ; × 2 (each bucket is 2 bytes)
            clc
            adc     HASHTST             ; Add hash table base address (lo)
            sta     HASHTBLP
            lda     HASHTBLP+1
            adc     HASHTST+1           ; Add hash table base address (hi)
            sta     HASHTBLP+1          ; HASHTBLP -> computed bucket in hash table

            ; Probe the bucket: check if empty or occupied
HASH_LKP_PROBE:
            ldy     #$00
            lda     (HASHTBLP),Y        ; Read bucket lo byte
            sta     SYMTBLP             ; SYMTBLP lo <- lo byte of stored pointer
            bne     @OCCUPIED           ; Non-zero lo -> slot occupied; compare name
            iny
            lda     (HASHTBLP),Y        ; Read bucket hi byte
            sta     SYMTBLP+1
            bne     @COMPARE            ; Non-zero hi only -> occupied; compare name

            ; Bucket is empty (00 00): create a new entry
            inc     SYMBOL_COUNT        ; SYMBOL_COUNT++
            bne     @SKIP
            inc     SYMBOL_COUNT+1

@SKIP:      lda     SYMVALP+1           ; Write SYMVALP hi byte into bucket hi slot
            sta     (HASHTBLP),Y        ;   (Y=1 here)
            sta     SYMTBLP+1           ; SYMTBLP hi <- SYMVALP hi
            dey                         ; Y = 0
            lda     SYMVALP             ; Write SYMVALP lo byte into bucket lo slot
            sta     (HASHTBLP),Y
            sta     SYMTBLP             ; SYMTBLP lo <- SYMVALP lo

            bit     SYMTBL_INIT         ; SYMTBL_INIT active? (inhibit name write)
            bmi     @DONE               ; Yes: skip name copy (predefined macro registration)

            ; Copy name from IDENT_BUF to primary heap
@COPY:      lda     IDENT_BUF,Y         ; Read name character (Y starts at 0)
            beq     @EONAME             ; NUL: end of name
            jsr     WRITE_TO_PRIMARY_HEAP ; Write character to heap
            iny
            jmp     @COPY

@EONAME:    clc                         ; C clear: new entry created
            sty     SYM_NAME_OFF        ; SYM_NAME_OFF = name length
@DONE:      rts

            ; Bucket occupied (lo byte non-zero): read hi byte and fall through
@OCCUPIED:  iny
            lda     (HASHTBLP),Y        ; Read hi byte of stored pointer
            sta     SYMTBLP+1

            ; Compare stored name (SYMTBLP) against search key (IDENT_BUF)
@COMPARE:   ldy     #$ff                ; Y = $FF; first iny makes Y=0
@CMPLOOP:   iny
            lda     IDENT_BUF,Y         ; Search key character at position Y
            beq     HASH_LKP_CHKEND     ; NUL in search key: check table end
            cmp     (SYMTBLP),Y         ; Compare with stored name character
            beq     @CMPLOOP            ; Match: continue comparing

            ; Mismatch: probe the previous bucket (linear probing backward)
HASH_LKP_MISMATCH:
            lda     HASHTBLP            ; Is HASHTBLP at the lo limit of the table?
            cmp     HASHTST
            beq     @CHKHI              ; At the lo boundary; check hi byte
@PROBE_LP:  sec
            sbc     #$02                ; Back up one bucket (2 bytes)
            sta     HASHTBLP
            bcs     HASH_LKP_PROBE      ; No borrow: probe previous bucket
            dec     HASHTBLP+1          ; Borrow: decrement hi byte
            jmp     HASH_LKP_PROBE      ; Probe previous bucket

@CHKHI:     lda     HASHTBLP+1          ; At lo boundary: check hi byte
            cmp     HASHTST+1
            beq     HASH_LKP_WRAP       ; Both lo and hi at base: wrap around to top
            lda     HASHTBLP            ; Hi byte differs: continue stepping lo
            jmp     @PROBE_LP

            ; Symbol-table corruption crash point: jumped to from PASS2_MAIN_LOOP
            ; when the symbol-table checksum fails between passes.
DO_HALT:    php                         ; Save flags (Z almost certainly clear)
            bne     HALT                ; Z clear -> always taken -> halt the CPU

            ; Wrap-around: restart probing from the end of the hash table
HASH_LKP_WRAP:
            bit     HASH_PROBE_WRAP     ; Have we already wrapped once?
            bpl     HASH_LKP_WRAPPED
            jsr     FATAL_HASH_FULL     ; Wrapped twice: hash table full -> fatal error
            ; Does not return
HASH_LKP_WRAPPED:
            dec     HASH_PROBE_WRAP     ; Mark as wrapped ($00 -> $FF)
            lda     HASH_LAST           ; Address of the last bucket
            sta     HASHTBLP
            lda     HASH_LAST+1
            sta     HASHTBLP+1
            jmp     HASH_LKP_PROBE      ; Continue probing from end of table

            ; Search key NUL reached: check that the stored name also ends here
HASH_LKP_CHKEND:
            lda     (SYMTBLP),Y         ; Read stored name byte at same position
            bpl     HASH_LKP_MISMATCH   ; Bit 7 clear: stored name has not ended -> mismatch
            ; Bit 7 set: end-of-name marker in stored name -> names match
            asl     A                   ; Shift out bit 7: bits 6 and 5 move to N and V
            sta     ENTRY_TYPE_FLAGS    ; Store extracted type flags (for caller inspection)
            sty     SYM_NAME_OFF        ; SYM_NAME_OFF = Y (offset to end-of-name byte)
            bit     ENTRY_TYPE_FLAGS    ; Set N and V flags from type byte for caller
            rts                         ; Return C set (entry found)

HALT:       hlt                         ; Unofficial JAM/HLT — freezes CPU; reset required

; ============================================================================
; SORT_SYMTBL
;
; Sorts the occupied (non-null) hash-table bucket entries into ascending
; alphabetical order by symbol name.  The sort operates entirely on the
; 2-byte bucket pointer pairs in the hash table (HASHTST..HASHTEND); the
; heap records themselves are not moved.
;
; Phase 1 — Compaction:
;   Walks through the hash table from HASHTST.  Whenever an empty (00 00)
;   bucket is found at HASHTBLP, a forward scan with OBJBUFP finds the next
;   non-empty bucket and the two entries are swapped.  This moves all filled
;   buckets to the low end of the table and all empty buckets to the high end.
;   At the end of compaction, SORTED_END/SORTED_END+1 is set to the address of the first
;   empty bucket (= one past the last filled entry).
;
; Phase 2 — Shell sort:
;   Performs a Shell sort on the compacted range HASHTST..SORTED_END using the
;   gap sequence {filled_count/2, gap/2, ...} with the gap halved and
;   rounded down to an even (2-byte aligned) value each pass.  When the
;   gap reaches zero the sort is complete and the routine returns.
;   Comparison and swapping is delegated to COMPARE_SYMBOL_ENTRIES.
;
; Called by PRINT_SYMTBL before iterating over the sorted entries.
; ============================================================================
SORT_SYMTBL:
            cld
            lda     HASHTST             ; Start HASHTBLP at the beginning of the hash table
            sta     HASHTBLP
            lda     HASHTST+1
            sta     HASHTBLP+1

            ; Phase 1: compact non-null entries toward the start of the table
@COMPACT:   ldy     #$00
            lda     (HASHTBLP),Y        ; Check current bucket lo byte
            bne     @ADVANCE            ; Non-empty: advance HASHTBLP, no action needed
            iny
            lda     (HASHTBLP),Y        ; Check current bucket hi byte
            bne     @ADVANCE            ; Non-empty: advance

            ; Found an empty bucket at HASHTBLP; scan forward with OBJBUFP for a filled one
            lda     HASHTBLP
            sta     OBJBUFP
            lda     HASHTBLP+1
            sta     OBJBUFP+1

@SCAN:      lda     OBJBUFP             ; Advance OBJBUFP by one bucket (2 bytes)
            clc
            adc     #$02
            sta     OBJBUFP
            bcc     @ISEOT
            inc     OBJBUFP+1

@ISEOT:     cmp     HASHTEND            ; Has OBJBUFP reached the end of the table?
            bne     @CONT
            lda     OBJBUFP+1
            cmp     HASHTEND+1
            beq     @PH1_COMPLETE       ; Yes: compaction complete
            lda     OBJBUFP

@CONT:      ldy     #$00
            lda     (OBJBUFP),Y         ; Check OBJBUFP bucket lo byte
            bne     @SWAP               ; Non-empty: swap with the empty slot at HASHTBLP
            iny
            lda     (OBJBUFP),Y
            beq     @SCAN               ; OBJBUFP also empty: keep scanning forward

            ; Swap the filled bucket at OBJBUFP with the empty bucket at HASHTBLP
@SWAP:      ldy     #$01
@XCHANGE:   lda     (HASHTBLP),Y        ; Exchange 2 bytes: HASHTBLP[y] ↔ OBJBUFP[y]
            tax
            lda     (OBJBUFP),Y
            sta     (HASHTBLP),Y
            txa
            sta     (OBJBUFP),Y
            dey
            bpl     @XCHANGE            ; Repeat for lo byte (Y: 1 -> 0)

            ; Advance HASHTBLP past this now-filled slot
@ADVANCE:   lda     HASHTBLP
            clc
            adc     #$02                ; Next bucket
            sta     HASHTBLP
            bcc     @SKIP
            inc     HASHTBLP+1

@SKIP:      cmp     HASHTEND            ; Reached end of table?
            bne     @COMPACT
            lda     HASHTBLP+1
            cmp     HASHTEND+1
            bne     @COMPACT            ; No: keep scanning

            ; Phase 1 complete: record the end of the filled region
@PH1_COMPLETE:
            lda     HASHTBLP
            sta     SORTED_END          ; First empty bucket address
            lda     HASHTBLP+1
            sta     SORTED_END+1

            ; Compute initial Shell sort gap = (filled_count / 2), rounded to even
            lda     SORTED_END
            sec
            sbc     HASHTST             ; Length of filled region (lo) = end − start
            sta     SORT_GAP
            lda     SORTED_END+1
            sbc     HASHTST+1
            sta     SORT_GAP+1

            ; Phase 2: Shell sort — halve gap each pass until gap == 0
@SORT:      lsr     SORT_GAP+1          ; Gap >>= 1 (hi byte)
            lda     SORT_GAP
            ror     A                   ; Gap >>= 1 (lo byte, with hi carry)
            and     #$fe                ; Round down to even (2-byte alignment)
            sta     SORT_GAP
            bne     @INIT               ; Gap non-zero: continue sorting
            ora     SORT_GAP+1
            bne     @INIT               ; Gap (hi) non-zero: continue
            rts                         ; Gap == 0: sort complete

            ; Initialise one Shell sort pass with the current gap
@INIT:      lda     HASHTST             ; Compute sort limit = HASHTST + gap
            clc
            adc     SORT_GAP
            sta     SORT_LIMIT
            lda     HASHTST+1
            adc     SORT_GAP+1
            sta     SORT_LIMIT+1
            lda     HASHTST             ; Outer loop pointer starts at HASHTST
            sta     HASHTBLP
            lda     HASHTST+1
            sta     HASHTBLP+1

            ; Outer loop: iterate HASHTBLP from HASHTST to SORT_LIMIT − 1
@OUTER:     lda     HASHTBLP
            sta     OBJBUFP             ; OBJBUFP = current outer-loop element
            lda     HASHTBLP+1
            sta     OBJBUFP+1

            ; Inner (insertion-sort) loop: compare element at OBJBUFP with
            ; the element gap positions ahead of it (at SYMNAMP)
@INNER:     lda     OBJBUFP
            clc
            adc     SORT_GAP            ; SYMNAMP = OBJBUFP + gap
            sta     SYMNAMP
            lda     OBJBUFP+1
            adc     SORT_GAP+1
            sta     SYMNAMP+1

            lda     SYMNAMP             ; Check SYMNAMP < SORTED_END (still within range)?
            cmp     SORTED_END
            lda     SYMNAMP+1
            sbc     SORTED_END+1        ; SYMNAMP+1 − SORTED_END+1 (with borrow)
            bcs     @ADV_OUTER          ; SYMNAMP >= SORTED_END: inner loop done

            lda     OBJBUFP             ; SYMNAMP2 = OBJBUFP (the element to insert)
            sta     SYMNAMP2
            lda     OBJBUFP+1
            sta     SYMNAMP2+1

            ; Compare elements at SYMNAMP2 and SYMNAMP; swap if out of order
@COMP:      jsr     COMPARE_SYMBOL_ENTRIES ; Compare and swap if needed
            bcc     @INNER_DONE         ; C clear: already in order; advance outer loop

            ; Swap happened: step SYMNAMP2 back by one gap and compare again
            lda     SYMNAMP2
            sec
            sbc     SORT_GAP            ; SYMNAMP2 -= gap
            sta     SYMNAMP2
            lda     SYMNAMP2+1
            sbc     SORT_GAP+1
            sta     SYMNAMP2+1
            lda     SYMNAMP2
            cmp     HASHTST             ; SYMNAMP2 < HASHTST (before start)?
            lda     SYMNAMP2+1
            sbc     HASHTST+1
            bcs     @BACK_SCAN          ; Still within range: continue backward scan

            ; Inner loop done: advance OBJBUFP by gap for next outer step
@INNER_DONE:
            lda     OBJBUFP
            clc
            adc     SORT_GAP            ; OBJBUFP += gap
            sta     OBJBUFP
            lda     OBJBUFP+1
            adc     SORT_GAP+1
            sta     OBJBUFP+1
            jmp     @INNER               ; Continue inner loop

            ; SYMNAMP >= SORTED_END: advance outer HASHTBLP by 2 and check limit
@ADV_OUTER: lda     HASHTBLP
            clc
            adc     #$02
            sta     HASHTBLP
            bcc     @SKIP2
            inc     HASHTBLP+1

@SKIP2:     lda     HASHTBLP
            cmp     SORT_LIMIT          ; HASHTBLP < SORT_LIMIT?
            lda     HASHTBLP+1
            sbc     SORT_LIMIT+1
            bcc     @OUTER               ; Yes: keep iterating outer loop
            jmp     @SORT               ; Pass done: halve gap and start next pass

            ; Backward scan: update SYMNAMP = SYMNAMP2 + gap and retry compare
@BACK_SCAN: lda     SYMNAMP2
            clc
            adc     SORT_GAP            ; SYMNAMP = SYMNAMP2 + gap
            sta     SYMNAMP
            lda     SYMNAMP2+1
            adc     SORT_GAP+1
            sta     SYMNAMP+1
            jmp     @COMP               ; Compare the new pair

; ============================================================================
; COMPARE_SYMBOL_ENTRIES
;
; Reads the 2-byte symbol-entry pointers stored at (SYMNAMP2) and (SYMNAMP)
; (each bucket pair holds a pointer into the primary heap), then compares the
; null-MSB-terminated name strings of the two entries lexicographically.
;
; If the entry at SYMNAMP2 sorts after the entry at SYMNAMP (i.e. SYMNAMP2
; comes alphabetically later and should be moved rightward), the two 2-byte
; pointers in the hash table are swapped in place and the routine returns with
; C set.
;
; If the entry at SYMNAMP2 sorts before or equal to SYMNAMP (already in
; order), no swap is performed and the routine returns with C clear.
;
; The end-of-name marker is bit 7 of the last character byte (as stored in
; the heap by SYMTBL_HASH_LKP).
;
; Entry:  SYMNAMP2 -> one hash-table bucket (2-byte entry pointer)
;         SYMNAMP  -> another hash-table bucket (gap positions ahead)
; Exit:   C set   -> entries were out of order and have been swapped
;         C clear -> entries were already in order; no change
; ============================================================================
COMPARE_SYMBOL_ENTRIES:
            ldy     #$01
            lda     (SYMNAMP2),Y        ; Read SYMNAMP2's heap pointer (hi byte)
            sta     SEGHEADP+1
            lda     (SYMNAMP),Y         ; Read SYMNAMP's heap pointer (hi byte)
            sta     MATCHP+1
            dey                         ; Y = 0
            lda     (SYMNAMP2),Y        ; SYMNAMP2's heap pointer (lo byte)
            sta     SEGHEADP
            lda     (SYMNAMP),Y         ; SYMNAMP's heap pointer (lo byte)
            sta     MATCHP
            ; SEGHEADP -> SYMNAMP2's symbol record; MATCHP -> SYMNAMP's symbol record

            lda     (SEGHEADP),Y        ; First character of SYMNAMP2's name (Y=0)
            cmp     (MATCHP),Y          ; Compare with first character of SYMNAMP's name
            bcs     @CONT               ; SYMNAMP2[0] >= SYMNAMP[0]: check further
            rts                         ; SYMNAMP2[0] < SYMNAMP[0]: already in order (C clear)

@CONT:      bne     @SWAP               ; SYMNAMP2[0] > SYMNAMP[0]: out of order -> swap

            ; First chars equal: compare remaining characters one by one
@NEXT:      iny
            lda     (MATCHP),Y          ; Read SYMNAMP's name byte at Y
            bmi     @SWAP               ; Bit 7 set: SYMNAMP name ends first -> SYMNAMP is
                                        ;   shorter/earlier -> SYMNAMP2 comes after -> swap
            lda     (SEGHEADP),Y        ; Read SYMNAMP2's name byte at Y
            bmi     @DONE               ; Bit 7 set: SYMNAMP2 name ends first -> SYMNAMP2 is
                                        ;   shorter/earlier -> already in order (C clear)
            cmp     (MATCHP),Y          ; SYMNAMP2[Y] vs SYMNAMP[Y]
            beq     @NEXT               ; Equal: continue to next character
            bcs     @SWAP               ; SYMNAMP2[Y] > SYMNAMP[Y]: out of order -> swap
            rts                         ; SYMNAMP2[Y] < SYMNAMP[Y]: in order (C clear)

            ; Out of order: swap the two 2-byte pointers in the bucket table
@SWAP:      ldy     #$01
@LOOP:      lda     (SYMNAMP2),Y        ; Exchange SYMNAMP2[y] ↔ SYMNAMP[y]
            tax
            lda     (SYMNAMP),Y
            sta     (SYMNAMP2),Y
            txa
            sta     (SYMNAMP),Y
            dey
            bpl     @LOOP               ; Repeat for lo byte (Y: 1 -> 0)
            rts                         ; Return with C set (swap performed)

@DONE:      clc                         ; SYMNAMP2 ends first: already in order
            rts

; ============================================================================
; PRINT_SYMTBL
;
; Prints the symbol table (and optionally cross-references) to the listing
; output channel after the end of pass 2.
;
; Steps:
;   1. Check XREF_ENABLED; if clear, return immediately (no symbol table).
;   2. Call PRINT_PASS_SUMMARY_FILE to flush the error summary first.
;   3. Build the page header title string at LISTING_BUF:
;        If XREF_MODE is set: "SYMBOL TABLE WITH CROSS REFERENCES"
;        Otherwise:           "SYMBOL TABLE             "
;   4. Reset VALUE_OUTPUT and force a page break via OUTPUT_LINE_PG_BRK.
;   5. Sort the hash table via SORT_SYMTBL.
;   6. Initialise column counters: XREF_COL2 (symbol-value column) = 9,
;        XREF_COL1 (next tab stop) = 1.
;   7. Iterate over the sorted (compacted) hash-table entries
;        (HASHTBLP from HASHTST to SORTED_END):
;      a. Load the entry pointer into SYMTBLP and walk to the end-of-name byte.
;      b. Skip entries whose type bits indicate they are built-in opcode entries
;         or similar internal markers (type & $30 != 0 after shift).
;      c. Call FORMAT_SYMBOL_NAME to write name + type indicator.
;      d. If XREF_MODE: call FORMAT_MACRO_XREF to output xref chain.
;         Otherwise: tab-fill and write value using OUTPUT_VAL_DIGITS2.
;      e. Flush listing line via FLUSH_LINE when column exceeds XRNAMCOLMAX.
;   8. Flush any remaining partial line.
; ============================================================================
PRINT_SYMTBL:
            cld
            bit     XREF_ENABLED        ; Symbol table output enabled?
            bmi     @CONT
            rts                         ; No: skip entirely

@CONT:      jsr     PRINT_PASS_SUMMARY_FILE ; Print pass summary (flushes error counts)

            ; Build title string in listing header at LISTING_BUF
            ldx     #$2d                ; Clear 46 bytes of title area with spaces
            lda     #' '
@CLEAR:     sta     LISTING_BUF,X
            dex
            bpl     @CLEAR

            ; Copy the appropriate header string into the title area
            ldx     #$0c                ; Default: "SYMBOL TABLE" header (13 chars, no xref)
            bit     XREF_MODE           ; Cross-reference mode active?
            bpl     @COPY
            ldx     #$21                ; Yes: full header "SYMBOL TABLE WITH CROSS REFERENCES"
@COPY:      lda     TABLE_HEADER,X      ; Copy from header string table at TABLE_HEADER
            sta     LISTING_BUF,X
            dex
            bpl     @COPY

            lda     #$00
            sta     VALUE_OUTPUT        ; Clear VALUE_OUTPUT before outputting header
            jsr     OUTPUT_LINE_PG_BRK  ; Force a new page and emit the header

            jsr     SORT_SYMTBL         ; Sort the symbol table (compact + Shell sort)

            ; Initialise hash-table scan pointer at HASHTST (start of sorted entries)
            lda     HASHTST
            sta     HASHTBLP
            lda     HASHTST+1
            sta     HASHTBLP+1

            ; Initialise column layout: value column at 9, first tab stop at 1
            ldx     #$09
            stx     XREF_COL2           ; Symbol value column
            ldx     #$01
            stx     XREF_COL1           ; Next tab stop

            ; Main iteration loop: process each sorted hash-table bucket
@LOOP:      ldy     #$00
            lda     (HASHTBLP),Y        ; Load heap pointer from bucket (lo byte)
            sta     SYMTBLP
            iny
            lda     (HASHTBLP),Y        ; Heap pointer (hi byte)
            sta     SYMTBLP+1

            ; Scan to the end-of-name byte (bit 7 set) in the heap entry
            ldy     #$ff
@SCAN:      iny
            lda     (SYMTBLP),Y
            bpl     @SCAN               ; Bit 7 clear: not yet at end of name

            ; Inspect type bits (bits 6 and 5 of end-of-name byte) to filter entries
            asl     A                   ; Bit 7 -> carry (always set here); bit 6 -> N
            bpl     @PROCESS            ; Bit 6 clear: not a macro-only entry; process
            and     #$30                ; Check bits 5 and 4 (now shifted: orig bits 4,3)
            beq     @PROCESS            ; Both zero: process this entry
            jmp     @ADVANCE            ; Internal/built-in entry: skip, advance to next bucket

            ; Process this symbol entry
@PROCESS:   sty     SYM_NAME_OFF        ; SYM_NAME_OFF = Y (offset to end-of-name byte)
            jsr     FORMAT_SYMBOL_NAME  ; Write name + type into listing buffer

            bit     XREF_MODE           ; Cross-reference mode?
            bpl     @NOCROSS            ; No: write value with tab-fill

            ; With cross-references: output xref list
            jsr     FORMAT_MACRO_XREF   ; Write definition line + xref chain
            ldx     #$01                ; Reset listing column to 1 for next entry
            jmp     @ADVANCE

            ; Without cross-references: tab-fill to next stop and write value digits
@NOCROSS:   cpx     XRNAMCOLMAX         ; Has listing column reached the name-column maximum?
            bcs     @FLUSH              ; Yes: flush line and start new one
            lda     XREF_COL1           ; Advance both column counters by LISTTABSPC
            clc
            adc     LISTTABSPC
            sta     XREF_COL1           ; XREF_COL1 += LISTTABSPC
            lda     XREF_COL2
            clc
            adc     LISTTABSPC
            sta     XREF_COL2           ; XREF_COL2 += LISTTABSPC
            lda     #' '                ; Fill listing buffer with spaces up to XREF_COL1
@FILL:      sta     LIST_BUF,X
            inx
            cpx     XREF_COL1
            bcc     @FILL
            jmp     @ADVANCE            ; Advance to next hash-table bucket

            ; Listing line full: flush it and reset column
@FLUSH:     txa                         ; Y = current listing column
            tay
            jsr     FLUSH_LINE          ; Flush listing line
            ldx     #$09                ; Reset XREF_COL2 to 9
            stx     XREF_COL2
            ldx     #$01                ; Reset XREF_COL1 to 1
            stx     XREF_COL1

            ; Advance HASHTBLP to the next bucket (2 bytes per bucket)
@ADVANCE:   lda     HASHTBLP
            clc
            adc     #$02
            sta     HASHTBLP
            bcc     @ISEND
            inc     HASHTBLP+1

            ; Check whether HASHTBLP has reached SORTED_END
@ISEND:     lda     HASHTBLP
            cmp     SORTED_END
            beq     @CHKHI              ; Lo bytes equal: check hi byte
            jmp     @LOOP               ; Not at end: process next bucket
@CHKHI:     lda     HASHTBLP+1
            cmp     SORTED_END+1        ; Compare hi bytes
            beq     @DOFLUSH
            jmp     @LOOP               ; Not at end yet

            ; All buckets processed: flush any remaining partial listing line
@DOFLUSH:   cpx     #$01                ; Any non-empty content in the listing buffer?
            beq     @RETURN             ; No (only the leading space): skip flush
            txa
            tay
            jsr     FLUSH_LINE          ; Flush the last listing line
@RETURN:    rts

; ============================================================================
; FORMAT_SYMBOL_NAME
;
; Formats one symbol table entry (name, type, and value) into the listing
; output buffer starting at LIST_BUF[X].  Leaves X pointing past the last
; written character.
;
; The entry in the primary heap has this layout:
;   bytes 0..N-1   : name characters (ASCII, all bits 7 clear)
;   byte  N        : last name character with bit 7 set as end-of-name marker;
;                    bits 6 and 5 encode the entry type:
;                      bit 6 clear, bit 5 clear -> labelled address / .EQU value
;                      bit 6 set                -> macro definition
;                      bit 5 set (bit 6 clear)  -> external symbol (shown as "****")
;   bytes N+1..N+3 : 24-bit value (lo, mid, hi) for address / .EQU entries
;
; Output format:
;   <name>  [padded to XREF_COL2 with spaces]  $ <value_hex>
;   or, for macros:   <name>  [padded]  MACRO
;   or, for external: <name>  [padded]  $ ****
;
; Entry:  SYMTBLP -> symbol heap record
;         X = current write index into LIST_BUF[]
;         SYM_NAME_OFF = Y offset to end-of-name byte, pre-computed
;         XREF_COL2 = target column for the '$' type indicator
; Exit:   X advanced past the written text; SYMVAL[0..2] set (for value entries)
; ============================================================================
FORMAT_SYMBOL_NAME:
            ldy     #$00
@READN:     lda     (SYMTBLP),Y         ; Read name byte from heap
            bmi     @PAD                ; Bit 7 set: end of name
            cmp     #'*'                ; Mnemonic table terminator '*'? (skip it)
            beq     @SKIP
            sta     LIST_BUF,X          ; Copy name character to listing buffer
            inx
@SKIP:      iny
            bne     @READN              ; Continue until end-of-name byte

            ; Pad the name field with spaces up to the target column (XREF_COL2)
@PAD:       lda     #' '
            sta     LIST_BUF,X
            inx
            cpx     XREF_COL2           ; Reached target column?
            bcc     @PAD               ; No: keep padding

            ; Emit the '$' type indicator
            lda     #'$'
            sta     LIST_BUF,X
            inx

            ; Decode entry type from the end-of-name byte (bit 7 was set)
            lda     (SYMTBLP),Y         ; Re-read the end-of-name byte
            asl     A                   ; Discard bit 7 (always 1); bit 6 -> carry
            asl     A                   ; Bit 6 (now N): 1 = macro or external
            bcs     @MACRO              ; Carry set (original bit 6 = 1): macro entry
            bmi     @EMIT               ; N set (original bit 5 = 1): regular value entry
                                        ;   wait: after two ASL, the bit that was originally
                                        ;   bit 5 is now bit 7 (N flag); if N set -> value
                                        ;   (this covers .EQU / label entries with type $A0)

            ; External symbol (not macro, not plain value): emit "****" placeholder
            lda     #'*'
            sta     LIST_BUF,X
            inx
            sta     LIST_BUF,X
            inx
            sta     LIST_BUF,X
            inx
            sta     LIST_BUF,X
            inx
            rts

            ; Regular address / .EQU entry: read 24-bit value and emit hex digits
@EMIT:      iny
            lda     (SYMTBLP),Y         ; Value lo byte
            sta     SYMVAL
            iny
            lda     (SYMTBLP),Y         ; Value mid byte
            sta     SYMVAL+1
            iny
            lda     (SYMTBLP),Y         ; Value hi byte
            sta     SYMVAL+2
            jmp     OUTPUT_VAL_DIGITS2               ; OUTPUT_VAL_DIGITS: emit up to 6 hex digits

            ; Macro entry: replace the preceding '$' with 'M' and write "ACRO"
@MACRO:     dex                         ; Step back to overwrite the '$' character
            lda     #'M'
            sta     LIST_BUF,X
            inx
            lda     #'A'
            sta     LIST_BUF,X
            inx
            lda     #'C'
            sta     LIST_BUF,X
            inx
            lda     #'R'
            sta     LIST_BUF,X
            inx
            lda     #'O'
            sta     LIST_BUF,X
            inx
            rts

; ============================================================================
; FORMAT_MACRO_XREF
;
; Formats the cross-reference listing entry for one symbol or macro.
; Called by PRINT_SYMTBL when XREF_MODE is active, after FORMAT_SYMBOL_NAME
; has written the name into the listing buffer.
;
; Layout emitted into the listing buffer (starting at LIST_BUF[X]):
;   <spaces to column 16>  <definition_line>  <xref_line> ...
;
; The definition line number is read from the symbol table entry:
;   at offset SYM_NAME_OFF + 6 from SYMTBLP (lo byte, hi byte).
;
; Cross-reference records form a backwards-linked list in the primary heap
; (or secondary heap if EXPANSION_BANK is set).  Each 4-byte xref record:
;   bytes 0–1: pointer to the previous xref record for this symbol (or 00 00)
;   bytes 2–3: source line number where this symbol was referenced
;
; The routine reverses this list by following it to the earliest record,
; then prints line numbers from oldest to newest.
;
; When the listing column X reaches or exceeds XRNUMCOLMAX, the current
; listing line is flushed via FLUSH_LINE and a new line is started with 16 spaces
; of indentation.
;
; Entry:  SYMTBLP -> symbol heap entry; X = current listing column
;         SYM_NAME_OFF; EXPANSION_BANK / BNKCTL reflect current bank
; Exit:   X updated; SYMTBLP clobbered; listing line(s) flushed as needed
; ============================================================================
FORMAT_MACRO_XREF:
            lda     #' '                ; Pad to column 16 with spaces
@PAD:       sta     LIST_BUF,X
            inx
            cpx     #$10                ; Reached column 16?
            bcc     @PAD

            ; Locate the definition line number field in the symbol table entry
            lda     SYM_NAME_OFF        ; SYM_NAME_OFF: offset to end-of-name byte
            clc
            adc     #$06                ; Skip type(1) + value_ptr(2) + ref_count(1)
                                        ;   + xref_chain_ptr(2) = 6 bytes past name end
            tay
            lda     (SYMTBLP),Y         ; Definition line number (lo byte)
            sta     SYMVAL
            iny
            lda     (SYMTBLP),Y         ; Definition line number (hi byte)
            sta     SYMVAL+1
            lda     #$00
            sta     SYMVAL+2            ; Zero-extend to 24 bits for FORMAT_DECIMAL_16

            ; Advance SYMTBLP to point at the xref chain link field (4 bytes before
            ; the definition line number, since definition = chain + 4 in the layout)
            iny
            tya                         ; Y = offset to definition line lo (SYM_NAME_OFF+6)
            clc
            adc     SYMTBLP
            sta     SYMTBLP
            lda     SYMTBLP+1
            adc     #$00
            sta     SYMTBLP+1           ; SYMTBLP now -> byte SYM_NAME_OFF+8 in the entry

            jsr     FORMAT_DECIMAL_16   ; Write definition line number

            lda     #' '                ; Space after definition line number
            sta     LIST_BUF,X
            inx

            ; Position SYMTBLP at the xref chain link field (4 bytes back)
            lda     SYMTBLP
            sec
            sbc     #$04
            sta     SYMTBLP
            lda     SYMTBLP+1
            sbc     #$00
            sta     SYMTBLP+1           ; SYMTBLP -> xref chain link (2 bytes)

            ; Read the first xref chain link (pointer to most-recent xref record)
            ldy     #$00
            lda     (SYMTBLP),Y         ; Chain link lo byte
            pha
            iny
            lda     (SYMTBLP),Y         ; Chain link hi byte
            sta     SYMTBLP+1
            pla
            sta     SYMTBLP             ; SYMTBLP -> most-recent xref record (or 00 00)

            ora     SYMTBLP+1           ; Is the chain empty (00 00)?
            bne     @CONT               ; No: follow the chain
            jmp     @DONE               ; Yes: no xref entries; done

            ; Reverse the xref linked list in place (forward-link while printing).
            ; The list is singly-linked from newest to oldest; we need to walk to
            ; the beginning and print in forward order.
@CONT:      lda     #$00                ; OBJBUFP = 0 (will hold the reversed "next" ptr)
            sta     OBJBUFP
            sta     OBJBUFP+1

            ; Switch to expansion RAM bank if available
            bit     EXPANSION_BANK
            bpl     @WALK               ; No expansion RAM: skip bank switch
            lda     BNKCTL
            and     #<~$03              ; Bank 3
            ora     BANK_CTL_BITS       ; Select working bank
            sta     BNKCTL

            ; Walk backward through the xref chain, reversing links as we go
@WALK:      lda     SYMTBLP
            sec
            sbc     #$04                ; Step 4 bytes back to reach this record's link field
            sta     SYMTBLP
            lda     SYMTBLP+1
            sbc     #$00
            sta     SYMTBLP+1

            ldy     #$00
            lda     (SYMTBLP),Y         ; Read the previous-record pointer (lo)
            sta     SYMNAMP
            iny
            lda     (SYMTBLP),Y         ; Previous-record pointer (hi)
            sta     SYMNAMP+1

            ; Overwrite the link field with the forward pointer (reverse the link)
            lda     OBJBUFP+1
            sta     (SYMTBLP),Y         ; Write reversed link hi
            dey
            lda     OBJBUFP
            sta     (SYMTBLP),Y         ; Write reversed link lo

            lda     SYMNAMP             ; Is the previous-record pointer non-null?
            ora     SYMNAMP+1
            beq     @PRINT              ; Zero: reached the end -> start printing

            ; Not yet at start: continue reversing
            lda     SYMTBLP
            sta     OBJBUFP             ; OBJBUFP = current record (for next iteration)
            lda     SYMTBLP+1
            sta     OBJBUFP+1
            lda     SYMNAMP
            sta     SYMTBLP             ; SYMTBLP = previous record
            lda     SYMNAMP+1
            sta     SYMTBLP+1
            jmp     @WALK               ; Continue traversal

            ; SYMTBLP is now the first (oldest) xref record; print from here forward
@PRINT:     ldy     #$02                ; Offset 2 within xref record = source line number
            lda     (SYMTBLP),Y         ; Line number lo byte
            sta     SYMVAL
            iny
            lda     (SYMTBLP),Y         ; Line number hi byte
            sta     SYMVAL+1

            ; Restore RAM bank (main bank 0) after reading from expansion RAM
            bit     EXPANSION_BANK
            bpl     @FMT16
            lda     BNKCTL
            and     #$fc                ; Mask out other bits
            ora     #$03                ; Bank 0 = main data bank
            sta     BNKCTL

@FMT16:     jsr     FORMAT_DECIMAL_16   ; Write line number in decimal

            lda     #' '                ; Space after line number
            sta     LIST_BUF,X
            inx

            ; Check if listing line is full
            cpx     XRNUMCOLMAX         ; Column >= XRNUMCOLMAX?
            bcc     @CONT2              ; No: continue on this line

            ; Flush current line and start a new indented line
            txa
            tay
            jsr     FLUSH_LINE          ; Flush listing line
            ldx     #$01
            lda     #' '
@INDENT:    sta     LIST_BUF,X          ; Indent new line to column 16
            inx
            cpx     #$10
            bcc     @INDENT

            ; Follow the forward-reversed link to the next (more-recent) xref record
@CONT2:     bit     EXPANSION_BANK
            bpl     @CONT3
            lda     BNKCTL
            and     #<~$03              ; Bank 3
            ora     BANK_CTL_BITS       ; Switch to expansion RAM
            sta     BNKCTL

@CONT3:     ldy     #$00
            lda     (SYMTBLP),Y         ; Read the (now-reversed) forward link lo byte
            iny
            ora     (SYMTBLP),Y         ; OR with hi byte: is it 00 00?
            beq     @DONE               ; Yes: last record; done

            ldy     #$00
            lda     (SYMTBLP),Y         ; Read forward link lo byte
            pha
            iny
            lda     (SYMTBLP),Y         ; Forward link hi byte
            sta     SYMTBLP+1
            pla
            sta     SYMTBLP             ; SYMTBLP -> next xref record
            jmp     @PRINT              ; Print next line number

            ; All xref records printed; restore main bank if needed
@DONE:      bit     EXPANSION_BANK
            bpl     @CONT4
            lda     BNKCTL
            and     #$fc                ; Mask out other bits
            ora     #$03                ; Restore bank 0
            sta     BNKCTL

            ; Flush the final listing line if any content was written past column 16
@CONT4:     cpx     #$10                ; Only indentation written (column = 16)?
            beq     @RETURN             ; Yes: nothing to flush
            txa
            tay
            jsr     FLUSH_LINE          ; Flush the last xref line
@RETURN:    rts

; ============================================================================
; FORMAT_DECIMAL_16
;
; Converts the 16-bit unsigned value in SYMVAL/SYMVAL+1 to a decimal ASCII
; string and writes it into the listing buffer at LIST_BUF[X], advancing X.
;
; Algorithm: successive subtraction using the powers-of-ten table at POW_OF_10
; (entries: 10000, 1000, 100, 10).  For each power of ten, subtracts
; repeatedly and counts, building each decimal digit.  The units digit is
; derived from the remainder.
;
; Leading zeros are suppressed for the ten-thousands digit (the first
; subtraction level, Y=6) by skipping its output when the count remains '0'.
; Remaining digits (thousands through units) are always emitted.
;
; Entry:  SYMVAL/SYMVAL+1 = 16-bit value (0..65535)
;         X = write index into LIST_BUF[]
;         Y = caller's Y (saved in FDEC16_Y_SAVE and restored on exit)
; Exit:   LIST_BUF[X..] = decimal digit characters; X advanced past the digits
;         SYMVAL/SYMVAL+1 clobbered (reduced to zero during computation)
; ============================================================================
FORMAT_DECIMAL_16:
            sty     FDEC16_Y_SAVE       ; Save caller's Y
            ldy     #$06                ; Start at the highest power (10000, index 6)

@LOOP:      lda     #'0'                ; Current digit starts at '0'
            sta     DEC_DIGIT

            ; Subtract the current power of ten until the value goes negative
@SUBST:     lda     SYMVAL
            sec
            sbc     POW_OF_10,Y         ; Subtract power[Y] (lo byte)
            sta     SYMVAL
            lda     SYMVAL+1
            sbc     POW_OF_10+1,Y       ; Subtract power[Y] (hi byte)
            sta     SYMVAL+1
            bcc     @UNDO               ; Borrow: subtracted too many times
            inc     DEC_DIGIT           ; No borrow: increment digit count
            jmp     @SUBST              ; Subtract again

            ; Undo the last over-subtraction and restore SYMVAL to the remainder
@UNDO:      lda     SYMVAL
            adc     POW_OF_10,Y         ; Add back lo byte of current power
            sta     SYMVAL
            lda     SYMVAL+1
            adc     POW_OF_10+1,Y       ; Add back hi byte
            sta     SYMVAL+1

            ; Write digit to listing buffer (suppress leading zero for ten-thousands)
            cpy     #$06                ; First (most-significant) digit?
            beq     @SKIP               ; Yes: suppress if '0' (leading zero)
            lda     DEC_DIGIT           ; Load computed ASCII digit
            sta     LIST_BUF,X          ; Write to listing buffer
            inx

@SKIP:      dey                         ; Move to next lower power of ten
            dey                         ; (table entries are .word, 2 bytes apart)
            bpl     @LOOP               ; Y >= 0: continue

            ; Write the units digit (remainder in SYMVAL, 0..9)
            lda     SYMVAL
            clc
            adc     #'0'                ; Convert remainder to ASCII
            sta     LIST_BUF,X
            inx
            ldy     FDEC16_Y_SAVE       ; Restore caller's Y
            rts

; ============================================================================
; POWERS-OF-TEN TABLE
;
; Four 16-bit little-endian entries used by FORMAT_DECIMAL_16 for successive-
; subtraction decimal conversion.  Indexed with Y = 0, 2, 4, 6 (lo/hi pairs).
;
;   Y=0: $000A =    10
;   Y=2: $0064 =   100
;   Y=4: $03E8 = 1,000
;   Y=6: $2710 = 10,000
; ============================================================================
POW_OF_10:
            .word   $000a               ; 10
            .word   $0064               ; 100
            .word   $03e8               ; 1,000
            .word   $2710               ; 10,000

; ============================================================================
; INSTRUCTION SIZE TABLE
;
; 13-entry byte table indexed by ADDR_MODE (0–12), giving the total byte
; count (opcode + operand bytes) for each 6502 addressing mode.  Used by
; HANDLE_DIRECTIVE to advance LOCCNT by the correct amount when
; processing instructions or directive operands.
;
; Index  Addressing mode                          Size
;   0    Accumulator / Implied                      1
;   1    Immediate            (#expr)               2
;   2    Zero-page / Absolute (expr)                2
;   3    Zero-page,X          (expr,X) ZP           2
;   4    Zero-page,Y          (expr,Y) ZP           2
;   5    Indexed-indirect X   ((expr,X))            2
;   6    Indirect-indexed Y   ((expr),Y)            2
;   7    Absolute             (expr)   ABS          3
;   8    Absolute,X           (expr,X) ABS          3
;   9    Absolute,Y           (expr,Y) ABS          3
;  10    Pure indirect        ((expr))              3
;  11    (directive-internal, EQU / label record)   1
;  12    (directive-internal, two-byte directive)   2
; ============================================================================
INST_SIZE:  .byte   $01                 ; 0  — Accumulator / Implied
            .byte   $02                 ; 1  — Immediate
            .byte   $02                 ; 2  — Zero-page or absolute (resolved later)
            .byte   $02                 ; 3  — Zero-page,X
            .byte   $02                 ; 4  — Zero-page,Y
            .byte   $02                 ; 5  — (zp,X)
            .byte   $02                 ; 6  — (zp),Y
            .byte   $03                 ; 7  — Absolute
            .byte   $03                 ; 8  — Absolute,X
            .byte   $03                 ; 9  — Absolute,Y
            .byte   $03                 ; 10 — Pure indirect
            .byte   $01                 ; 11 — Directive-internal (1 byte)
            .byte   $02                 ; 12 — Directive-internal (2 bytes)

PROG_SIZE = * - START

            .bss

; =============================================================================
; BSS SEGMENT — UNINITIALISED BUFFERS
;
; Laid out at fixed offsets above the code image.  All regions are page-aligned.
;
; $3400–$34FF  DDEFSBUF  : Direct-definitions buffer (command-line "==" text)
; $3500–$35FF  OBJCOBUF  : Object-code output buffer (256 bytes; flushed per full page)
; $3600–$39FF  SRCPRBUF  : Source program read buffer (1 KB)
; $3A00–$3EFF  LSTNGBUF  : Listing output accumulation buffer (1.25 KB)
; $3F00–$ABFF  (heap)    : Symbol table primary heap — names, values, xref records
; =============================================================================

DDEFSBUF:   .res    $100                ; Direct-definitions text buffer (256 bytes)
OBJCOBUF:   .res    $100                ; Object-code output buffer (256 bytes)
SRCPRBUF:   .res    $400                ; Source program read buffer (1 KB)
LSTNGBUF:   .res    $500                ; Listing output accumulation buffer (1.25 KB)

            .segment "BSS2"

; =============================================================================
; BSS2 SEGMENT — SYMBOL TABLE HASH TABLE AND MACRO ARGUMENT STACK
;
; $AC00–$BBFF  SYMHASHT  : Open-address hash table (4 KB; 2,048 two-byte buckets)
;                          Each bucket holds a 16-bit pointer into the primary heap
;                          (HEAPST..HEAPEND) for the corresponding symbol entry.
;                          Size is controlled by HASHSIZEC ($0F mask = 4 K buckets).
; $BC00–$BDFF  MACSTACK  : Macro invocation argument stack (512 bytes)
;                          Grows upward; MARGWRTP is the write pointer,
;                          MARGSTRP is the read pointer, MACSPTR tracks
;                          the current frame base.
; =============================================================================

SYMHASHT:   .res    $1000               ; Symbol table hash table (4 KB, 2 048 buckets)
MACSTACK:   .res    $200                ; Macro invocation argument stack (512 bytes)

            .segment "BSS3"

; =============================================================================
; BSS3 SEGMENT — SECONDARY HEAP (EXPANSION RAM, BANK 3)
;
; $0000–$FEFF (in bank 3)  SCNDHEAP : Secondary heap for macro body text,
;                                     cross-reference records, and symbol names
;                                     when expansion RAM is detected by PROBE_MEMORY.
;                                     HEAP2WRP and HEAP2PRP manage write/read pointers.
;                                     EXPANSION_BANK bit 7 gates all access.
; =============================================================================

SCNDHEAP:   .res    $FF00               ; Secondary heap in expansion RAM (≈ 64 KB)

            .data

            ; CODOS loadable file header for the TABLE data segment
            .byte   $58                 ; CODOS loadable file signature byte
            .byte   $00                 ; Memory overlay flag
            .byte   $00                 ; Memory bank
            .byte   $00                 ; Reserved
            .addr   START               ; Entry point address
            .addr   TABLE               ; Load address for this segment
            .word   DATA_SIZE           ; Size of the TABLE data image

; =============================================================================
; MNEMONIC / OPCODE TABLE  (TABLE, .data segment)
;
; A linear array of variable-length entries terminated by a $00 byte.
; Loaded at a separate address (TABLE) from the code segment.
; The table is searched by LOOKUP_OPCODE via the hash table (which
; is initialised from this table by INIT_SYMTBL at startup).
;
; Entry types and their layouts:
;
; Type $F8 — Multi-mode 6502 instruction (supports several addressing modes):
;   .byte  length          ; Total entry length in bytes (always $13 = 19)
;   .byte  "MNEM*"         ; 4-char mnemonic + '*' terminator
;   .byte  $F8             ; Type byte
;   .addr  $0000           ; Handler address (unused for instructions; $0000)
;   .byte  op_acc          ; Opcode for accumulator mode    ($FF = not available)
;   .byte  op_imm          ; Opcode for immediate mode
;   .byte  op_zp           ; Opcode for zero-page mode
;   .byte  op_zpx          ; Opcode for zero-page,X mode
;   .byte  op_zpy          ; Opcode for zero-page,Y mode
;   .byte  op_indx         ; Opcode for (zp,X) mode
;   .byte  op_indy         ; Opcode for (zp),Y mode
;   .byte  op_abs          ; Opcode for absolute mode
;   .byte  op_absx         ; Opcode for absolute,X mode
;   .byte  op_absy         ; Opcode for absolute,Y mode
;   .byte  op_ind          ; Opcode for indirect mode
;
; Type $F4 — Branch instruction (always 2 bytes: opcode + 8-bit signed offset):
;   .byte  length          ; Always $09 = 9
;   .byte  "MNEM*"         ; 4-char mnemonic + '*'
;   .byte  $F4             ; Type byte
;   .addr  $0000           ; Handler address (unused)
;   .byte  opcode          ; Single branch opcode
;
; Type $F0 — Implied / inherent instruction (1 byte, no operand):
;   .byte  length          ; Always $09 = 9
;   .byte  "MNEM*"         ; 4-char mnemonic + '*'
;   .byte  $F0             ; Type byte
;   .addr  $0000           ; Handler address (unused)
;   .byte  opcode          ; Single opcode byte
;
; Type $E8 — Assembler directive (pseudo-op):
;   .byte  length          ; Variable (name length + 4)
;   .byte  ".NAME*"        ; Directive name + '*'
;   .byte  $E8             ; Type byte
;   .addr  handler         ; Address of the directive handler routine
;
; .byte $00 ends the table.
;
; All mnemonic / directive names are terminated with the '*' character ($2A),
; which LOOKUP_OPCODE appends to the identifier buffer before hash lookup.
; =============================================================================

TABLE:      .byte   $13             ; Entry length
            .byte   "ADC*"          ; Mnemonic
            .byte   $f8             ; Type: multi-mode instruction
            .addr   $0000           ; Handler (unused)
            .byte   $ff             ; Accumulator (not available)
            .byte   $69             ; Immediate
            .byte   $65             ; Zero-page
            .byte   $75             ; Zero-page,X
            .byte   $ff             ; Zero-page,Y (not available)
            .byte   $61             ; (zp,X)
            .byte   $71             ; (zp),Y
            .byte   $6d             ; Absolute
            .byte   $7d             ; Absolute,X
            .byte   $79             ; Absolute,Y
            .byte   $ff             ; Indirect (not available)

            .byte   $13
            .byte   "AND*"
            .byte   $f8
            .addr   $0000
            .byte   $ff
            .byte   $29
            .byte   $25
            .byte   $35
            .byte   $ff
            .byte   $21
            .byte   $31
            .byte   $2d
            .byte   $3d
            .byte   $39
            .byte   $ff

            .byte   $13
            .byte   "ASL*"
            .byte   $f8
            .addr   $0000
            .byte   $0a             ; Accumulator
            .byte   $ff
            .byte   $06
            .byte   $16
            .byte   $ff
            .byte   $ff
            .byte   $ff
            .byte   $0e
            .byte   $1e
            .byte   $ff
            .byte   $ff

            .byte   $09             ; Entry length
            .byte   "BCC*"          ; Branch if Carry Clear
            .byte   $f4             ; Type: branch instruction
            .addr   $0000
            .byte   $90             ; Opcode

            .byte   $09
            .byte   "BLT*"          ; Branch if Less Than (alias for BCC)
            .byte   $f4
            .addr   $0000
            .byte   $90

            .byte   $09
            .byte   "BCS*"          ; Branch if Carry Set
            .byte   $f4
            .addr   $0000
            .byte   $b0

            .byte   $09
            .byte   "BGE*"          ; Branch if Greater or Equal (alias for BCS)
            .byte   $f4
            .addr   $0000
            .byte   $b0

            .byte   $09
            .byte   "BEQ*"          ; Branch if Equal (Z=1)
            .byte   $f4
            .addr   $0000
            .byte   $f0

            .byte   $13
            .byte   "BIT*"
            .byte   $f8
            .addr   $0000
            .byte   $ff
            .byte   $ff             ; Immediate (not available)
            .byte   $24             ; Zero-page
            .byte   $ff
            .byte   $ff
            .byte   $ff
            .byte   $ff
            .byte   $2c             ; Absolute
            .byte   $ff
            .byte   $ff
            .byte   $ff

            .byte   $09
            .byte   "BMI*"          ; Branch if Minus (N=1)
            .byte   $f4
            .addr   $0000
            .byte   $30

            .byte   $09
            .byte   "BNE*"          ; Branch if Not Equal (Z=0)
            .byte   $f4
            .addr   $0000
            .byte   $d0

            .byte   $09
            .byte   "BPL*"          ; Branch if Plus (N=0)
            .byte   $f4
            .addr   $0000
            .byte   $10

            .byte   $09             ; Entry length
            .byte   "BRK*"          ; Software interrupt
            .byte   $f0             ; Type: implied instruction
            .addr   $0000
            .byte   $00             ; Opcode

            .byte   $09
            .byte   "BVC*"          ; Branch if Overflow Clear (V=0)
            .byte   $f4
            .addr   $0000
            .byte   $50

            .byte   $09
            .byte   "BVS*"          ; Branch if Overflow Set (V=1)
            .byte   $f4
            .addr   $0000
            .byte   $70

            .byte   $09
            .byte   "CLC*"          ; Clear Carry
            .byte   $f0
            .addr   $0000
            .byte   $18

            .byte   $09
            .byte   "CLD*"          ; Clear Decimal mode
            .byte   $f0
            .addr   $0000
            .byte   $d8

            .byte   $09
            .byte   "CLI*"          ; Clear Interrupt disable
            .byte   $f0
            .addr   $0000
            .byte   $58

            .byte   $09
            .byte   "CLV*"          ; Clear oVerflow
            .byte   $f0
            .addr   $0000
            .byte   $b8

            .byte   $13
            .byte   "CMP*"
            .byte   $f8
            .addr   $0000
            .byte   $ff
            .byte   $c9
            .byte   $c5
            .byte   $d5
            .byte   $ff
            .byte   $c1
            .byte   $d1
            .byte   $cd
            .byte   $dd
            .byte   $d9
            .byte   $ff

            .byte   $13
            .byte   "CPX*"
            .byte   $f8
            .addr   $0000
            .byte   $ff
            .byte   $e0             ; Immediate
            .byte   $e4             ; Zero-page
            .byte   $ff
            .byte   $ff
            .byte   $ff
            .byte   $ff
            .byte   $ec             ; Absolute
            .byte   $ff
            .byte   $ff
            .byte   $ff

            .byte   $13
            .byte   "CPY*"
            .byte   $f8
            .addr   $0000
            .byte   $ff
            .byte   $c0
            .byte   $c4
            .byte   $ff
            .byte   $ff
            .byte   $ff
            .byte   $ff
            .byte   $cc
            .byte   $ff
            .byte   $ff
            .byte   $ff

            .byte   $13
            .byte   "DEC*"
            .byte   $f8
            .addr   $0000
            .byte   $ff
            .byte   $ff
            .byte   $c6
            .byte   $d6
            .byte   $ff
            .byte   $ff
            .byte   $ff
            .byte   $ce
            .byte   $de
            .byte   $ff
            .byte   $ff

            .byte   $09
            .byte   "DEX*"          ; Decrement X
            .byte   $f0
            .addr   $0000
            .byte   $ca

            .byte   $09
            .byte   "DEY*"          ; Decrement Y
            .byte   $f0
            .addr   $0000
            .byte   $88

            .byte   $13
            .byte   "EOR*"          ; Exclusive OR
            .byte   $f8
            .addr   $0000
            .byte   $ff
            .byte   $49
            .byte   $45
            .byte   $55
            .byte   $ff
            .byte   $41
            .byte   $51
            .byte   $4d
            .byte   $5d
            .byte   $59
            .byte   $ff

            .byte   $13
            .byte   "INC*"
            .byte   $f8
            .addr   $0000
            .byte   $ff
            .byte   $ff
            .byte   $e6
            .byte   $f6
            .byte   $ff
            .byte   $ff
            .byte   $ff
            .byte   $ee
            .byte   $fe
            .byte   $ff
            .byte   $ff

            .byte   $09
            .byte   "INX*"          ; Increment X
            .byte   $f0
            .addr   $0000
            .byte   $e8

            .byte   $09
            .byte   "INY*"          ; Increment Y
            .byte   $f0
            .addr   $0000
            .byte   $c8

            .byte   $13
            .byte   "JMP*"
            .byte   $f8
            .addr   $0000
            .byte   $ff
            .byte   $ff
            .byte   $ff
            .byte   $ff
            .byte   $ff
            .byte   $ff
            .byte   $ff
            .byte   $4c             ; Absolute
            .byte   $ff
            .byte   $ff
            .byte   $6c             ; Indirect

            .byte   $13
            .byte   "JSR*"
            .byte   $f8
            .addr   $0000
            .byte   $ff
            .byte   $ff
            .byte   $ff
            .byte   $ff
            .byte   $ff
            .byte   $ff
            .byte   $ff
            .byte   $20             ; Absolute only
            .byte   $ff
            .byte   $ff
            .byte   $ff

            .byte   $13
            .byte   "LDA*"
            .byte   $f8
            .addr   $0000
            .byte   $ff
            .byte   $a9
            .byte   $a5
            .byte   $b5
            .byte   $ff
            .byte   $a1
            .byte   $b1
            .byte   $ad
            .byte   $bd
            .byte   $b9
            .byte   $ff

            .byte   $13
            .byte   "LDX*"
            .byte   $f8
            .addr   $0000
            .byte   $ff
            .byte   $a2
            .byte   $a6
            .byte   $ff
            .byte   $b6             ; Zero-page,Y (LDX has Y-indexed ZP and abs)
            .byte   $ff
            .byte   $ff
            .byte   $ae
            .byte   $ff
            .byte   $be             ; Absolute,Y
            .byte   $ff

            .byte   $13
            .byte   "LDY*"
            .byte   $f8
            .addr   $0000
            .byte   $ff
            .byte   $a0
            .byte   $a4
            .byte   $b4
            .byte   $ff
            .byte   $ff
            .byte   $ff
            .byte   $ac
            .byte   $bc
            .byte   $ff
            .byte   $ff

            .byte   $13
            .byte   "LSR*"
            .byte   $f8
            .addr   $0000
            .byte   $4a             ; Accumulator
            .byte   $ff
            .byte   $46
            .byte   $56
            .byte   $ff
            .byte   $ff
            .byte   $ff
            .byte   $4e
            .byte   $5e
            .byte   $ff
            .byte   $ff

            .byte   $09
            .byte   "NOP*"          ; No Operation
            .byte   $f0
            .addr   $0000
            .byte   $ea

            .byte   $13
            .byte   "ORA*"          ; OR with Accumulator
            .byte   $f8
            .addr   $0000
            .byte   $ff
            .byte   $09
            .byte   $05
            .byte   $15
            .byte   $ff
            .byte   $01
            .byte   $11
            .byte   $0d
            .byte   $1d
            .byte   $19
            .byte   $ff

            .byte   $09
            .byte   "PHA*"          ; Push Accumulator
            .byte   $f0
            .addr   $0000
            .byte   $48

            .byte   $09
            .byte   "PHP*"          ; Push Processor status
            .byte   $f0
            .addr   $0000
            .byte   $08

            .byte   $09
            .byte   "PLA*"          ; Pull Accumulator
            .byte   $f0
            .addr   $0000
            .byte   $68

            .byte   $09
            .byte   "PLP*"          ; Pull Processor status
            .byte   $f0
            .addr   $0000
            .byte   $28

            .byte   $13
            .byte   "ROL*"          ; Rotate Left
            .byte   $f8
            .addr   $0000
            .byte   $2a             ; Accumulator
            .byte   $ff
            .byte   $26
            .byte   $36
            .byte   $ff
            .byte   $ff
            .byte   $ff
            .byte   $2e
            .byte   $3e
            .byte   $ff
            .byte   $ff

            .byte   $13
            .byte   "ROR*"          ; Rotate Right
            .byte   $f8
            .addr   $0000
            .byte   $6a             ; Accumulator
            .byte   $ff
            .byte   $66
            .byte   $76
            .byte   $ff
            .byte   $ff
            .byte   $ff
            .byte   $6e
            .byte   $7e
            .byte   $ff
            .byte   $ff

            .byte   $09
            .byte   "RTI*"          ; Return from Interrupt
            .byte   $f0
            .addr   $0000
            .byte   $40

            .byte   $09
            .byte   "RTS*"          ; Return from Subroutine
            .byte   $f0
            .addr   $0000
            .byte   $60

            .byte   $13
            .byte   "SBC*"          ; Subtract with Carry
            .byte   $f8
            .addr   $0000
            .byte   $ff
            .byte   $e9
            .byte   $e5
            .byte   $f5
            .byte   $ff
            .byte   $e1
            .byte   $f1
            .byte   $ed
            .byte   $fd
            .byte   $f9
            .byte   $ff

            .byte   $09
            .byte   "SEC*"          ; Set Carry
            .byte   $f0
            .addr   $0000
            .byte   $38

            .byte   $09
            .byte   "SED*"          ; Set Decimal mode
            .byte   $f0
            .addr   $0000
            .byte   $f8

            .byte   $09
            .byte   "SEI*"          ; Set Interrupt disable
            .byte   $f0
            .addr   $0000
            .byte   $78

            .byte   $13
            .byte   "STA*"
            .byte   $f8
            .addr   $0000
            .byte   $ff
            .byte   $ff             ; Immediate (not available for STA)
            .byte   $85
            .byte   $95
            .byte   $ff
            .byte   $81
            .byte   $91
            .byte   $8d
            .byte   $9d
            .byte   $99
            .byte   $ff

            .byte   $13
            .byte   "STX*"
            .byte   $f8
            .addr   $0000
            .byte   $ff
            .byte   $ff
            .byte   $86
            .byte   $ff
            .byte   $96             ; Zero-page,Y
            .byte   $ff
            .byte   $ff
            .byte   $8e
            .byte   $ff
            .byte   $ff
            .byte   $ff

            .byte   $13
            .byte   "STY*"
            .byte   $f8
            .addr   $0000
            .byte   $ff
            .byte   $ff
            .byte   $84
            .byte   $94
            .byte   $ff
            .byte   $ff
            .byte   $ff
            .byte   $8c
            .byte   $ff
            .byte   $ff
            .byte   $ff

            ; MTU CODOS SVC instruction (BRK + immediate byte)
            ; Encoded as a zero-page write (opcode $00 at zero-page address)
            ; so the assembler emits just the zero-page opcode byte
            .byte   $13
            .byte   "SVC*"
            .byte   $f8
            .addr   $0000
            .byte   $ff
            .byte   $ff
            .byte   $00             ; Zero-page "opcode": emits BRK ($00)
            .byte   $ff
            .byte   $ff
            .byte   $ff
            .byte   $ff
            .byte   $ff
            .byte   $ff
            .byte   $ff
            .byte   $ff

            .byte   $09
            .byte   "TAX*"          ; Transfer A to X
            .byte   $f0
            .addr   $0000
            .byte   $aa

            .byte   $09
            .byte   "TAY*"          ; Transfer A to Y
            .byte   $f0
            .addr   $0000
            .byte   $a8

            .byte   $09
            .byte   "TYA*"          ; Transfer Y to A
            .byte   $f0
            .addr   $0000
            .byte   $98

            .byte   $09
            .byte   "TSX*"          ; Transfer Stack pointer to X
            .byte   $f0
            .addr   $0000
            .byte   $ba

            .byte   $09
            .byte   "TXA*"          ; Transfer X to A
            .byte   $f0
            .addr   $0000
            .byte   $8a

            .byte   $09
            .byte   "TXS*"          ; Transfer X to Stack pointer
            .byte   $f0
            .addr   $0000
            .byte   $9a

            ; =================================================================
            ; Assembler directive entries (type $E8)
            ; Each entry points to a handler routine in the code segment.
            ; =================================================================

            .byte   $0a             ; Entry length (6-char name ".BYTE*" + 4)
            .byte   ".BYTE*"        ; Emit one or more bytes
            .byte   $e8             ; Type: directive
            .addr   EMIT_BYTE_ENTRY

            .byte   $0a
            .byte   ".WORD*"        ; Emit one or more 16-bit little-endian words
            .byte   $e8
            .addr   HANDLE_WORD

            .byte   $0b
            .byte   ".DBYTE*"       ; Emit one or more 16-bit big-endian words
            .byte   $e8
            .addr   HANDLE_DBYTE

            .byte   $09
            .byte   ".END*"         ; End of assembly (or end of .READ file)
            .byte   $e8
            .addr   HANDLE_END

            .byte   $0a
            .byte   ".PAGE*"        ; Force a new listing page with optional title
            .byte   $e8
            .addr   HANDLE_PAGE

            .byte   $08
            .byte   ".IF*"          ; Conditional assembly: if expression != 0
            .byte   $e8
            .addr   HANDLE_IF

            .byte   $0a
            .byte   ".ELSE*"        ; Else clause of .IF block
            .byte   $e8
            .addr   HANDLE_ELSE

            .byte   $0b
            .byte   ".ENDIF*"       ; End of .IF / .ELSE block
            .byte   $e8
            .addr   HANDLE_ENDIF

            .byte   $09
            .byte   ".DEF*"         ; Export symbol definitions to .DEF file
            .byte   $e8
            .addr   HANDLE_DEF

            .byte   $0a
            .byte   ".LIST*"        ; Control listing output (bitmask operand)
            .byte   $e8
            .addr   HANDLE_LIST

            .byte   $0b
            .byte   ".ENTRY*"       ; Declare the program entry-point address
            .byte   $e8
            .addr   HANDLE_ENTRY

            .byte   $0a
            .byte   ".READ*"        ; Include (nest) an additional source file
            .byte   $e8
            .addr   HANDLE_READ

            .byte   $09
            .byte   ".OVL*"         ; Set overlay number for the object segment header
            .byte   $e8
            .addr   HANDLE_OVL

            .byte   $0a
            .byte   ".BANK*"        ; Set memory bank number (0–3) for the segment
            .byte   $e8
            .addr   HANDLE_BANK

            .byte   $09
            .byte   ".OPT*"         ; Assembler option (ignored; returns immediately)
            .byte   $e8
            .addr   RETURN          ; -> stub (rts)

            .byte   $0a
            .byte   ".FILL*"        ; Emit N copies of a byte value
            .byte   $e8
            .addr   HANDLE_FILL

            .byte   $0b
            .byte   ".MACRO*"       ; Begin macro definition
            .byte   $e8
            .addr   HANDLE_MACRO 

            .byte   $0c
            .byte   ".ENDMAC*"      ; End of macro definition body
            .byte   $e8
            .addr   HANDLE_ENDMAC   ; Throws an error as this was foundoutside .MACRO

            .byte   $0a
            .byte   ".IFLT*"        ; Conditional: if operand1 < operand2
            .byte   $e8
            .addr   HANDLE_IFLT

            .byte   $0a
            .byte   ".IFLE*"        ; Conditional: if operand1 <= operand2
            .byte   $e8
            .addr   HANDLE_IFLE 

            .byte   $0a
            .byte   ".IFEQ*"        ; Conditional: if operand1 == operand2
            .byte   $e8
            .addr   HANDLE_IFEQ

            .byte   $0a
            .byte   ".IFNE*"        ; Conditional: if operand1 != operand2
            .byte   $e8
            .addr   HANDLE_IFNE

            .byte   $0a
            .byte   ".IFGE*"        ; Conditional: if operand1 >= operand2
            .byte   $e8
            .addr   HANDLE_IFGE

            .byte   $0a
            .byte   ".IFGT*"        ; Conditional: if operand1 > operand2
            .byte   $e8
            .addr   HANDLE_IFGT

            .byte   $0c
            .byte   ".IFNULL*"      ; Conditional: if the operand field is empty
            .byte   $e8
            .addr   HANDLE_IFNULL

            .byte   $0a
            .byte   ".DATE*"        ; Emit 9-byte date string (filled by SVC $1E at startup)
            .byte   $e8
            .addr   HANDLE_DATE

            .byte   $00             ; End-of-table sentinel

DATA_SIZE = * - TABLE

            .end
