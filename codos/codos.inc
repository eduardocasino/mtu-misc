; Possible values: 14, 15, 17
; If none is specified, 15 is assumed
; Codos 11 is the same as Codos 15
;

            .include "k1013.inc"

.ifndef CODOS2_VER
CODOS2_VER  = 15
.endif

.if .defined(SIM1)
RELEASE     = $10                   ; Release level of CODOS
SYSTEM      = 5                     ; SYM-1 with 8-inch floppy disk(s)
.elseif .defined(PET)
RELEASE     = $10                   ; Release level of CODOS
SYSTEM      = 4                     ; PET or CBM with 8-inch floppy disk
.elseif .defined(AIM65)
RELEASE     = $10                   ; Release level of CODOS
SYSTEM      = 3                     ; AIM-65 with 8-inch floppy disk(s)
.elseif .defined(KIM1)
RELEASE     = $10                   ; Release level of CODOS
SYSTEM      = 2                     ; KIM-1 with 8-inch floppy disk(s)
.else
RELEASE     = $20                   ; Release level of CODOS
SYSTEM      = 1                     ; MTU-130 with 8-inch floppy disk(s)
.endif

NCYLS       = 77                    ; Number of cylinders
NSECTS      = 26                    ; Sectors per track/side

MAXDRIVES   = 4                     ; Maximum number of drives in the system
MAXBUFFERS  = 8                     ; Naximum number of file buffers
MAXFILES    = 247                   ; Maximum number of files per disk
FNAMLEN     = 14                    ; Max file name length
DATELEN     = 9                     ; Date string length (excluding null terminator)

NUMSVCS     = $1F                   ; Number of available SVCs

; FINFO flags definitions
;
FLUNUSED    = $00
FLNORMAL    = $80                   ; Normal file
FISDIRTY    = $40                   ; There are pending changes in the file buffer
FLLOCKED    = $20                   ; File is locked

; P0SCRATCH offsets
;
_MEMBUFF    = MEMBUFF-P0SCRATCH
_MEMCOUNT   = MEMCOUNT-P0SCRATCH
_TMPBUFP    = TMPBUFP-P0SCRATCH
_DESTBUFF   = DESTBUFF-P0SCRATCH
_BYTRES     = BYTRES-P0SCRATCH
_PCSAVE     = PCSAVE-P0SCRATCH
_TMPPTR     = TMPPTR-P0SCRATCH

; File info (FINFO) structure
;
.struct     FINFO
    FLAGS   .byte                   ; Flags
    DRIVE   .byte                   ; Drive
    BATPT   .byte                   ; First block in BAT
    FSIZE   .faraddr                ; (3 bytes) file size (including 64 byte header)
    FPOS    .faraddr                ; (3 bytes) file pos (including 64 byte header)
    BUFF    .addr                   ; (word) File buffer
    DMABF   .byte                   ; Encoded K-1013 DMA file buffer
    RESVD   .byte
.endstruct

FINFOLEN    = .sizeof(FINFO)        ; FINFO structure length

; BAT block identifiers:
;
BLKFREE     = $00                   ; Free block
BLKLAST     = $FC                   ; Last block in a series
BLKOVLY     = $FE                   ; System overlay block
BLKUNUS     = $FF                   ; Unusable block

; Disk BAT:
;
.struct     BAT
            .org $F8
    TOPB    .byte                   ; Last block slot
    UNUS1   .byte
    VSN     .word                   ; (word) Disk volume number
    UNUS2   .byte
    NENT    .byte                   ; Number of files on disk
    UNUS3   .byte
    LAST    .byte                   ; Last allocated block number
.endstruct

; Directory entry
;
.struct     DIRE
    TAG     .byte
    FNAM    .byte ::FNAMLEN
    BATP    .byte
.endstruct

; File header
;
.struct     FHDR
    DIRE    .tag DIRE
    FLAG    .byte                   ; File flags
    FLEN    .faraddr                ; File length
    NENT    .byte                   ; Offset to entry in directory sector
    NSEC    .byte                   ; Sector in track 12 of the directory
    DATE    .byte ::DATELEN+1       ; Date of creation
    RSVD    .byte 28                ; Reserved for future use
    LAST    .byte                   ; Last sector to load
    DMA     .byte                   ; DMA encoded vaddress for loading the system 
    ENTRY   .addr                   ; Entry point - 1
.endstruct

FHDRLEN     = .sizeof(FHDR)

; Saved (loadable) files header
;
.struct     SHDR
    MAGIC   .byte
    OVLAY   .byte
    MEMBK   .byte
    RSRVD   .byte
    ENTRY   .addr
    LOAD    .addr
    SIZE    .word
.endstruct

SAVEDHDRLEN = .sizeof(SHDR)

; Saved file magic number
;
SVDMAGIC    = $58

DIRBUF      := SYSRAM+$500          ; Directory buffer
OVLORG      := SYSRAM+$1E00         ; Origin of CODOS overlays
BOOTP       := SYSRAM+$1F0E         ; Boot from PROM

.macro dma reg, addr
    .if ((::addr >= ::SYSRAM) && (::addr < ::SYSRAM + $2000) && (::SYSRAM .MOD $2000 = 0) && (::addr < ::SYSRAM + $1000)) || ((::addr >= ::SYSRAM) && (::addr < ::SYSRAM + $2000) && (::SYSRAM .MOD $2000 <> 0) && (::addr >= ::SYSRAM + $1000))
        .if .match ({reg}, A)
            lda #((::addr >> 6) & $BF) | $80
        .elseif .match ({reg}, X)
            ldx #((::addr >> 6) & $BF) | $80
        .elseif .match ({reg}, Y)
            ldy #((::addr >> 6) & $BF) | $80
        .else
            .byte ((::addr >> 6) & $BF) | $80
        .endif
    .elseif ((::addr >= ::SYSRAM  ) && (::addr < ::SYSRAM   + $2000) && (::SYSRAM .MOD $2000 = 0) && (::addr >= ::SYSRAM   + $1000)) || ((::addr >= ::SYSRAM  ) && (::addr < ::SYSRAM   + $2000) && (::SYSRAM .MOD $2000 <> 0) && (::addr < ::SYSRAM   + $1000))
        .if .match ({reg}, A)
            lda #((::addr >> 6) | $C0) & $FF
        .elseif .match ({reg}, X)
            ldx #((::addr >> 6) | $C0) & $FF
        .elseif .match ({reg}, Y)
            ldy #((::addr >> 6) | $C0) & $FF
        .else
            .byte ((::addr >> 6) | $C0) & $FF
        .endif
    .elseif ((::addr >= ::USRRAM) && (::addr < ::USRRAM + $2000) && (::USRRAM .MOD $2000 = 0) && (::addr < ::USRRAM + $1000)) || ((::addr >= ::USRRAM) && (::addr < ::USRRAM + $2000) && (::USRRAM .MOD $2000 <> 0) && (::addr >= ::USRRAM + $1000))
        .if .match ({reg}, A)
            lda #(::addr >> 6) & $3F
        .elseif .match ({reg}, X)
            ldx #(::addr >> 6) & $3F
        .elseif .match ({reg}, Y)
            ldy #(::addr >> 6) & $3F
        .else
            .byte (::addr >> 6) & $3F
        .endif
    .else
        .if .match ({reg}, A)
            lda #((::addr >> 6) & $7F) | $40
        .elseif .match ({reg}, X)
            ldx #((::addr >> 6) & $7F) | $40
        .elseif .match ({reg}, Y)
            ldy #((::addr >> 6) & $7F) | $40
        .else
            .byte ((::addr >> 6) & $7F) | $40
        .endif
    .endif
.endmacro