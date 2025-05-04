.ifndef CODOS2_VER
    CODOS2_VER  = 15
.endif

.if .defined(SIM1)
RELEASE         = $10               ; Release level of CODOS
SYSTEM          = 5                 ; SYM-1 with 8-inch floppy disk(s)
.elseif .defined(PET)
RELEASE         = $10               ; Release level of CODOS
SYSTEM          = 4                 ; PET or CBM with 8-inch floppy disk
.elseif .defined(AIM65)
RELEASE         = $10               ; Release level of CODOS
SYSTEM          = 3                 ; AIM-65 with 8-inch floppy disk(s)
.elseif .defined(KIM1)
RELEASE         = $10               ; Release level of CODOS
SYSTEM          = 2                 ; KIM-1 with 8-inch floppy disk(s)
.else
RELEASE         = $20               ; Release level of CODOS
SYSTEM          = 1                 ; MTU-130 with 8-inch floppy disk(s)
.endif

NSECTS          = 26                ; Sectors per track/side

MAXFILES        = 247               ; Maximum number of files per disk
FNAMLEN         = 12                ; Max file name length

NUMSVCS         = $1F               ; Number of available SVCs

; FINFO flags definitions
;
FLUNUSED    = $00
FLNORMAL    = $80
FLLOCKED    = $20

; Current file info structure
; FINFO structure offsets:
;
_FLAGS      = 0                     ; Flags
_DRIVE      = 1                     ; Drive
_BATPT      = 2                     ; First block in BAT
_FSIZE      = 3                     ; (3 bytes) file size (including 64 byte header)
_FPOS       = 6                     ; (3 bytes) file pos (including 64 byte header)
_BUFF       = 9                     ; (word) File buffer
_DMABF      = 11                    ; Encoded K-1013 DMA file buffer

FINFOLEN    = 13                    ; FINFO structure length

; BAT block identifiers:
;
BLKFREE     = $00                   ; Free block
BLKLAST     = $FC                   ; Last block in a series
BLKOVLY     = $FE                   ; System overlay block
BLKUNUS     = $FF                   ; Unusable block

; Offsets in BAT:
;
_BTVSN      = $FA                   ; (word) Disk volume number
_BNENT      = $FD                   ; Number of files on disk
_BLAST      = $FF                   ; Last allocated block number

; File header length
;
FHDRLEN     = $40

; Saved (loadable) files header
;
_MAGIC      = 0
_OVLAY      = 1
_MEMBK      = 2
_RSRVD      = 3
_PNTRS      = 4
SAVEDHDRLEN = 10

