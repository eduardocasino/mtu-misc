; CODOS 2.0 Boot sector disassembly
;


            .BYTE   $01
            .TEXT   "CODOS.Z"
            .TEXT   "CVER.Z"
            .BYTE   $00, $01, $80, $F6, $18, $00, $01, $01
            .TEXT   "16-NOV-82"
            .BYTE   $00, $00, $00, $00, $00, $00, $00, $00, $00
            .BYTE   $20, $20, $20, $20, $20, $20, $20, $20, $20
            .BYTE   $20, $20, $20, $20, $20, $20, $20, $20, $20
            .BYTE   $20, $20

            ; Relevant information:
            ;
            ;   $3C = FINALS = Final sector number for the load.
            ;   $3D = DMAPG = DMA Address code for loading of the first sector.
            ;   $3E, $3F = ENTRY = Address-1 of entry point into program.
            ;
FINALS:     .BYTE   $19     ; 25 sectors
DMAPG:      .BYTE   $98     ; $E600 (See section 2.1.4 of the manual)
ENTRY:      .WORD   $E5FF
        
            .BYTE   $58, $00, $00, $00, $00, $E6, $4A, $E5, $AC, $18

            .TEXT   "MTU-130 CODOS 2.0", $0D
            .TEXT   "COPYRIGHT (C) 1981, MICRO TECHNOLOGY UNLIMIMITED", $0D
            .TEXT   "PO BOX 12106, 2806 HILLSBOROUGH ST.", $0D
            .TEXT   "RALEIGH, NC 27605 USA", $0D
            .TEXT   "Written by Bruce D. Carbrey", $0D
            .TEXT   "ASM 1/18/82 patch 6/14/82", $0D, $0D, $0D, $20

            .END