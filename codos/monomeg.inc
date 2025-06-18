SPREGREN    := $BFC7        ; Special registers read enable
SPREGREAD   := $BFC3        ; Special registers read byte

SVIA1PORT   = $BFE0         ; System 1 6522 System control port data register
SVIA1DIR    := $BFE2        ; System 1 6522 System control port direction register
BNKCTL      := SVIA1PORT    ; System 1 6522 (Bank control data register)

SVIA2TPORT  = $BFF0         ; System 2 6522 MTUTAPE/cassette port data register
SVIA2APORT  = $BFF1         ; System 2 6522 Audio DAC port data register
SVIA2TDIR   = $BFF2         ; System 2 6522 MTUTAPE/cassette port direction register
SVIA2ADIR   = $BFF3         ; System 2 6522 Audio DAC port direction register

KSCCLR      = $BFC5         ; PORT TO CLEAR KEYBOARD SCAN COUNTER
KBDATA      = $BFE1         ; DATA REGISTER FOR KEYBOARD PORT
KBT1CH      = $BFE5         ; TIMER 1 COUNT HIGH REGISTER FOR KEYBOARD 6522
KBPCR       = $BFEC         ; PERIPHERAL CONTROL REGISTER FOR KEYBOARD 6522
SYS1IER     = $BFEE         ; SYS1 I/O CHIP INTERRUPT ENABLE REGISTER
KBIFR       = $BFED         ; INTERRUPT FLAG REGISTER FOR KEYBOARD 6522
KBIER       = $BFEE         ; INTERRUPT ENABLE REGISTER FOR KEYBOARD 6522
KBACR       = $BFEB         ; AUXILIARY CONTROL REGISTER FOR KEYBOARD 6522

IOENABLE    := $FFFE        ; Enable I/O space from $BE00 to $BFFF
IODISABLE   := $FFFF        ; Disable I/O space (enable RAM) from $BE00 to $BFFF

VIDEORAM    := $C000        ; Bit-mapped CRT display RAM
VIDEOLEN    = $3C00         ; Display RAM length
BCKTRBUF    := $FC50        ; Backtrack buffer (for lines recalled by CTRL-B)
BCKTRLEN    = $0100         ; Backtrack buffer length

LEGTSIZ     = $40           ; Size of legend table

VIDHRES     = 480           ; Horizontal and vertical video display resolution
VIDVRES     = 256           ;

TXTHRES     = 80            ; Horizontal and vertical text display resolution
TXTVRES     = 24            ;
