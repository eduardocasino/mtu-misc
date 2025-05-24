SVIA1PORT   = $BFE0         ; System 1 6522 System control port data register
SVIA1DIR    := $BFE2        ; System 1 6522 System control port direction register
BNKCTL      := SVIA1PORT    ; System 1 6522 (Bank control data register)

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
