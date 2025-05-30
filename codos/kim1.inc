; KIM-1 ROM entry points

KOUTCH = $1ea0
GETCH = $1e5a
DELAY = $1ed4
DEHALF = $1eeb
EXIT  = $1c4f

; KIM-1 I/O

CHAR = $fe             ; ZP char buffer
PORTA = $1700
DDRA = $1701
PORTB = $1702
DDRB = $1703
SAD = $1740            ; 6530 A Data (TTY)
SBD = $1742            ; 6530 B data register