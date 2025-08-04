; ---------------------------------------------------------------------------
;
; Image Manipulation Utility
;
; Copyright © 2025 Eduardo Casino
; This file is licensed under the terms of the 2-clause BSD license. Please
; see the COPYING file in the root project directory for the full text.
;
; ---------------------------------------------------------------------------

            .setcpu "6502"

            .include "symbols.inc"

            .include "codos.inc"

; Extended command set for the Pico uPD765 emulator

EXT_CMD_DIR         = %00000000
EXT_CMD_NXT         = %00000100
EXT_CMD_MNTS        = %00001000
EXT_CMD_NXT_MNT     = %00001100
EXT_CMD_NEW         = %00010000
EXT_CMD_ERA         = %00010100
EXT_CMD_CPY         = %00011000
EXT_CMD_MOV         = %00011100
EXT_CMD_MNT         = %00100000
EXT_CMD_UMNT        = %00100100
EXT_CMD_SAV         = %00101000

ST4_UNSUPPORTED     = %10000000
ST4_ABNORMAL_TERM   = %01000000
ST4_INVALID_CMD     = %00100000
ST4_BAD_PARAM       = %00010000
ST4_NOT_FOUND       = %00001000
ST4_INVLD_DRV       = %00000100
ST4_NOT_OPEN        = %00000010
ST4_NO_DATA         = %00000001
ST4_NORMAL_TERM     = %00000000

ST5_DRV_MOUNTED     = %00000001
ST5_DRV_NOT_MOUNTED = %00000010
ST5_IMG_MOUNTED     = %00000100
ST5_IMG2_MOUNTED    = %00001000
ST5_IMG_NAME        = %00010000
ST5_IMG_INVALID     = %00100000
ST5_IMG_EXISTS      = %01000000
ST5_DISK_FULL       = %10000000

PACKED_IMG          = %10000000
DUALSIDE_IMG        = %01000000

            .zeropage

string:     .res    2               ; Pointer to error message
fname2:     .res    1               ; Index to 2nd file name
savey:      .res    1               ; Just that

            .code

            ; Loadable file data
            ;
            .byte   $58             ; CODOS loadable file header byte
            .byte   $00             ; Memory overlay
            .byte   $00             ; Memory bank
            .byte   $00             ; Reserved
            .addr   start           ; Entry point
            .addr   init            ; Load address
            .word   PROG_SIZE       ; Memory image size

; Inits the controller, the output buffer and checks that the controller
; is the Pico uPD765 emulator and not the real hardware
;
; Cy clear if success, Cy set on error
;
.proc init
            ; Put the disk controller in a sane state

            lda     HSRCW           ; Test if an interrupt is pending
            bmi     cont            ; No, skip it
            jsr     fdc_exec_senseint ; Yes, serve interrupt

cont:       lda     #0              ; Set DMA read mode, unprotect SYSRAM
            sta     HSRCW
            cld

            dma     A, DIRBUF       ; Set buffer for exchanging info with
            sta     ADMA            ; the controller

            jsr     SETOUTBCH       ; Set output buffer to output line buffer and sets
                                    ; output channel to console if not set

            ; Using a harmless command to check extended cmds support

            ldy     #EXT_CMD_DIR    ; This just resets an SD card listing
            jsr     fdc_exec_extended ; Execute it
            bcs     error           ; If error, something is worng with the controller

            ; Dig further for errors
            lda     disk_status     ; Get first byte of result
            bmi     not_supported   ; If bit 8 set, then it is an unsupported commans
            clc                     ; Otherwise, the emulator is present
            rts                     ;

not_supported:
            lda     #<real_msg      ; Extended commands not supported
            ldx     #>real_msg
            bne     prt_err         ; Always jump

error:      lda     #<fdc_msg       ; Controller error
            ldx     #>fdc_msg

prt_err:    jsr     err_withmsg     ; Print error message
            sec                     ; And return with fail
            rts
.endproc

; Helper function. Coverts the character in A to upper case.
;
.proc toupper
            cmp     #$7B            ; If > z, does nothing
            bcs     return          ;
            cmp     #$61            ; if < a, does nothinf
            bcc     return          ;
            sbc     #$20            ; Cy set if we're here. Convert to upper case
return:     rts
.endproc

; Entry point
;
.proc start
            jsr     init            ; Initialize
            bcc     cont            ; Initialization successful
            rts

cont:       ldx     #$02            ; Output CR to console
            jsr     OUTCR           ;
            ldy     CMDLIDX         ; Get command line index
            jsr     GETNEXTNB       ; Get next non-blank from command line
            beq     list_mounts     ; No args, list mounts
chk_mnt:    cmp     #'M'            ; Mout command
            bne     chk_umnt
            jmp     mount_drive
chk_umnt:   cmp     #'U'            ; Unmount command
            bne     chk_list
            jmp     unmount_drive
chk_list:   cmp     #'L'            ; List files on SD
            bne     chk_save
            jmp     list_files
chk_save:   cmp     #'S'            ; Save command
            bne     chk_del
            jmp     save_config
chk_del:    cmp     #'D'            ; Delete command
            bne     chk_cpy
            jmp     delete_file
chk_cpy:    cmp     #'C'            ; Copy command
            bne     chk_mov         ;
            jmp     copy_or_move_file
chk_mov:    cmp     #'R'            ; Rename/move command
            bne     chk_new         ;
            inc     rn_flag         ; Set rename/move flag
            jmp     copy_or_move_file
chk_new:    cmp     #'N'            ; New image command
            bne     invalid         ; Any other command is unsupported
            jmp     create_image
invalid:    jsr     ERROR04         ; Syntax error in command argument
            ; Not reached
.endproc

; Produces a list on screen of current mounts
; 
.proc list_mounts
            ldy     #EXT_CMD_MNTS   ; Init mount listing
            jsr     fdc_exec_extended 
            bcs     error

get_mount:  ldy     #EXT_CMD_NXT_MNT ; Get next mount
            jsr     fdc_exec_extended
            bcs     error

            ; Check status for no more entries

            lda     disk_status
            and     #ST4_NO_DATA
            beq     print           ; Entry found, print it

            rts                     ; No more, just return

print:      jsr     print_mount     ; Print mount info
            jmp     get_mount       ; And go get next mount

error:      lda     #<fdc_msg       ; Controller error
            ldx     #>fdc_msg
            jmp     err_withmsg
.endproc

; Prints info of a mounted drive. On entry, it
; expects the following info starting at DIRBUF:
;
;    drive     BYTE        ; Base 0
;    ro_flag   BYTE        ; Non-zero if RO
;    name      STRING      ; Null-terminated Image name
;
.proc print_mount
            ldy     #0              ; Init output buffer index

            lda     DIRBUF          ; Get drive number
            clc
            adc     #'0'            ; Convert to ascii
            sta     (OUTBUFP),y     ; And save to buffer
            iny

            lda     #':'            ; Disk separator to output buffer
            sta     (OUTBUFP),y
            iny

            lda     DIRBUF+2
            beq     prt_buf         ; Skip if no image mounted

            lda     #' '            ; Space to output buffer
            sta     (OUTBUFP),y
            iny

            ldx     DIRBUF+1        ; Get RO flag
            beq     prt_ro          ; Not RO
            lda     #'R'            ; Put read-only indicator into A

prt_ro:     sta     (OUTBUFP),y     ; RO indicator to output buff (blank if RW)
            iny

prt_sep:    lda     #' '            ; Space to output buffer
            sta     (OUTBUFP),y
            iny

            ldx     #2              ; Index to image name on DIRBUF
prt_image:  lda     DIRBUF,x        ; Get char from image name
            beq     prt_buf         ; If null, no more chars
            jsr     toupper         ; Convert to uppe rcase
            sta     (OUTBUFP),y     ; And place into output buffer
            inx                     ; Advance position
            iny                     ;
            bne     prt_image       ; Always jump

prt_buf:    jmp     POUTBUFFCR02    ; Print output buffer to console (length in Y)
.endproc

; Directory listing of all files on the SD card
;
.proc list_files
            ; This must be preceded by an EXT_CMD_DIR command, but
            ; we executed just that during initialization.

loop:       ldy     #EXT_CMD_NXT    ; Get SD card directory entry
            jsr     fdc_exec_extended
            bcc     chk_status      ; Execution ok
            lda     #<fdc_msg       ; Controller error
            ldx     #>fdc_msg
            jmp     err_withmsg       

chk_status: ; No need to check for extended command support
            ; Check status for errors
            ;
            lda     #ST4_NO_DATA
            bit     disk_status
            
            ; Testing for ST4_NO_DATA
            bne     return          ; No more data, so we're done
            
            ; Testing for ST4_ABNORMAL_TERM
            bvc     print_img       ; No error, print the file entry

            lda     #<rddir_msg     ; Error reading directory
            ldx     #>rddir_msg
            jmp     err_withmsg

print_img:  ldy     #0
print_img2: lda     DIRBUF,y        ; Get character from DIRBUF
            beq     prt_buf         ; If null, reached end of string
            jsr     toupper         ; Convert to upper case
            sta     (OUTBUFP),y     ; Store into output buffer
            iny                     ; And get next char
            bne     print_img2      ; Always jump

prt_buf:    jsr     POUTBUFFCR02    ; Print output buffer to console (length in Y)
            jmp     loop            ; And get next image

return:     rts
.endproc

; Update the SD config file with the latest mounted drives
; situation. Issues warning if the file could not be
; updated.
;
.proc save_config
            ldy     #EXT_CMD_SAV    ; Exec SD mounts config save
            jsr     fdc_exec_extended
            bcc     chk_status

            lda     #<fdc_msg       ; Controller error
            ldx     #>fdc_msg
            jmp     err_withmsg

chk_status: lda     disk_status     ; Check for success
            and     #ST4_ABNORMAL_TERM
            beq     done            ; Everything OK

            lda     #<config_msg    ; Error saving the config file
            ldx     #>config_msg
            jmp     err_withmsg

done:       lda     #<done_msg      ; Print "DONE" message and return
            ldx     #>done_msg
            jmp     prt_message
.endproc

; Get drive number from the command line.
;
; Cy clear and A contains drive number (0 to 3) if success
; Cy set on error
;
.proc get_drive
            jsr     GETNEXTNB1      ; Get next non-blank from command line
            cmp     #'4'            ; Number of disks
            bcs     return          ; > 3, return with carry set
            cmp     #'0'            ; First drive
            bcc     reterr          ; < 0, return with carry set
            sbc     #'0'            ; Convert to number
            clc                     ; And return with carry clear
return:     rts
reterr:     sec
            rts  
.endproc

; Unmount a drive.
;
.proc unmount_drive
            jsr     get_drive       ; Get drive from command line
            bcc     umount          ; Continue if success

            lda     #<inval_msg     ; Invalid drive number
            ldx     #>inval_msg
            jmp     err_withmsg

umount:     ora     #EXT_CMD_UMNT   ; OR Unmount drive command with drive number
            tay                     ; Transfer to Y as parameter to next function
            jsr     fdc_exec_extended 
            bcc     chk_err         ; Execution ok, go check status

            lda     #<fdc_msg       ; Controller error
            ldx     #>fdc_msg
            jmp     err_withmsg

            ; Dig further for errors

chk_err:    lda     disk_status     ; Extended command executed OK?
            and     #ST4_ABNORMAL_TERM
            beq     done            ; Yes, we're done

            lda     disk_status+1   ; No, get next result byte
            and     #ST5_DRV_NOT_MOUNTED ; Drive was mounted?
            beq     unexpected      ; Yes, then it is another unexpected error

            lda     #<notmnt_msg    ; Error, drive was not mounted
            ldx     #>notmnt_msg
            jmp     err_withmsg

unexpected: lda     #<umnerr_msg    ; Unexpected error
            ldx     #>umnerr_msg
            jmp     err_withmsg

done:       lda     #<done_msg      ; Print "DONE" message and return
            ldx     #>done_msg
            jmp     prt_message
.endproc

; Checks that the image name is valid
; Y must point to the first character of the
; image name in the command line
; Cy set if invalid
;
; Just checks that length > 0 && length < 64 chars, excluding
; terminating null. The firmware does proper validation
;
.proc check_valid_file
            sty     savey
            jsr     GETNEXTNB1      ; Get next non-blank from the command line

            ; Find end of file
            ldx     #0              ; Init index to image name

getch:      jsr     GETNEXTCH       ; Get char
            beq     check           ; If null, end of string
            iny                     ; Increment char position
            inx                     ; Increment image name size
            bpl     getch           ; Safeguard: stop if len > 128 chars

check:      cpx     #0              ; Check if null
            beq     reterr          ; If so, it is invalid
            cpx     #64             ; Check if it is longer that 64 chars
            bcc     return          ; If not, seems valid

reterr:     lda     #<imgerr_msg    ; Invalid image name
            ldx     #>imgerr_msg
            jsr     err_withmsg
            sec

return:     ldy     savey           ; Restore Y and return
            rts
.endproc


; Copies the image name from the command line into
; the mount info block (see print_mount below)
;
.proc set_image_name_for_mount
            ldx     #2              ; Offset of file name in mount info block
.endproc

;
; Fall through
;
; Copies the image name from the command line into
; DIRBUF,X
;
; At exit, X points to the first position in DIRBUF after
; the null character.
;
.proc set_image_name
            jsr     GETNEXTNB1      ; Get next non-blank from command line

copy:       sta     DIRBUF,x        ; Copy to DIRBUF
            beq     return          ; If was a null, all done
            inx                     ; Next pos in DIRBUF
            jsr     GETNEXTCH1      ; Get next char from command line
            beq     setnul          ; If end or separator, treat as null
            cmp     #' '            ; If a blank, treat also as null
            bne     next            ;
setnul:     lda     #0              ;
next:       jmp     copy            ; loop

return:     rts
.endproc

; Mount disk image into drive
;
.proc mount_drive
            jsr     get_drive       ; Get drive from command line
            bcc     cont            ; Continue if success

            lda     #<inval_msg     ; Invalid drive number
            ldx     #>inval_msg
            jmp     err_withmsg

cont:       sta     DIRBUF          ; Set drive number

            jsr     check_valid_file ; Check that next argument is a valid file
            bcc     setimg          ; Continue if OK
            rts                     ; Return otherwise

setimg:     ; Copies image name from command line into DIRBUF
            jsr     set_image_name_for_mount

            jsr     GETNEXTNB       ; Get next non-blank from command line
            beq     setroflg        ; If no more, ro_flag remains unset
            cmp     #'R'            ; RO switch?
            bne     error4          ; No, bad argument
            inc     ro_flag         ; Yes, set the flag

setroflg:   lda     ro_flag         ; Copies the RO flag into DIRBUF
            sta     DIRBUF+1

            lda     #$01            ; Set DMA direction bit to write
            sta     HSRCW

            lda     DIRBUF          ; Get drive number
            ora     #EXT_CMD_MNT    ; ORs it with the mount command
            tay                     ; And transfer to Y as parameter for next function
            jsr     fdc_exec_extended 
            bcc     chk_err         ; Execution ok, go check status

            lda     #<fdc_msg       ; Controller error
            ldx     #>fdc_msg
            jmp     err_withmsg

            ; Dig further for errors

chk_err:    lda     #ST4_NOT_FOUND
            bit     disk_status

            ; Testing for ST4_NOT_FOUND
            beq     chk_st5         ; Wasn't not found, check if other errors

            lda     #<notfnd_msg
            ldx     #>notfnd_msg
            jmp     err_withmsg

chk_st5:    bvc     done            ; No errors

            lda     disk_status+1   ; Was drive mounted?
            and     #ST5_DRV_MOUNTED
            beq     chk_imgmnt      ; No, check for other errors

            lda     #<drvmnt_msg    ; Drive was already mounted
            ldx     #>drvmnt_msg
            jmp     err_withmsg

chk_imgmnt: lda     disk_status+1   ; Was image mounted?
            and     #ST5_IMG_MOUNTED
            beq     chk_invld       ; No, check for other errors

            lda     #<imgmnt_msg    ; Image was already mounted
            ldx     #>imgmnt_msg
            jmp     err_withmsg

chk_invld:  lda     disk_status+1   ; Invalid image name or format?
            and     #ST5_IMG_INVALID
            beq     unexpected      ; No, it is an unexpected error

            lda     #<imgerr_msg    ; Invalid image file
            ldx     #>imgerr_msg
            jmp     err_withmsg

unexpected: lda     #<mnterr_msg    ; Unexpected error
            ldx     #>mnterr_msg
            jmp     err_withmsg

done:       jmp     print_mount     ; Print the mount info and return

error4:     jsr     ERROR04         ; Syntax error in command argument
            ; Not reached
.endproc

; Validates and copies first file name from command line
; to DIRBUF.
;
; Returns Carry set on error
; On exit, fname2 points to second file name relative to DIRBUF
;
.proc set_first_image_name
            jsr     check_valid_file ; Validate file name in command line
            bcs     return          ; Not valid, rerun with carry set

            ldx     #0              ; Position of first image in DIRBUF
            jsr     set_image_name  ; Copy image name into position
            inx                     ; Advance past the null terminator
            stx     fname2          ; And set pos for second file name
            clc                     ; Return with carry clear

return:     rts
.endproc

; Validates and copies second image name from command line
; to buffer1.
;
; Returns Carry set on error
;
.proc set_second_image_name
            jsr     check_valid_file ; Validate file name in command line
            bcs     return          ; Not valid, rerun with carry set

            ldx     fname2          ; Set position of second file name
            jsr     set_image_name  ; And copy image into position
            clc                     ; Return with carry clear

return:     rts
.endproc

; Delete file from the SD card
;
.proc delete_file
            ; Copy image name from command line
            jsr     set_first_image_name
            bcc     cont            ; If valid image, continue
            rts                     ; Else, return

cont:       jsr     GETNEXTNB       ; Get next non-blank form command line
            beq     delete          ; No more, continue
            jsr     ERROR04         ; No extra arguments allowed

delete:     lda     #$01            ; Set DMA direction bit to write
            sta     HSRCW          

            ; Prepare the command data

            ldy     #EXT_CMD_ERA    ; Delete image
            jsr     fdc_exec_extended
            bcc     status          ; Executed ok, go check the status

            lda     #<fdc_msg       ; Controller error
            ldx     #>fdc_msg
            jmp     err_withmsg

            ; Dig further for errors

status:     lda     #ST4_NOT_FOUND
            bit     disk_status

            beq     chk_st5         ; Not ST4_NOT_FOUND, check other errors

            lda     #<notfnd_msg    ; File not found
            ldx     #>notfnd_msg
            jmp     err_withmsg

chk_st5:    bvc     done            ; No errors, we're done

chk_invld:  lda     disk_status+1   ; Check second status byte
            and     #ST5_IMG_NAME   ; Invalid file name
            beq     chk_mnted       ; No, check for mounted

            lda     #<imgerr_msg    ; Invalid image file
            ldx     #>imgerr_msg
            jmp     err_withmsg

chk_mnted:  lda     disk_status+1   ; Image mounted?
            and     #ST5_IMG_MOUNTED
            beq     unexpected      ; No, then it is an unexpected error

            lda     #<imgmnt_msg    ; Image mounted error
            ldx     #>imgmnt_msg
            jmp     err_withmsg

unexpected: lda     #<delerr_msg    ; Unexpected error
            ldx     #>delerr_msg
            jmp     err_withmsg

done:       lda     #<done_msg      ; Print the "DONE" message and return
            ldx     #>done_msg
            jmp     prt_message
.endproc

; Create a new, empty disk image on the SD card
;
.proc create_image
            jsr     check_valid_file ; Check if image in command line is valid
            bcc     cont            ; Yes, continue
            rts                     ; No, return

cont:       ldx     #4              ; Copy image name into position in DIRBUF
            jsr     set_image_name  ; 

            jsr     GETNEXTNB       ; Get next non-blank from command line
            beq     error4          ; Number of sides is mandatory

            cmp     #'1'            ; Is it a one?
            beq     chk_pkd         ; Then ds_flag remains at 0
            cmp     #'2'            ; Is it a two?
            bne     error4          ; No, invalid argument
            lda     #DUALSIDE_IMG   ; Yes, set the dual-side flag
            sta     ds_flag         ;

chk_pkd:    jsr     GETNEXTNB1      ; Get next non-blank
            beq     create          ; No more, go create the image
            cmp     #'P'            ; Is it the packed flag?
            bne     error4          ; No, invalid argument
            lda     #PACKED_IMG     ; Yes, set the packed image flag
            sta     pk_flag         ;

create:     ; Prepare the command data
            lda     #77             ; 77 Tracks
            sta     DIRBUF
            lda     #26             ; 26 Sectors
            sta     DIRBUF+1
            lda     #1              ; Sector size ( 1 == 256bytes )
            ora     pk_flag         ; Whether or not is a packaged image
            ora     ds_flag         ; Whether or not is a dual side image
            sta     DIRBUF+2
            lda     #0              ; Filler byte
            sta     DIRBUF+3

            lda     #$01            ; Set DMA direction bit to write
            sta     HSRCW          

            ldy     #EXT_CMD_NEW    ; Create image
            jsr     fdc_exec_extended
            bcc     status          ; Executed ok, go check the status

            lda     #<fdc_msg
            ldx     #>fdc_msg
            jmp     err_withmsg     ; Controller error

            ; Dig further for errors

status:     lda     disk_status     ; Check for abnormal termination
            and     #ST4_ABNORMAL_TERM
            beq     done            ; No, then its a success

            lda     #ST5_IMG_NAME   ; Check for errors in second result byte
            bit     disk_status+1
            beq     chk_dskful      ; If valid image name, check for other errors

            ; Invalid image file
            lda     #<imgerr_msg    ; Invalid image name
            ldx     #>imgerr_msg
            jmp     err_withmsg
            ; Not reached

chk_dskful: bpl     chk_exists      ; If not disk full, check if file existed

            ; Disk full
            lda     #<dskful_msg    ; SD card is full
            ldx     #>dskful_msg
            jmp     err_withmsg
            ; Not reached

chk_exists: bvc     unexpected      ; If file didn't exist, then unexpected error

            ; Image exists
            lda     #<exists_msg    ; File existed, can't create
            ldx     #>exists_msg
            jmp     err_withmsg
            ; Not reached

unexpected: lda     #<newerr_msg    ; Unexpected error
            ldx     #>newerr_msg
            jmp     err_withmsg

done:       lda     #<done_msg      ; Print the "DONE" message and return
            ldx     #>done_msg
            jmp     prt_message

error4:     jsr     ERROR04         ; Syntax error in command argument
.endproc

; Copy or rename file on the SD card
;
.proc copy_or_move_file
            ; Copy image names from command line

            jsr     set_first_image_name
            bcc     second
return:     rts

second:     jsr     set_second_image_name
            bcs     return

cont:       jsr     GETNEXTNB       ; Get next non-blank from command line
            beq     delete          ; No more, continue
            jsr     ERROR04         ; No more arguments allowed

delete:     lda     #$01            ; Set DMA direction bit to write
            sta     HSRCW          

            ; Prepare the command data

            ldy     #EXT_CMD_CPY    ; Set command to copy by default
            lda     rn_flag         ; Get rename flag
            beq     exec            ; If not set, go with copy
            ldy     #EXT_CMD_MOV    ; Set, change command to rename/move

exec:       jsr     fdc_exec_extended
            bcc     status          ; Execution OK, go check status

            lda     #<fdc_msg       ; Controller error
            ldx     #>fdc_msg
            jmp     err_withmsg

            ; Dig further for errors

status:     lda     #ST4_NOT_FOUND
            bit     disk_status     ; Check status

            beq     chk_st5         ; If file was found, check for other errors

            lda     #<notfnd_msg    ; File not found
            ldx     #>notfnd_msg
            jmp     err_withmsg

chk_st5:    bvc     done            ; If not abnormal term, then success

chk_invld:  lda     #ST5_IMG_NAME   ; Check the second ststus byte
            bit     disk_status+1

            ; Testing for ST5_IMG_NAME
            beq     chk_dskful      ; If valid image name, check if SD full

            lda     #<imgerr_msg    ; Invalid image name
            ldx     #>imgerr_msg
            jmp     err_withmsg

chk_dskful: bpl     chk_exists      ; If SD is not full, check if dest exists

            lda     #<dskful_msg    ; SD is full
            ldx     #>dskful_msg
            jmp     err_withmsg

chk_exists: bvc     chk_mnted       ; If dest doesn't exist, check if mounted

            lda     #<exists_msg    ; Dest file exists
            ldx     #>exists_msg
            jmp     err_withmsg

chk_mnted:  lda     disk_status+1   ; Check if any of the images are mounted
            and     #ST5_IMG_MOUNTED
            bne     mounted         ; Yes, error
            lda     disk_status+1   ; No, check for the second one
            and     #ST5_IMG2_MOUNTED
            beq     unexpected      ; Not mounted, then it is an unexpected error

mounted:    lda     #<imgmnt_msg    ; Either image was mounted
            ldx     #>imgmnt_msg
            jmp     err_withmsg

unexpected: lda     #<cpyerr_msg    ; Unexpected error
            ldx     #>cpyerr_msg
            jmp     err_withmsg

done:       lda     #<done_msg      ; Print "DONE" message and return
            ldx     #>done_msg
            jmp     prt_message
.endproc

; Execute extended command
; Command number in Y
;
.proc fdc_exec_extended
            sty     fdc_extended+2  ; Set command number 

            ldx     #fdc_extended-fdc_commands  ; Command index into X
            jsr     fdc_exec_command
            bcc     waitint

            ; Read results into memory even if it failed (should be
            ; unsupported command)
            ;
            jsr     fdc_read_result
            sec
            rts

waitint:    lda     HSRCW           ; Wait until FDC interrupt
            bmi     waitint

            jsr     fdc_read_result
            rts
.endproc

.proc fdc_exec_senseint
waitint:    lda     HSRCW           ; Wait until FDC interrupt
            bmi     waitint

            ; Execute Sense Interrupt command

            ldx     #fdc_senseint-fdc_commands
            jsr     fdc_exec_command
            bcs     return

            ; Read results into memory

            jsr     fdc_read_result
            bcs     return

            ; Look for error in the status registers

            lda     disk_status     ; Check ST0
            and     #$f8            ; Delete don't care bits
            cmp     #$20            ; Result must be "Seek Completed"
            bne     fdc_fail
            clc
return:     rts
.endproc

.proc fdc_fail
            sec
            rts
.endproc

.proc fdc_exec_command
            lda     MSTR            ; Load Main Status Register
            and     #$10            ; Check if busy
            bne     fdc_fail

            ldy     fdc_commands, x ; Load command length
            inx

sendbyte:   lda     MSTR            ; Wait until RQM from controller
            bpl     sendbyte

            and     #$40            ; Test data direction bit
            bne     fdc_fail        ; Error if controller wants to talk

            lda     fdc_commands, x ; Get command byte
            sta     DATR            ; Store into FDC data register
            inx                     ; Next command byte
            dey
            bne     sendbyte

            clc
            rts
.endproc

.proc fdc_read_result
            ldx     #0

getbyte:    lda     MSTR
            bpl     getbyte
            and     #$40            ; Test data direction bit        
            beq     fdc_fail        ; Error if controller wants to listen

            lda     DATR            ; Get status byte from data register
            sta     disk_status, x  ; Put it into memory
            inx                     ; Next byte
            nop                     ; Give the controller time to update
            nop                     ; the MSTR with a valid busy status
            lda     #$10            ; Check if busy and go get another
            and     MSTR            ; byte while so
            bne     getbyte         ;

            clc
            rts
.endproc

.proc err_withmsg
            sta     string+0        ; Save pointer to reason message
            stx     string+1        ;

            ldy     #0              ; Init output buffer index

prt_err:    lda     error_msg,y     ; Coppy error message to buffer 
            beq     prt_msg         ;
            sta     (OUTBUFP),y     ;

            ; Adjust string pointer so we can later use Y on it
            lda     string
            bne     skip
            dec     string+1
skip:       dec     string

            iny                     ; Next char
            bne     prt_err         ; Always jumps

prt_msg:    lda     (string),y      ; Copy reason message to buffer
            beq     prt_buf         ;
            sta     (OUTBUFP),y     ;
            iny                     ;
            bne     prt_msg         ; Always jumps
            ; Not reached
.endproc

.proc prt_buf
            jmp     POUTBUFFCR02    ; Print output buffer to console (length in Y)
.endproc

.proc prt_message
            sta     string
            stx     string+1

            ldy     #0
loop:       lda     (string),y
            beq     prt_buf
            sta     (OUTBUFP),y
            iny
            bne     loop
            ; Not reached
.endproc

            ; Initialized data

            .data

; Those three can be the same as they're never used at the same time
;
ro_flag:                            ; Read-only flag
rn_flag:                            ; Rename flag
pk_flag:    .byte   0               ; Packed flag
ds_flag:    .byte   0               ; Dual side flag   

; FDC command strings
;
fdc_commands:

fdc_senseint:
            .byte   1               ; Command length
            .byte   8               ; Sense Interrupt Status

fdc_extended:
            .byte   2               ; Command length
            .byte   $1f             ; Extended command
            .byte   0               ; Command number

; Messages and other strins
;
error_msg:  .byte   "ERROR: ", 0
fdc_msg:    .byte   "UNEXPECTED FLOPPY CONTROLLER FAILURE.", 0
real_msg:   .byte   "NOT SUPPORTED. OLD FIRMWARE OR REAL HARDWARE.", 0
rddir_msg:  .byte   "FAILED TO READ SD DIRECTORY.", 0
config_msg: .byte   "CAN'T UPDATE THE SD CONFIG FILE.", 0
inval_msg:  .byte   "INVALID DRIVE NUMBER.", 0
notmnt_msg: .byte   "DRIVE WAS NOT MOUNTED.", 0
drvmnt_msg: .byte   "DRIVE ALREADY MOUNTED.", 0
imgmnt_msg: .byte   "IMAGE IS (ALREADY) MOUNTED.", 0
umnerr_msg: .byte   "FAILED TO UNMOUNT DRIVE.", 0
mnterr_msg: .byte   "FAILED TO MOUNT DRIVE.", 0
delerr_msg: .byte   "FAILED TO DELETE FILE.", 0
newerr_msg: .byte   "FAILED TO CREATE NEW IMAGE FILE.", 0
cpyerr_msg: .byte   "FAILED TO COPY OR RENAME FILE.", 0
imgerr_msg: .byte   "MISSING OR INVALID IMAGE NAME OR FILE.", 0
notfnd_msg: .byte   "FILE NOT FOUND.", 0
dskful_msg: .byte   "SD CARD FULL.", 0
exists_msg: .byte   "FILE EXISTS.", 0

done_msg:   .byte   "DONE.", 0

PROG_SIZE = * - init

            ; Uninitialized data

            .bss

disk_status: .res   8               ; Result phase readouts from NEC-765

            .end