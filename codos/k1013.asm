        .export USRRAM, SYSRAM

USRRAM  := __USRRAM                 ; K-1013 onboard user RAM
SYSRAM  := __SYSRAM                 ; K-1013 onboard system RAM

        .segment "iodata"
        .segment "iodrvjmp"
        .segment "tabtbl"
        .segment "chartbl"
        .segment "header"
        .segment "zp" : zeropage
        .segment "scratch0"
        .segment "scratch1"
        .segment "ioscratch0"
        .segment "ioscratch"
        .segment "codos"
        .segment "cmdproc"
        .segment "svcproc"
        .segment "iodriver"
