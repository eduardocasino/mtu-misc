        .export USRRAM, SYSRAM

USRRAM  := __USRRAM_START__         ; K-1013 onboard user RAM
SYSRAM  := __SYSRAM_START__         ; K-1013 onboard system RAM

        .segment "sysdata"
        .segment "header"
        .segment "zp" : zeropage
        .segment "scratch0"
        .segment "scratch1"
        .segment "codos"
        .segment "cmdproc"
        .segment "svcproc"
