AS = $(CC65)ca65
LD = $(CC65)ld65


BINDIR = ./

all : $(BINDIR)bootrom.bin

$(BINDIR)bootrom.bin: $(BUILD)bootrom.o bootrom.cfg
	echo "LD $@"
	$(LD) -C bootrom.cfg -vm -m $(basename $@).map -o $@ $<

$(BINDIR)bootrom.o: bootrom.asm
	echo "AS $@"
	$(AS) -U -l $(basename $@).lst -o $@ $<

clean:
	echo "CLEAN"
	rm -f $(BINDIR)bootrom.bin $(BINDIR)bootrom.lst $(BINDIR)bootrom.map $(BINDIR)bootrom.o

.SILENT: