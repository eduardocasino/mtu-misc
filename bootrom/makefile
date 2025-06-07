ARCH ?= mtu

AS = $(CC65)ca65
LD = $(CC65)ld65

$(shell mkdir -p build/$(ver))

ifeq ($(wildcard src/$(ARCH)),)
    $(error #### UNSUPPORTED ARCH >>> $(ARCH) <<< ####)
endif

BUILD = ./build/
BINDIR = $(BUILD)

all : info tests

info:
	$(info #### BUILDING FOR ARCH >>> $(ARCH) <<< ####)


tests: $(BINDIR)bootrom.bin
ifeq ($(ARCH), mtu)
	echo -n "MD5SUM "
	bash -c "cd build/; (md5sum --quiet -c ../checksums.md5 && echo \"OK.\")";
endif

$(BINDIR)bootrom.bin: $(BUILD)bootrom.o src/$(ARCH)/bootrom.cfg
	echo "LD $@"
	$(LD) -C src/$(ARCH)/bootrom.cfg -vm -m $(basename $@).map -o $@ $<

$(BINDIR)bootrom.o: src/$(ARCH)/bootrom.asm
	echo "AS $@"
	$(AS) -U -l $(basename $@).lst -o $@ $<

clean:
	echo "CLEAN"
	rm -f $(BINDIR)bootrom.bin $(BUILD)bootrom.lst $(BUILD)bootrom.map $(BUILD)bootrom.o

.SILENT:
