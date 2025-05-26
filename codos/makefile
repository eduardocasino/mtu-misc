VERSIONS = 14 15 17

AS = $(CC65)ca65
LD = $(CC65)ld65

define newline


endef

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/ := $(shell mkdir -p build/$(ver))))
images/ := $(shell mkdir -p images)

FILESYSTEM = $(VERSIONS:%=build/%/codos.z) \
		$(VERSIONS:%=build/%/syserrmsg.z) \
		$(VERSIONS:%=build/%/overlays.bin) \
		$(VERSIONS:%=build/%/dir.c) \
		$(VERSIONS:%=build/%/format.c) \
		$(VERSIONS:%=build/%/sysgendisk.c) \
		fs/startup.j \
		fs/syserrmsg.z

IMAGES = $(VERSIONS:%=images/codos%.imd)

all: tests

images: $(IMAGES)

tests: images
	@echo "MD5SUM"
	@$(foreach ver,$(VERSIONS),bash -c "cd build/$(ver); echo -n \"Version $(ver): \"; \
			(md5sum --quiet -c ../../checksums$(ver).md5 && echo \"OK.\")";)

$(foreach ver,$(VERSIONS),$(eval images/codos$(ver).imd: build/$(ver)/codos.z build/$(ver)/overlays.bin build/$(ver)/syserrmsg.z build/$(ver)/startup.j build/$(ver)/dir.c build/$(ver)/sysgendisk.c build/$(ver)/format.c \
	; echo "CODOSDSK $$@" \
	; ../utils/codosdsk format -y -p -s 2 -t 12 -v $(ver) -c build/$(ver)/codos.z -o build/$(ver)/overlays.bin images/codos$(ver).imd \
	; ../utils/codosdsk copy images/codos$(ver).imd build/$(ver)/comdproc.z 0:comdproc.z \
	; ../utils/codosdsk copy -a images/codos$(ver).imd build/$(ver)/syserrmsg.z 0:syserrmsg.z \
	; ../utils/codosdsk copy -a images/codos$(ver).imd build/$(ver)/startup.j 0:startup.j \
	; ../utils/codosdsk copy images/codos$(ver).imd build/$(ver)/svcproc.z 0:svcproc.z \
	; ../utils/codosdsk copy images/codos$(ver).imd build/$(ver)/iodriver.z 0:iodriver.z \
	; ../utils/codosdsk copy images/codos$(ver).imd build/$(ver)/sysgendisk.c 0:sysgendisk.c \
	; ../utils/codosdsk copy images/codos$(ver).imd build/$(ver)/dir.c 0:dir.c \
	; ../utils/codosdsk copy images/codos$(ver).imd build/$(ver)/format.c 0:format.c))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/codos.z:: build/$(ver)/codos.cfg))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/codos.z:: build/$(ver)/codos.o build/$(ver)/comdproc.o build/$(ver)/svcproc.o build/$(ver)/iodriver.o \
	; echo "LD $$@" \
	; $(LD) -C build/$(ver)/codos.cfg -vm -m $(basename $$@).map -o $$@ $$^))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/symbols.inc: build/$(ver)/codos.z \
	; echo "SYMBOLS $$@" \
	; ./symbols.py --mapfile build/$(ver)/codos.z.map > build/$(ver)/symbols.inc))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/codos.cfg: codos.cfg.in \
	; echo "SED $$@" \
	; sed -e 's/%%VERSION%%/$(ver)/' < $$< > $$@))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/overlay.cfg: overlay.cfg.in \
	; echo "SED $$@" \
	; sed -e 's/%%VERSION%%/$(ver)/' < $$< > $$@))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/codos.o: codos.asm build/$(ver)/k1013.inc monomeg.inc \
	; echo "AS $$@" \
	; $(AS) -U -D CODOS2_VER=$(ver) -I build/$(ver) -l $(basename $$@).lst -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/comdproc.o: comdproc.asm build/$(ver)/k1013.inc monomeg.inc \
	; echo "AS $$@" \
	; $(AS) -U -D CODOS2_VER=$(ver) -I build/$(ver) -l $(basename $$@).lst -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/svcproc.o: svcproc.asm build/$(ver)/k1013.inc \
	; echo "AS $$@" \
	; $(AS) -U -D CODOS2_VER=$(ver) -I build/$(ver) -l $(basename $$@).lst -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/iodriver.o: iodriver.asm monomeg.inc \
	; echo "AS $$@" \
	; $(AS) -U -D CODOS2_VER=$(ver) -I build/$(ver) -l $(basename $$@).lst -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/%.o: %.asm build/$(ver)/symbols.inc build/$(ver)/k1013.inc \
	; echo "AS $$@" \
	; $(AS) -U -D CODOS2_VER=$(ver) -I build/$(ver) -l $(basename $$@).lst -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval \
OVERLAYS =	build/$(ver)/overlay01.bin \
			build/$(ver)/overlay02.bin \
			build/$(ver)/overlay03.bin \
			build/$(ver)/overlay04.bin \
			build/$(ver)/overlay05.bin \
			build/$(ver)/overlay06.bin \
			build/$(ver)/overlay07.bin \
			build/$(ver)/overlay08.bin \
			build/$(ver)/overlay09.bin \
			build/$(ver)/overlay10.bin \
			build/$(ver)/overlay11.bin \
			build/$(ver)/overlay12.bin \
			build/$(ver)/overlay13.bin \
			build/$(ver)/overlay14.bin \
			build/$(ver)/overlay15.bin \
			build/$(ver)/overlay16.bin \
			$(newline)\
			$(newline)\
build/$(ver)/overlays.bin: $${OVERLAYS} \
	; echo "CAT $$@" \
	; cat $$^ > $$@))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/overlay%.bin: build/$(ver)/overlay%.o build/$(ver)/overlay.cfg \
	; echo "LD $$@" \
	; $(LD) -C build/$(ver)/overlay.cfg -vm -m $(basename $$@).map -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/overlay%.o: overlay%.asm build/$(ver)/k1013.inc build/$(ver)/symbols.inc monomeg.inc \
	; echo "AS $$@" \
	; $(AS) -U -D CODOS2_VER=$(ver) -I build/$(ver) -l $(basename $$@).lst -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/dir.c: build/$(ver)/dir.o dir.cfg \
	; echo "LD $$@" \
	; $(LD) -C dir.cfg -vm -m $(basename $$@).map -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/dir.o: dir.asm build/$(ver)/k1013.inc build/$(ver)/symbols.inc  \
	; echo "AS $$@" \
	; $(AS) -U -D CODOS2_VER=$(ver) -I build/$(ver) -l $(basename $$@).lst -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/sysgendisk.c: build/$(ver)/sysgendisk.o sysgendisk.cfg \
	; echo "LD $$@" \
	; $(LD) -C sysgendisk.cfg -vm -m $(basename $$@).map -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/sysgendisk.o: sysgendisk.asm build/$(ver)/k1013.inc build/$(ver)/symbols.inc  \
	; echo "AS $$@" \
	; $(AS) -U -D CODOS2_VER=$(ver) -I build/$(ver) -l $(basename $$@).lst -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/format.c: build/$(ver)/format.o format.cfg \
	; echo "LD $$@" \
	; $(LD) -C format.cfg -vm -m $(basename $$@).map -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/format.o: format.asm build/$(ver)/k1013.inc build/$(ver)/symbols.inc  \
	; echo "AS $$@" \
	; $(AS) -U -D CODOS2_VER=$(ver) -I build/$(ver) -l $(basename $$@).lst -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/syserrmsg.z: fs/syserrmsg.z  \
	; echo "CP $$@" \
	; cp $$< $$@))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/startup.j: fs/startup.j  \
	; echo "CP $$@" \
	; cp $$< $$@))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/k1013.inc: k1013.asm build/$(ver)/codos.cfg \
	; echo "SYMBOLS $$@" \
	; $(AS) -U -D CODOS2_VER=$(ver) -l $(basename $$@).lst -o build/$(ver)/k1013.o $$< \
	; $(LD) -C build/$(ver)/codos.cfg -vm -m $(basename $$@).map -o build/$(ver)/k1013.dat build/$(ver)/k1013.o \
	; ./symbols.py --mapfile $(basename $$@).map > $$@ ))

clean:
	echo "CLEAN"
	rm -rf $(VERSIONS:%=build/%) $(VERSIONS:%=images/*)

.SILENT:
