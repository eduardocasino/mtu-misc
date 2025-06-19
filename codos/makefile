ARCH ?= mtu

VERSIONS = 14 15 17

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/ := $(shell mkdir -p build/$(ver))))
images/ := $(shell mkdir -p images)

AS = $(CC65)ca65
LD = $(CC65)ld65

AFLAGS = -U -D $(ARCH)

IMAGES = $(VERSIONS:%=images/codos%.imd)

ifeq ($(wildcard system_$(ARCH).map),)
    $(error #### UNSUPPORTED ARCH >>> $(ARCH) <<< ####)
endif

ifeq ($(ARCH),kim1)
  IODRIVER = kimiotty
  SYSDEPS = kim1.inc
else
  IODRIVER = iodriver
  SYSDEPS = monomeg.inc
endif

# Explicitly exclude codos.z as it is copied differently into the disk image
#
FILESYSTEM = 	comdproc.z \
				svcproc.z \
				$(IODRIVER).z

ifeq ($(ARCH),mtu)
  FILESYSTEM += graphdriver.z
endif				

FILESYSTEM +=	dir.c \
				kill.c \
				format.c \
				copyf.c \
				copyf1drive.c \
				sysgendisk.c \
				startup.j \
				syserrmsg.z

ifeq ($(ARCH),mtu)
  FILESYSTEM += cpuid.c
endif

FILESYSTEM +=	basic.c

CODOSOBJS = codos.o comdproc.o svcproc.o $(IODRIVER).o

CODOSFILES = $(CODOSOBJS:.o=.z) $(CODOSOBJS:.o=.z.map)

OVERLAYS =	overlay01.bin \
			overlay02.bin \
			overlay03.bin \
			overlay04.bin \
			overlay05.bin \
			overlay06.bin \
			overlay07.bin \
			overlay08.bin \
			overlay09.bin \
			overlay10.bin \
			overlay11.bin \
			overlay12.bin \
			overlay13.bin \
			overlay14.bin \
			overlay15.bin \
			overlay16.bin

all: info tests
	
info:
	$(info #### BUILDING FOR ARCH >>> $(ARCH) <<< ####)

images: $(IMAGES)

tests: images
ifeq ($(ARCH), mtu)
	echo "MD5SUM"
	$(foreach ver,$(VERSIONS),bash -c "cd build/$(ver); echo -n \"Version $(ver): \"; \
			(md5sum --quiet -c ../../checksums$(ver).md5 && echo \"OK.\")";)
endif

$(foreach ver,$(VERSIONS),$(eval images/codos$(ver).imd: build/$(ver)/codos.z build/$(ver)/overlays.bin $(addprefix build/$(ver)/,$(FILESYSTEM)) \
	; echo "CODOSDSK $$@" \
	; ../utils/codosdsk format -y -p -s 2 -t 12 -v $(ver) -c build/$(ver)/codos.z -o build/$(ver)/overlays.bin images/codos$(ver).imd \
	; for f in $(FILESYSTEM); do ../utils/codosdsk copy -a images/codos$(ver).imd build/$(ver)/$$$$f 0:$$$$f ; done))

$(foreach ver,$(VERSIONS),$(eval $(addprefix build/$(ver)/,$(CODOSFILES)) &: $(addprefix build/$(ver)/,$(CODOSOBJS)) build/$(ver)/codos.cfg \
	; echo "LD $$@" \
	; $(LD) -C $$(@:.z=.cfg) -vm -m $(basename $$@).map -o $$@ $(addprefix build/$(ver)/,$(CODOSOBJS))))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/symbols.inc: build/$(ver)/codos.z.map \
	; echo "SYMBOLS $$@" \
	; ./symbols.py --mapfile $$< > $$@))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/%.cfg: system_$(ARCH).map %.cfg.in \
	; echo "CFG $$@" \
	; echo "## AUTOMATICALLY GENERATED, DO NOT MODIFY!!!!" > $$@ \
	; cat $$^ | sed -e 's/%%VERSION%%/$(ver)/' -e 's/%%IODRIVER%%/$(IODRIVER)/' >> $$@))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/codos.o: codos.asm build/$(ver)/k1013.inc codos.inc $(SYSDEPS) \
	; echo "AS $$@" \
	; $(AS) $(AFLAGS) -D CODOS2_VER=$(ver) -I build/$(ver) -l $(basename $$@).lst -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/comdproc.o: comdproc.asm build/$(ver)/k1013.inc codos.inc $(SYSDEPS) \
	; echo "AS $$@" \
	; $(AS) $(AFLAGS) -D CODOS2_VER=$(ver) -I build/$(ver) -l $(basename $$@).lst -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/svcproc.o: svcproc.asm build/$(ver)/k1013.inc codos.inc \
	; echo "AS $$@" \
	; $(AS) $(AFLAGS) -D CODOS2_VER=$(ver) -I build/$(ver) -l $(basename $$@).lst -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/$(IODRIVER).o: $(IODRIVER).asm $(SYSDEPS) \
	; echo "AS $$@" \
	; $(AS) $(AFLAGS) -D CODOS2_VER=$(ver) -I build/$(ver) -l $(basename $$@).lst -o $$@ $$<))

ifeq ($(ARCH), mtu)
$(foreach ver,$(VERSIONS),$(eval build/$(ver)/graphdriver.z: build/$(ver)/graphdriver.o build/$(ver)/graphdriver.cfg \
	; echo "LD $$@" \
	; $(LD) -C $$(@:.z=.cfg) -vm -m $(basename $$@).map -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/graphdriver.o: graphdriver.asm build/$(ver)/symbols.inc $(SYSDEPS) \
	; echo "AS $$@" \
	; $(AS) $(AFLAGS) -D CODOS2_VER=$(ver) -I build/$(ver) -l $(basename $$@).lst -o $$@ $$<))
endif

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/overlays.bin: $(addprefix build/$(ver)/,$(OVERLAYS)) \
	; echo "CAT $$@" \
	; cat $$^ > $$@))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/overlay%.bin: build/$(ver)/overlay%.o build/$(ver)/overlay.cfg \
	; echo "LD $$@" \
	; $(LD) -C build/$(ver)/overlay.cfg -vm -m $(basename $$@).map -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/overlay%.o: overlay%.asm build/$(ver)/k1013.inc build/$(ver)/symbols.inc $(SYSDEPS) \
	; echo "AS $$@" \
	; $(AS) $(AFLAGS) -D CODOS2_VER=$(ver) -I build/$(ver) -l $(basename $$@).lst -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/%.c: build/$(ver)/%.o build/$(ver)/%.cfg \
	; echo "LD $$@" \
	; $(LD) -C $$(@:.c=.cfg) -vm -m $(basename $$@).map -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/%.o: %.asm build/$(ver)/k1013.inc build/$(ver)/symbols.inc  \
	; echo "AS $$@" \
	; $(AS) $(AFLAGS) -D CODOS2_VER=$(ver) -I build/$(ver) -l $(basename $$@).lst -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/basic.c: build/$(ver)/basic.o build/$(ver)/basic-$(ARCH).cfg \
	; echo "LD $$@" \
	; $(LD) -C $$(@:.c=-$(ARCH).cfg) -vm -m $(basename $$@).map -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/%: fs/$(ARCH)/% \
	; echo "CP $$@" \
	; cp $$< $$@))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/k1013.inc: k1013.asm build/$(ver)/codos.cfg \
	; echo "SYMBOLS $$@" \
	; $(AS) $(AFLAGS) -D CODOS2_VER=$(ver) -l $(basename $$@).lst -o $$(@:.inc=.o) $$< \
	; $(LD) -C build/$(ver)/codos.cfg -vm -m $(basename $$@).map -o $$(@:.inc=.dat) $$(@:.inc=.o) \
	; ./symbols.py --mapfile $(basename $$@).map > $$@ ))

clean:
	echo "CLEAN"
	rm -rf $(VERSIONS:%=build/%) $(VERSIONS:%=images/codos%.imd)

.SILENT:

.PHONY: info
