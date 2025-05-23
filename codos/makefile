VERSIONS = 14 15 17

define newline


endef

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/ := $(shell mkdir -p build/$(ver))))
images/ := $(shell mkdir -p images)

.PRECIOUS:	$(VERSIONS:%=build/%/overlay01.o) \
			$(VERSIONS:%=build/%/overlay02.o) \
			$(VERSIONS:%=build/%/overlay03.o) \
			$(VERSIONS:%=build/%/overlay04.o) \
			$(VERSIONS:%=build/%/overlay05.o) \
			$(VERSIONS:%=build/%/overlay06.o) \
			$(VERSIONS:%=build/%/overlay07.o) \
			$(VERSIONS:%=build/%/overlay08.o) \
			$(VERSIONS:%=build/%/overlay09.o) \
			$(VERSIONS:%=build/%/overlay10.o) \
			$(VERSIONS:%=build/%/overlay11.o) \
			$(VERSIONS:%=build/%/overlay12.o) \
			$(VERSIONS:%=build/%/overlay13.o) \
			$(VERSIONS:%=build/%/overlay14.o) \
			$(VERSIONS:%=build/%/overlay15.o) \
			$(VERSIONS:%=build/%/overlay16.o) \

all: 	$(VERSIONS:%=build/%/codos.z) \
		$(VERSIONS:%=build/%/syserrmsg.z) \
		$(VERSIONS:%=build/%/overlays.bin) \
		$(VERSIONS:%=build/%/dir.c) \
		$(VERSIONS:%=build/%/format.c)

	@echo "Running tests..."
	@$(foreach ver,$(VERSIONS),bash -c "cd build/$(ver); echo \"Version $(ver):\"; \
			(md5sum -c ../../checksums$(ver).md5 || exit 1)";)

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/syserrmsg.z: syserrmsg.src \
	; bbe -e 's/\x0a/\x0d/' < $$< > $$@))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/codos.z:: build/$(ver)/codos.cfg))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/codos.z:: build/$(ver)/codos.o build/$(ver)/comdproc.o build/$(ver)/svcproc.o build/$(ver)/iodriver.o\
	; ld65 -C build/$(ver)/codos.cfg -vm -m $(basename $$@).map -o $$@ $$^))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/symbols.inc: build/$(ver)/codos.z \
	; ./symbols.py --mapfile build/$(ver)/codos.z.map > build/$(ver)/symbols.inc))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/codos.cfg: codos.cfg.in \
	; @sed -e 's/%%VERSION%%/$(ver)/' < $$< > $$@))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/overlay.cfg: overlay.cfg.in \
	; @sed -e 's/%%VERSION%%/$(ver)/' < $$< > $$@))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/codos.o: codos.asm build/$(ver)/k1013.inc \
	; ca65 -U -D CODOS2_VER=$(ver) -I build/$(ver) -l $(basename $$@).lst -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/comdproc.o: comdproc.asm build/$(ver)/k1013.inc \
	; ca65 -U -D CODOS2_VER=$(ver) -I build/$(ver) -l $(basename $$@).lst -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/svcproc.o: svcproc.asm build/$(ver)/k1013.inc \
	; ca65 -U -D CODOS2_VER=$(ver) -I build/$(ver) -l $(basename $$@).lst -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/iodriver.o: iodriver.asm \
	; ca65 -U -D CODOS2_VER=$(ver) -I build/$(ver) -l $(basename $$@).lst -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/%.o: %.asm build/$(ver)/symbols.inc build/$(ver)/k1013.inc \
	; ca65 -U -D CODOS2_VER=$(ver) -I build/$(ver) -l $(basename $$@).lst -o $$@ $$<))

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
	; cat $$^ > $$@))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/overlay%.bin: build/$(ver)/overlay%.o build/$(ver)/overlay.cfg \
	; ld65 -C build/$(ver)/overlay.cfg -vm -m $(basename $$@).map -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/overlay%.o: overlay%.asm build/$(ver)/k1013.inc build/$(ver)/symbols.inc  \
	; ca65 -U -D CODOS2_VER=$(ver) -I build/$(ver) -l $(basename $$@).lst -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/dir.c: build/$(ver)/dir.o dir.cfg \
	; ld65 -C dir.cfg -vm -m $(basename $$@).map -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/dir.o: dir.asm build/$(ver)/k1013.inc build/$(ver)/symbols.inc  \
	; ca65 -U -D CODOS2_VER=$(ver) -I build/$(ver) -l $(basename $$@).lst -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/format.c: build/$(ver)/format.o format.cfg \
	; ld65 -C format.cfg -vm -m $(basename $$@).map -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/format.o: format.asm build/$(ver)/k1013.inc build/$(ver)/symbols.inc  \
	; ca65 -U -D CODOS2_VER=$(ver) -I build/$(ver) -l $(basename $$@).lst -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/k1013.inc: k1013.asm build/$(ver)/codos.cfg \
	; ca65 -U -D CODOS2_VER=$(ver) -l $(basename $$@).lst -o build/$(ver)/k1013.o $$< \
	; ld65 -C build/$(ver)/codos.cfg -vm -m $(basename $$@).map -o build/$(ver)/k1013.dat build/$(ver)/k1013.o \
	; ./symbols.py --mapfile $(basename $$@).map > $$@ ))

clean:
	rm -rf $(VERSIONS:%=build/%)
