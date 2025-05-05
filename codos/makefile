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
		$(VERSIONS:%=build/%/comdproc.z) \
		$(VERSIONS:%=build/%/svcproc.z) \
		$(VERSIONS:%=build/%/syserrmsg.z) \
		$(VERSIONS:%=build/%/overlays.bin)

	@echo "Running tests..."
	@$(foreach ver,$(VERSIONS),bash -c "cd build/$(ver); echo \"Version $(ver):\"; \
			(md5sum -c ../../checksums$(ver).md5 || exit 1)";)

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/syserrmsg.z: syserrmsg.src \
	; bbe -e 's/\x0a/\x0d/' < $$< > $$@))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/codos.z: build/$(ver)/codos.bin \
	; ./makez.sh $$< 0xe600 0xe54a))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/comdproc.z: build/$(ver)/comdproc.bin \
	; ./makez.sh $$< 0xd800 0xd800))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/svcproc.z: build/$(ver)/svcproc.bin \
	; ./makez.sh $$< 0xdd20 0xdd20))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/codos.bin:: build/$(ver)/codos.cfg))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/codos.bin:: build/$(ver)/codos.o build/$(ver)/comdproc.o build/$(ver)/svcproc.o \
	; ld65 -C build/$(ver)/codos.cfg -vm -m $(basename $$@).map -o $$@ $$^ \
	; ./symbols.py --mapfile $(basename $$@).map > build/$(ver)/symbols.inc))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/codos.cfg: codos.cfg.in \
	; @sed -e 's/%%VERSION%%/$(ver)/' < $$< > $$@))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/%.o: %.asm \
	; ca65 -U -D CODOS2_VER=$(ver) -l $(basename $$@).lst -o $$@ $$<))

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

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/overlay%.bin: build/$(ver)/overlay%.o overlay.cfg \
	; ld65 -C overlay.cfg -vm -m $(basename $$@).map -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval build/$(ver)/overlay%.o: overlay%.asm build/$(ver)/codos.bin \
	; ca65 -U -D CODOS2_VER=$(ver) -I build/$(ver) -l $(basename $$@).lst -o $$@ $$<))

clean:
	rm -rf $(VERSIONS:%=build/%)
