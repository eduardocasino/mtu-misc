VERSIONS = 14 15 17

define newline


endef

$(foreach ver,$(VERSIONS),$(eval $(ver)/ := $(shell mkdir -p $(ver))))

.PRECIOUS:	$(VERSIONS:%=%/overlay01.o) \
			$(VERSIONS:%=%/overlay02.o) \
			$(VERSIONS:%=%/overlay03.o) \
			$(VERSIONS:%=%/overlay04.o) \
			$(VERSIONS:%=%/overlay05.o) \
			$(VERSIONS:%=%/overlay06.o) \
			$(VERSIONS:%=%/overlay07.o) \
			$(VERSIONS:%=%/overlay08.o) \
			$(VERSIONS:%=%/overlay09.o) \
			$(VERSIONS:%=%/overlay10.o) \
			$(VERSIONS:%=%/overlay11.o) \
			$(VERSIONS:%=%/overlay12.o) \
			$(VERSIONS:%=%/overlay13.o) \
			$(VERSIONS:%=%/overlay14.o) \
			$(VERSIONS:%=%/overlay15.o) \
			$(VERSIONS:%=%/overlay16.o) \

all: 	$(VERSIONS:%=%/codos.z) \
		$(VERSIONS:%=%/comdproc.z) \
		$(VERSIONS:%=%/svcproc.z) \
		$(VERSIONS:%=%/syserrmsg.z) \
		$(VERSIONS:%=%/overlays.bin)

	@echo "Running tests..."
	@$(foreach ver,$(VERSIONS),bash -c "cd $(ver); echo \"Version $(ver):\"; \
			(md5sum -c ../checksums$(ver).md5 || exit 1)";)

$(foreach ver,$(VERSIONS),$(eval $(ver)/syserrmsg.z: syserrmsg.src \
	; bbe -e 's/\x0a/\x0d/' < $$< > $$@))

$(foreach ver,$(VERSIONS),$(eval $(ver)/codos.z: $(ver)/codos.bin \
	; ./makez.sh $$< 0xe600 0xe54a))

$(foreach ver,$(VERSIONS),$(eval $(ver)/comdproc.z: $(ver)/comdproc.bin \
	; ./makez.sh $$< 0xd800 0xd800))

$(foreach ver,$(VERSIONS),$(eval $(ver)/svcproc.z: $(ver)/svcproc.bin \
	; ./makez.sh $$< 0xdd20 0xdd20))

$(foreach ver,$(VERSIONS),$(eval $(ver)/codos.bin:: $(ver)/codos.cfg))

$(foreach ver,$(VERSIONS),$(eval $(ver)/codos.bin:: $(ver)/codos.o $(ver)/comdproc.o $(ver)/svcproc.o \
	; ld65 -C $(ver)/codos.cfg -vm -m $(basename $$@).map -o $$@ $$^ \
	; ./symbols.py --mapfile $(basename $$@).map > $(ver)/symbols.inc))

$(foreach ver,$(VERSIONS),$(eval $(ver)/codos.cfg: codos.cfg.in \
	; @sed -e 's/%%VERSION%%/$(ver)/' < $$< > $$@))

$(foreach ver,$(VERSIONS),$(eval $(ver)/%.o: %.asm \
	; ca65 -U -D CODOS2_VER=$(ver) -l $(basename $$@).lst -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval \
OVERLAYS =	$(ver)/overlay01.bin \
			$(ver)/overlay02.bin \
			$(ver)/overlay03.bin \
			$(ver)/overlay04.bin \
			$(ver)/overlay05.bin \
			$(ver)/overlay06.bin \
			$(ver)/overlay07.bin \
			$(ver)/overlay08.bin \
			$(ver)/overlay09.bin \
			$(ver)/overlay10.bin \
			$(ver)/overlay11.bin \
			$(ver)/overlay12.bin \
			$(ver)/overlay13.bin \
			$(ver)/overlay14.bin \
			$(ver)/overlay15.bin \
			$(ver)/overlay16.bin \
			$(newline)\
			$(newline)\
$(ver)/overlays.bin: $${OVERLAYS} \
	; cat $$^ > $$@))

$(foreach ver,$(VERSIONS),$(eval $(ver)/overlay%.bin: $(ver)/overlay%.o overlay.cfg \
	; ld65 -C overlay.cfg -vm -m $(basename $$@).map -o $$@ $$<))

$(foreach ver,$(VERSIONS),$(eval $(ver)/overlay%.o: overlay%.asm $(ver)/codos.bin \
	; ca65 -U -D CODOS2_VER=$(ver) -I $(ver) -l $(basename $$@).lst -o $$@ $$<))

clean:
	rm -f */*.o */*.bin */*.z */*.map */*.lst */*.cfg */*.inc
