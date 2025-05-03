VERSIONS = 14 15 17

$(foreach ver,$(VERSIONS),$(eval $(ver)/ := $(shell mkdir -p $(ver))))

.PRECIOUS: %.bin

all: $(VERSIONS:%=%/codos.z) $(VERSIONS:%=%/comdproc.z) $(VERSIONS:%=%/syserrmsg.z)

$(foreach ver,$(VERSIONS),$(eval $(ver)/syserrmsg.z: syserrmsg.src \
	; bbe -e 's/\x0a/\x0d/' < $$< > $$@))

$(foreach ver,$(VERSIONS),$(eval $(ver)/codos.z: $(ver)/codos.bin \
	; ./makez.sh $$< 0xe600 0xe54a))

$(foreach ver,$(VERSIONS),$(eval $(ver)/comdproc.z: $(ver)/comdproc.bin \
	; ./makez.sh $$< 0xd800 0xd800))

$(foreach ver,$(VERSIONS),$(eval $(ver)/codos.bin:: $(ver)/codos.cfg))

$(foreach ver,$(VERSIONS),$(eval $(ver)/codos.bin:: $(ver)/codos.o $(ver)/comdproc.o \
	; ld65 -C $(ver)/codos.cfg -vm -m $(basename $$@).map -o $$@ $$^))

$(foreach ver,$(VERSIONS),$(eval $(ver)/codos.cfg: codos.cfg.in \
	; sed -e 's/%%VERSION%%/$(ver)/' < $$< > $$@))

$(foreach ver,$(VERSIONS),$(eval $(ver)/%.o: %.asm \
	; ca65 -U -D CODOS2_VER=$(ver) -l $(basename $$@).lst -o $$@ $$<))

clean:
	rm -f */*.o */*.bin */*.z */*.map */*.lst */*.cfg
