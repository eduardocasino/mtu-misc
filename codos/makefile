VERSIONS = 14 15 17

CODOSSUM14    = 1e20bb6b0491704bff157cf2055a6d07
CODOSSUM15    = 7f04f42403cbf33254d3a6227b405d3d
CODOSSUM17    = d66fd98db5b22c8d10a69d69fc669171
COMDPROCSUM14 = e96359f4494df0bc70e8ef8f32f26a05
COMDPROCSUM15 = e96359f4494df0bc70e8ef8f32f26a05
COMDPROCSUM17 = e96359f4494df0bc70e8ef8f32f26a05
SVCPROCSUM14  = fbbdf04aed0515acb392145f773ff3b2
SVCPROCSUM15  = 65879de4ffc9092d6530d0d99805d08d
SVCPROCSUM17  = 16d44229b8ccff04277cf951bdec5451

$(foreach ver,$(VERSIONS),$(eval $(ver)/ := $(shell mkdir -p $(ver))))

.PRECIOUS: %.bin

all: 	$(VERSIONS:%=%/codos.z) \
		$(VERSIONS:%=%/comdproc.z) \
		$(VERSIONS:%=%/svcproc.z) \
		$(VERSIONS:%=%/syserrmsg.z)
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

$(foreach ver,$(VERSIONS),$(eval $(ver)/codos.bin:: $(ver)/codos.o $(ver)/comdproc.o $(ver)/svcproc.o\
	; ld65 -C $(ver)/codos.cfg -vm -m $(basename $$@).map -o $$@ $$^))

$(foreach ver,$(VERSIONS),$(eval $(ver)/codos.cfg: codos.cfg.in \
	; @sed -e 's/%%VERSION%%/$(ver)/' < $$< > $$@))

$(foreach ver,$(VERSIONS),$(eval $(ver)/%.o: %.asm \
	; ca65 -U -D CODOS2_VER=$(ver) -l $(basename $$@).lst -o $$@ $$<))

clean:
	rm -f */*.o */*.bin */*.z */*.map */*.lst */*.cfg
