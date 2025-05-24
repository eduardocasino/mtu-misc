MAKEFLAGS += --no-print-directory

TOPTARGETS := all clean

SUBDIRS := bootrom utils codos

$(TOPTARGETS): $(SUBDIRS)
$(SUBDIRS):
	echo "Entering $@..."
	$(MAKE) -C $@ $(MAKECMDGOALS)

.PHONY: $(TOPTARGETS) $(SUBDIRS)

.SILENT:
