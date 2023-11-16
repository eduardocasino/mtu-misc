BINDIR = .

AFLAGS =

all : $(BINDIR)/bootrom.bin

clean:
	rm -f $(BINDIR)/*.bin $(BINDIR)/*.hex $(BINDIR)/*.lst

$(BINDIR)/%.bin : %.asm
	64tass $(AFLAGS) -o $(@D)/$(*F).hex --intel-hex $< -L $(@D)/$(*F).lst
	srec_cat $(@D)/$(*F).hex -intel -offset -0xFF00 -o $@ -Binary
