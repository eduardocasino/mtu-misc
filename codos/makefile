.PRECIOUS: %.bin

all:	codos14.z codos15.z codos17.z

%.z: %.bin
	./makez.sh $< 0xe600 0xe54a

%.bin: %.o
	ld65 -C codos.cfg -vm -m $(basename $@).map -o $@ $< 

codos14.o: codos.asm 
	ca65 -D CODOS2_VER=14 -l $(basename $@).lst -o $@ $<

codos15.o: codos.asm 
	ca65 -D CODOS2_VER=15 -l $(basename $@).lst -o $@ $<

codos17.o: codos.asm 
	ca65 -D CODOS2_VER=17 -l $(basename $@).lst -o $@ $<

clean:
	rm -f *.o *.bin *.z *.map *.lst
