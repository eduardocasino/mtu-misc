.PRECIOUS: %.bin

all:	codos14.z codos15.z codos17.z comdproc.z

codos14.z: codos14.bin
	./makez.sh $< 0xe600 0xe54a

codos15.z: codos15.bin
	./makez.sh $< 0xe600 0xe54a

codos17.z: codos17.bin
	./makez.sh $< 0xe600 0xe54a

comdproc.z: comdproc.bin
	./makez.sh $< 0xd800 0xd800

codos14.bin: codos14.o comdproc.o
	ld65 -C codos.cfg -vm -m $(basename $@).map -o $@ $^

codos15.bin: codos15.o comdproc.o
	ld65 -C codos.cfg -vm -m $(basename $@).map -o $@ $^ 

codos17.bin: codos17.o comdproc.o
	ld65 -C codos.cfg -vm -m $(basename $@).map -o $@ $^

codos14.o: codos.asm
	ca65 -U -D CODOS2_VER=14 -l $(basename $@).lst -o $@ $<

codos15.o: codos.asm 
	ca65 -U -D CODOS2_VER=15 -l $(basename $@).lst -o $@ $<

codos17.o: codos.asm 
	ca65 -U -D CODOS2_VER=17 -l $(basename $@).lst -o $@ $<

comdproc.o: comdproc.asm 
	ca65 -U -l $(basename $@).lst -o $@ $<

clean:
	rm -f *.o *.bin *.z *.map *.lst
