all:
	cl65 -t nes -o gnop.nes gnop.asm
clean:
	rm -rf *.nes *.o
