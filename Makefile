.PHONY: all clean

all: asm tooly

asm:
	cd asm6f && $(MAKE) all

tooly: asm
	cd TOOLY && $(MAKE) all

clean:
	cd asm6f && $(MAKE) clean
	cd TOOLY && $(MAKE) clean
