.PHONY: all clean

all: asm tooly nesgo

asm:
	cd asm6f && $(MAKE) all

tooly: asm
	cd TOOLY && $(MAKE) all

nesgo:
	go build -o build/nesgo cmd/nesgo.go

clean:
	cd asm6f && $(MAKE) clean
	cd TOOLY && $(MAKE) clean
	rm -rf build
