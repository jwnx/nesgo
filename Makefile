.PHONY: all clean

all: asm tooly nesgo

asm:
	cd asm6f && $(MAKE) all

tooly: asm
	cd TOOLY && $(MAKE) all

nesgo:
	go build -o build/nesgo cmd/nesgo.go

test: asm
	./asm6f/asm6f ./tests/adc.s ./tests/adc.nes
	./asm6f/asm6f ./tests/and.s ./tests/and.nes
	./asm6f/asm6f ./tests/brk.s ./tests/brk.nes

	mkdir ./tmp

	./build/nesgo -rom ./tests/adc.nes > ./tmp/adc.out
	./build/nesgo -rom ./tests/and.nes > ./tmp/and.out
	./build/nesgo -rom ./tests/brk.nes > ./tmp/brk.out

	diff ./tmp/adc.out ./res/adc
	diff ./tmp/and.out ./res/and
	diff ./tmp/brk.out ./res/brk

clean:
	cd asm6f && $(MAKE) clean
	cd TOOLY && $(MAKE) clean
	rm -rf build
	rm -rf tmp
