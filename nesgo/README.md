# NESGO: A Go NES Emulator

Requires `go`!


Building:
```
git submodule update --init --recursive
make

```


Usage:

```
./build/nesgo -instructions       # to print all CPU instructions available
./build/nesgo -rom path_to_file   # to specify which rom to load
./build/nesgo 			  # to load TOOLY's rom
```
  

## TOOLY

A sample NES game about a naughty cat chasing a clueless Samsara.

## NES References

* [6502 Instruction Set](https://www.masswerk.at/6502/6502_instruction_set.html)
* [Assembly directives](https://github.com/freem/asm6f/blob/master/readme-original.txt)
* [APU](https://wiki.nesdev.com/w/index.php/APU#Pulse_.28.244000-4007.29)
* [APU DMC](https://wiki.nesdev.com/w/index.php/APU_DMC)
* [iNES](https://wiki.nesdev.com/w/index.php/INES)
* [PPU frame timing](https://wiki.nesdev.com/w/index.php/PPU_frame_timing)
* [PPU memory map](https://wiki.nesdev.com/w/index.php/PPU_memory_map)
* [PPU OAM](https://wiki.nesdev.com/w/index.php/PPU_OAM)
* [PPU palettes](https://wiki.nesdev.com/w/index.php/PPU_palettes)
* [PPU programmer reference](https://wiki.nesdev.com/w/index.php/PPU_programmer_reference)
* [PPU registers](https://wiki.nesdev.com/w/index.php/PPU_registers)
* [PPU rendering](https://wiki.nesdev.com/w/index.php/PPU_rendering)
* [PPU scrolling](https://wiki.nesdev.com/w/index.php/PPU_scrolling)
* [Sample RAM map](https://wiki.nesdev.com/w/index.php/Sample_RAM_map)
* [Sound](https://safiire.github.io/blog/2015/03/29/creating-sound-on-the-nes/)
* [Stack](https://wiki.nesdev.com/w/index.php/Stack)
