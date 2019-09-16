package main

import (
	"flag"
	"log"
	nesgo "nesgo/pkg"
)

func main() {
	romPath := flag.String("rom", "TOOLY/tooly.nes", "path to rom file")
	printInstructions := flag.Bool("instructions", false, "displays all of the supported instructions")
	flag.Parse()

	if *printInstructions {
		nesgo.PrintInstructions()
		return
	}

	if ines, err := nesgo.LoadiNESFile(romPath); err == nil {
		cpu := nesgo.NewCPU(ines.PRG)
		cpu.Reset()

		for {
			brk, _ := cpu.Step()

			if brk {
				break
			}
		}
	} else {
		log.Printf("Failed to load %s: %s", *romPath, err)
	}
}
