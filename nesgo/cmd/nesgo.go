package main

import (
	"flag"

	"nesgo/gui"
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

	gui.Launch(romPath)
}
