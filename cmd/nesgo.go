package main

import (
	"flag"
	"log"
	nesgo "nesgo/pkg"
)

func main() {
	romPath := flag.String("rom", "TOOLY/tooly.nes", "path to rom file")
	flag.Parse()

	if _, _, err := nesgo.LoadiNESFile(romPath); err != nil {
		log.Printf("Failed to load %s: %s", *romPath, err)
		return
	}
}
