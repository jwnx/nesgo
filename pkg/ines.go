package nesgo

import (
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"log"
	"os"
)

// Reference: https://wiki.nesdev.com/w/index.php/INES

// PRG ROM data
type PRG = []byte

// CHR ROM or RAM data
type CHR = []byte

const magic = 0x1a53454e

type iNESHeader struct {
	Magic   uint32  // Constant
	SizePRG byte    // Size of PRG ROM in 16 KB units
	SizeCHR byte    // Size of CHR ROM in 8 KB units
	Flags1  byte    // Upper nybble contains lower nybble of mapper number
	Flags2  byte    // Upper nybble contains upper nybble of mapper number
	_       [8]byte // Unused flags
}

const mapperMask = 0xf0

// LoadiNESFile reads an iNES file, returning the PRG and CHR sections on success.
func LoadiNESFile(path *string) (PRG, CHR, error) {
	file, err := os.Open(*path)
	if err != nil {
		return nil, nil, err
	}
	defer file.Close()

	header := iNESHeader{}
	if err := binary.Read(file, binary.LittleEndian, &header); err != nil {
		return nil, nil, err
	}

	log.Printf("Loaded header for %s: %+v", *path, header)

	if header.Magic != magic {
		return nil, nil, errors.New("Invalid .nes file: missing magic number")
	}

	if (header.Flags1|header.Flags2)&mapperMask != 0 {
		return nil, nil, errors.New("Wrong mapper type: only NROM is supported")
	}

	prg := make(PRG, int(header.SizePRG)*16*1024)
	if _, err := io.ReadFull(file, prg); err != nil {
		return nil, nil, fmt.Errorf("Unable to read PRG: %s", err)
	}

	chr := make(CHR, int(header.SizeCHR)*8*1024)
	if _, err := io.ReadFull(file, chr); err != nil {
		return nil, nil, fmt.Errorf("Unable to read CHR: %s", err)
	}

	if header.SizeCHR == 0 {
		// Value 0 means the board uses CHR RAM
		chr = make(CHR, 8*1024)
	}

	log.Printf("Successfully loaded ROM %s", *path)

	return prg, chr, nil
}
