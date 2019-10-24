package nesgo

import (
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"os"
)

// Reference: https://wiki.nesdev.com/w/index.php/INES

// Mirroring mode:
// 0 = Horizontal mirroring (vertical arrangment)
// 1 = Vertical mirroring (horizontal arrangment)
type Mirroring byte

// Horizontal returns true for horizontal mirroring.
func (m Mirroring) Horizontal() bool {
	return m == 0
}

// Vertical returns true for vertical mirroring.
func (m Mirroring) Vertical() bool {
	return m == 1
}

// PRG ROM data
type PRG struct {
	data  []byte
	banks byte
}

func (prg *PRG) Read(addr Address) byte {
	offset := Address(addr % 0x4000)            // FFFC - 4000 = 3FFC (BFFC = C000 - 3)
	base := ((addr - 0x8000) / 0x4000) * 0x4000 //
	index := (Address(prg.banks-1) * base) + offset
	//	log.Printf("PRG.read(returning index %s) and data %X", index, prg.data[index])
	return prg.data[index]
}

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

// Cartridge holds the result of loading an iNES file, containing
// the PRG, CHR and mirroring mode.
type Cartridge struct {
	PRG  *PRG
	CHR  CHR
	Mode Mirroring
}

// LoadiNESFile reads an iNES file, returning the PRG and CHR sections on success.
func LoadiNESFile(path string) (*Cartridge, error) {
	file, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	header := iNESHeader{}
	if err := binary.Read(file, binary.LittleEndian, &header); err != nil {
		return nil, err
	}

	//log.Printf("Loaded header for %s: %+v", *path, header)

	if header.Magic != magic {
		return nil, errors.New("Invalid .nes file: missing magic number")
	}

	if (header.Flags1|header.Flags2)&mapperMask != 0 {
		return nil, errors.New("Wrong mapper type: only NROM is supported")
	}

	mode := Mirroring(header.Flags1 & 1)

	prg := &PRG{make([]byte, int(header.SizePRG)*16*1024), header.SizePRG}
	if _, err := io.ReadFull(file, prg.data); err != nil {
		return nil, fmt.Errorf("Unable to read PRG: %s", err)
	}

	chr := make(CHR, int(header.SizeCHR)*8*1024)
	if _, err := io.ReadFull(file, chr); err != nil {
		return nil, fmt.Errorf("Unable to read CHR: %s", err)
	}

	if header.SizeCHR == 0 {
		// Value 0 means the board uses CHR RAM
		chr = make(CHR, 8*1024)
	}

	return &Cartridge{prg, chr, mode}, nil
}
