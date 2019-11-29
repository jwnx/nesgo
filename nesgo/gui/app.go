package gui

import (
	"bytes"
	"image"
	"image/png"
	"math"
	"os"
	"path/filepath"
	"reflect"

	"github.com/leaanthony/mewn"
	"github.com/wailsapp/wails"
	"github.com/gordonklaus/portaudio"

	nesgo "nesgo/pkg"
)

// GUI is the struct bound to the Javascript engine
type GUI struct {
	runtime *wails.Runtime
	log     *wails.CustomLogger
	romPath string
	nes     *nesgo.NES
	keys    chan int
	stop    chan struct{}
}

// WailsInit initializes the struct with the Wails runtime
func (g *GUI) WailsInit(r *wails.Runtime) error {
	g.runtime = r
	g.log = r.Log.New("nesgo")
	return nil
}

// SelectedRom returns the currently selected ROM for display
func (g *GUI) SelectedRom() string {
	return filepath.Base(g.romPath)
}

// OpenRom selects a file from the filesystem to use as a ROM
func (g *GUI) OpenRom() string {
	selectedFile := g.runtime.Dialog.SelectFile()
	if rom, err := os.Open(selectedFile); err == nil {
		g.romPath = rom.Name()
	}
	return g.SelectedRom()
}

// Start runs the game, if the selected ROM is a valid iNES file
func (g *GUI) Start() error {
	cartridge, err := nesgo.LoadiNESFile(g.romPath)
	if err != nil {
		return err
	}

	portaudio.Initialize()
	//defer portaudio.Terminate()

	audio := nesgo.NewAudio()
	audio.Start()
	//defer audio.Stop()

	g.nes = nesgo.NewNES(cartridge, audio)
	go func() {
		clk := nesgo.Clock{}
		for {
			btns := nesgo.Buttons{}
		Keys:
			for {
				select {
				case b := <-g.keys:
					reflect.Indirect(reflect.ValueOf(&btns)).Field(b).SetBool(true)
				case <-g.stop:
					return
				default:
					break Keys
				}
			}
			g.nes.Press(btns)
			g.nes.StepFrame()
			var img *image.RGBA = g.nes.Buffer()
			buf := new(bytes.Buffer)
			if err := png.Encode(buf, img); err != nil {
				g.log.Panic(err.Error())
			}
			g.runtime.Events.Emit("new_frame", buf.Bytes())
			clk.SleepToEndOfFrame()
		}
	}()
	return nil
}

// Press a button of the controller
func (g *GUI) Press(key int) {
	g.keys <- key
}

// Stop the current game
func (g *GUI) Stop() {
	g.stop <- struct{}{}
}

// Launch starts the Nesgo GUI
func Launch(romPath *string) {
	js := mewn.String("./nesgo/gui/frontend/build/static/js/main.js")
	app := wails.CreateApp(&wails.AppConfig{
		Width:     512,
		Height:    480,
		Title:     "nesgo",
		JS:        js,
		Colour:    "#A3AFAF",
		Resizable: true,
	})
	app.Bind(&GUI{
		romPath: *romPath,
		stop:    make(chan struct{}),
		keys:    make(chan int, math.MaxInt16)})
	app.Run()
}
