package nesgo

import (
	"math"
	"time"
)

// Clock rates
const (
	MasterClockRate = 21477272.0
	CPUClockRate    = MasterClockRate / 12.0           // ~1.79 MHz
	PPUClockRate    = MasterClockRate / 4.0            // ~5.37 MHz
	FPS             = PPUClockRate / (341*261 + 340.5) // ~60.1 FPS
	FPNS            = 1e9 / FPS
)

// Clock manages realtime and emulation time
type Clock struct {
	ts time.Time
}

// NewClock returns a new Clock
func NewClock() Clock {
	return Clock{time.Now()}
}

// SleepToEndOfFrame for clock synchronization
func (clock *Clock) SleepToEndOfFrame() {
	deadline := clock.ts.Add(time.Duration(math.Round(FPNS)))
	time.Sleep(deadline.Sub(clock.ts))
	clock.ts = deadline
}
