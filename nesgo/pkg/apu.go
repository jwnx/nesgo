package nesgo

import (
	"math/rand"

	"github.com/gordonklaus/portaudio"
)

// Duty 	Output waveform
// 0	 	0 1 0 0 0 0 0 0 (12.5%)
// 1	 	0 1 1 0 0 0 0 0 (25%)
// 2	 	0 1 1 1 1 0 0 0 (50%)
// 3	 	1 0 0 1 1 1 1 1 (25% negated)
var dutyCycleSequence = [][]byte{
	{0, 1, 0, 0, 0, 0, 0, 0},
	{0, 1, 1, 0, 0, 0, 0, 0},
	{0, 1, 1, 1, 1, 0, 0, 0},
	{1, 0, 0, 1, 1, 1, 1, 1},
}

// The sequencer sends the following looping 32-step sequence of values to the mixer:
var triangleTable = []byte{
	15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0,
	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
}

// Pulse DD--.---- - Duty cycle,
type pulseSequencer struct {
	duty byte
	pos  byte
}

// Since the period of the timer is t+1 APU cycles and the sequencer
// has 8 steps, the period of the waveform is 8*(t+1) APU cycles,
// or equivalently 16*(t+1) CPU cycles
func (sequencer *pulseSequencer) Step() byte {
	sequencer.pos = (sequencer.pos + 1) % 8
	return dutyCycleSequence[sequencer.duty][sequencer.pos]
}

// The duty cycle is changed (see table below), but the
// sequencer's current position isn't affected.
// https://wiki.nesdev.com/w/index.php/APU_Pulse
func (sequencer *pulseSequencer) write(value byte) {
	sequencer.duty = ((value >> 6) & 3)
}

// Pulse ---c.vvvv - constant volume/envelope flag,
// 					 and volume/envelope divider period
type pulseEnvelope struct {
	start          bool
	loop           bool
	constantVolume bool
	volume         byte
}

// The duty cycle is changed (see table below), but the
// sequencer's current position isn't affected.
// https://wiki.nesdev.com/w/index.php/APU_Pulse
func (envelope *pulseEnvelope) write(value byte) {
	envelope.volume = (value & 15)
	envelope.constantVolume = ((value >> 4) & 1) == 1
	envelope.loop = ((value >> 5) & 1) == 1
}

//			EPPP.NSSS
// bit 7 	E--- ---- 	Enabled flag
// bits 6-4 -PPP ---- 	The divider's period is P + 1 half-frames
// bit 3 	---- N--- 	Negate flag
// 						0: add to period, sweeping toward lower frequencies
// 						1: subtract from period, sweeping toward higher frequencies
// bits 2-0 ---- -SSS 	Shift count (number of bits)
// Side effects 		Sets the reload flag
type pulseSweep struct {
	reload  bool
	enabled bool
	period  byte
	negate  bool
	shift   byte
}

func (sweep *pulseSweep) write(value byte) {
	*sweep = pulseSweep{
		shift:   (value & 3),
		negate:  ((value >> 3) & 1) == 1,
		period:  ((value >> 4) & 3),
		enabled: ((value >> 7) & 1) == 1,
		reload:  true,
	}
}

// $4002 / $4006 	TTTT TTTT 	Timer low (T)
// $4003 / $4007 	LLLL LTTT 	Length counter load (L), timer high (T)
type pulseTimer struct {
	t        float32
	lowByte  byte
	highByte byte
}

func (timer *pulseTimer) writeLowByte(value byte) {
	timer.lowByte = value
}

func (timer *pulseTimer) writeHighByte(value byte) {
	timer.highByte = (value & 15)
}

// Since the period of the timer is t+1 APU cycles and
// the sequencer has 8 steps, the period of the waveform
// is 8*(t+1) APU cycles, or equivalently 16*(t+1) CPU cycles
func (timer *pulseTimer) Update(fPulse float32) {
	timer.t = (CPUClockRate / (16 * fPulse)) - 1
}

// The length counter provides automatic duration control for
// the NES APU waveform channels. Once loaded with a value, it
// can optionally count down (when the length counter halt flag
// is clear). Once it reaches zero, the corresponding channel is
// silenced.
type pulseLengthCounter struct {
	halt    bool
	counter byte
}

func (lengthCounter *pulseLengthCounter) writeHalt(value byte) {
	lengthCounter.halt = (((value >> 5) & 1) == 1)
}

func (lengthCounter *pulseLengthCounter) writeCounter(value byte) {
	lengthCounter.counter = (value >> 3)
}

type pulseRegisters struct {
	enabled bool

	// Pulse DDlc.vvvv - Duty cycle,
	//					 length counter halt,
	//					 constant volume/envelope flag,
	// 					 and volume/envelope divider period
	lengthCounter pulseLengthCounter
	sequencer     pulseSequencer // envelope.write($4000/$4004)
	envelope      pulseEnvelope  // envelope.write($4000/$4004)
	sweep         pulseSweep     // sweep.write($4001/$4005)
	timer         pulseTimer     // timer.write($4002/$4006, $4003/$4007)
}

// The sequencer is clocked by an 11-bit timer. Given the timer
// value t = HHHLLLLLLLL formed by timer high and timer low, this
// timer is updated every APU cycle (i.e., every second CPU cycle),
// and counts t, t-1, ..., 0, t, t-1, ..., clocking the waveform
// generator when it goes from 0 to t. Since the period of the timer
// is t+1 APU cycles and the sequencer has 8 steps, the period of
// the waveform is 8*(t+1) APU cycles, or equivalently 16*(t+1) CPU cycles.
func (p *pulseRegisters) output() byte {
	if !p.enabled {
		return 0
	}
	if p.lengthCounter.counter == 0 {
		return 0
	}
	if dutyCycleSequence[p.sequencer.duty][p.sequencer.pos] == 0 {
		return 0
	}
	if p.timer.t < 8 || p.timer.t > 0x7FF {
		return 0
	}
	return p.envelope.volume
}

type triangleRegisters struct {
	enabled      bool
	timer        float32
	lowByte      byte
	highByte     byte
	lengthValue  byte
	counterValue byte
}

func (triangle *triangleRegisters) writeLowByte(value byte) {
	triangle.lowByte = value
}

func (triangle *triangleRegisters) writeHighByte(value byte) {
	triangle.highByte = (value & 15)
}

func (triangle *triangleRegisters) output() byte {
	if !triangle.enabled {
		return 0
	}
	if triangle.lengthValue == 0 {
		return 0
	}
	if triangle.counterValue == 0 {
		return 0
	}
	return triangleTable[triangle.counterValue]
}

type APURegisters struct {
	Pulse    [2]pulseRegisters
	Triangle triangleRegisters
}

func (reg *APURegisters) Write(addr Address, value byte) {
	switch addr {
	case 0x4000:
		reg.Pulse[0].sequencer.write(value)
		reg.Pulse[0].envelope.write(value)
		reg.Pulse[0].lengthCounter.writeHalt(value)
	case 0x4001:
		reg.Pulse[0].sweep.write(value)
	case 0x4002:
		reg.Pulse[0].timer.writeLowByte(value)
	case 0x4003:
		reg.Pulse[0].timer.writeHighByte(value)
		reg.Pulse[0].lengthCounter.writeCounter(value)
	case 0x4004:
		reg.Pulse[1].sequencer.write(value)
		reg.Pulse[1].envelope.write(value)
	case 0x4005:
		reg.Pulse[1].sweep.write(value)
	case 0x4006:
		reg.Pulse[1].timer.writeLowByte(value)
	case 0x4007:
		reg.Pulse[1].timer.writeHighByte(value)
		reg.Pulse[1].lengthCounter.writeCounter(value)
	case 0x400A:
		reg.Triangle.writeLowByte(value)
	case 0x400B:
		reg.Triangle.writeHighByte(value)
	case 0x4015:
		reg.Pulse[0].enabled = ((value & 1) == 1)
		reg.Pulse[1].enabled = (((value >> 1) & 1) == 1)
		reg.Triangle.enabled = (((value >> 2) & 1) == 1)
	}
}

type Audio struct {
	stream         *portaudio.Stream
	channel        chan int32
	outputChannels int
	sampleRate     float64
}

func NewAudio() *Audio {
	a := Audio{}
	a.channel = make(chan int32, 44100)
	return &a
}

func (a *Audio) Start() error {
	host, err := portaudio.DefaultHostApi()
	if err != nil {
		return err
	}
	parameters := portaudio.HighLatencyParameters(nil, host.DefaultOutputDevice)
	stream, err := portaudio.OpenStream(parameters, a.Callback)
	if err != nil {
		return err
	}
	if err := stream.Start(); err != nil {
		return err
	}
	a.stream = stream
	a.sampleRate = parameters.SampleRate
	a.outputChannels = parameters.Output.Channels
	return nil
}

func (a *Audio) Callback(out []int32) {
	var output int32
	for i := range out {
		select {
		case sample := <-a.channel:
			output = sample
		default:
			output = 0
		}
		out[i] = output
	}
}

func (a *Audio) Stop() error {
	return a.stream.Close()
}

type APU struct {
	APURegisters

	channel chan int32
	Cycles  uint
	Audio   *Audio
}

func NewAPU(audio *Audio) *APU {
	apu := APU{Audio: audio}
	apu.Reset()
	return &apu

}

func (apu *APU) sendSample() {
	output := apu.output()
	select {
	case apu.Audio.channel <- output:
	default:
	}
}

// The NES APU mixer takes the channel outputs and converts them to
// an analog audio signal. Each channel has its own internal digital-to-analog
// convertor (DAC), implemented in a way that causes non-linearity and
// interaction between channels, so calculation of the resulting amplitude
// is somewhat involved.
// The following formula calculates the audio output level within the
// range of 0.0 to 1.0. It is the sum of two sub-groupings of the channels:
// https://wiki.nesdev.com/w/index.php/APU_Mixer
func (apu *APU) output() int32 {
	//pulse1 := apu.Pulse[0].output()
	//pulse2 := apu.Pulse[1].output()
	//pulseOut := 0.00752 * (pulse1 + pulse2)
	//return int32(pulseOut)
	return int32(rand.Uint32())
}

func (apu *APU) Reset() {
	apu.Pulse[0].enabled = false
	apu.Pulse[1].enabled = false
	apu.Triangle.enabled = false
}

func (apu *APU) Step() {
	if apu.Pulse[0].enabled || apu.Pulse[1].enabled || apu.Triangle.enabled {
		apu.sendSample()
	}
}
