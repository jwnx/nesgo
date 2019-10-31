package nesgo

import "reflect"

// Buttons and their pressed state
type Buttons struct {
	ButtonA      bool
	ButtonB      bool
	ButtonSelect bool
	ButtonStart  bool
	ButtonUp     bool
	ButtonDown   bool
	ButtonLeft   bool
	ButtonRight  bool
}

// Controller represents a NES controller
type Controller struct {
	btns   Buttons
	index  byte
	strobe bool
}

// NewController instantiates a new NES controller
func NewController() *Controller {
	return &Controller{}
}

// Press marks the specified buttons as pressed
func (c *Controller) Press(btns Buttons) {
	c.btns = btns
}

// Read returns whether the next button in the sequence is pressed
func (c *Controller) Read() byte {
	value := byte(0)
	if c.index < 8 && reflect.ValueOf(c.btns).Field(int(c.index)).Bool() {
		value = 1
	}
	c.index++
	if c.strobe {
		c.index = 0
	}
	return value
}

// Write set the strobe bit from the specified value
func (c *Controller) Write(value byte) {
	c.strobe = value&1 == 1
	if c.strobe {
		c.index = 0
	}
}
