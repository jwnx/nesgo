;----------------------------------------------------------------
; Constants
;----------------------------------------------------------------

  PPUCTRL        = $2000
  PPUMASK        = $2001
  PPUSTATUS      = $2002
  OAMADDR        = $2003
  PPUSCROLL      = $2005
  PPUADDR        = $2006
  PPUDATA        = $2007
  OAMDMA         = $4014

  SQUARE1        = $4000
  TRIANGLE       = $4008
  DMC            = $4010
  APUSTATUS      = $4015
  FRAMECOUNTER   = $4017

  CTRL1          = $4016

  TIMESECONDS1   = $0221
  TIMESECONDS2   = $0225
  TIMEMINUTES1   = $022D
  TIMEMINUTES2   = $0231

  HEALTHBASE1    = $0234
  HEALTHBASE2    = $025C
  MAXHEALTH      = $0A

  STATETITLE     = $00  ; Displaying title screen
  STATEPLAYING   = $01  ; Move player and tooly
  STATEGAMEOVER  = $02  ; Displaying game over screen

;----------------------------------------------------------------
; Variables
;----------------------------------------------------------------

  .enum $0000

  ; A pointer
  pointer         .dsw 1

  ; For function LoadToPPU
  datasize        .dsb 1

  ; Tooly state
  turn            .dsb 1
  toolyx          .dsb 1
  toolyy          .dsb 1

  ; Player state
  playerx         .dsb 1
  playery         .dsb 1
  health          .dsb 1
  relieve         .dsb 1
  stuffed         .dsb 1

  ; For function CheckCollision
  coordx          .dsb 1
  coordy          .dsb 1

  ; Time
  ticks           .dsb 1

  gamestate       .dsb 1

  buttons         .dsb 1

  .ende

;----------------------------------------------------------------
; Macros
;----------------------------------------------------------------

  MACRO setPointer addr
  LDA #<addr            ; Get low byte of Address
  STA pointer           ; Store in pointer
  LDA #>addr            ; Get high byte of Address
  STA pointer+1         ; Store in pointer+1
  ENDM

  MACRO setPpuAddr addr
  LDA #>addr            ; Get high byte of Address
  STA PPUADDR
  LDA #<addr            ; Get low byte of Address
  STA PPUADDR
  ENDM

;----------------------------------------------------------------
; iNES header
;----------------------------------------------------------------

	.inesprg 1            ; 1x 16KB PRG code
	.ineschr 1            ; 1x  8KB CHR data
	.inesmap 0            ; Mapper 0 = NROM, no bank swapping
	.inesmir 1            ; Background mirroring

;----------------------------------------------------------------
; PGR
;----------------------------------------------------------------

  .org $C000

vblankwait:             ; First wait for vblank to make sure PPU is ready
  BIT PPUSTATUS
  BPL vblankwait
  RTS

LoadToPPU:
  LDY #$00              ; Start out at 0
LoadToPPULoop:
  LDA (pointer), y      ; Load data from address
  STA PPUDATA           ; Write to PPU
  INY
  DEC datasize
  BNE LoadToPPULoop
  RTS

Reset:
  SEI                   ; Disable IRQs
  CLD                   ; Disable decimal mode
  JSR SoundInit
  LDX #$FF
  TXS                   ; Set up stack
  INX                   ; Now X = 0
  STX PPUCTRL           ; Disable NMI
  STX PPUMASK           ; Disable rendering
  JSR vblankwait

clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0200, x
  INX
  BNE clrmem
  JSR vblankwait

LoadPalettes:
  LDA PPUSTATUS         ; Read PPU status to reset the high/low latch
  setPpuAddr #$3F00
  setPointer palette
  LDA #$20
  STA datasize
  JSR LoadToPPU

  ; $0200-$02FF	contains 256 bytes to be copied to OAM during next vertical blank
LoadSprites:
  LDX #$00              ; Start at 0
LoadSpritesLoop:
  LDA sprites, x        ; Load data from address (sprites +  x)
  STA $0200, x          ; Store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$84
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero

LoadBackground:
  LDA PPUSTATUS         ; Read PPU status to reset the high/low latch
  setPpuAddr #$2000
  setPointer background ; Low byte is 00
  LDA #$00
  STA datasize
  LDX #$00
LoadBackgroundLoop:
  JSR LoadToPPU
  INC pointer+1         ; Low byte went 0 to 256, so high byte needs to be changed now
  INX
  CPX #$04
  BNE LoadBackgroundLoop

; Already written by the previous loop
;LoadAttribute:
;  LDA PPUSTATUS         ; Read PPU status to reset the high/low latch
;  setPpuAddr #$23C0
;  setPointer attribute
;  LDA #$3C
;  STA datasize
;  JSR LoadToPPU

  ; Set initial positions
  LDA #$20
  STA toolyy
  LDA #$20
  STA toolyx

  LDA #$40
  STA playery
  LDA #$80
  STA playerx

  ; Set initial health value
  LDA #MAXHEALTH
  STA health

  ; Set initial time value
  LDA #$00
  STA ticks

  ; Set starting game state
  LDA #STATETITLE
  STA gamestate

  LDA #%10010000        ; Enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA PPUCTRL

  LDA #%00011000        ; Enable sprites, enable background, clipping on left side
  STA PPUMASK

Forever:
  JMP Forever           ; Initialization is done, loop forever

NMI:
  ; Sprites have been loaded into $0200
  LDA #$00
  STA OAMADDR           ; Set the low byte (00) of the RAM address
  LDA #$02
  STA OAMDMA            ; Set the high byte (02) of the RAM address, start the transfer

  ; This is the PPU clean up section, so rendering the next frame starts properly.
  LDA #%10000000        ; Enable NMI, background and sprites from Pattern Table 0
  STA PPUCTRL
  LDA #%00011000        ; Enable sprites, enable background, clipping on left side
  STA PPUMASK
  LDA #$00              ; Tell the ppu there is no background scrolling
  STA PPUSCROLL
  STA PPUSCROLL

  JSR SoundDisable

  ; All graphics updates done by here, run game engine
  JSR ReadController     ; Get the current button data

GameEngine:
  LDA gamestate
  CMP #STATETITLE
  BEQ EngineTitle

  LDA gamestate
  CMP #STATEGAMEOVER
  BEQ EngineGameOver

  LDA gamestate
  CMP #STATEPLAYING
  BEQ EnginePlaying

GameEngineDone:
  JSR UpdateSprites     ; Set sprites from positions
  RTI

EngineTitle:
  LDA buttons
  CMP #%00010000
  BNE EngineTitle
  LDA #STATEPLAYING
  STA gamestate
  JMP GameEngineDone

EngineGameOver:
  JMP GameEngineDone

EnginePlaying:

MoveTooly:
  LDA TIMEMINUTES1
  CMP #$F0
  BNE MoveToolyX
  LDA #$01
  EOR turn
  STA turn
  BEQ MoveToolyDone

MoveToolyX:
  LDA playerx
  CMP toolyx
  BEQ MoveToolyY
  JSR LoadToolyCoords
  BCC MoveToolyLeft
MoveToolyRight:
  INC coordx
  JSR CheckCollision
  CMP #$01
  BEQ MoveToolyY
  INC toolyx
  JMP MoveToolyY
MoveToolyLeft:
  DEC coordx
  JSR CheckCollision
  CMP #$01
  BEQ MoveToolyY
  DEC toolyx

MoveToolyY:
  LDA playery
  CMP toolyy
  BEQ MoveToolyDone
  JSR LoadToolyCoords
  BCC MoveToolyUp
MoveToolyDown:
  INC coordy
  JSR CheckCollision
  CMP #$01
  BEQ MoveToolyDone
  INC toolyy
  JMP MoveToolyDone
MoveToolyUp:
  DEC coordy
  JSR CheckCollision
  CMP #$01
  BEQ MoveToolyDone
  DEC toolyy
MoveToolyDone:

MovePlayerUp:
  LDA buttons
  AND #%00001000
  BEQ MovePlayerUpDone

  JSR LoadPlayerCoords
  DEC coordy
  JSR CheckCollision
  CMP #$01
  BEQ MovePlayerUpDone
  DEC playery
MovePlayerUpDone:

MovePlayerDown:
  LDA buttons
  AND #%00000100
  BEQ MovePlayerDownDone

  JSR LoadPlayerCoords
  INC coordy
  JSR CheckCollision
  CMP #$01
  BEQ MovePlayerDownDone
  INC playery
MovePlayerDownDone:

MovePlayerLeft:
  LDA buttons
  AND #%00000010
  BEQ MovePlayerLeftDone

  JSR LoadPlayerCoords
  DEC coordx
  JSR CheckCollision
  CMP #$01
  BEQ MovePlayerLeftDone
  DEC playerx
MovePlayerLeftDone:

MovePlayerRight:
  LDA buttons
  AND #%00000001
  BEQ MovePlayerRightDone

  JSR LoadPlayerCoords
  INC coordx
  JSR CheckCollision
  CMP #$01
  BEQ MovePlayerRightDone
  INC playerx
MovePlayerRightDone:

  JSR CheckHealthUp
  JSR CheckHealthDown
  JSR IncrementTime

  JMP GameEngineDone

CheckHealthDown:
  LDA playerx
  CMP toolyx
  BNE CheckHealthDownDone
  LDA playery
  CMP toolyy
  BNE CheckHealthDownDone
  LDA relieve
  BEQ Bite
  DEC relieve
  JMP CheckHealthDownDone
Bite:
  JSR SoundPlayBite
  LDA #$10
  STA relieve
  DEC health
  LDA health
  ASL A
  ASL A
  TAX
  LDA #$FF
  STA HEALTHBASE1, x
  STA HEALTHBASE2, x
  LDA health
  BNE CheckHealthDownDone
  LDA #STATEGAMEOVER
  STA gamestate
CheckHealthDownDone:
  RTS

CheckHealthUp:
  LDA stuffed
  BEQ Recover
  DEC stuffed
  JMP CheckHealthUpDone
Recover:
  LDA health
  CMP #MAXHEALTH
  BEQ CheckHealthUpDone
  setPointer waterandfood
  JSR LoadPlayerCoords
  JSR CheckOverlap
  CMP #$01
  BNE CheckHealthUpDone
  JSR SoundPlayRecover
  LDA #$10
  STA stuffed
  LDA health
  ASL A
  ASL A
  TAX
  LDA #$CF
  STA HEALTHBASE1, x
  LDA #$D7
  STA HEALTHBASE2, x
  INC health
CheckHealthUpDone:
  RTS

  ; Convert pixels to blocks
NormalizeCoords:
  LSR coordx
  LSR coordx
  LSR coordx
  LSR coordy
  LSR coordy
  LSR coordy
  RTS

LoadPlayerCoords:
  LDA playerx
  STA coordx
  LDA playery
  STA coordy
  RTS

LoadToolyCoords:
  LDA toolyx
  STA coordx
  LDA toolyy
  STA coordy
  RTS

CheckCollision:
  setPointer collision
  JMP CheckOverlap

  ; Takes in (coordx,coordy), returns Z = 1 if there's a collision
CheckOverlap:
  JSR NormalizeCoords
  LDX #$00
  LDY #$00
CheckCollisionLoop:
  LDA (pointer), y
  INY
  CMP coordy
  BNE Continue
  LDA (pointer), y
  CMP coordx
  BNE Continue
  LDA #$01
  RTS
Continue:
  INY
  BNE CheckCollisionLoop
  INC pointer+1
  INX
  CPX #$02
  BNE CheckCollisionLoop
  LDA #$00
  RTS

UpdateSprites:
  ; Player
  LDA playery
  STA $0200
  STA $0204
  ADC #$07
  STA $0208
  STA $020C
  LDA playerx
  STA $0203
  STA $020B
  ADC #$08
  STA $0207
  STA $020F

  ; Tooly
  LDA toolyy
  STA $0210
  STA $0214
  ADC #$08
  STA $0218
  STA $021C
  LDA toolyx
  STA $0213
  STA $021B
  ADC #$08
  STA $0217
  STA $021F

  RTS

IncrementTime:
  INC ticks
  LDA #$1F
  AND ticks
  BNE IncDone
  INC TIMESECONDS1
  LDA TIMESECONDS1
  CMP #$FA              ; Check if it overflowed, now equals 10
  BNE IncDone
  LDA #$F0
  STA TIMESECONDS1      ; Wrap digit to 0
  INC TIMESECONDS2
  LDA TIMESECONDS2
  CMP #$F6              ; Check if it overflowed, now equals 6
  BNE IncDone
  LDA #$F0
  STA TIMESECONDS2      ; Wrap digit to 0
  INC TIMEMINUTES1
  LDA TIMEMINUTES1
  CMP #$FA              ; Check if it overflowed, now equals 10
  BNE IncDone
  LDA #$F0
  STA TIMEMINUTES1      ; Wrap digit to 0
  INC TIMEMINUTES2
  LDA TIMESECONDS2
  CMP #$F6              ; Check if it overflowed, now equals 6
  BNE IncDone
  LDA #$F0
  STA TIMEMINUTES2      ; Wrap digit to 0
IncDone:
  RTS

ReadController:
  LDA #$01
  STA CTRL1
  LDA #$00
  STA CTRL1
  LDX #$08
ReadControllerLoop:
  LDA CTRL1
  LSR A                 ; Bit 0 -> Carry
  ROL buttons           ; Carry -> bit 0; bit 7 -> Carry
  DEX
  BNE ReadControllerLoop
  RTS

;----------------------------------------------------------------
; Sound
;----------------------------------------------------------------

SoundInit:
  JSR SoundDisable
  STX DMC               ; Disable DMC IRQs
  STX APUSTATUS         ; Disable Square 1, Square 2, Triangle and Noise channels
  LDX #$40
  STX FRAMECOUNTER      ; Disable APU frame IRQ
  RTS

SoundDisable:
  LDX #$00
  STX APUSTATUS         ; Disable Square 1, Square 2, Triangle and Noise channels
  RTS

SoundPlayBite:
  LDA #%00000001        ; Enable Square 1
  STA APUSTATUS
  LDA #%10111111        ; Duty 10, Volume F
  STA SQUARE1
  LDA #$C9              ; 0C9 is a C# in NTSC mode
  STA SQUARE1+2
  LDA #$00
  STA SQUARE1+3
  RTS

SoundPlayRecover:
  LDA #%00000100        ; Enable Triangle
  STA APUSTATUS
  LDA #%10000001        ; Reload counter value
  STA TRIANGLE
  LDA #$42              ; 042 plays a G# in NTSC mode
  STA TRIANGLE+2
  LDA #$00
  STA TRIANGLE+3
  RTS

;----------------------------------------------------------------
; Data
;----------------------------------------------------------------

.org $E000

palette:
  ; Background
  .db $3b,$37,$27,$00, $3b,$01,$15,$1c, $3b,$0c,$19,$31, $3b,$0c,$19,$1c
  ; Sprites
  .db $3b,$1a,$00,$3d, $3b,$19,$30,$1d, $3b,$20,$15,$01, $3b,$2e,$30,$3f

sprites:
    ; vert tile attr horiz ; Player
  .db $00, $C9, $01, $00   ; sprite 0
  .db $00, $CA, $01, $00   ; sprite 1
  .db $00, $D9, $01, $00   ; sprite 2
  .db $00, $DA, $01, $00   ; sprite 3

    ; vert tile attr horiz ; Tooly
  .db $00, $11, $00, $00   ; sprite 0
  .db $00, $12, $00, $00   ; sprite 1
  .db $00, $21, $00, $00   ; sprite 2
  .db $00, $22, $00, $00   ; sprite 3

    ; vert tile attr horiz ; Clock
  .db $C8, $f0, $00, $E9   ; Seconds 1
  .db $C8, $f0, $00, $E1   ; Seconds 2
  .db $C8, $fa, $00, $D9   ; :
  .db $C8, $f0, $00, $D1   ; Minutes 1
  .db $C8, $f0, $00, $C9   ; Minutes 2

    ; vert tile attr horiz ; Health
  .db $CF, $e5, $02, $28
  .db $CF, $e5, $02, $30
  .db $CF, $e5, $02, $38
  .db $CF, $e5, $02, $40
  .db $CF, $e5, $02, $48
  .db $CF, $e5, $02, $50
  .db $CF, $e5, $02, $58
  .db $CF, $e5, $02, $60
  .db $CF, $e5, $02, $68
  .db $CF, $e5, $02, $70

  .db $D7, $e5, $02, $28
  .db $D7, $e5, $02, $30
  .db $D7, $e5, $02, $38
  .db $D7, $e5, $02, $40
  .db $D7, $e5, $02, $48
  .db $D7, $e5, $02, $50
  .db $D7, $e5, $02, $58
  .db $D7, $e5, $02, $60
  .db $D7, $e5, $02, $68
  .db $D7, $e5, $02, $70

background:
  .db $0e,$3c,$2d,$28,$29,$3a,$a4,$2b,$0b,$2d,$2d,$1d,$28,$29,$3a,$08
  .db $09,$0a,$2d,$3d,$1d,$3c,$1d,$2d,$28,$29,$2a,$a4,$a4,$2b,$0b,$1d
  .db $0e,$3c,$2d,$28,$29,$3a,$a4,$2b,$0b,$2d,$0d,$1d,$28,$29,$3a,$08
  .db $09,$0a,$1d,$3d,$84,$3c,$1d,$2d,$28,$29,$2a,$a4,$a4,$2b,$0b,$1d
  .db $0e,$3c,$2d,$28,$29,$3a,$a4,$2b,$0b,$2d,$2d,$2d,$28,$29,$3a,$08
  .db $09,$0a,$1d,$3d,$94,$3c,$1d,$2d,$28,$29,$2a,$a4,$a4,$2b,$0b,$1d
  .db $0e,$3c,$2d,$38,$39,$3a,$a4,$3b,$1b,$2d,$2d,$1d,$38,$39,$3a,$18
  .db $19,$1a,$1d,$3d,$1d,$3c,$1d,$2d,$38,$39,$3a,$a4,$a4,$3b,$1b,$1d
  .db $0e,$3c,$40,$41,$42,$43,$42,$43,$44,$45,$40,$43,$42,$43,$44,$41
  .db $46,$47,$44,$3d,$0e,$3c,$40,$41,$42,$43,$44,$43,$40,$41,$42,$43
  .db $0e,$3c,$50,$51,$52,$53,$54,$55,$50,$51,$52,$53,$54,$55,$50,$51
  .db $56,$57,$54,$3d,$0e,$3c,$50,$51,$52,$53,$54,$55,$80,$81,$82,$53
  .db $0e,$3c,$60,$61,$62,$63,$64,$65,$66,$67,$60,$61,$62,$63,$64,$65
  .db $66,$67,$60,$3d,$0e,$3c,$60,$61,$62,$63,$64,$65,$90,$91,$92,$61
  .db $04,$04,$04,$05,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
  .db $af,$04,$04,$04,$04,$04,$04,$04,$04,$04,$05,$05,$05,$98,$98,$98
  .db $16,$16,$af,$16,$16,$16,$16,$a8,$16,$16,$16,$a8,$25,$16,$16,$af
  .db $98,$98,$98,$98,$98,$a8,$98,$98,$98,$a8,$98,$98,$98,$98,$98,$98
  .db $16,$16,$16,$af,$25,$16,$16,$16,$34,$35,$16,$16,$16,$16,$16,$16
  .db $98,$48,$49,$4a,$4b,$4c,$4d,$4e,$4e,$4f,$98,$98,$98,$98,$98,$98
  .db $16,$16,$16,$a8,$16,$16,$16,$16,$16,$16,$16,$25,$16,$16,$16,$16
  .db $98,$58,$59,$5a,$5b,$5c,$5d,$5e,$5e,$5f,$98,$98,$98,$26,$98,$98
  .db $a8,$16,$16,$16,$16,$16,$36,$37,$26,$16,$a8,$16,$a8,$af,$16,$16
  .db $98,$68,$69,$6a,$6b,$6c,$6d,$6e,$6e,$6f,$98,$98,$a8,$a8,$98,$98
  .db $16,$16,$16,$af,$16,$16,$16,$16,$16,$16,$16,$16,$16,$16,$25,$16
  .db $98,$78,$6b,$79,$7a,$7b,$7c,$7d,$7e,$7f,$98,$98,$98,$af,$98,$98
  .db $16,$16,$16,$16,$16,$16,$16,$34,$35,$16,$16,$16,$25,$16,$98,$98
  .db $98,$88,$89,$8a,$8a,$8b,$8c,$8d,$8e,$8f,$98,$98,$98,$98,$98,$98
  .db $16,$16,$24,$af,$16,$16,$16,$16,$16,$25,$16,$16,$16,$98,$af,$98
  .db $98,$98,$99,$9a,$9b,$9b,$9b,$9b,$9e,$98,$98,$98,$98,$98,$98,$98
  .db $98,$98,$98,$98,$98,$34,$35,$16,$98,$98,$af,$98,$98,$98,$98,$af
  .db $98,$98,$a9,$aa,$ab,$ac,$ad,$ad,$ae,$98,$98,$98,$98,$98,$a8,$98
  .db $a8,$98,$98,$98,$98,$16,$16,$16,$98,$98,$98,$98,$98,$98,$98,$98
  .db $98,$af,$af,$af,$af,$af,$af,$bd,$af,$98,$af,$98,$98,$98,$98,$98
  .db $98,$98,$16,$16,$16,$16,$16,$16,$24,$a8,$98,$98,$98,$98,$98,$98
  .db $98,$98,$98,$98,$98,$98,$98,$98,$98,$98,$98,$af,$98,$98,$98,$98
  .db $16,$af,$24,$25,$16,$34,$35,$16,$98,$98,$af,$98,$98,$98,$98,$98
  .db $98,$98,$98,$98,$98,$98,$98,$98,$25,$98,$98,$98,$98,$98,$98,$98
  .db $16,$98,$16,$98,$98,$16,$16,$16,$98,$98,$98,$98,$98,$98,$a8,$af
  .db $98,$98,$98,$98,$98,$98,$98,$98,$98,$98,$98,$98,$98,$98,$98,$98
  .db $98,$25,$98,$98,$98,$9f,$16,$98,$98,$98,$98,$98,$98,$98,$98,$98
  .db $98,$98,$98,$98,$98,$98,$98,$98,$98,$26,$98,$98,$98,$98,$98,$98
  .db $98,$98,$98,$16,$34,$35,$98,$98,$98,$98,$98,$98,$98,$98,$98,$98
  .db $24,$98,$98,$af,$98,$98,$a8,$af,$98,$98,$98,$25,$98,$98,$af,$98
  .db $a8,$98,$14,$98,$98,$98,$98,$98,$98,$98,$af,$98,$98,$98,$98,$98
  .db $98,$98,$af,$af,$98,$98,$98,$98,$98,$98,$98,$98,$98,$af,$98,$98
  .db $98,$af,$98,$98,$98,$98,$98,$98,$98,$98,$98,$98,$98,$98,$af,$98
  .db $98,$98,$98,$98,$98,$98,$98,$98,$98,$98,$26,$98,$98,$98,$98,$98
  .db $a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4
  .db $a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4
  .db $a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4
  .db $a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4
  .db $a4,$a4,$a4,$96,$a4,$e5,$e5,$e5,$e5,$e5,$e5,$e5,$e5,$e5,$e5,$a4
  .db $a5,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4
  .db $a4,$a4,$87,$97,$a4,$e5,$e5,$e5,$e5,$e5,$e5,$e5,$e5,$e5,$e5,$a4
  .db $a6,$a7,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4
  .db $a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4
  .db $a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4
  .db $a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4
  .db $a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4,$a4

attribute:
  .db %10001000, %10101010, %00100010, %10101010, %00100010, %00000000, %10101010, %00100010
  .db %01000100, %01010101, %01010101, %01010101, %00010001, %01000100, %01010101, %01010101
  .db %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111, %11111111

collision: ; (x,y) coordinates, from (0,0) to (1F,1D)
  ; Top
  .dw $0003,$0103,$0203,$0303,$0403,$0503,$0603,$0703,$0803,$0903,$0a03,$0b03,$0c03,$0d03,$0e03,$0f03
  .dw $1003,$1103,$1203,$1303,$1403,$1503,$1603,$1703,$1803,$1903,$1a03,$1b03,$1c03,$1d03,$1e03,$1f03
  ; Bottom
  .dw $0016,$0116,$0216,$0316,$0416,$0516,$0616,$0716,$0816,$0916,$0a16,$0b16,$0c16,$0d16,$0e16,$0f16
  .dw $1016,$1116,$1216,$1316,$1416,$1516,$1616,$1716,$1816,$1916,$1a16,$1b16,$1c16,$1d16,$1e16,$1f16
  ; Left
  .dw $0000,$0001,$0002,$0003,$0004,$0005,$0006,$0007,$0008,$0009,$000A,$000B,$000C,$000D,$000E,$000F
  .dw $0010,$0011,$0012,$0013,$0014,$0015,$0016,$0017,$0018,$0019,$001A,$001B,$001C,$001D
  ; Right
  .dw $1D00,$1D01,$1D02,$1D03,$1D04,$1D05,$1D06,$1D07,$1D08,$1D09,$1D0A,$1D0B,$1D0C,$1D0D,$1D0E,$1D0F
  .dw $1D10,$1D11,$1D12,$1D13,$1D14,$1D15,$1D16,$1D17,$1D18,$1D19,$1D1A,$1D1B,$1D1C,$1D1D

  ; Top tree
  .dw $1008,$1108,$1208,$1308,$1408,$1508,$1608,$1708
  ; Bottom tree
  .dw $100F,$110F,$120F,$130F,$140F,$150F,$160F,$170F
  ; Left tree
  .dw $1008,$1009,$100A,$100B,$100C,$100D,$100E,$100F
  ; Right tree
  .dw $1808,$1809,$180A,$180B,$180C,$180D,$180E,$180F
  ; Pad to become multiple of 256
  .dsb 228

waterandfood:
  .dw $0F04,$1004
  .dw $1C04,$1B04,$1C05,$1B05
  .dsb 250

;----------------------------------------------------------------
; Interrupt vectors
;----------------------------------------------------------------

IRQ:
  ; No IRQs

  .org $FFFA

  .dw NMI
  .dw Reset
  .dw IRQ

;----------------------------------------------------------------
; CHR-ROM
;----------------------------------------------------------------

  .incbin "tooly.chr"
