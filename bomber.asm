  processor 6502

;--------------------------------------------------------
; Include files
;--------------------------------------------------------

  include "vcs.h"
  include "macro.h"


;--------------------------------------------------------
; Define RAM
;--------------------------------------------------------

  seg.u variables
  org $80

; sprite positions
JetXPos         ds 1
JetYPos         ds 1
BomberXPos      ds 1
BomberYPos      ds 1

; sprite graphics
JetSpritePtr    ds 2
JetColorPtr     ds 2
BomberSpritePtr ds 2
BomberColorPtr  ds 2

; score/timer
Score ds 1
Timer ds 1
ScoreSprite ds 1
TimerSprite ds 1
OnesDigitOffset ds 2
TensDigitOffset ds 2
DigitHelperByte ds 1

; misc
Random          ds 1

;--------------------------------------------------------
; Start of ROM
;--------------------------------------------------------

  seg rom
  org $F000

Start:
  CLEAN_START

;--------------------------------------------------------
; Initialize registers/vars/ptrs/constants
;--------------------------------------------------------

  lda #10                     ; set jet Y position
  sta JetYPos

  lda #60                     ; set jet X position
  sta JetXPos

  lda #83                     ; set bomber Y position
  sta BomberYPos

  lda #80                     ; set bomber X position
  sta BomberXPos

  lda #$84                    ; set background color
  sta COLUBK

  lda #$C2                    ; set playfield color
  sta COLUPF

  lda #$00                    ; set plafield blocks
  sta PF2

  lda #<JetSprite             ; store jet sprite address
  sta JetSpritePtr
  lda #>JetSprite
  sta JetSpritePtr+1

  lda #<JetColor              ; store jet sprite address
  sta JetColorPtr
  lda #>JetColor
  sta JetColorPtr+1

  lda #<BomberSprite          ; store jet sprite address
  sta BomberSpritePtr
  lda #>BomberSprite
  sta BomberSpritePtr+1

  lda #<BomberColor           ; store jet sprite address
  sta BomberColorPtr
  lda #>BomberColor
  sta BomberColorPtr+1

  lda #$05                    ; set double-wide size for bomber
  sta NUSIZ1

  lda #$D4                    ; set random seed value
  sta Random

  ; 16
  lda #5  ; 1 * NUMBER_HEIGHT
  sta TensDigitOffset

  lda #30 ; 6 * NUMBER_HEIGHT
  sta OnesDigitOffset

  lda #20 ; 4 * NUMBER_HEIGHT
  sta TensDigitOffset+1

  lda #45 ; 9 * NUMBER_HEIGHT
  sta OnesDigitOffset+1

JET_HEIGHT = 9
BOMBER_HEIGHT = 9
NUMBER_HEIGHT = 5

;--------------------------------------------------------
; Start next frame
;--------------------------------------------------------

NextFrame:
  lda #$02
  sta VBLANK        ; turn on VBLANK
  sta VSYNC         ; turn on VSYNC

;--------------------------------------------------------
; VSYNC
;--------------------------------------------------------

  sta WSYNC
  sta WSYNC
  sta WSYNC

  lda #$00
  sta VSYNC         ; turn off VSYNC

;--------------------------------------------------------
; Housekeeping (in VBLANK)
;--------------------------------------------------------

  lda JetXPos       ; set jet horizontal position
  ldx #0
  jsr SetObjectXPos

  lda BomberXPos       ; set bomber horizontal position
  ldx #1
  jsr SetObjectXPos

  sta WSYNC         ; 'carriage return'
  sta HMOVE         ; perform fine-tune offset

  ; reset graphics, playfield, playfield reflection
  lda #0
  sta GRP0                    ; disable sprites
  sta GRP1
  sta PF0                     ; disable playfield
  sta PF1
  sta PF2
  sta CTRLPF                  ; repeat playfield (since score/timer must be asymmetric)

  ; uses 5 scanlines

;--------------------------------------------------------
; VBLANK
;--------------------------------------------------------

  ldx #32
LoopVBlank:
  dex
  sta WSYNC
  bne LoopVBlank

  lda #$0
  sta VBLANK        ; turn off VBLANK

;--------------------------------------------------------
; Kernel
;--------------------------------------------------------

  ldx #10                     ; scanline 95
ScoreBoardLoop:               ; add 20 scanlines space for scoreboard 

  ldy TensDigitOffset         ; load tens digit for score
  lda Digits,Y
  and #$F0  ;$10
  sta ScoreSprite
  
  ldy OnesDigitOffset         ; load ones digit for score
  lda Digits,Y
  and #$0F  ;$07
  ora ScoreSprite
  sta ScoreSprite ; 17

  sta WSYNC
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  sta PF1                     ; display score digit (first scanline)

  ldy TensDigitOffset+1       ; load tens digit for timer
  lda Digits,Y
  and #$F0  ;$50
  sta TimerSprite

  ldy OnesDigitOffset+1       ; load tens digit for timer
  lda Digits,Y
  and #$0F  ;$07
  ora TimerSprite
  sta TimerSprite ;57
  
  SLEEP 10                    ; delay to ensure timer digits are drawn

  sta PF1                     ; display timer digit (first scanline)

  sta WSYNC
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  lda ScoreSprite
  sta PF1                     ; display score digit (second scanline)

  sleep 29                    ; delay to ensure timer digits are drawn

  lda TimerSprite
  dex
  sta PF1                     ; display timer digit (second scanline)

  bne ScoreBoardLoop

  ; at scaline 85, add extra 1 * 2 = 2 scanlines to do a "carriage return" before continuing
  dex
  sta WSYNC

  lda #$00                    ; clear PF1 of digits
  sta PF1

  sta WSYNC

  ; set background color, playfield prior to game loop
  lda #$84                    ; set background color
  sta COLUBK

  lda #$01                    ; mirror playfield (since 'land' must be symmetric)
  sta CTRLPF

  lda #$F0                    ; set plafield blocks
  sta PF0
  lda #$FC
  sta PF1

  ldx #84                     ; scanline 84
KernelLoop:
  
  ; draw jet sprite
  txa
  sec
  sbc JetYPos                 ; subtract jet Y coord
  cmp JET_HEIGHT              ; compare to jet height
  bcc DrawSpriteP0            ; if result < jet height, draw with current index
  lda #0                      ; else, else, set index to 0
DrawSpriteP0:
  tay
  lda (JetSpritePtr),Y        ; load bitmap data of given jet sprite
  sta GRP0                    ; set player 0 line bitmap
  lda (JetColorPtr),Y         ; load color data of given jet sprite
  sta COLUP0                  ; set player 0 line color
  sta WSYNC
  
  ; draw bomber sprite
  txa
  sec
  sbc BomberYPos              ; subtract bomber Y coord
  cmp BOMBER_HEIGHT           ; compare to bomber height
  bcc DrawSpriteP1            ; if result < bomber height, draw with current index
  lda #0                      ; else, else, set table index to 0
DrawSpriteP1:
  tay
  lda (BomberSpritePtr),Y     ; load bitmap data of given bomber sprite
  sta GRP1                    ; set player 1 line bitmap
  lda (BomberColorPtr),Y      ; load color data of given bomber sprite
  sta COLUP1                  ; set player 1 line color
  ;sta WSYNC
  
  dex
  cpx #$ff                    ; determine if end of screen has been reached
  sta WSYNC                   ; (STA doesn't affect flags, so safe to use here)
  bne KernelLoop



;--------------------------------------------------------
; Overscan
;--------------------------------------------------------

  lda #$02          ; turn on VBLANK
  sta VBLANK

;--------------------------------------------------------
; Housekeeping (in OVERSCAN)
;--------------------------------------------------------

; update bomber position
CheckBomberYPosition:
  lda BomberYPos
  cmp #247                    ; check if bomber if fully off-screen
  bne DecrementBomberYPos     ; if so, directly decrement its y-position
  lda #84                     ; else, reset its y-position at top of screen (accounting for scoreboard)
  sta BomberYPos
  jsr LFSR                    ; also, reset its x-position with a random value
  lsr                         ; divide random postion by 2
  lsr                         ; divide random postion by 2
  clc                         ; add an offset of 40
  adc #40
  sta BomberXPos

DecrementBomberYPos:
  sta WSYNC
  dec BomberYPos

ResetJetSprite:
  lda #<JetSprite             ; set jet sprite pointer to 'normal/non-turning' sprite
  sta JetSpritePtr
  lda #>JetSprite
  sta JetSpritePtr+1

; process input
CheckP0Up:                    ; check joy = up
  lda #$10
  bit SWCHA
  bne CheckP0Down             ; if joy != up, skip
  inc JetYPos                 ; else, increment JetYPos

CheckP0Down:                  ; check joy = down
  lda #$20
  bit SWCHA
  bne CheckP0Left             ; if joy != down, skip
  dec JetYPos                 ; if down, increment JetYPos

CheckP0Left:                  ; check joy = left
  lda #$40
  bit SWCHA
  bne CheckP0Right            ; if joy != left, skip
  dec JetXPos                 ; if left, decrement JetXPos

  lda #<JetSpriteTurn         ; set jet sprite pointer to 'turning' sprite
  sta JetSpritePtr
  lda #>JetSpriteTurn
  sta JetSpritePtr+1

CheckP0Right:                 ; check joy = right
  lda #$80
  bit SWCHA
  bne NoInput                 ; if joy != right, skip
  inc JetXPos                 ; else, increment JetXPos

  lda #<JetSpriteTurn         ; set jet sprite pointer to 'turning' sprite
  sta JetSpritePtr
  lda #>JetSpriteTurn
  sta JetSpritePtr+1

NoInput:

  sta WSYNC

; check collisions
CheckP0P1Collision:           ; check if jet and bomber have collided
  lda #$80
  bit CXPPMM
  beq CheckP0PFCollision      ; if no, skip
  jsr GameOver                ; else, call game over subroutine

CheckP0PFCollision:           ; check if jet and playfield have collided
  lda #$80
  bit CXP0FB
  beq NoCollision             ; if no, skip
  jsr GameOver                ; else, call game over subroutine

NoCollision:
  sta CXCLR                   ; clear all collision registers

  ; uses 2 scanline

;--------------------------------------------------------
; Overscan (Cont.)
;--------------------------------------------------------

  ldx #28                     ; 30 - 2 = 28 scanlines
LoopOverscan:
  dex
  sta WSYNC
  bne LoopOverscan

  jmp NextFrame

;--------------------------------------------------------
; Subroutines
;--------------------------------------------------------

  org $FF46                   ; set at end of rom

  ; game over subroutine
GameOver subroutine
  lda #$40
  sta COLUBK
  rts

  ; LFSR subroutine
LFSR subroutine
  lda Random
  asl
  eor Random
  asl
  eor Random
  asl
  asl
  eor Random
  asl
  rol Random
  lda Random
  rts

; set horizontal postion subroutine
SetObjectXPos subroutine
  cpx #2            ; carry flag for ball/missile
  adc #0            ; add 1 to account for different timings
  sec               ; set carry
  sta WSYNC         ; 'carriage return'

.DivideLoop
  sbc #15
  bcs .DivideLoop

  eor #7
  asl
  asl
  asl
  asl
  sta.a HMP0,X  ; force absolute addressing for timing!
  sta RESP0,X
  sta WSYNC
  rts

;--------------------------------------------------------
; Lookup tables
;--------------------------------------------------------

JetSprite:
    .byte #%00000000         ;
    .byte #%00101000         ;  # #
    .byte #%11111110         ;#######
    .byte #%01111100         ; #####
    .byte #%00111000         ;  ###
    .byte #%00111000         ;  ###
    .byte #%00010000         ;   #
    .byte #%00010000         ;   #
    .byte #%00010000         ;   #

JetSpriteTurn:
    .byte #%00000000         ;
    .byte #%00010000         ;   #
    .byte #%01111100         ; #####
    .byte #%00111000         ;  ###
    .byte #%00111000         ;  ###
    .byte #%00111000         ;  ###
    .byte #%00010000         ;   #
    .byte #%00010000         ;   #
    .byte #%00010000         ;   #

BomberSprite:
    .byte #%00000000         ;
    .byte #%00010000         ;   #
    .byte #%00010000         ;   #
    .byte #%01010100         ; # # #
    .byte #%01111100         ; #####
    .byte #%11111110         ;#######
    .byte #%01010100         ; # # #
    .byte #%00010000         ;   #
    .byte #%00111000         ;  ###

JetColor:
    .byte #$00
    .byte #$FE
    .byte #$0C
    .byte #$0E
    .byte #$0E
    .byte #$04
    .byte #$BA
    .byte #$0E
    .byte #$08

JetColorTurn:
    .byte #$00
    .byte #$FE
    .byte #$0C
    .byte #$0E
    .byte #$0E
    .byte #$04
    .byte #$0E
    .byte #$0E
    .byte #$08

BomberColor:
    .byte #$00
    .byte #$32
    .byte #$32
    .byte #$0E
    .byte #$40
    .byte #$40
    .byte #$40
    .byte #$40
    .byte #$40

Digits:
  .byte %01110111          ; ### ###
  .byte %01010101          ; # # # #
  .byte %01010101          ; # # # #
  .byte %01010101          ; # # # #
  .byte %01110111          ; ### ###

  .byte %00010001          ;   #   #
  .byte %00010001          ;   #   #
  .byte %00010001          ;   #   #
  .byte %00010001          ;   #   #
  .byte %00010001          ;   #   #

  .byte %01110111          ; ### ###
  .byte %00010001          ;   #   #
  .byte %01110111          ; ### ###
  .byte %01000100          ; #   #
  .byte %01110111          ; ### ###

  .byte %01110111          ; ### ###
  .byte %00010001          ;   #   #
  .byte %00110011          ;  ##  ##
  .byte %00010001          ;   #   #
  .byte %01110111          ; ### ###

  .byte %01010101          ; # # # #
  .byte %01010101          ; # # # #
  .byte %01110111          ; ### ###
  .byte %00010001          ;   #   #
  .byte %00010001          ;   #   #

  .byte %01110111          ; ### ###
  .byte %01000100          ; #   #
  .byte %01110111          ; ### ###
  .byte %00010001          ;   #   #
  .byte %01110111          ; ### ###

  .byte %01110111          ; ### ###
  .byte %01000100          ; #   #
  .byte %01110111          ; ### ###
  .byte %01010101          ; # # # #
  .byte %01110111          ; ### ###

  .byte %01110111          ; ### ###
  .byte %00010001          ;   #   #
  .byte %00010001          ;   #   #
  .byte %00010001          ;   #   #
  .byte %00010001          ;   #   #

  .byte %01110111          ; ### ###
  .byte %01010101          ; # # # #
  .byte %01110111          ; ### ###
  .byte %01010101          ; # # # #
  .byte %01110111          ; ### ###

  .byte %01110111          ; ### ###
  .byte %01010101          ; # # # #
  .byte %01110111          ; ### ###
  .byte %00010001          ;   #   #
  .byte %01110111          ; ### ###

  .byte %00100010          ;  #   #
  .byte %01010101          ; # # # #
  .byte %01110111          ; ### ###
  .byte %01010101          ; # # # #
  .byte %01010101          ; # # # #

  .byte %01110111          ; ### ###
  .byte %01010101          ; # # # #
  .byte %01100110          ; ##  ##
  .byte %01010101          ; # # # #
  .byte %01110111          ; ### ###

  .byte %01110111          ; ### ###
  .byte %01000100          ; #   #
  .byte %01000100          ; #   #
  .byte %01000100          ; #   #
  .byte %01110111          ; ### ###

  .byte %01100110          ; ##  ##
  .byte %01010101          ; # # # #
  .byte %01010101          ; # # # #
  .byte %01010101          ; # # # #
  .byte %01100110          ; ##  ##

  .byte %01110111          ; ### ###
  .byte %01000100          ; #   #
  .byte %01110111          ; ### ###
  .byte %01000100          ; #   #
  .byte %01110111          ; ### ###

  .byte %01110111          ; ### ###
  .byte %01000100          ; #   #
  .byte %01100110          ; ##  ##
  .byte %01000100          ; #   #
  .byte %01000100          ; #   #

;--------------------------------------------------------
; End of ROM
;--------------------------------------------------------

  org $FFFC
  .word Start
  .word Start