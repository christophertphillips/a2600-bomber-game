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

JetXPos         ds 1
JetYPos         ds 1
BomberXPos      ds 1
BomberYPos      ds 1
JetSpritePtr    ds 2
JetColorPtr     ds 2
BomberSpritePtr ds 2
BomberColorPtr  ds 2
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

  lda #$F0                    ; set plafield blocks
  sta PF0
  lda #$FC
  sta PF1
  lda #$00
  sta PF2

  lda #$01                    ; mirror playfield
  sta CTRLPF

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

JET_HEIGHT = 9
BOMBER_HEIGHT = 9

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

  ldx #95
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
  lda #96                     ; else, reset its y-position at top of screen
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

  ; uses 1 scanline

;--------------------------------------------------------
; Overscan (Cont.)
;--------------------------------------------------------

  ldx #29                     ; 30 - 1 = 29 scanlines
LoopOverscan:
  dex
  sta WSYNC
  bne LoopOverscan

  jmp NextFrame

;--------------------------------------------------------
; Subroutines
;--------------------------------------------------------

  org $FF9B         ; set at end of rom

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

;--------------------------------------------------------
; End of ROM
;--------------------------------------------------------

  org $FFFC
  .word Start
  .word Start