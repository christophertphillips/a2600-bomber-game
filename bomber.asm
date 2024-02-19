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
  
  txa                         ; draw jet sprite
  sec
  sbc JetYPos
  cmp JET_HEIGHT
  bcc DrawSpriteP0
  lda #0
DrawSpriteP0:
  tay
  lda (JetSpritePtr),Y
  sta GRP0
  lda (JetColorPtr),Y
  sta COLUP0
  sta WSYNC
  
  txa                         ; draw jet sprite
  sec
  sbc BomberYPos
  cmp BOMBER_HEIGHT
  bcc DrawSpriteP1
  lda #0
DrawSpriteP1:
  tay
  lda (BomberSpritePtr),Y
  sta GRP1
  lda (BomberColorPtr),Y
  sta COLUP1
  ;sta WSYNC
  
  dex
  cpx #$ff
  sta WSYNC                   ; (STA doesn't affect flags, so safe to use here)
  bne KernelLoop



;--------------------------------------------------------
; Overscan
;--------------------------------------------------------

  lda #$02          ; turn on VBLANK
  sta VBLANK

;--------------------------------------------------------
; Input (in OVERSCAN)
;--------------------------------------------------------

CheckP0Up:
  lda #$10
  bit SWCHA
  bne CheckP0Down
  inc JetYPos

CheckP0Down:
  lda #$20
  bit SWCHA
  bne CheckP0Left
  dec JetYPos

CheckP0Left:
  lda #$40
  bit SWCHA
  bne CheckP0Right
  dec JetXPos

CheckP0Right:
  lda #$80
  bit SWCHA
  bne NoInput
  inc JetXPos

NoInput:

;--------------------------------------------------------
; Overscan (Cont.)
;--------------------------------------------------------

  ldx #30
LoopOverscan:
  dex
  sta WSYNC
  bne LoopOverscan

  jmp NextFrame

;--------------------------------------------------------
; Subroutines
;--------------------------------------------------------

  org $FFAD         ; set at end of rom

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
    .byte #%00010100         ;   # #
    .byte #%01111111         ; #######
    .byte #%00111110         ;  #####
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #

JetSpriteTurn:
    .byte #%00000000         ;
    .byte #%00001000         ;    #
    .byte #%00111110         ;  #####
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #

BomberSprite:
    .byte #%00000000         ;
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00101010         ;  # # #
    .byte #%00111110         ;  #####
    .byte #%01111111         ; #######
    .byte #%00101010         ;  # # #
    .byte #%00001000         ;    #
    .byte #%00011100         ;   ###

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