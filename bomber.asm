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

  lda #$69                    ; set score value (in BCD)
  sta Score

  lda #$25                    ; set timer value (in BCD)
  sta Timer

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

  sta WSYNC;------------------
  sta WSYNC;------------------
  sta WSYNC;------------------

  lda #$00                    ;  2   00
  sta VSYNC                   ;  3   02 turn off VSYNC

;--------------------------------------------------------
; Housekeeping (in VBLANK)
;--------------------------------------------------------

  ; set jet horizontal position
  lda JetXPos                 ;  3   05   load GRP0 position from memory
  ldx #0                      ;  2   08   indicate GRP0
  jsr SetObjectXPos           ;  6   10   call subroutine to set GRP0 horizontal position

  ; set bomber horizontal position
  lda BomberXPos              ;  3   06*  load GRP1 position from memory
  ldx #1                      ;  2   09   indicate GRP1
  jsr SetObjectXPos           ;  6   11   call subroutine to set GRP1 horizontal position

  ; perform fine-tune horizontal offsets
  sta WSYNC;------------------;  3   06*  'carriage return' before HMOVE
  sta HMOVE                   ;  3   00   perform fine-tune offset

  ; reset graphics, playfield, playfield reflection
  lda #0                      ;  2   03
  sta GRP0                    ;  3   05   disable sprites
  sta GRP1                    ;  3   08 
  sta PF0                     ;  3   11   disable playfield
  sta PF1                     ;  3   14
  sta PF2                     ;  3   17
  sta CTRLPF                  ;  3   20   repeat playfield (since score/timer must be asymmetric)

  ; calculate score offsets
  sta WSYNC;------------------;  3   23
  jsr GetScoreOffsets         ;  6   00   (spans a scanline)
  sta WSYNC;------------------;  3   00

  ; uses 8 scanlines

;--------------------------------------------------------
; VBLANK
;--------------------------------------------------------

  ldx #29
LoopVBlank:
  dex
  sta WSYNC;------------------
  bne LoopVBlank

  lda #$0
  sta VBLANK        ; turn off VBLANK

;--------------------------------------------------------
; Kernel
;--------------------------------------------------------

  ; draw scoreboard (scanline 95)
  ldx #5
ScoreBoardLoop:               ;   (-37)   add 20 scanlines space for scoreboard 
  ldy TensDigitOffset         ;  3   46   load the tens digit offset for the score
  lda Digits,Y                ;  4   49   load the digit bit pattern from the lookup table
  and #$F0                    ;  2   53   remove the ones digit
  sta ScoreSprite             ;  3   55   save the Score (tens) digit pattern into RAM
  
  ldy OnesDigitOffset         ;  3   58   load the ones digit offset for the score
  lda Digits,Y                ;  4   61   load the digit bit pattern from the lookup table
  and #$0F                    ;  2   65   remove the tens digit
  ora ScoreSprite             ;  3   67   merge it with the tens digit pattern in RAM
  sta ScoreSprite             ;  3   70   save the Score (tens + ones) digit pattern into RAM

  sta WSYNC;------------------;  3   73   'carriage return' to give enough time to draw score to left side of screen
  
  sta PF1                     ;  3   00   draw score digits (first scanline)

  ldy TensDigitOffset+1       ;  3   03   load the tens digit offset for the timer
  lda Digits,Y                ;  4   06   load the digit bit pattern from the lookup table
  and #$F0                    ;  2   10   remove the ones digit
  sta TimerSprite             ;  3   12   save the Score (tens) digit pattern into RAM

  ldy OnesDigitOffset+1       ;  3   15   load the tens digit offset for the score
  lda Digits,Y                ;  4   18   load the digit bit pattern from the lookup table
  and #$0F                    ;  2   22   remove the tens digit
  ora TimerSprite             ;  3   24   merge it with the tens digit pattern in RAM
  sta TimerSprite             ;  3   27   save the Score (tens + ones) digit pattern into RAM
  
  SLEEP 10                    ; 10   30   delay to ensure timer digits are drawn

  sta PF1                     ;  3   40   draw timer digits (first scanline)

  sta WSYNC;------------------;  3   43   'carriage return' to give enough time to draw score to left side of screen

  lda ScoreSprite             ;  3   00   load score digits
  sta PF1                     ;  3   03   display score digits (second scanline)

  inc TensDigitOffset         ;  5   06   update all score/timer offsets to point to the next bit pattern of their respective digit
  inc TensDigitOffset+1       ;  5   11
  inc OnesDigitOffset         ;  5   16
  inc OnesDigitOffset+1       ;  5   21

  SLEEP 9                     ;  9   26   delay to ensure timer digits are drawn

  lda TimerSprite             ;  3   35   load timer digits
  dex                         ;  2   38
  sta PF1                     ;  3   40   display timer digits (second scanline)

  bne ScoreBoardLoop          ;3/2   43

  sta WSYNC;------------------;  3   45



  ; clear scoreboard
  lda #$00                    ;  2   00   clear PF1 of digits
  sta PF1                     ;  3   02

  sta WSYNC;------------------;  3   05   draw a "buffer line" btween scoreboard and gameplay area


  
  ; configure background color, playfield
  lda #$84                    ;  2   00   set background color
  sta COLUBK                  ;  3   02

  lda #$01                    ;  2   05 mirror playfield (since 'land' must be symmetric)
  sta CTRLPF                  ;  3   07

  lda #$F0                    ;  2   10 set plafield blocks
  sta PF0                     ;  3   12
  lda #$FC                    ;  2   15
  sta PF1                     ;  3   17



  ; draw gameplay area
  ldx #89                     ;  2   20 scanline 89
KernelLoop:                   ;           (+19 if first iteration)
  ; draw jet sprite
  txa                         ;  2   03
  sec                         ;  2   05
  sbc JetYPos                 ;  3   07   subtract jet Y coord
  cmp JET_HEIGHT              ;  3   10   compare to jet height
  bcc DrawSpriteP0            ;3/2   13   if result < jet height, draw with current index
  lda #0                      ;  2   15   else, else, set index to 0
DrawSpriteP0:                 ;           (-1 if jumped to)
  tay                         ;  2   17
  lda (JetSpritePtr),Y        ;  5   19   load bitmap data of given jet sprite
  sta GRP0                    ;  3   24   set player 0 line bitmap
  lda (JetColorPtr),Y         ;  5   27   load color data of given jet sprite
  sta COLUP0                  ;  3   32   set player 0 line color
  sta WSYNC;------------------;  3   35
  
  ; draw bomber sprite
  txa                         ;  2   00
  sec                         ;  2   02
  sbc BomberYPos              ;  3   04   subtract bomber Y coord
  cmp BOMBER_HEIGHT           ;  3   07   compare to bomber height
  bcc DrawSpriteP1            ;3/2   10   if result < bomber height, draw with current index
  lda #0                      ;  2   12   else, else, set table index to 0
DrawSpriteP1:                 ;           (-1 if jumped to)
  tay                         ;  2   14
  lda (BomberSpritePtr),Y     ;  5   16   load bitmap data of given bomber sprite
  sta GRP1                    ;  3   21   set player 1 line bitmap
  lda (BomberColorPtr),Y      ;  5   24   load color data of given bomber sprite
  sta COLUP1                  ;  3   29   set player 1 line color
  ;sta WSYNC
  
  dex                         ;  2   32
  cpx #$ff                    ;  2   34   determine if end of screen has been reached
  sta WSYNC;------------------;  3   36   (STA doesn't affect flags, so safe to use here)
  bne KernelLoop              ;  3   00



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
  lda #89                     ; else, reset its y-position at top of screen (accounting for scoreboard)
  sta BomberYPos
  jsr LFSR                    ; also, reset its x-position with a random value
  lsr                         ; divide random postion by 2
  lsr                         ; divide random postion by 2
  clc                         ; add an offset of 40
  adc #40
  sta BomberXPos

DecrementBomberYPos:
  sta WSYNC;------------------
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

  sta WSYNC;------------------

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
  sta WSYNC;------------------
  bne LoopOverscan

  jmp NextFrame

;--------------------------------------------------------
; Subroutines
;--------------------------------------------------------

  org $FF24                   ; set at end of rom

GetScoreOffsets subroutine
  ldx #1
.GetScoreOffsetsLoop
  lda Score,X                 ; load A with Timer (x=1) or Score (x=0)
  and #$0F                    ; remove the tens digit
  sta DigitHelperByte         ; save the value of A into Temp
  asl                         ; shift left (N*2)
  asl                         ; shift left (N*4)
  clc
  adc DigitHelperByte         ; add the value saved in Temp (+N, thus 2N + 2N + N = 5N)
  sta OnesDigitOffset,X       ; save A in OnesDigitOffset+1 or OnesDigitOffset

  lda Score,X                 ; load A with Timer (x=1) or Score (x=0)
  and #$F0                    ; remove the ones digit
  lsr                         ; shift right (N/2)
  lsr                         ; shift right (N/4)
  sta DigitHelperByte         ; save the value of A into Temp
  lsr                         ; shift right (N/8)
  lsr                         ; shift right (N/16)
  clc
  adc DigitHelperByte         ; add the value saved in Temp (+N/4, thus N/4 + N/16 = 5N/16)
  sta TensDigitOffset,X       ; save A in TensDigitOffset+1 or TensDigitOffset

  dex
  bpl .GetScoreOffsetsLoop
  rts

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
  cpx #2                      ; carry flag for ball/missile
  adc #0                      ; add 1 to account for different timings
  sec                         ; set carry
  sta WSYNC;------------------; 'carriage return'

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
  sta WSYNC;------------------
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