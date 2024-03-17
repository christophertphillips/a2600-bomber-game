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

  sta WSYNC;259---------------
  sta WSYNC;260---------------
  sta WSYNC;261---------------

  lda #$00                    ;  2   00
  sta VSYNC                   ;  3   02 turn off VSYNC

;--------------------------------------------------------
; Housekeeping (in VBLANK)
;--------------------------------------------------------

  ; set jet horizontal position
  lda JetXPos                 ;   3   05    load GRP0 position from memory
  ldx #0                      ;   2   08    indicate GRP0
  jsr SetObjectXPos           ;   6   10    call subroutine to set GRP0 horizontal position (spans two scanlines)

  ; set bomber horizontal position
  lda BomberXPos              ;   3   06*   load GRP1 position from memory
  ldx #1                      ;   2   09    indicate GRP1
  jsr SetObjectXPos           ;   6   11    call subroutine to set GRP1 horizontal position (spans two scanlines)

  ; perform fine-tune horizontal offsets
  sta WSYNC;4-----------------;   3   06*   'carriage return' before HMOVE
  sta HMOVE                   ;   3   00    perform fine-tune offset

  ; reset graphics, playfield, playfield reflection
  lda #0                      ;   2   03
  sta GRP0                    ;   3   05    disable sprites
  sta GRP1                    ;   3   08 
  sta PF0                     ;   3   11    disable playfield
  sta PF1                     ;   3   14
  sta PF2                     ;   3   17
  
  sta CTRLPF                  ;   3   20    repeat playfield (since score/timer must be asymmetric)
  
  sta COLUBK                  ;   3   23    set background to black

  lda #$1F                    ;   2   26    set playfield (digits) to yellow
  sta COLUPF                  ;   3   28

  ; calculate score offsets
  sta WSYNC;5-----------------;   3   31
  jsr GetScoreOffsets         ;   6   00    (spans a scanline)
  sta WSYNC;7-----------------;   3   00

  ; uses 8 scanlines

;--------------------------------------------------------
; VBLANK
;--------------------------------------------------------

  ldx #29
LoopVBlank:
  dex
  sta WSYNC;8,36--------------
  bne LoopVBlank

  lda #$0
  sta VBLANK        ; turn off VBLANK

;--------------------------------------------------------
; Kernel
;--------------------------------------------------------

  ; draw scoreboard (scanline 95)
  ldx #5
ScoreBoardLoop:               ;                   add 20 scanlines space for scoreboard 
  ldy TensDigitOffset         ;   3   09    46    load the tens digit offset for the score
  lda Digits,Y                ;   4   12    49    load the digit bit pattern from the lookup table
  and #$F0                    ;   2   16    53    remove the ones digit
  sta ScoreSprite             ;   3   18    55    save the Score (tens) digit pattern into RAM
  
  ldy OnesDigitOffset         ;   3   21    58    load the ones digit offset for the score
  lda Digits,Y                ;   4   24    61    load the digit bit pattern from the lookup table
  and #$0F                    ;   2   28    65    remove the tens digit
  ora ScoreSprite             ;   3   30    67    merge it with the tens digit pattern in RAM
  sta ScoreSprite             ;   3   33    70    save the Score (tens + ones) digit pattern into RAM

  sta WSYNC;37,45-------------;   3   36    73    'carriage return' to give enough time to draw score to left side of screen
  
  sta PF1                     ;   3   00          draw score digits (first scanline)

  ldy TensDigitOffset+1       ;   3   03          load the tens digit offset for the timer
  lda Digits,Y                ;   4   06          load the digit bit pattern from the lookup table
  and #$F0                    ;   2   10          remove the ones digit
  sta TimerSprite             ;   3   12          save the Score (tens) digit pattern into RAM

  ldy OnesDigitOffset+1       ;   3   15          load the tens digit offset for the score
  lda Digits,Y                ;   4   18          load the digit bit pattern from the lookup table
  and #$0F                    ;   2   22          remove the tens digit
  ora TimerSprite             ;   3   24          merge it with the tens digit pattern in RAM
  sta TimerSprite             ;   3   27          save the Score (tens + ones) digit pattern into RAM
  
  SLEEP 10                    ;  10   30          delay to ensure timer digits are drawn

  sta PF1                     ;   3   40          draw timer digits (first scanline)

  sta WSYNC;38,46-------------;   3   43          'carriage return' to give enough time to draw score to left side of screen

  lda ScoreSprite             ;   3   00          load score digits
  sta PF1                     ;   3   03          display score digits (second scanline)

  inc TensDigitOffset         ;   5   06          update all score/timer offsets to point to the next bit pattern of their respective digit
  inc TensDigitOffset+1       ;   5   11
  inc OnesDigitOffset         ;   5   16
  inc OnesDigitOffset+1       ;   5   21

  SLEEP 9                     ;   9   26          delay to ensure timer digits are drawn

  lda TimerSprite             ;   3   35          load timer digits
  dex                         ;   2   38
  sta PF1                     ;   3   40          display timer digits (second scanline)

  bne ScoreBoardLoop          ; 2/3   43

  sta WSYNC;47----------------;   3   45



  ; clear scoreboard
  lda #$00                    ;   2   00          clear PF1 of digits
  sta PF1                     ;   3   02

  sta WSYNC;48----------------;   3   05          draw a "buffer line" btween scoreboard and gameplay area
  sta WSYNC;49----------------;
  sta WSYNC;50----------------;


  
  ; configure background color, playfield
  lda #$84                    ;   2   00          set background color
  sta COLUBK                  ;   3   02

  lda #$C2                    ;   2   05          set playfield color
  sta COLUPF                  ;   3   07

  lda #$01                    ;   2   10          mirror playfield (since 'land' must be symmetric)
  sta CTRLPF                  ;   3   12

  lda #$F0                    ;   2   15          set plafield blocks
  sta PF0                     ;   3   17
  lda #$FC                    ;   2   20
  sta PF1                     ;   3   22



  ; draw gameplay area
  ldx #88                     ;   2   25          (scanline 89)
KernelLoop:
  ; draw jet sprite
  txa                         ;   2   27          03
  sec                         ;   2   29          05    
  sbc JetYPos                 ;   3   31          07          subtract jet Y coord
  cmp JET_HEIGHT              ;   3   34          10          compare to jet height
  bcc DrawSpriteP0            ; 2/3   37          13          if result < jet height, draw with current index
  lda #0                      ;   2   39          15          else, else, set index to 0
DrawSpriteP0:
  tay                         ;   2   41    40    17    16
  lda (JetSpritePtr),Y        ;   5   43    42    19    18    load bitmap data of given jet sprite
  sta GRP0                    ;   3   48    47    24    23    set player 0 line bitmap
  lda (JetColorPtr),Y         ;   5   51    50    27    26    load color data of given jet sprite
  sta COLUP0                  ;   3   56    55    32    31    set player 0 line color
  sta WSYNC;51,227------------;   3   59    58    35    34
  
  ; draw bomber sprite
  txa                         ;   2   00
  sec                         ;   2   02
  sbc BomberYPos              ;   3   04          subtract bomber Y coord
  cmp BOMBER_HEIGHT           ;   3   07          compare to bomber height
  bcc DrawSpriteP1            ; 2/3   10          if result < bomber height, draw with current index
  lda #0                      ;   2   12          else, else, set table index to 0
DrawSpriteP1:
  tay                         ;   2   14    13
  lda (BomberSpritePtr),Y     ;   5   16    15    load bitmap data of given bomber sprite
  sta GRP1                    ;   3   21    20    set player 1 line bitmap
  lda (BomberColorPtr),Y      ;   5   24    23    load color data of given bomber sprite
  sta COLUP1                  ;   3   29    28    set player 1 line color
  ;sta WSYNC
  
  dex                         ;   2   32
  cpx #$ff                    ;   2   34          determine if end of screen has been reached
  sta WSYNC;52,228------------;   3   36          (STA doesn't affect flags, so safe to use here)
  bne KernelLoop              ; 2/3   00



;--------------------------------------------------------
; Overscan
;--------------------------------------------------------

  lda #$02                    ;   2   02  turn on VBLANK
  sta VBLANK                  ;   3   04

;--------------------------------------------------------
; Housekeeping (in OVERSCAN)
;--------------------------------------------------------

; update bomber position
CheckBomberYPosition:
  lda BomberYPos              ;   3   07
  cmp #247                    ;   2   10          check if bomber if fully off-screen
  bne DecrementBomberYPos     ; 2/3   12          if so, directly decrement its y-position
  lda #89                     ;   2   14          else, reset its y-position at top of screen (accounting for scoreboard)
  sta BomberYPos              ;   3   16
  jsr LFSR                    ;   6   19          also, reset its x-position with a random value
  lsr                         ;   2   61*         divide random postion by 2
  lsr                         ;   2   63          divide random postion by 2
  clc                         ;   2   65          add an offset of 40
  adc #40                     ;   2   67
  sta BomberXPos              ;   3   69

DecrementBomberYPos:
  sta WSYNC;229---------------;   3   72    15
  dec BomberYPos              ;   5   00    00

ResetJetSprite:
  lda #<JetSprite             ;   2   05          set jet sprite pointer to 'normal/non-turning' sprite
  sta JetSpritePtr            ;   3   07
  lda #>JetSprite             ;   2   10
  sta JetSpritePtr+1          ;   3   12

; process input 
CheckP0Up:                    ; check joy = up
  lda #$10                    ;   2   15
  bit SWCHA                   ;   4   17
  bne CheckP0Down             ; 2/3   21    21    if joy != up, skip
  inc JetYPos                 ;   5         23    else, increment JetYPos

CheckP0Down:                  ; check joy = down
  lda #$20                    ;   2   24    28
  bit SWCHA                   ;   4   26    30
  bne CheckP0Left             ; 2/3   30    34    if joy != down, skip
  dec JetYPos                 ;   5               if down, increment JetYPos

CheckP0Left:                  ; check joy = left
  lda #$40                    ;   2   33    37  
  bit SWCHA                   ;   4   35    39
  bne CheckP0Right            ; 2/3   39    43    if joy != left, skip
  dec JetXPos                 ;   5         45    if left, decrement JetXPos

  lda #<JetSpriteTurn         ;   2         50    set jet sprite pointer to 'turning' sprite
  sta JetSpritePtr            ;   3         52
  lda #>JetSpriteTurn         ;   2         55
  sta JetSpritePtr+1          ;   3         57

CheckP0Right:                 ; check joy = right
  lda #$80                    ;   2   42    60
  bit SWCHA                   ;   4   44    62
  bne NoInput                 ; 2/3   48    66    if joy != right, skip
  inc JetXPos                 ;   5               else, increment JetXPos

  lda #<JetSpriteTurn         ;   2               set jet sprite pointer to 'turning' sprite
  sta JetSpritePtr            ;   3
  lda #>JetSpriteTurn         ;   2
  sta JetSpritePtr+1          ;   3

NoInput:

  sta WSYNC;230---------------;   3   51    69

; check collisions
CheckP0P1Collision:           ;                   check if jet and bomber have collided
  lda #$80                    ;   2   00
  bit CXPPMM                  ;   3   02
  beq NoCollision             ; 2/3   05          if no, skip
  jsr GameOver                ;   6   07          else, call game over subroutine

NoCollision:
  sta CXCLR                   ;   3   24*   08    clear all collision registers

  ; uses 2 scanline

;--------------------------------------------------------
; Overscan (Cont.)
;--------------------------------------------------------

  ldx #28                     ;   2   27    11      (30 - 2 = 28 scanlines)
LoopOverscan:
  dex                         ;   2   29    13
  sta WSYNC;231,258-----------;   3   31    15
  bne LoopOverscan            ; 2/3   00    00

  jmp NextFrame               ;   3   02

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
GameOver subroutine           ; (17 cycles total)
  lda #$40                    ; 2
  sta COLUBK                  ; 3
  rts                         ; 6

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