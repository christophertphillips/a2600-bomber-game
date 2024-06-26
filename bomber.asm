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
MissileXPos     ds 1
MissileYPos     ds 1

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

; playfield/background color
BackgroundColor ds 1
PlayfieldColor ds 1

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

  lda #88                     ; position missile immediate offscreen
  sta MissileYPos

  lda #$84                    ; set background color
  sta BackgroundColor

  lda #$C2                    ; set playfield color
  sta PlayfieldColor

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

  lda #0                      ; initialize score and timer
  sta Score
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
  sta WSYNC;0-----------------;   3   05
  lda JetXPos                 ;   3   00    load GRP0 position from memory
  ldx #0                      ;   2   03    indicate GRP0
  jsr SetObjectXPos           ;   6+  5    call subroutine to set GRP0 horizontal position (spans two scanlines)

  ; set bomber horizontal position
  sta WSYNC;3-----------------;   3   06*
  lda BomberXPos              ;   3   00   load GRP1 position from memory
  ldx #1                      ;   2   03    indicate GRP1
  jsr SetObjectXPos           ;   6+  05    call subroutine to set GRP1 horizontal position (spans two scanlines)

  ; set missile horizontal position
  sta WSYNC;6-----------------;   3   06*
  lda MissileXPos             ;   3   00    load M0 position from RAM
  ldx #2                      ;   2   03    indicate M0
  jsr SetObjectXPos           ;   6+  05    call subroutine to set M0 horizontal position (spans two scanlines)

  ; perform fine-tune horizontal offsets
  sta WSYNC;9-----------------;   3   06*   'carriage return' before HMOVE
  sta HMOVE                   ;   3   00    perform fine-tune offset

  ; reset graphics, playfield, playfield reflection
  sta WSYNC;10----------------;   3   03
  lda #0                      ;   2   00
  sta GRP0                    ;   3   02    disable sprites
  sta GRP1                    ;   3   05 
  sta PF0                     ;   3   08    disable playfield
  sta PF1                     ;   3   11
  
  sta CTRLPF                  ;   3   14    repeat playfield (since score/timer must be asymmetric)
  
  sta COLUBK                  ;   3   17    set background to black

  lda #$1F                    ;   2   20    set playfield (digits) to yellow
  sta COLUPF                  ;   3   22

  ; calculate score offsets
  sta WSYNC;11----------------;   3   25
  jsr GetScoreOffsets         ;   6   00    (spans a scanline)
  sta WSYNC;13----------------;   3   43

  ; uses 14 scanlines

;--------------------------------------------------------
; VBLANK
;--------------------------------------------------------

  ldx #23                     ;   2   00          (37 - 14 = 23 scanlines)
LoopVBlank:
  dex                         ;   2   02    03
  sta WSYNC;14,36-------------;   3   04    05
  bne LoopVBlank              ; 2/3   00    00

  lda #$0                     ;   2   02
  sta VBLANK                  ;   3   04          turn off VBLANK

;--------------------------------------------------------
; Kernel
;--------------------------------------------------------

  jmp OnePageBoundary         ;   3   07          jump to one page
  align $100                  ;                   align on one page boundary

OnePageBoundary:

  ; draw scoreboard (scanline 95)
  ldx #5                      ;   2   10
ScoreBoardLoop:               ;                   add 20 scanlines space for scoreboard 
  ldy TensDigitOffset         ;   3   12    46    load the tens digit offset for the score
  lda Digits,Y                ;   4   15    49    load the digit bit pattern from the lookup table
  and #$F0                    ;   2   19    53    remove the ones digit
  sta ScoreSprite             ;   3   21    55    save the Score (tens) digit pattern into RAM
  
  ldy OnesDigitOffset         ;   3   24    58    load the ones digit offset for the score
  lda Digits,Y                ;   4   27    61    load the digit bit pattern from the lookup table
  and #$0F                    ;   2   31    65    remove the tens digit
  ora ScoreSprite             ;   3   33    67    merge it with the tens digit pattern in RAM
  sta ScoreSprite             ;   3   36    70    save the Score (tens + ones) digit pattern into RAM

  sta WSYNC;37,45-------------;   3   39    73    'carriage return' to give enough time to draw score to left side of screen
  
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
  lda BackgroundColor         ;   3   00          set background color
  sta COLUBK                  ;   3   03

  lda PlayfieldColor          ;   3   06          set playfield color
  sta COLUPF                  ;   3   09

  lda #$01                    ;   2   12          mirror playfield (since 'land' must be symmetric)
  sta CTRLPF                  ;   3   14

  lda #$F0                    ;   2   17          set plafield blocks
  sta PF0                     ;   3   19
  lda #$FC                    ;   2   22
  sta PF1                     ;   3   24



  ; draw gameplay area
  ldx #87                     ;   2   27
  sta WSYNC;51----------------;   3   29
  sta WSYNC;52----------------;   3   00
  SLEEP 3
KernelLoop:
  ; draw jet sprite
  txa                         ;   2   03
  sec                         ;   2   05
  sbc JetYPos                 ;   3   07          subtract jet Y coord
  cmp JET_HEIGHT              ;   3   10          compare to jet height
  bcc DrawSpriteP0            ; 2/3   13          if result < jet height, draw with current index
  lda #0                      ;   2   15          else, else, set index to 0
DrawSpriteP0:
  tay                         ;   2   17          16
  lda (JetSpritePtr),Y        ;   5   19          18    load bitmap data of given jet sprite
  pha                         ;   3   24          23    push p0 color to stack
  lda (JetColorPtr),Y         ;   5   27          26    load color data of given jet sprite
  pha                         ;   3   32          31    push p0 bitmap to stack
  
  ; draw bomber sprite
  txa                         ;   2   35          34
  sec                         ;   2   37          36
  sbc BomberYPos              ;   3   39          38    subtract bomber Y coord
  cmp BOMBER_HEIGHT           ;   3   42          41    compare to bomber height
  bcc DrawSpriteP1            ; 2/3   45          44    if result < bomber height, draw with current index
  lda #0                      ;   2   47          46    else, else, set table index to 0
DrawSpriteP1:
  tay                         ;   2   49    48    48    47
  lda (BomberSpritePtr),Y     ;   5   51    50    50    49    load bitmap data of given bomber sprite
  pha                         ;   3   56    55    55    54
  lda (BomberColorPtr),Y      ;   5   59    58    58    57    load color data of given bomber sprite
  pha                         ;   3   64    63    63    62

  sta WSYNC;53,227------------;   3   67    66    66    65

  pla                         ;   4   00    00    00    00    pull p0 color from stack
  sta COLUP1                  ;   3   04          set p0 color
  pla                         ;   4   07          pull p0 bitmap from stack
  sta GRP1                    ;   3   11          set p0 bitmap

  pla                         ;   4   14          pull p1 color from stack
  sta COLUP0                  ;   3   18          set p1 color
  pla                         ;   4   21          pull p1 bitmap from stack
  sta GRP0                    ;   3   25          set p1 bitmap

  lda #$00                    ;   2   28
  cpx MissileYPos             ;   3   30          compare current scanline to missile position
  bne DrawMissile0            ; 2/3   33          if result = current scanline, activate missile
  lda #$02                    ;   2   35          else, don't activate missile
DrawMissile0:
  sta ENAM0                   ;   3   37    36    set missile
  
  dex                         ;   2   40    39    decrement kernel scanline counter
  cpx #$ff                    ;   2   42    41    determine if end of screen has been reached
  sta WSYNC;54,228------------;   3   44    43    (STA doesn't affect flags, so safe to use here)
  bne KernelLoop              ; 2/3   00    00


  jmp TwoPageBoundary         ;   3   02          jump to one page
  align $200                  ;                   align on one page boundary

TwoPageBoundary:


;--------------------------------------------------------
; Overscan
;--------------------------------------------------------

  lda #$02                    ;   2   05  turn on VBLANK
  sta VBLANK                  ;   3   07

;--------------------------------------------------------
; Housekeeping (in OVERSCAN)
;--------------------------------------------------------

; update bomber position
CheckBomberYPosition:
  sta WSYNC;229---------------;   3   10
  lda BomberYPos              ;   3   00
  cmp #247                    ;   2   03          check if bomber if fully off-screen
  bne SkipBomberReset         ; 2/3   05          if so, directly decrement its y-position
  lda #87                     ;   2   07          else, reset its y-position at top of screen (accounting for scoreboard)
  sta BomberYPos              ;   3   09
  jsr LFSR                    ;   6   12          also, reset its x-position with a random value
  sta WSYNC;230(a)------------;   3   54*
  lsr                         ;   2   00          divide random postion by 2
  lsr                         ;   2   02          divide random postion by 2
  clc                         ;   2   04          add an offset of 40
  adc #40                     ;   2   06
  sta BomberXPos              ;   3   08
  sed                         ;   2   11          activate BCD mode
  lda Score                   ;   3   13          increment score
  clc                         ;   2   16
  adc #1                      ;   2   18
  sta Score                   ;   3   20
  lda Timer                   ;   3   23          increment timer
  clc                         ;   2   26
  adc #1                      ;   2   28
  sta Timer                   ;   3   30
  cld                         ;   2   33          deactivate BCD mode
  jmp DecrementBomberYPos     ;   3   35

SkipBomberReset:
  sta WSYNC;230(b)------------;   3         08

DecrementBomberYPos:
  sta WSYNC;231---------------;   3   34    00
  dec BomberYPos              ;   5   00    00

ResetJetSprite:
  sta WSYNC;232---------------;   3   05
  lda #<JetSprite             ;   2   00          set jet sprite pointer to 'normal/non-turning' sprite
  sta JetSpritePtr            ;   3   02
  lda #>JetSprite             ;   2   05
  sta JetSpritePtr+1          ;   3   07

; process input 
CheckP0Up:                    ; check joy = up
  sta WSYNC;233---------------;   3   10
  lda #$10                    ;   2   00
  bit SWCHA                   ;   4   02
  bne CheckP0Down             ; 2/3   06          if joy != up, skip
  lda JetYPos                 ;   3   08          check if jet is at top of screen
  cmp #79                     ;   2   11
  bpl CheckP0Down             ; 2/3   13          if yes, skip
  inc JetYPos                 ;   5   15          else, increment JetYPos

CheckP0Down:                  ; check joy = down
  sta WSYNC;234---------------;   3   20    09    16
  lda #$20                    ;   2   00    00    00
  bit SWCHA                   ;   4   02 
  bne CheckP0Left             ; 2/3   06          if joy != down, skip
  lda JetYPos                 ;   3   08         check if jet is at bottom of screen
  cmp #0                      ;   2   11
  bmi CheckP0Left             ; 2/3   13          if yes, skip
  dec JetYPos                 ;   5   15          if down, increment JetYPos

CheckP0Left:                  ; check joy = left
  sta WSYNC;235---------------;   3   20    09    16
  lda #$40                    ;   2   00    00    00
  bit SWCHA                   ;   4   02    
  bne CheckP0Right            ; 2/3   06          if joy != left, skip
  lda JetXPos                 ;   3   08          check if jet is at left of screen
  cmp #41                     ;   2   11
  bmi CheckP0Right            ; 2/3   13          if yes, skip
  dec JetXPos                 ;   5   15          else, decrement JetXPos

  lda #<JetSpriteTurn         ;   2   20          set jet sprite pointer to 'turning' sprite
  sta JetSpritePtr            ;   3   22
  lda #>JetSpriteTurn         ;   2   25
  sta JetSpritePtr+1          ;   3   27

CheckP0Right:                 ; check joy = right
  sta WSYNC;236---------------;   3   30    09    16
  lda #$80                    ;   2   00    00    00
  bit SWCHA                   ;   4   02
  bne CheckP0Button           ; 2/3   06          if joy != right, skip
  lda JetXPos                 ;   3   08
  cmp #113                    ;   2   11
  bpl NoInput                 ; 2/3   13
  inc JetXPos                 ;   5   15          else, increment JetXPos

  lda #<JetSpriteTurn         ;   2   20          set jet sprite pointer to 'turning' sprite
  sta JetSpritePtr            ;   3   22
  lda #>JetSpriteTurn         ;   2   25
  sta JetSpritePtr+1          ;   3   27

CheckP0Button:
  sta WSYNC;237---------------;   3   30    09    16
  lda #$80                    ;   2   00    00    00
  bit INPT4                   ;   3   02
  bne NoInput                 ; 2/3   05
  lda JetXPos                 ;   3   07
  sta MissileXPos             ;   3   10
  lda JetYPos                 ;   3   13
  sta MissileYPos             ;   3   16

NoInput:
  sta WSYNC;238---------------;   3   19    08    
  jsr ResetBGPF               ;   6   00    00    reset background

; check collisions
CheckP0P1Collision:           ;                   check if jet and bomber have collided
  sta WSYNC;239---------------;   3   22*
  lda #$80                    ;   2   00
  bit CXPPMM                  ;   3   02
  beq NoCollision             ; 2/3   05          if no, skip
  jsr GameOver                ;   6   07          else, call game over subroutine

NoCollision:
  sta WSYNC;240---------------;   3   32*   08
  sta CXCLR                   ;   3   00    00    clear all collision registers

  ; uses 12 scanlines

;--------------------------------------------------------
; Overscan (Cont.)
;--------------------------------------------------------

  ldx #18                     ;   2   57    33      (30 - 12 = 18 scanlines)
LoopOverscan:
  dex                         ;   2   59    35
  sta WSYNC;241,258-----------;   3   61    37
  bne LoopOverscan            ; 2/3   00    00

  jmp NextFrame               ;   3   02

;--------------------------------------------------------
; Subroutines
;--------------------------------------------------------

  org $FF15                   ; set at end of rom

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
ResetBGPF subroutine           ; (16 cycles total)
  lda #$84                    ; 2 set background color
  sta BackgroundColor         ; 3
  lda #$C2                    ; 2 set playfield color
  sta PlayfieldColor          ; 3
  rts                         ; 6

  ; game over subroutine
GameOver subroutine           ; (19 cycles total)
  lda #$40                    ; 2
  sta BackgroundColor         ; 3
  sta PlayfieldColor          ; 3
  lda #0                      ; 2
  sta Score                   ; 3
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