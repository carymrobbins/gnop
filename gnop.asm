.segment "HEADER"

    .byte   "NES", $1A      ; iNES header identifier
    .byte   2               ; 2x 16KB PRG code
    .byte   1               ; 1x 8KB CHR data
    .byte   $01, $00        ; mapper 0, vertical mirroring

.segment "STARTUP"

.segment "ZEROPAGE"

ctrlOne: .res 1
ctrlTwo: .res 1
ballAngle: .res 1       ; bits : nul nul nul nul up down left right
paddle1Top: .res 1
paddle1Bottom: .res 1
paddle2Top: .res 1
paddle2Bottom: .res 1
paddle1Movement: .res 1
paddle2Movement: .res 1
paddleSpeed: .res 1
ballSpeedX: .res 1
ballSpeedY: .res 1
paddle1Score: .res 1
paddle2Score: .res 1
paddleTurn: .res 1      ; $01 is paddle1, $02 is paddle2
turnPause: .res 1

.segment "CODE"

; CONSTANTS
PPU_STAT = $2002
PPU_ADDR = $2006
PPU_DATA = $2007

CTRL_PORT = $4016

BUTTON_A      = $80      ; Use AND to test ctrlOne or ctrlTwo against
BUTTON_B      = $40      ; these BUTTON constants to see which are
BUTTON_SELECT = $20      ; being pressed ... example below ...
BUTTON_START  = $10      ; STA ctrlOne
BUTTON_UP     = $08      ; AND #BUTTON_A ; don't forget to use the #
BUTTON_DOWN   = $04      ; BEQ notPressed
BUTTON_LEFT   = $02      ; BNE isPressed
BUTTON_RIGHT  = $01

SPRITE_RAM   = $0200

PADDLE1_YPOS = SPRITE_RAM + 0
PADDLE1_TILE = SPRITE_RAM + 1
PADDLE1_ATTR = SPRITE_RAM + 2
PADDLE1_XPOS = SPRITE_RAM + 3 ; repeats 3 more times

PADDLE2_YPOS = SPRITE_RAM + 16
PADDLE2_TILE = SPRITE_RAM + 17
PADDLE2_ATTR = SPRITE_RAM + 18
PADDLE2_XPOS = SPRITE_RAM + 19 ; repeats 3 more times

BALL_YPOS = SPRITE_RAM + 32
BALL_TILE = SPRITE_RAM + 33
BALL_ATTR = SPRITE_RAM + 34
BALL_XPOS = SPRITE_RAM + 35 ; that's it for this one

TOTAL_SPRITES = $24 ; hex for 36 ( 16 + 16 + 4 )

MOVING_UP = $08
MOVING_DOWN = $04
MOVING_LEFT = $02
MOVING_RIGHT = $01

LEFT_WALL = $02
RIGHT_WALL = $F6
TOP_WALL = $08
BOTTOM_WALL = $DF

PADDLE_SIZE = $20

PADDLE1_X_LIMIT = $10
PADDLE2_X_LIMIT = $E8
PADDLE_Y_TOP_LIMIT = $08
PADDLE_Y_BOTTOM_LIMIT = $C7

PADDLE1_TURN = $01
PADDLE2_TURN = $02

BALL_DEFAULT_SPEED_X = $02
BALL_DEFAULT_SPEED_Y = $01

;;;;;;;;;;;;;;;

RESET:
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs

  JSR vblankWait

clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0200, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0300, x
  INX
  BNE clrmem

  JSR vblankWait
  JSR loadPalettes
  JSR loadSprites
  JSR initializeVariables

Forever:
  JMP Forever     ;jump back to Forever, infinite loop

NMI:
  JSR startNMI
  JSR readCtrl
  JSR movePaddles
  JSR moveBall
  JSR updateSprites

  RTI             ; return from interrupt

;;;;;;;;;;;;;;

vblankWait:
  BIT PPU_STAT
  BPL vblankWait
  RTS

startNMI:
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer
  RTS

initializeVariables:
  LDA #MOVING_DOWN
  ORA #MOVING_RIGHT
  STA ballAngle     ; ballAngle = MOVING_DOWN | MOVING_LEFT;
  LDA #$01
  STA paddleSpeed
  LDA #BALL_DEFAULT_SPEED_X
  STA ballSpeedX
  LDA #BALL_DEFAULT_SPEED_Y
  STA ballSpeedY
  STA paddleTurn
  STA turnPause
  RTS

loadPalettes:
  LDA PPU_STAT             ; read PPU status to reset the high/low latch
  LDA #$3F
  STA PPU_ADDR             ; write the high byte of $3F00 address
  LDA #$00
  STA PPU_ADDR             ; write the low byte of $3F00 address
  LDX #$00                ; X = 0
  loadPalettes_Loop:      ; do {
    LDA palette, x        ;   A = *(palette + X)
    STA PPU_DATA          ;   write to PPU
    INX                   ;   ++X
    CPX #$20              ;
    BNE loadPalettes_Loop ; } while( X != 0x20 );
  RTS

loadSprites:
  LDX #$00                ; X = 0
  loadSprites_Loop:       ; do {
    LDA sprites, x        ;   A = *(sprites + X)
    STA SPRITE_RAM, x     ;   *(SPRITE_RAM + X) = A
    INX                   ;   ++X
    CPX #TOTAL_SPRITES    ;
    BNE loadSprites_Loop  ; } while( X != TOTAL_SPRITES )
  LDA #%10000000   ; enable NMI, sprites from Pattern Table 1
  STA $2000
  LDA #%00010000   ; enable sprites
  STA $2001
  RTS

readCtrl:
  LDA #$01
  STA CTRL_PORT
  LDA #$00
  STA CTRL_PORT ; now both ctrl buttons are latched
  LDX #$08 ; initialize X for loop
  readCtrlOne_Loop:
    LDA CTRL_PORT
    LSR A                   ; bit 0 -> CF
    ROL ctrlOne             ; bit 0 <- CF
    DEX
    BNE readCtrlOne_Loop
  LDX #$08 ; initialize X for loop
  readCtrlTwo_Loop:
    LDA CTRL_PORT+1
    LSR A                   ; bit 0 -> CF
    ROL ctrlTwo             ; bit 0 <- CF
    DEX
    BNE readCtrlTwo_Loop
  RTS

resetBall:
  LDA #$00
  STA ballSpeedX
  STA ballSpeedY
  LDA #$01
  STA turnPause
  LDA ballAngle
  EOR #$0F ; invert angle
  STA ballAngle
  LDA #$80
  STA BALL_XPOS
  STA BALL_YPOS
  RTS

movePaddles:
; reset paddle movement variables
  LDA #00;
  STA paddle1Movement
  STA paddle2Movement
; PADDLE 1 - react to button events
  if_CtrlOneUp:
    LDA ctrlOne
    AND #BUTTON_UP
    BEQ endif_CtrlOneUp         ; if( ctrlOne & BUTTON_UP ) {
    LDA #MOVING_UP              ;
    STA paddle1Movement         ;   paddle1Movement = MOVING_UP;
    LDA PADDLE1_YPOS
    SEC
    SBC paddleSpeed
    STA PADDLE1_YPOS            ;   *PADDLE1_YPOS -= paddleSpeed;
    CMP #PADDLE_Y_TOP_LIMIT     ;
    BCS endif_CtrlOneUp         ;   if( *PADDLE1_YPOS < TOP_LIMIT  ) {
    LDA #PADDLE_Y_TOP_LIMIT     ;     *PADDLE1_YPOS = TOP_LIMIT;
    STA PADDLE1_YPOS            ;   }
  endif_CtrlOneUp:              ; }
  if_CtrlOneDown:
    LDA ctrlOne
    AND #BUTTON_DOWN            ;
    BEQ endif_CtrlOneDown       ; if( ctrlOne & BUTTON_DOWN ) {
    LDA #MOVING_DOWN            ;
    STA paddle1Movement         ;   paddle1Movement = MOVING_DOWN;
    LDA PADDLE1_YPOS
    CLC
    ADC paddleSpeed
    STA PADDLE1_YPOS            ;   *PADDLE1_YPOS += paddleSpeed;
    LDA #PADDLE_Y_BOTTOM_LIMIT
    CMP PADDLE1_YPOS            ;
    BCS endif_CtrlOneDown       ;   if( BOTTOM_LIMIT < *PADDLE1_YPOS ) {
    STA PADDLE1_YPOS            ;     *PADDLE1_YPOS = BOTTOM_LIMIT;
  endif_CtrlOneDown:            ;   }
                                ; }
; PADDLE 2 - react to button events
  if_CtrlTwoUp:
    LDA ctrlTwo
    AND #BUTTON_UP
    BEQ endif_CtrlTwoUp         ; if( ctrlTwo & BUTTON_UP ) {
    LDA #MOVING_UP              ;
    STA paddle2Movement         ;   paddle2Movement = MOVING_UP;
    LDA PADDLE2_YPOS
    SEC
    SBC paddleSpeed
    STA PADDLE2_YPOS            ;   *PADDLE2_YPOS -= paddleSpeed;
    CMP #PADDLE_Y_TOP_LIMIT     ;
    BCS endif_CtrlTwoUp         ;   if( *PADDLE2_YPOS < TOP_LIMIT  ) {
    LDA #PADDLE_Y_TOP_LIMIT     ;     *PADDLE2_YPOS = TOP_LIMIT;
    STA PADDLE2_YPOS            ;   }
  endif_CtrlTwoUp:              ; }
  if_CtrlTwoDown:
    LDA ctrlTwo
    AND #BUTTON_DOWN            ;
    BEQ endif_CtrlTwoDown       ; if( ctrlTwo & BUTTON_DOWN ) {
    LDA #MOVING_DOWN            ;
    STA paddle2Movement         ;   paddle2Movement = MOVING_DOWN;
    LDA PADDLE2_YPOS
    CLC
    ADC paddleSpeed
    STA PADDLE2_YPOS            ;   *PADDLE2_YPOS += paddleSpeed;
    LDA #PADDLE_Y_BOTTOM_LIMIT
    CMP PADDLE2_YPOS            ;
    BCS endif_CtrlTwoDown       ;   if( BOTTOM_LIMIT < *PADDLE2_YPOS ) {
    STA PADDLE2_YPOS            ;     *PADDLE2_YPOS = BOTTOM_LIMIT;
  endif_CtrlTwoDown:            ;   }
                                ; }
  RTS

moveBall:
  LDA turnPause
  BEQ endif_TurnPause           ; if( turnPause ) {
  LDA paddleTurn
  AND #$01
  BEQ else_Paddle1Turn          ;   if( paddleTurn & 1 ) {
  LDA ctrlOne                   ;     A = ctrlOne;
  JMP endif_Paddle1Turn         ;
  else_Paddle1Turn:             ;   } else {
  LDA ctrlTwo                   ;     A = ctrlTwo;
  endif_Paddle1Turn:            ;   }
  AND #BUTTON_START
  BEQ else_BallServed           ;     if( A & BUTTON_START ) {
  LDA #$00
  STA turnPause                 ;       turnPause = 0;
  INC paddleTurn                ;       ++paddleTurn;
  LDA #BALL_DEFAULT_SPEED_X
  STA ballSpeedX                ;       ballSpeedX = BALL_DEFAULT_SPEED_X;
  LDA #BALL_DEFAULT_SPEED_Y
  STA ballSpeedY                ;       ballSpeedY = BALL_DEFAULT_SPEED_Y;
  JMP endif_BallServed          ;
  else_BallServed:              ;     } else {
  RTS                           ;       return;
  endif_BallServed:             ;     }
  endif_TurnPause:              ;  }
  LDA ballAngle
  AND #MOVING_LEFT
  BEQ endif_ballLeft
  if_ballLeft:
    LDA BALL_XPOS
    SEC
    SBC ballSpeedX
    STA BALL_XPOS           ; BALL_XPOS -= ballSpeedX
    CMP #LEFT_WALL
    BCS notPastLeftWall     ; if( BALL_XPOS < LEFT_WALL ) {
    JSR resetBall           ;   resetBall();
    RTS                     ;   return;
    notPastLeftWall:        ; }
    CMP #PADDLE1_X_LIMIT    ; if( BALL_XPOS < PADDLE1_X_LIMIT )
    BNE endif_ballLeft      ;   goto endif
    LDA BALL_YPOS
    CMP paddle1Top          ; if( BALL_YPOS < paddle1Top )
    BCC endif_ballLeft      ;   goto endif
    CMP paddle1Bottom       ; if( BALL_YPOS < paddle1Bottom )
    BCS endif_ballLeft      ;   goto endif
    ; if ball hits paddle, bounce!
    LDA #MOVING_LEFT        ; here we bitmask the old MOVING and set it to the new one
    EOR #$FF                ; A = ~MOVING_LEFT
    AND ballAngle           ; A &= ballAngle
    ORA #MOVING_RIGHT       ; A |= MOVING_RIGHT
    STA ballAngle           ; ballAngle = A
    if_paddle1Spin:
      LDA paddle1Movement     ; if( paddle1Movement == 0 )
      BEQ endif_paddle1Spin   ;   goto endif
      LDA ballAngle
      AND paddle1Movement
      AND #MOVING_UP          ; if( ballAngle & paddle#Movement == MOVING_UP )
      BNE endif_paddle1Spin   ;   goto endif
      LDA ballAngle
      AND paddle1Movement
      AND #MOVING_DOWN        ; if( ballAngle & paddle#Movement == MOVING_DOWN )
      BNE endif_paddle1Spin   ;   goto endif
      INC ballSpeedY          ; ++ballSpeedY //apply spin!
      JMP endif_ballLeft      ; JMP to avoid resetting ballSpeedY
    endif_paddle1Spin:
    LDA #$01
    STA ballSpeedY
  endif_ballLeft:

  LDA ballAngle
  AND #MOVING_RIGHT
  BEQ endif_ballRight
  if_ballRight:
    LDA BALL_XPOS
    CLC
    ADC ballSpeedX
    STA BALL_XPOS
    CMP #RIGHT_WALL
    BCC notPastRightWall
    JSR resetBall
    RTS
    notPastRightWall:
    CMP #PADDLE2_X_LIMIT
    BNE endif_ballRight
    LDA BALL_YPOS
    CMP paddle2Top
    BCC endif_ballRight
    CMP paddle2Bottom
    BCS endif_ballRight
    ; if ball hits paddle, bounce!
    LDA #MOVING_RIGHT
    EOR #$FF
    AND ballAngle
    ORA #MOVING_LEFT
    STA ballAngle
    if_paddle2Spin:
      LDA paddle2Movement
      BEQ endif_paddle2Spin   ; if( paddle#Movement == 0 ) goto endif
      LDA ballAngle
      AND paddle2Movement
      AND #MOVING_UP          ; if( ballAngle & paddle#Movement == MOVING_UP )
      BNE endif_paddle2Spin   ;   goto endif
      LDA ballAngle
      AND paddle2Movement
      AND #MOVING_DOWN        ; if( ballAngle & paddle#Movement == MOVING_DOWN )
      BNE endif_paddle2Spin   ;   goto endif
      INC ballSpeedY          ; ++ballSpeedY //apply spin!
      JMP endif_ballLeft      ; JMP to avoid resetting ballSpeedY
    endif_paddle2Spin:
    LDA #$01
    STA ballSpeedY
  endif_ballRight:

  LDA ballAngle
  AND #MOVING_UP
  BEQ endif_ballUp
  if_ballUp:
    LDA BALL_YPOS
    SEC
    SBC ballSpeedY
    STA BALL_YPOS
    CMP #TOP_WALL
    BCC if_bounceDown
    LDA BALL_XPOS
    start_paddle1BottomBounceTest:
      CMP PADDLE1_XPOS
      BNE start_paddle2BottomBounceTest
      LDA paddle1Bottom
      CMP BALL_YPOS
      BEQ if_bounceDown
    end_paddle1BottomBounceTest:
    start_paddle2BottomBounceTest:
      CMP PADDLE2_XPOS
      BNE endif_ballUp
      LDA paddle2Bottom
      CMP BALL_YPOS
      BNE endif_ballUp
    end_paddle2BottomBounceTest:
    if_bounceDown:
      LDA #MOVING_UP
      EOR #$FF
      AND ballAngle  ; turn off BALL_UP bit
      ORA #MOVING_DOWN ; turn on BALL_DOWN bit
      STA ballAngle
    endif_bounceDown:
  endif_ballUp:

  LDA ballAngle
  AND #MOVING_DOWN
  BEQ endif_ballDown
  if_ballDown:
    LDA BALL_YPOS
    CLC
    ADC ballSpeedY
    STA BALL_YPOS
    CMP #BOTTOM_WALL
    BCS if_bounceUp
    LDA BALL_XPOS
    start_paddle1TopBounceTest:
      CMP PADDLE1_XPOS
      BNE start_paddle2TopBounceTest
      LDA paddle1Top
      CMP BALL_YPOS
      BEQ if_bounceUp
    end_paddle1TopBounceTest:
    start_paddle2TopBounceTest:
      CMP PADDLE2_XPOS
      BNE endif_ballDown
      LDA paddle2Top
      CMP BALL_YPOS
      BNE endif_ballDown
    end_paddle2TopBounceTest:
    if_bounceUp:
      LDA #MOVING_DOWN
      EOR #$FF       ; get inverse of BALL_DOWN
      AND ballAngle  ; turn off BALL_DOWN bit
      ORA #MOVING_UP ; turn on BALL_UP bit
      STA ballAngle
    endif_bounceUp:
  endif_ballDown:
  RTS

updateSprites:
; PADDLE 1
  LDA PADDLE1_YPOS      ; update sprites relative to "home" sprite
  CLC
  ADC #$08
  STA PADDLE1_YPOS + 4
  CLC
  ADC #$08
  STA PADDLE1_YPOS + 8
  CLC
  ADC #$08
  STA PADDLE1_YPOS + 12
; PADDLE 2
  LDA PADDLE2_YPOS      ; update sprites relative to "home" sprite
  CLC
  ADC #$08
  STA PADDLE2_YPOS + 4
  CLC
  ADC #$08
  STA PADDLE2_YPOS + 8
  CLC
  ADC #$08
  STA PADDLE2_YPOS + 12

  ;update paddle variables
  LDA PADDLE1_YPOS
  SEC
  SBC #$08
  STA paddle1Top
  CLC
  ADC #$28
  STA paddle1Bottom
  LDA PADDLE2_YPOS
  SEC
  SBC #$08
  STA paddle2Top
  CLC
  ADC #$28
  STA paddle2Bottom

  RTS

palette:
;background
  .byte $0F,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3A,$3B,$3C,$3D,$3E,$0F
;sprites
  .byte $0F,$08,$28,$18,$31,$02,$38,$3C,$0F,$1C,$15,$14,$31,$02,$38,$3C

sprites:
  ; PADDLE 1
     ;vert tile attr horiz
  .byte $80, $85, $00, $08
  .byte $88, $86, $00, $08
  .byte $90, $86, $00, $08
  .byte $98, $86, $00, $08
  ; PADDLE 2
     ;vert tile attr horiz
  .byte $80, $85, $00, $F0
  .byte $88, $86, $00, $F0
  .byte $90, $86, $00, $F0
  .byte $98, $86, $00, $F0
  ; BALL
     ;vert tile attr horiz
  .byte $80, $75, $00, $80

.segment "VECTORS"

  .word 0, 0, 0    ;Unused, but needed to advance PC to $fffa.
  .word NMI        ;when an NMI happens (once per frame if enabled) the
                   ;processor will jump to the label NMI:
  .word RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .word 0          ;external interrupt IRQ (not currently used).

.segment "CHARS"

  .incbin "mario.chr"   ;includes 8KB graphics file from SMB1
