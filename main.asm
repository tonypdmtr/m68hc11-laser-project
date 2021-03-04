;*******************************************************************************
; DESCRIPTION         : THIS PROGRAM IS A LASER PROJECT PROGRAM
; NAMES               : RENE MOISE KWIBUKA & DAVID BURMEIER
; START DATE SOFTWARE : OCTOBER 10, 2015
; END DATE SOFTWARE   : NOVEMBER 05, 2015
; START DATE HARDWARE : JANUARY 4TH, 2016
; END DATE HARDWARE   : MARCH 10TH, 2016
; EDITED ALONG WITH HARDWARE CONSTRUCTION: JANUARY - MARCH 2016
;*******************************************************************************

#ifdef ...
; Macros to help locate hardcoded values other than -1, 0, and 1
common              macro
                    mset      #
                    mset      0,#Hint Hardcoded value: ~macro~ ~1~
          #ifnum ~#1~
            #if ~#1~ > 1
                    ~text~
            #else if ~#1~ < -1
                    ~text~
            #endif
          #endif
;                   #Hint     ~macro~ ~1~
                    ~macro~   ~1~
                    endm
          ;--------------------------------------
lda                 macro
                    @common   ~@~
                    endm
          ;--------------------------------------
sta                 macro
                    @common   ~@~
                    endm
          ;--------------------------------------
ldb                 macro
                    @common   ~@~
                    endm
          ;--------------------------------------
stb                 macro
                    @common   ~@~
                    endm
          ;--------------------------------------
ldx                 macro
                    @common   ~@~
                    endm
          ;--------------------------------------
stx                 macro
                    @common   ~@~
                    endm
          ;--------------------------------------
ldy                 macro
                    @common   ~@~
                    endm
          ;--------------------------------------
sty                 macro
                    @common   ~@~
                    endm
          ;--------------------------------------
lds                 macro
                    @common   ~@~
                    endm

                    #MCF
#endif
;*******************************************************************************
; CONSTANTS
;*******************************************************************************

REGS                equ       $1000
D_ACONV             equ       REGS+$39            ; OPTION
ST_REG              equ       REGS+$30            ; A/D CONTROL/STATUS REGISTER
ADR_READ            equ       REGS+$31            ; READ CONVERTED ANALOG FROM HERE

STACKTOP            equ       $FF                 ; STACKTOP VALUE

SHIFT_VAL           equ       18                  ; SHIFT MAX VALUE

OFF                 equ       $A000               ; LASER OFF
ON                  equ       $A001               ; LASER ON

PIA_DDRA            equ       $B000               ; PIA DDRA
PIA_CRA             equ       $B001               ; PIA CRA
PIA_DDRB            equ       $B002               ; PIA DRRB
PIA_CRB             equ       $B003               ; PIA CRB

LCD_0               equ       $C000               ; INITIALIZE LCD REGISTER 0
LCD_1               equ       $C001               ; SET LCD REGISTER 1

KEYBD               equ       $D000               ; KEYBOARD INPUT

LOGIC_A             equ       $E103
LOGIC_B             equ       $F101
LOGIC_C             equ       $E100

;*******************************************************************************
                    #RAM
;*******************************************************************************

counter             rmb       1                   ; counter
buffer              rmb       16                  ; buffer location
mode_loc            rmb       1                   ; mode location
shift_loc           rmb       1                   ; shift location
x_offset            rmb       1                   ; X_OFFSET LOCATION
delcount            rmb       1                   ; delay counter holder

;*******************************************************************************
                    #ROM      $E000               ; WHERE TO STORE THE PROGRAM
;*******************************************************************************

;*******************************************************************************

INIT_LCD            proc
          ;--------------------------------------
          ; INITIALIZING THE ADPU bit 7 ($1039).
          ; Allow 100 us before initializing another command.
          ;--------------------------------------
                    ldx       #D_ACONV            ; load x with $1039.
                    bset      ,x,%10000000        ; SET BIT 7 OF DIG ANAL CONVER

                    ldx       #LCD_0              ; LOAD X WITH LCD_0 VALUE
                    brset     ,x,$80,*            ; BRANCH BACK HERE IF BF = 1
                    lda       #$01                ; LOAD A WITH $01
                    sta       LCD_0               ; STORE IN LCD_0

                    brset     ,x,$80,*            ; BRANCH BACK HERE IF BF = 1
                    lda       #$30                ; LOAD A WITH $30
                    sta       LCD_0               ; STORE IN LCD_0

                    brset     ,x,$80,*            ; BRANCH BACK HERE IF BF = 1
                    lda       #$08                ; LOAD A WITH $08
                    sta       LCD_0               ; STORE IN LCD_0

                    brset     ,x,$80,*            ; BRANCH BACK HERE IF BF = 1
                    lda       #$06                ; LOAD A WITH $06
                    sta       LCD_0               ; STORE IN LCD_0

                    brset     ,x,$80,*            ; BRANCH BACK HERE IF BF = 1
                    lda       #$3C                ; LOAD A WITH $3C
                    sta       LCD_0               ; STORE IN LCD_0

                    brset     ,x,$80,*            ; BRANCH BACK HERE IF BF = 1
                    lda       #$0F                ; LOAD A WITH $0F
                    sta       LCD_0               ; STORE IN LCD_0

                    brset     ,x,$80,*            ; BRANCH BACK HERE IF BF = 1
                    rts                           ; ONLY RETURN IF NOT DONE WITH INITIALIZING

;*******************************************************************************
; INITIALIZATION
; Since we are using subroutines and interrupts, some initializations are a must.
; Among them are to clear index, initialize the stack,and initialize the buffer.

Start               proc
                    cli                           ; Enable interrupts
                    lda       #1
                    sta       mode_loc            ; Select mode 1
                    clr       shift_loc
                    clr       x_offset
                    lds       #STACKTOP
                    lda       #$FF
                    ldy       #buffer
                    sta       buffer
                    sta       buffer+1
                    sta       buffer+2
                    sta       buffer+3
                    bsr       INIT_LCD            ; Initialize LCD
;                   bra       PIA_INIT

;*******************************************************************************
; Initialize PIA

PIA_INIT            proc
          ;-------------------------------------- ; INITIALIZE PIA A (X VALUES)
                    ldx       #PIA_CRA            ; LOAD PIA CRA VALUE TO X
                    bclr      ,x,%00000100        ; SET BIT 2 OF CRA TO 0 (ACCESS DDRA)
                    lda       #$FF                ; LOAD ACC A WITH FF
                    sta       PIA_DDRA            ; STORE FF IN B000 (SET DDRA TO FF)
                    ldx       #PIA_CRA            ; LOAD PIA CRA TO X
                    bset      ,x,%00000100        ; SET BIT 2 OF CRA TO 1 (ACCESS DRA)
          ;-------------------------------------- ; INITIALIZE PIA B  (Y VALUES)
                    ldx       #PIA_CRB            ; LOAD CRB VALUE IN X
                    bclr      ,x,%00000100        ; SET BIT 2 OF CRB TO 0 (ACCESS DDRB)
                    lda       #$FF                ; LOAD ACC A WITH FF
                    ldx       #PIA_CRB            ; LOAD CRB VALUE IN X
                    sta       PIA_DDRB            ; STORE FF IN B002 (SET DDRB TO FF)
                    bset      ,x,%00000100        ; SET BIT 2 OF CRB TO 1 (ACCESS DRB)
                    jsr       DELAY               ; TO ENSURE COMPLETE ADPU INITIALIZAION, DELAY
          ;-------------------------------------- ; ANOTHER INITIALIZATION OF D/A AFTER AT LEAST 100us
                    ldx       #ST_REG             ; LOAD X WITH $1030
                    bset      ,x,%00100000        ; SET BIT 5
;                   bra       MAIN

;*******************************************************************************
; MAIN
;*******************************************************************************

MAIN                proc
                    lda       ADR_READ            ; LOAD A WITH A/D CONVERTER VALUE
                    sta       LOGIC_A             ; PROGRAM IT OUT AT $E103 (FOR THE SAKE OF LOGIC ANALYZER)
          ;--------------------------------------
                    lda       #1
                    sta       counter             ; STORE A 1 IN COUNTER LOCATION
                    ldx       #mode_loc           ; LOADING X WITH MODE_LOC
                    cmpa      ,x                  ; CHECK IF MODE IS EQUAL TO COUNTER (1 SO FAR)
                    beq       MAIN
          ;--------------------------------------
                    inca                          ; make counter 2
                    sta       counter
                    cmpa      ,x                  ; check if mode is 2
                    beq       MODE_2
          ;--------------------------------------
                    inca                          ; make counter 3
                    sta       counter
                    cmpa      ,x                  ; check if mode is 3
                    beq       MODE_3
          ;--------------------------------------
                    inca                          ; make counter 4
                    sta       counter             ; load 4 in a
                    cmpa      ,x                  ; check if mode is 4
                    bne       MAIN
          ;--------------------------------------
                    jmp       MODE_4

;*******************************************************************************
; MODE_2
;*******************************************************************************

MODE_2              proc
                    lda       #$FF
                    cmpa      ,y                  ; CHECK IF VALUE IN BUFFER IS $FF
                    beq       MAIN                ; IF BUFFER CONTAINS $FF, GO BACK TO MAIN
                    ldb       ,y
                    lda       #32
                    mul
                    addd      #DIG_0              ; ADD WHAT IS IN D WITH THE VALUE AT LASER
                    xgdx

Loop@@              lda       #$FF
                    cmpa      ,x                  ; compare the Y coordinate to $FF
                    beq       OffAdjust@@         ; if true, turn off the laser
                    tst       ,x                  ; compare the Y coordinate to $00
                    beq       Off@@               ; if true, turn off the laser
                    lda       #1
                    cmpa      ,x                  ; compare the Y coordinate to $01
                    beq       On@@                ; if same, turn laser on
                    bsr       DISPLAY
                    bra       Loop@@

Off@@               bsr       LASOFF
                    inx
                    bra       Loop@@

On@@                bsr       LASON
                    inx
                    bra       Loop@@

OffAdjust@@         bsr       LASOFF

                    ldx       #x_offset
                    lda       ,x
                    adda      #64
                    cmpa      #255
                    blo       AdjBuf@@
                    clra

AdjBuf@@            sta       ,x                  ; reset the X_OFFSET
                    iny                           ; increment the current buffer pointer
                    lda       ,y
                    cmpa      #$FF
                    beq       RegY@@
                    cpy       #$24                ; compare it with the value 24
                    blo       GoMain
                    bra       ResetAndMain@@

RegY@@              clr       x_offset
ResetAndMain@@      ldy       #buffer             ; else reset it with 20
GoMain              jmp       MAIN

;*******************************************************************************
; MODE_3
;*******************************************************************************

MODE_3              proc
                    ldx       #MODE_3_STR
Loop@@              lda       #$FF
                    cmpa      ,x                  ; compare the Y coordinate to $FF
                    beq       GOOFFMAIN           ; if true, turn off the laser
                    tst       ,x                  ; compare the Y coordinate to $00
                    beq       _1@@                ; if true, turn off the laser
                    lda       #1
                    cmpa      ,x                  ; compare the Y coordinate to $01
                    beq       _2@@                ; turn laser on
                    bsr       DISPLAY
                    bra       Loop@@

GOOFFMAIN           bsr       LASOFF
                    bra       GoMain

_1@@                bsr       LASOFF
                    inx
                    bra       Loop@@

_2@@                bsr       LASON
                    inx
                    bra       Loop@@

;*******************************************************************************

DISPLAY             proc
                    lda       ,x
                    adda      x_offset            ; ADD X TO OFFSET VALUE
                    sta       PIA_DDRA            ; STORE IT AT PIA_DDRA
                    inx
                    lda       ,x                  ; load Y value
                    sta       PIA_DDRB
                    bsr       DELAY
                    inx
                    rts

;*******************************************************************************

LASON               proc
                    lda       #$FF                ; load 0 in A
                    sta       ON                  ; store to turn on
                    bra       DELAY

;*******************************************************************************

LASOFF              proc
                    lda       #$FF                ; load 0 in A
                    sta       OFF                 ; store to turn off
;                   bra       DELAY

;*******************************************************************************

DELAY               proc
                    clr       delcount
                    lda       #250

_1@@                cmpa      delcount            ; COMPARE 14 TO THE VAL
                    beq       _2@@                ; IF 14 GO RETURN
                    inc       delcount
                    bra       _1@@

_2@@                clr       delcount
                    lda       ADR_READ

_3@@                cmpa      delcount            ; COMPARE 14 TO THE VAL
                    beq       Done@@              ; IF 14 GO RETURN
                    inc       delcount
                    nop:7                         ; waste some cycles
                    bra       _3@@

Done@@              equ       :AnRTS

;*******************************************************************************
; MODE_4
;*******************************************************************************

MODE_4              proc
                    ldx       #MODE4_PATTERN      ; LOAD X WITH MODE_3_STR LOCATION
Loop@@              lda       #$FF
                    cmpa      ,x                  ; compare the Y coordinate to $FF
                    beq       GOOFFMAIN           ; if true, turn off the laser
                    tst       ,x                  ; compare the Y coordinate to $00
                    beq       Off@@               ; if true, turn off the laser
                    lda       #1
                    cmpa      ,x                  ; compare the Y coordinate to $01
                    beq       On@@                ; turn laser on
                    bsr       DISPLAY
                    bra       Loop@@

Off@@               bsr       LASOFF
                    inx
                    bra       Loop@@

On@@                bsr       LASON
                    inx
                    bra       Loop@@

;*******************************************************************************
; INTERRUPT BLOCK
;*******************************************************************************

ISR_Handler         proc
                    ldb       KEYBD               ; READ FROM KEYBOARD
                    cmpb      #18                 ; COMPARE THE KEY VALUE TO MODE
                    beq       Mode@@              ; GO UPDATE MODE
                    cmpb      #19                 ; ELSE COMPARE TO 19
                    beq       Shift@@             ; IF EQUAL GO UPDATE SHIFT
                    addb      shift_loc           ; ELSE ADD B TO THE SHIFT VALUE
                    clr       shift_loc
                    ldy       #buffer

CheckBuf@@          lda       #$FF
                    cmpa      ,y                  ; COMPARE A WITH WHAT IS POINTED TO BY Y
                    bne       IncBuf@@
                    stb       ,y                  ; STORE THE KEY IN BUFFER
                    ldx       #ASCII_TABLE
                    abx
                    lda       ,x
                    brset     ,x,$80,*            ; BRANCH BACK HERE IF BF = 1
                    sta       LCD_1               ; STORE X AT LCD_1
                    brset     ,x,$80,*            ; BRANCH BACK HERE IF BF = 1
                    bra       Done@@

IncBuf@@            iny
                    cpy       #$24                ; COMPARE Y TO 24
                    bne       CheckBuf@@
                    bra       Done@@

Mode@@              lda       counter
                    sta       LOGIC_B
                    lda       shift_loc           ; LOAD A WITH WHAT IS IN SHIFT_L
                    bne       Clear@@             ; BRANCH NOT EQUAL TO CLEAR
                    clr       shift_loc
                    inc       mode_loc
                    lda       mode_loc
                    sta       LOGIC_C
                    cmpa      #5                  ; COMPARE THE MODE TO 5
                    bne       Done@@
                    clr       mode_loc
                    inc       mode_loc
                    bra       Done@@

Shift@@             lda       shift_loc           ; LOAD A WITH THE SHIFT VALUE
                    beq       Make20@@            ; IF 0, GO MAKE IT 20
                    clra
                    sta       shift_loc           ; STORE THE SHIFT VALUE BACK IN THE MEM LOC
                    bra       Done@@

Make20@@            lda       #SHIFT_VAL          ; LOAD A WITH 20
                    sta       shift_loc           ; STORE THE SHIFT VALUE BACK IN SHIFT_L
                    bra       Done@@              ; GO RETURN

Clear@@             jsr       INIT_LCD
                    clr       x_offset            ; CLEAR X_OFFSET
                    clr       shift_loc           ; CLEAR SHIFT
                    ldx       #buffer             ; LOAD THE ADDRESS OF BUFFER IN X
                    lda       #$FF                ; LOAD VALUE FF IN ACC A
CLoop@@             sta       ,x                  ; STORE WHAT IS STORED IN ACC IN THE BUFFER
                    inx                           ; INCREMENT X
                    cpx       #$24                ; COMPARE WITH THE END OF THE BUFFER
                    bne       CLoop@@             ; CLEAR
Done@@              rti                           ; RETURN FROM THE INTERRUPT

;*******************************************************************************
; CONCLUDING LINES OF CODE
;*******************************************************************************

          ;-------------------------------------- ; LCDSTRING
ASCII_TABLE         fcc       '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'
          ;-------------------------------------- ; MODE 3 LASER STRING
MODE_3_STR          fcb       5,5,01,5,251,251,251,251,5,5,5,251,251,00,05,251,01,251,5,00,$FF
          ;--------------------------------------
DIG_0               fcb       $B                  ; X
                    fcb       $A                  ; Y
                    fcb       $01                 ; ON
                    fcb       $B                  ; X
                    fcb       $FC                 ; Y
                    fcb       $3C                 ; X
                    fcb       $FC                 ; Y
                    fcb       $3C                 ; X
                    fcb       $A                  ; Y
                    fcb       $B                  ; X
                    fcb       $A                  ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
DIG_1               fcb       $9                  ; X
                    fcb       $9C                 ; Y
                    fcb       $01                 ; ON
                    fcb       $3C                 ; X
                    fcb       $FA                 ; Y
                    fcb       $3C                 ; X
                    fcb       $D                  ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
DIG_2               fcb       $E                  ; X
                    fcb       $B6                 ; Y
                    fcb       $01                 ; ON
                    fcb       $9                  ; X
                    fcb       $CF                 ; Y
                    fcb       $10                 ; X
                    fcb       $E2                 ; Y
                    fcb       $24                 ; X
                    fcb       $EF                 ; Y
                    fcb       $2E                 ; X
                    fcb       $F2                 ; Y
                    fcb       $38                 ; X
                    fcb       $E3                 ; Y
                    fcb       $3C                 ; X
                    fcb       $C6                 ; Y
                    fcb       $3C                 ; X
                    fcb       $AF                 ; Y
                    fcb       $35                 ; X
                    fcb       $9E                 ; Y
                    fcb       $2B                 ; X
                    fcb       $7F                 ; Y
                    fcb       $6                  ; X
                    fcb       $14                 ; Y
                    fcb       $3C                 ; X
                    fcb       $15                 ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
DIG_3               fcb       $C                  ; X
                    fcb       $C0                 ; Y
                    fcb       $01                 ; ON
                    fcb       $F                  ; X
                    fcb       $E0                 ; Y
                    fcb       $25                 ; X
                    fcb       $EA                 ; Y
                    fcb       $36                 ; X
                    fcb       $DE                 ; Y
                    fcb       $3B                 ; X
                    fcb       $B5                 ; Y
                    fcb       $35                 ; X
                    fcb       $8A                 ; Y
                    fcb       $20                 ; X
                    fcb       $7B                 ; Y
                    fcb       $33                 ; X
                    fcb       $62                 ; Y
                    fcb       $3A                 ; X
                    fcb       $54                 ; Y
                    fcb       $3B                 ; X
                    fcb       $38                 ; Y
                    fcb       $36                 ; X
                    fcb       $21                 ; Y
                    fcb       $23                 ; X
                    fcb       $C                  ; Y
                    fcb       $10                 ; X
                    fcb       $15                 ; Y
                    fcb       $D                  ; X
                    fcb       $32                 ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
DIG_4               fcb       $3C                 ; X
                    fcb       $EE                 ; Y
                    fcb       $01                 ; ON
                    fcb       $7                  ; X
                    fcb       $55                 ; Y
                    fcb       $3C                 ; X
                    fcb       $55                 ; Y
                    fcb       $3D                 ; X
                    fcb       $EC                 ; Y
                    fcb       $3C                 ; X
                    fcb       $9                  ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
DIG_5               fcb       $3A                 ; X
                    fcb       $F3                 ; Y
                    fcb       $01                 ; ON
                    fcb       $A                  ; X
                    fcb       $F3                 ; Y
                    fcb       $9                  ; X
                    fcb       $99                 ; Y
                    fcb       $19                 ; X
                    fcb       $95                 ; Y
                    fcb       $2B                 ; X
                    fcb       $8B                 ; Y
                    fcb       $39                 ; X
                    fcb       $7B                 ; Y
                    fcb       $3C                 ; X
                    fcb       $49                 ; Y
                    fcb       $37                 ; X
                    fcb       $3A                 ; Y
                    fcb       $29                 ; X
                    fcb       $23                 ; Y
                    fcb       $1E                 ; X
                    fcb       $1B                 ; Y
                    fcb       $13                 ; X
                    fcb       $11                 ; Y
                    fcb       $B                  ; X
                    fcb       $11                 ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
DIG_6               fcb       $3A                 ; X
                    fcb       $E2                 ; Y
                    fcb       $01                 ; ON
                    fcb       $18                 ; X
                    fcb       $B0                 ; Y
                    fcb       $8                  ; X
                    fcb       $5B                 ; Y
                    fcb       $8                  ; X
                    fcb       $E                  ; Y
                    fcb       $3A                 ; X
                    fcb       $E                  ; Y
                    fcb       $3A                 ; X
                    fcb       $60                 ; Y
                    fcb       $9                  ; X
                    fcb       $5F                 ; Y

                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
DIG_7               fcb       $8                  ; X
                    fcb       $F0                 ; Y
                    fcb       $01                 ; ON
                    fcb       $3D                 ; X
                    fcb       $F0                 ; Y
                    fcb       $D                  ; X
                    fcb       $F                  ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
DIG_8               fcb       $1F                 ; X
                    fcb       $7C                 ; Y
                    fcb       $01                 ; ON
                    fcb       $39                 ; X
                    fcb       $B1                 ; Y
                    fcb       $3B                 ; X
                    fcb       $D5                 ; Y
                    fcb       $1A                 ; X
                    fcb       $FA                 ; Y
                    fcb       $3                  ; X
                    fcb       $C6                 ; Y
                    fcb       $3A                 ; X
                    fcb       $4D                 ; Y
                    fcb       $25                 ; X
                    fcb       $F                  ; Y
                    fcb       $4                  ; X
                    fcb       $48                 ; Y
                    fcb       $1F                 ; X
                    fcb       $81                 ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
DIG_9               fcb       $3A                 ; X
                    fcb       $FA                 ; Y
                    fcb       $01                 ; ON
                    fcb       $B                  ; X
                    fcb       $FA                 ; Y
                    fcb       $B                  ; X
                    fcb       $AD                 ; Y
                    fcb       $3C                 ; X
                    fcb       $AD                 ; Y
                    fcb       $3C                 ; X
                    fcb       $F9                 ; Y
                    fcb       $3D                 ; X
                    fcb       $12                 ; Y
                    fcb       $D                  ; X
                    fcb       $12                 ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
LET_A               fcb       $4                  ; X
                    fcb       $4                  ; Y
                    fcb       $01                 ; ON
                    fcb       $1E                 ; X
                    fcb       $F9                 ; Y
                    fcb       $3D                 ; X
                    fcb       $3                  ; Y
                    fcb       $00                 ; OFF
                    fcb       $11                 ; X
                    fcb       $76                 ; Y
                    fcb       $01                 ; ON
                    fcb       $2E                 ; X
                    fcb       $77                 ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
LET_B               fcb       $9                  ; X
                    fcb       $9                  ; Y
                    fcb       $01                 ; ON
                    fcb       $A                  ; X
                    fcb       $F2                 ; Y
                    fcb       $3A                 ; X
                    fcb       $F0                 ; Y
                    fcb       $38                 ; X
                    fcb       $9B                 ; Y
                    fcb       $30                 ; X
                    fcb       $8C                 ; Y
                    fcb       $B                  ; X
                    fcb       $72                 ; Y
                    fcb       $19                 ; X
                    fcb       $64                 ; Y
                    fcb       $34                 ; X
                    fcb       $52                 ; Y
                    fcb       $39                 ; X
                    fcb       $9                  ; Y
                    fcb       $9                  ; X
                    fcb       $7                  ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
LET_C               fcb       $3C                 ; X
                    fcb       $F9                 ; Y
                    fcb       $01                 ; ON
                    fcb       $21                 ; X
                    fcb       $E4                 ; Y
                    fcb       $19                 ; X
                    fcb       $D8                 ; Y
                    fcb       $E                  ; X
                    fcb       $BE                 ; Y
                    fcb       $C                  ; X
                    fcb       $AA                 ; Y
                    fcb       $A                  ; X
                    fcb       $93                 ; Y
                    fcb       $9                  ; X
                    fcb       $6F                 ; Y
                    fcb       $9                  ; X
                    fcb       $39                 ; Y
                    fcb       $13                 ; X
                    fcb       $2E                 ; Y
                    fcb       $1E                 ; X
                    fcb       $20                 ; Y
                    fcb       $26                 ; X
                    fcb       $19                 ; Y
                    fcb       $3D                 ; X
                    fcb       $C                  ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
LET_D               fcb       $9                  ; X
                    fcb       $8                  ; Y
                    fcb       $01                 ; ON
                    fcb       $A                  ; X
                    fcb       $F8                 ; Y
                    fcb       $28                 ; X
                    fcb       $F1                 ; Y
                    fcb       $31                 ; X
                    fcb       $E7                 ; Y
                    fcb       $38                 ; X
                    fcb       $D3                 ; Y
                    fcb       $3A                 ; X
                    fcb       $BB                 ; Y
                    fcb       $3C                 ; X
                    fcb       $A7                 ; Y
                    fcb       $3C                 ; X
                    fcb       $70                 ; Y
                    fcb       $3C                 ; X
                    fcb       $4E                 ; Y
                    fcb       $3C                 ; X
                    fcb       $38                 ; Y
                    fcb       $31                 ; X
                    fcb       $23                 ; Y
                    fcb       $24                 ; X
                    fcb       $13                 ; Y
                    fcb       $8                  ; X
                    fcb       $9                  ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
LET_E               fcb       $3C                 ; X
                    fcb       $FA                 ; Y
                    fcb       $01                 ; ON
                    fcb       $6                  ; X
                    fcb       $F9                 ; Y
                    fcb       $8                  ; X
                    fcb       $00                 ; OFF
                    fcb       $A                  ; Y
                    fcb       $3D                 ; X
                    fcb       $01                 ; ON
                    fcb       $9                  ; Y
                    fcb       $8                  ; X
                    fcb       $83                 ; Y
                    fcb       $3C                 ; X
                    fcb       $81                 ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
LET_F               fcb       $6                  ; X
                    fcb       $9                  ; Y
                    fcb       $01                 ; ON
                    fcb       $8                  ; X
                    fcb       $F9                 ; Y
                    fcb       $3A                 ; X
                    fcb       $F9                 ; Y
                    fcb       $00                 ; OFF
                    fcb       $7                  ; X
                    fcb       $78                 ; Y
                    fcb       $01                 ; ON
                    fcb       $3D                 ; X
                    fcb       $77                 ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
LET_G               fcb       $3C                 ; X
                    fcb       $F9                 ; Y
                    fcb       $01                 ; ON
                    fcb       $7                  ; X
                    fcb       $F9                 ; Y
                    fcb       $9                  ; X
                    fcb       $9                  ; Y
                    fcb       $3C                 ; X
                    fcb       $9                  ; Y
                    fcb       $3C                 ; X
                    fcb       $6D                 ; Y
                    fcb       $15                 ; X
                    fcb       $6F                 ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
LET_H               fcb       $7                  ; X
                    fcb       $F8                 ; Y
                    fcb       $01                 ; ON
                    fcb       $7                  ; X
                    fcb       $A                  ; Y
                    fcb       $00                 ; OFF
                    fcb       $3B                 ; X
                    fcb       $FB                 ; Y
                    fcb       $01                 ; ON
                    fcb       $3D                 ; X
                    fcb       $B                  ; Y
                    fcb       $00                 ; OFF
                    fcb       $8                  ; X
                    fcb       $89                 ; Y
                    fcb       $01                 ; ON
                    fcb       $3C                 ; X
                    fcb       $8A                 ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
LET_I               fcb       $28                 ; X
                    fcb       $C                  ; Y
                    fcb       $01                 ; ON
                    fcb       $29                 ; X
                    fcb       $F6                 ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
LET_J               fcb       $22                 ; X
                    fcb       $FB                 ; Y
                    fcb       $01
                    fcb       $3C                 ; X
                    fcb       $FA                 ; Y
                    fcb       $3C                 ; X
                    fcb       $C                  ; Y
                    fcb       $A                  ; X
                    fcb       $C                  ; Y
                    fcb       $A                  ; X
                    fcb       $4F                 ; Y
                    fcb       $28                 ; X
                    fcb       $4F                 ; Y
                    fcb       $28                 ; X
                    fcb       $30                 ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
LET_K               fcb       $14                 ; X
                    fcb       $F9                 ; Y
                    fcb       $01                 ; ON
                    fcb       $14                 ; X
                    fcb       $C                  ; Y
                    fcb       $00                 ; OFF
                    fcb       $3B                 ; X
                    fcb       $F9                 ; Y
                    fcb       $01                 ; ON
                    fcb       $15                 ; X
                    fcb       $8D                 ; Y
                    fcb       $3A                 ; X
                    fcb       $C                  ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
LET_L               fcb       $6                  ; X
                    fcb       $F8                 ; Y
                    fcb       $01                 ; ON
                    fcb       $6                  ; X
                    fcb       $C                  ; Y
                    fcb       $3E                 ; X
                    fcb       $C                  ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
LET_M               fcb       $7                  ; X
                    fcb       $C                  ; Y
                    fcb       $01                 ; ON
                    fcb       $7                  ; X
                    fcb       $F9                 ; Y
                    fcb       $1F                 ; X
                    fcb       $8F                 ; Y
                    fcb       $3C                 ; X
                    fcb       $F8                 ; Y
                    fcb       $3C                 ; X
                    fcb       $9                  ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
LET_N               fcb       $7                  ; X
                    fcb       $9                  ; Y
                    fcb       $01                 ; ON
                    fcb       $7                  ; X
                    fcb       $F9                 ; Y
                    fcb       $3B                 ; X
                    fcb       $C                  ; Y
                    fcb       $3C                 ; X
                    fcb       $F5                 ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
LET_O               fcb       $22                 ; X
                    fcb       $6                  ; Y
                    fcb       $01                 ; ON
                    fcb       $6                  ; X
                    fcb       $43                 ; Y
                    fcb       $4                  ; X
                    fcb       $7E                 ; Y
                    fcb       $10                 ; X
                    fcb       $DA                 ; Y
                    fcb       $27                 ; X
                    fcb       $F6                 ; Y
                    fcb       $3A                 ; X
                    fcb       $B2                 ; Y
                    fcb       $3E                 ; X
                    fcb       $84                 ; Y
                    fcb       $3C                 ; X
                    fcb       $58                 ; Y
                    fcb       $22                 ; X
                    fcb       $8                  ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
LET_P               fcb       $8                  ; X
                    fcb       $A                  ; Y
                    fcb       $01                 ; ON
                    fcb       $9                  ; X
                    fcb       $F3                 ; Y
                    fcb       $3C                 ; X
                    fcb       $F3                 ; Y
                    fcb       $3D                 ; X
                    fcb       $94                 ; Y
                    fcb       $A                  ; X
                    fcb       $95                 ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
LET_Q               fcb       $22                 ; X
                    fcb       $6                  ; Y
                    fcb       $6                  ; X
                    fcb       $43                 ; Y
                    fcb       $4                  ; X
                    fcb       $7E                 ; Y
                    fcb       $10                 ; X
                    fcb       $DA                 ; Y
                    fcb       $27                 ; X
                    fcb       $F6                 ; Y
                    fcb       $3A                 ; X
                    fcb       $B2                 ; Y
                    fcb       $3E                 ; X
                    fcb       $84                 ; Y
                    fcb       $3C                 ; X
                    fcb       $58                 ; Y

                    fcb       $22                 ; X
                    fcb       $8                  ; Y
                    fcb       $00                 ; OFF
                    fcb       $22                 ; X
                    fcb       $44                 ; Y
                    fcb       $01                 ; ON
                    fcb       $31                 ; X
                    fcb       $13                 ; Y
                    fcb       $3B                 ; X
                    fcb       $8                  ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
LET_R               fcb       $9                  ; X
                    fcb       $A                  ; Y
                    fcb       $01                 ; OFF
                    fcb       $7                  ; X
                    fcb       $FA                 ; Y
                    fcb       $28                 ; X
                    fcb       $F9                 ; Y
                    fcb       $34                 ; X
                    fcb       $D2                 ; Y
                    fcb       $29                 ; X
                    fcb       $B4                 ; Y
                    fcb       $8                  ; X
                    fcb       $B4                 ; Y
                    fcb       $33                 ; X
                    fcb       $A                  ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
LET_S               fcb       $3A                 ; X
                    fcb       $C7                 ; Y
                    fcb       $01                 ; ON
                    fcb       $21                 ; X
                    fcb       $F4                 ; Y
                    fcb       $A                  ; X
                    fcb       $C3                 ; Y
                    fcb       $6                  ; X
                    fcb       $8D                 ; Y
                    fcb       $21                 ; X
                    fcb       $72                 ; Y
                    fcb       $3A                 ; X
                    fcb       $61                 ; Y
                    fcb       $38                 ; X
                    fcb       $10                 ; Y
                    fcb       $9                  ; X
                    fcb       $D                  ; Y
                    fcb       $8                  ; X
                    fcb       $32                 ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
LET_T               fcb       $4                  ; X
                    fcb       $FA                 ; Y
                    fcb       $01                 ; ON
                    fcb       $3C                 ; X
                    fcb       $FA                 ; Y
                    fcb       $00                 ; OFF
                    fcb       $1F                 ; X
                    fcb       $FA                 ; Y
                    fcb       $01                 ; ON
                    fcb       $1F                 ; X
                    fcb       $9                  ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
LET_U               fcb       $3                  ; X
                    fcb       $FA                 ; Y
                    fcb       $01                 ; ON
                    fcb       $5                  ; X
                    fcb       $A                  ; Y
                    fcb       $3B                 ; X
                    fcb       $A                  ; Y
                    fcb       $3B                 ; X
                    fcb       $FA                 ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
LET_V               fcb       $4                  ; X
                    fcb       $FA                 ; Y
                    fcb       $01                 ; ON
                    fcb       $1F                 ; X
                    fcb       $7                  ; Y
                    fcb       $3B                 ; X
                    fcb       $FA                 ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
LET_W               fcb       $5                  ; X
                    fcb       $FA                 ; Y
                    fcb       $01                 ; ON
                    fcb       $D                  ; X
                    fcb       $9                  ; Y
                    fcb       $1F                 ; X
                    fcb       $75                 ; Y
                    fcb       $2E                 ; X
                    fcb       $9                  ; Y
                    fcb       $3C                 ; X
                    fcb       $F9                 ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
LET_X               fcb       $6                  ; X
                    fcb       $9                  ; Y
                    fcb       $01                 ; ON
                    fcb       $39                 ; X
                    fcb       $F9                 ; Y
                    fcb       $00                 ; OFF
                    fcb       $6                  ; X
                    fcb       $F9                 ; Y
                    fcb       $01                 ; ON
                    fcb       $3A                 ; X
                    fcb       $9                  ; Y
                    fcb       $01                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
LET_Y               fcb       $6                  ; X
                    fcb       $F9                 ; Y
                    fcb       $01                 ; ON
                    fcb       $20                 ; X
                    fcb       $80                 ; Y
                    fcb       $00                 ; OFF
                    fcb       $3B                 ; X
                    fcb       $F8                 ; Y
                    fcb       $01                 ; ON
                    fcb       $A                  ; X
                    fcb       $1E                 ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
LET_Z               fcb       $6                  ; X
                    fcb       $FA                 ; Y
                    fcb       $01                 ; ON
                    fcb       $3C                 ; X
                    fcb       $F9                 ; Y
                    fcb       $7                  ; X
                    fcb       $5                  ; Y
                    fcb       $3D                 ; X
                    fcb       $4                  ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END
          ;--------------------------------------
MODE4_PATTERN       fcb       $71                 ; X
                    fcb       $75                 ; Y
                    fcb       $01                 ; ON
                    fcb       $7C                 ; X
                    fcb       $75                 ; Y
                    fcb       $7D                 ; X
                    fcb       $81                 ; Y
                    fcb       $72                 ; X
                    fcb       $81                 ; Y
                    fcb       $69                 ; X
                    fcb       $83                 ; Y
                    fcb       $6A                 ; X
                    fcb       $65                 ; Y
                    fcb       $89                 ; X
                    fcb       $64                 ; Y
                    fcb       $8A                 ; X
                    fcb       $91                 ; Y
                    fcb       $5E                 ; X
                    fcb       $92                 ; Y
                    fcb       $5F                 ; X
                    fcb       $58                 ; Y
                    fcb       $93                 ; X
                    fcb       $56                 ; Y
                    fcb       $94                 ; X
                    fcb       $9E                 ; Y
                    fcb       $52                 ; X
                    fcb       $A0                 ; Y
                    fcb       $53                 ; X
                    fcb       $48                 ; Y
                    fcb       $AE                 ; X
                    fcb       $48                 ; Y
                    fcb       $AE                 ; X
                    fcb       $B3                 ; Y
                    fcb       $3D                 ; X
                    fcb       $B4                 ; Y
                    fcb       $3E                 ; X
                    fcb       $2E                 ; Y
                    fcb       $C2                 ; X
                    fcb       $2E                 ; Y
                    fcb       $C3                 ; X
                    fcb       $C5                 ; Y
                    fcb       $33                 ; X
                    fcb       $C7                 ; Y
                    fcb       $33                 ; X
                    fcb       $1A                 ; Y
                    fcb       $D5                 ; X
                    fcb       $1A                 ; Y
                    fcb       $D6                 ; X
                    fcb       $DF                 ; Y
                    fcb       $26                 ; X
                    fcb       $DE                 ; Y
                    fcb       $24                 ; X
                    fcb       $E                  ; Y
                    fcb       $E4                 ; X
                    fcb       $F                  ; Y
                    fcb       $E9                 ; X
                    fcb       $EF                 ; Y
                    fcb       $12                 ; X
                    fcb       $F0                 ; Y
                    fcb       $12                 ; X
                    fcb       $E                  ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END

;*******************************************************************************
                    #VECTORS
;*******************************************************************************

                    org       $FFF2
                    dw        ISR_Handler

                    org       $FFFE
                    dw        Start
