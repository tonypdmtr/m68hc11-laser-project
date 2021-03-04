; *NAMES: RENE MOISE KWIBUKA & DAVID BURMEIER
; *START DATE SOFTWARE : OCTOBER 10, 2015.
; *END DATE SOFTWARE:     NOVEMBER 05, 2015
; *START DATE HARDWARE: JANUARY 4TH, 2016.
; *END DATE HARDWARE      MARCH 10TH, 2016
; *EDITED ALONG WITH HARDWARE CONSTRUCTION: JAN - MARCH 2016

; *THIS PROGRAM IS A LASER PROJECT PROGRAM


;****************************************************************************************
;*******                         START OF THE PROGRAM        ********
;****************************************************************************************

                    org       $E000               ; WHERE TO STORE THE PROGRAM (BENEFIT OF ASSEMBLER)

;****************************************************************************************
;*******                         DEFINE CONSTANTS                   ********
;****************************************************************************************
COUNTER             equ       $10                 ; COUNTER
SHIFT_VAL           equ       18                  ; SHIFT MAX VALUE
BUFFER              equ       $20                 ; BUFFER LOCATION
MODE_L              equ       $30                 ; MODE LOCATION
SHIFT_L             equ       $40                 ; SHIFT LOCATION
X_OFFSET            equ       $50                 ; X_OFFSET LOCATION.
MULTLASSTR          equ       $60                 ; WHEN MULTIPLYING BY 32, WE NEED THIS LOCATION
DELCOUNT            equ       $70                 ; DELAY COUNTER HOLDER
STACK               equ       $FF                 ; STACK VALUE
OFF                 equ       $A000               ; LASER OFF
ON                  equ       $A001               ; LASER ON
PIA_DDRA            equ       $B000               ; PIA DDRA
PIA_CRA             equ       $B001               ; PIA CRA
PIA_DDRB            equ       $B002               ; PIA DRRB
PIA_CRB             equ       $B003               ; PIA CRB
REG_0               equ       $C000               ; INITIALIZE LCD REGISTER 0
REG_1               equ       $C001               ; SET LCD REGISTER 1
KEYBD               equ       $D000               ; KEYBOARD INPUT
ASCII_TABLE         equ       $F000               ; ASCII TABLE LOCATION
MODE_3_STR          equ       $F900               ; MODE 3 STRING LOCATION
D_ACONV             equ       $1039               ; OPTION
ST_REG              equ       $1030               ; A/D CONTROL/STATUS REGISTER
ADR_READ            equ       $1031               ; READ CONVERTED ANALOG FROM HERE
MODE4_PATTERN       equ       $F4C0               ; MODE_4 LOCATION

;****************************************************************************************
;*******                         CHARACTERS LOCATION                ********
;****************************************************************************************

DIG_0               equ       $F040               ; Digit 0 to Digit 9 sequentially.
DIG_1               equ       $F060
DIG_2               equ       $F080
DIG_3               equ       $F0A0
DIG_4               equ       $F0C0
DIG_5               equ       $F0E0
DIG_6               equ       $F100
DIG_7               equ       $F120
DIG_8               equ       $F140
DIG_9               equ       $F160
LET_A               equ       $F180               ; Letter A through Letter z
LET_B               equ       $F1A0
LET_C               equ       $F1C0
LET_D               equ       $F1E0
LET_E               equ       $F200
LET_F               equ       $F220
LET_G               equ       $F240
LET_H               equ       $F260
LET_I               equ       $F280
LET_J               equ       $F2A0
LET_K               equ       $F2C0
LET_L               equ       $F2E0
LET_M               equ       $F300
LET_N               equ       $F320
LET_O               equ       $F340
LET_P               equ       $F360
LET_Q               equ       $F380
LET_R               equ       $F3A0
LET_S               equ       $F3C0
LET_T               equ       $F3E0
LET_U               equ       $F400
LET_V               equ       $F420
LET_W               equ       $F440
LET_X               equ       $F460
LET_Y               equ       $F480
LET_Z               equ       $F4A0

;****************************************************************************************
;*******                         INITIALIZATION                                  ********
; *Since we are using subroutines and interrupts, some initializations are a must.
; *Among them are to clear index, initialize the stack,and initialize the buffer.
;****************************************************************************************`
                    cli                           ; CLEAR INTERRUPT INDEX
                    clr       MODE_L              ; CLEAR MODE
                    inc       MODE_L              ; MAKE MODE 1
                    clr       SHIFT_L             ; CLEAR SHIFT VALUE HOLDER
                    clr       X_OFFSET            ; CLEAR X_OFFSET
                    lds       #STACK              ; INITIALIZE THE STACK POINTER
                    lda       #$FF                ; LOAD A WITH FF
                    ldy       #BUFFER             ; LOAD Y WITH BUFFER STARTING LOCATION
                    sta       BUFFER              ; STORE FF AT BUFFER
                    sta       BUFFER+1            ; STORE FF AT BUFFER
                    sta       BUFFER+2            ; STORE FF AT BUFFER
                    sta       BUFFER+3            ; STORE FF AT BUFFER

;*******                 INITIALIZATION LCD                   ********

                    bsr       INIT_LCD            ; jump to subroutine init lcd
                    bra       PIA_INIT            ; branch to pia and follow up to main

INIT_LCD
;** INITIALIZING THE ADPU bit 7 ($1039). Allow 100 us before initializing another command.

                    ldx       #D_ACONV            ; load x with $1039.
                    bset      0,x,%10000000       ; SET BIT 7 OF DIG ANAL CONVER

                    ldx       #REG_0              ; LOAD X WITH REG_0 VALUE
HERE1               brset     0,x,$80,HERE1       ; BRANCH BACK HERE IF BF = 1
                    lda       #$01                ; LOAD A WITH $01
                    sta       REG_0               ; STORE IN REG_0

HERE2               brset     0,x,$80,HERE2       ; BRANCH BACK HERE IF BF = 1
                    lda       #$30                ; LOAD A WITH $30
                    sta       REG_0               ; STORE IN REG_0

HERE3               brset     0,x,$80,HERE3       ; BRANCH BACK HERE IF BF = 1
                    lda       #$08                ; LOAD A WITH $08
                    sta       REG_0               ; STORE IN REG_0

HERE4               brset     0,x,$80,HERE4       ; BRANCH BACK HERE IF BF = 1
                    lda       #$06                ; LOAD A WITH $06
                    sta       REG_0               ; STORE IN REG_0

HERE5               brset     0,x,$80,HERE5       ; BRANCH BACK HERE IF BF = 1
                    lda       #$3C                ; LOAD A WITH $3C
                    sta       REG_0               ; STORE IN REG_0

HERE6               brset     0,x,$80,HERE6       ; BRANCH BACK HERE IF BF = 1
                    lda       #$0F                ; LOAD A WITH $0F
                    sta       REG_0               ; STORE IN REG_0

HERE7               brset     0,x,$80,HERE7       ; BRANCH BACK HERE IF BF = 1

                    rts                           ; ONLY RETURN IF NOT DONE WITH INITIALIZING


;*******                 INITIALIZATION OF PIA                ********

PIA_INIT

;***************          INITIALIZE PIA A (X VALUES)    *************************
                    ldx       #PIA_CRA            ; LOAD PIA CRA VALUE TO X
                    bclr      0,x,%00000100       ; SET BIT 2 OF CRA TO 0 (ACCESS DDRA)
                    lda       #$FF                ; LOAD ACC A WITH FF
                    sta       PIA_DDRA            ; STORE FF IN B000 (SET DDRA TO FF)
                    ldx       #PIA_CRA            ; LOAD PIA CRA TO X
                    bset      0,x,%00000100       ; SET BIT 2 OF CRA TO 1 (ACCESS DRA)

;***************          INITIALIZE PIA B  (Y VALUES)   *************************
                    ldx       #PIA_CRB            ; LOAD CRB VALUE IN X
                    bclr      0,x,%00000100       ; SET BIT 2 OF CRB TO 0 (ACCESS DDRB)
                    lda       #$FF                ; LOAD ACC A WITH FF
                    ldx       #PIA_CRB            ; LOAD CRB VALUE IN X
                    sta       PIA_DDRB            ; STORE FF IN B002 (SET DDRB TO FF)
                    bset      0,x,%00000100       ; SET BIT 2 OF CRB TO 1 (ACCESS DRB)

                    jsr       DELAY               ; TO ENSURE COMPLETE ADPU INITIALIZAION, DELAY

; ANOTHER INITIALIZATION OF D/A AFTER AT LEAST 100us
                    ldx       #ST_REG             ; LOAD X WITH $1030
                    bset      0,x,%00100000       ; SET BIT 5

;****************************************************************************************
;*******                                 MAIN                                    ********
;****************************************************************************************`

MAIN
                    lda       ADR_READ            ; LOAD A WITH A/D CONVERTER VALUE
                    sta       $E103               ; PROGRAM IT OUT AT $E103 (FOR THE SAKE OF LOGIC ANALYZER)
                    clr       COUNTER             ; CLEAR COUNTER LOCATION
                    inc       COUNTER             ; STORE A 1 IN COUNTER LOCATION
                    ldx       #MODE_L             ; LOADING X WITH MODE_LOC
                    lda       COUNTER             ; LOAD COUNTER IN A
                    cmpa      0,x                 ; CHECK IF MODE IS EQUAL TO COUNTER (1 SO FAR)
                    beq       MAIN                ; IF YES BRANCH TO MAIN
                    inc       COUNTER             ; MAKE COUNTER 2
                    lda       COUNTER             ; LOAD 2 IN A
                    cmpa      0,x                 ; CHECK IF MODE IS 2
                    beq       MODE_2              ; BRANCH TO MODE 2
                    inc       COUNTER             ; MAKE COUNTER 3
                    lda       COUNTER             ; LOAD 3 IN A
                    cmpa      0,x                 ; CHECK IF MODE IS 3
                    beq       GOMODE_3            ; BRANCH TO MODE 3
                    inc       COUNTER             ; MAKE COUNTER 4
                    lda       COUNTER             ; LOAD 4 IN A
                    cmpa      0,x                 ; CHECK IF MODE IS 4
                    beq       GOMODE_4            ; BRANCH TO MODE 4
                    bra       MAIN                ; BRANCH TO MAIN

GOMODE_3
                    bra       MODE_3              ; JUMP TO MODE 3 JUMP TO MODE 3

GOMODE_4
                    jmp       MODE_4              ; JUMP TO MODE 4 JUMP TO MODE 4


;****************************************************************************************
;*******                         MODE_2                   ********
;****************************************************************************************

MODE_2

CHECKB
                    lda       #$FF                ; LOADS $FF INTO ACCUMULATOR A
                    cmpa      0,Y                 ; CHECK IF VALUE IN BUFFER IS $FF
                    beq       MAIN                ; IF BUFFER CONTAINS $FF, GO BACK TO MAIN
                    ldab      0,Y                 ; LOAD VALUE IN BUFFER INTO ACCUMULATOR B
                    lda       #32                 ; LOAD A WITH 32
                    mul                           ; MULTIPLY A WITH B
                    addd      #DIG_0              ; ADD WHAT IS IN D WITH THE VALUE AT LASER
                    std       MULTLASSTR          ; STORE THE LOCATION AT THIS MULTLASSTR
                    ldx       MULTLASSTR          ; LOAD IT FROM MULTLASSTR

NEXTY
                    lda       #$FF                ; LOAD #$FF INTO ACCUMULATOR A
                    cmpa      0,x                 ; COMPARE THE Y COORDINATE TO $FF
                    beq       LASOFFADJ           ; IF TRUE, TURN OFF THE LASER
                    lda       #$00                ; LOAD #$00 INTO ACCUMULATOR A
                    cmpa      0,x                 ; COMPARE THE Y COORDINATE TO $00
                    beq       LASOFFNEXTY         ; IF TRUE, TURN OFF THE LASER
                    lda       #$01                ; LOAD #$01 INTO ACCUMULATOR A
                    cmpa      0,x                 ; COMPARE THE Y COORDINATE TO $01
                    beq       LASONNEXTY          ; TURN LASER ON
                    bsr       DISPLAY             ; JUMP TO SUBROUTINE
                    bra       NEXTY               ; BRANCH


LASOFFNEXTY
                    bsr       LASOFF              ; JUMP TO SUBROUTINE
                    inx                           ; INCREMENT X
                    bra       NEXTY               ; BRANCH TO NEXTY

LASONNEXTY
                    bsr       LASON               ; JUMP TO SUBROUTINE
                    inx                           ; INCREMENT X
                    bra       NEXTY               ; BRANCH TO NEXTY


LASOFFADJ
                    bsr       LASOFF              ; JUMP TO LASOFF SUBROUTINE

ADJUSTX
                    ldx       #X_OFFSET           ; LOAD X WITH THE ADDRESS OF THE OFFSET
                    lda       0,x                 ; LOAD A WITH THE VALUE IN X
                    adda      #64                 ; ADD 64 ON ACC VALUE
                    cmpa      #255                ; COMPARE IT WITH 255
                    blo       ADJBUFF             ; IF GREATER OR EQUAL GO ADJSUT BUFFER
                    lda       #00                 ; LOAD A WITH 0

ADJBUFF
                    sta       0,x                 ; RESET THE X_OFFSET
                    iny                           ; INCREMENT THE CURRENT BUFFER POINTER
                    lda       0,Y
                    cmpa      #$FF
                    beq       RES_Y
                    cpy       #$24                ; COMPARE IT WITH THE VALUE 24
                    blo       GOMAIN              ; IF LESS BRA TO MAIN
                    ldy       #BUFFER             ; ELSE RESET IT WITH 20
                    bra       GOMAIN              ; AND GO BACK TO MAIN

RES_Y
                    ldy       #BUFFER             ; ELSE RESET IT WITH 20
                    clr       X_OFFSET
                    bra       GOMAIN              ; AND GO BACK TO MAIN


;****************************************************************************************
;*******                         MODE_3                   ********
;****************************************************************************************

MODE_3

                    ldx       #MODE_3_STR         ; LOAD X WITH MODE_3_STR LOCATION
NEXTY_3
                    lda       #$FF                ; LOAD #$FF INTO ACCUMULATOR A
                    cmpa      0,x                 ; COMPARE THE Y COORDINATE TO $FF
                    beq       GOOFFMAIN           ; IF TRUE, TURN OFF THE LASER
                    lda       #$00                ; LOAD #$00 INTO ACCUMULATOR A
                    cmpa      0,x                 ; COMPARE THE Y COORDINATE TO $00
                    beq       GOOFFNEXTY3         ; IF TRUE, TURN OFF THE LASER
                    lda       #$01                ; LOAD #$01 INTO ACCUMULATOR A
                    cmpa      0,x                 ; COMPARE THE Y COORDINATE TO $01
                    beq       GOONNEXTY3          ; TURN LASER ON
                    bsr       DISPLAY             ; JUMP TO SUBROUTINE
                    bra       NEXTY_3             ; BRANCH

GOOFFMAIN
                    bsr       LASOFF              ; JUMP TO SUBROUTINE
                    bra       GOMAIN              ; BRANCH

GOOFFNEXTY3
                    bsr       LASOFF              ; JUMP TO SUBROUTINE
                    inx                           ; INCREMENT X
                    bra       NEXTY_3             ; BRANCH

GOONNEXTY3
                    bsr       LASON               ; JUMP TO SUBROUTINE
                    inx                           ; INCREMENT
                    bra       NEXTY_3             ; BRANCH

DISPLAY
                    lda       0,x
                    adda      X_OFFSET            ; ADD X TO OFFSET VALUE
                    sta       PIA_DDRA            ; STORE IT AT PIA_DDRB (AT DDRB)
                    inx                           ; INCREMENT X
                    lda       0,x                 ; LOAD Y VALUE
                    sta       PIA_DDRB            ; STORE A AT B000
                    bsr       DELAY               ; JUMP TO DELAY
                    inx
                    rts

LASON
                    lda       #$FF                ; LOAD 0 IN A
                    sta       ON                  ; STORE IN A000 AND TURN ON
                    bsr       DELAY               ; JUMP TO DELAY
                    rts

LASOFF
                    lda       #$FF                ; LOAD 0 IN A
                    sta       OFF                 ; STORE IN A000 AND TURN OFF
                    bsr       DELAY               ; JUMP TO DELAY
                    rts

GOMAIN
                    jmp       MAIN                ; JUMP TO MAIN

DELAY
                    clr       DELCOUNT            ; (3) LOAD X WITH 0
                    lda       #250

AGAIN               cmpa      DELCOUNT            ; (3) COMPARE 14 TO THE VAL
                    beq       AGAIN4              ; (3) IF 14 GO RETURN
                    inc       DELCOUNT            ; (6) INCREMENT DELAY COUNTER
                    bra       AGAIN               ; (3) BRANCH AGAIN

AGAIN4              clr       DELCOUNT            ; (3) LOAD X WITH 0
                    lda       ADR_READ

AGAIN5              cmpa      DELCOUNT            ; (3) COMPARE 14 TO THE VAL
                    beq       SUBRET              ; (3) IF 14 GO RETURN
                    inc       DELCOUNT            ; (6) INCREMENT DELAY COUNTER
                    nop                           ; TO WASTE SOME CYCLES
                    nop                           ; TO WASTE SOME CYCLES
                    nop                           ; TO WASTE SOME CYCLES
                    nop                           ; TO WASTE SOME CYCLES
                    nop                           ; TO WASTE SOME CYCLES
                    nop                           ; TO WASTE SOME CYCLES
                    nop                           ; TO WASTE SOME CYCLES
                    bra       AGAIN5              ; (3) BRANCH AGAIN

SUBRET
                    rts                           ; RETURN FROM SUBROUTINE.

;****************************************************************************************
;*******                         MODE_4                   ********
;****************************************************************************************
MODE_4

                    ldx       #MODE4_PATTERN      ; LOAD X WITH MODE_3_STR LOCATION
NEXTY_4
                    lda       #$FF                ; LOAD #$FF INTO ACCUMULATOR A
                    cmpa      0,x                 ; COMPARE THE Y COORDINATE TO $FF
                    beq       GOOFFMAIN           ; IF TRUE, TURN OFF THE LASER
                    lda       #$00                ; LOAD #$00 INTO ACCUMULATOR A
                    cmpa      0,x                 ; COMPARE THE Y COORDINATE TO $00
                    beq       GOOFFNEXTY4         ; IF TRUE, TURN OFF THE LASER
                    lda       #$01                ; LOAD #$01 INTO ACCUMULATOR A
                    cmpa      0,x                 ; COMPARE THE Y COORDINATE TO $01
                    beq       GOONNEXTY4          ; TURN LASER ON
                    bsr       DISPLAY
                    bra       NEXTY_4


GOOFFNEXTY4
                    bsr       LASOFF              ; JUMP TO SUBROUTINE
                    inx                           ; INCREMENT X
                    bra       NEXTY_4             ; BRANCH

GOONNEXTY4
                    bsr       LASON               ; JUMP TO SUBROUTINE
                    inx
                    bra       NEXTY_4             ; BRANCH

;****************************************************************************************
;*******         INTERRUPT BLOCK              ********
;****************************************************************************************
ISR
                    ldab      KEYBD               ; READ FROM KEYBOARD
                    cmpb      #18                 ; COMPARE THE KEY VALUE TO MODE
                    beq       MODE                ; GO UPDATE MODE
                    cmpb      #19                 ; ELSE COMPARE TO 19
                    beq       SHIFT               ; IF EQUAL GO UPDATE SHIFT
                    addb      SHIFT_L             ; ELSE ADD B TO THE SHIFT VALUE
                    clr       SHIFT_L             ; CLEAR SHIFT LOCATION
                    ldy       #BUFFER             ; LOAD Y WITH BUFFER VALUE

CHECKBUF
                    lda       #$FF                ; LOAD A WITH FF
                    cmpa      0,Y                 ; COMPARE A WITH WHAT IS POINTED TO BY Y
                    bne       INCBUF              ; BRANCH NOT EQUAL
STORE               stab      0,Y                 ; STORE THE KEY IN BUFFER
                    ldx       #ASCII_TABLE        ; LOAD X WITH ACSCII_TABLE LOCATION
                    abx                           ; ADD ACCUMULATOR B TO X
                    lda       0,x                 ; LOAD IN ACC A WHAT IS POINTED TO BY Y
HERE8               brset     0,x,$80,HERE8       ; BRANCH BACK HERE IF BF = 1
                    sta       REG_1               ; STORE X AT REG_1
HERE9               brset     0,x,$80,HERE9       ; BRANCH BACK HERE IF BF = 1
                    bra       RETURN              ; BRANCH

INCBUF
                    iny                           ; INCREMENT Y
                    cpy       #$24                ; COMPARE Y TO 24
                    bne       CHECKBUF            ; BRANCH NOT EQUAL
                    bra       RETURN              ; RETURN

MODE
                    lda       COUNTER
                    sta       $F101
                    lda       SHIFT_L             ; LOAD A WITH WHAT IS IN SHIFT_L
                    cmpa      #$00                ; COMPARE IT WITH 0
                    bne       CLEAR               ; BRANCH NOT EQUAL TO CLEAR
                    clr       SHIFT_L             ; CLEAR SHIFT_L
                    inc       MODE_L              ; INCREMENT THE MODE IN MODE_L
                    lda       MODE_L              ; LOAD THE MODE VALUE IN A
                    sta       $E100
                    cmpa      #5                  ; COMPARE THE MODE TO 5
                    bne       RETURN              ; IF LESS RETURN
                    clr       MODE_L              ; ELSE CLEAR THE MODE
                    inc       MODE_L              ; MAKE IT A 1
                    bra       RETURN              ; BRANCH TO RETURN

SHIFT
                    lda       SHIFT_L             ; LOAD A WITH THE SHIFT VALUE
                                                  ; COMPARE IF IT IS 0
                    beq       NOW20               ; IF 0, GO MAKE IT 20
                    clra                          ; ELSE MAKE IT 0
                    sta       SHIFT_L             ; STORE THE SHIFT VALUE BACK IN THE MEM LOC
                    bra       RETURN              ; GO RETURN

NOW20
                    lda       #SHIFT_VAL          ; LOAD A WITH 20
                    sta       SHIFT_L             ; STORE THE SHIFT VALUE BACK IN SHIFT_L
                    bra       RETURN              ; GO RETURN

CLEAR
                    jsr       INIT_LCD
                    clr       X_OFFSET            ; CLEAR X_OFFSET
                    clr       SHIFT_L             ; CLEAR SHIFT
                    ldx       #BUFFER             ; LOAD THE ADDRESS OF BUFFER IN X
                    lda       #$FF                ; LOAD VALUE FF IN ACC A
CLRAG               sta       0,x                 ; STORE WHAT IS STORED IN ACC IN THE BUFFER
                    inx                           ; INCREMENT X
                    cpx       #$24                ; COMPARE WITH THE END OF THE BUFFER
                    beq       RETURN              ; RETURN
                    bra       CLRAG               ; CLEAR

RETURN
                    rti                           ; RETURN FROM THE INTERRUPT

;****************************************************************************************
;*******         CONCLUDING LINES OF CODE                     ********
;****************************************************************************************

;*******         LCDSTRING                    ********
                    org       ASCII_TABLE
                    fcb       $30,$31,$32,$33,$34,$35,$36,$37,$38,$39
                    fcb       $41,$42,$43,$44,$45,$46,$47,$48,$49,$4A,$4B,$4C,$4D,$4E,$4F,$50,$51
                    fcb       $52,$53,$54,$55,$56,$57,$58,$59,$5A

;*******         MODE 3 LASER STRING                  ********
                    org       MODE_3_STR          ; MODE_3_VECTOR
                    fcb       5,5,01,5,251,251,251,251,5,5,5,251,251,00,05,251,01,251,5,00,$FF


                    org       $FFF2               ; INTERRUPT VECTOR
                    fdb       ISR                 ; FORM INTERRUPT VECTOR BYTE
                    org       $FFFE               ; MOVES ASSEMBLER OUTPUT LOCATION TO $FFFE (FOR HARDWARE)
                    fdb       $E000               ; FORMS RESET VECTOR FOR HARDWARE

                    org       DIG_0
                    fcb       $B                  ; X
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

                    org       DIG_1
                    fcb       $9                  ; X
                    fcb       $9C                 ; Y
                    fcb       $01                 ; ON
                    fcb       $3C                 ; X
                    fcb       $FA                 ; Y
                    fcb       $3C                 ; X
                    fcb       $D                  ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END


                    org       DIG_2
                    fcb       $E                  ; X
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

                    org       DIG_3
                    fcb       $C                  ; X
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


                    org       DIG_4
                    fcb       $3C                 ; X
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



                    org       DIG_5
                    fcb       $3A                 ; X
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

                    org       DIG_6
                    fcb       $3A                 ; X
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


                    org       DIG_7
                    fcb       $8                  ; X
                    fcb       $F0                 ; Y
                    fcb       $01                 ; ON
                    fcb       $3D                 ; X
                    fcb       $F0                 ; Y
                    fcb       $D                  ; X
                    fcb       $F                  ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END


                    org       DIG_8
                    fcb       $1F                 ; X
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

                    org       DIG_9
                    fcb       $3A                 ; X
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


                    org       LET_A
                    fcb       $4                  ; X
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

                    org       LET_B
                    fcb       $9                  ; X
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


                    org       LET_C
                    fcb       $3C                 ; X
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


                    org       LET_D
                    fcb       $9                  ; X
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


                    org       LET_E
                    fcb       $3C                 ; X
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

                    org       LET_F
                    fcb       $6                  ; X
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

                    org       LET_G
                    fcb       $3C                 ; X
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

                    org       LET_H
                    fcb       $7                  ; X
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

                    org       LET_I
                    fcb       $28                 ; X
                    fcb       $C                  ; Y
                    fcb       $01                 ; ON
                    fcb       $29                 ; X
                    fcb       $F6                 ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END

                    org       LET_J
                    fcb       $22                 ; X
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

                    org       LET_K
                    fcb       $14                 ; X
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

                    org       LET_L
                    fcb       $6                  ; X
                    fcb       $F8                 ; Y
                    fcb       $01                 ; ON
                    fcb       $6                  ; X
                    fcb       $C                  ; Y
                    fcb       $3E                 ; X
                    fcb       $C                  ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END

                    org       LET_M
                    fcb       $7                  ; X
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

                    org       LET_N
                    fcb       $7                  ; X
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

                    org       LET_O
                    fcb       $22                 ; X
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

                    org       LET_P

                    fcb       $8                  ; X
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


                    org       LET_Q
                    fcb       $22                 ; X
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

                    org       LET_R
                    fcb       $9                  ; X
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

                    org       LET_S
                    fcb       $3A                 ; X
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

                    org       LET_T
                    fcb       $4                  ; X
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

                    org       LET_U
                    fcb       $3                  ; X
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

                    org       LET_V
                    fcb       $4                  ; X
                    fcb       $FA                 ; Y
                    fcb       $01                 ; ON
                    fcb       $1F                 ; X
                    fcb       $7                  ; Y
                    fcb       $3B                 ; X
                    fcb       $FA                 ; Y
                    fcb       $00                 ; OFF
                    fcb       $FF                 ; END

                    org       LET_W
                    fcb       $5                  ; X
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

                    org       LET_X
                    fcb       $6                  ; X
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

                    org       LET_Y
                    fcb       $6                  ; X
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

                    org       LET_Z
                    fcb       $6                  ; X
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

                    org       MODE4_PATTERN
                    fcb       $71                 ; X
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
