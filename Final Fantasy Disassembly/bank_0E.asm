.include "variables.inc"
.include "macros.inc"
.include "Constants.inc"

.export PrintNumber_2Digit, PrintPrice, PrintCharStat, PrintGold
.export TalkToObject, EnterLineupMenu, NewGamePartyGeneration
.export EnterMainMenu, EnterShop, EnterTitleScreen, EnterIntroStory

.import GameStart_L
.import lut_IntroStoryText

.import DoOverworld, PlaySFX_Error, DrawImageRect, AddGPToParty, DrawComplexString
.import ClearOAM, DrawPalette, FindEmptyWeaponSlot, CallMusicPlay, UpdateJoy

.import DrawEquipMenuStrings, DrawItemBox, FadeInBatSprPalettes, FadeOutBatSprPalettes, EraseBox, ReadjustEquipStats
.import SortEquipmentList, UnadjustEquipStats, LoadShopCHRPal, DrawSimple2x3Sprite, lutClassBatSprPalette, LoadNewGameCHRPal
.import DrawOBSprite, DrawCursor, WaitForVBlank_L, DrawBox, LoadMenuCHRPal, LoadPrice

.segment "BANK_0E"


BANK_THIS = $0E

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  LUT containing stock shop text  [$8000 :: 0x38010]

lut_ShopStrings:
  .INCBIN "bin/0E_8000_shopstrings.bin"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  LUT containing shop data  [$8300 :: 0x38310]

lut_ShopData:
  .INCBIN "bin/0E_8300_shopdata.bin"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  TitleScreen_Copyright  [$8480 :: 0x38490]
;;
;;    Prepares the screen and draws the little copyright message
;;  at the bottom of the screen.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TitleScreen_Copyright:
    JSR IntroTitlePrepare    ; clear NT, start music, etc
    BIT $2002                ;  reset PPU toggle

    LDX #0
    JSR @DrawString         ; JSR to the @DrawString to draw the first one
                            ;  then just let code flow into it to draw a second one (2 strings total)

  @DrawString:
    LDA @lut_Copyright+1, X ; get the Target PPU address from the LUT
    STA $2006
    LDA @lut_Copyright, X
    STA $2006
    INX                     ; move X past the address we just read
    INX

  @Loop:
    LDA @lut_Copyright, X   ; get the next character in the string
    BEQ @Exit               ;  if it's zero, exit (null terminator
    STA $2007               ; otherwise, draw the character
    INX                     ; INX to move to next character
    BNE @Loop               ; and keep looping (always branches)

  @Exit:
    INX                     ; INX to move X past the null terminator we just read
    RTS

 ;; LUT for the copyright text.  Simply a 2-byte target PPU address, followed by a
 ;;  null terminated string.  Two strings total.

@lut_Copyright:
  .WORD $2328
  .BYTE $8C,$FF,$81,$89,$88,$87,$FF,$9C,$9A,$9E,$8A,$9B,$8E,$FF,$FF,$00  ; "C 1987 SQUARE  "
  .WORD $2348
  .BYTE $8C,$FF,$81,$89,$89,$80,$FF,$97,$92,$97,$9D,$8E,$97,$8D,$98,$00  ; "C 1990 NINTENDO"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  IntroStory_Joy  [$84CA :: 0x384DA]
;;
;;    Updates joypad data and forces a game restart when the Start button is pressed
;;  (which brings up the title screen).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

IntroStory_Joy:
    LDA #0                ; reset the respond rate to zero
    STA a:respondrate     ;  (why do this here?  Very out of place)

    JSR UpdateJoy         ; Update joypad data
    LDA joy
    AND #BTN_START        ; see if start was pressed
    BNE :+                ;  if not, just exit
      RTS
:   JMP GameStart_L       ; if it was pressed, restart game (brings up title screen)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  TitleScreen_Music  [$84DC :: 0x384EC]
;;
;;    Very strange little routine that calls the music driver and then
;;  loads the A button catcher before exiting.  Is called by the title screen.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TitleScreen_Music:
    JSR CallMusicPlay
    LDA joy_a
    RTS


 ;; unused space

  .BYTE 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0
  .BYTE 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  LUT for menu text  [$8500 :: 0x38510]
;;
;;    This is a table of complex strings used in menus.

lut_MenuText:
  .INCBIN "bin/0E_8500_menutext.bin"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Print Character Stat  [$8D70 :: 0x38D80]
;;
;;    Called by DrawComplexString to print a specific character stat
;;  String is printed to 'format_buf'
;;
;;  IN:  char_index = character index ($00, $40, $80, or $C0)
;;       A          = ID of stat to draw (between $03-0B and 2C-FF .. other values invalid)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrintCharStat:
    CMP #$03      ; is ID == $03?
    BNE :+
      JMP @Level  ; if yes, print Level
:   CMP #$04
    BEQ @Exp      ; if $04, print Exp
    CMP #$05
    BEQ @CurHP    ; if $05, print CurHP
    CMP #$06
    BEQ @MaxHP    ; etc
    CMP #$07
    BEQ @Str
    CMP #$08
    BEQ @Agil
    CMP #$09
    BEQ @Int
    CMP #$0A
    BEQ @Vit
    CMP #$0B
    BEQ @Luck

    CMP #$3C
    BCS @CodeAbove3B   ; see if ID is >= #$3C

    CMP #$2C
    BCC @ExpToNext   ; see if ID is < #$2C (should never happen)

    ;;; code reaches here if the ID is between $2C-3B  (prints MP... cur or max)
      SEC
      SBC #$0C         ; subtract #$C  ($20-2F -- index + $20)
      CLC
      ADC char_index   ; add character index
      TAX
      LDA ch_mp-$20, X ; get MP  (need to subtract $20 because index is +$20)
      STA tmp          ;  and print it as 1 Digit
      JMP PrintNumber_1Digit


@CodeAbove3B:
    CMP #$42
    BCS @ExpToNext     ; see if ID is >= $42

    ;;; code reaches here if ID is between $3C-41  (prints substats, like Damage, Hit%, etc)
      SEC
      SBC #$3C            ; subtract #$3C  ($00-05)
      CLC
      ADC char_index      ; add character index
      TAX
      LDA ch_substats, X  ; get the substat
      STA tmp             ; write it as low byte
      LDA #0              ; set mid byte to 0 (need a mid byte for 3 Digit printing)
      STA tmp+1           ;  and print as 3 digits
      JMP PrintNumber_3Digit

    ;;; all other codes default to Exp to Next level
@ExpToNext:
      LDX char_index         ; get the index
      LDA ch_exptonext, X    ; low byte of Exp To Next
      STA tmp
      LDA ch_exptonext+1, X  ; mid byte
      STA tmp+1
      LDA #0                 ; high byte is 0 (5 digit numbers need a high byte)
      STA tmp+2              ; print it as 5 digits
      JMP PrintNumber_5Digit

@Exp:
    LDABRA <ch_exp, @Stat6Digit    ; put low byte of address of desired stat in A, then BRA to @Stat6Digit
                                   ;  see macros.inc for this macro

@CurHP:
    LDABRA <ch_curhp, @Stat3Digit

@MaxHP:
    LDABRA <ch_maxhp, @Stat3Digit

@Str:
    LDABRA <ch_str, @Stat2Digit

@Agil:
    LDABRA <ch_agil, @Stat2Digit

@Int:
    LDABRA <ch_int, @Stat2Digit

@Vit:
    LDABRA <ch_vit, @Stat2Digit

@Luck:
    LDABRA <ch_luck, @Stat2Digit

@Stat1Digit:       ; same as below routines -- but 1 byte, 1 digit
    CLC            ;  I do not believe this 1Digit code is ever called
    ADC char_index
    TAX
    LDA ch_stats, X
    STA tmp
    JMP PrintNumber_1Digit

@Stat2Digit:       ; same as below routines -- but 1 byte, 2 digits
    CLC
    ADC char_index
    TAX
    LDA ch_stats, X
    STA tmp
    JMP PrintNumber_2Digit

@Stat3Digit:
    CLC
    ADC char_index     ; add character index to stat ID (currently in A)
    TAX                ; use this to index stat from start of player stats ($6100)
    LDA ch_stats, X
    STA tmp
    LDA ch_stats+1, X  ; read a 2-byte number
    STA tmp+1          ; and print it as 3-digits
    JMP PrintNumber_3Digit

@Stat6Digit:
    CLC
    ADC char_index      ; add character index to stat ID (currently in A)
    TAX                 ; use this to index stat from start of player stats ($6100)
    LDA ch_stats, X
    STA tmp
    LDA ch_stats+1, X   ; read a 3-byte number
    STA tmp+1
    LDA ch_stats+2, X
    STA tmp+2           ; and print it as 6-digits
    JMP PrintNumber_6Digit

;;  Stat Code = $03 -- character level
@Level:
    LDX char_index   ; Get Character index
    LDA ch_level, X  ; Get character's level
    CLC
    ADC #$01         ; Add 1 to it ($00 is "Level 1")
    STA tmp          ; and print it as 2-digit
    JMP PrintNumber_2Digit



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Print Price  [$8E44 :: 0x38E54]
;;
;;    Fetches desired item price, then prints it to a temp drawing buffer
;;  (see Print Number below for details)
;;
;;  IN:  A = item ID whose price we're printing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrintPrice:
    JSR LoadPrice             ; just load the price
    JMP PrintNumber_5Digit    ; and print it as 5-digits!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Print Gold   [$8E4A :: 0x38E5A]
;;
;;    Loads and prints current gold amount into a temporary drawing buffer
;;    (see Print Number below for details)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrintGold:
    LDA gold     ; just copy the gold to the routine input vars
    STA tmp      ;   and then call "print"
    LDA gold+1
    STA tmp+1
    LDA gold+2
    STA tmp+2
    JMP PrintNumber_6Digit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Print Number   [$8E5C :: 0x38E6C]
;;
;;    These routines print a number of a desired number of digits to format_buf.
;;  The number is right-aligned with zeros trimmed off the front
;;  (ie:  "  67" instead of "0067").  A pointer to the start of the buffer
;;  is then stored at text_ptr.
;;
;;   The printed number is not null terminated... therefore the end of format_buf
;;  must always contain 0 so that this string is null terminated when it is attempted
;;  to be drawn
;;
;;  IN:  tmp = 3-byte number to print
;;            only the low byte is used for 1,2 digit printing
;;            and the highest byte is only used for 5,6 digit printing
;;
;;  OUT: format_buf = buffer receiving the printed string
;;       text_ptr   = pointer to start of buffer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrintNumber_1Digit:
    LDA tmp              ; no real formatting involved in 1-digit numbers
    ORA #$80             ; just OR the number with $80 to convert it to the tile ID
    STA format_buf-1     ; write it to output buffer
    LDABRA <format_buf-1, PrintNumber_Exit
                         ; A = start of string, then BNE (or BEQ) to exit

PrintNumber_2Digit:
    JSR FormatNumber_2Digits   ; format the number
    JSR TrimZeros_2Digits      ; trim leading zeros
    LDABRA <format_buf-2, PrintNumber_Exit      ; string start

PrintNumber_3Digit:            ; more of same...
    JSR FormatNumber_3Digits
    JSR TrimZeros_3Digits
    LDABRA <format_buf-3, PrintNumber_Exit

PrintNumber_4Digit:            ; more of same.
    JSR FormatNumber_4Digits   ; though... I don't think this 4-digit routine is used anywhere in the game
    JSR TrimZeros_4Digits
    LDABRA <format_buf-4, PrintNumber_Exit

PrintNumber_5Digit:
    JSR FormatNumber_5Digits
    JSR TrimZeros_5Digits
    LDABRA <format_buf-5, PrintNumber_Exit

PrintNumber_6Digit:
    JSR FormatNumber_6Digits
    JSR TrimZeros_6Digits
    LDABRA <format_buf-6, PrintNumber_Exit

PrintNumber_Exit:         ; on exit, each of the above routines put the low byte
    STA text_ptr          ; of the pointer in A -- store that to text_ptr, our output pointer
    LDA #>format_buf      ;  high byte
    STA text_ptr+1
    RTS                   ; and exit!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Trim Zeros  [$8E9F :: 0x38EAF]
;;
;;    These routines examine the formatted string in format_buf and
;;  replace leading '0' characters ($80) with a blank space character ($FF)
;;  which converts a string like "0100" to the desired " 100"
;;
;;    The ones digits (at format_buf-1) is never trimmed.  So you still get "  0"
;;  if the number is zero.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TrimZeros_6Digits:
    LDA format_buf-6   ; get first digit
    CMP #$80           ; check if it's "0" (tile $80)
    BNE TrimZeros_RTS  ; if it's not, exit
    LDA #$FF           ; if it is, replace with blank space ($FF)
    STA format_buf-6   ;  and continue on to lower digits

TrimZeros_5Digits:     ; etc
    LDA format_buf-5
    CMP #$80
    BNE TrimZeros_RTS
    LDA #$FF
    STA format_buf-5

TrimZeros_4Digits:
    LDA format_buf-4
    CMP #$80
    BNE TrimZeros_RTS
    LDA #$FF
    STA format_buf-4

TrimZeros_3Digits:
    LDA format_buf-3
    CMP #$80
    BNE TrimZeros_RTS
    LDA #$FF
    STA format_buf-3

TrimZeros_2Digits:
    LDA format_buf-2
    CMP #$80
    BNE TrimZeros_RTS
    LDA #$FF
    STA format_buf-2

TrimZeros_RTS:
    RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Format Number  [$8ED2 :: 0x38EE2]
;;
;;   These routines format a given number into a string that can be displayed
;;  on screen.  This involves converting to decimal base, which is a lengthy process
;;
;;  IN:  tmp = 3-byte value containing the number to format
;;
;;  OUT: format_buf = buffer filled with the formatted string to print (not explicitly null terminated
;;                    it is assumed format_buf is always null terminated)
;;
;;    There are several entry points to format the number into different lengths (6 digits, down to 2 digits)
;;  In the case of fewer digits, the first few bytes of the output buffer go unused.  IE:  for a 5-digit
;;  format, the first byte in format_buf remains unchanged.
;;
;;    Also, the high byte of the number is only used for 5 or 6 digit formats
;;    And the mid byte is only used for 3, 4, 5, 6 digits formats
;;
;;    Buffer will be filled with the '0' character (tile $80) for all digits that are 0,
;;  even if they're at the start of the string.  IE:  $0064 formatted as 4 digits will
;;  produce "0100" instead of " 100".  These leading zeros are later trimmed via the
;;  above TrimZeros series of routines
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


FormatNumber_6Digits:
    LDX #8            ; start with '900000' and work your way downward
@Loop:
    LDA tmp+2             ; get high byte of number
    CMP lut_DecD6_hi, X   ; see if it's greater than high byte of our check
    BEQ @Equal            ;  if it's equal.. to the check... need to do further comparisons
    BCS @Good             ;  if it's greater than the check... then this is the digit to print

@Less:                    ; if the number is less than the check...
    DEX                   ; decrement the check to look at next lowest
    BPL @Loop             ; and loop to continue checking until we've checked all 9 digits

      LDX #$80                   ;  code reaches here if we went through all 9 digits and there was no match
      STX format_buf-6           ; This means the number has no 6th digit -- so use the '0' character instead ($80)
      JMP FormatNumber_5Digits   ; And continue formatting by formatting for 5 digits

@Equal:                   ; if the high byte was equal to the check, we need to compare the middle byte
    LDA tmp+1             ;  load up the middle byte
    CMP lut_DecD6_md, X   ;  compare to check
    BEQ @Equal2           ; if equal, we still need to check the low byte.. so jump ahead to that
    BCC @Less             ; if less, digit is no good
    BCS @Good             ; if greater, digit is good

@Equal2:                  ; the final check for this digit
    LDA tmp               ; get low byte
    CMP lut_DecD6_lo, X   ; see if it's less than the check.  If it is, it's no good
    BCC @Less             ;  otherwise... if it's greater than or equal, it's good

@Good:                    ; code reaches here if the number is >= our check
    LDA tmp               ;  now.. we subtract the check from the number, so can can
    SEC                   ;  continue formatting for further digits
    SBC lut_DecD6_lo, X   ; subtract low byte of check
    STA tmp
    LDA tmp+1
    SBC lut_DecD6_md, X   ; mid byte
    STA tmp+1
    LDA tmp+2
    SBC lut_DecD6_hi, X   ; high byte
    STA tmp+2

    TXA                   ; lastly, X is our desired digit to print - 1 (ie:  X=0 means we want to print "1")
    CLC                   ;  so move X to A, and add #$81 (digits start at tile $80)
    ADC #$81              ;  and store it to our output buffer
    STA format_buf-6      ; afterwards, program flow moves seamlessly into the 5-digit format routine


FormatNumber_5Digits:         ; Flow in this routine is identical to the flow in
    LDX #8                    ;  FormatNumber_6Digits.  Rather than recomment it all, see that routine for details
@Loop:
    LDA tmp+2
    CMP lut_DecD5_hi, X
    BEQ @Equal
    BCS @Good

@Less:
    DEX
    BPL @Loop
 
      LDX #$80
      STX format_buf-5
      JMP FormatNumber_4Digits

@Equal:
    LDA tmp+1
    CMP lut_DecD5_md, X
    BEQ @Equal2
    BCC @Less
    BCS @Good

@Equal2:
    LDA tmp
    CMP lut_DecD5_lo, X
    BCC @Less


@Good:
    LDA tmp
    SEC
    SBC lut_DecD5_lo, X
    STA tmp
    LDA tmp+1
    SBC lut_DecD5_md, X
    STA tmp+1
    LDA tmp+2
    SBC lut_DecD5_hi, X
    STA tmp+2
    TXA
    CLC
    ADC #$81
    STA format_buf-5


FormatNumber_4Digits:     ; again... this routine is exactly the same as the above... so see that
    LDX #8                ;  for details.  Only difference here is there is no high byte check (4 digit numbers don't go beyond 2 bytes)
@Loop:
    LDA tmp+1
    CMP lut_DecD4_md, X
    BEQ @Equal
    BCS @Good

@Less:
    DEX
    BPL @Loop

      LDX #$80
      STX format_buf-4
      JMP FormatNumber_3Digits

@Equal:
    LDA tmp
    CMP lut_DecD4_lo, X
    BCC @Less

@Good:
    LDA tmp
    SEC
    SBC lut_DecD4_lo, X
    STA tmp
    LDA tmp+1
    SBC lut_DecD4_md, X
    STA tmp+1
    TXA
    CLC
    ADC #$81
    STA format_buf-4


FormatNumber_3Digits:  ; again... more of the same
    LDX #8
@Loop:
    LDA tmp+1
    CMP lut_DecD3_md, X
    BEQ @Equal
    BCS @Good

@Less:
    DEX
    BPL @Loop

      LDX #$80
      STX format_buf-3
      JMP FormatNumber_2Digits

@Equal:
    LDA tmp
    CMP lut_DecD3_lo, X
    BCC @Less

@Good:
    LDA tmp
    SEC
    SBC lut_DecD3_lo, X
    STA tmp
    LDA tmp+1
    SBC lut_DecD3_md, X
    STA tmp+1
    TXA
    CLC
    ADC #$81
    STA format_buf-3


FormatNumber_2Digits:   ; 2 digit numbers are done a bit differently... since they are only 1 byte in size
                        ;  no LUT is used... just keep subtracting 10 until you can't any more
    LDX #0              ; X is the counter to keep track of how many times we subtracted (thus, is the desired digit)
                        ;   start it at 0

    LDA tmp             ; get low digit into A
@Loop:
    CMP #10             ; if < 10, can't subtract anymore.. so we're done
    BCC @Done

      SBC #10           ; otherwise, subtract 10
      INX               ; increment our tens counter
      BNE @Loop         ; and loop (this will always branch, as X cannot be zero after above INX)

@Done:                  ; here, we're done.  A is now the ones and X is the tens
    ORA #$80            ;  so OR with $80 and output the ones digit
    STA format_buf-1
    TXA                 ; then grab X
    ORA #$80            ; OR it with $80
    STA format_buf-2    ; and output the tens digit
    RTS                 ; and we're done!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Decimal conversion LUTs  [$8FD1 :: 0x38FE1]
;;
;;   code uses these LUTs to do binary to decimal conversion to print
;;  numbers onto the screen.  Each group of tables has 9 entries, one for
;;  each digit.
;;
;;  2-digit numbers don't use LUTs, and you don't need a LUT for single digits.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

lut_DecD6_hi:  .BYTE ^100000,^200000,^300000,^400000,^500000,^600000,^700000,^800000,^900000
lut_DecD6_md:  .BYTE >100000,>200000,>300000,>400000,>500000,>600000,>700000,>800000,>900000
lut_DecD6_lo:  .BYTE <100000,<200000,<300000,<400000,<500000,<600000,<700000,<800000,<900000

lut_DecD5_hi:  .BYTE ^10000,^20000,^30000,^40000,^50000,^60000,^70000,^80000,^90000
lut_DecD5_md:  .BYTE >10000,>20000,>30000,>40000,>50000,>60000,>70000,>80000,>90000
lut_DecD5_lo:  .BYTE <10000,<20000,<30000,<40000,<50000,<60000,<70000,<80000,<90000

lut_DecD4_md:  .BYTE >1000,>2000,>3000,>4000,>5000,>6000,>7000,>8000,>9000
lut_DecD4_lo:  .BYTE <1000,<2000,<3000,<4000,<5000,<6000,<7000,<8000,<9000

lut_DecD3_md:  .BYTE >100,>200,>300,>400,>500,>600,>700,>800,>900
lut_DecD3_lo:  .BYTE <100,<200,<300,<400,<500,<600,<700,<800,<900


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  TalkToObject  [$902B :: 0x3903B]
;;
;;    Called to talk to a object on the map (townsperson, etc).
;;
;;  IN:        X = index to map object (to index 'mapobj' buffer)
;;        dlgsfx, dlgflg_reentermap = assumed to be zero
;;
;;  OUT:       A = ID of dialogue text to print onscreen
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TalkToObject:
    LDA mapobj_id, X    ; get the ID of the object they're talking to
    STA tmp+6           ; back the ID up for later

    LDY #0              ; mulitply the ID by 4 (4 bytes of talk data per object)
    STY tmp+5
    ASL A
    ROL tmp+5
    ASL A
    ROL tmp+5

    ADC #<lut_MapObjTalkData   ; and add the pointer to the start of the talk data table to that
    STA tmp+4
    LDA #>lut_MapObjTalkData
    ADC tmp+5
    STA tmp+5                  ; (tmp+4) now points to the talk data for this object

    LDY #0              ; copy the 4 bytes of talk data to the first 4 bytes of temp RAM
    LDA (tmp+4), Y
    STA tmp
    INY
    LDA (tmp+4), Y
    STA tmp+1
    INY
    LDA (tmp+4), Y
    STA tmp+2
    INY
    LDA (tmp+4), Y
    STA tmp+3

    LDA tmp+6           ; get the object ID (previously backed up)
    ASL A               ; *2 (two bytes per pointer) (high bit shifted into C)
    TAY                 ; throw in Y for indexing
    BCC :+              ; if C clear, we read from bottom half of table, otherwise, top half

     LDA lut_MapObjTalkJumpTbl+$100, Y  ; copy the desired pointer from the talk jump table
     STA tmp+6
     LDA lut_MapObjTalkJumpTbl+$101, Y
     STA tmp+7
     JMP (tmp+6)                        ; and jump to it, then exit

:    LDA lut_MapObjTalkJumpTbl, Y       ; same, but with low half of table
     STA tmp+6
     LDA lut_MapObjTalkJumpTbl+1, Y
     STA tmp+7
     JMP (tmp+6)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Check Game Event Flag  [$9079 :: 0x39089]
;;
;;  IN:   Y = object ID whose event flag you want to check
;;  OUT:  C = state of event flag
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CheckGameEventFlag:
    LDA game_flags, Y    ; Get the game flags using Y as index
    LSR A                ;   and shift the event flag into C
    LSR A
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Set Game Event Flag  [$907F :: 0x3908F]
;;
;;  IN:  Y = object ID whose flag to set
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SetGameEventFlag:
    LDA game_flags, Y   ; get the game flags
    ORA #GMFLG_EVENT    ; set the event bit
    STA game_flags, Y   ; and write back
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Clear Game Event Flag  [$9088 :: 0x39098]
;;
;;  IN:  Y = object ID whose flag to clear
;;
;;  This routine is unused by the original game
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ClearGameEventFlag:
    LDA game_flags, Y  ; get game flags
    AND #~GMFLG_EVENT  ; clear event bit
    STA game_flags, Y  ; write back
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  IsObjectVisible  [$9091 :: 0x390A1]
;;
;;  IN:  Y = ID of object to test
;;  OUT: C = set if object is visible
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

IsObjectVisible:
    LDA game_flags, Y     ; get the game flags using object ID as index
    LSR A                 ; shift object visibility flag into C
    RTS                   ; and exit


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  HideThisMapObject [$9096 :: 0x390A6]
;;
;;    Hides a map object, just like HideMapObject, but assumes the object
;;  exists in the current list of map objects (once and only once) -- and also
;;  assumes where that object is located is known.
;;
;;    As opposed to HideMapObject, which scans the entire list of current map
;;  objects and removes all occurances of the object.
;;
;;  IN:  Y = ID of object (to index 'game_flags')
;;       X = map object list index (to index 'mapobj')
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

HideThisMapObject:
    LDA game_flags, Y        ; get the game flags using object ID as index
    AND #~GMFLG_OBJVISIBLE   ; flip off the obj visibility flag (hide object)
    STA game_flags, Y        ; write it back

    LDA #0                   ; kill the object on the map by removing it from the list of
    STA mapobj_id, X         ; map objects

    RTS                      ; then exit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Show Map Object [$90A4 :: 0x390B4]
;;
;;    Makes the given object ID visible, and shows one object on the map which uses that
;;  ID (if there is one).
;;
;;  IN:   Y = ID of object to show
;;
;;    Note, this routine writes over 'tmp', so caution should be used when calling from
;;  one of the talk routines (which also use 'tmp' for something unrelated)
;;
;;    X remains unchanged by this routine
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ShowMapObject:
    STY tmp               ; back up the object ID

    LDA game_flags, Y
    ORA #GMFLG_OBJVISIBLE ; set the object visibility flag
    STA game_flags, Y     ; and write it back

    LDY #0                ; zero Y for indexing (our loop counter -- mapobj index)
  @Loop:
      LDA tmp             ; get the backed up object ID
      CMP mapobj_rawid, Y ; compare to this map object's raw ID
      BEQ @Found          ; if they match, we found the object!

      TYA                 ; otherwise, increment our loop counter to look at
      CLC                 ; next map object
      ADC #$10
      TAY

      CMP #$F0            ; and loop until all 15 map objects checked
      BCC @Loop
    RTS

  @Found:                 ; if we found the object we need to show...
    STA mapobj_id, Y      ; .. write the raw ID to the used ID to make the object visible
    RTS                   ; and exit



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  TalkBattle  [$90C5 :: 0x390D5]
;;
;;    Triggers a battle via talking to someone (Garland, Astos, etc)
;;
;;  IN:  A = ID of battle formation to trigger
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TalkBattle:
    STA btlformation     ; record the formation
    LDA #TP_BATTLEMARKER ; then overwrite the tile properties with the battle marker bit
    STA tileprop         ;    so a battle is triggered when the dialogue box closes
    RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  TalkNormTeleport  [$90CC :: 0x390DE]
;;
;;    Triggers a normal teleport (standard map->standard map) via talking
;;  to someone (ie:  when you rescue the princess)
;;
;;  IN:  A = normal teleport ID
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TalkNormTeleport:
    STA tileprop+1       ; overwrite tile properties so set up a normal teleport
    LDA #TP_TELE_NORM    ;  with given teleport ID
    STA tileprop         ; This will cause the teleport to happen as soon as the
    RTS                  ; dialogue box closes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Jump table for actions when talking to townspeople  [$90D3 :: 0x390E3]
;;
;;    This is a ginourmous jump table.  It consists of $D0 entries -- one for each object ID.
;;  When you talk to an object on the map, its ID is used to index this table and the appropriate
;;  routine is jumped to.  See TalkRoutines below for further explanation.


lut_MapObjTalkJumpTbl:

 ;; No object (ID=00)
  .WORD Talk_None

 ;; Several special objects  (ID=01-1F)
  .WORD Talk_KingConeria, Talk_Garland, Talk_Princess1, Talk_Bikke, Talk_ElfDoc, Talk_ElfPrince, Talk_Astos     ; 01-07
  .WORD Talk_Nerrick, Talk_Smith, Talk_Matoya, Talk_Unne, Talk_Vampire, Talk_Sarda, Talk_Bahamut, Talk_ifvis    ; 08-0F
  .WORD Talk_SubEng, Talk_CubeBot, Talk_Princess2, Talk_Fairy, Talk_Titan, Talk_CanoeSage, Talk_norm, Talk_norm ; 10-17
  .WORD Talk_Replace, Talk_Replace, Talk_fight, Talk_fight, Talk_fight, Talk_fight, Talk_fight, Talk_Unused     ; 18-1F

 ;; Coneria people (ID=20-39)
  .WORD Talk_ifvis, Talk_ifvis, Talk_ifvis, Talk_ifitem, Talk_ifvis, Talk_ifvis, Talk_Invis, Talk_ifbridge    ; 20-27
  .WORD Talk_ifvis, Talk_ifvis, Talk_ifvis, Talk_ifvis, Talk_ifitem, Talk_ifvis, Talk_ifitem, Talk_ifevent    ; 28-2F
  .WORD Talk_ifvis, Talk_ifvis, Talk_GoBridge, Talk_ifvis, Talk_4Orb, Talk_norm, Talk_norm, Talk_ifvis        ; 30-37
  .WORD Talk_ifvis, Talk_norm                                                                                 ; 38-39

 ;; Sky Warriors  (ID=3A-3E)
  .WORD Talk_4Orb, Talk_4Orb, Talk_4Orb, Talk_4Orb, Talk_4Orb                                                 ; 3A-3E

 ;; The rest  (ID=3F-CF)
  .WORD Talk_norm                                                                                                       ; 3F
  .WORD Talk_norm, Talk_norm, Talk_ifevent, Talk_ifevent, Talk_ifevent, Talk_norm, Talk_ifevent, Talk_ifitem            ; 40-47
  .WORD Talk_norm, Talk_ifevent, Talk_ifevent, Talk_ifitem, Talk_ifevent, Talk_ifevent, Talk_ifevent, Talk_ifevent      ; 48-4F
  .WORD Talk_ifevent, Talk_ifevent, Talk_norm, Talk_ifcanoe, Talk_ifitem, Talk_ifevent, Talk_ifevent, Talk_norm         ; 50-57
  .WORD Talk_norm, Talk_ifcanal, Talk_norm, Talk_norm, Talk_ifitem, Talk_ifitem, Talk_norm, Talk_ifcanal                ; 58-5F
  .WORD Talk_ifkeytnt, Talk_norm, Talk_ifcanal, Talk_norm, Talk_norm, Talk_norm, Talk_norm, Talk_norm                   ; 60-67
  .WORD Talk_ifvis, Talk_norm, Talk_ifearthvamp, Talk_ifitem, Talk_ifvis, Talk_ifearthvamp, Talk_norm, Talk_norm        ; 68-6F
  .WORD Talk_ifitem, Talk_ifairship, Talk_norm, Talk_ifevent, Talk_ifitem, Talk_norm, Talk_norm, Talk_norm              ; 70-77
  .WORD Talk_4Orb, Talk_4Orb, Talk_4Orb, Talk_4Orb, Talk_4Orb, Talk_4Orb, Talk_4Orb, Talk_ifitem                        ; 78-7F
  .WORD Talk_ifearthfire, Talk_ifitem, Talk_norm, Talk_norm, Talk_CoOGuy, Talk_norm, Talk_norm, Talk_norm               ; 80-87
  .WORD Talk_norm, Talk_norm, Talk_norm, Talk_norm, Talk_norm, Talk_norm, Talk_norm, Talk_norm                          ; 88-8F
  .WORD Talk_norm, Talk_norm, Talk_norm, Talk_norm, Talk_ifitem, Talk_norm, Talk_norm, Talk_norm                        ; 90-97
  .WORD Talk_norm, Talk_norm, Talk_ifitem, Talk_ifevent, Talk_norm, Talk_norm, Talk_norm, Talk_norm                     ; 98-9F
  .WORD Talk_norm, Talk_norm, Talk_CubeBotBad, Talk_norm, Talk_norm, Talk_norm, Talk_norm, Talk_norm                    ; A0-A7
  .WORD Talk_norm, Talk_ifitem, Talk_norm, Talk_norm, Talk_norm, Talk_norm, Talk_norm, Talk_ifevent                     ; A8-AF
  .WORD Talk_norm, Talk_norm, Talk_norm, Talk_norm, Talk_norm, Talk_norm, Talk_norm, Talk_norm                          ; B0-B7
  .WORD Talk_norm, Talk_norm, Talk_norm, Talk_Chime, Talk_ifevent, Talk_ifevent, Talk_ifevent, Talk_ifevent             ; B8-BF
  .WORD Talk_ifevent, Talk_ifevent, Talk_ifevent, Talk_ifevent, Talk_ifevent, Talk_ifevent, Talk_ifevent, Talk_ifevent  ; C0-C7
  .WORD Talk_ifevent, Talk_ifevent, Talk_BlackOrb, Talk_norm, Talk_norm, Talk_norm, Talk_norm, Talk_norm                ; C8-CF


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Hide Map Object [$9273 :: 0x39283]
;;
;;    Makes the given object ID invisible, and hides one object on the map which uses that
;;  ID (if there is one).
;;
;;  IN:   Y = ID of object to hide
;;
;;    Note, this routine writes over 'tmp', so caution should be used when calling from
;;  one of the talk routines (which also use 'tmp' for something unrelated)
;;
;;    X remains unchanged by this routine
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


HideMapObject:
    STY tmp                   ; back up the object ID
    LDA game_flags, Y
    AND #~GMFLG_OBJVISIBLE    ; clear the visibility flag for this object
    STA game_flags, Y

    LDY #0                ; zero Y for loop counter / mapobject indexing
  @Loop:
      LDA tmp             ; get the object ID
      CMP mapobj_id, Y    ; see if it matches this object's ID
      BEQ @Found          ; if it does, we found the object we need to hide!

      TYA                 ; otherwise, increment the loop counter to look at the next object
      CLC
      ADC #$10
      TAY

      CMP #$F0            ; and keep looping until all 15 map objects checked
      BCC @Loop
    RTS

  @Found:                 ; if we've found the map object we're hiding...
    LDA #0
    STA mapobj_id, Y      ; set it's ID to zero to hide it
    RTS                   ;  and exit


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  TalkRoutines  [$9296 :: 0x392A6]
;;
;;    One of these routines is called each time you talk to a map object.
;;  It determines the action performed by the object (if any) and the text that is
;;  to appear in dialogue on-screen.
;;
;;    Routines are not JSR'd to directly.  They are all accessed via a jump table
;;  See lut_MapObjTalkJumpTbl above.
;;
;;    Before jumping to these routines, the game fills the first 4 bytes of temp
;;  RAM (tmp through tmp+3) with the data for the object being talked to.  See
;;  lut_MapObjTalkData for this data.
;;
;;    Not all of these bytes go used -- sometimes only one is used, but at least one is used
;;  always (except for dummy routines that are never called).  To save space/time, these values
;;  will be referred to by index in brakets in the routines below.  IE:  [0] for the first
;;  byte of data, [1] for the next, then [2], [3].
;;
;;    Most of the time (but not always), [1], [2], and [3] are only used for a dialogue
;;  string ID.  Sometimes, though, they might be used for an object ID as part of a condition
;;  check.  [0] is used for the more dynamic routines that are used for several different
;;  but similar objects... and is always used for a condition check.  Several other of these
;;  routines are (needlessly) hardcoded to be for a specific object.
;;
;;    Some objects hide themselves after you talk to them (like Garland, the fiend orbs, etc,
;;  anything you fight).  This is usually accomplished by loading the object ID into Y and
;;  calling HideThisMapObject, instead of calling the more general HideMapObject routine.
;;  See HideThisMapObject for details on the differences between the two.
;;
;;  IN:   tmp - tmp+3 = map object's data
;;                  X = runtime map object list index (to index 'mapobj') -- only used
;;                        for HideThisMapObject.
;;             dlgsfx = assumed to be zero
;;
;;  OUT:            A = dialogue ID of text to print
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 ;; none (Blank sprite -- object ID=0)  [$9296 :: 0x392A6]

Talk_None:
    RTS

 ;; King of Coneria  [$9297 :: 0x392A7]
 ;;  [1] if princess kidnapped
 ;;  [2] if princess rescued but bridge not built yet
 ;;  [3] if bridge has been built

Talk_KingConeria:
    LDY #OBJID_PRINCESS_2   ; see if the saved princess is visible (princess has been rescued)
    JSR IsObjectVisible
    BCS :+                  ; if not...
      LDA tmp+1             ;  ... print [1]
      RTS

:   LDA bridge_vis          ; otherwise (princess rescued), see if bridge is visible
    BEQ :+                  ; if it is...
      LDA tmp+3             ;  ... print [3]
      RTS
                            ; otherwise (princess rescued, bridge not visible)
:   LDA tmp+2               ; print [2]
    INC bridge_vis          ; make bridge visible
    INC dlgsfx              ; play fanfare
    RTS

 ;; Garland (regular, not the ToFR version)  [$92B1 :: 0x392C1]
 ;;  [1] always

Talk_Garland:
    LDY #OBJID_GARLAND
    JSR HideThisMapObject   ; hide (kill) the Garland object (this object)

    LDA #BTL_GARLAND
    JSR TalkBattle          ; trigger the battle with Garland

    LDA tmp+1               ; and print [1]
    RTS

 ;; Kidnapped Princess (in the ToF)  [$92BE :: 0x392CE]
 ;;  [1] always

Talk_Princess1:
    LDY #OBJID_PRINCESS_1
    JSR HideThisMapObject   ; hide the kidnapped princess (this object)

    LDY #OBJID_PRINCESS_2
    JSR ShowMapObject       ; show (replace with) the rescued princess

    LDA #NORMTELE_SAVEDPRINCESS  ; trigger the teleport back to Coneria Castle
    JSR TalkNormTeleport

    LDA tmp+1               ; and print [1]
    RTS

 ;; Bikke the Pirate  [$92D0 :: 0x392E0]
 ;;  [1] if haven't fought him yet
 ;;  [2] if fought him but haven't taken his ship yet
 ;;  [3] after you have the ship

Talk_Bikke:
    LDY #OBJID_BIKKE
    JSR CheckGameEventFlag  ; check Bikke's event flag to see if we fought him yet
    BCS @AlreadyFought      ; if we already have, skip ahead

      JSR SetGameEventFlag  ; otherwise, set event flag to mark him as fought
      LDA #BTL_BIKKE        ; then start a battle with Bikke (his pirates)
      JSR TalkBattle
      LDA tmp+1             ; and print [1]
      RTS

  @AlreadyFought:        ; if we've already fought bikke...
    LDA ship_vis         ; see if the party has the ship
    BNE @HaveShip        ; if they do, skip ahead

      INC ship_vis            ; otherwise, give the player the ship
      LDY #OBJID_PIRATETERR_1
      JSR ShowMapObject       ; and show a bunch of scaredy-cat townspeople that the pirates
      LDY #OBJID_PIRATETERR_2 ;  were terrorizing
      JSR ShowMapObject
      LDY #OBJID_PIRATETERR_3
      JSR ShowMapObject

      LDA tmp+2          ; print [2]
      INC dlgsfx         ; and play fanfare
      RTS

  @HaveShip:           ; otherwise, if we have the ship already
    LDA tmp+3          ; just print [3]
    RTS

 ;; Elf Doctor (taking care of the sleeping prince)  [$9301 :: 0x09311]
 ;;  [1] if prince is sleeping and you don't have the herb
 ;;  [2] if prince is sleeping and you DO have the herb
 ;;  [3] once prince is awake

Talk_ElfDoc:
    LDY #OBJID_ELFPRINCE    ; check the elf prince's event flag
    JSR CheckGameEventFlag  ;  it will be clear if the prince is still asleep
    BCC @PrinceAsleep       ; if prince is awake...
      LDA tmp+3             ;  .. then simply print [3]
      RTS

  @PrinceAsleep:          ; if the prince is still asleep
    LDA item_herb         ; check to see if the player has any herb
    BNE @HaveHerb         ; if not...
      LDA tmp+1           ; .. then simply print [1]
      RTS

  @HaveHerb:              ; prince is asleep and you have herb!
    DEC item_herb         ; take away the herb from the party
    JSR SetGameEventFlag  ; set the prince's event flag (wake him up)
    INC dlgsfx            ; play fanfare
    LDA tmp+2             ; and print [2]
    RTS

  ;; Elf Prince  [$931E :: 0x3932E]
  ;;  [3] if sleeping
  ;;  [1] if awake and you don't have the key yet
  ;;  [2] once you have the key

Talk_ElfPrince:
    LDY #OBJID_ELFPRINCE    ; check the prince's event flag to see if he's sleeping
    JSR CheckGameEventFlag
    BCS @Awake              ; if he's still sleeping...
      LDA tmp+3             ;  .. then just print [3]
      RTS

  @Awake:               ; if prince is awake...
    LDA item_mystickey  ; check to see if the player has the key
    BEQ @GiveTheKey     ; if they already do...
      LDA tmp+2         ; .. then just print [2]
      RTS

  @GiveTheKey:          ; otherwise, we need to give them the key
    INC item_mystickey  ; give it to them
    INC dlgsfx          ; play fanfare
    LDA tmp+1           ; and print [1]
    RTS

  ;; Astos  [$9338 :: 0x39348]
  ;;  [1] if you don't have the Crown
  ;;  [2] if you do

Talk_Astos:
    LDA item_crown         ; see if the player has the crown
    BNE @HaveCrown         ; if they don't...
      LDA tmp+1            ; ... simply print [1]
      RTS

  @HaveCrown:              ; otherwise (they have the crown)
    INC item_crystal       ; give them the Crystal
    LDY #OBJID_ASTOS
    JSR HideThisMapObject  ; hide (kill) Astos' map object (this object)

    LDA #BTL_ASTOS         ; trigger battle with Astos
    JSR TalkBattle

    INC dlgsfx             ; play fanfare
    LDA tmp+2              ; and print [2]
    RTS


  ;; Nerrick (dwarf who opens the Canal)  [$9352 :: 0x39362]
  ;;  [1] if you don't have the TNT
  ;;  [2] if you do

Talk_Nerrick:
    LDA item_tnt           ; check to see if the player has TNT
    BNE @HaveTNT           ; if not...
      LDA tmp+1            ; ... simply print [1]
      RTS

  @HaveTNT:
    DEC item_tnt           ; otherwise, remove the TNT from the party
    LDA #0                 ; kill the Canal
    STA canal_vis
    LDY #OBJID_NERRICK     ; hide Nerrick (this object)
    JSR HideThisMapObject

    INC dlgsfx             ; play fanfare
    LDA tmp+2              ; and print [2]
    RTS

  ;; Smith (dwarf blacksmith)  [$936C :: 0x3937C]
  ;;  [1] if you don't have the adamant
  ;;  DLGID_DONTBEGREEDY  if you have adamant, but no free weapon slot (HARDCODED!)
  ;;  [2] if you have adamant and a free weapon slot
  ;;  [3] after you've handed over the adamant

Talk_Smith:
    LDY #OBJID_SMITH       ; check Smith's event flag to see if we already made
    JSR CheckGameEventFlag ;  the Xcalbur for the party
    BCC @WantSword         ; if he already made it....
      LDA tmp+3            ; ... then simply print [3]
      RTS

  @WantSword:
    LDA item_adamant       ; otherwise check to see if party has the Adamant
    BNE @HaveAdamant       ; if not...
      LDA tmp+1            ; ... simply print [1]
      RTS

  @HaveAdamant:             ; otherwise, make the sword!
    JSR FindEmptyWeaponSlot ; find an empty slot
    BCS @WontFit            ; if no empty slot, sword won't fit

     LDA #WPNID_XCALBUR     ; put the XCalbur in the previously found slot
     STA ch_stats, X
     LDY #OBJID_SMITH       ; set Smith's event flag to mark that we made the sword
     JSR SetGameEventFlag
     DEC item_adamant       ; take the Adamant away from the party
     INC dlgsfx             ; play fanfare
     LDA tmp+2              ; and print [2]
     RTS

  @WontFit:                 ; if XCalbur won't fit in the inventory...
    LDA #DLGID_DONTBEGREEDY ; print "Don't be Greedy" text (note:  hardcoded)
    RTS

  ;; Matoya (witch with the herb) [$9398 :: 0x393A8]
  ;;  [1] if prince is asleep and you don't have the crystal
  ;;  [2] if you have the crystal
  ;;  [3] if you have the herb, or if prince is awake

Talk_Matoya:
    LDA item_herb           ; see if they already have the herb
    BEQ @NoHerb             ; if not... jump ahead.  If so, do the default

  @Default:
    LDA tmp+3               ; default just prints [3]
    RTS

  @NoHerb:
    LDA item_crystal          ; see if the player has the crystal
    BNE @DoExchange           ; if they do, exchange!

     LDY #OBJID_ELFPRINCE     ; otherwise, see if the elf prince is still asleep
     JSR CheckGameEventFlag   ;  by checking his game flag
     BCS @Default             ; if he's awake, revert to default action

      LDA tmp+1               ; otherwise, elf is still asleep.  print [1]
      RTS

  @DoExchange:          ; exchanging Crystal for Herb
    INC item_herb       ; give player herb
    DEC item_crystal    ; take away crystal
    INC dlgsfx          ; play fanfare
    LDA tmp+2           ; print [2]
    RTS

  ;; Dr. Unne  [$93BA :: 0x393CA]
  ;;  [1] if you don't know Lefeinish, don't have slab
  ;;  [2] if you have Slab, but don't know Lefeinish
  ;;  [3] if you know Lefeinish

Talk_Unne:
    LDY #OBJID_UNNE         ; Check Unne's event flag to see if he taught you
    JSR CheckGameEventFlag  ;   Lefeinish yet
    BCC @NeedToLearn        ; if he already taught you...
      LDA tmp+3             ; .. print [3]
      RTS

  @NeedToLearn:
    LDA item_slab           ; otherwise, check to see if the party has the Slab
    BNE @Teach              ; if they don't...
      LDA tmp+1             ; .. print [1]
      RTS

  @Teach:                   ; if they have the slab... teach them!
    DEC item_slab           ; take away the slab
    JSR SetGameEventFlag    ; set Unne's event flag (teach you lefeinish)
    INC dlgsfx              ; fanfare
    LDA tmp+2               ; print [2]
    RTS

  ;; Vampire  [$93D7 :: 0x393E7]
  ;;  [1] always

Talk_Vampire:
    LDY #OBJID_VAMPIRE      ; Kill/Hide the Vampire object (this object)
    JSR HideThisMapObject
    LDA #BTL_VAMPIRE        ; Trigger a battle with the Vampire
    JSR TalkBattle
    LDA tmp+1               ; and print [1]
    RTS

  ;; Sarda (gives you the Rod)  [$93E4 :: 0x393F4]
  ;;  [1] if vampire has been killed but you don't have the Rod yet
  ;;  [2] if you have the Rod or Vampire is still alive

Talk_Sarda:
    LDA item_rod            ; see if the party already has the Rod
    BNE @Default            ; if they do, skip to default

    LDY #OBJID_VAMPIRE      ; see if they killed the vampire yet (seems pointless -- can't reach Sarda
    JSR IsObjectVisible     ;   until you kill the vampire)
    BCS @Default            ; if Vampire is still alive, skip to default

    INC item_rod            ; otherwise, reward them with the Rod
    INC dlgsfx              ; play fanfare
    LDA tmp+1               ; and print [1]
    RTS

  @Default:
    LDA tmp+2               ; default just prints [2]
    RTS

  ;; Bahamut  [$93FB :: 0x3940B]
  ;;  [1] if haven't been promoted, and don't have the Tail
  ;;  [2] if haven't been promoted, and DO have the Tail
  ;;  [3] once promoted

Talk_Bahamut:
    LDY #OBJID_BAHAMUT      ; Check Bahamut's Event flag (see if he promoted you yet)
    JSR CheckGameEventFlag
    BCC @CheckTail          ; if he has...
      LDA tmp+3             ; ... print [3]
      RTS

  @CheckTail:
    LDA item_tail           ; he hasn't promoted you yet... so check to see if you have the tail
    BNE @ClassChange        ; if you don't...
      LDA tmp+1             ; ... print [1]
      RTS

  @ClassChange:             ; otherwise (have tail), do the class change!
    DEC item_tail           ; remove the tail from inventory
    JSR SetGameEventFlag    ; set Bahamut's event flag
    JSR DoClassChange       ; do class change
    INC dlgsfx              ; play fanfare
    LDA tmp+2               ; and print [2]
    RTS

  ;; Generic condition check based on object visibility  [$941B :: 0x3942B]
  ;;  [1] if object ID [0] is hidden
  ;;  [2] if it's visible

Talk_ifvis:
    LDY tmp                 ; check to see if object [0] is visible
    JSR IsObjectVisible
    BCS :+                  ; if it is, print [2]
      LDA tmp+1             ; otherwise, print [1]
      RTS
:   LDA tmp+2
    RTS

  ;; Submarine Engineer (in Onrac, blocking enterance to Sea Shrine)  [$9428 :: 0x39438]
  ;;  [1] if you don't have the Oxyale
  ;;  [2] if you do

Talk_SubEng:
    LDA item_oxyale         ; see if the player has the Oxyale
    BNE :+                  ; if they don't...
      LDA tmp+1             ; ...print [1]
      RTS
:   LDY #OBJID_SUBENGINEER  ; otherwise (they do)
    JSR HideThisMapObject   ; hide the sub engineer object (this object)
    LDA tmp+2               ; and print [2]
    RTS

  ;; Waterfall Robot (gives you the cube)  [$9438 :: 0x39448]
  ;;  [1] if you don't have the Cube
  ;;  [2] if you do

Talk_CubeBot:
    LDA item_cube        ; see if the player has the cube
    BEQ :+               ; if they do...
      LDA tmp+2          ; ... print [2]
      RTS
:   INC item_cube        ; if they don't, give them the cube
    LDA tmp+1            ; print [1]
    INC dlgsfx           ; and play fanfare
    RTS

  ;; Rescued Princess (in Coneria Castle)  [$9448 :: 0x39458]
  ;;  [1] if you don't have the Lute
  ;;  [2] if you do

Talk_Princess2:
    LDA item_lute          ; see if the player has the Lute
    BEQ :+
      LDA tmp+2            ; if they do, print [2]
      RTS
:   INC item_lute          ; otherwise, give them the lute
    INC dlgsfx             ; play fanfare
    LDA tmp+1              ; and print [1]
    RTS

  ;; Fairy (trapped in the Bottle)  [$9458 :: 0x39468]
  ;;  [1] if you don't have the Oxyale
  ;;  [2] if you do

Talk_Fairy:
    LDA item_oxyale        ; see if the player has the oxyale
    BEQ :+
      LDA tmp+2            ; if they do, print [2]
      RTS
:   INC item_oxyale        ; otherwise, give them the oxyale
    INC dlgsfx             ; play fanfare
    LDA tmp+1              ; print [1]
    RTS

  ;; Titan  [$9468 :: 0x39478]
  ;;  [1] if you don't have the Ruby
  ;;  [2] if you do

Talk_Titan:
    LDA item_ruby          ; does the player have the ruby?
    BNE :+                 ; if not...
      LDA tmp+1            ; ... simply print [1]
      RTS
:   DEC item_ruby          ; if they do have it, take it away
    LDY #OBJID_TITAN       ; hide/remove Titan (this object)
    JSR HideThisMapObject
    LDA tmp+2              ; print [2]
    INC dlgsfx             ; and play fanfare
    RTS

  ;; Nameless sage who gives you the canoe  [$947D :: 0x3948D]
  ;;  [1] if you don't have the canoe and Earth Orb has been lit
  ;;  [2] if you have the canoe or if Earth Orb hasn't been lit yet

Talk_CanoeSage:
    LDA has_canoe         ; see if party has canoe
    BNE @Default          ; if they do, show default text
      LDA orb_earth       ; if they have the canoe, check to see if they've recovered the Earth Orb
      BEQ @Default        ; if not, show default
        INC has_canoe     ; otherwise, give them the canoe
        INC dlgsfx        ; play fanfare
        LDA tmp+1         ; and print [1]
        RTS
  @Default:
    LDA tmp+2             ; for default, just print [2]
    RTS

  ;; Generic eventless object  [$9492 :: 0x394A2]
  ;;  [1] always

Talk_norm:
    LDA tmp+1             ; uneventful object -- just print [1]
    RTS

  ;; Replacable Object (first 2 of the 3 ToFR Garlands)  [$9495 :: 0x394A5]
  ;;  [1] always --- hide THIS object whose ID is [0], and show object ID [3]

Talk_Replace:
    LDY tmp               ; get object ID from [0]
    JSR HideThisMapObject ; hide that object (this object)
    LDY tmp+3
    JSR ShowMapObject     ; show object ID [3]
    LDA tmp+1             ; and print [1]
    RTS

  ;; Mysterious Sage (in the CoO -- disappears after you talk to him) [$94A2 :: 0x394B2]
  ;;  [1] always --- hide THIS object whose ID is [0]

Talk_CoOGuy:
    LDY tmp
    JSR HideThisMapObject ; hide object ID [0] (this object)
    LDA tmp+1             ; and print [1]
    RTS

  ;; Generic fight (Final ToFR Garland, Fiends)  [$94AA :: 0x394BA] 
  ;;  [1] always --- hide THIS object whose ID is [0], and initiate battle ID [3]

Talk_fight:
    LDY tmp
    JSR HideThisMapObject ; hide object [0] (this object)
    LDA tmp+3
    JSR TalkBattle        ; trigger battle ID [3]
    LDA tmp+1             ; and print [1]
    RTS

  ;; Unused object / waste of space  [$94B7 :: 0x394C7]
  ;;  note, though, that the label is in fact used (it is in the jump table)

Talk_Unused:
    RTS

  ;; Generic condition based on item index  [$94B8 :: 0x394C8]
  ;;  [1] if party contains at least one of item index [0]
  ;;  [2] otherwise (none of that item)

Talk_ifitem:
    LDY tmp              ; use [0] as an item index
    LDA items, Y         ; see if the player has said item
    BEQ @DontHave        ; if they do have it
      LDA tmp+1          ; print [1]
      RTS
  @DontHave:
    LDA tmp+2            ; otherwise, print [2]
    RTS

  ;; Invisible Lady  (infamous invisible lady in Coneria Castle)  [$94C5 :: 0x394D5]
  ;;  [1] if princess not rescued and you don't have the Lute
  ;;  [2] if princess rescued or you do have the Lute

Talk_Invis:
    LDY #OBJID_PRINCESS_2
    JSR IsObjectVisible  ; see if the princess has been rescued (rescued princess object visible)
    BCS :+               ; if she's not rescued...
      LDA item_lute
      BNE :+             ; ... and if you don't have the lute (redundant)
        LDA tmp+1        ; print [1]
        RTS
:   LDA tmp+2            ; otherwise print [2]
    RTS


  ;; Condition based on whether or not the bridge has been built [$94D7 :: 0x394E7]
  ;;   this condition is not used by any objects in the game, however it's still in the jump table
  ;;  [1] if bridge is built
  ;;  [2] otherwise

Talk_ifbridge:
    LDA bridge_vis       ; see if bridge is visible (has been built)
    BEQ :+               ; if it has...
      LDA tmp+1          ; print [1]
      RTS
:   LDA tmp+2            ; otherwise, print [2]
    RTS


  ;; Generic condition based on game event flag [$94E2 :: 0x394F2]
  ;;  [1] if game event flag ID [0] is clear
  ;;  [2] if it's set

Talk_ifevent:
    LDY tmp                 ; use [0] as an event flag index
    JSR CheckGameEventFlag  ;  see if that event flag has been set
    BCS :+                  ; if not...
      LDA tmp+1             ; ... print [1]
      RTS
:   LDA tmp+2               ; otherwise print [2]
    RTS

 ; Unused (truely unused -- no entry in the jump table)
    RTS

  ;; Some guard in Coneria town  [$94F0 :: 0x39500]
  ;;  [1] if princess has been saved, but bridge isn't built yet
  ;;  [2] if princess still kidnapped or bridge is built

Talk_GoBridge:
    LDY #OBJID_PRINCESS_2   ; check to see if princess has been rescued
    JSR IsObjectVisible
    BCC :+                  ; if she has...
      LDA bridge_vis        ; see if bridge has been built
      BNE :+                ; if not... (princess saved, but bridge not built yet...)
        LDA tmp+1           ;  ... print [1]
        RTS
:   LDA tmp+2               ; otherwise print [2]
    RTS

  ;; The Black Orb  [$9502 :: 0x39512]
  ;;  [1] if all 4 orbs are lit
  ;;  [2] otherwise

Talk_BlackOrb:
    LDA orb_fire            ; see if all orbs have been lit
    AND orb_water
    AND orb_air
    AND orb_earth
    BEQ @NotAllLit          ; if all of them are lit...
      LDY #OBJID_BLACKORB   ; hide the black orb object (this object)
      JSR HideThisMapObject
      INC dlgsfx            ; play TC sound effect  (not fanfare)
      INC dlgsfx
      LDA tmp+1             ; and print [1]
      RTS
  @NotAllLit:
    LDA tmp+2               ; otherwise, (not all orbs lit), print [2]
    RTS

  ;; Conditional Check for 4 Orbs  [$951F :: 0x3952F]
  ;;  [1] if all 4 orbs lit
  ;;  [2] otherwise

Talk_4Orb:
    LDA orb_fire        ; see if all orbs have been lit
    AND orb_water
    AND orb_air
    AND orb_earth
    BEQ :+              ; if they have...
      LDA tmp+1         ; print [1]
      RTS
:   LDA tmp+2           ; otherwise (not all of them lit)
    RTS                 ;  print [2]

 ;; Conditional check for canoe (some lady in Elfland)  [$9533 :: 0x39543]
 ;;  [1] if you have the canoe
 ;;  [2] if you don't

Talk_ifcanoe:
    LDA has_canoe       ; see if the player has the canoe
    BEQ @NoCanoe        ; if they do...
      LDA tmp+1         ; print [1]
      RTS
  @NoCanoe:             ; otherwise (no canoe), print [2]
    LDA tmp+2
    RTS

 ;; Conditional check for Canal (some dwarves)  [$953E :: 0x3954E]
 ;;  [1] if Canal has been opened up
 ;;  [2] if Canal is still blocked

Talk_ifcanal:
    LDA canal_vis       ; see if the canal has been blown yet
    BNE @CanalBlocked   ; if it has been opened up already
      LDA tmp+1         ;   print [1]
      RTS
  @CanalBlocked:        ; otherwise (still blocked)
    LDA tmp+2           ;   print [2]
    RTS

 ;; Conditional check for key+TNT  (some dwarf?)  [$9549 :: 0x39559]
 ;;  [1] if have key, but not TNT
 ;;  [2] if no key, or have TNT

Talk_ifkeytnt:
    LDA item_mystickey  ; check to see if the party has the key
    BEQ :+              ; if they do...
      LDA item_tnt      ; check to see if they have the TNT
      BNE :+            ; if they don't  (key but no TNT)
        LDA tmp+1       ;   print [1]
        RTS
:   LDA tmp+2           ; otherwise, print [2]
    RTS

 ;; Conditional check for Earth Orb and Vampire (people in Melmond) [$9559 :: 0x39569]
 ;;  [1] if Vampire is dead and Earth Orb not lit
 ;;  [2] if Vampire lives, or Earth Orb has been lit

Talk_ifearthvamp:
    LDY #OBJID_VAMPIRE    ; see if the vampire has been killed yet
    JSR IsObjectVisible
    BCS :+                ; if not...
      LDA orb_earth       ; check to see if player revived earth orb
      BNE :+              ; if not... (Vampire killed, Earth Orb not lit yet)
        LDA tmp+1         ; print [1]
        RTS
:   LDA tmp+2             ; otherwise print [2]
    RTS

 ;; Conditional check for airship  [$956B :: 0x3957B]
 ;;  [1] if you don't have the airship
 ;;  [2] if you do

Talk_ifairship:
    LDA airship_vis      ; see if the party has the airship
    BNE @HaveAirship     ; if they don't....
      LDA tmp+1          ; print [1]
      RTS
  @HaveAirship:
    LDA tmp+2            ; if they do, print [2]
    RTS

 ;; Conditional check for earth/fire orbs [$9576 :: 0x39586]
 ;;  [1] if Earth Orb not lit, or Fire Orb Lit
 ;;  [2] if Earth Orb lit, and Fire Obj not lit

Talk_ifearthfire:
    LDA orb_earth        ; see if the Earth Orb has been recovered
    BEQ :+               ; if it has...
      LDA orb_fire       ; check Fire Orb
      BNE :+             ; if it's still dark (Earth lit, but not Fire)
        LDA tmp+1        ; print [1]
        RTS
:   LDA tmp+2            ; otherwise, print [2]
    RTS

 ;; Unused duplicate of Cube bot (doesn't play fanfare)  [$9586 :: 0x39596]
 ;;  [1] if you don't have the Cube
 ;;  [2] if you do.

Talk_CubeBotBad:
    LDA item_cube        ; see if they have the Cube
    BEQ :+               ; if they do...
      LDA tmp+2          ;  print [2]
      RTS
:   INC item_cube        ; otherwise, give them the cube
    LDA tmp+1            ; and print [1]
    RTS                  ; but no fanfare!

 ;; Guy with the Chime (in Lefein)  [$9594 :: 0x395A4]
 ;;  [1] if you speak Lefeinish, and don't have the Chime
 ;;  [2] if you speak Lefeinish, and do have the Chime
 ;;  [3] if you don't speak Lefeinish

Talk_Chime:
    LDY #OBJID_UNNE         ; see if Unne event has happened yet (they speak Lefeinish)
    JSR CheckGameEventFlag
    BCS :+                  ; if not (they don't speak it)
      LDA tmp+3             ; ... print [3]
      RTS
:   LDA item_chime          ; otherwise, check to see if they have the Chime
    BEQ :+                  ; if they do...
      LDA tmp+2             ; ... print [2]
      RTS
:   INC item_chime          ; otherwise, give them the Chime
    INC dlgsfx              ; play fanfare
    LDA tmp+1               ; and print [1]
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DoClassChange [$95AE :: 0x395BE]
;;
;;    Performs class change (promotion) on all party members.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


DoClassChange:
    LDA ch_class        ; simply bump up every party member's class ID number
    CLC                 ; to up them to the promoted version of their class
    ADC #6
    STA ch_class

    LDA ch_class+(1<<6)
    CLC
    ADC #6
    STA ch_class+(1<<6)

    LDA ch_class+(2<<6)
    CLC
    ADC #6
    STA ch_class+(2<<6)

    LDA ch_class+(3<<6)
    CLC
    ADC #6
    STA ch_class+(3<<6)

    INC dlgflg_reentermap  ; set flag to indicate map needs reentering 
    RTS                    ;   in order to reload party's mapman graphic

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  LUT for Map Object Talk Data  [$95D5 :: 0x395E5]
;;
;;    Each object has 4 bytes of data which is used with the various talk routines
;;  to determine which text to draw when you talk to this object (and possibly other
;;  things).  See TalkRoutines for more details.
;;
;;    There are $D0 objects, each having 4 bytes of data in this table.  Therefore
;;  this table is $340 bytes large.


lut_MapObjTalkData:
  .INCBIN "bin/0E_95D5_objectdata.bin"




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Enter Lineup Menu  [$9915 :: 0x39925]
;;
;;    The lineup menu is the menu that lets the player re-arrange the order
;;  of their characters.  It is reached by pressing Select when on a map.
;;
;;    This menu uses atypical variables instead of the normal variables,
;;  for whatever reason.... which is why most of the variables are prefixed with
;;  "lu_".
;;
;;    The menu does not move characters around until after you exit.  Before then
;;  it shuffles around an intermediate buffer ("slot table").  There's a slot for each
;;  of the 4 characters, and each slot is 8 bytes.  The slot table uses the "str_buf" block
;;  in RAM (even though it's not a string).
;;
;;    Only 6 of the 8 bytes per slot are used.  They signify the following:
;;  str_buf   = nonzero if this character is dead/stone (can't be party leader)
;;  str_buf+1 = char index of the character in this slot
;;  str_buf+2 = current X coord of the sprite for this character
;;  str_buf+3 = current Y coord
;;  str_buf+4 = desired X coord (for slowly moving the sprite to another position)
;;  str_buf+5 = desired Y coord
;;
;;    To manage animations, there's a variable which contains the current "mode".
;;  There are 5 possible modes:
;;
;;  mode=0 -> No animation occuring, player is free to make their first selection
;;  mode=1 -> Player made their first selection and the sprite is slowly moving left
;;               from its initial position.  Input is ignored while this animation is
;;               taking place.
;;  mode=2 -> Player made first selection, and the sprite has is done moving left
;;               (animation is complete).  Player is free to make their second selection
;;  mode=3 -> Player made second selection, and the sprite is slowly moving right.
;;               Input is ignored while this animation is taking place
;;  mode=4 -> mode 3 animation is complete -- the two selected sprite begin slowly
;;               moving toward their 'desired' coordinates.  Input is ignored
;;               during this time as well.
;;
;;    Once the player exits this menu... the slots are examined, and character stats
;;  are rearranged appropriately.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


EnterLineupMenu:
    LDA #0
    STA $2001             ; turn off the PPU
    STA $4015             ; silence APU
    LDA #$08
    STA soft2000          ; reset soft2000 to typical setup

    JSR LoadMenuCHRPal    ; load menu related CHR and palettes
    JSR ClearNT           ; clear the nametable

    LDA #$0A              ; box coords = $0A,$05
    STA box_x             ; box dims   = $0E,$13
    LDA #$05
    STA box_y
    LDA #$0E
    STA box_wd
    LDA #$13
    STA box_ht            ; but it doesn't draw the box yet

    LDA #0
    STA menustall         ; PPU is off, so clear menustall

       ; fill the slot table with initial values (stored in a LUT)
    LDX #$1F
  @CopyLoop:
      LDA lut_LineupSlots, X    ; copy byte from LUT
      STA str_buf, X            ; to slot table
      DEX
      BPL @CopyLoop             ; $20 iterations

    LDA ch_ailments             ; get char 0's OB ailments
    CLC
    ADC #$01                    ; add 1, AND with 2.  This effectively results
    AND #$02                    ;  in nonzero=dead/stone  zero=alive
    STA str_buf                 ; record that in slot dead state

    LDA ch_ailments+(1<<6)      ; do the same for char 1 / slot 1
    CLC
    ADC #$01
    AND #$02
    STA str_buf+(1*8)

    LDA ch_ailments+(2<<6)      ; char 2 / slot 2
    CLC
    ADC #$01
    AND #$02
    STA str_buf+(2*8)

    LDA ch_ailments+(3<<6)      ; char 3 / slot 3
    CLC
    ADC #$01
    AND #$02
    STA str_buf+(3*8)

    JSR DrawBox                 ; draw box (coords/dims previously filled)
    JSR DrawLineupMenuNames     ; draw names of all the characters
    JSR WaitForVBlank_L         ; wait for VBlank
    JSR DrawPalette             ; and draw the palette

    LDA #$55
    STA music_track             ; switch to music track $55  (crystal theme)

    LDA soft2000
    STA $2000                   ; reset scroll
    LDA #0
    STA $2005
    STA $2005

    JSR ClearOAM                ; clear OAM

    JSR UpdateJoy               ; update joy data
    LDA joy                     ;  so we can fill our lu_joyprev
    AND #$0C
    STA lu_joyprev

    LDA #0                      ; zero A and B button catchers
    STA joy_a
    STA joy_b

    STA lu_cursor2              ; zero cursors and mode
    STA lu_mode
    STA lu_cursor

  ;; Then start the main loop

  @MainLoop:
    JSR WaitForVBlank_L         ; wait for VBlank

    LDA #>oam
    STA $4014                   ; do sprite DMA

    LDA soft2000
    STA $2000
    LDA #$1E
    STA $2001                   ; set PPU state (and turn PPU on)

    LDA #0                      ; reset scroll
    STA $2005
    STA $2005

    LDA #BANK_THIS
    STA cur_bank
    JSR CallMusicPlay           ; call music routine

    JSR ClearOAM                     ; clear OAM
    JSR LineupMenu_DrawCharSprites   ; draw the character sprites
    JSR LineupMenu_DrawCursor        ; draw the cursor
    JSR LineupMenu_UpdateJoy         ; update joypad info
    JSR LineupMenu_ProcessMode       ; process mode operations (animation)
    JSR LineupMenu_ProcessJoy        ; process joypad input
    JMP @MainLoop                    ; and keep looping!

    ; Routine can only exit via ProcessJoy -- which drops the return address and JMPs
    ; out of the routine when the player exits the menu.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  LineupMenu_UpdateJoy  [$99DD :: 0x399ED]
;;
;;    Update Joy data, and plays any sound effects related to cursor
;;  movement for the lineup menu.
;;
;;    See following routine (LineupMenu_ProcessJoy) for explanation of
;;  why sound effects appear to "hang" when you press them during animations
;;  in this menu.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LineupMenu_UpdateJoy:
    LDA joy                ; get joy data
    AND #$0C               ; isolate up/down buttons
    STA tmp+7              ; and store in tmp RAM as previous buttons

    JSR UpdateJoy          ; update the joypad

    LDA joy_a
    ORA joy_b              ; check if either A or B pressed
    BEQ :+
      JMP PlaySFX_MenuSel  ; if either one, play selection SFX, and exit

:   LDA joy                ; otherwise, get newly updated joy data
    AND #$0C               ; isolate up/down buttons
    BEQ @Exit              ; if nothing pressed, exit

    CMP tmp+7              ; if what is pressed is the same as what was already
    BEQ @Exit              ;  pressed.. no change.  So exit

    JMP PlaySFX_MenuMove   ; otherwise, play the cursor move SFX, and exit

  @Exit:
    RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  LineupMenu_ProcessJoy  [$99FD :: 0x39A0D]
;;
;;    Processes joypad input for the lineup menu
;;
;;    Note that LineupMenu_UpdateJoy above will play the selection sound effect every
;;  frame that joy_a or joy_b is nonzero.  This routine is in charge of zeroing joy_a
;;  and joy_b after a button press is detected... HOWEVER... this routine will completely
;;  ignore everything and exit without doing anything when in modes 1, 3, or 4 (during character
;;  movement animation).  Therefore, joy_a and joy_b are not being cleared during that time.
;;
;;    This is why the game makes that horrible noise when you press A or B when character
;;  animations are in progress.  What is happening is that UpdateJoy is incrementing joy_a/joy_b
;;  when you press the button and starting the sound effect -- but this routine isn't clearing joy_a
;;  or joy_b... so it's restarting the sound effect over and over every frame until the animation is
;;  complete (or until you press the button 256 times to loop the catcher back to zero).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  LUJoy_BPressed:      ; B was pressed (must be in mode 0 or 2 to reach here)
    LDA lu_mode        ; get the mode
    BEQ @ExitMenu      ; if mode=0, exit the entire menu

    LDA #$04           ; otherwise... must be mode=2
    STA lu_mode        ; switch to mode=4 

    LDA lu_cursor2     ; and ensure both primary and secondary cursors are the same
    STA lu_cursor

    LDA #0
    STA joy_b          ; then clear B button catcher
    RTS                ; and return

  @ExitMenu:
    PLA                       ; drop return address (so when we return from here,
    PLA                       ;  we exit the Lineup menu completely)
    JMP LineupMenu_Finalize   ; then jump to lineup finalization, and exit (exiting lineup menu)


  LUJoy_Exit:
    RTS

LineupMenu_ProcessJoy:
    LDY lu_mode       ; get mode
    BEQ @Mode_0or2    ; if anything other than 0 or 2 -- it's mid animation.
    CPY #$02          ;   so ignore input until the mode changes
    BNE LUJoy_Exit    ; just exit if not mode 0 or 2

  @Mode_0or2:
    LDA joy_b         ; check for B or A button presses
    BNE LUJoy_BPressed
    LDA joy_a
    BNE @A_Pressed

    LDA joy           ; otherwise, check for direction presses.  Get joy
    AND #$0C          ; isolate up/down buttons
    CMP lu_joyprev    ; compare to prev down
    BEQ LUJoy_Exit    ; if no changes, exit
    STA lu_joyprev    ; otherwise, record changes
    AND #$0C          ; mask again (to refresh Z flag)
    BEQ LUJoy_Exit    ; if no buttons down, exit

    CMP #$08          ; see if they pressed up or down
    BEQ @Up

  @Down:
    LDA lu_cursor
    CLC
    ADC #$08          ; add 8 to cursor (next item down)
    BNE @MoveDone

  @Up:
    LDA lu_cursor
    SEC
    SBC #$08          ; subtract 8 from cursor (next item up)

  @MoveDone:
    AND #$1F          ; wrap so it stays within the 4 slots
    STA lu_cursor     ; and write it back to the cursor
    RTS               ;  then exit


@A_Pressed:
    LDA #0
    STA joy_a         ; clear A button catcher

    LDA lu_mode       ; get the mode
    BNE @A_Mode2      ; if not mode 0, it must be mode 2 (other modes were filtered already)

  @A_Mode0:           ; selection made in mode 0
    INC lu_mode       ; increment mode (to mode 1)
    LDA lu_cursor     ; get the primary cursor
    STA lu_cursor2    ;  and copy it to the secondary cursor
    RTS               ; then exit

  @A_Mode2:           ; selection made in mode 2
    LDA lu_cursor     ; see if primary cursor is equal to the secondary
    CMP lu_cursor2
    BNE @LeaderCheck1 ; if not... start doing leader checks
                      ;  otherwise, the swap is meaningless... so run into KillSwap

  @KillSwap:          ; this kills an illegal swap (one that tries to make the leader dead/stone)
    LDA lu_cursor2    ; kill the swap by setting cursor and cursor2 to the same value
    STA lu_cursor
    INC lu_mode       ; then go to mode=3 (mode=4 would make more sense... but would be larger code)
    RTS               ; then exit

  @LeaderCheck1:      ; here's where we need to make sure the leader isn't dead/stone
    TAY               ; put cursor in Y
    BNE @LeaderCheck2 ; if not zero, this isn't the lead slot.. so skip to checking cursor2
      LDX lu_cursor2  ;  cursor2 in X (if cursor is slot 0, cursor2 is BECOMING slot 0)
      LDA str_buf, X  ;  make sure cursor2's character is alive
      BNE @KillSwap   ;  if not... don't make any change

  @LeaderCheck2:
    LDX lu_cursor2    ; put cursor2 in X
    BNE @SwapSafe     ; if nonzero, isn't lead slot, so the swap is safe.  Otherwise...
      LDA str_buf, Y  ;   get slot 1's alive status
      BNE @KillSwap   ;   if slot 1 is dead... cancel the swap

     ; we get here if the swap is safe to make (leader will not be dead/stone as a result)
     ; however we do not actually perform the swap here... that's done by ProcessMode.  Here,
     ; we just swap the desired Y coords so that ProcessMode will start moving the sprites
     ; towards their new slot.

  @SwapSafe:
    LDA str_buf+5, Y   ; get 'Y' slot desired y coord
    STA tmp            ; back it up

    LDA str_buf+5, X   ; replace 'Y' slot desired y with 'X' slot's desired y
    STA str_buf+5, Y

    LDA tmp            ; replace 'X' slot's desired y with backed up 'Y' slot's y
    STA str_buf+5, X

    INC lu_mode        ; enter mode=3

                    ; following instruction is an RTS -- but it's part of
                    ;   the next routine

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  LineupMenu_ProcessMode  [$9A88 :: 0x39A98]
;;
;;    This does all the animation involved in moving sprites around as the player
;;  makes selections.  Movement is determined by the current mode (lu_mode)
;;
;;    This also actually performs the slot swapping once all the animation is
;;  complete.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  LUMode_Exit:
    RTS

LineupMenu_ProcessMode:
    LDA lu_mode           ; get the current mode
    BEQ LUMode_Exit       ; if mode=0, no action.  Exit

    CMP #$02              ; if mode=2, no action.  Exit
    BEQ LUMode_Exit

    CMP #$01
    BNE @Mode_3or4        ; if not mode 1, must be mode 3 or 4

    ;  Mode 1 -- a slot has been selected and we're in the process of moving
    ;   the character image left.

  @Mode_1:
    LDY lu_cursor2        ; get the selected slot in Y
    LDA str_buf+2, Y      ; get the current X coord of that slot's graphic

    CMP #$89              ; if X coord < $89
    BCC @IncModeAndExit   ;  ... then inc mode (mode=2) and exit

    SEC
    SBC #$01              ; otherwise, subtract 1 from the X coord
    STA str_buf+2, Y      ; and write it back
    RTS                   ; then exit


  @Mode_3or4:
    CMP #$03              ; is mode 3 or 4?
    BNE @Mode_4

    ;  Mode 3 -- a second slot has been selected, and we're in the process of moving
    ;   that character image right.

  @Mode_3:
    LDY lu_cursor         ; get the selected slot in Y
    LDA str_buf+2, Y      ; get that slot's X coord

    CMP #$A8              ; if X coord >= $A8
    BCS @IncModeAndExit   ;  ... then inc mode (mode=4) and exit

    CLC
    ADC #$01              ; otherwise, add 1 to the X coord
    STA str_buf+2, Y      ; and write it back
    RTS                   ; then exit


  @IncModeAndExit:      ; pretty self explanitory
    INC lu_mode
    RTS

    ;  Mode 4 -- both slots have been selected, and we're in the process of moving
    ;   both graphics towards their destination.

  @Mode_4:
    LDY lu_cursor
    JSR LineupMenu_AnimStep    ; move first slot towards its destination
    BCC @M4_FirstDone          ; if first slot is done... jump ahead

      LDY lu_cursor2           ; otherwise... just move the second selected slot
      JMP LineupMenu_AnimStep  ;   and exit

  @M4_FirstDone:
    LDY lu_cursor2
    JSR LineupMenu_AnimStep    ; move the second selected slot
    BCS LUMode_Exit            ; if it's not done yet... exit otherwise they're both done


  ;;;  once they're both done, we need to swap slot information
  ;;;    this will exchange the 8 bytes of slot information in str_buf
  ;;;    in the two selected slots

    LDY lu_cursor         ; primary cursor in Y (for indexing)
    LDA #$08              ; loop down counter (8 bytes to transfer) in A
    LDX lu_cursor2        ; secondary cursor in X (for indexing)

  @SwapLoop:
    PHA                   ; push loop counter to back it up
      LDA str_buf, Y      ; get 'Y' data
      PHA                 ; push it
        LDA str_buf, X  
        STA str_buf, Y    ; replace 'Y' data with 'X' data
      PLA                 ; pull original 'Y' data
      STA str_buf, X      ; and replace 'X' data with it (byte swap complete)

      INX                 ; increment both indeces so that the next swap
      INY                 ;   does the next byte in the buffer

    PLA                   ; pull the loop down counter
    SEC
    SBC #$01              ; decrement it by 1
    BNE @SwapLoop         ; and keep looping until it expires (8 iterations)

    LDA #0
    STA lu_mode           ; zero the mode

    LDA lu_cursor2        ; now that the slots have been swapped, swap the cursor
    STA lu_cursor         ;  don't need to set cursor2 because it's not used in mode 0

    LDA #1
    STA menustall            ; PPU is currently on, so we need to menustall for drawing
    JMP DrawLineupMenuNames  ; redraw the char names to reflect the swap.  Then exit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  LineupMenu_AnimStep   [$9AFB :: 0x39B0B]
;;
;;    Moves a slot's sprite 1 step (in this case, 1 pixel) closer to its
;;  desired coordinates.  First moving along the Y axis, then along the X.
;;
;;    This is called by LineupMenu's mode 4, which animates the char sprites
;;  to move them towards their new slot.
;;
;;  IN:   Y = slot index ($00,$08,$10, or $18) whose graphic we're to move
;;
;;  OUT:  C = set if more movement is required.  clear if sprite is at its
;;              desired coords.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LineupMenu_AnimStep:
    LDA str_buf+3, Y     ; get slot's current Y coord
    CMP str_buf+5, Y     ; compare to desired Y coord
    BNE @MoveVertical    ; if they're not equal, have vertical movement to do

    LDA str_buf+2, Y     ; otherwise get slot's X coord
    CMP str_buf+4, Y     ; compare to desired X coord
    BNE @MoveHorizontal  ; if not equal... have horizontal movement

    CLC                  ; otherwise... char is right where we want it.  CLC to indicate
    RTS                  ;   no further movement necessary.  Then exit

  @MoveHorizontal:
    BCS @MoveLeft        ; if current > desired... move left.  Otherwise, move right

  @MoveRight:
    ADC #$01
    STA str_buf+2, Y     ; add 1 to the current X coord
    SEC                  ; SEC to indicate more movement required
    RTS                  ; and exit

  @MoveLeft:
    SBC #$01             ; subtract 1 from X
    STA str_buf+2, Y     ; and write it back
    SEC                  ; SEC to indicate more movement required
    RTS                  ; and exit

  @MoveVertical:
    BCS @MoveUp          ; if current > desired... move up.  Otherwise, move down

  @MoveDown:
    ADC #$01
    STA str_buf+3, Y     ; +1 to Y
    SEC
    RTS

  @MoveUp:
    SBC #$01
    STA str_buf+3, Y     ; -1 to Y
    SEC
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  LineupMenu_DrawCursor   [$9B2D :: 0x39B3D]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LineupMenu_DrawCursor:
    LDY lu_cursor          ; put the cursor in Y

    LDA #$58
    STA spr_x              ; set cursor X coord to $58
    LDA str_buf+3, Y       ; get the selected slot's Y coord
    CLC
    ADC #$08               ; +8
    STA spr_y              ; and that's the cursor Y coord

    JMP DrawCursor         ; draw the cursor and exit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Faux/Unused routine  [$9B3E :: 0x39B4E]
;;
;;    Not sure what this is supposed to do.  Apparently it scans the
;;  slot list for the first matching dead/stone state (stored in tmp).
;;  Sets C if the state couldn't be found, and clears it if it could.
;;
;;    How this is supposed to be of any use, I don't know.  But I guess
;;  that's why it's unused.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


UnusedRoutine_9B3E:
    LDY #0
  @Loop:
    LDA str_buf, Y
    CMP tmp
    BEQ @Found

    TYA
    CLC
    ADC #$08
    AND #$1F
    TAY
    BNE @Loop

    SEC
    RTS

  @Found:
    CLC
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  LineupMenu_DrawCharSprites  [$9B54 :: 0x39B64]
;;
;;    Draws all the character sprites in the lineup menu
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LineupMenu_DrawCharSprites:
    LDA #0                  ; start A at zero (will be our loop counter)
  @Loop:
    PHA                     ; push loop counter to stack
    TAY                     ; and put it in Y for indexing

    LDA str_buf+2, Y        ; get this slot's char sprite coords
    STA spr_x
    LDA str_buf+3, Y
    STA spr_y

    LDA str_buf+1, Y        ; get slot's char index
    JSR DrawOBSprite        ; and draw that sprite

    PLA                     ; pull our loop counter
    CLC
    ADC #8                  ; add 8 to it (look at next slot)
    CMP #8*4                ; and loop until we do all 4 slots
    BCC @Loop

    RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Lineup menu initial slot table LUT  [$9B71 :: 0x39B81]
;;
;;    This is the initial values for the slot information table
;;  for the lineup menu.
;;
;;    See EnterLineupTable for description of how this table works

lut_LineupSlots:
  .BYTE   $00,$00,$98,$38,$98,$38,$00,$00
  .BYTE   $00,$40,$98,$58,$98,$58,$00,$00
  .BYTE   $00,$80,$98,$78,$98,$78,$00,$00
  .BYTE   $00,$C0,$98,$98,$98,$98,$00,$00


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Lineup Menu Names  [$9B91 :: 0x39BA1]
;;
;;  Draws all 4 character names for the Lineup menu
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawLineupMenuNames:
    LDA #$0D
    STA dest_x
    LDA #$08
    STA dest_y              ; dest = $0D,$08
    LDA str_buf+1           ; get the char index of slot 0
    JSR DrawCharacterName   ; draw his name

    LDA #$0C
    STA dest_y              ; dest = $0D,$0C
    LDA str_buf+1+(1*8)     ; char index of slot 1
    JSR DrawCharacterName   ; draw name

    LDA #$10
    STA dest_y
    LDA str_buf+1+(2*8)
    JSR DrawCharacterName   ; draw slot 2's name

    LDA #$14
    STA dest_y
    LDA str_buf+1+(3*8)
    JMP DrawCharacterName   ; and slot 3's.. then exit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  LineupMenu_Finalize  [$9BBD :: 0x39BCD]
;;
;;    Called when the player exits the lineup menu.  This finalizes the
;;  reordering by actually swapping around charater stats to reflect the new
;;  slots they've been moved to.
;;
;;    The rearranging is accomplished by copying all character data to a secondary
;;  buffer (lutmp_ch_***).  Then filling each slot with the desired character data
;;  that has been selected for that slot.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LineupMenu_Finalize:
    LDX #0                    ; start X at zero
   @BackupLoop:
      LDA ch_stats, X         ; copy $100 bytes from ch_stats
      STA lutmp_ch_stats, X   ; put it in the lu tmp stat buffer
      LDA ch_magicdata, X     ; and copy $100 bytes from ch_magic
      STA lutmp_ch_magic, X   ; and copy to the lu tmp magic buffer
      INX
      BNE @BackupLoop         ; loop until X wraps ($100 iterations)

    LDX str_buf+1             ; char ID to be put in
    LDY #0                    ;   slot 0
    JSR @PutInSlot

    LDX str_buf+1+(1*8)       ; char ID for...
    LDY #(1<<6)               ;   slot 1
    JSR @PutInSlot

    LDX str_buf+1+(2*8)       ; slot 2
    LDY #(2<<6)
    JSR @PutInSlot

    LDX str_buf+1+(3*8)       ; slot 3
    LDY #(3<<6)
                              ; just flow into @PutInSlot

    ;  This local subroutine will copy the char ID in 'X' to the slot in 'Y'
  @PutInSlot:
    LDA #$40
    STA tmp                  ; tmp will be our loop down counter.  Copy $40 bytes per char

   @SlotLoop:
      LDA lutmp_ch_stats, X  ; copy stats
      STA ch_stats, Y

      LDA lutmp_ch_magic, X  ; and magic
      STA ch_magicdata, Y

      INX                    ; inc both source and dest indeces
      INY

      DEC tmp                ; dec loop down counter
      BNE @SlotLoop          ; and loop until it expires ($40 iterations)

    RTS                      ; then exit!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Clear Nametable  [$9C02 :: 0x39C12]
;;
;;    This clears the full nametable (ppu$2000) to 00, as well as filling
;;   the attribute table with FF.  This provides a "clean slate" on which
;;   menu boxes and stuff can be drawn to.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ClearNT:
    LDA $2002     ; reset PPU toggle
    LDA #$20
    STA $2006
    LDA #$00
    STA $2006     ; set PPU addr to $2000 (start of NT)
    LDY #$00      ; zero out A and Y
    TYA           ;   Y will be the low byte of our counter
    LDX #$03      ; X=3 -- this is the high byte of our counter (loop $0300 times)

@Loop:            ; first loop clears the first $0300 bytes of the NT
      STA $2007
      INY
      BNE @Loop      ; once Y wraps
        DEX          ;  decrement X
        BNE @Loop    ;  and stop looping once X expires (total $0300 iterations)

@Loop2:           ; next loop clears the next $00C0 (up to the attribute table)
      STA $2007
      INY
      CPY #$C0       ; loop until Y reaches #$C0
      BCC @Loop2


    LDA #$FF      ; A=FF (this is what we will fill attribute table with
@Loop3:           ;  3rd and final loop fills the last $40 bytes (attribute table) with FF
      STA $2007
      INY
      BNE @Loop3

    RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Character Name  [$9C2E :: 0x39C3E]
;;
;;     Draws a given character name at given coords
;;
;;  IN:             A = char index (00,40,80,C0)
;;      dest_x,dest_y = coords to draw at
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


DrawCharacterName:
    TAX                    ; put char index in X

    LDA ch_name, X         ; copy the character name to the format_buf
    STA format_buf-4
    LDA ch_name+1, X
    STA format_buf-3
    LDA ch_name+2, X
    STA format_buf-2
    LDA ch_name+3, X
    STA format_buf-1

    LDA #<(format_buf-4)   ; set text_ptr to point to it
    STA text_ptr
    LDA #>(format_buf-4)
    STA text_ptr+1

    LDA #BANK_THIS         ; set banks required for DrawComplexString
    STA cur_bank
    STA ret_bank

    JMP DrawComplexString  ; Draw it!  Then return




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  NewGamePartyGeneration  [$9C54 :: 0x39C64]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

NewGamePartyGeneration:
    LDA #$00                ; turn off the PPU
    STA $2001
    LDA #$0F                ; turn ON the audio (it should already be on, though
    STA $4015               ;  so this is kind of pointless)
    
    JSR LoadNewGameCHRPal   ; Load up all the CHR and palettes necessary for the New Game menus
    
    LDA cur_pal+$D          ; Do some palette finagling
    STA cur_pal+$1          ;  Though... these palettes are never drawn, so this seems entirely pointless
    LDA cur_pal+$F
    STA cur_pal+$3
    LDA #$16
    STA cur_pal+$2
    
    LDX #$3F                ; Initialize the ptygen buffer!
    : LDA lut_PtyGenBuf, X  ;  all $40 bytes!  ($10 bytes per character)
      STA ptygen, X
      DEX
      BPL :-
      
    LDA #$00        ; This null-terminates the draw buffer for when the character's
    STA $60         ;   name is drawn on the name input screen.  Why this is done here
                    ;   and not with the actual drawing makes no sense to me.
    
    
  @Char_0:                      ; To Character generation for each of the 4 characters
    LDA #$00                    ;   branching back to the previous char if the user
    STA char_index              ;   cancelled by pressing B
    JSR DoPartyGen_OnCharacter
    BCS @Char_0
  @Char_1:
    LDA #$10
    STA char_index
    JSR DoPartyGen_OnCharacter
    BCS @Char_0
  @Char_2:
    LDA #$20
    STA char_index
    JSR DoPartyGen_OnCharacter
    BCS @Char_1
  @Char_3:
    LDA #$30
    STA char_index
    JSR DoPartyGen_OnCharacter
    BCS @Char_2
    
    
    ; Once all 4 characters have been generated and named...
    JSR PtyGen_DrawScreen       ; Draw the screen one more time
    JSR ClearOAM                ; Clear OAM
    JSR PtyGen_DrawChars        ; Redraw char sprites
    JSR WaitForVBlank_L         ; Do a frame
    LDA #>oam                   ;   with a proper OAM update
    STA $4014
    
    JSR MenuWaitForBtn_SFX      ; Wait for the user to press A (or B) again, to
    LDA joy                     ;  confirm their party decisions.
    AND #$40
    BNE @Char_3                 ; If they pressed B, jump back to Char 3 generation
    
    ;;  Otherwise, they've pressed A!  Party confirmed!
    LDA #$00
    STA $2001                   ; shut the PPU off
    
    LDX #$00                    ; Move class and name selection
    JSR @RecordClassAndName     ;  out of the ptygen buffer and into the actual character stats
    LDX #$10
    JSR @RecordClassAndName
    LDX #$20
    JSR @RecordClassAndName
    LDX #$30
  ; JMP @RecordClassAndName
    
  @RecordClassAndName:
    TXA                     ; X is the ptygen source index  ($10 bytes per character)
    ASL A
    ASL A
    TAY                     ; Y is the ch_stats dest index  ($40 bytes per character)
    
    LDA ptygen_class, X     ; copy class
    STA ch_class, Y
    
    LDA ptygen_name+0, X    ; and name
    STA ch_name    +0, Y
    LDA ptygen_name+1, X
    STA ch_name    +1, Y
    LDA ptygen_name+2, X
    STA ch_name    +2, Y
    LDA ptygen_name+3, X
    STA ch_name    +3, Y
    
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  PtyGen_DrawScreen  [$9CF8 :: 0x39D08]
;;
;;    Prepares and draws the Party Generation screen
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PtyGen_DrawScreen:
    LDA #$08
    STA soft2000          ; set BG/Spr pattern table assignments
    LDA #0
    STA $2001             ; turn off PPU
    STA joy_a             ;  clear various joypad catchers
    STA joy_b
    STA joy
    STA joy_prevdir

    JSR ClearNT             ; wipe the screen clean
    JSR PtyGen_DrawBoxes    ;  draw the boxes
    JSR PtyGen_DrawText     ;  and the text in those boxes
    JMP TurnMenuScreenOn_ClearOAM   ; then clear OAM and turn the PPU On


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DoPartyGen_OnCharacter  [$9D15 :: 0x39D25]
;;
;;    Does character selection and name input for one character.
;;
;;  input:      ptygen = should be filled appropriately
;;          char_index = $00, 10, 20, 30 to indicate which character's name we're setting
;;
;;  output:    C is cleared if the user confirmed/named their character
;;             C is set if the user pressed B to cancel/go back
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DoPartyGen_OnCharacter:
    JSR PtyGen_DrawScreen           ; Draw the Party generation screen
    
    ; Then enter the main logic loop
  @MainLoop:
      JSR PtyGen_Frame              ; Do a frame and update joypad input
      LDA joy_a
      BNE DoNameInput               ; if A was pressed, do name input
      LDA joy_b
      BEQ :+
        ; if B pressed -- just SEC and exit
        SEC
        RTS
      
      ; Code reaches here if A/B were not pressed
    : LDA joy
      AND #$0F
      CMP joy_prevdir
      BEQ @MainLoop             ; if there was no change in directional input, loop to another frame
      
      STA joy_prevdir           ; otherwise, record new directional input as prevdir
      CMP #$00                  ; if directional input released (rather than pressed)
      BEQ @MainLoop             ;   loop to another frame.
    
     ; Otherwise, if any direction was pressed:
      LDX char_index
      CLC
      LDA ptygen_class, X       ; Add 1 to the class ID of the current character.
      ADC #1
      CMP #6
      BCC :+
        LDA #0                  ; wrap 5->0
    : STA ptygen_class, X
  
      LDA #$01                  ; set menustall (drawing while PPU is on)
      STA menustall
      LDX char_index            ; then update the on-screen class name
      JSR PtyGen_DrawOneText
      JMP @MainLoop
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DoNameInput  [$9D50 :: 0x39D60]
;;
;;    Does the name input screen.  Draw the screen, gets the name, etc, etc.
;;
;;  input:      ptygen = should be filled appropriately
;;          char_index = $00, 10, 20, 30 to indicate which character's name we're setting
;;
;;  output:    C is cleared to indicate name successfully input
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DoNameInput:
    LDA #$00                ; Turn off the PPU (for drawing)
    STA $2001
    
    STA menustall           ; zero a bunch of misc vars being used here
    STA joy_a
    STA joy_b
    STA joy
    STA joy_prevdir
    
    STA cursor              ; letter of the name we're inputting (0-3)
    STA namecurs_x          ; X position of letter selection cursor (0-9)
    STA namecurs_y          ; Y position (0-6)
    
    ; Some local temp vars
                @cursoradd      = $63
                @selectedtile   = $10
    
    JSR ClearNT
    JSR DrawNameInputScreen
    
    LDX char_index          ; wipe this character's name
    LDA #$FF
    STA ptygen_name, X
    STA ptygen_name+1, X
    STA ptygen_name+2, X
    STA ptygen_name+3, X
    
    JSR TurnMenuScreenOn_ClearOAM   ; now that everything is drawn, turn the screen on
    
    LDA #$01                ; Set menustall, as future drawing will
    STA menustall           ;  be with the PPU on
    
  @MainLoop:
    JSR CharName_Frame      ; Do a frame & get input
    
    LDA joy_a
    BNE @A_Pressed          ; Check if A or B pressed
    LDA joy_b
    BNE @B_Pressed
    
    LDA joy                 ; Otherwise see if D-pad state has changed
    AND #$0F
    CMP joy_prevdir
    BEQ @MainLoop           ; no change?  Jump back
    STA joy_prevdir
    
       ; D-pad state has changed, see what it changed to
    CMP #$00
    BEQ @MainLoop           ; if released, do nothing and loop
    
    CMP #$04
    BCC @Left_Or_Right      ; if < 4, L or R pressed
    
    CMP #$08                ; otherwise, if == 8, Up pressed
    BNE @Down               ; otherwise, if != 8, Down pressed
    
  @Up:
    DEC namecurs_y          ; DEC cursor Y position
    BPL @MainLoop
    LDA #$06                ; wrap 0->6
    STA namecurs_y
    JMP @MainLoop
    
  @Down:
    INC namecurs_y          ; INC cursor Y position
    LDA namecurs_y
    CMP #$07                ; wrap 6->0
    BCC @MainLoop
    LDA #$00
    STA namecurs_y
    JMP @MainLoop
    
  @Left_Or_Right:
    CMP #$02                ; if D-pad state == 2, Left pressed
    BNE @Right              ; else, Right pressed
    
  @Left:
    DEC namecurs_x          ; DEC cursor X position
    BPL @MainLoop
    LDA #$09                ; wrap 0->9
    STA namecurs_x
    JMP @MainLoop
    
  @Right:
    INC namecurs_x          ; INC cursor X position
    LDA namecurs_x
    CMP #$0A                ; wrap 9->0
    BCC @MainLoop
    LDA #$00
    STA namecurs_x
    JMP @MainLoop
    
    ;;;;;;;;;;;;;;;;;;
  @B_Pressed:
    LDA #$FF                ; if B was pressed, erase the previous tile
    STA @selectedtile       ;   by setting selectedtile to be a space
    
    LDA cursor              ; then by pre-emptively moving the cursor back
    SEC                     ;   so @SetTile will overwrite the prev char
    SBC #$01                ;   instead of the next one
    BMI :+                  ; (clip at 0)
      STA cursor
      
  : LDA #$00                ; set cursoradd to 0 so @SetTile doesn't change
    STA @cursoradd          ; the cursor
    STA joy_b               ; clear joy_b as well
    
    BEQ @SetTile            ; (always branches)
    
    ;;;;;;;;;;;;;;;;;;
  @A_Pressed:
    LDX namecurs_y                  ; when A is pressed, clear joy_a
    LDA #$00
    STA joy_a                       ; Then get the tile they selected by first
    LDA lut_NameInputRowStart, X    ;  running the Y cursor through a row lut
    CLC
    ADC namecurs_x                  ; add X cursor
    ASL A                           ; and multiply by 2 -- since there are spaces between tiles
    TAX                             ; use that value as an index to the lut_NameInput
    BCC :+                          ; This will always branch, as C will always be clear
        LDA lut_NameInput+$100, X       ; I can only guess this was used in the Japanese version, where the NameInput table might have been bigger than 
        JMP :++                         ; 256 bytes -- even though that seems very unlikely.
        
  : LDA lut_NameInput, X
  : STA @selectedtile               ; record selected tile
    LDA #$01
    STA @cursoradd                  ; set cursoradd to 1 to indicate we want @SetTile to move the cursor forward
    
    LDA cursor                      ; check current cursor position
    CMP #$04                        ;  If we've already input 4 letters for this name....
    BCS @Done                       ;  .. then we're done.  Branch ahead
                                    ; Otherwise, fall through to SetTile

  @SetTile:
    LDA cursor                  ; use cursor and char_index to access the appropriate
    CLC                         ;   letter in this character's name
    ADC char_index
    TAX
    LDA @selectedtile
    STA ptygen_name, X          ; and write the selected tile
    
    JSR NameInput_DrawName      ; Redraw the name as it appears on-screen
    
    LDA cursor                  ; Then add to our cursor
    CLC
    ADC @cursoradd
    BPL :+                      ; clipping at 0 (if subtracting -- although this never happens)
      LDA #$00
  : STA cursor
  
    JMP @MainLoop               ; And keep going!
    
  @Done:
    CLC                 ; CLC to indicate name was successfully input
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  PtyGen_Frame  [$9E33 :: 0x39E43]
;;
;;    Does the typical frame stuff for the Party Gen screen
;;  Note the scroll is not reset here, since there is a little bit of drawing
;;  done AFTER this (which is dangerous -- what if the music routine runs long!)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PtyGen_Frame:
    JSR ClearOAM           ; wipe OAM then draw all sprites
    JSR PtyGen_DrawChars
    JSR PtyGen_DrawCursor

    JSR WaitForVBlank_L    ; VBlank and DMA
    LDA #>oam
    STA $4014

    LDA #BANK_THIS         ; then keep playing music
    STA cur_bank
    JSR CallMusicPlay

    JMP PtyGen_Joy         ; and update joy data!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  CharName_Frame  [$9E4E :: 0x39E5E]
;;
;;    Does typical frame stuff for the Character naming screen
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CharName_Frame:
    JSR ClearOAM           ; wipe OAM then draw the cursor
    JSR CharName_DrawCursor

    JSR WaitForVBlank_L    ; VBlank and DMA
    LDA #>oam
    STA $4014

    LDA soft2000           ; reset the scroll to zero.
    STA $2000
    LDA #0
    STA $2005
    STA $2005

    LDA #BANK_THIS         ; keep playing music
    STA cur_bank
    JSR CallMusicPlay

      ; then update joy by running seamlessly into PtyGen_Joy

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  PtyGen_Joy  [$9E70 :: 0x39E80]
;;
;;    Updates Joypad data and plays button related sound effects for the Party
;;  Generation AND Character Naming screens.  Seems like a huge waste, since sfx could
;;  be easily inserted where the game handles the button presses.  But whatever.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PtyGen_Joy:
    LDA joy
    AND #$0F
    STA tmp+7            ; put old directional buttons in tmp+7 for now

    JSR UpdateJoy        ; then update joypad data

    LDA joy_a            ; if either A or B pressed...
    ORA joy_b
    BEQ :+
      JMP PlaySFX_MenuSel ; play the Selection SFX, and exit

:   LDA joy              ; otherwise, check new directional buttons
    AND #$0F
    BEQ @Exit            ; if none pressed, exit
    CMP tmp+7            ; if they match the old buttons (no new buttons pressed)
    BEQ @Exit            ;   exit
    JMP PlaySFX_MenuMove ; .. otherwise, play the Move sound effect
  @Exit:
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  PtyGen_DrawBoxes  [$9E90 :: 0x39EA0]
;;
;;    Draws the 4 boxes for the Party Generation screen
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PtyGen_DrawBoxes:
    LDA #0
    STA tmp+15       ; reset loop counter to zero

  @Loop:
      JSR @Box       ; then loop 4 times, each time, drawing the next
      LDA tmp+15     ; character's box
      CLC
      ADC #$10       ; incrementing by $10 each time (indexes ptygen buffer)
      STA tmp+15
      CMP #$40
      BCC @Loop
    RTS

 @Box:
    LDX tmp+15           ; get ptygen index in X

    LDA ptygen_box_x, X  ; get X,Y coords from ptygen buffer
    STA box_x
    LDA ptygen_box_y, X
    STA box_y

    LDA #10              ; fixed width/height of 10
    STA box_wd
    STA box_ht

    LDA #0
    STA menustall        ; disable menustalling (PPU is off)
    JMP DrawBox          ;  draw the box, and exit


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  PtyGen_DrawText  [$9EBC :: 0x39ECC]
;;
;;    Draws the text for all 4 character boxes in the Party Generation screen.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PtyGen_DrawText:
    LDA #0             ; start loop counter at zero
  @MainLoop:
     PHA                ; push loop counter to back it up
     JSR @DrawOne       ; draw one character's strings
     PLA                ;  pull loop counter
     CLC                ; and increase it to point to next character's data
     ADC #$10           ;  ($10 bytes per char in 'ptygen')
     CMP #$40
     BCC @MainLoop      ;  loop until all 4 chars drawn
    RTS

  @DrawOne:
    TAX                 ; put the ptygen index in X for upcoming routine

      ; no JMP or RTS -- code flows seamlessly into PtyGen_DrawOneText

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  PtyGen_DrawOneText  [$9ECC :: 0x39EDC]
;;
;;    This draws text for *one* of the character boxes in the Party Generation
;;  screen.  This is called by the above routine to draw all 4 of them at once,
;;  but is also called to redraw an individual class name when the player changes
;;  the class of the selected character.
;;
;;    The text drawn here is just two short strings.  First is the name of the
;;  selected class (Fighter/Thief/etc).  Second is the character's name.
;;
;;    Text is drawn by simply copying short strings to the format buffer, then
;;  calling DrawComplexString to draw them.  The character's name is simply
;;  the 4 letters copied over.. whereas the class name makes use of one
;;  of DrawComplexString's control codes.  See that routine for further details.
;;
;;  IN:  X = ptygen index of the char whose text we want to draw
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PtyGen_DrawOneText:
    LDA ptygen_class_x, X   ; get X,Y coords where we're going to place
    STA dest_x              ;  the class name
    LDA ptygen_class_y, X
    STA dest_y

    LDA ptygen_class, X     ; get the selected class
    CLC
    ADC #$F0                ; add $F0 to select the class' "item name"
    STA format_buf-1        ;  store that as 2nd byte in format string
    LDA #$02                ; first byte in string is $02 -- the control code to
    STA format_buf-2        ;  print an item name

    LDA #<(format_buf-2)    ; set the text pointer to point to the start of the 2-byte
    STA text_ptr            ;  string we just constructed
    LDA #>(format_buf-2)
    STA text_ptr+1

    LDA #BANK_THIS          ; set cur and ret banks (see DrawComplexString for why)
    STA cur_bank
    STA ret_bank

    TXA                     ; back up our index (DrawComplexString will corrupt it)
    PHA
    JSR DrawComplexString   ; draw the string
    PLA
    TAX                     ; and restore our index

    LDA ptygen_name, X      ; next, copy over the 4-byte name of the character
    STA format_buf-4        ;  over to the format buffer
    LDA ptygen_name+1, X
    STA format_buf-3
    LDA ptygen_name+2, X
    STA format_buf-2
    LDA ptygen_name+3, X
    STA format_buf-1

    LDA ptygen_name_x, X    ; set destination coords appropriately
    STA dest_x
    LDA ptygen_name_y, X
    STA dest_y

    LDA #<(format_buf-4)    ; set pointer to start of 4-byte string
    STA text_ptr
    LDA #>(format_buf-4)
    STA text_ptr+1

    LDA #BANK_THIS          ; set banks again (not necessary as they haven't changed from above
    STA cur_bank            ;   but oh well)
    STA ret_bank

    JMP DrawComplexString   ; then draw another complex string -- and exit!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  PtyGen_DrawCursor  [$9F26 :: 0x39F36]
;;
;;    Draws the cursor for the Party Generation screen
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PtyGen_DrawCursor:
    LDX char_index          ; use the current index to get the cursor
    LDA ptygen_curs_x, X    ;  coords from the ptygen buffer.
    STA spr_x
    LDA ptygen_curs_y, X
    STA spr_y
    JMP DrawCursor          ; and draw the cursor there



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  CharName_DrawCursor  [$9F35 :: 0x39F45]
;;
;;    Draws the cursor for the Character Naming screen
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CharName_DrawCursor:
    LDA namecurs_x      ; X position = (cursx * 16) + $20
    ASL A
    ASL A
    ASL A
    ASL A
    CLC
    ADC #$20
    STA spr_x
    
    LDA namecurs_y      ; Y position = (cursy * 16) + $50
    ASL A
    ASL A
    ASL A
    ASL A
    CLC
    ADC #$50
    STA spr_y
    
    JMP DrawCursor


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  PtyGen_DrawChars  [$9F4E :: 0x39F5E]
;;
;;    Draws the sprites for all 4 characters on the party gen screen.
;;  This routine uses DrawSimple2x3Sprite to draw the sprites.
;;  See that routine for details.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PtyGen_DrawChars:
    LDX #$00         ; Simply call @DrawOne four times, each time
    JSR @DrawOne     ;  having the index of the char to draw in X
    LDX #$10
    JSR @DrawOne
    LDX #$20
    JSR @DrawOne
    LDX #$30

  @DrawOne:
    LDA ptygen_spr_x, X   ; load desired X,Y coords for the sprite
    STA spr_x
    LDA ptygen_spr_y, X
    STA spr_y

    LDA ptygen_class, X   ; get the class
    TAX
    LDA lutClassBatSprPalette, X   ; get the palette that class uses
    STA tmp+1             ; write the palette to tmp+1  (used by DrawSimple2x3Sprite)

    TXA               ; multiply the class index by $20
    ASL A             ;  this gets the tiles in the pattern tables which have this
    ASL A             ;  sprite's CHR ($20 tiles is 2 rows, there are 2 rows of tiles
    ASL A             ;  per class)
    ASL A
    ASL A
    STA tmp           ; store it in tmp for DrawSimple2x3Sprite
    JMP DrawSimple2x3Sprite



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  NameInput_DrawName  [$9F7D :: 0x39F8D]
;;
;;    Used during party generation.. specifically the name input screen
;;  to draw the character's name at the top of the screen.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

NameInput_DrawName:
            @buf  = $5C     ; local - buffer to hold the name for printing
            
    LDX char_index          ; copy the character's name to our temp @buf
    LDA ptygen_name, X
    STA @buf
    LDA ptygen_name+1, X
    STA @buf+1
    LDA ptygen_name+2, X
    STA @buf+2
    LDA ptygen_name+3, X
    STA @buf+3              ; The code assumes @buf+4 is 0
    
    LDA #>@buf              ; Set the text pointer
    STA text_ptr+1
    LDA #<@buf
    STA text_ptr
    
    LDA #BANK_THIS          ; set cur/ret banks
    STA cur_bank
    STA ret_bank
    
    LDA #$0E                ; set X/Y positions for the name to be printed
    STA dest_x
    LDA #$04
    STA dest_y
    
    LDA #$01                ; drawing while PPU is on, so set menustall
    STA menustall
    
    JMP DrawComplexString   ; Then draw the name and exit!
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DrawNameInputScreen  [$9FB0 :: 0x39FC0]
;;
;;  Draws everything except for the player's name.
;;
;;  Assumes PPU is off upon entry
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawNameInputScreen:
    LDA $2002               ; clear PPU toggle
    
    LDA #>$23C0             ; set PPU addr to the attribute table
    STA $2006
    LDA #<$23C0
    STA $2006
    
    LDA #$00                ; set $10 bytes of the attribute table to use palette 0
    LDX #$10                ;  $10 bytes = 8 rows of tiles (32 pixels)
    : STA $2007             ; This makes the top box the orangish color instead of the normal blue
      DEX
      BNE :-

    LDA #0
    STA menustall           ; no menustall (PPU is off at this point)
    
    LDA #$04                ; Draw the big box containing input
    STA box_x
    LDA #$08
    STA box_y
    LDA #$17
    STA box_wd
    LDA #$14
    STA box_ht
    JSR DrawBox
    
    LDA #$0D                ; Draw the small top box containing the player's name
    STA box_x
    LDA #$02
    STA box_y
    LDA #$06
    STA box_wd
    LDA #$04
    STA box_ht
    JSR DrawBox
    
    LDA #<lut_NameInput     ; Print the NameInput lut as a string.  This will fill
    STA text_ptr            ;  the bottom box with the characters the user can select.
    LDA #>lut_NameInput
    STA text_ptr+1
    LDA #$06
    STA dest_x
    LDA #$0A
    STA dest_y
    LDA #BANK_THIS
    STA cur_bank
    STA ret_bank
    JMP DrawComplexString
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Name Input Row Start lut  [$A00A :: 0x3A01A]
;;
;;    offset (in usable characters) to start of each row in the below lut_NameInput

lut_NameInputRowStart:
  .BYTE  0, 10, 20, 30, 40, 50, 60  ; 10 characters of data per row
                                    ;  (which is actually 20 bytes, because they have spaces between them)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Name Input lut  [$A011 :: 0x3A021]
;;
;;    This lut is not only used to get the character the user selection on the name input screen,
;;  but it also is stored in null-terminated string form so that the entire thing can be drawn with
;;  with a single call to DrawComplexString.  It's intersperced with $FF (spaces) and $01 (double line breaks)

lut_NameInput:
  .BYTE $8A, $FF, $8B, $FF, $8C, $FF, $8D, $FF, $8E, $FF, $8F, $FF, $90, $FF, $91, $FF, $92, $FF, $93, $01  ; A - J
  .BYTE $94, $FF, $95, $FF, $96, $FF, $97, $FF, $98, $FF, $99, $FF, $9A, $FF, $9B, $FF, $9C, $FF, $9D, $01  ; K - T
  .BYTE $9E, $FF, $9F, $FF, $A0, $FF, $A1, $FF, $A2, $FF, $A3, $FF, $BE, $FF, $BF, $FF, $C0, $FF, $FF, $01  ; U - Z ; , . <space>
  .BYTE $80, $FF, $81, $FF, $82, $FF, $83, $FF, $84, $FF, $85, $FF, $86, $FF, $87, $FF, $88, $FF, $89, $01  ; 0 - 9
  .BYTE $A4, $FF, $A5, $FF, $A6, $FF, $A7, $FF, $A8, $FF, $A9, $FF, $AA, $FF, $AB, $FF, $AC, $FF, $AD, $01  ; a - j
  .BYTE $AE, $FF, $AF, $FF, $B0, $FF, $B1, $FF, $B2, $FF, $B3, $FF, $B4, $FF, $B5, $FF, $B6, $FF, $B7, $01  ; k - t
  .BYTE $B8, $FF, $B9, $FF, $BA, $FF, $BB, $FF, $BC, $FF, $BD, $FF, $C2, $FF, $C3, $FF, $C4, $FF, $C5, $01  ; u - z - .. ! ?
  .BYTE $01
  .BYTE $FF, $FF, $FF, $9C, $8E, $95, $8E, $8C, $9D, $FF, $FF, $97, $8A, $96, $8E, $00                      ;   SELECT  NAME

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  LUT for party generation  [$A0AE :: 0x3A0BE]
;;
;;    This LUT is copied to the RAM buffer 'ptygen' which is used to
;;  track which class is selected for each character, what their name is,
;;  where they're to be drawn, etc.  This can be changed to assign a default
;;  party or default names, and to rearrange some of the graphics for the
;;  Party Generation Screen
;;
;;    See details of 'ptygen' buffer in RAM for a full understanding of
;;  the format of this table.

lut_PtyGenBuf:
  .BYTE $00,$00,$FF,$FF,$FF,$FF,$07,$0C,$05,$06,$40,$40,$04,$04,$30,$40
  .BYTE $01,$00,$FF,$FF,$FF,$FF,$15,$0C,$13,$06,$B0,$40,$12,$04,$A0,$40
  .BYTE $02,$00,$FF,$FF,$FF,$FF,$07,$18,$05,$12,$40,$A0,$04,$10,$30,$A0
  .BYTE $03,$00,$FF,$FF,$FF,$FF,$15,$18,$13,$12,$B0,$A0,$12,$10,$A0,$A0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Enter Intro Story   [$A0EE :: 0x3A0FE]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


EnterIntroStory:
    JSR IntroTitlePrepare      ; load CHR, start music, other prepwork

    LDA $2002           ; reset PPU toggle and set PPU address to $2000
    LDA #>$2000         ;   (start of nametable)
    STA $2006
    LDA #<$2000
    STA $2006

    LDX #($03C0 / 4)    ; Fill the nametable with tile $FF (blank space)
    LDA #$FF            ;  this loop does a full $03C0 writes
  @NTLoop:
      STA $2007
      STA $2007
      STA $2007
      STA $2007
      DEX
      BNE @NTLoop

    LDX #$40            ; Next, fill the attribute table so that all tiles use
    LDA #%01010101      ;  palette %01.  This palette will be used for fully
  @AttrLoop:            ;  faded-out text (ie:  text using this palette is
      STA $2007         ;  invisible)
      DEX
      BNE @AttrLoop

    LDA #$01            ; fill palettes %01 (faded out) and %10 (animating) with 
    STA cur_pal + $6    ;  $01 blue.
    STA cur_pal + $7
    STA cur_pal + $A
    STA cur_pal + $B

    ; now that the NT is cleared and palettes are prepped -- time to draw
    ;  the intro story text.  This is accomplished by drawing a single
    ;  Complex String

    LDA #<lut_IntroStoryText  ; load up the pointer to the intro story text
    STA text_ptr
    LDA #>lut_IntroStoryText
    STA text_ptr+1

    LDA #0               ; disable menu stalling (PPU is off)
    STA menustall

    LDA #BANK_INTROTEXT  ; select bank containing text
    STA cur_bank
    LDA #BANK_THIS       ; and record this bank to return to
    STA ret_bank

    LDA #3               ; draw text at coords 1,3
    STA dest_y
    LDA #1
    STA dest_x

    JSR DrawComplexString   ; draw intro story as a complex string!

    JSR TurnMenuScreenOn_ClearOAM  ; turn on the PPU
    JSR IntroStory_MainLoop        ; and run the main loop of the intro story

    LDA #0              ; once the intro story exits, shut off the PPU
    STA $4015
    STA respondrate     ; reset the respond rate
    RTS                 ;  and exit



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Enter Title Screen  [$A156 :: 0x3A166]
;;
;;    Draws the title screen and runs its logic loop.
;;
;;  OUT:  respondrate
;;                  C = clear if player selected "Continue"
;;                      set if player selected "New Game"
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

EnterTitleScreen:
    JSR TitleScreen_Copyright     ; Do prepwork, and draw the copyright text

   ;; The rest of the title screen consists of 3 boxes, each containing a single
   ;;  complex string.  The game here simply draws each of those boxes and 
   ;;  the contained string in order.

    LDA #BANK_THIS   ; set current bank (containing text)
    STA cur_bank     ;  and ret bank (to return to) to this bank
    STA ret_bank     ;  see DrawComplexString for why this is needed

    LDA #11          ; first box is at 11,10  with dims 10x4
    STA box_x        ;  and contains "Continue" text
    LDA #10
    STA box_y
    LDA #4
    STA box_ht
    LDA #10
    STA box_wd
      JSR DrawBox
    LDA #<lut_TitleText_Continue
    STA text_ptr
    LDA #>lut_TitleText_Continue
    STA text_ptr+1
    LDA #0
    STA menustall    ; disable menu stalling (PPU is off)
     JSR DrawComplexString

    LDA #15          ; next box is same X pos and same dims, but at Y=15
    STA box_y        ;  and contains text "New Game"
      JSR DrawBox
    LDA #<lut_TitleText_NewGame
    STA text_ptr
    LDA #>lut_TitleText_NewGame
    STA text_ptr+1
      JSR DrawComplexString

    LDA #20          ; last box is moved left and down a bit (8,20)
    STA box_y        ;  and is a little fatter (wd=16)
    LDA #8           ; this box contains "Respond Rate"
    STA box_x
    LDA #16
    STA box_wd
      JSR DrawBox
    LDA #<lut_TitleText_RespondRate
    STA text_ptr
    LDA #>lut_TitleText_RespondRate
    STA text_ptr+1
      JSR DrawComplexString

    LDA #$0F                ; enable APU (isn't necessary, as the music driver
    STA $4015               ;   will do this automatically)
    JSR TurnMenuScreenOn_ClearOAM  ; turn on the screen and clear OAM
                                   ;  and continue on to the logic loop


  ;; This is the main logic loop for the Title screen.

  @Loop:
    JSR ClearOAM            ; Clear OAM

    LDX cursor              ; Draw the cursor sprite using a fixed X coord of $48
    LDA #$48                ;  and using the current cursor position to get the Y coord
    STA spr_x               ;  from a LUT
    LDA lut_TitleCursor_Y, X
    STA spr_y
    JSR DrawCursor

    JSR WaitForVBlank_L     ; Wait for VBlank
    LDA #>oam               ;  and do Sprite DMA
    STA $4014               ; Then redraw the respond rate
    JSR TitleScreen_DrawRespondRate

    JSR UpdateJoy           ; update joypad data
    LDA #BANK_THIS          ;  set cur_bank to this bank (for CallMusicPlay)
    STA cur_bank

    JSR TitleScreen_Music   ; call music playback, AND get joy_a (weird little routine)
    ORA joy_start           ; OR with joy_start to see if either A or Start pressed
    BNE @OptionChosen       ; if either pressed, a menu option was chosen.

    LDA joy                 ; otherwise mask out the directional buttons from the joy data
    AND #$0F
    CMP joy_prevdir         ; see if the state of any directional buttons changed
    BEQ @Loop               ; if not, keep looping

    STA joy_prevdir         ; otherwise, record changes to direction
    CMP #0                  ;  see if the change was buttons being pressed or lifted
    BEQ @Loop               ;  if buttons were being lifted, do nothing (keep looping)

    CMP #$04                ; see if they pressed up/down or left/right
    BCC @LeftRight

  @UpDown:
    LDA cursor              ; if up/down, simply toggle the cursor between New Game
    EOR #1                  ;  and continue
    STA cursor
    JSR PlaySFX_MenuSel     ; play a little sound effect (the sel sfx, not the move sfx like you
    JMP @Loop               ;  may expect).  Then resume the loop.

  @LeftRight:
    CMP #RIGHT              ; did they press Right?
    BNE @Left               ;  if not, they must've pressed Left
    LDA #1                  ; add +1 to rate if right
    BNE :+
       @Left:
         LDA #-1            ; or -1 if left
:   CLC
    ADC respondrate         ; add/subtract 1 from respond rate
    AND #7                  ; mask to wrap it from 0<->7
    STA respondrate

    JSR PlaySFX_MenuMove    ; play the move sound effect, and continue looping!
    JMP @Loop

@OptionChosen:              ; Jumps here when the player presses A or Start (selected an option)
    LDA cursor              ;  this CMP will set C if they selected option 1 (New Game)
    CMP #1                  ;  and will clear C if they selected option 0 (Continue)
    RTS                     ;  then exit!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  IntroTitlePrepare  [$A219 :: 0x3A229]
;;
;;    Does various preparation things for the intro story and title screen.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

IntroTitlePrepare:
    LDA #$08               ; set soft2000 so that sprites use right pattern
    STA soft2000           ;   table while BG uses left
    LDA #0
    STA $2001              ; turn off the PPU

    JSR LoadMenuCHRPal     ; Load necessary CHR and palettes

    LDA #$41
    STA music_track        ; Start up the crystal theme music

    LDA #0
    STA joy_a              ; clear A, B, Start button catchers
    STA joy_b
    STA joy_start
    STA cursor
    STA joy_prevdir        ; as well as resetting the cursor and previous joy direction

    JMP ClearNT            ; then wipe the nametable clean and exit


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  TitleScreen_DrawRespondRate  [$A238 :: 0x3A248]
;;
;;    Draws the respond rate on the title screen.  Called every frame
;;  because the respond rate can change via the user.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TitleScreen_DrawRespondRate:
    LDA #>$22D6        ; Respond rate is drawn at address $22D6
    STA $2006
    LDA #<$22D6
    STA $2006

    LDA respondrate    ; get the current respond rate (which is zero based)
    CLC                ;  add $80+1 to it.  $80 to convert it to the coresponding tile
    ADC #$80+1         ;  for the desired digit to print, and +1 to convert it from zero
    STA $2007          ;  based to 1 based (so it's printed as 1-8 instead of 0-7)

    LDA #0             ; then reset the scroll.
    STA $2005
    STA $2005
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Pointerless strings for the title screen  [$A253 :: 0x3A263]
;;
;;    These are complex strings drawn onto the title screen.  They
;;  have no pointer table -- instead the drawing code points to the
;;  strings directly (hence why each string is labelled)

lut_TitleText_Continue:
  .BYTE $8C,$98,$97,$9D,$92,$97,$9E,$8E,$00

lut_TitleText_NewGame:
  .BYTE $97,$8E,$A0,$FF,$90,$8A,$96,$8E,$00

lut_TitleText_RespondRate:
  .BYTE $9B,$8E,$9C,$99,$98,$97,$8D,$FF,$9B,$8A,$9D,$8E,$00


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Small LUT for the Y position of the title screen cursor  [$A272 :: 0x3A282]

lut_TitleCursor_Y:
  .BYTE $58   ; to point at "Continue"
  .BYTE $80   ; to point at "New Game"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  IntroStory_MainLoop  [$A274 :: 0x3A284]
;;
;;    Main loop for the intro story.  Called once the screen has been fully
;;  drawn.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

IntroStory_MainLoop:
    LDA #<$23C0                ; start animating blocks from the start of the attribute
    STA intro_ataddr           ; table ($23C0)

  @Loop:
    JSR IntroStory_AnimateBlock  ; animate a block

    LDA intro_ataddr             ; then add 8 to animate the next block (8 bytes of
    CLC                          ;   attribute per block)
    ADC #8
    STA intro_ataddr

    CMP #<$23F8                  ; and keep looping until all except for the very last block
    BCC @Loop                    ;  have been animated

     ; once all blocks have been animated, the entire intro story is now visible
     ;  simply keep doing frames in an endless loop.  IntroStory_Frame, will double-RTS
     ;  if the user presses A or B, which will break out of this loop.  It will also
     ;  escape this routine altogether if the user presses start, so this infinite
     ;  loop isn't really all that infinite.  See IntroStory_Frame for details.

  @InfiniteLoop:
    JSR IntroStory_Frame
    JMP @InfiniteLoop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  IntroStory_AnimateBlock  [$A28C :: 0x3A29C]
;;
;;    Animates a "block" (8 bytes of attribute data, 2 rows of text) for
;;  the intro story.  Not to be confused with the below _AnimateRow routine,
;;  which animates a single row within a block.
;;
;;    This routine updates intro_atbyte, which in turn updates onscreen
;;  attributes so that different rows of text become animated.
;;
;;    Note this routine calls IntroStory_Frame directly.. and with a JMP no less!
;;  IntroStory_Frame can double-RTS (see that routine for details), which means
;;  it is theoretically possible for the intro story to be prematurely exited
;;  if you happen to press A or B at *exactly* the wrong time (there's only a very
;;  slim 1 frame window where it could happen -- but still).  This could be considered
;;  BUGGED -- with the appropriate fix being to change the JMP IntroStory_Frame into
;;  JSR IntroStory_Frame, RTS.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

IntroStory_AnimateBlock:
    LDA #%01011010          ; set desired attribute byte.  This sets the top row of the block to use
    STA intro_atbyte        ;  palette %10 (the animating palette), and the bottom row to use
                            ;  palette %01 (faded-out / invisible palette)
    JSR IntroStory_AnimateRow  ; animate the top row of text

    LDA intro_ataddr        ; Check to see if this is the very last block ($23F8).  If it is, there's
    CMP #<$23F8             ;  no bottom row to animate -- the last block is really only half a block
    BEQ @Done               ; so if last block .. just exit now
                            ; However since this routine never gets called for the last block -- this
                            ;  is pointless

    LDA #%10101111             ; otherwise, set attribute so that top row uses %11 (fully faded in)
    STA intro_atbyte           ;  and bottom row uses %10 (animating)
    JSR IntroStory_AnimateRow  ;  animate the bottom row

  @Done:
    LDA #%11111111          ; lastly, set attribute byte so that the entire block uses %11
    STA intro_atbyte        ;  this prevents the bottom row from animating further
    JMP IntroStory_Frame    ;  Do a frame to update the actual attribute tables, then exit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  IntroStory_AnimateRow  [$A2A7 :: 0x3A2B7]
;;
;;    Animates the palette to "fade in" a row of text.  It assumes the
;;  desired text is using the appropriate palette (palette 2:  $3F08-3F0B).
;;
;;    The palette animation simply alternates between the "main" color (grey), and
;;  another "sub" color that's one shade darker than it.  It switches between those colors
;;  every frame for 16 frames... then brightens the "main" color by one shade until it
;;  is fully white ($30).
;;
;;    The "main" color starts at the dark grey ($00), and increases in shade by adding
;;  $10 to it every frame.  The other color ("one shade darker") is simply the main color
;;  minus $10 -- unless the main color is $00, in which case the background color of $01 blue
;;  is used instead.
;;
;;    This produces the following pattern:
;;  00 01 00 01 ...
;;  10 00 10 00 ...
;;  20 10 20 10 ...
;;  30 20 30 20 ...
;;  -routine exits-
;;
;;    This routine calls IntroStory_Frame, which can double-RTS (see that routine for details)
;;  The result of a double-RTS here is that this routine exits mid-animation, which essentially
;;  makes the entire row fade in immediately.  This is why repeatedly pressing A or B during
;;  the intro story makes the text appear faster.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

IntroStory_AnimateRow:
    LDA #$00
    STA intro_color        ; start the intro color at $00 grey

  @MainLoop:
    LDA intro_color        ; use the "main" intro color
    STA cur_pal + $B       ;   by copying it to the palette

  @SubLoop:
    JSR IntroStory_Frame   ; Do a frame
    INC framecounter       ; and update the frame counter

    LDA framecounter       ; see if we're on a 16th frame
    AND #$0F               ;  by masking out the low bits of the frame counter
    BNE @Alternate         ;  if not an even 16th frame, just alternate between main and sub colors

      LDA intro_color      ; ... if we are on an even 16th frame, brighten the main
      CLC                  ; color by adding $10 to it.
      ADC #$10
      STA intro_color
      CMP #$40             ; then check to see if we're done.  Done when the color was brightened
      BCC @MainLoop        ; from full white ($30) -- which would mean it's >= $40 after
      RTS                  ;  brightening.  If not done (< $40), continue loop.  Otherwise, exit

  @Alternate:
    LSR A                ; move the low bit of the frame counter into C to see if this is an even
    BCC @MainLoop        ;  or odd frame.  If even frame, use the main color next frame (@MainLoop)

    LDA cur_pal + $B     ; if an odd frame, get the previously used color (the main color)
    SEC                  ;  subtract $10 to make it one shade darker.
    SBC #$10
    BPL :+               ; if that caused it to wrap below 0
      LDA #$01           ;  use $01 blue instead
:   STA cur_pal + $B     ; and use this color (the sub color) next frame
    JMP @SubLoop         ; and continue looping

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  IntroStory_WriteAttr  [$A2DA :: 0x3A2EA]
;;
;;    Updates a row of attribute data for the intro story.  Writes
;;  8 bytes of attribute data to the given PPU address.
;;
;;  IN:  intro_ataddr = low byte of PPU address to write to
;;       intro_atbyte = attribute byte to write
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

IntroStory_WriteAttr:
    LDA $2002            ; reset PPU toggle

    LDA #$23             ; set PPU addr to $23xx (where xx is intro_ataddr)
    STA $2006
    LDA intro_ataddr
    STA $2006

    LDX #$08
    LDA intro_atbyte     ; write intro_atbyte 8 times
  @Loop:
      STA $2007
      DEX
      BNE @Loop

    RTS                  ; then exit!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  IntroStory_Frame  [$A2F2 :: 0x3A302]
;;
;;    Does a frame for the intro story.  It has a very strange way of returning
;;  control to the calling routine, though.
;;
;;    If A or B is pressed, it does a "double RTS" -- IE, not returning control
;;  to the the calling routine, but returning control to the routine that called
;;  the calling routine.
;;
;;    If Start is pressed, the routine doesn't exit at all, and instead, the game
;;  jumps back to GameStart (which brings up the title screen -- escaping the intro
;;  story).
;;
;;    If none of those buttons are pressed, the routine exits normally
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

IntroStory_Frame:
    JSR WaitForVBlank_L        ; wait for VBlank
    JSR IntroStory_WriteAttr   ; then do the attribute updates
    JSR DrawPalette            ; and draw the animating palette

    LDA soft2000
    STA $2000
    LDA #0
    STA $2005
    STA $2005                  ; then reset the scroll to zero

    STA joy_a                  ; clear A and B button catchers
    STA joy_b

    LDA #BANK_THIS             ; set current bank (needed when calling CallMusicPlay from
    STA cur_bank               ;   a swappable bank)
    JSR CallMusicPlay          ; Then call music play to keep music playing!

    JSR IntroStory_Joy         ; update joypad

    LDA joy_a             ; check to see if either A
    ORA joy_b             ;  or B were pressed
    BNE :+                ; if not...
      RTS                 ; ... exit normally

:   PLA                   ; if either A or B pressed, PLA to drop the last
    PLA                   ;  return address, then exit (does a double-RTS)
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Shop entry jump table [$A320 :: 0x3A330]
;;
;;    The jump table indicating entry points for various shop
;;  types.

lut_ShopEntryJump:
  .WORD EnterShop_Equip      ; weapon
  .WORD EnterShop_Equip      ; armor
  .WORD EnterShop_Magic      ; white magic
  .WORD EnterShop_Magic      ; black magic
  .WORD EnterShop_Clinic     ; clinic
  .WORD EnterShop_Inn        ; inn
  .WORD EnterShop_Item       ; item
  .WORD EnterShop_Caravan    ; caravan

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Enter Shop [$A330 :: 0x3A340]
;;
;;  IN:  shop_id
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

EnterShop:
    LDA #0
    STA $2001              ; turn off PPU
    STA $4015              ; silence audio
    STA joy_b              ; erase joypad A and B buttons
    STA joy_a

    JSR LoadShopCHRPal     ; load up the CHR and palettes (and the shop type)
    JSR DrawShop           ; draw the shop

    LDA shop_type              ; use the shop type to get the entry point for this shop
    ASL A                      ; double it (2 bytes per pointer)
    TAX                        ; put in X
    LDA lut_ShopEntryJump, X   ; load up the entry point from our jump table
    STA tmp
    LDA lut_ShopEntryJump+1, X
    STA tmp+1

    LDA #$4F
    STA music_track        ; set the music track to $4F (shop music)

    JMP (tmp)              ; jump to shop's entry point


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Enter Magic Shop  [$A357 :: 0x3A367]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  MagicShop_Exit:
    RTS

  MagicShop_CancelPurchase:
    LDA #$25
    JSR DrawShopDialogueBox      ; "too bad... something else?" dialogue
    JMP MagicShop_Loop           ; jump ahead to loop


EnterShop_Magic:
    LDA #$17
    JSR DrawShopDialogueBox      ; "Who will learn the spell" dialogue

  MagicShop_Loop:
    JSR ShopLoop_CharNames       ; Have the player select a party member
    BCS MagicShop_Exit           ; if they pressed B, exit the shop

    LDA cursor                   ; otherwise, get their selection
    ROR A
    ROR A
    ROR A
    AND #$C0                     ; shift and mask to get the char index
    STA shop_charindex           ; record it

    JSR ShopSelectBuyMagic       ; now have them select the spell to buy from the
                                 ;   shop inventory
    BCS MagicShop_Loop           ; if they press B, restart the loop

    LDX cursor                   ; otherwise get the cursor in X
    LDA item_box, X              ; use it to get the item ID they selected
    STA shop_curitem             ; record that as the current item

    JSR MagicShop_AssertLearn    ; assert that the selected character can learn
                                 ;  this spell.

                                 ; code only reaches here if the character
                                 ; can learn the spell.  If they can't
                                 ; AssertLearn jumps back to the magic loop.
    JSR DrawShopBuyItemConfirm   ; Draw item price and confirmation dialogue
    JSR ShopLoop_YesNo           ; Give the player the yes/no option

    BCS MagicShop_CancelPurchase ; cancel purchase if they pressed B
    LDA cursor
    BNE MagicShop_CancelPurchase ; or if they selected "No"

    JSR Shop_CanAfford           ; check to make sure they can afford the purchase
    BCC @FinalizePurchase        ; if yes... finalize the purchase

      LDA #$10                   ; ... otherwise
      JSR DrawShopDialogueBox    ; "you can't afford it" dialogue
      JMP MagicShop_Loop         ; keep looping

  @FinalizePurchase:
    JSR ShopPayPrice             ; subtract the item price from party GP
    LDX shop_charindex           ; get the empty slot in X
    LDA shop_spell               ; get the adjusted spell ID
    STA ch_spells, X             ; add this spell to char's magic list
    JMP EnterShop_Magic          ; and re-enter the shop



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Enter Equip Shop  [$A3AC :: 0x3A3BC]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  EquipShop_Cancel:
    LDA #$25
    JSR DrawShopDialogueBox     ; "Too bad... something else?" dialogue
    JMP EquipShop_Loop          ; jump ahead to loop


EnterShop_Equip:
    LDA #$09
    JSR DrawShopDialogueBox     ; "Welcome" dialogue

  EquipShop_Loop:
    JSR ShopLoop_BuySellExit    ; give player Buy/Sell/Exit option
    BCS @Exit                   ; if they pressed B, exit
    LDA cursor
    BEQ @Buy                    ; cursor=0  means they selected "Buy"
    CMP #$01
    BEQ @Sell                   ; cursor=1  is "Sell"

  @Exit:                        ; otherwise, "Exit"
    RTS

  ;; Buying

  @Buy:
    JSR LoadShopInventory       ; load shop inventory.  Needs to be done here
                                ;  because item box can be filled with a character's
                                ;  equipment instead of shop inventory.

    LDA #$0D
    JSR DrawShopDialogueBox     ; "what would you like" dialogue

    JSR ShopSelectBuyItem       ; have the player select something
    BCS EquipShop_Loop          ; if they pressed B, return to loop

    JSR DrawShopBuyItemConfirm  ; otherwise.. draw price confirmation text
    JSR ShopLoop_YesNo          ; and have them confirm it
    BCS EquipShop_Cancel        ; if they press B, cancel purchase
    LDA cursor
    BNE EquipShop_Cancel        ; if they select "No", cancel purchase

    JSR Shop_CanAfford          ; check to see if they can afford it
    BCC @CanAfford              ; if they can, jump ahead... otherwise...

      LDA #$10
      JSR DrawShopDialogueBox   ; "You can't afford it" dialogue
      JMP EquipShop_Loop        ; keep looping

  @CanAfford:
    LDA #$11
    JSR DrawShopDialogueBox      ; "who will you give it to" dialogue"
    JSR ShopLoop_CharNames       ; have the player select a character
    BCS EquipShop_Loop           ; if they press B, jump back to loop

    JSR EquipShop_GiveItemToChar ; give the item to the character
    BCC @FinalizePurchase        ; if they had room, finalize the purchase

      LDA #$0C                   ; otherwise (no room)
      JSR DrawShopDialogueBox    ; "You don't have room" dialogue
      JMP EquipShop_Loop         ; jump back to loop

  @FinalizePurchase:
    JSR ShopPayPrice             ; subtract the GP
    LDA #$13
    JSR DrawShopDialogueBox      ; "Thank you, what else?" dialogue
    JMP EquipShop_Loop           ; jump back to loop


  ;; Selling

  @_Loop:   JMP EquipShop_Loop   ; these two are here so that these labels
  @_Cancel: JMP EquipShop_Cancel ;  can be branched to.  The main labels
                                 ;  might be too far for a branch (can only branch
                                 ;  back 128 bytes).  I'm not sure that's
                                 ;  necessary though.... don't think the routine
                                 ;  is that big

  @Sell:
    LDA #$14
    JSR DrawShopDialogueBox      ; "Whose item do you want to sell" dialogue
    JSR ShopLoop_CharNames       ; have the player select a character
    BCS @_Loop                   ; if they pressed B, jump back to loop

    JSR EquipMenu_BuildSellBox   ; fill the item box with this character's equipment
    BCC @ItemsForSale            ; if there are items for sale... proceed.  otherwise....

      LDA #$1E
      JSR DrawShopDialogueBox    ; "you have nothing to sell" dialogue
      JMP EquipShop_Loop         ; jump back to loop

  @ItemsForSale:
    JSR ShopSelectBuyItem        ; have the user select an item to sell
    BCS @_Loop                   ; if they pressed B, jump back to the loop

    JSR DrawShopSellItemConfirm  ; draw the sell confirmation dialogue
    JSR ShopLoop_YesNo           ; give them the yes/no option
    BCS @_Cancel                 ; if they pressed B, canecl
    LDA cursor
    BNE @_Cancel                 ; if they selected "No", cancel

    LDA shop_type                ; push shop_type to stack to back it up
    PHA                          ;  (this is because it shares space with equipoffset)

    ASL A                        ; multiply shop type by 4
    ASL A                        ; (0=weapon, 4=armor)
    ORA #ch_weapons-ch_stats     ; OR that with weapon offset to get weapon or armor offset
    STA equipoffset              ; and put that in equipoffset

      ;  Need to do all this work here so that stats are adjusted if you're
      ;  selling an item that is currently equipped

      JSR UnadjustEquipStats       ; unadjust equipment stats

      LDX shop_charindex           ; erase the item from char's inventory
      LDA #0
      STA ch_stats, X

      JSR SortEquipmentList        ; sort the equipment list
      JSR ReadjustEquipStats       ; and readjust stats.

    PLA                    ; pull backed up shop type
    STA shop_type          ; and restore it

    LDA shop_curprice      ; copy 3 bytes of sale price to tmp
    STA tmp
    LDA shop_curprice+1
    STA tmp+1
    LDA #0
    STA tmp+2

    JSR AddGPToParty       ; give that money to the party
    JSR DrawShopGoldBox    ; redraw the gold box to reflect changes
    JMP EquipShop_Loop     ; and jump back to loop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Enter Item Shop   [$A471 :: 0x3A481]
;;
;;    Enter Caravan shop is also here.  Caravan operates exactly
;;  like an item shop -- only with different graphics.  But since the
;;  shop has been drawn already by the time code reaches this routine,
;;  the game can treat them as if they're the same.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ItemShop_Exit:
    RTS

  ItemShop_CancelBuy:           ; jumped to for cancelled purchases
    LDA #$25
    JSR DrawShopDialogueBox     ; "too bad, something else?" dialogue
    JMP ItemShop_Loop           ; return to loop

EnterShop_Caravan:
    BNE EnterShop_Item          ; this always branches.. but even if it didn't....
                                ;  EnterShop_Item is the next instruction.  Pretty silly.


EnterShop_Item:
    LDA #$09
    JSR DrawShopDialogueBox     ; draw the "welcome" dialogue

  ItemShop_Loop:
    JSR ShopLoop_BuyExit        ; give them the option to buy or exit
    BCS ItemShop_Exit           ; if they pressed B, exit
    LDA cursor
    BNE ItemShop_Exit           ; otherwise if they selected 'exit', then exit

    LDA #$0D
    JSR DrawShopDialogueBox     ; "what would you like" dialogue
    JSR ShopSelectBuyItem       ; let them choose an item from the shop inventory
    BCS ItemShop_Loop           ; if they pressed B, restart the loop

    JSR DrawShopBuyItemConfirm  ; confirm price dialogue
    JSR ShopLoop_YesNo          ; give them the yes/no option
    BCS ItemShop_CancelBuy      ; if they pressed B, cancel the purchase
    LDA cursor
    BNE ItemShop_CancelBuy      ; if they selected "No", cancel the purchase

    JSR Shop_CanAfford          ; check to ensure they can afford this item
    BCC @CheckForSpace          ; if they can, jump ahead to check to see if they have room for this item
      LDA #$10
      JSR DrawShopDialogueBox   ; if they can't, "you can't afford it" dialogue
      JMP ItemShop_Loop         ; and return to loop

  @CheckForSpace:
    LDX shop_curitem            ; get the item ID in X
    LDA items, X                ; use it to get the qty of this item that the player has
    CMP #99                     ; do they have less than 99 of this item?
    BCC @CompletePurchase       ; if yes, jump ahead to complete the purchase.  Otherwise...
      LDA #$0C
      JSR DrawShopDialogueBox   ; "you have too many" dialogue
      JMP ItemShop_Loop         ; return to loop

  @CompletePurchase:
    INC items, X                ; add one of this item to their inventory
    JSR ShopPayPrice            ; subtract the price from your gold amount
    LDA #$13
    JSR DrawShopDialogueBox     ; "Thank you, anything else?" dialogue
    JMP ItemShop_Loop           ; and continue loop



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ShopPayPrice  [$A4CD :: 0x3A4DD]
;;
;;    Subtracts the current shop price from the player's gold total
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ShopPayPrice:
    LDA gold
    SEC
    SBC shop_curprice         ; subtract low byte
    STA gold

    LDA gold+1
    SBC shop_curprice+1       ; mid byte
    STA gold+1

    LDA gold+2
    SBC #0                    ; and get borrow from high byte
    STA gold+2

    JMP DrawShopGoldBox       ; then redraw the gold box to reflect changes, and return

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Shop Can Afford [$A4EB :: 0x3A4FB]
;;
;;   Checks the current item price and sees if the player can afford it.
;;
;;  IN:  shop_curprice = price to check
;;
;;  OUT:             C = clear if they can afford, set if they can't
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


Shop_CanAfford:
    LDA gold+2           ; if high byte of gold is nonzero (> 65535 GP), they can afford it
    BNE @Yes

    LDA gold+1           ; check mid byte of gold
    CMP shop_curprice+1  ;  against high byte of price
    BEQ @CheckLow        ; if equal, need to check the low byte
    BCC @No              ; if gold < price, can't afford
    BCS @Yes             ; otherwise... can afford

  @CheckLow:
    LDA gold             ; compare low byte of gold
    CMP shop_curprice    ;  with low byte of price
    BCS @Yes             ;  if >=, can afford
                         ;  otherwise... can't
  @No:
    SEC                  ; SEC to indicate can't afford
    RTS

  @Yes:
    CLC                  ; CLC to indicate can afford
    RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Enter Shop Inn  [$A508 :: 0x3A518]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

EnterShop_Inn:
    LDA #$1B
    JSR DrawShopDialogueBox     ; "Welcome.. would you like to stay?" dialogue

    JSR ShopLoop_YesNo          ; give them the yes/no option
    BCS @Exit                   ; if they pressed B, exit
    LDA cursor
    BNE @Exit                   ; also exit if they selected 'No'

    JSR DrawInnClinicConfirm    ; draw the price confirmation dialogue
    JSR ShopLoop_YesNo          ; give them another yes/no option
    BCS @Exit                   ; again... pressed B = exit
    LDA cursor
    BNE @Exit                   ; select No = exit

    JSR InnClinic_CanAfford     ; assert that they can afford the price, and charge them

    LDA #$30                    ; code only reaches here if the price could be afforded
    STA $4004                   ;   silence square 2
    LDA #$7F                    ;   and reset it's F-value to zero.
    STA $4005                   ;  Game seems to do this in a few places... still not sure why
    LDA #$00                    ;  probably prevents unwanted ugly sounds when switching tracks?
    STA $4006                   ;   but it only does it for some tracks?
    STA $4007

    JSR MenuFillPartyHP         ; refill the party's HP
    JSR MenuRecoverPartyMP      ;  and MP
    JSR SaveGame                ;  then save the game (this starts the "you saved your game" jingle)

    LDA #0
    STA joy_a                   ; clear A and B catchers
    STA joy_b

    LDA #$1C
    JSR DrawShopDialogueBox     ; "Don't forget when you leave your game" dialogue

    LDA #$03
    JSR LoadShopBoxDims         ; erase shop box 3 (command box)
    JSR EraseBox

    JSR ClearOAM                ; clear OAM (to remove the cursor)
    JSR DrawShopPartySprites    ; draw the party
    JSR WaitForVBlank_L         ; then wait for VBlank before
    LDA #>oam                   ;   performing sprite DMA
    STA $4014                   ; all of this is to remove the cursor graphic without doing a real frame

    JSR FadeOutBatSprPalettes   ; and fade the party out

  @LoopOne:
    JSR ShopFrameNoCursor       ; do a shop frame (with no visible cursor)

    LDA music_track             ; check the music track
    CMP #$81                    ; if $81 (no music currently playing)...
    BNE :+
      LDA #$4F                  ; restart track $4F (shop music)
      STA music_track           ; this happens because music stops after the save jingle

:   LDA joy_a                   ; check to see if either A or B have been pressed
    ORA joy_b
    BEQ @LoopOne                ; and keep looping until one of them has

    JSR FadeInBatSprPalettes    ; then fade the party back in

  @Exit:
    LDA #$03
    JSR LoadShopBoxDims         ; erase shop box 3 (the command box)
    JSR EraseBox                ; this is redundant if they stayed at the inn, but
                                ; if the code jumped here because the user wanted to
                                ; exit the inn, then this has meaning

    LDA #$1D
    JSR DrawShopDialogueBox     ; "Hold Reset when turning power off" dialogue

    LDA #0
    STA joy_a
    STA joy_b                   ; clear A and B catchers

  @LoopTwo:
    JSR ShopFrameNoCursor       ; do a frame

    LDA music_track             ; check to see if the music has silenced (will happen
    CMP #$81                    ;  after the save jingle ends)
    BNE :+
      LDA #$4F                  ; if is has, restart track $4F (shop music)
      STA music_track
:   LDA joy_a                   ; check to see if A or B have been pressed
    ORA joy_b
    BEQ @LoopTwo                ; and keep looping until one of them has

    RTS                         ; then exit


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Enter Shop Clinic  [$A5A1 :: 0x3A5B1]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ClinicShop_Exit:
    RTS


EnterShop_Clinic:
    LDA #0
    STA joy_a                  ; clear A and B button catchers
    STA joy_b

    JSR ClinicBuildNameString  ; build the name string (also tells us if anyone is dead)
    BCC @NobodysDead           ; if nobody is dead... skip ahead

    LDA #$20
    JSR DrawShopDialogueBox    ; "Who needs to come back to life" dialogue

    JSR Clinic_SelectTarget    ; Get a user selection
    LDA cursor                 ;   grab their selection
    STA shop_curitem           ;   and put it in cur_item to hold it for later
    BCS ClinicShop_Exit        ; If they pressed B, exit.

    JSR DrawInnClinicConfirm   ; Draw the cost confirmation dialogue
    JSR ShopLoop_YesNo         ; give them the yes/no option
    BCS EnterShop_Clinic       ; If they pressed B, restart loop
    LDA cursor
    BNE EnterShop_Clinic       ; if they selected "No", restart loop

    JSR InnClinic_CanAfford    ; otherwise, they selected "Yes".  Make sure they can afford the charge

    LDA shop_curitem           ; code only reaches here if they can afford it.
    CLC
    ADC shop_curitem           ; add their original selection to itself twice (ie:  *3)
    ADC shop_curitem
    TAX                        ; cursor*3 in X
    LDA str_buf+$10, X         ; use that to get the char ID from the previously compiled string
                               ;  (see ClinicBuildNameString for how this string is built and why this works)

    ROR A                      ; A is currently $10-$13
    ROR A                      ; convert this number into a usable char index ($00,$40,$80, or $C0)
    ROR A                      ;   by shifting it
    AND #$C0                   ; and masking out desired bits
    TAX                        ; put the char index in X.  This is the car we are to revive.

    LDA #$00
    STA ch_ailments, X         ; erase this character's ailments (curing his "death" ailment)
    LDA #$01
    STA ch_curhp, X            ; and give him 1 HP

    LDA #0
    STA joy_a
    STA joy_b                  ; clear A and B catchers

    LDA #$21
    JSR DrawShopDialogueBox    ; "Warrior!  Return to life!"  dialogue

    LDA #$03
    JSR LoadShopBoxDims        ; erase shop box 3 (command box)
    JSR EraseBox

  @ReviveLoop:
    JSR ShopFrameNoCursor      ; do a frame
    LDA joy_a
    ORA joy_b
    BEQ @ReviveLoop            ; and loop (keep doing frames) until A or B pressed
    JMP EnterShop_Clinic       ; then restart the clinic loop

  @NobodysDead:
    LDA #$23
    JSR DrawShopDialogueBox    ; if nobody is dead... "You don't need my help" dialogue

    LDA #0
    STA joy_a
    STA joy_b                  ; clear A and B catchers

  @ExitLoop:
    JSR ShopFrameNoCursor      ; do a frame
    LDA joy_a
    ORA joy_b
    BEQ @ExitLoop              ; and loop (keep doing frames) until either A or B pressed

    RTS                        ; then exit


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  EquipShop_GiveItemToChar  [$A61D :: 0x3A62D]
;;
;;    Finds whether or not a character has room in his inventory
;;  for the given weapon/armor.  If he does, the item is placed in
;;  his inventory.
;;
;;  IN:     cursor = char ID (0-3) of target character
;;       shop_type = 0 for weapon shop, 1 for armor shop
;;    shop_curitem = item ID of the weapon/armor
;;
;;  OUT:         C = clear if character had room
;;                     set if he didn't
;;               * = item is placed in char's inventory if he has room
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


EquipShop_GiveItemToChar:
    LDA cursor          ; get the char ID
    ROR A
    ROR A
    ROR A
    AND #$C0            ; shift and mask to get the char index
    STA shop_charindex  ; and record

    LDX shop_type       ; see if this is weapon or armor, and
    BNE @CheckArmor     ;  fork appropriately

  @CheckWeapons:
    LDY #ch_weapons - ch_stats   ; Y is index to empty slot
    LDX shop_charindex           ; X is char index

    LDA ch_weapons, X       ; check every weapon slot until
    BEQ @FoundWeaponSlot    ; we find an empty one
    INY                     ; INY to keep track of which slot
    LDA ch_weapons+1, X
    BEQ @FoundWeaponSlot
    INY
    LDA ch_weapons+2, X
    BEQ @FoundWeaponSlot
    INY
    LDA ch_weapons+3, X
    BEQ @FoundWeaponSlot

    SEC                     ; if no empty slot, SEC to indicate so
    RTS                     ; and exit

  @CheckArmor:
    LDY #ch_armor - ch_stats  ; exactly the same as CheckWeapons
    LDX shop_charindex        ; only check ch_armor instead

    LDA ch_armor, X
    BEQ @FoundArmorSlot       ; and jump to a different "found" fork
    INY
    LDA ch_armor+1, X
    BEQ @FoundArmorSlot
    INY
    LDA ch_armor+2, X
    BEQ @FoundArmorSlot
    INY
    LDA ch_armor+3, X
    BEQ @FoundArmorSlot

    SEC
    RTS

  @FoundWeaponSlot:
    TYA                   ; put index to empty slot in A
    CLC
    ADC shop_charindex    ; add to that the char index
    TAX                   ; and put in X

    LDA shop_curitem      ; get the item ID
    SEC
    SBC #$1C-1            ; subtract to convert to weapon IDs

    STA ch_stats, X       ; write this weapon ID to char's equipment list
    CLC                   ; CLC to indicate success
    RTS                   ; and exit!

  @FoundArmorSlot:
    TYA                   ; exactly the same as FoundWeaponSlot
    CLC
    ADC shop_charindex
    TAX

    LDA shop_curitem
    SEC
    SBC #$44-1            ; only subtract to get the armor ID

    STA ch_stats, X
    CLC
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Inn / Clinic Can Afford [$A689 :: 0x3A699]
;;
;;    Checks the cost of the inn/clinic and sees if the player can afford it.
;;  if they can't afford it, this displays text telling them so, then EXITS
;;  the inn/clinic.
;;
;;    This routine will only return to the code that called it if the player
;;  can afford the price
;;
;;  IN:  item_box = price to check (inn/clinic prices are stored in the itembox)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

InnClinic_CanAfford:
    LDA gold+2         ; if high byte of gold is nonzero (> 65535 GP), they can afford it
    BNE @CanAfford

    LDA gold+1
    CMP item_box+1     ; otherwise, compare mid byte of gold to high byte of cost
    BEQ @CheckLow      ; if equal... need to compare low bytes
    BCS @CanAfford     ; if greater... can afford
    BCC @CantAfford    ; if less... can't afford

  @CheckLow:
    LDA gold           ; compare low bytes
    CMP item_box
    BCS @CanAfford     ; if gold >= cost, can afford.  otherwise....

  @CantAfford:
    LDA #$10
    JSR DrawShopDialogueBox    ; draw "you can't afford it" dialogue

    LDA #0                     ; clear joy_a and joy_b markers
    STA joy_a
    STA joy_b
   @Loop:
      JSR ShopFrameNoCursor    ; then just keep looping frames
      LDA joy_a                ;  until either A or B pressed
      ORA joy_b
      BEQ @Loop

    PLA                ; then pull the previous return address, and exit
    PLA                ;  this is effectively a double-RTS.  Returning not
    RTS                ;  only from this routine, but the routine that called this routine
                       ;  IE:  this exits the shop.  Code does not return to the Inn/Clinic routine.

  @CanAfford:
    LDA gold           ; subtract the cost of the inn/clinic from the player's gold
    SEC
    SBC item_box
    STA gold
    LDA gold+1
    SBC item_box+1
    STA gold+1
    LDA gold+2
    SBC #0
    STA gold+2

    JMP DrawShopGoldBox  ; redraw the gold box to reflect changes, and exit



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Clinic_SelectTarget  [$A6D7 :: 0x3A6E7]
;;
;;     Draws the command box, fills it with the names of all the dead
;;  characters, then waits for the user to select one of them.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


Clinic_SelectTarget:
    LDA #$03
    JSR DrawShopBox            ; draw shop box #3 (command box)
    JSR ClinicBuildNameString  ; build the name string (this is a bit wasteful here.. this was
                               ;  just done in the clinic code prior to calling this routine.  Oh well.

    LDA #<(str_buf+$10)        ; set our text pointer to point to the generated string
    STA text_ptr
    LDA #>(str_buf+$10)
    STA text_ptr+1
    JSR DrawShopComplexString  ; and draw it

    JMP CommonShopLoop_Cmd     ; then do the shop loop to get the user's selection


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Clinic Build Name String   [$A6ED :: 0x3A6FD]
;;
;;     Builds the string to fill the command box.  The string consists of names
;;  of characters in the party that are dead.  This tring is placed in str_buf+$10,
;;  because str_buf shares space with item_box, and item_box still contains the price
;;  of this clinic.
;;
;;     The string is only built here... it is not actually drawn.
;;
;;     In addition, cursor_max is filled to the proper number of options
;;  available (ie:  the number of dead guys in the party).
;;
;;  OUT:   cursor_max = number of dead guys
;;                  C = set if at least 1 dead guy, clear if no dead guys
;;
;;     The compiled string is 3 bytes per included character in format:
;;  "1X 00 01" where 'X' is 0-3 indicating the char ID, and 01 is a double line
;;  break.  "1X 00" is a control code to draw that character's name.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

ClinicBuildNameString:
    LDY #0                ; Y will be our string index
    LDX #0                ; X will be our character index
    STX cursor_max        ; will count how many characters are dead

  @Loop:
    LDA ch_ailments, X    ; get this char's OB ailments
    CMP #$01              ; check to see if he's dead
    BNE @NotDead          ; if not... skip him.  Otherwise...

      TXA                 ; put char index in A
      ROL A
      ROL A
      ROL A
      AND #$03            ; shift and mask to make it char ID (0-3)

      ADC #$10            ; add 10 to get the "draw stat" string control code ($10-$13).  Carry is impossible here
      STA str_buf+$10, Y  ; put it in the string buffer
      LDA #$00
      STA str_buf+$11, Y  ; draw stat 0 (name)
      LDA #$01
      STA str_buf+$12, Y  ; followed by a double line break

      TYA
      CLC
      ADC #$03            ; add 3 to the string index (we just put 3 bytes in the buffer)
      TAY

      INC cursor_max      ; and increment our dead guy counter

  @NotDead:
    TXA
    CLC
    ADC #$40              ; add $40 to our char index (look at next char)
    TAX

    BNE @Loop             ; and keep looping until it wraps (4 iterations)

    LDA #$00
    STA str_buf+$10, Y    ; then put a null terminator at the end of the string

    LDA cursor_max        ; set C if we have at least 1 dead guy
    CMP #$01              ; clear C otherwise

    RTS                   ; and exit!

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Shop Frame  [$A727 :: 0x3A737]
;;
;;    Does a frame for shops.  Plays movement and selection sound effects
;;  where appropriate.  Like EquipMenuFrame... tmp+7 is used for previous
;;  directions pressed for the purposes of playing sound effects only!
;;  other parts of the shop code still use joy_prevdir for detecting
;;  cursor movement.
;;
;;    Routine comes in two flavors.  ShopFrame and ShopFrameNoCursor.
;;  both are identical, only the latter does not draw the cursor.
;;
;;    Note the inefficiency here.  Both routines meet up after they call
;;  music play, but they COULD meet up just before the call to WaitForVBlank_L
;;  since the code in both is identical at that point.
;;
;;    Strangely, NEITHER routine clears joy_a or joy_b, which means the shop
;;  code has to do this!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

ShopFrame:
    JSR ClearOAM               ; clear OAM
    JSR DrawShopPartySprites   ; draw the party sprites
    JSR DrawShopCursor         ; and the cursor
    JSR WaitForVBlank_L        ; the wait for VBlank

    LDA #>oam
    STA $4014                  ; do sprite DMA

    LDA #BANK_THIS
    STA cur_bank
    JSR CallMusicPlay          ; set the current bank, and call music play

    JMP _ShopFrame_CheckBtns   ; the jump ahead to check the buttons

    RTS                        ; useless RTS (impossible to reach)

ShopFrameNoCursor:
    JSR ClearOAM               ; do all the same things as above, in the same order
    JSR DrawShopPartySprites   ;  only do not draw the cursor
    JSR WaitForVBlank_L
    LDA #>oam
    STA $4014
    LDA #BANK_THIS
    STA cur_bank
    JSR CallMusicPlay          ; after we call MusicPlay, proceed to check the buttons

  _ShopFrame_CheckBtns:
    LDA joy                    ; get old joypad data for last frame
    AND #$0F                   ; isolate the directional buttons
    STA tmp+7                  ; and store it as our prev joy data

    JSR UpdateJoy              ; update joypad data
    LDA joy_a                  ; see if either A or B pressed
    ORA joy_b
    BEQ @CheckMovement         ; if not... check directionals

    JMP PlaySFX_MenuSel        ; if either A or B pressed, play the selection sound effect, and exit

  @CheckMovement:
    LDA joy                    ; joy current joypad data
    AND #$0F                   ; isolate directional buttons
    BEQ @Exit                  ; if no directional buttons down, exit
    CMP tmp+7                  ; compare to previous directional buttons
    BEQ @Exit                  ; if no change, exit
    JMP PlaySFX_MenuMove       ; otherwise, play the movement sound effec, and exit

  @Exit:
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Shop   [$A778 :: 0x3A788]
;;
;;     Draws most of the shop.  This includes the title box, gold box, and
;;  shopkeeper graphics... as well as the attribute tables for the whole screen.
;;  It does not draw the inventory, command, or dialogue boxes, nor does it draw
;;  any sprites.
;;
;;    shop_id and shop_type must both be set appropriately prior to calling
;;  this routine.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


DrawShop:
    JSR LoadShopInventory      ; load up this shop's inventory into the item box
    JSR ClearNT                ; clear the nametable

              ; Fill attribute tables
    LDA $2002                  ; reset the PPU toggle
    LDA #>$2300                ; set the ppu addr to $23C0  (attribute tables)
    STA $2006
    LDA #<$23C0
    STA $2006

    LDX #$00                     ; loop $40 time to copy our attribute LUT to the on-screen attribute tables
  @AttribLoop:
      LDA lut_ShopAttributes, X  ; fetch a byte from the lut
      STA $2007                  ; draw it
      INX
      CPX #$40                   ; repeat until X=$40
      BCC @AttribLoop

              ; Draw the shopkeeper
    LDX shop_type                ; get the shop type in X
    LDA lut_ShopkeepAdditive, X  ; use it to fetch the image additive from our LUT
    STA tmp+2                    ; tmp+2 is the image additive (see DrawImageRect)

    LDA #$06                     ; the shopkeeper image rectangle
    STA dest_y                   ;  coords:  $0B,$06
    LDA #$0B                     ;    dims:  10x10
    STA dest_x
    LDA #10
    STA dest_wd
    STA dest_ht

    LDA #<lut_ShopkeepImage      ; get the pointer to the shopkeeper image
    STA image_ptr
    LDA #>lut_ShopkeepImage
    STA image_ptr+1

    JSR DrawImageRect            ; draw the image rect

    LDA #0
    STA menustall                ; disable menu stalling (PPU is off)

    LDA #$01
    JSR DrawShopBox              ; draw shop box ID=1  (the title box)

    LDA shop_type                ; get the shop type
    JSR DrawShopString           ; and draw that string (the shop title string)

    JSR DrawShopGoldBox          ; draw the gold box

    JSR TurnMenuScreenOn_ClearOAM   ; then clear OAM and turn the screen on

    LDA #1
    STA menustall                ; now that the screen is on, turn on menu stalling as well

    RTS                          ; and exit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Load Shop Inventory  [$A7D1 :: 0x3A7E1]
;;
;;   Loads a shop's inventory into the item box
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LoadShopInventory:
    LDA shop_id          ; get the shop ID
    ASL A                ; double it
    TAX                  ; put it in X for indexing

    LDA lut_ShopData, X      ; load up the pointer to shop's inventory
    STA tmp
    LDA lut_ShopData+1, X
    STA tmp+1

    LDY #$04             ; copy 5 items from the inventory (max for a shop)
  @Loop:
     LDA (tmp), Y        ; get the item from the shop LUTs
     STA item_box, Y     ; put it in the item box
     DEY                 ; decrement loop counter and index
     BPL @Loop           ; and loop until counter wraps

    LDA #0
    STA item_box+5       ; put a null terminator at the end of the item_box

    RTS                  ; and exit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Shop Gold Box   [$A7EF :: 0x3A7FF]
;;
;;     Draws the box containing remaining GP in the shops and all of its contents
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawShopGoldBox:
    LDA #$04
    JSR DrawShopBox            ; Draw shop box #4  (the gold box)
    JSR PrintGold              ; print current gold to format buffer
    JSR DrawShopComplexString  ; draw formatted string

    LDA dest_x                 ; add 6 to the X coord
    CLC
    ADC #$06
    STA dest_x

    LDA #$08                   ; draw shop string ID=$08 (" G")
    JMP DrawShopString         ; then exit


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  EquipMenu_BuildSellBox  [$A806 :: 0x3A816]
;;
;;     This routine fills the item box with a character's weapon or armor
;;  list.
;;
;;     This routine is totally stupid.  It only works if the character's equipment
;;  list is sorted (it stops as soon as it sees an empty slot).  Hence all the annoying
;;  automatic sorting in the equip menus and shops.
;;
;;     So yeah -- this routine is dumb.  But I suppose it works...
;;
;;  IN:      cursor = char ID (0-3) whose items we want to sell
;;        shop_type = 0 for weapon, 1 for armor
;;
;;  OUT:   item_box = filled with items for sale.  Null terminated
;;                C = set if no items available to sell
;;                      clear if at least 1 item
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

EquipMenu_BuildSellBox:
    LDA #0               ; zero Y (why not LDY?)  Y will count the number of 
    TAY                  ;  items this char has to sell, and be the item_box index

    LDA cursor           ; get the selected char ID
    ROR A
    ROR A
    ROR A
    AND #$C0             ; shift and mask to get char index

    LDX shop_type        ; check shop type, an fork appropriately
    BNE @Armor

  @Weapons:
    CLC
    ADC #ch_weapons - ch_stats  ; add weapon offset to char index
    STA shop_charindex          ; record
    TAX                         ;   and put in X for indexing

  @WeaponLoop:
    LDA ch_stats, X      ; get the weapon ID in this slot
    BEQ @LoopBreak       ; if slot is empty break out of loop
    AND #$7F             ;  otherwise, mask to remove equipped bit
    CLC
    ADC #$1C-1           ; add to convert to an Item ID
    STA item_box, Y      ; put it in the item_box
    INY
    INX                  ; inc both source and dest indeces
    CPY #$04             ; keep looping until we check all 4 slots
    BCC @WeaponLoop

  @LoopBreak:
    CPY #0               ; check to see if the char had any items
    BNE :+
      SEC                ; if they didn't... SEC to indicate so
      RTS                ;  and exit

:   LDA #0
    STA item_box, Y      ; otherwise, slap a null terminator at the end
    CLC                  ; CLC to indicate items are for sale
    RTS                  ; and exit

  @Armor:                      ; Same as above @Weapons block
    CLC
    ADC #ch_armor - ch_stats   ; except we check armor instead of weapons
    STA shop_charindex
    TAX

  @ArmorLoop:
    LDA ch_stats, X
    BEQ @LoopBreak
    AND #$7F
    CLC
    ADC #$44-1           ; convert from armor ID to item ID
    STA item_box, Y
    INY
    INX
    CPY #$04
    BCS @LoopBreak
    BCC @ArmorLoop


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Shop Select Buy Item   [$A857 :: 0x3A867]
;;
;;     Builds the string to fill the inventory box from the shop's
;;  inventory (ie:  items you can buy).  These items are taken from the item_box
;;  Once the string is drawn, cursor_max is set appropriatly (the number of items
;;  available for sale), and this routine calls the common shop loop.
;;
;;  OUT:   cursor = selected item
;;              C = set if B pressed, clear if A pressed
;;
;;     str_buf+$10 is used to hold the string because item_box and str_buf share
;;  space.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

ShopSelectBuyItem:
    LDA #0
    STA cursor_max    ; zero cursor max.
                      ; this will be an up counter counting the items in the shop.

    LDY #0            ; zero Y... this will be our string building index

  @Loop:
    LDX cursor_max       ; get the current shop item index in X
    LDA item_box, X      ; use it to get the next shop item
    BEQ @Done            ; if null terminator, no more shop items.  We're done

                         ; otherwise... start building the string
    STA str_buf+$11, Y   ; put item ID at +$11
    STA str_buf+$16, Y   ; and at +$16
    LDA #$02
    STA str_buf+$10, Y   ; put #2 (Item Name control code) at +$10 
    LDA #$03
    STA str_buf+$15, Y   ; put #3 (Item Price control code) at +$15
    LDA #$01
    STA str_buf+$12, Y   ; put #1 (double line break) at +$12 and +$17
    STA str_buf+$17, Y
    LDA #$FF
    STA str_buf+$13, Y   ; put a space at +$13, +$14
    STA str_buf+$14, Y   ; compiled string is:
                         ;[ItemName]
                         ;  [Price ]
                         ; (with double line breaks before and after the price line)
    TYA
    CLC                  ; add 8 to our string index so the next item
    ADC #8               ;  is drawn after this item
    TAY

    INC cursor_max       ; increment cursor_max, our item counter

    LDA cursor_max
    CMP #5               ; ensure we don't exceed 5 items (max the shop space will allow)
    BCC @Loop            ; if we haven't reached 5 items yet, keep looping

  @Done:
    LDA #0
    STA str_buf+$10, Y     ; slap a null terminator at the end of our string

    LDA #$02
    JSR DrawShopBox        ; draw shop box #2 (inv list box)

    LDA #<(str_buf+$10)    ; load up the pointer to our string
    STA text_ptr
    LDA #>(str_buf+$10)
    STA text_ptr+1
    JSR DrawShopComplexString  ; and draw it

    LDA #$03
    JSR LoadShopBoxDims
    JSR EraseBox           ; erase shop box #3 (command box)

    JMP CommonShopLoop_List  ; everything's ready!  Just run the common loop from here, then return

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ShopLoop_BuyExit [$A8B1 :: 0x3A8C1]
;;
;;    Opens up the shop command box, gives options "Buy" and "Exit"
;;  and loops until the user selects one.
;;
;;  OUT:  cursor = 0 for "Buy", 1 for "Exit"
;;             C = set if B pressed (exit), clear if A pressed
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ShopLoop_BuyExit:
    LDA #$03
    JSR DrawShopBox          ; draw shop box ID=3 (the command box)
    LDA #$0B
    JSR DrawShopString       ; draw shop string ID=$0B ("Buy"/"Exit")

    LDA #2
    STA cursor_max           ; 2 cursor options

    JMP CommonShopLoop_Cmd   ; do the common shop loop, and exit


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ShopLoop_YesNo [$A8C2 :: 0x3A8D2]
;;
;;    Exactly the same as ShopLoop_BuyExit, only it gives
;;  options "Yes" and "No".
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ShopLoop_YesNo:
    LDA #$03
    JSR DrawShopBox          ; draw shop box ID=3 (the command box)
    LDA #$0F
    JSR DrawShopString       ; draw shop string ID=$0F ("Yes"/"No")

    LDA #2
    STA cursor_max           ; 2 cursor options

    JMP CommonShopLoop_Cmd   ; do command shop loop and exit



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ShopLoop_BuySellExit  [$A8D3 :: 0x3A8E3]
;;
;;    Same thing as above... but with options "Buy", "Sell"
;;  and "Exit"
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ShopLoop_BuySellExit:
    LDA #$03
    JSR DrawShopBox          ; draw box 3 (command box)
    LDA #$0A
    JSR DrawShopString       ; string 0A ("Buy Sell Exit")

    LDA #$03
    STA cursor_max           ; 3 options

    JMP CommonShopLoop_Cmd   ; do command loop


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ShopLoop_CharNames [$A8E4 :: 0x3A8F4]
;;
;;    opens up the shop command box, fills it with the names of the
;;  party members, and has the user select one of them.  The selection
;;  is put in 'cursor', and C is set if B was pressed, and is cleared if
;;  A was pressed.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ShopLoop_CharNames:
    LDA #$03
    JSR DrawShopBox            ; draw shop box 3 (command box)

    LDA #<@NamesString         ; set our pointer to the string containing char names
    STA text_ptr
    LDA #>@NamesString
    STA text_ptr+1
    JSR DrawShopComplexString  ; and draw it

    LDA #4
    STA cursor_max             ; give the user 4 options

    JMP CommonShopLoop_Cmd     ; then run the common loop

  @NamesString:
  .BYTE $10,$00,$01   ; char 0's name, double line break
  .BYTE $11,$00,$01   ; char 1's, double line break
  .BYTE $12,$00,$01   ; char 2's, double line break
  .BYTE $13,$00,$00   ; char 3's, null terminator


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Common Shop Loop  [$A907 :: 0x3A917]
;;
;;    Shops consist of waiting for the user to navigate a menu.  And the cursor is always
;;  restricted to a fixed up/down motion.  Because of this, the loop that drives shops
;;  can all share the same routine.
;;
;;    Why the game didn't do something like this for ALL the menus... I'll never know.
;;
;;    This common shop loop will keep doing frames, checking for cursor movement each frame
;;  and exits only once the player presses A or B.
;;
;;    The routine comes in two flavors.  CommonShopLoop_Cmd and CommonShopLoop_List.  The
;;  difference between the two is where the cursor is drawn.  For _Cmd, it's drawn in the
;;  command box ("buy", "sell", etc box).  For _List it's drawn in the inventory list box.
;;
;;  IN:   cursor_max
;;
;;  OUT:       C = set if B pressed
;;                 clear if A pressed
;;        cursor = the selected menu item (assuming A was pressed)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


CommonShopLoop_Cmd:
    LDA #<lut_ShopCurs_Cmd     ; get the pointer to the desired cursor position LUT
    STA text_ptr               ;  put the pointer in (text_ptr).  Yes, I know... 
    LDA #>lut_ShopCurs_Cmd     ;  it's not really text.
    STA text_ptr+1
    JMP _CommonShopLoop_Main   ; then jump ahead to the main entry for these routines

CommonShopLoop_List:
    LDA #<lut_ShopCurs_List    ; exactly the same as _Cmd version of the routine
    STA text_ptr               ; only have (text_ptr) point to a different LUT
    LDA #>lut_ShopCurs_List
    STA text_ptr+1

      ; both flavors of this routine meet up here, after filling (text_ptr)
      ;   with a pointer to a LUT containing the cursor positions.

 _CommonShopLoop_Main:
    LDA #0
    STA cursor           ; reset the cursor to zero

    LDA joy              ; get the joy data
    AND #$0C             ; isolate up/down bits
    STA joy_prevdir      ; and store in prev_dir
                         ; then begin the loop...

  @Loop:
    LDA cursor           ; get the cursor
    ASL A                ; multiply by 2 (2 bytes per position)
    TAY                  ; put in Y for indexing

    LDA (text_ptr), Y    ; fetch the cursor X coord from out LUT
    STA shopcurs_x       ; and record it
    INY                  ; inc Y to get Y coord
    LDA (text_ptr), Y    ; read it
    STA shopcurs_y       ; and record it

    JSR ShopFrame        ; now that cursor position has been recorded... do a frame

    LDA joy_b
    BNE @B_Pressed       ; check to see if A or B have been pressed
    LDA joy_a
    BNE @A_Pressed

                         ; if neither pressed.. see if the cursor has been moved
    LDA joy              ; get joy
    AND #$0C             ; isolate up/down buttons
    CMP joy_prevdir      ; compare to previous buttons to see if button state has changed
    BEQ @Loop            ; if no change.. do nothing, and continue loop

    STA joy_prevdir      ; otherwise, record changes

    CMP #0               ; then check to see if buttons have been pressed or not
    BEQ @Loop            ; if not.. do thing, and continue loop

    CMP #$08             ; see if the button pressed was up or down
    BNE @Down

  @Up:
    DEC cursor           ; if up pressed, decrement the cursor by 1
    BPL @Loop            ; if it hasn't gone below zero, that's all -- continue loop

    LDA cursor_max       ; otherwise (below zero), wrap to cursor_max-1
    SEC
    SBC #$01
    JMP @MoveDone        ; desired cursor is in A, jump ahead to @MoveDone to write it back

  @Down:
    LDA cursor           ; if down pressed, get the cursor
    CLC
    ADC #$01             ; increment it by 1
    CMP cursor_max       ; check to see if we've gone over the cursor max
    BCC @MoveDone        ; if not, jump ahead to @MoveDone

    LDA #0               ; if yes, wrap cursor to zero

  @MoveDone:             ; code reaches here when A is to be the new cursor position
    STA cursor           ; just write it back to the cursor
    JMP @Loop            ; and continue loop


  @B_Pressed:            ; if B pressed....
    SEC                  ; SEC to indicate player pressed B
                         ;  and proceed to @ButtonDone

  @ButtonDone:           ; reached when the player has pressed B or A (exit this shop loop)
    LDA #0
    STA joy_a            ; zero joy_a and joy_b so further buttons will be detected
    STA joy_b
    STA joy_select       ; and select... but why?  select isn't used in shops?
    RTS

  @A_Pressed:            ; if A pressed...
    CLC                  ; CLC to indicate player pressed A
    BCC @ButtonDone      ;  and jump to @ButtonDone (always branches)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Shop cursor position luts  [$A977 :: 0x3A987]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

lut_ShopCurs_Cmd:    ; cursor positions for the command box
  .BYTE $28,$A0
  .BYTE $28,$B0
  .BYTE $28,$C0
  .BYTE $28,$D0

lut_ShopCurs_List:   ; cursor positions for the inventory list box
  .BYTE $A8,$20
  .BYTE $A8,$40
  .BYTE $A8,$60
  .BYTE $A8,$80
  .BYTE $A8,$A0


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Shop Select Buy Magic   [$A989 :: 0x3A999]
;;
;;     Builds the string to fill the inventory box from the shop's
;;  inventory (ie:  spells you can buy).  These items are taken from the item_box
;;  Once the string is drawn, cursor_max is set appropriatly (the number of items
;;  available for sale), and this routine calls the common shop loop.
;;
;;  OUT:   cursor = selected item
;;              C = set if B pressed, clear if A pressed
;;
;;     str_buf+$10 is used to hold the string because item_box and str_buf share
;;  space.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

ShopSelectBuyMagic:
    LDA #0
    STA cursor_max     ; zero cursor max... this will count the number of spells for sale.
    LDY #0             ; Y will be our string index

  @Loop:
    LDX cursor_max
    LDA item_box, X    ; get next item in shop inventory
    BEQ @Done          ; if it's zero (the null terminator), break out of the loop

    STA str_buf+$11, Y ; store item ID at $11
    STA str_buf+$16, Y ; and $16
    LDA #$02
    STA str_buf+$10, Y ; store $02 ("draw item name" control byte) at $10
    LDA #$03
    STA str_buf+$15, Y ; store $03 ("draw item price" control byte) at $15
    LDA #$01
    STA str_buf+$12, Y ; store $01 (double line break) at $12 and $17
    STA str_buf+$17, Y
    LDA #$C6
    STA str_buf+$13, Y ; store tile $C6 (the special 'L' character) at $13

    LDA str_buf+$11, Y ; get the item ID
    SEC
    SBC #$B0           ; subtract $B0 (spell IDs start at $B0
    LSR A
    LSR A
    LSR A              ; then divide by 8.  This gives us the spell's level
    SEC
    ADC #$80           ; SEC then add $80 (so really... add $81).  This converts the 0-based
                       ;  level, into the 1-based tile to draw.  IE:  level=0 prints the "1" tile.
    STA str_buf+$14, Y ; put that tile at $14

                 ; string is now:  "02 XX 01 C6 VV 03 XX 01" where XX is the item ID, and VV is the spell level
                 ;  which... after processing all control codes... will draw to:
                 ;
                 ; Name
                 ; LVPrice

    TYA
    CLC                ; that bit of string is 8 bytes... so add 8 to our
    ADC #$08           ;  string index:  Y
    TAY

    INC cursor_max     ; increment cursor max to count this entry
    LDA cursor_max
    CMP #5             ; check to see if we have 5 spells yet (can't sell more than 5)
    BCC @Loop          ; keep looping if we have less than 5

  @Done:
    LDA #$00
    STA str_buf+$10, Y ; put a null terminator at the end of the string

    LDA #$02
    JSR DrawShopBox    ; draw shop box 2 (inventory list box)

    LDA #<(str_buf+$10)         ; set the text pointer to our string
    STA text_ptr
    LDA #>(str_buf+$10)
    STA text_ptr+1
    JSR DrawShopComplexString   ; and draw it

    LDA #$03
    JSR LoadShopBoxDims         ; then erase shop box 3 (command box)
    JSR EraseBox

    JMP CommonShopLoop_List     ; and have the user select an option from the shop inventory list

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Shop Cursor   [$A9EF :: 0x3A9FF]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawShopCursor:
    LDA shopcurs_x     ; copy over the shop cursor coords
    STA spr_x
    LDA shopcurs_y
    STA spr_y
    JMP DrawCursor     ; then draw it, and exit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Shop Party Sprites [$A9FA :: 0x3AA0A]
;;
;;    Draws the sprites for the party when in a shop
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawShopPartySprites:
    LDA #$98
    STA spr_x
    LDA #$38
    STA spr_y
    LDA #1<<6
    JSR DrawOBSprite    ; draw char 1 at $98,$38

    LDA #$50
    STA spr_y
    LDA #2<<6
    JSR DrawOBSprite    ; draw char 2 at $98,$50

    LDA #$68
    STA spr_y
    LDA #3<<6
    JSR DrawOBSprite    ; draw char 3 at $98,$68

    LDA #$50
    STA spr_y
    LDA #$88
    STA spr_x
    LDA #0<<6
    JMP DrawOBSprite    ; draw char 0 at $88,$50, then exit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Shop String  [$AA26 :: 0x3AA36]
;;
;;     Draws a stock shop string as a Complex String given its ID number.  These strings
;;  include shop dialogue, stop titles, and other things.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawShopString:
    ASL A              ; double the ID (2 bytes per pointer)
    TAX                ; put it in X

    LDA lut_ShopStrings, X  ; load the pointer from the shop string LUT
    STA text_ptr
    LDA lut_ShopStrings+1, X
    STA text_ptr+1     ;  ... then draw it....
                       ; no JMP or RTS -- code seamlessly flows into DrawShopComplexString

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Shop Complex String  [$AA32 :: 0x3AA42]
;;
;;    This just calls DrawComplexString, but sets the required bank information
;;  first.
;;
;;    Somewhat wastefully, there's a routine virtually identical to this one
;;  that is used for menus!  See DrawMenuComplexString.  The only difference is that this
;;  routine uses X instead of A -- but since DrawComplexString overwrites both A and X...
;;  that is utterly meaningless.
;;
;;    Anyway, yeah.  Big waste.  No reason this routine needs to exist.  All references to it
;;  could just call DrawMenuComplexString.  C'est la vie... one of this game's many quirks.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawShopComplexString:
    LDX #BANK_THIS
    STX cur_bank
    STX ret_bank
    JMP DrawComplexString


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Shop Box  [$AA3B :: 0x3AA4B]
;;
;;    Draws a shop box
;;
;;  IN:   A = shop box ID number
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawShopBox:
    JSR LoadShopBoxDims      ; load the dims
    JMP DrawBox              ; draw it, then exit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Load Shop Box Dims  [$AA41 :: 0x3AA51]
;;
;;    Loads the positions and dimensions for the given shop box ID number (in A)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LoadShopBoxDims:
    TAX                    ; put box ID in X

    LDA lut_ShopBox_X, X        ; use it to copy data from LUTs
    STA box_x
    LDA lut_ShopBox_Y, X
    STA box_y
    LDA lut_ShopBox_Wd, X
    STA box_wd
    LDA lut_ShopBox_Ht, X
    STA box_ht

    LDA #BANK_THIS         ; set the cur bank.  cur bank is needed to be set
    STA cur_bank           ; for when boxes are drawn/erased when stalled

    RTS




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Shop Dialogue Box  [$AA5B :: 0x3AA6B]
;;
;;     Draws the shop dialogue and desired text string (shop text string ID in A)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawShopDialogueBox:
    PHA                  ; back up desired dialogue string by pushing it
    LDA #$00
    JSR DrawShopBox      ; draw shop box ID 0 (the dialogue box)
    PLA                  ; pull our dialogue string
    JMP DrawShopString   ; draw it, then exit



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Shop Buy Item Confirm  [$AA65 :: 0x3AA75]
;;
;;    This routine draws the 'are you sure you want to buy this' confirmation
;;  dialogue text for the shopkeeper after you select an item to buy.  This
;;  text involves printing the price of the item... and the game does a very weird
;;  way of getting the text for the item.  Rather than just building a string in
;;  a temp buffer and calling DrawComplexString... it modifies the existing
;;  string in str_buf (the string that was used to draw the shop inventory)
;;
;;    Each item in str_buf consists of 8 bytes:  "02 XX 01 FF FF 03 XX 01" where
;;  'XX' is the ID of this item.  (02 is the control code to indicate the item
;;  name is to be drawn, and 03 is the control code to indicate the price is
;;  to be drawn, and 01 is a double line break).
;;
;;    Rather than repeat '03 XX' in RAM somewhere, the game will calculate the
;;  position of the 03 XX bytes in that string and point to it!  It will then
;;  stick a null terminator after the price.
;;
;;    The shop inventory string starts at str_buf+$10 rather than str_buf, because
;;  str_buf is shared with item_box, which is still needed to hold the shop inventory.
;;  Also, the 03 XX bytes are 5 bytes into the string for the item.... and as said before
;;  each item has 8 bytes in the string.  So the formula for finding the price in that
;;  string is:  (cursor * 8) + str_buf+$15
;;
;;    This routine also fills shop_curitem and shop_curprice with the selected item
;;  and its price.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawShopBuyItemConfirm:
    LDA #$0E
    JSR DrawShopDialogueBox  ; draws "Gold  OK?" -- IE:  all the non-price text

    LDA cursor            ; get the cursor
    ASL A
    ASL A
    ASL A                 ; multiply by 8
    CLC
    ADC #<(str_buf+$15)   ; add str_buf+$15
    STA text_ptr          ; use as low byte of pointer.  See routine description
                          ; for details of why its doing this

    CLC                   ; add 2 and put in X.  X will now be
    ADC #$02              ;  where we need to put the null terminator (2 bytes after
    TAX                   ;  the start of the string -- all we're drawing is "03 XX")

    LDA #0
    STA str_buf, X        ; put the null terminator there

    DEX                   ; decrement X...
    LDA str_buf, X        ;   this gets the item ID from the string
    STA shop_curitem      ;  store in the current item

    LDA #>(str_buf+$15)   ; record high byte of our string pointer
    STA text_ptr+1

    LDA shop_curitem      ; get the current item
    JSR LoadPrice         ; load its price (gets put in tmp, tmp+1)

    LDA tmp               ; copy the price to shop_curprice
    STA shop_curprice
    LDA tmp+1
    STA shop_curprice+1

    JMP DrawShopComplexString  ; draw our complex string (item price), and exit



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Inn Clinic Confirm  [$AA9B :: 0x3AAAB]
;;
;;    Draws the confirmation dialogue for inns/clinics
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawInnClinicConfirm:
    LDA #$0E
    JSR DrawShopDialogueBox   ; draw "Gold  OK?" -- all the non-price text

    LDA item_box              ; copy the inn price (first two bytes in item_box)
    STA tmp                   ;  to tmp  (for PrintNumber)
    LDA item_box+1
    STA tmp+1
    LDA #0
    STA tmp+2                 ; 5digit print number needs 3 bytes... so just set high byte to zero

    JSR PrintNumber_5Digit    ; print it
    JMP DrawShopComplexString ; and draw it


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Shop Sell Item Confirm  [$AAB4 :: 0x3AAC4]
;;
;;    This routine draws the 'are you sure you want to sell this' confirmation
;;  dialogue text for the shopkeeper after you select an item to sell.  It also
;;  calculates the sale price before printing it, and stores the price in shop_curprice
;;
;;  IN:           cursor = selected menu item
;;        shop_charindex = index to start of character's equipment list
;;
;;  OUT:  shop_charindex = index to precise slot of item being sold (cursor is added to it)
;;         shop_curprice = sale price of item
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


DrawShopSellItemConfirm:
    LDA cursor               ; put the cursor (selected item) in X
    TAX
    CLC
    ADC shop_charindex       ; and add it to our char index
    STA shop_charindex       ;  so char index points directly to the item being sold

    LDA item_box, X          ; get the item ID from the item box (stupid)
    PHA                      ; push it (stupid)
    LDA #$0E
    JSR DrawShopDialogueBox  ; draw " Gold OK?" dialogue -- all the text except the actual price
    PLA                      ; pull the previously pushed item ID (would make more sense to just LDA it here)

    JSR LoadPrice            ; load the price of this item
    LSR tmp+1                ; then divide that price by 2 to get the sale price
    ROR tmp

    LDA tmp
    STA shop_curprice
    LDA tmp+1
    STA shop_curprice+1      ; copy the price to shop_curprice

    JSR PrintNumber_5Digit    ; print the sale price as 5 digits
    JMP DrawShopComplexString ; then draw it, and exit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  MagicShop_AssertLearn  [$AADF :: 0x3AAEF]
;;
;;    This routine checks to see whether or not the selected character
;;  is capable of learning the selected spell.  It checks for magic
;;  permissions... it checks to see whether or not the spell has already
;;  been learned... and it checks to see if the character has a free slot
;;
;;    If any of those checks fail... this routine does NOT return to the
;;  code that called it.  Instead it drops the return address by manually
;;  pulling it off the stack, then JMPs back to the magic shop loop.
;;
;;    The routine only performs an RTS if the character is capable of
;;  learning the spell.
;;
;;  OUT:  shop_charindex = index to empty slot to receive spell
;;            shop_spell = adjusted ID of this spell (1-8)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MagicShop_AssertLearn:
    LDX shop_charindex            ; get the target char's index
    LDA ch_class, X               ; use it to get his class
    ASL A                         ; double it (2 bytes per pointer)
    TAX                           ; and put in X for indexing

    LDA lut_MagicPermisPtr, X     ; get the pointer to this class's
    STA tmp                       ;    magic permissions table
    LDA lut_MagicPermisPtr+1, X   ; put that pointer in (tmp)
    STA tmp+1

    LDA shop_curitem    ; get the item ID of the spell we're to learn
    SEC
    SBC #$B0            ; subtract $B0 to convert it to magic ID (magic starts at item $B0)
    STA tmp+2           ; store magic ID in tmp+2 for future use

    AND #$07            ; get low 3 bits.  This will indicate the bit to use for permissions
    STA tmp+3           ; store it in tmp+3 for future use

    LDA tmp+2           ; get the magic ID
    LSR A               ; divide by 8 (gets the level of the spell)
    LSR A
    LSR A
    TAY                 ; put spell level in Y
    LDA (tmp), Y        ; use it as index to get the desired permissions byte
    STA tmp+4           ; store permissions byte in tmp+4 for future use

    LDX tmp+3           ; get required bit position
    LDA lut_BIT, X      ; use as index in the BIT lut to get the desired bit
    AND tmp+4           ; AND with permissions byte
    BEQ @HasPermission  ;  if result is zero, they have permission to learn

      LDA #$19                  ; otherwise...
      JSR DrawShopDialogueBox   ; "You can't learn that" dialogue
      PLA                       ; drop the return address
      PLA
      JMP MagicShop_Loop        ; and jump back to the magic shop loop

  @HasPermission:
    LDA tmp+2            ; get magic ID
    LSR A                ; divide by 2
    AND #$1C             ; and mask out high bits.
                         ;   this is effetively (spell_level*4)
    CLC                  ; add to that the char index, and you have
    ADC shop_charindex   ;  the index to the start of this level's spells
                         ;  for the target character

    TAX                  ; put that index in X
    LDA tmp+3            ; then get the low bits of the spell ID (0-7)
    CLC                  ;  add 1 to that, and you have the level-based
    ADC #$01             ;  spell ID (1-8).  These are how spell IDs are stored
                         ;  in the character's spell list

    CMP ch_spells, X     ; check each of this character's spells
    BEQ @AlreadyKnow     ;  on this level.  If any of them match the
    CMP ch_spells+1, X   ;  current spell... then the character
    BEQ @AlreadyKnow     ;  already knows this spell
    CMP ch_spells+2, X
    BEQ @AlreadyKnow

    LDA ch_spells, X     ; If they don't already know the spell.. check
    BEQ @FoundEmptySlot  ;  each slot until we find an empty one
    INX                  ; We need an empty slot to put this spell in
    LDA ch_spells, X
    BEQ @FoundEmptySlot
    INX
    LDA ch_spells, X
    BEQ @FoundEmptySlot

    LDA #$22                 ; if no empty slot found...
    JSR DrawShopDialogueBox  ; "That level is full" dialogue
    PLA                      ; drop return address
    PLA
    JMP MagicShop_Loop       ; and jump back to magic loop

  @AlreadyKnow:
    LDA #$1A                 ; if they already know the spell...
    JSR DrawShopDialogueBox  ; "You already know that" dialogue
    PLA                      ; drop return addy
    PLA
    JMP MagicShop_Loop       ; jump back to magic loop

                         ; if found empty slot -- we have success!
  @FoundEmptySlot:       ;  All conditions are met
    LDA tmp+3            ; get low bits
    CLC
    ADC #$01             ; and add 1 again to get the adjusted spell ID
    STA shop_spell       ; record that adjusted spell ID

    STX shop_charindex   ; record the index to the empty slot in our char index
    RTS                  ; and exit!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Save Game  [$AB69 :: 0x3AB79]
;;
;;     Saves the game to SRAM and plays that little jingle
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SaveGame:
    LDX #0            ; zero X for upcoming loop

    LDA ow_scroll_x           ; copy over OW information
    STA unsram_ow_scroll_x
    LDA ow_scroll_y
    STA unsram_ow_scroll_y
    LDA vehicle
    STA unsram_vehicle

  @CopyLoop:
      LDA unsram       , X    ; copy $400 bytes from "unsram" to sram
      STA   sram       , X
      LDA unsram + $100, X
      STA   sram + $100, X
      LDA unsram + $200, X
      STA   sram + $200, X
      LDA unsram + $300, X
      STA   sram + $300, X
      INX
      BNE @CopyLoop           ; loop until X expires ($100 iterations)

    LDA #$55                  ; set assertion bytes
    STA sram_assert_55        ;  if assertion bytes are ever different values
    LDA #$AA                  ;  the game knows SRAM has been corrupted
    STA sram_assert_AA        ;   like due to battery failure or something

        ; now we need to compute the checksum!
        ;  checksum further verifies that SRAM has not been comprimised

    LDA #$00
    STA sram_checksum         ; clear the checksum byte so that it will not interfere with checksum calculations
    LDX #$00                  ; clear X (loop counter)
    CLC                       ; and clear carry so it isn't included in checksum

  @ChecksumLoop:
      ADC sram       , X    ; sum every byte in SRAM
      ADC sram + $100, X    ;  note that carry is not cleared between additions
      ADC sram + $200, X
      ADC sram + $300, X
      INX
      BNE @ChecksumLoop     ; loop until X expires ($100 iterations)

                      ; after loop, A is now what the checksum computes to
    EOR #$FF          ;  to force it to compute to FF, invert the value
    STA sram_checksum ;  and write it to the checksum byte.  Checksum calculations will now result in FF

    LDA #$56
    STA music_track   ; play music track $56 (the "you saved your game" jingle)

    LDA #%00110000
    STA $4004         ; silence sq2 (volume=0)
    LDA #$7F
    STA $4005         ; disable sweep, and clear freq
    LDA #$00          ;  this probably just prevents an unwanted squeak or something when
    STA $4006         ;  the jingle starts.  Not entirely sure why the game does this,
    STA $4007         ;  but it does it for a few of these jingles.
    RTS



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Menu Fill Party HP  [$ABD2 :: 0x3ABE2]
;;
;;    Refills all party members' HP to maximum unless they're dead/stone
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MenuFillPartyHP:
    LDX #0                  ; X is our char index/loop counter.  start at zero
  @Loop:
    LDA ch_ailments, X      ; get this character's ailment

    CMP #$01
    BEQ @Skip               ; if dead... skip him
    CMP #$02
    BEQ @Skip               ; if stone... skip him

      LDA ch_maxhp, X       ; otherwise copy Max HP over to Cur HP
      STA ch_curhp, X
      LDA ch_maxhp+1, X
      STA ch_curhp+1, X

  @Skip:
    TXA                ; add $40 to X (next char)
    CLC
    ADC #$40
    TAX
    BNE @Loop          ; loop until X wraps (4 iterations)

    RTS                ; then exit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Menu Recover Party MP  [$ABF3 :: 0x3AC03]
;;
;;    Refills all MP for every party member except those that
;;  are dead or turned to stone.  For use out of battle only (in menus)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MenuRecoverPartyMP:
    LDX #0                 ; X is our character index.  Start with character 0
  @Loop:
    LDA ch_ailments, X     ; check OB ailments
    CMP #$01
    BEQ @Skip              ; if dead... skip
    CMP #$02
    BEQ @Skip              ; if stone... skip

      LDA ch_maxmp, X      ; otherwise... refill MP on all level to maximum
      STA ch_curmp, X
      LDA ch_maxmp+1, X
      STA ch_curmp+1, X
      LDA ch_maxmp+2, X
      STA ch_curmp+2, X
      LDA ch_maxmp+3, X
      STA ch_curmp+3, X
      LDA ch_maxmp+4, X
      STA ch_curmp+4, X
      LDA ch_maxmp+5, X
      STA ch_curmp+5, X
      LDA ch_maxmp+6, X
      STA ch_curmp+6, X
      LDA ch_maxmp+7, X
      STA ch_curmp+7, X

  @Skip:
    TXA             ; move index into A to do some math
    CLC
    ADC #$40        ; add $40 (next character in part
    TAX             ; put back in X
    BNE @Loop       ; and loop until it wraps (full party)

    RTS             ; then exit


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Bit position LUT  [$AC38 :: 0x3AC48]
;;
;;    This LUT simply contains bytes with 1 bit in each
;;  position set.  The game uses this table for magic permissions
;;  checking.
;;
;;    The basic formula for entries in this table is ($80 >> X)
;;  High bit is the first entry

lut_BIT:
  .BYTE %10000000
  .BYTE %01000000
  .BYTE %00100000
  .BYTE %00010000
  .BYTE %00001000
  .BYTE %00000100
  .BYTE %00000010
  .BYTE %00000001
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Shop Box LUTs  [$AC40 :: 0x3AC50]
;;
;;    These are the LUTs for the 5 boxes that appear in the shops.
;;  Since it's not easy to multiply an index by 5 and use that to index a single
;;  LUT, each element (X coord, Y coord, Width, Height) are each in their own
;;  seperate LUT (personally I think this is the better way to do it for all
;;  LUTs... no multiplication needed... but meh)
;;
;;  As for the box IDs:
;;
;;    0 = the shopkeeper's dialogue box
;;    1 = the title box (WEAPON / INN / whathaveyou)
;;    2 = the shop inventory box
;;    3 = the command box (buy/sell/exit/etc)
;;    4 = the gold box (how much money you have left)
;;


lut_ShopBox_X:    .BYTE $01,$0C,$16,$06,$12
lut_ShopBox_Y:    .BYTE $04,$02,$02,$12,$18
lut_ShopBox_Wd:   .BYTE $09,$08,$09,$09,$0A
lut_ShopBox_Ht:   .BYTE $0C,$04,$16,$0A,$04



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Shopkeep additive LUT  [$AC54 :: 0x3AC64]
;;
;;    Shopkeeper graphics all use the same image when drawn.  The thing that makes them
;;  different is that the tiles used in the drawing are offset by a specific amount, so that
;;  different tiles are drawn for different shops.  This LUT is the offset/additive to use
;;  for each shop type.  Each shopkeep's graphics consist of 14 tiles, so this LUT is basically
;;  just a multiplication by 14

lut_ShopkeepAdditive:
  .BYTE (0*14),(1*14),(2*14),(3*14),(4*14),(5*14),(6*14),(7*14)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Shop Attribute Table LUT  [$AC5C :: 0x3AC6C]
;;
;;    This is a copy of the attribute table to be used for the shop screen.
;;  This is a full $40 bytes that is copied IN FULL to the attribute table
;;  for the shop.

lut_ShopAttributes:
  .BYTE $FF,$FF,$FF,$55,$55,$FF,$FF,$FF
  .BYTE $FF,$FF,$3F,$05,$05,$CF,$FF,$FF
  .BYTE $FF,$FF,$33,$00,$00,$CC,$FF,$FF
  .BYTE $FF,$FF,$33,$00,$00,$CC,$FF,$FF
  .BYTE $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
  .BYTE $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
  .BYTE $FF,$FF,$FF,$FF,$AA,$AA,$AA,$AA
  .BYTE $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Shopkeeper image LUT  [$AC9C :: 0x3ACAC]
;;
;;    this is the 10x10 image that is drawn for the shopkeeper graphics
;;  in all shops.

lut_ShopkeepImage:

 .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 .BYTE $00,$00,$00,$00,$01,$01,$00,$00,$00,$00
 .BYTE $04,$05,$00,$00,$01,$01,$00,$00,$00,$00
 .BYTE $06,$07,$08,$09,$01,$01,$00,$00,$00,$00
 .BYTE $04,$05,$0A,$0B,$01,$01,$00,$00,$00,$00
 .BYTE $06,$07,$0C,$0D,$01,$01,$00,$00,$00,$00
 .BYTE $04,$05,$00,$00,$01,$01,$00,$00,$00,$00
 .BYTE $06,$07,$00,$00,$01,$01,$00,$00,$00,$00
 .BYTE $00,$00,$00,$00,$02,$03,$00,$00,$00,$00

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Magic permissions LUT [$AD00 :: 0x3AD10]
;;
;;    Each class has an 8-byte LUT to indicate which spells
;;  he can learn.  There is also a pointer table that points
;;  to each of these LUTs, so the game can use the character's class
;;  as an index to find the start of the desired permissions table
;;
;;    Personally... that seems like a waste, since you can just multiply
;;  the class ID by 8 to get the offset of the permissions table.  If they
;;  were going to use a pointer table, they should've at least shared
;;  common permissions tables (ie:  have fighter, thief, BB, master all share
;;  the same table, since none of them can learn any spells).  But the games
;;  doesn't do that.  Oh well... whatever.
;;
;;    Anyway, in the permissions tables... each byte represents 8 spells.
;;  The first byte represents level 1 spells, next byte is level 2 spells,
;;  etc.  The high bit reprents the first spell on that level (ie:  white
;;  magic is the high 4 bits).  If the cooresponding bit is set... that means
;;  that class CANNOT cast that spell


   ; pointer table -- one entry for each class
lut_MagicPermisPtr:
  .WORD @FT, @TH, @BB, @RM, @WM, @BM
  .WORD @KN, @NJ, @MA, @RW, @WW, @BW

   ; each class's permission table.  8 bytes (64 spells) per table

 @FT: .BYTE $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
 @TH: .BYTE $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
 @BB: .BYTE $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
 @RM: .BYTE $50,$00,$50,$50,$76,$FF,$FF,$FF
 @WM: .BYTE $0F,$0F,$0F,$0F,$0F,$4F,$CF,$FF
 @BM: .BYTE $F0,$F0,$F0,$F0,$F2,$F0,$F6,$FF

 @KN: .BYTE $4F,$0F,$5F,$FF,$FF,$FF,$FF,$FF
 @NJ: .BYTE $F0,$F0,$F0,$F0,$FF,$FF,$FF,$FF
 @MA: .BYTE $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
 @RW: .BYTE $40,$00,$50,$40,$30,$87,$D7,$FF
 @WW: .BYTE $0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F
 @BW: .BYTE $F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  LUT for Menu palettes  [$AD78 :: 0x3AD88]
;;
;;    This is only for the 3 BG palettes that aren't loaded by LoadMenuCHRPal
;;  IE:  the 'lit orb' palette, and the two middles ones that are mirrors of the 4th
;;  palette.  The middle palettes are coded to be used by the Equip Menu in order to
;;  highlight some text, but due to some problems (not highlighting all the letters in the
;;  string) that functionality is removed by having those palettes unchanged.

lutMenuPalettes:
  .BYTE  $0F,$30,$01,$22,  $0F,$00,$01,$30,  $0F,$00,$01,$30



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Play SFX Menu Sel  [$AD84 :: 0x3AD94]
;;
;;    Plays the ugly sound effect you hear when a selection is made (or a deselection)
;;  ie:  most of the time when A or B is pressed in menus, this sound effect is played.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PlaySFX_MenuSel:
    LDA #%10111010   ; 50% duty, length disabed, decay disabed, volume=$A
    STA $4004

    LDA #%10111010   ; sweep pitch upwards at speed %011 with shift %010
    STA $4005

    LDA #$40         ; set starting pitch to F=$040
    STA $4006
    LDA #$00
    STA $4007

    LDA #$1F
    STA sq2_sfx      ; indicate square 2 is busy with sfx for $1F frames
    RTS              ;  and exit!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Play SFX  Menu Move  [$AD9D :: 0x3ADAD]
;;
;;    Plays the ugly sound effect you hear when you move the cursor inside of menus
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PlaySFX_MenuMove:
    LDA #%01111010   ; 25% duty, length counter disabled, decay disabled, volume=$A
    STA $4004

    LDA #%10011011   ; sweep pitch upwards at speed %001 with shift %011
    STA $4005

    LDA #$20
    STA $4006        ; set starting pitch to F=$020
    LSR A
    STA $4007

    STA sq2_sfx      ; indicate square 2 is playing a sound effect for $10 frames
    RTS              ;  and exit!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Enter Main Menu  [$ADB3 :: 0x3ADC3]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

EnterMainMenu:
    LDA #$51
    STA music_track     ; set music track $51 (menu music)

    LDA #0
    STA $2001           ; turn off the PPU (we need to do some drawing)     
    STA $4015           ; and silence the APU.  Music sill start next time MusicPlay is called.

    JSR LoadMenuCHRPal        ; load menu related CHR and palettes
    LDX #$0B
  @Loop:                      ; load a few other main menu related palettes
      LDA lutMenuPalettes, X  ; fetch the palette from the LUT
      STA cur_pal, X          ; and write it to the palette buffer
      DEX
      BPL @Loop               ; loop until X wraps ($0C colors copied)

;; ResumeMainMenu is called to redraw and reenter the main menu from other
;;  sub menus (like from the item menu).  This will redraw the main menu, but
;;  won't restart the music or reload CHR/Palettes like EnterMainMenu does

ResumeMainMenu:
    LDA #0
    STA $2001                       ; turn off the PPU
    LDA #0
    STA menustall                   ; and disable menu stalling

    JSR DrawMainMenu                ; draw the main menu
    JSR TurnMenuScreenOn_ClearOAM   ; then clear OAM and turn the screen on

    LDA #0
    STA cursor                      ; flush cursor, joypad, and prev joy directions
    STA joy
    STA joy_prevdir

    LDA #5                          ; set cursor max to 5 (only 5 options on the main menu)
    STA cursor_max
                                    ; flow seamlessly into MainMenuLoop


MainMenuLoop:
    JSR ClearOAM                  ; clear OAM (erasing all existing sprites)
    JSR DrawMainMenuCursor        ; draw the cursor
    JSR DrawMainMenuCharSprites   ; draw the character sprites
    JSR MenuFrame                 ; Do a frame

    LDA joy_a                     ; check to see if A has been pressed
    BNE @A_Pressed
    LDA joy_b                     ; then see if B has been pressed
    BNE @B_Pressed
    JSR MoveCursorUpDown          ; then move the cursor up or down if up/down pressed
    JMP MainMenuLoop              ;  rinse, repeat

  @B_Pressed:
    LDA #0            ; turn PPU off
    STA $2001
    STA joy_a         ; flush A, B, and Start joypad recordings
    STA joy_b
    STA joy_start
    RTS               ; and exit the main menu (by RTSing out of its loop)


    ; if A pressed, we need to move into the appropriate sub menu based on 'cursor' (selected menu item)

  @A_Pressed:
    JSR PlaySFX_MenuSel         ; play the selection sound effect
    LDA cursor                  ; get the cursor
    BNE @NotItem                ; if zero.... (ITEM)

    @Item:
      JSR EnterItemMenu         ; enter item menu
      JMP ResumeMainMenu        ; then resume (redraw) main menu

  @NotItem:
    CMP #$01
    BNE @NotMagic               ; if cursor = 1... they selected 'magic'


    @MagicLoop:
      JSR MainMenuSubTarget     ; select a sub target
      BCS @EscapeSubTarget      ; if B pressed, they want to escape sub target menu.

      LDA cursor                ; otherwise (A pressed), get the selected character
      ROR A
      ROR A
      ROR A
      AND #$C0                  ; and shift it to a useable character index
      TAX                       ; and put in X

      LDA ch_ailments, X        ; get this character's OB ailments
      CMP #$01
      BEQ @CantUseMagic         ; if dead.. can't use their magic
      CMP #$02
      BNE @CanUseMagic          ; otherwise.. if they're not stone, you can

    @CantUseMagic:              ;if dead or stone...
      JSR PlaySFX_Error         ;  play error sound effect
      JMP @MagicLoop            ;  and continue magic loop until valid option selected

    @CanUseMagic:
      JSR EnterMagicMenu        ; if target is valid.. enter magic menu
      JMP ResumeMainMenu        ; then resume (redraw) main menu and continue

  @NotMagic:
    CMP #$02
    BNE @NotWeapon              ; if cursor = 2... they selected 'Weapon'

    @Weapon:
      LDA #ch_weapons-ch_stats  ; select offset for Weapon data
      JSR EnterEquipMenu        ; and enter equip menu (Weapons menu)
      JMP ResumeMainMenu        ; then resume main menu

  @NotWeapon:
    CMP #$03
    BNE @Status                 ; if cursor = 3... they selected 'Armor'

    @Armor:
      LDA #ch_armor-ch_stats    ; select offset for Armor data
      JSR EnterEquipMenu        ; and enter equip menu (Armor menu)
      JMP ResumeMainMenu        ; then resume main menu

  @Status:                      ; otherwise (cursor=4)... they selected 'Status'
    JSR MainMenuSubTarget       ; select a sub target
    BCS @EscapeSubTarget        ;  if they escaped the sub target selection, then escape it
    JSR EnterStatusMenu         ; otherwise, enter Status menu
    JMP ResumeMainMenu          ; then resume (redraw) main menu

@EscapeSubTarget:             ; if they escaped the sub target menu...
    LDA #0
    STA cursor                ; reset the cursor to zero
    LDA #5
    STA cursor_max            ; and reset cursor_max to 5 (only 5 main menu options)
    JMP MainMenuLoop          ; then return to main menu loop


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Main Menu Sub Target  [$AE71 :: 0x3AE81]
;;
;;    Gets the main menu sub target (ie:  gets the target for
;;  'Magic' and 'Status' main menu options
;;
;;  OUT:  C = set if B pressed (exited)
;;            clear if A pressed (selection made)
;;
;;        submenu_targ = desired target (only valid if A pressed)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MainMenuSubTarget:
    LDA #0              ; clear the cursor
    STA cursor

  @Loop:
    JSR ClearOAM                 ; clear OAM
    JSR DrawMainMenuCharSprites  ; draw the main menu battle sprite
    JSR DrawMainMenuSubCursor    ; draw the sub target cursor
    JSR MenuFrame                ; do a frame

    LDA joy_a
    BNE @A_Pressed               ; check if A pressed
    LDA joy_b
    BNE @B_Pressed               ; or B

    JSR MoveMainMenuSubCursor    ; if neither, move the cursor
    JMP @Loop                    ; and keep looping

  @B_Pressed:
    SEC            ; if B pressed, just SEC before exiting
    RTS            ;  to indicate failure / user escaped

  @A_Pressed:
    LDA cursor         ; if A pressed, record the submenu target
    STA submenu_targ
    CLC                ; then clear C to indicate a target has been selected
    RTS                ; and exit!



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Enter Magic Menu  [$AE97 :: 0x3AEA7]
;;
;;    The good old magic menu!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

EnterMagicMenu:
    LDA #0
    STA $2001                      ; turn off PPU
    STA menustall                  ; clear menustall
    STA descboxopen                ; and mark description box as closed

    JSR ClearNT                    ; clear the nametable
    JSR DrawMagicMenuMainBox       ; draw the big box containing all the spells
    PHP                            ; C is set if char has no spells -- we'll use that later, so PHP for now

    LDA #$07
    JSR DrawMainItemBox            ; draw the title box
    LDA #$29
    JSR DrawCharMenuString         ; and draw the "MAGIC" title text
    JSR TurnMenuScreenOn_ClearOAM  ; clear OAM and turn the screen on

    PLP                            ; pull status to see if character has any spells
    BCC :+                         ; if not....
      JMP MenuWaitForBtn_SFX       ;    simply wait for a button press and exit

:   LDA #0                    ; otherwise.... (they have magic)
    STA joy                   ; clear joypad
    STA joy_prevdir           ; and previous joy directions

MagicMenu_Loop:
    JSR ClearOAM              ; clear OAM
    JSR DrawMagicMenuCursor   ; draw the cursor
    JSR MenuFrame             ; and do a frame

    LDA joy_a
    BNE @A_Pressed            ; check if A pressed
    LDA joy_b
    BNE @B_Pressed            ; and B

    JSR MoveMagicMenuCursor   ; otherwise, move the cursor if a direction was pressed
    JMP MagicMenu_Loop        ; and keep looping

  @B_Pressed:
    RTS                       ; if B pressed, just exit

  @A_Pressed:
    JSR PlaySFX_MenuSel         ; play the selection sound effect
    JSR UseMagic_GetRequiredMP  ; see if we have MP to cast selected spell
    BCS @HaveMP                 ; if so, skip ahead

      LDA #$32                  ; otherwise...
      JSR DrawItemDescBox       ;  print "you don't have enough MP" or whatever message (description text ID=$32)
      JMP MagicMenu_Loop        ;  and return to loop

  @HaveMP:
    LDA submenu_targ       ; get character ID
    LSR A
    ROR A
    ROR A                  ; shift to get usable character index
    ORA cursor             ; ORA with cursor to get index to spell
    TAX                    ; and put in X for indexing

    ASL A                  ; double A and mask out the level bits
    AND #$38               ;  this effectively makes A the spell level * 8

    ORA ch_spells, X       ; then ORA with the selected spell on this level
    CLC                    ; and add the spell start constant - 1 (-1 because 0 is not a spell)
    ADC #MG_START-1        ;  A is now the ID number of the selected spell (supposedly.. but that's wrong)
                           ; BUGGED:  it just so happens that this code bug never occured in game because the spells
                           ;  were arranged in a way where it wouldn't be a problem.  HOWEVER.  The problem here lies
                           ;  in the fact that spells are 1-based and not zero based, and ORA is used instead of ADC
                           ; This means that the 8th spell on each spell level is 08 instead of 07.  Of course, 08 is
                           ;  also the start of the NEXT level of spells.
                           ; Example..... level 1 spells before this last ADC should be A=01-08.  and level 2 spells should
                           ;  be A=09-10.  Level 3 spells are A=11-18, and so on.  But consider SLOW, which is the 8th spell
                           ;  on level 2.  This spell will be listed as $08 in RAM on level 2.  now the above AND
                           ;  will result in A=08 for all level 2 spells... but when you ORA with SLOW's spell ID ($08)
                           ;  you still have $08!  not $10 like you should have (like what you'd get if you used ADC).
                           ; as a result... SLOW effectively has the same ID here as LIT.
                           ; But note that since the 8th spell on the level is always a black magic spell... and none
                           ; of them can be cast outside of battle, this bug never causes a problem.                        

       ;  This is all a hardcoded mess.  Which I guess is fine because only a handful
       ; of spells can be cast outside of battle.  This code just checks the above calculated
       ; spell ID against every spell you can cast outside of battle, and jumps to that spell's
       ; routine where appropriate.


    CMP #MG_CURE             ; just keep CMPing with every spell you can cast out of battle
    BNE :+                   ;  until we find a match
      JMP UseMagic_CURE      ;  then jump to that spell's routine
:   CMP #MG_CUR2
    BNE :+
      JMP UseMagic_CUR2
:   CMP #MG_CUR3
    BNE :+
      JMP UseMagic_CUR3
:   CMP #MG_CUR4
    BNE :+
      JMP UseMagic_CUR4
:   CMP #MG_HEAL
    BNE :+
      JMP UseMagic_HEAL
:   CMP #MG_HEL3
    BNE :+
      JMP UseMagic_HEL3
:   CMP #MG_HEL2
    BNE :+
      JMP UseMagic_HEL2
:   CMP #MG_PURE
    BNE :+
      JMP UseMagic_PURE
:   CMP #MG_LIFE
    BNE :+
      JMP UseMagic_LIFE
:   CMP #MG_LIF2
    BNE :+
      JMP UseMagic_LIF2
:   CMP #MG_WARP
    BNE :+
      JMP UseMagic_WARP
:   CMP #MG_SOFT
    BNE :+
      JMP UseMagic_SOFT
:   CMP #MG_EXIT
    BNE :+
      JMP UseMagic_EXIT

:   LDA #$33                ; gets here if no match found.
    JSR DrawItemDescBox     ; print description text ("can't cast that here")
    JMP MagicMenu_Loop      ; and return to magic loop

;;;;;;;;;;;;;;;

UseMagic_CURE:
    LDA framecounter        ; use the frame counter as a make-shift pRNG
    AND #$0F                ; mask out the low bits
    ORA #$10                ; and ORA (effective range:  16-31)
    BNE UseMagic_CureFamily ; and do the cure family of spells (always branches)

UseMagic_CUR2:
    LDA framecounter        ; same deal, but double the recovery
    AND #$1F
    ORA #$20                ; (effective range:  32-63)
    BNE UseMagic_CureFamily ; always branches

UseMagic_CUR3:
    LDA framecounter        ; same deal -- but double it again
    AND #$3F
    ORA #$40                ; (effective range:  64-127)
                            ; flow right into UseMagic_CureFamily

UseMagic_CureFamily:
    STA hp_recovery         ; store the HP to be recovered for future use
    JSR DrawItemTargetMenu  ; draw the item target menu (gotta choose who to target with this spell)
    LDA #$2B
    JSR DrawItemDescBox     ; load up the relevent description text

 CureFamily_Loop:
    JSR ItemTargetMenuLoop  ; handle the item target menu loop
    BCS CureFamily_Exit     ; if they pressed B, just exit

    LDA cursor              ; otherwise... get cursor
    ROR A
    ROR A
    ROR A
    AND #$C0                ; shift it to get a usable index
    TAX                     ; and put in X

    LDA ch_ailments, X      ; get target's OB ailments
    CMP #$01
    BEQ CureFamily_CantUse  ; if dead... can't target
    CMP #$02
    BEQ CureFamily_CantUse  ; can't target if stone, either

    LDA hp_recovery         ; otherwise, we can target.  Get the HP to recover
    JSR MenuRecoverHP_Abs   ; and recover it
    JSR DrawItemTargetMenu  ; then redraw the target menu screen to reflect changes

    LDX mp_required         ; put mp required index in X
    DEC ch_magicdata, X     ; and subtract 1 MP from the proper level

    JSR MenuWaitForBtn_SFX  ; Then just wait for the player to press a button.  Then exit by re-entering magic menu

  CureFamily_Exit:
    JMP EnterMagicMenu      ; to exit, re-enter (redraw) magic menu

  CureFamily_CantUse:
    JSR PlaySFX_Error       ; if can't use, play the error sound effect
    JMP CureFamily_Loop     ; and loop until you get a proper target

;;;;;;;;;;;;;;

UseMagic_CUR4:
    JSR DrawItemTargetMenu  ; draw item target menu
    LDA #$2B
    JSR DrawItemDescBox     ; and appropriate description text
    JSR ItemTargetMenuLoop  ; do the item target menu loop
    BCS CureFamily_Exit     ; if they pressed B to escape.. just exit

    LDA cursor              ; otherwise, get cursor (target character ID)
    ROR A
    ROR A
    ROR A
    AND #$C0                ; shift to get a usable charater index
    TAX                     ; and put in X

    LDA ch_maxhp+1, X       ; and copy max HP to cur HP
    STA ch_curhp+1, X       ; BUGGED:  game does not check ailments.  So you can cast
    LDA ch_maxhp, X         ;  this on a stoned or even a dead character!
    STA ch_curhp, X         ;  but while it will refill a dead character's HP, he will stay dead
                            ;  because he'll still have the "dead" ailment.

    JSR DrawItemTargetMenu  ; redraw target menu to reflect changes
    LDX mp_required         ; put MP required index in X
    DEC ch_magicdata, X     ; and subtract MP from proper level

    JSR MenuWaitForBtn_SFX  ; then just wait for the player to press a button
    JMP EnterMagicMenu      ; and re-enter (redraw) the magic menu

;;;;;;;;;;;;;;

UseMagic_HEAL:
    LDA framecounter        ; use the framecounter as a makeshift pRNG
    AND #$07                ;  get low bits
    CLC
    ADC #$10                ; and ADD $10 (not ORA)  (effective range:  16-23)
    BNE UseMagic_HealFamily ; and do the heal family of spells (always branches)

UseMagic_HEL2:
    LDA framecounter        ; same deal as HEAL, only different number
    AND #$0F
    CLC
    ADC #$20                ; (effective range:  32-47)
    BNE UseMagic_HealFamily ; always branches

UseMagic_HEL3:
    LDA framecounter        ; same deal....
    AND #$1F
    CLC
    ADC #$40                ; (effective range:  64-95)
                            ; flow into UseMagic_HealFamily

UseMagic_HealFamily: 
    STA hp_recovery         ; store HP recovery for future use
    LDA #$2C
    JSR DrawItemDescBox     ; draw the relevent description text
    JSR ClearOAM            ; clear OAM (no sprites)
    JSR MenuWaitForBtn      ; wait for the user to press a button

    LDA joy                 ; see whether the user pressed A or B
    AND #$80                ; check A
    BEQ HealFamily_Exit     ; if not A, they pressed B... so exit

    LDA hp_recovery         ; otherwise (pressed A), get HP recovery
    JSR MenuRecoverPartyHP  ; and give it to the entire party (also redraws the target menu for us)
    LDX mp_required
    DEC ch_magicdata, X     ; subtract the MP for this level
    JSR MenuWaitForBtn_SFX  ; then just wait for the player to press a button before exiting

 HealFamily_Exit:
    JMP EnterMagicMenu      ; to exit, just re-enter magic menu

;;;;;;;;;;;;;;;;;;;;;;;

UseMagic_LIFE:
    JSR DrawItemTargetMenu  ; draw the target menu
    LDA #$2E
    JSR DrawItemDescBox     ; and relevent description text
  @Loop:
    JSR ItemTargetMenuLoop  ; do the target loop
    BCS HealFamily_Exit     ; if they pressed B to exit, exit (hijack the HealFamily exit, whynot)

    LDA #$01                ; mark the ailment-to-cure as "death" ($01)
    STA tmp                 ; put it in tmp for 'CureOBAilment' routine
    JSR CureOBAilment       ; attempt to cure death!
    BCS @CantUse            ; if the char didn't have the death ailment... can't use this spell on him

    LDA #1                  ; otherwise it worked.  Give them 1 HP now that they're alive
    STA ch_curhp, X

    LDX mp_required
    DEC ch_magicdata, X     ; and take away MP for the spell cast

    JSR DrawItemTargetMenu  ; redraw target menu to reflect changes
    JSR MenuWaitForBtn_SFX  ; then wait for the user to press a button
    JMP EnterMagicMenu      ; and exit by re-entering magic menu

  @CantUse:
    JSR PlaySFX_Error       ; if you can't use it, play the error sound
    JMP @Loop               ;  and loop!


UseMagic_LIF2:
    JSR DrawItemTargetMenu  ; Exactly the same as LIFE, except...
    LDA #$2E
    JSR DrawItemDescBox
  @Loop:
    JSR ItemTargetMenuLoop
    BCS HealFamily_Exit

    LDA #$01
    STA tmp
    JSR CureOBAilment
    BCS @CantUse

    LDA ch_maxhp, X         ; refill their HP to max, instead of just giving them 1 HP
    STA ch_curhp, X
    LDA ch_maxhp+1, X
    STA ch_curhp+1, X

    LDX mp_required
    DEC ch_magicdata, X

    JSR DrawItemTargetMenu
    JSR MenuWaitForBtn_SFX
    JMP EnterMagicMenu

  @CantUse:
    JSR PlaySFX_Error
    JMP @Loop

UseMagic_PURE:
    JSR DrawItemTargetMenu  ; Exactly the same as LIFE, except...
    LDA #$2D
    JSR DrawItemDescBox     ; different description text
  UseMagic_PURE_Loop:
    JSR ItemTargetMenuLoop
    BCS UseMagic_PURE_Exit

    LDA #$03                   ; cure "poison" ailment instead of "death"
    STA tmp
    JSR CureOBAilment
    BCS UseMagic_PURE_CantUse

    LDX mp_required            ; do not recover any HP
    DEC ch_magicdata, X
    JSR DrawItemTargetMenu
    JSR MenuWaitForBtn_SFX

 UseMagic_PURE_Exit:
    JMP EnterMagicMenu

 UseMagic_PURE_CantUse:
    JSR PlaySFX_Error
    JMP UseMagic_PURE_Loop

UseMagic_SOFT:
    JSR DrawItemTargetMenu     ; again... more of the same
    LDA #$30
    JSR DrawItemDescBox        ; but different description text
  @Loop:
    JSR ItemTargetMenuLoop
    BCS UseMagic_PURE_Exit
    LDA #$02                   ; and cure stone instead of death or poison
    STA tmp
    JSR CureOBAilment
    BCS @CantUse
    LDX mp_required
    DEC ch_magicdata, X
    JSR DrawItemTargetMenu
    JSR MenuWaitForBtn_SFX
    JMP EnterMagicMenu
  @CantUse:
    JSR PlaySFX_Error
    JMP @Loop


;;;;;;;;;;;;;;;;;;;;;;;

UseMagic_WARP:
    LDA #$2F
    JSR DrawItemDescBox       ; draw description text
    LDX mp_required
    DEC ch_magicdata, X       ; decrement MP
    JSR MenuWaitForBtn_SFX    ; wait for a button press

    TSX                  ; get the stack pointer
    TXA                  ; and put it in A
    CMP #$FF - 16        ; check the stack pointer to see if at least 16 bytes have been pushed
    BCS UseMagic_DoEXIT  ;  if not, WARP would have the same effect as EXIT, so just jump to EXIT code

    CLC                  ; otherwise, we're to go back one floor
    ADC #6               ;   so add 6 to the stack pointer (kills the last 3 JSRs)
    TAX                  ;   which would be:  JSR to Magic Menu
    TXS                  ;                    JSR to Main Menu
                         ;                and JSR to Standard Map loop
    LDA #0               ; turn off PPU and APU
    STA $2001
    STA $4015

    RTS                  ; and RTS.  See notes below

UseMagic_EXIT:
    LDA #$31
    JSR DrawItemDescBox       ; draw description text
    LDX mp_required
    DEC ch_magicdata, X       ; decrement MP
    JSR MenuWaitForBtn_SFX    ; wait for button press

  UseMagic_DoEXIT:
    JMP DoOverworld           ; then restart logic on overworld by JMPing to DoOverworld

;  Notes regarding WARP/EXIT:
;
;    The Overword loop wipes the stack clean, so it is effectively the "top" of all game
;  logic execution.  When you JMP to DoOverworld (as EXIT does), the end result is that
;  all warp chain data (which exists on the stack) is cleared, and you find yourself
;  back on the overworld map, at the same coords you were when you left.
;
;    WARP is a bit trickier.  When you teleport between maps, each non-warp and non-exit
;  teleport pushes 5 bytes of data to the stack (coords, map, etc).  This data is used
;  by the game to restore you to the map and coords you were previously at if you cast WARP
;  or step on a warp-style teleport tile.  In addition to this 5 bytes of data, the game recursively
;  JSRs to the Standard Map loop, resulting in a 7 byte stack increase.  (see @NormalTeleport
;  local label inside of StandardMapLoop)
;
;    To perform a WARP, all the game has to do is escape the most recent JSR to the standard map code.
;  once this is accomplished, the returning code in StandardMapLoop will pull the old position
;  off the stack and all that good stuff, just as if the player had stepped on a warp-style
;  teleport.
;
;    To escape mentioned JSR, the WARP code above simply adds 6 to the stack pointer, which
;  has the same effect as 6 PLAs (drops the last 6 bytes on the stack).  This escapes the last 3
;  JSRs, which escapes all the menus and exits the most recent call to DoStandardMap, resulting in
;  the WARP.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  UseMagic_GetRequiredMP  [$B102 :: 0x3B112]
;;
;;     Calculate which level of MP the selected spell uses, and
;;  determines whether or not the character has MP on that level
;;
;;  OUT:  mp_required = index to level's MP (from ch_magicdata -- not ch_curmp like you might expect)
;;                  C = set if character has MP.  Cleared if they don't
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

UseMagic_GetRequiredMP:
    LDA submenu_targ      ; get character's ID
    ROR A
    ROR A
    ROR A
    AND #$C0              ; shift to get usable char index
    STA tmp               ; and put in tmp

    LDA cursor            ; get the cursor
    LSR A                 ; /4 (get row -- ie:  the magic level)
    LSR A
    CLC
    ADC tmp                      ; add char index
    ADC #ch_curmp - ch_magicdata ; and add MP offset to it (seems silly to do it this way....)
    TAX                          ; put it in X
    STA mp_required              ; and put it in our MP required var
    LDA ch_magicdata, X          ; get the relevent cur MP
    CMP #1                       ; set C if nonzero (have MP), clear C if zero (don't)
    RTS                          ; and exit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Enter Item Menu  [$B11D :: 0x3B12D]
;;
;;    Pretty self explanitory.  Enters the item menu, returns when player
;;  exits item menu
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


EnterItemMenu:
    LDA #0
    STA $2001           ; turn the PPU off
    STA menustall       ; zero menustall (don't want to stall for drawing the screen for the first time)
    STA descboxopen     ; indicate that the descbox is closed
    JSR ClearNT         ; wipe the NT clean
                        ;  then start drawing the item menu

;; ResumeItemMenu is jumped to to refresh the item box (like after you use a key item and it disappears)
ResumeItemMenu:
    JSR DrawItemBox        ; Draw the item box
    PHP                    ; C will be set if there was no inventory -- push it to stack for use later

    LDA #$03
    JSR DrawItemTitleBox           ; draw the "ITEM" title box
    JSR TurnMenuScreenOn_ClearOAM  ; clear OAM and turn the screen on

    PLP                    ; pull the previously pushed C (C set if no inventory)
    BCC :+                 ; if the player has no inventory...
      LDA #$04
      JSR DrawItemDescBox     ; draw the "You have nothing" description text
      JMP MenuWaitForBtn_SFX  ; then just wait for A or B to be pressed -- then exit

    ; otherwise (player has at least 1 item in inventory)
:   LDA #0
    STA cursor         ; clear the current cursor position       
    STA joy            ; clear joy data
    STA joy_prevdir    ; and previous joy directionals

ItemMenu_Loop:
    JSR ClearOAM            ; clear OAM
    JSR DrawItemMenuCursor  ; draw the cursor where it needs to be
    JSR MenuFrame           ; do a frame

    LDA joy_a               ; see if A has been pressed
    BNE @APressed           ; if it has... jump ahead
    CMP joy_b               ; otherwise check for B
    BNE @Exit               ; and exit if B pressed
    JSR MoveItemMenuCurs    ; neither button pressed... so move cursor if a direction was pressed
    JMP ItemMenu_Loop       ; then continue the loop

  @Exit:
    RTS

  @APressed:
    JSR PlaySFX_MenuSel        ; play the menu selection sound effect
    LDX cursor                 ; put the cursor in X
    LDA item_box, X            ; get the selected item, and put it in A
    ASL A                      ; double it (2 bytes per pointer)
    TAX                        ;   and stick it in X
    LDA @ItemJumpTable, X      ; load the address to jump to from our jump table
    STA tmp                    ; and put it in (tmp)
    LDA @ItemJumpTable+1, X
    STA tmp+1
    JMP (tmp)                  ; then jump to it!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  [$B177 :: 0x3B187]
;;
;;  Each item has its own entry in this jump table.  When that item is selected,
;;    its routine gets jumped to.
;;
;;  Note this is still sort of part of ItemMenu_Loop... kinda
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 @ItemJumpTable:
  .WORD UseItem_Bad, UseItem_Lute, UseItem_Crown, UseItem_Crystal, UseItem_Herb, UseItem_Key, UseItem_TNT, UseItem_Adamant, UseItem_Slab, UseItem_Ruby
  .WORD UseItem_Rod, UseItem_Floater, UseItem_Chime, UseItem_Tail, UseItem_Cube, UseItem_Bottle, UseItem_Oxyale, UseItem_Canoe, UseItem_Bad, UseItem_Bad
  .WORD UseItem_Bad, UseItem_Bad, UseItem_Tent, UseItem_Cabin, UseItem_House, UseItem_Heal, UseItem_Pure, UseItem_Soft, UseItem_Bad, UseItem_Bad


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  [$B1B3 :: 0x3B1C3]
;;
;;  Following the item jump table is all the routines jumped to!  The first few are simplistic
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ; called for invalid item IDs (should never be called -- just sort of a safety catch)
UseItem_Bad:
  JMP ItemMenu_Loop   ; just jump back to the item loop


    ; called when the CROWN is selected
UseItem_Crown:
    LDA #$07      ; select description text "The stolen CROWN"
                  ; seamlessly flow into UseItem_SetDesc

    ; Jumped to by items that just print a simple description
UseItem_SetDesc:
    JSR DrawItemDescBox    ; draw the description box with given description (in A)
    JMP ItemMenu_Loop      ;  then return to the item loop

    ; these various items just print simple description.
UseItem_Crystal:  LDABRA $08, UseItem_SetDesc
UseItem_Herb:     LDABRA $09, UseItem_SetDesc
UseItem_Key:      LDABRA $0A, UseItem_SetDesc
UseItem_TNT:      LDABRA $0B, UseItem_SetDesc
UseItem_Adamant:  LDABRA $0C, UseItem_SetDesc
UseItem_Slab:     LDABRA $0D, UseItem_SetDesc
UseItem_Ruby:     LDABRA $0E, UseItem_SetDesc
UseItem_Chime:    LDABRA $13, UseItem_SetDesc
UseItem_Tail:     LDABRA $14, UseItem_SetDesc
UseItem_Cube:     LDABRA $15, UseItem_SetDesc
UseItem_Oxyale:   LDABRA $18, UseItem_SetDesc
UseItem_Canoe:    LDABRA $19, UseItem_SetDesc


;;;;;;;;;;;;;;;;;;;;;
;;
;;  UseItem_Bottle  [$B1EE :: 0x3B1FE]
;;
;;;;;;;;;;;;;;;;;;;;;

UseItem_Bottle:
    LDA game_flags + OBJID_FAIRY    ; check the fairy object's state (to see if bottle has been opened already)
    LSR A                           ; move flag into C
    BCC @OpenBottle                 ; if flag is clear... fairy isn't visible, so bottle hasn't been opened yet.  Otherwise...
      LDA #$17                      ; Draw "It is empty" description text
      JSR DrawItemDescBox
      JMP ItemMenu_Loop             ;  and return to the item loop

@OpenBottle:                        ; if the bottle hasn't been opened yet
    LDA #0
    STA item_bottle                 ; remove the bottle from inventory
    LDY #OBJID_FAIRY
    JSR ShowMapObject               ; mark the fairy object as visible
    LDA #$16                        ; Draw "Pop... a fiary pops out" etc description text
    JSR DrawItemDescBox_Fanfare     ;   with fanfare!
    JMP ResumeItemMenu              ; Then RESUME item menu (redraw the item list -- now that the bottle isn't there)

;;;;;;;;;;;;;;;;;;;;;
;;
;;  UseItem_Rod  [$B20E :: 0x3B21E]
;;
;;;;;;;;;;;;;;;;;;;;;

UseItem_Rod:
    LDA mapflags        ; see if we're on the overworld
    LSR A               ;  put SM flag in C (C will be clear if on overworld)
    BCC @CantUse        ;  if on overworld, can't use the Rod here

    LDA tileprop_now          ; get the properties of the tile we're stepping on
    AND #TP_SPEC_MASK         ; mask out the 'special' bits
    CMP #TP_SPEC_USEROD       ; see if the special bits mark this tile as a "use rod" tile
    BNE @CantUse              ; if not... can't use the rod here

    LDY #OBJID_RODPLATE       ; check the rod plate object, to see if
    LDA game_flags, Y         ;   the rod has been used yet
    LSR A                     ;   shift that flag into C
    BCC @CantUse              ;   if clear, plate is gone, so Rod has already been used.. can't use the Rod again

    JSR HideMapObject           ; otherwise.. first time rod is being used.  Hide the rod plate
    LDA #$0F                    ;  load up the relevent description text
    JSR DrawItemDescBox_Fanfare ;  and draw it with fanfare!
    JMP ItemMenu_Loop           ; then return to item loop

  @CantUse:
    LDA #$10                  ; if you can't use the Rod here, just load up
    JSR DrawItemDescBox       ;   the generic description text
    JMP ItemMenu_Loop         ; and return to the item loop


;;;;;;;;;;;;;;;;;;;;
;;
;;  UseItem_Lute  [$B236 :: 0x3B246]
;;
;;;;;;;;;;;;;;;;;;;;

UseItem_Lute:
    LDA mapflags        ; see if we're on the overworld
    LSR A               ;  move SM flag into C
    BCC @CantUse        ;  if SM flag clear (on overworld), can't use the Lute here

    LDA tileprop_now            ; get the special properties  of the tile we're stepping on
    AND #TP_SPEC_MASK           ;  mask out 'special' bits
    CMP #TP_SPEC_USELUTE        ;  see if this tile is marked as "use lute"
    BNE @CantUse                ; if not... can't use

    LDY #OBJID_LUTEPLATE        ; check the lute plate object, to see if
    LDA game_flags, Y           ;   the lute has been used yet
    LSR A                       ;   shift the flag into C
    BCC @CantUse                ;   if clear, lute plate is gone, so lute was already used.  Can't use it again

    ASL A                       ; completely pointless shift (undoes above LSR, but has no real effect)
    JSR HideMapObject           ; hide the lute plate object
    LDA #$05                    ; get relevent description text
    JSR DrawItemDescBox_Fanfare ;  and draw it ... WITH FANFARE!
    JMP ItemMenu_Loop           ; then return to item loop

  @CantUse:
    LDA #$06                    ; if you can't use the lute here, just
    JSR DrawItemDescBox         ;  load up generic description text
    JMP ItemMenu_Loop           ; and return to item loop


;;;;;;;;;;;;;;;;;;;;
;;
;;  UseItem_Floater  [$B25F :: 0x3B26F]
;;
;;;;;;;;;;;;;;;;;;;;

UseItem_Floater:
    LDA mapflags         ; see if we're on the overworld
    LSR A                ;  move SM flag into C
    BCS @CantUse         ;  if SM flag set (in standard map -- not overworld), can't use floater here

    LDA tileprop_now        ; get the special properties of the tile you're standing on
    AND #OWTP_SPEC_MASK     ;  mask out the 'special' bits
    CMP #OWTP_SPEC_FLOATER  ;  is tile marked to allow floater?
    BNE @CantUse            ; if not, can't use it here

    LDA airship_vis         ; check current airship visibility state
    BNE @CantUse            ;  if it's nonzero, airship is already raised.  Can't raise it again

    INC airship_vis             ; otherwise... increment airship visibility (= $01)
    LDA #$11                    ; load up the "omg you raised the airship" description text
    JSR DrawItemDescBox_Fanfare ;   and draw it with fanfare
    JMP ItemMenu_Loop           ; then return to item loop

  @CantUse:
    LDA #$12
    JSR DrawItemDescBox     ; can't use... so just draw lame description text
    JMP ItemMenu_Loop       ;  and return to loop


;;;;;;;;;;;;;;;;;;;;
;;
;;  UseItem_Tent  [$B284 :: 0x3B294]
;;
;;;;;;;;;;;;;;;;;;;;

UseItem_Tent:
    LDA mapflags            ; ensure we're on the overworld
    LSR A                   ;  shift SM flag into C
    BCS @CantUse            ;  if set (in standard map), can't use tent here

    DEC item_tent           ; otherwise... remove 1 tent from the inventory
    LDA #30
    JSR MenuRecoverPartyHP  ; give 30 HP to the whole party
    LDA #$1A
    JSR MenuSaveConfirm     ; and bring up confirm save screen (with description text $1A)
    JMP EnterItemMenu       ; then re-enter item menu (need to re-enter, because screen needs full redrawing)

  @CantUse:
    LDA #$1B                ; if we can't use, just print description text
    JSR DrawItemDescBox
    JMP ItemMenu_Loop       ; and return to loop

;;;;;;;;;;;;;;;;;;;;
;;
;;  UseItem_Cabin  [$B2A1 :: 0x3B2B1]
;;
;;;;;;;;;;;;;;;;;;;;

UseItem_Cabin:
    LDA mapflags            ; exactly the same as tents... except....
    LSR A
    BCS @CantUse
    DEC item_cabin          ; remove cabins from inventory instead of tents
    LDA #60                 ;  recover 60 HP instead of 30
    JSR MenuRecoverPartyHP
    LDA #$1C                ; and use different description strings
    JSR MenuSaveConfirm
    JMP EnterItemMenu
  @CantUse:
    LDA #$1D                ; another different description string
    JSR DrawItemDescBox
    JMP ItemMenu_Loop

;;;;;;;;;;;;;;;;;;;;
;;
;;  UseItem_House  [$B2BE :: 0x3B2CE]
;;
;;;;;;;;;;;;;;;;;;;;

UseItem_House:
    LDA mapflags            ; make sure we're on the overworld
    LSR A                   ;  Get SM flag, and shift it into C
    BCS @CantUse            ;  if set (not on overworld), can't use house here

    DEC item_house          ; otherwise... remove a house from our inventory
    LDA #120
    JSR MenuRecoverPartyHP  ; give the whole party 120 HP
    JSR MenuRecoverPartyMP  ; recover MP
    LDA #$1E
    JSR MenuSaveConfirm     ; bring up the save confirmation screen.  (description text $1E)
:   JMP EnterItemMenu       ; then, whether they saved or not, re-enter item menu

  @CantUse:
    LDA #$1F
    JSR DrawItemDescBox     ; if you can't use the house... just print description text ($1F)
    JMP ItemMenu_Loop       ; and return to loop


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Menu Save Confirmation  [$B2E0 :: 0x3B2F0]
;;
;;    Draws the item description box with given string ID (in A)
;;  then waits for the user to press A or B.  If A is pressed, the game is
;;  saved.
;;
;;  IN:   A = ID of description string to draw
;;
;;  OUT:  C = set if game was saved
;;            clear if game was not saved
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MenuSaveConfirm:
    JSR DrawItemDescBox       ; draw the description box
    JSR ClearOAM              ; clear OAM
    JSR MenuWaitForBtn        ; then wait for player to press A or B

    LDA joy                   ; see if they pressed A or B
    AND #$80                  ;  check the 'A' bit
    BNE :+                    ; if they didn't press A...
      JSR CloseDescBox         ;  close the description box
      CLC                      ;  CLC to indicate they did not save
      RTS                      ;  and exit

:   LDA #$3F                  ; draw description box with text ID $3F
    JSR DrawItemDescBox       ; "your gave is being saved" or whatever
    JSR SaveGame              ; save the game
    JSR MenuWaitForBtn_SFX    ; then wait for them to press A or B again
    RTS                       ;  and exiting


;;;;;;;;;;;;;;;;;;;;
;;
;;  UseItem_Heal  [$B301 :: 0x3B311]
;;
;;    Back to UseItem routines
;;
;;;;;;;;;;;;;;;;;;;;

  ;; can't make these labels local because UseItem_Pure hijacks one of the labels ;_;

UseItem_Heal:
    JSR DrawItemTargetMenu     ; Draw the item target menu (need to know who to use this heal potion on)
    LDA #$20
    JSR DrawItemDescBox        ; open up the description box with text ID $20

  _UseItem_Heal_Loop:
    JSR ItemTargetMenuLoop     ; run the item target loop.
    BCS UseItem_Exit           ; if B was pressed (C set), exit this menu

    LDA cursor                 ; otherwise... A was pressed.
    ROR A                      ;  get the cursor (target character)
    ROR A                      ;  left shift by 6 (make char index:  $40, $80, $C0)
    ROR A
    AND #$C0                   ; mask out relevent bits
    TAX                        ; and put in X

    LDA ch_ailments, X         ; check their OB ailments
    CMP #$01
    BEQ _UseItem_Heal_CantUse  ; if dead... can't use
    CMP #$02
    BEQ _UseItem_Heal_CantUse  ; if stone... can't use

    LDA #30                    ; otherwise.. can use!
    JSR MenuRecoverHP_Abs      ;   recover 30 HP for target (index is still in X).  Can use _Abs version
    JSR DrawItemTargetMenu     ;   because we already checked the ailments
    JSR MenuWaitForBtn_SFX     ; then redraw the menu to reflect the HP change, and wait for the user to press a button

    DEC item_heal              ; then remove a heal potion from the inventory

UseItem_Exit:
    JMP EnterItemMenu          ; re-enter item menu (item menu needs to be redrawn)

  _UseItem_Heal_CantUse:       ; can't make this local because of stupid UseItem_Pure hijacking the above label
    JSR PlaySFX_Error          ; play the error sound effect
    JMP _UseItem_Heal_Loop     ; and keep looping until they select a legal target or escape with B

;;;;;;;;;;;;;;;;;;;;
;;
;;  UseItem_Pure  [$B338 :: 0x3B348]
;;
;;;;;;;;;;;;;;;;;;;;

UseItem_Pure:
    JSR DrawItemTargetMenu     ; draw target menu
    LDA #$21
    JSR DrawItemDescBox        ; print relevent description text (ID=$21)
  @Loop:
    JSR ItemTargetMenuLoop     ; do the target menu loop
    BCS UseItem_Exit           ; if they pressed B (C set), exit

    LDA #$03                   ; otherwise, put "poison" OB ailment
    STA tmp                    ;   in tmp as our ailment to cure
    JSR CureOBAilment          ; then try to cure it
    BCS @CantUse               ; if we couldn't... can't use this item

    DEC item_pure              ; if we could... remove one from the inventory
    JSR DrawItemTargetMenu     ; redraw the target menu to reflect the changes
    JSR MenuWaitForBtn_SFX     ; then wait for the player to press a button
    JMP EnterItemMenu          ; before re-entering the item menu (redrawing item menu)

  @CantUse:
    JSR PlaySFX_Error          ; if can't use... give the error sound effect
    JMP @Loop                  ;  and keep looping

;;;;;;;;;;;;;;;;;;;;
;;
;;  UseItem_Soft  [$B360 :: 0x3B370]
;;
;;;;;;;;;;;;;;;;;;;;

UseItem_Soft:
    JSR DrawItemTargetMenu     ; this is all EXACTLY the same as UseItem_Pure.  Except...
    LDA #$22
    JSR DrawItemDescBox        ; different description text ID
  @Loop:
    JSR ItemTargetMenuLoop
    BCS UseItem_Exit

    LDA #$02                   ; cure "stone" ailment
    STA tmp
    JSR CureOBAilment
    BCS @CantUse

    DEC item_soft              ; remove soft from inventory
    JSR DrawItemTargetMenu
    JSR MenuWaitForBtn_SFX
    JMP EnterItemMenu

  @CantUse:
    JSR PlaySFX_Error
    JMP @Loop


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Conditially cure OB Ailment  [$B388 :: 0x3B398]
;;
;;    Checks a character's OB ailment.  See if it matches the given
;;  ailment.  If it does, it cures the ailment, otherwise it doesn't
;;
;;  IN:  cursor = ID (0,1,2,3) of the character to check
;;          tmp = ailment to cure
;;
;;  OUT:      X = char index to target
;;            C = set on failure (ailment not cured)
;;                clear on success (ailment cured)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CureOBAilment:
    LDA cursor            ; get cursor (desired character)
    ROR A                 ; shift to get usable character index ($00,$40,$80,$C0)
    ROR A
    ROR A
    AND #$C0              ; and mask relevant bits
    TAX                   ; then stuff in X
    LDA ch_ailments, X    ; get OB ailments
    CMP tmp               ; compare to given ailment
    BEQ @Success          ; if they match.. success!
    SEC                   ;  otherwise... failure.  SEC to indicate failure
    RTS                   ;   and exit

@Success:
    LDA #$00              ; clear the character's OB Ailment (curing them)
    STA ch_ailments, X
    CLC                   ; CLC to indicate success
    RTS                   ; and exit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ItemTargetMenuLoop    [$B3A0 :: 0x3B3B0]
;;
;;     Runs the Item Target Menu loop.  Let's the player move the cursor between
;;  the 4 characters to select a target, and looks for A and B button presses.
;;
;;  OUT:  C = cleared if A pressed
;;            set if B pressed
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ItemTargetMenuLoop:
    LDA #0
    STA cursor      ; reset the cursor to zero
  @Loop:
    LDA #0
    STA joy_a       ; clear joy_a and joy_b so that a button press
    STA joy_b       ;  will be recognized

    JSR ClearOAM               ; clear OAM
    JSR DrawItemTargetCursor   ; draw the cursor for this menu
    JSR MenuFrame              ; do a frame

    LDA joy_a
    BNE @A_Pressed     ; check to see if they pressed A
    LDA joy_b
    BNE @B_Pressed     ; or B

    LDA joy            ; get joy data
    AND #$03           ;  isolate left/right buttons
    CMP joy_prevdir    ; compare it to the prev pressed buttons (to see if buttons are pressed or held)
    BEQ @Loop          ; if they match... no new button presses.  Keep looping.

    STA joy_prevdir    ; otherwise... record the change
    CMP #0             ; see if this was a release
    BEQ @Loop          ; if it was, no button press... keep looping

    CMP #$01           ; otherwise.. they just pressed left or right...
    BNE @Left          ;  see which they pressed

  @Right:
    LDA cursor         ; get cursor
    CLC                ;  and add 1 (move it to the right)
    ADC #$01
    JMP @MoveCurs      ; skip over the @Left block

  @Left:
    LDA cursor         ; get cursor
    SEC                ;  and subtract 1 (move it to the left)
    SBC #$01

  @MoveCurs:
    AND #$03               ; whether we moved left or right, AND with 3 to effectively wrap the cursor
    STA cursor             ;  and keep it in bounds.  Then write it back to the 'cursor' var
    JSR PlaySFX_MenuMove   ; Play the "move" sound effect
    JMP @Loop              ; and continue looping

  @A_Pressed:              ; if A was pressed
    JSR PlaySFX_MenuSel    ;  play the selection sound effect
    CLC                    ;  clear carry to indicate A pressed
    RTS                    ;  and exit

  @B_Pressed:              ; if B pressed
    JSR PlaySFX_MenuSel    ;  play selection sound effect
    SEC                    ;  and set carry before exiting
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Item Target Cursor  [$B3EE :: 0x3B3FE]
;;
;;     Draws the cursor for the item target menu
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


DrawItemTargetCursor:
    LDX cursor           ; put the cursor in X
    LDA @lut, X          ; use it to index our LUT
    STA spr_x            ; that lut is the X coord for cursor
    LDA #$68
    STA spr_y            ; Y coord is always $68
    JMP DrawCursor       ; draw it, and exit

  @lut:
    .BYTE $10,$48,$80,$B8


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Item Target Menu  [$B400 :: 0x3B410]
;;
;;    Draws the item target menu (the menu that pops up when you select a heal potion
;;  and that kind of thing -- where you select who you want to use it on)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawItemTargetMenu:
    LDA #0
    STA $2001            ; turn the PPU off
    STA menustall        ; and disable menu stalling
    JSR ClearNT          ; wipe the NT clean

    LDA #$0B             ; hardcoded box
    STA box_y            ; x,y   = $01,$0B
    LDA #$01             ; wd,ht = $1E,$08
    STA box_x
    LDA #$1E
    STA box_wd
    LDA #$08
    STA box_ht
    JSR DrawBox          ; draw it

    JSR @DrawBoxBody                ; draw the box body
    JMP TurnMenuScreenOn_ClearOAM   ; then clear OAM and turn the screen back on.  then exit

    RTS     ; useless RTS -- impossible to reach


  ;;  this isn't really a local routine -- but it's only called by the above routine,
  ;;    so I figured I might as well make it local

 @DrawBoxBody:
    LDX #0*2         ; X is ID of string to draw (*2).  Start with string 0 (max hp)
    LDA #$04
    STA dest_x       ; draw this string at $04,$11
    LDA #$11
    STA dest_y
    JSR @DrawString  ; draw it

    DEC dest_y       ; dec Y coord ($04,$10)
    LDX #1*2         ; draw string 1 (cur hp)
    JSR @DrawString

    DEC dest_y       ; dec Y coord again ($04,$0F)
    LDX #2*2         ; draw string 2 (ailment blurb)
    JSR @DrawString

    DEC dest_y       ; dec Y coord by 2  ($04,$0D)
    DEC dest_y
    LDX #3*2         ; draw string 3, then exit

 @DrawString:
    LDA @lut_str_pointertable, X      ; load up the pointer from our pointer table
    STA text_ptr                      ; put it in text_ptr
    LDA @lut_str_pointertable+1, X
    STA text_ptr+1
    JMP DrawMenuComplexString        ; then draw it as a local complex string, and exit


 @lut_str_pointertable:
  .WORD @str_charmaxhp, @str_charcurhp, @str_ailmentblurb, @str_name

  ; These strings all use stat control codes ($10-$13).  See DrawComplexString
  ;  description for details

 @str_charmaxhp:                    ; "/999   /999   /999   /999"
  .BYTE $7A,$10,$06,$FF,$FF,$FF     ; "/999   "  <- character 0's max HP
  .BYTE $7A,$11,$06,$FF,$FF,$FF     ; same, but char 1's
  .BYTE $7A,$12,$06,$FF,$FF,$FF     ; char 2's
  .BYTE $7A,$13,$06,$00             ; char 3's, then null terminator

 @str_charcurhp:                    ; " 999    999    999    999"
  .BYTE $FF,$10,$05,$FF,$FF,$FF     ; " 999   "  <- character 0's cur HP
  .BYTE $FF,$11,$05,$FF,$FF,$FF     ; same, but char 1's
  .BYTE $FF,$12,$05,$FF,$FF,$FF     ; char 2's
  .BYTE $FF,$13,$05,$00             ; char 3's, then null terminator

 @str_ailmentblurb:
  .BYTE $10,$02,$FF,$FF,$FF,$FF     ; character 0's OB Ailment blurb  ("HP" when healthy)
  .BYTE $11,$02,$FF,$FF,$FF,$FF
  .BYTE $12,$02,$FF,$FF,$FF,$FF
  .BYTE $13,$02,$00

 @str_name:
  .BYTE $10,$00,$FF,$FF,$FF         ; character 0's name, followed by 3 spaces
  .BYTE $11,$00,$FF,$FF,$FF
  .BYTE $12,$00,$FF,$FF,$FF
  .BYTE $13,$00,$00



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  EnterStatusMenu    [$B4AD :: 0x3B4BD]
;;
;;    Just draws the status screen, then waits for the user to press a button
;;  before exiting
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


EnterStatusMenu:
    LDA #0
    STA $2001               ; turn off the PPU
    LDA #0
    STA menustall           ; disable menu stalling
    JSR ClearNT             ; clear the NT

    LDX #0*4                ; draw status box 0
    JSR @DrawStatusBox
    LDA #$23                ; and its contents (menu text ID $23 = character name)
    JSR DrawCharMenuString

    LDX #1*4                ; then status box 1
    JSR @DrawStatusBox
    LDA #$24                ; and its contents
    JSR DrawCharMenuString

    LDX #2*4                ; and so on
    JSR @DrawStatusBox
    LDA #$25
    JSR DrawCharMenuString

    LDX #3*4                ; and so on
    JSR @DrawStatusBox
    LDA #$26
    JSR DrawCharMenuString

    LDX #4*4
    JSR @DrawStatusBox
    LDA #$27
    JSR DrawCharMenuString

    LDX #5*4
    JSR @DrawStatusBox
    LDA #$28
    JSR DrawCharMenuString  ; 6th and final box drawn

    JSR ClearOAM            ; clear OAM

    LDA #$58                ; set sprite coords to $58,$20
    STA spr_x
    LDA #$20
    STA spr_y

    LDA submenu_targ        ; get target character ID
    ROR A
    ROR A
    ROR A
    AND #$C0                ; shift to make ID a usable index
    JSR DrawOBSprite        ; then draw this character's OB sprite

    JSR TurnMenuScreenOn    ; turn the screen on
    JMP MenuWaitForBtn_SFX  ; then just wait for the user to press a button before exiting



 @DrawStatusBox:                 ; draws a status screen box.  box index*4 in X
    LDA @lutStatusBoxes, X       ; load up coords and dims from below LUT
    STA box_x
    LDA @lutStatusBoxes+1, X
    STA box_y
    LDA @lutStatusBoxes+2, X
    STA box_wd
    LDA @lutStatusBoxes+3, X
    STA box_ht
    JMP DrawBox                  ; then draw the box and return

 @lutStatusBoxes:             ; coords and dims for status menu boxes
  .BYTE $01,$03,$08,$05       ; box containing character name                (top left)
  .BYTE $09,$03,$0E,$05       ; box containing battle sprite and class name  (top center)
  .BYTE $17,$03,$08,$05       ; box containing level                         (top right)
  .BYTE $04,$08,$17,$07       ; box containing exp                           (center)
  .BYTE $01,$0F,$0E,$0D       ; box containing base stats (str,int,etc)      (bottom left)
  .BYTE $0F,$0F,$10,$0D       ; box containing sub stats (dmg,hit,absorb,etc)(bottom right)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  MenuRecoverPartyHP    [$B53F :: 0x3B54F]
;;
;;    Recovers HP for the whole party, AND draws the item target menu
;;
;;  IN:  A = HP to recover
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MenuRecoverPartyHP:
    LDX #$00
    JSR MenuRecoverHP        ; recover HP for each character
    LDX #$40
    JSR MenuRecoverHP
    LDX #$80
    JSR MenuRecoverHP
    LDX #$C0
    JSR MenuRecoverHP
    JMP DrawItemTargetMenu   ; then draw item target menu, and exit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Menu Recover HP   [$B556 :: 0x3B566]
;;
;;   Recovers HP for a party member.  For use in menus (out of battle) only.
;;
;;  IN:   X = char index to receive HP ($00, $40, $80, or $C0)
;;        A = ammount of HP to recover (1 byte only -- means max is 255)
;;
;;  OUT:  neither A nor X are changed by these routines
;;
;;   Two flavors.  MenuRecoverHP does nothing if the player is dead/stone
;;  MenuRecoverHP_Abs recovers HP even if dead/stone (does not check)
;;  Also.. when HP is recovered, the "recover hp" jingle is played.
;;
;;   Too hard to use local labels here because of the two entry points branching around
;;  each other... so had to use global labels.  Hence the long label names.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MenuRecoverHP:
    LDY ch_ailments, X          ; get out of battle ailments for this character
    CPY #$01
    BEQ _MenuRecoverHP_Exit     ; if dead... skip to exit (can't recover HP when dead)
    CPY #$02
    BEQ _MenuRecoverHP_Exit     ; if stone... skip to exit

MenuRecoverHP_Abs:
    STA tmp                     ; back up HP to recover by stuffing it in tmp
    CLC
    ADC ch_curhp, X             ; add recover HP to low byte of HP
    STA ch_curhp, X
    LDA ch_curhp+1, X           ; add 0 to high byte of HP (to catch carry from low byte)
    ADC #0
    STA ch_curhp+1, X
    CMP ch_maxhp+1, X           ; then compare against max HP to make sure we didn't go over
    BEQ _MenuRecoverHP_CheckLow ; if high byte of cur = high byte of max, we need to check the low byte
    BCS _MenuRecoverHP_OverMax  ; if high byte of cur > high byte of max... we went over
                                ; otherwise.. we're done...

  _MenuRecoverHP_Done:
    LDA #$57
    STA music_track             ; play music track $57 (the little gain HP jingle)

    LDA #%00110000              ; set vol for sq2 to zero
    STA $4004
    LDA #%01111111              ; disable sweep
    STA $4005
    LDA #0                      ; and clear freq
    STA $4006                   ;  best I can figure... shutting off sq2 here prevents some ugly sounds
    STA $4007                   ;  from happening when the gain HP jingle starts.  Though I don't see why
                                ;  that should happen...

    LDA tmp                     ; restore the HP recovery ammount in A, before exiting
  _MenuRecoverHP_Exit:
    RTS


  _MenuRecoverHP_CheckLow:
    LDA ch_curhp, X             ; check low byte of HP against low byte of max
    CMP ch_maxhp, X
    BCS _MenuRecoverHP_OverMax  ; if cur >= max, we're over the max
    BCC _MenuRecoverHP_Done     ;  otherwise we're not, so we're done (always branches)

  _MenuRecoverHP_OverMax:
    LDA ch_maxhp, X             ; if over max, just replace cur HP with maximum HP.
    STA ch_curhp, X
    LDA ch_maxhp+1, X
    STA ch_curhp+1, X
    JMP _MenuRecoverHP_Done     ; and then jump to done

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Move Item Menu Cursor   [$B5AB :: 0x3B5BB]
;;
;;    Moves the cursor around in the item menu.  Checks all 4 directions
;;  and wraps where appropriate.  The description box is closed after every
;;  move (if it's opened).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


MoveItemMenuCurs_Exit:
    RTS

MoveItemMenuCurs:
    LDA joy                   ; get current joy data
    AND #$0F                  ; mask out the directional bits
    CMP joy_prevdir           ; see if any bits have changed (button has been pressed/released)
    BEQ MoveItemMenuCurs_Exit ; if there was no change, there's nothing to do here, just exit
    STA joy_prevdir           ; write the new value back so that the joy changes are recorded
    CMP #0                    ; see if a button is being pressed or released (since either would cause a transition)
    BEQ MoveItemMenuCurs_Exit ; if 'joy' was zero... then buttons are released.. no buttons being pressed.. so just exit

    CMP #$04            ; check the 'Down' bit
    BCC @LeftOrRight    ;  if it's less than that... it's either Left or Right
    BNE @Up             ;  if it's not equal to that... it must be Up
                        ;  otherwise... it's Down
 @Down:
    LDA cursor         ; to move down... get the cursor
    CLC
    ADC #$03           ; add 3 to it
    CMP cursor_max     ; then if it's less than the max
    BCC @Done          ; we're done
    CMP #$03           ;   otherwise... if it's less than 3
    BCC @Done          ;   we're also done  (I don't see how this could ever happen, though)
                       ; otherwise we need to wrap to top of the column
 @DownWrap:
    SEC                ; wrap to top of the column
    SBC #$03           ;  by repeatedly subtracting 3
    CMP #$03           ;  until the cursor is < 3
    BCS @DownWrap      ; if >= 3, keep subtracting, otherwise...
    BCC @Done          ; we're done (always branches)


 @Up:
    LDA cursor         ; to move up...
    SEC
    SBC #$03           ; subtract 3 from the cursor
    BPL @Done          ; if we're still above zero, we're done.  Otherwise...

    LDA cursor         ; re-load the cursor into A
    LDX cursor_max     ; and put the maximum in X
    CPX #$03           ; if the max is less than 3
    BCC @Done          ;  we're done (do nothing -- cursor can't move)

 @UpWrap:              ; to wrap to bottom of column...
    CLC                ;  just repeatedly add 3
    ADC #$03
    CMP cursor_max
    BCC @UpWrap        ;  until the cursor is greater than the maximum
    SBC #$03           ; at which point, you subtract 3 to get it *just under* the maximum (but keeping it in its column)
    JMP @Done          ; then we're done


 @LeftOrRight:
    CMP #$01           ; check to see if they pressed Right instead of Left
    BNE @Left          ; if they didn't... jump to Left
                       ; otherwise... they must've pressed right
 @Right:
    LDA cursor         ; to move right... just add 1 to the cursor
    CLC
    ADC #$01
    CMP cursor_max     ; if the cursor is < max...
    BCC @Done          ;  then we're done
    LDA #0             ; otherwise wrap the cursor to zero
    BEQ @Done          ;  (always branches)

 @Left:
    LDA cursor         ; to move left... subtract 1 from the cursor
    SEC
    SBC #$01
    BPL @Done          ; still >= 0, we're done
    LDA cursor_max     ; otherwise, wrap to max-1
    SEC
    SBC #$01

 @Done:
    STA cursor            ; write the new cursor value
    JMP CloseDescBox_Sfx  ; close the description box, play the menu move sound effect, and exit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Menu Wait for Btn [$B613 :: 0x3B623]
;;
;;    These routines will simply wait until the user pressed either the A or B buttons, then
;;  will exit.  MenuFrame is called during the wait loop, so the music driver and things stay
;;  up to date.
;;
;;    MenuWaitForBtn_SFX   will play the MenuSel sound effect once A or B is pressed
;;    MenuWaitForBtn       will not play any sound effect
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MenuWaitForBtn_SFX:
    JSR MenuFrame           ; do a frame
    LDA joy_a               ;  check A and B buttons
    ORA joy_b
    BEQ MenuWaitForBtn_SFX  ;  if both are zero, keep looping.  Otherwise...
    LDA #0
    STA joy_a               ; clear both joy_a and joy_b
    STA joy_b
    JMP PlaySFX_MenuSel     ; play the MenuSel sound effect, and exit


MenuWaitForBtn:
    JSR MenuFrame           ; exactly the same -- only no call to PlaySFX_MenuSel at the end
    LDA joy_a
    ORA joy_b
    BEQ MenuWaitForBtn
    LDA #0
    STA joy_a
    STA joy_b
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Main Menu Character Sprites [$B635 :: 0x3B645]
;;
;;    Pretty self explanitory.  Draws the 4 characters sprites as seen
;;  on the main menu
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawMainMenuCharSprites:
    LDA #$88           ; Draw char 0's OB Sprite at $88,$18
    STA spr_x
    LDA #$18
    STA spr_y
    LDA #$00
    JSR DrawOBSprite

    LDA #$D8           ; Draw char 1's OB sprite at $D8,$18
    STA spr_x
    LDA #$40
    JSR DrawOBSprite

    LDA #$88           ; Draw char 3's OB sprite at $D8,$88
    STA spr_y
    LDA #$C0
    JSR DrawOBSprite

    LDA #$88           ; and lastly, char 2's OB sprite at $88,$88
    STA spr_x
    LDA #$80
    JMP DrawOBSprite   ; then exit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Menu Frame  [$B65D :: 0x3B66D]
;;
;;    This does various things that must be done every frame when in the menus.
;;  This involves:
;;    1)  waiting for VBlank
;;    2)  Sprite DMA
;;    3)  scroll resetting
;;    4)  calling MusicPlay
;;    5)  incrementing frame counter
;;    6)  updating joypad
;;
;;    Menu loops will call this routine every iteration before processing the user
;;  input to navigate menus.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MenuFrame:
    JSR WaitForVBlank_L    ; wait for VBlank
    LDA #>oam              ; Do sprite DMA (update the 'real' OAM)
    STA $4014

    LDA soft2000           ; reset scroll and PPU data
    STA $2000
    LDA #0
    STA $2005
    STA $2005

    LDA music_track        ; if no music track is playing...
    BPL :+
      LDA #$51             ;  start music track $51  (menu music)
      STA music_track

:   LDA #BANK_THIS         ; record this bank as the return bank
    STA cur_bank           ; then call the music play routine (keep music playing)
    JSR CallMusicPlay

    INC framecounter       ; increment the frame counter to count this frame

    LDA #0                 ; zero joy_a and joy_b so that an increment will bring to a
    STA joy_a              ;   nonzero state
    STA joy_b
    JMP UpdateJoy          ; update joypad info, then exit


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Move Main Menu Sub Cursor  [$B68C :: 0x3B69C]
;;
;;    Moves the cursor for the main menu sub target.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MoveMainMenuSubCursor:
    LDA joy              ; get joy buttons
    AND #$0F             ; isolate directional arrows
    CMP joy_prevdir      ; compare to prev directions to see if there was a change
    BEQ @Exit            ; no change... exit

    STA joy_prevdir      ; otherwise record change
    CMP #0               ; see if the change was a press or release
    BEQ @Exit            ; if release... exit

    LDX #$01             ; X=1 (for left/right)
    CMP #$04             ;  see if player pressed up or down
    BCC :+
      LDX #$02           ; if they did... X=2 (for up/down)
:   TXA                  ; then move X to A

                         ; A is now 1 for horizontal movement and 2 for vertical movement
    EOR cursor           ; EOR with the cursor (wrap around appropriate axis)
    STA cursor           ; and write back
    JMP PlaySFX_MenuMove ; then play the move sound effect
  @Exit:
    RTS                  ; and exit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Move Cursor Up / Down  [$B6AB :: 0x3B6CB]
;;
;;    Checks joypad data for up/down button presses, and increments/decrements
;;  the cursor appropriately.  The cursor is wrapped at cursor_max.  This also
;;  plays that ugly sound effect if the cursor has been moved
;;
;;    Left/Right button presses are not checked here -- this is for up/down only
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MoveCursorUpDown:
    LDA joy           ; get joypad data
    AND #$0C          ;  isolate up/down buttons
    CMP joy_prevdir   ;  compare it to previously checked button states
    BEQ @Exit         ; if they equal, do nothing (button has already been pressed and is currently just held)

    STA joy_prevdir   ; otherwise, button state has changed, so record new button state in prevdir
    CMP #$00          ;  and check to see if a button is being pressed or released (nonzero=pressed)
    BEQ @Exit         ;  if zero, button is being released, so do nothing and just exit

    CMP #$04          ; see if the user pressed down or up
    BNE @Up

  @Down:              ; moving down...
    LDA cursor        ;  get cursor, and increment by 1
    CLC
    ADC #$01
    CMP cursor_max    ;  if it's < cursor_max, it's still in range, so
    BCC @Move         ;   jump ahead to move
    LDA #0            ;  otherwise, it needs to be wrapped to zero
    BEQ @Move         ;   (always branches)

  @Up:                ; up is the same deal...
    LDA cursor        ;  get cursor, decrement by 1
    SEC
    SBC #$01
    BPL @Move         ; if the result didn't wrap (still positive), jump ahead to move
    LDA cursor_max    ;  otherwise wrap so that it equals cursor_max-1
    SEC
    SBC #$01

  @Move:
    STA cursor            ; set cursor to changed value
    JMP PlaySFX_MenuMove  ; then play that hideous sound effect and exit

  @Exit:
    RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Move Magic Menu Cursor  [$B6DC :: 0x3B6EC]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MoveMagicMenuCursor_Exit:
    RTS

MoveMagicMenuCursor:
    LDA submenu_targ            ; get target character ID
    ROR A
    ROR A
    ROR A
    AND #$C0                    ; shift to get a usable character index
    STA tmp                     ; put it in tmp

    LDA joy                      ; get joypad state
    AND #$0F                     ; isolate directional buttons
    CMP joy_prevdir              ; compare to previous buttons to see if any have been pressed/released
    BEQ MoveMagicMenuCursor_Exit ; if there's no change, just exit
    STA joy_prevdir              ;  otherwise record changes
    CMP #0                       ; see if buttons have been pressed (rather than released)
    BEQ MoveMagicMenuCursor_Exit ; if no buttons pressed, just exit

    CMP #$04               ; now see which button was pressed
    BCS @UpDown            ; check for up/down
    CMP #$01               ; otherwise, check for left/right
    BNE @Left


  @Right:
    LDA cursor            ; get cursor
    CLC
    ADC #$01              ; add 1 to it (move it right)
    AND #$03              ; mask out the low bits (column)
    CMP #$03              ; see if in column 3 (column 3 doesn't exist -- is padding)
    BNE @Right_ColOK      ; if not column 3, this column is okay!  skip ahead

      LDA cursor             ; otherwise (column is bad), get the cursor
      AND #$1C               ; wrap it to start of row
      STA cursor             ; write it back
      JSR @CheckCursor       ; check to make sure slot isn't empty
      BEQ @Right             ; if it is, keep looping until we get to a slot that isn't empty
      JMP CloseDescBox_Sfx   ; otherwise, close the description box and exit

  @Right_ColOK:
    INC cursor             ; if we're not in the last column... just INC the cursor
    JSR @CheckCursor       ; then check to make sure it's not an empty slot
    BEQ @Right             ; if it is, keep looping
    JMP CloseDescBox_Sfx   ; otherwise, close desc box and exit


  @Left:                  ; moving left is just like moving right, just in opposite direction
    LDA cursor            ; get cursor
    SEC
    SBC #$01              ; subtract 1 and mask
    AND #$03
    CMP #$03              ; then see if we're in the padding column
    BNE @Left_ColOK       ; if not, col is OK

      LDA cursor            ; if we are in padding column, get cursor
      AND #$1C              ; mask out the row
      ORA #$02              ; snap to last column in row
      STA cursor            ; write back
      JSR @CheckCursor      ; verify slot isn't empty
      BEQ @Left             ; if it is, keep looping
      JMP CloseDescBox_Sfx  ; otherwise, exit

  @Left_ColOK:
    DEC cursor            ; not in the first column... so we can just dec the cursor
    JSR @CheckCursor      ; verify it
    BEQ @Left             ; loop if empty
    JMP CloseDescBox_Sfx  ; otherwise exit


@UpDown:         ; if we pressed up or down... see which
    BNE @Up


  @Down:                   ; moving up/down is a bit easier than left/right
    LDA cursor             ; get cursor
    CLC
    ADC #$04               ; just add 4 to it (one row)
    AND #$1F               ; mask to keep within 8 rows
    STA cursor             ; and write back
    JSR @CheckCursor       ; verify
    BEQ @Down              ; loop if slot is empty
    JMP CloseDescBox_Sfx   ; then exit once we find a nonempty slot

  @Up:                     ; moving up is exactly the same
    LDA cursor
    SEC
    SBC #$04               ; only we subtract 4 instead of adding
    AND #$1F
    STA cursor
    JSR @CheckCursor
    BEQ @Up
    JMP CloseDescBox_Sfx

  ;;;;;;
  ;;  A little mini local subroutine here that checks to see if the cursor
  ;;    is on a spell or an empty slot.
  ;;
  ;;  Result is store in Z on exit.  Z set = cursor on blank slot
  ;;  Z clear = cursor is on actual spell
  ;;;;;;

  @CheckCursor:
    LDA cursor       ; get the cursor
    ORA tmp          ; add the char index to it
    TAX              ; put in X to index
    LDA $6300, X     ; and fetch the spell at current cursor
    RTS              ; then exit.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Close Description Box  [$B771 :: 0x3B781]
;;
;;    Closes the item description box if it is open, and clears 'descboxopen'
;;  to indicate that the box is closed.
;;
;;    Routine comes in two flavors:
;;
;;  CloseDescBox_Sfx = plays the menu move sound effect, and closes
;;                     the box only if it's open
;;
;;  CloseDescBox     = doesn't play any sound effect, and closes the box
;;                     even if it's already closed (no checking to see if it's open)
;;
;;    Note the difference between these and EraseDescBox.  EraseDescBox simply
;;  does the PPU drawing.  These routines also clear 'descboxopen', which EraseDescBox
;;  does not do.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


CloseDescBox_Sfx:
    JSR PlaySFX_MenuMove     ; play the menu move sound effect
    LDA descboxopen          ; see if the box is currently open
    BNE CloseDescBox         ;  if it is, close it... otherwise
      RTS                    ;    just exit

CloseDescBox:
    LDA #0
    STA descboxopen          ; clear descboxopen to indicate that the box is now closed
    JMP EraseDescBox         ; and erase the box, then exit


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Turn Menu Screen On  [$B780 :: 0x3B790]
;;
;;    This is called to switch on the PPU once all the drawing for the menus is complete
;;
;;   Comes in 2 flavors... normal, and 'ClearOAM' -- both of which are the same, only
;;   the ClearOAM version, as the name implies, clears OAM (clearing all sprites).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


TurnMenuScreenOn_ClearOAM:
    JSR ClearOAM             ; clear OAM
                             ;  then just do the normal stuff

TurnMenuScreenOn:
    JSR WaitForVBlank_L      ; wait for VBlank (don't want to turn the screen on midway through the frame)
    LDA #>oam                ; do Sprite DMA
    STA $4014
    JSR DrawPalette          ; draw/apply the current palette

    LDA #$08
    STA soft2000             ; set $2000 and soft2000 appropriately
    STA $2000                ;  (no NT scroll, BG uses left pattern table, sprites use right, etc)

    LDA #$1E
    STA $2001                ; enable BG and sprite rendering
    LDA #0
    STA $2005
    STA $2005                ; reset scroll

    LDA #BANK_THIS           ; record current bank and CallMusicPlay
    STA cur_bank
    JMP CallMusicPlay


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Main Menu Sub Cursor  [$B7A9 :: 0x3B7B9]
;;
;;    Draws the cursor for the main menu sub target (ie:  when you select
;;  a character for a sub menu -- like for magic or status)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawMainMenuSubCursor:
    LDA cursor                      ; get cursor
    ASL A                           ; double it (2 bytes per coord)
    TAX                             ; and stuff it in X
    LDA lut_MainMenuSubCursor, X    ; load up the coords from our LUT
    STA spr_x
    LDA lut_MainMenuSubCursor+1, X
    STA spr_y
    JMP DrawCursor                  ; then draw the cursor and exit


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DrawMainMenuCursor  [$B7BA :: 0x3B7CA]
;;
;;    Loads the coords for the main menu cursor, and draw the cursor sprite
;;  at those coords.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawMainMenuCursor:
    LDY cursor                    ; get current cursor selection
    LDA lut_MainMenuCursor_Y, Y   ;  use cursor as an index to get the desired Y coord
    STA spr_y                     ;  write the Y coord
    LDA #$10                      ; X coord for main menu cursor is always $10
    STA spr_x
    JMP DrawCursor                ; draw it!  and exit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DrawItemMenuCursor  [$B7C8 :: 0x3B7D8]
;;
;;   Loads the coords for the item menu cursor, and draws the cursor sprite there
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawItemMenuCursor:
    LDA cursor                   ; get current cursor and double it (loading X,Y pair)
    ASL A
    TAX                          ;  put it in X

    LDA lut_ItemMenuCursor, X    ; load X,Y pair into spr_x and spr_y
    STA spr_x
    LDA lut_ItemMenuCursor+1, X
    STA spr_y

    JMP DrawCursor               ; then draw the cursor


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Main and item menu cursor position LUTs  [$B7D9 :: 0x3B7E9]
;;
;;    X/Y Coords for cursor placement for main and item menus
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

lut_MainMenuSubCursor:
  .BYTE $48,$10,      $98,$10
  .BYTE $48,$80,      $98,$80

lut_MainMenuCursor_Y:           ; Y coord only... X coord is hardcoded
  .BYTE   $90,$A0,$B0,$C0,$D0

lut_ItemMenuCursor:
  .BYTE   $10,$30,   $58,$30,   $A0,$30
  .BYTE   $10,$40,   $58,$40,   $A0,$40
  .BYTE   $10,$50,   $58,$50,   $A0,$50
  .BYTE   $10,$60,   $58,$60,   $A0,$60
  .BYTE   $10,$70,   $58,$70,   $A0,$70
  .BYTE   $10,$80,   $58,$80,   $A0,$80
  .BYTE   $10,$90,   $58,$90,   $A0,$90
  .BYTE   $10,$A0,   $58,$A0,   $A0,$A0



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Magic Menu Cursor [$B816 :: 0x3B826]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawMagicMenuCursor:
    LDA cursor            ; get the cursor
    STA tmp               ; back it up (dumb -- since we can just look at 'cursor' any time >_>)

    AND #$03              ; mask out the low bits (column)
    ASL A                 ;   *2
    STA tmp+1             ;  store in tmp+1
    CLC
    ADC tmp+1             ; add it again (*4)
    ADC tmp+1             ; and again (*6)
    ASL A
    ASL A
    ASL A                 ; then multiply by 8 (*48)
    CLC
    ADC #$50              ; and add $50  (col*48 + $50)
    STA spr_x             ; this is our X coord for the cursor

    LDA tmp               ; get the cursor (row*4)
    ASL A                 ; multiply by 4  (*16)
    ASL A
    AND #$F0              ; mask to remove the column bits (now a clean *16)
    CLC
    ADC #$28              ; add $28  (row*16 + $28)
    STA spr_y             ; htis is our Y coord

    JMP DrawCursor        ; Draw it!  and exit


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Main Menu   [$B83A :: 0x3B84A]
;;
;;    Does all BG drawing for the main menu
;;   Does not draw sprites
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawMainMenu:
    JSR ClearNT                    ; start by clearing the NT
    JSR DrawOrbBox                 ; draw the orb box
    JSR DrawMainMenuGoldBox        ; gold box
    JSR DrawMainMenuOptionBox      ; and option box

    LDA #1                         ; then draw the boxes for each character
    JSR DrawMainItemBox            ;  stats...starting with the first character
    LDA #$00
    JSR DrawMainMenuCharBoxBody

    LDA #2                         ; second
    JSR DrawMainItemBox
    LDA #$40
    JSR DrawMainMenuCharBoxBody

    LDA #3                         ; third
    JSR DrawMainItemBox
    LDA #$80
    JSR DrawMainMenuCharBoxBody

    LDA #4                         ; fourth
    JSR DrawMainItemBox
    LDA #$C0
    JMP DrawMainMenuCharBoxBody    ; and then exit


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Main Menu Gold Box   [$B86E :: 0x3B87E]
;;
;;    Self explanitory.  Draws the box on the main menu that shows your current GP
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawMainMenuGoldBox:
    LDA #5               ; draw main/item box number 5 (the GP box)
    JSR DrawMainItemBox
    LDA #$01             ; draw menu string ID=$01  (current GP, followed by " G")
    JMP DrawMenuString


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Orb Box   [$B878 :: 0x3B888]
;;
;;   Draws the Orb box (and its contents) as seen on the main menu.
;;
;;   tmp+7 is used to build the attribute byte
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawOrbBox:
    LDA #0             ; Draw main menu box ID 0  (the orb box)
    JSR DrawMainItemBox

      ; Fire Orb
    LDX #$84           ; dest ppu address       = $2084
    LDY #$64           ; lit orb tiles start at = $64
    LDA orb_fire       ; fire orb status
    JSR @DrawOrb

      ; Water Orb
    LDX #$86           ; dest ppu address       = $2086
    LDY #$68           ; lit orb tiles start at = $68
    LDA orb_water      ; water orb status
    JSR @DrawOrb

      ; Air Orb
    LDX #$C4           ; dest ppu address       = $20C4
    LDY #$6C           ; lit orb tiles start at = $6C
    LDA orb_air        ; air orb status
    JSR @DrawOrb

      ; Earth Orb
    LDX #$C6           ; dest ppu address       = $20C6
    LDY #$70           ; lit orb tiles start at = $70
    LDA orb_earth      ; earth orb status
    JSR @DrawOrb

      ; Attributes for all orbs
    LDA $2002    ; reset PPU toggle
    LDA #>$23C9
    STA $2006
    LDA #<$23C9
    STA $2006    ; attribute byte at $23C9
    LDA tmp+7    ; load computed attribute byte
    STA $2007    ;   and draw it
    RTS

   ; DrawOrb local subroutine
   ;  X = low byte of dest PPU address
   ;  Y = orb tile ID to draw (each lit orb has different graphics)
   ;  A = orb status.  0 = not lit... nonzero=lit

  @DrawOrb:
    LSR tmp+7
    LSR tmp+7     ; shift 2 bits out of computed attribute byte
    CMP #0        ; check orb status
    BNE :+        ; if lit, skip ahead

      LDY #$76    ; if orb not lit... replace tile with $76 (unlit orb graphics)
      LDA #$C0    ; and OR #$C0 to our attribute byte
      ORA tmp+7   ;  unlit orbs use palette 3
      STA tmp+7   ;  lit orbs use palette 0 -- so this changes attributes accordingly

:   LDA $2002     ; reset PPU toggle
    LDA #$20
    STA $2006     ; set high byte of ppu addr to $20
    STX $2006     ; and low byte to our desired address

    STY $2007     ;  draw the first tile
    INY
    STY $2007     ;  then the second
    INY

    LDA #$20      ; Set PPU address to look 1 row below what we just drew
    STA $2006     ;  high byte is still $20
    TXA           ;  but take the low byte
    CLC
    ADC #$20      ;  and add #$20 so that it is the next row
    STA $2006

    STY $2007     ;  draw the 3rd tile
    INY
    STY $2007     ;  and lastly the 4th
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Main/Item Box   [$B8EF :: 0x3B8FF]
;;
;;    A contains the ID of box we're to draw.
;;      simply loads dims then draws box
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawMainItemBox:
    JSR LoadMainItemBoxDims
    JMP DrawBox

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   Load Main/Item Menu Box Dimensions  [$B8F5 :: 0x3B905]
;;
;;     A contains the ID of the box we're to load dims for:
;;
;;     0-6 are main menu boxes
;;     7-9 are item menu boxes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LoadMainItemBoxDims:
    ASL A        ; multiply ID by 4 (4 bytes per box)
    ASL A
    TAX          ; put it in X to index

    LDA lut_MainItemBoxes, X     ; X coord of box
    STA box_x
    LDA lut_MainItemBoxes+1, X   ; Y coord of box
    STA box_y
    LDA lut_MainItemBoxes+2, X   ; Width of box
    STA box_wd
    LDA lut_MainItemBoxes+3, X   ; Height of box
    STA box_ht

    LDA #BANK_THIS   ; set swap back bank to this bank
    STA cur_bank     ;  so that stalled boxes will swap back appropriately
    RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DrawMainMenuOptionBox  [$B911 :: 0x3B921]
;;
;;    Draws the option box for the main menu (the one that contains further menu options,
;;  like "Status", "Item", "Weapon", etc
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawMainMenuOptionBox:
    LDA #6
    JSR DrawMainItemBox    ; Draw Main/Item Box ID=$06  (the option box)
    INC dest_y             ;  draw the containing text one line lower than usual (so the cursor will fit in the box)
    LDA #$02               ; Draw Menu String ID=$02 (the option text)
    JMP DrawMenuString


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Item Title Box  [$B91D :: 0x3B92D]
;;
;;    Draws the 'ITEM' title box as appears in the item menu
;;  Somewhat oddly, the box to draw is hardcoded into this routine.. but the string isn't!
;;
;;  IN:  A = Menu String ID to draw inside this box ($03 = "ITEM")
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawItemTitleBox:
    PHA                   ; push menu string ID to back it up
    LDA #$07              ; draw mainitem box ID 7 (the "ITEM" title box)
    JSR DrawMainItemBox
    PLA                   ; pull menu string ID
    JMP DrawMenuString    ;  and draw it and return



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Item Description Box  [$B927 :: 0x3B937]
;;
;;    This comes in two flavors.  One plays the special event fanfare music
;;  (heard when you use a special item at the right time -- like when you use the Lute in ToFR)
;;  the other doesn't.  Apart from that they're both the same.  They draw the
;;  description box, then the desired description text
;;
;;  IN:   A = ID of menu to string to draw (for description text)
;;
;;    Note that the description box is drawn while the PPU is on and rendering!
;;  Therefore all the box and text drawing must be done in VBlank.  To accomplish
;;  this... the game uses 'menustall'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


DrawItemDescBox_Fanfare:
    LDX #$54              ; play music track $54 (special event fanfare)
    STX music_track

DrawItemDescBox:
    PHA                   ; push menu string ID to back it up
    LDA #1                ; set menustall to nonzero (indicating we need to stall)
    STA menustall
    LDA #$08              ; draw main/item box ID $08  (the description box)
    JSR DrawMainItemBox
    PLA                   ; restore menu string ID
    INC descboxopen       ; set descboxopen to a nonzero value to mark the description box as open

    ;;;  no JMP or RTS -- code flows seamlessly into DrawMenuString

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Menu String  [$B938 :: 0x3B948]
;;
;;    Draws a string from a series of menu strings.  These include titles,
;;  labels, as well as item descriptions, and pretty much anything else that
;;  appears in menus
;;
;;  IN:  A              = ID of menu string to draw ($00-7F)
;;       dest_x, dest_y = Coords to draw string to (this is usually filled by DrawBox)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawMenuString:
    ASL A                   ; double A (pointers are 2 bytes)
    TAX                     ; put in X to index menu string pointer table
    LDA lut_MenuText, X
    STA text_ptr
    LDA lut_MenuText+1, X   ; load pointer from table, store to text_ptr  (source pointer for DrawComplexString)
    STA text_ptr+1
         ;;  code seamlessly flows into DrawMenuComplexString


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Menu Complex String  [$B944 :: 0x3B954]
;;
;;    Simply calls DrawComplexString -- however it sets the desired return and data bank
;;  to this bank.  This can be called when the string to be drawn is either on this bank
;;  or is in RAM.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawMenuComplexString:
    LDA #BANK_THIS
    STA cur_bank          ; set data bank (string to draw is on this bank -- or is in RAM)
    STA ret_bank          ; set return bank (we want it to RTS to this bank when complete)
    JMP DrawComplexString ;  Draw Complex String, then exit!



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Erase Description Box [$B94D :: 0x3B95D]
;;
;;    Erases the item description box from the item menu.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

EraseDescBox:
    LDA #1
    STA menustall            ; set menustall -- we will need to stall here, since the PPU is on
    LDA #$08
    JSR LoadMainItemBoxDims  ; load box dimensions for box ID 8 (the item description box)
    JMP EraseBox             ;  erase the box, then exit



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Character Menu String  [$B959 :: 0x3B969]
;;
;;     Draws a desired menu string (see DrawMenuString), but replaces character stat
;;  codes in the string to reflect the stats of the given character.
;;
;;  IN:              A = menu string ID to draw
;;        submenu_targ = character ID whose stats to draw
;;                   Y = length of string (DrawCharMenuString_Len only!)
;;
;;     Why the length of the string is required is beyond me -- the game really should've just
;;  looked for the null terminator instead.  But it doesn't... so blah.  Chalk it up to one of the
;;  many stupid things this game does.
;;
;;    DrawCharMenuString_Len  is the same as DrawCharMenuString, only it does not set the
;;  string length, and thus the string length must be put in Y before calling.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawCharMenuString:
    LDY #$7F                ; set length to default 7F

DrawCharMenuString_Len:
    ASL A                   ; double menu string ID
    TAX                     ; put in X
    LDA lut_MenuText, X     ; and load up the pointer into (tmp)
    STA tmp
    LDA lut_MenuText+1, X
    STA tmp+1

    LDA #<bigstr_buf        ; set the text pointer to our bigstring buffer
    STA text_ptr
    LDA #>bigstr_buf
    STA text_ptr+1

  @Loop:                    ; now step through each byte of the string....
    LDA (tmp), Y            ; get the byte
    CMP #$10                ; compare it to $10 (charater stat control code)
    BNE :+                  ;   if it equals...
      ORA submenu_targ      ;   OR with desired character ID to draw desired character's stats
:   STA bigstr_buf, Y       ; copy the byte to the big string buffer
    DEY                     ; then decrement Y
    CPY #$FF                ; check to see if it wrapped
    BNE @Loop               ; and keep looping until it has

                                ; once the loop is complete and our big string buffer has been filled
    JMP DrawMenuComplexString   ; draw the complex string and exit.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Main Menu Character Box Body      [$B982 :: 0x3B992]
;;
;;    Fills a previously drawn character box with the given character's data
;;
;;  IN:  A              = $00, $40, $80, or $C0 indicating which character's stats to draw
;;       dest_x, dest_y = Destination coordinates (filled by DrawBox, which is called just before this routine)
;;
;;    Note this routine does BG changes only -- it does NOT draw the character sprites
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawMainMenuCharBoxBody:
    PHA               ; push char index to stack (temporary)
    TAX               ; also put index in X

    LDA #>str_buf     ; the strings we will be drawing here will be from a RAM buffer
    STA text_ptr+1    ;  so set the high byte of the string pointer

    LDA ch_maxmp, X   ; check all of this character's Max MP
    ORA ch_maxmp+1, X ;  if any level of spells is nonzero, we'll need to draw the MP
    ORA ch_maxmp+2, X ;  for this character.  Otherwise, we won't need to.
    ORA ch_maxmp+3, X ;  this is checked easily by just ORing all the max MP bytes
    ORA ch_maxmp+4, X
    ORA ch_maxmp+5, X
    ORA ch_maxmp+6, X
    ORA ch_maxmp+7, X
    BNE @DrawMP       ; if max MP is nonzero, jump ahead to DrawMP
      JMP @NoMP       ; otherwise... jump ahead to NoMP


  @DrawMP:
    LDY #$00          ; Y is dest index for our drawing loop, start it at zero
                      ;  the loop also modifies X to use it as a source index (this is why the char index was pushed)
   @MPLoop:
      LDA ch_curmp, X ; Get current MP for this level
      ORA #$80        ;  Or with $80 to get this digit's tile
      STA str_buf, Y  ; write this tile to the temp string

      INY             ; inc our dest pointer
      LDA #$7A        ; write the '/' tile to seperate the spell levels
      STA str_buf, Y

      INX             ; inc src pointer (to look at next level MP)
      INY             ; inc dest pointer
      CPY #8*2        ; continue until all 8 levels drawn (2 characters drawn per level)
      BCC @MPLoop

    LDA dest_y        ; we draw the 2nd row of this MP first
    CLC
    ADC #$0A          ; Add $A to the given row so we draw this string 10 rows 
    STA dest_y        ;  into the box

    LDA #$00          ; replace the last '/' in the string with a null terminator
    STA str_buf+$F

    LDA #<str_buf + $08  ; start drawing from 8 characters into the string
    STA text_ptr         ;  this will draw MP for levels 4-7 only (the second row)
    JSR DrawMenuComplexString    ; draw it

    LDA #<str_buf     ; set low byte of pointer to start of the string buf
    STA text_ptr      ;  this is MP levels 0-3 (the first row)
    LDA #0            ; write a null terminator to the start of the second row, so we don't draw that row again
    STA str_buf + $08
    DEC dest_y        ; decrement the Y coord so we draw this row one row above the previous draw
    JSR DrawMenuComplexString   ; draw it!

    LDA #$52          ; "MA" DTE tile
    STA str_buf
    LDA #$4A          ; "GI" DTE tile
    STA str_buf+1
    LDA #$8C          ; "C" tile
    STA str_buf+2
    LDA #$00          ; null terminator
    STA str_buf+3

    LDA #<str_buf
    STA text_ptr      ; draw "MAGIC" string as loaded above
    DEC dest_y        ; dec row to print 1 above last
    JSR DrawComplexString    ; draw it!

    LDA dest_y        ; subtract 8 from the Y coord to put it back to where it started
    SEC               ;  (we added 10 at first, and then DEC'd twice)
    SBC #8
    STA dest_y

  @NoMP:        ; code reaches here when the character has no MP, or after MP drawing is complete
    PLA         ; pull previously pushed character index
    ASL A       ;  convert it from $40 base to $1 base (ie:  $03 is character 3 instead of $C0)
    ROL A
    ROL A
    AND #$03    ; mask out low 2 bits (precautionary -- isn't really necessary)
    ORA #$10    ; or with $10 (creates the 'draw stat' control code)

     ;;; Draw Character HP
    STA str_buf    ; put 'draw stat' code at str_buf and str_buf+3
    STA str_buf+3
    LDA #$05       ; draw stat 5 (Cur HP)
    STA str_buf+1
    LDA #$7A       ; '/' character
    STA str_buf+2
    LDA #$06       ; stat 6 (Max HP)
    STA str_buf+4
    LDA #$00       ; null terminator
    STA str_buf+5  ; effectively creates "CurHP/MaxHP"

    LDA dest_y
    CLC
    ADC #5         ; add 5 to the Y coord (draw this 5 rows down)
    STA dest_y
    LDA #<str_buf  ; set low byte of string pointer (start drawing the string from $0300)
    STA text_ptr
    JSR DrawMenuComplexString    ; Draw it!
    LDA dest_y
    SEC
    SBC #5         ; then subtract the 5 we just added to restore the Y coord
    STA dest_y

     ;;; Draw everything else (name, level, "HP" text or ailment blurb)
    LDA #$00       ; first byte of str_buf is still has the 'draw stat' code
    STA str_buf+1  ; draw stat 0 (Name)
    LDA #$01       ; double line break
    STA str_buf+2
    LDA #$C6       ; special 'L' character for the level
    STA str_buf+3
    LDA str_buf    ; fetch 'draw stat' code
    STA str_buf+4  ;   and put it here as well
    LDA #$03       ; draw stat 3 (Level)
    STA str_buf+5
    LDA #$01       ; double line break
    STA str_buf+6
    LDA str_buf    ; fetch 'draw stat' code again
    STA str_buf+7  ;   and put here
    LDA #$02       ; draw stat 2 (Ailment blurb -- "HP" if healthy, "ST" if stone, etc)
    STA str_buf+8
    LDA #$00       ; null terminator
    STA str_buf+9
    LDA #<str_buf  ; start drawing from start of string
    STA text_ptr
    JMP DrawComplexString    ; Draw it, then exit!



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Magic Menu Main Box  [$BA6D :: 0x3BA7D]
;;
;;    Draws the magic menu main box (containing all the spells)
;;  Also sets the cursor to the first spell in the list.
;;
;;  OUT:       C = set if player has no spells
;;                 clear if player has at least 1 spell
;;        cursor = first spell on the character's list (if they have any spells)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawMagicMenuMainBox:
    LDA #$09
    JSR DrawMainItemBox          ; Draw the box itself from the list of MainItem boxes

    LDY #$C0                     ; set char menu string length to $C0
    LDA #$2A                     ; and draw string 2A (entire spell list, along with level names an MP amounts
    JSR DrawCharMenuString_Len   ;   -- ALL the text in one string!)

    LDA submenu_targ             ; get the character we're looking at
    ROR A
    ROR A
    ROR A
    AND #$C0                     ; shift his ID into the useable char index
    TAX                          ; and put in X
    LDY #$08                     ; going to loop 8 times (one for each level of spells

  @Loop:
      LDA ch_spells, X        ; check first spell in level
      BNE @FoundSpell         ; if nonzero (has a spell)  escape
      INX
      LDA ch_spells, X        ; otherwise check next spell...
      BNE @FoundSpell
      INX
      LDA ch_spells, X        ; and next...
      BNE @FoundSpell
      INX                     ; INX by 2 because the 4th slot in each level is empty (padding)
      INX                     ; then decrement Y (loop counter)
      DEY
      BNE @Loop               ; loop until we've checked every spell

    SEC             ; if no spell found, SEC and exit
    RTS

  @FoundSpell:
    TXA             ; if we found a spell... move which spell into A
    AND #$1F        ;  and mask out which spell it is (remove the char index)
    STA cursor      ;  and store it in the current cursor
    CLC             ; then CLC to indicate the character has a spell
    RTS             ; and exit


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Boxes for Main/Item menus  [$BAA2 :: 0x3BAB2]
;;
;;     in groups of 4:  X, Y, width, height
;;
;;  Item menu boxes are also used for the magic menu.

lut_MainItemBoxes:
    .BYTE   $02,$02,$08,$08    ; main menu - box holding the orbs
    .BYTE   $0B,$01,$0A,$0E    ; main menu - Character 0's stat box
    .BYTE   $15,$01,$0A,$0E    ; main menu - Character 1's stat box
    .BYTE   $0B,$0F,$0A,$0E    ; main menu - Character 2's stat box
    .BYTE   $15,$0F,$0A,$0E    ; main menu - Character 3's stat box
    .BYTE   $01,$0A,$0A,$05    ; main menu - GP box
    .BYTE   $02,$0F,$08,$0E    ; main menu - option box (containing further menu options)
    .BYTE   $01,$01,$08,$04    ; item menu - "ITEM" title box
    .BYTE   $01,$16,$1E,$07    ; item menu - Description box at the bottom
    .BYTE   $01,$03,$1E,$13    ; item menu - the main box containing item list (but isn't used for
                               ;             item menu -- see DrawItemBox.  Is used for magic menu, though)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Enter Equip Menu  [$BACA :: 0x3BADA]
;;
;;    Good old equip menus!  (Weapon and Armor menus).  On entry, the game calls UnadjustEquipStats
;;  to modify the party's stats to be what they would be if all their equipment was removed.  Then on exit
;;  it calls ReadjustEquipStats to modify the party's stats to reflect the equipment they're wearing.
;;  This makes it so it doesn't have to constantly modify stats as you unequip/reequip items in the menu...
;;  and can just do it all at once.
;;
;;  IN:   A = ch_weapons-ch_stats  for weapon menu
;;            ch_armor-ch_stats    for armor menu
;;            this value to be stored in 'equipoffset' by this routine
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

EnterEquipMenu:
    STA equipoffset       ; record the equipoffset to indicate weapon or armor menu

    LDA #0
    STA $2001             ; turn off the PPU
    STA joy_a             ; clear joy_a and joy_b counters
    STA joy_b
    STA menustall         ; and turn off menu stalling (since the PPU is off)

    JSR SortEquipmentList           ; sort the equipment list to remove gaps
    JSR DrawEquipMenu               ; draw the equip menu (but not the item text)
    JSR CopyEquipToItemBox          ; copy equipment from inventory to item_box so it's easier to work with
    JSR UnadjustEquipStats          ; unadjust equipment stats so they can be readjusted later
    JSR TurnMenuScreenOn_ClearOAM   ; then clear OAM and turn the screen on  (even though item names have not been drawn)

    LDA #1
    STA menustall                   ; now that the PPU is on, turn on menu stalling (DrawEquipMenuStrings does
    JSR DrawEquipMenuStrings        ;   this already though...).  Then draw the menu strings (item names)

  @Start:
    LDA #0
    STA eq_modecurs               ; reset the mode cursor to 0  ("Equip")

  @Loop:
    JSR ClearOAM                  ; clear OAM
    JSR DrawEquipMenuModeCurs     ; draw the mode cursor
    JSR EquipMenuFrame            ; then do an Equip Menu Frame

    LDA joy_a
    BNE @A_Pressed            ; check to see if A pressed
    LDA joy_b
    BNE @B_Pressed            ; or B

    JSR MoveEquipMenuModeCurs ; if neither A nor B pressed, move the mode cursor
    JMP @Loop                 ; and loop until one of them is pressed

  @A_Pressed:
    JSR @GoToSubMenu          ; go to the desired sub menu
    JMP @Start                ; then restart this loop once they exit that sub menu

  @B_Pressed:                 ; if B pressed....
    JSR CopyEquipFromItemBox  ;  move all equipment from item box back to player inventory
    JSR SortEquipmentList     ;  sort equipment list to remove gaps
    JMP ReadjustEquipStats    ;  adjust stats to reflect new equipment.  Then exit.


  @GoToSubMenu:
    JSR PlaySFX_MenuSel       ; play the selection sound effect (a waste, it was already played by EquipMenuFrame)

    LDA #0
    STA cursor                ; reset the primary
    STA cursor2               ;   and secondary cursor to zero

    LDA eq_modecurs           ; get the mode cursor to see which sub menu we're to go to.
    BNE :+
      JMP EquipMenu_EQUIP     ;   mode=0 -> EQUIP
:   CMP #$01
    BNE :+
      JMP EquipMenu_TRADE     ;   mode=1 -> TRADE
:   JMP EquipMenu_DROP        ;   mode=2 -> DROP


;;
;;  TRADE
;;

EquipMenu_TRADE:              ; "TRADE" option selected
    JSR ClearOAM              ; clear OAM
    JSR DrawEquipMenuCurs     ; draw the cursor (primary cursor only)
    JSR EquipMenuFrame        ; do a frame

    LDA joy_a                 ; check or A and B button presses
    BNE @A_Pressed
    LDA joy_b
    BNE @B_Pressed

    JSR MoveEquipMenuCurs     ; then move the cursor for directional presses
    JMP EquipMenu_TRADE       ; rinse, repeat

  @B_Pressed:                 ; if B pressed... just exit
    RTS

  @A_Pressed:                 ; if A pressed...
    LDA cursor
    STA cursor2               ; copy the primary cursor to the secondary cursor
                              ; then proceed to the Trade subloop
    @SubLoop:
      JSR ClearOAM                      ; clear OAM
      JSR DrawEquipMenuCursSecondary    ; draw both primary+secondary cursors
      JSR EquipMenuFrame                ; do a frame

      LDA joy_a
      BNE @DoTrade            ; check for A/B button presses
      LDA joy_b
      BNE EquipMenu_TRADE     ;   if B pressed, jump back to main Trade loop

      JSR MoveEquipMenuCurs   ; check for cursor movement
      JMP @SubLoop            ; and loop

  @DoTrade:              ; DoTrade is called when two items to trade have been selected
    LDX cursor           ; primary cursor in X (one of the items to trade)
    LDY cursor2          ; secondary cursor in Y (the other item)

    LDA item_box, X      ; get the 'X' item
    STA tmp              ;  store it to temp RAM
    LDA item_box, Y      ; get the 'Y' item
    AND #$7F             ;  unequip it
    STA item_box, X      ; move Y item to X item
    LDA tmp              ; get original X item
    AND #$7F             ;  unequip it
    STA item_box, Y      ; and move X item to Y item (swapped the two items)

    JSR DrawEquipMenuStrings   ; redraw all the equipment strings to reflect changes
    JMP EquipMenu_TRADE        ; then jump back to Trade loop

;;
;;  EQUIP
;;

EquipMenu_EQUIP:
    JSR ClearOAM             ; clear OAM
    JSR DrawEquipMenuCurs    ; draw the primary cursor
    JSR EquipMenuFrame       ; do a frame

    LDA joy_a
    BNE @A_Pressed           ; check for A/B button presses
    LDA joy_b
    BNE @B_Pressed

    JSR MoveEquipMenuCurs    ; and check for cursor movement
    JMP EquipMenu_EQUIP      ; rinse, repeat

  @B_Pressed:                ; if B pressed, just exit
    RTS

  @A_Pressed:                ; if A pressed...
    LDX cursor               ; get the cursor in X
    LDA item_box, X          ; use it to index and get the selected item
    BNE @ConfirmEquip        ; if this item is nonzero, jump ahead to confirm that it's equippable
                             ;  otherwise.. can't equip (can't equip a blank slot)
    @CantEquip:
      JSR PlaySFX_Error      ; if can't equip, play the error sound effect
      JMP EquipMenu_EQUIP    ; and keep looping

  @ConfirmEquip:
    BMI @ToggleEquip         ; see if item is already equipped, if it is, you can ALWAYS unequip it (no check necessary)
    JSR IsEquipLegal         ; Confirm to see if this item is equippable by this class
    BCS @CantEquip           ; if it isn't (C set), then we can't equip it

    LDX cursor               ; otherwise (can equip), restore X to be the cursor again, then toggle equip state

  @ToggleEquip:
    LDA item_box, X           ; get the item
    EOR #$80                  ; toggle its equip state (unequip it if equipped... or equip it if unequipped)
    STA item_box, X           ; and write it back
    JSR DrawEquipMenuStrings  ; redraw the item names to reflect changes
    JMP EquipMenu_EQUIP       ; and continue looping


;;
;;  DROP
;;

EquipMenu_DROP:
    JSR ClearOAM              ; clear OAM
    JSR DrawEquipMenuCurs     ; draw the primary cursor
    JSR EquipMenuFrame        ; do a frame

    LDA joy_a
    BNE @A_Pressed            ; check for A and B presses
    LDA joy_b
    BNE @B_Pressed

    JSR MoveEquipMenuCurs     ; check for cursor movement
    JMP EquipMenu_DROP        ; rinse, repeat until A/B pressed

  @B_Pressed:
    RTS                       ; if B pressed, just exit

  @A_Pressed:                 ; if A pressed....
    LDX cursor                ; put the cursor in X
    LDA item_box, X           ; get the item to drop from the item box
    BNE @ConfirmLoop          ; if it's zero....
      JSR PlaySFX_Error       ;  ... play the error sound effect (can't drop an empty slot)
      JMP EquipMenu_DROP      ;      and continue looping

  @ConfirmLoop:
    JSR ClearOAM              ; clear OAM

    LDA framecounter          ; for confirmation, the cursor is to flicker.  Use the frame counter
    LSR A                     ;   and put bit 1 in C  (but remember that framecounter is INC'd by 2
    LSR A                     ;   every EquipMenuFrame
    BCS :+                    ; skip over drawing the cursor if C set (odd frame) --
      JSR DrawEquipMenuCurs   ;   -- so only draw the cursor every other frame
:   JSR EquipMenuFrame        ; Do a frame

    LDA joy_a
    BNE @DoDrop               ; if they pressed A, do the drop
    LDA joy_b
    BNE EquipMenu_DROP        ; if they pressed B, abort, and return to main DROP loop

    JMP @ConfirmLoop          ; if neither A nor B pressed, keep looping until one of them is.

@DoDrop:
    LDX cursor                ; get the cursor in X
    LDA #0
    STA item_box, X           ; erase the item from the item box
    JSR DrawEquipMenuStrings  ; redraw the item names to reflect changes
    JMP EquipMenu_DROP        ; and return to Drop loop


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Is Equip Legal [$BC0A :: 0x3BC1A]
;;
;;     Checks to see if a specific class can equip a specific item.
;;  ALSO!  This routine modifies the item_box to de-equip all items of the same
;;  type that this character is equipping.  So if you test to see if a weapon is
;;  equippable, and it is, then all other weapons will be unequipped by this routine.
;;  Or if you try to equip a helmet.. all other helmets will be unequipped.
;;
;;  IN:            A = 1-based ID of item to check (0=blank item)
;;       equipoffset = indicates to check weapon or armor
;;            cursor = 4* the character ID whose class we're to check against
;;
;;  OUT:           C = set if equipping is illegal for this class (can't equip it)
;;                     clear if legal (can)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


IsEquipLegal:
    SEC
    SBC #$01          ; subtract 1 from the item ID to make it zero based
    ASL A             ; double it
    STA tmp           ; tmp = (2*item_id)

    LDA cursor        ; get the cursor
    ASL A
    ASL A
    ASL A
    ASL A
    AND #$C0          ; shift and mask to get usable character index
    TAX               ; put char index in X

    LDA ch_class, X   ; get the character's class
    ASL A             ; double it (2 bytes for equip permissions)
    TAX               ; and put in X to index the equip bit

    LDA lut_ClassEquipBit, X        ; get the class permissions bit position word
    STA tmp+4                       ;  and put in tmp+4,5
    LDA lut_ClassEquipBit+1, X
    STA tmp+5

    LDY equipoffset              ; now, see if we're dealing with weapons or armor
    CPY #ch_weapons-ch_stats
    BNE @Armor

  @Weapon:
    LDX tmp                        ; get the weapon id (*2)
    LDA lut_WeaponPermissions, X   ; use it to get the weapon permissions word (low byte)
    AND tmp+4                      ; mask with low byte of class permissions
    STA tmp                        ;  temporarily store result
    LDA lut_WeaponPermissions+1, X ; then do the same with the high byte of the permissions word
    AND tmp+5                      ;  mask with high byte of class permissions
    ORA tmp                        ; then combine with results of low mask
                          ;  here... any nonzero value will indicate that the item cannot be equipped

    CMP #$01              ; compare with 1 (any nonzero value will set C)
    BCC :+                ; if C is set (can't equip)....
      RTS                 ;   ... just exit.  otherwise...

:   LDA cursor            ; get the cursor (character id*4)
    AND #$0C              ; isolate the character bits
    TAX                   ; and put in X for indexing

    LDA item_box, X       ; unequip all weapons
    AND #$7F
    STA item_box, X
    LDA item_box+1, X
    AND #$7F
    STA item_box+1, X
    LDA item_box+2, X
    AND #$7F
    STA item_box+2, X
    LDA item_box+3, X
    AND #$7F
    STA item_box+3, X

    RTS                   ; then exit (C is still clear, indicating item can be equipped)

  @Armor:
    LDX tmp                       ; get the armor id (*2)
    LDA lut_ArmorPermissions, X   ; use it to get the armor permissions word
    AND tmp+4                     ;  and mask it with the class permissions word
    STA tmp
    LDA lut_ArmorPermissions+1, X
    AND tmp+5
    ORA tmp               ; and OR both high and low bytes of result together.  A nonzero result here indicates
                          ;  armor cannot be equipped

    CMP #$01              ; compare with 1 (any nonzero value sets C)
    BCC :+                ; if armor can't be equipped....
      RTS                 ; .. just exit.  Otherwise...

:   TXA                   ; get back the armor_id*2
    LSR A                 ; /2 to restore armor_id
    TAX                   ; and put back in X

    LDA lut_ArmorTypes, X ; use armor ID to index and find out what kind of armor it is (body, helmet, shield, etc)
    STA tmp+2             ; store armor type in tmp+2

    LDA cursor            ; store current cursor in tmp (seems dumb... we could just reference cursor directly)
    STA tmp
    AND #$0C              ; mask out the charater bits  (char ID * 4)
    STA tmp+1             ;  store that in tmp+1  (loop's item to unequip)

    LDY #$04              ; set Y (loop counter) to 4 (loop 4 times)

  @ArmorLoop:
    LDA tmp+1             ; get the loop item index
    CMP tmp               ; compare to the item we're trying to equip
    BEQ @ArmorSkip        ; if they're equal, skip over this item (seems pointless... the item we're checking wouldn't be equipped)

    LDX tmp+1             ; put loop item index in X
    LDA item_box, X       ;  use it to get that item from the item_box
    BPL @ArmorSkip        ; if that item is not equipped, skip it

    SEC                   ; otherwise (it is equipped), subtract $81 to get the armor ID
    SBC #$81
    TAX                   ; put armor ID in X
    LDA lut_ArmorTypes, X ; use that to look up what type of armor this is

    CMP tmp+2             ; compare that to the type of armor we're trying to equip
    BNE @ArmorSkip        ; if it doesn't match (different kind of armor), skip it

                          ; otherwise... it's a match!  need to unequip it.
    LDX tmp+1             ; get this loop item index in X
    LDA item_box, X       ; use it to get the item from the item box
    AND #$7F              ;    unequip it
    STA item_box, X       ;    then write it back

  @ArmorSkip:
    INC tmp+1             ; increment our loop item counter (to examine next item in the item box)
    DEY                   ; decrement our loop counter
    BNE @ArmorLoop        ; and keep looping until it expires

    CLC                   ; once we're done with all that, CLC to indicate the item can be equipped
    RTS                   ; and exit


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   Class Equip Bit LUT   [$BCB9 :: 0x3BCC9]
;;
;;    For weapon/armor equip permissions, each bit coresponds to a class.  If the class's
;;  bit is set for the permissions word... then that piece of equipment CANNOT be equipped
;;  by that class.  Permissions are stored in words (2 bytes) instead of just 1 byte because
;;  there are more than 8 classes
;;
;;    This lookup table is used to get the bit which represents a given class.  The basic
;;  formula is "equip_bit = ($800 >> class_id)".  So Fighter=$800, Thief=$400, etc
;;


lut_ClassEquipBit: ;  FT   TH   BB   RM   WM   BM      KN   NJ   MA   RW   WW   BW
   .WORD            $800,$400,$200,$100,$080,$040,   $020,$010,$008,$004,$002,$001


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   Armor Type LUT  [$BCD1 :: 0x3BCE1]
;;
;;   This LUT determines which type of armor each armor piece is.  The 4 basic types are:
;;
;;  0 = body armor
;;  1 = shield
;;  2 = helmet
;;  3 = gloves / gauntlets
;;
;;   Note the numbers themselves really don't signify anything.  They're only there to
;;  prevent a player from equipping multiple pieces of armor of the same type.  So you could
;;  have 256 different types of armor if you wanted (even though there are only 40 pieces of
;;  armor  ;P).
;;

lut_ArmorTypes:
  .BYTE    0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0        ; 16 pieces of body armor
  .BYTE    1,1,1,1,1, 1,1,1,1                        ; 9 shields
  .BYTE    2,2,2,2,2, 2,2                            ; 7 helmets
  .BYTE    3,3,3,3,3, 3,3,3                          ; 8 gauntlets


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  EquipMenuFrame  [$BCF9 :: 0x3BD09]
;;
;;    Same as MenuFrame in that it does all the relevent work that needs to 
;;  be done every frame.  This routine, however, is specifically geared to be
;;  used for the equip menus.  Specifically, it updates the mode attribute
;;  bytes (though that's useless in this ROM).  It also does a few extra
;;  things for the equip menu.
;;
;;  USED:  tmp+7 = used for previous direction info *in addition to* joy_prevdir
;;                 this is apparently so this routine can play the menu move sound effect
;;                 so the rest of the menu code doesn't have to.  joy_prevdir is still used
;;                 in other menu code to detect directional presses for cursor movement, though.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

EquipMenuFrame:
    JSR WaitForVBlank_L     ; wait for VBlank
    LDA #>oam
    STA $4014               ; do sprite DMA
    JSR UpdateEquipMenuModeAttrib   ; update mode attributes (useless, see this routine for why)

    LDA soft2000          ; reset scroll
    STA $2000
    LDA #$00
    STA $2005
    STA $2005

    LDA #BANK_THIS
    STA cur_bank          ; set cur_bank to this bank
    JSR CallMusicPlay     ;   so we can call music play routine

    INC framecounter      ; inc the frame counter to count this frame

    LDA #0                ; clear joy_a and joy_b markers so button presses
    STA joy_a             ;  will be recognized
    STA joy_b

    INC framecounter      ; inc frame counter again!  I would say this is bugged -- but who cares?

    LDA joy               ; get the joy data
    AND #$0F              ; isolate directional buttons
    STA tmp+7             ; and store it as the previous joy data
    JSR UpdateJoy         ; then update joy data

    LDA joy_a
    ORA joy_b             ; see if either A or B pressed
    BEQ @NotPressed       ; if not... jump ahead
    JMP PlaySFX_MenuSel   ; otherwise, play the selection sound effect and exit


  @NotPressed:            ; if neither A nor B have been pressed
    LDA joy               ; get the joy data
    AND #$0F              ; isolate directional buttons
    BEQ @Exit             ; if no directions are pressed, exit

    CMP tmp+7             ; compare current buttons to previous buttons
    BEQ @Exit             ; if no change, then exit

    JMP PlaySFX_MenuMove  ; otherwise, a new button has been pressed, so play the menu move sound, then exit

  @Exit:
    RTS




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Move Equip Menu Mode Cursor [$BD42 :: 0x3BD52]
;;
;;    Moves the equip menu mode cursor (the one that selects
;;  "Equip", "Trade", or "Drop")
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


MoveEquipMenuModeCurs:
    LDA joy           ; get joypad
    AND #$03          ; isolate left/right buttons
    CMP joy_prevdir   ; if they equal previous buttons...
    BEQ @Exit         ;  no new buttons pressed, so exit

    STA joy_prevdir   ; otherwise, record changes
    CMP #0            ; see if a button was pressed or released
    BEQ @Exit         ; if released (no buttons currently down), exit

    CMP #1
    BNE @Left         ; see if they pressed left or right

  @Right:
    LDA eq_modecurs   ; to move right...
    CLC
    ADC #$01          ; add 1 to the mode cursor
    CMP #$03
    BCC @WriteAndExit ; if >= 3....
    LDA #0            ;  ... wrap it to zero

  @WriteAndExit:
    STA eq_modecurs
  @Exit:
    RTS

  @Left:
    LDA eq_modecurs   ; to move left....
    SEC
    SBC #$01          ; subtract one
    BCS @WriteAndExit ; if still >= 0, we're don
    LDA #$02          ; otherwise, wrap to 2
    STA eq_modecurs
    RTS               ; then exit


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Move Equip Menu Cursor [$BD6E :: 0x3BD7E]
;;
;;    Moves the primary/main cursor in the equip menu (the one that actually
;;  moves amongst the charater equipment).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MoveEquipMenuCurs:
    LDA joy            ; get joy data
    AND #$0F           ; isolate directional buttons
    CMP joy_prevdir    ; see if there were changes
    BEQ @Exit          ; if not, exit

    STA joy_prevdir    ; otherwise, record changes
    CMP #0             ; see if buttons are being pressed
    BEQ @Exit          ; if not, exit

    CMP #$04           ; see if they're pressing up/down or left/right
    BCS @UpDown

  @LeftRight:          ; if left/right, simply toggle the column
    LDA cursor
    EOR #$01           ; EOR with 1 toggles the column
    STA cursor         ; then exit

  @Exit:
    RTS

  @UpDown:
    BNE @Up            ; If not left/right, see if they pressed up or down.

  @Down:
    LDA cursor         ; If down, add 2 to the cursor (next row)
    CLC
    ADC #$02
    AND #$0F           ; mask with $0F to keep in bounds / wrap
    STA cursor
    RTS                ; then exit

  @Up:
    LDA cursor         ; for up.. subtract 2
    SEC
    SBC #$02
    AND #$0F           ; and wrap
    STA cursor
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Copy Equipment to Item Box  [$BD9D :: 0x3BDAD]
;;
;;    Copies equipment from the player inventory over to
;;  the item box buffer.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CopyEquipToItemBox:
    LDX equipoffset       ; X is our source index -- start at the equipment offset
    LDY #0                ; Y is our dest index -- start at zero

  @Loop:
    LDA ch_stats, X       ; copy 4 bytes from source buffer (character inventory)
    STA item_box, Y       ;  to dest buffer (item_box)
    LDA ch_stats+1, X
    STA item_box+1, Y
    LDA ch_stats+2, X
    STA item_box+2, Y
    LDA ch_stats+3, X
    STA item_box+3, Y

    TXA                   ; add $40 to source index (look at next character's inventory)
    CLC
    ADC #$40
    TAX

    TYA                   ; add 4 to dest index
    CLC
    ADC #4
    TAY

    CMP #4*4              ; keep looping until we've copied 16 items (4 characters' inventory)
    BCC @Loop

    RTS                   ; then exit



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Copy Equipment from Item Box  [$BDC8 :: 0x3BDD8]
;;
;;    Copies equipment from the item box buffer back to player inventory
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CopyEquipFromItemBox:    ; this routine is exactly the same as the above
    LDX equipoffset      ; CopyEquipToItemBox routine, except...
    LDY #0

  @Loop:
    LDA item_box, Y      ; it copies bytes in the other direction
    STA ch_stats, X
    LDA item_box+1, Y
    STA ch_stats+1, X
    LDA item_box+2, Y
    STA ch_stats+2, X
    LDA item_box+3, Y
    STA ch_stats+3, X

    TXA
    CLC
    ADC #$40
    TAX

    TYA
    CLC
    ADC #4
    TAY

    CMP #4*4
    BCC @Loop

    RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Equip Menu  [$BDF3 :: 0x3BE03]
;;
;;    Does not draw the names of the equipment -- only the boxes and other text
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawEquipMenu:
    JSR ClearNT             ; clear the NT

    LDA #0
    JSR @DrawEquipBox           ; draw the title box
    LDA #$34                    ; menu string $34 ("WEAPON")
    LDX equipoffset
    CPX #ch_weapons - ch_stats  ; check whether this is the weapon or armor screen
    BEQ :+
      LDA #$3E                  ;   if not the weapon screen, change menu string ID to $3E ("ARMOR")
:   JSR DrawMenuString          ; then draw that title string

    LDA #1
    JSR @DrawEquipBox           ; then draw the top menu box
    LDA #$35
    JSR DrawMenuString          ; and its contained text ("EQUIP   TRADE   DROP")

    LDA #$03
    JSR @DrawEquipBox           ; char 0's equip box (but leave it empty for now)
    LDA #$02
    JSR @DrawEquipBox           ; char 0's name box
    LDA #$36
    JSR DrawMenuString          ; char 0's name

    LDA #$05
    JSR @DrawEquipBox           ; then char 1...
    LDA #$04
    JSR @DrawEquipBox
    LDA #$38
    JSR DrawMenuString

    LDA #$07
    JSR @DrawEquipBox           ; char 2...
    LDA #$06
    JSR @DrawEquipBox
    LDA #$3A
    JSR DrawMenuString

    LDA #$09
    JSR @DrawEquipBox           ; char 3...
    LDA #$08
    JSR @DrawEquipBox
    LDA #$3C
    JSR DrawMenuString

    LDA #$C0
    JSR SetPPUAddrTo_23aa       ; PPU Address = $23C0  (start of attribute tables)
    LDA #$7F                    ; some useless attribute changes...
    STA $2007                   ;  this sets a paticular square to use palette 1 instead of the normal palette 3
    LDA #$DF                    ; and this sets another square to use palette 1
    STA $2007                   ;  but palettes 1 and 3 are identical for this menu!
                                ;  These attributes changes *almost* change the palette used for the title text
                                ;  .. the problem is that they only change the palettes for the first 4 tiles
                                ;  leaving the "N" in weapon and the "R" in armor using palette 3.  My guess is this
                                ; is a hold-over from the Japanese version of the game where the weapon/armor strings
                                ; were shorter.

    RTS                         ; exit after drawing is complete


 @DrawEquipBox:
    ASL A                     ; *4  (4 bytes per box)
    ASL A
    TAX                       ; put box*4 in X

    LDA @lut_EquipBoxes, X    ; use it to index our lut
    STA box_x                 ; and load box coords/dims
    LDA @lut_EquipBoxes+1, X
    STA box_y
    LDA @lut_EquipBoxes+2, X
    STA box_wd
    LDA @lut_EquipBoxes+3, X
    STA box_ht

    LDA #BANK_THIS            ; set cur_bank (for DrawMenuString?) -- kind of pointless because DrawMenuString
    STA cur_bank              ;   already does this

    JMP DrawBox               ; then draw the box, and return

 @lut_EquipBoxes:
  .BYTE $01,$01,$07,$04   ; title box
  .BYTE $08,$01,$17,$04   ; top menu box ("Equip   Trade   Drop")
  .BYTE $01,$05,$08,$04   ; char 0's name box
  .BYTE $08,$05,$17,$06   ; char 0's equipment box
  .BYTE $01,$0B,$08,$04   ; ch 1's
  .BYTE $08,$0B,$17,$06
  .BYTE $01,$11,$08,$04   ; ch 2's
  .BYTE $08,$11,$17,$06
  .BYTE $01,$17,$08,$04   ; ch 3's
  .BYTE $08,$17,$17,$06


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Equip Menu Cursor  Primary/Secondary  [$BEA4 :: 0x3BEB4]
;;
;;    Draws the primary cursor for the equip menu, or both the primary and secondary cursors.
;;  If both are drawn... they "flicker" so that only a paticular one is drawn in any given frame.
;;  An example of when both are drawn would be when you are trading two items.
;;
;;  DrawEquipMenuCurs          =  draws just the primary
;;  DrawEquipMenuCursSecondary =  draws both in flicker fashion
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawEquipMenuCursSecondary:
    LDA framecounter           ; get the frame counter
    LSR A                      ;  shift to put bit 1 in C (alternate which cursor to draw every other frame...
    LSR A                      ;  however... since the framecounter is incremented TWICE by EquipMenuFrame,
    BCS DrawEquipMenuCurs      ;  this really alternates *every* frame).  If odd frame.. just draw the primary cursor

    LDA cursor2                ; get secondary cursor
    ASL A                      ;  double it and put it in X for LUT indexing
    TAX
    LDA lut_EquipMenuCurs, X   ; get cursor X coord
    ORA #$04                   ; add (or really, OR) 4 to draw it a little to the right of the primary cursor
    BNE EquipMenuCurs_DoDraw   ; then draw it (always branches)

DrawEquipMenuCurs:
    LDA cursor                 ; get primary cursor
    ASL A                      ;  double it and put in X for LUT indexing
    TAX
    LDA lut_EquipMenuCurs, X   ; get cursor X coord... then draw

  EquipMenuCurs_DoDraw:
    STA spr_x                    ; record X coord
    LDA lut_EquipMenuCurs+1, X   ; then fetch
    STA spr_y                    ;    and record Y coord
    JMP DrawCursor               ; draw the cursor, and exit

  lut_EquipMenuCurs:
  .BYTE $40,$38,   $90,$38
  .BYTE $40,$48,   $90,$48
  .BYTE $40,$68,   $90,$68
  .BYTE $40,$78,   $90,$78
  .BYTE $40,$98,   $90,$98
  .BYTE $40,$A8,   $90,$A8
  .BYTE $40,$C8,   $90,$C8
  .BYTE $40,$D8,   $90,$D8


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Equip Menu Mode Cursor  [$BEE6 :: 0x3BEF6]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawEquipMenuModeCurs:
    LDX eq_modecurs          ; put mode cursor in X
    LDA @lut_CursorX, X       ; use it as an index to get x coord
    STA spr_x                ; set it
    LDA #$14
    STA spr_y                ; y coord fixed at $14
    JMP DrawCursor           ; draw the cursor and return

  @lut_CursorX:
     .BYTE $48,  $80,  $B8


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Update Equip Menu Mode Attrib [$BEF7 :: 0x3BF07]
;;
;;    This updates the attributes at the top of the nametable
;;  so that the currently selected "mode" (equip/trade/drop) is highlighted
;;  to use a different palette (palette 2 instead of the normal 3).
;;
;;    This is useless because palette 2 and 3 consist of the exact same colors.  Also,
;;  the attributes are only enough to change the first 4 letters (tiles) in the name, so
;;  in "Equip" only the "Equi" would be highlighed.  This could probably be corrected, though.
;;
;;    My guess is this is a hold over from the japanese version which probably had shorter
;;  titles up top, and thus could highlight them easier.  At any rate, in the US version,
;;  this routine is 100% completely worthless.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


UpdateEquipMenuModeAttrib:
    LDA eq_modecurs           ; get the mode cursor
    ASL A
    ASL A                     ; multiply it by 4 (4 bytes of attribute)
    TAX                       ; and stuff in X

    LDA #$C3
    JSR SetPPUAddrTo_23aa     ; Set PPU address to $23C3 (relevent attribute bytes)

    LDA @lut_Attr, X          ; copy over 4 bytes of attribute
    STA $2007
    LDA @lut_Attr+1, X
    STA $2007
    LDA @lut_Attr+2, X
    STA $2007
    LDA @lut_Attr+3, X
    STA $2007

    LDA #$00                  ; then reset the PPU address
    STA $2006
    STA $2006

    RTS                       ; and exit


                            ; all attribute changes made to row 1.
  @lut_Attr:                ; row and column number are 16x16 blocks, zero based.
 ;column:   7 6 - -   9 8 - -   B A - -   D C - -
    .BYTE %10101111,%11111111,%11111111,%11111111  ; highlight columns 6,7
    .BYTE %11111111,%10111111,%11101111,%11111111  ; highlight columns 9,A
    .BYTE %11111111,%11111111,%11111111,%10101111  ; highlight columns C,D


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  SetPPUAddrTo_23aa  [$BF2E :: 0x3BF3E]
;;
;;     Itty bitty support routine.  Just sets the PPU address to $23xx where 'xx' is the value
;;  in A upon call.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SetPPUAddrTo_23aa:
    BIT $2002       ; clear PPU toggle
    LDY #$23
    STY $2006       ; set PPU address
    STA $2006
    RTS             ; and exit



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Unused Phantom Code  [$BF3A :: 0x3BF4A]
;;
;;     Looks like this is remnants of code that used to exist, or just didn't
;;  get overwritten by the assembler for whatever reason.  If you'll notice, it's
;;  an echo of the above LUT from UpdateEquipMenuModeAttrib and the code
;;  from SetPPUAddrTo_23aa.  It's as if the assembler pulled all that code back a few
;;  bytes and just didn't overwrite this with zeros.
;;
;;     At any rate... this is totally unused and is included here only for
;;  completeness sake.


    .BYTE           %10111111,%11101111,%11111111
    .BYTE %11111111,%11111111,%11111111,%10101111

    BIT $2002
    LDY #$23
    STY $2006
    STA $2006
    RTS

; this is just more unused junk
  .BYTE $00,$00,$00



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Weapon Permissions LUT   [$BF50 :: 0x3BF60]
;;
;;    Weapon permissions.  Each word corresponds to the equip permissions for
;;  a weapon.  See lut_ClassEquipBit for which bit corresponds to which class.
;;  Note again that bit set = weapon CANNOT be equipped by that class.

lut_WeaponPermissions:
  .WORD   $0DE7,$028A,$0400,$02CB,$074D,   $06CB,$07CF,$02CB,$0DE7,$028A
  .WORD   $05C7,$02CB,$06CB,$07CF,$02CB,   $028A,$06CB,$074D,$07CF,$06CB
  .WORD   $06CB,$02CB,$06CB,$06CB,$02CB,   $06CB,$02CB,$0504,$07CF,$0F6D
  .WORD   $0FAE,$0FCB,$0FFE,$0FCB,$0FCA,   $0FCD,$0FCB,$0FEF,$0FDF,$0000

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Armor Permissions LUT  [$BFA0 :: 0x3BFB0]
;;
;;    Same deal as above, only for the armor instead of the weapons.

lut_ArmorPermissions:
  .WORD   $0000,$00C3,$06CB,$07CF,$07DF,   $06CB,$07CF,$07CF,$0FDF,$0FDF
  .WORD   $0000,$0000,$0000,$0000,$0FFD,   $0FFE,$07CF,$07CF,$07CF,$07CF
  .WORD   $07CF,$0FDF,$0FDF,$02CB,$0208,   $0000,$07CF,$07CF,$07CF,$0FDF
  .WORD   $0FCF,$0000,$0000,$07CF,$07CF,   $07CB,$0FCB,$07CB,$0FDF,$0000



;; unused bytes  [$BFF0 :: 0x3C000]

  .BYTE   $00,$00,$00,$00,$00,$00,$00,$00,    $00,$00,$00,$00,$00,$00,$00,$00

