.include "variables.inc"
.include "macros.inc"
.include "Constants.inc"

.export lut_BattlePalettes, BankC_CrossBankJumpList

.import GameStart_L

.import CallMusicPlay_L
.import SwapBtlTmpBytes_L, FormatBattleString_L, BattleScreenShake_L, DrawBattleMagicBox_L, BattleRNG_L, BattleWaitForVBlank_L
.import DrawCombatBox_L, DrawBattleItemBox_L, DrawDrinkBox_L, UndrawNBattleBlocks_L, DrawCommandBox_L, DrawRosterBox_L
.import BattleCrossPageJump_L, WaitForVBlank_L, ClearBattleMessageBuffer_L
.import BattleOver_ProcessResult_L

BANK_THIS = $0C

.segment "BANK_0C"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Weapon data!  [$8000 :: 0x30010]
;;
;;  8 bytes per weapon, 40 weapons ($140 bytes total)
;;
;;  byte 0:  Hit rate
;;  byte 1:  Damage
;;  byte 2:  Critial rate (BUGGED - not used .. see LoadOneCharacterIBStats for offending code)
;;  byte 3:  Spell cast
;;  byte 4:  Element
;;  byte 5:  Category effectiveness (Giant/Undead/etc)
;;  byte 6:  graphic
;;  byte 7:  palette

lut_WeaponData:
    .INCBIN "bin/0C_8000_weapondata.bin"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Armor data!  [$8140 :: 0x30150]
;;
;;  4 bytes per weapon, 40 weapons ($A0 bytes total)
;;
;;  byte 0:  Evade penality
;;  byte 1:  Absorb boost
;;  byte 2:  Elemental defense
;;  byte 3:  Spell cast

lut_ArmorData:
    .INCBIN "bin/0C_8140_armordata.bin"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Magic data!  [$81E0 :: 0x301F0]
;;
;;  8 bytes per magic spell:
;;      $40 spells          ($200 bytes)
;;      $02 potions         ($10 bytes)
;;      $1A enemy attacks   ($D0 bytes)
;;  = $2E0 bytes total
;;
;;    see MAGDATA constants in Constants.inc for layout description

lut_MagicData:
    .INCBIN "bin/0C_81E0_magicdata.bin"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Magic battle messages  [$84C0 :: 0x304D0]
;;
;;    1 bytes per magic spell, $40+$02+$1A spells+potions+attacks ($5C bytes total)
;;  Each entry indicates which battle message to print when the spell connects with its target.
;;  A value of 0 indicates no message should be printed.
;;
;;    The values themselves match BTLMSG_XXXX names in Constants.inc

lut_MagicBattleMessages:
  .BYTE $01, $00, $02, $03,  $00, $00, $05, $00,    $00, $00, $08, $03,  $00, $00, $0A, $0B     ; spells
  .BYTE $01, $00, $0C, $01,  $00, $0D, $00, $05,    $00, $0F, $10, $00,  $00, $12, $00, $00
  .BYTE $01, $4A, $00, $01,  $00, $4D, $4A, $0B,    $4A, $4A, $02, $03,  $00, $15, $16, $00
  .BYTE $18, $00, $19, $01,  $00, $00, $1B, $00,    $4A, $00, $1C, $1D,  $00, $1E, $1F, $15
  
  .BYTE $00, $00                                                                                ; potions
  
  .BYTE $00, $00, $00, $00,  $00, $00, $16, $15,    $00, $1F, $00, $00,  $00, $00, $4D, $00     ; enemy attacks
  .BYTE $00, $00, $15, $00,  $00, $00, $00, $00,    $00, $00


  ; unused padding:
  .BYTE $00, $00, $00, $00
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Data for enemy stats [$8520 :: 0x30530]
;;
;;  $80 Enemies * $14 bytes per enemy
;;    = $A00 bytes of data
;;
;;  See ENROMSTAT_xxx constants in Constants.inc for layout

data_EnemyStats:
  .INCBIN "bin/0C_8520_enemydata.bin"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  LUT for Battle Palettes [$8F20 :: 0x30F30]
;;
;;    LUT of 64 4-byte palettes for use with battle formations

lut_BattlePalettes:
  .INCBIN "bin/0C_8F20_battlepalettes.bin"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  LUT for Enemy AI [$9020 :: 0x31030]
;;
;;    $10 bytes per AI
;;
;;  byte      0 = chance to cast spell         ($00-80)
;;  byte      1 = chance to use special attack ($00-80)
;;  bytes   2-9 = magic spells available.  Each entry 0-based.  Or 'FF' to mark end of spells
;;  bytes $B-$E = special attacks (0 based), or 'FF' to mark end of attacks

lut_EnemyAi:
  .INCBIN "bin/0C_9020_aidata.bin"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ExitBattle  [$92E0 :: 0x312F0]
;;
;;    Called when battle is over.  Fades out and does other end-of-battle stuff.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ExitBattle:
    JSR BattleFadeOut               ; Fade to black
    
    LDA btl_result                  ; check battle result
    CMP #$FF                        ; if not $FF...
    BNE WaitFrames_BattleResult_RTS ; ...just jump to a nearby RTS
    
    LDA #120                        ; otherwise, wait 120 frames (2 seconds)
    STA btl_result                  ;  before exiting
    BNE WaitFrames_BattleResult
    
    
; Unused space -- looks like old code from above ExitBattle routine -- like from a previous assembly
;   that didn't get overwritten
  .BYTE $A0                     ; ?
  .BYTE $A9, $78                ; LDA #$78
  .BYTE $8D, $86, $6B           ; STA btl_result
  .BYTE $D0, $29                ; BNE somewhere (WaitFrames_BattleResult?)
  .BYTE $F2, $38, $60, $20      ; ?
  
  
.align $100

BankC_CrossBankJumpList:

ExitBattle_L:                   JMP ExitBattle                  ; $9000
BattleFadeOutAndRestartGame_L:  JMP BattleFadeOutAndRestartGame ; $9003
FinishBattlePrepAndFadeIn_L:                                    ; $9006

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  FinishBattlePrepAndFadeIn  [$9306 :: 0x31316]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FinishBattlePrepAndFadeIn:
    JSR LoadEnemyStats              ; this is all pretty self explanitory
    
    LDA #$08                        ; enable BG drawing, but not sprites
    STA btl_soft2000
    LDA #$0E
    STA btl_soft2001
    
    JSR ClearBattleMessageBuffer_L
    JSR BattleFadeIn
    
    LDA #$1E
    STA btl_soft2001                ; enable BG and sprites
    
    JSR EnterBattlePrepareSprites
    JSR LoadAllCharacterIBStats
    JSR DrawCharacterStatus
    
    JMP Battle_AfterFadeIn
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  WaitFrames_BattleResult  [$932A :: 0x3133A]
;;
;;  input:  btl_result = number of frames to wait (zero'd on exit)
;;
;;    Will actually wait N+1 frames.  It's weird that btl_result is hijacked for
;;  this purpose, but whatever.
;;
;;  NOTE:  This routine will not update music!  Therefore it should only be called
;;    when music is stopped.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

WaitFrames_BattleResult:
    JSR WaitForVBlank_L     ; wait a frame
    
    LDA #$00                ; burn a bunch of CPU time -- presumably so that 
    : SEC                   ;  WaitForVBlank isn't called again so close to start of
      SBC #$01              ;  vblank.  I don't think this is actually necessary,
      BNE :-                ;  but it doesn't hurt.
      
    DEC btl_result          ; loop until counter has expired
    BNE WaitFrames_BattleResult
    
    SEC                     ; SEC before exit?  This seems to be pointless.
  WaitFrames_BattleResult_RTS:
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  CheckForEndOfBattle  [$933B :: 0x3134B]
;;
;;    Checks btl_result, and if nonzero, ends the battle.
;;  When ending the battle, this routine will NOT RTS, but will double-RTS,
;;  returning control to whoever called EnterBattle originally.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CheckForEndOfBattle:
    LDA btl_result
    BNE :+
      RTS                       ; if battle result is zero, do nothing -- keep battling!
      
  : CMP #$02                    ; if battle result == 2, the party is victorious
    BNE :+
      JSR PlayFanfareAndCheer   ; play fanfare music and do cheering animation
      
  : LDA #<BattleOver_ProcessResult_L    ; For all non-zero battle results, the battle is over
    STA btltmp+6                        ; Hand off control to another routine in bank B
    LDA #>BattleOver_ProcessResult_L
    STA btltmp+7
    
    LDA #$0B
    JMP BattleCrossPageJump_L
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BattleFadeOutAndRestartGame  [$9355 :: 0x31365]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BattleFadeOutAndRestartGame:
    JSR BattleFadeOut
    PLA                 ; drop the return address (this is pointless, because GameStart resets the
    PLA                 ;  stack pointer anyway)
    JMP GameStart_L     ; then jump to GameStart, which returns the user to the title screen.
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  SetCharacterBattleCommand  [$935D :: 0x3136D]
;;
;;  Sets the battle command for character 'btlcmd_curchar' to the following:
;;
;;  btl_charcmdbuf+0 = A
;;  btl_charcmdbuf+1 = X
;;  btl_charcmdbuf+2 = Y
;;
;;  Afterward, it will walk the character back to the right.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SetCharacterBattleCommand:
    PHA
    TYA
    PHA
    TXA
    PHA
    LDA btlcmd_curchar
    ASL A
    ASL A
    TAY
    PLA
    STA btl_charcmdbuf+1, Y     ; [1] = X
    PLA
    STA btl_charcmdbuf+2, Y     ; [2] = Y
    PLA
    STA btl_charcmdbuf, Y       ; [0] = A
    
    LDA btlcmd_curchar
    JMP CharWalkAnimationRight  ; Then walk the char back to the right
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  UndoCharacterBattleCommand  [$937A :: 0x3138A]
;;
;;    Undoes a character's input battle command.  This is called when the player
;;  pressed B in the battle menu to move back to a previous character.
;;  This routine is really only significant for 2 things:
;;
;;  1) The DRINK command will dec a potion counter.  If the player moves back to undo
;;     that drink, the potion counter needs to be incremented back.
;;
;;  2) We might not necessarily want to go back to the previous character, since the
;;     previous character might be sleeping/stunned/dead/stone.  Therefore this
;;     routine will backtrack to the next "able-bodied" character.
;;
;;    After this routine, btlcmd_curchar will be 1+ the desired character to backtrack to,
;;  so future code can merely subtract 1 from this to get the desired character.  Or...
;;  btlcmd_curchar will be 0 if you can't backtrack any more.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
UndoCharacterBattleCommand:
    LDA btlcmd_curchar                  ; if we're on character 0, we can't back up any more
    BEQ @UndoPrevCharDrink              ;   so skip ahead
    
    ; Otherwise, try to back up more...
    SEC                                 ; get the prev char's stats
    SBC #$01
    JSR PrepCharStatPointers
    LDY # ch_ailments - ch_stats
    
    LDA (btl_ob_charstat_ptr), Y        ; see if they have ailments that would stop them from doing anything
    AND #AIL_DEAD | AIL_STONE | AIL_STUN | AIL_SLEEP
    BEQ @UndoPrevCharDrink              ; if they are able to input a command
    
    DEC btlcmd_curchar                  ; if they can't, dec curchar to go to the prev character
    JMP UndoCharacterBattleCommand      ;  and try again
    
  @UndoPrevCharDrink:
    LDY btlcmd_curchar                  ; get the current char
    BEQ :+                              ; if nonzero, DEC it to move back to the prev char
      DEY
      
    ; Then see if this char originally wanted to drink a potion.  If they did, we DEC'd a potion QTY counter
    ;   which we need to INC now to undo that.
  : LDA btl_charcmdconsumetype, Y
    CMP #$02                ; will be 02 for DRINK commands
    BEQ :+
      BNE @Done             ; if they didn't DRINK, just exit
      
  : LDX btl_charcmdconsumeid, Y     ; if they did, get the index of the potion they drank
    INC btl_potion_heal, X          ; INC the potion counter
    LDA #$00
    STA btl_charcmdconsumetype, Y   ; then clear the consumable marker.
    
  @Done:
    RTS
    
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Battle_AfterFadeIn  [$93AE :: 0x313BE]
;;
;;    Called after battle has faded in for the first time (shortly after entry).
;;  It actually starts doing actual battle stuff!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Battle_AfterFadeIn:
    LDA item_heal           ; prep potion counts
    STA btl_potion_heal
    LDA item_pure
    STA btl_potion_pure
    
    LDY #$1C
    LDA #$00
    : STA btl_charcmdbuf-1, Y   ; zero the player command buffer
      DEY
      BNE :-
      
    LDA #$00                    ; zero a bunch of misc vars to prep them
    STA btl_combatboxcount
    STA btl_strikingfirst
    STA btl_attackid
    
    LDA btlform_norun
    AND #$01                ; see if "no run" bit is set.  If it is, there is no strike first/surprised check
    BNE BattleLogicLoop
    
    ; This block of code does some math to see if the party will be surprised or
    ;   get a first strike in the battle.  The end result of this math is:
    ; 
    ; S = (leaders_agility + leaders_luck) / 8
    ; V = random[S+S, S+100] - surpised_rate
    ;
    ; if V <  11, party is surprised  (possibly BUGGED, since 0 is a valid value, should this be 10 instead of 11?)
    ; if V >= 90, party strikes first
    ; otherwise, normal fight
    
    LDA #$00
    STA btl_mathbuf+1   ; clear high byte of mathbuf0
    
    LDA ch_agil
    CLC
    ADC ch_luck
    LSR A
    LSR A
    LSR A
    STA btl_mathbuf     ; mathbuf0 = (agil+luck)/8   -- party leader's stats only
    
    LDX #100
    JSR RandAX          ; random value between that and 100
    TAX
    
    LDA #$00
    JSR MathBuf_Add     ; add random value, effectively creating range of [2*stat, stat+100], where 'stat' is above agil+luck value
    LDX btlform_surprise; get surprise rate
    LDA #$00
    JSR MathBuf_Sub     ; subtracts surprise rate from that value
    
    LDY btl_mathbuf         ; put value in XY
    LDX btl_mathbuf+1
    JSR ZeroXYIfNegative    ; clip at 0 (pointless, because MathBuf_Sub already does this)
    
    TYA                     ; drop high byte and check low byte
    CMP #11
    BCC @Surprised          ; if < 11, SURPRISED!
    CMP #90
    BCC BattleLogicLoop     ; if < 90, normal fight
    
      ; otherwise (>= 90), STRIKE FIRST
      LDA #BTLMSG_STRIKEFIRST
      JSR DrawBtlMsg_ClearCombatBoxes   ; draw strike first message
      INC btl_strikingfirst             ; set flag
      JMP BattleLogicLoop               ; and jump ahead to logic loop
    
    ; surprised code
  @Surprised:
    LDA #BTLMSG_SURPRISED
    JSR DrawBtlMsg_ClearCombatBoxes
    JMP BattleLogicLoop_DoCombat        ; skip over first round of player input, and just do combat
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BattleLogicLoop  [$9420 :: 0x31430]
;;
;;    The main loop for battle.  Get player commands, then perform combat.
;;  Rinse and Repeat until battle is over.
;;
;;    When the party is surprised, the game will jump to an alternate entry point,
;;  skipping player commands for the first round of combat.  Note that while this means party members
;;  can't attack or do other commands, they *DO* still get a turn during combat.  This turn is actually
;;  used if the character becomes stunned or falls asleep.
;;
;;    So if you are surpised by Geists or something, and one of your chars gets stunned, he might
;;  still un-stun in the first round, even though you were surprised.  One might consider this
;;  to be BUGGED.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BattleLogicLoop:
    JSR RebuildEnemyRoster          ; Rebuild the roster in case some enemies died
    JSR DrawRosterBox_L             ; Draw the roster box
    JSR DrawCommandBox_L            ; and the command box
    JSR UpdateSprites_BattleFrame   ; then do a frame with updated battle sprites
    
    LDY #$1C
    LDA #$00
    STA btl_attackid                ; clear attack id?  This seems very strange to do here...
    : STA btl_charcmdbuf-1, Y       ; clear the character battle command buffer
      DEY
      BNE :-
      
    ;;   Get input commands for each of the 4 characters.  This is done kind of confusingly,
    ;; since the player can undo/backtrack by pressing B.  This code does not necessarily
    ;; flow linearly.
    ;;
    ;;   If the user backtracks, instead of RTS'ing, GetCharacterBattleCommand will drop
    ;; its return address and jump back to one of these 'Backtrack' labels.  See
    ;; GetCharacterBattleCommand for more details
    
                                __GetCharacterBattleCommand_Backtrack_0:
    LDA #$00
    JSR GetCharacterBattleCommand
                                __GetCharacterBattleCommand_Backtrack_1:
    LDA #$01
    JSR GetCharacterBattleCommand
                                __GetCharacterBattleCommand_Backtrack_2:
    LDA #$02
    JSR GetCharacterBattleCommand

    LDA #$03
    JSR GetCharacterBattleCommand
    
    ;; ----
    ;;  Once all commands are input
    
    LDA #$02                    ; undraw the Roster and Battle menu battle blocks.
    JSR UndrawNBattleBlocks_L
    
    ; And then do the actual combat!
  BattleLogicLoop_DoCombat:     ; alternative entry point for when the party is surprised
    JSR DoBattleRound
    JSR CheckForEndOfBattle
    JMP BattleLogicLoop
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BacktrackBattleCommand  [$945B :: 0x3146B]
;;
;;    Called when the user pressed B to move to a previous character's commands.  This does
;;  the weird non-linear jumping around bullshit described in GetCharacterBattleCommand.
;;  See that routine for details.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BacktrackBattleCommand:
    LDA btlcmd_curchar          ; use cur char as index to our jump table, so we know where to backtrack to.
    ASL A
    TAY
    LDA @JumpTable, Y
    STA $88
    LDA @JumpTable+1, Y
    STA $89                     ; $88,89 is where to jump back to
    
    PLA                         ; but first, pull and discard the old return address
    PLA
    JMP ($0088)
    
  @JumpTable:
  .WORD __GetCharacterBattleCommand_Backtrack_0     ; character 0 can't backtrack, so do char 0 again
  .WORD __GetCharacterBattleCommand_Backtrack_0     ; char 1 backtracks to char 0
  .WORD __GetCharacterBattleCommand_Backtrack_1     ; char 2 backtracks to char 1
  .WORD __GetCharacterBattleCommand_Backtrack_2     ; char 3 backtracks to char 2
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  GetCharacterBattleCommand  [$9477 :: 0x31487]
;;
;;  input:   A = the character whose commands to get
;;  output:  btl_charcmdbuf is filled appropriately
;;
;;    This routine will acquire and record the battle commands for a single character.
;;
;;  NOTE!:
;;    This routine is strange and may not actually RTS out.  Instead, if the player presses
;;  'B' in the battle menu to move back to a previous character, the return address is dropped
;;  and instead, this routine will jump back to the middle of the BattleLogicLoop.
;;
;;    This is terrible design, IMO, as there are dozens of better ways to do this, but whatever.
;;  Ultimately this means that this routine cannot be safely called from anywhere other than
;;  BattleLogicLoop.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GetCharacterBattleCommand:
    STA btlcmd_curchar              ; record the character
    LDA btlcmd_curchar              ; <- (pointless)
    JSR PrepCharStatPointers        ; Prep the stat pointers (this persists through all the sub menus)
    
    LDY # ch_ailments - ch_stats    ; See if this character has any ailment that would prevent them from inputting
    LDA (btl_ob_charstat_ptr), Y    ;   any commands
    AND # AIL_DEAD | AIL_STONE | AIL_STUN | AIL_SLEEP
    BEQ InputCharacterBattleCommand ; If they can, jump to routine to get input from menus
    
      AND # AIL_DEAD | AIL_STONE    ; if they are unable to input commands.. then isolate the Dead/Stone bits
      PHA
      LDA btlcmd_curchar
      ASL A
      ASL A
      TAY
      PLA
      STA btl_charcmdbuf, Y         ; And store dead/stone bits in the command buffer (seems strange to do this to me
      RTS                           ;   but whatever)
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  InputCharacterBattleCommand  [$9496 :: 0x314A6]
;;
;;     Gets the character battle command from user input.  This is called
;;  by GetCharacterBattleCommand if the character in question is able to
;;  input commands.  See that routine for details
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
InputCharacterBattleCommand:
    LDA btlcmd_curchar
    JSR CharWalkAnimationLeft                   ; walk this character left
    JSR PrepAndGetBattleMainCommand             ; get the main menu command
    CMP #$02
    BNE Battle_MainMenu_APressed                ; If they pressed A, jump ahead to get their sub-menu selection
    
    ; if B pressed in main menu
    LDA btlcmd_curchar
    JSR CharWalkAnimationRight                  ; walk this character back to the right
    JSR UndoCharacterBattleCommand              ; undo the previously input command
    JMP BacktrackBattleCommand                  ; and backtrack to previous character.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  CancelBattleAction  [$94AF :: 0x314BF]
;;
;;    Called when the player presses B to back out of a sub menu, resulting in the
;;  on-screen character walking back to the right, then back to the left, and allowing
;;  the player to input a different command.
;;
;;    The walk right then left thing is strange, and this routine could very easily have
;;  Just kept the character to the left.  But whatever.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CancelBattleAction:
    LDA btlcmd_curchar
    JSR CharWalkAnimationRight      ; walk the character back to the right
    JMP InputCharacterBattleCommand ; then jump back to restart the battle action
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Battle_MainMenu_APressed  [$94B8 :: 0x314C8]
;;
;;     This subroutine is jumped to when A is pressed to select something on
;;  the main menu.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Battle_MainMenu_APressed:
    LDA btlcurs_x               ; check which column they're in
    AND #$01
    BEQ :+                      ; if in the right column (RUN)
      LDA btlcmd_curchar
      ORA #$80                  ; get character index, put in Y
      TAY
      LDA #$20
      JMP SetCharacterBattleCommand     ; command '20 ?? TT' for running, where 'TT' is character running and ?? is unused.
    
    ; Otherwise, A was pressed in the left column
  : LDA btlcurs_y               ; use the selected row to index the sub menu jump table
    AND #$03
    ASL A
    TAY
    LDA lut_BattleSubMenu, Y
    STA $88
    LDA lut_BattleSubMenu+1, Y
    STA $89
    JMP ($0088)                 ; then jump to the appropriate sub menu logic

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  lut_BattleSubMenu  [$94DE :: 0x314EE]
;;
;;    Jump table for battle submenus.  Called when the player selects one of the FIGHT/MAGIC/DRINK/ITEM
;;  options on the main battle screen.

lut_BattleSubMenu:
  .WORD BattleSubMenu_Fight
  .WORD BattleSubMenu_Magic
  .WORD BattleSubMenu_Drink
  .WORD BattleSubMenu_Item

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BattleSubMenu_Fight  [$94E6 :: 0x314F6]
;;
;;  Called when the player selects 'FIGHT'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BattleSubMenu_Fight:
    JSR SelectEnemyTarget           ; Pick a target
    CMP #$02
    BNE :+                          ; If they pressed B....
      JMP CancelBattleAction        ; ... cancel
  : LDA #$04                        ; If they pressed A, record the command
    JMP SetCharacterBattleCommand   ; Command:   04 xx TT    attack enemy slot TT (xx appears to be unused)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BattleSubMenu_Magic  [$94F5 :: 0x31505]
;;
;;  Called when the player selects 'MAGIC'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BattleSubMenu_Magic:
    LDA btlcmd_curchar              ; <- This is never used
    JSR MenuSelection_Magic
    
    PHA                             ; backup A/B button press
    LDA #$01
    JSR UndrawNBattleBlocks_L       ; undraw the magic box
    PLA                             ; restore A/B state
    
    CMP #$02
    BNE :+                          ; if they pressed B to exit the menu box
      JMP CancelBattleAction        ;   then cancel this action
      
  : LDA btlcmd_curchar
    ASL A
    ASL A
    ASL A
    ASL A
    ASL A
    ASL A                           ; put usable char index in 68B3,4  (00,40,80,C0)
    STA $68B3                       ;  68B3 will eventually be index to the player's magic list
    STA $68B4                       ;  68B4 will eventually be index to the player's MP
    
    LDA $6AF8                       ; this was the "magic page".  So this will be 1 if the user selected a L5-8 spell, and 0 if L1-4 spell
    AND #$01
    ASL A
    ASL A
    PHA                             ; push Page*4
    ASL A
    ASL A
    CLC
    ADC $68B3
    STA $68B3                       ; 68B3 = CharIndex + Page*4*4.  (4 spells per level * 4 levels per page)
    
    PLA                             ; pull Page*4
    CLC
    ADC $68B4
    STA $68B4                       ; 68B4 = CharIndex + Page*4.  (4 levels per page)
    
    LDA btlcurs_y                   ; get Y position of their selection
    ASL A
    ASL A                           ; *4 (4 spells per level)
    CLC
    ADC btlcurs_x                   ; + X position
    CLC
    ADC $68B3                       ; add to 68B3.  index is now complete:  Index from start of char's spell list, to their chosen spell.
    
    TAY                             ; put that index in Y, and use it to get the chosen spell
    LDA ch_spells, Y
    BNE :+                          ; if they selected an empty slot, do the @NothingBox -- otherwise skip over it
    
  @NothingBox:
      JSR DoNothingMessageBox       ; Show the nothing box
      JMP BattleSubMenu_Magic       ; and redo the magic selection submenu from the beginning
      
  : STA $6B7D                       ; store spell in 6B7D
    LDA btlcurs_y                   ; get Y selection (effectively the spell level)
    AND #$03
    CLC
    ADC $68B4                       ; add to 68B4.  This index is now complete:  Index from start of char's MP to the level of their chosen spell
    
    TAX
    LDA ch_curmp, X                 ; use that index to get their MP for this level
    BEQ @NothingBox                 ; if no more MP for this level, cut to "nothing" box and repeat.
    
    LDY btlcmd_curchar
    LDA #$01
    STA btl_charcmdconsumetype, Y   ; put 01 as consumable type (to indicate magic)
    TXA
    STA btl_charcmdconsumeid, Y     ; put the spell level as the consumable ID
    
    DEC $6B7D                       ; dec index of selected spell (make it 0 based instead of 1 based)
    LDA $6B7D
    JSR GetPointerToMagicData
    STA $80                         ; $80,81 points to this spell's data
    STX $81
    
    LDY #$05
    LDA ($80), Y
    STA $68B3                       ; put magic graphic at 68B3 (temp)
    
    LDY #$06
    LDA ($80), Y
    STA $68B4                       ; put magic palette at 68B4 (temp)
    
    LDA btlcmd_curchar
    ASL A
    TAY                             ; use 2*char as index for btlcmd_magicgfx
    
    LDA $68B3
    STA btlcmd_magicgfx, Y          ; record magic graphic
    LDA $68B4
    STA btlcmd_magicgfx+1, Y        ; and magic palette
    
    LDY #$03
    LDA ($80), Y                    ; get the target for this spell (stored as semi-flags:  01,02,04,08,10,20 are valid values)
    
  @CheckTarget_01:
    LSR A                           ; shift out low bit
    BCC @CheckTarget_02             ; if set (target=01 -- target all enemies)...
      LDY #$FF
      LDA #$40                      ;  command = 40 xx FF  (where xx = spell index)
      LDX $6B7D
      JMP SetCharacterBattleCommand ; set command and exit
  
  @CheckTarget_02:                  ; target 02 = target one enemy
    LSR A
    BCC @CheckTarget_04
      JSR SelectEnemyTarget         ; puts target in Y
      CMP #$02
      BNE :+                        ; if they pressed B to exit
        JMP BattleSubMenu_Magic     ; redo magic submenu from the beginnning
    : LDA #$40
      LDX $6B7D
      JMP SetCharacterBattleCommand ; command = 40 xx TT  (xx = spell, TT = enemy target)
    
  @CheckTarget_04:                  ; target 04 = target self
    LSR A
    BCC @CheckTarget_08     
      LDA btlcmd_curchar            ; use cur char
      ORA #$80                      ; OR with 80 to indicate targetting a player character
      TAY
      LDA #$40
      LDX $6B7D
      JMP SetCharacterBattleCommand
    
  @CheckTarget_08:                  ; target 08 = target whole party
    LSR A
    BCC @Target_10
      LDA #$40
      LDY #$FE                      ; 'FE' targets party
      LDX $6B7D
      JMP SetCharacterBattleCommand ; 40 xx FE

  @Target_10:                       ; target 10 = target one player
    LDA btlcmd_curchar
    JSR SelectPlayerTarget          ; get a player target?
    CMP #$02                        ; did they press B to exit
    BNE :+
      JMP BattleSubMenu_Magic       ; if yes, jump back to magic submenu
  : LDA btlcurs_y
    AND #$03
    ORA #$80                        ; otherwise, put the player target in Y
    TAY
    LDA #$40
    LDX $6B7D
    JMP SetCharacterBattleCommand   ; 40 xx TT

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BattleSubMenu_Drink  [$95F5 :: 0x31605]
;;
;;  Called when the player selects 'DRINK'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BattleSubMenu_Drink:
    LDA btl_potion_heal         ; see if there are any Heal/Pure potions remaining
    ORA btl_potion_pure
    BNE :+                      ; if there are no potions...
      JSR DoNothingMessageBox   ; show the 'Nothing' box
      JMP CancelBattleAction    ; then cancel
    
  : JSR DrawDrinkBox_L          ; otherwise (have at least 1 potion), draw the drink box
  
    JSR MenuSelection_Drink     ; get menu selection from the player  
    PHA                         ; backup the A/B button press
    LDA #$01
    JSR UndrawNBattleBlocks_L   ; undraw the drink menu
    PLA
    CMP #$02
    BNE :+                      ; was B pressed to get out of the drink menu?
      JMP CancelBattleAction    ;   if yes, cancel the action
      
  : LDA btlcurs_y               ; see if they selected Heal or Pure potion
    AND #$01
    STA $6B7D                   ; stick it in temp mem (0=Heal, 1=Pure)
    BNE @PureSelected           ; see which they selected?  If heal....
      LDA btl_potion_heal       ;   ... make sure they have at least 1 heal potion
      BNE @HealOK               ; if they don't
      JSR DoNothingMessageBox   ;   show the "nothing" box and cancel
      JMP CancelBattleAction
    
  @PureSelected:                ; if pure selected, make sure they have a Pure potion
    LDA btl_potion_pure
    BNE :+                      ; if none, show "nothing" and cancel
      JSR DoNothingMessageBox
      JMP CancelBattleAction
      
  : LDA #$01
    STA $6856                   ; store menu selection for pure potion in temp mem $6856
    JMP @GetTarget              ;  (this means both 6856 AND 6B7D both contain the item being used -- why both?)
  @HealOK:
    LDA #$00                    ; for heal potion
    STA $6856

  @GetTarget:
    LDA btlcmd_curchar          ; get input for SelectPlayerTarget
    JSR SelectPlayerTarget      ; and call it!
    CMP #$02
    BNE :+                      ; if they pressed B...
      JMP BattleSubMenu_Drink   ; ... return to the Drink sub menu
      
  : LDY btlcmd_curchar
    LDA #$02
    STA btl_charcmdconsumetype, Y   ; store 02 as the consumable type (to indicate DRINK)
    LDA $6856
    STA btl_charcmdconsumeid, Y     ; store menu selection as consumed ID -- to indicate which potion  (00/01 for Heal/Pure potion)
    
    LDX $6856               ; get menu selection
    DEC btl_potion_heal, X  ; remove the item from the qty
    
    LDA btlcurs_y           ; get target
    AND #$03
    ORA #$80                ; OR with 80 to indicate it's a player target
    TAY                     ; put in Y (for SetCharacterBattleCommand)
    
    LDA $6B7D               ; get the menu selection
    CLC
    ADC #$40                ; + $40.
    TAX                     ; X=40 for heal / 41 for pure
    LDA #$08
    JMP SetCharacterBattleCommand

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BattleSubMenu_Item  [$9679 :: 0x31689]
;;
;;  Called when the player selects 'ITEM'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BattleSubMenu_Item:
    LDA #$00            ; Check the 8 equipment slots and make sure there's at least 1 item
    LDX #$08
    LDY #ch_weapons - ch_stats
    : ORA (btl_ob_charstat_ptr), Y  ; OR all equipment together to see if any are nonzero
      INY
      DEX
      BNE :-
      
    AND #$FF                        ; update Z flag to see if all slots were zero
    BNE :+                          ; if all slots were 0 (no items), flow into @NothingBox, otherwise skip ahead
    
  @NothingBox:
      JSR DoNothingMessageBox       ; Show the "nothing" box
      JMP CancelBattleAction        ; And cancel this battle action
      
  : JSR DrawBattleItemBox_L         ; Draw the item box
    JSR MenuSelection_Item          ; and run the logic for selecting an item
    
    PHA                             ; backup A/B state
    LDA #$01
    JSR UndrawNBattleBlocks_L       ; undraw the item box
    PLA                             ; restore A/B state
    
    CMP #$02
    BNE :+                          ; if B pressed
      JMP CancelBattleAction        ; cancel action
      
  : LDA btlcurs_x               ; Selected column
    AND #$01
    STA $88     ; (selected column stored in $88 for later)
    ASL A                       ;  * 4
    ASL A
    STA $68B3
    LDA btlcurs_y               ;  + Selected row
    AND #$03
    CLC
    ADC $68B3                   ;  = equip slot of selected item
    
    ADC #ch_weapons - ch_stats  ; + offset for character equipment = index for ob stats
    TAY
    LDA (btl_ob_charstat_ptr), Y; Get the selected equipment
    AND #$7F                    ; mask off the 'equipped' bit
    BEQ @NothingBox             ; if zero, print nothing box and exit
    
    STA $89                     ; if nonzero, stick it in $89
    DEC $89                     ; convert from 1-based to 0-based
    
    LDA $88                     ; get the selected column (0=weapon, 1=armor)
    BNE @GetArmorSpell          ; if armor selected jump ahead
    
    LDA $89                     ; otherwise weapon selected, get the index
    LDY btlcmd_curchar
    CLC
    ADC #TCITYPE_WEPSTART       ; convert from weapon index to item index
    STA btl_charcmditem, Y      ; record it in command buffer as item being used
    
    LDA $89                     ; get weapon index
    JSR GetPointerToWeaponData  ; get a pointer to that weapon's data (in XA)
    JMP @GetSpellCast

  @GetArmorSpell:
    LDA $89                     ; armor index
    LDY btlcmd_curchar
    CLC
    ADC #TCITYPE_ARMSTART       ; convert to item index
    STA btl_charcmditem, Y      ; record as item being used
    
    LDA $89
    JSR GetPointerToArmorData   ; pointer to armor data in XA
    
  @GetSpellCast:
    STA $88                     ; store pointer to wep/armor data in $88,89
    STX $89
    LDY #$03                    ; Y=3 to index, since byte 3 in both wep/armor data is the "spell cast" entry
    LDA ($88), Y                ; get spell cast
    
    TAX                             ; put it in X
    DEX                             ; DEX to make it 0-based (FF becomes "no spell")
    LDY btlcmd_curchar              ; Y=cur char -- default to targetting yourself
    LDA #$10
    JMP SetCharacterBattleCommand

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DoNothingMessageBox  [$96FE :: 0x3170E]
;;
;;    Draw the "Nothing" message box that appears when you select an empty
;;  menu.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DoNothingMessageBox:
    LDA #$04                    ; Draw the "Nothing" combat box
    LDX #<data_NothingText
    LDY #>data_NothingText
    JSR DrawCombatBox_L
    
  @InputLoop:                   ; Wait for the player to provide
      JSR DoFrame_WithInput     ;   ANY input
    BEQ @InputLoop
    
    LDA #$01                    ; then undraw the "Nothing" box and exit
    JMP UndrawNBattleBlocks_L
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  GetPointerToMagicData  [$9711 :: 0x31721]
;;
;;  in:    A = magic index
;;  out:  XA = pointer to that magic spell's data
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GetPointerToMagicData:
    LDX #$08                ; *8 (8 bytes per spell)
    JSR MultiplyXA
    CLC
    ADC #<lut_MagicData     ; low byte
    STA $68B3
    TXA
    ADC #>lut_MagicData     ; high byte in X
    TAX
    LDA $68B3               ; low byte in A
    RTS
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  EnterBattlePrepareSprites  [$9724 :: 0x31734]
;;
;;  Prepare the battle sprites!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

EnterBattlePrepareSprites:
    LDA #$00
    STA btl_drawflagsA      ; clear drawflags
    STA btl_drawflagsB
    
    JSR BattleClearOAM      ; clear shadow OAM
    JSR BattleFrame         ; do a frame with oam updated to clear actual oam)
    JSR BattleUpdatePPU     ; reset scroll and stuffs
    
    LDA #$00
    STA btl_msgdraw_blockcount  ; clear the block count
    STA $6AF8                   ; ??
    STA btlattackspr_nodraw                   ; ??
    
    ;;  Prep all the character drawing stuff
    LDA #$B0
    STA btl_chardraw_x + $0     ; put all characters in the $B0 column
    STA btl_chardraw_x + $4
    STA btl_chardraw_x + $8
    STA btl_chardraw_x + $C
    
    LDA #$30                    ; set Y coords, starting at $30, and increasing by $18
    STA btl_chardraw_y + $0
    LDA #$48
    STA btl_chardraw_y + $4
    LDA #$60
    STA btl_chardraw_y + $8
    LDA #$78
    STA btl_chardraw_y + $C
    
    LDA #$00                    ; graphics sets are pretty much fixed -- it's effectively just ID<<5
    STA btl_chardraw_gfxset + $0
    LDA #$20
    STA btl_chardraw_gfxset + $4
    LDA #$40
    STA btl_chardraw_gfxset + $8
    LDA #$60
    STA btl_chardraw_gfxset + $C
    
  ; JMP SetAllNaturalPose       ; <- flow into
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  SetAllNaturalPose  [$9776 :: 0x31786]
;;
;;  Call SetNaturalPose for all characters
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SetAllNaturalPose:
    LDA #$00
    JSR SetNaturalPose
    LDA #$01
    JSR SetNaturalPose
    LDA #$02
    JSR SetNaturalPose
    LDA #$03
    JMP SetNaturalPose
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BattleUpdatePPU  [$978A :: 0x3179A]
;;
;;  Applies btl_soft2001 and resets the scroll
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BattleUpdatePPU:
    LDA btl_soft2001
    STA $2001               ; copy over soft2001
    LDA #$00
    STA $2005               ; reset scroll
    STA $2005
    RTS
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BattleFrame  [$9799 :: 0x317A9]
;;
;;  This does all the work you'd expect to be done in a typical frame.
;;  Except for maybe resetting the scroll
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BattleFrame:
    JSR BattleWaitForVBlank_L   ; Wait for VBlank 
    LDA $2002
    LDA #>oam
    STA $4014                   ; Do OAM DMA
    JMP BattleUpdateAudio       ; Update audio
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BattleClearOAM  [$97A7 :: 0x317B7]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BattleClearOAM:
    LDX #$00            ; flood fill OAM with $F0
    LDA #$F0
    : STA oam, X
      INX
      BNE :-
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BattleClearVariableSprite  [$97B2 :: 0x317C2]
;;
;;    Erases the Variable sprite (cursor/weapon/magic) from shadow OAM
;;  And clears the drawflags which would redraw them
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BattleClearVariableSprite:
    LDX #$00
    LDA #$F0
    : STA oam, X            ; clear first $10 bytes of shadow OAM  (the first 4 sprites)
      INX                   ;   (or 1 16x16 sprite)
      CPX #$10
      BNE :-
      
    LDA btl_drawflagsA      ; clear draw flags which draw those sprites
    AND #$0F
    STA btl_drawflagsA
    
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DoFrame_WithInput  [$97C7 :: 0x317D7]
;;
;;  Calls DoMiniFrame
;;  Also fetches controller input and returns it in A, and in btl_input
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DoFrame_WithInput:
    JSR BattleRNG_L     ; generate a number and throw it away (makes RNG less predictable -- sorta)
    
    LDY #$01            ; strobe the controllers
    STY $4016
    DEY
    STY $4016
    
    LDY #$08            ; loop 8 times (once for each button)
  @Loop:
    LDA $4016           ; get button
    LSR A               ; shift out low bit
    BCS :+              ; if clear, shift out another one
      LSR A             ;  (this captures detachable Famicom controllers which report in bit 1)
  : ROR btl_input       ; Roll button state into temp rarm
    DEY
    BNE @Loop           ; repeat for all buttons
    
    JSR DoMiniFrame     ; do a frame
    LDA btl_input       ; reload the controller state, now in A
    RTS
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BattleDraw8x8Sprite  [$97E9 :: 0x317F9]
;;
;;  Draws a single 8x8 sprite.  Or rather, just copies it to shadow oam to be drawn next frame
;;
;;  input:  all 'btl8x8spr' variables
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BattleDraw8x8Sprite:
    TYA
    PHA             ; backup Y (don't want to change it)
    
    LDA btl8x8spr_i ; get slot
    ASL A
    ASL A           ; *4 to use as index
    TAY
    
    LDA btl8x8spr_a ; copy values over
    STA oam_a, Y
    LDA btl8x8spr_t
    STA oam_t, Y
    LDA btl8x8spr_y
    STA oam_y, Y
    LDA btl8x8spr_x
    STA oam_x, Y
    
    PLA             ; restore Y
    TAY
    
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  UpdateSprites_BattleFrame  [$980C :: 0x3181C]
;;
;;  Draws all battle sprites
;;  Then does a frame.
;;
;;  Note that if magic is being drawn, this routine will actually do 5 frames instead of 1, because
;;     it does 4 additional frames for the background flash animation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

UpdateSprites_BattleFrame:
    LDA btlattackspr_nodraw
    BEQ :+                  ; If we are not to draw the attacksprite
      LDA btl_drawflagsA    ; clear all 'A' drawflags except for the 'dead' bits
      AND #$0F
      STA btl_drawflagsA
      
    ; This will update the dead bits in drawflagsA
    ;   and the stone bits in drawflagsB
  : LDA #$00
    STA $88     ; to hold new dead bits
    STA $89     ; to hold new dead bits
    TAX
    
  @ExtractDeadStoneBits:    ; loop 4 times, starting with character 3 down to character 0
      TXA                   ; subtract $40 to go to the prev character index
      SEC
      SBC #$40
      TAX
      
      LDA ch_ailments, X    ; get the ailments
      LSR A                 ; shift out dead bit
      ROL $88               ;   and into $88
      LSR A                 ; shift out stone bit
      ROL $89               ;   and into $89
      
      TXA                   ; Loop until we do character 0
      BNE @ExtractDeadStoneBits
    
    LDA btl_drawflagsA  ; move new dead bits into drawflagsA
    AND #$F0
    ORA $88
    STA btl_drawflagsA
    
    LDA btl_drawflagsB  ; move new stone bits into drawflagsB
    AND #$F0
    ORA $89
    STA btl_drawflagsB
    
    
    LDA btl_drawflagsA  ; see if we 'draw battle cursor' flag is set
    AND #$10
    BEQ @DrawChars      ; if not, skip drawing the cursor, and jump ahead to drawing characters
    
      ; Draw the battle cursor
      LDX btlcursspr_x  ; apply X coord
      STX btl8x8spr_x
      STX btl8x8spr_x+1
      LDY btlcursspr_y  ; apply Y coord
      STY btl8x8spr_y
      STY btl8x8spr_y+1
      LDA #$00          ; put cursor in highest-priority slot
      STA btl8x8spr_i
      LDA #$03          ; use palette 3 for cursor
      STA btl8x8spr_a
      LDA #$F0          ; use tile $F0
      STA btl8x8spr_t
      JSR Draw16x8SpriteRow ; draw 16x16 sprite
      JSR Draw16x8SpriteRow
    
  @DrawChars:
    LDA #$04            ; draw char 0 at oam 4
    STA btl8x8spr_i
    LDA #$00
    JSR DrawCharacter
    
    LDA #$0A            ; draw char 1 at oam 10
    STA btl8x8spr_i
    LDA #$01
    JSR DrawCharacter
    
    LDA #$10            ; draw char 2 at oam 16
    STA btl8x8spr_i
    LDA #$02
    JSR DrawCharacter
    
    LDA #$16            ; draw char 3 at oam 22
    STA btl8x8spr_i
    LDA #$03
    JSR DrawCharacter
    
  ; Draw Weapon Swing graphic
    LDA btl_drawflagsA
    AND #$20                ; only draw the weapon graphic if the appropriate draw flag is set
    BEQ @DrawMagic
    
      LDA #$00                  ; draw in oam slot 0
      STA btl8x8spr_i
      LDA btlattackspr_x        ; set X,Y position
      STA btl8x8spr_x
      STA btl8x8spr_x+1
      LDA btlattackspr_y
      STA btl8x8spr_y
      STA btl8x8spr_y+1
      LDX btlattackspr_pose     ; get pose (0 or 8 depending on whether or not to flip the graphic)
      JSR DrawWeaponGraphicRow  ; draw 2 rows of tiles.  No need to set tile here, as DrawWeaponGraphicRow
      JSR DrawWeaponGraphicRow  ;    takes care of that
    
    
    LDA btlattackspr_hidell     ; See if we need to hide the lower-left tile
    BEQ @DrawMagic
      LDA #$FF                  ; if set hide the lower-left tile of the weapon sprite
      STA oam_y+8               ;   by moving of to the bottom of the screen.
      LDA #$00
      STA btlattackspr_hidell   ; then clear that flag
    
  @DrawMagic:
    LDA btl_drawflagsA
    AND #$40
    BEQ @Done
      JSR DoMagicFlash          ; flash the background color
      
      LDA #$00                  ; put magic sprite at oam slot 0
      STA btl8x8spr_i
      
      LDA btlattackspr_x        ; set X,Y coords
      STA btl8x8spr_x+1
      LDA btlattackspr_y
      STA btl8x8spr_y
      STA btl8x8spr_y+1
      
      LDA #$02                  ; use palette 2
      STA btl8x8spr_a
      
      LDA btlattackspr_t        ; set tile
      CLC
      ADC btlattackspr_pose     ; add pose to adjust for magic animation
      STA btl8x8spr_t
      
      JSR Draw16x8SpriteRow     ; then just draw the 16x16 sprite
      JSR Draw16x8SpriteRow
     
  @Done:
    JSR BattleFrame             ; Lastly, do a frame
    LDA btl_drawflagsA          ; and clear drawflags (other than 'dead' bits)
    AND #$0F
    STA btl_drawflagsA
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DrawCharacter  [$9910 :: 0x31920]
;;
;;  Draws the given character sprite (to shadow OAM)
;;
;;  input:   A = character ID to draw (0,1,2 or 3)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawCharacter:
    PHA                     ; backup character ID
    
    TAY
    LDA btl_charattrib, Y
    STA btl8x8spr_a         ; assign attribute
    
    PLA                     ; restore, and put in X (why didn't it just put it in X initially?)
    TAX
    ASL A
    ASL A
    TAY                     ; *4 in Y.  Now it can be used as index for btl_chardraw stuff
    
    LDA btl_chardraw_x, Y   ; get X position
    STA btl8x8spr_x
    STA btl8x8spr_x+1       ; also store in temp as a backup
    
    LDA btl_chardraw_y, Y   ; Y position
    STA btl8x8spr_y
    STA btl8x8spr_y+1       ; also store in temp as a backup
    
    INX                     ; X is the char ID.  INX for purposes of below loop.
    LDA #$01                ; This will effectively set temp ram to be 1<<X
    STA $68B6               ; This seems incredibly stupid though, as this could more easily be accomplished
    : ASL $68B6             ; with a LUT of:  01,02,04,08
      DEX
      BNE :-
    LSR $68B6               ; <- at this point, $68B6 = 1<<characterID
    
    
    LDA btl_drawflagsA      ; See if the character is dead
    AND #$0F                ;   Not sure what 6AD1 actually holds, but low 4 bits seem to be a flag to indicate which
    AND $68B6               ;   characters should be drawn lying on the ground
    BEQ @DrawNotDead            
    
    ;;;;;;;;;;;;;;;
    ; If the character is to be drawn dead
    LDA #$1A
    CLC
    ADC btl_chardraw_gfxset, Y
    STA btl8x8spr_t             ; Tile is $1A + gfxset
    
    LDA #$A8                    ; force them to X=A8   (1 tile left of normal drawing pos)
    STA btl8x8spr_x             ; this is because the graphic changes from 16x24 to 24x16
    STA btl8x8spr_x+1           ;   so we have to move left a bit to accomidate
    
    LDA btl8x8spr_y             ; add 8 to Y
    CLC                         ;  again because 16x24 to 24x16
    ADC #$08
    STA btl8x8spr_y
    
    JSR DrawCharacter_DeadRow   ; Then draw 2 rows of tiles, and exit
    JMP DrawCharacter_DeadRow
    
  @DrawNotDead:
    ;;;;;;;;;;;;;;
    ; Not dead, but see if they're stone
    LDA btl_drawflagsB  ; Again, not sure exactly what this holds, but low 4 bits seem to indicate
    AND #$0F            ;  which characters are stoned
    AND $68B6
    BEQ @DrawChar       ; if not stoned, just draw them
    
    ; otherwise, if they're stone
    LDA #$03
    STA btl8x8spr_a             ; overwrite their attribute value to use the stone palette
    LDA #CHARPOSE_CROUCH
    STA btl_chardraw_pose, Y    ; force them to be crouched
    LDA #$B0
    STA btl8x8spr_x             ; force them at X position $B0  (normal position)
    STA btl8x8spr_x+1
    
  @DrawChar:
    LDX btl_chardraw_pose, Y    ; X becomes our index for the pose TSA lut
    JSR @DrawPoseRow            ; draw 3 rows, properly incrementing after each one
    JSR @DrawPoseRow            ;   2 JSRs, and the 3rd just flows into @DrawPoseRow
    
  @DrawPoseRow:
    LDA btl_chardraw_gfxset, Y      ; get the graphic set
    CLC
    ADC lut_CharacterPoseTSA, X     ; add our pose TSA to it
    STA btl8x8spr_t                 ; that is the tile to draw
    INX                             ; inc TSA index so next pose row we'll use different tiles
            ; Flow continues into Draw16x8SpriteRow, which will draw the tiles
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw16x8SpriteRow  [$9998 :: 0x319A8]
;;
;;  Draws 2 8x8 tiles with given 'btl8x8spr' information
;;  NOTE that it actually resets the X coord to the backup X coord, and
;;    afterwards it increments the Y coord to point to the next row.
;;  It also will increment the tile ID
;;
;;  All of these things mean you can simply call it twice in a row to draw a normal
;;  16x16 sprite.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Draw16x8SpriteRow:
    LDA btl8x8spr_x+1           ; restore backup X position
    STA btl8x8spr_x
    JSR BattleDraw8x8Sprite     ; draw 1 tile
    JSR DrawCharacter_NextTile  ; then increment, then draw another tile
    INC btl8x8spr_i             ; then increment again
    INC btl8x8spr_t
    LDA btl8x8spr_y             ; add 8 to Y to move to next row
    CLC
    ADC #$08
    STA btl8x8spr_y
    RTS                         ; and exit!
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DrawCharacter_DeadRow  [$99B4 :: 0x319C4]
;;
;;  Support routine for DrawCharacter.
;;  Draws a row of tiles for the dead character graphic
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawCharacter_DeadRow:
    LDA btl8x8spr_x+1           ; restore original X position for this row
    STA btl8x8spr_x
    
    JSR BattleDraw8x8Sprite     ; draw first tile
    JSR DrawCharacter_NextTile  ; then inc+draw 2 more
    JSR DrawCharacter_NextTile
    INC btl8x8spr_i             ; then inc oam pos and tile index
    INC btl8x8spr_t
    LDA btl8x8spr_y             ; lastly, add 8 to Y position so next time this is called
    CLC                         ;  it'll draw the next row
    ADC #$08
    STA btl8x8spr_y
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DrawCharacter_NextTile  [$99D3 :: 0x319E3]
;;
;;  Support routine for DrawCharacter.
;;  Increments tile positions and IDs, then draws another tile
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawCharacter_NextTile:
    INC btl8x8spr_i     ; Increment the oam index
    INC btl8x8spr_t     ; increment the tile index to draw
    LDA btl8x8spr_x     ; add 8 to our drawing X position
    CLC
    ADC #$08
    STA btl8x8spr_x
    JMP BattleDraw8x8Sprite ; draw & exit

    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DrawWeaponGraphicRow  [$99E5 :: 0x319F5]
;;
;;  This is effectively the same idea as Draw16x8SpriteRow, but for weapon graphics.
;;  Like that other routine, it is designed to be called twice in a row to draw the
;;  full graphic.
;;
;;  On first entry, X should be 0 to draw the graphic normally
;;  or X should be 8 to draw the graphic flipped on the X axis
;;
;;  The weapon swing animation alternates between these two
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawWeaponGraphicRow:
    LDA btl8x8spr_x+1               ; reset X coord to backup
    STA btl8x8spr_x
    
    LDA btlattackspr_t              ; get weapon graphic tile
    CLC
    ADC lut_WeaponSwingTSA, X       ; add TSA value
    STA btl8x8spr_t                 ; record it
    LDA lut_WeaponSwingTSA+4, X     ; get attribute & record it
    STA btl8x8spr_a
    JSR BattleDraw8x8Sprite         ; draw this tile
    
    INC btl8x8spr_i                 ; inc the oam position
    INX                             ; inc the TSA index
    LDA btl8x8spr_x                 ; add 8 to X position for next tile
    CLC
    ADC #$08
    STA btl8x8spr_x
    
    LDA btlattackspr_t              ; do all the same shit to draw another tile
    CLC
    ADC lut_WeaponSwingTSA, X
    STA btl8x8spr_t
    LDA lut_WeaponSwingTSA+4, X
    STA btl8x8spr_a
    JSR BattleDraw8x8Sprite
    
    LDA btl8x8spr_y                 ; add 8 to Y position
    CLC
    ADC #$08
    STA btl8x8spr_y
    INX                             ; inc TSA index
    INC btl8x8spr_i                 ; and oam position
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  PrepAndGetBattleMainCommand  [$9A2C :: 0x31A3C]
;;
;;  Preps cursor position table, then calls MenuSelection_2x4
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrepAndGetBattleMainCommand:
    LDY #$10
    : LDA lut_MainCombatBoxCursorPos-1, Y   ; -1 because Y is 1-based
      STA btlcurs_positions-1, Y
      DEY
      BNE :-
    JMP MenuSelection_2x4

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  SelectPlayerTarget  [$9A3A :: 0x31A4A]
;;
;;    Preps cursor positions for the party, then calls MenuSelection_2x4 to
;;  get the selection
;;
;;  input:  A = the current character.  (WHY?!?  Why can't the routine just use btlcmd_curchar directly?)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SelectPlayerTarget:
    STA $6AF9                   ; backup the current character
    
    LDY #$10                    ; Set the cursor positions
    : LDA lut_PlayerTargetCursorPos-1, Y
      STA btlcurs_positions-1, Y
      DEY
      BNE :-
      
    ; We have to push some of the cursor positions to the left
    ; Dead characters need to be pushed left 8 pixels
    ; And the current character needs to be move left 16 pixels (because they've stepped forward)
    LDA #$08                    ; first, move dead characters left 8
    STA $68B4                   ; $68B4 is temp to hold how much to push left
    
    
    LDA btl_drawflagsA
    AND #$01                    ; if char 0 is dead
    BEQ :+
      LDA #$00
      JSR @PushLeft             ; push char 0 left
      
  : LDA btl_drawflagsA          ; do same for char 1
    AND #$02
    BEQ :+
      LDA #$01
      JSR @PushLeft
      
  : LDA btl_drawflagsA          ; and char 2
    AND #$04
    BEQ :+
      LDA #$02
      JSR @PushLeft

  : LDA btl_drawflagsA          ; and char 3
    AND #$08
    BEQ :+
      LDA #$03
      JSR @PushLeft
    
  : JSR @PushCurChar            ; push the current character
    JMP MenuSelection_2x4       ; then do the menu logic!
    
  @PushCurChar:
    LDA #16                 ; current char gets pushed left 16 pixels
    STA $68B4
    LDA $6AF9               ; Get the current char from backup (why doesn't it just use btlcmd_curchar?)
    
  @PushLeft:
    STA $68B3               ; increment the character index.  WHY?!?! This doesn't make any sense and
    INC $68B3               ;   is a waste of code and time, and just complicates the below labels!
    LDA $68B3               ;   This is so stupid!!!
    
    ASL A                           ; *2 to use as index for the cursor positions.  Put in Y
    TAY
    LDA btlcurs_positions-2, Y      ; Get original X position (-2 because of the stupid INC above)
    SEC
    SBC $68B4                       ; Subtract the 'push' amount
    STA btlcurs_positions-2, Y      ; Store to this X position
    STA btlcurs_positions-2+8, Y    ;   and the mirrored X position
    
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  SelectEnemyTarget  [$9AA4 :: 0x31AB4]
;;
;;  Allow the user to select an enemy target.
;;
;;  output: Y and btlcmd_target = enemy slot being targetted
;;                            A = 01 if A pressed to exit, or 02 if B pressed
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SelectEnemyTarget:  
    LDA btl_drawflagsA          ; set the flag to indicate we want the cursor drawn
    ORA #$10                    ; note that this is unnecessary here, since EnemyTargetMenu
    STA btl_drawflagsA          ; (called below) also does this.
    
    LDA #$00                    ; initialize/clear the cursor position
    STA btlcurs
    LDX #$00                    ; zero X?  Why?  It's never used?
    
    LDA btl_battletype          ; get the formation type and use it as an index to the jump table
    ASL A
    TAY
    LDA lut_EnemyTargetMenuJumpTbl, Y
    STA $88
    LDA lut_EnemyTargetMenuJumpTbl+1, Y
    STA $89
    JMP ($0088)                 ; jump to appropriate target menu code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  lut_EnemyTargetMenuJumpTbl  [$9AC5 :: 0x31AD5]

lut_EnemyTargetMenuJumpTbl:
  .WORD EnemyTargetMenu_9Small
  .WORD EnemyTargetMenu_4Large
  .WORD EnemyTargetMenu_Mix
  .WORD EnemyTargetMenu_FiendChaos
  .WORD EnemyTargetMenu_FiendChaos
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  EnemyTargetMenu_FiendChaos  [$9ACF :: 0x31ADF]
;;
;;    Same idea as the below EnemyTargetMenu_XXX routines, only this doesn't need to actually
;;  have a menu because there is only 1 target to select.  So instead... just provide
;;  the the only possible output
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

EnemyTargetMenu_FiendChaos:
    JSR BattleClearVariableSprite   ; clear the cursor sprite
    JSR BattleFrame                 ; update PPU to refresh OAM
    LDY #$00                        ; output: Y = the target slot
    LDA #$01                        ; output: A = 1 ('A' button pressed)
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  EnemyTargetMenu_9Small  [$9ADA :: 0x31AEA]
;;
;;  Calls EnemyTargetMenu, with some prep to configure it for the '9 small' formation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

EnemyTargetMenu_9Small:
    ; Take the list of enemy slots and convert them to
    ;  legal cursor indexes.
    LDX #$00                        ; X is the cursor index and loop counter
  @TranslateLoop:
      LDY lut_EnemyIndex9Small, X   ; Y is the enemy slot index
      LDA btl_enemyIDs, Y           ; see if an enemy is in this slot
      CMP #$FF
      BEQ :+                        ; if there is, move enemy slot index to A
        TYA                         ;    otherwise, keep $FF in A (to indicate slot is empty)
    : STA btltmp_targetlist, X      ; store to targetlist
    
      INX
      CPX #$09
      BNE @TranslateLoop            ; loop for all 9 enemies.
    
    ; Then prep the btlcurs_positions buffer
    LDY #$00
  @CursPosLoop:
      LDA lut_Target9SmallCursorPos, Y  ; copy cursor positions from lut
      STA btlcurs_positions, Y          ; to RAM lut
      INY
      CPY #9*2                  ; 9 enemies, 2 bytes per slot
      BNE @CursPosLoop

    ; 8 is the max target slot (0-8)
    LDA #$08
    STA btlcurs_max
    JMP EnemyTargetMenu
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  EnemyTargetMenu_4Large  [$9B04 :: 0x31B14]
;;
;;    Calls EnemyTargetMenu, with some prep to configure it for the '4 large' formation.
;;  Identical to the 9Small version, but just using different tables and constants.
;;  See that routine for details, comments here are sparse.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

EnemyTargetMenu_4Large:
    LDX #$00
  @TranslateLoop:
      LDY lut_EnemyIndex4Large, X
      LDA btl_enemyIDs, Y
      CMP #$FF
      BEQ :+
        TYA
    : STA btltmp_targetlist, X
      INX                       ; sort of bugged.  There are only 4 entries in the 4large lut.  This reads
      CPX #$09                  ; 9 entries, which steps out of bounds, but the entries are never used so 
      BNE @TranslateLoop        ; it doesn't matter.
      
    LDY #$00
  @CursPosLoop:
      LDA lut_Target4LargeCursorPos, Y
      STA btlcurs_positions, Y
      INY
      CPY #9*2                  ; same deal -- should only be reading 4 entries, not 9
      BNE @CursPosLoop          ;   clearly this routine was copy/pasted
      
    LDA #$03
    STA btlcurs_max
    JMP EnemyTargetMenu

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  EnemyTargetMenu_Mix  [$9B2E :: 0x31B3E]
;;
;;    Same idea as 9Small/4Large routines above.  See them for details.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

EnemyTargetMenu_Mix:
    LDX #$00
  @TranslateLoop:
      LDY lut_EnemyIndexMix, X
      LDA btl_enemyIDs, Y
      CMP #$FF
      BEQ :+
        TYA
    : STA btltmp_targetlist, X
      INX
      CPX #$09                  ; <- should be 8
      BNE @TranslateLoop
    
    LDY #$00
  @CursPosLoop:
      LDA lut_TargetMixCursorPos, Y
      STA btlcurs_positions, Y
      INY
      CPY #9*2                  ; <- should be 8*2
      BNE @CursPosLoop
    
    LDA #$07
    STA btlcurs_max
  ; JMP EnemyTargetMenu         ; <- Flow into

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  EnemyTargetMenu  [$9B55 :: 0x31B65]
;;
;;  Do the menu logic for selecting an enemy target.
;;
;;  input:  btlcurs_max
;;          btlcurs             (assumed to be initalized to zero)
;;          btlcurs_positions   (assumed to be filled with cursor positions)
;;
;;  output: Y and btlcmd_target (enemy slot being targetted)
;;          A                   (01 if A pressed to exit, or 02 if B pressed)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

EnemyTargetMenu:
    JSR BattleTarget_DownSub        ; move down to the first legal slot

  @MainLoop:
    LDA btl_drawflagsA              ; set the flag to show the cursor
    ORA #$10
    STA btl_drawflagsA
    
    LDA btlcurs                     ; set the X,Y coord for the cursor
    ASL A
    TAY
    LDA btlcurs_positions, Y
    STA btlcursspr_x
    LDA btlcurs_positions+1, Y
    STA btlcursspr_y
    
    JSR UpdateSprites_BattleFrame   ; do frame to draw the cursor
    JSR DoFrame_WithInput           ; do ANOTHER frame to get input
    
    CMP btlinput_prevstate
    BNE :+                          ; if no change in input...
      JSR UpdateInputDelayCounter
      BEQ @MainLoop                 ; ... update input delay, and loop if input delay still in effect
      LDA #$03                      ; otherwise, reset input delay (pointless, since UpdateInputDelayCounter does this)
      STA inputdelaycounter
      JMP @CheckInput               ; and process input
      
  : LDA #$05                        ; there was a change in input, so reset the input delay counter
    STA inputdelaycounter           ;    to a slightly longer time than usual

  @CheckInput:
    LDA btl_input                   ; record state as prev state
    STA btlinput_prevstate
    
    LDA btl_input
    AND #$03
    BEQ :+                          ; see if A or B were pressed.  If yes...
      PHA
      JSR BattleClearVariableSprite ; clear the cursor sprite in shadow OAM
      JSR BattleFrame               ; do a frame to clear it in the PPU
      LDY btlcmd_target             ; put the target in Y
      PLA                           ; and the A/B button state in A, and exit!
      RTS
    
  : JSR @ProcessDPad                ; Otherwise, A/B not pressed, check the DPad
    JMP @MainLoop                   ; Rinse, repeat
    
  @ProcessDPad:
    LDA btl_input           ; check the DPad
    AND #$F0
    CMP #$20
    BEQ BattleTarget_Down   ; see if Down pressed
    CMP #$10
    BEQ BattleTarget_Up     ; see if Up pressed
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BattleTarget_Down    [$9BBB :: 0x31BCB]
;;  BattleTarget_DownSub [$9BCB :: 0x31BDB]
;;
;;  Chooses the next legal battle target.
;;
;;    The 'Sub' entry point is for the first target selection.  It cuts in at the target
;;  verification code.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BattleTarget_Down:
    LDA btlcurs             ; see if the cursor is at our max
    CMP btlcurs_max
    BNE :+                  ; if it's at the max, replace with -1 (so we INC it to zero)
      LDA #-1
      STA btlcurs
  : INC btlcurs             ; inc to next slot
  ; JMP BattleTarget_DownSub            ; <- flows into

BattleTarget_DownSub:
    LDY btlcurs                 ; put the cursor in Y
    LDA btltmp_targetlist, Y    ; get the target slot
    CMP #$FF
    BEQ BattleTarget_Down       ; if it's empty, keep moving down until we find a non-empty slot
    
    STA btlcmd_target           ; once we have a valid slot, store it in ?'btltmp_target'?
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BattleTarget_Up    [$9BD9 :: 0x31BE9]
;;
;;  Chooses the next legal battle target.
;;
;;    The 'Sub' entry point is for the first target selection.  It cuts in at the target
;;  verification code.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
BattleTarget_Up:
    LDA btlcurs             ; see if the cursor is at zero
    BNE :+
      LDA btlcurs_max       ; if it is, set it to max+1
      STA btlcurs           ;  (+1 because we'll be DEC'ing it in a second)
      INC btlcurs
      
  : DEC btlcurs             ; DEC to move to previous slot
    LDY btlcurs
    LDA btltmp_targetlist, Y; Check the enemy slot, see if it's empty
    CMP #$FF
    BEQ BattleTarget_Up     ; if it is, keep moving up
    STA btlcmd_target       ; otherwise, if it's not empty, record the target
    RTS                     ;   and exit
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  MenuSelection_Item  [$9BF8 :: 0x31C08]
;;
;;    Just calls MenuSelection_2x4 with the Item menu cursor positions.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MenuSelection_Item:
    LDY #$10
    : LDA lut_ItemCursorPos-1, Y
      STA btlcurs_positions-1, Y
      DEY
      BNE :-
    JMP MenuSelection_2x4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  MenuSelection_Drink  [$9C06 :: 0x31C16]
;;
;;    Just calls MenuSelection_2x4 with the Drink menu cursor positions.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MenuSelection_Drink:
    LDY #$10
    : LDA lut_DrinkCursorPos-1, Y   ; copy over the cursor positions for
      STA btlcurs_positions-1, Y    ;  the drink menu
      DEY
      BNE :-
    JMP MenuSelection_2x4           ; and do the logic

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  MenuSelection_Magic  [$9C14 :: 0x31C24]
;;
;;    Same idea as MenuSelection_2x4, but rewritten as it is more complex than
;;  a basic 2x4 menu.  See MenuSelection_2x4 for input/output and other details.
;;
;;  additional output:   $6AF8 = magic page
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MenuSelection_Magic:
    STA $6AF9                   ; ??? This is never used
    
    LDA #$00
    STA $6AF8                   ; set page number to 0 (draw top page of magic box)
    
    JSR DrawBattleMagicBox_L    ; draw it!
    JMP @MenuSelection          ; pointless jump, as this code immediately follws
    
@MenuSelection:
    LDA #$00
    STA btlcurs_x
    STA btlcurs_y
    
  @MainLoop:
    LDA btl_drawflagsA
    ORA #$10
    STA btl_drawflagsA
    
    LDA btlcurs_y
    AND #$03
    ASL A
    STA $68B4
    LDA btlcurs_x
    ASL A
    ASL A
    ASL A
    CLC
    ADC $68B4           ; Y*2 + X*8 -- putting the cursor positions in COL-major order instead of row major
    TAY
    
    LDA lut_MagicCursorPos, Y
    STA btlcursspr_x
    LDA lut_MagicCursorPos+1, Y
    STA btlcursspr_y
    
    JSR UpdateSprites_BattleFrame   ; Do a frame to update the cursor
    JSR DoFrame_WithInput           ; And another frame to get input
    
    CMP btlinput_prevstate          ; See if there was any change in input
    BNE @InputChanged               ; if yes, jump ahead to process it
      JSR UpdateInputDelayCounter   ; if no, update input delay to see if we should ignore input or not
      BEQ @MainLoop                 ; if we're to ignore it, just loop
      LDA #$03
      STA inputdelaycounter         ; otherwise, reset the counter and proceed to process it
      JMP :+
  @InputChanged:
      LDA #$05                      ; if input changed, reset the counter to a higher value
      STA inputdelaycounter         ;   then process the input
      
  : LDA btl_input                   ; set prev state
    STA btlinput_prevstate
    
    LDA btl_input
    AND #$03                        ; see if A/B were pressed
    BEQ :+                          ; if yes....
      PHA                           ; push A/B state
      
      LDA btlcurs_y                 ; clip X,Y cursor positions
      AND #$03
      STA btlcurs_y
      LDA btlcurs_x
      AND #$03
      STA btlcurs_x
      
      JSR BattleClearVariableSprite ; hide the cursor sprite
      JSR BattleFrame               ; do a frame to hide it from view
      
      LDY btlcurs_y                 ; put Y cursor pos in Y (why?  haha)
      PLA                           ; restore A/B state
      RTS
    
    ; jumps here if A/B were not pressed
  : JSR @CheckDPad
    JMP @MainLoop
    
  @CheckDPad:
    LDA btl_input
    AND #$F0
    CMP #$80
    BEQ @Cursor_Right
    CMP #$40
    BEQ @Cursor_Left
    CMP #$20
    BEQ @Cursor_Down
    CMP #$10
    BEQ @Cursor_Up
    RTS
    
  @Cursor_Down:
    LDA btlcurs_y
    AND #$03
    CMP #$03                    ; see if it's at the bottom of the page
    BNE @NormalMove_Down        ; if not, do a normal move
    LDA $6AF8                   ; otherwise, check the page
    BEQ :+
      RTS                       ; page 1 = do nothing
  : INC $6AF8                   ; page 0 = inc to page 1
    LDA #$01
    JSR UndrawNBattleBlocks_L   ; undraw the page 0 magic box
    JSR DrawBattleMagicBox_L    ; draw the page 1 magic box
  @NormalMove_Down:
    INC btlcurs_y
    RTS

  @Cursor_Up:
    LDA btlcurs_y
    AND #$03                    ; top row of page?
    BNE @NormalMove_Up          ; no?  then normal move
    LDA $6AF8                   ; yes?  check page
    CMP #$01
    BEQ :+
      RTS                       ; page 0?  exit
  : DEC $6AF8                   ; page 1?  move to page 0, and redraw it
    LDA #$01
    JSR UndrawNBattleBlocks_L
    JSR DrawBattleMagicBox_L
  @NormalMove_Up:
    DEC btlcurs_y
    RTS
    
  @Cursor_Left:
    LDA btlcurs_x
    BNE :+                  ; are we in the left column?  If yes..
      LDA #$03              ; change to 3, so we'll DEC it to 2 (right column)
      STA btlcurs_x
  : DEC btlcurs_x
    RTS

  @Cursor_Right:
    LDA btlcurs_x
    CMP #$02
    BNE :+              ; if in right column
      LDA #-1           ; change to -1 (so we INC to zero)
      STA btlcurs_x
  : INC btlcurs_x
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  MenuSelection_2x4  [$9D0E :: 0x31D1E]
;;
;;    The the menu logic for selecting items on a 2x4 menu.  This is used by the
;;  main combat menu (FIGHT/RUN/MAGIC/etc), but also for other menus (like the
;;  DRINK menu).
;;
;;    Menus can use fewer than 2x4 items as long as the cursor entries exist
;;  (for smaller menus some entries will be mirrored).
;;
;;  input:   btlcurs_positions should be filled with 8 entries (in col major order)
;;
;;  output:
;;                    A = A/B button state (bit 0=A pressed... bit 1=B pressed)
;;      Y and btlcurs_y = row of item selected (0-3)
;;            btlcurs_x = column of item selected (0=left column, 1="Run" column)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MenuSelection_2x4:
    LDA #$00
    STA btlcurs_x
    STA btlcurs_y
    
  @MainLoop:
    LDA btl_drawflagsA      ; flip on the flag to draw the cursor
    ORA #$10
    STA btl_drawflagsA
    
    LDA btlcurs_y           ; Get the cursor X,Y pixel position
    AND #$03                ; Main combat menu has 4 rows.  So take low 2 bits
    ASL A                   ;   *2  (2 bytes per entry in btlcurs_positions)
    STA $68B4               ; store in tmp mem
    LDA btlcurs_x
    AND #$01                ; only 2 columns (Fight/Run), so mask low bit
    ASL A                   ;   *8
    ASL A
    ASL A
    CLC
    ADC $68B4               ; add with tmp mem
    
    TAY                     ; use as index for position lut to get the sprite position
    LDA btlcurs_positions, Y
    STA btlcursspr_x
    LDA btlcurs_positions+1, Y
    STA btlcursspr_y
    
    JSR UpdateSprites_BattleFrame   ; Update sprites & do a frame
    JSR DoFrame_WithInput           ; Do *ANOTHER* frame and get input in A
    CMP btlinput_prevstate
    BNE :+                          ; If there was no change in input...
      JSR UpdateInputDelayCounter   ; update delay counter
      BEQ @MainLoop                 ; not accepting input, repeat from main loop
      LDA #$03
      STA inputdelaycounter         ; otherwise reset delay counter (pointless, since UpdateInputDelayCounter
      JMP @CheckButtons             ;   already does this), then check buttons
      
  : LDA #$05                        ; if there was a change of input, reset the delay counter (make it a little
    STA inputdelaycounter           ;   longer than normal), and then check buttons
    
  @CheckButtons:
    LDA btl_input                   ; record current input as previous input
    STA btlinput_prevstate
    
    LDA btl_input
    AND #$03
    BEQ :+                          ; see if A or B are pressed.  If they are...
      PHA                           ; push A/B state
      
      LDA btlcurs_y                 ; clip/wrap the cursor so it's within valid range.
      AND #$03
      STA btlcurs_y
      LDA btlcurs_x
      AND #$01
      STA btlcurs_x
      
      JSR BattleClearVariableSprite ; erase the cursor sprite (in shadow oam)
      JSR BattleFrame               ; redraw the screen (to erase it in the PPU)
      
      LDY btlcurs_y                 ; put cursor selection in Y (though Fight/Run column selection is still in btlcurs_x
      PLA                           ; put A/B button state in A
      RTS                           ; exit!
    
  : JSR @MoveCursor                 ; if A/B are not pressed, then check arrow buttons to move the cursor
    JMP @MainLoop                   ; and repeat
    
  @MoveCursor:
    LDA btl_input
    AND #$F0                        ; isolate arrow buttons
    CMP #$80
    BEQ @Cursor_Right               ; branch around depending on which button is pressed
    CMP #$40
    BEQ @Cursor_Left
    CMP #$20
    BEQ @Cursor_Down
    CMP #$10
    BEQ @Cursor_Up
    RTS
  
  @Cursor_Down:                     ; these are all pretty self explanitory
    INC btlcurs_y
    RTS
  @Cursor_Up:
    DEC btlcurs_y
    RTS
  @Cursor_Left:
    DEC btlcurs_x
    RTS
  @Cursor_Right:
    INC btlcurs_x
    RTS
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Set Natural Pose  [$9DB2 :: 0x31DC2]
;;
;;  input:   A = ID of character to do (0-3)
;;
;;  output:  Desired character's 'btl_chardraw_pose' value
;;           A will also contain the pose value
;;
;;  This routine will check to see if the character should be standing (pose $00)
;;    or crouching (pose $18) in their natural pose.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SetNaturalPose:
    ASL A
    ASL A
    TAX                 ; X=%0000xx00  where 'xx' is character ID
    ASL A
    ASL A
    ASL A
    ASL A               ; Y=%xx000000  where 'xx' is character ID
    TAY                 ;   Y can now be used as an index for character stats
    
    LDA ch_ailments, Y  ; check this character's ailment
    AND #AIL_DEAD
    BEQ :+              ; if they're dead
      LDA #$00          ; zero their hit points (seems strange to do this here, but whatever)
      STA ch_curhp, Y
      STA ch_curhp+1, Y
  : LDA ch_ailments, Y  ; get ailments again
    AND #(AIL_POISON | AIL_STUN | AIL_SLEEP)    ; see if they are poisoned, stunned, or asleep
    BEQ @CheckHP                ; if they are... then they are crouching
  @DoCrouching:
    LDA #CHARPOSE_CROUCH                
    STA btl_chardraw_pose, X
    RTS
    
@CheckHP:                       ; if they don't have poison/stun/sleep, we need to check their HP
    LDA ch_maxhp, Y     ; move max HP to $68B3,4
    STA $68B3
    LDA ch_maxhp+1, Y
    STA $68B4
    
    LSR $68B4           ; divide it by 4 (25% of max HP)
    ROR $68B3
    LSR $68B4
    ROR $68B3
    
    LDA ch_curhp+1, Y   ; if the high byte of their HP is nonzero, they have over 256, which is
    BNE @DoStanding     ;   definitely more than 25% max, so DoStanding
    
    LDA ch_curhp, Y     ; otherwise, compare low byte of HP to low byte of 25%
    CMP $68B3
    BCC @DoCrouching    ; if cur HP is less, they are crouching
                        ; otherwise, they're standing
@DoStanding:            
    LDA #CHARPOSE_STAND
    STA btl_chardraw_pose, X
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  CharWalkAnimationLeft  [$9E01 :: 0x31E11]
;;
;;  A = the character to animate (0-3)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CharWalkAnimationLeft:
    PHA         ; push char index
    LDA #-2     ; negative directional value = move left
    BNE :+      ;  <-- FLOW:  This label is in CharWalkAnimationRight

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  CharWalkAnimationRight  [$9E06 :: 0x31E16]
;;
;;  A = the character to animate (0-3)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CharWalkAnimationRight:
    PHA         ; push char index
    LDA #2      ; positive directional value = move right
    
:   STA $68AB       ; 68AB = walk direction
    PLA
    STA btl_animatingchar
    JSR CharacterWalkAnimation
    LDA btl_animatingchar
    JSR SetNaturalPose          ; after animation is complete, revert this character to their natural pose
    JMP UpdateSprites_TwoFrames ; update, then return

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  CharacterWalkAnimation  [$9E1C :: 0x31E2C]
;;
;;  input:
;;   btl_animatingchar = character index to animate (0-3)
;;               $68AB = direction/speed (pixels per 2 frames)
;;                       negative = move left, positive = move right
;;
;;    This routine does the walk forward and walk back animation for the
;;  characters.
;;
;;    This routine takes 16 frames to complete.
;;
;;    For whatever weird reason, this routine moves the character every
;;  OTHER frame rather than every frame.  Which is especially weird since
;;  the supplied speed is 2 pixels (either -2 to walk left or 2 to walk right).
;;  The game could have very easily made animation smoother by halving the
;;  walk speed and updating every frame.  Weird weird.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CharacterWalkAnimation:
    LDA #$08
    STA $6AA8                   ; loop down counter -- looping 8 times for 16 total frames
    
  @Loop:
      LDA btl_animatingchar     ; get the character index
      ASL A
      ASL A
      TAX                       ; index for btl_chardraw buffer
      
      LDA $6AA8
      AND #$02                  ; toggle animation pose every 4 frames
      ASL A                     ; switch between pose '0' (stand) and pose '4' (walk)
      STA btl_chardraw_pose, X
      
      LDA btl_chardraw_x, X     ; add the directional value to the X position
      CLC
      ADC $68AB
      STA btl_chardraw_x, X
      
      JSR UpdateSprites_TwoFrames   ; update sprites, do 2 frames of animation
      
      DEC $6AA8
      BNE @Loop                 ; keep looping
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  PlayFanfareAndCheer    [$9E43 :: 0x31E53]
;;
;;    Plays the "you won the battle" fanfare music and do the animation for the
;;  party cheering over their accomplishment.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PlayFanfareAndCheer:
    LDA #$53                    ; play fanfare music
    STA a:music_track
    STA btl_followupmusic
    
    LDA #$40                    ; loop counter
    STA $6AA8
  @Loop:
      LDA $6AA8
      AND #(CHARPOSE_CHEER >> 1)    ; alternate between CHEER/STAND poses every 8 loop iterations
      ASL A
      STA btl_chardraw_pose+$0
      STA btl_chardraw_pose+$4
      STA btl_chardraw_pose+$8
      STA btl_chardraw_pose+$C
      
      JSR UpdateSprites_TwoFrames   ; draw!
      
      DEC $6AA8
      BNE @Loop
      
    JSR SetAllNaturalPose           ; afterwards, give everyone their natural pose
    JMP UpdateSprites_TwoFrames     ; draw, and exit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  WalkForwardAndStrike    [$9E70 :: 0x31E80]
;;
;;    Does the animation to walk a character forward, swing their weapon (or cast their spell),
;;  then walk back to their original position.
;;
;;  input:
;;      A = the character to animate (0-3)
;;      X = the attack sprite graphic
;;      Y = 0 is swinging with a weapon, 1 if casting a magic spell
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

WalkForwardAndStrike:
    STA btl_animatingchar       ; A = character to animate
    STX btlattackspr_gfx        ; X = attack sprite graphic
    STY btlattackspr_wepmag     ; Y = 0,1 to choose between weapon/magic
    
    LDA #-2
    STA $68AB                   ; walk the character to the left
    JSR CharacterWalkAnimation
    
    LDA #$08                    ; 6AA8 is the loop counter (loop 8 times), alternates between 
    STA $6AA8                   ;   animation frames every 2 iterations.
    
  @DoAttackAnimationLoop:
    LDA btl_drawflagsA                  ; set the "draw weapon" draw flag
    ORA #$20                            ;  note:  for magic, this is changed in PrepAttackSprite call below
    STA btl_drawflagsA
    
    LDA $6AA8
    AND #$02                            ; every other frame...
    BEQ @BFrame
        JSR PrepAttackSprite_AFrame     ; alternate between AFrame of animation
        JMP :+
    @BFrame:
        JSR PrepAttackSprite_BFrame     ; ... and BFrame of animation
        
    : JSR UpdateSprites_TwoFrames       ; redraw sprites on screen, do 2 frames.
    
      DEC $6AA8
      BNE @DoAttackAnimationLoop        ; loop until counter expires
    
    
    JSR BattleClearVariableSprite       ; Finally, once attack animation complete, clear the weapon/magic sprite
    JSR BattleFrame                     ; Do another frame to make sure it's erased  (seems silly to do that here... since
                                        ;   we do another frame shortly...)

    LDA btl_animatingchar
    ASL A
    ASL A
    TAX
    LDA #CHARPOSE_STAND
    STA btl_chardraw_pose, X            ; reset the character's pose to 'standing'
    JSR UpdateSprites_TwoFrames         ; then update sprites and do 2 frames.
    
    LDA #2
    STA $68AB
    JSR CharacterWalkAnimation          ; Do the animation to walk the character back to the right
    
    LDA btl_animatingchar
    JSR SetNaturalPose                  ; reset character back to their natural pose
    JSR BattleClearVariableSprite       ; clear the wep/mag sprite (again?  Pointless to do here)
  ; JMP UpdateSprites_TwoFrames         ; <- flow into.  Update sprites and do 2 frames
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  UpdateSprites_TwoFrames [$9ECB :: 0x31EDB]
;;
;;    Same as UpdateSprites_BattleFrame, but waits an additional frame
;;  (to slow down animations a bit?)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

UpdateSprites_TwoFrames:
    JSR UpdateSprites_BattleFrame
    JMP BattleFrame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  PrepAttackSprite_AFrame [$9ED1 :: 0x31EE1]
;;
;;    Weapon and Magic attacks consist of 2 frames of animation.  This routine will
;;  prep the 'AFrame' of animation.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrepAttackSprite_AFrame:
    LDA btlattackspr_wepmag                 ; see if this is a weapon or a magic graphic
    BEQ __PrepAttackSprite_Weapon_AFrame    ; if weapon, jump ahead to weapon processing
                                            ; otherwise, flow into magic processing
    
    ;; [$9ED6 :: 0x31EE6]
    ;;  Support routine:  Sets the btlattackspr to the 'A' frame for magic    
__PrepAttackSprite_Magic_AFrame:
    JSR __PrepAttackSprite_Weapon_AFrame    ; magic AFrame is the same as weapon AFrame, but with some changes:
    LDA #CHARPOSE_CHEER
    STA btl_chardraw_pose, X                ; player is cheering rather than swinging
    
    LDA btlattackspr_gfx                    ; (moving graphic over to _t is unnecessary here, as this was
    STA btlattackspr_t                      ;  already done in the WeaponAFrame)
    
    LDA btl_drawflagsA                      ; turn on the magic drawflag bit, and turn off the weapon
    ORA #$40                                ; drawflag bit.
    AND #~$20                               ; this will cause the BG color to flash.
    STA btl_drawflagsA
    RTS
    
    ;; [$9EEF :: 0x31EFF]
    ;;  Support routine:  Sets the btlattackspr to the 'A' frame for weapons    
__PrepAttackSprite_Weapon_AFrame:
    LDA btl_animatingchar           ; use character*4 as index
    ASL A
    ASL A
    TAX
    
    LDA #CHARPOSE_ATTACK_F
    STA btl_chardraw_pose, X        ; set the pose to forward attack
    
    LDA btl_chardraw_x, X           ; weapon graphic is $10 pixels left of the char graphic
    SEC
    SBC #$10
    STA btlattackspr_x
    
    LDA btl_chardraw_y, X           ; weapon graphic is at same Y position as char
    STA btlattackspr_y
    
    LDA btlattackspr_gfx            ; Set the tile to the appropriate graphic
    STA btlattackspr_t              ; And set the pose to scene 0
    LDA #$00
    STA btlattackspr_pose
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  PrepAttackSprite_BFrame [$9F15 :: 0x31F25]
;;
;;    Weapon and Magic attacks consist of 2 frames of animation.  This routine will
;;  prep the 'BFrame' of animation.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrepAttackSprite_BFrame:
    LDA btlattackspr_wepmag                 ; weapon or magic?
    BEQ :+                                  ; if magic...
      JSR __PrepAttackSprite_Magic_AFrame   ; Same setup as AFrame
      LDA #$04
      STA btlattackspr_pose                 ; just the pose is different.
      RTS

  : LDA btl_animatingchar       ; BFrame for weapons is different.  The weapon has to move
    ASL A                       ;   back behind the player's head.
    ASL A
    TAX                         ; X=4*char index
    
    LDA btl_chardraw_x, X       ; weapon graphic is 8 pixels to the right of the character
    CLC
    ADC #$08
    STA btlattackspr_x
    
    LDA btl_chardraw_y, X       ; and 8 pixels ABOVE the character
    SEC
    SBC #$08
    STA btlattackspr_y
    
    LDA btlattackspr_gfx        ; copy graphic over
    STA btlattackspr_t
    
    LDA #CHARPOSE_ATTACK_B      ; change the player's pose to backward attack
    STA btlattackspr_pose       ;  also.. it just works out the pose is the same for the BFrame of the attack sprite
    STA btl_chardraw_pose, X
    
    INC btlattackspr_hidell     ; set the hideLL flag to stop the graphic from drawing over the character's face
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  UpdateInputDelayCounter  [$9F4D :: 0x31F5D]
;;  
;;    Counts down the input delay counter, and indicates whether or not input should
;;  be accepted.  Input should not be accepted every frame since that would make the cursor
;;  hyper-sensitive when you hold a direction on the Dpad.
;;
;;  output:  A,Z will be zero if input is to be ignored.
;;               or nonzero if input should be accepted
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

UpdateInputDelayCounter:
    DEC inputdelaycounter       ; dec the counter
    BEQ :+                      ; if nonzero, indicate that we should ignore input
      LDA #$00                  ;  (A=0)
      RTS
  : LDA #$03                    ; otherwise, if zero, reset delay counter
    STA inputdelaycounter
    LDA #$01                    ; and indicate we can use input (A=nonzero)
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  [$9F5D :: 0x31F6D]
;;  Lut - pointer table to the beginning of each character's stats OB in RAM

lut_CharStatsPtrTable:
  .WORD ch_stats
  .WORD ch_stats+$40
  .WORD ch_stats+$80
  .WORD ch_stats+$C0
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  [$9F65 :: 0x31F75]
;;  Lut - pointer table to the beginning of each character's IB stats in RAM

lut_IBCharStatsPtrTable:
  .WORD btl_chstats
  .WORD btl_chstats + (1*$12)
  .WORD btl_chstats + (2*$12)
  .WORD btl_chstats + (3*$12)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  lut_CharMagicData  [$9F6D :: 0x31F7D]

lut_CharMagicData:
  .WORD ch_magicdata
  .WORD ch_magicdata + $40
  .WORD ch_magicdata + $80
  .WORD ch_magicdata + $C0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  lut_MainCombatBoxCursorPos  [$9F75 :: 0x31F85]
;;
;;    Pixel positions to draw the cursor sprite for the main combat menu.

lut_MainCombatBoxCursorPos:
;         X    Y
  .BYTE $60, $9E    ; FIGHT
  .BYTE $60, $AE    ; MAGIC
  .BYTE $60, $BE    ; DRINK
  .BYTE $60, $CE    ; ITEM
  .BYTE $90, $9E    ; RUN (repeated 4 times to pad to 2x4 menu)
  .BYTE $90, $9E
  .BYTE $90, $9E
  .BYTE $90, $9E
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  lut_PlayerTargetCursorPos  [$9F85 :: 0x31F95]
;;
;;    Pixel positions to draw the cursor sprite for selecting the player/character
;;  targets.  Uses 2x4 menu, but only has 1 column, so the 2nd column mirrors the first.

lut_PlayerTargetCursorPos:
;         X    Y
  .BYTE $A0, $34    ; char 0
  .BYTE $A0, $4C    ; char 1
  .BYTE $A0, $64    ; char 2
  .BYTE $A0, $7C    ; char 3
  .BYTE $A0, $34    ; mirrors
  .BYTE $A0, $4C
  .BYTE $A0, $64
  .BYTE $A0, $7C
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  lut_Target9SmallCursorPos  [$9F95 :: 0x31FA5]
;;  lut_Target4LargeCursorPos  [$9FA7 :: 0x31FB7]
;;  lut_TargetMixCursorPos     [$9FAF :: 0x31FBF]
;;
;;    Pixel positions to draw the cursor sprite for target selection (for various
;;  formation types)

lut_Target9SmallCursorPos:
  .BYTE $00, $30
  .BYTE $00, $50
  .BYTE $00, $70
  .BYTE $20, $30
  .BYTE $20, $50
  .BYTE $20, $70
  .BYTE $40, $30
  .BYTE $40, $50
  .BYTE $40, $70
  
lut_Target4LargeCursorPos:
  .BYTE $00, $30
  .BYTE $00, $60
  .BYTE $40, $30
  .BYTE $40, $60

lut_TargetMixCursorPos:
  .BYTE $00, $30
  .BYTE $00, $60
  .BYTE $30, $30
  .BYTE $30, $50
  .BYTE $30, $70
  .BYTE $50, $30
  .BYTE $50, $50
  .BYTE $50, $70
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  lut_MagicCursorPos  [$9FBF :: 0x31FCF]
;;
;;    Pixel positions for the MAGIC submenu.  It doesn't use 2x4 menu because
;;  it has more entries and also because it has to be able to switch pages.
;;  Also, these cursor positions are in COL-major order instead of row-major.
;;  This makes more sense here since there's 4 rows but only 3 cols, and it's
;;  easier to multiply by 4 than it is to multiply by 3

lut_MagicCursorPos:
  .BYTE $20, $A6
  .BYTE $20, $B6
  .BYTE $20, $C6
  .BYTE $20, $D6
  .BYTE $48, $A6
  .BYTE $48, $B6
  .BYTE $48, $C6
  .BYTE $48, $D6
  .BYTE $70, $A6
  .BYTE $70, $B6
  .BYTE $70, $C6
  .BYTE $70, $D6


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  lut_ItemCursorPos  [$9FD7 :: 0x31FE7]
;;
;;    Pixel positions for the DRINK submenu.  It uses the 2x4 menu and
;;  actually uses all 8 slots!
lut_ItemCursorPos:
  .BYTE $10, $A6
  .BYTE $10, $B6
  .BYTE $10, $C6
  .BYTE $10, $D6
  .BYTE $58, $A6
  .BYTE $58, $B6
  .BYTE $58, $C6
  .BYTE $58, $D6

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  lut_DrinkCursorPos  [$9FE7 :: 0x31FF7]
;;
;;    Pixel positions for the DRINK submenu.  It uses the 2x4 menu but only
;;  has 2 entries (Heal and Pure potions), so those 2 entries are mirrored
;;  several times

lut_DrinkCursorPos:
  .BYTE $18, $A6        ; Heal
  .BYTE $18, $B6        ; Pure
  .BYTE $18, $A6        ; Heal mirror
  .BYTE $18, $B6        ; Pure mirror
  .BYTE $18, $A6        ; etc
  .BYTE $18, $B6
  .BYTE $18, $A6
  .BYTE $18, $B6

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  lut_EnemyIndex9Small  [$9FF7 :: 0x32007]
;;  lut_EnemyIndex4Large  [$A000 :: 0x32010]
;;  lut_EnemyIndexMix     [$A004 :: 0x32014]
;;
;;    LUT to convert a cursor index to an actual enemy index (for various formation types)
;;  This is needed because the order in which enemies are generated do not match the order they're
;;  drawn.
;;  Example:  in the 9 small formation type, Enemy 0 is in the center row, but cursor=0 points to the top row.

lut_EnemyIndex9Small:
  .BYTE $01, $00, $02
  .BYTE $04, $03, $05
  .BYTE $07, $06, $08
  
lut_EnemyIndex4Large:
  .BYTE $00, $01
  .BYTE $02, $03
  
lut_EnemyIndexMix:
  .BYTE $00, $01            ; 2 large
  .BYTE $03, $02, $04       ; 6 small
  .BYTE $06, $05, $07
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  lut for character poses  [$A00C :: 0x3201C]
;;
;;  Each entry is basically a TSA for constructing an in-battle character sprite.  Each entry
;;   is a different pose, and constructs a 16x24 image out of 2x3 tiles.  Entries are 3 bytes each (with 1 byte
;;   of padding).  Each byte is the tile to use for the 16x8 sprite for a single tile row in the pose.

lut_CharacterPoseTSA:
  .BYTE $00, $02, $04, $00      ; 00 = standing pose
  .BYTE $00, $02, $06, $00      ; 04 = walking pose
  .BYTE $08, $0A, $0C, $00      ; 08 = attacking (arm back) pose
  .BYTE $00, $02, $06, $00      ; 0C = attacking (arm forward) pose
  .BYTE $0E, $10, $12, $00      ; 10 = cheering pose
  .BYTE $0E, $10, $12, $00      ; 14 = cheering pose again (possibly for casting magic or something?)
  .BYTE $14, $16, $18, $00      ; 18 = crouching pose  (for when you're hurt)
  .BYTE $14, $16, $18, $00      ; 1C = crouching pose again
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  lut for weapon swing TSA  [$A02C :: 0x3203C]
;;
;;  There are 2 frames here, each consisting of 8 bytes.
;;  For each frame, the first 4 bytes are the tiles to draw, and the next 4 are the attributes
;;
;;  Frame 0 is normal drawing, Frame 1 is flipped horizontally

lut_WeaponSwingTSA:
  .BYTE $00, $01, $02, $03,     $02, $02, $02, $02  ; <- normal graphic
  .BYTE $01, $00, $03, $02,     $42, $42, $42, $42  ; <- flipped horizontally
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  lut for assigning palettes to in-battle char sprites  [$A03C :: 0x3204C]
;;
lut_InBattleCharPaletteAssign:
  .BYTE 1, 0, 0, 1, 1, 0
  .BYTE 1, 1, 0, 1, 1, 0
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  data_NothingText  [$A048 :: 0x32058]
;;
;;    A strange place to store the "Nothing" text that appears when you select
;;  an empty menu.

data_NothingText:
  .BYTE $97, $B2, $B7, $AB, $AC, $B1, $AA, $00, $00
  ;       N    o    t    h    i    n    g   <terminator>
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DoMagicFlash  [$A051 :: 0x32061]
;;
;;    Flashes the color of the magic spell onto the background of the screen.  This routine actually
;;  takes 4 frames, 2 are drawn normal, and 2 are drawn with the flashed bg color
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DoMagicFlash:
    LDA btl_usepalette + $18 + 1    ; get the color of the magic spell from the usepalette
    CMP #$20                        ; if it's white, replace it with gray so it isn't
    BNE :+                          ;   so overwhelming
      LDA #$10
  : STA $89                         ; store in tmp  ($89 is the flash color)
  
    LDA btl_palettes + $10          ; get the original BG color
    STA $6D16                       ; back it up (probably unnecessary, since btl_palettes is never changed)
    
    JSR @FrameNoPaletteChange       ; Draw a normal frame
    
    LDA $89                         ; change the BG color to the flash color
    JSR @FramePaletteChange         ;  and draw 2 more frames
    JSR @FrameNoPaletteChange
    
    LDA $6D16                       ; restore original BG color and draw another frame
  ; JMP @FramePaletteChange         ;   <- code flows into this routine
    
  @FramePaletteChange:
    STA btl_usepalette + $10
    JMP DoFrame_UpdatePalette
    
  @FrameNoPaletteChange:
    JSR WaitForVBlank_L
    JSR BattleUpdatePPU
  ; JMP BattleUpdateAudio  ; <- code flows into this routine
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Battle Update Audio  [$A07C :: 0x3208C]
;;
;;  Updates battle sound effects and music playback.  Called every frame to keep audio playback moving
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BattleUpdateAudio:
    LDA #BANK_THIS
    STA a:cur_bank          ; set the swap-back bank (necessary because music playback is in another bank)
    LDA a:music_track
    BPL :+                  ; if the high bit of the music track is set (indicating the current song is finished)...
      LDA btl_followupmusic ;   then play the followup music
      STA a:music_track
:   JSR CallMusicPlay_L     ; Call music playback to keep it playing
    JMP UpdateBattleSFX     ; and update sound effects to keep them playing
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DrawPlayerAttackerCombatBox  [$A092 :: 0x320A2]
;;
;;  input:   A = player ID
;;
;;    This routine will draw the player's name in the Attacker combat box.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawPlayerAttackerCombatBox:
    AND #$03                            ; mask out interesting bits (flip off high bit)
    CLC
    ADC #$04                            ; convert to format code for player name  [4,7]
    TAY
    LDA #$00
    TAX                                 ; A X Y = 00 00 Name
    BEQ PrepareAndDrawSimpleCombatBox   ; (always branch) -- draw combat box 0, with character's name as contents
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DrawBattleMessageCombatBox [$A09D :: 0x320AD]
;;
;;  input:  A = battle message ID to draw
;;
;;    This is used to draw the battle messages that appear in the bottom
;;  combat boxes.  Like "Terminated", "Critical Hit!", etc.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawBattleMessageCombatBox:
    TAX             ; Move battle message ID to X
    LDA #$04        ; A=4 is the bottom (wide) combat box.
    LDY #$0F        ; 0F is the control code for printing battle messages (see FormatBattleString)
  ; JMP PrepareAndDrawSimpleCombatBox    ; <- flow into


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  PrepareAndDrawSimpleCombatBox [$A0A2 :: 0x320B2]
;;
;;  input:   A = ID of combat box to draw  (0-5)
;;         Y,X = format codes to draw in that box
;;
;;    This routine merely puts the byte string 'YY XX 00' in the unformatted buffer
;;  for the desired combat box, then draws it.  This can be used to easily
;;  display some simple combat boxes.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrepareAndDrawSimpleCombatBox:
    STY $68B1                   ; backup Y (control code)
    PHA                         ; backup A
    
    ASL A                       ; get pointer to this combat box's unformatted buffer
    TAY
    LDA lut_UnformattedCombatBoxBuffer, Y
    STA $88
    LDA lut_UnformattedCombatBoxBuffer+1, Y
    STA $89                     ; put in $88,89
    
    LDY #$00
    LDA $68B1                   ; get the control code
    STA ($88), Y                ; write it to pos 0
    INY
    TXA
    STA ($88), Y                ; write secondary byte to pos 1
    INY
    LDA #$00
    STA ($88), Y                ; write terminator to pos 2
    
    LDX $88                     ; put source pointer for drawing in YX
    LDY $89
    
    INC btl_combatboxcount      ; count this combat box
    
    PLA                         ; restore combat box ID in A
    JMP DrawCombatBox_L         ; draw the combat box!
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  lut_UnformattedCombatBoxBuffer [$A0CD :: 0x320DD]
;;
;;   The addresses for the unformated buffer for each combat box

lut_UnformattedCombatBoxBuffer:
  .WORD btl_unfmtcbtbox_buffer
  .WORD btl_unfmtcbtbox_buffer + $10
  .WORD btl_unfmtcbtbox_buffer + $20
  .WORD btl_unfmtcbtbox_buffer + $30
  .WORD btl_unfmtcbtbox_buffer + $40
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  RespondDelay_ClearCombatBoxes [$A0D7 :: 0x320E7]
;;
;;  Waits for the Respond Rate, then clears all drawn combat boxes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

RespondDelay_ClearCombatBoxes:
    JSR RespondDelay            ; respond rate wait
    LDA btl_combatboxcount
    JSR UndrawNBattleBlocks_L   ; clear all combat boxes
    LDA #$00
    STA btl_combatboxcount      ; zero combat box count
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  RespondDelay [$A0E6 :: 0x320F6]
;;
;;    Waits the appropriate number of frames, as indicated by the player's
;;  Respond Rate setting.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

RespondDelay:
    LDA btl_responddelay        ; get the delay
    STA $6AD0                   ; stuff it in temp ram as loop counter
    : JSR WaitForVBlank_L       ; wait that many frames
      JSR BattleUpdateAudio     ; updating audio each frame
      DEC $6AD0
      BNE :-
    RTS
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  lut_CharacterOAMOffset [$A0F8 :: 0x32108]
;;
;;    Offset for each character's sprite data in OAM
lut_CharacterOAMOffset:
  .BYTE $10, $28, $40, $58
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  FlashCharacterSprite [$A0FC :: 0x3210C]
;;
;;    Flashes the on-screen sprite for a character in battle.  This is
;;  done when an enemy attacks the player, or if a spell is cast on the player
;;
;;  input:  A = index of character to flash (0-3)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FlashCharacterSprite:
    TAY
    LDA lut_CharacterOAMOffset, Y   ; get character's OAM offset
    STA $6AAA                       ; back it up
    
    TAX                 ; X = OAM offset as source index
    LDY #$00            ; Y = loop counter and dest index 
    : LDA oam, X
      STA $6AAC, Y      ; copy all this character's sprite data to temp mem buffer
      INX
      INY
      CPY #6*4          ; 6 sprites * 4 bytes per sprite
      BNE :-
      
    LDA #$10
    STA $6BAD           ; Main Loop counter
    
  @MainLoop:
    LDX $6AAA           ; X = OAM offset
    LDY #$00            ; Y = inner loop counter
    
    LDA $6BAD
    AND #$02            ; every 2 iterations of main loop, toggle character's visibility
    BEQ @ShowSpriteLoop
    
  @HideSpriteLoop:
      LDA #$F0          ; hide sprite by moving if offscreen
      STA oam, X
      INX
      INY
      CPY #6*4
      BNE @HideSpriteLoop
    BEQ @NextIteration  ; (always branches)
  
  @ShowSpriteLoop:
      LDA $6AAC, Y      ; show sprite by restoring original sprite data
      STA oam, X
      INX
      INY
      CPY #$18
      BNE @ShowSpriteLoop
  
  @NextIteration:
    JSR BattleFrame     ; Do a frame and update sprites
    DEC $6BAD
    BNE @MainLoop       ; Keep looping until main counter expires
    
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  PrepCharStatPointers  [$A145 :: 0x32155]
;;
;;  Fills btl_ib_charstat_ptr and btl_ob_charstat_ptr
;;
;;  input:  A = the desired char index 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrepCharStatPointers:
    ASL A                               ; 2* for pointer lut
    TAY
    
    LDA lut_IBCharStatsPtrTable, Y      ; copy pointers from pointer luts
    STA btl_ib_charstat_ptr
    LDA lut_IBCharStatsPtrTable+1, Y
    STA btl_ib_charstat_ptr+1
    
    LDA lut_CharStatsPtrTable, Y
    STA btl_ob_charstat_ptr
    LDA lut_CharStatsPtrTable+1, Y
    STA btl_ob_charstat_ptr+1
    
    RTS
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  lut_InitialTurnOrder  [$A15C :: 0x3216C]
;;
;;  lut used to initialize battle turn order.  Simply contains IDs for all enemies/characters.

lut_InitialTurnOrder:
  .BYTE $00, $01, $02, $03, $04, $05, $06, $07, $08     ; enemy IDs
  .BYTE $80, $81, $82, $83                              ; character IDs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DoBattleRound  [$A169 :: 0x32179]
;;
;;  Do a single round of battle!!!!
;;
;;    This is the automated portion of battle -- after the user has input
;;  commands for all the characters.  This routine will return after all animation
;;  and automation for the round is over.
;;
;;  output:  btl_result
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DoBattleRound:
    LDY #$0D                        ; initialize turn order buffer
    : LDA lut_InitialTurnOrder-1, Y ;  by just copying values from a lut
      STA btl_turnorder-1, Y
      DEY
      BNE :-
    ; at this point, Y=0
    
    STY btl_combatboxcount          ; clear the battle box counter
    STY btl_result                  ; Zero the battle result to indicate we should keep looping)
    
    
    ; Shuffle the turn order.  This is done by looping 16 times, each time
    ;  it will pick 2 random entries in the turn order table and swap them.
    ;
    ; Note that this is suboptimal, since it's more likely for an entry to
    ; remain unmoved than a typical iterative swap method.  So the heroes get
    ; a little screwed here, since they have a higher chance of remaining at
    ; the end of the turn order list
    ;
    ; You could argue that this is BUGGED.  The fix would be to rewrite this
    ; loop to do a traditional iterative shuffle algorithm.
    
    LDA #$10 + 1
    STA btl_curturn             ; temp loop counter (loop 16 times.  +1 because it's 1-based)
  @ShuffleTurnOrderLoop:
    LDA #$00
    LDX #$0C
    JSR RandAX                  ; pick two random indexes
    PHA
    LDA #$00
    LDX #$0C
    JSR RandAX
    TAY                         ; one in Y
    PLA
    TAX                         ; one in X
    
    LDA btl_turnorder, Y        ; swap those entries in the turn order
    PHA
    LDA btl_turnorder, X
    STA btl_turnorder, Y
    PLA
    STA btl_turnorder, X
    
    DEC btl_curturn             ; keep looping until counter expires
    BNE @ShuffleTurnOrderLoop
    
    ; at this point, btl_curturn = 0
  @PerformBattleTurnLoop:
    LDY btl_curturn
    
    LDA #$00
    STA btl_defender_ailments   ; zero defender ailments (important for turn output)
    
    LDA btl_turnorder, Y        ; get whoever's turn it is
    BMI @TakeTurn               ; if it's a player, take their turn
      TAY                       ; otherwise it's an enemy... so:
      LDA btl_enemyIDs, Y       ; get this slot's enemy ID
      CMP #$FF
      BEQ @NextTurn             ; if the slot is empty, skip their turn
      LDA btl_strikingfirst     ; if the party is striking first
      BNE @NextTurn             ;   skip their turn
      TYA                       ; otherwise, put the SLOT ID in A, and take their turn
      
  @TakeTurn:
    JSR Battle_DoTurn                       ; do their turn
    JSR DrawCharacterStatus                 ; update character on-screen stats
    JSR BattleTurnEnd_CheckForBattleEnd     ; wrap up / see if battle is over (will double-RTS if battle is over)
  @NextTurn:
    INC btl_curturn
    LDA btl_curturn
    CMP #9+4                        ; 9 enemy slots + 4 player slots
    BNE @PerformBattleTurnLoop      ; keep looping until all entities have had a turn
    
    
    ;; once all entities have had their turn, the round is over.
    LDA #$00
    STA btl_strikingfirst       ; clear striking first flag so enemies can now act.
    
    JSR ApplyEndOfRoundEffects
  DoBattleRound_RTS:
    RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BattleTurnEnd_CheckForBattleEnd  [$A1DB :: 0x321EB]
;;
;;    Removed defeated defenders from combat and checks to see if the battle should
;;  end.  Note that it only checks if the defender was defeated
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BattleTurnEnd_CheckForBattleEnd:
    LDA btl_defender_ailments       ; Check to see if the defender has died
    AND #(AIL_DEAD | AIL_STONE)
    BEQ DoBattleRound_RTS           ; if not, nothing to do here, so just exit
        ; NOTE!!!  The game is BUGGED because if the last remaining enemy is
        ;  confused, and if they kill themselves, then btl_defender_ailments will
        ;  not be set properly and the battle will not exit.  An alternative fix for this
        ;  is simple... instead of branching to an RTS here, just branch to CheckForBattleEnd.
        ;  That way the game will check for end of battle after EVERY turn, instead of just
        ;  turns where an entity died.
    
    ; Code reaches here if the defender is dead/stone
    LDA battle_defenderisplayer
    BNE @PlayerKilled           ; see if they're a player or an enemy
    
      ; If they were an enemy...
      LDY battle_defender_index ; get their ID
      LDA btl_enemyIDs, Y
      CMP #$FF                  ; See if they were already removed from the roster.
      BEQ CheckForBattleEnd     ;  If they were, then just jump ahead to checking for battle end
      
      LDA #$FF
      STA btl_enemyIDs, Y       ; otherwise, remove this enemy from the lineup
      TYA
      STA btl_defender
      JSR EraseEnemyGraphic     ; and erase the enemy graphic
      
      JMP CheckForBattleEnd     ; Then check for battle end
    
  @PlayerKilled:                ; if killed defender was a player
    LDA battle_defender_index   ; get ID of the killed/stoned player
    AND #$03
    ASL A
    ASL A                       ; *4 to create a command index
    TAY
    LDA #$00
    STA btl_charcmdbuf, Y       ; erase their buffered command (can't perform actions when you're dead!)
  ; JMP CheckForBattleEnd       ; <- flow into -- check to see if the battle is over.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  CheckForBattleEnd  [$A20D :: 0x3221D]
;;
;;    Checks to see if all enemies or players have been defeated.
;;
;;  output:
;;    none/normal RTS if battle is to resume
;;    btl_result set appropriate and Double-RTS if battle is to end.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CheckForBattleEnd:
    LDA btl_drawflagsA      ; check drawflags to get party 'dead' status
    ORA btl_drawflagsB      ; combine with 'stone' status
    AND #$0F                ; isolate low bits.
    CMP #$0F                ; if all 4 bits are set, then all players are either dead or stone (game over)
    BNE :+
      INC btl_result            ; party is dead... set btl_result to 1 (party defeated)
      BNE @DoubleRTS            ; always branch -- double RTS to break out of the battle round
    
  : LDY #$09                ; If the party isn't dead, loop through all enemy slots to see if enemies are dead
  @EnemyLoop:
      LDA btl_enemyIDs-1, Y     ; check this slot (-1 because Y is 1-based)
      CMP #$FF
      BNE @Exit                 ; If enemy slot is not empty, enemy party is not defeated, so just exit
      DEY
      BNE @EnemyLoop            ; loop to check all 9 enemy slots
    
    LDA #$02                ; we reach here if all enemy slots are empty
    STA btl_result          ; set btl_result to 2 (enemies defeated)
                            ;   and flow into DoubleRTS to break out of battle round.
  @DoubleRTS:
    PLA
    PLA
  @Exit:
    RTS
    
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ApplyRegenToAllEnemies  [$A232 :: 0x32242]
;;
;;    Enemies that belong to the Regenerative category recover 3
;;  HP each round.  Kind of a weird thing...
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ApplyRegenToAllEnemies:
            @id = $6BAD         ; local - enemy ID and loop up-counter
            @loopctr = $68AF    ; local - loop down-counter
            @ram = $88          ; local - pointer to enemy RAM stats
            @rom = $8A          ; local - pointer to enemy ROM stats
            
    LDA #0
    STA @id
    LDA btl_enemycount
    STA @loopctr
    
  @MainLoop:
    LDA @id
    JSR GetEnemyStatPtr
    STA @ram
    STX @ram+1              ; get the pointer to RAM stats and record it
    
    LDY #en_romptr          ; use that to get the ROM pointer, and record it
    LDA (@ram), Y
    STA @rom
    INY
    LDA (@ram), Y
    STA @rom+1
    
    ; Now that we have RAM and ROM pointers for this enemy...
    LDY #ENROMSTAT_CATEGORY     ; Get the enemy category, and see if they are
    LDA (@rom), Y               ;  regenerative
    AND #CATEGORY_REGEN
    BEQ @Next                   ; If not, skip them -- go to next iteration
    
    LDY #en_hp                  ; move their HP to the math buffer
    LDA (@ram), Y
    STA btl_mathbuf
    INY
    LDA (@ram), Y
    STA btl_mathbuf+1
    
    LDA #$00
    LDX #$03
    JSR MathBuf_Add             ; regenerative enemies recover 3 HP
    
    LDY #ENROMSTAT_HPMAX        ; put HP max in mathbuf2
    LDA (@rom), Y               ;  ... because a normal CMP would be too simple
    STA btl_mathbuf+2           ;  ...
    INY                         ;  ...
    LDA (@rom), Y
    STA btl_mathbuf+3
    
    LDY #$00                    ; compare HP and MaxHP buffers
    LDX #$01
    JSR MathBuf_Compare         ; C will be set if HP >= HPMax
    
    BCC :+                      ; cap at Max HP
      LDA btl_mathbuf+2
      STA btl_mathbuf+0
      LDA btl_mathbuf+3
      STA btl_mathbuf+1
      
  : LDY #en_hp                  ; move HP back to RAM stats
    LDA btl_mathbuf
    STA (@ram), Y
    INY
    LDA btl_mathbuf+1
    STA (@ram), Y
    
  @Next:
    INC @id                     ; loop until all enemies counted
    DEC @loopctr
    BNE @MainLoop
    
    RTS
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ApplyEndOfRoundEffects  [$A2A6 :: 0x322B6]
;;
;;    Applies end of round effects (poison damage, regenerative recovery)
;;  Note that poison damage is NOT applied to enemies, meaning that enemies are
;;  immune to poison.  You could argue that this is BUGGED, but I don't think there
;;  are any in-game means to poison an enemy.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ApplyEndOfRoundEffects:
    LDA #$00                    ; apply poison to all 4 players
    JSR ApplyPoisonToPlayer
    LDA #$01
    JSR ApplyPoisonToPlayer
    LDA #$02
    JSR ApplyPoisonToPlayer
    LDA #$03
    JSR ApplyPoisonToPlayer
    
    JSR DrawCharacterStatus     ; redraw character stats to reflect post-poison HP
    JSR ApplyRegenToAllEnemies  ; apply regen to all enemies
    
    JMP CheckForBattleEnd       ; poison may have killed the party -- check for battle end.
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ApplyPoisonToPlayer  [$A2C3 :: 0x322D3]
;;
;;  input:  A = ID of player (0-3)
;;
;;    Note that poison is applied even if the player is stoned.  Meaning if a player
;;  is both poisoned and stoned, their life will drain.  One could argue that this is
;;  BUGGED -- since a stoned character cannot receive or recover damage by any other
;;  means.... so you wouldn't expect them to receive it from poison.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ApplyPoisonToPlayer:
            @id    = $685A  ; local - temp to hold character ID
            @index = $6BAD  ; local - temp to hold OB stat index (00, 40, 80, C0)
            
    STA @id             ; record character ID
    ASL A
    ASL A
    ASL A
    ASL A
    ASL A
    ASL A               ; left shift 6 to convert to index
    TAX                 ; put index in X
    STA @index          ; and back it up for later
    
    LDA ch_ailments, X  ; get this character's ailments
    AND #AIL_POISON
    BEQ @Exit           ; if they're not poisoned, nothing to do here, so exit
    
    LDA ch_curhp, X     ; otherwise, put their HP in the math buffer (GROAN -- why don't you just
    STA btl_mathbuf     ;  subtract normally?!?!  math buffer is overly complex for this!)
    LDA ch_curhp+1, X
    STA btl_mathbuf+1
    
    LDA #$00
    LDX #$02
    JSR MathBuf_Sub     ; subtract 2 HP for poison
    
    LDY btl_mathbuf         ; GRAAAH DRIVE ME INSANE GAME DRIVE ME INSANE!!!  GRAAAH
    LDX btl_mathbuf+1       ;  (this clips at zero, but is 100% unnecessary.  I rant about
    JSR ZeroXYIfNegative    ;   this elsewhere in the code)
    STY btl_mathbuf
    STX btl_mathbuf+1
    
    LDA #$00
    JSR MathBuf_NZ          ; update NZ flags according to math buffer
    BNE @ExtractFromMathBuf ; if the poison has killed them....
    
      LDA #$00                  ; clip their HP at zero.... again... because...
      STA btl_mathbuf           ;  ...  3rd time's a charm... right?
      STA btl_mathbuf+1         ;  (grrrrrrr)
      
      LDX @index
      LDA #AIL_DEAD
      STA ch_ailments, X        ; give them the DEAD ailment
      
      LDA @id                   ; then remove their action from the command buffer
      AND #$03                  ;   this is entirely unnecessary, since poison damage is only
      ASL A                     ;   applied between rounds
      ASL A
      TAY
      LDA #$00
      STA btl_charcmdbuf, Y
      
  @ExtractFromMathBuf:
    LDX @index
    LDA btl_mathbuf             ; take their HP back out of the math buffer
    STA ch_curhp, X
    LDA btl_mathbuf+1
    STA ch_curhp+1, X
    
  @Exit:
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  WalkForwardAndCastMagic  [$A32D :: 0x3233D]
;;
;;    Update the variable palette, then walk the character forward, draw the
;;  magic animation, and walk back.
;;
;;  input:
;;     A = Character index to walk forward  (0-3)
;;     X = zero -> draw magic sprite and flashy effect  (for actual magic spells)
;;         nonzero -> don't draw sprite/flashy effects  (for item usage)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

WalkForwardAndCastMagic:
    STX btlattackspr_nodraw     ; write nodraw flag
    PHA                         ; (backup character index)
    ASL A
    TAY                         ; *2 char index in Y, to get their magic graphic
    LDX btlcmd_magicgfx, Y
    TXA                         ; graphic in X & pushed to stack
    PHA
    TYA 
    PHA                         ; push 2*char index  (pointless)
    
    LDA btlcmd_magicgfx+1, Y    ; A = palette
    LDX #$01                    ; X = nonzero to indicate magic palette
    JSR UpdateVariablePalette   ; update the palette
    
    PLA
    TAY                         ; retore 2*char index in Y  (pointless because we never use it)
    PLA
    TAX                         ; restore graphic ID in X
    PLA                         ; restore character index in A
    LDY #$01                    ; Y = nonzero to indicate doing the magic effect
    JSR WalkForwardAndStrike    ; walk forward and strike with magic!!!
    
    LDA #$00
    STA btlattackspr_nodraw     ; clear the nodraw flag
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Battle_DoTurn  [$A352 :: 0x32362]
;;
;;    Do the battle animation and action for an entity's turn.
;;  
;;  input:  A = entity whose turn to take:
;;              00-08 -> enemy slot ID
;;              80-83 -> player ID
;;
;;          N = set to reflect high bit of A
;;
;;
;;  output:
;;      battle_defenderisplayer
;;      battle_defender_index
;;      btl_defender_ailments
;;
;;    The output for this routine is designed with basic physical attacks
;;  in mind.  Very simply, the defender ID, type, and ailments are retained, so
;;  that after this entity's turn is over, another routine can check to see if
;;  the defender died -- and if they did, it can remove/erase the entity from battle
;;  and check to see if battle is over.
;;
;;    Where this gets weird is that not all actions result in this simple single-defender
;;  logic.  Specifically, spells that have multiple targets don't work so well if the
;;  output only tracks one defender.  Therefore magic effects will have to remove
;;  entities from battle on their own.
;;
;;    What's WORSE is that the game doesn't check to see if the battle is over UNLESS
;;  this output indicates that at least one entity died.
;;
;;    Therefore magic logic, and running logic, and whatever other logic has to "fake"
;;  this output so that the end-of-battle checking doesn't choke.  Magic logic in
;;  particular will use btlmag_fakeout_xxx to "compile" appropriate output.  These
;;  fakeout vars will be copied to the above output before Battle_DoTurn exits.
;;
;;    This is incredibly goofy and overly complicated.  Like most of this combat code!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Battle_DoTurn:
    BMI Battle_DoPlayerTurn     ; if high bit set, this is a player, do a player turn
    JMP Battle_DoEnemyTurn      ; otherwise, do an enemy turn

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Battle_DoPlayerTurn  [$A357 :: 0x32367]
;;
;;  input:  A = player ID  ($80-83)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Battle_DoPlayerTurn:
        @playerid = $88         ; local - holds player ID (0-3)
        
    AND #$03                    ; mask off the high bit to get the player ID
    STA @playerid               ;  and record it for future use
    
    ASL A
    ASL A
    TAY                         ; id*4 in Y (command index)
    
    LDA btl_charcmdbuf, Y       ; get command byte
    LSR A                       ; shift out bit 0 ('dead' bit)      and throw away
    LSR A                       ; shift out bit 1 ('stone' bit)     and throw away
    
    LSR A                       ; shift out bit 2 ('attack' bit)
    BCC :+                      ; if set... attack the enemy
      LDX btl_charcmdbuf+2, Y           ; X = enemy target
      LDA @playerid                     ; A = attacker
      JMP PlayerAttackEnemy_Physical    ; Do the attack!
      
  : LSR A                       ; shift out bit 3 ('drink' bit)
    BCC :+                      ; if set... drink a potion!
      TYA
      PHA                           ; push command index
      
      LDY @playerid
      LDX btl_charcmdconsumeid, Y   ; get the potion they're using in X (0=heal, 1=pure)
      DEC item_heal, X              ; actually remove the potion from their inventory
      
      PLA
      TAY                           ; restore command index
      
      LDA btl_charcmdbuf+1, Y       ; get the effect ID in A ($40 for heal, $41 for pure)
      LDX btl_charcmdbuf+2, Y       ; get the target in X
      LDY @playerid                 ; get the actor in Y
      JMP Player_DoDrink
      
  : LSR A                       ; shift out bit 4 ('item' bit)
    BCC :+                      ; if set... use item!
      LDA btl_charcmdbuf+1, Y       ; A = effect ID
      LDX btl_charcmdbuf+2, Y       ; X = target
      LDY @playerid                 ; Y = attacker
      JMP Player_DoItem
      
  : LSR A                       ; shift out bit 5 ('run' bit)
    BCC :+
      LDA @playerid                 ; load this player's ID
      JMP Battle_PlayerTryRun       ; try to run!
      
  : LSR A                       ; shift out bit 6 ('magic' bit)
    BCC :+
        TYA                         ; back up command index
        PHA
        
        LDY @playerid
        LDX btl_charcmdconsumeid, Y ; get the level of this spell
        DEC ch_mp, X                ; take away a spell charge
        
        PLA                         ; restore command index
        TAY
        
        LDA btl_charcmdbuf+1, Y     ; A = effect
        LDX btl_charcmdbuf+2, Y     ; X = target
        LDY @playerid               ; Y = attacker
        JMP Player_DoMagic
    
    ;;  Code reaches here if the player had no command, which would only happen if they are
    ;;  immobilized or dead.
    
    :
        @ail = $89          ; local, temp ram to hold ailments
    
    LDA @playerid
    JSR PrepCharStatPointers        ; load player's ailments
    LDY #ch_ailments - ch_stats
    LDA (btl_ob_charstat_ptr), Y
    STA @ail
    
    AND #AIL_SLEEP          ; are they asleep?
    BEQ :+
      LDA @playerid         ; if yes, try to wake up
      JMP Battle_PlayerTryWakeup
      
  : LDA @ail
    AND #AIL_STUN           ; are they stunned?
    BEQ :+
      LDA @playerid
      JMP Battle_PlayerTryUnstun    ; if yes, try to unstun
      
    ; otherwise, they simply don't have an action or they're dead/stone
    ;   so just exit without doing anything
  : RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Battle_PlayerTryRun  [$A3D8 :: 0x323E8]
;;
;;  Player taking their turn trying to run.
;;
;;  input:  A = player ID
;;
;;    NOTE!  This routine will double-RTS on run success!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Battle_PlayerTryRun:
    PHA                             ; backup player ID
    JSR DrawPlayerAttackerCombatBox ; draw attacker box
    PLA                             ; restore ID
    
    JSR PrepCharStatPointers        ; prep stat pointers for later
    
    LDA btl_strikingfirst           ; are we striking first?
    BNE @Success                    ; If yes, run success is guaranteed
    
    LDA btlform_norun
    AND #$01                        ; if the 'no run' bit is set in the formation,
    BNE @Fail                       ;   then failure is guaranteed.
    
    ; Formula for running is pretty simple.
    ;
    ;   Run if:  Luck > random[0, level+15]
    ;
    ; The problem is... the game is BUGGED and reads the wrong value for the level.. it ends up reading
    ;  the ailment byte for 2 players after this one (for top 2 slots) or other garbage memory for the
    ;  bottom 2 players.
    ;
    ; This code is also WAAAY more complicated and bigger than it needs to be.  This could be done with a
    ;   simple CMP instruction -- why they decided to bring the math buffer into this is beyond me.
    
    LDY #ch_level - ch_stats        ; Get Level
    LDA (btl_ib_charstat_ptr), Y    ; BUGGED - Reading wrong value -- should be OB, not IB
    CLC
    ADC #15                         ; level + 15
    TAX                             ; put it in X
    
    LDA #$00
    STA btl_mathbuf+3               ; zero high byte of math buffer
    JSR RandAX                      ; random[ 0, level+15 ]
    TAX                             ; random value in X
    
    LDY #ch_luck - ch_stats         ; get player luck
    LDA (btl_ob_charstat_ptr), Y
    
        ; At this point, the game could just STX tmp, CMP tmp and be done with it.. but noooo.  Bring the math buffer into it!
        ;  make it even more complicated!  Waste all sorts of space!
    
    STA btl_mathbuf+2               ; mathbuf+2 now has the player's luck
    LDA #$01
    JSR MathBuf_Sub                 ; mathbuf+2 is now luck-randomval
    
    LDY btl_mathbuf+2               ; clip at zero.  WHY THE FUCK DO YOU KEEP DOING THIS AFTER EVERY SINGLE CALL TO
    LDX btl_mathbuf+3               ; MATHBUF_SUB, FINAL FANTASY!?!?  MATHBUF_SUB ALREADY FREAKING CLIPS THE VALUE! THERE
    JSR ZeroXYIfNegative            ; IS NO NEED TO WASTE FIFTEEN FUCKING BYTES TO DO SOMETHING THAT WAS ALREADY DONE. YOU
    STY btl_mathbuf+2               ; DO IT EVERY TIME, IT IS DRIVING ME CRAZY! MY BRAIN IS GOING TO EXPLODE!!!!!1
    STX btl_mathbuf+3               ; GRAAAAAAAAAH
    
    LDA btl_mathbuf+2               ; success if low byte was nonzero (if luck > randomval)
    BNE @Success                    ; failure if zero (luck <= randomval)
    
  @Fail:
    LDA #BTLMSG_CANTRUN             ; on failure, print 'Can't Run' 
    JMP DrawBtlMsg_ClearCombatBoxes ; then clear all combat boxes and exit
  
  @Success:
    LDA #BTLMSG_CLOSECALL           ; on success, print 'Close Call....' 
    JSR DrawBtlMsg_ClearCombatBoxes ; then clear all combat boxes
    
    LDA #$03
    STA btl_result                  ; Set the battle result to 3 (party ran)
    
    PLA                             ; double-RTS to exit the battle round
    PLA
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Battle_PlayerTryWakeup  [$A42F :: 0x3243F]
;;
;;  Player taking their turn trying to wake themselves up.
;;
;;  input:  A = player ID
;;
;;      This routine is horrendously inefficient.  The formula for waking up
;;  is very simple.  If MaxHP <= rand[0,$50], you stay asleep, otherwise you
;;  wake up.  This is a very easy check to do... but for whatever reason this
;;  routine complictes it by running everything through the math buffer and shit.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Battle_PlayerTryWakeup:
    PHA                             ; backup ID
    JSR DrawPlayerAttackerCombatBox ; draw attacker
    PLA                             ; retore ID
    
    JSR PrepCharStatPointers
    
    LDY #ch_maxhp - ch_stats        ; put max HP in mathbuf
    LDA (btl_ob_charstat_ptr), Y
    STA btl_mathbuf
    INY
    LDA (btl_ob_charstat_ptr), Y
    STA btl_mathbuf+1
    
    LDA #$00
    LDX #$50
    JSR RandAX                      ; random[$00,$50]
    
    TAX
    LDA #$00
    JSR MathBuf_Sub                 ; mathbuf -= random
    
    LDY btl_mathbuf                 ; clip at zero (pointless, MathBuf_Sub already does this!!!)
    LDX btl_mathbuf+1
    JSR ZeroXYIfNegative
    STY btl_mathbuf
    STX btl_mathbuf+1
    
    LDA #$00
    JSR MathBuf_NZ                  ; See if mathbuf is zero
    
    BNE :+                          ; if it is, stay asleep
        LDA #BTLMSG_SLEEPING            ; print "Sleeping" message, and exit
        JMP DrawBtlMsg_ClearCombatBoxes
    
  : LDY #btlch_ailments             ; remove sleep ailment from IB stats
    LDA (btl_ib_charstat_ptr), Y
    AND #~AIL_SLEEP
    STA (btl_ib_charstat_ptr), Y
    
    LDY #ch_ailments - ch_stats     ; and OB stats
    LDA (btl_ob_charstat_ptr), Y
    AND #~AIL_SLEEP
    STA (btl_ob_charstat_ptr), Y
    
    LDA #BTLMSG_WOKEUP              ; print "Woke up" message and exit
    JMP DrawBtlMsg_ClearCombatBoxes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Battle_PlayerTryUnstun  [$A481 :: 0x32491]
;;
;;  Player taking their turn trying to unstun themselves.
;;
;;  input:  A = player ID
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Battle_PlayerTryUnstun:
    PHA                                 ; backup player ID
    JSR DrawPlayerAttackerCombatBox     ; draw the attacker box
    PLA                                 ; restore player ID
    
    JSR PrepCharStatPointers            ; Get pointers to stats
    
    JSR BattleRNG_L
    AND #$03                            ; random [0,3]
    BEQ :+                              ; unstun if 0 (25% chance)
      LDA #BTLMSG_PARALYZED_B               ; otherwise, if nonzero, stay stunned
      JMP DrawBtlMsg_ClearCombatBoxes       ; print "Paralyzed" message, then clear boxes and end turn.
      
  : LDY #btlch_ailments                 ; remove Stun ailment from IB ailments
    LDA (btl_ib_charstat_ptr), Y
    AND #~AIL_STUN
    STA (btl_ib_charstat_ptr), Y
    
    LDY #ch_ailments - ch_stats         ; ... and also from OB ailments
    LDA (btl_ob_charstat_ptr), Y
    AND #~AIL_STUN
    STA (btl_ob_charstat_ptr), Y
    
    LDA #BTLMSG_CURED                   ; print "Cured!" message, and exit!
    JMP DrawBtlMsg_ClearCombatBoxes
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  CapXYAtFF  [$A4AA :: 0x324BA]
;;
;;  XY contains a 16-bit value.  If that value is > 00FF, cap it at 00FF
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CapXYAtFF:
    TXA
    BEQ :+          ; if X (high byte) is not zero
      LDX #$00      ; set XY to 00FF
      LDY #$FF
  : RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ZeroXYIfNegative [$A4B2 :: 0x324C2]
;;
;;  XY contains a 16-bit signed value.  If that value is negative, replace it with zero.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ZeroXYIfNegative:
    TXA             ; check high byte
    BPL :+          ; if positive, just exit.  Otherwise
      LDX #$00      ; XY = 0
      LDY #$00
  : RTS
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  PlayerAttackEnemy_Physical  [$A4BA :: 0x324CA]
;;
;;    Perform a physical attack of Player->Enemy.
;;  Simply prep stats and calls DoPhysicalAttack, then updates enemy HP/ailments.
;;  Does not erase the enemy if he is defeated.
;;
;;  input:
;;    A = attacking player index
;;    X = defending enemy slot index
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PlayerAttackEnemy_Physical:
    AND #$03
    STA battle_attacker_index       ; set player attacker index
        ; btl_attacker is updated later
    JSR PrepCharStatPointers        ; get pointer to attacker's OB and IB stats
    
    STX battle_defender_index       ; set defender index
    STX btl_defender
    
    TXA
    JSR GetEnemyStatPtr
    STA $84                         ; $84,85 points to enemy stats in RAM
    STX $85
    LDY #en_romptr
    LDA ($84), Y
    STA $86
    INY
    LDA ($84), Y
    STA $87                         ; $86,87 points to enemy stats in ROM
    
    ;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Attacker/PLAYER stats
    LDA #$00                        ; clear this value to zero to indicate the defender
    STA battle_defenderisplayer     ;   is an enemy
    
    LDY #btlch_dmg                  ; IB damage
    LDA (btl_ib_charstat_ptr), Y
    STA btl_attacker_strength
    
    LDY #btlch_category             ; BUGGED - this is pulling a player's category (always 0).  It should be
    LDA (btl_ib_charstat_ptr), Y    ;  pulling the category of the weapon they're using.
    STA btl_attacker_category
    
    LDY #btlch_elemweak             ; BUGGED - uses player elemental weakness as attack element.  This is always 0
    LDA (btl_ib_charstat_ptr), Y    ;  this should be pulling their elemental value from their equipped weapon
    STA btl_attacker_element
    
    LDY #btlch_hitrate              ; IB hit rate
    LDA (btl_ib_charstat_ptr), Y
    STA btl_attacker_hitrate
    
    LDY #btlch_numhitsmult          ; IB hit multiplier
    LDA (btl_ib_charstat_ptr), Y
    STA btl_attacker_numhitsmult
    
    LDY #btlch_numhits              ; IB num hits
    LDA (btl_ib_charstat_ptr), Y
    STA btl_attacker_numhits
    
    LDY #btlch_critrate             ; IB crit rate
    LDA (btl_ib_charstat_ptr), Y
    STA btl_attacker_critrate
    
    LDY #ch_ailments - ch_stats     ; OB ailments (why not IB?)
    LDA (btl_ob_charstat_ptr), Y
    STA btl_attacker_ailments
    
    LDY #btlch_wepgfx               ; IB weapon graphic
    LDA (btl_ib_charstat_ptr), Y
    STA btl_attacker_graphic
    
    LDY #btlch_wepplt               ; IB weapon palette
    LDA (btl_ib_charstat_ptr), Y
    STA btl_attacker_varplt
    
    ;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Defender/ENEMY stats
    LDY #en_ailments                ; ailment from RAM
    LDA ($84), Y
    STA btl_defender_ailments
    
    LDY #ENROMSTAT_CATEGORY         ; category from ROM
    LDA ($86), Y
    STA btl_defender_category
    
    LDY #ENROMSTAT_ELEMWEAK         ; elemental weakness from ROM
    LDA ($86), Y                    ; BUGGED - it should be taking this from enemy RAM stats, not from ROM stats
    STA btl_defender_elemweakness   ;   as a result, spells which modify this (XFER) have no effect when used on enemies.
    
    LDY #en_evade                   ; evade from RAM
    LDA ($84), Y
    STA btl_defender_evade
    
    LDY #en_defense                 ; absorb/defense from RAM
    LDA ($84), Y
    STA btl_defender_absorb
    
    LDY #en_hp                      ; HP from RAM
    LDA ($84), Y
    STA btl_defender_hp
    INY
    LDA ($84), Y
    STA btl_defender_hp+1
    
    
    LDA battle_attacker_index       ; set btl_attacker
    ORA #$80
    STA btl_attacker
    
    ;;;;;;;;;;;;;;;;;;;;
    JSR DoPhysicalAttack            ; Do the attack!!
    ;;;;;;;;;;;;;;;;;;;;
    
    LDY #en_ailments                ; IB ailements
    LDA btl_defender_ailments
    STA ($84), Y
    
    LDY #en_hp                      ; IB HP
    LDA btl_defender_hp
    STA ($84), Y
    INY
    LDA btl_defender_hp+1
    STA ($84), Y
    
    LDY #btlch_ailments             ; update attacker ailments (pointless, they're not changed by physical attacks)
    LDA btl_attacker_ailments
    STA (btl_ib_charstat_ptr), Y    ; IB ailment
    LDY #ch_ailments - ch_stats
    STA (btl_ob_charstat_ptr), Y    ; and OB ailment
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  EnemyAttackPlayer_Physical  [$A581 :: 0x32591]
;;
;;    Perform a physical attack of Enemy->Player.
;;  Simply prep stats and calls DoPhysicalAttack, then updates player HP/ailments.
;;
;;  input:
;;    A = defending player index
;;    X = attacking enemy slot index
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
EnemyAttackPlayer_Physical:
    AND #$03
    STA battle_defender_index       ; record the defender index
                                    ; btl_defender is set later
    JSR PrepCharStatPointers        ; get pointer to char stats in btl_ib_charstat_ptr and btl_ob_charstat_ptr
    
    STX battle_attacker_index       ; record attacker index
    STX btl_attacker
    TXA
    JSR GetEnemyStatPtr             ; get pointer to enemy stats in XA
    
    STA $84                 ; pointer to enemy RAM stats at $84,85
    STX $85
    LDY #en_romptr
    LDA ($84), Y
    STA $86
    INY
    LDA ($84), Y
    STA $87                 ; pointer to enemy ROM stats at $86,87
        
    ;;;;;;;;;;;;;;;;;;;;;
    ; Attacker ENEMY stats
    
    LDA #$01                    ; mark that the defender is a player and not an enemy
    STA battle_defenderisplayer
    
    LDY #en_strength            ; strength from RAM
    LDA ($84), Y
    STA btl_attacker_strength
    
    LDY #ENROMSTAT_CATEGORY     ; category from ROM (this doesn't really make sense because 
    LDA ($86), Y                ;   defending player does not have any category)
    STA btl_attacker_category
    
    LDY #ENROMSTAT_ELEMWEAK     ; uses enemy's elemental WEAKNESS (from ROM) as their attack element.  BUGGED ?
    LDA ($86), Y                ; This doesn't make any sense, as it leads to things like FrWolves
    STA btl_attacker_element    ;  attacking with fire.
    
    LDY #ENROMSTAT_HITRATE      ; hit rate from ROM
    LDA ($86), Y
    STA btl_attacker_hitrate
    
    LDY #en_numhitsmult         ; num hits multiplier from RAM
    LDA ($84), Y
    STA btl_attacker_numhitsmult
    
    LDY #ENROMSTAT_NUMHITS      ; num hits from ROM
    LDA ($86), Y
    STA btl_attacker_numhits
    
    LDY #ENROMSTAT_CRITRATE     ; crit rate from ROM
    LDA ($86), Y
    STA btl_attacker_critrate
    
    LDY #ENROMSTAT_ATTACKAIL    ; attack ailment from ROM  (curse you, Sorcerers!!!)
    LDA ($86), Y
    STA btl_attacker_attackailment
    
    LDY #en_ailments            ; inflicted ailments from RAM
    LDA ($84), Y
    STA btl_attacker_ailments
    
    ;;;;;;;;;;;;;;;;;;;;;
    ; Defender PLAYER CHARACTER stats
    LDY #btlch_category                 ; IB:  assigned category.  Always zero?  Players don't have categories.
    LDA (btl_ib_charstat_ptr), Y
    STA btl_defender_category
    
    LDY #btlch_elemweak                 ; IB:  elemental weakness.  (Always zero?)
    LDA (btl_ib_charstat_ptr), Y
    STA btl_defender_elemweakness
    
    LDY #btlch_evade                    ; IB evade
    LDA (btl_ib_charstat_ptr), Y
    STA btl_defender_evade
    
    LDY #btlch_absorb                   ; IB absorb
    LDA (btl_ib_charstat_ptr), Y
    STA btl_defender_absorb
    
    LDY #btlch_magdef                   ; IB magdef
    LDA (btl_ib_charstat_ptr), Y
    STA btl_defender_magdef
    
    LDY #btlch_elemresist               ; IB elemental resist
    LDA (btl_ib_charstat_ptr), Y
    STA btl_defender_elemresist
    
    LDY #(ch_curhp - ch_stats)          ; OB HP.  Why OB, shouldn't this be IB?  I think IB HP goes unused.
    LDA (btl_ob_charstat_ptr), Y
    STA btl_defender_hp
    INY
    LDA (btl_ob_charstat_ptr), Y
    STA btl_defender_hp+1
    
    LDY #(ch_ailments - ch_stats)       ; OB ailments... again why OB?  This is stored in IB, too.  Why not use that?
    LDA (btl_ob_charstat_ptr), Y        ;   I also think IB ailments go unused.
    STA btl_defender_ailments
    
    LDA battle_defender_index           ; fill btl_defender
    ORA #$80
    STA btl_defender
    
    ;;;;;;;;;;;;;;;;;;;;;;
    JSR DoPhysicalAttack                ; Do the freaking attack!!!!
    ;;;;;;;;;;;;;;;;;;;;;;
    
    LDY #en_ailments                ; update attacker's ailments
    LDA btl_attacker_ailments       ; This is totally pointless... attacker ailments are 
    STA ($84), Y                    ;   completely unaffected by the attack.
    
    LDY #btlch_hp                   ; this actually updates IB HP -- but again... it doesn't seem to be used
    LDA btl_defender_hp
    PHA                             ; the HP value is pushed, as well, so that it can be popped and written
    STA (btl_ib_charstat_ptr), Y    ;  to OB HP later.
    INY
    LDA btl_defender_hp+1
    PHA
    STA (btl_ib_charstat_ptr), Y
    
    LDY #(ch_curhp-ch_stats+1)      ; pull HP value from stack to write to OB HP
    PLA
    STA (btl_ob_charstat_ptr), Y
    DEY
    PLA
    STA (btl_ob_charstat_ptr), Y
    
    LDY #btlch_ailments             ; update both IB and OB ailments
    LDA btl_defender_ailments
    STA (btl_ib_charstat_ptr), Y
    LDY #(ch_ailments-ch_stats)
    STA (btl_ob_charstat_ptr), Y
    
    RTS                             ; done!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ClearMathBufHighBytes  [$A65A :: 0x3266A]
;;
;;    Clear the high bytes of the entries in the math buffer.
;;  A is expected to be 0 upon exit
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ClearMathBufHighBytes:
    LDA #$00
    STA btl_mathbuf +  1
    STA btl_mathbuf +  3
    STA btl_mathbuf +  5
    STA btl_mathbuf +  7
    STA btl_mathbuf +  9
    STA btl_mathbuf + 11
    STA btl_mathbuf + 13
    STA btl_mathbuf + 15
    STA btl_mathbuf + 17
    STA btl_mathbuf + 19
    RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DoPhysicalAttack  [$A67B :: 0x3268B]
;;
;;    Called to do all the mathematical calculations AND ANIMATIONS of a physical attack.
;;  This routine does everything, from the animation before the attack, to calculating the damage.
;;  to printing the on-screen messages.
;;
;;    The only things it DOESN'T do are:
;;  a) fetch attacker/defender stats
;;  b) apply new HP/ailment values to entity
;;  c) erase defeated enemies from the screen
;;
;;    Everything else (everything between steps 'a' and 'b') is done here.
;;
;;    This routine is HUGE -- and TREMENDOUSLY inefficient.  A lot of the math is done in
;;  an extremely convoluted way and is way more complicated and takes up way more code
;;  than it needs to.  Ranges are checked and values clipped multiple times.  There is
;;  some unnecessary storing of variables, only to load them again immediately.  And
;;  the way values are clipped don't even really make sense some of the time.
;;
;;   The only thing I can think of is that this code was generated with heavy use of
;;  macros or something -- I can't imagine anyone actually writing code like this by
;;  hand.  Especially since the style does not match the rest of the game at all.
;;
;;   There's too much input for this routine to list here, but basically it assumes that
;;  all btl_attacker_xxx and btl_defender_xxx vars have been prepped, as well as a few
;;  other battle variables in the $688x range.
;;
;;    'btl_defender_ailments' and 'btl_defender_hp' are the output for this routine, and
;;  should be applied to their respective entities afterward.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DoPhysicalAttack:
    LDA #$00        ; print '02 00 00' to combat box 0
    TAX             ;  (attacker name in attacker combat box)
    LDY #$02
    JSR PrepareAndDrawSimpleCombatBox
    
    LDA #$02        ; print '03 00 00' to combat box 2
    LDY #$03        ;  (defender name in defender combat box)
    LDX #$00
    JSR PrepareAndDrawSimpleCombatBox
    
    JSR ClearMathBufHighBytes   ; zero high bytes of math buffers
    
    LDA #168                    ; base hit chance of 168
    STA math_hitchance
    
    LDA btl_attacker_strength   ; base damage of attacker's strength value
    STA math_basedamage
    
    LDA btl_attacker_critrate   ; base crit chance of attacker's crit rate
    STA math_critchance
    
    LDA btl_attacker_ailments   ; if attacker has DARK status, penalty of 40 to their hit chance
    AND #AIL_DARK
    BEQ :+
      LDA math_hitchance
      SEC
      SBC #40
      STA math_hitchance
      
  : LDA btl_defender_ailments   ; if defender has DARK, bonus of 40 to attacker's hit chance
    AND #AIL_DARK
    BEQ :+
      LDA math_hitchance
      CLC
      ADC #40
      STA math_hitchance
      
  : LDA btl_attacker_category       ; see if attacker category matches defender category
    AND btl_defender_category
    STA math_category
    
    LDA btl_attacker_element        ; see if attacker element matches defender elemental weakness
    AND btl_defender_elemweakness
    STA math_element
    
    LDA math_category
    ORA math_element                    ; merge categoy/element matches
    BEQ :+                              ; if any weaknesses found...
      LDA #MATHBUF_HITCHANCE            ; +40 bonus to hit chance
      LDX #40
      JSR MathBuf_Add
      LDY math_hitchance
      LDX math_hitchance+1
      JSR CapXYAtFF
      STY math_hitchance
      STX math_hitchance+1              ; maximum hit chance of 255
      
      LDA math_basedamage               ; and +4 bonus to base damage
      CLC
      ADC #4
      STA math_basedamage
      BCC :+
        LDA #$FF
        STA math_basedamage             ; maximum base damage of 255

        
  : LDA btl_defender_ailments
    AND #AIL_STUN | AIL_SLEEP
    BEQ @DefenderMobile         ; if defender alseep or stunned....
      LDA math_basedamage       ; apply 25% bonus to base damage
      LSR A
      LSR A
      CLC
      ADC math_basedamage
      STA math_basedamage
      BCC :+                    ; (jump past the @DefenderMobile block)
        LDA #$FF
        STA math_basedamage     ; cap at 255 base damage
        JMP :+                  ; (jump past the @DefenderMobile block)
  
  @DefenderMobile:                      ; if the defender is mobile (not asleep of stunned)
      LDA #MATHBUF_HITCHANCE            ; add their hit rate to their hit chance
      LDX btl_attacker_hitrate          ;   This seems strange to me.  Shouldn't this be done even if defender
      JSR MathBuf_Add                   ;    is immobile?  Is this BUGGED?
      LDY math_hitchance
      LDX math_hitchance+1
      JSR CapXYAtFF                     ; cap at 255
      STY math_hitchance
      STX math_hitchance+1
    
      LDA #MATHBUF_HITCHANCE            ; and subtract the defender's evade rate from
      LDX btl_defender_evade            ;  the hit chance.
      JSR MathBuf_Sub
      LDY math_hitchance
      LDX math_hitchance+1
      JSR ZeroXYIfNegative              ; cap at 0  (not necessary to do here, since
      STY math_hitchance                ;   MathBuf_Sub already does this. Whatever)
      STX math_hitchance+1
    
    ;;;;;
  : LDA math_hitchance
    BNE :+
      INC math_hitchance                ; minimum hit chance of 1
      
  : LDA math_critchance
    CMP math_hitchance
    BCC :+
      LDA math_hitchance                ; crit chance cannot exceed hit chance
      STA math_critchance
      
  : LDA btl_attacker_numhits            ; get proper number of hits (numhits * mult)
    LDX btl_attacker_numhitsmult        ;  the mult is essentially the multiplier for the FAST spell
    JSR MultiplyXA
    STA math_numhits
    
    LDA math_numhits
    BNE :+
      INC math_numhits                  ; minimum of 1 hit
      
  : LDA btl_attacker_varplt     ; palette to use (unimportant/unused for enemy attacks)
    LDX #$00                    ; 0 for physical/weapon attacks, nonzero for magic attacks
    JSR UpdateVariablePalette
    
    LDA battle_defenderisplayer
    BNE @EnemyAttackingPlayer   ; jump ahead if enemy is attacking a player
    
  @PlayerAttackingEnemy:
    LDX btl_attacker_graphic    ; If the graphic is zero... then we shouldn't draw it
    BNE :+
      INC btlattackspr_nodraw   ; set set the 'nodraw' flag for attack sprites
      
  : LDX btl_attacker_graphic    ; weapon graphic
    LDY #$00                    ; 0 to indicate swinging a weapon
    LDA battle_attacker_index   ; the attacker (0-3)
    JSR WalkForwardAndStrike    ; Do the animation to walk the character forward, swing their weapon, and walk back
    
    LDA btl_defender_ailments
    AND #AIL_DEAD | AIL_STONE
    BNE :+                      ; if the enemy target is not already dead or stone...
      JSR DoExplosionEffect     ; play the "cha" sound effect and draw explosion animation on the enemy
      
  : LDA #$00
    STA btlattackspr_nodraw     ; clear the 'nodraw' flag for the attack sprites
    BEQ :+                      ; (always branch to just after @EnemyAttackingPlayer block)
    
  @EnemyAttackingPlayer:
    LDA #$01
    JSR PlayBattleSFX           ; play the "boom bash" sound effect
    JSR BattleScreenShake_L     ; do the 'screen shake' animation
    
    LDA btl_defender_ailments
    AND #AIL_DEAD | AIL_STONE
    BNE :+
      LDA battle_defender_index
      AND #$03                  ; if the player target is not already dead or stone...
      JSR FlashCharacterSprite  ; flash their character sprite
    
    
    ;;;;;;;
  : LDA btl_defender_ailments
    AND #AIL_DEAD | AIL_STONE
    BEQ :+                              ; if defender is dead/stone
      LDA #BTLMSG_INEFFECTIVE           ; draw the "Ineffective" battle message, clear all combat boxes, and exit
      JMP DrawBtlMsg_ClearCombatBoxes
      
  : LDA #$00
    STA battle_critsconnected
    STA battle_hitsconnected
    STA battle_totaldamage
    STA battle_totaldamage+1
    
    LDA math_basedamage
    STA $6BAD                   ; 6BAD is temp space for base damage.  Moved here because we will be writing over math_basedamage
    
  @HitLoop:             ;  [A7DD :: 327ED]
    JSR ClearMathBufHighBytes   ; A=0
    STA math_dmgcalc            ; zero damage calculation
    LDX #200
    JSR RandAX                  ; [0,200]
    STA math_randhit
    LDA math_randhit
    CMP #200
    BNE :+
       JMP @NextHitIteration    ; skip if got 200 exactly (guaranteed miss)
       
  : LDX $6BAD
    LDA #$00
    JSR RandAX
    CLC
    ADC $6BAD
    STA math_basedamage     ; random between [basedmg, basedmg*2]
    BCC :+                
      LDA #$FF
      STA math_basedamage   ; max 255
      
  : LDA math_hitchance
    CMP math_randhit        ; if hit chance is < randhit value, then this was a miss.  Jump ahead
    BCC @Miss
    
    LDA math_basedamage                     ; if it was a hit
    STA math_dmgcalc                        ;   take random calculated damage
    LDA #(math_dmgcalc - btl_mathbuf) / 2
    LDX btl_defender_absorb
    JSR MathBuf_Sub                         ;   subtract defender's absorb
    
    LDY math_dmgcalc            ; really inefficient way to set minimum of 1 damage
    LDX math_dmgcalc+1
    JSR ZeroXYIfNegative
    STY math_dmgcalc
    STX math_dmgcalc+1
    LDA math_dmgcalc
    ORA math_dmgcalc+1
    BNE :+
      INC math_dmgcalc
      
  : INC battle_hitsconnected    ; count number of hits connected
  
  @Miss:                    ; jumps here if we missed, but this code runs regardless of whether or not we hit/missed
    LDA math_critchance     ; see if hit value is <= crit chance  (this will be impossible if the attack was a miss)
    CMP math_randhit
    BCC :+                                  ; if it was... we scored a critical
      LDA #MATHBUF_DMGCALC
      LDX math_basedamage                   ; add random damage to our total
      JSR MathBuf_Add
      INC battle_critsconnected
      
  : LDA #MATHBUF_TOTALDAMAGE
    LDX #MATHBUF_TOTALDAMAGE
    LDY #MATHBUF_DMGCALC
    JSR MathBuf_Add16               ; total damage += dmgcalc
    
    LDA $6887
    BEQ @NextHitIteration           ; we're done if player is attacking
    LDA btl_attacker_attackailment
    BEQ @NextHitIteration           ; we're done if no ailment to apply
    LDA battle_hitsconnected
    BEQ @NextHitIteration           ; we're done if no hits connected yet
                                    ;  This is BUGGED!  Logically you would only perform this check if
                                    ;  THIS attack was a hit.  But instead, this will perform the check
                                    ;  if ANY attacks up to this point were hits.  This means once an enemy
                                    ;  connects with one of their hits, each swing afterwards will have a chance
                                    ;  to inflict an ailment, even if they miss!
    
    LDA #100
    STA math_ailmentchance          ; base chance of connecting ailment = 100
    LDA btl_defender_elemresist
    AND btl_attacker_element
    STA $6868                       ; ?pointless sta/lda
    LDA $6868
    BEQ :+                          ; if defender resists attacker's element
      LDA #$00                      ; chance to connect with ailment is zero  (later will be changed to 1)
      STA math_ailmentchance
      
  : LDA #MATHBUF_AILMENTCHANCE
    LDX btl_defender_magdef         ; subtract defender's magdef from ailment chance
    JSR MathBuf_Sub
    LDY math_ailmentchance
    LDX math_ailmentchance+1
    JSR ZeroXYIfNegative
    STY math_ailmentchance
    STX math_ailmentchance+1        ; another pointless clipping at zero even though MathBuf_Sub already does it
    
    LDA #MATHBUF_AILMENTCHANCE
    JSR MathBuf_NZ
    BNE :+
      INC math_ailmentchance        ; minimum ailment chance of 1
      
  : LDA #$00
    LDX #200
    JSR RandAX
    STA battle_ailmentrandchance    ; random value between [0,200]
    
    CMP #200
    BEQ @NextHitIteration           ; if == 200, skip ahead (no ailment)
    
    LDA math_ailmentchance
    CMP battle_ailmentrandchance
    BCC @NextHitIteration           ; if ailment chance >= rand value, apply the ailment!
      LDA btl_defender_ailments     ; Do some bit trickery to get only the ailments that
      EOR #$FF                      ;  the defender does not already have.
      AND btl_attacker_attackailment
      
      JSR PrintPlayerAilmentMessageFromAttack   ; print the message for those ailments
      
      LDA btl_defender_ailments     ; apply the ailment
      ORA btl_attacker_attackailment
      STA btl_defender_ailments
    
    ; Jumps here if not applying any ailment
  @NextHitIteration:
    DEC math_numhits
    BEQ :+
      JMP @HitLoop
      
  : LDA battle_hitsconnected
    CMP #$02                    ; if they connected with 2 or more hits, draw the # of Hits box
    BCC :+
      STA btl_unfmtcbtbox_buffer+$11    ; write:  11 xx 00 0F 2B 00   to unformatted buffer +$10   (where 'xx 00' is number of hits)
      LDA #$11                          ;   ('0F 2B' prints "hits!")
      STA btl_unfmtcbtbox_buffer+$10
      LDA #$00
      STA btl_unfmtcbtbox_buffer+$12
      STA btl_unfmtcbtbox_buffer+$15
      LDA #$0F
      STA btl_unfmtcbtbox_buffer+$13
      LDA #BTLMSG_HITS
      STA btl_unfmtcbtbox_buffer+$14
      
      LDA #$01                          ; draw it in combat box 1
      LDX #<(btl_unfmtcbtbox_buffer + $10)
      LDY #>(btl_unfmtcbtbox_buffer + $10)
      JSR DrawCombatBox_L
      
      INC btl_combatboxcount            ; inc box counter
      
  : LDA battle_totaldamage
    ORA battle_totaldamage+1
    BNE :+                              ; if there is zero damage....
      LDA #$0F                          ; print format code for "Missed!" into unformatted buffer
      STA btl_unfmtcbtbox_buffer + $30
      LDA #BTLMSG_MISSED
      STA btl_unfmtcbtbox_buffer + $31
      LDA #$00
      STA btl_unfmtcbtbox_buffer + $32
      BEQ @OutputDamageBox              ; (always branch)
      
  : LDA #$11                            ; if there is nonzero damage...
    STA btl_unfmtcbtbox_buffer + $30    ; print:  '11 xx xx 0F 2E 00'
    LDA battle_totaldamage              ;  where '11 xx xx' prints the damage
    STA btl_unfmtcbtbox_buffer + $31    ;  and '0F 2E' prints "DMG"
    LDA battle_totaldamage+1
    STA btl_unfmtcbtbox_buffer + $32
    LDA #$0F
    STA btl_unfmtcbtbox_buffer + $33
    LDA #BTLMSG_DMG
    STA btl_unfmtcbtbox_buffer + $34
    LDA #$00
    STA btl_unfmtcbtbox_buffer + $35
  
  @OutputDamageBox:
    LDA #$03                            ; output either damage or "Missed!"
    LDX #<(btl_unfmtcbtbox_buffer + $30);  to the #3 combat box
    LDY #>(btl_unfmtcbtbox_buffer + $30)
    JSR DrawCombatBox_L
    INC btl_combatboxcount              ; and inc combatbox count
    
    LDA battle_critsconnected
    BEQ :+                              ; if any criticals connected...
      LDA #BTLMSG_CRITICALHIT           ; print the Critical Hit!! combat message box
      JSR DrawBattleMessageCombatBox
      JSR RespondDelay
      
  : LDA #MATHBUF_DEFENDERHP
    LDX #MATHBUF_DEFENDERHP
    LDY #MATHBUF_TOTALDAMAGE
    JSR MathBuf_Sub16           ; defender_hp -= totaldamage
    LDY btl_defender_hp
    LDX btl_defender_hp+1
    JSR ZeroXYIfNegative
    STY btl_defender_hp
    STX btl_defender_hp+1
    
    LDA #MATHBUF_DEFENDERHP
    JSR MathBuf_NZ
    BNE DoPhysicalAttack_Exit           ; done if HP is > 0
    
    LDA #AIL_DEAD                       ; otherwise (HP == 0)
    STA btl_defender_ailments           ; add the 'Dead' ailment
    LDA $6887
    BEQ :+                              ; if this is a player
      LDA #BTLMSG_SLAIN                 ;  print "Slain" battle message
      BNE @DeathMsg
  : LDA #BTLMSG_TERMINATED              ; otherwise, print the "Terminated" battle message
  @DeathMsg:
    JSR DrawBattleMessageCombatBox
  ; JMP DoPhysicalAttack_Exit    <- flow into

DoPhysicalAttack_Exit:
    JMP RespondDelay_ClearCombatBoxes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  PrintPlayerAilmentMessageFromAttack  [$A988 :: 0x32998]
;;
;;    Prints the battle message(s) associated with the given ailments (in A)
;;
;;  What's weird is that this only applies to ailments applied to player characters,
;;  and only those caused by physical attacks.  It's such a specific use case -- you'd think
;;  this would be more generalized...
;;
;;    Note this double-returns if the ailment being added is 'death' in order to prevent a damage
;;  amount from displaying.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrintPlayerAilmentMessageFromAttack:
    ASL A
    BCC :+                      ; bit 7 set = confusion
      PHA
      LDA #BTLMSG_CONFUSED
      JSR DrawBtlMsg_ClearIt
      PLA
  : ASL A                       ; bit 6 set = mute
    BCC :+
      PHA
      LDA #BTLMSG_SILENCED
      JSR DrawBtlMsg_ClearIt
      PLA
  : ASL A                       ; bit 5 set = sleep (removes action)
    BCC :+
      PHA
      LDA #BTLMSG_ASLEEP
      JSR DrawBtlMsg_ClearIt
      JSR @RemoveDefenderAction
      PLA
  : ASL A                       ; bit 4 set = stun (removes action)
    BCC :+
      PHA
      LDA #BTLMSG_PARALYZED_A
      JSR DrawBtlMsg_ClearIt
      JSR @RemoveDefenderAction
      PLA
  : ASL A                       ; bit 3 set = dark
    BCC :+
      PHA
      LDA #BTLMSG_DARKNESS
      JSR DrawBtlMsg_ClearIt
      PLA
  : ASL A                       ; bit 2 set = poison
    BCC :+
      PHA
      LDA #BTLMSG_POISONED
      JSR DrawBtlMsg_ClearIt
      PLA
  : ASL A                       ; bit 1 set = stone (removes action)
    BCC :+
      PHA
      LDA #BTLMSG_STOPPED
      JSR DrawBtlMsg_ClearIt
      JSR @RemoveDefenderAction
      PLA
  : ASL A                       ; bit 1 set = dead (removes action)
    BCC :+
      LDA #$00
      STA btl_defender_hp
      STA btl_defender_hp+1
      LDA #BTLMSG_SLAIN
      JSR DrawBtlMsg_ClearIt
      JSR @RemoveDefenderAction
      PLA                       ; Drop the return address.  Do not return to DoPhysicalAttack, but instead return 
      PLA                       ;  to whoever called it.  This will prevent attack damage from being drawn on
                                ;  screen (which would be pointless because the player just died)
      
      LDA btl_defender_ailments         ; apply dead ailment
      ORA btl_attacker_attackailment
      STA btl_defender_ailments
      JMP DoPhysicalAttack_Exit ; Then jump to erase all combat boxes
      
  : RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  RemoveDefenderAction  [$A9F9 :: 0x32A09]
;;
;;    Erases the chosen action that a player is to perform this round, and has them do
;;  nothing instead.  This is called when a player gets stunned/slept by an enemy.
;;
;;    Note that this is only called from PrintPlayerAilmentMessageFromAttack.
;;  and therefore applies only to defending players, and not to defending enemies.
;;  Again.. you'd think this code would have been used elsewhere...
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@RemoveDefenderAction:
    LDA battle_defender_index   ; defender 
    AND #$03
    ASL A
    ASL A
    TAY                         ; *4 as index
    LDA #$00
    STA btl_charcmdbuf, Y       ; set command in buffer to 0 (do nothing)
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DrawBtlMsg_ClearCombatBoxes  [$AA07 :: 0x32A17]
;;
;;  A = Battle Message ID to draw
;;
;;    Draws the battle message combat box, waits (respond rate), then clears ALL combat boxes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawBtlMsg_ClearCombatBoxes:
    JSR DrawBattleMessageCombatBox
    JMP RespondDelay_ClearCombatBoxes
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DrawBtlMsg_ClearIt  [$AA0D :: 0x32A1D]
;;
;;  A = Battle Message ID to draw
;;
;;    Draws the battle message combat box, waits (respond rate), then clears the battle message.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawBtlMsg_ClearIt:
    JSR DrawBattleMessageCombatBox  ; Draw the box
    JSR RespondDelay                ; Wait
    DEC btl_combatboxcount          ; Then dec the combat box count to erase the battle message
    LDA #$01
    JMP UndrawNBattleBlocks_L       ; and undraw the battle message

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  YXDivideA  [$AA1B :: 0x32A2B]
;;
;;  A = YX / A
;;  X = YX % A
;;
;;  divHi has the high byte of the division result (but it's never used)
;;
;;    As with 'DoDivision', you'd think this routine would be used more, but it's only used
;;  for formatting HP for printing.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

YXDivideA:
    STA btltmp_divV
    STX btltmp_divLo
    STY btltmp_divHi
    JSR DoDivision
    LDA btltmp_divLo
    LDX btltmp_divV
    RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  FormatHPForText  [$AA2E :: 0x32A3E]
;;
;;  input:     YX = HP value to format
;;  output: $6856 = Buffer to receive formatted text (4 digits, every other byte)
;;
;;    This is a weird one-off routine to do text formatting to print the player HP.
;;  For whatever reason this routine exists just to print HP for the battle screen,
;;  even though there are more generic (and versatile) formatting routines that the
;;  game could have used.  Oh well.
;;
;;    Also strangely, this will keep the leading 0s.  That is, it will print
;;  '008' instead of '8'.  This is corrected later.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FormatHPForText:
    LDA #$FF
    STA $6856           ; start with a space
    
    LDA #100
    JSR YXDivideA       ; get the 100s digit
    ORA #$80            ; convert to tile
    STA $6858           ; print 100s digit
    
    LDY #$00
    LDA #10
    JSR YXDivideA       ; divide to get 10s and 1s
    
    ORA #$80
    STA $685A           ; print 10s
    TXA
    ORA #$80
    STA $685C           ; 1s
    
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DrawCharacterStatus  [$AA50 :: 0x32A60]
;;
;;     Draws the contents of each character's status box (the 4 small boxes on the
;;  right side of the battle screen).  This includes name, "HP" or ailment text,
;;  and their current HP.
;;
;;     Drawing is spread over 5 frames, one frame for each character's text, and one frame
;;  to update the sprites.
;;
;;     Note that strangely, this routine prints the OB stats instead of the IB stats.
;;  I would expect in-battle code to print in-battle stats.  Is IB HP even used?
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawCharacterStatus:
    JSR SetAllNaturalPose
    JSR UpdateSprites_BattleFrame
    
    LDA #$00
    STA $685E                           ; temp var for character index/loop counter
    
@CharacterLoop:
    LDA $685E
    JSR PrepCharStatPointers
    LDY #ch_ailments - ch_stats
    LDA (btl_ob_charstat_ptr), Y        ; get OB ailments (why not IB?)
    DEY                                 ; Convert the ailment byte to a status string by finding first set bit
    : LSR A                         ; find first set bit
      BCS :+                        ; once a set bit is found, exit this loop and use the bit number (in Y)
      INY                           ;   as the status string index
      CPY #$07
      BNE :-                        ; stop at Y=7, which will draw "HP" instead of ailment text
      
  : TYA                 ; code reaches here with Y=bitnumber of first ailment (0=death, 1=stone, 2=poison, etc)
    CLC                 ; Add $41 to that ailment to convert it to a "battle message" text index (see FormatBattleString)
    ADC #$41            
    STA $6B60           ; ailment index @ 6B60
    
    LDA $685E           ; get 4+character index, which is the FormatBattleString character code for
    CLC                 ; printing the character's name
    ADC #$04
    STA $6B5E           ; name code @ $6B5E
    
    LDA #$0F            ; $0F is the "print battle message" code
    STA $6B5F           ; @ $6B5F, with the actual battle message index (our ailment index) @ 6B60
    
    LDA #$00            ; null terminate
    STA $6B61           ; At this point, $6B5E contains a string that will be formatted to "<name><ailment>" -- or "<name>HP" if
                        ;   the character has no ailments.
    
    LDY #ch_curhp - ch_stats        ; Get OB HP stats (why OB?  Why not IB?  wtf?)
    LDA (btl_ob_charstat_ptr), Y
    TAX
    INY
    LDA (btl_ob_charstat_ptr), Y
    TAY                             ; YX = current HP
    JSR FormatHPForText             ; prints HP string to $6856 (interleaved).  This is done here instead of
                                    ;   below by the actual HP printing so as not to burn valuable VBlank time.
    
    LDX #<$6B5E                 ; YX = pointer to our "<name><ailment>" string to be formatted
    LDY #>$6B5E
    JSR FormatBattleString_L    ; format it (printed to btl_stringoutputbuf).  Note again that
                                ;  FormatBattleString creates an interleaved string.
    
    LDA $685E                   ; use a LUT to get the target PPU address at which to draw this
    ASL A                       ;   character's status
    TAY
    LDA lut_CharStatusPPUAddr, Y
    STA $88
    LDA lut_CharStatusPPUAddr+1, Y
    STA $89                     ; put the target PPU address at $88,89
    
    JSR WaitForVBlank_L         ; since we're about to do some drawing, wait for VBlank
    LDA $2002                   ; clear toggle
    
            ; remember at this point, btl_stringoutputbuf contains our "<name><ailment>" string
    LDY #$00                    ; draw top row of char's name (all blank spaces)
    JSR DrawStatusRow           ;   see DrawStatusRow for more details
    LDY #$01
    JSR DrawStatusRow           ; draw bottom row of char's name (their actual name)
    LDY #$08
    JSR DrawStatusRow           ; draw top row of ailment/status (all blank spaces)
    LDY #$09
    JSR DrawStatusRow           ; draw bottom row of ailment/status
    
    ; Interleaved HP string is stored at $6856
    LDY #$08
    : LDA $6856-1, Y                ; copy 8 bytes of that HP string to
      STA btl_stringoutputbuf-1, Y  ; the string output buffer
      DEY                           ;  (-1 because Y is 1-based)
      BNE :-
    
    ; Before we can draw the character's HP, FormatHPForText left leading 0s in the string.  We
    ;   have to clear those out here.
    LDY #$00
  @ClearZerosLoop:
      LDA btl_stringoutputbuf+2, Y      ; +2 to start at 100's digit, since there is no 1000s digit
      CMP #$80
      BNE :+                            ; if not the '0' tile, exit
      LDA #$FF
      STA btl_stringoutputbuf+2, Y      ; otherwise, replace it with a space and continue on
      INY
      INY
      CPY #$04                          ; only check Y=0 (hundreds) and Y=2 (tens)
      BNE @ClearZerosLoop               ;   once Y=4, we exit
      
:   LDY #$00
    JSR DrawStatusRow                   ; draw the final status row (player's current HP)
    
    JSR BattleUpdatePPU                 ; reset scroll
    JSR BattleUpdateAudio               ; update audio for the frame we just waited for
    
    INC $685E                   ; inc char index/loop counter
    LDA $685E
    CMP #$04
    BEQ :+
      JMP @CharacterLoop        ; loop for all 4 characters
:   RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DrawStatusRow  [$AAFC :: 0x32B0C]
;;
;;    Draws a single row of in-battle status text.  This text is stored
;;  interleaved presumably because of the high-low portions of hiragana being
;;  stored together in the buffer (which isn't applicable to the US version).
;;
;;    As a result, to draw a single-row of text, only *every OTHER* tile from
;;  the source buffer is drawn.  Therefore, a routine would call this routine with
;;  Y=even to draw the top row, then Y=odd to draw the bottom row.
;;
;;    Each row consists of exactly 4 tiles to be drawn.
;;
;;  input:
;;      $88,89 = pointer to target PPU address  (this pointer is modified to point to the next row)
;;      Y      = source offset of the btl_stringoutputbuf buffer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawStatusRow:
    LDA $89         ; set PPU addr
    STA $2006
    LDA $88
    STA $2006
    
    LDX #$04        ; draw 4 tiles from the btl_stringoutputbuf
  @Loop:
      LDA btl_stringoutputbuf, Y    ; (using Y to index)
      STA $2007
      INY                           ; draw every other source character (rows are interleaved)
      INY
      DEX
      BNE @Loop
      
    LDA $88                         ; then add $20 to dest pointer to move to next row
    CLC
    ADC #$20
    STA $88
    LDA $89
    ADC #$00
    STA $89
    
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  lut_CharStatusPPUAddr  [$AB21 :: 0x32B31]
;;
;;  Each entry is the PPU address at which to draw the players' name/status/HP on the battle screen
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

lut_CharStatusPPUAddr:
  .WORD $209A
  .WORD $215A
  .WORD $221A
  .WORD $22DA
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DoMiniFrame  [$AB29 :: 0x32B39]
;;
;;    This is a strange little routine that does some typical frame work.  I'm sort of at a loss
;;  to even describe it in a meaningful way.
;;
;;    What's especially strange is that it DOES use the Battle audio routine, but NOT the BattleVBlank routine
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DoMiniFrame:
    JSR WaitForVBlank_L
    JMP BattleUpdateAudio
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  UpdateVariablePalette  [$AB2F :: 0x32B3F]
;;
;;    Updates the variable palette (used by weapon/magic graphics).
;;
;;  A = palette color (assumed to be $2x)
;;  X = 0 for weapon, nonzero for magic
;;
;;    Palette written is:
;;  weapon:  FF 2x 1x 0x
;;  magic:   FF 2x 1x 30   (last color is white for magic)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

UpdateVariablePalette:
    PHA
    JSR ResetUsePalette
    PLA
    STA btl_usepalette + $19        ; set first shade
    SEC
    SBC #$10
    STA btl_usepalette + $1A        ; second shade
    SEC
    SBC #$10
    STA btl_usepalette + $1B        ; third shade
    LDA #$FF
    STA btl_usepalette + $18        ; this is pointless, as this color is never visible
    TXA
    BEQ DoFrame_UpdatePalette       ; if weapon, jump ahead to update the PPU
      LDA #$30                      ; if magic, replace third shade with white
      STA btl_usepalette + $1B
    ; JMP DoFrame_UpdatePalette     ; then update PPU  (flow into)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DoFrame_UpdatePalette  [$AB50 :: 0x32B60]
;;
;;  Actually draws the usepalette, and does some typical frame work:
;;    - Wait for VBlank
;;    - draw usepalette
;;    - reset scroll
;;    - apply btl_soft2001
;;    - update audio
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DoFrame_UpdatePalette:
    JSR BattleWaitForVBlank_L       ; wait for VBlank
    
    LDA #$3F            ; set PPU addr to point to palettes
    STA $2006
    LDA #$00
    STA $2006
    
    LDY #$00                ; draw the usepalette to the PPU
  @Loop:
      LDA btl_usepalette, Y
      STA $2007
      INY
      CPY #$20
      BNE @Loop
      
    LDA #$3F                ; reset PPU address
    STA $2006
    LDA #$00
    STA $2006
    STA $2006
    STA $2006
    
    JSR BattleUpdatePPU     ; reset scroll & apply soft2001
    JMP BattleUpdateAudio   ; update audio and exit
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ResetUsePalette  [$AB80 :: 0x32B90]
;;
;;  Resets btl_usepalettes by copying btl_palettes on top of it
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ResetUsePalette:
    LDY #$20                    ; copy $20 bytes over
  @Loop:
      LDA btl_palettes-1, Y     ; -1 because Y is 1-based and stops at 0
      STA btl_usepalette-1, Y
      DEY
      BNE @Loop
    RTS
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BattleFadeIn  [$AB8C :: 0x32B9C]
;;
;;  Self explanitory
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BattleFadeIn:
    JSR BattleClearOAM          ; Clear OAM
    LDA #$00
    STA a:soft2000              ; clear other PPU settings
    STA $2001                   ; including disabling rendering, though since btl_soft2001 has
                                ;   rendering enabled, rendering will be re-enabled when palettes are updated
    
    JSR WaitForVBlank_L         ; wait for VBlank
    LDA $2002
    LDA #>oam
    STA $4014                   ; Do DMA (clearing actual PPU OAM)
    LDA #$04
    STA $68B4                   ; loop downcounter.  Looping 4 times -- once for each shade
    JSR BattleUpdateAudio       ; since we just did a frame, keep audio updated
    
    ; This fades in by using the fadeout routine.  Basically it will reset the palette each iteration
    ;   fading it out less and less each time, giving the appearance that it is fading it.
    ; So first frame it will fade out 4 shades, then draw
    ; next frame will reset, then fade out 3 shades, then draw
    ; next will reset, fade 2 shades, etc
  @Loop:
      JSR ResetUsePalette       ; reset palette
      
      LDX $68B4
      : JSR FadeOutOneShade     ; fade out X shades, where X is our loop down-counter
        DEX
        BNE :-
        
      JSR Do3Frames_UpdatePalette   ; draw palette (and turn on PPU)
      DEC $68B4
      BNE @Loop                 ; loop until down-counter exhausted
      
    JSR ResetUsePalette         ; Reset to fully-faded in palette
    JMP DoFrame_UpdatePalette   ; Then draw it and exit
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BattleFadeOut  [$ABC4 :: 0x32BD4]
;;
;;  Self explanitory
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
BattleFadeOut:
    JSR ResetUsePalette ; reset usepalette
    LDA #$04
    STA $68B4           ; Loop down counter (loop 4 shades, enough to make solid black)
  @Loop:
      JSR FadeOutOneShade           ; fade out one shade
      JSR Do3Frames_UpdatePalette   ; draw it
      DEC $68B4
      BNE @Loop                     ; repeat 4 times
    RTS

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  FadeOutOneShade  [$ABD8 :: 0x32BE8]
;;
;;  Iterates btl_usepalette and fades every color out by 1 shade
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FadeOutOneShade:
    LDY #$00                ; index and loop up-counter
  @Loop:
      LDA btl_usepalette, Y ; get the color
      AND #$30              ; mask out brighness bits
      BNE :+                ; if brightness is 0
        LDA #$0F            ;  use 0F black
        BNE @SetColor
    : SEC                   ; otherwise, subtract $10 to take it down a shade
      SBC #$10
      STA $68B3             ; save new brightness to temp ram
      LDA btl_palettes, Y   ; get the original color
      AND #$CF              ; remove brightness
      ORA $68B3             ; apply new brightness
      
    @SetColor:
      STA btl_usepalette, Y ; write new color to usepalette
      INY
      CPY #$20              ; loop until all $20 colors faded
      BNE @Loop
      
    RTS
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Do3Frames_UpdatePalette  [$ABFC :: 0x32C0C]
;;
;;    Same as DoFrame_UpdatePalette, only this waits a few additional
;;  extra frames to slow down the fade effect
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Do3Frames_UpdatePalette:
    JSR DoMiniFrame
    JSR DoFrame_UpdatePalette
    JMP DoMiniFrame
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  GetPointerToWeaponData  [$AC05 :: 0x32C15]
;;
;;  in:    A = weapon index
;;  out:  XA = pointer to that weapon's data
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GetPointerToWeaponData:
    LDX #$08
    JSR MultiplyXA          ; multiply by 8 (8 bytes per weapon)
    
    CLC
    ADC #<lut_WeaponData    ; add to low byte of data
    PHA                     ; backup low byte
    TXA
    ADC #>lut_WeaponData
    TAX                     ; high byte in X
    PLA                     ; low byte in A
    
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  GetPointerToArmorData  [$AC14 :: 0x32C24]
;;
;;  in:    A = armor index
;;  out:  XA = pointer to that armor's data
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GetPointerToArmorData:
    LDX #$04
    JSR MultiplyXA      ; 4 bytes per armor, so mulitply index by 4
    CLC
    ADC #<lut_ArmorData
    PHA
    TXA
    ADC #>lut_ArmorData
    TAX
    PLA
    
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  GetCharOBPointers  [$AC23 :: 0x32C33]
;;
;;    Gets OB stat pointers for 2 characters.
;;
;;  input:  A, X = IDs of characters whose stat pointers to get
;;
;;  output:  btl_ib_charstat_ptr = pointer to A's OB ch_stats
;;           btl_ob_charstat_ptr = pointer to X's OB ch_stats
;;
;;    Yeah, I know the use of 'ib charstat ptr' is a little misleading since
;;  it's not IB stats... but whatever.  This is only used by one part of the game.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GetCharOBPointers:
    ASL A                           ; ID * 2 to use as index for lut
    
    TAY
    LDA lut_CharStatsPtrTable, Y    ; copy pointer from lut to output
    STA btl_ib_charstat_ptr
    LDA lut_CharStatsPtrTable+1, Y
    STA btl_ib_charstat_ptr+1
    
    TXA
    ASL A                           ; X ID * 2
    
    TAY
    LDA lut_CharStatsPtrTable, Y    ; copy pointer from lut again
    STA btl_ob_charstat_ptr
    LDA lut_CharStatsPtrTable+1, Y
    STA btl_ob_charstat_ptr+1
    
    RTS
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  GetCharOBMagDataPointers  [$AC3D :: 0x32C4D]
;;
;;    Identical to GetCharOBPointers, but gets pointers to ch_magicdata rather
;;  than ch_stats.
;;
;;    Sparse comments.  See GetCharOBPointers for details
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GetCharOBMagDataPointers:
    ASL A
    
    TAY
    LDA lut_CharMagicData, Y
    STA btl_ib_charstat_ptr
    LDA lut_CharMagicData+1, Y
    STA btl_ib_charstat_ptr+1
    
    TXA
    ASL A
    
    TAY
    LDA lut_CharMagicData, Y
    STA btl_ob_charstat_ptr
    LDA lut_CharMagicData+1, Y
    STA btl_ob_charstat_ptr+1
    
    RTS
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  SwapCharsForSorting  [$AC57 :: 0x32C67]
;;
;;  input:  $88 and $89 = char IDs (0-3) to swap
;;
;;    This routine swaps not only character stats,
;;  but also the character entries in char_order_buf
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SwapCharsForSorting:
    LDA $88                 ; Swap ch_stats
    LDX $89
    JSR GetCharOBPointers
    JSR SwapCharOBStats
    
    LDA $88                 ; Swap ch_magic
    LDX $89
    JSR GetCharOBMagDataPointers
    JSR SwapCharOBStats
    
    LDA $88                 ; put char IDs in X,Y
    TAY
    LDA $89
    TAX
    
    LDA char_order_buf, Y   ; so that we can swap their entries in char_order_buf
    PHA
    LDA char_order_buf, X
    STA char_order_buf, Y
    PLA
    STA char_order_buf, X
    
    RTS                     ; done!
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  SwapCharOBStats  [$AC80 :: 0x32C90]
;;
;;  input:
;;    btl_ib_charstat_ptr
;;    btl_ob_charstat_ptr
;;
;;    Both pointers point to *OB* stats for characters.  This routine
;;  will do a buffer swap.
;;
;;    It'll swap $40 bytes total.  This is the size of ch_stats, and the
;;  size of ch_magicdata.  To fully have characters swap places, both of
;;  those buffers will have to be swapped, so this needs to be called twice.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SwapCharOBStats:
    LDY #$00                ; index and loop up counter
  @Loop:
      LDA (btl_ob_charstat_ptr), Y  ; Swap a byte
      PHA
      LDA (btl_ib_charstat_ptr), Y
      STA (btl_ob_charstat_ptr), Y
      PLA
      STA (btl_ib_charstat_ptr), Y
      
      INY                           ; loop $40 times
      CPY #$40
      BNE @Loop
      
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  TransferByte  [$ACE1 :: 0x32CF1]
;;
;;  Transfer a byte between two buffers (specifically for transferring OB char stats to IB stats)
;;
;;  input:
;;    btl_ib_charstat_ptr = pointer to dest buffer
;;    btl_ob_charstat_ptr = pointer to source buffer
;;                      A = dest index
;;                      Y = source index
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TransferByte:
    PHA                             ; backup A
    LDA (btl_ob_charstat_ptr), Y    ; Load the source stat
    TAX                             ; stick it in X
    PLA                             ; restore A, put in Y
    TAY
    TXA                             ; get the source stat
    STA (btl_ib_charstat_ptr), Y    ; write it to the dest
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  LoadAllCharacterIBStats  [$ACEB :: 0x32CFB]
;;
;;    Loads the in-battle stats for all charaters
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LoadAllCharacterIBStats:
    LDA #$00                    ; just load each of them one at a time
    JSR LoadOneCharacterIBStats
    LDA #$01
    JSR LoadOneCharacterIBStats
    LDA #$02
    JSR LoadOneCharacterIBStats
    LDA #$03
    JMP LoadOneCharacterIBStats

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  LoadOneCharacterIBStats  [$ACFF :: 0x32D0F]
;;
;;    Loads the in-battle stats for one character (in A)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LoadOneCharacterIBStats:
    STA btl_tmpindex                ; store char index (temporarily)
    JSR PrepCharStatPointers        ; prep stat pointers
    
    LDY #ch_class - ch_stats
    LDA (btl_ob_charstat_ptr), Y    ; get char class
    TAY                             ;  use as index to get assigned palette
    LDA lut_InBattleCharPaletteAssign, Y
    LDY btl_tmpindex
    STA btl_charattrib, Y
    
    LDA btl_tmpindex                ; store the character slot index
    LDY #btlch_slotindex
    STA (btl_ib_charstat_ptr), Y
    
    ; Copy a bunch of stats verbatim from the OB stats to IB stats
    ;   Y = src index
    ;   A = dst index
    LDY #ch_class      - ch_stats
    LDA #btlch_class
    JSR TransferByte
    
    LDY #ch_ailments   - ch_stats
    LDA #btlch_ailments
    JSR TransferByte
    
    LDY #ch_curhp      - ch_stats
    LDA #btlch_hp
    JSR TransferByte
    
    LDY #ch_curhp + 1  - ch_stats
    LDA #btlch_hp + 1
    JSR TransferByte
    
    LDY #ch_hitrate    - ch_stats
    LDA #btlch_hitrate
    JSR TransferByte
    
    LDY #ch_magdef     - ch_stats
    LDA #btlch_magdef
    JSR TransferByte
    
    LDY #ch_evade      - ch_stats
    LDA #btlch_evade
    JSR TransferByte
    
    LDY #ch_absorb     - ch_stats
    LDA #btlch_absorb
    JSR TransferByte
    
    LDY #ch_dmg        - ch_stats
    LDA #btlch_dmg
    JSR TransferByte
    
    LDY #ch_resist     - ch_stats
    LDA #btlch_elemresist
    JSR TransferByte
    
    ; Set initial hit multiplier to 1
    LDA #$01
    LDY #btlch_numhitsmult
    STA (btl_ib_charstat_ptr), Y
    
    ; Number of Hits    
    LDY #ch_hitrate    - ch_stats
    LDA (btl_ob_charstat_ptr), Y        ; num hits = hitrate / 32 + 1
    LSR A
    LSR A
    LSR A
    LSR A
    LSR A
    CLC
    ADC #$01
    LDY #btlch_numhits
    STA (btl_ib_charstat_ptr), Y
    
    ; Players start with no elemental weakness or assigned category
    LDA #$00
    LDY #btlch_category
    STA (btl_ib_charstat_ptr), Y
    LDY #btlch_elemweak
    STA (btl_ib_charstat_ptr), Y
    
    ; Check the player's currently equipped weapon.
    ;  Check all 4 weapon slots and find which is equipped (high bit set=equipped)
    LDY #ch_weapons - ch_stats
    LDA (btl_ob_charstat_ptr), Y
    BMI @ApplyWeapon
    INY
    LDA (btl_ob_charstat_ptr), Y
    BMI @ApplyWeapon
    INY
    LDA (btl_ob_charstat_ptr), Y
    BMI @ApplyWeapon
    INY
    LDA (btl_ob_charstat_ptr), Y
    BMI @ApplyWeapon
    
    LDA #$00            ; if there's no weapon, use $00 as weapon ID

  @ApplyWeapon:
    LDY #btlch_critrate
    AND #$7F
    STA (btl_ib_charstat_ptr), Y        ; BUGGED - this sets the critical rate to the weapon index,
                                        ;  rather than actually fetching the critical rate from the weapon
                                        ;  stats.
    
    AND #$FF                        ; update Z flag
    BEQ :+                          ; only do this next block if a weapon is actually equipped
    
        SEC                         ; weapon index is 1-based (convert to 0 based)
        SBC #$01
        JSR GetPointerToWeaponData  ; get a pointer to the weapon data
        STA $88                     ; put the pointer at $88
        STX $89
      
        LDY #$07
        LDA ($88), Y
        LDY #btlch_wepplt           ; get palette
        STA (btl_ib_charstat_ptr), Y
        LDY #$06
        LDA ($88), Y                ; get the weapon graphic

  :
    LDY #btlch_wepgfx
    STA (btl_ib_charstat_ptr), Y    ; record weapon graphic (or 00 if no weapon equipped)
    
    LDY #ch_class - ch_stats        ; BB/Master have special stats if they're unarmed
    LDA (btl_ob_charstat_ptr), Y    ; Check the class
    CMP #$02
    BEQ @BB_Master                  ; is it a black belt?
    CMP #$08
    BNE @Done                       ; or master?
  
  @BB_Master:                       ; if yes...
    LDY #btlch_wepgfx
    LDA (btl_ib_charstat_ptr), Y    ; see if they have no weapon equipped
    BNE @Done

    ; Unarmed BB/Master:
    LDA #$AC
    LDY #btlch_wepgfx               ; set weapon/plt to black belt fist
    STA (btl_ib_charstat_ptr), Y
    LDA #$28
    LDY #btlch_wepplt
    STA (btl_ib_charstat_ptr), Y
    
    LDY #ch_level - ch_stats        ; Unarmed BB's crit rate is 2x his level
    LDA (btl_ob_charstat_ptr), Y
    ASL A
    LDY #btlch_critrate
    STA (btl_ib_charstat_ptr), Y
    
    LDY #btlch_numhits              ; unarmed BB/master gets 2x as many hits
    LDA (btl_ib_charstat_ptr), Y
    ASL A
    STA (btl_ib_charstat_ptr), Y
    
  @Done:
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  MathBuf_Compare [$ADEA :: 0x32DFA]
;;
;;  Compares two entries in the math buffer.
;;
;;  input:  X,Y = indexes of math buffer to compare
;;
;;  output:   C = set if Y >= X, clear if Y < X
;;            N = set if high byte of Y < high byte of X
;;
;;            All other flags are cleared.... **INCLUDING THE I FLAG**.  This is very strange.
;;         Since IRQs are not used by the game, this doesn't matter.  But still, very strange.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MathBuf_Compare:
    JSR DoubleXAndY         ; Double X,Y so we can use them as proper indexes
    LDA btl_mathbuf+1, Y    ; Get high byte of Y entry
    CMP btl_mathbuf+1, X    ; Compare to high byte of X entry
    BEQ :+                  ;  if not equal...
      PHP
      PLA
      AND #$81              ; preserve NC flags, but clear all other flags
      PHA
      PLP
      RTS                   ; and exit
    
  : LDA btl_mathbuf, Y      ; if high bytes were equal, compare low bytes
    CMP btl_mathbuf, X
    PHP
    PLA
    AND #$01                ; this time, only preserve the C flag
    PHA
    PLP
    RTS
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Multiply X,A [$AE09 :: 0x32E19]
;;
;;    Does unsigned multiplication:  X*A
;;  High 8 bits of result stored in X
;;  Low  8 bits of result stored in A
;;
;;  There is a 100% identical routine in bank B at $9EFB.  It's duplicated here so its accessible in
;; this bank as well.  Probably should have been put in the fixed bank.
;;
;;  And I mean 100% identical.  I actually copy/pasted that code here.  There is literally no difference
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MultiplyXA:
    STA btltmp_multA    ; store the values we'll be multiplying
    STX btltmp_multB
    LDX #$08            ; Use x as a loop counter.  X=8 for 8 bits
    
    LDA #$00            ; A will be the high byte of the product
    STA btltmp_multC    ; multC will be the low byte
    
    ; For each bit in multA
  @Loop:
      LSR btltmp_multA      ; shift out the low bit
      BCC :+
        CLC                 ; if it was set, add multB to our product
        ADC btltmp_multB
    : ROR A                 ; then rotate down our product
      ROR btltmp_multC
      DEX
      BNE @Loop
    
    TAX                     ; put high bits of product in X
    LDA btltmp_multC        ; put low bits in A
    RTS   
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DoDivision  [$AE2B :: 0x32E3B]
;;
;;  vars:
;;    btltmp_divLo
;;    btltmp_divHi
;;    btltmp_divV
;;
;;  result:
;;    HiLo = HiLo / V
;;       V = HiLo % V
;;
;;  You'd think this routine would be used more than it is....
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DoDivision:
    TXA                     ; backup X
    PHA
    LDA #$00                ; clear temp ram (to hold remainder)
    STA $68B3
    
    LDX #16                 ; loop 16 times (each bit in HiLo)
    ROL btltmp_divLo
    ROL btltmp_divHi        ; left shift 1 bit out
  @Loop:
      ROL $68B3             ; roll the bit into remainder
      LDA $68B3
      CMP btltmp_divV       ; see if it's >= divisor
      BCC :+
        SBC btltmp_divV     ; if yes, subtract divisor
        STA $68B3
    : ROL btltmp_divLo      ; if subtracted, roll 1 into low bit, otherwise, roll 0
      ROL btltmp_divHi      ;   this ultimately will perform the division
      DEX
      BNE @Loop             ; loop for all 16 bits
    LDA $68B3
    STA btltmp_divV         ; store remainder
    
    PLA                     ; restory X
    TAX
    
    RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  RandAX [$AE5D :: 0x32E6D]
;;
;;  Generates a random number between [A,X] inclusive.
;;  Generated number is stored in A on return
;;
;;  This is quite literally an exact copy (seriously, copy/paste) from a routine
;;  in bank 0B.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

RandAX:
    STA $68AF       ; 68AF is the 'lo' value
    INX
    STX $68B0       ; 68B0 is hi+1.  But this STX is totally unnecessary because this value is never used
    
    TXA
    SEC
    SBC $68AF       ; subtract to get the range.
    STA $68B6       ; 68B6 = range
    
    JSR BattleRNG_L
    LDX $68B6
    JSR MultiplyXA  ; random()*range
    
    TXA             ; drop the low 8 bits, put high 8 bits in A  (effectively:  divide by 256)
    CLC
    ADC $68AF       ; + lo
    
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  MathBuf_Add16 [$AE7B :: 0x32E8B]
;;
;;  Adds two 16-bit value in the math buffer.  Stores result in math buffer.
;;
;;  input:  A = index of math buffer to receive sum
;;        X,Y = indexes of math buffer to add
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MathBuf_Add16:
    PHA                     ; backup target index
    JSR DoubleXAndY         ; double X,Y so they are usable indexes
    
    LDA btl_mathbuf, Y
    CLC
    ADC btl_mathbuf, X
    STA $6BCF               ; add low bytes together
    
    LDA btl_mathbuf+1, Y
    ADC btl_mathbuf+1, X
    STA $6BD0               ; and high bytes
    
    BCC :+                  ; if there was high-byte carry
      LDA #$FF              ; cap at $FFFF
      STA $6BCF
      STA $6BD0
      
  : PLA                     ; restore target index
    ASL A                   ; *2 to use as index
    TAX
    
    LDA $6BCF               ; move sum to target slot in math buffer
    STA btl_mathbuf, X
    LDA $6BD0
    STA btl_mathbuf+1, X
    
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  MathBuf_Sub16 [$AEAC :: 0x32EBC]
;;
;;  Subtracts two 16-bit value in the math buffer.  Stores result in math buffer.
;;
;;  input:  A = index of math buffer to receive result
;;        X,Y = indexes of math buffer to subtract   (A = X-Y)
;;
;;     Code is same as MathBuf_Add16.  See that routine for details, comments here are sparse.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MathBuf_Sub16:
    PHA
    JSR DoubleXAndY
    
    LDA btl_mathbuf, X
    SEC
    SBC btl_mathbuf, Y
    STA $6BCF
    
    LDA btl_mathbuf+1, X
    SBC btl_mathbuf+1, Y
    STA $6BD0
    
    BCS :+
      LDA #$00
      STA $6BCF
      STA $6BD0
      
  : PLA
    ASL A
    TAX
    LDA $6BCF
    STA btl_mathbuf, X
    LDA $6BD0
    STA btl_mathbuf+1, X
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  MathBuf_Add [$AEDD :: 0x32EED]
;;
;;  Adds an 8-bit value to an entry in the math buffer, capping the sum at FFFF
;;
;;  input:  A = index of math buffer to add to
;;          X = value to add
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MathBuf_Add:
    STA $6BCF       ; backup index
    PHA             ; backup A,X,Y
    TXA
    PHA
    TYA
    PHA
    
    LDA $6BCF       ; index value * 2 to use as index
    ASL A
    TAY
    
    TXA             ; value to add from X
    CLC
    ADC btl_mathbuf, Y      ; add it
    STA btl_mathbuf, Y
    LDA #$00
    ADC btl_mathbuf+1, Y
    STA btl_mathbuf+1, Y
    BCC :+                  ; if exceeded FFFF
      LDA #$FF
      STA btl_mathbuf, Y    ; cap at FFFF
      STA btl_mathbuf+1, Y
      
  : PLA                     ; restore backups of all regs
    TAY                     ; before exiting
    PLA
    TAX
    PLA
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  MathBuf_Sub [$AF0A :: 0x32F1A]
;;
;;  Subtracts an 8-bit value from an entry in the math buffer, capping the result at 0000
;;
;;  input:  A = index of math buffer to subtract from
;;          X = value to subtract
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MathBuf_Sub:
    STA $6BCF           ; routine is identical to MathBuf_Add, only it subtracts
    PHA                 ;   instead.  Spare comments.
    TXA
    PHA
    TYA
    PHA
    LDA $6BCF
    ASL A
    TAY
    STX $6BCF
    SEC
    LDA btl_mathbuf, Y
    SBC $6BCF
    STA btl_mathbuf, Y
    LDA btl_mathbuf+1, Y
    SBC #$00
    STA btl_mathbuf+1, Y
    BCS :+
      LDA #$00
      STA btl_mathbuf, Y
      STA btl_mathbuf+1, Y
  : PLA
    TAY
    PLA
    TAX
    PLA
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  MathBuf_NZ [$AF3C :: 0x32F1A]
;;
;;  Sets NZ flags to reflect a 16-bit value in the math buffer
;;
;;  input:  A = index of math buffer to use
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MathBuf_NZ:
    ASL A                   ; double and stick in Y to use as index
    TAY
    LDA btl_mathbuf+1, Y    ; get high byte of value
    BNE @Exit               ; if nonzero, just use it as our NZ settings and exit
    
      LDA $6856, Y          ; if zero, get the low byte, to use its Z flag setting
      PHP
      PLA                   ; move status flags to A
      AND #$7F              ; clear the N flag (value is not negative if high byte was zero)
      PHA
      PLP                   ; move back to status flags
  @Exit:
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  MathBuf_CopyXToY [$AF4D :: 0x32F5D]
;;
;;  X = source index
;;  Y = dest index
;;
;;  Copies the source entry in the math buf to the dest entry
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MathBuf_CopyXToY:
    JSR DoubleXAndY         ; double X,Y so they are usable indexes
    LDA btl_mathbuf, X      ; copy low byte
    STA btl_mathbuf, Y
    LDA btl_mathbuf+1, X    ; high byte
    STA btl_mathbuf+1, Y
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DoubleXAndY [$AF5D :: 0x32F6D]
;;
;;  X *= 2
;;  Y *= 2
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DoubleXAndY:
    TXA     ; Double X!
    ASL A
    TAX
    
    TYA     ; Double Y!
    ASL A
    TAY
    
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  LoadEnemyStats  [$AF64 :: 0x32F74]
;;
;;    Load the stats for all enemies.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LoadEnemyStats:
    LDA btl_enemycount      ; useless, as this is immediately discarded
    LDA #$09
    STA $6BD1               ; loop down-counter
    LDA #$00
    STA $6BD2               ; loop up-counter / enemy index
    
  @EnemyLoop:
    LDA $6BD2               ; Put a pointer to the current enemy's stat RAM
    JSR GetEnemyStatPtr     ;    in btltmp+A
    STA btltmp+$A
    STX btltmp+$B
    
    LDX $6BD2               ; Check to see if this enemy even exists
    JSR DoesEnemyXExist
    BNE :+
      JMP @NextEnemy        ; if it doesn't, skip ahead...
      
  : LDX #$14                ; multiply current enemy ID by #$14  ($14 bytes of data per enemy)
    JSR MultiplyXA          ;   add the result to data_EnemyStats to generate a pointer to the enemy
    CLC                     ;   data in ROM.
    ADC #<data_EnemyStats   ; Store that pointer in btltmp+$C,D
    STA btltmp+$C
    LDY #$00
    STA (btltmp+$A), Y      ; AND store it in the first 2 bytes of the enemy's stats in RAM
    TXA
    ADC #>data_EnemyStats
    STA btltmp+$D
    LDY #$01
    STA (btltmp+$A), Y
    
    ; Copy $C bytes of enemy stat data from ROM to RAM
    LDA #$00
    STA $6C8E           ; 6C8E is used as an index to lut_RomRamMapping, and is also the loop counter
  :   JSR @MoveOneByte
      LDA $6C8E
      CMP #$0C * 2      ; Loop until all C bytes have been copied (*2 because the loop counter increases by 2 for
      BNE :-            ;    each byte)
      
    JMP @Part2  ; Jump ahead over these small subroutines
    
    ; support subroutine
    ; moves 1 byte of enemy stats from ROM to RAM
    ;   btltmp+A has the dest pointer   (RAM ptr)
    ;   btltmp+C has the src pointer    (ROM ptr)
  @MoveOneByte:
    JSR @GetNextLutIndex        ; Get the next index for our LUT in X
    LDA @lut_RomRamMapping, X   ; put Source offset in Y
    TAY
    LDA (btltmp+$C), Y          ; reads source ROM byte
    PHA                         ; push it
    JSR @GetNextLutIndex        
    LDA @lut_RomRamMapping, X   ; Get Dest offset in Y
    TAY
    PLA                         ; restore source byte
    STA (btltmp+$A), Y          ; write it to our dest
    RTS
  
    ; support subroutine
    ; Gets the next index for the LUT in X
  @GetNextLutIndex:
    LDX $6C8E       ; put old counter in X
    INC $6C8E       ; inc counter
    RTS
    
  ; This LUT maps the source ROM offset to a dest RAM offset
  ;  For whatever reason, the order the data appears in ROM does not
  ;  match the order it appears in RAM.
  @lut_RomRamMapping:
    .BYTE ENROMSTAT_HPMAX,      en_hp
    .BYTE ENROMSTAT_HPMAX+1,    en_hp+1
    .BYTE ENROMSTAT_ABSORB,     en_defense
    .BYTE ENROMSTAT_MORALE,     en_morale
    .BYTE ENROMSTAT_EVADE,      en_evade
    .BYTE ENROMSTAT_DAMAGE,     en_strength
    .BYTE ENROMSTAT_AI,         en_ai
    .BYTE ENROMSTAT_EXP,        en_exp
    .BYTE ENROMSTAT_EXP+1,      en_exp+1
    .BYTE ENROMSTAT_GP,         en_gp
    .BYTE ENROMSTAT_GP+1,       en_gp+1
    .BYTE ENROMSTAT_HPMAX,      en_unknown12     ; <- ?  is this BUGGED?  Why is HP duplicated here?  And why just the low byte?
;          ^                       ^
;          |                       |
;          |                      dest RAM offset
;         src ROM offset
  
  @Part2:
    LDY #en_numhitsmult
    LDA #$01
    STA (btltmp+$A), Y      ; default to hit multiplier of 1
    
    ; default a bunch of values to zero:
    INY                     ; <- en_ailments
    LDA #$00
    STA (btltmp+$A), Y
    INY                     ; <- en_aimagpos
    STA (btltmp+$A), Y
    INY                     ; <- en_aiatkpos
    STA (btltmp+$A), Y
    
    
    LDX $6BD2               ; get the enemy ID
    LDA btl_enemyIDs, X
    LDY #en_enemyid
    STA (btltmp+$A), Y      ; put it in slot 'btlen_enemyid'
    
  @NextEnemy:
    INC $6BD2           ; inc up-counter to look at next enemy
    DEC $6BD1           ; dec down-counter
    BEQ :+
      JMP @EnemyLoop    ; loop until all 9 enemies processed
  : JMP BattleUpdatePPU
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ClearAltMessageBuffer  [$B00C :: 0x3301C]
;;
;;     Also clears explode_count and alternate combo box count
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ClearAltMessageBuffer:
    PHA         ; preserves A,X
    TXA
    PHA
    LDX #$04            ; index starts at 4
    LDA #$00
    STA btl_combatboxcount_alt
    : STA btltmp_altmsgbuffer-5, X  ; but -5?  (also clears explode_count)
      INX
      CPX #$20
      BNE :-
    PLA
    TAX
    PLA
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ClearAllCombatBoxes  [$B022 :: 0x33032]
;;
;;   Also clears alt message buffer.
;;  Uses *alternate* combat box counter for whatever reason
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ClearAllCombatBoxes:
    PHA                         ; backup AXY
    TXA
    PHA
    TYA
    PHA
    
    LDA btl_combatboxcount_alt  ; undraw all counted combat boxes
    BEQ :+
    BMI :+
      JSR UndrawNBattleBlocks_L
  : LDA #$00
    STA btl_combatboxcount_alt  ; zero the counter
    
    JSR ClearAltMessageBuffer   ; Clear alt message buffer
    JSR RespondDelay            ; wait a bit
  ; JMP RestoreAXY              ; then restore and exit  (FLOW INTO)
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  RestoreAXY  [$B03C :: 0x3304C]
;;
;;    Restores A,X,Y which were presumed to have been pushed on the stack
;;  and then RTS's.  Called on exit to various routines that will back up these regs
;;  upon entry
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

RestoreAXY:
    PLA
    TAY
    PLA
    TAX
    PLA
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  RespondDelay_UndrawAllBut2Boxes [$B042 :: 0x33052]
;;
;;  Waits for the Respond Rate, then clears all drawn combat boxes except for 2:
;;      the attacker and the spell they're casting
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

RespondDelay_UndrawAllBut2Boxes:
    PHA                             ; backup AXY, whynot
    TXA
    PHA
    TYA
    PHA
    JSR RespondDelay                ; delay!
    
  @Loop:
      LDA btl_combatboxcount_alt
      CMP #$03
      BCC @Done                     ; if there are < 3 blocks, we're done
      LDA #$01
      JSR UndrawNBattleBlocks_L     ; otherwise, undraw one
      DEC btl_combatboxcount_alt    ; and loop (always branches)
      BNE @Loop
      
  @Done:
    JMP RestoreAXY

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DrawCombatBox_Attacker  [$B05E :: 0x3306E]
;;
;;  Draws the combat box containing the attacker's name.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawCombatBox_Attacker:
    PHA             ; backup A,X,Y
    TXA
    PHA
    TYA
    PHA
    
    LDA #$02                    ; $02 is the code for the attacker
    STA btltmp_attackerbuffer   ; put that in buffer to hold attacker name
    LDX #<btltmp_attackerbuffer ; set XY to point to that buffer
    LDY #>btltmp_attackerbuffer
    
    INC btl_combatboxcount_alt  ; count the combat box
    
    LDA #$00                ; combat box 0 = attacker box
    
    JMP DrawCombatBox_RestoreAXY    ; draw the box, then restore and exit
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DrawCombatBox_Attack  [$B074 :: 0x33084]
;;
;;  Draws the combat box containing the attack name, or the item being used.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawCombatBox_Attack:
    PHA                     ; backup A,X,Y
    TXA
    PHA
    TYA
    PHA
    
    ; To draw the attack name, we need to get the Item ID
    ;  How to get the Item ID depends on the source
    
    LDA btlmag_magicsource  ; check the source
    BNE @NotMagic           ; source=0 is magic.  So if zero...
    
    ; source = magic
    LDA btl_attackid        ; see if the attack type is magic or a special attack
    CMP #$42
    BCS :+                  ; if it's magic...
      CLC
      ADC #MG_START         ; add MG_START to the index to convert from a magic index to an item index.
  : JMP @Print   
    
  @NotMagic:
    CMP #$01                ; if source=1, it's a DRINK
    BNE :+                          ; for drinks....
      LDA btl_attackid              ; ... get the item ID for heal/pure potions
      SEC                           ; subtract $40 because drink effects start at $40 -- get 0 for heal, 1 for pure
      SBC #$40 - (item_heal - items); but then add item_heal-items to convert to the item ID for the heal potion/pure potion
      BNE @Print                    ; (always branch)
      
    ; Reach here if the source=item
  : LDA btl_attacker
    AND #$03
    TAX
    LDA btl_charcmditem, X          ; get the item ID from the cmd buffer
    
    
  @Print:
    STA $6D25                       ; write item ID
    LDA #$0E
    STA $6D24                       ; preface it with 0E, the command to draw an attack (item) name
    LDX #<$6D24
    LDY #>$6D24                     ; get pointer to that string in YX
    
    INC btl_combatboxcount_alt      ; inc the combat box counter
    
    LDA #$01                        ; draw the attack name in box 1
    JMP DrawCombatBox_RestoreAXY
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DrawCombatBox_Defender  [$B0B4 :: 0x330C4]
;;
;;  Draws the combat box containing the defender/target's name.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawCombatBox_Defender:
    PHA             ; backup AXY
    TXA
    PHA
    TYA
    PHA
    LDA #$03                            ; 03 = defender format code
    STA btltmp_altmsgbuffer + 9
    LDX #<(btltmp_altmsgbuffer + 9)
    LDY #>(btltmp_altmsgbuffer + 9)
    INC btl_combatboxcount_alt          ; inc combat box counter
    LDA #$02                            ; then print defender combat box
  ; JMP DrawCombatBox_RestoreAXY

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DrawCombatBox_RestoreAXY  [$B0C7 :: 0x330D7]
;;
;;    Simply calls DrawCombatBox, then restores A,X,Y.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawCombatBox_RestoreAXY:
    JSR DrawCombatBox_L
    JMP RestoreAXY
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DrawDamageCombatBox  [$B0CD :: 0x330DD]
;;
;;    Draws the combat box which contains the amount of damage the attack dealt.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawDamageCombatBox:
    PHA                 ; backup A,X,Y
    TXA
    PHA
    TYA
    PHA
    
    LDX #$06            ; loop 6 times
    LDY #$00
  @Loop:
      LDA @Data, Y      ; copy unformatted text to output buffer
      STA $6D1C, Y
      INY
      DEX
      BNE @Loop
      
    LDX #<$6D1C                     ; YX is a a pointer to our text buffer in RAM
    LDY #>$6D1C
    INC btl_combatboxcount_alt      ; increment combat box counter
    LDA #$03                        ; combat box ID 3
    JMP DrawCombatBox_RestoreAXY    ; Draw the combat box and exit
    
    @Data:                          ; data for "###DMG"
      .BYTE $0C                     ; format number
      .WORD math_basedamage         ; pointer to number to print
      .BYTE $0F, BTLMSG_DMG         ; "DMG" battle message
      .BYTE $00                     ; null terminator
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ShowAltBattleMessage  [$B0F2 :: 0x33102]
;;
;;  input:   A = the ALTBLTMSG id of the battle message to print.
;;
;;    For whatever reason (I have no idea why), this routine does not use the normal string ID
;;  for the battle message.  Instead, it has its own set of "alternative" battle message IDs
;;  that simply get mapped to normal IDs through a LUT.  This makes absolutely zero sense to
;;  me and is nothing more than code obfuscation in my book.
;;
;;    What's worse... there are even some duplicate alternate IDs.  That is, two IDs to refer
;;  to the exact same string.
;;
;;    The routine will print the battle message, wait a bit (respond delay), then
;;  erase the battle message.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ShowAltBattleMessage:
    PHA                     ; backup AXY
    STA $6BCF               ; (also, store the alt message ID in temp mem)
    TXA
    PHA
    TYA
    PHA
    
    LDA #$0F                ; 0F is the control code for printing battle messages
    STA btltmp_altmsgbuffer ;  write it to a string buffer
    
    LDX $6BCF               ; Get the alt message ID in X
    LDA @AltMessageLut, X   ; run it through a LUT to get the ACTUAL ID.
    
        ; One message in particular isn't applied to player targets
    LDX btl_defender                ; check the defender/target
    BPL :+                          ; if it's an enemy, print it normally
    CMP #BTLMSG_BROKENTOPIECES      ; otherwise, check to see if the message is "broken into pieces"
    BNE :+                          ; if it is....
    LDA #BTLMSG_STOPPED             ; ... replace that with "Stopped"

  : STA btltmp_altmsgbuffer + 1 ; write the message ID
  
    LDX #<btltmp_altmsgbuffer   ; make XY point to the buffer
    LDY #>btltmp_altmsgbuffer
    LDA #$04                    ; combat box 4 is the bottom battle message box
    
    JSR DrawCombatBox_L         ; show the box
    JSR RespondDelay            ; wait
    LDA #$01
    JSR UndrawNBattleBlocks_L   ; hide the box
    
    JMP RestoreAXY              ; restore & exit
    
  ; converts ALTBLTMSG_XXX ID to matching BTLMSG_XXX ID.
      @AltMessageLut:               
      .BYTE BTLMSG_RUNAWAY
      .BYTE BTLMSG_PARALYZED_B
      .BYTE BTLMSG_SLEEPING
      .BYTE BTLMSG_SILENCED
      .BYTE BTLMSG_INEFFECTIVE
      .BYTE BTLMSG_CONFUSED
      .BYTE BTLMSG_SILENCED
      .BYTE BTLMSG_ASLEEP
      .BYTE BTLMSG_PARALYZED_A
      .BYTE BTLMSG_DARKNESS
      .BYTE BTLMSG_POISONED
      .BYTE BTLMSG_BROKENTOPIECES
      .BYTE BTLMSG_TERMINATED
      .BYTE BTLMSG_CURED
      .BYTE BTLMSG_BREAKSILENCE
      .BYTE BTLMSG_WOKEUP
      .BYTE BTLMSG_CURED
      .BYTE BTLMSG_SIGHTRECOVERED
      .BYTE BTLMSG_NEUTRALIZED
      .BYTE BTLMSG_INEFFECTIVENOW
      .BYTE BTLMSG_SLAIN
      .BYTE BTLMSG_NOTHINGHAPPENS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_PrintMagicMessage  [$B13D :: 0x3314D]
;;
;;    Prints the appropriate battle message for a spell after it was cast
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_PrintMagicMessage:
    PHA                             ; backup A,X,Y
    TXA
    PHA
    TYA
    PHA
    
    LDA btlmag_spellconnected       ; see if the spell connected
    BNE :+
      JSR Battle_ShowIneffective    ; if not, just show the 'Ineffective' message and exit
      JMP @Exit
      
  : LDA btl_attackid               ; otherwise, get the attack type
    CMP #$42                        ; if >= $42, it indicates an enemy attack
    BCC :+                          ;   These messages are printed elsewhere, so just delay and exit
      JSR RespondDelay              ;   Note that instead of BCC and these calls, it could just "BCS @DelayAndExit"
      JMP @Exit                     ;   and save 6 bytes.  Whatev.
      
  : LDX btl_attackid               ; attack type in X
    CPX #$40                        ; if >= $40, it is either a heal potion ($40), or a pure potion ($41)
    BCC :+
      CPX #$40                      ; if pure potion, just delay and exit (message already printed)
      BNE @DelayAndExit             ;   note that this could be removed if the CMP #$42 above changed to CMP #$41
      
      LDA #BTLMSG_HPUP              ; if heal potion, print the "HP up" message
      BNE @PrintMessage
      
    ; reaches here if it's a legit spell ($00-$3F) -- value in X
  : LDA lut_MagicBattleMessages, X  ; Get the desired message to print
    BEQ @DelayAndExit               ; if 0, don't print anything.  Instead, just delay and exit
    
  @PrintMessage:
    STA $6D19+1                     ; store ID of message to print
    LDA #$0F                        ; 0F = code to indicate we want to print a battle message.
    STA $6D19                       ; It's assumed $6D19+2 was already zero'd for the terminator.
    
    LDX #<$6D19
    LDY #>$6D19                     ; YA is pointer to the text to print
    LDA #$04                        ; combat box 4 (battle message box)
    JSR DrawCombatBox_L             ; Draw it!
    
    JSR RespondDelay                ; Wait a bit so they can read it
    
    LDA #$01
    JSR UndrawNBattleBlocks_L       ; Undraw it!
    JMP RestoreAXY                  ; Then restore and exit!
    
  @DelayAndExit:
    JSR RespondDelay
  @Exit:
    JMP RestoreAXY

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ApplyEnemyAilmentMask  [$B190 :: 0x331A0]
;;
;;  input:   A = ailments to keep
;;
;;  effectively does:   ailments &= A
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ApplyEnemyAilmentMask:
    LDY #en_ailments
    AND ($9A), Y
    STA ($9A), Y
    RTS

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Battle_DoEnemyTurn  [$B197 :: 0x331A7]
;;
;;  Takes an enemy's turn in battle.
;;
;;  input:  A = slot id of enemy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Battle_DoEnemyTurn:
        @enram = $9A            ; local (points to enemy stats in RAM)

    STA btl_attacker
    STA btl_attacker_alt
    
    JSR GetEnemyStatPtr
    STA @enram                  ; put pointer to this enemy's stats in $9A,9B
    STX @enram+1
    
    LDA #$02
    STA btlmag_playerhitsfx     ; enemy->player magic plays the "cha" sound effect (ID 2)
    
    JSR ClearAltMessageBuffer
    LDA #$00
    STA btlmag_magicsource      ; Enemies can't use potions or items -- so their magic source is always 'magic'
    
    LDY #en_ailments
    LDA (@enram), Y             ; see if they're stunned or asleep
    AND #AIL_SLEEP | AIL_STUN
    BEQ @EnemyActive
    
    ;;;;  This block runs if enemy is stunned or asleep
    JSR DrawCombatBox_Attacker      ; draw the attacker box
    
    AND #AIL_SLEEP                  ; see if they are asleep
    BNE @Asleep                     ;  if yes, jump ahead to asleep code                    
        
        ;; Otherwise, they are paralyzed
    JSR BattleRNG_L         ; random number between [0,255]
    CMP #25                 ; if that number is less than 25 (less than 10% chance)
    BCS :+                  ; then the paralysis is cured:
      LDA #~AIL_STUN
      JSR ApplyEnemyAilmentMask     ; remove STUN ailment mask
      LDA #ALTBTLMSG_CURED_2        ; display "Cured!" message and end their turn
      BNE @PrintAndEnd
  : LDA #ALTBTLMSG_PARALYZED_B
    BNE @PrintAndEnd
    
  @Asleep:                  ; If the enemy is asleep
    LDY #en_unknown12       ; This is TOTALLY *BUGGED*
    LDA (@enram), Y         ; Loads a 16-bit value into the math buffer that is NEVER properly initialized.
    STA btl_mathbuf         ;   The low byte is enemy's max HP... but the high byte is never set properly
    INY
    LDA (@enram), Y
    STA btl_mathbuf+1
    
    LDA #$00
    LDX #$50
    JSR RandAX              ; then pick a random number between [0,$50]
    TAX
    LDA #$01                ; And subtract that number from THE WRONG MATH BUFFER!!!  A should be 0 here, not 1
    JSR MathBuf_Sub
    LDA btl_mathbuf+1       ; Then check the high byte
    BMI :+                          ; if negative, keep sleeping.  Note again, this is IMPOSSIBLE because the value
      LDA #~AIL_SLEEP               ;  MathBuf_Sub *CAPS THE DIFFERENCE AT ZERO* so it will NEVER result in a negative number.
      JSR ApplyEnemyAilmentMask     ;  This is so broken.
      LDA #ALTBTLMSG_WOKEUP
      BNE @PrintAndEnd              ; Regardless, the end result is that an enemy will ALWAYS immediately wake up when asleep.
      
  : LDA #ALTBTLMSG_SLEEPING
    
  @PrintAndEnd:
    JMP ShowAltBattleMessage_ClearAllBoxes

  @EnemyActive:                         ; jumps here if enemy is active (not stunned or asleep)
    LDY #en_ailments
    LDA (@enram), Y                     ; check the high bit to see if they're confused
    BPL @EnemyActive_AndNotConfused     ; if clear, jump ahead to EnemyActive_AndNotConfused.  Otherwise...
    
    ; If enemy is confused:
    JSR BattleRNG_L                     ; random [0,$FF]
    CMP #$40
    BCS :+                              ; cured if < $40  (25% chance)
      LDA #~AIL_CONF
      JSR ApplyEnemyAilmentMask
      JSR DrawCombatBox_Attacker
      LDA #ALTBTLMSG_CURED_1
      BNE @PrintAndEnd
      
  : JSR DrawCombatBox_Attacker          ; otherwise, the enemy is confused....
    LDA #MG_FIRE - MG_START             ;   cast FIRE on a random enemy  -- which is totally lame.  I would expect stronger
    STA btl_attackid                    ;   enemies to have a stronger attack.
    JSR Battle_PrepareMagic             ; Note that even though we are casting FIRE, the attack combat box is never drawn, so
    JSR Battle_CastMagicOnRandEnemy     ;   it *looks* like a physical attack.
    JMP ClearAllCombatBoxes             ; then clear all combat boxes and exit
            ;; NOTE:  This is BUGGED!!!!  As it does a magic effect but does not call Battle_EndMagicTurn so
            ;;   DoTurn output is not properly prepared.  This means that if the last enemy kills himself
            ;;   through confusion, the battle will not end and the game will potentially hang.
            ;;   This should not be JMPing to ClearAllCombatBoxes, but should instead be JMPing to
            ;;   Battle_EndMagicTurn.  Note that there's an alternative way to fix this in
            ;;   BattleTurnEnd_CheckForBattleEnd.  See that routine for details.
    
    ;;  Code reaches here if enemy has normal status -- not stunned, asleep, or confused.  It can just do
    ;;  its normal action:
  @EnemyActive_AndNotConfused:
        @enrom = $9C            ; local - points to enemy data in ROM
        
    LDY #en_romptr
    LDA (@enram), Y
    STA @enrom
    INY
    LDA (@enram), Y
    STA @enrom+1           ; pointer to enemy ROM data in $9C,9D  (for Enemy_DoAi)
    
    LDA ch_level
    ASL A
    STA $9E             ; $9E = 2*level of party leader
    
    ; Check the enemy's morale and see if they are going to run away.
    ; The formula for this is:
    ;
    ;   V = X + Morale - L
    ;
    ; where:
    ;   X = random number between [$00,$32]
    ;   Morale = enemy's morale stat
    ;   L = 2* level of the party leader.
    ;
    ;   if V is less than $50, the enemy will run!
    
    LDY #en_morale
    LDA (@enram), Y     ; morale
    SEC
    SBC $9E
    BCC @RunAway        ; run if morale < 2*level_of_leader
    STA $9E             ; store Morale-L in 9E
    LDA #$00
    LDX #$32
    JSR RandAX          ; random between [$00-$32]
    CLC
    ADC $9E             ; A is now 'V' above... X+Morale-L
    BCS Enemy_DoAi      ; if > 255, do not run
    CMP #$50
    BCS Enemy_DoAi      ; if >= $50, do not run
    ; Otherwise, fall through to @RunAway
    
  @RunAway:
    JSR DrawCombatBox_Attacker
    LDA #ALTBTLMSG_RUNAWAY
    JSR ShowAltBattleMessage
    
    JSR TargetSelf                  ; Target self
    STA battle_defender_index       ; record as defender index (See Battle_DoTurn for why this is important)
    
    JSR EraseEnemyGraphic
    LDA #AIL_DEAD
    STA btl_defender_ailments       ; Mark ourselves as dead (See Battle_DoTurn for why this is important)
    LDY #en_ailments
    STA (@enram), Y                 ; Give ourselves the 'DEAD' ailment
    
    LDA #$00
    STA battle_defenderisplayer     ; Fill output:  defender is an enemy
    
    LDY #en_exp
    : STA (@enram), Y               ; erase this enemy's GP and Exp rewards
      INY
      CPY #en_exp+4
      BNE :-
      
    LDX btl_attacker
    JSR ClearEnemyID
    JMP ClearAllCombatBoxes


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ClearEnemyID  [$B288 :: 0x33298]
;;
;;  X = slot index whose enemy ID to clear
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ClearEnemyID:
    LDA #$FF
    STA btl_enemyIDs, X
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ShowAltBattleMessage_ClearAllBoxes  [$B28E :: 0x3329E]
;;
;;  A = Alt battle message to display
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ShowAltBattleMessage_ClearAllBoxes:
    JSR ShowAltBattleMessage
    JMP ClearAllCombatBoxes


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  EnemyAi_ShouldPerformAction  [$B294 :: 0x332A4]
;;
;;  input:   A = 'rate' between $00-80
;;  output:  C = CLEAR if the action should be performed.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

EnemyAi_ShouldPerformAction:
    STA $6BCF       ; store rate
    LDA #$00
    LDX #$80
    JSR RandAX      ; rand[ 0, 128 ]
    CMP $6BCF       ; C clear if rand is less than rate (action should be performed)
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Enemy_DoAi  [$B2A2 :: 0x332A2]
;;
;;    Have the enemy check its AI and pick an action to perform, then actually perform
;;  the action.
;;
;;  input:  $9A,9B = points to enemy stats in RAM
;;          $9C,9D = points to enemy stats in ROM
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Enemy_DoAi:
        @enram = $9A        ; input - pointer to enemy RAM stats
        @enrom = $9C        ; input - pointer to enemy ROM stats
        
        @aiptr = $9E        ; local - pointer to enemy's AI data
    
    LDA #$00                    ; initialize high byte of AI pointer (in preparation for math below)
    STA @aiptr+1                ;   (this could be done in the below math, but whatever)
    
    LDY #ENROMSTAT_AI
    LDA (@enrom), Y
    CMP #$FF                    ; get AI id from ROM (if it's read from ROM, why is it stored in RAM?)
    BNE :+
      JMP ChooseAndAttackPlayer ; no AI -- just attack a random player
      
    ; use enemy AI
  : LDX #$10                ; get a pointer to this enemy's AI data
    JSR MultiplyXA          ; $10 bytes per AI entry
    CLC                     ;   end result, of this math:  @aiptr points to this enemy's AI data
    ADC #<lut_EnemyAi
    STA @aiptr
    TXA
    ADC #>lut_EnemyAi
    STA @aiptr+1            ; pointer to AI data
    
    LDY #$00
    LDA (@aiptr), Y                 ; Get byte 0 (magic rate)
    JSR EnemyAi_ShouldPerformAction ; See if we should do a magic attack
    BCS @CheckSpecialAttack         ; if not, jump ahead to check for special attacks
    
  @DoMagicAttack:
    LDY #en_aimagpos
    LDA (@enram), Y
    AND #$07                        ; get current magic pos
    JSR @IncrementAiPos             ; increment magic position
    ADC #$02                        ; position+2 is the index to the spell to cast
    TAY
    LDA (@aiptr), Y                 ; get the spell to cast
    CMP #$FF                        ; if $FF (empty slot)...
    BNE :+
      LDA #$00                      ; ...reset position, and start over
      LDY #en_aimagpos
      STA (@enram), Y
      JMP @DoMagicAttack            ; keep going until we find a non-empty spell slot
    
  : JMP Enemy_DoMagicEffect         ; with the magic spell in A, do the magic attack
    
    ;;  This is like a mini-subroutine used by the surrounding code
    ;;    it is always JSR'd to, never branched to.  So RTS is OK.
  @IncrementAiPos:
    PHA                             ; backup position
    CLC
    ADC #$01                        ; +1
    STA (@enram), Y                    ; and write it back
    PLA                             ; restore backup
    RTS
  
  @CheckSpecialAttack:
    LDY #$01
    LDA (@aiptr), Y                 ; get special attack rate
    JSR EnemyAi_ShouldPerformAction ; see if we should do it
    BCS ChooseAndAttackPlayer       ; if not, just do a normal attack
    
    ; otherwise, do a special attack
  @DoSpecialAttack:
    LDY #en_aiatkpos            ; This block is the same as @DoMagicAttack -- the only difference
    LDA (@enram), Y             ;   is it cycles through the 4 enemy attack slots instead of the
    AND #$03                    ;   8 magic slots.
    JSR @IncrementAiPos
    ADC #$0B
    TAY
    LDA (@aiptr), Y
    CMP #$FF
    BNE :+
      LDA #$00
      LDY #en_aiatkpos
      STA (@enram), Y
      JMP @DoSpecialAttack
  : CLC
    ADC #$42                    ; add $42 to the special attack ID to indicate it's a special attack (0-3F are magic, and 40,41 are heal/pure potions)
    JMP Enemy_DoMagicEffect     ; and perform the special attack

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ChooseAndAttackPlayer  [$B319 :: 0x33329]
;;
;;    Choose a random player target, and then do a physical attack against them.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ChooseAndAttackPlayer:
    JSR GetRandomPlayerTarget       ; get random target
    LDA btl_randomplayer
    LDX btl_attacker                ; attacker slot
    JMP EnemyAttackPlayer_Physical  ; do the attack and exit
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  GetRandomPlayerTarget [$B325 :: 0x33335]
;;
;;  output:  btl_randomplayer = 0-based player ID of who to target
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

GetRandomPlayerTarget:
    PHA                 ; backup A,X
    TXA
    PHA
    
  @GetTargetLoop:
      LDA #$00
      STA btl_randomplayer  ; zero output
      
      JSR BattleRNG_L       ; get a random number
      CMP #$20
      BCS :+
        INC $6BCF           ; inc target if < $20
    : CMP #$40
      BCS :+
        INC $6BCF           ; inc target if < $40
    : CMP #$80
      BCS :+
        INC $6BCF           ; inc target if < $80
        
      ; The end result here, is:
      ;  4/8 chance of target=0   (rand is [80,FF])
      ;  2/8 chance of target=1   (rand is [40,7F])
      ;  1/8 chance of target=2   (rand is [20,3F])
      ;  1/8 chance of target=3   (rand is [00,1F])
      
    : LDA btl_drawflagsA        ; get 'dead' flags
      ORA btl_drawflagsB        ; OR with 'stone' flags
      
      ASL A                 ; left shift 1 because below loop is 1-based
      LDX $6BCF             ; get target
      INX                   ; make target 1 based
      : LSR A
        DEX
        BNE :-              ; loop X times, to get target's dead|stone bit into bit position 0
        
      AND #$01              ; check target's dead|stone bit
      BNE @GetTargetLoop    ; if nonzero, this target is invalid, so try again until we get a valid target
      
    PLA         ; restore A,X
    TAX
    PLA
    RTS
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Battle_PrepareMagic  [$B35C :: 0x3336C]
;;
;;    Loads the stats for the desired magic spell.
;;
;;  input:   btl_attackid = ID of spell
;;           btl_attacker = ID of attacker
;;
;;  output:  btl_magdataptr,  btlmag_xxx.
;;
;;  NOTE!!
;;      If the spell has no effect, this routine will double-RTS out to abort the player's turn.
;;  Since this routine is called before the player walks forward to cast their spell, this routine
;;  must walk them forward before double-RTSing.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Battle_PrepareMagic:
    LDA btl_attackid                ; get the attack ID
    CMP #$FF                        ; if $FF, it's a non-attack.  This can only happen when
    BEQ @Ineffective                ;    a player uses an item that has no effect
    
    ; At this point, A has the attack ID which is in the range [$00,$3F] and contains a magic index
    LDX #$08
    JSR MultiplyXA                  ; Do a bunch of math
    STA btl_magdataptr              ; end result:  btl_magdataptr = lut_MagicData + (attackid * 8)
    STX btl_magdataptr+1            ;   and therefore points to the data for this spell
    LDA #<lut_MagicData             ;   (each spell has 8 bytes of data)
    CLC                             ; Note that this TOTALLY could be accomplished by calling
    ADC btl_magdataptr              ;   GetPointerToMagicData and simply STA/STX'ing the result.
    STA btl_magdataptr              ;   This is a waste of space.
    LDA #>lut_MagicData
    ADC btl_magdataptr+1
    STA btl_magdataptr+1
    
    
    LDY #MAGDATA_EFFECT             ; get the effect byte
    LDA (btl_magdataptr), Y         ; if it's nonzero (valid effect), jump ahead to @SpellOk
    BNE @SpellOk                    
    
        ; otherwise (spell with no IB effect, like EXIT), fall through to @Ineffective
        ; Now... the below code assumes the player selected a bad spell... and it will do walking/casting
        ;  animation.  This is arguably BUGGED, since this code will run if an enemy selects a bad spell
        ;  ... which granted will never happen unless the enemy AI is retarded (will never happen in the
        ;  original game), but still.
        ;  A proper fix would be to skip the WalkForwardAndCastMagic call if btl_attacker has the high bit clear.
    
    ; The Ineffective block will actually do a double-RTS to abort the player's entire action.
    ;   In the original game, this code will only be reached for players (because enemies will never use
    ;   bad items/spells)
      @Ineffective:
        LDA btl_attacker
        AND #$03                        ; mask out character index from the attacker ID
        LDX #$01                        ; X=1 to indicate to NOT draw magic sprite.
        JSR WalkForwardAndCastMagic     ; do the walk/magic animation
        
        LDA btlmag_magicsource
        CMP #$02
          BNE :+
          LDA #ALTBTLMSG_NOTHINGHAPPENS ; source=02 (item)
          BNE :++
        : LDA #ALTBTLMSG_INEFFECTIVENOW ; source=00 or 01 (magic or drink)
      : JSR ShowAltBattleMessage_ClearAllBoxes  ; Show the battle message, then clear all boxes
      
        PLA                             ; Double-RTS to abort player's turn
        PLA
        RTS
    
    ; Code reaches here if the spell has a valid effect  (effect in A)
    ;   Load the stats of this magic spell
  @SpellOk:
    STA btlmag_effect           ; record effect
    
    LDY #MAGDATA_ELEMENT        ; magic's element
    LDA (btl_magdataptr), Y
    STA btlmag_element
    
    LDY #MAGDATA_HITRATE        ; hit rate
    LDA (btl_magdataptr), Y
    STA btlmag_hitrate
    
    LDY #MAGDATA_EFFECTIVITY    ; and effectivity
    LDA (btl_magdataptr), Y
    STA btlmag_effectivity
    
    RTS
    
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Player_DoItem [$B3B5 :: 0x333C5]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Player_DoItem:
    STA btl_attackid        ; record the attack ID
    LDA #$02                ; source = item
    JMP Player_DoMagicEffect
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Player_DoDrink [$B3BD :: 0x333CD]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Player_DoDrink:
    STA btl_attackid        ; record the attack ID
    LDA #$01                ; source = drink
    JMP Player_DoMagicEffect
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Player_DoMagic [$B3C5 :: 0x333D5]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Player_DoMagic:             ; Player Magic
    STA btl_attackid        ; record the attack ID
    LDA #$00                ; source = magic
  ; JMP Player_DoMagicEffect; <- flow into
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Player_DoMagicEffect [$B3CA :: 0x333DA]
;;
;;    Does EVERYTHING for when a player is taking their turn and is doing
;;  a MAGIC/DRINK/ITEM action.
;;
;;  input:
;;    btl_attackid = the ID of the magic effect to perform
;;               A = the source (written to btlmag_magicsource)
;;               X = the target/defender
;;               Y = the attacker
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Player_DoMagicEffect:
    STA btlmag_magicsource      ; record the source
    TYA                         ; Y = attacker, put attacker in A
    AND #$03
    ORA #$80
    STA btl_attacker            ; make sure high bit is set, and record them as an attacker
    STA $6BD1                   ; ???  This value seems to never be used.
    STX btl_defender            ; X contains the defender, record them as well
    
    LDA #$00
    STA btlmag_playerhitsfx     ; player->player magic plays the 'heal' sound effect (ID 0)
    
    JSR ClearAltMessageBuffer   ; Clear alt message buffer
    JSR DrawCombatBox_Attacker  ; Draw the attacker name box
    JSR DrawCombatBox_Attack    ; Draw the spell name box
    
    LDA btl_attacker            ; Load attacker's stat pointer
    JSR PrepEntityPtr_Player
        
    LDY #ch_ailments - ch_stats ; check their ailments to see if they're muted.  This is weird, as it's done for
    LDA (btl_entityptr_obrom), Y; Magic (makes sense), items (weird), and DRINK (no sense at all)
    AND #AIL_MUTE               ; You could argue this is BUGGED
    BEQ :+
      LDA #ALTBTLMSG_SILENCED_1                 ; if muted, just print "Silenced" message
      JMP ShowAltBattleMessage_ClearAllBoxes    ; clear all boxes, and exit (don't actually do the spell effect)
      
  : LDA #$00
    STA btlmag_fakeout_ailments             ; clear the fakeout ailments
    
    JSR Battle_PrepareMagic                 ; Otherwise, load up magic effect info
    JSR Battle_PlayerMagic_CastOnTarget     ; And actually cast the spell
    JMP Battle_EndMagicTurn                 ; End the turn

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Battle_PlayerMagic_CastOnTarget [$B40A :: 0x3341A]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Battle_PlayerMagic_CastOnTarget:
    JSR PreparePlayerMagAttack          ; load some attacker stats and walk the player forward
    
    LDY #MAGDATA_TARGET                 ; Check the target for the spell they're casting
    LDA (btl_magdataptr), Y
    
    LSR A
    BCC :+
      JMP Battle_PlMag_TargetAllEnemies ; bit 0 set = target all opponents
  : LSR A
    BCC :+
      JMP Battle_PlMag_TargetOneEnemy   ; bit 1 set = target one opponent
  : LSR A
    BCC :+
      BCS Battle_PlMag_TargetSelf       ; bit 2 set = target self
  : LSR A
    BCC Battle_PlMag_TargetOnePlayer    ; (other) = target one ally
    BCS Battle_PlMag_TargetAllPlayers   ; bit 3 set = target all allies
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Battle_PlMag_TargetSelf [$B427 :: 0x33437]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Battle_PlMag_TargetSelf:
    JSR TargetSelf                      ; set target to yourself
    JSR DrawCombatBox_Defender          ; draw defender box
    JMP Battle_CastMagicOnPlayer        ; and cast the spell
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Battle_PlMag_IsPlayerValid  [$B430 :: 0x33440]
;;
;;    Used by Battle_PlMag_XXX routines to see if a player target is valid.
;;  Player targets are invalid if they are dead/stone.
;;
;;  input:  btl_entityptr_obrom - should point to target's OB stats
;;
;;  output: Z = set if target is valid, clear if invalid
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Battle_PlMag_IsPlayerValid:
    LDY #ch_ailments - ch_stats
    LDA (btl_entityptr_obrom), Y
    AND #AIL_DEAD | AIL_STONE
    RTS
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Battle_PlMag_TargetOnePlayer [$B437 :: 0x33447]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Battle_PlMag_TargetOnePlayer:
    JSR DrawCombatBox_Defender              ; Draw defender box
    JSR BtlMag_LoadPlayerDefenderStats      ; Load defender stats (and do "hit with magic" animation/sound)
    JSR Battle_PlMag_IsPlayerValid          ; Is this a valid target?
    BEQ :+
      JMP Battle_ShowIneffective            ; if not, show "Ineffective" and exit
  : JMP Battle_CastMagicOnPlayer_NoLoad     ; otherwise, do the actual spell (noload because we already loaded above)
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Battle_PlMag_TargetAllPlayers [$B448 :: 0x33458]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Battle_PlMag_TargetAllPlayers:
    LDA btl_attacker
    STA btl_attacker_alt            ; set the stupid alt attacker val
    
    LDA #$00
    STA $6BCE                       ; loop counter (loop through all 4 players)
    
@Loop:
    LDA $6BCE                           ; use loop counter as player index
    ORA #$80                            ; set high bit to indicate it is a player target
    STA btl_defender                    ; set as defender
    
    JSR DrawCombatBox_Defender          ; ... do magic casting stuff
    JSR BtlMag_LoadPlayerDefenderStats
    JSR Battle_PlMag_IsPlayerValid
    BEQ :+
      JSR Battle_ShowIneffective        ; show Ineffective if target is dead/stone
      JMP @Next
  : JSR Battle_CastMagicOnPlayer_NoLoad ; (NoLoad because we loaded above)
  
  @Next:
    JSR RespondDelay_UndrawAllBut2Boxes ; undraw all but the Attacker/Attack boxes
    JSR DrawCharacterStatus             ; update player status' (redraw their HP and stuff)
    
    INC $6BCE                           ; loop through all 4 players.
    LDA $6BCE
    CMP #$04
    BNE @Loop
    
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Battle_PlMag_TargetOneEnemy [$B480 :: 0x33490]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Battle_PlMag_TargetOneEnemy:
    JSR DrawCombatBox_Defender      ; draw the defender box (even if they no longer exist)
    LDX btl_defender
    JSR DoesEnemyXExist
    BNE :+                          ; if the enemy does not exist
      JSR Battle_ShowIneffective    ;  just show "Ineffective" message, and exit
      BNE @Exit                     ; (always branches... why doesn't it just RTS?)
  : TXA
    JSR Battle_CastMagicOnEnemy     ; otherwise, if they exist, cast the magic on them!
  @Exit:
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Battle_PlMag_TargetAllEnemies [$B495 :: 0x334A5]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Battle_PlMag_TargetAllEnemies:
    LDA #$00
    STA $6BCE                       ; loop counter (loop through 9 enemy slots)
    
  @Loop:
    LDY $6BCE
    LDA btl_enemyIDs, Y             ; get the ID of this enemy slot
    CMP #$FF                        ;  if FF, the enemy does not exist, so skip it
    BEQ @Next
      LDA $6BCE                             ; otherwise, if not FF, it's a valid target.
      STA btl_defender                      ; make it the defender
      JSR DrawCombatBox_Defender            ; draw the Defender box
      JSR Battle_CastMagicOnEnemy           ; do the actual spell
      JSR RespondDelay_UndrawAllBut2Boxes   ; undraw all but attacker and attack boxes
      
  @Next:
    INC $6BCE
    LDA $6BCE           ; Keep looping until all 9 slots targetted
    CMP #$09
    BNE @Loop
    
    RTS
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  PrepEntityPtr_Enemy  [$B4BE :: 0x334CE]
;;
;;    Fills btl_entity_ptrs to point to enemy data.
;;
;;  input:        A = enemy slot ID
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


PrepEntityPtr_Enemy:
    JSR GetEnemyStatPtr             ; get the RAM pointer in XA
    STA btl_entityptr_ibram
    STX btl_entityptr_ibram+1
    
    LDY #en_enemyid
    LDA (btl_entityptr_ibram), Y    ; get enemy ID and throw it away (100% pointless)
    
    LDY #en_romptr                  ; get ROM pointer
    LDA (btl_entityptr_ibram), Y
    STA btl_entityptr_obrom
    INY
    LDA (btl_entityptr_ibram), Y
    STA btl_entityptr_obrom+1
    
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Enemy_DoMagicEffect  [$B4D5 :: 0x334E5]
;;
;;  input:  $9A = point to enemy's stats in RAM
;;            A = ID of effect they're casting (>= $42 for enemy attacks)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
Enemy_DoMagicEffect:
            @enram =    $9A     ; input - points to this enemy's stats in RAM
            
    STA btl_attackid                        ; store spell/attack we're using
    JSR ClearAltMessageBuffer
    LDA btl_attacker_alt                    ; get attacker slot index (why doesn't this just load btl_attacker?!?!?!)
    JSR PrepEntityPtr_Enemy
    JSR DrawCombatBox_Attacker
    
    LDA #$00
    STA btlmag_magicsource                  ; set magic source as 'magic'
    JSR DrawCombatBox_Attack
    
    LDA #$00
    STA btlmag_fakeout_ailments             ; clear the fakeout ailments
    
    JSR Battle_PrepareMagic                 ; load magic's stats
    JSR PrepareEnemyMagAttack               ; load attacker's stats (never used!  Pointless!)
    
    LDY #en_ailments
    LDA (@enram), Y                         ; get this enemy's ailments
    AND #AIL_MUTE                           ; are they muted?
    BEQ :+
      JSR DrawCombatBox_Attack              ; if yes, draw their attack name
      LDA #ALTBTLMSG_INEFFECTIVE            ; but then just say "ineffective"
      JMP ShowAltBattleMessage_ClearAllBoxes; then clear boxes and exit
    
  : LDY #MAGDATA_TARGET                 ; get the spell's Target byte
    LDA (btl_magdataptr), Y
    JSR Battle_EnemyMagic_CastOnTarget  ; and use it to cast this spell!
  ; JMP Battle_EndMagicTurn             ; <- flow into -- end the turn

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Battle_EndMagicTurn  [$B50E :: 0x3351E]
;;
;;  Called at the end of a player/enemy's turn to finalize some stuff.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Battle_EndMagicTurn:
    JSR ClearAllCombatBoxes
    
    LDA btlmag_fakeout_ailments     ; copy the 'fakeout' values to output
    STA btl_defender_ailments       ; See Battle_DoTurn for explanation
    LDA btlmag_fakeout_defindex
    STA battle_defender_index
    LDA btlmag_fakeout_defplayer
    STA battle_defenderisplayer
    
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Battle_EnemyMagic_CastOnTarget  [$B524 :: 0x33534]
;;
;;    Casts a magic spell!  (note:  not an enemy attack, this is for magic only).
;;
;;  input:
;;               A = spell's target value (as stored in MAGDATA_TARGET)
;;      btlmag_xxx = filled with magic info
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Battle_EnemyMagic_CastOnTarget:
    LSR A
    BCC :+
      JMP Battle_CastMagicOnAllPlayers      ; 01 = target all opponents
      
  : LSR A
    BCC :+
      JMP Battle_CastMagicOnRandomPlayer    ; 02 = target one opponent
      
  : LSR A
    BCC :+
      BCS Battle_CastMagicOnSelf_Enemy      ; 04 = target self
      
  : LSR A
    BCC :+
      BCS Battle_CastMagicOnAllEnemies      ; 08 = target all allies
      
  : JMP Battle_CastMagicOnRandEnemy         ; 10 (others) = target one ally

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  TargetSelf  [$B53D :: 0x3354D]
;;
;;  defender = attacker
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TargetSelf:
    LDA btl_attacker
    STA btl_defender
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Battle_CastMagicOnSelf_Enemy  [$B544 :: 0x33554]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Battle_CastMagicOnSelf_Enemy:           ; pretty straight forward....
    JSR TargetSelf
    JSR DrawCombatBox_Defender
    JMP Battle_CastMagicOnEnemy
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Battle_CastMagicOnRandEnemy  [$B54D :: 0x3355D]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Battle_CastMagicOnRandEnemy:
  @Loop:
    LDA #$00
    LDX #$08
    JSR RandAX
    TAX                     ; random enemy slot [0,8]
    
    JSR DoesEnemyXExist     ; does an enemy exist in that slot?
    BEQ @Loop               ; loop until we find an enemy that exists
    
    ; once we have an enemy that exists
    STX btl_defender            ; set it as the defender
    JSR DrawCombatBox_Defender  ; print defender box
    JMP Battle_CastMagicOnEnemy ; and cast the spell!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Battle_CastMagicOnAllEnemies  [$B563 :: 0x33573]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Battle_CastMagicOnAllEnemies:
    LDA #$00
    STA $6BCE                   ; loop counter.  Loop through all 9 enemy slots
    
  @Loop:
    LDA $6BCE                   ; BUGGED - see if this enemy is the attacker, and skip it if it is.
    CMP btl_attacker            ;  This doesn't make any sense, as there's no reason why "target all" spells
    BEQ @Next                   ;  should skip the caster.  It doesn't behave this way for players.
    
    TAX                         ; Skip the enemy if the slot is empty
    JSR DoesEnemyXExist
    BEQ @Next
    
    ; Otherwise, the enemy exists!  Do it!
    LDA $6BCE
    STA btl_defender                    ; set defender
    JSR PrepEntityPtr_Enemy             ; prep pointers
    JSR DrawCombatBox_Defender          ; Draw defender box
    JSR Battle_CastMagicOnEnemy         ; cast it!
    JSR RespondDelay_UndrawAllBut2Boxes ; undraw all boxes except for attacker/attack
    
  @Next:
    INC $6BCE
    LDA $6BCE
    CMP #$09                    ; loop until all 9 enemy slots targetted
    BNE @Loop
    
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Battle_CastMagicOnEnemy  [$B593 :: 0x335A3]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Battle_CastMagicOnEnemy:
    JSR BtlMag_LoadEnemyDefenderStats   ; load enemy stats into defender mem
    JSR BtlMag_PerformSpellEffect       ; do the spell (modifying defender's stats)
    JMP BtlMag_SaveEnemyDefenderStats   ; update changed enemy stats.
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Battle_CastMagicOnRandomPlayer  [$B59C :: 0x335AC]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Battle_CastMagicOnRandomPlayer:
    JSR GetRandomPlayerTarget       ; set the defender to a random player target
    LDA btl_randomplayer
    ORA #$80
    STA btl_defender
    
    JSR DrawCombatBox_Defender      ; draw defender box
    JMP Battle_CastMagicOnPlayer    ; then cast the magic and exit!
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Battle_CastMagicOnPlayer  [$B5AD :: 0x335BD]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Battle_CastMagicOnPlayer:
    JSR BtlMag_LoadPlayerDefenderStats
Battle_CastMagicOnPlayer_NoLoad:
    JSR BtlMag_PerformSpellEffect
    JMP BtlMag_SavePlayerDefenderStats
    
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Battle_CastMagicOnAllPlayers  [$B5B6 :: 0x335C6]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Battle_CastMagicOnAllPlayers:
    LDA #$00
    STA $6BCE                   ; loop counter (loop through all players)
    
  @Loop:
    LDA $6BCE
    JSR PrepEntityPtr_Player                ; prep pointers to this player
    
    LDY #ch_ailments - ch_stats             ; if they are dead or stoned, skip them
    LDA (btl_entityptr_obrom), Y            ;  (OB ailments!  why does IB exist?!?!)
    AND #(AIL_DEAD | AIL_STONE)
    BNE :+
    
      LDA $6BCE
      ORA #$80
      STA btl_defender                      ; set defender (high bit set to indicate it's a player)
      
      JSR BtlMag_LoadPlayerDefenderStats    ; load their stats, play sound effect, and flash player graphic
      JSR DrawCombatBox_Defender            ; THEN draw the defender combat box (slightly BUGGED - shouldn't it do this before flashing the character?)
      JSR Battle_CastMagicOnPlayer_NoLoad   ; the cast the magic on the player (NoLoad because we already loaded them above)
      JSR RespondDelay_UndrawAllBut2Boxes   ; undraw all but the attacker and attack boxes
      JSR DrawCharacterStatus               ; And redraw character stats to show updated HP
      
  : INC $6BCE                   ; inc loop counter
    LDA $6BCE
    CMP #$04
    BNE @Loop                   ; loop until all 4 players have been targetted
    
    RTS

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  PrepEntityPtr_Player  [$B5EB :: 0x335FB]
;;
;;    Fills btl_entity_ptrs to point to player character data.
;;
;;    This routine is pretty much the same to PrepCharStatPointers -- only it writes to a different
;;  place in memory, and uses differen (but identical!) lookup tables.
;;
;;  input:  A = character index (0-3), high bit is dropped, so $80-$83 is also OK
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrepEntityPtr_Player:
    ASL A
    TAX
    LDA lut_IBCharStatsPtrTable_alt, X          ; pointer to IB data
    STA btl_entityptr_ibram
    LDA lut_IBCharStatsPtrTable_alt+1, X
    STA btl_entityptr_ibram+1
    
    LDA lut_CharStatsPtrTable_alt, X            ; pointer to OB data
    STA btl_entityptr_obrom
    LDA lut_CharStatsPtrTable_alt+1, X
    STA btl_entityptr_obrom+1
    
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_LoadPlayerDefenderStats  [$B602 :: 0x33612]
;;
;;    Loads battle magic stats for the defender, when the defender is a player character.
;;  Also plays the appropriate sound effect for when the player gets hit with magic, and flashes
;;  the character graphic.
;;
;;  input:  below values are expected to be set properly:
;;          - btlmag_playerhitsfx
;;          - btl_defender
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_LoadPlayerDefenderStats:
    LDA btlmag_playerhitsfx
    JSR PlayBattleSFX               ; play the appropriate sound effect for this spell
    
    LDA btl_defender
    AND #$03
    JSR FlashCharacterSprite        ; flash this character's graphic
    
    LDA btl_defender
    JSR PrepEntityPtr_Player        ; prep entityptr's
    
    LDY #ch_class - ch_stats        ; load category from OB
    LDA (btl_entityptr_obrom), Y    ;  BUGGED - this is actually using the character's class as the category.
    STA btlmag_defender_category    ;    This only matters for HARM spells, which check the Undead bit.  This bit
                                    ;    will be set for the 'Master' and 'Wizard' classes -- making those classes
                                    ;    susceptible to HARM spells.
    
    LDY #ch_ailments - ch_stats     ; Ailments from OB  (again.. why OB?  this stuff is in IB!)
    LDA (btl_entityptr_obrom), Y    ;   (IB ailments/HP just aren't used.  wtf)
    STA btlmag_defender_ailments
    
    LDY #ch_curhp - ch_stats        ; HP from OB  (again, why not IB!)
    LDA (btl_entityptr_obrom), Y
    STA btlmag_defender_hp
    INY
    LDA (btl_entityptr_obrom), Y
    STA btlmag_defender_hp+1
    
    LDY #ch_maxhp - ch_stats        ; max HP from OB
    LDA (btl_entityptr_obrom), Y
    STA btlmag_defender_hpmax
    INY
    LDA (btl_entityptr_obrom), Y
    STA btlmag_defender_hpmax+1
    
    ;; The rest of these stats come from IB
    LDY #btlch_numhitsmult
    LDA (btl_entityptr_ibram), Y
    STA btlmag_defender_numhitsmult
    
    LDY #btlch_absorb               
    LDA (btl_entityptr_ibram), Y
    STA btlmag_defender_absorb
    
    LDY #btlch_evade
    LDA (btl_entityptr_ibram), Y
    STA btlmag_defender_evade
    
    LDY #btlch_elemweak
    LDA (btl_entityptr_ibram), Y
    STA btlmag_defender_elemweakness
    
    LDY #btlch_elemresist
    LDA (btl_entityptr_ibram), Y
    STA btlmag_defender_elemresist
    
    LDY #btlch_magdef
    LDA (btl_entityptr_ibram), Y
    STA btlmag_defender_magdef
    
    ;; BUGGED
    ;  This routine does not load btlmag_defender_strength, which means TMPR/SABR
    ;  will not work when cast on players!
    
    ; The rest of these values don't really matter....
    INY                         ; ??? wasteful
    LDA #$00
    STA btlmag_defender_unknownRawInit0
    LDA #$00
    STA btlmag_defender_morale
    RTS
    
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  [$B674 :: 0x33684]
;;  Lut - pointer table to the beginning of each character's IB stats in RAM
;;
;;   This is literally an exact copy of the table at $9F65

lut_IBCharStatsPtrTable_alt:
  .WORD btl_chstats
  .WORD btl_chstats + (1*$12)
  .WORD btl_chstats + (2*$12)
  .WORD btl_chstats + (3*$12)
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  [$B67C :: 0x3368C]
;;  Lut - pointer table to the beginning of each character's stats OB in RAM
;;
;;    Again... COMPLETELY duplicate of the table at $9F5D

lut_CharStatsPtrTable_alt:
  .WORD ch_stats
  .WORD ch_stats+$40
  .WORD ch_stats+$80
  .WORD ch_stats+$C0
  
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  PreparePlayerMagAttack  [$B684 :: 0x33694]
;;
;;    Just like PrepareEnemyMagAttack, this loads a bunch of stats for the person
;;  casting the spell, but also like that routine, the stats are inconsistent and are never
;;  used, making this routine largely pointless.
;;
;;    What ISN'T pointless, though, is that this routine will actually do the animation and
;;  sound effects involved in the attack... and therefore it does have some value.
;;
;;  input:
;;      btl_attacker
;;      btlmag_magicsource
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PreparePlayerMagAttack:
    LDA btl_attacker
    AND #$7F
    
    LDX btlmag_magicsource
    BNE :+                          ; if zero, it's magic... so...
      LDA #$00                      ; ...play magic sfx.  Although this is slightly BUGGED
      JSR PlayBattleSFX             ;  since the sound effect is audible before you walk forward.  It
      LDA btl_attacker              ;  would make more sense to do this in WalkForwardAndCastMagic
      AND #$7F
      
  : LDX btlmag_magicsource          ; X=0 for magic (indicating we should do magic casting animation)
    JSR WalkForwardAndCastMagic
    
    ; Everything below this is loading the useless attacker stats.
    ;   As with PrepareEnemyMagAttack, this is all entirely pointless.
    LDA btl_attacker
    JSR PrepEntityPtr_Player
    
    LDY #ch_ailments - ch_stats
    LDA (btl_entityptr_obrom), Y
    STA btlmag_attacker_unk686C
    
    LDY #btlch_dmg
    LDA (btl_entityptr_ibram), Y
    STA btlmag_attacker_unk6875
    
    LDY #ch_level - ch_stats
    LDA (btl_entityptr_obrom), Y
    STA btlmag_attacker_unk6883
    
    LDY #btlch_hitrate
    LDA (btl_entityptr_ibram), Y
    STA btlmag_attacker_unk6884
    
    LDY #ch_class - ch_stats
    LDA (btl_entityptr_obrom), Y
    STA btlmag_attacker_unk6879
    
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_LoadEnemyDefenderStats  [$B6C8 :: 0x336D8]
;;
;;    Loads battle magic stats for the defender, when the defender is an enemy.
;;  Also does the explosion effect, since any magic targetting an enemy will have that effect.
;;
;;  input:   btl_defender = enemy index.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_LoadEnemyDefenderStats:
    LDA btl_defender
    JSR PrepEntityPtr_Enemy
    
    LDY #en_hp                      ; get HP from RAM stats
    LDA (btl_entityptr_ibram), Y
    STA btlmag_defender_hp
    INY
    LDA (btl_entityptr_ibram), Y
    STA btlmag_defender_hp+1
    
    INY                             ; Y = ENROMSTAT_HPMAX
    LDA (btl_entityptr_obrom), Y    ; Map HP from ROM stats
    STA btlmag_defender_hpmax
    INY
    LDA (btl_entityptr_obrom), Y
    STA btlmag_defender_hpmax+1
    
                                    ; Y = en_numhitsmult
    LDA (btl_entityptr_ibram), Y    ; hit multiplier from RAM stats
    STA btlmag_defender_numhitsmult
    
    INY                             ; Y = en_ailments
    LDA (btl_entityptr_ibram), Y    ; ailments from RAM
    STA btlmag_defender_ailments
    
    LDY #ENROMSTAT_CATEGORY
    LDA (btl_entityptr_obrom), Y    ; category from ROM
    STA btlmag_defender_category
    INY                             ; Y = ENROMSTAT_MAGDEF
    LDA (btl_entityptr_obrom), Y    ; magdef from ROM
    STA btlmag_defender_magdef
    
    LDA #$00
    STA btlmag_defender_unknownRawInit0
    
    INY                             ; Y = ENROMSTAT_ELEMWEAK
    LDA (btl_entityptr_obrom), Y    ; elemental weakness from ROM
    STA btlmag_defender_elemweakness
    INY                             ; Y = ENROMSTAT_ELEMRESIST
    LDA (btl_entityptr_obrom), Y    ; elemental resistance from ROM
    STA btlmag_defender_elemresist  ;   BUGGED - loaded from ROM instead of RAM.  Contributes to XFER bug
    
    LDY #en_morale
    LDA (btl_entityptr_ibram), Y    ; morale from RAM
    STA btlmag_defender_morale
    
    LDA (btl_entityptr_obrom), Y    ; Y also = ENROMSTAT_ABSORB
    STA btlmag_defender_absorb      ; Absorb/defense from ROM
    
    INY                             ; Y = en_evade
    LDA (btl_entityptr_ibram), Y    ; Evade from RAM
    STA btlmag_defender_evade
    INY                             ; Y = en_strength
    LDA (btl_entityptr_ibram), Y    ; from RAM
    STA btlmag_defender_strength
    
    ;  Lastly, do the explosion effect and exit
    LDA #$02
    JSR PlayBattleSFX                   ; "cha" sound effect (unnecessary, as DoExplosionEffect does this already)
    JMP DoExplosionEffect
    
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  PrepareEnemyMagAttack  [$B730 :: 0x33740]
;;
;;    This loads a bunch of attacker stats for magic attacks
;;  However, *NONE* of these stats are used in magic calculations, and the
;;  stats themselves are totally inconsistent between which stats get loaded for
;;  enemies and which stats get loaded for players.  It's likely that one of these
;;  is supposed to be character level, and another should probably be intelligence.
;;
;;    Apart from that, the only one that makes sense is ailments (since that could be
;;  used to check to see if they're muted) -- but this ailment load is never used, and instead
;;  the stat is loaded from normal stats to check if the caster is muted.
;;
;;    So yeah, this routine is entirely pointless.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PrepareEnemyMagAttack:
    LDA btl_attacker
    JSR PrepEntityPtr_Enemy
    
    LDY #en_ailments
    LDA (btl_entityptr_ibram), Y
    STA btlmag_attacker_unk686C     ; ailments -- makes sense
    
    LDY #en_gp+1                    ; ?? high byte of GP?
    LDA (btl_entityptr_ibram), Y
    STA btlmag_attacker_unk6879
    
    LDY #en_strength
    LDA (btl_entityptr_ibram), Y
    STA btlmag_attacker_unk6884
    
    INY                             ; en_ai  ??
    LDA (btl_entityptr_ibram), Y
    STA btlmag_attacker_unk6875
    
    LDA #$00
    STA btlmag_attacker_unk6883
    
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_SaveEnemyDefenderStats  [$B757 :: 0x33767]
;;
;;    Saved stats that the spell effect may have changed back to enemy stat RAM.
;;
;;  BUGGED:
;;    This routine does not save 'btlmag_defender_elemresist' anywhere, and therefore
;;  spells which change enemy resistence (like XFER, AFIR, etc) have no effect when cast
;;  on enemies.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_SaveEnemyDefenderStats:
    LDA btl_defender                ; get defender slot index
    JSR PrepEntityPtr_Enemy         ; use it to get a pointer to their stats in RAM
    
    LDA btlmag_defender_hp          ; update their HP
    LDY #en_hp
    STA (btl_entityptr_ibram), Y
    LDA btlmag_defender_hp+1
    INY
    STA (btl_entityptr_ibram), Y
                                    ; their absorb/defense
    INY                             ; en_defense
    LDA btlmag_defender_absorb
    STA (btl_entityptr_ibram), Y
    
    LDA btlmag_defender_numhitsmult ; their hit multiplier
    INY                             ; en_numhitsmult
    STA (btl_entityptr_ibram), Y
    
    LDA btlmag_defender_ailments    ; their ailments
    INY                             ; en_ailments
    STA (btl_entityptr_ibram), Y
    
    LDA btlmag_defender_morale      ; their morale
    LDY #en_morale
    STA (btl_entityptr_ibram), Y
    
    LDA btlmag_defender_evade       ; evade
    INY                             ; en_evade
    STA (btl_entityptr_ibram), Y
    
    LDA btlmag_defender_strength    ; attack power
    INY                             ; en_strength
    STA (btl_entityptr_ibram), Y
    
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_SavePlayerDefenderStats  [$B790 :: 0x337A0]
;;
;;    Saved stats that the spell effect may have changed back to IB player stats
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_SavePlayerDefenderStats:
    LDA btl_defender
    JSR PrepEntityPtr_Player
    
    ;; Update OB stats (WHY!?!!??!?!?!??  This stuff is in IB!!!  Why is it using OB HP and ailments?!?!
    ;;  WTF GAME YOU DON'T MAKE ANY SENSE WHY DID YOU PUT HP/AILMENTS IN IB IF YOU'RE NEVER GOING TO USE THEM!
    LDA btlmag_defender_ailments
    LDY #ch_ailments - ch_stats
    STA (btl_entityptr_obrom), Y    ; ailments
    
    LDA btlmag_defender_magdef      ; magdef??  This is weird because not only does no magic actually change
    LDY #ch_magdef - ch_stats       ;  the target's magdef, but also because this is modifying *OB* stats!!  Meaning
    STA (btl_entityptr_obrom), Y    ;  that the change would be PERMANENT.  You could argue that this is BUGGED, but
                                    ;  really since this value is never changed, it ends up being just a waste of space.
    
    LDA btlmag_defender_hp          ; HP
    LDY #ch_curhp - ch_stats
    STA (btl_entityptr_obrom), Y
    LDA btlmag_defender_hp+1
    INY
    STA (btl_entityptr_obrom), Y
    
    ;; Update IB stats
    LDA btlmag_defender_elemresist  ; elemental resistence
    LDY #btlch_elemresist
    STA (btl_entityptr_ibram), Y
    
    LDA btlmag_defender_numhitsmult ; hit multiplier
    INY ;btlch_numhitsmult
    STA (btl_entityptr_ibram), Y
    
    LDA btlmag_defender_absorb      ; absorb/defense
    LDY #btlch_absorb
    STA (btl_entityptr_ibram), Y
    
    LDY #btlch_evade                ; evade
    LDA btlmag_defender_evade
    STA (btl_entityptr_ibram), Y
    
    ;; BUGGED
    ;  This routine does not save btlmag_defender_strength, which means TMPR/SABR
    ;  will not work when cast on players!
    
    RTS
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_PerformSpellEffect  [$B7CD :: 0x337DD]
;;
;;    Actually do the effect of the spell!
;;
;;  output:  'btlmag_fakeout_xxx' vars (see Battle_DoTurn for an explanation)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_PerformSpellEffect:
    LDA btlmag_effect               ; Get this spell's effect ID
    ASL A                           ; x2 to use as index
    TAX
    LDA @jumptable_MagicEffect, X
    STA btltmp+6
    LDA @jumptable_MagicEffect+1, X
    STA btltmp+7                    ; pointer to spell logic in btltmp+6
    
    LDA btlmag_defender_ailments    ; backup defender's original ailments
    STA btlmag_ailment_orig
    
    LDA #0
    STA btlmag_spellconnected       ; initialize spell connected (have it miss by default)
    
    JSR @DoMagicEffect              ; Do the actual effect!
    JSR BtlMag_PrintMagicMessage    ; print appropriate battle message for this spell effect
    
    LDA #MATHBUF_MAGDEFENDERHP      ; if the spell reduced their HP to or below 0, add the 'DEAD' ailment
    JSR MathBuf_NZ
    BEQ @AddDeadAilment
    BMI @AddDeadAilment
    BPL :+                          ; HP > 0, so jump ahead to HandleAilmentChanges
    
  @AddDeadAilment:
      LDA btlmag_defender_ailments
      ORA #AIL_DEAD
      STA btlmag_defender_ailments
      
  : JMP BtlMag_HandleAilmentChanges ; Handle changes to defender's ailments... and exit!
    
  @DoMagicEffect:
    CLC
    JMP (btltmp+6)      ; call the routine from our jump table

    @jumptable_MagicEffect:
        .WORD Battle_ShowIneffective        ; Spell has no in-battle effect
        .WORD BtlMag_Effect_Damage
        .WORD BtlMag_Effect_DamageUndead
        .WORD BtlMag_Effect_InflictAilment
        .WORD BtlMag_Effect_Slow
        .WORD BtlMag_Effect_LowerMorale
        .WORD BtlMag_Effect_RecoverHP
        .WORD BtlMag_Effect_RecoverHP       ; not used / duplicated (effect $07)
        .WORD BtlMag_Effect_CureAilment
        .WORD BtlMag_Effect_AbsorbUp
        .WORD BtlMag_Effect_ElemResist
        .WORD BtlMag_Effect_AttackUp        ; not actually used by any spells in the original game
        .WORD BtlMag_Effect_Fast
        .WORD BtlMag_Effect_AttackUp2
        .WORD BtlMag_Effect_EvadeDown
        .WORD BtlMag_Effect_CureAll
        .WORD BtlMag_Effect_EvadeUp
        .WORD BtlMag_Effect_RemoveResist
        .WORD BtlMag_Effect_InflictAilment2
        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_PrepHitAndDamage  [$B82D :: 0x3383D]
;;
;;    Prepares hit rate and damage for a spell.  Specifically it does the following:
;;
;;  hitrate   += spell hit rate
;;  hitrate   -= defender's magdef
;;  damage     = rand[ damage, damage*2 ]
;;  magrandhit = rand[ 0, 200 ]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_PrepHitAndDamage:
    LDA #MATHBUF_HITCHANCE
    LDX btlmag_hitrate
    JSR MathBuf_Add             ; math_hitchance += spell's base hit rate
    
    LDA #MATHBUF_HITCHANCE
    LDX btlmag_defender_magdef
    JSR MathBuf_Sub             ; math_hitchance -= defender's magdef
    
    LDA #0
    LDX math_basedamage
    JSR RandAX
    TAX                         ; X = rand[ 0, spelldamage ]
    
    LDA #MATHBUF_BASEDAMAGE
    JSR MathBuf_Add             ; math_basedamage = rand[ spelldamage, spelldamage*2 ]
    
    JSR Random_0_200
    JMP WriteAToMagRandHit      ; math_magrandhit = rand[ 0, 200 ]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_CalcElemHitChance  [$B851 :: 0x33861]
;;
;;    Loads base hit chance and adds elemental bonus/penalty.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_CalcElemHitChance:
    JSR BtlMag_LoadBaseHitChance        ; load base hit chance
    
    LDA btlmag_element
    AND btlmag_defender_elemresist
    BEQ :+                              ; if defender resists
      LDA #0                            ; ... reset hit chance to zero
      STA math_hitchance
      STA math_hitchance+1
      
  : LDA btlmag_element                  
    AND btlmag_defender_elemweakness
    BEQ BtlMag_LoadBaseHitChance_RTS    ; (RTS)
    LDA #0                              ; if defender is weak to element
    LDX #40                             ;   add +40 to hit chance
    JMP MathBuf_Add
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_LoadBaseHitChance  [$B873 :: 0x33883]
;;
;;    Loads the base hit chance for a spell in the math buffer.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_LoadBaseHitChance:
    LDA #<148               ; base hit chance of 148
    STA math_hitchance
    LDA #>148
    STA math_hitchance+1
    
BtlMag_LoadBaseHitChance_RTS:
    RTS
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_DidSpellConnect  [$B87E :: 0x3388E]
;;
;;    Sets C if a spell connects with its target, or clears C if it misses
;;
;;  input:  math_hitchance  = chance for the spell to connect
;;          math_magrandhit = random number between 0,200
;;
;;  output: C = set if spell connected
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_DidSpellConnect:
    LDY #MATHBUF_HITCHANCE      ; compare hit chance
    LDX #MATHBUF_MAGRANDHIT     ; to mag rand hit
    JMP MathBuf_Compare

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_MarkSpellConnected  [$B885 :: 0x33895]
;;
;;    Indicates that the spell has connected with its target
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_MarkSpellConnected:
    LDA #1
    STA btlmag_spellconnected       ; set spellconnected var to nonzero to indicate it connected
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_ZeroHitChance  [$B88B :: 0x3389B]
;;
;;    Zero the spell's hit chance.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_ZeroHitChance:
    LDA #0
    STA math_hitchance
    STA math_hitchance+1
    RTS
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Battle_ShowIneffective  [$B894 :: 0x338A4]
;;
;;    Shows the "Ineffective" battle message.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Battle_ShowIneffective:
    LDA #ALTBTLMSG_INEFFECTIVE
    JMP ShowAltBattleMessage

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_Effect_Damage  [$B899 :: 0x338A9]
;;
;;    Routine for in-battle spell logic:   Do damage.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_Effect_Damage:
    JSR BtlMag_MarkSpellConnected           ; damage spells always connect
    JSR BtlMag_LoadBaseHitChance            ; load base hit chance (since damage always hits, this becomes more of a "critical" chance)
    JSR PutEffectivityInDamageMathBuf       ; Load spell effectivity into 'damage' math buffer
    
    LDA btlmag_element
    AND btlmag_defender_elemresist          ; see if the defender resists this element
    BEQ :+                                  ; if they do...
      JSR BtlMag_ZeroHitChance              ; ... zero the hit/crit chance
      LSR math_basedamage+1                 ; and halve the damage
      ROR math_basedamage
      
  : LDA btlmag_element
    AND btlmag_defender_elemweakness        ; see if they are weak to the element
    BEQ :+                                  ; if yes...
      LDA #MATHBUF_HITCHANCE
      LDX #40
      JSR MathBuf_Add                       ; crit bonus of +40
      
                                            ; damage *= 1.5
      LDX #MATHBUF_BASEDAMAGE               ;   copy 1x damage into temp buffer
      LDY #$02                              ;  (use math buf 2 as a temp buffer)
      JSR MathBuf_CopyXToY
      
      LSR math_basedamage+1                 ;   damage *= 0.5
      ROR math_basedamage
      
      LDA #MATHBUF_BASEDAMAGE               ;   then add the backup back into the damage
      LDX #$02                              ;   resulting in 1.5 damage
      LDY #MATHBUF_BASEDAMAGE
      JSR MathBuf_Add16
      
    ; Once damage is set up...
  : JSR BtlMag_PrepHitAndDamage             ; Get hit/damage stuff into math buffers
  ; JMP BtlMag_ApplyDamage                  ; <- Flow into

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_ApplyDamage  [$B8DB :: 0x338EB]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_ApplyDamage:
    LDA math_magrandhit
    CMP #200
    BEQ :+                          ; if the hit/crit roll was 200, this is NOT a crit
      JSR BtlMag_DidSpellConnect    ; See if the spell connected as a critical
      BCC :+                        ; if yes...
        ASL math_basedamage         ; ... do 2x damage
        ROL math_basedamage+1
        
  : LDA #MATHBUF_MAGDEFENDERHP
    LDX #MATHBUF_MAGDEFENDERHP
    LDY #MATHBUF_BASEDAMAGE
    JSR MathBuf_Sub16               ; HP -= damage
    JMP DrawDamageCombatBox         ; Then draw the damage combat box and exit.
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  PutEffectivityInDamageMathBuf  [$B8F9 :: 0x33909]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PutEffectivityInDamageMathBuf:
    LDA btlmag_effectivity          ; move effectivity into the math buffer!
    STA math_basedamage
    LDA #0
    STA math_basedamage+1
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_Effect_DamageUndead  [$B905 :: 0x33915]
;;
;;    Routine for in-battle spell logic:   Damage undead.
;;
;;    Note that this routine does NOT check elemental resistence/weakness.  Therefore, all
;;  damage undead spells are treated as non-elemental even if they are assigned an element.
;;  One could argue this is BUGGED -- although HARM spells are non-elemental in the game
;;  anyway so it doesn't really matter.
;;
;;    An easy fix for this would be to check the category first, and simply jump to the normal
;;  BtlMag_Effect_Damage if they are undead... instead of jumping to BtlMag_ApplyDamage
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_Effect_DamageUndead:
    JSR BtlMag_LoadBaseHitChance        ; Load base hit/crit chance
    JSR PutEffectivityInDamageMathBuf   ; load up base damage
    JSR BtlMag_PrepHitAndDamage         ; crit rate, randomize damage
    
    LDA btlmag_defender_category        ; Check defender category to see if they are undead...
    AND #CATEGORY_UNDEAD
    BNE :+                              ; if not...
      LDA #0
      STA math_basedamage
      STA math_basedamage+1             ; zero the amount of damage we're doing (is this necessary?)
      
      LDA #0
      STA btlmag_spellconnected         ; indicate that the spell did not connect (unnecessary, as this was initialized to zero)
      RTS                               ; and exit.
      
    ; Otherwise, if defender is undead..
  : JSR BtlMag_MarkSpellConnected       ; Mark the spell as connected
    JMP BtlMag_ApplyDamage              ; And do the actual damage!
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_DoStatPrep  [$B929 :: 0x33939]
;;
;;    Prep hit rate and damage for a spell.  Factors elemental resistence into hit rate.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_DoStatPrep:
    JSR BtlMag_CalcElemHitChance        ; apply elemental bonus/penalty to spell hit chance
    JMP BtlMag_PrepHitAndDamage

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_Effect_InflictAilment  [$B92F :: 0x3393F]
;;
;;    Routine for in-battle spell logic:   Inflict ailment.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_Effect_InflictAilment:
    JSR BtlMag_DoStatPrep
    LDA math_magrandhit             ; get random hit roll
    CMP #200                        ; if rolled an even 200, skip ahead to a 'miss'
    BEQ @Miss
      JSR BtlMag_DidSpellConnect
      BCC @Miss
        JSR BtlMag_ApplyAilments
        JMP BtlMag_MarkSpellConnected
  
  @Miss:
    LDA #0            
    STA btlmag_spellconnected       ; indicate that the spell did not connect (unnecessary, as this was initialized to zero)
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_ApplyAilments  [$B94A :: 0x3395A]
;;
;;    Applies the magic's ailments to the defender.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_ApplyAilments:
    LDA btlmag_effectivity          ; effectivity byte is the ailment(s) to add
    PHA                             ; back it up
    AND btlmag_defender_ailments    ; See if defender has this ailment (or really, any of these ailment) already
    BEQ :+                          ;   If yes...
      JSR Battle_ShowIneffective    ;   ... show "Ineffective" message
      
  : PLA                             ; restore ailment byte
    ORA btlmag_defender_ailments    ; add to existing ailments
    STA btlmag_defender_ailments
    RTS                             ; and exit
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_Effect_Slow  [$B95E :: 0x3396E]
;;
;;    'Slow' effect reduces the defender's hit mulitplier, which negates 'FAST'
;;  if they are fasted, or reduces their maximum hit count to 1 otherwise.
;;
;;    Note you could argue that this spell is BUGGED, since if the target is already slowed, this spell
;;  will have no effect, but will still visually indicate that it worked.  It would be more appropriate
;;  to print "ineffective" to show that it doesn't stack.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_Effect_Slow:
    JSR BtlMag_DoStatPrep           ; Load up hit chance w/ element applied
    LDA math_magrandhit
    CMP #200                        ; hit roll of 200 = guaranteed miss
    BEQ @Done
    JSR BtlMag_DidSpellConnect      ; See if they connected.  If not...
    BCC @Done                       ;  ... miss
    
    JSR BtlMag_MarkSpellConnected   ; Otherwise, mark that this connected
    DEC btlmag_defender_numhitsmult ; Decrease their hit multiplier
    BPL @Done                       ;   if it wrapped...
    INC btlmag_defender_numhitsmult ;   ... INC it to undo it.  (This is where the 'bug' is, btlmag_spellconnected should be zero'd here)
  
  @Done:
  ; RTS                             ; <- Flow into

;; Common RTS that is branched to by various surrounding code
BtlMag_Effect_Slow_RTS:
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_Effect_LowerMorale  [$B979 :: 0x33989]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_Effect_LowerMorale:
    JSR BtlMag_DoStatPrep           ; Load up hit chance w/ element applied
    LDA math_magrandhit
    CMP #200                        ; hit roll of 200 = automatic miss
    BEQ BtlMag_Effect_Slow_RTS
    
    JSR BtlMag_DidSpellConnect      ; see if the spell connected
    BCC BtlMag_Effect_CureAil_RTS   ; if not, jump ahead to an RTS
    
    LDA btlmag_defender_morale      ; subtract spell effectivity from defender morale
    SEC
    SBC btlmag_effectivity
    BCS :+
        LDA #0                      ; cap at 0 (don't let it wrap)
  : STA btlmag_defender_morale
    JMP BtlMag_MarkSpellConnected   ; then mark the spell as connected and exit.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_Effect_RecoverHP  [$B999 :: 0x339A9]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_Effect_RecoverHP:
    JSR BtlMag_MarkSpellConnected   ; HP recovery always connects (doesn't it miss if dead?)
    
    LDA btlmag_defender_ailments    ; Check defender ailment
    AND #AIL_DEAD
    BNE BtlMag_Effect_Slow_RTS      ; If they're dead, do nothing
    
    LDA btlmag_effectivity          ; This block just does:  X = rand[ effectivity, effectivity*2 ], capping at 255
    TAX
    LDA #0
    JSR RandAX                      ;   random between [0,effectivity]
    CLC
    ADC btlmag_effectivity          ;   random between [effectivity, effectivity*2]
    BCC :+
      LDA #$FF                      ;   (cap at 255)
  : TAX
  
    LDA #MATHBUF_MAGDEFENDERHP      ; Add X to defender's HP
    JSR MathBuf_Add
    
    LDX #MATHBUF_MAGDEFENDERMAXHP
    LDY #MATHBUF_MAGDEFENDERHP
    JSR MathBuf_Compare             ; Compare current HP with max HP
    BCC BtlMag_Effect_CureAil_RTS   ; if hp < maxhp, just exit
    JMP BtlMag_SetHPToMax           ; otherwise, set HP to the maximum   (strangely, this could just flow into it, but it actually JMPs)
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_SetHPToMax  [$B9C6 :: 0x339D6]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_SetHPToMax:
    LDX #MATHBUF_MAGDEFENDERMAXHP   ; just copy the defender's max HP over to their actual HP
    LDY #MATHBUF_MAGDEFENDERHP
    JMP MathBuf_CopyXToY
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_Effect_CureAilment  [$B9CD :: 0x339DD]
;;
;;    Note that strangely this WILL cure the 'death' ailment, but it does not set the HP
;;  to a nonzero value, so they'll just die again.  It also will cure the STONE ailment...
;;  yet strangely SOFT is not usable in battle (why not?)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_Effect_CureAilment:
    LDA btlmag_defender_ailments    ; Get the defender's ailments
    AND btlmag_effectivity          ; See if they have any of the ailments we're trying to cure
    BEQ BtlMag_Effect_CureAil_RTS   ;  If not, no effect, just exit
    
    LDA btlmag_effectivity          ; Otherwise, cure ailment bits
    EOR #$FF
    AND btlmag_defender_ailments
    STA btlmag_defender_ailments
    JSR BtlMag_MarkSpellConnected   ; and mark the spell as connected
  ; RTS                             ; <- flow into
  
    
;; Common RTS that is branched to by various surrounding code
BtlMag_Effect_CureAil_RTS:
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_Effect_AbsorbUp  [$B9E4 :: 0x339F4]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_Effect_AbsorbUp:
    LDA btlmag_defender_absorb      ; get defender absorb
    ADC btlmag_effectivity          ; add effectivity to it
    BCC :+
      LDA #$FF                      ; (cap at 255)
  : STA btlmag_defender_absorb      ; that's our new absorb!
    JMP BtlMag_MarkSpellConnected   ; This spell always connects.
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_Effect_ElemResist  [$B9F4 :: 0x33A04]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_Effect_ElemResist:
    LDA btlmag_effectivity          ; pretty straight forward...
    ORA btlmag_defender_elemresist
    STA btlmag_defender_elemresist
    JMP BtlMag_MarkSpellConnected
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_Effect_AttackUp  [$BA00 :: 0x33A10]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_Effect_AttackUp:
    LDA btlmag_defender_strength    ; identical to BtlMag_Effect_AbsorbUp, but modify attack power instead
    ADC btlmag_effectivity
    BCC :+
      LDA #$FF
  : STA btlmag_defender_strength
    JMP BtlMag_MarkSpellConnected
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_Effect_Fast  [$BA10 :: 0x33A20]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_Effect_Fast:
    JSR BtlMag_MarkSpellConnected   ; Always connects (except when maxed -- this will be undone then)
    INC btlmag_defender_numhitsmult ; Increase hit multiplier
    
    LDA btlmag_defender_numhitsmult
    CMP #3
    BCC :+                          ; if hit multiplier is >= 3
      LDA #0
      STA btlmag_spellconnected     ; undo the connect (this spell has no effect)
      LDA #$02                      ; and max the multiplier at 2
      
  : STA btlmag_defender_numhitsmult
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_Effect_AttackUp2  [$BA28 :: 0x33A38]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_Effect_AttackUp2:
    LDA $6884                       ; BUGGED - this is *probably* supposed to be using the spell hit-rate value
    ADC btlmag_hitrate              ;   as a HIT bonus and then the effectivity as a DAMAGE bonus, but defender's
    BCC :+                          ;   hit rate is not loaded into memory, so this end up adding it to
      LDA #$FF                      ;   some other part of mem.  Note that this doesn't matter in the original
  : STA $6884                       ;   game, as TMPR and SABR both have 0 for the spell's hit rate.
  
            ; Code here is a duplicate of BtlMag_Effect_AttackUp.  You could just JMP there and save
            ;   all this space.
    LDA btlmag_effectivity          ; Add the spell effectivity to the defender's attack power.
    CLC
    ADC btlmag_defender_strength
    BCC :+
      LDA #$FF                      ; cap at 255
  : STA btlmag_defender_strength
    JMP BtlMag_MarkSpellConnected   ; Indicate spell connected.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_Effect_EvadeDown  [$BA46 :: 0x33A56]
;;
;;      This spell effect is BUGGED and will always miss its target.  It's actually a very easy
;;  fix -- all you have to do is change the offending JMP to a BEQ
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_Effect_EvadeDown:
    JSR BtlMag_DoStatPrep           ; prep hit rate & apply elemental resistance and stuff
    LDA math_magrandhit
    CMP #200                        ; hit roll of 200 = automatic miss
    JMP @Done                       ; BUGGED -- this should be BEQ, not JMP.  Since it always jumps,
                                    ;   this means the spell never has any effect and always misses!
        ; due to bug, this code is never reached, but it works as you'd expect:
    JSR BtlMag_DidSpellConnect      ; see if the spell connected
    BCC @Done                       ; if not, exit
    LDA btlmag_defender_evade       ; if yes, subtract effectivity from defender's evade
    SEC
    SBC btlmag_effectivity
    BCS :+
      LDA #0                        ; clip at 0
  : STA btlmag_defender_evade
    JSR BtlMag_MarkSpellConnected   ; then mark that the spell connected
  @Done:
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_Effect_CureAll  [$BA68 :: 0x33A78]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_Effect_CureAll:
    JSR BtlMag_SetHPToMax           ; Fill HP to max
    LDA #0                          ; and zero ailments, curing all of them
    STA btlmag_defender_ailments
    JMP BtlMag_MarkSpellConnected   ; mark as connected

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_Effect_EvadeUp  [$BA73 :: 0x33A83]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_Effect_EvadeUp:
    LDA btlmag_defender_evade           ; Same as BtlMag_Effect_AttackUp, but increases evade instead
    ADC btlmag_effectivity
    BCC :+
      LDA #$FF
  : STA btlmag_defender_evade
    JMP BtlMag_MarkSpellConnected
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Random_0_200  [$BA83 :: 0x33A93]
;;
;;  Gets a random number between [0,200]
;;
;;    This is used for "hit rolls".  The attack/magic is considered a hit if this
;;  number is less than a calculated hit rate.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Random_0_200:
    LDA #0
    LDX #200
    JSR RandAX
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  WriteAToMagRandHit  [$BA8B :: 0x33A9B]
;;
;;  Writes A to mag rand hit entry in math buffer.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

WriteAToMagRandHit:
    STA math_magrandhit
    LDA #0
    STA math_magrandhit+1
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_Effect_RemoveResist  [$BA94 :: 0x33AA4]
;;
;;    This routine does its own version of elemental resistance checking to see about the elemental
;;  hit bonus -- and really, there's [little] reason for it.  It could be SERIOUSLY trimmed by calling
;;  BtlMag_DoStatPrep.
;;
;;    The only difference I can see is that this is doing is when the target is both weak AND resistent
;;  to the element.  In that case, this code will result in a hit rate of 188, whereas DoStatPrep will
;;  result in a hit rate of 40.
;;
;;    It's weird that this does elemental checks at all -- I mean... the whole point of this is to remove
;;  elemental resistance, so what good is it if it is thwarted by elemental resistence?  Fortunately,
;;  the only spell to use it (XFER) is nonelemental so it doesn't matter.
;;
;;  XFER bug explained:
;;      XFER removes elemental resistence.  It works properly when cast on players, but not when cast on
;;  enemies.  On enemies it has no effect.
;; 
;;      The bug is not in this code -- the below routine works fine.  It modifies the target's elemental
;;  resistence correctly.  The problem is, for enemies, this value is taken from their ROM stats
;;  and not their RAM stats.  So after the spell is resolved, this new elemental resistence stat is not
;;  used and the enemy stat remains unchanged.
;;
;;      The problem is in BtlMag_LoadEnemyDefenderStats, BtlMag_SaveEnemyDefenderStats, and
;;  PlayerAttackEnemy_Physical.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_Effect_RemoveResist:
    JSR BtlMag_LoadBaseHitChance        ; load base hit chance (148)
    LDA btlmag_element
    AND btlmag_defender_elemresist
    BEQ :+
      JSR BtlMag_ZeroHitChance          ; if defender resists, hit chance = 0
  : LDA btlmag_element
    AND btlmag_defender_elemweakness
    BEQ :+                              ; if defender is weak,
      LDA #188                          ; hit chance = 188
      STA math_hitchance                ;   (148 base + 40 bonus)
      
  : LDA #MATHBUF_HITCHANCE
    LDX btlmag_hitrate
    JSR MathBuf_Add                     ; add spell's hit rate to chance
    LDA #MATHBUF_HITCHANCE
    LDX btlmag_defender_magdef
    JSR MathBuf_Sub                     ; subtract defender's magdef from hit chance
    
    JSR Random_0_200                    ; get hit roll
    CMP #200                            ; 200 = always miss
    BEQ BtlMag_Effect_RemRst_RTS
    
    JSR WriteAToMagRandHit              ; record hit roll
    JSR BtlMag_DidSpellConnect          ; check to see if spell connected
    BCC BtlMag_Effect_RemRst_RTS        ; if it did...
    LDA #0
    STA btlmag_defender_elemresist      ; clear all defender's elemental resistance
    JSR BtlMag_MarkSpellConnected       ; and mark that the spell connected.
    
  ; RTS                                 ; <- flow into

  
;; Common RTS that is branched to by various surrounding code
BtlMag_Effect_RemRst_RTS:
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_Effect_InflictAilment2  [$BAD7 :: 0x33AE7]
;;
;;  Inflicts the desired ailment.  There is NO randomness involved.  Whether or not it
;;  hits depends solely on remaining HP and elemental resistence.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_Effect_InflictAilment2:
    LDA btlmag_element                  ; If the defender resists the elemenu
    AND btlmag_defender_elemresist
    BNE BtlMag_Effect_RemRst_RTS        ; then they are 100% immune.  Branch to RTS
    
    LDA #<300
    STA math_magrandhit
    LDA #>300
    STA math_magrandhit+1
    LDY #MATHBUF_MAGRANDHIT
    LDX #MATHBUF_MAGDEFENDERHP
    JSR MathBuf_Compare                 ; see if defender's HP is under 300
    
    BCC BtlMag_Effect_RemRst_RTS        ; if not, FAIL/exit
    
    JSR BtlMag_ApplyAilments            ; otherwise, apply the ailments
    JMP BtlMag_MarkSpellConnected       ; and mark as connected, and exit


    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BtlMag_HandleAilmentChanges  [$BAF8 :: 0x33B08]
;;
;;    After a spell takes effect, this checks the target's ailments before and after
;;  the spell and handles changes.  This includes:
;;
;;  - Printing battle messages to show ailments that have been added/removed
;;  - Removing enemies from the battle if they've been slain
;;  - Clear enemy graphics if they've been slain
;;  - Remove player's action from the command buffer if they've been immobilized.
;;
;;    About the only thing it DOESN'T do that you might expect it to is set a player's HP
;;  to zero if they've been killed.
;;
;;    Note that this routine also handles the 'fakeout' necessary for faking the output
;;  of mulitple targets.  See Battle_DoTurn for an explanation.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BtlMag_HandleAilmentChanges:
    LDX #$08                            ; loop 8 times -- once for each ailment
    LDA btlmag_defender_ailments
    STA btltmp+7                        ; store their new ailments in btltmp+7
    
  @AilmentLoop:
    LDA #0                              ; shift out low orig ailment bit
    STA btltmp+6                        ; shift it into btltmp+6
    LSR btlmag_ailment_orig
    ROL btltmp+6                        ; results in 0 or 1 if the defender originally had this ailment
    
    LSR btltmp+7                        ; shift low bit of NEW ailment into A
    ROL A
    
    CMP btltmp+6                        ; compare to see if the state of this ailment changed.
    
    BNE @AilmentChanged
   
  @NextAilment:
    DEX                                 ; dec loop counter, and loop until we're done
    BNE @AilmentLoop
    BEQ @DoneWithAilMessages


  @AilmentChanged:
    CMP #0                          ; see if the ailment is added or if it was cured
    BEQ @AilmentCured               ; jump ahead if cured
    
    ; Otherwise, ailment has been inflicted
    CPX #$08                        ; is this the first ailment?  (first ailment is DEATH)
    BNE @AilmentAdded               ; if not death, jump ahead
    
    ; Reaches here if the target has died
    LDA #ALTBTLMSG_TERMINATED                       ; use "Terminated" message
    LDX btl_defender                                ; is the defender a player?
    BPL :+                                          ; if yes...
      CLC                                           ; ... switch to "Slain" message instead
      ADC #(ALTBTLMSG_SLAIN - ALTBTLMSG_TERMINATED) ;  (why doesn't this just LDA?  Why the weird addition?)
  : JSR ShowAltBattleMessage                        ; Show Terminated/Slain
    JMP @DoneWithAilMessages                        ; And exit this loop (no point in checking other ailments)
    
  @AilmentAdded:
    TXA                             ; use loop counter to figure out which message to draw
    CLC
    ADC #ALTBTLMSG_CONFUSED-1       ; -1 because loop counter is 1 based
    
  @PrintMessageAndNext:
    JSR ShowAltBattleMessage        ; Show the message
    JMP @NextAilment                ; And continue looping through ailments.
    
  @AilmentCured:
    TXA                             ; use loop counter to get message
    CLC
    ADC #ALTBTLMSG_CURED_1-1        ; -1 since loop counter is 1 based
    BNE @PrintMessageAndNext        ; print it and keep looping

    
    ;; Once all messages for inflicted/cured ailments have been printed...
  @DoneWithAilMessages:
    LDX btl_defender                ; get defender
    BMI @CheckPlayerState           ; if it's a player, jump ahead to player logic
    
    ; Otherwise, defender is an enemy
    LDA btlmag_defender_ailments    ; See if the enemy has been killed/stoned
    AND #(AIL_DEAD | AIL_STONE)
    BEQ :+                          ; if yes....
      JSR EraseEnemyGraphic         ;  Erase their graphic
      LDX btl_defender
      JSR ClearEnemyID              ;  Remove them from the battle
      
      LDA btlmag_defender_ailments
      STA btlmag_fakeout_ailments   ; record their ailments to fakeout output to show that
      LDA btl_defender              ;   at least one enemy has been killed
      STA btlmag_fakeout_defindex
      
  : LDA #$00
    STA btlmag_fakeout_defplayer
    RTS
    
  @CheckPlayerState:
    LDA btlmag_defender_ailments                        ; see if the player has been rendered immobile
    AND #(AIL_DEAD | AIL_STONE | AIL_STUN | AIL_SLEEP)
    BEQ :+                          ; if yes...
      STA btlmag_fakeout_ailments   ; record fakout ailments to show that at least one player has
      LDA btl_defender              ;   been removed from combat
      ORA #$80
      STA btlmag_fakeout_defindex
      
      LDA btl_defender
      ASL A
      ASL A
      TAY
      LDA #0
      STA btl_charcmdbuf, Y         ; erase this character's battle command (can't do anything if they're immobile)
      
  : LDA #$01
    STA btlmag_fakeout_defplayer
    RTS
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  GetEnemyStatPtr  [$BB86 :: 0x33B96]
;;
;;  input:
;;      A = desired enemy index  (0-8)
;;
;;  output:
;;      XA = pointer to that enemy's stats in RAM
;;
;;  Again this is a 100% identical copy of a routine in bank B
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
GetEnemyStatPtr:
    STY $6C87               ; back-up Y
    LDX #$14                ; multiply enemy index by $14  (number of bytes per enemy)
    JSR MultiplyXA
    
    CLC                     ; then add btl_enemystats to the result
    ADC #<btl_enemystats
    PHA
    TXA
    ADC #>btl_enemystats
    TAX
    PLA
    
    LDY $6C87               ; restore Y
    RTS


    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Inc Y routines  [$BB9B :: 0x33BAB]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

IncYBy8:                ; BB9B - this is never used
    INY
    INY
    INY
    INY
    
IncYBy4:                ; BB9F
    INY
    INY
    INY
    INY
    
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  RebuildEnemyRoster  [$BBA4 :: 0x33BB4]
;;
;;  Rebuilds the roster to show which enemies are actually in the fight.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

RebuildEnemyRoster:
    LDA #$FF
    LDX #$03
  @ClearLoop:
      STA btl_enemyroster, X
      DEX
      BPL @ClearLoop
      
    INX             ;X=0.  X will be the enemy slot index
    LDY #$00        ;Y=0.  Y will be the roster index
    
  @FillLoop:
      JSR ShouldAddEnemyToRoster    ; should we add this enemy to the roster?
      LDA $6DB1
      BEQ :+                        ; if yes...
        LDA btl_enemyIDs, X         ; put the enemy in the roster
        STA btl_enemyroster, Y
        INY                         ; inc roster index
    : INX
      CPX #$09
      BNE @FillLoop                 ; loop for all 9 enemy slots.
    
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DoesEnemyXExist  [$BBC6 :: 0x33BD6]
;;
;;  input:   X = enemy index
;;  output:  Z = clear if enemy exists, set if enemy slot is empty
;;           A = ID of enemy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
DoesEnemyXExist:
    LDA btl_enemyIDs, X
    CMP #$FF
    RTS
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ShouldAddEnemyToRoster  [$BBCC :: 0x33BDC]
;;
;;  input:       X = enemy slot index
;;  output:  $6DB1 = 0 if enemy should not be added to the roster
;;                   1 if enemy should be added
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ShouldAddEnemyToRoster:
    LDA #$00                ; clear output by default
    STA $6DB1
    
    TXA                     ; backup X,Y
    PHA
    TYA
    PHA
    
    JSR DoesEnemyXExist     ; if this enemy slot is empty
    BEQ @Exit               ;  just exit
    
    LDY #$00                ; otherwise, loop over the existing roster to see if the enemy
  @Loop:                    ; is already in it
      CMP btl_enemyroster, Y
      BEQ @Exit             ; if it already exists, exit
      INY
      CPY #$04              ; loop over all 4 roster entries
      BNE @Loop
    
    ; If the enemy did not exist in the roster, then we should add it! 
    ;  set our output to 1, then exit
    INC $6DB1
    
  @Exit:
    PLA
    TAY
    PLA
    TAX         ; restore Y,X
    RTS
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  EraseEnemyGraphic  [$BBEE :: 0x33BFE]
;;
;;  input:   btl_defender = the target enemy slot to erase
;;
;;    Note, for the Chaos fight, this routine will do nothing.  The disintegration
;;  effect when Chaos is killed is done elsewhere.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

EraseEnemyGraphic:
    INC btl_enemyeffect         ; make enemyeffect nonzero
    JSR DrawEnemyEffect         ; draw the erase effect
    LDA #$00
    STA btl_enemyeffect         ; then zero enemy effect again
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DoExplosionEffect  [$BBFA :: 0x33C0A]
;;
;;    Plays the "cha" sound effect when you attack an enemy, and draws the explosion sound effect
;;  on top of the target enemy.
;;
;;  input:  btl_enemyeffect = assumed to be 0
;;             btl_defender = the target enemy slot on which to draw the effect
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DoExplosionEffect:
    LDA #$02
    JSR PlayBattleSFX           ; the the "cha" sound effect.
  ; JMP DrawEnemyEffect         ; <- then flow into drawing the enemy effect.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DrawEnemyEffect  [$BBFF :: 0x33C0F]
;;
;;  Either draws the explosion effect over an enemy... or erases the enemy.
;;  Depending on the status of btl_enemyeffect.
;;
;;  if btl_enemyeffect = 0, it draws explosion effects
;;  otherwise, it erases the enemy.
;;
;;    I have NO idea why these two different things are combined into the same routine -- there isn't really
;;  any reason for them to be.  Whatever.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawEnemyEffect:
    LDA btl_defender
    PHA
    JSR SwapBtlTmpBytes_L
    LDX btl_battletype
    BNE :+
      JMP DrawEnemyEffect_9Small          ; 9small formation
  : DEX
    BNE :+
      JMP DrawEnemyEffect_4Large          ; 4large formation
  :   DEX
    BNE :+
      JMP DrawEnemyEffect_Mix          ; mix formation
    
    ;; The rest of this is for Fiends/Chaos -- but is the same
    ;;  idea as DrawEnemyEffect_9Small.  See that routine for
    ;;  details -- comments here are sparese.
  : PLA                     ; throw the enemy slot away -- we don't need it since there's only 1 enemy
  
    LDA #$20
    STA explode_min_x
    LDA #$40
    STA explode_min_y
    LDA #$60
    STA explode_max_x
    LDA #$78
    STA explode_max_y
    LDA #$14
    STA explode_count
    JSR DrawExplosions
    
    LDA btl_enemyeffect
    BEQ @Exit               ; exit if we don't want to erase the enemy
    LDA btl_battletype
    CMP #$04
    BEQ @Exit               ; exit if this is a Chaos fight -- we don't erase Chaos normally -- he has a fancy
                            ;  dissolve effect
    
    LDA #$0C
    STA $6D14               ; row counter / loop counter
    
    LDA #<$20C2             ; $6D15,6 = target PPU address
    STA $6D15
    LDA #>$20C2
    STA $6D16
    
  @EraseLoop:
      JSR WaitForVBlank_L           ; Vblank
      LDA $6D16                     ; Set PPU Addr
      STA $2006
      LDA $6D15
      STA $2006
      
      LDA #$00
      JSR WriteAToPPU6Times         ; clear 12 tiles in this row
      JSR WriteAToPPU6Times
      
      JSR BattleUpdatePPU           ; reset scroll, etc
      
      CLC                           ; move PPU addr to next row
      LDA $6D15
      ADC #$20
      STA $6D15
      BCC :+
        INC $6D16
    : JSR BattleUpdateAudio         ; update audio for the frame
      DEC $6D14
      BNE @EraseLoop
      
  @Exit:
    JMP SwapBtlTmpBytes_Local


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  VBlank_SetPPUAddr  [$BC85 :: 0x33C95]
;;
;;  Waits for VBlank, and sets PPU addr to btltmp+A,B
;;  Also makes a point to preseve A
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

VBlank_SetPPUAddr:
    PHA                         ; backup A
    JSR WaitForVBlank_L         ; VBlank
    LDA btltmp+$B               ; set ppu addr
    STA $2006
    LDA btltmp+$A
    STA $2006
    PLA                         ; restore A
    RTS
    
    
    ; Unused code???
    ; BC95 :: 0x33CA5
    PHP
    PHA
    CLC
    LDA #$10
    ADC $9A
    STA $9A
    BCC :+
      INC $9B
  : PLA
    PLP
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DrawExplosions_PreserveX  [$BCA5 :: 0x33CB5]
;;
;;  Also loads btl_enemyeffect into A upon exit
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawExplosions_PreserveX:
    TXA                 ; backup X
    PHA
    JSR DrawExplosions  ; draw explosions
    PLA
    TAX                 ; restore X
    LDA btl_enemyeffect ; load btl_enemyeffect
    RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DrawEnemyEffect_9Small  [$BCB0 :: 0x33CC0]
;;
;;  Either erase the enemy or draw the explosion graphics on it, depending on btl_enemyeffect.
;;
;;  input:             btl_enemyeffect = choose to erase or draw explosions
;;         value pushed to stack and A = enemy slot
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawEnemyEffect_9Small:
    PLA                     ; get enemy slot and back it up again
    PHA                     ;  (backing it up is pointless because it is never used again)
    
    ASL A                   ; *2 to use it for an index for the coord LUT
    TAX                         
    
    LDA lut_ExplosionCoords_9Small, X
    STA explode_min_x
    CLC
    ADC #$18
    STA explode_max_x       ; get min/max X coord
    
    LDA lut_ExplosionCoords_9Small+1, X
    STA explode_min_y
    CLC
    ADC #$18
    STA explode_max_y       ; get min/max Y coord
   
    LDA #$06                ; set the number of explosions for this enemy
    STA explode_count
    
    JSR DrawExplosions_PreserveX        ; Draw the actual explosion
    BEQ __DrawEnemyEffect_9Small_Exit   ; branches if btl_enemyeffect is zero
        
    ; This code only runs if btl_enemyeffect is nonzero (erase the enemy)
    PLA                                     ; get the backup and double it, but never TAX so this is completely pointless
    ASL A                                   ;   because this value is immediately thrown away
    LDA lut_EraseEnemyPPUAddress_9Small, X  ; X is only set correctly because DrawExplosions_PreserveX preserves X
    STA btltmp+$A
    LDA lut_EraseEnemyPPUAddress_9Small+1, X
    STA btltmp+$B
  ; JMP EraseSmallEnemy                     ; <- flow into

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  EraseSmallEnemy  [$BCE2 :: 0x33CF2]
;;
;;   input:  btltmp+$A,B PPU address pointing to enemy graphic
;;
;;  Erases an enemy graphic, then swaps out btltmp bytes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

EraseSmallEnemy:
    LDA #$04                        ; loop counter (clearing 4 rows)
    STA btltmp
    : JSR VBlank_SetPPUAddr         ; set the PPU addr
      LDA #$00
      JSR WriteAToPPU4Times         ; clear 4 tiles
      JSR MoveDown1Row_UpdateAudio  ; finish the frame and prep for next row
      DEC btltmp
      BNE :-                        ; dec loop counter and loop
      
    JMP SwapBtlTmpBytes_Local
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  A weird out of place exit for DrawEnemyEffect_9Small  [$BCF8 :: 0x33D08]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

__DrawEnemyEffect_9Small_Exit:
    PLA
    JMP SwapBtlTmpBytes_Local
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  WriteAToPPUXTimes  [$BCFC :: 0x33D0C]
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

WriteAToPPU6Times:
    JSR WriteAToPPU2Times
    
WriteAToPPU4Times:
    JSR WriteAToPPU2Times
    
WriteAToPPU2Times:
    STA $2007
    STA $2007
    RTS
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  MoveDown1Row_UpdateAudio  [$BD09 :: 0x33D19]
;;
;;    Updates the PPU address stored at btltmp+$A to move down
;;  one row of tiles, and then update audio.
;;
;;  This routine and VBlank_SetPPUAddr are used to bookend in-battle PPU updates.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MoveDown1Row_UpdateAudio:
    JSR BattleUpdatePPU         ; reset scroll and stuffs
    LDA btltmp+$A               ; update soft ppu addr to point to next row
    CLC
    ADC #$20
    STA btltmp+$A
    LDA btltmp+$B
    ADC #$00
    STA btltmp+$B
    JMP BattleUpdateAudio       ; update audio and exit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  lut_ExplosionCoords_9Small  [$BD1C :: 0x33D2C]
;;
;;  The sprite coords to draw explosions for enemies in the 9 small formation

lut_ExplosionCoords_9Small:
  .BYTE $10, $50
  .BYTE $10, $30
  .BYTE $10, $70
  .BYTE $30, $50
  .BYTE $30, $30
  .BYTE $30, $70
  .BYTE $50, $50
  .BYTE $50, $30
  .BYTE $50, $70


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DrawEnemyEffect_4Large  [$BD2E :: 0x33D3E]
;;
;;  Identical to DrawEnemyEffect_9Small but with different constants.
;;  See that routine for details, comments here will be sparse
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawEnemyEffect_4Large:
    PLA
    PHA
    
    ASL A
    TAX
    
    LDA @lut_ExplosionCoords_4Large, X
    STA explode_min_x
    CLC
    ADC #$28
    STA explode_max_x
    
    LDA @lut_ExplosionCoords_4Large+1, X
    STA explode_min_y
    CLC
    ADC #$28
    STA explode_max_y
    
    LDA #$08
    STA explode_count
    
    JSR DrawExplosions_PreserveX
    BEQ @Exit
    
    PLA                         ; again, this is pointless, as the value is throw away.
    ASL A
    LDA lut_EraseEnemyPPUAddress_4Large, X
    STA btltmp+$A
    LDA lut_EraseEnemyPPUAddress_4Large+1, X
    STA btltmp+$B
    
    LDA #$06                        ; 6 rows for large enemies
    STA btltmp
    : JSR VBlank_SetPPUAddr
      LDA #$00
      JSR WriteAToPPU6Times
      JSR MoveDown1Row_UpdateAudio
      DEC btltmp
      BNE :-
    JMP SwapBtlTmpBytes_Local

  @Exit:
    PLA
    JMP SwapBtlTmpBytes_Local

  ; [$BD7A :: 0x33D8A]
  @lut_ExplosionCoords_4Large:
    .BYTE $10, $30
    .BYTE $10, $60
    .BYTE $50, $30
    .BYTE $50, $60

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DrawEnemyEffect_Mix  [$BD82 :: 0x33D92]
;;
;;  Identical to DrawEnemyEffect_9Small but with different constants for the 2Large+6Small mix formation
;;  See that routine for details, comments here will be sparse
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawEnemyEffect_Mix:
    PHA                             ; push slot AGAIN (it's already pushed -- this is pointless)
    CMP #$02                        ; if slot is < 2
    BCS :+
      PLA                           ; undo our pointless push
      JMP DrawEnemyEffect_4Large    ; and jump to the 4Large version -- since the first 2 enemies in this formation
                                    ;   are large and are placed in the same spot as the 4large formation
    
    ; Jumps here if slot >= 2 (one of the small enemies)
  : PLA                         ; undo our pointless push
    SEC
    SBC #$02                    ; subtract 2 to make the index [0-5], which indexes the small enemy slots
    ASL A
    TAX
    
    LDA @lut_ExplosionCoords_6Small, X
    STA explode_min_x
    CLC
    ADC #$18
    STA explode_max_x
    
    LDA @lut_ExplosionCoords_6Small+1, X
    STA explode_min_y
    CLC
    ADC #$18
    STA explode_max_y
    
    LDA #$0F                    ; way more explosions for small enemies in the mix formation than in the 9small formation
    STA explode_count           ;  is this BUGGED?
    
    JSR DrawExplosions          ; doesn't call the Preserve X version -- the one time it would actually make sense to...
    
    PLA                         ; pull the target... ONLY TO THROW IT AWAY!!!! wtf Nasir this makes no sense!
    LDA btl_defender            ; instead, use the defender as the target.
    SEC
    SBC #$02                    ; -2 to get the small index
    ASL A
    TAX
    
    LDA btl_enemyeffect
    BEQ SwapBtlTmpBytes_Local   ; exit unless the enemyeffect is set to erase the enemy
    
    LDA lut_EraseEnemyPPUAddress_Mix_Small, X
    STA btltmp+$A
    LDA lut_EraseEnemyPPUAddress_Mix_Small+1, X
    STA btltmp+$B
    JMP EraseSmallEnemy         ; reuse the EraseSmallEnemy routine used in the 9Small formation
    
    ;; [$BDCC :: 0x33DDC]
  @lut_ExplosionCoords_6Small:
    .BYTE $40, $50
    .BYTE $40, $30
    .BYTE $40, $70
    .BYTE $60, $50
    .BYTE $60, $30
    .BYTE $60, $70
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  SwapBtlTmpBytes_Local  [$BDD8 :: 0x33DE8]
;;
;;    wtf why does this exist?  Why not just call SwapBtlTmpBytes_L directly?
;;  This makes no sense.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SwapBtlTmpBytes_Local:
    JMP SwapBtlTmpBytes_L

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DrawExplosions  [$BDDB :: 0x33DEB]
;;
;;    Draws the explosions that appear on an enemy when you attack them.
;;  This routine takes several frames to complete.
;;
;;  input:
;;    btl_enemyeffect = if nonzero, this routine exits immediately without drawing anything
;;      explode_count = number of explosion graphics to draw (/ 3)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawExplosions:
    LDA btl_enemyeffect         ; Only draw the explosion if enemy effect is set to draw explosions
    BEQ @DrawExplosionSprites
      RTS                       ;  if not, exit immediately
      
  @DrawExplosionSprites:
    LDA #$00
    STA btltmp+2            ; btltmp+2 is a loop counter
    : LDA #$00              ;  loop 3 times, drawing an explosion sprite in each explosion slot
      LDX #$02              ; get a random value, one of:  F4, F8, or FC
      JSR RandAX            ; this will be used as the explosion graphic tile
      ASL A
      ASL A
      CLC
      ADC #$F4
      
      LDX btltmp+2              ; use the loop counter as the slot
      JSR DrawExplosion_Frame   ; draw the graphic and do a frame
      
      INC btltmp+2              ; inc loop counter (and slot index)
      LDA btltmp+2
      CMP #$03
      BNE :-                    ; loop 3 times to fill all slots
      
    DEC explode_count           ; dec the explosion counter
    BNE @DrawExplosionSprites   ;  loop until no more explosions to draw
    
    ; Wipe all the explosion sprites
    LDY #$30                ; 3 slots * 4 sprites per slot * 4 bytes per slot -- loop down counter
    LDA #$FF                ; $FF value to clear
    LDX #$00                ; loop up counter
    
    : STA oam+$D0, X        ; clear oam
      INX
      DEY
      BNE :-
      
    JSR WaitForVBlank_L     ; Do one more frame
    LDA #>oam               ; so we can update sprite data in the PPU
    STA $4014
    JMP BattleUpdateAudio   ; update audio since we did a frame
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DrawExplosion_Frame  [$BE1B :: 0x33E2B]
;;
;;  Draws an explosion/damage sprite, then does a frame updating OAM.
;;
;;  input:
;;     explode min/max vars
;;     A = F4, F8, or FC, indicating which of the 3 graphics to draw
;;     X = 0, 1, or 2, indicating which explosion sprite slot to use
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawExplosion_Frame:
    PHA                         ; backup tile ID
    TXA
    ASL A
    ASL A
    ASL A
    ASL A                       ; get slot * $10 (4 sprites per graphic * 4 bytes per sprite)
    CLC
    ADC #$D0                    ; $D0 is the first oam slot for explosion graphics
    STA btltmp
    LDA #>oam
    STA btltmp+1                ; btltmp now points to dest area in oam to draw the sprite
    
    LDA explode_min_x
    LDX explode_max_x
    JSR RandAX                  ; get a random X coordinate
    LDY #oam_x - oam
    JSR Explosion_Write_0808    ; write X coords to all 4 sprites
    
    LDA explode_min_y
    LDX explode_max_y
    JSR RandAX                  ; get a random Y coordinate
    LDY #oam_y - oam
    JSR Explosion_Write_0088    ; write Y coords
    
    LDA #$02                    ; attribute = no flipping, use palette 2 (the weapon/magic palette)
    LDY #oam_a - oam
    JSR Explosion_Write_0808
    
    PLA                         ; get the tile
    LDY #oam_t - oam
    LDX #$04
    : STA (btltmp), Y           ; set the tile for all 4 sprites
      JSR IncYBy4
      CLC
      ADC #$01
      DEX
      BNE :-
      
    JSR WaitForVBlank_L         ; do a frame
    LDA #>oam                   ; where OAM is updated
    STA $4014
    JMP BattleUpdateAudio       ; update music/sfx during this frame as well
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Explosion_Write_0808  [$BE68 :: 0x33E78]
;;
;;  Writes a value to OAM for 4 sprites (each sprite of a 16x16 sprite)
;;  The values written are A+0, A+8, A+0, A+8
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Explosion_Write_0808:
    STA (btltmp), Y             ; write +0 value
    PHA
    CLC
    ADC #$08                    ; write +8 value
    JSR IncYBy4_WriteToOam
    
    PLA
    JSR IncYBy4_WriteToOam      ; write +0 value
    JSR IncYBy4
    CLC
    ADC #$08
    STA (btltmp), Y             ; write +8 value
    RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Explosion_Write_0088  [$BE7E :: 0x33E8E]
;;
;;  Same as Expolosion_Write_0808, but the values are written in a different order:
;;  The values written are A+0, A+0, A+8, A+8
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Explosion_Write_0088:
    STA (btltmp), Y             ; write +0 value
    JSR IncYBy4_WriteToOam      ; write +0 value
    CLC
    ADC #$08
    JSR IncYBy4_WriteToOam      ; write +8 value
    JMP IncYBy4_WriteToOam      ; write +8 value
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  IncYBy4_WriteToOam  [$BE8C :: 0x33E9C]
;;
;;    Inc Y by 4 to move to the next sprite data in OAM, then write A to
;;  oam (via btltmp ptr).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

IncYBy4_WriteToOam:
    JSR IncYBy4
    STA (btltmp), Y
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  lut_EraseEnemyPPUAddress_9Small  [$BE92 :: 0x33EA2]
;;
;;     PPU addresses used to erase enemies in the 9 small formation type

lut_EraseEnemyPPUAddress_9Small:
  .WORD $2142, $20C2, $21C2
  .WORD $2146, $20C6, $21C6
  .WORD $214A, $20CA, $21CA

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  lut_EraseEnemyPPUAddress_4Large  [$BEA4 :: 0x33EB4]
;;
;;     PPU addresses used to erase enemies in the 4 large formation type

lut_EraseEnemyPPUAddress_4Large:
  .WORD $20C2, $2182
  .WORD $20CA, $218A
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  lut_EraseEnemyPPUAddress_Mix_Small  [$BEAC :: 0x33EBC]
;;
;;     PPU addresses used to erase *SMALL* enemies in the Mix formation type
;;  Large enemies are omitted -- and the 4Large table is used for them instead.

lut_EraseEnemyPPUAddress_Mix_Small:
  .WORD $2148, $20C8, $21C8
  .WORD $214C, $20CC, $21CC
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  PlayBattleSFX  [$BEB8 :: 0x33EC8]
;;
;;  Begin playing a battle sound effect.  See data_BattleSoundEffects for sound effect descriptions
;;
;;  Desired sound effect stored in A
;;     0 = rising bleeps heard for magic casting and healing
;;     1 = The "boom bash" sound effect heard when an enemy attacks physically
;;     2 = The "chuh" noise heard when a player attacks or when a player is hit with magic.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PlayBattleSFX:
    PHA                             ; backup the desired sfx
    JSR SwapBattleSFXBytes          ; swap in sound effect data
    PLA
    ASL A                           ; double sfx ID and use as index to pointer table lut
    TAX
    
    LDA data_BattleSoundEffects, X      ; copy pointer to btltmp+10, btlsfxsq2_ptr, and btlsfxnse_ptr
    STA btltmp+10
    STA btlsfxsq2_ptr
    STA btlsfxnse_ptr
    LDA data_BattleSoundEffects+1, X
    STA btltmp+11
    STA btlsfxsq2_ptr+1
    STA btlsfxnse_ptr+1
    
    LDA btlsfxsq2_ptr           ; add 3 to the btlsfxsq2_ptr to skip over the header
    CLC
    ADC #$03
    STA btlsfxsq2_ptr
    LDA btlsfxsq2_ptr+1
    ADC #$00
    STA btlsfxsq2_ptr+1
    
    LDA btlsfxnse_ptr           ; add 8 to the noise pointer to skip over the header and square data
    CLC
    ADC #$08
    STA btlsfxnse_ptr
    LDA btlsfxnse_ptr+1
    ADC #$00
    STA btlsfxnse_ptr+1
    
    LDY #$00                    ; Start reading the header
    LDA (btltmp+10), Y          ; byte 0 of header:  the sound effect length in frames
    STA a:sq2_sfx               ; record this to sq2_sfx so that music playback doesn't interfere with the sound effect
    STA btlsfx_framectr
    INY
    LDA (btltmp+10), Y          ; byte 1 of header:  sequence length of the square
    STA btlsfxsq2_len
    INY
    LDA (btltmp+10), Y          ; byte 2 of header:  sequence length of noise
    STA btlsfxnse_len
    
    LDA #$01
    STA btlsfxsq2_framectr      ; init frame counters for sqare/noise to 1, so that when we call the below Update
    STA btlsfxnse_framectr      ;  routines, it will force the channel to be updated
    JSR UpdateBattleSFX_Square
    JSR UpdateBattleSFX_Noise
    
    LDA #$0F
    STA $4015                   ; make sure channels are enabled
    JSR SwapBattleSFXBytes      ; swap out sfx bytes
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  UpdateBattleSFX  [$BF13 :: 0x33F23]
;;
;;  Continues play of battle sound effects.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

UpdateBattleSFX:
    JSR SwapBattleSFXBytes
    LDA btlsfx_framectr
    BEQ :+
      JSR UpdateBattleSFX_Square
      JSR UpdateBattleSFX_Noise
      DEC btlsfx_framectr
  : JSR SwapBattleSFXBytes
    RTS
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  UpdateBattleSFX_Square  [$BF26 :: 0x33F36]
;;
;;  Steps through Square2 sound effect data and updates APU regs
;;
;;  See data_BattleSoundEffects for details of sound effect format
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

UpdateBattleSFX_Square:
    LDA btlsfxsq2_len
    BEQ @Exit
    DEC btlsfxsq2_framectr
    BNE @Exit
      LDY #$04
      LDA (btlsfxsq2_ptr), Y
      STA btlsfxsq2_framectr    ; byte [4] if the frame length of this portion
      LDY #$00
      LDA (btlsfxsq2_ptr), Y
      STA $4004                 ; byte [0] is the volume/duty setting
      INY
      LDA (btlsfxsq2_ptr), Y
      STA $4005                 ; byte [1] is the sweep setting
      INY
      LDA (btlsfxsq2_ptr), Y
      STA $4006                 ; byte [2] is low 8 bits of F-value
      INY
      LDA (btlsfxsq2_ptr), Y
      STA $4007                 ; byte [3] is high 3 bits of F-value + length counter
    
      CLC                       ; add 8 to the pointer (even though we only used 5 bytes of data)
      LDA btlsfxsq2_ptr         ;   (the other 3 bytes are the noise sfx data?)
      ADC #$08
      STA btlsfxsq2_ptr
      LDA btlsfxsq2_ptr+1
      ADC #$00
      STA btlsfxsq2_ptr+1
    
      DEC btlsfxsq2_len
      RTS
  @Exit:
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  UpdateBattleSFX_Noise  [$BF5E :: 0x33F6E]
;;
;;  Steps through Noise sound effect data and updates Noise APU regs
;;
;;  See data_BattleSoundEffects for details of sound effect format
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
UpdateBattleSFX_Noise:
    LDA btlsfxnse_len           ; check the length of this sfx (if any)
    BEQ @Exit                   ; if we've completed it, then just exit
    DEC btlsfxnse_framectr      ; Count down our frame counter
    BNE @Exit                   ; Once it expires, we update the sfx
    
      LDY #$02
      LDA (btlsfxnse_ptr), Y
      STA btlsfxnse_framectr    ; byte [2] if the frame length of this portion
      LDY #$00
      LDA (btlsfxnse_ptr), Y
      STA $400C                 ; byte [0] is the volume setting
      INY
      LDA (btlsfxnse_ptr), Y
      STA $400E                 ; byte [1] is the Freq/tone of the noise
      LDA #$FF
      STA $400F                 ; fixed value of FF used for length counter (keep noise playing for a long time)
      
      CLC                       ; add 8 to the noise pointer (even though we only used 3 bytes of data)
      LDA btlsfxnse_ptr         ;   (the other 5 bytes are the square sfx data?)
      ADC #$08
      STA btlsfxnse_ptr
      LDA btlsfxnse_ptr+1
      ADC #$00
      STA btlsfxnse_ptr+1
      
      DEC btlsfxnse_len         ; decrease the remaining data length of the sfx
      RTS
  @Exit:
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  SwapBattleSFXBytes  [$BF8F :: 0x33F9F]
;;
;;  Swaps 'btlsfx' variables from their "back seat" in RAM to their usable block in zero page.
;;     Either bringing the btlsfx into zero page, or putting them back
;;
;;  Ultimately this routine just swaps the 16 bytes at address $90 and $6D97
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SwapBattleSFXBytes:
    LDX #$00                    ; loop up-counter
    LDY #$10                    ; loop down-counter (copy $10 bytes)
  @Loop:
      LDA btlsfx_frontseat, X   ; swap front and back bytes
      PHA
      LDA btlsfx_backseat, X
      STA btlsfx_frontseat, X
      PLA
      STA btlsfx_backseat, X
      
      INX                       ; update loop counter and keep looping until all bytes swapped
      DEY
      BNE @Loop
    RTS
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  data_BattleSoundEffects  [$BFA4 :: 0x33FB4]
;;
;;  This is the data for the 3 sound effects heard in battle.
;;    sfx 0 = rising bleeps heard for magic casting and healing
;;    sfx 1 = The "boom bash" sound effect heard when an enemy attacks
;;    sfx 2 = The "chuh" noise heard when a player attacks.
;;
;;  This data starts with a short pointer table with entries for each sound effect.
;;
;;  The actual data consists of a 3 byte header, followed by N blocks of 8 bytes each.
;;
;;  Header:
;;     byte 0 = overall length of the sound effect in frames
;;     byte 1 = number of data blocks for the Square channel  (must be at least 1)
;;     byte 2 = number of data blocks for the Noise channel   (must be at least 1)
;;
;;    Following the header is N blocks, where N is the higher of bytes 1 and 2.  Each block has
;;  8 bytes:
;;
;;     byte 0 = Volume/duty for the Square (copied directly to $4004)
;;     byte 1 = Sweep for the Square (copied direclty to $4005)
;;     byte 2 = Low 8 bytes of F-value for Square ($4006)
;;     byte 3 = High 3 bytes of F-value and len counter for Square ($4007)
;;     byte 4 = Length in Frames for the above Square data to be applied
;;
;;     byte 5 = Volume setting for Noise  (copied to $400C)
;;     byte 6 = Freq/tone of Noise  ($400E)
;;     byte 7 = Length in frames for the above Noise data to be applied
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

data_BattleSoundEffects:
  .WORD @sfx_Magic
  .WORD @sfx_EnemyAttack
  .WORD @sfx_PlayerAttack
  
@sfx_Magic:
  .BYTE $3C, $01, $01
  
  .BYTE $FF, $FB, $00, $FB, $32,     $00, $00, $1E
  
@sfx_EnemyAttack:
  .BYTE $0F, $01, $04
  
  .BYTE $00, $00, $00, $00, $01,     $2F, $0E, $03  ; This sound effect starts with a low pitched 'boom'.  That effect actually only plays for
  .BYTE $00, $00, $00, $00, $01,     $2F, $0A, $06  ;   3 frames here... but due to the fact that the game does not properly update sound effect
  .BYTE $00, $00, $00, $00, $01,     $2F, $04, $06  ;   playback while it does the "screen shake" effect, it plays longer than indicated.
  .BYTE $00, $00, $00, $00, $01,     $00, $00, $01

@sfx_PlayerAttack:
  .BYTE $0C, $01, $04
  
  .BYTE $00, $00, $00, $00, $01,     $2F, $07, $03
  .BYTE $00, $00, $00, $00, $01,     $2F, $0C, $05
  .BYTE $00, $00, $00, $00, $01,     $2F, $07, $04
  .BYTE $00, $00, $00, $00, $01,     $00, $00, $01
  
  
; BFFB - unused
  .BYTE $00
  .BYTE $00
  .BYTE $01
  .BYTE $09
  .BYTE $07

