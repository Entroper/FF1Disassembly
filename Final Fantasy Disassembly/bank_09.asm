.include "Constants.inc"
.include "variables.inc"


.export EnterMinimap

.import CallMinimapDecompress, UpdateJoy_L, CallMusicPlay_L, DrawPalette_L, WaitForVBlank_L

.segment "BANK_09"

 .INCBIN "bin/bank_09_data.bin"


BANK_THIS = $09

  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $08
  .BYTE $0E
  .BYTE $01
  .BYTE $2B
  .BYTE $2E
  .BYTE $07
  .BYTE $1F
  .BYTE $1F
  .BYTE $17
  .BYTE $0E
  .BYTE $00
  .BYTE $20
  .BYTE $20
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $A0
  .BYTE $00
  .BYTE $84
  .BYTE $3A
  .BYTE $80
  .BYTE $C0
  .BYTE $E0
  .BYTE $E0
  .BYTE $E0
  .BYTE $E0
  .BYTE $74
  .BYTE $7A
  .BYTE $6D
  .BYTE $73
  .BYTE $37
  .BYTE $DB
  .BYTE $CB
  .BYTE $0F
  .BYTE $07
  .BYTE $07
  .BYTE $20
  .BYTE $10
  .BYTE $D0
  .BYTE $39
  .BYTE $29
  .BYTE $0F
  .BYTE $07
  .BYTE $07
  .BYTE $FE
  .BYTE $FE
  .BYTE $FC
  .BYTE $EB
  .BYTE $F7
  .BYTE $E2
  .BYTE $F0
  .BYTE $F0
  .BYTE $72
  .BYTE $60
  .BYTE $E3
  .BYTE $C4
  .BYTE $F0
  .BYTE $EC
  .BYTE $CC
  .BYTE $C8
  .BYTE $07
  .BYTE $03
  .BYTE $03
  .BYTE $00
  .BYTE $01
  .BYTE $01
  .BYTE $03
  .BYTE $07
  .BYTE $07
  .BYTE $03
  .BYTE $03
  .BYTE $01
  .BYTE $01
  .BYTE $01
  .BYTE $03
  .BYTE $07
  .BYTE $C0
  .BYTE $C0
  .BYTE $E0
  .BYTE $20
  .BYTE $C0
  .BYTE $E0
  .BYTE $C0
  .BYTE $C0
  .BYTE $C0
  .BYTE $C0
  .BYTE $E0
  .BYTE $E0
  .BYTE $E0
  .BYTE $E0
  .BYTE $C0
  .BYTE $C0
  .BYTE $07
  .BYTE $0F
  .BYTE $02
  .BYTE $1C
  .BYTE $3C
  .BYTE $FC
  .BYTE $78
  .BYTE $18
  .BYTE $07
  .BYTE $0F
  .BYTE $1E
  .BYTE $1E
  .BYTE $3C
  .BYTE $FC
  .BYTE $78
  .BYTE $18
  .BYTE $F0
  .BYTE $78
  .BYTE $34
  .BYTE $0E
  .BYTE $0F
  .BYTE $0F
  .BYTE $0E
  .BYTE $1C
  .BYTE $F0
  .BYTE $7C
  .BYTE $3C
  .BYTE $1E
  .BYTE $0F
  .BYTE $0F
  .BYTE $0E
  .BYTE $1C
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $08
  .BYTE $0E
  .BYTE $01
  .BYTE $2B
  .BYTE $2E
  .BYTE $07
  .BYTE $1F
  .BYTE $1F
  .BYTE $17
  .BYTE $0E
  .BYTE $00
  .BYTE $20
  .BYTE $20
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $A0
  .BYTE $00
  .BYTE $84
  .BYTE $3A
  .BYTE $80
  .BYTE $C0
  .BYTE $E0
  .BYTE $E0
  .BYTE $E0
  .BYTE $E0
  .BYTE $74
  .BYTE $7A
  .BYTE $6D
  .BYTE $73
  .BYTE $37
  .BYTE $DB
  .BYTE $CB
  .BYTE $0F
  .BYTE $07
  .BYTE $07
  .BYTE $20
  .BYTE $10
  .BYTE $D0
  .BYTE $39
  .BYTE $29
  .BYTE $0F
  .BYTE $07
  .BYTE $07
  .BYTE $FE
  .BYTE $FE
  .BYTE $FC
  .BYTE $EB
  .BYTE $F7
  .BYTE $E2
  .BYTE $F0
  .BYTE $F0
  .BYTE $72
  .BYTE $60
  .BYTE $E3
  .BYTE $C4
  .BYTE $F0
  .BYTE $EC
  .BYTE $CC
  .BYTE $C8
  .BYTE $07
  .BYTE $0F
  .BYTE $02
  .BYTE $1C
  .BYTE $3C
  .BYTE $FC
  .BYTE $78
  .BYTE $18
  .BYTE $07
  .BYTE $0F
  .BYTE $1E
  .BYTE $1E
  .BYTE $3C
  .BYTE $FC
  .BYTE $78
  .BYTE $18
  .BYTE $F0
  .BYTE $78
  .BYTE $34
  .BYTE $0E
  .BYTE $0F
  .BYTE $0F
  .BYTE $0E
  .BYTE $1C
  .BYTE $F0
  .BYTE $7C
  .BYTE $3C
  .BYTE $1E
  .BYTE $0F
  .BYTE $0F
  .BYTE $0E
  .BYTE $1C
  .BYTE $00
  .BYTE $C0
  .BYTE $C0
  .BYTE $08
  .BYTE $0E
  .BYTE $C1
  .BYTE $EB
  .BYTE $EE
  .BYTE $07
  .BYTE $1F
  .BYTE $1F
  .BYTE $97
  .BYTE $CE
  .BYTE $00
  .BYTE $20
  .BYTE $20
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $A0
  .BYTE $00
  .BYTE $84
  .BYTE $3A
  .BYTE $80
  .BYTE $C0
  .BYTE $E0
  .BYTE $E0
  .BYTE $E0
  .BYTE $E0
  .BYTE $74
  .BYTE $7A
  .BYTE $ED
  .BYTE $73
  .BYTE $37
  .BYTE $1B
  .BYTE $1B
  .BYTE $0F
  .BYTE $07
  .BYTE $07
  .BYTE $20
  .BYTE $30
  .BYTE $30
  .BYTE $19
  .BYTE $19
  .BYTE $0F
  .BYTE $07
  .BYTE $07
  .BYTE $FE
  .BYTE $FE
  .BYTE $FC
  .BYTE $EB
  .BYTE $F7
  .BYTE $E2
  .BYTE $F0
  .BYTE $F0
  .BYTE $72
  .BYTE $60
  .BYTE $E3
  .BYTE $C4
  .BYTE $F0
  .BYTE $EC
  .BYTE $CC
  .BYTE $C8
  .BYTE $0E
  .BYTE $0E
  .BYTE $1E
  .BYTE $04
  .BYTE $38
  .BYTE $38
  .BYTE $78
  .BYTE $F0
  .BYTE $0E
  .BYTE $0E
  .BYTE $1E
  .BYTE $1C
  .BYTE $3C
  .BYTE $38
  .BYTE $78
  .BYTE $F0
  .BYTE $E0
  .BYTE $F0
  .BYTE $70
  .BYTE $60
  .BYTE $18
  .BYTE $1C
  .BYTE $1E
  .BYTE $0F
  .BYTE $E0
  .BYTE $F0
  .BYTE $70
  .BYTE $78
  .BYTE $38
  .BYTE $1C
  .BYTE $1E
  .BYTE $0F
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $07
  .BYTE $0F
  .BYTE $1F
  .BYTE $1F
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $20
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $C0
  .BYTE $E0
  .BYTE $F0
  .BYTE $F0
  .BYTE $0E
  .BYTE $8F
  .BYTE $F2
  .BYTE $BB
  .BYTE $77
  .BYTE $6B
  .BYTE $FC
  .BYTE $FF
  .BYTE $19
  .BYTE $97
  .BYTE $F0
  .BYTE $B0
  .BYTE $30
  .BYTE $28
  .BYTE $1C
  .BYTE $1F
  .BYTE $41
  .BYTE $CE
  .BYTE $2D
  .BYTE $5E
  .BYTE $D7
  .BYTE $83
  .BYTE $63
  .BYTE $E0
  .BYTE $F9
  .BYTE $BE
  .BYTE $0D
  .BYTE $1C
  .BYTE $18
  .BYTE $3C
  .BYTE $1C
  .BYTE $80
  .BYTE $F0
  .BYTE $FF
  .BYTE $47
  .BYTE $00
  .BYTE $80
  .BYTE $88
  .BYTE $DC
  .BYTE $7C
  .BYTE $00
  .BYTE $0F
  .BYTE $37
  .BYTE $78
  .BYTE $B8
  .BYTE $B0
  .BYTE $C0
  .BYTE $60
  .BYTE $5C
  .BYTE $E0
  .BYTE $DC
  .BYTE $3C
  .BYTE $38
  .BYTE $78
  .BYTE $78
  .BYTE $FC
  .BYTE $1C
  .BYTE $FE
  .BYTE $FC
  .BYTE $3C
  .BYTE $38
  .BYTE $78
  .BYTE $78
  .BYTE $FC
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $7E
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $7E
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $03
  .BYTE $1F
  .BYTE $26
  .BYTE $1C
  .BYTE $3C
  .BYTE $04
  .BYTE $E0
  .BYTE $E0
  .BYTE $03
  .BYTE $1F
  .BYTE $23
  .BYTE $03
  .BYTE $03
  .BYTE $7B
  .BYTE $13
  .BYTE $01
  .BYTE $81
  .BYTE $00
  .BYTE $F0
  .BYTE $08
  .BYTE $04
  .BYTE $01
  .BYTE $03
  .BYTE $03
  .BYTE $A5
  .BYTE $7D
  .BYTE $FE
  .BYTE $FF
  .BYTE $FE
  .BYTE $FC
  .BYTE $F8
  .BYTE $F8
  .BYTE $CC
  .BYTE $F7
  .BYTE $FB
  .BYTE $C6
  .BYTE $B8
  .BYTE $98
  .BYTE $88
  .BYTE $88
  .BYTE $CC
  .BYTE $F7
  .BYTE $FB
  .BYTE $C6
  .BYTE $88
  .BYTE $20
  .BYTE $70
  .BYTE $70
  .BYTE $00
  .BYTE $03
  .BYTE $07
  .BYTE $07
  .BYTE $0F
  .BYTE $F8
  .BYTE $05
  .BYTE $17
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $05
  .BYTE $07
  .BYTE $00
  .BYTE $C0
  .BYTE $81
  .BYTE $6E
  .BYTE $F8
  .BYTE $80
  .BYTE $80
  .BYTE $00
  .BYTE $00
  .BYTE $1C
  .BYTE $70
  .BYTE $80
  .BYTE $00
  .BYTE $E0
  .BYTE $F0
  .BYTE $7C
  .BYTE $36
  .BYTE $79
  .BYTE $99
  .BYTE $6B
  .BYTE $70
  .BYTE $E7
  .BYTE $C3
  .BYTE $C3
  .BYTE $06
  .BYTE $00
  .BYTE $04
  .BYTE $20
  .BYTE $10
  .BYTE $00
  .BYTE $C0
  .BYTE $C0
  .BYTE $AA
  .BYTE $A9
  .BYTE $B5
  .BYTE $E0
  .BYTE $1F
  .BYTE $9F
  .BYTE $FF
  .BYTE $F7
  .BYTE $54
  .BYTE $56
  .BYTE $4A
  .BYTE $05
  .BYTE $08
  .BYTE $00
  .BYTE $30
  .BYTE $30
  .BYTE $41
  .BYTE $41
  .BYTE $40
  .BYTE $40
  .BYTE $40
  .BYTE $40
  .BYTE $7D
  .BYTE $03
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $E7
  .BYTE $E7
  .BYTE $07
  .BYTE $E7
  .BYTE $F7
  .BYTE $E7
  .BYTE $E7
  .BYTE $E6
  .BYTE $00
  .BYTE $00
  .BYTE $E0
  .BYTE $10
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $47
  .BYTE $4F
  .BYTE $42
  .BYTE $5C
  .BYTE $3C
  .BYTE $FC
  .BYTE $7B
  .BYTE $18
  .BYTE $00
  .BYTE $00
  .BYTE $1C
  .BYTE $02
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $F3
  .BYTE $7B
  .BYTE $33
  .BYTE $0B
  .BYTE $0B
  .BYTE $0B
  .BYTE $EB
  .BYTE $1A
  .BYTE $00
  .BYTE $00
  .BYTE $08
  .BYTE $10
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $03
  .BYTE $07
  .BYTE $07
  .BYTE $0F
  .BYTE $F8
  .BYTE $05
  .BYTE $27
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $0D
  .BYTE $17
  .BYTE $00
  .BYTE $C0
  .BYTE $81
  .BYTE $6E
  .BYTE $F8
  .BYTE $80
  .BYTE $80
  .BYTE $00
  .BYTE $00
  .BYTE $1C
  .BYTE $70
  .BYTE $80
  .BYTE $00
  .BYTE $E0
  .BYTE $FE
  .BYTE $79
  .BYTE $46
  .BYTE $39
  .BYTE $58
  .BYTE $4E
  .BYTE $40
  .BYTE $47
  .BYTE $23
  .BYTE $23
  .BYTE $36
  .BYTE $40
  .BYTE $85
  .BYTE $01
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $A2
  .BYTE $2D
  .BYTE $B5
  .BYTE $CA
  .BYTE $1F
  .BYTE $9F
  .BYTE $FF
  .BYTE $F3
  .BYTE $5C
  .BYTE $D2
  .BYTE $4A
  .BYTE $25
  .BYTE $08
  .BYTE $04
  .BYTE $30
  .BYTE $30
  .BYTE $27
  .BYTE $2F
  .BYTE $02
  .BYTE $1C
  .BYTE $3C
  .BYTE $FC
  .BYTE $7B
  .BYTE $18
  .BYTE $00
  .BYTE $00
  .BYTE $1C
  .BYTE $02
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $F3
  .BYTE $7B
  .BYTE $33
  .BYTE $0B
  .BYTE $0B
  .BYTE $0B
  .BYTE $EB
  .BYTE $1A
  .BYTE $00
  .BYTE $00
  .BYTE $08
  .BYTE $10
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $03
  .BYTE $07
  .BYTE $07
  .BYTE $0F
  .BYTE $F8
  .BYTE $85
  .BYTE $D7
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $85
  .BYTE $C7
  .BYTE $00
  .BYTE $C0
  .BYTE $81
  .BYTE $6E
  .BYTE $F8
  .BYTE $80
  .BYTE $80
  .BYTE $00
  .BYTE $00
  .BYTE $1C
  .BYTE $70
  .BYTE $80
  .BYTE $00
  .BYTE $E1
  .BYTE $FE
  .BYTE $F8
  .BYTE $FE
  .BYTE $78
  .BYTE $C6
  .BYTE $39
  .BYTE $C0
  .BYTE $F9
  .BYTE $FF
  .BYTE $FF
  .BYTE $C1
  .BYTE $07
  .BYTE $39
  .BYTE $06
  .BYTE $01
  .BYTE $06
  .BYTE $00
  .BYTE $00
  .BYTE $2A
  .BYTE $A9
  .BYTE $B5
  .BYTE $58
  .BYTE $07
  .BYTE $DB
  .BYTE $FB
  .BYTE $97
  .BYTE $D4
  .BYTE $56
  .BYTE $4A
  .BYTE $A5
  .BYTE $20
  .BYTE $20
  .BYTE $00
  .BYTE $00
  .BYTE $FC
  .BYTE $FF
  .BYTE $FF
  .BYTE $F8
  .BYTE $FF
  .BYTE $7F
  .BYTE $0F
  .BYTE $73
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $77
  .BYTE $EF
  .BYTE $9F
  .BYTE $7F
  .BYTE $FF
  .BYTE $FF
  .BYTE $FE
  .BYTE $FB
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $03
  .BYTE $07
  .BYTE $07
  .BYTE $FF
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $C0
  .BYTE $E0
  .BYTE $E0
  .BYTE $DF
  .BYTE $00
  .BYTE $00
  .BYTE $08
  .BYTE $08
  .BYTE $18
  .BYTE $10
  .BYTE $10
  .BYTE $20
  .BYTE $3F
  .BYTE $07
  .BYTE $21
  .BYTE $55
  .BYTE $D7
  .BYTE $F3
  .BYTE $34
  .BYTE $C7
  .BYTE $00
  .BYTE $18
  .BYTE $19
  .BYTE $2D
  .BYTE $2F
  .BYTE $0B
  .BYTE $08
  .BYTE $08
  .BYTE $F8
  .BYTE $E4
  .BYTE $0F
  .BYTE $A3
  .BYTE $DC
  .BYTE $BF
  .BYTE $7F
  .BYTE $E1
  .BYTE $00
  .BYTE $18
  .BYTE $10
  .BYTE $B0
  .BYTE $CC
  .BYTE $86
  .BYTE $66
  .BYTE $E0
  .BYTE $F8
  .BYTE $F7
  .BYTE $FB
  .BYTE $FA
  .BYTE $B8
  .BYTE $B8
  .BYTE $9F
  .BYTE $DC
  .BYTE $00
  .BYTE $10
  .BYTE $30
  .BYTE $20
  .BYTE $00
  .BYTE $00
  .BYTE $0C
  .BYTE $1C
  .BYTE $DD
  .BYTE $E1
  .BYTE $DD
  .BYTE $3D
  .BYTE $39
  .BYTE $79
  .BYTE $7B
  .BYTE $FD
  .BYTE $C0
  .BYTE $9E
  .BYTE $20
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $03
  .BYTE $07
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $06
  .BYTE $1E
  .BYTE $BE
  .BYTE $FE
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $0F
  .BYTE $0F
  .BYTE $07
  .BYTE $0E
  .BYTE $37
  .BYTE $6F
  .BYTE $FF
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $09
  .BYTE $00
  .BYTE $40
  .BYTE $C0
  .BYTE $00
  .BYTE $1E
  .BYTE $7C
  .BYTE $3C
  .BYTE $78
  .BYTE $F0
  .BYTE $CA
  .BYTE $3D
  .BYTE $7F
  .BYTE $E1
  .BYTE $83
  .BYTE $C3
  .BYTE $87
  .BYTE $0F
  .BYTE $0D
  .BYTE $26
  .BYTE $76
  .BYTE $00
  .BYTE $00
  .BYTE $78
  .BYTE $BC
  .BYTE $DE
  .BYTE $DF
  .BYTE $5F
  .BYTE $7E
  .BYTE $00
  .BYTE $C0
  .BYTE $80
  .BYTE $40
  .BYTE $20
  .BYTE $20
  .BYTE $A0
  .BYTE $80
  .BYTE $00
  .BYTE $07
  .BYTE $0F
  .BYTE $17
  .BYTE $17
  .BYTE $08
  .BYTE $05
  .BYTE $07
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $04
  .BYTE $06
  .BYTE $00
  .BYTE $05
  .BYTE $07
  .BYTE $00
  .BYTE $E0
  .BYTE $F0
  .BYTE $F0
  .BYTE $F8
  .BYTE $F8
  .BYTE $FC
  .BYTE $72
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $80
  .BYTE $0C
  .BYTE $0E
  .BYTE $08
  .BYTE $08
  .BYTE $08
  .BYTE $00
  .BYTE $00
  .BYTE $C4
  .BYTE $C3
  .BYTE $36
  .BYTE $50
  .BYTE $37
  .BYTE $D7
  .BYTE $EE
  .BYTE $33
  .BYTE $D3
  .BYTE $D4
  .BYTE $50
  .BYTE $50
  .BYTE $50
  .BYTE $40
  .BYTE $40
  .BYTE $40
  .BYTE $30
  .BYTE $F0
  .BYTE $26
  .BYTE $AF
  .BYTE $8B
  .BYTE $37
  .BYTE $B3
  .BYTE $8D
  .BYTE $F7
  .BYTE $37
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $37
  .BYTE $B7
  .BYTE $B7
  .BYTE $6D
  .BYTE $0E
  .BYTE $0B
  .BYTE $0C
  .BYTE $1F
  .BYTE $C0
  .BYTE $80
  .BYTE $A0
  .BYTE $A0
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $07
  .BYTE $57
  .BYTE $56
  .BYTE $4C
  .BYTE $30
  .BYTE $F8
  .BYTE $98
  .BYTE $FC
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $1C
  .BYTE $37
  .BYTE $B7
  .BYTE $B7
  .BYTE $6E
  .BYTE $0B
  .BYTE $0E
  .BYTE $13
  .BYTE $01
  .BYTE $C0
  .BYTE $C0
  .BYTE $A0
  .BYTE $A0
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $07
  .BYTE $17
  .BYTE $56
  .BYTE $4C
  .BYTE $30
  .BYTE $F8
  .BYTE $18
  .BYTE $FC
  .BYTE $00
  .BYTE $0F
  .BYTE $1F
  .BYTE $2F
  .BYTE $2F
  .BYTE $11
  .BYTE $2B
  .BYTE $6E
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $08
  .BYTE $0C
  .BYTE $00
  .BYTE $6B
  .BYTE $EE
  .BYTE $00
  .BYTE $C0
  .BYTE $E0
  .BYTE $E0
  .BYTE $F0
  .BYTE $FE
  .BYTE $F9
  .BYTE $D4
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $28
  .BYTE $0C
  .BYTE $10
  .BYTE $10
  .BYTE $10
  .BYTE $00
  .BYTE $00
  .BYTE $04
  .BYTE $03
  .BYTE $AC
  .BYTE $A1
  .BYTE $AF
  .BYTE $EE
  .BYTE $F3
  .BYTE $6F
  .BYTE $0B
  .BYTE $04
  .BYTE $D0
  .BYTE $50
  .BYTE $50
  .BYTE $40
  .BYTE $40
  .BYTE $40
  .BYTE $00
  .BYTE $F0
  .BYTE $2E
  .BYTE $8F
  .BYTE $2B
  .BYTE $B7
  .BYTE $B3
  .BYTE $AD
  .BYTE $CF
  .BYTE $37
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $1C
  .BYTE $07
  .BYTE $07
  .BYTE $0F
  .BYTE $0E
  .BYTE $0B
  .BYTE $0E
  .BYTE $13
  .BYTE $01
  .BYTE $F0
  .BYTE $C0
  .BYTE $A0
  .BYTE $A0
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $37
  .BYTE $07
  .BYTE $5E
  .BYTE $40
  .BYTE $30
  .BYTE $F8
  .BYTE $18
  .BYTE $FC
  .BYTE $00
  .BYTE $47
  .BYTE $8F
  .BYTE $D7
  .BYTE $97
  .BYTE $C8
  .BYTE $C5
  .BYTE $C7
  .BYTE $00
  .BYTE $40
  .BYTE $80
  .BYTE $C4
  .BYTE $86
  .BYTE $00
  .BYTE $85
  .BYTE $E7
  .BYTE $00
  .BYTE $E0
  .BYTE $F0
  .BYTE $F0
  .BYTE $F8
  .BYTE $FC
  .BYTE $FA
  .BYTE $71
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $80
  .BYTE $0C
  .BYTE $CE
  .BYTE $48
  .BYTE $08
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $04
  .BYTE $03
  .BYTE $F6
  .BYTE $70
  .BYTE $B7
  .BYTE $B6
  .BYTE $BB
  .BYTE $EF
  .BYTE $F3
  .BYTE $64
  .BYTE $51
  .BYTE $50
  .BYTE $50
  .BYTE $40
  .BYTE $40
  .BYTE $00
  .BYTE $30
  .BYTE $F0
  .BYTE $26
  .BYTE $AF
  .BYTE $8B
  .BYTE $37
  .BYTE $B3
  .BYTE $CD
  .BYTE $F7
  .BYTE $37
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $07
  .BYTE $07
  .BYTE $07
  .BYTE $0D
  .BYTE $0E
  .BYTE $0B
  .BYTE $0C
  .BYTE $1F
  .BYTE $C0
  .BYTE $80
  .BYTE $A0
  .BYTE $A0
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $07
  .BYTE $57
  .BYTE $56
  .BYTE $4C
  .BYTE $30
  .BYTE $F8
  .BYTE $98
  .BYTE $FC
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $07
  .BYTE $0F
  .BYTE $1F
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $C0
  .BYTE $E0
  .BYTE $F0
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $40
  .BYTE $1F
  .BYTE $3F
  .BYTE $71
  .BYTE $B5
  .BYTE $17
  .BYTE $13
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $01
  .BYTE $01
  .BYTE $45
  .BYTE $E7
  .BYTE $E3
  .BYTE $DC
  .BYTE $FE
  .BYTE $EE
  .BYTE $E9
  .BYTE $10
  .BYTE $A8
  .BYTE $C0
  .BYTE $80
  .BYTE $60
  .BYTE $60
  .BYTE $E0
  .BYTE $E6
  .BYTE $07
  .BYTE $A3
  .BYTE $DD
  .BYTE $BF
  .BYTE $7F
  .BYTE $67
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $FF
  .BYTE $7F
  .BYTE $7F
  .BYTE $BF
  .BYTE $BF
  .BYTE $DE
  .BYTE $E5
  .BYTE $7F
  .BYTE $40
  .BYTE $00
  .BYTE $60
  .BYTE $60
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $5B
  .BYTE $9B
  .BYTE $6B
  .BYTE $6B
  .BYTE $1A
  .BYTE $FC
  .BYTE $FE
  .BYTE $06
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $03
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $1C
  .BYTE $E0
  .BYTE $9E
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $0C
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $0C
  .BYTE $32
  .BYTE $61
  .BYTE $3F
  .BYTE $7F
  .BYTE $FF
  .BYTE $FE
  .BYTE $C5
  .BYTE $82
  .BYTE $81
  .BYTE $E0
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $01
  .BYTE $3A
  .BYTE $7D
  .BYTE $5E
  .BYTE $EF
  .BYTE $F8
  .BYTE $E0
  .BYTE $E0
  .BYTE $30
  .BYTE $80
  .BYTE $40
  .BYTE $80
  .BYTE $00
  .BYTE $01
  .BYTE $07
  .BYTE $1F
  .BYTE $C7
  .BYTE $7F
  .BYTE $AF
  .BYTE $77
  .BYTE $F7
  .BYTE $0C
  .BYTE $03
  .BYTE $03
  .BYTE $1C
  .BYTE $1A
  .BYTE $07
  .BYTE $07
  .BYTE $00
  .BYTE $C2
  .BYTE $84
  .BYTE $80
  .BYTE $81
  .BYTE $84
  .BYTE $C8
  .BYTE $E0
  .BYTE $F8
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $01
  .BYTE $07
  .BYTE $00
  .BYTE $05
  .BYTE $1F
  .BYTE $07
  .BYTE $3F
  .BYTE $0F
  .BYTE $06
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $04
  .BYTE $02
  .BYTE $12
  .BYTE $A2
  .BYTE $56
  .BYTE $EE
  .BYTE $DC
  .BYTE $C0
  .BYTE $F4
  .BYTE $FA
  .BYTE $EA
  .BYTE $5A
  .BYTE $26
  .BYTE $0E
  .BYTE $1C
  .BYTE $17
  .BYTE $06
  .BYTE $0F
  .BYTE $80
  .BYTE $6F
  .BYTE $EF
  .BYTE $EF
  .BYTE $EF
  .BYTE $10
  .BYTE $F0
  .BYTE $7F
  .BYTE $90
  .BYTE $6F
  .BYTE $EF
  .BYTE $2F
  .BYTE $2F
  .BYTE $7C
  .BYTE $E0
  .BYTE $85
  .BYTE $4B
  .BYTE $C7
  .BYTE $BF
  .BYTE $77
  .BYTE $77
  .BYTE $7C
  .BYTE $FF
  .BYTE $BD
  .BYTE $7B
  .BYTE $C7
  .BYTE $BF
  .BYTE $07
  .BYTE $07
  .BYTE $2F
  .BYTE $AF
  .BYTE $AF
  .BYTE $AF
  .BYTE $AF
  .BYTE $AF
  .BYTE $AF
  .BYTE $5F
  .BYTE $2F
  .BYTE $AF
  .BYTE $AF
  .BYTE $AF
  .BYTE $AF
  .BYTE $AF
  .BYTE $AF
  .BYTE $5F
  .BYTE $37
  .BYTE $4F
  .BYTE $4E
  .BYTE $4E
  .BYTE $4E
  .BYTE $4C
  .BYTE $4A
  .BYTE $B7
  .BYTE $07
  .BYTE $4F
  .BYTE $4E
  .BYTE $4E
  .BYTE $4E
  .BYTE $4C
  .BYTE $4A
  .BYTE $B7
  .BYTE $2F
  .BYTE $AF
  .BYTE $AF
  .BYTE $AF
  .BYTE $AF
  .BYTE $BF
  .BYTE $47
  .BYTE $B8
  .BYTE $2F
  .BYTE $AF
  .BYTE $AF
  .BYTE $AF
  .BYTE $AF
  .BYTE $BF
  .BYTE $47
  .BYTE $80
  .BYTE $37
  .BYTE $4F
  .BYTE $4E
  .BYTE $4E
  .BYTE $4E
  .BYTE $4C
  .BYTE $4A
  .BYTE $B7
  .BYTE $07
  .BYTE $4F
  .BYTE $4E
  .BYTE $4E
  .BYTE $4E
  .BYTE $4C
  .BYTE $4A
  .BYTE $B7
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $01
  .BYTE $17
  .BYTE $10
  .BYTE $35
  .BYTE $1F
  .BYTE $07
  .BYTE $3F
  .BYTE $0F
  .BYTE $06
  .BYTE $10
  .BYTE $10
  .BYTE $38
  .BYTE $00
  .BYTE $04
  .BYTE $02
  .BYTE $12
  .BYTE $A2
  .BYTE $56
  .BYTE $EE
  .BYTE $DC
  .BYTE $C0
  .BYTE $F4
  .BYTE $FA
  .BYTE $EA
  .BYTE $5A
  .BYTE $26
  .BYTE $0E
  .BYTE $1C
  .BYTE $27
  .BYTE $66
  .BYTE $6F
  .BYTE $60
  .BYTE $6F
  .BYTE $6F
  .BYTE $6F
  .BYTE $6F
  .BYTE $30
  .BYTE $70
  .BYTE $6F
  .BYTE $60
  .BYTE $6F
  .BYTE $6F
  .BYTE $6F
  .BYTE $6F
  .BYTE $7C
  .BYTE $E0
  .BYTE $85
  .BYTE $4B
  .BYTE $E3
  .BYTE $DF
  .BYTE $BF
  .BYTE $BF
  .BYTE $7C
  .BYTE $FF
  .BYTE $BD
  .BYTE $7B
  .BYTE $E3
  .BYTE $DF
  .BYTE $87
  .BYTE $87
  .BYTE $2F
  .BYTE $2F
  .BYTE $2F
  .BYTE $2F
  .BYTE $0F
  .BYTE $0F
  .BYTE $0F
  .BYTE $1F
  .BYTE $2F
  .BYTE $2F
  .BYTE $2F
  .BYTE $2F
  .BYTE $0F
  .BYTE $0F
  .BYTE $0F
  .BYTE $1F
  .BYTE $BF
  .BYTE $A7
  .BYTE $A7
  .BYTE $A6
  .BYTE $A6
  .BYTE $A6
  .BYTE $A6
  .BYTE $D9
  .BYTE $A7
  .BYTE $A7
  .BYTE $A7
  .BYTE $A6
  .BYTE $A6
  .BYTE $A6
  .BYTE $A6
  .BYTE $D9
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $01
  .BYTE $07
  .BYTE $80
  .BYTE $B5
  .BYTE $1F
  .BYTE $07
  .BYTE $3F
  .BYTE $0F
  .BYTE $06
  .BYTE $00
  .BYTE $00
  .BYTE $30
  .BYTE $00
  .BYTE $04
  .BYTE $02
  .BYTE $12
  .BYTE $A2
  .BYTE $56
  .BYTE $EE
  .BYTE $C2
  .BYTE $C0
  .BYTE $F4
  .BYTE $FA
  .BYTE $EA
  .BYTE $5A
  .BYTE $26
  .BYTE $0E
  .BYTE $1E
  .BYTE $B7
  .BYTE $00
  .BYTE $DF
  .BYTE $DF
  .BYTE $9F
  .BYTE $9F
  .BYTE $1F
  .BYTE $1F
  .BYTE $30
  .BYTE $00
  .BYTE $1F
  .BYTE $1F
  .BYTE $1F
  .BYTE $1F
  .BYTE $1F
  .BYTE $1F
  .BYTE $00
  .BYTE $78
  .BYTE $FC
  .BYTE $FE
  .BYTE $FE
  .BYTE $F6
  .BYTE $F6
  .BYTE $EE
  .BYTE $7C
  .BYTE $7E
  .BYTE $FE
  .BYTE $FE
  .BYTE $FE
  .BYTE $F6
  .BYTE $F6
  .BYTE $EE
  .BYTE $1F
  .BYTE $1F
  .BYTE $0F
  .BYTE $0F
  .BYTE $06
  .BYTE $09
  .BYTE $0F
  .BYTE $1F
  .BYTE $1F
  .BYTE $1F
  .BYTE $0F
  .BYTE $0F
  .BYTE $06
  .BYTE $09
  .BYTE $0F
  .BYTE $1F
  .BYTE $EE
  .BYTE $DE
  .BYTE $BC
  .BYTE $7C
  .BYTE $FE
  .BYTE $FE
  .BYTE $FE
  .BYTE $FF
  .BYTE $EE
  .BYTE $DE
  .BYTE $BC
  .BYTE $7C
  .BYTE $FE
  .BYTE $FE
  .BYTE $FE
  .BYTE $FF
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $1F
  .BYTE $07
  .BYTE $3F
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $38
  .BYTE $04
  .BYTE $02
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $F8
  .BYTE $F4
  .BYTE $FA
  .BYTE $00
  .BYTE $10
  .BYTE $05
  .BYTE $67
  .BYTE $41
  .BYTE $FD
  .BYTE $FB
  .BYTE $F9
  .BYTE $1F
  .BYTE $1F
  .BYTE $32
  .BYTE $78
  .BYTE $50
  .BYTE $F8
  .BYTE $E0
  .BYTE $C0
  .BYTE $02
  .BYTE $22
  .BYTE $F2
  .BYTE $E2
  .BYTE $14
  .BYTE $A4
  .BYTE $DB
  .BYTE $B3
  .BYTE $FA
  .BYTE $DA
  .BYTE $0A
  .BYTE $0B
  .BYTE $07
  .BYTE $07
  .BYTE $1F
  .BYTE $37
  .BYTE $DE
  .BYTE $BF
  .BYTE $BF
  .BYTE $BE
  .BYTE $BE
  .BYTE $7F
  .BYTE $3F
  .BYTE $FF
  .BYTE $DE
  .BYTE $BF
  .BYTE $BF
  .BYTE $BE
  .BYTE $BE
  .BYTE $7F
  .BYTE $3F
  .BYTE $FF
  .BYTE $77
  .BYTE $2F
  .BYTE $EF
  .BYTE $EF
  .BYTE $EF
  .BYTE $5F
  .BYTE $1F
  .BYTE $9D
  .BYTE $77
  .BYTE $2F
  .BYTE $EF
  .BYTE $EF
  .BYTE $EF
  .BYTE $5F
  .BYTE $1F
  .BYTE $81
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $03
  .BYTE $07
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $03
  .BYTE $9F
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $80
  .BYTE $C0
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $80
  .BYTE $C0
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $0C
  .BYTE $32
  .BYTE $E1
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $0C
  .BYTE $32
  .BYTE $ED
  .BYTE $07
  .BYTE $0E
  .BYTE $09
  .BYTE $07
  .BYTE $1F
  .BYTE $3F
  .BYTE $5F
  .BYTE $6F
  .BYTE $BF
  .BYTE $FE
  .BYTE $79
  .BYTE $F7
  .BYTE $7F
  .BYTE $3F
  .BYTE $5F
  .BYTE $0F
  .BYTE $C3
  .BYTE $7F
  .BYTE $FF
  .BYTE $FF
  .BYTE $9F
  .BYTE $E7
  .BYTE $F9
  .BYTE $FE
  .BYTE $C3
  .BYTE $7F
  .BYTE $FF
  .BYTE $FF
  .BYTE $9F
  .BYTE $E7
  .BYTE $F9
  .BYTE $FE
  .BYTE $C4
  .BYTE $C0
  .BYTE $80
  .BYTE $81
  .BYTE $84
  .BYTE $C8
  .BYTE $E0
  .BYTE $F8
  .BYTE $CA
  .BYTE $C7
  .BYTE $83
  .BYTE $9D
  .BYTE $9B
  .BYTE $D7
  .BYTE $E7
  .BYTE $F8
  .BYTE $00
  .BYTE $70
  .BYTE $78
  .BYTE $7C
  .BYTE $3E
  .BYTE $1F
  .BYTE $0D
  .BYTE $06
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $02
  .BYTE $01
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $80
  .BYTE $C0
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $03
  .BYTE $01
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $60
  .BYTE $B0
  .BYTE $D9
  .BYTE $69
  .BYTE $33
  .BYTE $02
  .BYTE $0D
  .BYTE $3B
  .BYTE $80
  .BYTE $40
  .BYTE $21
  .BYTE $11
  .BYTE $0F
  .BYTE $0E
  .BYTE $0F
  .BYTE $3B
  .BYTE $00
  .BYTE $10
  .BYTE $10
  .BYTE $18
  .BYTE $08
  .BYTE $0C
  .BYTE $06
  .BYTE $03
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $01
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $80
  .BYTE $C0
  .BYTE $60
  .BYTE $30
  .BYTE $18
  .BYTE $06
  .BYTE $07
  .BYTE $02
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $02
  .BYTE $04
  .BYTE $0E
  .BYTE $17
  .BYTE $02
  .BYTE $20
  .BYTE $30
  .BYTE $38
  .BYTE $38
  .BYTE $3A
  .BYTE $1E
  .BYTE $0F
  .BYTE $07
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $02
  .BYTE $01
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $80
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $03
  .BYTE $01
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $C0
  .BYTE $E2
  .BYTE $72
  .BYTE $3B
  .BYTE $1D
  .BYTE $0A
  .BYTE $76
  .BYTE $19
  .BYTE $80
  .BYTE $42
  .BYTE $22
  .BYTE $13
  .BYTE $09
  .BYTE $06
  .BYTE $76
  .BYTE $19
  .BYTE $60
  .BYTE $70
  .BYTE $78
  .BYTE $3C
  .BYTE $1E
  .BYTE $0F
  .BYTE $05
  .BYTE $02
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $02
  .BYTE $01
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $80
  .BYTE $C0
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $01
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $60
  .BYTE $B2
  .BYTE $59
  .BYTE $2D
  .BYTE $16
  .BYTE $0A
  .BYTE $4D
  .BYTE $32
  .BYTE $80
  .BYTE $42
  .BYTE $21
  .BYTE $11
  .BYTE $0A
  .BYTE $06
  .BYTE $4D
  .BYTE $32
  .BYTE $00
  .BYTE $00
  .BYTE $18
  .BYTE $1C
  .BYTE $0E
  .BYTE $07
  .BYTE $03
  .BYTE $01
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $10
  .BYTE $08
  .BYTE $04
  .BYTE $02
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $80
  .BYTE $C0
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $01
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $E0
  .BYTE $72
  .BYTE $39
  .BYTE $1D
  .BYTE $0D
  .BYTE $02
  .BYTE $05
  .BYTE $18
  .BYTE $00
  .BYTE $82
  .BYTE $41
  .BYTE $21
  .BYTE $13
  .BYTE $0E
  .BYTE $05
  .BYTE $18
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $19
  .BYTE $0A
  .BYTE $00
  .BYTE $01
  .BYTE $02
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $01
  .BYTE $13
  .BYTE $07
  .BYTE $1F
  .BYTE $3F
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $80
  .BYTE $C0
  .BYTE $E0
  .BYTE $B0
  .BYTE $60
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $80
  .BYTE $C0
  .BYTE $E0
  .BYTE $F0
  .BYTE $E0
  .BYTE $04
  .BYTE $00
  .BYTE $01
  .BYTE $02
  .BYTE $04
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $7F
  .BYTE $7F
  .BYTE $3F
  .BYTE $1E
  .BYTE $0C
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $C0
  .BYTE $A0
  .BYTE $30
  .BYTE $18
  .BYTE $0C
  .BYTE $02
  .BYTE $01
  .BYTE $00
  .BYTE $C0
  .BYTE $80
  .BYTE $40
  .BYTE $20
  .BYTE $10
  .BYTE $0E
  .BYTE $07
  .BYTE $03
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $02
  .BYTE $03
  .BYTE $03
  .BYTE $01
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $80
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $01
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $C0
  .BYTE $60
  .BYTE $30
  .BYTE $18
  .BYTE $0C
  .BYTE $0A
  .BYTE $07
  .BYTE $03
  .BYTE $00
  .BYTE $80
  .BYTE $40
  .BYTE $20
  .BYTE $1C
  .BYTE $0A
  .BYTE $07
  .BYTE $03
  .BYTE $00
  .BYTE $0C
  .BYTE $08
  .BYTE $06
  .BYTE $83
  .BYTE $E1
  .BYTE $40
  .BYTE $38
  .BYTE $18
  .BYTE $1D
  .BYTE $0D
  .BYTE $05
  .BYTE $0E
  .BYTE $1F
  .BYTE $BF
  .BYTE $46
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $80
  .BYTE $C0
  .BYTE $20
  .BYTE $00
  .BYTE $00
  .BYTE $80
  .BYTE $C0
  .BYTE $E0
  .BYTE $00
  .BYTE $80
  .BYTE $40
  .BYTE $30
  .BYTE $0E
  .BYTE $02
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $4E
  .BYTE $30
  .BYTE $0C
  .BYTE $03
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $10
  .BYTE $08
  .BYTE $04
  .BYTE $06
  .BYTE $03
  .BYTE $01
  .BYTE $00
  .BYTE $00
  .BYTE $20
  .BYTE $10
  .BYTE $08
  .BYTE $06
  .BYTE $03
  .BYTE $01
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $1E
  .BYTE $31
  .BYTE $21
  .BYTE $22
  .BYTE $12
  .BYTE $01
  .BYTE $1C
  .BYTE $7F
  .BYTE $7F
  .BYTE $F1
  .BYTE $E1
  .BYTE $E3
  .BYTE $73
  .BYTE $01
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $80
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $80
  .BYTE $80
  .BYTE $80
  .BYTE $80
  .BYTE $80
  .BYTE $C0
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $80
  .BYTE $40
  .BYTE $20
  .BYTE $14
  .BYTE $0A
  .BYTE $05
  .BYTE $02
  .BYTE $01
  .BYTE $E0
  .BYTE $70
  .BYTE $38
  .BYTE $1C
  .BYTE $0E
  .BYTE $07
  .BYTE $03
  .BYTE $01
  .BYTE $60
  .BYTE $C0
  .BYTE $14
  .BYTE $38
  .BYTE $58
  .BYTE $0C
  .BYTE $06
  .BYTE $03
  .BYTE $60
  .BYTE $E4
  .BYTE $F0
  .BYTE $20
  .BYTE $9C
  .BYTE $0E
  .BYTE $07
  .BYTE $03
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $80
  .BYTE $01
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $01
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $80
  .BYTE $C0
  .BYTE $60
  .BYTE $30
  .BYTE $18
  .BYTE $0C
  .BYTE $06
  .BYTE $03
  .BYTE $C0
  .BYTE $E0
  .BYTE $70
  .BYTE $38
  .BYTE $1C
  .BYTE $0E
  .BYTE $07
  .BYTE $03
  .BYTE $80
  .BYTE $80
  .BYTE $C0
  .BYTE $60
  .BYTE $30
  .BYTE $18
  .BYTE $0C
  .BYTE $06
  .BYTE $C0
  .BYTE $E0
  .BYTE $F0
  .BYTE $78
  .BYTE $3C
  .BYTE $1E
  .BYTE $0E
  .BYTE $07
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $30
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $A0
  .BYTE $30
  .BYTE $18
  .BYTE $0C
  .BYTE $06
  .BYTE $03
  .BYTE $00
  .BYTE $00
  .BYTE $F8
  .BYTE $3C
  .BYTE $1E
  .BYTE $0F
  .BYTE $07
  .BYTE $03
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $73
  .BYTE $53
  .BYTE $23
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $0C
  .BYTE $2C
  .BYTE $4C
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $01
  .BYTE $01
  .BYTE $00
  .BYTE $00
  .BYTE $01
  .BYTE $01
  .BYTE $00
  .BYTE $00
  .BYTE $80
  .BYTE $80
  .BYTE $80
  .BYTE $80
  .BYTE $80
  .BYTE $80
  .BYTE $80
  .BYTE $80
  .BYTE $00
  .BYTE $80
  .BYTE $40
  .BYTE $C0
  .BYTE $00
  .BYTE $80
  .BYTE $40
  .BYTE $C0
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $01
  .BYTE $01
  .BYTE $00
  .BYTE $00
  .BYTE $01
  .BYTE $01
  .BYTE $00
  .BYTE $00
  .BYTE $80
  .BYTE $80
  .BYTE $80
  .BYTE $80
  .BYTE $80
  .BYTE $80
  .BYTE $80
  .BYTE $80
  .BYTE $00
  .BYTE $80
  .BYTE $40
  .BYTE $C0
  .BYTE $00
  .BYTE $80
  .BYTE $40
  .BYTE $C0
  .BYTE $01
  .BYTE $01
  .BYTE $01
  .BYTE $01
  .BYTE $01
  .BYTE $01
  .BYTE $01
  .BYTE $01
  .BYTE $02
  .BYTE $02
  .BYTE $00
  .BYTE $00
  .BYTE $02
  .BYTE $02
  .BYTE $00
  .BYTE $00
  .BYTE $C0
  .BYTE $C0
  .BYTE $C0
  .BYTE $C0
  .BYTE $C0
  .BYTE $C0
  .BYTE $C0
  .BYTE $C0
  .BYTE $A0
  .BYTE $A0
  .BYTE $80
  .BYTE $80
  .BYTE $A0
  .BYTE $A0
  .BYTE $80
  .BYTE $80
  .BYTE $01
  .BYTE $01
  .BYTE $01
  .BYTE $01
  .BYTE $01
  .BYTE $01
  .BYTE $01
  .BYTE $01
  .BYTE $02
  .BYTE $02
  .BYTE $00
  .BYTE $00
  .BYTE $02
  .BYTE $02
  .BYTE $00
  .BYTE $00
  .BYTE $C0
  .BYTE $C0
  .BYTE $C0
  .BYTE $C0
  .BYTE $C0
  .BYTE $C0
  .BYTE $C0
  .BYTE $C0
  .BYTE $A0
  .BYTE $A0
  .BYTE $80
  .BYTE $80
  .BYTE $A0
  .BYTE $A0
  .BYTE $80
  .BYTE $80
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $0A
  .BYTE $04
  .BYTE $0A
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $11
  .BYTE $00
  .BYTE $04
  .BYTE $00
  .BYTE $11
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $50
  .BYTE $20
  .BYTE $50
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $88
  .BYTE $00
  .BYTE $20
  .BYTE $00
  .BYTE $88
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $14
  .BYTE $08
  .BYTE $14
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $22
  .BYTE $00
  .BYTE $08
  .BYTE $00
  .BYTE $22
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $50
  .BYTE $20
  .BYTE $50
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $88
  .BYTE $00
  .BYTE $20
  .BYTE $00
  .BYTE $88
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $04
  .BYTE $0E
  .BYTE $1E
  .BYTE $0E
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $04
  .BYTE $00
  .BYTE $05
  .BYTE $2F
  .BYTE $04
  .BYTE $04
  .BYTE $00
  .BYTE $00
  .BYTE $20
  .BYTE $70
  .BYTE $F8
  .BYTE $70
  .BYTE $20
  .BYTE $00
  .BYTE $00
  .BYTE $20
  .BYTE $00
  .BYTE $20
  .BYTE $74
  .BYTE $20
  .BYTE $00
  .BYTE $60
  .BYTE $00
  .BYTE $08
  .BYTE $1C
  .BYTE $3E
  .BYTE $1C
  .BYTE $08
  .BYTE $00
  .BYTE $00
  .BYTE $08
  .BYTE $00
  .BYTE $09
  .BYTE $5D
  .BYTE $08
  .BYTE $00
  .BYTE $08
  .BYTE $00
  .BYTE $20
  .BYTE $70
  .BYTE $F8
  .BYTE $70
  .BYTE $20
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $20
  .BYTE $74
  .BYTE $A0
  .BYTE $00
  .BYTE $20
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $02
  .BYTE $00
  .BYTE $00
  .BYTE $40
  .BYTE $00
  .BYTE $04
  .BYTE $00
  .BYTE $20
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $70
  .BYTE $74
  .BYTE $00
  .BYTE $00
  .BYTE $0E
  .BYTE $20
  .BYTE $00
  .BYTE $20
  .BYTE $A8
  .BYTE $20
  .BYTE $50
  .BYTE $04
  .BYTE $54
  .BYTE $04
  .BYTE $38
  .BYTE $3A
  .BYTE $00
  .BYTE $00
  .BYTE $07
  .BYTE $27
  .BYTE $00
  .BYTE $00
  .BYTE $54
  .BYTE $10
  .BYTE $28
  .BYTE $02
  .BYTE $2A
  .BYTE $52
  .BYTE $25
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $44
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $08
  .BYTE $00
  .BYTE $02
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $38
  .BYTE $3A
  .BYTE $00
  .BYTE $00
  .BYTE $07
  .BYTE $07
  .BYTE $00
  .BYTE $10
  .BYTE $54
  .BYTE $10
  .BYTE $28
  .BYTE $02
  .BYTE $4A
  .BYTE $12
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $20
  .BYTE $08
  .BYTE $00
  .BYTE $00
  .BYTE $04
  .BYTE $40
  .BYTE $00
  .BYTE $00
  .BYTE $10
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $02
  .BYTE $00
  .BYTE $00
  .BYTE $42
  .BYTE $00
  .BYTE $00
  .BYTE $71
  .BYTE $74
  .BYTE $00
  .BYTE $00
  .BYTE $0E
  .BYTE $4E
  .BYTE $00
  .BYTE $20
  .BYTE $A8
  .BYTE $20
  .BYTE $50
  .BYTE $04
  .BYTE $55
  .BYTE $A4
  .BYTE $4A
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $0C
  .BYTE $33
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $0C
  .BYTE $33
  .BYTE $FF
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $C0
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $FF
  .BYTE $FF
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $FF
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $FF
  .BYTE $0C
  .BYTE $03
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $30
  .BYTE $FF
  .BYTE $03
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $03
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $FF
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $30
  .BYTE $CC
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $30
  .BYTE $CC
  .BYTE $FF
  .BYTE $FF
  .BYTE $30
  .BYTE $C0
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $0C
  .BYTE $FF
  .BYTE $C0
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $FF
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $FF
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $63
  .BYTE $FF
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $63
  .BYTE $9C
  .BYTE $B1
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $C0
  .BYTE $FE
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $E0
  .BYTE $3C
  .BYTE $E1
  .BYTE $FF
  .BYTE $FF
  .BYTE $FF
  .BYTE $43
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $7F
  .BYTE $BF
  .BYTE $63
  .BYTE $BC
  .BYTE $73
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $FF
  .BYTE $FE
  .BYTE $FC
  .BYTE $F8
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $FE
  .BYTE $FF
  .BYTE $E2
  .BYTE $87
  .BYTE $D0
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $0E
  .BYTE $3F
  .BYTE $7F
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $67
  .BYTE $19
  .BYTE $91
  .BYTE $CE
  .BYTE $9F
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $30
  .BYTE $F9
  .BYTE $FF
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $80
  .BYTE $EC
  .BYTE $CF
  .BYTE $36
  .BYTE $F9
  .BYTE $FF
  .BYTE $FF
  .BYTE $7F
  .BYTE $7F
  .BYTE $3C
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $7F
  .BYTE $7F
  .BYTE $BF
  .BYTE $1C
  .BYTE $C3
  .BYTE $1D
  .BYTE $63
  .BYTE $00
  .BYTE $FF
  .BYTE $FF
  .BYTE $FF
  .BYTE $FE
  .BYTE $38
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $FF
  .BYTE $FF
  .BYTE $7E
  .BYTE $19
  .BYTE $C7
  .BYTE $FE
  .BYTE $2C
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $03
  .BYTE $07
  .BYTE $07
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $03
  .BYTE $04
  .BYTE $0B
  .BYTE $0B
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $C0
  .BYTE $E0
  .BYTE $E0
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $C0
  .BYTE $20
  .BYTE $D0
  .BYTE $D0
  .BYTE $07
  .BYTE $07
  .BYTE $03
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $0B
  .BYTE $0B
  .BYTE $04
  .BYTE $03
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $E0
  .BYTE $E0
  .BYTE $C0
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $D0
  .BYTE $D0
  .BYTE $20
  .BYTE $C0
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $03
  .BYTE $0F
  .BYTE $1F
  .BYTE $3F
  .BYTE $3F
  .BYTE $7F
  .BYTE $7F
  .BYTE $03
  .BYTE $0C
  .BYTE $13
  .BYTE $2F
  .BYTE $5F
  .BYTE $5F
  .BYTE $BF
  .BYTE $BF
  .BYTE $00
  .BYTE $C0
  .BYTE $F0
  .BYTE $F8
  .BYTE $FC
  .BYTE $FC
  .BYTE $FE
  .BYTE $FE
  .BYTE $C0
  .BYTE $30
  .BYTE $C8
  .BYTE $F4
  .BYTE $FA
  .BYTE $FA
  .BYTE $FD
  .BYTE $FD
  .BYTE $7F
  .BYTE $7F
  .BYTE $3F
  .BYTE $3F
  .BYTE $1F
  .BYTE $0F
  .BYTE $03
  .BYTE $00
  .BYTE $BF
  .BYTE $BF
  .BYTE $5F
  .BYTE $5F
  .BYTE $2F
  .BYTE $13
  .BYTE $0C
  .BYTE $03
  .BYTE $FE
  .BYTE $FE
  .BYTE $FC
  .BYTE $FC
  .BYTE $F8
  .BYTE $F0
  .BYTE $C0
  .BYTE $00
  .BYTE $FD
  .BYTE $FD
  .BYTE $FA
  .BYTE $FA
  .BYTE $F4
  .BYTE $C8
  .BYTE $30
  .BYTE $C0
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $01
  .BYTE $03
  .BYTE $03
  .BYTE $00
  .BYTE $02
  .BYTE $00
  .BYTE $08
  .BYTE $02
  .BYTE $24
  .BYTE $00
  .BYTE $15
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $20
  .BYTE $C0
  .BYTE $E0
  .BYTE $00
  .BYTE $00
  .BYTE $40
  .BYTE $00
  .BYTE $88
  .BYTE $00
  .BYTE $90
  .BYTE $C8
  .BYTE $03
  .BYTE $05
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $06
  .BYTE $03
  .BYTE $2A
  .BYTE $00
  .BYTE $01
  .BYTE $08
  .BYTE $01
  .BYTE $00
  .BYTE $C0
  .BYTE $60
  .BYTE $80
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $B0
  .BYTE $C4
  .BYTE $00
  .BYTE $20
  .BYTE $00
  .BYTE $08
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $08
  .BYTE $00
  .BYTE $00
  .BYTE $04
  .BYTE $01
  .BYTE $00
  .BYTE $40
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $44
  .BYTE $01
  .BYTE $00
  .BYTE $00
  .BYTE $08
  .BYTE $00
  .BYTE $00
  .BYTE $40
  .BYTE $00
  .BYTE $10
  .BYTE $00
  .BYTE $40
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $40
  .BYTE $02
  .BYTE $10
  .BYTE $00
  .BYTE $24
  .BYTE $00
  .BYTE $08
  .BYTE $01
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $04
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $20
  .BYTE $00
  .BYTE $00
  .BYTE $20
  .BYTE $02
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $20
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $04
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $06
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $01
  .BYTE $3E
  .BYTE $3E
  .BYTE $7F
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $C0
  .BYTE $38
  .BYTE $CC
  .BYTE $16
  .BYTE $BF
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $01
  .BYTE $0B
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $7E
  .BYTE $FF
  .BYTE $3E
  .BYTE $0C
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $9C
  .BYTE $3E
  .BYTE $DC
  .BYTE $30
  .BYTE $42
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $01
  .BYTE $01
  .BYTE $07
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $13
  .BYTE $00
  .BYTE $3E
  .BYTE $02
  .BYTE $38
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $40
  .BYTE $3E
  .BYTE $FF
  .BYTE $FF
  .BYTE $FE
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $9C
  .BYTE $D8
  .BYTE $3C
  .BYTE $FE
  .BYTE $7C
  .BYTE $0F
  .BYTE $03
  .BYTE $0F
  .BYTE $03
  .BYTE $00
  .BYTE $04
  .BYTE $00
  .BYTE $00
  .BYTE $F1
  .BYTE $3C
  .BYTE $93
  .BYTE $7C
  .BYTE $03
  .BYTE $28
  .BYTE $00
  .BYTE $00
  .BYTE $FF
  .BYTE $FE
  .BYTE $FE
  .BYTE $FF
  .BYTE $FC
  .BYTE $C0
  .BYTE $00
  .BYTE $00
  .BYTE $FF
  .BYTE $FC
  .BYTE $FE
  .BYTE $5C
  .BYTE $0A
  .BYTE $36
  .BYTE $08
  .BYTE $00
  .BYTE $07
  .BYTE $1F
  .BYTE $3E
  .BYTE $7F
  .BYTE $DF
  .BYTE $FF
  .BYTE $DF
  .BYTE $DF
  .BYTE $07
  .BYTE $18
  .BYTE $21
  .BYTE $60
  .BYTE $A0
  .BYTE $80
  .BYTE $A0
  .BYTE $A0
  .BYTE $80
  .BYTE $C0
  .BYTE $1E
  .BYTE $FF
  .BYTE $FE
  .BYTE $E1
  .BYTE $DE
  .BYTE $D0
  .BYTE $80
  .BYTE $40
  .BYTE $FE
  .BYTE $01
  .BYTE $01
  .BYTE $1F
  .BYTE $3E
  .BYTE $30
  .BYTE $DF
  .BYTE $CF
  .BYTE $A7
  .BYTE $70
  .BYTE $1F
  .BYTE $FF
  .BYTE $FF
  .BYTE $3F
  .BYTE $A0
  .BYTE $B0
  .BYTE $F8
  .BYTE $7F
  .BYTE $1F
  .BYTE $FF
  .BYTE $FF
  .BYTE $3F
  .BYTE $90
  .BYTE $A0
  .BYTE $20
  .BYTE $40
  .BYTE $FC
  .BYTE $FF
  .BYTE $FE
  .BYTE $80
  .BYTE $70
  .BYTE $60
  .BYTE $E0
  .BYTE $C0
  .BYTE $FC
  .BYTE $FF
  .BYTE $FE
  .BYTE $80
  .BYTE $00
  .BYTE $22
  .BYTE $72
  .BYTE $38
  .BYTE $0A
  .BYTE $00
  .BYTE $30
  .BYTE $02
  .BYTE $62
  .BYTE $D5
  .BYTE $A8
  .BYTE $50
  .BYTE $28
  .BYTE $40
  .BYTE $D0
  .BYTE $42
  .BYTE $00
  .BYTE $20
  .BYTE $68
  .BYTE $70
  .BYTE $E0
  .BYTE $40
  .BYTE $8E
  .BYTE $20
  .BYTE $30
  .BYTE $5C
  .BYTE $94
  .BYTE $CC
  .BYTE $B8
  .BYTE $42
  .BYTE $09
  .BYTE $02
  .BYTE $08
  .BYTE $09
  .BYTE $00
  .BYTE $01
  .BYTE $71
  .BYTE $61
  .BYTE $08
  .BYTE $00
  .BYTE $10
  .BYTE $35
  .BYTE $98
  .BYTE $60
  .BYTE $AB
  .BYTE $92
  .BYTE $63
  .BYTE $09
  .BYTE $40
  .BYTE $10
  .BYTE $8E
  .BYTE $86
  .BYTE $C0
  .BYTE $88
  .BYTE $04
  .BYTE $00
  .BYTE $09
  .BYTE $16
  .BYTE $05
  .BYTE $C9
  .BYTE $27
  .BYTE $68
  .BYTE $C2
  .BYTE $96
  .BYTE $80
  .BYTE $40
  .BYTE $20
  .BYTE $18
  .BYTE $1E
  .BYTE $0F
  .BYTE $0F
  .BYTE $07
  .BYTE $80
  .BYTE $40
  .BYTE $20
  .BYTE $10
  .BYTE $08
  .BYTE $06
  .BYTE $07
  .BYTE $03
  .BYTE $01
  .BYTE $00
  .BYTE $00
  .BYTE $08
  .BYTE $30
  .BYTE $F0
  .BYTE $E0
  .BYTE $F9
  .BYTE $01
  .BYTE $00
  .BYTE $00
  .BYTE $08
  .BYTE $10
  .BYTE $20
  .BYTE $C0
  .BYTE $E0
  .BYTE $BF
  .BYTE $07
  .BYTE $0F
  .BYTE $0C
  .BYTE $10
  .BYTE $20
  .BYTE $00
  .BYTE $80
  .BYTE $87
  .BYTE $03
  .BYTE $04
  .BYTE $08
  .BYTE $00
  .BYTE $20
  .BYTE $00
  .BYTE $80
  .BYTE $E0
  .BYTE $F0
  .BYTE $F0
  .BYTE $78
  .BYTE $18
  .BYTE $04
  .BYTE $02
  .BYTE $01
  .BYTE $C0
  .BYTE $E0
  .BYTE $60
  .BYTE $10
  .BYTE $08
  .BYTE $04
  .BYTE $02
  .BYTE $01
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $01
  .BYTE $03
  .BYTE $03
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $01
  .BYTE $02
  .BYTE $05
  .BYTE $05
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $C0
  .BYTE $E0
  .BYTE $E0
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $C0
  .BYTE $20
  .BYTE $D0
  .BYTE $D0
  .BYTE $03
  .BYTE $01
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $05
  .BYTE $02
  .BYTE $01
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $E0
  .BYTE $C0
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $D0
  .BYTE $20
  .BYTE $C0
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00
  .BYTE $00

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Minimap NT LUT  [$B000 :: 0x27010]
;;
;;    This LUT contains the full name and attribute table for the minimap
;;  It is copied in full to the nametables when the minimap is drawn.

lut_MinimapNT:
  .INCBIN "bin/minimap_nt.nam"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Minimap BG CHR  [$B400 :: 0x27410]
;;
;;    This is the CHR for the non-map BG graphics for the minimap
;;  Due to the way it's loaded, each of these blocks must be aligned
;;  on a $100 byte page


  .ALIGN $100
chr_MinimapDecor:
  .INCBIN "bin/minimap_decor.chr"


  .ALIGN $100
chr_MinimapTitle:
  .INCBIN "bin/minimap_title.chr"




;; B980  ????  can't be unused... can it?   Sure isn't used by minimap

 .BYTE $00,$21,$11,$11,$02,$02,$04,$18,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
 .BYTE $00,$1F,$11,$11,$2D,$02,$04,$08,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
 .BYTE $00,$02,$1C,$04,$7F,$04,$04,$08,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
 .BYTE $00,$01,$29,$29,$02,$02,$04,$18,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
 .BYTE $00,$3F,$00,$3F,$04,$04,$04,$08,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
 .BYTE $00,$10,$10,$10,$18,$16,$10,$10,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
 .BYTE $00,$04,$7F,$04,$04,$04,$08,$30,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
 .BYTE $00,$00,$1E,$00,$00,$00,$00,$3F,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
 .BYTE $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
 .BYTE $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
 .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 .BYTE $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
 .BYTE $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
 .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 .BYTE $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
 .BYTE $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
 .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 .BYTE $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
 .BYTE $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
 .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 .BYTE $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Minimap Draw Title CHR  [$BB00 :: 0x27B10]
;;
;;    Progressively draws the title CHR for the minimap
;;  This drawing is done progressively while the PPU is on so that
;;  tiles appear to gradually fill the screen.
;;
;;    Because it's done when PPU is on, it must be done in VBlank.  Which
;;  means it needs to be reasonably fast... and you can't do too much at once.
;;  The game spreads the job out over 16 frames, drawing 1 byte of each tile's
;;  CHR each frame.  ($28 bytes drawn per frame).
;;
;;    CHR must have already been preloaded and prearranged so that this
;;  routine can simply load and draw it without having to do heavy computations.
;;  See Minimap_PrepTitleCHR for how that's accomplished.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Minimap_DrawTitleCHR:
    LDA #<mm_titlechr
    STA minimap_ptr
    LDA #>mm_titlechr
    STA minimap_ptr+1        ; load up the pointer to our pre-loaded CHR

    LDX #0                   ; X will be our frame counter.  We will spread this drawing
                             ;  out over 16 frames

  @FrameLoop:
    JSR WaitForVBlank_L      ; wait for VBlank
    LDY #0                   ; Y will be loop counter/index

    @SmallLoop:
      LDA lut_MinimapTitleCHRDest_hi, Y  ; load up the destination PPU address for this
      STA $2006                          ;  tile of the title graphic
      LDA lut_MinimapTitleCHRDest_lo, Y  ; get low byte as well
      CLC
      ADC lut_MinimapBitplane, X         ; and add bitplane/row info to low byte
      STA $2006                          ;  before writing it

      LDA (minimap_ptr), Y   ; get the preloaded CHR data
      STA $2007              ; and draw it

      INY                    ; inc loop counter/index
      CPY #$28
      BCC @SmallLoop         ; and keep looping until a single bitplane row has been drawn for $28 tiles

    LDA minimap_ptr          ; add $28 to our source pointer
    CLC                      ;   so that we load the next chunk of graphic data
    ADC #$28
    STA minimap_ptr
    LDA minimap_ptr+1
    ADC #0
    STA minimap_ptr+1

    LDA soft2000             ; reset the scroll
    STA $2000
    LDA #$00
    STA $2005
    LDA #$E8
    STA $2005

    JSR Minimap_DrawSFX       ; play ugly drawing sound effect
    INX
    CPX #16                   ; increment the frame loop counter
    BCC @FrameLoop            ; and loop until we do 16 frames (8 rows, 2 bitplanes per row = full tile)

    RTS                       ; then exit





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Minimap - Prep Title CHR  [$BB4E :: 0x27B5E]
;;
;;    Exact same idea as Minimap_PrepDecorCHR (see that routine for details)
;;
;;    The loop in this routine is constructed a bit differently because it needs
;;  to copy only $280 bytes instead of $300.  Other than that (and using a different
;;  src/dest buffers), the routines are identical
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


Minimap_PrepTitleCHR:
    LDA #<mm_titlechr
    STA tmp+2
    LDA #>mm_titlechr
    STA tmp+3            ; set (tmp+2) to point to our dest buffer

    LDX #0               ; X will be outer loop counter
    LDY #0               ; Y must stay at zero for indexing
  @OuterLoop:
    LDA lut_MinimapBitplane, X
    STA tmp                  ; set (tmp) to point to our source buffer
    LDA #>chr_MinimapTitle   ; and offset it by current row/bitplane
    STA tmp+1

    @InnerLoop:
      LDA (tmp), Y       ; copy a byte from source to dest
      STA (tmp+2), Y

      INC tmp+2          ; inc dest pointer
      BNE :+
        INC tmp+3

  :   LDA tmp            ; add $10 to low byte of source pointer
      CLC                ;  to have it look at the next tile
      ADC #$10
      STA tmp
      BCS @IncHigh       ; if it had carry, inc high byte of pointer

      BPL @InnerLoop     ; if result of low byte is positive ($00-$70) keep looping

      LDA tmp+1
      CMP #>(chr_MinimapTitle + $280)  ; otherwise (low byte is >= $80), check to see if high byte
                                       ; is at the end of our source buffer
      BCC @InnerLoop        ;  if not.. keep looping
      BCS @ExitInnerLoop    ;  if yes... break out of inner loop (always branches)

    @IncHigh:
      INC tmp+1          ; inc high byte of source pointer
      BNE @InnerLoop     ; and keep looping (always branches)

  @ExitInnerLoop:
    INX                  ; increment row/bitplane counter
    CPX #$10             ; and keep looping until we do all $10 of them
    BCC @OuterLoop

    RTS                  ; then exit!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Minimap Title CHR Dest LUT  [$BB8A :: 0x27B9A]
;;
;;    There are $28 tiles that make up the "Final Fantasy" title that is displayed
;;  in the minimap.  These tiles (along with those for the dragon graphic thing)
;;  are scattered around in the pattern table so as to not interefere with the map's CHR.
;;
;;    These LUTs tell where the game is supposed to put each of those $28 tiles in
;;  the pattern tables.  Each entry is the desired tile ID of each graphic left
;;  shift 4 (to make it a CHR address).  One table has the low byte and the other
;;  has the high byte.

lut_MinimapTitleCHRDest_lo:
  .BYTE <$0160,<$0260,<$02F0,<$0400, <$0450,<$0480,<$0490,<$0500
  .BYTE <$0540,<$0550,<$0560,<$0570, <$0580,<$0590,<$0600,<$0610
  .BYTE <$0640,<$0650,<$0660,<$0670, <$0680,<$0690,<$06A0,<$07B0
  .BYTE <$07C0,<$07F0,<$08F0,<$0900, <$0910,<$0A00,<$0A70,<$0AA0
  .BYTE <$0B00,<$0B80,<$0B90,<$0BA0, <$0C00,<$0C20,<$0C90,<$0EF0

lut_MinimapTitleCHRDest_hi:
  .BYTE >$0160,>$0260,>$02F0,>$0400, >$0450,>$0480,>$0490,>$0500
  .BYTE >$0540,>$0550,>$0560,>$0570, >$0580,>$0590,>$0600,>$0610
  .BYTE >$0640,>$0650,>$0660,>$0670, >$0680,>$0690,>$06A0,>$07B0
  .BYTE >$07C0,>$07F0,>$08F0,>$0900, >$0910,>$0A00,>$0A70,>$0AA0
  .BYTE >$0B00,>$0B80,>$0B90,>$0BA0, >$0C00,>$0C20,>$0C90,>$0EF0



;; unused

  .BYTE $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
  .BYTE $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
  .BYTE $FF,$FF,$FF,$FF,$6D,$76


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Enter Minimap  [$BC00 :: 0x27C10]
;;
;;    Ahhh... the minimap!  On one hand I think it's very clever... on the other hand
;;  the way it draws is CRIMINALLY inefficient.
;;
;;    Usually when you think of NES drawing, you think of a (more or less) fixed pattern
;;  table that contains your tiles, and you arrange those tiles on the screen with
;;  the name table.  However... the minimap is done somewhat differently.
;;
;;    The world map is 256x256 tiles, but it draws it as a 128x128 pixel image (so each
;;  pixel represents 4 tiles in a 2x2 arrangement).  Somewhat coincidentally... the BG pattern
;;  tables (16x16 tiles) also measure 128x128 pixels!  So the game simply uses the BG
;;  pattern tables as a canvas on which to draw the minimap... and has a fixed nametable
;;  placing the tiles on the screen.
;;
;;    Tiles on the pattern table that are nothing but water get drawn over with other
;;  supporting graphics (title and decoration graphics) after map drawing is complete.
;;
;;
;;    So yeah... that much is pretty slick.  But what is incredibly dumb about the
;;  minimap is that it wastes a LOT of ROM space and CPU time.
;;
;;    Since the drawing is done with the PPU on (so you can see the map progressively being
;;  drawn onscreen as sort of a neat visual effect), all PPU writes must be performed during
;;  VBlank.  This means that the number of writes you can do is very limited... and you have
;;  to end up wasting most of your CPU time waiting for VBlank to happen.  Now most people
;;  would use that down time to decompress map tiles and get them ready to be drawn next frame
;;  but for whatever reason the game doesn't!  Instead of the simple and expected 
;;  "load, draw, load, draw" pattern which uses the rendering time to pre-load graphics
;;  for next VBlank -- this routine will actually load EIGHT rows all at once, then draw
;;  one row a frame for 8 frames ("load, load, load, load...  draw, draw, draw, draw....").
;;
;;    Now...you have to wait for a frame between draws anyway (it's required since writes have
;;  to fall in VBlank) -- but the loading can be done any time.  So it makes more sense to spread
;;  it out between draws when you're doing nothing but waiting.  But NO!  The game crams a bunch
;;  of its loading in a SINGLE frame rather than spreading it out across 8 frames.
;;
;;    This results in an EXTRA and unnecessary pause every 8 rows while the CPU is busy wasting time
;;  loading tiles.
;;
;;
;;    As for wasting space... the game doesn't use the existing map decompression routine!
;;  I still don't know why.  Instead the game has ****TWO**** of its own routines for decompressing
;;  map tiles.  TWO.  I kid you not.  This makes a total of 3 versions of the exact same routine.
;;  But I rant about that in that routine (see MinimapDecompress).
;;
;;
;;
;;    Anyway... the drawing methods here will be confusing unless you're familiar with how NES
;;  2bpp graphics are layed out.  I won't get into it here... but basically each 8x8 tile is 16 bytes...
;;  the first 8 bytes ($x0-$x7) are the low bitplane, and the next 8 ($x8-$xF) are the high bitplane.
;;  each byte in the bitplane is a row of 8 pixels (each bit of the byte is a pixel)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



EnterMinimap:
    LDA #BANK_THIS
    STA cur_bank           ; set cur bank (required for music routine)

    LDA #$00
    STA $2001
    STA $4015              ; turn off PPU and APU

    LDA #$41               ; switch to music track $41 (crystal theme)
    STA music_track        ;   but it's not heard until after all drawing is complete (music routine isn't called)

    JSR Minimap_PrepDecorCHR   ; Load decoration CHR to RAM (those dragon graphic things)
    JSR Minimap_PrepTitleCHR   ; And the "Final Fantasy" title text
    JSR Minimap_YouAreHere     ; Load the "You are here" graphic -- clear OAM, and draw the "you are here" sprite
    JSR Minimap_FillNTPal      ; Fill the nametable and palettes

    LDA #0
    STA mm_maprow          ; start decompression tiles from row 0 (top row)

    LDA #>$0000            ; set high byte of dest PPU addr.
    STA minimap_ptr+1

  @MainLoop:
    LDA #0                 ; reset low byte of PPU addr to 0
    STA minimap_ptr

    @InnerLoop:            ;   Inner loop is run 8 times -- each time loads a single row of pixels
      JSR Minimap_PrepRow  ; Load a single row of 128 pixels from 2 rows of map data

      INC mm_maprow        ; increment map row counter by 2
      INC mm_maprow

      LDA minimap_ptr      ; increment dest PPU address by 1 (next row of pixels)
      CLC
      ADC #1
      AND #$07             ; and mask with 7 (0-7)
      STA minimap_ptr
      BNE @InnerLoop       ; once it wraps from 7->0, we've filled 256 bytes of graphic data (8 rows of pixels)

    JSR Minimap_DrawRows       ; draw those 8 rows of pixels
    INC minimap_ptr+1          ; increment high byte of PPU dest

    LDA minimap_ptr+1
    CMP #>$1000
    BNE @MainLoop              ; loop until PPU dest=$1000 (filling entire left pattern table)

      ;  now map drawing is done

    JSR Minimap_DrawDecorCHR   ; draw the dragon decorations (previously loaded)
    JSR Minimap_DrawTitleCHR   ; and the "Final Fantasy" title graphics (also previously loaded)


  @ExitLoop:
    JSR MinimapFrame      ; do a frame... animating sprite palettes and whatnot

    LDA joy_a
    ORA joy_b
    BEQ @ExitLoop         ; and simply loop until the user presses A or B

    RTS                   ; at which point... exit!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Minimap Draw Decorative CHR  [$BC55 :: 0x27C65]
;;
;;    EXACTLY the same as Minimap_DrawTitleCHR (see that routine for
;;  details).  Only difference here is we're drawing the CHR for the 
;;  dragon decoration graphic, and so we use different LUTs.  Also, this
;;  graphic consists of $30 tiles instead of $28, so a bit more is drawn
;;  each frame.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Minimap_DrawDecorCHR:
    LDA #<mm_decorchr
    STA minimap_ptr
    LDA #>mm_decorchr
    STA minimap_ptr+1        ; load up the pointer to our pre-loaded CHR

    LDX #0                   ; X will be our frame counter.  We will spread this drawing
                             ;  out over 16 frames

  @FrameLoop:
    JSR WaitForVBlank_L      ; wait for VBlank
    LDY #0                   ; Y will be loop counter/index

    @SmallLoop:
      LDA lut_MinimapDecorCHRDest_hi, Y  ; load up the destination PPU address for this
      STA $2006                          ;  tile of the title graphic
      LDA lut_MinimapDecorCHRDest_lo, Y  ; get low byte as well
      CLC
      ADC lut_MinimapBitplane, X         ; and add bitplane/row info to low byte
      STA $2006                          ;  before writing it

      LDA (minimap_ptr), Y   ; get the preloaded CHR data
      STA $2007              ; and draw it

      INY                    ; inc loop counter/index
      CPY #$30
      BCC @SmallLoop         ; and keep looping until a single bitplane row has been drawn for $30 tiles

    LDA minimap_ptr          ; add $30 to our source pointer
    CLC                      ;   so that we load the next chunk of graphic data
    ADC #$30
    STA minimap_ptr
    LDA minimap_ptr+1
    ADC #0
    STA minimap_ptr+1

    LDA soft2000             ; reset the scroll
    STA $2000
    LDA #$00
    STA $2005
    LDA #$E8
    STA $2005

    JSR Minimap_DrawSFX       ; play ugly drawing sound effect
    INX
    CPX #16                   ; increment the frame loop counter
    BCC @FrameLoop            ; and loop until we do 16 frames (8 rows, 2 bitplanes per row = full tile)

    RTS                       ; then exit


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Minimap - Prep Decorative CHR  [$BCA3 :: 0x27CB3]
;;
;;    Preps the decorative CHR (the little dragon graphic thing that goes
;;  in the corners of the minimap) by re-arranging it to its necessary
;;  format, then dumping to a temporary RAM buffer, so it can be quickly copied to
;;  the pattern tables later.
;;
;;    Since each tile is drawn bitplane and row at a time, there is some necessary
;;  interleaving to perform here.  To speed up the routine that actually draws
;;  this data to the pattern tables, we want to do all the calculations here, and store the
;;  data in the temp buffer so that bytes can just be read and output in the order they're
;;  given.  We'll only be drawing one byte per tile at a time (rather than 16 bytes
;;  in a row for each tile -- which is how the pattern tables work).
;;
;;    Too hard to explain how this works in words... so here's an example:
;;
;;  CHR data as it needs to appear in the PPU pattern tables:
;;  00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F    (all 16 bytes of tile 0's graphic)
;;  10 11 12 13 14 15 16 17 18 19 1A 1B 1C 1D 1E 1F    (followed by all 16 bytes of tile 1's graphic)
;;  20 21 22 23 24 25 26 27 28 29 2A 2B 2C 2D 2E 2F    (followed by tile 2, etc)
;;
;;   But to draw this one bitplane row at a time, we need to have it stored
;;  in our RAM buffer as the following:
;;  00 10 20 08 18 28 01 11 21 09 19 29 03 13 23 ...   (all top row low bitplanes
;;                                                      followed by top row high bitplanes
;;                                                      followed by row 1 low bitplanes... etc)
;;
;;    That is the interleaving this routine is performing -- and why it isn't just a straight
;;  copy.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

Minimap_PrepDecorCHR:
    LDA #<mm_decorchr
    STA tmp+2
    LDA #>mm_decorchr
    STA tmp+3              ; set (tmp+2) to our dest pointer (RAM to receive interleaved graphics)

    LDX #0          ; X is the outer loop counter, and indicates which row/bitplane to output next
    LDY #0          ; Y needs to always be zero for indexing

  @OuterLoop:
    LDA lut_MinimapBitplane, X
    STA tmp                  ; Load source pointer (tmp) to point to chr_MinimapDecor
    LDA #>chr_MinimapDecor   ;   offset by the row/bitplane we're to be decoded
    STA tmp+1                ;   from lut_MinimapBitplane)

    @InnerLoop:
      LDA (tmp), Y       ; copy a single byte from source
      STA (tmp+2), Y     ;  to dest

      INC tmp+2          ; increment dest pointer by 1
      BNE :+
        INC tmp+3

  :   LDA tmp            ; increment source pointer by $10 (to look at the same
      CLC                ;  row/bitplane on the next tile)
      ADC #$10
      STA tmp
      BCC @InnerLoop     ; if there was no wrap -- continue looping.  Otherwise...

      INC tmp+1            ; include the carry in the high byte of src pointer
      LDA tmp+1            ; check it to see if we copied data for $30 tiles
      CMP #>(chr_MinimapDecor + $300)
      BCC @InnerLoop       ; if not, keep looping  ($30 iterations)

    INX                 ; once the inner loop exits, we finished a single row/bitplane
    CPX #$10            ;  INX to start the next row/bitplane.
    BCC @OuterLoop      ; and continue outer loop until we do all $10 bytes of each tile
                        ; (outer loop = $10 iterations ... * $30 inner iterations =
                        ;     total $300 bytes copied)

    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Minimap CHR bitplane LUT  [$BCD9 :: 0x27CE9]
;;
;;    CHR for the minimap support graphics (title and decorations) is drawn progressively.
;;  CHR for each tile is drawn one row at a time -- or more specificlaly, one row of
;;  one bitplane at a time.
;;
;;    This progressive drawing is performed in a loop with an upcounter.  That counter
;;  is run through this LUT to know which bitplane of which row to draw during which
;;  loop iteration
;;
;;    You can scramble this table up in any order to change how the title and decorations
;;  get drawn without changing the final result.  As long as every value between 00-0F
;;  exists in this table.

lut_MinimapBitplane:

;      bitplane
;        lo  hi
  .BYTE $00,$08   ; top row
  .BYTE $01,$09
  .BYTE $02,$0A
  .BYTE $03,$0B
  .BYTE $04,$0C
  .BYTE $05,$0D
  .BYTE $06,$0E
  .BYTE $07,$0F   ; bottom row


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Minimap Decoration CHR Dest LUT  [$BCE9 :: 0x27CF9]
;;
;;    Exactly the same as lut_MinimapDecorCHRDest (see that LUT description for
;;  details).  Except this is for the 'dragon' decoration that appears on the map.
;;  It consists of $30 tiles



lut_MinimapDecorCHRDest_lo:
  .BYTE <$0010,<$0020,<$0030,<$0040, <$0050,<$0060,<$0070,<$0080
  .BYTE <$00B0,<$00C0,<$00D0,<$00E0, <$00F0,<$0700,<$0710,<$0720
  .BYTE <$0730,<$0740,<$0750,<$0760, <$0800,<$0810,<$0820,<$0830
  .BYTE <$0840,<$0D00,<$0D10,<$0D20, <$0D30,<$0D40,<$0E00,<$0E10
  .BYTE <$0E20,<$0E30,<$0E40,<$0F00, <$0F10,<$0F20,<$0F30,<$0F40
  .BYTE <$0F50,<$0F60,<$0F70,<$0F80, <$0F90,<$0FA0,<$0FB0,<$0FC0

lut_MinimapDecorCHRDest_hi:
  .BYTE >$0010,>$0020,>$0030,>$0040, >$0050,>$0060,>$0070,>$0080
  .BYTE >$00B0,>$00C0,>$00D0,>$00E0, >$00F0,>$0700,>$0710,>$0720
  .BYTE >$0730,>$0740,>$0750,>$0760, >$0800,>$0810,>$0820,>$0830
  .BYTE >$0840,>$0D00,>$0D10,>$0D20, >$0D30,>$0D40,>$0E00,>$0E10
  .BYTE >$0E20,>$0E30,>$0E40,>$0F00, >$0F10,>$0F20,>$0F30,>$0F40
  .BYTE >$0F50,>$0F60,>$0F70,>$0F80, >$0F90,>$0FA0,>$0FB0,>$0FC0



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Minimap Frame  [$BD49 :: 0x27D59]
;;
;;     Does a frame for the minimap!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MinimapFrame:
    JSR WaitForVBlank_L    ; wait for VBlank
    LDA #>oam              ; Sprite DMA
    STA $4014

    JSR DrawPalette_L      ; draw the palette
    LDA #$1E               ; turn PPU on
    STA $2001

    LDA soft2000           ; reset scroll to $00,$E8
    STA $2000
    LDA #$00
    STA $2005
    LDA #$E8
    STA $2005

    JSR CallMusicPlay_L    ; keep the music playing

    LDA #0
    STA joy_a              ; clear A and B button catchers
    STA joy_b
    JSR UpdateJoy_L        ; then update joypad data

    INC framecounter       ; inc the frame counter

    LDA framecounter
    AND #$08               ; use bit 3 of frame counter to toggle between
    BNE :+
      LDA #$30             ; $37 (tan color)
:   EOR #$07               ; and $0F (black)  every 8 frames
    STA cur_pal+$11        ; this is the color for the "you are here" sprite

    LDA framecounter       ; use bits 4,5 of frame counter
    AND #$30               ; to cycle between $00,$10,$20,$0F
    CMP #$30               ;  changing color every 16 frames
    BNE :+
      LDA #$0F
:   STA cur_pal+$12        ; this is the color for the town/dungeon marker sprites

    RTS                    ; and exit!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Minimap Draw Rows  [$BD91 :: 0x27DA1]
;;
;;    Draws 8 preloaded rows of pixels to the PPU pattern tables.
;;  Each row is drawn in its own VBlank, so the drawing happens progressively.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


Minimap_DrawRows:
    LDA #0
    STA mm_pixrow            ; start drawing pixel row 0, increment by 1 row each loop

    @MainLoop:
      LDX mm_pixrow          ; put row in X -- X will be the loop up counter and source index
      JSR WaitForVBlank_L    ; wait for VBlank

      @RowLoop:
        LDA minimap_ptr+1    ; set PPU address
        STA $2006
        STX $2006

        LDA mm_drawbuf, X    ; get graphic from drawing buffer
        STA $2007            ; and draw it

        TXA
        CLC
        ADC #$08             ; add 8 to the index (moves to high bitplane if we're on the low bitplane
        TAX                  ;  (or to low plane of next tile if on high plane.  Does not change rows)

        BCC @RowLoop         ; keep looping until X wraps ($20 interations -- $10 tiles)

      LDA soft2000          ; drawing for this row is done -- reset the scroll
      STA $2000
      LDA #$00
      STA $2005
      LDA #$E8
      STA $2005

      JSR Minimap_DrawSFX   ; play the ugly drawing sound effect

      LDA mm_pixrow         ; add 1 to our row counter
      CLC                   ; and mask low bits (0-7)
      ADC #1
      AND #$07
      STA mm_pixrow

      BNE @MainLoop         ; once it wraps 7->0, we've drawn all 8 rows, so exit

    RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Minimap - Drawing Sound effect [$BDCD :: 0x27DDD]
;;
;;    Plays that ugly sound effect you hear when the map is drawing.  Just
;;  plays a steady tone, but when called every frame, the phase resets each
;;  frame, which is why it sounds almost like it's buzzing.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Minimap_DrawSFX:
    LDA #$02
    STA $4015            ; silence all channels except for square 2

    LDA #%00110110
    STA $4004            ; 12.5% duty (harshest), volume=6
    LDA #$7F
    STA $4005            ; disable sweep
    LDA #$60
    STA $4006            ; play tone at F=$060
    LDA #$00
    STA $4007

    RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Minimap Prep Row  [$BDE7 :: 0x27DF7]
;;
;;    Decompresses two rows of map data, and 'merges' them into 1 row of
;;  pixels.  The pixels are dumped into the intermediate buffer 'mm_drawbuf'.
;;  Also, dungeon marker sprites are generated for tiles which require them
;;  in this routine.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


Minimap_PrepRow:
    JSR CallMinimapDecompress    ; decompress 2 rows of map data
    LDY #0                       ; Y will be the x coord (column) counter

  @MainLoop:
    LDA #8            ; tmp+7 is a counter to keep track of the number of bits
    STA tmp+7         ;  that have yet to be shifted into the output CHR bytes
                      ; since each byte is actually 8 pixels on a bitplane... multiple pixels
                      ; must be rotated into the same byte to be output as graphics.

    @RotateLoop:
      LDX mm_mapbuf, Y            ; here we get the minimap tileset data for 4 seperate
      LDA lut_MinimapTileset, X   ; map tiles (in a 2x2 square) and OR them together to combine
      LDX mm_mapbuf+1, Y          ; them to a single pixel.
      ORA lut_MinimapTileset, X   ;  This scales the map down from 256x256 tiles
      LDX mm_mapbuf2, Y           ;  to 128x128 pixels
      ORA lut_MinimapTileset, X
      LDX mm_mapbuf2+1, Y         ; after all this, A contains the output pixel (low 2 bits)
      ORA lut_MinimapTileset, X   ;  and bit 2 indicates whether or not a marker sprite needs to be placed here

      LSR A           ; shift out low bit
      ROL mm_bplo     ; rotate it into the low bitplane

      LSR A           ; shift out the high bit
      ROL mm_bphi     ; rotate into high bitplane

      LSR A                   ; shift out marker sprite flag
      BCC :+                  ; if set....
        JSR DrawDungeonSprite ; ... generate a dungeon marker sprite

  :   INY             ; increment our X coord by 2
      INY
      DEC tmp+7       ; decrement our bit counter
      BNE @RotateLoop ; if more bits needed to fill our bitplanes... keep going

      ; @RotateLoop exits when 8 pixels have been shifted into both high and low
      ; bitplanes.

    LDX minimap_ptr      ; use low byte of dest pointer as index for draw buffer

    LDA mm_bplo          ; copy both bitplanes to the appropriate areas of the buffer
    STA mm_drawbuf, X
    LDA mm_bphi
    STA mm_drawbuf+8, X

    LDA minimap_ptr      ; add $10 to low byte of pointer (look at next tile graphic)
    CLC
    ADC #$10
    STA minimap_ptr

    BCC @MainLoop        ; if there was no carry, keep looping
                         ;  otherwise, if there was carry, we have completed a single row
                         ;  from 16 tiles (128 pixels).  So this routine has completed its task
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Draw Dungeon Sprite  [$BE30 :: 0x27E40]
;;
;;    Draws a town/dungeon marker sprite at given coords
;;
;;  IN:  mm_maprow = y coord on world map
;;               Y = x coord on world map
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DrawDungeonSprite:
    LDA sprindex     ; Add 4 to the sprite index (drawing a single sprite -- 4 bytes)
    CLC              ;  do this before drawing the sprite to leave sprite 0 alone
    ADC #$04         ;  since that is the party position sprite
    STA sprindex
    TAX              ; put sprite index in X for indexing

    LDA mm_maprow    ; get Y coord
    LSR A            ; divide by 2 (minimap display is 128x128 -- world map is 256x256)
    CLC
    ADC #$34         ; add $34 to offset the sprite to the start of the map on the screen
    STA oam, X       ; write this as Y coord of sprite

    LDA #$81
    STA oam+1, X     ; use tile $81

    LDA #$00
    STA oam+2, X     ; attributes (palette 0, no flipping, foreground priority)

    TYA              ; get the X coord
    LSR A            ; divide by 2 for same reason
    CLC
    ADC #$3D         ; add $3D to offset it to start of the map
    STA oam+3, X     ; write as X coord

    RTS              ; done!




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Minimap - Fill NT and Palette [$BE54 :: 0x27E64]
;;
;;    Fills the nametable for the minimap.
;;  Also clears all of CHR-RAM for the BG, loads and draws the palette,
;;  and loads CHR for the sprites.

Minimap_FillNTPal:
    LDA #$08           ; write to soft2000 to clear NT scroll
    STA soft2000

   ; 
   ; draw the NT data at lut_MinimapNT
   ;

    LDA $2002          ; reset PPU toggle
    LDA #>$2000        ; set PPU address to $2000  (start of nametables)
    STA $2006
    LDA #<$2000
    STA $2006

    LDA #<lut_MinimapNT
    STA tmp
    LDA #>lut_MinimapNT ; load up the pointer to the LUT containing the minimap NT
    STA tmp+1           ;  data.  Store the pointer in (tmp)

    LDX #$04            ; X is high byte of loop counter
    LDY #$00            ; Y is low byte of counter and index
   @NTLoop:
       LDA (tmp), Y     ; get byte from LUT
       STA $2007        ; draw it
       INY              ; inc source index
       BNE @NTLoop      ; if it wrapped....
      INC tmp+1         ; inc high byte of source pointer
      DEX               ; dec high byte of loop counter
      BNE @NTLoop       ; and keep looping until expires ($400 iterations)

   ;
   ; clear the BG pattern table
   ;

    LDA $2002           ; reset PPU toggle
    LDA #>$0000         ; set PPU address to $0000 (start of BG pattern table)
    STA $2006
    LDA #<$0000
    STA $2006

                        ; A=0
    LDY #$10            ; Y is high byte of loop counter
    LDX #$00            ; X is low byte
   @ClearCHRLoop:
       STA $2007           ; zero out the pattern tables
       INX
       BNE @ClearCHRLoop
      DEY
      BNE @ClearCHRLoop    ; $1000 iterations (full BG pattern table)

   ;
   ; load CHR for sprites ("you are here" mark, and town/dungeon points)
   ;

    LDA $2002          ; reset PPU toggle
    LDA #>$1800        ; PPU address = $1800  (start of CHR for sprite tile $80)
    STA $2006
    LDA #<$1800
    STA $2006

    LDX #$00
   @SpriteCHRLoop:
      LDA lut_MinimapSprCHR, X
      STA $2007                      ; copy the CHR to the PPU
      INX                            ; inc loop counter
      CPX #$20
      BCC @SpriteCHRLoop             ; loop until $20 bytes copied (2 tiles)

   ;
   ; load BG palettes for this screen
   ;

    LDX #$0F
   @BGPalLoop:
      LDA lut_MinimapBGPal, X   ; copy color from LUT
      STA cur_pal, X            ;  to palette
      DEX
      BPL @BGPalLoop            ; loop until X wraps ($10 iterations)

    LDA cur_pal           ; copy the background color to the mirrored copy in the sprite palette
    STA cur_pal+$10       ;  this ensures the BG color will be what's desired when the palette is drawn

    LDA #$0F
    STA cur_pal+$11       ; start the "you are here" sprite
    STA cur_pal+$12       ;  and the town/dungeon marker sprite at $0F black

   ;
   ;  Do last minute PPU stuff before exiting
   ;

    JSR WaitForVBlank_L   ; wait for VBlank
    LDA #>oam             ; then do sprite DMA
    STA $4014

    JSR DrawPalette_L     ; draw the palette

    LDA soft2000
    STA $2000             ; set NT scroll and pattern page assignments
    LDA #$0A
    STA $2001             ; turn on BG rendering, but leave sprites invisible for now

    LDA #$00              ; set scroll to $00,$E8  (8 pixels up from bottom of the NT)
    STA $2005             ;   since there's vertical mirroring, this scrolls up 8 pixels,
    LDA #$E8              ;   which makes the screen appear to be 8 pixels lower than
    STA $2005             ;   what you might expect.  This centers the image on the screen a bit better
                          ; The NT could've just been drawn 1 tile down.. but that would mess with
                          ; attribute alignment

    RTS                   ; then exit!


;;  unused space
  .BYTE 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0
  .BYTE 0,0,0,0, 0



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Minimap Sprite CHR  [$BF00 :: 0x27F10]
;;    CHR for sprites on the minimap

lut_MinimapSprCHR:
  .INCBIN "bin/minimap_sprite.chr"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BG palette for minimap  [$BF20 :: 0x27F30]

lut_MinimapBGPal:
  .BYTE $02,$1B,$38,$2B, $02,$04,$37,$0F, $02,$28,$18,$0F, $02,$24,$1A,$30



;; unused
  .BYTE 0,0,0,0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Minimap - Draw "You Are Here" sprite [$BF34 :: 0x27F44]
;;
;;     Clears OAM and draws the "you are here" marker sprite
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


Minimap_YouAreHere:
    LDX #0
    STX sprindex      ; clear the sprite index

    LDA #$F8          ; flood OAM with $F8 (to clear it -- moves all sprites offscreen, making them invisible)
   @Loop:
      STA oam, X
      INX
      BNE @Loop       ; $100 iterations (all of OAM)

    LDA #$08
    STA soft2000      ; clear NT scroll (seems inapporpriate to do it here, but I guess it doesn't hurt)

    LDA #$80
    STA oam+1         ; "you are here" sprite uses tile $80

    LDA ow_scroll_x   ; get scroll X
    CLC
    ADC #$07          ; add 7 to get player position
    LSR A             ; divide that by 2 to scale to minimap (world map is 256x256 -- minimap is 128x128)
    CLC
    ADC #$3D          ; add $3D to offset it so it starts at the start of the map
    STA oam+3, X      ; record as X coord

    LDA ow_scroll_y   ; do the same to get Y coord
    CLC
    ADC #$07
    LSR A
    CLC
    ADC #$34          ; but add a little less ($34 instead of $3D)
    STA oam+0         ; record as Y coord

    LDA #$00
    STA oam+2         ; set sprite attributes (palette 0, no flipping, foreground priority)

    RTS               ; and exit!



  ; unused
  .BYTE 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0
  .BYTE 0,0,0,0, 0,0,0,0, 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Minimap Tileset color assignment LUT  [$BF80 :: 0x27F90]
;;
;;    Each entry in this table coresponds to an overworld tile.
;;  This table specifies what color (of the 4 colors available
;;  colors) this tile will be represented by in the minimap.
;;
;;    The low 2 bits specify the color... bit 2 ($4), is set if
;;  the tile represents a town/dungeon and indicates that a sprite
;;  should be drawn on that tile.
;;

lut_MinimapTileset:
  .BYTE 1,5,5,1,1,1,1,1,1,1,1,1,1,1,6,0
  .BYTE 1,1,1,1,1,1,0,0,0,1,1,5,5,6,2,3
  .BYTE 1,3,1,1,1,1,1,0,1,5,5,5,1,1,1,5
  .BYTE 1,1,5,1,5,5,2,2,5,5,5,1,3,3,3,1
  .BYTE 0,0,2,2,0,2,4,1,1,5,5,1,5,5,5,1
  .BYTE 0,0,2,2,1,1,1,5,5,1,5,1,1,5,1,1
  .BYTE 1,1,1,1,5,5,5,5,5,5,5,3,5,5,5,3
  .BYTE 1,1,1,1,1,1,1,1,1,2,0,3,3,3,3,3




