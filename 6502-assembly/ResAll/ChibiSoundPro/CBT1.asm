;CBT,CMT,ChibiTracker Version 1.0 ASM Song
;CBT,VER,1.0

;CBT,FMT,db,  db
;CBT,FMT,dw,  dw
;CBT,FMT,hex,$

;CBT,VAR,SongName,MySong
;CBT,VAR,Channels,3
;CBT,VAR,RepeatPoint,0

;CBT,HED
MySong:
  db 3		;Channels
  db 0		;RepPoint
  db 3		;SongSpeed
  dw MySong
  dw MySong_PatternList
  dw MySong_InstrumentList
  dw MySong_Sequence_0
  dw MySong_Sequence_1
  dw MySong_Sequence_2
;CBT,END

;CBT,SEQ,0
MySong_Sequence_0:
  db 0,0,3,4,3,4,6,6,7,7,7,7,8,8,255
;CBT,END

;CBT,SEQ,1
MySong_Sequence_1:
  db 1,1,5,5,5,5,5,5,5,5,5,5,1,1,255
;CBT,END

;CBT,SEQ,2
MySong_Sequence_2:
  db 2,2,2,2,2,2,2,2,2,2,2,2,2,2,255
;CBT,END

;CBT,PAT,0
MySong_Pattern_0:
  db 2,$0D,$34,$0B,$03,$0E,$F0,$00
  db 2,$0D,$34,$0B,$03,$00
  db 2,$0D,$32,$0B,$03,$00
  db 2,$0D,$31,$0B,$03,$00
  db 2,$0D,$34,$0B,$03,$00
  db 2,$0D,$34,$0B,$03,$00
  db 2,$0D,$32,$0B,$03,$00
  db 2,$0D,$31,$0B,$03,$00
  db 2,$0D,$32,$0B,$03,$00
  db 2,$0D,$32,$0B,$03,$00
  db 2,$0D,$30,$0B,$03,$00
  db 2,$0D,$2E,$0B,$03,$00
  db 2,$0D,$32,$0B,$03,$00
  db 2,$0D,$32,$0B,$03,$00
  db 2,$0D,$30,$0B,$03,$00
  db 2,$0D,$2E,$0B,$03,$00
  db 1,$10,$00
;CBT,END

;CBT,PAT,1
MySong_Pattern_1:
  db 4,$0D,$50,$0B,$03,$0E,$C0,$00
  db 4,$0D,$51,$0B,$03,$00
  db 4,$0D,$4E,$0B,$03,$00
  db 4,$0D,$4F,$0B,$03,$00
  db 4,$0D,$50,$0B,$03,$00
  db 4,$0D,$51,$0B,$03,$00
  db 4,$0D,$52,$0B,$03,$00
  db 4,$0D,$54,$0B,$03,$00
  db 1,$10,$00
;CBT,END

;CBT,PAT,2
MySong_Pattern_2:
  db 2,$0D,$30,$0B,$01,$0E,$F0,$00
  db 2,$0D,$3E,$0B,$00,$00
  db 2,$0D,$3E,$0B,$00,$00
  db 2,$0D,$3E,$0B,$00,$00
  db 2,$0D,$30,$0B,$01,$00
  db 2,$0D,$3E,$0B,$00,$00
  db 2,$0D,$3E,$0B,$00,$00
  db 2,$0D,$3E,$0B,$00,$00
  db 2,$0D,$30,$0B,$01,$00
  db 2,$0D,$30,$0B,$01,$00
  db 2,$0D,$3E,$0B,$00,$00
  db 2,$0D,$3E,$0B,$00,$00
  db 2,$0D,$30,$0B,$01,$00
  db 2,$0D,$3E,$0B,$00,$00
  db 2,$0D,$3E,$0B,$00,$00
  db 2,$0D,$3E,$0B,$00,$00
  db 1,$10,$00
;CBT,END

;CBT,PAT,3
MySong_Pattern_3:
  db 2,$0D,$2E,$0B,$03,$00
  db 2,$0D,$2E,$0B,$03,$00
  db 2,$0D,$2D,$0B,$03,$00
  db 10,$0D,$31,$0B,$03,$00
  db 2,$0D,$2D,$0B,$03,$00
  db 2,$0D,$2D,$0B,$03,$00
  db 2,$0D,$2C,$0B,$03,$00
  db 10,$0D,$30,$0B,$03,$00
  db 1,$10,$00
;CBT,END

;CBT,PAT,4
MySong_Pattern_4:
  db 2,$0D,$2C,$0B,$03,$00
  db 2,$0D,$2C,$0B,$03,$00
  db 2,$0D,$2B,$0B,$03,$00
  db 10,$0D,$2E,$0B,$03,$00
  db 2,$0D,$2D,$0B,$03,$00
  db 2,$0D,$2D,$0B,$03,$00
  db 2,$0D,$2C,$0B,$03,$00
  db 10,$0D,$30,$0B,$03,$00
  db 1,$10,$00
;CBT,END

;CBT,PAT,5
MySong_Pattern_5:
  db 2,$0D,$50,$0B,$03,$0E,$C0,$00
  db 2,$0E,$A0,$00
  db 2,$0D,$51,$0B,$03,$0E,$80,$00
  db 2,$0E,$40,$00
  db 2,$0D,$4E,$0B,$03,$0E,$C0,$00
  db 2,$0E,$A0,$00
  db 2,$0D,$4F,$0B,$03,$0E,$80,$00
  db 2,$0E,$40,$00
  db 2,$0D,$50,$0B,$03,$0E,$C0,$00
  db 2,$0E,$A0,$00
  db 2,$0D,$51,$0B,$03,$0E,$80,$00
  db 2,$0E,$40,$00
  db 2,$0D,$52,$0B,$03,$0E,$C0,$00
  db 2,$0E,$A0,$00
  db 2,$0D,$54,$0B,$03,$0E,$80,$00
  db 2,$0E,$40,$00
  db 1,$10,$00
;CBT,END

;CBT,PAT,6
MySong_Pattern_6:
  db 4,$0D,$34,$0B,$03,$00
  db 2,$0D,$34,$0B,$03,$00
  db 6,$0D,$34,$0B,$03,$00
  db 1,$0D,$30,$0B,$03,$00
  db 1,$0D,$31,$0B,$03,$00
  db 2,$0D,$34,$0B,$03,$00
  db 4,$0D,$32,$0B,$03,$00
  db 2,$0D,$32,$0B,$03,$00
  db 6,$0D,$32,$0B,$03,$00
  db 1,$0D,$2E,$0B,$03,$00
  db 1,$0D,$31,$0B,$03,$00
  db 2,$0D,$32,$0B,$03,$00
  db 1,$10,$00
;CBT,END

;CBT,PAT,7
MySong_Pattern_7:
  db 4,$0D,$34,$0B,$03,$0E,$F0,$00
  db 2,$0D,$34,$0B,$03,$00
  db 3,$0D,$34,$0B,$03,$00
  db 1,$0D,$60,$0B,$03,$0E,$C0,$00
  db 2,$0D,$5E,$0B,$03,$00
  db 1,$0D,$30,$0B,$03,$0E,$F0,$00
  db 1,$0D,$31,$0B,$03,$00
  db 2,$0D,$34,$0B,$03,$00
  db 4,$0D,$32,$0B,$03,$00
  db 2,$0D,$32,$0B,$03,$00
  db 3,$0D,$32,$0B,$03,$00
  db 1,$0D,$5C,$0B,$03,$0E,$C0,$00
  db 2,$0D,$5A,$0B,$03,$00
  db 1,$0D,$2E,$0B,$03,$0E,$F0,$00
  db 1,$0D,$31,$0B,$03,$00
  db 2,$0D,$32,$0B,$03,$00
  db 1,$10,$00
;CBT,END

;CBT,PAT,8
MySong_Pattern_8:
  db 2,$0D,$35,$0B,$03,$00
  db 2,$0D,$35,$0B,$03,$00
  db 2,$0D,$33,$0B,$03,$00
  db 2,$0D,$32,$0B,$03,$00
  db 2,$0D,$35,$0B,$03,$00
  db 2,$0D,$35,$0B,$03,$00
  db 2,$0D,$33,$0B,$03,$00
  db 2,$0D,$32,$0B,$03,$00
  db 2,$0D,$34,$0B,$03,$00
  db 2,$0D,$34,$0B,$03,$00
  db 2,$0D,$32,$0B,$03,$00
  db 2,$0D,$31,$0B,$03,$00
  db 2,$0D,$34,$0B,$03,$00
  db 2,$0D,$34,$0B,$03,$00
  db 2,$0D,$32,$0B,$03,$00
  db 2,$0D,$31,$0B,$03,$00
  db 1,$10,$00
;CBT,END

;CBT,VAR,InstrumentName,0,Ins0
;CBT,INS,0
MySong_Instrument_0:
  db 1,$0F,$00,$0D,$6E,$C1,$00
  db 1,$C0,$00
  db 0
;CBT,END

;CBT,VAR,InstrumentName,1,Ins1
;CBT,INS,1
MySong_Instrument_1:
  db 3,$0F,$00,$0D,$0A,$C1,$00
  db 3,$C0,$00
  db 0
;CBT,END

;CBT,VAR,InstrumentName,2,Ins2
;CBT,INS,2
MySong_Instrument_2:
  db 4,$0F,$00,$0D,$1E,$C1,$00
  db 1,$C0,$00
  db 0
;CBT,END

;CBT,VAR,InstrumentName,3,InsThree
;CBT,INS,3
MySong_Instrument_3:
  db 7,$0F,$00,$00
  db 0
;CBT,END

;CBT,HED
MySong_PatternList:
  dw MySong_Pattern_0
  dw MySong_Pattern_1
  dw MySong_Pattern_2
  dw MySong_Pattern_3
  dw MySong_Pattern_4
  dw MySong_Pattern_5
  dw MySong_Pattern_6
  dw MySong_Pattern_7
  dw MySong_Pattern_8

MySong_InstrumentList:
  dw MySong_Instrument_0
  dw MySong_Instrument_1
  dw MySong_Instrument_2
  dw MySong_Instrument_3
;CBT,END
;CBT,HED
MySong_END:
;CBT,END
