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
  db 4		;SongSpeed
  dw MySong
  dw MySong_PatternList
  dw MySong_InstrumentList
  dw MySong_Sequence_0
  dw MySong_Sequence_1
  dw MySong_Sequence_2
;CBT,END

;CBT,SEQ,0
MySong_Sequence_0:
  db 3,3,0,4,0,4,3,3,7,7,0,4,0,4,8,8,9,9,0,4,0,4,0,4,0,4,3,3,255
;CBT,END

;CBT,SEQ,1
MySong_Sequence_1:
  db 1,1,1,1,1,1,1,1,5,5,1,1,1,1,1,1,1,1,1,1,1,1,5,5,5,5,5,5,255
;CBT,END

;CBT,SEQ,2
MySong_Sequence_2:
  db 2,2,2,2,2,2,2,2,6,6,2,2,2,2,2,2,2,2,2,2,2,2,6,6,6,6,6,6,255
;CBT,END

;CBT,PAT,0
MySong_Pattern_0:
  db 2,$0D,$2A,$0B,$06,$0E,$F0,$00
  db 2,$0D,$2B,$0B,$06,$00
  db 2,$0D,$2C,$0B,$06,$00
  db 1,$0E,$E0,$00
  db 1,$0E,$D0,$00
  db 2,$0E,$00,$00
  db 6,$0D,$2E,$0B,$06,$0E,$D0,$00
  db 2,$0D,$2A,$0B,$06,$0E,$F0,$00
  db 2,$0D,$2B,$0B,$06,$00
  db 2,$0D,$2C,$0B,$06,$00
  db 1,$0E,$E0,$00
  db 1,$0E,$D0,$00
  db 3,$0E,$00,$00
  db 2,$0D,$2E,$0B,$06,$0E,$D0,$00
  db 3,$0D,$2E,$0B,$06,$00
  db 1,$10,$00
;CBT,END

;CBT,PAT,1
MySong_Pattern_1:
  db 3,$0D,$20,$0B,$08,$0E,$F0,$00
  db 3,$0D,$22,$0B,$08,$00
  db 3,$0D,$1E,$0B,$08,$00
  db 7,$0D,$1E,$0B,$08,$00
  db 3,$0D,$20,$0B,$08,$00
  db 3,$0D,$22,$0B,$08,$00
  db 3,$0D,$1E,$0B,$08,$00
  db 3,$0D,$1E,$0B,$08,$00
  db 4,$0D,$24,$0B,$08,$00
  db 1,$10,$00
;CBT,END

;CBT,PAT,2
MySong_Pattern_2:
  db 2,$0D,$1C,$0B,$01,$0E,$F0,$00
  db 2,$0D,$1C,$0B,$02,$00
  db 2,$0D,$20,$0B,$09,$00
  db 2,$0D,$1C,$0B,$02,$00
  db 2,$0D,$1C,$0B,$01,$00
  db 2,$0D,$1C,$0B,$01,$00
  db 2,$0D,$1C,$0B,$02,$00
  db 2,$0D,$1C,$0B,$02,$00
  db 2,$0D,$1C,$0B,$01,$00
  db 2,$0D,$1C,$0B,$02,$00
  db 2,$0D,$20,$0B,$09,$00
  db 2,$0D,$1C,$0B,$02,$00
  db 2,$0D,$1C,$0B,$01,$00
  db 2,$0D,$1C,$0B,$01,$00
  db 2,$0D,$1C,$0B,$02,$00
  db 2,$0D,$20,$0B,$09,$00
  db 1,$10,$00
;CBT,END

;CBT,PAT,3
MySong_Pattern_3:
  db 32,$0E,$00,$00
  db 1,$10,$00
;CBT,END

;CBT,PAT,4
MySong_Pattern_4:
  db 2,$0D,$2A,$0B,$06,$0E,$F0,$00
  db 2,$0D,$2B,$0B,$06,$00
  db 2,$0D,$2C,$0B,$06,$00
  db 1,$0E,$E0,$00
  db 1,$0E,$D0,$00
  db 2,$0E,$00,$00
  db 6,$0D,$2E,$0B,$06,$0E,$D0,$00
  db 2,$0D,$2A,$0B,$06,$0E,$F0,$00
  db 2,$0D,$2B,$0B,$06,$00
  db 2,$0D,$2C,$0B,$06,$00
  db 1,$0E,$E0,$00
  db 1,$0E,$D0,$00
  db 3,$0D,$32,$0B,$06,$0E,$F0,$00
  db 2,$0D,$32,$0B,$06,$00
  db 3,$0D,$34,$0B,$06,$00
  db 1,$10,$00
;CBT,END

;CBT,PAT,5
MySong_Pattern_5:
  db 1,$0D,$20,$0B,$08,$00
  db 2,$0D,$50,$0B,$03,$00
  db 2,$0D,$22,$0B,$08,$00
  db 1,$0D,$50,$0B,$03,$00
  db 3,$0D,$1E,$0B,$08,$00
  db 4,$0D,$1E,$0B,$08,$00
  db 3,$0D,$50,$0B,$03,$00
  db 1,$0D,$20,$0B,$08,$00
  db 2,$0D,$56,$0B,$03,$00
  db 2,$0D,$22,$0B,$08,$00
  db 1,$0D,$56,$0B,$03,$00
  db 3,$0D,$1E,$0B,$08,$00
  db 3,$0D,$1E,$0B,$08,$00
  db 1,$0D,$24,$0B,$08,$00
  db 3,$0D,$56,$0B,$03,$00
  db 1,$10,$00
;CBT,END

;CBT,PAT,6
MySong_Pattern_6:
  db 2,$0D,$1C,$0B,$01,$00
  db 1,$0D,$1C,$0B,$02,$00
  db 1,$0D,$4E,$0B,$03,$00
  db 2,$0D,$20,$0B,$09,$00
  db 1,$0D,$1C,$0B,$02,$00
  db 1,$0D,$4E,$0B,$03,$00
  db 1,$0D,$1C,$0B,$01,$00
  db 1,$0D,$50,$0B,$03,$00
  db 1,$0D,$1C,$0B,$01,$00
  db 1,$0D,$4E,$0B,$03,$00
  db 2,$0D,$1C,$0B,$02,$00
  db 1,$0D,$1C,$0B,$02,$00
  db 1,$0D,$4E,$0B,$03,$00
  db 2,$0D,$1C,$0B,$01,$00
  db 1,$0D,$1C,$0B,$02,$00
  db 1,$0D,$54,$0B,$03,$00
  db 2,$0D,$20,$0B,$09,$00
  db 1,$0D,$1C,$0B,$02,$00
  db 1,$0D,$54,$0B,$03,$00
  db 1,$0D,$1C,$0B,$01,$00
  db 1,$0D,$56,$0B,$03,$00
  db 1,$0D,$1C,$0B,$01,$00
  db 1,$0D,$54,$0B,$03,$00
  db 2,$0D,$1C,$0B,$02,$00
  db 1,$0D,$20,$0B,$09,$00
  db 1,$0D,$54,$0B,$03,$00
  db 1,$10,$00
;CBT,END

;CBT,PAT,7
MySong_Pattern_7:
  db 2,$0D,$50,$0B,$03,$0E,$30,$00
  db 2,$0D,$4E,$0B,$03,$00
  db 2,$0D,$50,$0B,$03,$00
  db 2,$0D,$4E,$0B,$03,$00
  db 2,$0D,$50,$0B,$03,$00
  db 2,$0D,$4E,$0B,$03,$00
  db 2,$0D,$50,$0B,$03,$00
  db 2,$0D,$4E,$0B,$03,$00
  db 2,$0D,$56,$0B,$03,$00
  db 2,$0D,$54,$0B,$03,$00
  db 2,$0D,$56,$0B,$03,$00
  db 2,$0D,$54,$0B,$03,$00
  db 2,$0D,$56,$0B,$03,$00
  db 2,$0D,$54,$0B,$03,$00
  db 2,$0D,$56,$0B,$03,$00
  db 1,$0D,$54,$0B,$03,$00
  db 1,$10,$00
;CBT,END

;CBT,PAT,8
MySong_Pattern_8:
  db 2,$0D,$44,$0B,$06,$0E,$F0,$00
  db 1,$0E,$C0,$00
  db 1,$0E,$A0,$00
  db 1,$0E,$80,$00
  db 1,$0E,$40,$00
  db 2,$0D,$42,$0B,$06,$0E,$F0,$00
  db 1,$0E,$C0,$00
  db 1,$0E,$A0,$00
  db 1,$0E,$80,$00
  db 1,$0E,$40,$00
  db 4,$0D,$44,$0B,$06,$0E,$F0,$00
  db 2,$0D,$44,$0B,$06,$00
  db 1,$0E,$C0,$00
  db 1,$0E,$A0,$00
  db 1,$0E,$80,$00
  db 1,$0E,$40,$00
  db 4,$0D,$3C,$0B,$06,$0E,$F0,$00
  db 1,$0E,$C0,$00
  db 4,$0D,$3C,$0B,$06,$0E,$F0,$00
  db 1,$0E,$C0,$00
  db 1,$10,$00
;CBT,END

;CBT,PAT,9
MySong_Pattern_9:
  db 2,$0D,$43,$0B,$06,$0E,$F0,$00
  db 1,$0E,$C0,$00
  db 1,$0E,$A0,$00
  db 1,$0E,$80,$00
  db 1,$0E,$40,$00
  db 2,$0D,$41,$0B,$06,$0E,$F0,$00
  db 1,$0E,$C0,$00
  db 1,$0E,$A0,$00
  db 1,$0E,$80,$00
  db 1,$0E,$40,$00
  db 4,$0D,$43,$0B,$06,$0E,$F0,$00
  db 2,$0D,$43,$0B,$06,$00
  db 1,$0E,$C0,$00
  db 1,$0E,$A0,$00
  db 1,$0E,$80,$00
  db 1,$0E,$40,$00
  db 4,$0D,$39,$0B,$06,$0E,$F0,$00
  db 1,$0E,$C0,$00
  db 4,$0D,$39,$0B,$06,$0E,$F0,$00
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

;CBT,VAR,InstrumentName,3,Ins3
;CBT,INS,3
MySong_Instrument_3:
  db 7,$0F,$00,$00
  db 0
;CBT,END

;CBT,VAR,InstrumentName,4,Ins4
;CBT,INS,4
MySong_Instrument_4:
  db 1,$0E,$00,$00
  db 0
;CBT,END

;CBT,VAR,InstrumentName,5,Ins5
;CBT,INS,5
MySong_Instrument_5:
  db 1,$00
  db 0
;CBT,END

;CBT,VAR,InstrumentName,6,Ins6
;CBT,INS,6
MySong_Instrument_6:
  db 2,$0F,$00,$D4,$00
  db 2,$E4,$00
  db 2,$D4,$00
  db 2,$E4,$00
  db 2,$D4,$00
  db 2,$E4,$00
  db 2,$D4,$00
  db 2,$E4,$00
  db 0
;CBT,END

;CBT,VAR,InstrumentName,7,Ins7
;CBT,INS,7
MySong_Instrument_7:
  db 2,$0F,$00,$D4,$00
  db 2,$E4,$00
  db 2,$D4,$00
  db 2,$E4,$00
  db 0
;CBT,END

;CBT,VAR,InstrumentName,8,Ins8
;CBT,INS,8
MySong_Instrument_8:
  db 7,$0F,$00,$00
  db 0
;CBT,END

;CBT,VAR,InstrumentName,9,Ins9
;CBT,INS,9
MySong_Instrument_9:
  db 2,$0F,$00,$D4,$00
  db 2,$E4,$00
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
  dw MySong_Pattern_9

MySong_InstrumentList:
  dw MySong_Instrument_0
  dw MySong_Instrument_1
  dw MySong_Instrument_2
  dw MySong_Instrument_3
  dw MySong_Instrument_4
  dw MySong_Instrument_5
  dw MySong_Instrument_6
  dw MySong_Instrument_7
  dw MySong_Instrument_8
  dw MySong_Instrument_9
;CBT,END
;
;CBT,HED
MySong_END:
;CBT,END
