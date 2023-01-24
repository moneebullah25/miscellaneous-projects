

VecTitleWall:
    db %00000100,%01100000		
    db %00000010,%10001111		
    db %01111101,%10000100
    db %01101111,%10000000
    db %01111110,%11101101
    db %00010011,%00010010
    db %01111110,%11101110
    db %00000010,%00100001
    db %00000101,%10000000
    db %01111101,%10000100
    db %00000011,%10011010
    db %01111101,%01100110
    db %01111101,%10000000
    db %01110101,%00010011
    db %10000001,%10000111



VecTitleZoom:
	;   CYYYYYYY, DXXXXXXX  CD=Command 
    db %00000100,%01110110	;Move Command
    db %01111100,%10010001	; Line Command
    db %01111100,%01101101	;Move Command
    db %01111101,%10001100	; LINE Command
    db %01111010,%01110100	;Move Command
    db %11111010,%10010001	; Line Command + End



VecBall:
    db $00,$EB,$02
    db $FF,$04,$01
    db $FF,$04,$01
    db $FF,$05,$04
    db $FF,$05,$04
    db $FF,$03,$07
    db $FF,$00,$0A
    db $FF,$FE,$05
    db $FF,$FC,$05
    db $FF,$FB,$04
    db $FF,$FA,$02
    db $FF,$FA,$00
    db $FF,$FA,$FF
    db $FF,$F9,$FC
    db $FF,$FC,$FA
    db $FF,$FE,$F9
    db $FF,$00,$FB
    db $FF,$02,$FA
    db $FF,$03,$FA
    db $FF,$07,$FB
    db $FF,$05,$FE
    db $01



VecHands:
    db $00,$EB,$06
    db $FF,$00,$F8
    db $FF,$04,$FC
    db $FF,$01,$FD
    db $FF,$FF,$00
    db $FF,$FC,$03
    db $FF,$FF,$FA
    db $FF,$FE,$FE
    db $FF,$FF,$08
    db $FF,$FD,$FB
    db $FF,$FE,$02
    db $FF,$04,$04
    db $FF,$FB,$02
    db $FF,$00,$02
    db $FF,$05,$00
    db $FF,$02,$09
    db $00,$16,$1C
    db $FF,$02,$02
    db $FF,$02,$FC
    db $FF,$03,$00
    db $FF,$FD,$05
    db $FF,$05,$FF
    db $FF,$00,$02
    db $FF,$FD,$01
    db $FF,$03,$01
    db $FF,$FF,$02
    db $FF,$FC,$FE
    db $FF,$02,$03
    db $FF,$FC,$01
    db $FF,$FF,$FB
    db $FF,$FD,$FC
    db $01


VecEyes:
    db $00,$F6,$12
    db $FF,$00,$FC
    db $FF,$FD,$FC
    db $FF,$FA,$00
    db $FF,$FC,$05
    db $FF,$01,$04
    db $FF,$02,$02
    db $FF,$03,$02
    db $FF,$05,$FE
    db $FF,$02,$FD
    db $00,$FA,$FF
    db $FF,$FF,$FD
    db $FF,$FE,$00
    db $FF,$FE,$02
    db $FF,$02,$03
    db $FF,$03,$FE
    db $00,$06,$11
    db $FF,$00,$FE
    db $FF,$FD,$FC
    db $FF,$FC,$FF
    db $FF,$FC,$02
    db $FF,$FE,$05
    db $FF,$02,$05
    db $FF,$05,$01
    db $FF,$04,$FE
    db $FF,$02,$FC
    db $00,$FE,$01
    db $FF,$FE,$FD
    db $FF,$FE,$02
    db $FF,$01,$03
    db $FF,$03,$FE
    db $01

VecMouth:
	;   CC  YY  XX  	CC=Command  YY=Ypos  XX=Xpos
    db $00,$E5,$0D		;Move
    db $FF,$FE,$04		; Line
    db $FF,$00,$07		; Line
    db $00,$02,$08		;Move
    db $FF,$03,$08		; Line
    db $FF,$FA,$FC		; Line
    db $00,$FB,$F9		;Move
    db $FF,$FE,$FA		; Line
    db $FF,$01,$FB		; Line
    db $FF,$0A,$FA		; Line
    db $01				;  End


VecToung:
    db $00,$E3,$12
    db $FF,$FE,$02
    db $FF,$03,$08
    db $FF,$00,$03
    db $FF,$FD,$05
    db $FF,$FD,$02
    db $FF,$FE,$FF
    db $FF,$FE,$FD
    db $FF,$03,$FC
    db $FF,$03,$FE
    db $FF,$00,$FC
    db $FF,$FC,$FD
    db $00,$06,$06
    db $FF,$FE,$05
    db $FF,$FD,$02
    db $01


VecTitleF:
    db $00,$2A,$C5
    db $FF,$00,$0E
    db $FF,$FD,$00
    db $FF,$00,$02
    db $FF,$FC,$00
    db $FF,$00,$FE
    db $FF,$FD,$00
    db $FF,$00,$F9
    db $FF,$FB,$00
    db $FF,$00,$02
    db $FF,$FE,$00
    db $FF,$00,$F7
    db $FF,$02,$00
    db $FF,$00,$02
    db $FF,$0C,$00
    db $FF,$00,$FE
    db $FF,$03,$00
    db $00,$FE,$06
    db $FF,$00,$06
    db $FF,$FB,$00
    db $FF,$00,$FA
    db $FF,$05,$00
    db $00,$02,$0D
    db $FF,$EF,$00
    db $FF,$00,$05
    db $FF,$07,$00
    db $FF,$00,$05
    db $FF,$F9,$00
    db $FF,$00,$05
    db $FF,$11,$00
    db $FF,$00,$FB
    db $FF,$F9,$00
    db $FF,$00,$FB
    db $FF,$07,$00
    db $FF,$00,$FB
    db $00,$00,$19
    db $FF,$00,$07
    db $FF,$FA,$05
    db $FF,$FA,$00
    db $FF,$FB,$FB
    db $FF,$00,$FA
    db $FF,$00,$FF
    db $FF,$05,$FC
    db $FF,$07,$00
    db $FF,$05,$04
    db $00,$FD,$02
    db $FF,$00,$03
    db $FF,$FE,$02
    db $FF,$F9,$00
    db $FF,$FD,$FE
    db $FF,$00,$FD
    db $FF,$03,$FE
    db $FF,$07,$00
    db $FF,$02,$02
    db $00,$03,$0E
    db $FF,$00,$0F
    db $FF,$FB,$00
    db $FF,$00,$FE
    db $FF,$02,$00
    db $FF,$00,$FD
    db $FF,$F4,$00
    db $FF,$00,$02
    db $FF,$FE,$00
    db $FF,$00,$F8
    db $FF,$00,$FF
    db $FF,$02,$00
    db $FF,$00,$02
    db $FF,$0C,$00
    db $FF,$00,$FE
    db $FF,$FE,$00
    db $FF,$00,$FD
    db $FF,$05,$00
    db $00,$FB,$14
    db $FF,$05,$06
    db $FF,$00,$06
    db $FF,$FB,$05
    db $FF,$F8,$00
    db $FF,$FC,$FB
    db $FF,$00,$FA
    db $FF,$05,$FA
    db $FF,$07,$00
    db $00,$00,$05
    db $FF,$02,$03
    db $FF,$00,$02
    db $FF,$FE,$03
    db $FF,$F9,$00
    db $FF,$FD,$FC
    db $FF,$00,$FE
    db $FF,$03,$FD
    db $FF,$07,$00
    db $00,$F4,$10
    db $00,$01,$00
    db $FF,$10,$00
    db $FF,$00,$05
    db $FF,$F9,$07
    db $FF,$07,$00
    db $FF,$00,$05
    db $FF,$EF,$00
    db $FF,$00,$FC
    db $FF,$09,$F8
    db $FF,$F7,$00
    db $FF,$00,$FB
    db $01


VecTitleB:
    db $00,$0,$FE
    db $00,$1B,$C6
    db $FF,$00,$FF
    db $FF,$02,$00
    db $FF,$00,$01
    db $FF,$0D,$00
    db $FF,$00,$FE
    db $FF,$02,$00
    db $FF,$00,$0E
    db $FF,$FE,$03
    db $00,$02,$EF
    db $FF,$FE,$03
    db $00,$FD,$0A
    db $FF,$FE,$00
    db $FF,$00,$FD
    db $00,$01,$02
    db $FF,$FD,$03
    db $00,$07,$09
    db $00,$00,$01
    db $FF,$02,$00
    db $FF,$00,$FB
    db $FF,$EF,$00
    db $FF,$FE,$02
    db $00,$13,$FE
    db $FF,$FE,$02
    db $00,$01,$0D
    db $00,$FA,$F9
    db $FF,$00,$02
    db $FF,$07,$00
    db $FF,$00,$04
    db $FF,$FE,$03
    db $00,$02,$F9
    db $FF,$FE,$02
    db $00,$FC,$FE
    db $FF,$FD,$02
    db $00,$FD,$FE
    db $FF,$FB,$00
    db $FF,$FE,$02
    db $00,$11,$16
    db $FF,$02,$FE
    db $FF,$00,$F9
    db $FF,$FE,$00
    db $FF,$00,$FE
    db $FF,$FE,$00
    db $FF,$00,$FD
    db $FF,$F8,$00
    db $FF,$F9,$07
    db $00,$0F,$FB
    db $00,$04,$03
    db $FF,$FE,$02
    db $00,$FE,$F9
    db $FF,$FE,$02
    db $00,$00,$0A
    db $00,$FF,$00
    db $FF,$FB,$00
    db $FF,$00,$FE
    db $FF,$FE,$00
    db $00,$02,$02
    db $FF,$FE,$03
    db $00,$08,$08
    db $FF,$02,$FE
    db $FF,$04,$00
    db $FF,$00,$0E
    db $FF,$FE,$03
    db $00,$02,$EE
    db $00,$00,$01
    db $FF,$FE,$02
    db $00,$FB,$02
    db $FF,$F8,$00
    db $FF,$00,$FE
    db $FF,$FE,$00
    db $FF,$FE,$02
    db $00,$04,$00
    db $FF,$FF,$02
    db $00,$09,$1A
    db $FF,$FB,$00
    db $FF,$00,$FD
    db $FF,$FD,$00
    db $00,$04,$03
    db $FF,$FD,$02
    db $00,$0C,$00
    db $FF,$02,$FD
    db $FF,$00,$FA
    db $FF,$FE,$00
    db $FF,$00,$FD
    db $FF,$FE,$00
    db $FF,$00,$FE
    db $FF,$F8,$00
    db $FF,$F9,$06
    db $00,$0F,$FA
    db $FF,$FE,$03
    db $00,$06,$02
    db $FF,$FE,$02
    db $00,$EF,$0E
    db $FF,$03,$FF
    db $FF,$10,$00
    db $FF,$00,$04
    db $FF,$FE,$02
    db $00,$02,$FA
    db $FF,$FE,$02
    db $00,$FC,$0A
    db $FF,$06,$00
    db $FF,$00,$05
    db $FF,$FE,$02
    db $00,$02,$F9
    db $FF,$FE,$02
    db $00,$F9,$F9
    db $FF,$FB,$05
    db $FF,$FD,$00
    db $FF,$FE,$02
    db $01
