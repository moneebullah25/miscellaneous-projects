Song1:
	db 3    ;Channels
	db 0    ;Repeat Point
	dw PatternList
		dw SongSequence_1 ; Most important channel first
	dw SongSequence_2

	dw SongSequence_0
	

SongSequence_0:
    db 1,1,5,5,5,5,5,5,5,5,5,5,1,1,255 
SongSequence_1:
    db 0,0,3,4,3,4,6,6,7,7,7,7,8,8,255 
SongSequence_2:
    db 2,2,2,2,2,2,2,2,2,2,2,2,2,2,255 

PatternIndexes:

Pattern0:
    db 2*MSpd ,Cmd_Note,52 ,Cmd_Inst,Ins23, Cmd_Volu,240 ,0
    db 2*MSpd ,Cmd_Note,52 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,50 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,49 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,52 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,52 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,50 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,49 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,50 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,50 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,48 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,46 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,50 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,50 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,48 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,46 ,Cmd_Inst,Ins23 ,0
    db 1,$10

Pattern1:
    db 4*MSpd ,Cmd_Note,80 ,Cmd_Inst,Ins20, Cmd_Volu,192 ,0
    db 4*MSpd ,Cmd_Note,81 ,Cmd_Inst,Ins20 ,0
    db 4*MSpd ,Cmd_Note,78 ,Cmd_Inst,Ins20 ,0
    db 4*MSpd ,Cmd_Note,79 ,Cmd_Inst,Ins20 ,0
    db 4*MSpd ,Cmd_Note,80 ,Cmd_Inst,Ins20 ,0
    db 4*MSpd ,Cmd_Note,81 ,Cmd_Inst,Ins20 ,0
    db 4*MSpd ,Cmd_Note,82 ,Cmd_Inst,Ins20 ,0
    db 4*MSpd ,Cmd_Note,84 ,Cmd_Inst,Ins20 ,0
    db 1,$10

Pattern2:
    db 2*MSpd ,Cmd_Note,48 ,Cmd_Inst,Ins7, Cmd_Volu,240 ,0
    db 2*MSpd ,Cmd_Note,62 ,Cmd_Inst,Ins6 ,0
    db 2*MSpd ,Cmd_Note,62 ,Cmd_Inst,Ins6 ,0
    db 2*MSpd ,Cmd_Note,62 ,Cmd_Inst,Ins6 ,0
    db 2*MSpd ,Cmd_Note,48 ,Cmd_Inst,Ins7 ,0
    db 2*MSpd ,Cmd_Note,62 ,Cmd_Inst,Ins6 ,0
    db 2*MSpd ,Cmd_Note,62 ,Cmd_Inst,Ins6 ,0
    db 2*MSpd ,Cmd_Note,62 ,Cmd_Inst,Ins6 ,0
    db 2*MSpd ,Cmd_Note,48 ,Cmd_Inst,Ins7 ,0
    db 2*MSpd ,Cmd_Note,48 ,Cmd_Inst,Ins7 ,0
    db 2*MSpd ,Cmd_Note,62 ,Cmd_Inst,Ins6 ,0
    db 2*MSpd ,Cmd_Note,62 ,Cmd_Inst,Ins6 ,0
    db 2*MSpd ,Cmd_Note,48 ,Cmd_Inst,Ins7 ,0
    db 2*MSpd ,Cmd_Note,62 ,Cmd_Inst,Ins6 ,0
    db 2*MSpd ,Cmd_Note,62 ,Cmd_Inst,Ins6 ,0
    db 2*MSpd ,Cmd_Note,62 ,Cmd_Inst,Ins6 ,0
    db 1,$10

Pattern3:
    db 2*MSpd ,Cmd_Note,46 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,46 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,45 ,Cmd_Inst,Ins23 ,0
    db 10*MSpd ,Cmd_Note,49 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,45 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,45 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,44 ,Cmd_Inst,Ins23 ,0
    db 10*MSpd ,Cmd_Note,48 ,Cmd_Inst,Ins23 ,0
    db 1,$10

Pattern4:
    db 2*MSpd ,Cmd_Note,44 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,44 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,43 ,Cmd_Inst,Ins23 ,0
    db 10*MSpd ,Cmd_Note,46 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,45 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,45 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,44 ,Cmd_Inst,Ins23 ,0
    db 10*MSpd ,Cmd_Note,48 ,Cmd_Inst,Ins23 ,0
    db 1,$10

Pattern5:
    db 2*MSpd ,Cmd_Note,80 ,Cmd_Inst,Ins20, Cmd_Volu,192 ,0
    db 2*MSpd , Cmd_Volu,160 ,0
    db 2*MSpd ,Cmd_Note,81 ,Cmd_Inst,Ins20, Cmd_Volu,128 ,0
    db 2*MSpd , Cmd_Volu,64 ,0
    db 2*MSpd ,Cmd_Note,78 ,Cmd_Inst,Ins20, Cmd_Volu,192 ,0
    db 2*MSpd , Cmd_Volu,160 ,0
    db 2*MSpd ,Cmd_Note,79 ,Cmd_Inst,Ins20, Cmd_Volu,128 ,0
    db 2*MSpd , Cmd_Volu,64 ,0
    db 2*MSpd ,Cmd_Note,80 ,Cmd_Inst,Ins20, Cmd_Volu,192 ,0
    db 2*MSpd , Cmd_Volu,160 ,0
    db 2*MSpd ,Cmd_Note,81 ,Cmd_Inst,Ins20, Cmd_Volu,128 ,0
    db 2*MSpd , Cmd_Volu,64 ,0
    db 2*MSpd ,Cmd_Note,82 ,Cmd_Inst,Ins20, Cmd_Volu,192 ,0
    db 2*MSpd , Cmd_Volu,160 ,0
    db 2*MSpd ,Cmd_Note,84 ,Cmd_Inst,Ins20, Cmd_Volu,128 ,0
    db 2*MSpd , Cmd_Volu,64 ,0
    db 1,$10

Pattern6:
    db 4*MSpd ,Cmd_Note,52 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,52 ,Cmd_Inst,Ins23 ,0
    db 6*MSpd ,Cmd_Note,52 ,Cmd_Inst,Ins23 ,0
    db 1*MSpd ,Cmd_Note,48 ,Cmd_Inst,Ins23 ,0
    db 1*MSpd ,Cmd_Note,49 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,52 ,Cmd_Inst,Ins23 ,0
    db 4*MSpd ,Cmd_Note,50 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,50 ,Cmd_Inst,Ins23 ,0
    db 6*MSpd ,Cmd_Note,50 ,Cmd_Inst,Ins23 ,0
    db 1*MSpd ,Cmd_Note,46 ,Cmd_Inst,Ins23 ,0
    db 1*MSpd ,Cmd_Note,49 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,50 ,Cmd_Inst,Ins23 ,0
    db 1,$10

Pattern7:
    db 4*MSpd ,Cmd_Note,52 ,Cmd_Inst,Ins23, Cmd_Volu,240 ,0
    db 2*MSpd ,Cmd_Note,52 ,Cmd_Inst,Ins23 ,0
    db 3*MSpd ,Cmd_Note,52 ,Cmd_Inst,Ins23 ,0
    db 1*MSpd ,Cmd_Note,96 ,Cmd_Inst,Ins23, Cmd_Volu,192 ,0
    db 2*MSpd ,Cmd_Note,94 ,Cmd_Inst,Ins23 ,0
    db 1*MSpd ,Cmd_Note,48 ,Cmd_Inst,Ins23, Cmd_Volu,240 ,0
    db 1*MSpd ,Cmd_Note,49 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,52 ,Cmd_Inst,Ins23 ,0
    db 4*MSpd ,Cmd_Note,50 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,50 ,Cmd_Inst,Ins23 ,0
    db 3*MSpd ,Cmd_Note,50 ,Cmd_Inst,Ins23 ,0
    db 1*MSpd ,Cmd_Note,92 ,Cmd_Inst,Ins23, Cmd_Volu,192 ,0
    db 2*MSpd ,Cmd_Note,90 ,Cmd_Inst,Ins23 ,0
    db 1*MSpd ,Cmd_Note,46 ,Cmd_Inst,Ins23, Cmd_Volu,240 ,0
    db 1*MSpd ,Cmd_Note,49 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,50 ,Cmd_Inst,Ins23 ,0
    db 1,$10

Pattern8:
    db 2*MSpd ,Cmd_Note,53 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,53 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,51 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,50 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,53 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,53 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,51 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,50 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,52 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,52 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,50 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,49 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,52 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,52 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,50 ,Cmd_Inst,Ins23 ,0
    db 2*MSpd ,Cmd_Note,49 ,Cmd_Inst,Ins23 ,0
    db 1,$10       ;Pattern end

PatternList:
    dw Pattern0
    dw Pattern1
    dw Pattern2
    dw Pattern3
    dw Pattern4
    dw Pattern5
    dw Pattern6
    dw Pattern7
    dw Pattern8



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MSpd equ 1

Ins23 equ 3

Ins20 equ 3

Ins6 equ 0
Ins7 equ 1