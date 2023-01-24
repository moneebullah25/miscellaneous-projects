Song1:
	db 3    ;Channels
	db 0    ;Repeat Point
	dw MysongPatternList
	dw MysongSequence_1 ; Most important channel first
	dw MysongSequence_0
	dw MysongSequence_2

MysongSequence_0:
    db 1,1,1,1,1,1,1,1,5,5,1,1,1,1,1,1,1,1,1,1,1,1,5,5,5,5,5,5,Seq_Repeat 
MysongSequence_1:
    db 3,3,0,4,0,4,3,3,7,7,0,4,0,4,8,8,9,9,0,4,0,4,0,4,0,4,3,3,Seq_Repeat 
MysongSequence_2:
    db 2,2,2,2,2,2,2,2,6,6,2,2,2,2,2,2,2,2,2,2,2,2,6,6,6,6,6,6,Seq_Repeat 

MysongPatternIndexes:

MysongPattern0:
    db 2*MSpd ,Cmd_Note,42 ,Cmd_Inst,Ins20, Cmd_Volu,240 ,0
    db 2*MSpd ,Cmd_Note,43 ,Cmd_Inst,Ins20 ,0
    db 2*MSpd ,Cmd_Note,44 ,Cmd_Inst,Ins20 ,0
    db 1*MSpd , Cmd_Volu,224 ,0
    db 1*MSpd , Cmd_Volu,208 ,0
    db 2*MSpd , Cmd_Volu,0 ,0
    db 6*MSpd ,Cmd_Note,46 ,Cmd_Inst,Ins22, Cmd_Volu,208 ,0
    db 2*MSpd ,Cmd_Note,42 ,Cmd_Inst,Ins20, Cmd_Volu,240 ,0
    db 2*MSpd ,Cmd_Note,43 ,Cmd_Inst,Ins20 ,0
    db 2*MSpd ,Cmd_Note,44 ,Cmd_Inst,Ins20 ,0
    db 1*MSpd , Cmd_Volu,224 ,0
    db 1*MSpd , Cmd_Volu,208 ,0
    db 3*MSpd , Cmd_Volu,0 ,0
    db 2*MSpd ,Cmd_Note,46 ,Cmd_Inst,Ins22, Cmd_Volu,208 ,0
    db 3*MSpd ,Cmd_Note,46 ,Cmd_Inst,Ins22 ,0
    db 1,Cmd_Pend

MysongPattern1:
    db 3*MSpd ,Cmd_Note,32 ,Cmd_Inst,Ins8, Cmd_Volu,240 ,0
    db 3*MSpd ,Cmd_Note,34 ,Cmd_Inst,Ins8 ,0
    db 3*MSpd ,Cmd_Note,30 ,Cmd_Inst,Ins8 ,0
    db 7*MSpd ,Cmd_Note,30 ,Cmd_Inst,Ins8 ,0
    db 3*MSpd ,Cmd_Note,32 ,Cmd_Inst,Ins8 ,0
    db 3*MSpd ,Cmd_Note,34 ,Cmd_Inst,Ins8 ,0
    db 3*MSpd ,Cmd_Note,30 ,Cmd_Inst,Ins8 ,0
    db 3*MSpd ,Cmd_Note,30 ,Cmd_Inst,Ins8 ,0
    db 4*MSpd ,Cmd_Note,36 ,Cmd_Inst,Ins8 ,0
    db 1,Cmd_Pend

MysongPattern2:
    db 2*MSpd ,Cmd_Note,28 ,Cmd_Inst,Ins2, Cmd_Volu,240 ,0
    db 2*MSpd ,Cmd_Note,28 ,Cmd_Inst,Ins6 ,0
    db 2*MSpd ,Cmd_Note,32 ,Cmd_Inst,Ins7 ,0
    db 2*MSpd ,Cmd_Note,28 ,Cmd_Inst,Ins6 ,0
    db 2*MSpd ,Cmd_Note,28 ,Cmd_Inst,Ins2 ,0
    db 2*MSpd ,Cmd_Note,28 ,Cmd_Inst,Ins2 ,0
    db 2*MSpd ,Cmd_Note,28 ,Cmd_Inst,Ins6 ,0
    db 2*MSpd ,Cmd_Note,28 ,Cmd_Inst,Ins6 ,0
    db 2*MSpd ,Cmd_Note,28 ,Cmd_Inst,Ins2 ,0
    db 2*MSpd ,Cmd_Note,28 ,Cmd_Inst,Ins6 ,0
    db 2*MSpd ,Cmd_Note,32 ,Cmd_Inst,Ins7 ,0
    db 2*MSpd ,Cmd_Note,28 ,Cmd_Inst,Ins6 ,0
    db 2*MSpd ,Cmd_Note,28 ,Cmd_Inst,Ins2 ,0
    db 2*MSpd ,Cmd_Note,28 ,Cmd_Inst,Ins2 ,0
    db 2*MSpd ,Cmd_Note,28 ,Cmd_Inst,Ins6 ,0
    db 2*MSpd ,Cmd_Note,32 ,Cmd_Inst,Ins7 ,0
    db 1,Cmd_Pend

MysongPattern3:
    db 32*MSpd , Cmd_Volu,0 ,0
    db 1,Cmd_Pend

MysongPattern4:
    db 2*MSpd ,Cmd_Note,42 ,Cmd_Inst,Ins20, Cmd_Volu,240 ,0
    db 2*MSpd ,Cmd_Note,43 ,Cmd_Inst,Ins20 ,0
    db 2*MSpd ,Cmd_Note,44 ,Cmd_Inst,Ins20 ,0
    db 1*MSpd , Cmd_Volu,224 ,0
    db 1*MSpd , Cmd_Volu,208 ,0
    db 2*MSpd , Cmd_Volu,0 ,0
    db 6*MSpd ,Cmd_Note,46 ,Cmd_Inst,Ins22, Cmd_Volu,208 ,0
    db 2*MSpd ,Cmd_Note,42 ,Cmd_Inst,Ins20, Cmd_Volu,240 ,0
    db 2*MSpd ,Cmd_Note,43 ,Cmd_Inst,Ins20 ,0
    db 2*MSpd ,Cmd_Note,44 ,Cmd_Inst,Ins20 ,0
    db 1*MSpd , Cmd_Volu,224 ,0
    db 1*MSpd , Cmd_Volu,208 ,0
    db 3*MSpd ,Cmd_Note,50 ,Cmd_Inst,Ins20, Cmd_Volu,240 ,0
    db 2*MSpd ,Cmd_Note,50 ,Cmd_Inst,Ins20 ,0
    db 3*MSpd ,Cmd_Note,52 ,Cmd_Inst,Ins20 ,0
    db 1,Cmd_Pend

MysongPattern5:
    db 1*MSpd ,Cmd_Note,32 ,Cmd_Inst,Ins8 ,0
    db 2*MSpd ,Cmd_Note,80 ,Cmd_Inst,Ins3 ,0
    db 2*MSpd ,Cmd_Note,34 ,Cmd_Inst,Ins8 ,0
    db 1*MSpd ,Cmd_Note,80 ,Cmd_Inst,Ins3 ,0
    db 3*MSpd ,Cmd_Note,30 ,Cmd_Inst,Ins8 ,0
    db 4*MSpd ,Cmd_Note,30 ,Cmd_Inst,Ins8 ,0
    db 3*MSpd ,Cmd_Note,80 ,Cmd_Inst,Ins3 ,0
    db 1*MSpd ,Cmd_Note,32 ,Cmd_Inst,Ins8 ,0
    db 2*MSpd ,Cmd_Note,86 ,Cmd_Inst,Ins3 ,0
    db 2*MSpd ,Cmd_Note,34 ,Cmd_Inst,Ins8 ,0
    db 1*MSpd ,Cmd_Note,86 ,Cmd_Inst,Ins3 ,0
    db 3*MSpd ,Cmd_Note,30 ,Cmd_Inst,Ins8 ,0
    db 3*MSpd ,Cmd_Note,30 ,Cmd_Inst,Ins8 ,0
    db 1*MSpd ,Cmd_Note,36 ,Cmd_Inst,Ins8 ,0
    db 3*MSpd ,Cmd_Note,86 ,Cmd_Inst,Ins3 ,0
    db 1,Cmd_Pend

MysongPattern6:
    db 2*MSpd ,Cmd_Note,28 ,Cmd_Inst,Ins2 ,0
    db 1*MSpd ,Cmd_Note,28 ,Cmd_Inst,Ins6 ,0
    db 1*MSpd ,Cmd_Note,78 ,Cmd_Inst,Ins3 ,0
    db 2*MSpd ,Cmd_Note,32 ,Cmd_Inst,Ins7 ,0
    db 1*MSpd ,Cmd_Note,28 ,Cmd_Inst,Ins6 ,0
    db 1*MSpd ,Cmd_Note,78 ,Cmd_Inst,Ins3 ,0
    db 1*MSpd ,Cmd_Note,28 ,Cmd_Inst,Ins2 ,0
    db 1*MSpd ,Cmd_Note,80 ,Cmd_Inst,Ins3 ,0
    db 1*MSpd ,Cmd_Note,28 ,Cmd_Inst,Ins2 ,0
    db 1*MSpd ,Cmd_Note,78 ,Cmd_Inst,Ins3 ,0
    db 2*MSpd ,Cmd_Note,28 ,Cmd_Inst,Ins6 ,0
    db 1*MSpd ,Cmd_Note,28 ,Cmd_Inst,Ins6 ,0
    db 1*MSpd ,Cmd_Note,78 ,Cmd_Inst,Ins3 ,0
    db 2*MSpd ,Cmd_Note,28 ,Cmd_Inst,Ins2 ,0
    db 1*MSpd ,Cmd_Note,28 ,Cmd_Inst,Ins6 ,0
    db 1*MSpd ,Cmd_Note,84 ,Cmd_Inst,Ins3 ,0
    db 2*MSpd ,Cmd_Note,32 ,Cmd_Inst,Ins7 ,0
    db 1*MSpd ,Cmd_Note,28 ,Cmd_Inst,Ins6 ,0
    db 1*MSpd ,Cmd_Note,84 ,Cmd_Inst,Ins3 ,0
    db 1*MSpd ,Cmd_Note,28 ,Cmd_Inst,Ins2 ,0
    db 1*MSpd ,Cmd_Note,86 ,Cmd_Inst,Ins3 ,0
    db 1*MSpd ,Cmd_Note,28 ,Cmd_Inst,Ins2 ,0
    db 1*MSpd ,Cmd_Note,84 ,Cmd_Inst,Ins3 ,0
    db 2*MSpd ,Cmd_Note,28 ,Cmd_Inst,Ins6 ,0
    db 1*MSpd ,Cmd_Note,32 ,Cmd_Inst,Ins7 ,0
    db 1*MSpd ,Cmd_Note,84 ,Cmd_Inst,Ins3 ,0
    db 1,Cmd_Pend

MysongPattern7:
    db 2*MSpd ,Cmd_Note,80 ,Cmd_Inst,Ins3, Cmd_Volu,48 ,0
    db 2*MSpd ,Cmd_Note,78 ,Cmd_Inst,Ins3 ,0
    db 2*MSpd ,Cmd_Note,80 ,Cmd_Inst,Ins3 ,0
    db 2*MSpd ,Cmd_Note,78 ,Cmd_Inst,Ins3 ,0
    db 2*MSpd ,Cmd_Note,80 ,Cmd_Inst,Ins3 ,0
    db 2*MSpd ,Cmd_Note,78 ,Cmd_Inst,Ins3 ,0
    db 2*MSpd ,Cmd_Note,80 ,Cmd_Inst,Ins3 ,0
    db 2*MSpd ,Cmd_Note,78 ,Cmd_Inst,Ins3 ,0
    db 2*MSpd ,Cmd_Note,86 ,Cmd_Inst,Ins3 ,0
    db 2*MSpd ,Cmd_Note,84 ,Cmd_Inst,Ins3 ,0
    db 2*MSpd ,Cmd_Note,86 ,Cmd_Inst,Ins3 ,0
    db 2*MSpd ,Cmd_Note,84 ,Cmd_Inst,Ins3 ,0
    db 2*MSpd ,Cmd_Note,86 ,Cmd_Inst,Ins3 ,0
    db 2*MSpd ,Cmd_Note,84 ,Cmd_Inst,Ins3 ,0
    db 2*MSpd ,Cmd_Note,86 ,Cmd_Inst,Ins3 ,0
    db 1*MSpd ,Cmd_Note,84 ,Cmd_Inst,Ins3 ,0
    db 1,Cmd_Pend

MysongPattern8:
    db 2*MSpd ,Cmd_Note,68 ,Cmd_Inst,Ins20, Cmd_Volu,240 ,0
    db 1*MSpd , Cmd_Volu,192 ,0
    db 1*MSpd , Cmd_Volu,160 ,0
    db 1*MSpd , Cmd_Volu,128 ,0
    db 1*MSpd , Cmd_Volu,64 ,0
    db 2*MSpd ,Cmd_Note,66 ,Cmd_Inst,Ins20, Cmd_Volu,240 ,0
    db 1*MSpd , Cmd_Volu,192 ,0
    db 1*MSpd , Cmd_Volu,160 ,0
    db 1*MSpd , Cmd_Volu,128 ,0
    db 1*MSpd , Cmd_Volu,64 ,0
    db 4*MSpd ,Cmd_Note,68 ,Cmd_Inst,Ins20, Cmd_Volu,240 ,0
    db 2*MSpd ,Cmd_Note,68 ,Cmd_Inst,Ins20 ,0
    db 1*MSpd , Cmd_Volu,192 ,0
    db 1*MSpd , Cmd_Volu,160 ,0
    db 1*MSpd , Cmd_Volu,128 ,0
    db 1*MSpd , Cmd_Volu,64 ,0
    db 4*MSpd ,Cmd_Note,60 ,Cmd_Inst,Ins20, Cmd_Volu,240 ,0
    db 1*MSpd , Cmd_Volu,192 ,0
    db 4*MSpd ,Cmd_Note,60 ,Cmd_Inst,Ins20, Cmd_Volu,240 ,0
    db 1*MSpd , Cmd_Volu,192 ,0
    db 1,Cmd_Pend

MysongPattern9:
    db 2*MSpd ,Cmd_Note,67 ,Cmd_Inst,Ins20, Cmd_Volu,240 ,0
    db 1*MSpd , Cmd_Volu,192 ,0
    db 1*MSpd , Cmd_Volu,160 ,0
    db 1*MSpd , Cmd_Volu,128 ,0
    db 1*MSpd , Cmd_Volu,64 ,0
    db 2*MSpd ,Cmd_Note,65 ,Cmd_Inst,Ins20, Cmd_Volu,240 ,0
    db 1*MSpd , Cmd_Volu,192 ,0
    db 1*MSpd , Cmd_Volu,160 ,0
    db 1*MSpd , Cmd_Volu,128 ,0
    db 1*MSpd , Cmd_Volu,64 ,0
    db 4*MSpd ,Cmd_Note,67 ,Cmd_Inst,Ins20, Cmd_Volu,240 ,0
    db 2*MSpd ,Cmd_Note,67 ,Cmd_Inst,Ins20 ,0
    db 1*MSpd , Cmd_Volu,192 ,0
    db 1*MSpd , Cmd_Volu,160 ,0
    db 1*MSpd , Cmd_Volu,128 ,0
    db 1*MSpd , Cmd_Volu,64 ,0
    db 4*MSpd ,Cmd_Note,57 ,Cmd_Inst,Ins20, Cmd_Volu,240 ,0
    db 1*MSpd , Cmd_Volu,192 ,0
    db 4*MSpd ,Cmd_Note,57 ,Cmd_Inst,Ins20, Cmd_Volu,240 ,0
    db 1,Cmd_Pend      ;Pattern end

MysongPatternList:
    dw MysongPattern0
    dw MysongPattern1
    dw MysongPattern2
    dw MysongPattern3
    dw MysongPattern4
    dw MysongPattern5
    dw MysongPattern6
    dw MysongPattern7
    dw MysongPattern8
    dw MysongPattern9

	
MSpd equ 1

Ins3 equ 3
Ins7 equ 9	;Wavy

Ins6 equ 2
Ins2 equ 1

Ins8 equ 8	

Ins20 equ 6 ;plain long
Ins22 equ 6 ;bass long
	