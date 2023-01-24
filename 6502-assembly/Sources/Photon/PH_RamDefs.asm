
;Player Data

PlayerDirection equ UserRam 	;What direction Player is facing (1 byte)

PlayerX equ PlayerDirection+1 	;Player X position (2 bytes)
PlayerY equ PlayerX +2			;Player Y position (2 bytes)

playerxacc equ PlayerY+2 		;Player X acceleration (2 bytes)
playeryacc equ playerxacc+2 	;Player Y acceleration (2 bytes)

;Cpu Data

CpuX equ playeryacc+2 			;Cpu X position (2 bytes)
CpuY equ CpuX+2 				;Cpu Y position (2 bytes)

CpuDirection equ CpuY+2			;What direction Cpu is facing (1 byte)

Cpuxacc equ CpuDirection+1 		;Cpu X acceleration (2 bytes)
Cpuyacc equ Cpuxacc+2 			;Cpu Y acceleration (2 bytes)

CpuTurn equ Cpuyacc+2			;What direction Cpu will turn (1 byte)

;Other Game Data

KeyTimeout equ CpuTurn+1 		;time a keypress will be ignored (1 byte)
BestLevel equ KeyTimeout+1 		;'Highscore' - best level reached (1 byte)
Level equ BestLevel+1 			;Current level (1 byte)
CpuAI equ Level+1 				;CPU Pixel look ahead lower=smarter (1 byte)
Lives equ CpuAI+1				;Player Lives (1 byte)
Tick equ Lives+1				;Game Tick (used for boost) (1 byte)
boost equ Tick+1 				;Turbo speed (1 byte)
BoostPower equ boost+1 			;Remaining boost power (1 byte)
ShownBoostPower equ BoostPower+1 ;Boost power value shown to screen (1 byte)
RandomSeed 	equ ShownBoostPower+1 ;Random seed (2 bytes)

;Line drawing

XposDir equ RandomSeed+2		;X Line Direction (1 byte)
Xpos24 equ XposDir+1 			;Xpos of current pixel (3 bytes)

YposDir equ Xpos24+3			;Y Line Direction (1 byte)
Ypos24 equ YposDir+1			;Ypos of current pixel (3 bytes)

Scale equ Ypos24+3				;Line Scale (1 byte)
LineColor equ Scale+1 			;Line Color (1 byte)

