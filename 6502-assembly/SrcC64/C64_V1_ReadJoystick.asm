Player_ReadControlsDual:
		lda $DC00			;Read in Joystick 1
		ora #%11100000		;Set unused bits
		sta z_l
		lda $DC01			;Read in Joystick 2
		ora #%11100000		;Set unused bits
		sta z_h
	rts
	
	
		
	;$DC00
;56320 Port A, keyboard matrix columns and joystick #2. Read bits:
;Bit #0: 0 = Port 2 joystick up pressed.
;Bit #1: 0 = Port 2 joystick down pressed.
;Bit #2: 0 = Port 2 joystick left pressed.
;Bit #3: 0 = Port 2 joystick right pressed.
;Bit #4: 0 = Port 2 joystick fire pressed.

;$DC01
;56321 Port B, keyboard matrix rows and joystick #1. Bits:
;Bit #x: 0 = A key is currently being pressed in keyboard matrix row #x, in the column selected at memory address $DC00.
;Bit #0: 0 = Port 1 joystick up pressed.
;Bit #1: 0 = Port 1 joystick down pressed.
;Bit #2: 0 = Port 1 joystick left pressed.
;Bit #3: 0 = Port 1 joystick right pressed.
;Bit #4: 0 = Port 1 joystick fire pressed.
 
