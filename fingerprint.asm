DATA segment
		ArgV		db 256 dup(?)				;args table
		ArgC		db 0						;args number
		ArgPtr      dw 9 dup(?)					;pointers to args

		EoL			db 10, 13, '$'				;end of line
		
		Input		db 16 dup(0), '$'			;input bytes stream
	
				
		;Do not separate it!
					db 18 dup('$')
		Output		db 162 dup (0)				;153+9
					db 18 dup('$')
		;---------------------------------------------
		
		topBorder   db "+-----------------+$"
		botBorder	db "+-----------------+$"
		sideBorder  db "|$"
		
		ErrMsg1		db "Not enough arguments!$"
		ErrMsg2		db "Too many arguments!$"
		ErrMsg3		db "Syntax error!$"
		ErrMsg4		db "Wrong first argument!$"
		ErrMsg5		db "Wrong second argument!$"

DATA ends
CODE segment

	START:
									
		mov ax, seg STACK1						;init stack
		mov ss, ax
		mov sp, offset StackPointer
		
		mov ax, seg DATA						;init data segment
		mov ds, ax
	
		call parseArguments						;
		call checkArguments
		call convertToBytes
		call checkVariant
		call makeOutput
		call writeOutput
		
	PROGRAM_END:
		mov ax, 04c00h
		int 21h	

		
	parseArguments:
		push si
		push dx
		push ax
		push cx
		push bx
			
		xor cx, cx								;same as "mov cx, 0"
		mov cl, byte ptr es:[80h]				;load args length to cx (loop counter)
		mov si, 82h								;set source pointer
		mov al, es:[si]							
		mov di, offset ArgV						;set destination pointer
		
		cmp cl, 0								;check if ArgC>0
		jne parseArgumentsLoop
		
		mov dx, offset ErrMsg1					;print error message
		call handleError						
		
		parseArgumentsLoop:						;load args to ArgV
			mov al, byte ptr es:[si]			
			
			cmp al, 20h							;check if space
			je whiteSpace						;if space, skip to white space removal
			
			cmp al, 9h							;check if tab
			je whiteSpace						;if tab, skip to white space removal
			
			cmp al, 0Dh							;check if CR (enter pressed)
			je endOfArgs					
			
			mov byte ptr ds:[di], al			;if normal char => load to ArgV
			inc si								
			jmp parseArgumentsLoopCont
			
			endOfArgs:
			mov byte ptr ds:[di], '$'			;replace CR with end of text
			inc si
			jmp parseArgumentsLoopCont
			
			whiteSpace:							;if white space found
			mov byte ptr ds:[di], 0				;load args separator
			
			whiteSpaceLoop:						;remove all white spaces left
				inc si								
				mov al, byte ptr es:[si]
				cmp al, 20h						;check if space
				je whiteSpaceLoop				;if space found => skip to next char
				cmp al, 9h						;check if tab
				je whiteSpaceLoop				;if tab found => skip to next char
			
			parseArgumentsLoopCont:		    ;at this point, we do not have any white spaces left
			inc di
		
		loop parseArgumentsLoop
		
		mov si, offset ArgV						;load args source pointer
		mov di, offset ArgPtr					;load arg pointers table
		mov bx, offset ArgC						;load arg number pointer	
		
		mov word ptr ds:[di], si				;load first arg pointer
		inc si
		inc byte ptr ds:[bx]
		add di, 2
		
		
		separateArgsLoop:
			mov al, byte ptr ds:[si]			;load char to al
			
			inc si								;point next char
			cmp al, '$'							;if char == $
			je parseArgumentsCont				;break					
			cmp al, 0							;if char != NUL
			jne separateArgsLoop				;continue
												;else
			mov byte ptr ds:[si-1d], '$'
			mov word ptr ds:[di], si			;load arg pointer to ArgPtr
			inc byte ptr ds:[bx]				;increment ArgC
		
			add di, 2							;go to next arg pointer in ArgPtr (word=2byte)
			jmp separateArgsLoop
			
		parseArgumentsCont:
		
												;fix first argument when there are more than one white spaces before first arg (DOSBox issue)
		mov bx, 0
		call getArgument
		cmp byte ptr ds:[bx], 0
		jne fixSpaceCont
			inc word ptr ds:[ArgPtr]
		
		fixSpaceCont:
		
		
		pop bx	
		pop cx	
		pop ax
		pop dx
		pop si
	ret
	
	getArgumentLength:							;returns in al length of argument with index from al (0, 1, 2, 3, etc.)
		push si
		push bx
		
												
		xor bx, bx								;getArgument uses whole bx
		mov bl, al
		call getArgument
		xor ax, ax								;reset counter
		
		getArgumentLengthLoop:
			inc al	
			inc bx
			cmp byte ptr ds:[bx], '$'			;if end of argument => break
			jne getArgumentLengthLoop
		
		pop bx
		pop si
	ret
	
	getArgument:								;returns argument offset in bx (arg number from bx)
		push si
		
		sal bl, 1d
		mov si, offset ArgPtr
		mov bx, word ptr ds:[si+bx]
		
		pop si
	ret
	
	printArgument:								;prints argument with index from al (0, 1, 2, 3, etc.); call parseArguments first
		push si
		push dx
		push bx
		
		xor bx, bx								;getArgument uses whole bx
		mov bl, al
		
		call getArgument
		mov dx, bx
		call print
		
		mov dx, offset EoL						;print EoL
		call print
		
		pop bx
		pop dx
		pop si
	ret
	
	printAllArguments:
		push si
		push cx
		push ax
		
		xor cx, cx								;same as mov cx, 0
		xor bx, bx
		xor al, al
		mov si, offset ArgC
		mov cl, byte ptr ds:[si]
		
		allArgumentsLoop:
			call printArgument
			inc al
		loop allArgumentsLoop
		
		pop ax
		pop cx
		pop si
	ret
	
	checkArguments:
		push bx
		push dx
												;check arg number
		mov bx, offset ArgC				
		cmp byte ptr ds:[bx], 2d				;if ArgC=2
		jnb checkArgumentsCont1			
			mov dx, offset ErrMsg1					;write out error msg
			call handleError
	
			
		checkArgumentsCont1:
		
		cmp byte ptr ds:[bx], 2d
		jna checkArgumentsCont2
			mov dx, offset ErrMsg2					;write out error msg
			call handleError
		
		checkArgumentsCont2:
		
		call checkFirstArgument					;check arguments (length, value)
		call checkSecondArgument
		
		pop dx
		pop bx
	ret
	
	checkFirstArgument:							;call parseArguments first
		push ax
		push dx
		push bx
		
		mov al, 0								;check argument length
		call getArgumentLength
		
		cmp al, 1d
		je checkFirstArgumentCont1
		
		mov dx, offset ErrMsg4
		call handleError
		
		checkFirstArgumentCont1:
		
		mov bx, 0								;check if 0 or 1
		call getArgument
		
		cmp byte ptr ds:[bx], '0'
		je checkFirstArgumentCont2
		
		cmp byte ptr ds:[bx], '1'
		je checkFirstArgumentCont2
		
		mov dx, offset ErrMsg4
		call handleError
		
		checkFirstArgumentCont2:
		
		pop bx
		pop dx
		pop ax
	ret
	
	checkSecondArgument:						;call parseArguments first
		push ax
		push bx
		push dx
		push cx
		
		mov al, 1								;check argument length
		call getArgumentLength
		
		cmp al, 32d
		je checkSecondArgumentCont1
		
		mov dx, offset ErrMsg5
		call handleError
		
		checkSecondArgumentCont1:
		
		mov bx, 1								;check value
		call getArgument
		
		xor cx, cx
		mov cl, al
	
		checkSecondArgumentLoop:
			mov al, byte ptr ds:[bx]
			
				cmp al, '0'
				jnb checkSecArgLoopCont1		;if (al<0) then
			
					mov dx, offset ErrMsg5			;print error
					call handleError
												;else
			checkSecArgLoopCont1:				
			
				cmp al, 'a'							;if (al<a) then
				jnb checkSecArgLoopCont2	
			
				cmp al, '9'								;if (al>9)
				jna checkSecArgLoopCont3
							
					mov dx, offset ErrMsg5					;print error
					call handleError
			
			checkSecArgLoopCont2:					;else
			
				cmp al, 'f'								;if (al>f)
				jna checkSecArgLoopCont3			
			
					mov dx, offset ErrMsg5					;print error
					call handleError
													;else ALL OK
			checkSecArgLoopCont3:
			
			inc bx
			
		loop checkSecondArgumentLoop
		
		pop	cx
		pop dx
		pop bx
		pop ax
	ret
	
	convertToBytes:
		push ax
		push bx
		push cx
		push di
		
		mov bx, 1								;arg index for getArgument
		mov al, 1								;arg index for getArgumentLength
		mov di, offset Input		
		
		call getArgument
		call getArgumentLength
		
		sar al, 1d								;we will merge two bytes into one, so ArgC/2
		xor cx, cx
		mov cl, al								;set counter to argument length
		
		convertToBytesLoop:
			mov al, byte ptr ds:[bx]
			
			cmp al, '9'							;check if digit or letter
			jna digit1							;jump not above
				add al, -87d					;if letter
				jmp convertToBytesLoopCont1
				
			digit1:						
				add al, -48d					;if digit
			
			convertToBytesLoopCont1:
												
												;move bits to left half-byte
			push cx								;shl al, X <= if X>1 then we have to use cl
			mov cl, 4d
			shl al, cl
			pop cx
			
			inc bx								;go to next char
			
			mov ah, byte ptr ds:[bx]
			cmp ah, '9'							;check if digit or letter
			jna digit2							
				add ah, -87d					;if letter
				jmp convertToBytesLoopCont2
				
			digit2:						
				add ah, -48						;if digit
			
			convertToBytesLoopCont2:
			
			add al, ah							;merge two bytes
			
			mov byte ptr ds:[di], al
			
			inc di
			inc bx
			
		loop convertToBytesLoop
		
		pop di
		pop cx
		pop bx
		pop ax
	ret
	
	modifyInput:								;adds to each byte number of steps that have been done already
		push bx
		push cx
		push ax
		
		mov bx, offset Input+1d
		mov al, 4d
		mov cx, 15d
		modifyInputLoop:
			add byte ptr ds:[bx], al
			add al, 4d
			inc bx
		loop modifyInputLoop
		
		pop ax
		pop cx
		pop bx
	ret
	
	checkVariant:								;checks algorithm variant (modified or not)
		push bx
		
		call getArgument
		
		cmp byte ptr ds:[bx], '0'
		je normalVariant
			call modifyInput
		normalVariant:
		
		pop bx
	ret
	
	generateTable:								;iterate over pairs of bits and move
		push cx
		push si
		push di
		push ax
		push bx
		
		mov si, offset Input
		mov di, offset Output+80d				;set pointer to table center
		
		mov byte ptr ds:[di], 1d				;set start
		
		mov cx, 16d
		bytesLoop:								;iterate on each byte (high -> low)
			mov al, byte ptr ds:[si]
			
			push cx
			mov cx, 4d							;2 bits on each iteration
			bitsLoop:							;iterate on each pair of bits (low -> high)
	
				shr al, 1						;move first low bit to carry flag
				jnc secondBitNull				;jump if cf==0
					mov bl, 1  					;if 1
					jmp secondBitCont
	
				secondBitNull:
					mov bl, 0					;if 0
		
				secondBitCont:
				
				shr al, 1					
				jnc firstBitNull				;jump if cf==0
					
					add bl, 10b					;if 1
					
				firstBitNull:
												;if 0
											
												;switch over all 4 different moves
				cmp bl, 00b						;left-up
				jne bitsLoopSwitch1
					add di, -19d
					cmp byte ptr ds:[di], '$'
					jne leftUpMove
						add di, 18d
						cmp byte ptr ds:[di], '$'
						jne leftUpMove
							add di, -17d
							cmp byte ptr ds:[di], '$'
							jne leftUpMove
								add di, 18d
					leftUpMove:
					
					jmp bitsLoopSwitchBreak
				
				bitsLoopSwitch1:
				cmp bl, 01b						;right-up
				jne bitsLoopSwitch2
					add di, -17d
					cmp byte ptr ds:[di], '$'
					jne rightUpMove
						add di, 18d
						cmp byte ptr ds:[di], '$'
						jne rightUpMove
							add di, -19d
							cmp byte ptr ds:[di], '$'
							jne rightUpMove
								add di, 18d
					rightUpMove:
					
					jmp bitsLoopSwitchBreak
					
				bitsLoopSwitch2:
				cmp bl, 10b						;left-down
				jne bitsLoopSwitch3
					add di, 17d
					cmp byte ptr ds:[di], '$'
					jne leftDownMove
						add di, -18d
						cmp byte ptr ds:[di], '$'
						jne leftDownMove
							add di, 19d
							cmp byte ptr ds:[di], '$'
							jne leftDownMove
								add di, -18d
					leftDownMove:
					
					jmp bitsLoopSwitchBreak
				
				bitsLoopSwitch3:				;right-down
				cmp bl, 11b
				jne bitsLoopSwitchBreak
					add di, 19d
					cmp byte ptr ds:[di], '$'
					jne rightDownMove
						add di, -18d
						cmp byte ptr ds:[di], '$'
						jne rightDownMove
							add di, 17d
							cmp byte ptr ds:[di], '$'
							jne rightDownMove
								add di, -18d
					rightDownMove:
					
				bitsLoopSwitchBreak:
				
				inc byte ptr ds:[di]
	
			dec cx
			jnz bitsLoop
			
			pop cx
			
			inc si
		
		dec cx									;haven't used loop, because it can perform only short jumps (-128 to +127 bytes)
		jnz bytesLoop
		
		mov byte ptr ds:[di], 'E'
		mov byte ptr ds:[Output+80d], 'S'
		
		pop bx
		pop ax
		pop di
		pop si
		pop cx
	ret
	
	convertNumToChar:							;converts visit counters to ASCII
		push bx
		push ax
		push cx
			mov bx, offset Output
			mov cx, 162	
			
			convertLoop:
				mov al, byte ptr ds:[bx]
				
				cmp al, 1d						;switch over all 14 different chars
				jne convertLoopSwitch1
					mov byte ptr ds:[bx], '.'
					jmp convertLoopSwitchBreak
				
				convertLoopSwitch1:
				cmp al, 2d
				jne convertLoopSwitch2
					mov byte ptr ds:[bx], 'o'
					jmp convertLoopSwitchBreak
				
				convertLoopSwitch2:
				cmp al, 3d
				jne convertLoopSwitch3
					mov byte ptr ds:[bx], '+'
					jmp convertLoopSwitchBreak
				
				convertLoopSwitch3:
				cmp al, 4d
				jne convertLoopSwitch4
					mov byte ptr ds:[bx], '='
					jmp convertLoopSwitchBreak
				
				convertLoopSwitch4:
				cmp al, 5d
				jne convertLoopSwitch5
					mov byte ptr ds:[bx], '*'
					jmp convertLoopSwitchBreak
				
				convertLoopSwitch5:
				cmp al, 6d
				jne convertLoopSwitch6
					mov byte ptr ds:[bx], 'B'
					jmp convertLoopSwitchBreak
				
				convertLoopSwitch6:
				cmp al, 7d
				jne convertLoopSwitch7
					mov byte ptr ds:[bx], 'O'
					jmp convertLoopSwitchBreak
					
				convertLoopSwitch7:
				cmp al, 8d
				jne convertLoopSwitch8
					mov byte ptr ds:[bx], 'X'
					jmp convertLoopSwitchBreak
					
				convertLoopSwitch8:
				cmp al, 9d
				jne convertLoopSwitch9
					mov byte ptr ds:[bx], '@'
					jmp convertLoopSwitchBreak
					
				convertLoopSwitch9:
				cmp al, 10d
				jne convertLoopSwitch10
					mov byte ptr ds:[bx], '%'
					jmp convertLoopSwitchBreak
					
				convertLoopSwitch10:
				cmp al, 11d
				jne convertLoopSwitch11
					mov byte ptr ds:[bx], '&'
					jmp convertLoopSwitchBreak
					
				convertLoopSwitch11:
				cmp al, 12d
				jne convertLoopSwitch12
					mov byte ptr ds:[bx], '#'
					jmp convertLoopSwitchBreak
				
				convertLoopSwitch12:
				cmp al, 13d
				jne convertLoopSwitch13
					mov byte ptr ds:[bx], '/'
					jmp convertLoopSwitchBreak
					
				convertLoopSwitch13:
				cmp al, 14d
				jb convertLoopSwitchBreak
					cmp al, '$'
					je convertLoopSwitchBreak
						cmp al, 'A'				;skip Start and End
						jnb convertLoopSwitchBreak
							mov byte ptr ds:[bx], '^'
				
				convertLoopSwitchBreak:
				inc bx
			dec cx
			jnz convertLoop
		
		pop cx
		pop ax
		pop bx
	ret
	
	makeOutput:									;prepares table
		push bx
		push cx
		
		mov bx, offset Output
		add bx, 17d								;width
		
		mov cx, 9d								;height
		
		separateLines:
			mov byte ptr ds:[bx], '$'			;end of line after each 17 bytes
			add bx, 18d
		loop separateLines
		
		call generateTable					;analyze input and performs move
		call convertNumToChar				;converts visit counter to ASCII
			
		
		pop cx
		pop bx
	ret
	
	writeOutput:								;write out table	
		push dx
		push ax
		push bx
		push cx
		
		mov dx, offset topBorder				;write top border & end of line
		call print
		mov dx, offset EoL
		call print
			
		xor ax, ax
		xor cx, cx
		xor bx, bx
		mov bl, 18d								;row length (17+'$')
			
		writeOutputLoop:						;print side border + row + side border
			mov dx, offset sideBorder
			call print
				
			mov dx, offset Output
			mov al, cl
			mul bl
			add dx, ax
			call print
				
			inc cl
				
			mov dx, offset sideBorder
			call print
				
			mov dx, offset EoL
			call print
				
			cmp cl, 8d							;we start with  0
			jna writeOutputLoop
				
		mov dx, offset botBorder				;print bottom border
		call print
		mov dx, offset EoL
		call print
		
		pop cx
		pop bx
		pop ax
		pop dx
	ret
	
	print:										;prints text from ds:dx, use "mov dx, offset <label>" before
		push ax
		
		mov ah, 9h
		int 21h
		
		pop ax
	ret
	
	handleError:
		push ax
		
		call print
		mov ax, 04c00h
		int 21h	
		
		pop ax
	ret
		
CODE ends

STACK1 segment STACK
							dw 127 dup(?)
		StackPointer		dw ?

STACK1 ends

end START	