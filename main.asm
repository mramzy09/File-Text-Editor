INCLUDE IRVINE32.INC
INCLUDE macros.inc
.DATA
	hHeap				 HANDLE ?  ;handle holding the address of the heap
	filehandle			 HANDLE ?  
	Text				 DWORD ?    ;pointer to the allocated heap
	Text_Length			 DWORD 0  ;counter of the bytes in the heap
	size1				 DWORD 5000

	;find
	;WordCount			 DWORD ?
	counterisfinish		 DWORD 0
	NumberOfOccuer		 DWORD 0
	counterHeap			 DWORD ?
	wordc				 DWORD 0

	;count
	Wcounter			 DWORD 0
	Ccounter			 DWORD 0

	oldWord_Length		 DWORD 0	
	newWord_Length		 DWORD 0

	Tpos				 DWORD 0
	txtpos				 DWORD 0
	
	txtpos_replace		 DWORD 0

	;///////////////////////////////////////////////////
	filePath			 BYTE 50 DUP(0)
	;append
	apend_word			 BYTE 200 DUP(0)
	;Find
	Opos				 BYTE 200 DUP(0)
	;replace
	replace_word		 BYTE 200 DUP(0)
	replace_NewWord		 BYTE 200 DUP(0)
	numofR				 DWORD 0

	check dword 0

	path dword 0
.CODE
;procedure prototypes
	Read		PROTO, File_Name:PTR BYTE
	remove		PROTO, oldpos:PTR BYTE, lengthWord:DWORD , r_num:DWORD
	Str_remove	PROTO, position:PTR BYTE, chars:DWORD
	isExist		PROTO, t_pos:PTR BYTE	, o_pos:PTR BYTE
	;Find		PROTO, Opos:PTR BYTE
	replace		PROTO, oldpos:PTR BYTE, newpos:PTR BYTE, r_num:DWORD, OldWordLength:DWORD, NewWordLength:DWORD
	Str_replace	PROTO, Tposition:PTR BYTE, Nposition:PTR BYTE, chars:DWORD

main PROC
	call getHandleHeap
	call allocateArray
	Start:
		mWrite "Enter the path of the file: "
		CALL CRLF
		MOV EDX,OFFSET filePath
		MOV ECX,SIZEOF filePath
		CALL ReadString
		INVOKE Read, ADDR [filePath]
	read_Function:
		CALL waitmsg
		CALL CLRSCR
	mWrite "Please enter the number of the functions you want to perfrom:"
	CALL CRLF
	mWrite "(1)Read"
	CALL CRLF
	mWrite "(2)Display"
	CALL CRLF
	mWrite "(3)Append"
	CALL CRLF
	mWrite "(4)Find"
	CALL CRLF
	mWrite "(5)Replace"
	CALL CRLF
	mWrite "(6)Remove"
	CALL CRLF
	mWrite "(7)Count Words and Chars"
	CALL CRLF
	mWrite "(8)Write"
	CALL CRLF
	mWrite "(9)Create New File"
	CALL CRLF

	CALL ReadDec

	CMP EAX,1
	JE Read1
	
	CMP EAX,2
	JE display1

	CMP EAX,3
	JE Append1

	CMP EAX,4
	JE Find1

	CMP EAX,5
	JE Replace1

	CMP EAX,6
	JE Remove1

	CMP EAX,7
	JE count
	
	CMP EAX,8
	JE Write1

	CMP EAX,0
	JE skip
	
	Read1:
		
		CALL CLRSCR
		jmp Start
	display1:
		CALL Display
		call crlf
		jmp read_Function
	Append1:
		CALL Append
		CALL CRLF
		JMP read_Function
	Find1:
		CALL Find
		CALL CRLF
		JMP read_Function
	Replace1:
		CALL Display
		mWrite "Enter word to replace : "
		MOV ECX, 200
		MOV EDX, OFFSET replace_word
		CALL ReadString
		MOV oldWord_Length, EAX
		mWrite "Enter new word : "
		MOV ECX, 200
		MOV EDX, OFFSET replace_NewWord
		CALL ReadString
		MOV newWord_Length, EAX
		mWrite "Enter number of replacement : "
		CALL ReadDec		; EAX = number of replacement (R)
		INVOKE Replace, ADDR [replace_word], ADDR [replace_NewWord], EAX, oldWord_Length, newWord_Length
		jmp read_Function
	Remove1:
		CALL Display
		mWrite "Enter word to replace : "
		MOV ECX, 200
		MOV EDX, OFFSET replace_word
		CALL ReadString
		MOV oldWord_Length, EAX
		mWrite "Enter number of replacement : "
		CALL ReadDec		; EAX = number of replacement (R)
		INVOKE Remove, ADDR [replace_word], oldWord_Length, EAX
		jmp read_Function
	Count:
	call countword
	call crlf
	call countchars
		call crlf
		jmp read_Function
	Write1:
		call write
		jmp read_Function
	
	skip:
	mwrite "End"
	CALL CRLF
	CALL WaitMsg
EXIT
Main ENDP

;-----------------------------------------------------------------------------
;getHandleHeap PROC get a handle of the heap
;Returnes: returns 1 in EAX if it succeeds in getting the handle and 0 if t failes
;-----------------------------------------------------------------------------
getHandleHeap PROC
	;GetProcessHeap , this function will return the address of the heap in EAX.
	INVOKE heapdestroy, hheap ; not used
	INVOKE GetProcessHeap		; get handle prog's heap
	CMP EAX,  0				; if failed,  display message
	JNE success
		MOV EAX, 0
		JMP quit
	success:
	MOV hHeap, EAX    
	MOV EAX, 1    ;this procedure will return 1 in EAX if it success in returning the address of the heap
	quit:
Ret 
getHandleHeap ENDP

;-----------------------------------------------------------------------------
;allocateArray PROC get a handle of the heap
;Returnes: returns 1 in EAX if it succeeds in allocating the heap  and 0 if t failes
;-----------------------------------------------------------------------------
allocateArray PROC       ;this procedure will allocate the heap with size1 and return pointer to heap in EAX
	INVOKE HeapAlloc,  hHeap,  HEAP_ZERO_MEMORY,  size1
	CMP EAX,  NULL			 ; heap not created
	JNE success
		MOV EAX, 0 ; Cannot allocate memory   - this function will return one if it succedes in allcating the heap
		JMP quit
	success:
		MOV Text, EAX ; save the pointer to varible DWORD text
		MOV EAX, 1  ;this function will return one if it succedes in allcating the heap
	quit:
	Ret
allocateArray ENDP

;-----------------------------------------------------------------------------
;Read PROC read the file into the Text (Heap)
;Recieves: 1 offset parameter pointer to the FileName
;Returnes: file handle in filehandle handle
;-----------------------------------------------------------------------------
Read PROC uses EDX,  File_Name:PTR BYTE
	MOV EDX, File_Name     ;CALL openinputfile takes in EDX the address of the file_name since its already a pointer we do not use offset
	mov path,edx
	CALL openInputfile    ;returns the handle of the file in EAX
	CMP EAX, INVALID_HANDLE_VALUE
	jne successOpen
	MOV EAX, 0
	mwrite"Fail to open the file"
	call crlf
	JMP quit1
	successOpen:
	mwrite"success in opening the file"
	call crlf
	MOV filehandle, EAX    ; move the file handle in from EAX nto filehandle handle

	MOV EDX, Text             ;PROC readfromfile takes a pointer to the heap to fill it and the maximum size of the reading just just like readstring , it will return in EAX the actual number of bytes that it reads
	MOV ECX, size1     
	CALL readfromfile
	jnc sucessRead
	MOV EAX, 0
	JMP quit
	sucessRead:
	MOV Text_Length, EAX
   
	quit:
	MOV EAX, filehandle  ;after reading move the file handle into EAX to close the file
	CALL closefile
	MOV EAX, 1
	quit1:
	Ret
Read ENDP

;-----------------------------------------------------------------------------
;Display PROC Apend a string at the last of the heap
;-----------------------------------------------------------------------------
Display PROC USES EDX ESI
	CMP Text_length, 0
	JE File_Is_Empty
		MOV EDX, text
		CALL writestring
	File_Is_Empty:
		CALL CRLF
ret
Display ENDP

;-----------------------------------------------------------------------------
;Append PROC Apend a string at the last of the heap
;Recieves: 
;-----------------------------------------------------------------------------
Append PROC uses ESI
mwrite"Enter the word you want to append : "
	MOV ECX, 300
	MOV EDX, OFFSET apend_word
	CALL readstring

	MOV EBX, 0
	MOV EDX, Text				;pinter to text in EDX
	ADD EDX, Text_Length		;ADD the length of the text to make EDX points at the last element in the heap
	MOV ESI, offset apend_word			;pointer to the apended word
	MOV ECX, EAX
	L:
		MOV bl, BYTE PTR[ESI]
		MOV BYTE PTR [EDX], bl
		INC ESI
		INC EDX
		INC Text_Length
	Loop L
	MOV BYTE PTR [EDX], 32
	INC Text_Length
Ret
Append ENDP

;-----------------------------------------------------------------------------
;Write PROC Writes the updated text from the heap into the file
;Recieves: 1 offset parameter File Name
;Return: File Handle in EAX
;-----------------------------------------------------------------------------
write PROC 
	MOV EAX, filehandle    
	CALL CloseFile   ;close the file just in case it was open
	MOV EDX, path    ; MOV the file name into EDX
	CALL CreateOutputFile	  ;create new file and delete the file in the same path if it exists
	MOV filehandle, EAX
	MOV ECX, Text_Length        ;procedure writetofile Takes the text length that will be written in ECX and pointer to the text in EDX
	MOV EDX, Text
	CALL WriteToFile
	mov eax,filehandle
	CALL CloseFile
	MOV EAX, 1
	Ret
write ENDP

;-----------------------------------------------------------------------------
;Find PROC Counts number of occurrence of the entered word in the file's text
;Recieves: 2 offsets parameters text and entered word
;Return: Number of occurrence of the word in EAX
;-----------------------------------------------------------------------------
Find PROC USES ESI EDI EDX
	mWrite "Please enter word to find it: "
	MOV ECX, 200
	MOV EDX, OFFSET Opos
	CALL ReadString
	MOV oldWord_Length, EAX

	MOV EAX, text
	MOV Tpos, EAX

	MOV EDX, 0						 ;counts number of occurrence of a word 
	MOV EDI, Tpos					 ;ES:DI => text
	MOV ECX, text_Length			 ;ECX = length of text
	resume:
		MOV ESI, offset Opos
		MOV AL, BYTE PTR [ESI]	 ;first char of oldWord
		CLD
		REPNE SCASB					 ;scan until we find it in text
		JNE Find_Finish
	; found 1st char of oldWord in text
			PUSH ECX						  ;save count
			PUSH EDI						  ;save pointer
			MOV ESI, EDI 
			MOV EDI, offset Opos		  ;second character
			INC EDI
			MOV ECX, oldWord_Length			  ;ECX = length of text - 1
			DEC ECX
			REPE CMPSB						  ;scan until we find mismatch
			JNE skip						  ;no mismatch - so we found str2
				MOV EAX, Tpos
				ADD EAX, text_Length
				CMP EAX, ESI
				JE Check_Space_Before
					MOV AL, BYTE PTR [ESI]
					CMP AL, ' '					; checks if there was an SPACE after word
					JE Check_Space_Before
						CMP AL, 0dh				; checks if there was an ENTER after word
						JNE skip				; skips this word if there wasn't SPACE nor Enter after it
				Check_Space_Before:
						SUB ESI, oldWord_Length ; puts ESI on the first sentence
						MOV EAX, Tpos
						CMP EAX, ESI
						JE succeed
							DEC ESI
							MOV AL, BYTE PTR [ESI]
							CMP AL, ' '
							JE succeed
								CMP AL, 0ah
								JE succeed
			skip:
	; false => resume search
				POP EDI						  ;pointer from stack
				POP ECX						  ;count from stack
				JMP resume					  ;resume search
	; succeed - second string found in first
		succeed:
			POP EDI						;point to char AFTER 1st match
			POP ECX				
			INC EDX
			JMP resume
		Find_Finish:
			MOV EAX, EDX
			mwrite"The Number of matched words are : "
			
call writedec
	Ret
Find ENDP

;procedure CountWord first it compares the text length it 0 then it will jump to finsh and return 0 
;else
;ESI point on the text 
;and ECX the length of the text
;and it will compares each BYTE if it a space 32( ascci) it will count it as a word and
;if it founds a new line (13 ascci) it will count it as a word any thing else will ignore it.
;-----------------------------------------------------------------------
;countWord PROC counts the number of Words in the heap
;Returnes: the counter of Words in EAX
;-----------------------------------------------------------------------
countWord PROC                    
	CMP text_length, 0
	JE FinishWord
		MOV Wcounter, 0
		MOV ESI, Text
		MOV ECX, Text_Length
	Do:
		
		CMP BYTE PTR [ESI], 32
		Jne next3	
		cmp ecx,Text_Length
		je next3
		cmp byte ptr [esi-1],32
		je next3
		INC Wcounter
		next3:
		CMP BYTE PTR[ESI], 13
		JNE next4
			INC Wcounter
		next4:
			INC ESI
			DEC ECX
			CMP ECX, 0
			JE FinishWord
	JMP Do
FinishWord:
	MOV EAX, Wcounter     ;return the counter in EAX
	mwrite"The number of Words in the File : "
call writedec	
	Ret
countWord ENDP

;this procedure will count the number of characters without the space char(32) or the nl(10) char or cr char (13) 
;if it finds 32 or 10 or 13 then will ignore it
;else will count it as a char
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-----------------------------------------------------------------------
;countChars PROC counts the number of characters in the heap
;Returnes: the counter of characters in EAX
;-----------------------------------------------------------------------
countChars PROC                      
	CMP text_length, 0
	JE Finishchars
		MOV ccounter, 0
		MOV ESI, Text
		MOV ECX, Text_Length
		Do2:
			CMP BYTE PTR [ESI], 32
			JE next2
				CMP BYTE PTR [ESI], 10
				JE next2
					CMP BYTE PTR [ESI], 13
					JE next2
			INC Ccounter
			next2:
				INC ESI
				DEC ECX
				CMP ECX, 0
				JE FinishChars
		JMP Do2
	FinishChars:
		MOV EAX, 0
		MOV EAX, Ccounter

mwrite"The number of Chars in the File : "
call writedec	
	Ret
countChars ENDP

;-----------------------------------------------------------------------
;NewFile PROC creates an empty new file
;Recieves: 1 offset parameter File Name
;Returnes: file handle in EAX
;-----------------------------------------------------------------------
NewFile PROC F_Name:PTR BYTE
	MOV EDX, F_Name
	CALL CreateOutputFile	
	MOV filehandle, EAX
	CALL closeFile
	Ret
NewFile ENDP 

;-----------------------------------------------------------------------
;remove PROC deletes existing word from file R times
;Recieves: 3 parameters oldword,number of deletion ,word and number of replacement (R)
;-----------------------------------------------------------------------
remove PROC, oldpos:PTR BYTE, LengthWord:DWORD , r_num:DWORD
	MOV EAX, text
	MOV txtpos, EAX
	MOV EAX, LengthWord
	MOV oldWord_Length, EAX

	CMP r_num, 0
	JE No_Change
		MOV EAX, 0
		MOV ECX, r_num
		Do:
			PUSH ECX
			MOV ESI, txtpos
			MOV EDI, oldpos
			INVOKE isExist, ESI , EDI
			CMP EAX, -1
			JE No_Change
				MOV EBX, ESI
				ADD EBX, EAX								; EBX = [Address of txtpos] + EAX
				INVOKE Str_remove, EBX, oldWord_Length      ;CALL the remove procedure
			POP ECX
		LOOP Do
	No_Change:
	Ret
remove ENDP

;----------------------------------------------------------------------------
;Str_remove PROC deletes the first word of the given text
;Recieves: offset of the starting position and number of chars to be removed
;----------------------------------------------------------------------------
Str_remove PROC, position:PTR BYTE, chars:DWORD
	INVOKE Str_length, position                     ;get the length of the string from the starting position
	MOV ECX, EAX                                    ;set ECX to the length of the string
	INC ECX                                         ;ADD one for the null BYTE
	SUB ECX, chars                                  ;set the counter for the REP
	MOV ESI, position                               ;point to the begining of the string
	ADD ESI, chars									;point to first character to copy
	CMP BYTE PTR [ESI], 0 ;under test
	JE last_char
		INC ESI
	last_char:
	MOV EDI, position                               ;point to position to begin
	CLD                                             ;COPY STRING OVER
	REP MOVSB
;Edit the length of the text
	MOV EAX, text_Length
	SUB EAX, oldWord_Length
	DEC EAX
	MOV text_Length, EAX
	Ret
Str_remove ENDP

;-----------------------------------------------------------------------------
;isExist PROC Checks if the file contains the entered word or not
;Recieves: 2 offsets parameters file's text and the entered word
;Return: If the word was found successfully, EAX contains the index of the word
;		in the file's text. Otherwise, EAX equals -1
;-----------------------------------------------------------------------------
isExist PROC USES ESI EDI, t_pos:PTR BYTE, o_pos:PTR BYTE
	CLD
	MOV EDI, t_pos					 ;ES:DI => text
	MOV ECX, text_Length			 ;ECX = length of text
	resume:
		MOV ESI, o_pos
		MOV AL, BYTE PTR [ESI]	 ;first char of oldWord
		REPNE SCASB					 ;scan until we find it in text
		JNE Not_Found				 ;didn't find it
	; found 1st char of oldWord in text
			PUSH ECX						  ;save count
			PUSH EDI						  ;save pointer
			MOV ESI, EDI 
			MOV EDI, o_pos		  ;second character
			INC EDI
			MOV ECX, oldWord_Length			  ;ECX = length of text - 1
			DEC ECX
			REPE CMPSB						  ;scan until we find mismatch
			JNE skip						  ;no mismatch - so we found str2
				MOV EAX, t_pos
				ADD EAX, text_Length
				CMP EAX, ESI
				JE Check_Space_Before
					MOV AL, BYTE PTR [ESI]
					CMP AL, ' '					; checks if there was an SPACE after word
					JE Check_Space_Before
						CMP AL, 0dh				; checks if there was an ENTER after word
						JNE skip				; skips this word if there wasn't SPACE nor Enter after it
				Check_Space_Before:
						SUB ESI, oldWord_Length ; puts ESI on the first sentence
						MOV EAX, t_pos
						CMP EAX, ESI
						JE succeed
							DEC ESI
							MOV AL, BYTE PTR [ESI]
							CMP AL, ' '
							JE succeed
								CMP AL, 0ah
								JE succeed
			skip:
	; false => resume search
				POP EDI						  ;pointer from stack
				POP ECX						  ;count from stack
				JMP resume					  ;resume search
	; succeed - second string found in first
		succeed:
			POP ESI						;point to char AFTER 1st match
			POP ECX						;(length text - offset 1st match -1)
			MOV EAX, text_Length
			DEC EAX
			SUB EAX, ECX				;calculate #chars AFTER matched-string to move
			JMP finish
		Not_Found:
			MOV EAX, -1
		finish:
	Ret
isExist ENDP

;------------------------------------------------------------------------------------
;replace PROC changes a specific word from file with new one R times
;Recieves: 3 offsets parameters  oldWord, newWord and number of replacement (R)
;------------------------------------------------------------------------------------
replace PROC, oldpos:PTR BYTE, newpos:PTR BYTE, r_num:DWORD, OldWordLength:DWORD, NewWordLength:DWORD
	
	MOV EAX, text
	MOV txtpos_replace, EAX 


	MOV EAX, OldWordLength
	MOV oldWord_Length, EAX

	MOV EAX, NewWordLength
	MOV newWord_Length, EAX

	CMP r_num, 0
	JE No_Change
		MOV EAX, 0
		MOV ECX, r_num
		Do:
			PUSH ECX
			MOV ESI, txtpos_replace
			MOV EDI, oldpos
			INVOKE isExist, ESI , EDI
			CMP EAX, -1
			JE No_Change
				MOV EBX, ESI
				ADD EBX, EAX								; EBX = [Address of txtpos] + EAX
				MOV EAX, newpos								; EAX = [Address of newpos]
				INVOKE Str_replace, EBX, EAX, oldWord_Length
			POP ECX
		LOOP Do
	No_Change:
	Ret
replace ENDP

;----------------------------------------------------------------------------
;Str_replace PROC Changes the first word of the given text with the new one
;Recieves: 2 offsets the starting position of old word in file, new word and 
;           the number of characters of the old word
;----------------------------------------------------------------------------
Str_replace PROC USES ECX EBX EAX, Tposition:PTR BYTE, Nposition:PTR BYTE, chars:DWORD
	pushad
	INVOKE Str_length, Tposition
	MOV EDX, EAX								; EDX = text length

	INVOKE Str_length, Nposition			; EAX = new word length

	CMP EAX, chars
	JE Word_Equal
	JA NewWord_Bigger
		MOV ESI, Nposition
		MOV EDI, Tposition
		MOV ECX, EAX
		CLD
		REP MOVSB
		MOV ESI, Tposition
		ADD ESI, chars
		MOV ECX, EDX
		SUB ECX, chars
		INC ECX
		CLD
		REP MOVSB
		JMP Next
	NewWord_Bigger:
		MOV ESI, Tposition
		ADD ESI, EDX
		MOV EDI, Tposition
		MOV ECX, EDX
		ADD ECX, EAX
		SUB ECX, chars
		ADD EDI, ECX
		INC ECX ; under test
		
		STD
		REP MOVSB
	Word_Equal:
		MOV EDI, Tposition
		MOV ESI, Nposition
		MOV ECX, EAX
		CLD
		REP MOVSB
	Next:
	; Edit the length of the text
	MOV EBX, chars
	SUB text_Length, EBX
	ADD text_Length, EAX
	popad
Ret
Str_replace ENDP

END main