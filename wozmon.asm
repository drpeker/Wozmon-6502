;		  
; EWoz Monitor SYMON monitor version
; Ported from Hans Otten, 2024
; Idea and this version by Ronny Ribeiro, placed in the unused space in KIM tape ROM at $1AA0
; X command returns to KIM monitor
; Based upon a version by Jim McClanahan, W4JBM, April 2021
;
; Based on EWoz 1.0 by fsafstrom, March 2007. Relevant notes from original:
;
; The EWoz 1.0 is just the good old Woz mon with a few improvements and extensions so to say.
;
; It prints a small welcome message when started.
; All key strokes are converted to uppercase.
; The backspace works so the _ is no longer needed.
; When you run a program, it's called with an JSR so if the program ends
;   with an RTS, you will be taken back to the monitor.
;
; Also incorporated some changes from the Glitch Works and TangentDelta
; version created for the R65x1Q SBC.
;
; Notes for this version (1.0P as in 'PAL-1'):
;
; Currently designed to load into RAM.
; Inits itself 6550 ACIA for serial I/O for standalone operation.
; Code Quits to SYMON monitor with Q command.
;		  
;ACIA device address:
SIODAT   .EQU $8000    ;ACIA data register   <--put your 6551 ACIA base address here (REQUIRED!)************************
SIOSTAT  .EQU SIODAT+1 ;ACIA status REGISTER
SIOCOM   .EQU SIODAT+2 ;ACIA command REGISTER
SIOCON   .EQU SIODAT+3 ;ACIA control REGISTER
;		  
SYMON	.EQU 	$EAF4  				; SYMON ROM monitor start
	
;

; 
; Keys and Characters
;
BS		= 	$08					; Backspace
LF		= 	$0A					; Line Feed
CR		=	$0D					; Carriage Return
ESC		= 	$1B					; Escape
FF		= 	$0C					; Form feed 
SPC		= 	' '					; Space
DOT		= 	'.'					; Period
COLON	= 	':'					; Colon  
BACKSL	= 	$5C					; back slash 
CTRLQ   = 	$11					; Ctrl-Q to enter SYMON monitor
 
; eWoz Page Zero Usage
; 
XAML	= 	$30				; start at free space in upper zero page
XAMH	= 	XAML + 1
STL		= 	XAMH + 1
STH		= 	STL + 1
L		= 	STH + 1
H		= 	L + 1
YSAV	= 	H + 1
MODE	= 	YSAV + 1
COUNTR	= 	MODE + 1
SADDR   = 	COUNTR+1  					  

;
; input buffer
;
IN		= 	$0300


; To run in ROM change .ORG line below. And add your own vertor table to end of code!!!!
		.ORG $1000

			SEI 	   ; Disable interrupts
			LDX #$FF    ; Stack sayfa 1'in en üstünden başlasın
			TXS         ; SP = X
			JSR INIT_ACIA   ; Initialize ACIA for 19200 @1.8432 Mhz, 
                            ;8-N-1, no IRQ, no handshake
		



START 		CLD					; Clear decimal			
     		;LDX 	#$FF		; Clear stack
     		;TXS
SFTRST		LDA 	#ESC		; Load Escape key
NOTCR		CMP 	#BS			; was it backspace?
     		BEQ 	BCKSPC
     		CMP 	#ESC		; escape?
     		BEQ 	ESCAPE
        	INY					; increment buffer index
     		BPL 	NXTCHR		; buffer > 127?
ESCAPE		LDA 	#FF			; clear screen
     		JSR 	ECHO		
     		LDA 	#BACKSL		; show \
     		JSR 	ECHO
GETLIN		JSR 	OUTCRLF		; print CRLF
     		LDY 	#$01		; initialize buffer index
BCKSPC		DEY					; backspacing
     		BMI 	GETLIN		; too far start again
     		LDA 	#SPC		; overwrite 
     		JSR 	ECHO
     		LDA 	#BS			; and BS again
     		JSR 	ECHO
NXTCHR		JSR 	GETKEY		; get next char
     		CMP 	#$60
     		BMI 	CNVRT
     		AND 	#$5F		; convert to uppercase
CNVRT		STA 	IN,Y		; add to buffer
     		JSR 	ECHO		; and show on screen
     		CMP 	#CR			; CR?
     		BNE 	NOTCR
     		LDY 	#$FF		; reset text index
     		LDA 	#$00
     		TAX
SETSTR		ASL 	A
SETMOD		STA 	MODE
BLSKIP		INY
NXTITM		LDA 	IN,Y
     		CMP 	#CR			; CR?
     		BEQ GETLIN
     		CMP 	#DOT		; . ?
     		BCC 	BLSKIP
     		BEQ 	SETMOD
     		CMP 	#COLON		; : ?
     		BEQ 	SETSTR		; store mode
     		CMP 	#'R'		; R?
     		BEQ 	RUN			; run user program
     		CMP 	#'Q'		; Q? 
     		BEQ 	QSYMON		; back to SYMON monitor
     		STX 	L			; clear L H
     		STX 	H
     		STY 	YSAV
NXTHEX		LDA 	IN,Y
     		EOR 	#$30		; map digits 0-9
     		CMP 	#$0A		; digit?
     		BCC 	DIG
     		ADC 	#$88		; map letter A_F to FA - FF
     		CMP 	#$FA		; hex character?
     		BCC 	NOTHEX		; not hex
DIG			ASL 	A
     		ASL 	A			; hex digit to MSD of A
     		ASL 	A
     		ASL 	A
     		LDX 	#$04		; shift count
HEXSFT		ASL 	A			; hex digit left MSB to carry
     		ROL 	L
     		ROL 	H			; rotate in LSD MSD
     		DEX
     		BNE 	HEXSFT
     		INY
     		BNE 	NXTHEX		; loop 4
NOTHEX		CPY 	YSAV
     		BNE 	NOESC
     		JMP 	SFTRST		; reset EWOZ
QSYMON		JMP 	SYMON		; Start S O S monitor
RUN			JSR 	RUNU	
     		JMP 	SFTRST
RUNU		JMP 	(XAML)
;
NOESC		BIT 	MODE		; test MODE
     		BVC 	NOTSTR		; bit 6 = 0 for stoe, 1 for XAM and block XAM
     		LDA 	L
     		STA 	(STL,X)		; current store address
     		INC 	STL	
     		BNE 	NXTITM		; next item
     		INC 	STH
TONXIT		JMP 	NXTITM
;
NOTSTR		LDA 	MODE
     		CMP 	#DOT
     		BEQ 	XAMNXT
     		LDX 	#$02
SETADR		LDA 	L-1,X		; copy hex data store index
     		STA 	STL-1,X
     		STA 	XAML-1,X
     		DEX					; next 2 bytes
     		BNE 	SETADR
NXTPRN		BNE 	PRDATA
     		JSR 	OUTCRLF
     		LDA 	XAMH		; output in hex data
     		JSR 	PRBYTE
     		LDA 	XAML
     		JSR 	PRBYTE
     		LDA 	#COLON
     		JSR 	ECHO
PRDATA		LDA 	#SPC
     		JSR 	ECHO
     		LDA 	(XAML,X)	; output hex
     		JSR 	PRBYTE
XAMNXT		STX 	MODE
     		LDA 	XAML		; compare index
     		CMP L
     		LDA XAMH
     		SBC H
     		BCS TONXIT
     		INC XAML
     		BNE MD8CHK			; increment examine index
     		INC $31
MD8CHK		LDA $30
     		AND #$07            ; 8 Data columns in every line (like original)
     		BPL NXTPRN
PRBYTE		PHA					; save A for LSD
     		LSR A
     		LSR A
     		LSR A
     		LSR A
     		JSR PRHEX			; ouput hex digit			
     		PLA
PRHEX		AND #$0F
     		ORA #'0'
     		CMP #$3A			; digit?
     		BCC ECHO
     		ADC #$06			; offset for hex char
ECHO		STA SADDR
     		TYA
     		STA SADDR+1
     		LDA SADDR
     		AND #$7F
     		JSR OUTCH			; strip upper bit
     		LDA SADDR+1
     		TAY
     		LDA SADDR
     		RTS
;
;	KIM-1 GETCH routine with echo suppression
;			
			
GETKEY		TYA
     		STA SADDR+1			; save Y
     		;LDA SBD				; mask lowbit
     		;AND #$FE			
     		;STA SBD			 	; set echo port to block
     		JSR GETCH			; Get a character in A  
     		STA SADDR
     		;LDA SBD				; clear echo port
     		;ORA #$01
     		;STA SBD
     		LDA SADDR+1			; restore Y
     		TAY
     		LDA SADDR			; restore read character
     		RTS
;
; Print CR and LF 
;
OUTCRLF		TYA					; print CRLF. saves Y
     		STA SADDR+1			; save Y
     		LDA #CR
     		JSR OUTCH
     		LDA #LF
     		JSR OUTCH
     		LDA SADDR+1			; restore Y
     		TAY
     		RTS

; ------------------------------------------------------------
; INIT_ACIA - Init routine for 6551 ACIA @19200 baud, 8-N-1, no IRQ, no handshake
; ------------------------------------------------------------
INIT_ACIA
        LDA #$1F        ; Control: 19200 baud (x16 clock), 8 data bits, 1 stop bit, no parity
        STA SIOCON

        LDA #$0B        ; Command: No IRQ, No echo, DTR & RTS active (but no handshake), TX enabled, RX enabled
        STA SIOCOM

        RTS

; ------------------------------------------------------------
; ACIA_TX - Transmit a character (A register holds the char)
; Waits for transmit buffer to become empty.
; ------------------------------------------------------------
OUTCH
        PHA
WAIT_TX
        LDA SIOSTAT
        AND #$10        ; Bit 4: Transmit buffer empty
        BEQ WAIT_TX
        PLA
        STA SIODAT
        RTS

; ------------------------------------------------------------
; ACIA_RX - Receive a character into A register
; Waits for receive data to be ready.
; ------------------------------------------------------------
GETCH
WAIT_RX
		NOP
		NOP
        LDA SIOSTAT
        AND #$08        ; Bit 3: Receive data ready
        BEQ WAIT_RX
        LDA SIODAT
        RTS
			
			.END

