
; Motorola DSP56300 Assembler  Version 6.3.15   07-09-09


;*************************************************************
; EQUIVALENTS
;*************************************************************
CMDVERSION	EQU	$76		;'v'
CMDOUTPUT	EQU	$6F		;'o'
CMDINPUT	EQU 	$69		;'i'
CMDDELAY	EQU 	$64		;'d'
CMDDELAYLONG	EQU	$44		;'D'
CMDSQUARE	EQU 	$71		;'q'
CMDRAMP		EQU 	$72		;'r'
CMDRAMPREAD 	EQU 	$52		;'R'
CMDTIPSETUP 	EQU	$74		;'t'
CMDGIANT	EQU	$67		;'g'
CMDAPPROACH	EQU	$54		;'T'
CMDMINUS	EQU 	$2D		;'-'
CMDPLUS		EQU	$2B		;'+'
CMDSTOP		EQU	$21		;'!'
CMDSCANSETUP 	EQU 	$73		;'s'
CMDSCANLINE	EQU	$53		;'S'
CMDHIGHRES	EQU 	$48		;'H'
CMDTRACKSETUP 	EQU 	$6B		;'k'
CMDTRACK	EQU	$4B		;'K'
CMDTRACKCONT	EQU	$63		;'c' to continue tracking
CMDRSSETUP	EQU	$51		;'Q'
CMDSPSETUP	EQU	$79		;'y'
CMDSPEC		EQU	$59		;'Y'
CMDDIAGSETUP 	EQU 	$6C		;'l'
CMDDIAG		EQU 	$4C		;'L'
CMDHOPSETUP	EQU 	$6A		;'j'
CMDHOP		EQU 	$4A		;'J'
CMDDEPSETUP 	EQU 	$70		;'p'
CMDDEP		EQU	$50		;'P'
CMDUPDATE	EQU 	$23		;'#'
CMDDAC		EQU 	$61		;'a'
CMDDEBUGGET	EQU 	$62		;'b'
CMDDEBUGSET	EQU 	$42		;'B'


VER1	EQU		$303030		;version "000 03102003"
VER2	EQU		$333020		;note that LSB is sent first!
VER3	EQU		$323031
VER4	EQU		$333030

MAXDATA	EQU		2048		;maximum number of data can store in x:

ADCTIME		EQU		1750	;ADC conversion time, limited by ADC chip
;ADCTIME	EQU		1	;for debugging(!!!)
DIOOUTTIME	EQU		259
OUTPUTFUDGE	EQU		43
;DIOOUTTIME	EQU		1
;OUTPUTFUDGE	EQU		1

BABYWAITI	EQU		$A85F7  ; 8 ms
BABYWAITII	EQU		$693BA	; 5 ms
;BABYWAITI	EQU		$1
;BABYWAITII	EQU		$1

AUTOWAIT	EQU		$A85F7  ; 8 ms, in principle, 
					; it depends on feedback response
;AUTOWAIT	EQU		$1

TRACKNU		EQU		10		;number of iterrations for tracking

;*************************************************************
; HOST INTERFACE (GPIO) REGISTER SETTINGS
;*************************************************************

;********************* Host Port Control Register ********************* 
HPCR	EQU $000001

;********************* Host Data Direction Register ********************* 
HDDR	EQU $0000FF
;Bit 15	Input
;Bit 14	Input
;Bit 13	Input
;Bit 12	Input
;Bit 11	Input
;Bit 10	Input
;Bit 9	Input
;Bit 8	Input
;Bit 7	Output
;Bit 6	Output
;Bit 5	Output
;Bit 4	Output
;Bit 3	Output
;Bit 2	Output
;Bit 1	Output
;Bit 0	Output

;********************* Host Port Data Masks ********************* 
CHMASK		EQU	$F0				; mask to get channel

CHDUPPERMSK	EQU	$DF				; ch13($D) upper 8 bits
UPPERMSK	EQU	$FF00			; upper 8 bits
UPDATAMSK	EQU	$0F00			; upper 4 data bits
HUPDATAMSK	EQU	$F000			; upper 4 data bits for high res. channels
LOWERMSK	EQU 	$00FF			; lower 8 bits
DATAMSK		EQU 	$0FFF			; all 12 data bits

NUMOUTCHS   EQU $10				; total number of output channels
BIASCH		EQU	$00			; output channel 0 - bias
ZOFFCH		EQU	$10
XCH		EQU	$20			; output channel 2 - x
YCH		EQU	$30			; output channel 3 - y
ZOCH		EQU 	$40			; output channel 4 - z outer
FBACKCH		EQU 	$E0			; output channel 14 - feedback
DITHERCH	EQU 	$D0			; output channel 13 - dither

ICH		EQU $00				; input channel 0 - current
ZCH		EQU $01				; input channel 1 - feedback z

ZERO		EQU	$0800			; output in bits that equals 0 volts
OUTMAX		EQU	$0FFF			; maximum output
INZERO		EQU	$8000			; input in bits that equals 0 volts
INMAX		EQU	$FFFF			; input in bits that equals 0 volts

BABYSIZE	EQU $7FF			; baby step size = 2047 bits = 100 V

READSEQOCH		EQU $0F0000			; read sequence output ch mask
READSEQICH		EQU $0C0000			; read sequence input ch mask
READSEQODATA	EQU $00FFFF			; read sequence output data mask
READSEQAVG		EQU	$03FFFF			; read sequence # of avg's mask

FBACKMSK	EQU	$00FFFB			; and with current ch 14 to clear fback bit
DITH0MSK	EQU	$00F7FF			; and with current ch 13 to clear dither0 bit
DITH1MSK	EQU	$00FDFF			; and with current ch 13 to clear dither1 bit
			;not $00FBFF, corrected by SW

;*************************************************************
; ESSI1 (PORT D / GPIO) REGISTER SETTINGS
;*************************************************************

;********************* Port D Control Register ********************* 
PCRD	EQU $000000
;Reserved 23-6	should be 0
;Bit 5	GPIO PD5 <-> STD1
;Bit 4	GPIO PD4 <-> SRD1
;Bit 3	GPIO PD3 <-> SCK1
;Bit 2	GPIO PD2 <-> SC12
;Bit 1	GPIO PD1 <-> SC11
;Bit 0	GPIO PD0 <-> SC10

;********************* Port D Data Direction Register ********************* 
PRRD	EQU $00003F
;Reserved 23-6	should be 0
;Bit 5	Output, but currently unused
;Bit 4	Output (CB4)
;Bit 3	Output (CB3)
;Bit 2	Output (CB2)
;Bit 1	Output (CB1)
;Bit 0	Output (CB0)

;********************* Port D Data Masks ********************* 
CBMSKIN		EQU	$1C		;'and' with this to keep CB2,3,4
CBMSKOUT	EQU	$03		;'and' with this to keep CB0,1

;*************************************************************
; SERIAL CABLE INTERFACE (SCI) REGISTER SETTINGS
;*************************************************************

;********************* SCI Control Register ********************* 
SCR		EQU	$000002
;Bit 16		REIE	0	RX exception interupt disabled
;Bit 15		SCKP	0	Clk polarity positive
;Bit 14		STIR	0	Timer int rate / 32
;Bit 13		TMIE	0	Timer interrupt disabled
;Bit 12		TIE		0	TX interrupt enabled
;Bit 11		RIE		0	RX interrupt enabled
;Bit 10		ILIE	0	Idle interrupt enabled
;Bit 9		TE		0	TX (transmit) disabled until below
;Bit 8		RE		0	RX (receive) disabled
;Bit 7		WOMS	0	Point-to-Point
;Bit 6		RWU		0	RX wakeup disabled
;Bit 5		WAKE	0	Wakeup idle line
;Bit 4		SBK		0	Send break disabled
;Bit 3		SSFTD	0	Shift MSB first
;Bit 0-2	WDS		010	Mode = 2: 10-bit async (1 start, 8 data, 0 parity, 1 stop)

;********************* SCI Clock Control Register ********************* 
SCCR	EQU	$001022
;Bit 15		TCM		0	Internal Clk for transmit
;Bit 14		RCM		0	Internal Clk for receive
;Bit 13		SCP		0	Prescaler = divide by 1
;Bit 12		COD		1	Clk divide before output = 1
;Bit 11-0	CD		000000100010	Divide Ratio = 35
; 7 * 12.288 MHz / (64 * 35) = 38400 bps

;********************* Extended Memory Settings ***********************
AAR0_V		EQU		$040811
BCRV        EQU		$012581
EXT_BEGIN	EQU		$040000

;*************************************************************
; MACRO DEFINITIONS
;*************************************************************
DELAY	macro	CYCLES		;CYCLES = length of delay in clock cycles
	move	CYCLES,x0		;1 CYCLE = 11.625744 ns when PLL = 86.016 MHz
	rep		x0				;Beware of call to DELAY with CYCLES = 0 !
	nop						;rep 0 = rep 0xFFFF !
	endm

READIN	macro	INCHAN		; INCHAN = input channel (0,1,2,3)
	clr		a
	move	INCHAN,a1
	jsr		INPUTBITS		; read
	move	a1,x0
	move	a0,x1
	clr		b
	move	x1,b1
	lsl		#8,b
	add		x0,b
	endm


;*************************************************************
; X MEMORY ORGANIZATION
;*************************************************************
	org	x:0

x_data		ds 2048		;use all of x memory for data storage

;*************************************************************
; EXTENDED X MEMORY ORGANIZATION
;*************************************************************
	org x:EXT_BEGIN

x_ext		ds 64000	;64K of the extended memory


;*************************************************************
; Y MEMORY ORGANIZATION
;*************************************************************
	org	y:0

CB_Box		ds	1		;control bits for conversion module
Ch13_uBits	ds	1		;ch 13 upper 8 bits
Ch15_Bits	ds	1		;channel 10 and 11 upper bits
OUT_ADDRESS	EQU *
Out_Bits	ds	16		;the values that have been sent to the electronics
					;format $CDDD
z_off_ch	ds	1		;z offset channel
z_offset	ds	1		;not updated continuously!

Tip_Zo_Mult	ds	1		;giant steps: zo parabola multiplier (bits)
Tip_Zo_Step	ds	1		;giant steps: Number of cycles required for each zo parabola
Tip_X_Mult	ds	1		;giant steps: x parabola multiplier (bits)
Tip_X_Step	ds	1		;giant steps: Number of cycles required for x parabola
Tip_X_Zero	ds	1		;giant steps: Number of cylces to ouput 0 V before parab
Tip_Delay	ds	1		;giant steps: delay

Tip_Num_Giant	ds	1	;tip approach: number of giant steps
Tip_Baby_Size	ds	1	;tip approach: baby step size
Tip_Min_I		ds	1	;tip approach: minimum tunnelling current
Tip_Baby_Mult	ds	1	;tip approach: baby return parabola multiplier
Tip_Update		ds	1	;tip approach: false trigger I or real trigger V

Scan_Protection	ds	1	;crash protection during scan
						; 00 : do nothing
						; 01 : stop scanning
						; 10 : change z offset (auto plus to 0V) and continue 
						; 11 : find min/max to auto plus and continue
Scan_Limit		ds	1	;crash protection if input val within this many bits of min/max
Scan_OldZ		ds	1   ;crash protection - for calculating delta z
Scan_DeltaZ		ds	1	;crash protection - offset needed for consistent z values after
						; protection
Scan_Size	ds 1		;scan: scan size in steps
Scan_Step	ds 1		;scan: size of one step in bits
Scan_Interdelay ds	1	;scan: interstep delay
Scan_Readdelay  ds  1	;scan: read delay
Scan_Flags		ds	1	; (#0): Set for Scan_DeltaZ (+), clear for Scan_DeltaZ (-)
						; (#1): Set when ABSIN finds we're above ZERO, cleared below ZERO
						;		Used by z min/max crash protection.

LastLine_Max	ds	1	;used with calc min/max z crash protection
LastLine_Min	ds	1	;used with calc min/max z crash protection
ThisLine_Max	ds	1	;used with calc min/max z crash protection
ThisLine_Min	ds	1	;used with calc min/max z crash protection

Track_Average	ds	1	;track setup: number of points for averaging 256 max
Track_Sample_Every	ds	1	;track setup: sample delay
Track_Delay		ds	1	;track setup: ??? delay
Track_Plane_a	ds	1	;track setup: plane fit "a" parameter
Track_Plane_b	ds	1	;track setup: plane fit "b" parameter

Spec_Movedelay	ds	1	;spectroscopy: delay at each bit
Spec_StepSize	ds	1	;spectroscopy: bits per step during ramp
Spec_Higher		ds	1	;spectroscopy: highest output value of ramp
Spec_Lower		ds	1   ;spectroscopy: lowest output value of ramp
Spec_HiResCh	ds	1	;spectroscopy: hires ch storage

ReadSeqNum		ds	1	;number of read sequence steps
ReadSeq_Ch		ds	1	;used by READSEQ to store input ch during reading
ReadSeq_Mult	ds	1	;used by READSEQ to store input multiplier during reading
ReadSeq			ds	128 ;the read sequence steps max of 2 words per step, 64 steps

Ramp_Count		ds	1	;counts how many times the OUTPUTBITS was called from the ramp
Ramp_Read_Every	ds	1	;tells the ramp how often to read the data
Ramp_Average	ds	1	;the current sum of the readings
Ramp_Average_Every ds 1 ;how many ponts to read during ramp
Ramp_Read_Delay	ds	1	;read delay

Diag_Long_Start	ds	1	;long ch initial value
Diag_Long_Final	ds	1	;long ch final value
Diag_Short_Start ds	1	;short ch initial value
Diag_Z_Start	ds	1	;z offset initial value
Diag_Step_Size	ds	1	;read every N bits on long ch
Diag_Ratio		ds	1	;slope of diag ramp (longbits/shortbits)
						;interpret as float with 12 bits before decimal, 12 bits after
Diag_Z_Ratio	ds	1	;slope of diag ramp in z offset (longbits/z_bits) for const height
						;interpret as float with 12 bits before decimal, 12 bits after
Diag_Move_Delay	ds	1	;move delay

Hop_Step_Delay	ds	1	;delay before reading z
Hop_IStep_Delay	ds	1	;interstep delay used for ramping
Hop_Circ_Delay	ds	1	;delay after each cirlce
Hop_R			ds	1	;radius in bits
Hop_R_Sqrt		ds	1	;radius*Sqrt(2)/2
Hop_Avg			ds	1	;number of z readings
Hop_Plane_a		ds	1	;plane fit a
Hop_Plane_b		ds	1	;plane fit b
Hop_Timer		ds	1	;the current timer value		

Send_to_x		ds	1	;the x coordinate to send the tip to
Send_to_y		ds	1	;the y coordinate to send the tip to
Send_Delay		ds	1	;delay	

Dep_BiasM		ds	1	;moving bias value
Dep_BiasW		ds	1	;writing bias value
Dep_IM			ds	1	;moving current value
Dep_IW			ds	1	;writing current value
Dep_Z_Offset	ds	1	;delta Z-offset
Dep_Wait		ds	1	;wait between offset and changing bias
Dep_Num_Pts		ds	1	;number of points to be read during the 
Dep_Delay		ds	1	;the delay between two readings in the deposition 
Dep_Aver_Pts	ds	1	;number of points to average during the pulse
Dep_Ignore_Init	ds	1	;number of points after which the pulse can be stopped
Dep_Chng_T		ds	1	;if the change is effective after this number of points stop pulse
Dep_After_T		ds	1	;number of points to wait before terminating the pulse
Dep_Chng_Ratio	ds	1	;percent of change
Dep_Aver_Every	ds	1	;number of points to average during read
Dep_Aver_Inv	ds	1	;one over the average
Dep_Flags		ds	1	;all the boolean parameters
						;2 LS bits are the input channel (None,I,Z)
						;bit 2 is feedback on/off
						;bit 3 is clear to ramp bias and is set 
						;to step bias to deposition conditions
						;bit 4 is clear for fixed length pulse
						;bit 5 is set if bias is on ch10 and 
						;is clear if bias is on ch0
						;bit 6 is use move yes/no
						;bit 7 not used
Dep_Temp		ds	1	;used to temporary store data
Dep_Temp2		ds	1	;used in SW's deposition

Auto_Target		ds	1	;the target z value for auto+/- (16 bit)

Debug_Var1		ds	1

;*************************************************************
; STM PROGRAM
;*************************************************************
	org	p:0

;*************************************************************
; SETUP
; Initialize phase-locked loop, serial comm interface and GPIO.
;*************************************************************
SETUP
	movep	#$040006,x:M_PCTL	; Use PLL as f multiplier for xtal oscillator 
								; 7 X 12.288 = 86.016MHz

	;Set up Host Port as GPIO
	movep	#HPCR,x:M_HPCR
	movep	#HDDR,x:M_HDDR
	move	#>$D0,y1			; initial Ch13 values; dithering off
	move	y1,y:Ch13_uBits

	;Set up ESSI1 (Port D) as GPIO
	movep	#PCRD,x:M_PCRD
	movep	#PRRD,x:M_PRRD
	move	#$000000,y1		;initialize CB's
	move	y1,y:CB_Box
	move	y1,b
	movep	b,x:M_PDRD

	;Initialize SCI
	movep	#SCCR,x:M_SCCR	;serial settings
	movep	#SCR,x:M_SCR	;serial settings
	movep	#$7,x:M_PCRE	;enable SCI
	bset	#8,x:M_SCR		;enable RX (receive)
	bset	#9,x:M_SCR		;enable TX (transmit)

	;Set the external memory to map from x:$040000 to x:$04FFFF
	movep	#AAR0_V,x:M_AAR0
	movep	#BCRV,x:M_BCR

	;Initialize variables
	move	#>0,y1
	move	y1,y:Scan_DeltaZ
	move	#>INZERO,y1
	move	y1,y:Auto_Target
	bset	#0,y:Scan_Flags
	move	#>Out_Bits,r0		; pointer to Y:Out_bits memory location
	move	#>0,a
	do		#>NUMOUTCHS,INIT_OUT_BITS
	move	a1,y1
	move	y1,y:(r0)+
	add		#>$1000,a
INIT_OUT_BITS
	clr		a

;*************************************************************
; MAIN_LOOP
; Receive and handle stm control characters via serial. 
;*************************************************************
MAIN_LOOP

	jclr	#2,x:M_SSR,*	;bit set when receive buffer full
							;until then, jmp to self
	movep	x:M_SRXL,a		;receive serial data


	move	#>CMDSCANLINE,x0
	cmpu	x0,a
	beq		SCANLINE
	move	#>CMDSCANSETUP,x0
	cmpu	x0,a
	beq		SCANSETUP
	move	#>CMDRSSETUP,x0
	cmpu	x0,a
	beq		READSEQSETUP
	move	#>CMDSPSETUP,x0
	cmpu	x0,a
	beq		SPECSETUP
	move	#>CMDSPEC,x0
	cmpu	x0,a
	beq		SPEC
	move	#>CMDMINUS,x0
	cmpu	x0,a
	bclr	#0,r1
	beq		AUTO
	move	#>CMDPLUS,x0
	cmpu	x0,a
	bset	#0,r1
	beq		AUTO
	move	#>CMDDEPSETUP,x0
	cmpu	x0,a
	beq		DEPSETUP
	move	#>CMDDEP,x0
	cmpu	x0,a
	beq		DEPOSITION
	move	#>CMDTRACKSETUP,x0
	cmpu	x0,a
	beq		TRACKSETUP
	move	#>CMDTRACK,x0
	cmpu	x0,a
	beq		TRACK
	move	#>CMDAPPROACH,x0
	cmpu	x0,a
	beq		TIPAPPROACH
	move	#>CMDGIANT,x0
	cmpu	x0,a
	beq		GIANT
	move	#>CMDTIPSETUP,x0
	cmpu	x0,a
	beq		TIPSETUP
	move	#>CMDOUTPUT,x0
	cmpu	x0,a
	beq		OUTPUT
	move	#>CMDHIGHRES,x0
	cmpu	x0,a
	beq		HIGHRES
	move	#>CMDINPUT,x0
	cmpu	x0,a
	beq		INPUT
	move	#>CMDDIAGSETUP,x0
	cmpu	x0,a
	beq		RAMPDIAGSETUP
	move	#>CMDDIAG,x0
	cmpu	x0,a
	beq		RAMPDIAG
	move	#>CMDRAMP,x0
	cmpu	x0,a
	beq		RAMP
	move	#>CMDRAMPREAD,x0
	cmpu	x0,a
	beq		RAMPREAD
	move	#>CMDDELAY,x0
	cmpu	x0,a
	beq		USERDELAY
	move	#>CMDDELAYLONG,x0
	cmpu	x0,a
	beq		USERDELAYLONG
	move	#>CMDVERSION,x0
	cmpu	x0,a
	beq		VERSION
	move	#>CMDSQUARE,x0
	cmpu	x0,a
	beq		SQUARE
	move	#>CMDHOPSETUP,x0
	cmpu	x0,a
	beq		HOPSETUP
	move	#>CMDHOP,x0
	cmpu	x0,a
	beq		HOP
	move	#>CMDDAC,x0
	cmpu	x0,a
	beq		DAC		;added to send out the values of 16 DAC channels. 
	move	#>CMDDEBUGGET,x0
	cmpu	x0,a
	beq		DEBUGGET		;added to retrieve the values of variable in Y memory.
	move	#>CMDDEBUGSET,x0
	cmpu	x0,a
	beq		DEBUGSET		;added to set the values of variable in Y memory.
	

	;FUTURE (?): jmp to peripheral command handler

endloop	jmp MAIN_LOOP		;infinite loop

;*************************************************************
; SCANSETUP
; Receive & store scan parameters.
;	TOUCHES: a, x0(via RECEIVE), y0
;	EXIT:    (y:*) parameter values
;*************************************************************
SCANSETUP

	jclr	#2,x:M_SSR,*	;receive crash protection
	movep	x:M_SRXL,a1
	move	a1,y0
	move	y0,y:Scan_Protection

	jsr		RECEIVETWO		;receive limit in bits
	move	a1,y0
	move	y0,y:Scan_Limit
	
	jsr		RECEIVETWO		;current z offset
	move	a1,y0
	move	y0,y:z_offset

	jclr	#2,x:M_SSR,*
	movep	x:M_SRXL,a1		;receive z offset ch bits
	lsl		#4,a
	move	a1,y0
	move	y0,y:z_off_ch

	jsr		RECEIVETWO		;scan size
	move	a1,y0
	move	y0,y:Scan_Size

	jsr		RECEIVETWO		;scan step size
	move	a1,y0
	move	y0,y:Scan_Step

	jsr		RECEIVETHREE
	move	a1,y0
	move	y0,y:Scan_Interdelay ;interstep delay

	jsr		RECEIVETHREE
	move	a1,y0
	move	y0,y:Scan_Readdelay ;read delay

	jmp		MAIN_LOOP

;*************************************************************
; READSEQSETUP
; Receive & store read sequence parameters. Note that since
; the number of steps in a read sequence may be any number between
; 1 and 64, the number of total bytes received will vary.
; Also, some steps are one word long, some are two. So, the number
; of words is not necessarily equal to the number of steps.
;	TOUCHES: a, x0(via RECEIVE), y0, y1
;	USES:	 (r7) : current readseq storage spot in y mem
;	EXIT:    (y:*) parameter values
;*************************************************************
READSEQSETUP
	
	move	#>ReadSeq,r7

	jclr	#2,x:M_SSR,*	;how many words will we receive?
	movep	x:M_SRXL,a1
	move	a1,y1

	jclr	#2,x:M_SSR,*	;how many read seq steps is that?
	movep	x:M_SRXL,a1
	move	a1,y0
	move	y0,y:ReadSeqNum

	do		y1,RSSETUP_LOOP

	jsr		RECEIVETHREE
	move	a1,y0
	move	y0,y:(r7)+		;each word of the read sequence

RSSETUP_LOOP

	jmp		MAIN_LOOP

;*************************************************************
; SCANLINE
; Scan one line by ramping an output channel and possibly performing
; a read sequence at each point. A SCANLINE without reading differs
; from a standard RAMP in that SCANLINE performs scan-specific crash
; protection.
;	TOUCHES: a, b, x0, x1, y0, y1
;	USES: (r0) : scan size in bits (steps * bits/step)
;		  (r1) : flags for AUTOSUB
;		  (r2) : flags
;		  (r3) : passes z offset to AUTOSUB
;		  (r4) : saves current output value during OUTPUTBITS
;		  (r5) : saves current z offset value during OUTPUTBITS
;		  (r6) : current location for data storage in x memory
;		  (r7) : input channel (future home of read seq info)
;	FLAGS: (#0) : set for forward (increasing) direction on output ch
;		   (#1) : set for reading during ramp
;		   (#2) : set for first bit of line... initialize ThisLine_Min,Max
;		   (#3) : set if crash protect stopped execution of scanline
;		   (#4) : set to reset all Line_Min and Line_Max
;		   (#5)	: set for high target for z min max, off for low target
;	EXIT:
;*************************************************************
SCANLINE

	move	#$000004,r2     	;initialize flags
	move	#x_data,r6			;points to location for storing data

	jclr	#2,x:M_SSR,*
	movep	x:M_SRXL,a1		;receive output ch bits, flags
							;in principle, output ch should be x or y
							;MS bit is read on/off
							;2nd MS bit is direction bit
							;3rd MS bit is on to clear Scan_DeltaZ and MIN/MAX
	move	a1,x1

	bclr	#5,x1			;test and clear read bit
	bcc		SCAN_DELTAZ_UNTOUCHED
	move	#>0,y0
	move	y0,y:Scan_DeltaZ
	bset	#4,r2
	jmp		CUED	
SCAN_DELTAZ_UNTOUCHED	
	move	y:ThisLine_Max,y0	; bump min/max variables
	move	y0,y:LastLine_Max
	move	y:ThisLine_Min,y0
	move	y0,y:LastLine_Min

CUED
	bset	#1,r2
	bclr	#7,x1			;test and clear read bit
	bcs		SCAN_READ_ON
	bclr	#1,r2
SCAN_READ_ON
	bset	#0,r2
	bclr	#6,x1			;test and clear direction bit
	bcs		SCAN_FORWARD
	bclr	#0,r2
SCAN_FORWARD

	;convert scan size to bits and put in r0	
	move	y:Scan_Step,r0		;bits / step
	move	y:Scan_Size,x0		;steps / scan line
	clr		a
	rep		r0
	add		x0,a			;crude multiplication
	move	a1,r0	

	;calculate starting value in b1
	lsr		a				; divide size by 2
	move	#>ZERO,b
	jset	#0,r2,FORWARD_LINE
	add		a,b
	sub		#1,b			; since (# of values > ZERO) = (# of values < ZERO) + 1
	jmp		CUEB
FORWARD_LINE
	sub		a,b
CUEB

	;splice in output ch
	move	x1,a1
	lsl		#12,a
	add		a,b	

	;output initial value, just to be safe
	move	b1,a1
	move	#>0,y1		;not necessary to pass channel, already spliced
	move	b1,r4		;save
	jsr		OUTPUTBITS

	move	y:Scan_Size,y0
	do	y0,TOTAL_LINE
	
	move	r4,b1

	move	y:Scan_Step,y1
	do	y1,TOTAL_STEP

	move	y:Scan_Interdelay,y0
	DELAY	y0

	move	b1,a1
	move	#>0,y1		;not necessary to pass channel, already spliced
	move	b1,r4		;save
	jsr		OUTPUTBITS

STEP_IT
	move	r4,b1		;restore
	;increment / decrement
	jclr	#0,r2,STEP_IT_BACK
STEP_IT_FWD
	add		#>1,b
	jmp		STEP_IT_DONE
STEP_IT_BACK
	move	b1,r4
	move	#>DATAMSK,x0
	and		x0,b
	move	#>0,x0
	cmpu	x0,b
	jeq		NOSUB			;backward goes one bit past low end unless low end is ZERO
	move	r4,b1
	sub		#>1,b
	jmp		STEP_IT_DONE
NOSUB
	move	r4,b1
STEP_IT_DONE
	nop

	;not implemented: dio digital feedback

TOTAL_STEP

	move	b1,r4

	; crash protection
	READIN	#>ZCH
	move	b1,y0
	move	y0,y:Scan_OldZ

	; min/max stuff
	bclr	#2,r2
	bcc		NOT_FIRST_BIT
FIRST_BIT						; on first bit, initialize min/max vars
	move	y0,y:ThisLine_Max
	move	y0,y:ThisLine_Min
	bclr	#4,r2
	bcc		DONE_MINMAX
FIRST_BIT_FIRST_LINE
	move	y0,y:LastLine_Max
	move	y0,y:LastLine_Min
	jmp		DONE_MINMAX
NOT_FIRST_BIT
	jsr		ZMINMAXUPDATE
DONE_MINMAX

	jsr		ABSIN
	clr		a
	move	#>INZERO,a
	move	y:Scan_Limit,y0
	sub		y0,a
	cmpu	a,b				;are we less than limit?
	jlt		SCAN_PROTECT_DONE

	jset	#1,y:Scan_Protection,ADJUST_Z_PROTECT
	jclr	#0,y:Scan_Protection,SCAN_PROTECT_DONE	; do nothing option
	
STOP_SCAN_PROTECT
	enddo
	bset	#3,r2
	jmp		TOTAL_LINE

ADJUST_Z_PROTECT
	jclr	#0,y:Scan_Protection,AUTO_Z_PROTECT
SCAN_MINMAX_PROTECT
	move	y:LastLine_Max,a
	move	y:LastLine_Min,x0
	sub		x0,a
	lsr		#1,a			; (Max - Min) /2
	move	#>INZERO,b

	jclr	#1,y:Scan_Flags,MINMAX_LOWTARGET
MINMAX_HIGHTARGET			; we're on high side
	add		a,b
	move	b1,y0
	jmp		SCAN_ADJUST_Z
MINMAX_LOWTARGET			; we're on low side
	sub		a,b
	move	b1,y0
	jmp		SCAN_ADJUST_Z
AUTO_Z_PROTECT
	move	#>INZERO,y0
SCAN_ADJUST_Z
	move	y:z_offset,r3
	move	y0,y:Auto_Target
	bset	#0,r1			;auto plus
	jsr		AUTOSUB
	move	r3,y:z_offset
	jclr	#2,r1,SCAN_ADJUST_DONE
	enddo					;end it if auto minus railed z offset
	bset	#3,r2
	jmp		TOTAL_LINE
SCAN_ADJUST_DONE
	READIN	#>ZCH

	move	y:Scan_OldZ,y0
	cmpu	y0,b
	jgt		MUST_SUB_IT
MUST_ADD_IT
	move	b1,y1
	move	y0,b1
	sub		y1,b
	jset	#0,y:Scan_Flags,MORE_OFF
	jmp		LESS_OFF

MUST_SUB_IT
	sub		y0,b
	jclr	#0,y:Scan_Flags,MORE_OFF
	jmp		LESS_OFF

MORE_OFF
	lsl		#4,b				; switch to expanded z format where bit 0 is 2^(-4) and bit 4 is 2^0.
	move	y:Scan_DeltaZ,y0
	add		y0,b
	jcs		OVERFLOW			; we've run out of room!
	move	b1,y0
	move	y0,y:Scan_DeltaZ
	jmp		SCAN_PROTECT_DONE

LESS_OFF
	lsl		#4,b				; switch to expanded z format where bit 0 is 2^(-4) and bit 4 is 2^0.
	move	b1,y1
	move	y:Scan_DeltaZ,y0
	move	y0,b
	cmpu	y1,b
	jlt		SWITCH_OFFSET
	sub		y1,b
	jmp		END_OFFSET
SWITCH_OFFSET
	bchg	#0,y:Scan_Flags
	move	b1,y0
	move	y1,b1
	sub		y0,b
END_OFFSET
	move	b1,y0
	move	y0,y:Scan_DeltaZ
	jmp		SCAN_PROTECT_DONE

OVERFLOW
	bset	#3,r2
	enddo
	jmp		TOTAL_LINE

SCAN_PROTECT_DONE
	jsr		READSEQ
	nop

TOTAL_LINE
	
	jset	#3,r2,SCAN_DATA_FAILURE
	jclr	#1,r2,SCAN_DATA_SUCCESS		; no data read
	clr		a
	move	r6,y0
	cmpu	y0,a
	beq		SCAN_DATA_SUCCESS
	jsr		XDSO			    ; out with the rest of the data
	jmp		SCAN_DATA_SUCCESS
SCAN_DATA_FAILURE
	move	#>CMDSTOP,a1		; notification of failure
	jmp		SCAN_DATA_CODA

SCAN_DATA_SUCCESS
	move	#>CMDSCANLINE,a1    ; notification of completion

SCAN_DATA_CODA
	jclr	#0,x:M_SSR,*	;bit set when transmit buffer empty
							;until then, jmp to self
	movep	a1,x:M_STXL		;send byte

	;if crash protecting, send current z offset, final output
	jset	#1,y:Scan_Protection,SCAN_NOTIFY
	jset	#0,y:Scan_Protection,SCAN_NOTIFY
	jmp		CUEC
SCAN_NOTIFY
	move	y:z_offset,y0
	move	y0,a1
	jsr		SERIAL_TWO		; z offset
	move	r4,a1
	move	#>DATAMSK,y0
	and		y0,a1			; send data only
	jsr		SERIAL_TWO		; final output

CUEC
	jmp		MAIN_LOOP

;*************************************************************
; SPECSETUP
; Receive & store spec parameters.
;	TOUCHES: a, x0(via RECEIVE), y0
;	EXIT:    (y:*) parameter values
;*************************************************************
SPECSETUP

	jsr		RECEIVETWO		;start (lower)
	move	a1,y0
	move	y0,y:Spec_Lower

	jsr		RECEIVETWO		;finish (higher)
	move	a1,y0
	move	y0,y:Spec_Higher

	jclr	#2,x:M_SSR,*
	movep	x:M_SRXL,a1		;bits per step (step size)
	move	a1,y0
	move	y0,y:Spec_StepSize

	jsr		RECEIVETHREE
	move	a1,y0
	move	y0,y:Spec_Movedelay ;move delay

	jsr		RECEIVETHREE
	move	a1,y0
	move	y0,y:Scan_Readdelay ;read delay - may be overwritten by SCANSETUP!

	jmp		MAIN_LOOP

;*************************************************************
; SPEC
; Performs one pass (ramp forward and backward) of a spectroscopy
; scan. Performs a read sequence at each point during the ramp
; if the appropriate flags are set.
;	TOUCHES: a, b, x0, x1, y0, y1
;	USES: (r0) : number of steps
;		  (r2) : flags
;		  (r4) : saves current output value during OUTPUTBITS
;		  (r6) : current location for data storage in x memory
;		  (r7) : read sequence (through READSEQ)
;		  y:Scan_DeltaZ is set to zero for compatability with READSEQ
;	FLAGS: (#0) : set to do READSEQ during forward half
;		   (#1) : always set for SPEC, needed for compatability with READSEQ
;		   (#2) : set to do READSEQ during backward half
;		   (#3) : set to use highresbits
;	EXIT:
;*************************************************************
SPEC
    clr		b				;initialize b
	move	#>0,y0
	move	y0,y:Scan_DeltaZ
	move	#>0,r2			;clear flags
	bset	#1,r2
	move	#x_data,r6

	jclr	#2,x:M_SSR,*
	movep	x:M_SRXL,b1		;receive output ch bits
					;bit #6, set for backward
					;bit #7, set for forward
					;bit #5, set for high resolution

	bset	#0,r2			;MS bit set for READSEQ during forward half
	bclr	#7,b1			
	bcs		SPEC_FWD
	bclr	#0,r2
SPEC_FWD

	bset	#2,r2			;2nd MS bit set for READSEQ during backward half
	bclr	#6,b1			
	bcs		SPEC_BKD
	bclr	#2,r2
SPEC_BKD

	bset	#3,r2			;3rd MS bit set for using hires (16 bits)
	bclr	#5,b1			
	bcs		SPEC_HIRES
	bclr	#3,r2
SPEC_HIRES

	move	y:Spec_Lower,y0  ;splice channel info with output
	jset	#3,r2,SPEC_NOSPLICE
	lsl		#12,b
	add		y0,b
	jmp		SPEC_SPLICED
SPEC_NOSPLICE
    move	b1,y1
	move	y1,y:Spec_HiResCh
	move	y0,b
SPEC_SPLICED
	move	b1,r4			  ;save

	move	y:Spec_Higher,y1  ;how many steps in the ramp?
	move	y1,b1
	sub		y0,b
	move	b1,y0
	clr		b
	move	y0,b0
	move	y:Spec_StepSize,y0
	jsr		DIVISION		  ;size (in bits) / bits per step = number of steps
	move	x1,r0

	move	y:Spec_Movedelay,y0	;added by SW
	DELAY	y0

	move	r4,a1
	jclr	#3,r2,SPEC_OUTONE
	move	y:Spec_HiResCh,y1
	jsr		HIGHRESBITS		  ;hires output initial (lower) value
	jmp		SPEC_HIONE
SPEC_OUTONE
	move	#>0,y1
	jsr		OUTPUTBITS		  ;output initial (lower) value
SPEC_HIONE

	jclr	#0,r2,SPEC_FWDUPDATE
	jsr		READSEQ			  ;readseq, if appropriate
	jmp		SPEC_FWDCONT
SPEC_FWDUPDATE
	move	#>CMDUPDATE,a1	
	jclr	#0,x:M_SSR,*	;bit set when transmit buffer empty
							;until then, jmp to self
	movep	a1,x:M_STXL		;send byte
SPEC_FWDCONT

    do		r0,SPEC_FWDRAMP	  ;forward ramp

	move	y:Spec_StepSize,y0
	do		y0,SPEC_FWDSTEP

	move	y:Spec_Movedelay,y0
	DELAY	y0

	move	r4,a1
	add		#>1,a			   ;increment...
	move	a1,r4
	jclr	#3,r2,SPEC_OUTTWO
	move	y:Spec_HiResCh,y1
	jsr		HIGHRESBITS		  ;...and hires output
	jmp		SPEC_HITWO
SPEC_OUTTWO
	move	#>0,y1
	jsr		OUTPUTBITS		  ;...or normal output
SPEC_HITWO
	nop
SPEC_FWDSTEP				   ;for one step

	jclr	#0,r2,SPEC_FWDOUT
	jsr		READSEQ			  ;readseq, if appropriate
	jmp		SPEC_FWDSKIP
SPEC_FWDOUT
	move	#>CMDUPDATE,a1	
	jclr	#0,x:M_SSR,*	;bit set when transmit buffer empty
							;until then, jmp to self
	movep	a1,x:M_STXL		;send byte
SPEC_FWDSKIP
    nop
SPEC_FWDRAMP

	move	y:Spec_Movedelay,y0	;added by SW
	DELAY	y0

	move	r4,a1
	jclr	#3,r2,SPEC_OUTTHR
	move	y:Spec_HiResCh,y1
	jsr		HIGHRESBITS		  ;hires output final (upper) value
	jmp		SPEC_HITHR
SPEC_OUTTHR
	move	#>0,y1
	jsr		OUTPUTBITS		  ;output final (upper) value
SPEC_HITHR

	jclr	#2,r2,SPEC_BKDUPDATE
	jsr		READSEQ			  ;readseq, if appropriate
	jmp		SPEC_BKDCONT
SPEC_BKDUPDATE
	move	#>CMDUPDATE,a1	
	jclr	#0,x:M_SSR,*	;bit set when transmit buffer empty
							;until then, jmp to self
	movep	a1,x:M_STXL		;send byte
SPEC_BKDCONT

    do		r0,SPEC_BKDRAMP	  ;backward ramp

	move	y:Spec_StepSize,y0
	do		y0,SPEC_BKDSTEP

	move	y:Spec_Movedelay,y0
	DELAY	y0

	move	r4,a1
	sub		#>1,a			   ;decrement...
	move	a1,r4
	jclr	#3,r2,SPEC_OUTFOR
	move	y:Spec_HiResCh,y1
	jsr		HIGHRESBITS		  ;...and hires output
	jmp		SPEC_HIFOR
SPEC_OUTFOR
	move	#>0,y1
	jsr		OUTPUTBITS		  ;...or normal output
SPEC_HIFOR
	nop

SPEC_BKDSTEP
	jclr	#2,r2,SPEC_BKDOUT
	jsr		READSEQ			  ;readseq, if appropriate
	jmp		SPEC_BKDSKIP
SPEC_BKDOUT
	move	#>CMDUPDATE,a1	
	jclr	#0,x:M_SSR,*	;bit set when transmit buffer empty
							;until then, jmp to self
	movep	a1,x:M_STXL		;send byte
SPEC_BKDSKIP
    nop
SPEC_BKDRAMP
	clr		a
	move	r6,y0
	cmpu	y0,a
	beq		SPEC_NOOUT
	jsr		XDSO			;out with the rest of the data

SPEC_NOOUT
	nop
	jmp		MAIN_LOOP

;*************************************************************
; READSEQ
; Additionally, this routine touches m0 and m1.
; So far, this routine is called by SCANLINE, SPEC, RAMPDIAG.
;*************************************************************

READSEQ

	;input, if we're reading during ramp
	jclr	#1,r2,READSEQ_DONE	; we're not reading at all

	move	y:Scan_Readdelay,y0
	;DELAY	y0
	;modified by SW
	do	y0,TenTimesDelay	; this allows maximum delay time of $FFFFFF*10*11.625744 ns = 1.95 s
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
TenTimesDelay

	move	y:ReadSeqNum,y0
	clr		a
	cmpu	y0,a
	beq		READSEQ_DONE
	move	#ReadSeq,r7
	do		y0,READSEQ_LOOP

	move	y:(r7)+,y0		;next sequence step

	;sort out which command
	bclr	#20,y0
	bcs		READSEQ_READ
	bclr	#21,y0
	bcs		READSEQ_OUTPUT
	bclr	#22,y0
	bcs		READSEQ_DITHER
	bclr	#23,y0
	bcs		READSEQ_FBACK

READSEQ_DELAY
    move    y0,y1
	move    y:(r7)+,y0
	do		y1,READSEQ_DELOOP
	DELAY	y0
	nop
READSEQ_DELOOP
    nop
	jmp		READSEQ_CONT

READSEQ_OUTPUT		;Step the output on one channel, not ramp bit by bit.
	move	y0,a
	move	#>READSEQOCH,y1
	and		y1,a
	;lsr		#16,a
	lsr	#12,a		; corrected by SW on 090907	

	move	a1,y1
	move	y0,a
	move	#>READSEQODATA,y0
	and		y0,a
	jsr		OUTPUTBITS
	jmp		READSEQ_CONT

READSEQ_FBACK
	move	y0,a			; assuming all non-feeback bits are now 0
	lsl		#2,a			; move feedback bit to proper location
	move	a1,y1
	move	r7,x0
	move	#>Out_Bits,b
	add		#>14,b
	move	b1,r7
	move	y:(r7),y0
	move	x0,r7
	move	#>FBACKMSK,a
	and		y0,a
	add		y1,a
	move	#0,y1			; no ch necessary, already pulled w/ Out_Bits
	jsr		OUTPUTBITS
	jmp		READSEQ_CONT

READSEQ_DITHER
	move	y0,a
	jclr	#23,y0,READSEQ_ZERO
READSEQ_ONE
	lsl		#9,a
	move	a1,y1
	move	#DITH1MSK,a
	jmp		READSEQ_NEXT
READSEQ_ZERO
	lsl		#11,a
	move	a1,y1
	move	#DITH0MSK,a
READSEQ_NEXT
	move	r7,x0
	move	#>Out_Bits,b
	add		#>13,b
	move	b1,r7
	move	y:(r7),y0
	move	x0,r7
	and		y0,a
	add		y1,a
	move	#0,y1			; no ch necessary, already pulled w/ Out_Bits
	jsr		OUTPUTBITS
	jmp		READSEQ_CONT

READSEQ_READ

	bclr	#21,y0
	bcs		READSEQ_SERIAL

	move	#>0,m0
	move	#>0,m1

	move	y0,a
	move	#>READSEQICH,y1
	and		y1,a				; get input ch
	lsr		#18,a
	move	a1,y1
	move	y1,y:ReadSeq_Ch

	move	y:(r7)+,y1			; read sequence multiplier
	move	y1,y:ReadSeq_Mult

	move	y0,a
	move	#>READSEQAVG,y1		; get number of inputs to average
	and		y1,a

	do		a1,READSEQ_AVERAGING

	move	y:ReadSeq_Ch,y0
	move	y0,a
	jsr		INPUTBITS
	clr		b
	move	a0,x0
	move	a1,y0
	move	x0,a
	lsl		#8,a
	add		y0,a      ; latest read in a1

	move	a1,x0
	move	y:ReadSeq_Mult,y0		;24 bit format where bit 23 is 2^5 and bit 0 is 2^(-18).
	mpyuu	x0,y0,b	  ; multiplier = 1 / averaging

	move	m1,a1
	move	m0,a0
	add		b,a
	move	a0,m0
	move	a1,m1	  ; accumulate
READSEQ_AVERAGING

	lsl		#9,a
	move	a1,y0
	move	a0,y1
	clr		a
	move	y1,a1
	lsr		#15,a
	add		y0,a			;expanded data format where bit 0 is 2^(-4) and bit 4 is 2^0.

	move	y:ReadSeq_Ch,y0
	move	#>ZCH,b
	cmpu	y0,b
	bne		POSTDELTA

	move	y:Scan_DeltaZ,y0
	jclr	#0,y:Scan_Flags,SUBDELTA
ADDDELTA
	add		y0,a
	jmp		POSTDELTA
SUBDELTA
	sub		y0,a
POSTDELTA
	move	a1,x1

	move	x1,x:(r6)+

	move	r6,a
	move	#>MAXDATA,x0
	cmpu	x0,a
	jlt		READSEQ_CONT
	move	r7,y0			;save
	jsr		XDSO			;memory full so send out data
	move	y0,r7			;restore
	move	#>0,r6
	nop
	jmp		READSEQ_CONT

READSEQ_SERIAL
	move	y0,a1	
	jclr	#0,x:M_SSR,*	;bit set when transmit buffer empty
							;until then, jmp to self
	movep	a1,x:M_STXL		;send byte

READSEQ_CONT
	nop
READSEQ_LOOP
	nop
READSEQ_DONE
	nop

	rts

;*************************************************************
; AUTO, AUTOSUB
; Perform auto minus or auto plus. More specifically, adjust z offset
; until feedback z is equal to a target value.
;	TOUCHES: a
;	USES: (r3) : to store current z off, pass to AUTOSUB
;*************************************************************
AUTO
	jsr		RECEIVETWO		;current z offset
	move	a1,r3

	jclr	#2,x:M_SSR,*
	movep	x:M_SRXL,a1		;receive z offset ch bits
	lsl		#4,a
	move	a1,y0
	move	y0,y:z_off_ch
	move	#>INZERO,y0
	move	y0,y:Auto_Target

	jsr		AUTOSUB

	; need to send out final value
	move	r3,a
	jclr	#0,x:M_SSR,*
	movep	a1,x:M_STXL		;send ls byte
	lsr		#8,a			;shift second next byte
	DELAY	#>100
	jclr	#0,x:M_SSR,*
	movep	a1,x:M_STXL		;send ms byte (only 4 bits really)

	jmp		MAIN_LOOP

;*************************************************************
; AUTOSUB
;	ENTRY: (r3) : current z off
;	TOUCHES: a, b, x0, x1, y0, y1
;	USES:	(r1): flags
;			(r3): current z off
;	FLAGS:	(#0) : set for auto plus, cleared for auto minus
;			(#1) : set for initial z > target, cleared for < target
;				can think of (#1) 1: Decrement Z off 0: Increment Z off
;			(#2) : set if we hit max or min
;	ENTRY:	y:Auto_Target should be set to the target. The target
;			must be a valid 16 bit input value!
;	EXIT:	(r3) : is final z offset value
;			flag set if we have ramped to MAX or MIN
;*************************************************************
AUTOSUB
	bclr	#2,r1		; init
	bset	#1,r1
	clr		b
	READIN  #>ZCH
;	move	#>INZERO,x0
	move	y:Auto_Target,y0
	move	y0,x0
	cmpu	x0,b
	jgt		CUEA
	bclr	#1,r1

CUEA
	do		forever,AUTOLOOP

	clr		b
	READIN	#>ZCH
;	move	#>INZERO,x0
	move	y:Auto_Target,y0
	move	y0,x0
	jset	#1,r1,AUTODOWN
AUTOUP
	cmpu	x0,b	;is feedback z greater than INZERO?
	nop
	brkgt			

	move	r3,b
	add		#>1,b	;increment z off

	move	#>OUTMAX,x0
	cmpu	x0,b
	jle		AUTOOUT	;don't go over max!
	bset	#2,r1
	enddo
	jmp		AUTOLOOP

AUTODOWN
	cmpu	x0,b	;is feedback z less than INZERO?
	nop
	brklt			

	move	r3,b
	sub		#>1,b	;increment z off
	jge		AUTOOUT ;watch out for negative values!
	bset	#2,r1
	enddo
	jmp		AUTOLOOP

AUTOOUT
	move	b1,a1
	move	y:z_off_ch,y1
	move	b1,r3		;save
	jsr		OUTPUTBITS

	DELAY	#>AUTOWAIT
	nop
	
AUTOLOOP

	jset	#2,r1,ENDAUTO		; no correction if we are max'ed or min'ed

; correct by one bit, if necessary
	move	r3,b
	move	#>1,x0
	jset	#0,r1,PLUSCORRECT
MINUSCORRECT
	jset	#1,r1,ENDAUTO
	sub		x0,b	;increment z off
	jmp		AUTOCORRECT
PLUSCORRECT
	jclr	#1,r1,ENDAUTO
	add		x0,b	;increment z off
AUTOCORRECT
	move	b1,a1
	move	y:z_off_ch,y1
	move	b1,r3		;save
	jsr		OUTPUTBITS

	DELAY	#>AUTOWAIT
	nop					;unnecessary?

ENDAUTO
	nop
	rts

;*************************************************************
; RAMPREAD,RAMP
; Ramp an output ch bit by bit. Optionally, read an input channel
; after each increment. Optionally, provide primitive crash protection.
; Serial output at conclusion of ramp varies depending on which options
; are in effect.
;	TOUCHES: a, b, x0, x1, y0, y1
;	USES: (r0) : number of bits between start point and endpoint, inclusive
;		  (r1) : checks flags of AUTOSUB
;		  (r2) : flags
;		  (r3) : via AUTOSUB
;		  (r4) : store current output value during subroutines
;		  (r5) : input ch, if there is one
;		  (r6) : pointer to currect x memory location for data storage
;		  (r7) : delay in cycles between each increment
;	FLAGS: (#0) : set for forward (increasing) direction
;		 (#1) : set for reading during ramp
;		 (#2) : set for protection parameter on
; 		 (#3) : set for 16 bit channel ramp
;		 (#4) : 0 for ch10 and 1 for ch 11 output
;	EXIT:
;*************************************************************
RAMPREAD
	move	#$000000,r2
	bset	#1,r2			;read during ramp
	jclr	#2,x:M_SSR,*
	movep	x:M_SRXL,a1		;receive input ch bits
	move	a1,r5			;save for later

							;do crash protection?

	move	#x_data,r6		;points to location for storing data
	jmp		ONWARD

;*************************************************************
; RAMP (see above)
;*************************************************************
RAMP
	move	#$000000,r2
	bclr	#1,r2			;don't read during ramp
ONWARD
	jclr	#2,x:M_SSR,*
	movep	x:M_SRXL,a1		;receive output ch bits

	bset	#2,r2			;MS bit is protection on/off
	bclr	#7,a1			;test and clear protection bit
	bcs		PROTECT_ON
	bclr	#2,r2
PROTECT_ON

	bset	#3,r2			;set the 16 bit mode on
    move	#>$000A,x1
	cmpu	x1,a			;find out which channel to write to
	beq		USE_CH10
    move	#>$000B,x1
	cmpu	x1,a			;find out which channel to write to
	beq		USE_CH11
	bclr	#3,r2			;turn the 16 bit mode off if the channel is not 10 or 11
							;a1 has the output channel
USE_CH11
	bset	#4,r2
USE_CH10

	move	a1,x1			;output channel is in x1
							
	jsr		RAMP_PREP

	jsr		RAMPLOOP		;calls the ramping subroutine
	
	jclr	#1,r2,RAMPNOTE
	jsr		XDSO			;out with the rest of the data

RAMPNOTE
	move	#>CMDRAMP,a1    ; notification of completion
	jclr	#0,x:M_SSR,*	;bit set when transmit buffer empty
							;until then, jmp to self
	movep	a1,x:M_STXL		;send byte

	;if crash protecting, send current z offset, final output
	jclr	#2,r2,RAMPLAST
	move	y:z_offset,y0
	move	y0,a1
	jsr		SERIAL_TWO		; z offset
	move	r4,a1
	move	#>DATAMSK,y0
	and		y0,a1			; send data only
	jsr		SERIAL_TWO		; final output

RAMPLAST
	jmp		MAIN_LOOP

;**************************************************************
;RAMP_PREP prepares the registers for RAMPLOOP
;**************************************************************	
RAMP_PREP	
		;put starting value into a & final value into b
	jsr		RECEIVETWO
	move	a1,y0
	jsr		RECEIVETWO
	move	a1,b1
	move	y0,a1
	
	jsr		RAMPDIR

	;receive delay in cycles
	jsr		RECEIVETHREE
	move	a1,r7

	rts

RAMPDIR
	;determine direction (put in bit 0 of r2)
	;put (final - start + 1) or (start - final + 1) in r0	
	move	a1,x0			; save start value (for increment/decrement)
	cmpu	a,b
	jlt		BACKDIR
FORDIR
	bset	#0,r2
	sub		a,b
	add		#>1,b			;inclusive
	move	b1,r0
	jmp		CONTINUE
BACKDIR
	bclr	#0,r2
	sub		b,a
	add		#>1,a			;inclusive
	move	a1,r0
CONTINUE

	
	;put start into b1 (for increment/decrement)
	;and splice in ch
	move	x0,b1			;put initial value in b1
	jset 	#3,r2,GO_16		;don't splice the channel
	move	x1,b1			;put the channel in b1
	lsl		#12,b
	add		x0,b			;splice the value
GO_16
	rts

;*****************************************************************************
;	RAMPLOOP ramps an output channel bit by bit
;	TOUCHES: a,b(dep),y0,y1
;	STARTS WITH	: b1(dep) the start value with the channel spliced in for 12 bit channel
;				  or b1 the 16 bit start value with the 16 bit channel determined by 
;				  the r2 #4 flag.
;	USES:	(r0) : number of bits between start point and endpoint, inclusive (dep)
;			(r1) : checks flags of AUTOSUB
;			(r2) : flags (dep)
;			(r3) : via AUTOSUB
;			(r4) : store current output value during subroutines
;			(r5) : input ch, if there is one
;			(r6) : pointer to currect x memory location for data storage
;			(r7) : delay (dep)
;	FLAGS:	(#0) : set for forward (increasing) direction (dep)
;			(#1) : set for reading during ramp (dep. set to 0)
;			(#2) : set for protection parameter on (dep)
; 			(#3) : set for 16 bit channel ramp (dep)
;			(#4) : 0 for ch10 and 1 for ch 11 output (dep. set to 0)
;	PARAMETERS:	y:Scan_Limit
;				y:Scan_Protection
;				y:z_offset
;	Note: (dep) means this field should be set if the subroutine is called
;		  from the deposition.
;*****************************************************************************
RAMPLOOP
	
	do	r0,TOTALRAMP
	
	move	b1,a1				; b1 is the start value with the channel spliced for 12 bits
								; or not spliced for 16 bits
	move	b1,r4				;save
	move	#>0,y1
	
	jset 	#3,r2,DO_HIGHRES	;channel 10 or 11
	
	jsr		OUTPUTBITS
	jmp	DO_LOWRES
DO_HIGHRES
	move	#>$000A,y1
	jclr	#4,r2,CH10
	move	#>$000B,y1
CH10	
	jsr		HIGHRESBITS
DO_LOWRES

	DELAY	r7

	; crash protection
	jclr	#2,r2,PROTECT_COMPLETE
	clr		b
	READIN	#>ZCH
	jsr		ABSIN
	move	#>INZERO,a
	move	y:Scan_Limit,y0
	sub		y0,a
	cmpu	a,b				;are we less than limit?
	jlt		PROTECT_COMPLETE
	jset	#1,y:Scan_Protection,AUTOPROTECT
	jclr	#0,y:Scan_Protection,PROTECT_COMPLETE	; do nothing option
	
STOP_PROTECT
	enddo
	jmp		TOTALRAMP

AUTOPROTECT		; execute 10 type y:Scan_Protection
	move	y:z_offset,r3
	move	#>INZERO,y0
	move	y0,y:Auto_Target
	bset	#0,r1			;auto plus
	jsr		AUTOSUB
	move	r3,y:z_offset
	jclr	#2,r1,PROTECT_COMPLETE
	enddo	;end it if auto minus railed z offset
	jmp		TOTALRAMP

PROTECT_COMPLETE

	;input, if we're reading during ramp
	jclr	#1,r2,INCORDEC
	move	r5,a1
	jsr		INPUTBITS

	;stuff data in x memory
	move	a0,x0
	move	a1,y0
	move	x0,a
	lsl		#8,a
	add		y0,a
	move	a1,x:(r6)+

	;if x memory is full, send out via SCI and reset
	move	r6,a
	move	#>MAXDATA,x0
	cmpu	x0,a
	jlt		INCORDEC
	jsr		XDSO			;memory full so send out data
	move	#>0,r6

INCORDEC
	move	r4,b1		;restore
	;increment / decrement
	jclr	#0,r2,DECREMENT
INCREMENT
	add		#>1,b
	jmp		INCDONE
DECREMENT
	sub		#>1,b
INCDONE
	nop

TOTALRAMP
	rts

;*******************************************************************************
; RAMPLOOP_2 is used by the diag ramp.  Unlike the RAMPLOOP the reading does 
; not have do be done at every point.  Uses same registers as RAMPLOOP
;*******************************************************************************
;RAMPLOOP_2
	
;	do	r0,TOTALRAMP_2
	
;	move	b1,a1				; b1 is the start value with the channel spliced for 12 bits
								; or not spliced for 16 bits
;	jset 	#3,r2,DO_HIGHRES_2	;channel 10 or 11
;	move	#>0,y1
;	move	b1,r4				;save
;	jsr		OUTPUTBITS
;	jmp	DO_LOWRES_2
;DO_HIGHRES_2
;	move	#>$000A,y1
;	jclr	#4,r2,CH10_2
;	move	#>$000B,y1
;CH10_2
;	jsr		HIGHRESBITS
;DO_LOWRES_2

;	DELAY	r7

	; crash protection
;	jclr	#2,r2,PROTECT_COMPLETE_2
;	clr		b
;	READIN	#>ZCH
;	jsr		ABSIN
;	move	#>INZERO,a
;	move	y:Scan_Limit,y0
;	sub		y0,a
;	cmpu	a,b				;are we less than limit?
;	jlt		PROTECT_COMPLETE_2
;	jset	#1,y:Scan_Protection,AUTOPROTECT_2
;	jclr	#0,y:Scan_Protection,PROTECT_COMPLETE_2	; do nothing option
;	
;STOP_PROTECT_2
;	enddo
;	jmp		TOTALRAMP_2
;
;AUTOPROTECT_2
;	move	y:z_offset,r3
;	bset	#0,r1			;auto plus
;	jsr		AUTOSUB
;	move	r3,y:z_offset
;	jclr	#2,r1,PROTECT_COMPLETE_2
;	enddo	;end it if auto minus railed z offset
;	jmp		TOTALRAMP_2
;
;PROTECT_COMPLETE_2

	;input, if we're reading during ramp
;	jclr	#1,r2,INCORDEC_2
	
;	jsr		RAMP_READ_AVER
	
;INCORDEC_2
;	move	r4,b1		;restore
;	;increment / decrement
;	jclr	#0,r2,DECREMENT_2
;INCREMENT_2
;	add		#>1,b
;	jmp		INCDONE_2
;DECREMENT_2
;	sub		#>1,b
;INCDONE_2
;	nop

;TOTALRAMP_2
;	rts

;RAMP_READ_AVER	;subroutine
	;this is the new part
	;to make this work every time just set y:Ramp_Read_Every = 0. 
;	move	y:Ramp_Count,y0
;	move	y0,a1
;	add		#>1,a				;update the counter
;	move	a1,y1
;	move	y1,y:Ramp_Count		
;	move	y:Ramp_Read_Every,y0
;	cmp		y0,a
;	blt		END_RAMP_READ_AVER
;	move	y:Ramp_Read_Delay,y0
;	DELAY	y0
;	move	#>0,y0
;	move	y0,y:Ramp_Count	;set the counter to zero

;	move	y:Ramp_Average_Every,y0
;	do		y0,RAMP_END_AVER

;	move	r5,a1
;	jsr		INPUTBITS
;	move	a0,x0
;	move	a1,y0
;	move	x0,a
;	lsl		#8,a
;	add		y0,a				;the input value is in a1 now
;
;	move	y:Ramp_Average,y0
;	add		y0,a
;	move	a1,y0
;	move	y0,y:Ramp_Average	;add the previous average
;RAMP_END_AVER
;	move	a1,x:(r6)+			;stuff data in x memory
;
;	;if x memory is full, send out via SCI and reset
;	move	r6,a
;	move	#>MAXDATA,x0
;	cmpu	x0,a
;	jlt		INCORDEC
;	jsr		XDSO			;memory full so send out data
;	move	#>0,r6

;END_RAMP_READ_AVER

;	rts

;*************************************************************
; RAMPDIAGSETUP setup parameters for RAMPDIAG.
; TOUCHES: a, y0, x0
;*************************************************************
RAMPDIAGSETUP

	jsr		RECEIVETWO		; receive long ch start	(2 bytes)
	move	a1,y0
	move	y0,y:Diag_Long_Start

	jsr		RECEIVETWO		; receive long ch end	(2 bytes)
	move	a1,y0
	move	y0,y:Diag_Long_Final

	jsr		RECEIVETWO		; receive short ch start(2 bytes)
	move	a1,y0
	move	y0,y:Diag_Short_Start

	jsr		RECEIVETWO		; receive z start(2 bytes)
	move	a1,y0
	move	y0,y:Diag_Z_Start

	jclr	#2,x:M_SSR,*	;# of bits/step (read every N bits)
	movep	x:M_SRXL,y0		
	move	y0,y:Diag_Step_Size

	jsr		RECEIVETHREE	; receive ratio			(3 bytes)
	move	a1,y0
	move	y0,y:Diag_Ratio

	jsr		RECEIVETHREE	; receive Z off ratio	(3 bytes)
	move	a1,y0
	move	y0,y:Diag_Z_Ratio

	jsr		RECEIVETHREE	; receive move delay	(3 bytes)
	move	a1,y0
	move	y0,y:Diag_Move_Delay

	jsr		RECEIVETHREE
	move	a1,y0
	move	y0,y:Scan_Readdelay ;read delay - may be overwritten by SCANSETUP!,SPECSETUP!

	jmp		MAIN_LOOP

;*************************************************************
; RAMPDIAG
; Ramps two channels simultaneously, i.e. a straight trajectory
; in a 2d space with a fixed slope (Diag_Ratio). Optionally read
; during the ramp. Optionally, run in constant height mode, i.e.
; a straight trajectory in a 3d space with fixed slopes.
; ENTRY: expects parameters set up by RAMPDIAGSETUP to be valid
; TOUCHES: a, b, y0, y1, x0, x1
; USES: (r0) long ch data
;		(r1) short ch data
;		(r2) flags
;		(r6) used for current xdata address
;		(r7) used by READSEQ
; FLAGS:(#0) set for increasing long ch
;		(#1) set for reading during ramp
;		(#2) set for increasing short ch
;		(#3) set if we're done
;		(#4) set if we're in constant height mode
;		(#5) set for increasing z off in const height mode
; EXIT:
;*************************************************************
RAMPDIAG
	move	#>0,r2			; zero all the flags
	move	#x_data,r6		; points to location for storing data
	move	#>0,y1
	move	y1,y:Scan_DeltaZ
	move	y:Diag_Z_Start,y0
	move	y0,y:z_offset

	jclr	#2,x:M_SSR,*	;short ch
	movep	x:M_SRXL,a

	bset	#4,r2			;MS bit is to do const z manip
	bclr	#7,a1			;test and clear bit
	bcs		DIAG_CONST_Z
	bclr	#4,r2
DIAG_CONST_Z

	bset	#5,r2			;2nd MS bit is for inc/dec z off
	bclr	#6,a1			;test and clear bit
	bcs		DIAG_Z_UP
	bclr	#5,r2
DIAG_Z_UP

	lsl		#12,a
	move	y:Diag_Short_Start,y0
	add		y0,a			;splice in short ch
	move	a1,r1

	jclr	#2,x:M_SSR,*	;long ch
	movep	x:M_SRXL,a

	bset	#1,r2			;MS bit is read on/off
	bclr	#7,a1			;test and clear bit
	bcs		DIAG_READ_ON
	bclr	#1,r2
DIAG_READ_ON

	bset	#0,r2			;2nd MS bit is increment up/down long ch
	bclr	#6,a1			;test and clear bit
	bcs		DIAG_LONG_UP
	bclr	#0,r2
DIAG_LONG_UP

	bset	#2,r2			;3rd MS bit is increment up/down short ch
	bclr	#5,a1			;test and clear bit
	bcs		DIAG_SHORT_UP
	bclr	#2,r2
DIAG_SHORT_UP

	lsl		#12,a
	move	y:Diag_Long_Start,y0
	add		y0,a
	move	a1,r0			;splice in long ch

	; output initial short ch value
	move	r1,a
	move	#>0,y1
	jsr		OUTPUTBITS

	do		forever,DIAG_END_STEPS

	move	y:Diag_Step_Size,y0
	do		y0,DIAG_ONESTEP

	move	y:Diag_Move_Delay,y0
	DELAY	y0

	;output current long value
	move	r0,a1
	move	#>0,y1
	jsr		OUTPUTBITS

	;check if we should increment short ch
	move	r1,a
	move	#>DATAMSK,y0
	and		y0,a
	move	y:Diag_Short_Start,b
	jclr	#2,r2,DIAG_SHORT_LESS
DIAG_SHORT_MORE
	sub		b,a
	add		#1,a
	move	a1,x1
	jmp		DIAG_SHORT_DONE
DIAG_SHORT_LESS
	sub		a,b
	add		#1,b
	move	b1,x1
DIAG_SHORT_DONE
	move	y:Diag_Ratio,y1		;24 bit format is "bit 23 is 2^11, bit 0 is 2^(-12)"
	clr		a
	mpyuu	x1,y1,a
	move	a0,y1
	lsl		#11,a
	move	a1,y0
	clr		a
	move	y1,a
	lsr		#13,a
	add		y0,a
	move	a1,x0		;"bump"

	move	r0,a
	move	#>DATAMSK,y0
	and		y0,a
	move	y:Diag_Long_Start,b
	jclr	#0,r2,DIAG_LONG_LESS
DIAG_LONG_MORE
	sub		b,a
	move	a1,b
	jmp		DIAG_LONG_DONE
DIAG_LONG_LESS
	sub		a,b
DIAG_LONG_DONE
	move	x0,a
	cmpu	a,b
	jne		DIAG_GO_ON
	move	r1,a
	jclr	#2,r2,DIAG_DEC_SHORT
DIAG_INC_SHORT	
	add		#>1,a
	jmp		DIAG_INCS_DONE
DIAG_DEC_SHORT
	sub		#>1,a			
DIAG_INCS_DONE
	move	a1,r1
	move	#>0,y1
	jsr		OUTPUTBITS
DIAG_GO_ON

	jclr	#4,r2,DIAG_SKIP_Z

	;check if we should increment z off
	move	y:z_offset,y0
	move	y0,a
	move	y:Diag_Z_Start,b
	jclr	#5,r2,DIAG_Z_LESS
DIAG_Z_MORE
	sub		b,a
	add		#1,a
	move	a1,x1
	jmp		DIAG_Z_DONE
DIAG_Z_LESS
	sub		a,b
	add		#1,b
	move	b1,x1
DIAG_Z_DONE
	move	y:Diag_Z_Ratio,y1	;24 bit format is "bit 23 is 2^11, bit 0 is 2^(-12)"
	clr		a
	mpyuu	x1,y1,a
	move	a0,y1
	lsl		#11,a
	move	a1,y0
	clr		a
	move	y1,a
	lsr		#13,a
	add		y0,a
	move	a1,x0		;"bump"

	move	r0,a
	move	#>DATAMSK,y0
	and		y0,a
	move	y:Diag_Long_Start,b
	jclr	#0,r2,DIAG_ZLONG_LESS
DIAG_ZLONG_MORE
	sub		b,a
	move	a1,b
	jmp		DIAG_ZLONG_DONE
DIAG_ZLONG_LESS
	sub		a,b
DIAG_ZLONG_DONE
	move	x0,a
	cmpu	a,b
	jne		DIAG_SKIP_Z
	move	y:z_offset,y1
	move	y1,a
	jclr	#5,r2,DIAG_DEC_Z
DIAG_INC_Z	
	add		#>1,a
	jmp		DIAG_INCZ_DONE
DIAG_DEC_Z
	sub		#>1,a			
DIAG_INCZ_DONE
	move	a1,y1
	move	y1,y:z_offset
	move	#>ZOFFCH,y1
	jsr		OUTPUTBITS
DIAG_SKIP_Z
	nop

	;compare current long value to final long value and end, if necessary
	move	r0,a
	move	#>DATAMSK,y0
	and		y0,a
	move	y:Diag_Long_Final,y0
	cmpu	y0,a
	jne		DIAG_CONT
	bset	#3,r2
	enddo
	jmp		DIAG_GETOUT
DIAG_CONT

	;increment long value
	move	r0,a
	jclr	#0,r2,DIAG_DEC_LONG
DIAG_INC_LONG
	add		#>1,a
	jmp		DIAG_INCL_DONE
DIAG_DEC_LONG
	sub		#>1,a
DIAG_INCL_DONE
	move	a1,r0

DIAG_GETOUT
	nop
DIAG_ONESTEP

	;check if we're done
	jclr	#3,r2,DIAG_KEEP_GOING
	enddo
	jmp		DIAG_ESCAPE	

DIAG_KEEP_GOING			;read, if appropriate
	jclr	#1,r2,DIAG_ESCAPE
	jsr		READSEQ
	nop
DIAG_ESCAPE
	nop
DIAG_END_STEPS
	;output data
	clr		a
	move	r6,y0
	cmpu	y0,a
	beq		DIAG_NOOUT
	jsr		XDSO			;out with the rest of the data
DIAG_NOOUT

	move	#>CMDDIAG,a1    ; notification of completion
	jclr	#0,x:M_SSR,*	;bit set when transmit buffer empty
							;until then, jmp to self
	movep	a1,x:M_STXL		;send byte

	;output final coordinates
	jclr	#4,r2,DIAG_JUSTXY
	move	y:z_offset,y0
	move	y0,a
	jsr		SERIAL_TWO		;final z offset
DIAG_JUSTXY
	move	r0,a
	and		#>DATAMSK,a
	jsr		SERIAL_TWO		;final long output
	move	r1,a
	and		#>DATAMSK,a
	jsr		SERIAL_TWO		;final short output

	jmp		MAIN_LOOP


;*************************************************************
; DEPSETUP
; Receive & store DEPOSITION parameters
;	TOUCHES: a, x0(via RECEIVE), y0
;	EXIT:    (y:*) parameter values
;*************************************************************
DEPSETUP
	
	jsr		RECEIVETWO
	move	a1,y0
	move	y0,y:Dep_BiasM		;currently not implemented

	jsr		RECEIVETWO
	move	a1,y0
	move	y0,y:Dep_BiasW		;writing bias

	jsr		RECEIVETWO
	move	a1,y0
	move	y0,y:Dep_IM		;currently not implemented

	jsr		RECEIVETWO
	move	a1,y0
	move	y0,y:Dep_IW		;writing current

	jsr		RECEIVETWO
	move	a1,y0
	move	y0,y:Dep_Z_Offset	;Z Offset for deposition

	jsr		RECEIVETHREE	
	move	a1,y0
	move	y0,y:Dep_Wait		;delay in cycles between each increment during ramp, don't put 0!

	jsr		RECEIVETWO
	move	a1,y0
	move	y0,y:Dep_Num_Pts	;total number of data points to store

	jsr		RECEIVETHREE
	move	a1,y0
	move	y0,y:Dep_Delay		;delay in cycles between stored data points, don't put 0!
	
	jsr		RECEIVETWO
	move	a1,y0
	move	y0,y:Dep_Aver_Pts	;currently not implemented

	jsr		RECEIVETWO	
	move	a1,y0
	move	y0,y:Dep_Ignore_Init	;currently not implemented

	jsr		RECEIVETWO	
	move	a1,y0
	move	y0,y:Dep_Chng_T		;currently not implemented

	jsr		RECEIVETWO	
	move	a1,y0
	move	y0,y:Dep_After_T	;currently not implemented

	jsr		RECEIVETWO	
	move	a1,y0
	move	y0,y:Dep_Chng_Ratio	;currently not implemented

	jsr		RECEIVETWO	
	move	a1,y0
	move	y0,y:Dep_Aver_Every	;average number per stored data points? (recommend not to exceed 64) 

	jsr		RECEIVETHREE	
	move	a1,y0
	move	y0,y:Dep_Aver_Inv	;1/(average number per stored data points)
					;24 bit format where bit 0 is 2^(-22) and bit 23 is 2^1.

	jclr	#2,x:M_SSR,*
	movep	x:M_SRXL,a1
	move	a1,y0
	move	y0,y:Dep_Flags		;refer to DEPOSITION notes
				
	jmp		MAIN_LOOP

;*************************************************************
; DEPOSITION
;	TOUCHES: a,b, x0,y0,x1,y1
;	USES:	(n0): initial bias value
;		(n1): initial current value
;		(n2): bias value to ramp to and current sum for the pulse stopping condition
;		(n3): current value to ramp to and previous sum for stopping the pulse
;		(n4): current memory address
;		(n5): input channel
;		(n6): counter
;		(n7): flags
;		(r7): counter
;	FLAGS:	(#0): set if read
;		(#1): input channel, set for I and clear for Z
;		(#2): set if initial feedback on
;		(#3): set for step bias/current, clear for ramp bias/current
;		(#4): set for fixed length pulse
;		(#5): set if using hires bias
;		(#6): use move yes/no (not implemented)
;		(#7): set if we are looking for an increase in the value to stop the pulse (not implemented)
;	EXIT:   
;*************************************************************
DEPOSITION
	
	move	y:Dep_Flags,y0
	move	y0,n7			;all the flags are now stored in n7

	;Step 1: Take care of the feedback
	;save the current feedback condition in bit #2 of n7
	;and respond to the feedback request in y:Dep_Flags

	move	y:(Out_Bits+14),y1
	bclr	#2,n7			;the following lines save the previous settings
	jclr	#2,y1,DEP_FB_OK1
	bset	#2,n7
DEP_FB_OK1

	move	y1,a
	and		#>DATAMSK,a
	bclr	#2,a1
	jclr	#2,y0,DEP_FB_OK2
	bset	#2,a1
DEP_FB_OK2
	move	#>FBACKCH,y1		;the channel info
	jsr		OUTPUTBITS
	
	;Step 2: Ramp/Step bias and current to their moving values
	;not currently implemented
	
DEP_STEP3
	;Step 3: Delta Z-offset

	move	y:Dep_Z_Offset,y1

	move	y:(Out_Bits+1),y0
	move	y0,a1
	and		#>DATAMSK,a
	move	a1,y:Dep_Z_Offset
	
	move	y1,a1
	and		#>DATAMSK,a
	move	#>ZOFFCH,y1
	jsr		OUTPUTBITS

	;Step 4: Ramp/Step bias and current to their writing values

	move	y:Dep_BiasW,y1
	move	y1,n2
	move	y:Dep_IW,y1
	move	y1,n3
	
	jsr		DEP_BIAS_AND_I

	move	y:Dep_Wait,y1
	DELAY	y1

;*****
	;Step 5: Read or wait
	
	jset	#0,n7,DEP_READ
	
	;if the bit is clear don't read during pulse
	move	y:Dep_Num_Pts,y0
	move	y:Dep_Aver_Every,y1
	
	;the following do loop takes about (lower bound) Dep_Num_Pts*[Dep_Delay+Dep_Aver_Every*(2228)+22] clock cycles.
	;After calibration using a function generator, Time=Dep_Num_Pts*[Dep_Delay+Dep_Aver_Every*(2276)+22] clock cycles.

	do	y0,DEP_END_PULSE1
	
	move	#>2276,x1
	clr	a
	mpyuu	x1,y1,a
	move	a0,x1
	move	a1,y1
	clr	a
	move	y1,a1
	lsl	#23,a
	move	a1,y1
	move	x1,a1
	lsr	#1,a
	add	y1,a
	move	#>11,x1
	sub	x1,a
	move	a1,y1
	rep	y1
	nop			
	
	move	y:Dep_Delay,y1
	rep	y1
	nop
DEP_END_PULSE1


	;DSP doesn't execute that way to delay proper time. SW
	;do		y0,DEP_END_PULSE1	;those two loops are just to take up time
	;do		y1,DEP_END_AVER1
	;DELAY	#>ADCTIME			
;DEP_END_AVER1
;	nop
;DEP_END_PULSE1

	jmp		DEP_STEP6

DEP_READ

	move	#>0,n5			;n5 has the input channel set to read I
	jset	#1,n7,DEP_IN_SET
	move	#>$0001,n5				;read Z
DEP_IN_SET
					
	move	#EXT_BEGIN,n4		;start of the extended memory
	
	;move	y:Dep_Ignore_Init,y0	;read Num_Pts if pulse is fixed
	;jset	#4,n7,DEP_WAIT_CHANGE	;and Ignore_Init in the start of the pulse 
	
	move	y:Dep_Num_Pts,y0

;DEP_WAIT_CHANGE
	
	;the following do loop takes about (lower bound) Dep_Num_Pts*[Dep_Delay+Dep_Aver_Every*(2228)+22] clock cycles.
	;After calibration using a function generator, Time=Dep_Num_Pts*[Dep_Delay+Dep_Aver_Every*(2276)+22] clock cycles.

	do	y0,DEP_END_PULSE2
	
	move	#>0,y0	
	move	y0,y:Dep_Temp			;zero the average, for MSBs
	move	y0,y:Dep_Temp2			;zero the average, for LSBs

	move	y:Dep_Aver_Every,y1

	do	y1,DEP_END_AVER2			
	move	n5,a1				;set up for inputval
	jsr		INPUTVAL		
	move	y:Dep_Aver_Inv,y1
	move	a1,x1
	
	mpyuu	x1,y1,a

	move	y:Dep_Temp,b1
	move	y:Dep_Temp2,b0
	add	a,b
	move	b0,y:Dep_Temp2
	move	b1,y:Dep_Temp

DEP_END_AVER2

	move	b,a
	move	a0,y1
	lsl		#1,a
	move	a1,y0
	clr		a
	move	y1,a
	lsr		#23,a
	add		y0,a
	move	a1,y1

	move	y1,x0				
	move	y0,y:Dep_Temp
	move	r0,y0
	move	n4,r0
	move	x0,x:(r0)+			;update the extended memory address
	move	r0,n4
	move	y0,r0
	move	y:Dep_Temp,y0

	move	y:Dep_Delay,y0
	rep	y0
	nop

DEP_END_PULSE2

;*********
;I haven't checked the following code for changeable length pulse. SW
;
;	jclr	#4,n7,DEP_STEP6
;
;	move	y1,n3				;previous input for the check
;	move	#>0,n6				;counter
;
;	move	y:Dep_Num_Pts,y0
;		
;	do		y0,DEP_END_PULSE3	
;	move	#>0,y0				;zero the average
;	move	y0,y:Dep_Temp
;	move	y:Dep_Aver_Every,y1
;	do		y1,DEP_END_AVER3	
;	move	n5,a1				;set up for inputval
;	jsr		INPUTVAL
;	move	y:Dep_Temp,y1
;	add		y1,a				;note that the output is a sum not average
;	move	a1,y1
;	move	y1,y:Dep_Temp
;DEP_END_AVER3
;	
;	move	y1,n2				;current input for the check
;	
;	;check for the change
;	move	y:Dep_Chng_Ratio,y0
;	move	n3,x0				;n3 is the previous value for check
;	mpyuu	x0,y0,a	
;	move	#>$20,y0
;	move	n2,x0				
;	mpyuu	x0,y0,b
;	jset	#7,n7,DEP_INCR		;looking for an increase in the value
;	cmpu	a,b
;	blt		DEP_HAS_CHANGED
;	move	n2,n3
;	move	#>0,n6
;	jmp		DEP_NO_CHANGE
;DEP_INCR
;	cmpu	a,b
;	bgt		DEP_HAS_CHANGED
;	move	n2,n3
;	move	#>0,n6
;	jmp		DEP_NO_CHANGE
;DEP_HAS_CHANGED
;	move	n6,a
;	add		#>1,a
;	move	a1,n6				;update the counter
;	move	y:Dep_Chng_T,y0
;	cmpu	y0,a
;	bne		DEP_NO_CHANGE
;	enddo						;exit this loop
;DEP_NO_CHANGE
;	move	y:Dep_Temp,y1		;the sum is in the output
;	move	y1,x0
;	move	y0,y:Dep_Temp
;	move	r0,y0
;	move	n4,r0
;	move	x0,x:(r0)+			;update the extended memory address
;	move	r0,n4
;	move	y0,r0
;	move	y:Dep_Temp,y0
;DEP_END_PULSE3
;
;	move	y:Dep_After_T,y0
;	
;	do		y0,DEP_END_PULSE4	
;	move	#>0,y0				;zero the average
;	move	y0,y:Dep_Temp
;	move	y:Dep_Aver_Every,y1
;	do		y1,DEP_END_AVER4	
;	move	n5,a1				;set up for inputval
;	jsr		INPUTVAL
;	move	y:Dep_Temp,y1
;	add		y1,a				;note that the output is a sum not average
;	move	a1,y1
;	move	y1,y:Dep_Temp
;DEP_END_AVER4
;	move	y1,x0
;	move	y0,y:Dep_Temp
;	move	r0,y0
;	move	n4,r0
;	move	x0,x:(r0)+			;update the extended memory address
;	move	r0,n4
;	move	y0,r0
;	move	y:Dep_Temp,y0
;DEP_END_PULSE4
;	move	n4,a
;	sub		#>EXT_BEGIN,a
;	move	a1,y0
;	move	y0,y:Dep_Num_Pts	;update the number of points to the actual number read
;
;**************

DEP_STEP6
	;Step 6: Ramp/Step bias and current to their initial values
	move	n0,n2
	move	n1,n3
	jsr		DEP_BIAS_AND_I

DEP_STEP7
	;Step 7: Undo delta Z-offset
	move	y:Dep_Z_Offset,y1

	move	y:(Out_Bits+1),y0
	move	y0,a1
	and		#>DATAMSK,a
	move	a1,y:Dep_Z_Offset
	
	move	y1,a1
	and		#>DATAMSK,a
	move	#>ZOFFCH,y1
	jsr		OUTPUTBITS
	
	;Step 8: Ramp/Step bias and current to their moving values
	;not currently implemented

DEP_STEP9
	;Step 9: Change the feedback back
	move	y:(Out_Bits+14),y1
	move	y1,a
	and		#>DATAMSK,a
	bclr	#2,a1
	jclr	#2,n7,DEP_FB_OK3
	bset	#2,a1
DEP_FB_OK3
	move	#>FBACKCH,y1			;channel 14
	jsr		OUTPUTBITS
	
	jclr	#0,n7,MAIN_LOOP			;do not output if there was no reading		

	;Step 10: Output the read data
	
	move	y:Dep_Num_Pts,y0
	move	y0,a1
	jsr		SERIAL_TWO		;output the number of points to be sent to the serial
	move	#EXT_BEGIN,r0
	do		y0,ENDDEPOUT		;output the x memory to the PC
	move	x:(r0)+,x0	
	move	x0,a1	
	jsr		SERIAL_TWO
	DELAY	#>100
ENDDEPOUT

	jmp		MAIN_LOOP

;*********************************************************************************	
;DEP_BIAS_AND_I 	
;	ramps/step the bias and the current
;	inputs n2 - final bias value, n3 - final current value
;	saves the original values in n0 and n1
;*********************************************************************************	
DEP_BIAS_AND_I
	jset	#3,n7,DEP_DONT_RAMP1

	;ramp bias
	jset	#5,n7,DEP_16BIT
	move	y:Out_Bits,y1
	move	y1,a			;initial value
	move	a1,n0			;save the initial bias value for later
	move	n2,b			;final value
	move	#>$0000,x1		;bias is on channel 0
	move	#>$0000,r2		;set all the flags to 0
	
	jsr		RAMPDIR		;the direction bit of r2, r0, and b1 are now set
	move	y:Dep_Wait,y0
	move	y0,r7			;set up the delay
	jsr		RAMPLOOP

	jmp		DEP_RAMP_I

DEP_16BIT				;(I didn't check. SW)
	move	y:(Out_Bits+15),y1
	move	y1,b
	and		#>$0F00,b
	lsl		#4,b
	move	y:(Out_Bits+10),y1
	move	y1,a
	and		#>DATAMSK,a
	add		b,a			;initial bias value is in a1
	move	a1,n0
	move	n2,b			;move bias value is in b1
	move	#>$0000,r2
	bset	#3,r2			;set for 16 bit bias
	move	#>$000A,x1

	jsr		RAMPDIR		;the direction bit of r2, r0, and b1 are now set
	move	y:Dep_Wait,y0
	move	y0,r7			;set up the delay
	jsr		RAMPLOOP
	
DEP_RAMP_I
	;ramping the current
	move	y:(Out_Bits+8),y1	
	move	y1,a			;initial value
	and	#>DATAMSK,a
	move	a1,n1			;save the initial current value for later	
	move	n3,b			;final value
	move	#>$0008,x1		;current is on channel 8
	move	#>$0000,r2		;set all the flags to 0

	jsr		RAMPDIR		;the direction bit of r2, r0, and b1 are now set
	move	y:Dep_Wait,y0
	move	y0,r7			;set up the delay
	jsr		RAMPLOOP

	jmp		DEP_RAMP_DONE

DEP_DONT_RAMP1				;step bias and current
	jset	#5,n7,DEP_16BIT2
	move	y:Out_Bits,y1
	move	y1,n0			;save the initial bias value for later
	move	n2,a1			;move bias value
	move	#>$0000,y1
	jsr		OUTPUTBITS
	jmp		DEP_STEP_I

DEP_16BIT2				;(I didn't check. SW)
	move	y:(Out_Bits+15),y1
	move	y1,b
	and	#>$0F00,b
	lsl	#4,b
	move	y:(Out_Bits+10),y1
	move	y1,a			;initial bias value is in a1
	and	#>DATAMSK,a
	add	b,a
	move	a1,n0
	move	n2,a			;move bias value
	move	#>$000A,y1		;channel (check the format)
	jsr		HIGHRESBITS

DEP_STEP_I
	move	y:(Out_Bits+8),y1
	move	y1,a1			;initial value
	and	#>DATAMSK,a
	move	a1,n1			;save the initial current value for later	
	move	n3,a1			;move bias value
	and	#>DATAMSK,a
	move	#>$0080,y1		;Iset channel
	jsr		OUTPUTBITS
	
DEP_RAMP_DONE
	nop
	rts

;*************************************************************
; TRACKSETUP
; Receive & store TRACK parameters
;	TOUCHES: a, x0(via RECEIVE), y0
;	EXIT:    (y:*) parameter values
;*************************************************************
TRACKSETUP

	jclr	#2,x:M_SSR,*	;receive average 256 max
	movep	x:M_SRXL,y0		
	move	y0,y:Track_Average

	jsr		RECEIVETHREE	;receive sample delay
	move	a1,y0
	move	y0,y:Track_Sample_Every

	jsr		RECEIVETHREE	;receive delay
	move	a1,y0
	move	y0,y:Track_Delay

	jsr		RECEIVETHREE	;receive a, 24 bit format where bit 23 is 2^5 and bit 0 is 2^(-18).
	move	a1,y0
	move	y0,y:Track_Plane_a

	jsr		RECEIVETHREE	;receive b, 24 bit format where bit 23 is 2^5 and bit 0 is 2^(-18).
	move	a1,y0
	move	y0,y:Track_Plane_b

	jmp		MAIN_LOOP

;*************************************************************
; TRACK
; Measures Z at nine points (the current tip position + 8 neghboors)
; then moves the tip to the highest (lowest) position.  Repeats 
; everything TRACKNU times then sends the new values for x and y to
; the computer.  If the computer responds with 'c' does another set of 
; iterrations.
;   TOUCHES: a
;	USES: (r0): best x coordinate
;		  (r1): best y coordinate
;		  (r2): current x
;		  (r3): current y
;		  (r4): best Z
;		  (r5): current Z
;		  (r7): flags
;	FLAGS:(#0) : set for min
;	EXIT: After receiving 'c', trackes again. If receiving other than 'c',
;		such as 's', goes back to the main loop. The tip 
;		is left in the final position.
;*************************************************************
TRACK

	move	#$000000,r7	;all flags set to 0
	bset	#0,r7		;set the min/max flag to 1
	move	#$FFFFFF,r4	;best Z is set to max (this is the "worst" Z for minimum)
	jsr		RECEIVETWO	;receive initial x (MSB(bit #15) is set if tracking min)
	move	a1,r2
	jsr		RECEIVETWO	;receive initial y
	move	a1,r3
	bclr	#15,r2		;check for the min

	;bcc		MIN
	bcs		MIN	;corrected by SW

	bclr	#0,r7
	move	#$000000,r4	;set best Z to 0 if looking for max
MIN
	move	r2,r0		;set x_best=x_initial
	move	r3,r1		;set y_best=y_initail
	jmp		START_TRACK
TRACK_AGAIN
	
	move	#$FFFFFF,r4
	btst	#0,r7
	
	;bcc		START_TRACK
	bcs		START_TRACK	;corrected by SW
	
	move	#$000000,r4
START_TRACK	
	; going around the square TRACKNU times
	do		#>TRACKNU,ENDTRACK
	jsr		MOVE_READ_COMP

	move	r2,b
	jsr     INCR_ANY
	move	b1,r2
	jsr		MOVE_READ_COMP

	move	r3,b
	jsr		INCR_ANY
	move	b1,r3
	jsr		MOVE_READ_COMP

	move	r2,b
	jsr		DECR_ANY
	move	b1,r2
	jsr		MOVE_READ_COMP

	move	r2,b
	jsr		DECR_ANY
	move	b1,r2
	jsr		MOVE_READ_COMP

	move	r3,b
	jsr		DECR_ANY
	move	b1,r3
	jsr		MOVE_READ_COMP

	move	r3,b
	jsr		DECR_ANY
	move	b1,r3
	jsr		MOVE_READ_COMP

	move	r2,b
	jsr     INCR_ANY
	move	b1,r2
	jsr		MOVE_READ_COMP

	move	r2,b
	jsr     INCR_ANY
	move	b1,r2
	jsr		MOVE_READ_COMP

	move	r3,b
	jsr		INCR_ANY
	move	b1,r3
	jsr		MOVE_READ_COMP

	move	r2,b
	jsr		DECR_ANY
	move	b1,r2
	jsr		MOVE_READ_COMP

	move	r0,r2
	move	r1,r3
	move	#>XCH,y1	
	move	r2,a1
	jsr		OUTPUTBITS
	
	move	y:Track_Delay,y0	;added by SW
	DELAY	y0			
	
	move	#>YCH,y1
	move	r3,a1
	jsr		OUTPUTBITS
	move	y:Track_Sample_Every,y0
	DELAY	y0	
ENDTRACK
	move	r0,a1
	jsr		SERIAL_TWO	;send x_best to computer
	move	r1,a1
	jsr		SERIAL_TWO	;send y_best to computer
	jclr	#2,x:M_SSR,* 		;get the answer back  (??? should this be in a loop)
	movep	x:M_SRXL,a
	
	move	#>CMDTRACKCONT,x0
	cmpu	x0,a
	beq		TRACK_AGAIN

	jmp		MAIN_LOOP

MOVE_READ_COMP				
	;moves the tip to the position specified by x=r2, y=r3
	move	#>XCH,y1	
	move	r2,a1
	jsr		OUTPUTBITS	;move tip to x initial
	move	y:Track_Delay,y0
	DELAY	y0			
	move	#>YCH,y1
	move	r3,a1
	jsr		OUTPUTBITS	;move tip to y initial
	move	y:Track_Delay,y0
	DELAY	y0	

	;reads the Z and updates the x_best, y_best, and z_best

	move	y:Track_Average,y0	;get number or reads
	move	#$000000,r5			;current is set to 0
	
	do		y0,ENDREAD
	move	#>ZCH,a1
	jsr		INPUTVAL			;read the Z
	move	r5,b
	add		a,b			;sums the current from several readings
	move	b1,r5
ENDREAD
	
	;now, r5 stores Track_Average*Current Z
	;plane fit
	;z(r5) =z-(Track_Plane_a*current_x(r2) + Track_Plane_b*current_y(r3))
	;rewritten by SW
	
	move	r2,x1
	move	y:Track_Plane_a,y1		;24 bit format is "bit 23 is 2^5, bit 0 is 2^(-18)"
	clr		a
	mpyuu	x1,y1,a
	move	a0,y1
	lsl		#5,a
	move	a1,y0
	clr		a
	move	y1,a
	lsr		#19,a
	add		y0,a
	
	move	a1,x1
	move	y:Track_Average,y1
	clr	a	
	do	y1,MULT1
	add	x1,a
MULT1	
	move	a1,x1
	move	r5,b1
	cmpu	x1,b
	blt	TRACK_NOSUB1
	sub	x1,b
TRACK_NOSUB1
	move	b1,r5
	
	move	r3,x1
	move	y:Track_Plane_b,y1		;24 bit format is "bit 23 is 2^5, bit 0 is 2^(-18)"
	clr		a
	mpyuu	x1,y1,a
	move	a0,y1
	lsl		#5,a
	move	a1,y0
	clr		a
	move	y1,a
	lsr		#19,a
	add		y0,a
	
	move	a1,x1
	move	y:Track_Average,y1
	clr	a	
	do	y1,MULT2
	add	x1,a
MULT2	
	move	a1,x1
	move	r5,b1
	cmpu	x1,b
	blt	TRACK_NOSUB2
	sub	x1,b
TRACK_NOSUB2
	move	b1,r5

	move	r5,a
	move	r4,x0
	cmpu	x0,a
	bgt		CHECK_MAX			;if current Z>Z_best
	btst	#0,r7
	;bcs		LEAVE_SAME
	bcc		LEAVE_SAME	;corrected by SW
	jmp		UPDATE
CHECK_MAX
	btst	#0,r7
	;bcc		LEAVE_SAME	
	bcs		LEAVE_SAME	;corrected by SW
UPDATE
	move	r2,r0
	move	r3,r1
	move	r5,r4
LEAVE_SAME	

	rts

;*************************************************************
; HOPSETUP
; Receive & store HOP parameters
;	TOUCHES: a, x0(via RECEIVE), y0
;	EXIT:    (y:*) parameter values
;*************************************************************
HOPSETUP
	jclr	#2,x:M_SSR,*	;receive average 256 max
	movep	x:M_SRXL,y0		
	move	y0,y:Hop_Avg

	jsr		RECEIVETHREE	;receive step delay
	move	a1,y0
	move	y0,y:Hop_Step_Delay
	
	jsr		RECEIVETHREE	;receive inter step delay
	move	a1,y0
	move	y0,y:Hop_IStep_Delay

	jsr		RECEIVETHREE	;receive circle delay
	move	a1,y0
	move	y0,y:Hop_Circ_Delay

	jsr		RECEIVETWO		;receive R
	move	a1,y0
	move	y0,y:Hop_R
	
	jsr		RECEIVETWO		;receive R*Sqrt(2)/2
	move	a1,y0
	move	y0,y:Hop_R_Sqrt	

	jsr		RECEIVETHREE	;receive a
	move	a1,y0
	move	y0,y:Hop_Plane_a

	jsr		RECEIVETHREE	;receive b
	move	a1,y0
	move	y0,y:Hop_Plane_b

	jmp		MAIN_LOOP

;*************************************************************
; HOP
; Performs hop tracking
;	TOUCHES:	(n0): central x coordinate
;				(n1): central y coordinate
;				(n2): not used
;				(n3): not used
;				(n4): used for reading 
;				(n5): x slope
;				(n6): y slope
;	EXIT:    
;*************************************************************


HOP
	;Stop all timers
	bclr	#0,x:M_TCSR0
	bclr	#0,x:M_TCSR1
	bclr	#0,x:M_TCSR2

	;Set up timer0
	movep	#$000000,x:M_TLR0	;start counting from 0
	movep	#$FFFFFF,x:M_TCPR0	;flip the counter at the max value
	move	#$001800,a			;this makes the sinlgle counter event = 1ms
	and		#$1FFFFF,a			;ensure that the bits 21-23 are 0
	movep	a1,x:M_TPLR			;set up the prescaler register
	
	;setup the TCSR
	movep	#$000000,x:M_TCSR0	;zero all the settings registers
	bset	#15,x:M_TCSR0		;bit 15 prescaler enable = 1
HOP_AGAIN

	;Step 1 recieve the initial x and y coordinates
	jsr		RECEIVETWO	;receive initial x
	move	a1,n0
	jsr		RECEIVETWO	;receive initial y
	move	a1,n1
	
	;Step 2 moving the tip to the starting position
	move	y:Hop_IStep_Delay,y0
	move	y0,r7			;set the delay for RAMPLOOP
	move	n0,y0
	move	n1,y1

	move	y:Hop_Step_Delay,y0
	move	y0,y:Send_Delay
	jsr		SEND_TIP		;sends tip to the initial position
	
	move	x:M_TCR0,x0		;read the timer
	move	x0,y0
	move	y0,y:Hop_Timer	;save the timer value

		;try without zeroing the timer each time (!!!)
		;bclr	#0,x:M_TCSR0	;zero the timer
		;bset	#0,x:M_TCSR0	;start the timer

	jsr		HOP_SLOPE	
	
	;Step 3: Send the slopes to the computer
	move	n5,a
	jsr		SERIAL_WORD		;send x slope to the computer			
	move	n6,a
	jsr		SERIAL_WORD		;send y slope to the computer
	move	y:Hop_Timer,y0
	move	y0,a
	jsr		SERIAL_WORD		;send the time to the computer

	;Step 4:find out whether to continue or to stop
	jclr	#2,x:M_SSR,*	;get the ansver back
	movep	x:M_SRXL,a1
	move	#>CMDTRACKCONT,x0
	cmpu	x0,a
	beq		HOP_AGAIN		;if the answer is 'c'

	bclr	#0,x:M_TCSR0	;stop the timer

	;go to the center of the last circle
	move	n0,y0
	move	n1,y1
	jsr		SEND_TIP

	jmp		MAIN_LOOP


SEND_TIP	;sends tip from the current location to the given coordinates
			;ramps x first
			;Input: y0 = x final, y1 = y final, r7 delay used in ramp
			
	move	y0,y:Send_to_x 
	move	y1,y:Send_to_y	;save the settings for the future use
	
	;Step 1: Ramp x
	move	#>0,r2
	move	#>2,x1		;put the channel in x1
	move	y:OUT_ADDRESS+2,y0
	move	y0,a			;move current x to a1
	and		#>DATAMSK,a
	move	y:Send_to_x,y0
	move	y0,b			;move final x to b1
	jsr		RAMPDIR			;setup for the ramp
	jsr		RAMPLOOP		;ramping x

	;Step 2: Ramp y
	move	#>0,r2
	move	#>3,x1		;put the channel in x1
	move	y:OUT_ADDRESS+3,y0
	move	y0,a			;move current y to a1
	and		#>DATAMSK,a
	move	y:Send_to_y,y0
	move	y0,b			;move final y to b1
	jsr		RAMPDIR			;setup for the ramp
	jsr		RAMPLOOP		;ramping y

	;Step 3: Delay
	move	y:Send_Delay,y0
	DELAY	y0

	rts

HOP_SLOPE	;move around the circle or radius R and calculate the x and y slopes

	move	#>$800000,n5			;initialize slopes to 0
	move	#>$800000,n6
	
	;Point 1
	
	move	n0,y0			;x coord of point 1
	move	n1,a			;initial coord
	move	y:Hop_R,y1
	add		y1,a
	move	a1,y1			;y coord of point 1
	jsr		SEND_TIP
	jsr		HOP_READ
	lsr		#1,a			;divide by 2
	move	n6,y0
	add		y0,a
	move	a1,n6
	
	
	;Point 5
	move	n0,y0			;x coord of point 5
	move	n1,a
	move	y:Hop_R,y1
	sub		y1,a
	move	a1,y1			;y coord of point 5
	jsr		SEND_TIP
	jsr		HOP_READ
	lsr		#1,a			;divide by 2
	move	a1,y0
	move	n6,a
	sub		y0,a
	move	a1,n6

	;Point 3
	move	n0,a
	move	y:Hop_R,y0
	add		y0,a
	move	a1,y0			;x coord
	move	n1,y1			;y coord
	jsr		SEND_TIP
	jsr		HOP_READ
	lsr		#1,a			;divide by 2
	move	n5,y0
	add		y0,a
	move	a1,n5

	;Point 7
	move	n0,a
	move	y:Hop_R,y0
	sub		y0,a
	move	a1,y0			;x coord
	move	n1,y1			;y coord
	jsr		SEND_TIP
	jsr		HOP_READ
	lsr		#1,a			;divide by 2
	move	a1,y0
	move	n5,a
	sub		y0,a
	move	a1,n5

	
	;Point 8
	move	n0,a
	move	y:Hop_R_Sqrt,y0
	sub		y0,a
	move	a,y0
	move	n1,a
	move	y:Hop_R_Sqrt,y1
	add		y1,a
	move	a1,y1
	jsr		SEND_TIP	
	jsr		HOP_READ
	lsr		#2,a			;divide by 4
	move	a1,y1
	move	n6,y0
	add		y0,a
	move	a1,n6			;update y slope
	move	n5,a
	sub		y1,a
	move	a1,n5			;update x slope
	
	;Point 2
	move	n0,a
	move	y:Hop_R_Sqrt,y0
	add		y0,a
	move	a,y0
	move	n1,a
	move	y:Hop_R_Sqrt,y1
	add		y1,a
	move	a1,y1
	jsr		SEND_TIP	
	jsr		HOP_READ
	lsr		#2,a			;divide by 4
	move	a1,b
	move	n6,y0
	add		y0,a
	move	a1,n6			;update y slope
	move	n5,y0
	add		y0,b
	move	b1,n5			;update x slope
	
	;Point 4
	move	n0,a
	move	y:Hop_R_Sqrt,y0
	add		y0,a
	move	a,y0
	move	n1,a
	move	y:Hop_R_Sqrt,y1
	sub		y1,a
	move	a1,y1
	jsr		SEND_TIP	
	jsr		HOP_READ
	lsr		#2,a			;divide by 4
	move	a1,y1
	move	n5,y0
	add		y0,a
	move	a1,n5
	move	n6,a
	sub		y1,a
	move	a1,n6

	;Point 6
	move	n0,a
	move	y:Hop_R_Sqrt,y0
	sub		y0,a
	move	a,y0
	move	n1,a1
	move	y:Hop_R_Sqrt,y1
	sub		y1,a
	move	a1,y1
	jsr		SEND_TIP	
	jsr		HOP_READ
	lsr		#2,a			;divide by 4
	move	a1,y1
	move	n6,a
	sub		y1,a
	move	a,n6
	move	n5,a
	sub		y1,a
	move	a1,n5

	rts

HOP_READ	;exits with a1 equal to the sum
	move	y:Hop_Avg,y0	;get number or reads
	move	#$000000,n4		;sum is set to 0
	
	do		y0,HOP_ENDREAD
	move	#>ZCH,a
	jsr		INPUTVAL		;read the Z
	move	n4,y0
	add		y0,a			;sums the current from several readings
	move	a1,n4
HOP_ENDREAD
	nop
	rts

;*************************************************************
; TIPSETUP
; Receive & store parameters common to both GIANT and TIPAPPROACH.
;	TOUCHES: a, x0(via RECEIVE), y0
;	EXIT:    (y:*) parameter values
;*************************************************************
TIPSETUP
	jsr		RECEIVETHREE	; receive zo parabola multiplier	(3 bytes)
	move	a1,y0
	move	y0,y:Tip_Zo_Mult

	jsr		RECEIVETWO	; receive zo parabola step size (2 bytes)
	move	a1,y0
	move	y0,y:Tip_Zo_Step

	jsr		RECEIVETHREE	; receive x parabola multiplier	(3 bytes)
	move	a1,y0
	move	y0,y:Tip_X_Mult

	jsr		RECEIVETWO	; receive x parabola step size (2 bytes)
	move	a1,y0
	move	y0,y:Tip_X_Step

	jsr		RECEIVETWO	; receive x zeroing cycles (2 bytes)
	move	a1,y0
	move	y0,y:Tip_X_Zero
					
	jsr		RECEIVETHREE	; receive delay	(3 bytes)
	move	a1,y0
	move	y0,y:Tip_Delay

	jmp		MAIN_LOOP

;*************************************************************
; TIPAPPROACH
; Performs coarse approach. Alternates between baby steps
; (searching for current) and giant steps (coarse approach).
; Stops if min tunneling current is observed or if
; a CMDSTOP signal is received.
;	TOUCHES: a, b, x0, x1, y0, y1 (mostly via subroutines)
;	USES:	(r1): flags (see below for bit by bit description)
;			(rN): subroutines
;	FLAGS:  (#0): set for increasing parabolas, cleared for decreasing
;			(#1): set for inverted parabolas, cleared for normal
;			(#2): set for giant steps up, cleared for giant steps down
;			(#3): set when min tunneling current detected
;*************************************************************
TIPAPPROACH

	jclr	#2,x:M_SSR,*
	movep	x:M_SRXL,a1		; receive # giant steps / pass
	move	a1,y0
	move	y0,y:Tip_Num_Giant

	jclr	#2,x:M_SSR,*
	movep	x:M_SRXL,a1		; receive baby step size
	move	a1,y0
	move	y0,y:Tip_Baby_Size

	jsr		RECEIVETWO		; receive min tunneling current
	move	a1,y0
	move	y0,y:Tip_Min_I

	jsr		RECEIVETHREE	; receive baby zo parabola multiplier
	move	a1,y0
	move	y0,y:Tip_Baby_Mult

	bclr	#3,r1			; initialize

	do		forever,ONEPASS

	move	#>0,y0
	move	y0,y:Tip_Update
	jsr		BABYSTEPS
	btst	#3,r1			; are we done?
	nop
	brkcs

	move	y:Tip_Num_Giant,y0
	bclr	#2,r1			; steps are down during tip approach
	do		y0,APPROACHGIANTS
	jsr		ONEGIANT
	nop
APPROACHGIANTS

	; check for CMDSTOP on serial input and enddo if necessary
	jset	#2,x:M_SSR,STOPAPPROACH
	jmp		MOREAPPROACH
STOPAPPROACH
	clr		a
	movep	x:M_SRXL,a1
	move	#>CMDSTOP,x0
	cmpu	x0,a
	nop
	brkeq

MOREAPPROACH
	DELAY	#>100			;unnecessary?
	move	y:Tip_Update,y0
	move	y0,a
	lsl		#8,a
	move	#>CMDAPPROACH,y0
	add		y0,a
	jsr		SERIAL_WORD
	nop

ONEPASS

; send out final current
	jclr	#3,r1,DUNAPPROACH
	move	y:Tip_Update,y0
	move	y0,a
	lsl		#8,a
	move	#>CMDSTOP,y0
	add		y0,a
	jsr		SERIAL_WORD

DUNAPPROACH
	jmp		MAIN_LOOP

;*************************************************************
; BABYSTEPS
; Ramps z outer, searching for min tunneling current. If no
; current is found, restore z outer via parabolic waveform.
;	TOUCHES: a, b, x0, x1, y0, y1 (via INPUTBITS, OUTPUTBITS and others)
;	USES: (r1) : flags
;		  (r3) : channel, passed to PARABOLA
;		  (r4) : start for PARABOLA
;		  (r6) : current z outer during babysteps
;			     & multiplier for PARABOLA during restoration
;		  (r7) : end for PARABOLA
;	FLAGS: see TIPAPPROACH flags
;	EXIT: appropriate flag set if current is seen
;*************************************************************
BABYSTEPS
	DELAY	#>BABYWAITI
	READIN	#>ICH			; read I (into b1)
	jsr		ABSIN

	move	y:Tip_Min_I,y0
	cmpu	y0,b			; do we see min tunneling current?
	jlt		FORTH
	
	DELAY	#>BABYWAITII	; false trigger? wait and be sure
	move	b1,y0			; store I that triggered us
	move	y0,y:Tip_Update

	READIN	#>ICH			; read I (into b1)
	jsr		ABSIN

FORTH
	move	#>ZERO,r6		; initial z outer
	do		#>BABYSIZE,ONEBABY		; may execute less than this if step size is > 1

	move	y:Tip_Min_I,y0
	cmpu	y0,b			; do we see min tunneling current?
	nop
	brkgt

	move	r6,a			; increment zo
	move	y:Tip_Baby_Size,y0
	add		y0,a

	move	#>OUTMAX,y0
	cmpu	y0,a			; have we maxed z outer?
	nop
	brkge

	move	a,r6
	move	#>ZOCH,y1
	jsr		OUTPUTBITS

	DELAY	#>1750			; allow time for output to reach final voltage
	READIN	#>ICH
	jsr		ABSIN

	move	y:Tip_Min_I,y0
	cmpu	y0,b			; do we see min tunneling current?
	nop
	jle		NEXTBABY

	DELAY	#>BABYWAITII	; false trigger? wait and be sure
	move	b1,y0			; store I that triggered us
	move	y0,y:Tip_Update
	READIN	#>ICH			; read I (in b1)
	jsr		ABSIN

NEXTBABY
	nop

ONEBABY

	move	y:Tip_Min_I,y0
	cmpu	y0,b			; did we see min tunneling current?
	jle		ZEROZO
	
	bset	#3,r1			; flag that we are done!
	move	r6,y0			; save the z outer value that saw tunneling current
	move	y0,y:Tip_Update
	jmp		ENDBABY

ZEROZO						; no tunneling current seen, restore z outer to rest value 		
	move	#>ZOCH,r3
	move	y:Tip_Baby_Mult,r6
	move	#>OUTMAX,r4

	; calculate target
	move	#>BABYSIZE,a
	lsr		a					; max/2 is size of half-parabola
	move	#>ZERO,y1
	add		y1,a
	move	a1,r7

	bclr	#0,r1				; decreasing parabola				
	bclr	#1,r1				; normal parabola, *not* inverted
	jsr		PARABOLA

	; output zouter parabola - 2nd half (inverted)
	move	r7,r4
	move	#>ZERO,r7
	bset	#1,r1				; inverted parabola
	move	x1,a
	move	#>1,x1
	sub		x1,a
	move	a1,x1
	jsr		PARABOLA

	;output zero to zouter
	move	#>ZERO,a
	move	#>ZOCH,y1
	jsr		OUTPUTBITS

ENDBABY
	rts

;*************************************************************
; GIANT, ONEGIANT
; Performs giant steps.
;	TOUCHES: a, b, x0, x1, y0, y1 (mostly via subroutines)
;	USES: (r0) : max number of giant steps
;		  (r1) : flags
;		  (r3) : channel, passed to PARABOLA
;		  (r4) : start for PARABOLA
;		  (r6) : multiplier for PARABOLA
;		  (r7) : end for PARABOLA
;	FLAGS: see TIPAPPROACH flags
;*************************************************************
GIANT

	jsr		RECEIVETWO	; receive number of steps + direction (2 bytes)
	move	a1,r0
	
	bset	#2,r1
	bclr	#15,r0		; test and clear direction bit
	bcs		GIANTUP
	bclr	#2,r1
GIANTUP

	do		r0,ONESTEP

	jsr		ONEGIANT

	; check for serial input and enddo if necessary
	jset	#2,x:M_SSR,STOPSTEPS
	jmp		PROCEED
STOPSTEPS
	clr		a
	movep	x:M_SRXL,a1
	move	#>CMDSTOP,x0
	cmpu	x0,a
	nop
	brkeq

PROCEED
	; send control character via serial for each step completed 
	DELAY	#>100			;unnecessary?
	move	#>CMDGIANT,a
	jclr	#0,x:M_SSR,*	;bit set when transmit buffer empty
							;until then, jmp to self
	movep	a1,x:M_STXL		;send byte

ONESTEP
	jmp		MAIN_LOOP

;*************************************************************
; ONEGIANT (see above)
;*************************************************************

ONEGIANT

	bset	#0,r1
	btst	#2,r1
	bcs		PARABUP
	bclr	#0,r1
PARABUP

	; pre-parabola: just output zero volts to x
	move	#>ZERO,a
	move	#>XCH,y1
	jsr		OUTPUTBITS
	move	y:Tip_X_Zero,y0
	do		y0,DUNXZERO			
	DELAY	#>DIOOUTTIME
DUNXZERO

	; output x parabola
	move	#>XCH,r3
	move	#>ZERO,r4
	move	y:Tip_X_Mult,r6
	move	y:Tip_X_Step,y0
	move	#>ZERO,a
	jclr	#0,r1,DOWNTARGET
	add		y0,a
	jmp		DUNTARGET
DOWNTARGET
	sub		y0,a
DUNTARGET
	move	a1,r7
	bclr	#1,r1				; normal parabola, *not* inverted

	jsr		PARABOLA

	; set zouter to step value
	move	y:Tip_Zo_Step,y0
	move	y0,a
	move	#>ZERO,y0
	add		y0,a
	move	a1,r4
	move	#>ZOCH,y1
	jsr		OUTPUTBITS

	; delay 5 * output time
	do		#>5,RELAXATION
	DELAY	#>DIOOUTTIME
RELAXATION

	; relax x
	move	#>ZERO,a
	move	#>XCH,y1
	jsr		OUTPUTBITS

	; do tip delay
	move	y:Tip_Delay,y0
	DELAY	y0

	; output zouter parabola - 1st half
	move	#>ZOCH,r3
	move	y:Tip_Zo_Mult,r6

	; calculate target
	move	y:Tip_Zo_Step,y0
	move	y0,a
	lsr		a					; Step/2 is size of half-parabola
	move	#>ZERO,y1
	add		y1,a
	move	a1,r7

	bclr	#0,r1				; decreasing parabola				
	bclr	#1,r1				; normal parabola, *not* inverted
	jsr		PARABOLA

	; output zouter parabola - 2nd half (inverted)
	move	r7,r4
	move	#>ZERO,r7
	bset	#1,r1				; inverted parabola
	move	x1,a
	move	#>1,x1
	sub		x1,a
	move	a1,x1
	jsr		PARABOLA

	;output zero to zouter
	move	#>ZERO,a
	move	#>ZOCH,y1
	jsr		OUTPUTBITS

	rts

;Pushkin
;*************************************************************
; PARABOLA
; Output parabolic waveform on an output channel.
;	ENTRY: (r3) : Output Ch
;		   (r4) : Start output value
;		   (r6) : Multiplier
;		   (r7) : Target output value
;	TOUCHES: a, b, x0, x1, y0, y1 (some via OUTPUTBITS, others)
;	USES: (x1) : index
;		  (r1) : flags
;		  (r3),(r6),(r7) : passing parameters, as listed above
;		  (r4) : current output value
;		  (r5) : storage of index during call to subroutines
;	EXIT: (r4) : Actual final output value
;		  (x1) : number of cycles to make parabola + 1
;*************************************************************
PARABOLA

	clr		b			; b is where we'll keep the current 12 bit output value
	move	r4,b1		; move start output value to b1

	; copy bits to a and output
	move	b1,a1			
	move	r3,y1
	move	b1,r4			; save
	jsr		OUTPUTBITS		; this will output the start value
	move	r4,b1			; restore

	jset	#1,r1,INVINDEX	;set for inverted parabola
	move	#>0,x1			;x1 is current index, init to 0 - normal parabola
INVINDEX
	; note: when an inverted parabola is called, x1 is initialized before the call

	do forever,ENDXPARAB

	; calculate {(x1 * 2 + 1) * multiplier}, put in accumulator a 
	clr		a
	move	x1,a1
	lsl		a				; * 2
	add		#>1,a			; + 1 (a1 now has x1 * 2 + 1)
	move	a1,x0			; x0 = x1 * 2 + 1
	move	r6,y0
	mpyuu	x0,y0,a			; * multiplier

	; total shift = 1(mpy) + 5(lsl) = 6 bits above "."
	lsl		#17,a			; preserve 7 ms bits that are in a1
	move	a1,y0			; 7 MSB of y0 are now the 7 LSB of a1 after multiplication
	move	a0,y1			
	clr		a
	move	y1,a1			
	lsr		#7,a
	add		y0,a
	lsr		#12,a

	; add / subtract result to b1
	jclr	#0,r1,SUBSTEP
	add		a,b
	jmp		DUNADDSUB
SUBSTEP
	sub		a,b
	nop
	brklt						; never go negative!
DUNADDSUB

	move	b1,a1
	move	r7,y0
	jclr	#0,r1,SUBCOMP
	cmpu	y0,a				; compare current output to target
	nop
	brkgt						; is it greater than or equal?
	jmp		DUNCOMP
SUBCOMP
	cmpu	y0,a				; compare current output to target
	nop
	brklt						; is it less than or equal?
DUNCOMP

	move	b1,r4
	

	move	#>DATAMSK,y0		; paranoia: make sure we don't write to wrong ch!
	and		y0,a

	move	r3,y1
	move	x1,r5		; save x1 in register

	jsr		OUTPUTBITS	; do I need to add additonal delay?
	DELAY	#>OUTPUTFUDGE	; OUTPUTBITS takes about two microsecs
							; To work with conventional dio tip approach parameters
							; we need for each output to take a little longer.

	move	r4,b1		; restore relevant values
	move	r5,x1
	
	; increments/decrements x1 by 1 for normal/inverted parabola
	clr		a
	move	x1,a	
	move	#>1,y0
	jset	#1,r1,INVINCR
	add		y0,a	; increment x1 (normal)
	jmp		DUNINVINC
INVINCR
	sub		y0,a	; decrement x1 (inverted)
	nop
	brklt			; don't let x1 go negative
	nop
	nop
DUNINVINC
	move	a1,x1

ENDXPARAB
	nop

	rts
	
;*************************************************************
; VERSION
; Send back version string: VVVVDDMMYYYY
;	TOUCHES: a, x0(via SERIAL_WORD)
;*************************************************************
VERSION
	move	#VER1,a
	jsr		SERIAL_WORD
	move	#VER2,a
	jsr		SERIAL_WORD
	move	#VER3,a
	jsr		SERIAL_WORD
	move	#VER4,a
	jsr		SERIAL_WORD
	jmp		MAIN_LOOP

;*************************************************************
; USERDELAY
; Receives 24 bits that give the duration of the delay in cycles.
; The total delay is then (delay cycles) * 11.6 nanoseconds.
; Sends back a control character when delay is complete.
;	TOUCHES: a, b, x0(via subroutines)
;*************************************************************
USERDELAY
	clr		b
	jsr		RECEIVETHREE	; receive delay in cycles
	add		a,b
	DELAY	b1
	jclr	#0,x:M_SSR,*	;bit set when transmit buffer empty
							;until then, jmp to self
	move	#>CMDDELAY,a0
	movep	a0,x:M_STXL		;send byte

	jmp		MAIN_LOOP

;*************************************************************
; USERDELAYLONG
; Receives 24 bits that give the duration of the delay in milliseconds.
; The total delay is then (delay cycles) * 1 milliseconds.
; Sends back a control character when delay is complete.
;	TOUCHES: a, b, x0
;*************************************************************
USERDELAYLONG
	clr		b
	jsr		RECEIVETHREE	; receive delay in cycles
	add		a,b

	move	#>86011,x0
	do	b1,DELAYLONG		;The total time consumed by this do loop is
	rep 	x0			;t=(b1*(5+86011)+5)*(clock cycles)
	nop 			
DELAYLONG	

	jclr	#0,x:M_SSR,*		;bit set when transmit buffer empty
					;until then, jmp to self
	move	#>CMDDELAYLONG,a0
	movep	a0,x:M_STXL		;send byte

	jmp		MAIN_LOOP


;*************************************************************
; SQUARE
; Output a square wave on the bias channel.
;   TOUCHES: a, b, x0, y0, y1 (some via subroutines)
;	USES: (r1) : first bias
;		  (r2) : second bias
;		  (r3) : delay in cycles
;	EXIT: Output bias will be left at second bias.
;*************************************************************
SQUARE
	jsr		RECEIVETHREE	;receive delay in cycles
	move	a1,r3

	jsr		RECEIVETWO	    ;receive second bias in bits
	move	a1,r2
	
	jsr		RECEIVETWO		;receive first bias in bits
	move	a1,r1

WAVE
	;set up first bias in a1, a0 and send
	move	r1,a1
	move	#>BIASCH,y1
	jsr		OUTPUTBITS

	DELAY	r3

	;set up second bias in a1, a0 and send
	move	r2,a1
	move	#>BIASCH,y1
	jsr		OUTPUTBITS

	DELAY	r3

	jset	#2,x:M_SSR,STOPWAVE
	jmp		WAVE
STOPWAVE
	clr		a
	movep	x:M_SRXL,a1
	move	#>CMDSQUARE,x0
	cmpu	x0,a
	bne		WAVE
	jmp		MAIN_LOOP

;*************************************************************
; OUTPUT
; Receive 16 bits of data (read serial twice) and send those bits
; to the electronics via the conversion module.
;	TOUCHES: a, b, x0, y0, y1 (mostly via INPUTBITS) 
;*************************************************************
OUTPUT
	;receive 16 more bits of information: CH[15-12] DATA[11-0]
	jclr	#2,x:M_SSR,*
	movep	x:M_SRXL,a0		;receive 4 ch bits, 4 MS data bits
	jclr	#2,x:M_SSR,*
	movep	x:M_SRXL,a1		;receive 8 LS data bits

	jsr		OUTPUTBITS_TWO
	jmp		MAIN_LOOP

;*************************************************************
; HIGHRES
; Receive 16 bits of data and 4 bits channel(read serial three times) 
; and send those bits to channels 10 or 11 via the conversion module.
;	TOUCHES: a, b, x0, y0, y1 (mostly via OUTPUTBITS) 
;*************************************************************
HIGHRES
	;receive 16 more bits of information: CH[15-12] DATA[11-0]
	jclr	#2,x:M_SSR,*
	movep	x:M_SRXL,a1		;receive 8 MS data bits
	jclr	#2,x:M_SSR,*
	movep	x:M_SRXL,x0		;receive 8 LS data bits
    jclr	#2,x:M_SSR,*
	movep	x:M_SRXL,y1		;receive 4 LS channel bits
	lsl		#8,a1
	add		x0,a
	
	jsr		HIGHRESBITS
	jmp		MAIN_LOOP

;*************************************************************
; INPUT
; Receive input channel from serial, read 16 bits of data for that
; channel via conversion module and send those bits out via
; the serial connection.
;	TOUCHES: a, b, x0, x1, y0, y1 (mostly via INPUTBITS) 
;*************************************************************
INPUT
	; receive channel from serial
	jclr	#2,x:M_SSR,*
	movep	x:M_SRXL,a1		;receive 6 bits cleared, 2 bits channel

	jsr		INPUTBITS

	; send out 16 bits of data via SCI
	jclr	#0,x:M_SSR,*	;bit set when transmit buffer empty
							;until then, jmp to self
	movep	a0,x:M_STXL		;send byte
	DELAY	#>100
	jclr	#0,x:M_SSR,*	;bit set when transmit buffer empty
							;until then, jmp to self
	movep	a1,x:M_STXL		;send byte

	jmp		MAIN_LOOP

;*************************************************************
; INPUTBITS
; Read 16 bits of data via the conversion module.
;	ENTRY: (a1): input channel
;	TOUCHES: a, b, x0(via DELAY), x1, y0, y1 
;	EXIT: (a0): 8 MS bits of data that was read
;		  (a1): 8 LS bits of data that was read
;*************************************************************

INPUTBITS
	; Format is Step #n: (CB2 CB3 CB4)
	; Step #1: (x x -) Output Channel to read
	lsl		#2,a1
	move	#>CBMSKOUT,b			;mask to retain CB0,1
	move	y:CB_Box,y1
	and		y1,b
	add		a,b
	movep	b,x:M_PDRD
	lsr		#2,a1					;undo previous shift
	move	b1,y1
	move	y1,y:CB_Box

	; Step #2: Strobe A/D conversion & delay
	move	a1,x1					;keep it around
	move	#>CHDUPPERMSK,a
	move	y:Ch13_uBits,y0
	and		y0,a
	move	a1,y0

	move	#>1,a1
	clr		b
	cmpu	x1,b
	beq		ZEROSHIFT	;can't do rep with x0 = 0; will repeat 0xFFFF times!
	rep		x1			;input_ch = 1<<(ch*2+1);
	lsl		#2,a
ZEROSHIFT
	lsl		a
	move	y0,a0

	jsr		OUTPUTBITS_TWO	;strobe => 1
	DELAY	#>10
	move	#$000000,a1
	jsr		OUTPUTBITS_TWO	;strobe => 0
	
	;delay to allow for a->d conversion
	DELAY	#>ADCTIME	;1750 * 11.6 nsec > 20 usec

	; Step #3: (x x 1) Read 8 LS bits.
	move	x1,a1
	lsl		#2,a1
	move	#>CBMSKOUT,b			;mask to retain CB0,1
	move	y:CB_Box,y1
	and		y1,b
	add		a,b
	add		#$10,b
	movep	b,x:M_PDRD
	DELAY	#>10
	movep	x:M_HDR,a1	
	DELAY	#>10
	lsr		#8,a
	move	a1,x1

	; Step #4: (x x 0) Read 8 MS bits.
	sub		#$10,b
	movep	b,x:M_PDRD
	DELAY	#>10
	movep	x:M_HDR,a1	
	DELAY	#>10
	lsr		#8,a
	move	a1,x0
	move	x1,a1
	move	x0,a0

	move	b1,y1
	move	y1,y:CB_Box

	rts	

;*************************************************************
; INPUTVAL
; Read 16 bits of data via the conversion module.
;	ENTRY: (a1): input channel
;	TOUCHES: a, b, x0, x1, y0, y1 (via INPUTBITS)
;	EXIT:  (a1): 16 bits of read data
;*************************************************************
INPUTVAL				;calls INPUTBITS and splices the output into 
						;a single 24 bit number
	jsr		INPUTBITS
	move	a1,x0		;record 8 LS bits in x0
	move	a0,a1		;move 8 MS bits to a1
	lsl		#8,a		;shift 8 MS bits left
	add		x0,a		;8 LS + 8 MS
	move	#>0,a0

	rts

;*************************************************************
; OUTPUTBITS
; Takes a 12 bits of data and 4 bits of channel information
; and sends them to the electronics via the conversion module.
;	ENTRY: (a1) 12 bits of data
;		   (y1) 4 bits of channel information
;	ALTERNATE ENTRY:
;		   (a1) 12 bits of data spliced with channel for 16 bits
;			    total.
;		   (y1) cleared
;	TOUCHES: a, b, x0, y0, y1
;*************************************************************
OUTPUTBITS
	; preout
	move	a1,y0
	lsr		#8,a
	add		y1,a
	move	a1,y1
	move	y0,a1
	move	#>LOWERMSK,y0
	and		y0,a
	move	y1,a0

	; Format is "Step #n: (CB1 CB0)"
	; Step #1: (0 1) Output 4 ch bits, 4 MS data bits
OUTPUTBITS_TWO					; this label is for calling when preout is unnecessary
	move	#>CBMSKIN,b			;mask to retain CB2,3,4
	move	y:CB_Box,y1
	and		y1,b
	add		#$01,b
	movep	b,x:M_PDRD
	DELAY	#>10
	movep	a0,x:M_HDR
	DELAY	#>10

	; Step #2: (0 0) Latch bits
	sub		#$01,b
	movep	b,x:M_PDRD
	DELAY	#>10

	; Step #3: (0 0) Output 4 LS data bits
	movep	a1,x:M_HDR
	DELAY	#>10

	; Step #4: (1 0) Acknowledgement
	add		#$02,b
	movep	b,x:M_PDRD
	DELAY	#>10

	; Step #5: (0 0) Turn off acknowledgement
	sub		#$02,b
	movep	b,x:M_PDRD
	DELAY	#>10
	move	b1,y1
	move	y1,y:CB_Box

	; Additional Step: need to save upper bits if ch == 13
	move	#>CHMASK,b
	move	a0,x0
	and		x0,b
	move	#>$D0,x0
	cmpu	x0,b
	bne		NOTCHD
	move	a0,y1
	move	y1,y:Ch13_uBits
NOTCHD

	; Save the Bits (including upper 4 channel bits)
	move	a1,x0
	move	a0,y0
	move	y0,a1
	lsl		#8,a
	add		x0,a
	move	a1,y1
	lsr		#4,b
	move	b1,y0
	move	#>Out_Bits,b
	add		y0,b		;offset equal to Ch
	move	r1,y0		;save
	move	b1,r1
	move	y1,y:(r1)
	move	y0,r1		;restore

	rts

;*************************************************************
; HIGHRESBITS
; Receive 16 bits of data and 4 bits channel 
; and send those bits to channels 10 or 11 via the conversion module.
;	ENTRY:	(a1) 16 bits of data
;			(y1) 4 bits of channel information
;	TOUCHES: a, b, x0, x1, y0, y1 (mostly via OUTPUTBITS) 
;*************************************************************
HIGHRESBITS
	move	a1,b1
	and		#>DATAMSK,b
	move	b1,x1			;the 12 bits are stored in x1
	and		#>HUPDATAMSK,a	;leave only 4 upper bits in a1
	move	#>$000A,b1
	cmpu	y1,b			;find out which channel to write to
	beq		CHTEN
	move	#>$000B,b1
	cmpu	y1,b
	beq		CHELEVEN
	move	y1,b1			; for those unfortunate people who call
	lsl		#4,b			; highresbits with channel other than 10 or 11
	move	b1,y1
	jmp		OUTREST
CHTEN
	lsr		#4,a						
	and		#>$0F00,a		;a1 has the upper bits from ch10
	move	y:Ch15_Bits,y0
	move	y0,b1
	and		#>$00F0,b
	move	b1,y0		
	add		y0,a			;a1 has the new ch10 bits and the old ch11 bits
	move	a1,y0			;updating the memory
	move	y0,y:Ch15_Bits
	move	#>$00F0,y1		;go to ch15
	jsr		OUTPUTBITS
	move	#>$00A0,y1
	jmp		OUTREST
CHELEVEN
	lsr		#8,a						
	and		#>$00F0,a		;a1 has the upper bits from ch10
	move	y:Ch15_Bits,y0
	move	y0,b1
	and		#>$0F00,b
	move	b1,y0		
	add		y0,a			;a1 has the new ch11 bits and the old ch10 bits
	move	a1,y0			;updating the memory
	move	y0,y:Ch15_Bits
	move	#>$00F0,y1
	jsr		OUTPUTBITS
	move	#>$00B0,y1
OUTREST
	move	x1,a1
	jsr	OUTPUTBITS

	rts


;*************************************************************
; ZMINMAXUPDATE 
;	Updates ThisLine_Min and ThisLine_Max
;	ENTRY: (b): most recent z value
;	TOUCHES: y0
;	EXIT:  (b): most recent z value (unchanged)
;*************************************************************
ZMINMAXUPDATE
	move	y:ThisLine_Min,y0
	cmpu	y0,b
	jge		NO_MIN_UPDATE
	move	b1,y0
	move	y0,y:ThisLine_Min
NO_MIN_UPDATE
	move	y:ThisLine_Max,y0
	cmpu	y0,b
	jle		NO_MAX_UPDATE
	move	b1,y0
	move	y0,y:ThisLine_Max
NO_MAX_UPDATE
	nop
	rts

;*************************************************************
; INCR_ANY	;adds one to anything, uses b
; ENTRY: value to increment in b
; EXIT: value in b is incremented, unless we're at MAX
;*************************************************************

INCR_ANY
	cmp		#>4094,b
	bgt		DONT_INCR
	add		#>1,b
DONT_INCR
	rts

;*************************************************************
; DECR_ANY	;subtracts one from anything, uses b
; ENTRY: value to decrement in b
; EXIT: value in b is decremented, unless we're at MIN
;*************************************************************

DECR_ANY
	cmp		#>1,b
	blt		DONT_DECR
	sub		#>1,b
DONT_DECR
	rts

;*************************************************************
; ABSIN 
;	Takes the "absolute" value of an input value relative to INZERO:
;	 new value = | value - INZERO |
;	ENTRY: (b): value
;	TOUCHES: b, y0, y1, y:Scan_Flags
;	EXIT:  (b): "absolute" value
;*************************************************************
ABSIN
	move	#>INZERO,y0
	cmpu	y0,b
	jlt		NEGBIAS
POSBIAS
	sub		y0,b
	bset	#1,y:Scan_Flags
	jmp		ABSCOMPLETE
NEGBIAS
	move	b1,y1
	move	y0,b1
	sub		y1,b
	bclr	#1,y:Scan_Flags
ABSCOMPLETE
	rts

;*************************************************************
; DIVISION
; !! extremely inefficient for big dividends !!
; ENTRY: b: dividend
;		 y0: divisor
; TOUCHES: a, b, x1, y0
; EXIT:  x1: quotient
;*************************************************************

DIVISION 
	move	#>0,x1
	do		forever,CALCULATION
	clr		a
	move	y0,a0
	sub		a,b
	jge		MOREDIV
	enddo
	jmp		ENDDIV
MOREDIV
	move	x1,a1
	add		#>1,a
	move	a,x1			; quotient ends up in x1
ENDDIV
	nop
CALCULATION
	clr		b
	rts

;*************************************************************
; XDSO (X Data Serial Out)
; Send out x data from the DSP module to the PC via serial.
; A one-word header block is sent first.
; Header block byte		use
;		1				 control character (RAMPREAD)
;		2-3				 number of data words that will follow
;
;	ENTRY: (r6): the number of words of data to be sent
;	USES:  r6, r7
;	TOUCHES: a, x0
;	EXIT:  (r7): the number of words of data that were sent
;*************************************************************
XDSO
	move	r6,x0		;set up and send header block
	move	r6,a1
	lsl		#16,a
	add		x0,a
	lsr		#8,a
	lsl		#8,a
	move	#>CMDRAMPREAD,x0
	add		x0,a
	jsr		SERIAL_WORD

	;send the data
	move	#>0,r7
	do		r6,XDSOWORD
	move	x:(r7)+,a1
	jsr		SERIAL_WORD
	nop
XDSOWORD
	rts

;*************************************************************
; SERIAL_TWO
; Send two bytes out via serial from a1.
;	ENTRY:	 (a1): bytes to send
;	TOUCHES: a, x0(via DELAY macro)
;	EXIT:	 (a): cleared
;*************************************************************
SERIAL_TWO
	jclr	#0,x:M_SSR,*	;bit set when transmit buffer empty
							;until then, jmp to self
	movep	a1,x:M_STXL		;send ls byte
	DELAY	#>100
	lsr		#8,a
	jclr	#0,x:M_SSR,*	;bit set when transmit buffer empty
							;until then, jmp to self
	movep	a1,x:M_STXL		;send ms byte
	clr		a
	rts

;*************************************************************
; SERIAL_WORD
; Send one word (3 bytes) out via serial from a1.
;	ENTRY:	 (a1): word to send
;	TOUCHES: a, x0(via DELAY macro)
;	EXIT:	 (a1): cleared
;*************************************************************
SERIAL_WORD
	do		#3,ONE_BYTE_SENT
	DELAY	#>100
	jclr	#0,x:M_SSR,*	;bit set when transmit buffer empty
							;until then, jmp to self
	movep	a1,x:M_STXL		;send byte

	lsr		#8,a			;shift to next byte

ONE_BYTE_SENT
	rts

;*************************************************************
; RECEIVETHREE
; Receives 3 byte value via serial, MS byte first.
;	TOUCHES: a, x0
;	EXIT: a1: value
;*************************************************************
RECEIVETHREE
	clr		a
	do		#3,GETBYTE
	lsl		#8,a
	move	a1,x0
	jclr	#2,x:M_SSR,*
	movep	x:M_SRXL,a1
	add		x0,a
GETBYTE
	nop
	rts

;*************************************************************
; RECEIVETWO
; Receives a two-byte value via serial, MS byte first.
;	TOUCHES: a, x0
;	EXIT: a1: value
;*************************************************************
RECEIVETWO
	clr 	a
	jclr	#2,x:M_SSR,*
	movep	x:M_SRXL,a1
	lsl		#8,a
	move	a1,x0
	jclr	#2,x:M_SSR,*
	movep	x:M_SRXL,a1
	add		x0,a
	rts

;*************************************************************
; DAC
; Send out the current Out_Bits values in all 16 electronic 12-bit DAC channels.
; The format of each DAC value is the MS 4 bits for channel number, the LS 12 bits for value.
;	TOUCHES: a,r0,x0
;	EXIT: 
;*************************************************************
DAC
	move	#>Out_Bits,r0
	
	do	#>NUMOUTCHS,DAC_out
	move	y:(r0)+,a1
	jsr	SERIAL_TWO
	nop
DAC_out
	jmp	MAIN_LOOP	

;*************************************************************
; DEBUGGET
; Receive two bytes to position the variable in Y memory,
; then return the value of the variable (3 bytes).
;	Entry: 
;	TOUCHES: a,r0,x0
;	EXIT: 
;*************************************************************
DEBUGGET
	jsr	RECEIVETWO
	
	move	a1,r0
	move	y:(r0),a1
	jsr	SERIAL_WORD
	nop

	jmp	MAIN_LOOP	

;*************************************************************
; DEBUGSET
; Receive the position (two bytes) and value (three bytes) of variable in Y memory,
; then set the values of variable.
;	Entry: 
;	TOUCHES: a,r0,x0,x1
;	EXIT: 
;*************************************************************
DEBUGSET
	;receive the position
	jsr	RECEIVETWO	
	move	a1,x1

	;receive the value
	jsr	RECEIVETHREE
	
	move	x1,r0
	move	a1,y:(r0)
	
	nop
	jmp	MAIN_LOOP	

;**************************************************************
ENDALL	jmp *

;*************************************************************
; END OF PROGRAM
;*************************************************************
