SetDefaultMouseSpeed, 1
SetTitleMatchMode, 2
SetCapsLockState, off
SetNumLockState, on
#Include S:\Institutional_Research\.Staff & Admin\.IR Analyst - Fraser\~Scripts\WindowPad\source\WindowPad.ahk
#Include S:\Institutional_Research\.Staff & Admin\.IR Analyst - Fraser\~Scripts\Gdip.ahk

; Note: everything that begins with a semi-colon (;) is a comment, it is not executed as code.
; For reference, in hotkey (code) calls: !=ALT, ^=CTRL, +=SHIFT, #=WINDOWS-KEY
; ==============================KEYSTROKES======
; ESC = Pause script / set Breakloop variables
; CTRL+SHIFT+R = Reload script
; WIN+C = Launch Calculator
; WIN+S = S:\Institutional_Research shortcut
; WIN+G = G:\(personal folder) shortcut
; WIN+O = Toggle 'Always On Top' for window under mouse
; Numlock = Copy current mouse position (window relative coordinate) to clipboard
; SHIFT+Numlock = Copy current mouse position (screen coordinate) to clipboard
; CTRL+SHIFT+/ = Launch Window Spy (Assistant to read window names and coordinates)
; CTRL+WIN+G = Launch Google in a Firefox new window
; CTRL+WIN+O = Launch OCAS in default browser
; CTRL+WIN+S = Launch SIS in default browser
;
; ---[ WindowPad ]--- (Fraser: I didn't make this! I take no credit for it.)
; WIN+<Any Numpad Key except 0> = Arrange windows. It's pretty neat, intuitive, and useful.
; WIN+<Num+> = Maximize window
;--------------------------------------------------------------------------------------
; Program-specific Shortcuts
;--------------------------------------------------------------------------------------
; ** MICROSOFT EXCEL (2010) **
;------------------------------------------
; ` = Edit current cell (End)
; Insert = Edit current cell (Beginning)
; SHIFT+Mousewheel = Switch sheets
; CTRL+SHIFT+Mousewheel = Switch Books (per instance of excel)
; Middle Mouse Button = Format Painter
; CTRL+SHIFT+V = Paste values
; SHIFT + Middle Mouse Button = Insert Row
; ALT + Middle Mouse Button = Insert Column
; 
;------------------------------------------
; ** MICROSOFT WORD (2010) **
;------------------------------------------
; Middle Mouse Button = Format Painter
; CTRL+SHIFT+V = Paste as text-only
; CTRL+SHIFT+F = Initiates a <Find> command for what is currently in the clipboard [Disabled for PSRs]
;
;------------------------------------------
; ** MICROSOFT OUTLOOK (2010) **
;------------------------------------------
; CTRL+SHIFT+V = Paste as text-only
;
;--------------------------------------------------------------------------------------

; ==============================FUNCTIONS=======
; AWin("Full name of window in quotes like this")
; Mouse(XCoordinate, YCoordinate [Optional:, 1{LButton}/2{RButton}])
; Check("C:\Path\of\imagefile.bmp", TopCoorX, TopCoorY, BottomCoorX, BottomCoorY, [0/1{indicator}])
; WaitFor("C:\Path\of\imagefile.bmp", TopCoorX, TopCoorY, BottomCoorX, BottomCoorY, [0/1{indicator}])
; Bug("Message to flash in tooltip" [Optional:, XCoordinate, YCoordinate])
; ==============================================

; ========= EXCEL SHORTCUTS ==========
#IfWinActive, ahk_class XLMAIN

; Paste Values
; ---------------------------------------------------------------------------------
^+v::Send, {APPSKEY}sv

; Change books (based on mouse wheel)
; ---------------------------------------------------------------------------------
^!WheelUp::SendPlay, ^{TAB}
^!WheelDown::SendPlay, ^+{TAB}


; Format Painter
; ---------------------------------------------------------------------------------
MButton::
Send, !h
Sleep, 25
Send, fp
return

; Insert Row
; ---------------------------------------------------------------------------------
Shift & MButton::
Send, +{SPACE}
Sleep, 25
Send, {AppsKey}
Sleep, 25
send, i
return

; Insert Column
; ---------------------------------------------------------------------------------
ALT & MButton::
Send,^{SPACE}
Sleep, 35
Send, {AppsKey}
Sleep, 35
Send, i
return

; Edit (start of) current cell in Excel
; ---------------------------------------------------------------------------------
Insert::
MouseGetPos, mtempX, mtempY
Click, 265, 164
MouseMove, mtempX, mtempY
return

`::  ; Assumes you have the "split" option activated in the current sheet (View tab - centre screen). Checks left screen first
Check("S:\Institutional_Research\.Staff & Admin\.IR Analyst - Fraser\~Scripts\Images\ExcelFormulaAreaRef.bmp", 0, 129, 1280, 353)
If ErrorLevel = 0
{
	MouseGetPos, mtempX, mtempY
	FoundX -= 6
	FoundY -= 6
	Mouse(FoundX,FoundY,1)
	MouseMove, mtempX, mtempY
}
else
{
	Check("S:\Institutional_Research\.Staff & Admin\.IR Analyst - Fraser\~Scripts\Images\ExcelFormulaAreaRef.bmp", -1280, 129, 0, 353)
	If ErrorLevel = 0
	{
		MouseGetPos, mtempX, mtempY
		FoundX -= 6
		FoundY -= 6
		Mouse(FoundX,FoundY,1)
		MouseMove, mtempX, mtempY
	}
}
return

; ========= WORD SHORTCUTS ==========

#IfWinActive, - Word

; Find what's in the clipboard
; ---------------------------------------------------------------------------------
;^+f::
Send, ^f
Sleep, 75
Send, {END}{SHIFTDOWN}{HOME}{SHIFTUP}{DEL}{CTRLDOWN}v{CTRLUP}{ENTER}
return

; Paste as text
; ---------------------------------------------------------------------------------
^+v::Send, {APPSKEY}t{SPACE}
return

MButton::
{
Send, !h
Sleep, 25
Send, fp
}
return

; ========= OUTLOOK (Message) SHORTCUTS ==========

#IfWinActive, - Message (

; Paste as text - On Messages
; ---------------------------------------------------------------------------------
^+v::Send, !ovt
return

; Format Painter - On Messages
; ---------------------------------------------------------------------------------
MButton::
Send, !h
Sleep, 25
Send, fp
return

; Format Painter - On Tasks
; ---------------------------------------------------------------------------------
#IfWinActive, - Task
MButton::
Send, !o
Sleep, 25
Send, fp
return

; Paste as text - On Tasks
; ---------------------------------------------------------------------------------
^+v::Send, !ovt
return

; ========= SYSTEM-WIDE SHORTCUTS ==========
#IfWinActive

^+r::Reload
#g::Run, G:\

#s::Run, S:\Institutional_Research

#c::    ; This is stupidly complicated because Windows 10: https://www.autohotkey.com/boards/viewtopic.php?t=43997
SetTitleMatchMode, 3 ; Must match exactly
ifWinNotExist,  Calculator
{
	run, C:\Windows\system32\calc.exe
}
else
{
	WinGet, CalcIDs, List, Calculator
	If (CalcIDs = 1) ; Calc is NOT minimized
	{
		CalcID := CalcIDs1
	}
	else
	{
		CalcID := CalcIDs2 ; Calc is Minimized use 2nd ID
	}
	winActivate, ahk_id %CalcID%
}
SetTitleMatchMode, 2 ; Back to "contains"
return


Esc::
BreakLoop = 1
BreakLoop2 = 1
Escape += 1
if (Escape = 5)
{
	ExitApp
}
pause
return

; Change sheets in Excel, pages in Firefox/Chrome
; ---------------------------------------------------------------------------------
Shift & WheelUp::
IfWinActive, ahk_class XLMAIN
{
	SendPlay, ^{PGUP}
}
IfWinActive, Firefox
{
	SendPlay, ^{PGUP}
}
IfWinActive, Google Chrome
{
	SendPlay, ^{PGUP}
}
return
Shift & WheelDown::
IfWinActive, ahk_class XLMAIN
{
	SendPlay, ^{PGDN}
}
IfWinActive, Firefox
{
	SendPlay, ^{PGDN}
}
IfWinActive, Google Chrome
{
	SendPlay, ^{PGDN}
}
return


; Kill caps lock!
; ---------------------------------------------------------------------------------
~capslock::SetCapsLockState, off


; Copy current mouse position (screen) to clipboard
; ---------------------------------------------------------------------------------
NumLock::
MouseGetPos, mouseTempX, mouseTempY
Clipboard = %mouseTempX%, %mouseTempY%
return
+NumLock::
CoordMode, Mouse, Screen
MouseGetPos, mouseTempX, mouseTempY
CoordMode, Mouse
Clipboard = %mouseTempX%, %mouseTempY%
return

; Launch Window Spy hotkey
; ---------------------------------------------------------------------------------
^+/::
IfWinExist, Active Window Info
{
	WinActivate, Active Window Info
}
else
{
	Run, "S:\Institutional_Research\.Staff & Admin\.IR Analyst - Fraser\~Scripts\AU3_Spy.exe"
}
return

; This key sets the window under the mouse to "Always on top"
; ---------------------------------------------------------------------------------
#o::
MouseGetPos,,, MouseWin
WinSet, AlwaysOnTop, toggle, ahk_id %MouseWin%
return

; Launch OCAS
; ---------------------------------------------------------------------------------
^#o::Run, https://reporting.ocas.ca/ibmcognos/cgi-bin/cognosisapi.dll?b_action=xts.run&m=portal/main.xts&startwel=yes

; Launch SIS
; ---------------------------------------------------------------------------------
^#s::Run, https://sis.conestogac.on.ca/CampusNav/Login/Login.aspx

; Launch Google
; ---------------------------------------------------------------------------------
^#g::Run, "C:\Program Files (x86)\Mozilla Firefox\firefox.exe" -new-window "https://www.google.com"

; -----------------------------------------------------------------------------------------------------
; FUNCTIONS
; -----------------------------------------------------------------------------------------------------

; ---------------------------------------------------------------------------------
; CHECK() and WAITFOR() FUNCTIONS:
; ---------------------------------------------------------------------------------
; 
; Syntax:
;
; Check("C:\Path\of\imagefile.bmp", TopCoorX, TopCoorY, BottomCoorX, BottomCoorY, 0/1)
;
; These handy functions allow you to save a lot of coding time.
;
; The function returns you to your code with both the ErrorLevel of the ImageSearch
; and the FoundX and FoundY for the top-left corner of the image (if found).
;
; Check() only runs a single screen check. WaitFor() continuously checks for an image
; and only exits once it's found. *MAKE SURE TO SELECT THE CORRECT APPLICATION YOU'RE
; LOOKING AT THE IMAGE FROM BEFORE YOU CALL THESE FUNCTIONS!*
;
; Check2() checks for something twice, in an attempt to be twice as precise...
;
; The Rect() function below them both breifly superimposes a rectangle over the area
; that was just searched, to give a visual indication of the location. (This slows down
; the process considerably, use only for testing purposes.) You can toggle this behaviour
; by adding a 1 as a final option to the Check() function call.
;
; WaitFor() will loop endlessly, waiting until it finds what you're asking it to search
; for. {I'm thinking I should make WaitFor() end after a certain number of checks, and then
; display diagnostic info, and maybe make a WaitForever() function that goes forever as
; this one does, but... Meh.
;
; ---------------------------------------------------------------------------------
Check(path, xTop, yTop, xBot, yBot, rectangle=0, sensitivity=20)
{
	global FoundX, FoundY
	CoordMode Pixel
	ImageSearch, FoundX, FoundY, %xTop%, %yTop%, %xBot%, %yBot%, *%sensitivity% %path%
	if rectangle = 1
	{
		Rect(xTop, yTop, xBot, yBot)
	}
	return, ErrorLevel
}

Check2(path, xTop, yTop, xBot, yBot, rectangle=0, sensitivity=20)
{
	global FoundX, FoundY
	evidence = 0
	CoordMode Pixel
	if rectangle = 1
	{
		Rect(xTop, yTop, xBot, yBot)
	}
	ImageSearch, FoundX, FoundY, %xTop%, %yTop%, %xBot%, %yBot%, *%sensitivity% %path%
	if ErrorLevel = 0
	{
		evidence += 1
	}
	ImageSearch, FoundX, FoundY, %xTop%, %yTop%, %xBot%, %yBot%, *%sensitivity% %path%
	;Rect(xTop, yTop, xBot, yBot)
	if ErrorLevel = 0
	{
		evidence += 1
	}
	if evidence > 0
	{
		ErrorLevel = 0
	}
	else
	{
		ErrorLevel = 1
	}
	return, ErrorLevel
}

WaitFor(path, xTop, yTop, xBot, yBot, rectangle=0, sensitivity=20)
{
	if rectangle = 1
	{
		Rect(xTop, yTop, xBot, yBot)
	}
	global FoundX, FoundY
	Loop,
	{
		CoordMode Pixel
		ImageSearch, FoundX, FoundY, %xTop%, %yTop%, %xBot%, %yBot%, *%sensitivity% %path%
		if ErrorLevel = 1
		{
			Sleep, 25
		}
		else
			BreakLoop = 1
		if (BreakLoop = 1)
	break
	}
	BreakLoop = 0
	;Rect(xTop, yTop, xBot, yBot)
	return, ErrorLevel
}

Rect(x, y, x2, y2)
{
	CoordMode, Pixel, Screen
	Gui, +AlwaysOnTop -caption +Border +ToolWindow +LastFound
	WinSet, Transparent, 30
	Gui, Color, maroon
	w := abs(x - x2)
	h := abs(y - y2)
	Gui, Show, x%x% y%y% w%w% h%h%
	Sleep, 1000 ; Default 20-40
	Gui, Hide ; Careful to test: for some reason, certain values screw up mouse-clicks.
	Sleep, 40 ; Default 30-40
}


; ---------------------------------------------------------------------------------
; AWIN FUNCTION
; ---------------------------------------------------------------------------------
; 
; Syntax:
;
; AWin("Full name of window in quotes like this")
;
; AWin (aka "Activate Window") is part of a standard process of checking that the window a 
; script is looking for exists before allowing a script to continue. As many scripts can 
; cause some minor havoc when their desired windows are not available, the script serves as 
; a failsafe and gives a warning when a specified window is not available, preventing scripts
; from automatically continuing.
; ---------------------------------------------------------------------------------
AWin(wname)
{
	WinWait, %wname%,,1
	if ErrorLevel = 1
	{
		Loop,
		{
			IfWinNotExist, %wname%
			{
				msgbox, 1, Window Not Detected, A window that is required for the current script or shortcut has not been detected:`n`n "%wname%"`n`nPlease open this window and press OK to continue. If you cannot, or do not wish to do so, then press Cancel to exit the script/command.
				IfMsgBox, Cancel
				{
					reload
				}
			}
			IfWinExist, %wname%
			{
				break
			}
		}
	}
	IfWinNotActive, %wname%, , WinActivate, %wname%
	WinWaitActive, %wname%
}

; ---------------------------------------------------------------------------------
; MOUSE FUNCTION
; ---------------------------------------------------------------------------------
; 
; Syntax:
;
; Mouse(XCoordinate, YCoordinate [Optional:, 1/2])
;
; This function moves the mouse relative to the screen. The 1 or 2 in the final option denotes
; whether the mouse should click the Left or Right buttons (respectively) once it arrives.
; ---------------------------------------------------------------------------------
Mouse(x, y, Click=0)
{
	CoordMode, Mouse, Screen
	MouseMove, %x%, %y%
	if Click=1
	{
		Click
	}
	else if Click=2
	{
		Click right
	}
	CoordMode, Mouse
}


; ---------------------------------------------------------------------------------
; BUG FUNCTION
; ---------------------------------------------------------------------------------
; 
; Syntax:
;
; Bug("Message to Display in ToolTip" [Optional:XCoordinate, YCoordinate])
;
; This function simply flashes a tooltip wherever you may want it to. Defaults to mouse cursor
; and 1 second intervals. A function call with an empty message cancels the function.
; ---------------------------------------------------------------------------------
Bug(message="", xbug=-1, ybug=0)
{
	if message = ""
	{
		SetTimer, BugTip, off
		ToolTip,,
		ToolTip,,
		beingbugged = 0
	}
	else
	{
		if beingbugged = 1
		{
			SetTimer, BugTip, off
			ToolTip,,
			ToolTip,,
		}
		global messager, xbugger, ybugger, bugme
		messager = %message%
		xbugger = %xbug%
		ybugger = %ybug%
		SetTimer, BugTip, 1000
	}
}
return
BugTip:
beingbugged = 1
if flash = 1
{
	SetTimer, BugTip, 500
	ToolTip,,
	flash = 0
}
else
{
	SetTimer, BugTip, 1000
	if xbugger = -1
	{
		ToolTip, %messager%
	}
	else
	{
		CoordMode, ToolTip, Screen
		ToolTip, %messager%, xbugger, ybugger
		CoordMode, ToolTip, Window
	}
	flash = 1
}
return

; ---------------------------------------------------------------------------------
; FINDCLICK() FUNCTION
; ---------------------------------------------------------------------------------
; 
; Syntax:
;
; FindClick("C:\Path\of\imagefile.bmp", TopCoorX, TopCoorY, BottomCoorX, BottomCoorY, 0/1)
;
; Basically, FindClick() is a natural, practical extension of Check(). Instead of just giving
; an indication of whether something is on the screen, it looks for it and clicks in the 
; centre of the image. It uses the GDI Standard Library to check the size of the reference
; image to calibrate that click. The default settings are to check the entire screen, but you
; can specify a particular area if you want, ala the Check() syntax. The rectangle option 
; remains for troubleshooting. As with Check(), for best results, try to have the application
; you're wanting to click in selected.
;
; ---------------------------------------------------------------------------------
FindClick(path, xTop=0, yTop=0, xBot=-12345, yBot=-12345, rectangle=0, sensitivity=20)
{
	global FoundX, FoundY
	CoordMode Pixel
	if xBot = -12345 
	{
		SysGet, VirtualWidth, 78
		xBot = %VirtualWidth%
	}
	if yBot = -12345 
	{
		SysGet, VirtualHeight, 79
		yBot = %VirtualHeight%
	}
	; Find Image Dimensions with GDI Standard Library (https://autohotkey.com/board/topic/29449-gdi-standard-library-145-by-tic/)
	GDIPToken := Gdip_Startup()                                     
	pBM := Gdip_CreateBitmapFromFile( path )                 
	ImageWidth:= Gdip_GetImageWidth( pBM )
	ImageHeight:= Gdip_GetImageHeight( pBM )   
	Gdip_DisposeImage( pBM )                                          
	Gdip_Shutdown( GDIPToken )                                        

	ImageSearch, FoundX, FoundY, %xTop%, %yTop%, %xBot%, %yBot%, *%sensitivity% %path%
	if rectangle = 1
	{
		Rect(xTop, yTop, xBot, yBot)
	}
	If ErrorLevel = 0
	{
		ClickX := FoundX + (ImageWidth / 2)
		ClickY := FoundY + (ImageHeight / 2)
		CoordMode, Mouse, Screen
		MouseClick, left, %ClickX%, %ClickY%
		CoordMode, Mouse
	}
	return, ErrorLevel
}
