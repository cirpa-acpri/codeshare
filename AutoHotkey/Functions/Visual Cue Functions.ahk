; Fraser Hay, Conestoga College (Kitchener, Ontario, Canada)
; fhay@conestogac.on.ca
;
; Functions to assist in finding, checking, waiting for and clicking on visual cues.
; These are essential when I'm trying to automate pulling information from a website on
; an automated basis - since each page can load differently (faster/slower). Heck, even
; Office programs can slow down after awhile. Making automated loops that wait for cues
; on the screen are a best practice. These functions make that much easier.
;
;
; NOTE: Certain functions below rely on Gdip.ahk. If you want to use it, you'll need to have
; it in the same folder as the script. Otherwise, Rect() and FindClick() will be broken and
; need to be removed.

SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.
#Include Gdip.ahk

; I highly recommend the following as a helper when making visual cue loops:
;
; Copy current mouse position (screen) to clipboard
; ---------------------------------------------------------------------------------
;NumLock::
;MouseGetPos, mouseTempX, mouseTempY
;Clipboard = %mouseTempX%, %mouseTempY%
;return
;
;+NumLock::
;CoordMode, Mouse, Screen
;MouseGetPos, mouseTempX, mouseTempY
;CoordMode, Mouse
;Clipboard = %mouseTempX%, %mouseTempY%
;return

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
	Sleep, 700 ; Default 20-40
	Gui, Hide ; Careful to test: for some reason, certain values screw up mouse-clicks.
	Sleep, 40 ; Default 30-40
}

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
; REQUIRES GDip standard library. (Gdip.ahk)
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