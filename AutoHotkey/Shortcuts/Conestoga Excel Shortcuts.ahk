; Fraser Hay, Conestoga College (Kitchener, Ontario, Canada)
; fhay@conestogac.on.ca
;
; Just a selection of my favourite and most-used Excel shortcuts.
;
; Note that you can make the Format painter and Paste values shortcuts
; work just about in any Office program, which is what I do. You just 
; need to use WindowSpy or something to figure out what window class or 
; title is the one you're working with.
;
; ========= EXCEL SHORTCUTS ==========
#IfWinActive, ahk_class XLMAIN

; Paste Values: CTRL+SHIFT+V
; ---------------------------------------------------------------------------------
^+v::Send, {APPSKEY}sv

; Change books (based on mouse wheel): CTRL+ALT+MouseWheel
; ---------------------------------------------------------------------------------
^!WheelUp::SendPlay, ^{TAB}
^!WheelDown::SendPlay, ^+{TAB}

; Format Painter: Middle Mouse Button (for me, it's click the wheel)
; ---------------------------------------------------------------------------------
MButton::
Send, !h
Sleep, 25
Send, fp
return

; Insert Row: SHIFT+Middle Mouse
; ---------------------------------------------------------------------------------
Shift & MButton::
Send, +{SPACE}
Sleep, 25
Send, {AppsKey}
Sleep, 25
send, i
return

; Insert Column: ALT+Middle Mouse
; ---------------------------------------------------------------------------------
ALT & MButton::
Send,^{SPACE}
Sleep, 35
Send, {AppsKey}
Sleep, 35
Send, i
return

; Edit (start of) current cell in Excel: INSERT
; ---------------------------------------------------------------------------------
Insert::
MouseGetPos, mtempX, mtempY
Click, 264, 191 ; You may need to update this x,y reference on different versions of Excel
MouseMove, mtempX, mtempY
return
