; Fraser Hay, Conestoga College (Kitchener, Ontario, Canada)
; fhay@conestogac.on.ca
;
; This script is *supposed* to launch calculator when you press WIN+C. In Windows 7, it doesn't have to be this 
; complicated: all you have to do is check for the program being around, and if not, launch it. But Windows 10 
; makes launching the calculator stupid. See: https://www.autohotkey.com/boards/viewtopic.php?t=43997. This is
; the best solution I've found so far. Sometimes it doesn't work, for reasons I can't explain. If you hit the 
; hotkey and nothing happens, then just try launching it (calculator) manually. It should work after that.

#c::
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
