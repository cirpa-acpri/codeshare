; Fraser Hay, Conestoga College (Kitchener, Ontario, Canada)
; fhay@conestogac.on.ca
;
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