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
;
; Note: This function sets the "TitleMatchMode" to 2, meaning it will wait for any window
; that includes the passed text anywhere. So if you have multiple windows with "Excel" in the
; title, and you AWin("Excel"), it will be satisfied by *any* of those titled windows. You'll 
; have to give it more specific terms if you want it to be more specific.
;
; Note also: This function *reloads the script* if a user hits "cancel". That was my attempt
; at stopping whatever operation was happening in AutoHotkey from happening, but your case
; may call for something different.
; ---------------------------------------------------------------------------------

AWin(wname)
{
	SetTitleMatchMode, 2
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