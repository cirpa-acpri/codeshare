
ComObjCreate("SAPI.SpVoice").Speak("Fraser")

; Run Notepad
#n::run, C:\Program Files (x86)\Notepad++\notepad++.exe

; Run Snipper Tool
^!s::
IfWinExist, Snipping Tool
{
	WinActivate, Snipping Tool
	Send, ^n
}
else
{
	run, C:\Windows\system32\SnippingTool.exe
	WinWait, Snipping Tool
	Send, ^n
}
return

^+f:: ; Facebook
IfWinExist, Facebook
{
	WinActivate, Facebook
	WinMove, Facebook,,1406,491,519,533
}
else
{
	run, C:\Program Files\Mozilla Firefox\firefox.exe -new-window "www.facebook.com"
	WinWait, Facebook
	WinMove, Facebook,,1406,491,519,533
}
return

^+\:: ; Random Character Generator
Random, charvar, 33, 255
Send % Chr(charvar)
return

; Some auto-completes...
:*?:@con::@conestogac.on.ca
:*?c:@IR::institutionalresearch@conestogac.on.ca^+m::run, "G:\My Music\Playlists\The Classics (2019).m3u8"
#f::run, "C:\Program Files\Everything\Everything.exe"