AutoHotkey
=======

[AutoHotkey](https://autohotkey.com/) is a useful tool for when you find yourself doing repetitive tasks in Windows. More speicifcally, AutoHotkey (.ahk) is a Windows scripting language that allows you to create shortcuts, hotkeys, and scripts for your computer. You can make repetitive tasks faster, get your computer loop commands, even have the computer generate files, display information, and look for cues on the screen. It is a very handy automation tool for people who do our sort of all-over-the-place work.  Think of it as "macros for Windows". Plus, if you pick it up, you'll learn some basic computer programming skills. Look at you, professionally developing!

## What's here
Code is separated into two types, which may overlap at times. They are all meant to be helpful exhibits or examples of what people are using or what AutoHotkey can do.
* **Shortcuts** are where we put fun hotkeys, subroutines, and time-saving measures that are aimed at accomplishing a small and/or useful task.
* **Functions** are where we put code building blocks for people to use, to save time inside their own scripts.

### How to Download and use a script
1. Click on the script you want.
2. Right-click the "Raw" button on the top of the file's text box interface.
3. Save the script somewhere you'll remember.
4. If you have AutoHotkey installed, you can just double-click the script to run / load it. Or you can edit the .AHK file in any text editor.

## Introduction for Beginners: Starting Off
Once you install AutoHotkey, the program operates using scripts, which are just text files with code, typically with an .AHK extension. Remember you can edit .AHK files in any text editor, like notepad. Once you create an AHK script, you can run/load it as if it was a program by double-clicking it, or running it with AutoHotkey. Scripts can be closed by finding the AutoHotkey tray icon for the script and closing it, or going to the Task Manager and closing the AutoHotkey process.

Starting off, you'll probably want to look at some examples, as well as the [help / documentation](https://www.autohotkey.com/docs/AutoHotkey.htm) (which is decently well done), with an index of syntax commands and examples. We've included some very basic examples below.

This tutorial is a great place to start: [https://autohotkey.com/docs/Tutorial.htm](https://autohotkey.com/docs/Tutorial.htm). Like Excel, AutoHotkey benefits from a fairly decent online community of coders who have tried many strange things and posted on the [forums](https://autohotkey.com/boards/).

When I was starting off, I found a Script Recorder to be quite helpful. (This has been discontinued in the basic install, but the following seems like a good modern attempt of it: https://autohotkey.com/boards/viewtopic.php?f=6&t=143) The recorder is mainly helpful for both getting an idea of what actions equate to in terms of the actual scripting language, and in addition, it is useful if you don't really want to do any coding at all - just record yourself doing something, then play it back, or loop it, if you so choose, over and over again. Handy dandy. 

As a jump-start example, here's a script of my top-3 favourite shortcuts. You can [download and give them a look / try here](https://www.dropbox.com/s/wdnjjg313pjneff/Handy%20Shortcuts.ahk?raw=1). 

## A few examples of what you can do...

1. Make your computer talk to you
~~~~
ComObjCreate("SAPI.SpVoice").Speak("I'm sorry Dave, I'm afraid I can't do that.")
~~~~

2. Run Notepad using WIN+N
~~~~
#n::run, C:\Program Files (x86)\Notepad++\notepad++.exe
~~~~

3. Run the Windows Snipper Tool with CTRL+ALT+S
~~~~
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
~~~~

4. Open Facebook... For work purposes...
~~~~
^+f::
IfWinExist, Facebook
{
	WinActivate, Facebook
}
else
{
	run, C:\Program Files\Mozilla Firefox\firefox.exe -new-window "www.facebook.com"
}
return
~~~~

5. Spit out random ASCII characters, CTRL+SHIFT+\
~~~~
^+\::
Random, charvar, 33, 255
Send % Chr(charvar)
return
~~~~

6. [Auto-complete](https://www.autohotkey.com/docs/Hotstrings.htm) your email address upon a cue set of keystrokes (eg. here: @con, @IR)
~~~~
:*?:@con::@conestogac.on.ca
:*?c:@IR::institutionalresearch@conestogac.on.ca
~~~~


For more examples, go to [https://www.autohotkey.com/docs/Tutorial.htm](https://www.autohotkey.com/docs/Tutorial.htm). You can also check out the [script showcase](https://www.autohotkey.com/docs/scripts/).
