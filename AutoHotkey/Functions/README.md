## Functions
This is where we store useful functions and subroutines in AutoHotkey. 

Typically this means, if you download these scripts and run them, nothing will happen. They typically accomplish a coding task, and/or are meant to be simply copied for inspiration, or 'Included' in your script for you to call at will. You can do so by:

~~~~
#Include <script>
~~~~

If you haven't specified a working directory in your own script, you may want to do so by using the following code:
~~~~
; Sets the working directory as below...
SetWorkingDir C:\Scripts
~~~~
... Or...
~~~~
; Sets the working directory to the same one where the script is located...
SetWorkingDir %A_ScriptDir%
~~~~
Otherwise, you'll need to link to the full file path in your _Include_ statement.
