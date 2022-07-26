# The Python Excel Dataframe Paster
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fraser Hay, Farhad Kerimov - July, 2022
# (Multiple days of man-hours were wasted trying to figure this out, but now it's done, so you're welcome.)

# This script allows you to paste (as text) a dataframe into an Excel file without messing up any of that Excel file's source
# formatting.

# Specify a source file, sheet, and cell reference, as well as a destination set of those, and you can copy content and/or formatting at will,
# as well as specify whether you want to see the finished product or simply save it somewhere.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Package dependencies: win32com, pandas
#
# Arguments:
#   df - The dataframe you wish to paste
#   file - Source file (full / relative path)
#   sheet = 1; Source sheet (within the source file). Can be the string name ('Sheet1') or an integer index (1)
#   cell = "A1"; Source cell reference (within the source sheet). Must be a single cell ('A1').
#             the entire sheet copied.
#   headers = True; Set to 'False' if you do not want headers copy-pasted from your df.
#   show = True; Do you want to see the finished product? If not, set to False.
#   output = None; Where to save the finished product. Specify a full file path for best results.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

from win32com import client
import pandas as pd

def pyxl_df_paste(df, file, sheet = 1, cell = "A1", headers = True, show = True, output = None):
  
   import sys # Used to facilitate script exits later on (sys.exit)
    
   def errorOut(message):
       raise Exception(message)
       excel_app.Workbooks.Close()
       excel_app.Quit()
       sys.exit(1)
   
   try:
     df.to_clipboard(index=False, header=headers) # Copy the supplied dataframe to the clipboard
   except:
     errorOut("Unable to load data into clipboard. Function is expecting a simple flat dataframe as source data.")
   
   # Let's launch Excel...
   excel_app = client.gencache.EnsureDispatch("Excel.Application") # Initialize instance
   excel_app.Visible = False
   excel_app.DisplayAlerts = False # Turn off Excel dialogs
   try:
     wb = excel_app.Workbooks.Open(file) # Load source file
   except:
     errorOut("Source file not currently accessible. It may be open (even in the background).")
   try:
     ws = wb.Worksheets(sheet) # Select source sheet 
   except:
     errorOut("Source sheet reference is not correct / accessible. Use sheet name (string) or integer index (1).")
   try:
     ws.Range(cell).Select() # Select source range
   except:
     errorOut("Source cell reference incompatible. Refer to a single cell with a string using Excel terminology ('A1')")
   try:
     ws.PasteSpecial(Format='Unicode Text') # Paste as text (values)
   except:
     errorOut("Paste operation was unsuccessful. Function is expecting a simple flat dataframe as source data.")
   ws.Range(cell).Select() # Just replacing the selected cell back to where it started, so that it doesn't look like you just pasted it in (with the pasted dataframe selected in its entirety).
   
   if not output == None:
     try:
       wb.SaveAs(output)
     except:
       errorOut("Cannot save file - problem with supplied filename.")
   if show:
     excel_app.DisplayAlerts = True
     excel_app.Visible = True
   else:
     wb.Close(False)
     excel_app.Quit()
   print("py_xl_paste: Operation completed.")
