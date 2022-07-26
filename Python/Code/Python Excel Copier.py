# The Python Excel Copier
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fraser Hay, Farhad Kerimov - July, 2022

# This script is meant to facilitate automated formatting and copying between templates and production reports using R or Python.
# Excel reports can be made pretty much faster using Excel than using code. So the reasoning here was: you make an Excel sheet you
# want to share, you dress it up, and then you can use a script to isolate pieces of formatting - or even entire worksheets - and
# copy them into future worksheets, without having to do it yourself.

# Specify a source file, sheet, and range, as well as a destination set of those, and you can copy content and/or formatting at will,
# as well as specify whether you want to see the finished product or simply save it somewhere.

# The win32com method is powerful - since it basically mimics a user - but not incredibly fast. It still has to load the full Excel
# instance in some way to function. If I was doing a lot of copying with this, I might take out the closing of the source template
# file with each run. [ source_wb.Close(False) ]
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Package dependencies: win32com
#
# Arguments:
#   s_file - Source file (full / relative path)
#   s_sheet - Source sheet (within the source file)
#   s_range - Source range (within the source sheet). Can be a single cell ('A1'), range ('A1:B2'), or asterisk ('*') if you want 
#             the entire sheet copied.
#   d_file - Destination file (full / relative path)
#   d_sheet - Destination sheet (within the source file)
#   d_range - Destination range (within the source sheet). Can be a single cell ('A1'), range ('A1:B2'), or asterisk ('*') if you want 
#             the entire sheet pasted. NOTE: You should only use the asterisk if using an asterisk in the source range. Otherwise
#             you'll be pasting a value or format over a couple billion cells, and that will likely error out.
#   formatting = False; Do you want to only copy the formatting? If so, set to True.
#   show = True; Do you want to see the finished product? If not, set to False.
#   output = None; Where to save the finished product. Specify a full file path for best results.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

from win32com import client

def pyxl_copy(s_file, s_sheet, s_range, d_file, d_sheet, d_range, formatting = False, show = True, output = None):
  
  import sys # Used to facilitate script exits later on (sys.exit)
  
  def errorOut(message):
    raise Exception(message)
    excel_app.Workbooks.Close()
    excel_app.Quit()
    sys.exit(1)
  
  excel_app = client.gencache.EnsureDispatch("Excel.Application") # Initialize instance
  excel_app.Visible = False
  excel_app.DisplayAlerts = False # Turn off Excel dialogs
  
  try:
    source_wb = excel_app.Workbooks.Open(s_file) # Load source file
  except:
    errorOut("Source file not currently accessible. It may be open (even in the background).")
  try:
    source_ws = source_wb.Worksheets(s_sheet) # Select source sheet
  except:
    errorOut("Source sheet reference is not correct / accessible. Use sheet name (string) or integer index (1).")
  try:
    destination_wb = excel_app.Workbooks.Open(d_file) # Load destination file
  except:
    errorOut("Destination file (" + d_file + ") not currently accessible. It may be open (even in the background).")
  try:
    destination_ws = destination_wb.Worksheets(d_sheet) # Select destination sheet
  except:
    errorOut("Destination sheet reference is not correct / accessible. Use sheet name (string) or integer index (1).")
  
  if s_range == "*" or d_range == "*":
    print("py_xl_copy: NOTE - Copying entire sheets ('*') currently requires *destroying* filters. Please manually re-add if needed.")

  try:
    if s_range == "*":
      source_ws.AutoFilterMode = False # Clear filters - Cells can't handle those.
      source_ws.Cells.Copy()
    else:
      source_ws.Range(s_range).Copy() # Grab source content
    if d_range == "*":
      destination_ws.AutoFilterMode = False
      if not formatting:
        destination_ws.Paste(destination_ws.Cells) # Paste in destination
      else:
        destination_ws.AutoFilterMode = False # Have to actually disable filters in this case...
        destination_ws.Cells.PasteSpecial(Paste=client.constants.xlPasteFormats)
    else:
      if not formatting:
        destination_ws.Paste(destination_ws.Range(d_range)) # Paste in destination
      else:
        destination_ws.Range(d_range).PasteSpecial(Paste=client.constants.xlPasteFormats)
    source_wb.Close(False)
    if not output == None:
      try:
        destination_wb.SaveAs(output)
      except:
        errorOut("Cannot save file - problem with supplied filename.")
    if show:
      excel_app.DisplayAlerts = True
      excel_app.Visible = True
    else:
      destination_wb.Close(False)
      excel_app.Quit()
    print("py_xl_copy: Operation completed.")
  except:
      errorOut("Unable to complete operation - source or destination cell references / ranges are likely incorrect. Use either a cell ('A1'), range ('A1:B2') or asterisk ('*') to specify. Additionally, you should only copy asterisks to other asterisks.")
