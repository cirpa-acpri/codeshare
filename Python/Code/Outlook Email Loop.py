# Fraser Hay - Conestoga College, Kitchener Ontario - fhay@conestogac.on.ca
# -----------------------------------------------------------------------------
# Short title: Python Coded Mail-merge Loop
# -----------------------------------------------------------------------------
# Longer description:
# As occasionally happens, I found myself creating a report for some teaching evaluation activity, and needing to send
# each Chair their report that I had made for them via Outlook email attachment. Typically this would involve opening
# 35-odd emails and crafting each by hand. I had looked into making a mail-merge with attachments, never had any luck.
# I didn't want to install anyone's add-ons. 

# Thought I'd Google it again, and stumbled across a fascinating youtube series: https://www.youtube.com/watch?v=b7_bW45G_1o
# Copied the instructions, and by golly, it worked.

# In this example, I basically just was going from my folder of reports, which happened to have Chair names in the file names.
# I made a list of the reports (file names), harvested the Chair names, and used them as the addressees (Outlook would check
# the names and look up the email addresses) and attached the appropriate file. I ended up with all of those 35 shells I would
# have made by hand within 10 seconds, after clicking "run".

# The only thing to keep in mind is you need to get your message into HTML format. There are some websites that will let
# you copy-paste your rich text and get HTML out of it. (eg. https://wordtohtml.net/)

# Full credit:  Izzy Analytics - https://www.youtube.com/watch?v=b7_bW45G_1o
# -----------------------------------------------------------------------------

import glob # Relevant module.

# Email setup
import win32com.client as client 
import pathlib
outlook = client.Dispatch('Outlook.Application')

# File list matching pattern.
files = glob.glob(r"C:\OneDrive\Conestoga College\Institutional Research - File Resources - Shared Resources\SATs & Blue\17 ~Fall 2020\Quick-SAT\Report Drafts\Fall 2020*.*")

# Extracting person names from that file list.
# Exmaple file name: "Fall 2020 Quick-SAT Summary Report - John Smith.xlsx"
names = [] # Full names
for i in files:
    temp = i.split("Report - ",1)[1]
    end = temp.find(".xlsx", 1)
    names.append(temp[0:end])
firstnames = [] # First names
for i in names:
    end = i.find(" ", 0)
    firstnames.append(i[0:end])

# Make an email shell for each person
for count, person in enumerate(names):
    message = outlook.CreateItem(0) # Create email instance
    message.To = person # Addressee
    message.Subject = "Fall 2020 Quick-SAT Summary Chair’s Report" # Subject line
    # Message body (""" = fenced quotation)
    message.HTMLBody = """
    <p style='margin: 0in; line-height: 1;'>Hello """ + firstnames[count] + """,</p>
    <p style='margin: 0in; line-height: 1;'>&nbsp;</p>
    <p style='margin: 0in; line-height: 1;'>Please find attached a summary Excel document that provides an at-a-glance reference of the Fall 2020 Quick-SATs for faculty that are linked to you in SIS. Interpretation guidelines are included on the title page. This report is for your own use - faculty have already recieved their individual reports.</p>
    <p style='margin: 0in; line-height: 1;'><br></p>
    <p style='margin: 0in; line-height: 1;'>The Quick-SAT was run during week 5 of the semester. &nbsp;For 14-week courses that started the week of Sep 8<sup>th</sup>, the Quick-SAT ran <span style="background-position: 0% 0%; background-repeat: repeat; background-attachment: scroll; background-image: none; background-size: auto; background-origin: padding-box; background-clip: border-box;">Oct 5-12</span>. For 14-week courses starting the week of Sep 28<sup>th</sup>, the Quick-SAT <span style="background-position: 0% 0%; background-repeat: repeat; background-attachment: scroll; background-image: none; background-size: auto; background-origin: padding-box; background-clip: border-box;">ran Oct 21-28</span>. The attached report includes the results for all Quick-SATs completed by Oct 28, 2020.</p>
    <p style='margin: 0in; line-height: 1;'><br></p>
    <p style='margin: 0in; line-height: 1;'>If you have any questions or concerns, please let me know.</p>
    <p style='margin: 0in; line-height: 1;'>&nbsp;</p>
    <p style='margin: 0in; line-height: 1;'>Best,</p>
    <p style='margin: 0in; line-height: 1;'><br></p>
    <p style='margin: 0in; line-height: 1;'><strong><span style='font-family:"Tahoma",sans-serif;color:black;'>Fraser Hay</span></strong></p>
    <p style='margin: 0in; line-height: 1;'><em><span style='font-size:13px;font-family:"Segoe UI",sans-serif;color:#333333;'>Analyst, Office of Institutional Research and Planning<br></span></em><span style='font-size:12px;font-family:"Segoe UI",sans-serif;color:#7F7F7F;'>Conestoga College Institute of Technology &amp; Advanced Learning</span></p>
    <p style='margin: 0in; line-height: 1;'><span style='font-size:11px;font-family:"Segoe UI",sans-serif;color:#7F7F7F;'>Welcome Centre 270-4, Doon Campus &nbsp;</span><span style='font-size:9px;font-family:"Segoe UI",sans-serif;color:#BFBFBF;'>| &nbsp;299 Doon Valley Drive, Kitchener ON, N2G 4M4</span></p>
    <p style='margin: 0in; line-height: 1;'><a href="mailto:fhay@conestogac.on.ca" target="_blank"><span style='font-size:13px;font-family:"Segoe UI",sans-serif;color:blue;'>fhay@conestogac.on.ca</span></a></p>
    """
    message.Attachments.Add(files[count]) # Add attachment
    message.Display() # Show the shell for review (and sending)
