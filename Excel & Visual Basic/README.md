# ![Excel](https://www.dropbox.com/s/b6v5vj2c3gw3pmx/Excel.png?raw=1)

Excel is a spreadsheet software developed by Microsoft. It features calculation, graphing tools, pivot tables, and a macro programming language called "Visual Basic for Applications". It largely remains the industry standard for spreadsheets, obviously one of the most common ways of working with data, tables and graphs in many small IR offices. If your data is "small" (under 1M rows and ~16k columns), Excel is usually a decent way to interface with it and distribute it to colleagues.

Excel has a variety of intermediate and advanced-level features that are often overlooked, and can be incredibly useful to IR teams in their ad-hoc, stop-gap, and even main-line work.

**Be a trend-setter!** - Remember to check out the [Visualization Gallery](/Visualization%20Gallery) and submit your own creations for others to see and copycat.

[See our Guide](/Guide.md#how-to-contribute-your-own-creations) for tips on uploading your own submissions / content. 

## What's here
We currently have the following categories of things on-offer here:
* **Examples** are where we show and tell: useful or interesting things we've made; demonstrations of concepts; and examples of how we use it when Excel really does the job. These are all self-contained workbooks.
* **VBA Code** is where we put code snippets for people to use. VBA (Visual Basic for Applications) really extends the power of Excel to a whole 'nother level, so if you aren't using it, chances are you're missing out (and/or could save yourself some serious time).

## How to Download a file
You can easily download or work with files by creating your own clone or fork of parts of the CIRPA repository. But if you just want to download a single file:
1. Click on the file/script you want.
2. Right-click the "Raw" button on the top of the file's text box interface.

![Raw button](https://www.dropbox.com/s/fyt1qz0qeqjn0vf/GitHub-RawButton.png?raw=1)

3. Click "Save link as..." (or equivalent in your browser) to save the file/script and use it.

## Important Concepts
These are things you should be aware of that Excel can do; and if don't know how to do them, should probably learn!
* Pivot Tables: take a large dataset and summarize it in any way you want. Sums, averages, and totals. Add a filter.
* Slicers: point-and-click interactive filters for pivot-tables that are very easy to set-up.
* Pivot Charts: hook up your pivot tables to charts, and watch them change on command.
* Connecting to databases: Excel can import tables and views from SQL and many other places
* Hosting Excel documents on SharePoint: You can embed Excel documents and parts onto SharePoint for people to interact with.
* VBA Macros: Create yourself some shortcuts and speed up your routine work.
* VBA Macros - Reporting Loops: Have a reporting template but need a document exported for hundreds of different unique cases / entites? Use VBA to run through the entire list in a few minutes, spitting out a correctly formatted report for each.

## Learning Excel - From Beginner to Advanced
Links to resources as submitted by members:
* A basic overview of VLOOKUP, one of the more useful commands: [https://support.office.com/en-us/article/vlookup-function-0bbc8083-26fe-4963-8ab8-93a18ad188a1](https://support.office.com/en-us/article/vlookup-function-0bbc8083-26fe-4963-8ab8-93a18ad188a1)
* Example of INDEX and MATCH: [https://www.deskbright.com/excel/using-index-match/](https://www.deskbright.com/excel/using-index-match/)

## Tips and Tricks
Send us your formulas or small life-hacks and we'll post them here.
* *Determining the age difference (in years) between two points in time.* 
  Assumes A1 is a functional date value. If it's not, you may have to try converting it, possibly by encapsulating with DATEVALUE(). The DATE() term is supposed to be the *later* date you're gauging against. "y" means return number of years.
  
  =DATEDIF(A1,DATE(2019,10,21),"y")
* You can use Excel to "fill down" values with big, empty tables - such as those from mulit-level SPSS cross-tabs, or with cube / report outputs from some COGNOS systems. Just get your big merged-cell table, unmerge those cells (if applicable), then select all the table's row labels / levels (not the values) > F5 (Go to) > Special > Blanks > OK, and then click on the formula bar, and make the selected cell "={cell above it}", then CTRL+ENTER (fill contents to selection). Paste everything as values, and bam, you have a table full of repeated-lables. Great for pivot-tables. <br>
       <img src="https://www.dropbox.com/s/6gjhzvqnr1yflab/ExcelFillDownExample.jpg?raw=1" alt="Excel Fill Down Example" title="Excel Fill Down Example" width="750"/>
