Stata<br>		
<img src="https://www.stata.com/includes/images/stata-logo-blue.svg" alt="Stata Logo" title="Stata" width="250"/>
=======

Stata is a general-purpose statistical software package. Its users span across many fields, and it provides all of the key features for statistical analysis and programmatic data science, with some additions not often found elsewhere. As in some other packages (like R), it is primarily a command-line interface, and also has a system to disseminate user-written packages and subroutines that enables its features to continuously grow. Although it is not free software, licensing is incredibly affordable (compared to SPSS).

**Be a trend-setter!** - Remember to check out the [Visualization Gallery](https://github.com/Sopwith/IR/tree/master/Visualization%20Gallery) and submit your own creations for others to see and copycat.

## What's here?
* **Syntax** - Until we have a need to start sorting code into categories, it can all go into this folder! Upload anything you are using or find useful.
  * Useful scripts
  * Interesting discoveries
  * Your favourite use cases / output

**Please ensure all submissions are adequately commented, both with an introduction section at the top, and in-line whenever useful.**<br>
*Example:*
```
* Fraser Hay, Conestoga College (Kitchener, Ontario) - fhay@conestogac.on.ca.
* Side-by-side histogram distributions of a score, for various groups. In this case, teaching scores by FT / PT faculty.

insheet using "S:\Institutional_Research\SAT Summary Dataset.csv", comma clear  * Import CSV dataset.
replace teacherfacultytype = "1" if teacherfacultytype == "FT"   * Some data cleaning.
replace teacherfacultytype = "0" if teacherfacultytype == "PT"
destring  teacherfacultytype, replace   * Make the variable an integer type. Not sure why.
sort teacherfacultytype   * Sort data by variable - required for "by" commands.

by teacherfacultytype: hist teach_avg, freq norm w(1)    
* For every faculty type, make a histogram, with the scale being the frequency of score. Also insert a normal curve, and make the bin width 1 (bar for every score value, 0-5).
```

* **Useful links** that we've stumbled across, providing useful information, or showcasing something useful or interesting.

## How to Download a file
You can easily download or work with files by creating your own clone or fork of parts of the CIRPA repository. But if you just want to download a single file:
1. Click on the file/script you want.
2. Right-click the "Raw" button on the top of the file's text box interface.

![Raw button](https://www.dropbox.com/s/fyt1qz0qeqjn0vf/GitHub-RawButton.png?raw=1)

3. Click "Save link as..." (or equivalent in your browser) to save the file/script and use it.

## Helpful Links
* UCLA - Institute for Digital Research & Education - Stata FAQ: How to do basically anything in Stata, with examples and instructions. https://stats.idre.ucla.edu/stata/faq/
