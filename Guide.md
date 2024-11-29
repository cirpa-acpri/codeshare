# A Simple Guide to This Repository <img src="https://is4-ssl.mzstatic.com/image/thumb/Purple128/v4/c1/37/2a/c1372a8b-7779-cc88-917f-17db385bdae2/source/512x512bb.jpg" alt="CIRPA Logo" title="CIRPA" width="100"/>
You're currently reading a file within a repository of files, sorted into folders. We're using **[GitHub](https://github.com/)**, which is a platform for storing and developing code in a collaborative way.

It's collaborative in a few ways.
1. **You can download and use any code that you can access on GitHub**. In addition to grabbing individual files, you can make your own copy of entire project file systems and use them as you see fit. This is called a "Fork". 
2. **You can message the authour(s) of the code, and ask them questions.** Most code has the authour's contact info included. In addition, each file has version history that explains how it has changed over time (if appliable) and who made those changes. You can also use the "[Issues](https://guides.github.com/features/issues/)" feature at the top of each page to ask a question or raise concerns. Finally, remember you can always post a message on the [CIRPA Member365 discussion forums](https://cirpa.member365.com/sharingnetwork/discussion/viewAllDiscussions/).
3. **You can submit your own code or revisions to be posted here.** You are able to edit a copy of the code and submit your version to the authuor(s) for them to consider merging into the original (Master) copy. This merging action is called a "Pull request": where you request that your "fork" (copy of the original) be "pulled" (integrated) into the main repository.

## How to Find and Download Code
Click around into the various folders. Different folders contain different types of files and code based on the platform being referenced. If you're interested in a file, click on it. If that file happens to have text-readable code (eg. R, Python, AutoHotkey scripts, etc.), the file will preview on the screen.

To download a single file:
1. Right-click on the file/script you want.
2. Click "Save link as..." (or equivalent in your browser) to save the file/script and use it.

## How to Make a Local Copy of this Repository
If you go back one page, to where all the folders are listed, you'll notice a big green button on the top-right, labelled "Clone or download". You can use this to download a full copy of the current file repository - either by downloading a ZIP, or opening it in GitHub's desktop client (install required).

## How to Contribute your Own Creations
Have something you'd like to feature here or upload? That's awesome. Let's get it up where people can see it. 

**First**, ensure your code is properly referenced and commented. We recommend you have some sort of header comment space that notes who you are, and how to contact you, as well as a description of what you code or contribution accomplishes. In-line comments and commentary are also encouraged - help people understand what's going on! Also please ensure your code identifies somewhere any required packages or dependencies, if applicable.

### *Examples:*
```
# Fraser Hay, Conestoga College (Kitchener, Ontario) - fhay{at}conestogac.on.ca
# R/Python: This code calculates and displays the sum of 2+2.

a = 2+2
print(a) # Display output
```
```
* Fraser Hay, Conestoga College (Kitchener, Ontario) - fhay{at}conestogac.on.ca
* SPSS: Demonstration of AUTORECODE, which is useful when you have a variable of just captions, 
* but want to make it into a coded variable.

AUTORECODE varible			* Source
	/INTO recoded_variable 		* Output
	/BLANK MISSING			* Optional: Set blanks to missing.
	/PRINT.				* Optional: Show a coding scheme table.
```
```
* Fraser Hay, Conestoga College (Kitchener, Ontario) - fhay{at}conestogac.on.ca.
* Stata: Side-by-side histogram distributions of a score, for various groups. In this case, teaching scores by FT / PT faculty.

insheet using "S:\Institutional_Research\SAT Summary Dataset.csv", comma clear  * Import CSV dataset.
replace teacherfacultytype = "1" if teacherfacultytype == "FT"   * Some data cleaning.
replace teacherfacultytype = "0" if teacherfacultytype == "PT"
destring  teacherfacultytype, replace   * Make the variable an integer type. Not sure why.
sort teacherfacultytype   * Sort data by variable - required for "by" commands.

by teacherfacultytype: hist teach_avg, freq norm w(1)    
* For every faculty type, make a histogram, with the scale being the frequency of score. Also insert a normal curve,
* and make the bin width 1 (bar for every score value, 0-5). The histograms generated this way will be shown in a grid.
```

**Second**, send it over to us! You have two options to do this:
1. **Keep it simple**: Send your creations to [Stephen Childs](mailto:Stephen.Childs}{at}humber.ca) (Humber College) or [Fraser Hay](mailto:fhay{at}conestogac.on.ca) (Conestoga College). We'll get your creations up here as promptly as we can.
2. **Fork it**: [Create a fork](https://help.github.com/en/articles/fork-a-repo) (or branch) of this repository and add in your new code to the appropriate place, then create a [pull request](https://help.github.com/en/articles/about-pull-requests) (with a good description of your additions / changes) to merge them into the master. We'll review it and work with you to get things up and running.

****Please note*** *that all materials that are posted to this repository are considered considered under the [MIT License](https://github.com/Sopwith/IR/blob/master/LICENSE.md). Please consider this fact, and your ownership of the code, before uploading or sharing any new code.*

## GitHub Tutorials
For more information about GitHub and related goodness, start here:

https://guides.github.com/activities/hello-world/

https://help.github.com/en/articles/fork-a-repo

https://help.github.com/en/articles/about-branches

https://help.github.com/en/articles/about-pull-requests
