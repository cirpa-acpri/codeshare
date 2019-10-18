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