* Fraser Hay, Conestoga College (Kitchener, Ontario) - fhay{at}conestogac.on.ca
* SPSS: Demonstration of AUTORECODE, which is useful when you have a variable of just captions, 
* but want to make it into a coded one. Useful when your survey platform doesn't export natively to SPSS.
* More info: https://www.ibm.com/support/knowledgecenter/en/SSLVMB_24.0.0/spss/base/syn_autorecode.html#syn_autorecode
*.

AUTORECODE varible				* Source.
	/INTO recoded_variable 		* Output.
	/BLANK MISSING				* Optional: Set blanks to missing.
	/PRINT.						* Optional: Show a coding scheme table.