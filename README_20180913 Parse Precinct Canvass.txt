Hi there,

Here are some instructions on how to use the R script named "20180913 Parse Precinct Canvass.R". This script takes a precinct canvass file and re-formats it so that it is easier to work with.

_____________________________________________________________
Step 0 - Get R
-------------------------------------------------------------

Before you start, you will need to install R, and RStudio on your terminal. These programs are free, and available on the internet. If you 

R: http://cran.cnr.berkeley.edu/

Studio: https://www.rstudio.com/

_____________________________________________________________
Step 1 - Get your data
-------------------------------------------------------------

R will look for data named in a very specific way, in a very specific place. Save the canvas file you want to use with a name that follows this pattern:

<YYYYMMDD>_<electiontype>_election_precinct_canvass
Example: 20100202_primary_election_precinct_canvass
Example: 20170405_consolidated_election_precinct_canvass

Do not use any capital letters. Make sure you spell each work correctly. Do not use spaces; the underscore '_' is shift+'-', the minus sign next to the zero. 

Precinct Canvasses can be found here: https://www.cookcountyclerk.com/service/precinct-canvasses

You must save the file in the following directory:

"U:/Election Data Unit/Tableau/Data/Data tables"

If you get the error message "Error: `path` does not exist:", you may have a typo in your path or file name.

_____________________________________________________________
Step 2 - Tell R where you data is
-------------------------------------------------------------

Open the script "20180913 Parse Precinct Canvass.R."

On line 29, you will see "file_names <- c("......." This is where you tell R what files you want to translate. You can do multiple files at a time - just put them in the list, enclosed in quotes and separated by commas.

For example: 

file_names <- c("20100202_primary_election_precinct_canvass"
                ,"20101102_gubernatorial_election_precinct_canvass")

Don't forget to close the parenthases at the end of your list.

_____________________________________________________________
Step 3 - Run the script
-------------------------------------------------------------

Highlight the entire file and click run. control+'A' will highlight everything for you. 

_____________________________________________________________
Step 4 - Spot check your file
-------------------------------------------------------------

The script will produce a CSV file(s) with the same name(s) as the files you fed into the program, with "_LONG" tacked onto the end of the file name. I would compare a few lines in the LONG file to the data in the original file to make sure everything came out all right.

