
# This file uses the data produced by the R script "20180913 Parse Precinct Canvass.R"
# Here, we created tabulated data and graphics
#Written by: Rob Ross, 2018-09-20, raross1217@gmail.com


rm(list=ls())

# Libraries ---------------------------------------------------------------
libs <- c("RSQLite", "digest", "sqldf")
# Check to see if packages are installed. Install them if they are not, then load them into the R session.
check.packages <- function(libs){
  new.libs <- libs[!(libs %in% installed.packages()[, "Package"])]
  if (length(new.libs)) 
    install.packages(new.libs, dependencies = TRUE)
  sapply(libs, require, character.only = TRUE)
}
for (iter in 1:length(libs))
  check.packages(libs[iter]);

# Designate file paths ----------------------------------------------------

election_date <-"20170405_"
election_type <-"consolidated_"
file_path <-"U:/Election Data Unit/Tableau/Data/Data tables/"
input_file <-paste(file_path, election_date,"election_precinct_canvass_LONG.csv",sep="")

source_data <- read.csv(input_file, header = TRUE)

#Data validation from previous step - did "20180913 Parse Precinct Canvass.R" compile this data properly?
print(nrow(sqldf("SELECT DISTINCT precinct FROM source_data")))
print(nrow(sqldf("SELECT DISTINCT precinct, registered FROM source_data")))
print(nrow(sqldf("SELECT DISTINCT precinct, registered, ballots_cast FROM source_data")))

turnout <- sqldf("SELECT precinct, contest, Sheet, SUM(registered) as registered, SUM(ballots_cast) as ballots_cast
                  , COUNT(precinct) as obs
                  FROM (
                    SELECT DISTINCT precinct, contest, Sheet,registered, ballots_cast FROM source_data)
                  GROUP BY precinct, contest, Sheet
                  ORDER by precinct, contest")

turnout$pct <- (turnout$ballots_cast/turnout$registered)*100
