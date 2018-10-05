# This file is designed to read the data that comes out of the Cook County Clerk's system and parse 
# it for use in Tableau.
# Written by: Rob Ross, 2018-09-13, raross1217@gmail.com

rm(list=ls())
########################################################################################
# IF THIS IS THE FIRST TIME USING THIS PROGRAM ON THIS MACHINE, INSTALL THESE PACKAGES
# TO INSTALL PACKAGES, JUST REMOVE THE # SYMBOL AND RUN
########################################################################################

#install.packages("readxl")
#install.packages("dplyr")
#install.packages("plyr")

library(readxl); library(plyr); library(dplyr); 

########################################################################################
# INPUT DESIRED FILE(s) BELOW 
########################################################################################
# NOTE: You must save the file as an xlsx file with a file name of the form
# <YYYYMMDD>_<electiontype>_election_precinct_canvass
# Example: 20100202_primary_election_precinct_canvass
# NOTE: You must save the file in the following directory
# "U:/Election Data Unit/Tableau/Data/Data tables"


# 20090407_conolidated_election_precinct_canvass excluded because of technical problems
# April 5th, 2011 election is excluded because the data is stored as a PDF
file_names <- c("20150407_consolidated_election_precinct_canvass"
                ,"20161108_presidential_election_precinct_canvass"
                ,"20170228_primary_election_precinct_canvass"
                ,"20100202_primary_election_precinct_canvass"
                ,"20101102_gubernatorial_election_precinct_canvass"
                ,"20120320_primary_election_precinct_canvass"
                ,"20121106_presidential_election_precinct_canvass_1"
                ,"20121106_presidential_election_precinct_canvass_2"
                ,"20121106_presidential_election_precinct_canvass_3"
                ,"20130226_primary_election_precinct_canvass"
                ,"20130409_consolidated_election_precinct_canvass"
                ,"20140318_primary_election_precinct_canvass"
                ,"20141104_gubernatorial_election_precinct_canvas"
                ,"20150224_primary_election_precinct_canvass"
                ,"20170405_consolidated_election_precinct_canvass")
                

########################################################################################

# Program begins here
start_time <- Sys.time()
for(f in file_names){
#User designates the election date
election_date <-substr(f,1,8)

file_name <- f
file_path <-"U:/Election Data Unit/Tableau/Data/Data tables"

input_file <-paste(file_path, "/", file_name, ".xlsx", sep="")
output_file_wide <-paste(file_path, "/", file_name, "_WIDE", ".csv", sep="")
output_file_long <-paste(file_path, "/", file_name, "_LONG", ".csv", sep="")

# Ok, so the layout of these sheets is a PIA. So, we want to create a script that will re-format the data
# so that Tableau can read it. Each sheet may have multiple contests on it, so we want to be able to tell
# which candidate is running in which contest.We want to be able to know how many sheets there are in
# the excel file so we can loop over the sheets.
# Finally, we want to compile all the sheets together into a single data file which may be queried by Tableau

#Want a unique number to assign to each contest. Start at 0 and number sequentially.
contest_number <- 0

#How many sheets in this workbook?
canvasses <- read_xlsx(input_file, sheet="Document map", col_types="text")
canvass_count <- nrow(canvasses)
rm(canvasses)

#Create an empty data frame to hold final data
final_data <- data.frame(precinct=as.character())

for (sheet in 1:canvass_count){
  
  #List of precincts on this sheet
  precincts <- subset(read_xlsx(input_file, sheet=paste("Sheet", sheet, sep="" ), skip=6, col_types="text")
                      , X__1!="All Tally Types" & X__1!="Contest Total"
                      , select=c("X__1", "Registered", "Ballots Cast"))
  colnames(precincts)[1] <- "precinct"
  colnames(precincts)[2] <- "registered"
  colnames(precincts)[3] <- "ballots_cast"
  precincts$row <- as.numeric(rownames(precincts))
  
  #Count the number of contests on this sheet
  contests_number <- read_xlsx(input_file, sheet=paste("Sheet", sheet, sep="" ), range="A5:Z6", col_types="numeric")
  contests_number[is.na(contests_number)] <- 0
  max_contests <- max(contests_number[1,1:15])
  
  #Grab the string with the contest names
  contest_names <- unlist(
    sapply(read_xlsx(input_file, sheet=paste("Sheet", sheet, sep="" ), range="C5:C5", col_names=FALSE)
           , function(i) strsplit(i, "\r\n2")))
  #Remove the leading 1
  contest_names[1] <- substr(contest_names[1],3,nchar(contest_names[1]))
  
  #Ok, some sheets have numbers >1 but only one contest listed...
  if (length(contest_names)>max_contests) next
  #Made a handy matrix to help figure out naming sequence
  # Col	Contest	formula
  # 1	1	i
  # 2	1	i-1
  # 3	2	i-1
  # 4	2	i/2
  # 5	3	i+1/2
  # 6	3	i/2
  # 7	4	i+1/2
  # 8	4	i/2
  # and so on...

  #capture the ballots cast for each candidate in each contest on the sheet, and their names
  #Data 
  for (x in 1:max_contests){
    contest_number <- contest_number + 1
    columns <- read_xlsx(input_file, sheet=paste("Sheet", sheet, sep="" ))
    columns <- subset(columns[5,])
    columns[is.na(columns)] <- 0
    # We want to know dimensions of the whole sheet
    cols <- ncol(columns)
    rows <- nrow(precincts)
    #Make a list of excel column titles
    excel_colnames <- c("A", 	"B", 	"C", 	"D", 	"E", 	"F", 	"G", 	"H", 	"I", 	"J", 	"K", 	"L", 	"M", 	"N", 	"O", 	"P", 	"Q", 	"R", 	"S", 	"T", 	"U", 	"V", 	"W", 	"X", 	"Y", 	"Z")
    #Rename columns consistent with excel for ease
    for (i in 1:cols) {
        colnames(columns)[i] <- paste(excel_colnames[[i]])
    }
    # we want to make a list of the names of the columns with a number in row 5 corrosponding to the contest we want
    keep_list <- as.vector(apply(columns==x,1,function(a) paste0(colnames(columns)[a], sep=",", collapse = "")))
    keep_list <- unique(unlist(sapply(keep_list, function(i) strsplit(i, ','))))
    cols <- length(keep_list)
    #generate the area of the spreadsheet that contains the election results for this race
    keep_range <- paste(keep_list[1], "7:", keep_list[cols], rows+7, sep = "")
    
    #Pull these results, along with precinct identifiers
    race <- read_xlsx(input_file, sheet=paste("Sheet", sheet, sep="" ), range=keep_range, col_types="numeric")
    race[is.na(race)] <- 0
    if(length(-grep("X__", colnames(race)))>0)
    {
      race <- race[, -grep("X__", colnames(race))]
    }
    
    candidate_names <- names(race)
    candidate_number <- length(race)
    
    for (i in 1:candidate_number) {
      colnames(race)[i] <- paste("cand_","blts_",i, sep="")
    }
    
    for(i in 1:candidate_number) {
      race[ , paste0("candnm_",i)] = candidate_names[i]
    }
    
    race$contest = contest_names[x]
    
    race$row <- as.numeric(rownames(race))
    
    race <- merge(precincts, race, by="row")
    race$Sheet <- sheet
    race$contest_number <- contest_number
    race$contest_number_onsheet <- x
    
    dim(race)
    final_data <- rbind.fill(final_data, race)
    
    print(paste("Completed contest", contest_number, "sheet", sheet, "at", Sys.time()))
  }
  print(paste("Completed Sheet", sheet, "at", Sys.time()))
  nrows <- nrow(final_data)  
  print(paste("Final data is now", nrows, "long"))
  
}
final_data$election_date <- as.Date(election_date, "%Y%m%d")
final_data$contest<-trimws(final_data$contest)
final_data$ballots_cast <- as.numeric(as.character(final_data$ballots_cast))
final_data$registered <- as.numeric(as.character(final_data$registered))
election <- unique(unlist(sapply(file_name, function(i) strsplit(i, '_'))))

final_data$election_type <-  election[2]

# Final column selection
# Max number of candidates, list of colnames
l <- colnames(final_data)
l1 <-Filter(function(x) any(grepl("candnm", x)), l)
l2 <-Filter(function(x) any(grepl("cand_", x)), l)
rm(l)

final_data <- final_data[c("election_type", "election_date","precinct", "contest_number", "Sheet", "contest_number_onsheet"
                             , "contest", "registered", "ballots_cast", l1, l2)]
test <- final_data[
  order(final_data[,1], final_data[,2], final_data[,3]),
  ] 

write.csv(final_data, file = output_file_wide, row.names=FALSE)

# We want long-form data for some purposes

# Empty data frame to contain long-form data
final_data_long <- data.frame(precinct=as.character())

# reshape the data
end <- length(l1)
for (i in 1:end){
  temp <-  final_data[c("election_type", "election_date","precinct", "contest_number", "Sheet", "contest_number_onsheet"
                        , "contest", "registered", "ballots_cast", l1[i], l2[i])]
  colnames(temp)[colnames(temp)==paste0("candnm_",i,sep="")] <- "candidate_name"
  colnames(temp)[colnames(temp)==paste0("cand_blts_",i,sep="")] <- "candidate_ballots"
  final_data_long=rbind(final_data_long, temp)
  rm(temp)
}
test <- final_data_long[
  order(final_data_long[,1], final_data_long[,2], final_data_long[,3], final_data_long[,10]),
  ] 

# Export 
write.csv(final_data_long, file = output_file_long, row.names=FALSE)

print(paste(file_name, "finished at", Sys.time()))

}
##############################################################################
# Create master long and wide files

master_precinct_canvass_LONG <- data.frame(precinct=as.character())
master_precinct_canvass_WIDE <- data.frame(precinct=as.character())

for(f in file_names){
  
  file_name <- f
  file_path <-"U:/Election Data Unit/Tableau/Data/Data tables"
  
  input_file <-paste(file_path, "/", file_name, ".xlsx", sep="")
  output_file_wide <-paste(file_path, "/", file_name, "_WIDE", ".csv", sep="")
  output_file_long <-paste(file_path, "/", file_name, "_LONG", ".csv", sep="")
  
  master_output_file_long <-paste(file_path, "/", "master_precinct_canvass", "_LONG", ".csv", sep="")
  master_output_file_wide <-paste(file_path, "/", "master_precinct_canvass", "_WIDE", ".csv", sep="")

  print(output_file_long)
  x <- read.csv(output_file_long)
  master_precinct_canvass_LONG <- rbind.fill(master_precinct_canvass_LONG, x)
  rm(x)
  print(output_file_wide)
  y <- read.csv(output_file_wide)
  master_precinct_canvass_WIDE <- rbind.fill(master_precinct_canvass_WIDE, y)
  rm(y)
  
}
master_precinct_canvass_LONG <- subset(master_precinct_canvass_LONG, candidate_name!="NA")

write.csv(master_precinct_canvass_LONG, file = master_output_file_long, row.names=FALSE)
write.csv(master_precinct_canvass_WIDE, file = master_output_file_wide, row.names=FALSE)

##############################################################################
end_time <- Sys.time()

print(paste("Script finished at", Sys.time()))
end_time - start_time

rm(list=ls())




