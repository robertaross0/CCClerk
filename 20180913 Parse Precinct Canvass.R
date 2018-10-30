# This file is designed to read the data that comes out of the Cook County Clerk's system and parse 
# it for use in Tableau.
# Written by: Rob Ross, 2018-09-13, raross1217@gmail.com

rm(list=ls())

# Libraries ---------------------------------------------------------------
libs <- c("readxl", "dplyr", "plyr", "stats", "data.table", "matrixStats",  "svMisc")
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


# INPUT DESIRED FILE(s) BELOW 

# NOTE: You must save the file as an xlsx file with a file name of the form
# <YYYYMMDD>_<electiontype>_election_precinct_canvass
# Example: 20100202_primary_election_precinct_canvass
# NOTE: You must save the file in the following directory
# "U:/Election Data Unit/Tableau/Data/Data tables"


# 20090407_conolidated_election_precinct_canvass excluded because of technical problems
# April 5th, 2011 election is excluded because the data is stored as a PDF
file_names <- c("20180320_primary_election_precinct_canvass")
                #  ,"20161108_presidential_election_precinct_canvass"
                # ,"20150407_consolidated_election_precinct_canvass"
                # ,"20170228_primary_election_precinct_canvass"
                # ,"20100202_primary_election_precinct_canvass"
                # ,"20101102_gubernatorial_election_precinct_canvass"
                # ,"20120320_primary_election_precinct_canvass"
                # ,"20121106_presidential_election_precinct_canvass_1"
                # ,"20121106_presidential_election_precinct_canvass_2"
                # ,"20121106_presidential_election_precinct_canvass_3"
                # ,"20130226_primary_election_precinct_canvass"
                # ,"20130409_consolidated_election_precinct_canvass"
                # ,"20140318_primary_election_precinct_canvass"
                # ,"20141104_gubernatorial_election_precinct_canvas"
                # ,"20150224_primary_election_precinct_canvass"
                # ,"20170405_consolidated_election_precinct_canvass")
                

# Script to ingest data ------------

# Program begins here
start_time <- Sys.time()

# Assign directories
for(f in length(file_names)){
# Election date comes from file names
election_date <-substr(file_names[f],1,8)
file_name <- file_names[f]}


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

#How many sheets in this workbook? Need to loop through sheets until you get an error.
count_sheets <- function(sheet) {
  out <- tryCatch(
    {
     read_xlsx(input_file, sheet=paste("Sheet",sheet,sep=""),range="A1:A2",col_types="text")
      return(NULL)
    },
    error=function(cond) {
      return(sheet-1)
    },
    warning=function(cond) {
      return(NULL)
    },
    finally={
    }
  )    
  return(out)
}
for(s in 1:1000){
  canvass_count <-count_sheets(s)
  if (length(canvass_count)==1) break; 
}
cat(file_name, "has", canvass_count, "sheets")

#Create an empty data frame to hold final data
final_data <- data.frame(precinct=as.character())

for (sheet in 1:canvass_count){

  cat("Sheet", sheet)
  # Different files have slightly different layouts
  
  if(as.Date(election_date, "%Y%m%d")>=as.Date("20180302", "%Y%m%d")){skip <- 5
  } else{skip <- 6}
  
  #List of precincts on this sheet
  precincts <- subset(read_xlsx(input_file, sheet=paste("Sheet", sheet, sep="" ), skip=skip, col_types="text")
                      , X__1!="All Tally Types" & X__1!="Contest Total"
                      , select=c("X__1", "Registered", "Ballots Cast"))
  colnames(precincts)[1] <- "precinct"
  colnames(precincts)[2] <- "registered"
  colnames(precincts)[3] <- "ballots_cast"
  precincts$row <- as.numeric(rownames(precincts))
  
  if(as.Date(election_date, "%Y%m%d")>=as.Date("20180302", "%Y%m%d")) {
  #Count the number of contests on this sheet
  read_xlsx(input_file, sheet=paste("Sheet", 1, sep="" ), range="A7:Z8", col_types="numeric")
  contests_number <- read_xlsx(input_file, sheet=paste("Sheet", sheet, sep="" ), range="A5:Z6", col_types="numeric")
  contests_number[is.na(contests_number)] <- 0
  max_contests <- max(contests_number[1,1:15])
  } else {max_contests <- 1}
  
  #Grab the string with the contest names
  contest_names <- unlist(
    sapply(sapply(sapply(
      read_xlsx(input_file, sheet=paste("Sheet", sheet, sep="" ), range="C5:C5", col_names=FALSE)
      , function(i) strsplit(i, "\r\n2"))
      , function(i) strsplit(i, "\r\n3"))
      , function(i) strsplit(i, "\r\n4"))
    )

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

  if(as.Date(election_date, "%Y%m%d")>=as.Date("20180302", "%Y%m%d")) {
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
    
    
  }}else{
    # For newer files, there is only one contest on each sheet. 
    columns <- read_xlsx(input_file, sheet=paste("Sheet", 1, sep="" ))
    cols <- ncol(columns)
    rows <- nrow(precincts)
  }
  
  cat("Completed contest", contest_number, "sheet", sheet, ". Final data is now", nrow(final_data), "long")
  
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


rm(precincts, race, columns)
# Compile all fles into a single master file ----------

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
rm(final_data_long, final_data)
master_precinct_canvass_LONG <- subset(master_precinct_canvass_LONG, candidate_name!="NA")

write.csv(master_precinct_canvass_LONG
          , file = paste(file_path, "/", "master_precinct_canvass_LONG.csv", sep="")
          , row.names=FALSE)
save(master_precinct_canvass_LONG
     , file = paste(file_path, "/", "master_precinct_canvass_LONG.Rdata", sep=""))

write.csv(master_precinct_canvass_WIDE
          , file = paste(file_path, "/", "master_precinct_canvass_WIDE.csv", sep="")
          , row.names=FALSE)
save(master_precinct_canvass_WIDE
     , file = paste(file_path, "/", "master_precinct_canvass_WIDE.Rdata", sep=""))


# Helpful Variables (WIDE) ----
load("U:/Election Data Unit/Tableau/Data/Data tables/master_precinct_canvass_WIDE.Rdata")
l <- colnames(master_precinct_canvass_WIDE)
l2 <-Filter(function(x) any(grepl("cand_", x)), l)
a <- master_precinct_canvass_WIDE[l2]
a[is.na(a)] <- 0
winners <- as.data.frame(cbind(row.names(a),apply(a,1,function(x)
  names(a)[which(x==max(x))])))

# THIS DOESN"T WORK YET
# for(r in 1:2){
#   winno <- substring(winners$V2[r],11,12)
#   X <-paste0("candnm_",winno,"[",r,"]",sep="")
#   print(X)
#   print(master_precinct_canvass_WIDE$candnm_1[1])
#   print(master_precinct_canvass_WIDE$X)
#   winners$winner_name <- master_precinct_canvass_WIDE$X
# }
colnames(winners)[colnames(winners)=="V1"] <- "winner_ballots"

master_precinct_canvass_WIDE <- cbind(master_precinct_canvass_WIDE, winners["winner_ballots"])

# Helpful Variables (LONG) ----
# Want name and vote of winning candidate for graphic production
# Identify maximum number of ballots cast
winner_names <-
   aggregate(master_precinct_canvass_LONG$candidate_ballots
          , by=list(contest=master_precinct_canvass_LONG$contest
                    ,election_date=master_precinct_canvass_LONG$election_date
                    ,candidate_name=master_precinct_canvass_LONG$candidate_name)
          , sum)
colnames(winner_names)[colnames(winner_names)=="x"] <- "candidate_ballots"
winner_names <- arrange(winner_names
  , desc(election_date)
  , contest, candidate_name)
winner_ballots <-
  aggregate(winner_names$candidate_ballots
            , by=list(contest=winner_names$contest
                      ,election_date=winner_names$election_date)
            , max)
winner_ballots$winner_flag <- 1
colnames(winner_ballots)[colnames(winner_ballots)=="x"] <- "candidate_ballots"
winners <- merge(winner_names
  , winner_ballots
  , by=c("election_date", "contest", "candidate_ballots")
  , all.y=TRUE)
winners <- merge(winner_names
       , winner_ballots
       , by=c("candidate_ballots", "election_date", "contest")
       , all.y=TRUE
       , no.dups = TRUE)
colnames(winners)[colnames(winners)=="candidate_ballots"] <- "total_winner_ballots"
colnames(winners)[colnames(winners)=="candidate_name"] <- "winner_name"
winners <- winners[!duplicated(winners),]
# For precinct-by-precinct
winner_by_precinct <- winners[,c(2,3,4)]
colnames(winner_by_precinct)[colnames(winner_by_precinct)=="winner_name"] <- "candidate_name"
winner_by_precinct <- merge(master_precinct_canvass_LONG[,c(1,2,3,4,11)]
                            , winner_by_precinct
                            , by = c("election_date", "contest", "candidate_name")
                            , all.x=TRUE)
winner_by_precinct <-winner_by_precinct[c(1,2,4,5)]
colnames(winner_by_precinct)[colnames(winner_by_precinct)=="candidate_ballots"] <- "winner_ballots"

# Identify ties - are there really no perfect ties? I suppose that's likely, but actually true?
# ties <- subset(merge(
#   aggregate(master_precinct_canvass_LONG$precinct
#             , by=list(contest=master_precinct_canvass_LONG$contest
#                       , election_date=master_precinct_canvass_LONG$election_date)
#             , NROW)
#   ,
#   aggregate(master_precinct_canvass_LONG$winner_flag
#                     , by=list(contest=master_precinct_canvass_LONG$contest
#                               , election_date=master_precinct_canvass_LONG$election_date)
#                     , sum)
#   , by = c("election_date", "contest")
# ), x.y > x.x)
# if(nrow(ties)>0){
# ties$tie_flag = 1
# master_precinct_canvass_LONG <- merge(master_precinct_canvass_LONG
#                                       , winners
#                                       , by=c("election_date", "contest")
#                                       , all.x=TRUE)
# master_precinct_canvass_LONG$tie_flag <- as.numeric(!is.na(master_precinct_canvass_LONG$tie_flag))
# } else{rm(ties)}

# Now capture the names of winners

master_precinct_canvass_LONG <- merge(master_precinct_canvass_LONG
                                      , winners
                                      , by=c("election_date", "contest")
                                      , all.x=TRUE)
master_precinct_canvass_LONG <- merge(master_precinct_canvass_LONG
                                      , winner_by_precinct
                                      , by=c("election_date", "contest", "precinct")
                                      , all.x=TRUE)

master_precinct_canvass_LONG$Idpct_txt <- as.character(as.numeric(master_precinct_canvass_LONG$precinct))

# Re-order column
colnames(master_precinct_canvass_LONG)
master_precinct_canvass_LONG <- master_precinct_canvass_LONG[,c(1,5,3,2,6,7,8,9,10,14,11,4,15,12,13)]
colnames(master_precinct_canvass_LONG)
master_precinct_canvass_LONG <- arrange(master_precinct_canvass_LONG
                                        , desc(election_date)
                                        , contest, candidate_name, precinct)
# Data integrity checks -------

vars <- c("contest", "sheet", "election_date", "candidate_name", "precinct")
# No NAs
for (v in 1:length(vars)) { 
  if(anyNA(master_precinct_canvass_LONG$vars[v])) {
    nas <- subset(master_precinct_canvass_LONG, is.na(master_precinct_canvass_LONG$vars[v]))
    stop()
    print(paste("Not good, ",vars[v],"contains NA values", sep=""))
  } else{print(paste("Good, ",vars[v]," does not contains NA values", sep=""))}
}

# Candidate ballots <= registered
if (nrow(subset(master_precinct_canvass_LONG
          , master_precinct_canvass_LONG$candidate_ballots>master_precinct_canvass_LONG$registered))>0){
  print("WARNING: in some cases the number of ballots cast for a candidate is greater than the number of registered voters")
}
# Example
wrong_number_votes_example <-  subset(master_precinct_canvass_LONG
                              , master_precinct_canvass_LONG$contest_number == 475 
                              & master_precinct_canvass_LONG$Sheet == 403)
# Odd contest strings: Alexander Patrick White

bad_data <- subset(master_precinct_canvass_WIDE, master_precinct_canvass_WIDE$contest=="Alexander Patrick White")

# Make sure joins well to shapefiles
ccsuburbs <- readOGR("U:/Election Data Unit/Tableau/Data/Spatial/County Precincts CC Open Data")
ccsuburbs <- data.frame(Idpct_txt=ccsuburbs$Idpct_txt, shape=1)
num_precincts <- nrow(data.frame(unique(master_precinct_canvass_LONG$precinct)))
num_Idpct_txt <- nrow(data.frame(unique(ccsuburbs$Idpct_txt)))
x <- merge(x = ccsuburbs, y = master_precinct_canvass_LONG, by = "Idpct_txt")
num_matches <- nrow(data.frame(unique(x$Idpct_txt)))
num_precincts
num_Idpct_txt
num_matches
rm(x)

# Save output and clear ----

write.csv(master_precinct_canvass_LONG
          , file = paste(file_path, "/", "master_precinct_canvass_LONG.csv", sep="")
          , row.names=FALSE)
save(master_precinct_canvass_LONG
     , file = paste(file_path, "/", "master_precinct_canvass_LONG.Rdata", sep=""))

# write.csv(master_precinct_canvass_WIDE, file = master_output_file_wide, row.names=FALSE)

end_time <- Sys.time()

print(paste("Script finished at", Sys.time()))
end_time - start_time

rm(list=ls())




