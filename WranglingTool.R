setwd("C:\Users\hamza.jibran\Documents\2019-05-16DialerWranglingTool\WorkFolder")
library(dplyr)
library(tidyr)
library(stringr)


##import data into dataframe and merge this data
my_data1 <- read.csv(file = file.choose(),strip.white=TRUE, stringsAsFactors = T)
my_data2<- read.csv(file = file.choose(),strip.white=TRUE, stringsAsFactors = T)
my_data3 <- read.csv(file = file.choose(),strip.white=TRUE, stringsAsFactors = T)

callReports <- rbind(my_data1,my_data2,my_data3)


##restructure my_data to variables that are important
selectedData <- callReports[,c("call_date", "phone_number_dialed", "user","campaign_id","list_id","state","length_in_sec","list_name","status_name")]

##seperate call_date column from datetime to date and time 
selectedData <- separate(selectedData, call_date, into= c("call_date", "call_time") , sep= " ")

##Cleaning States Variable
selectedData$state <- toupper(selectedData$state)

is.na(selectedData$state) <- which(selectedData$state == "") 
is.na(selectedData$state) <- which(selectedData$state == "-")
is.na(selectedData$state) <- which(nchar(as.vector(selectedData$state)) != 2)
selectedData$state <- as.factor(selectedData$state)
summary(selectedData$state)


#################################Disposition Transformations###################################
# Comment: I changed the labels of the factors to automatically affect the records #
#levels(selectedData$status_name)
#summary(selectedData$status_name)

levels(selectedData$status_name) <- sub("Auto Sale", "Sale Made", levels(selectedData$status_name))
levels(selectedData$status_name) <- sub("Do Not Qualify", "Does Not Qualify", levels(selectedData$status_name))
levels(selectedData$status_name) <- sub("Customer Hung Up", "Hang Up", levels(selectedData$status_name))
levels(selectedData$status_name) <- sub("Mute Call OR No Voice", "Mute", levels(selectedData$status_name))
levels(selectedData$status_name) <- sub("Voice Disappeared During Call", "Mute", levels(selectedData$status_name))
levels(selectedData$status_name) <- sub("Pre Hangup", "Pre Hang Up", levels(selectedData$status_name))
levels(selectedData$status_name) <- sub("Pre-Hangup", "Pre Hang Up", levels(selectedData$status_name))
levels(selectedData$status_name) <- sub("PREhangup", "Pre Hang Up", levels(selectedData$status_name))
levels(selectedData$status_name) <- sub("Pre hung up", "Pre Hang Up", levels(selectedData$status_name))

#################################Campaign Identification#######################################

selectedData$campaign_name[selectedData$campaign_id == 10011] <- "Auto Live"
selectedData$campaign_name[selectedData$campaign_id == 1007] <- "Debt Consolidation"
selectedData$campaign_name[selectedData$campaign_id == 1006] <- "Debt Consolidation"
selectedData$campaign_name[selectedData$campaign_id == 9999] <- "Auto Insurance"
selectedData$campaign_name[selectedData$campaign_id == 1002] <- "Auto Insurance"

###############################Agent Name######################################################
#levels(selectedData$user)


## Set length in seconds of call to 0 ##
selectedData$length_in_sec <- replace_na(selectedData$is_workable,0)

################ Create 2 new boolean variables for Connected and Conversion###################
selectedData$is_connected[selectedData$status_name == "Business Number"] <- TRUE
selectedData$is_connected[selectedData$status_name == "Call Back"] <- TRUE
selectedData$is_connected[selectedData$status_name == "Call Picked Up"] <- TRUE
selectedData$is_connected[selectedData$status_name == "Declined Sale"] <- TRUE
selectedData$is_connected[selectedData$status_name == "DO NOT CALL"] <- TRUE
selectedData$is_connected[selectedData$status_name == "Does Not Qualify"] <- TRUE
selectedData$is_connected[selectedData$status_name == "Hang Up"] <- TRUE
selectedData$is_connected[selectedData$status_name == "Language Barier"] <- TRUE
selectedData$is_connected[selectedData$status_name == "Lead Being Called"] <- TRUE
selectedData$is_connected[selectedData$status_name == "No Pitch No Price"] <- TRUE
selectedData$is_connected[selectedData$status_name == "Not At Home"] <- TRUE
selectedData$is_connected[selectedData$status_name == "Not Interested"] <- TRUE
selectedData$is_connected[selectedData$status_name == "Sale Made"] <- TRUE
selectedData$is_connected[selectedData$status_name == "Unsuccessful Transfer"] <- TRUE
selectedData$is_connected[selectedData$status_name == "Wrong Number"] <- TRUE
selectedData$is_connected[selectedData$status_name == "Xfer Line Hung Up"] <- TRUE
selectedData$is_connected[selectedData$status_name == "Agent Not Available"] <- TRUE
selectedData$is_connected[selectedData$status_name == "Mute"] <- TRUE

selectedData$is_connected <- replace_na(selectedData$is_connected,FALSE)

############################################## Workable Non-Workable numbers ##################
selectedData$is_workable[selectedData$status_name == "Agent Not Available"] <- TRUE
selectedData$is_workable[selectedData$status_name == "Answering Machine"] <- TRUE
selectedData$is_workable[selectedData$status_name == "Busy"] <- TRUE
selectedData$is_workable[selectedData$status_name == "Call Back"] <- TRUE
selectedData$is_workable[selectedData$status_name == "Hang Up"] <- TRUE
selectedData$is_workable[selectedData$status_name == "Customer Not Available"] <- TRUE
selectedData$is_workable[selectedData$status_name == "Disconnected Number"] <- TRUE
selectedData$is_workable[selectedData$status_name == "Distortion"] <- TRUE
selectedData$is_workable[selectedData$status_name == "Line Ringing"] <- TRUE
selectedData$is_workable[selectedData$status_name == "Mute"] <- TRUE
selectedData$is_workable[selectedData$status_name == "No Answer"] <- TRUE
selectedData$is_workable[selectedData$status_name == "No Pitch No Price"] <- TRUE
selectedData$is_workable[selectedData$status_name == "Hang Up"] <- TRUE
selectedData$is_workable[selectedData$status_name == "Answering Machine Auto"] <- TRUE
selectedData$is_workable[selectedData$status_name == "Agent Error"] <- TRUE
selectedData$is_workable[selectedData$status_name == "No Answer AutoDial"] <- TRUE
selectedData$is_workable[selectedData$status_name == "Disconnected Number Auto"] <- TRUE
selectedData$is_workable[selectedData$status_name == "Outbound Pre-Routing Drop"] <- TRUE
selectedData$is_workable[selectedData$status_name == "Outbound Local Channel Res Err"] <- TRUE
selectedData$is_workable[selectedData$status_name == "Pre Drop"] <- TRUE
selectedData$is_workable[selectedData$status_name == "Pre Hang Up"] <- TRUE
selectedData$is_workable[selectedData$status_name == "Busy Auto"] <- TRUE
selectedData$is_workable[selectedData$status_name == "Disconnected Number Auto"] <- TRUE

selectedData$is_workable <- replace_na(selectedData$is_workable,FALSE)


#############################################Set Conversion logical variable ##################
selectedData$is_converted[selectedData$status_name == "Sale Made"] <- TRUE
selectedData$is_converted <- replace_na(selectedData$is_converted,FALSE)



################################################Write Master File to Excel#####################
filename <- paste(c(as.character(selectedData$call_date[1]),"MasterFile.csv"),collapse="-")
filename <- str_replace_all(filename, "/", "-")
write.csv(selectedData, filename, row.names = F)

########################################## Create Variables for Report ########################
Date <- selectedData$call_date[1]
#####AtuoLive#####
 AutoLiveData <- selectedData %>%
   filter(selectedData$campaign_name == "Auto Live")

 Live_campaign = AutoLiveData$campaign_name[1]
 Live_totalDialled = as.numeric(count(AutoLiveData)) 
 Live_totalConnected = sum(AutoLiveData$is_connected)
 Live_connectionRatio = (Live_totalConnected/Live_totalDialled)*100
 Live_totalConverted = sum(AutoLiveData$is_converted)
 Live_conversionRatio = (Live_totalConverted/Live_totalConnected)*100
 Live_workable = sum(AutoLiveData$is_workable)


######AutoInsurance#########
AutoData<- selectedData %>%
  filter(selectedData$campaign_name == "Auto Insurance")

Auto_campaign = AutoData$campaign_name[1]
Auto_totalDialled = as.numeric(count(AutoData))
Auto_totalConnected = sum(AutoData$is_connected)
Auto_connectionRatio = (Auto_totalConnected/Auto_totalDialled)*100
Auto_totalConverted = sum(AutoData$is_converted)
Auto_conversionRatio = (Auto_totalConverted/Auto_totalConnected)*100

DebtData<- selectedData %>%
  filter(selectedData$campaign_name == "Debt Consolidation")

Debt_campaign = DebtData$campaign_name[1]
Debt_totalDialled = as.numeric(count(DebtData))
Debt_totalConnected = sum(DebtData$is_connected)
Debt_connectionRatio = (Debt_totalConnected/Debt_totalDialled)*100
Debt_totalConverted = sum(DebtData$is_converted)
Debt_conversionRatio = (Debt_totalConverted/Debt_totalConnected)*100

##Store results in vectors
Date <- c(Date,Date,Date)
campaign <- c(Live_campaign,Auto_campaign,Debt_campaign)
totalDialled <- c(Live_totalDialled, Auto_totalDialled, Debt_totalDialled)
totalConnected <- c(Live_totalConnected, Auto_totalConnected, Debt_totalConnected)
connectedRatio <- c(Live_connectionRatio, Auto_connectionRatio, Debt_connectionRatio)
totalConverted <- c(Live_totalConverted, Auto_totalConverted, Debt_totalConverted)
conversionRatio <- c(Live_conversionRatio, Auto_conversionRatio, Debt_conversionRatio)


##create the dataframe
reportFrame <- data.frame(Date,
                          campaign,
                          totalDialled,
                          totalConnected,
                          connectedRatio,
                          totalConverted,
                          conversionRatio)

#######Check if no File then add headers else dont
setHeader <- FALSE
if(is.na(file.info('DaywiseReport.csv')$size)){
    setHeader <- TRUE
}

write.table(reportFrame, "DaywiseReport.csv", sep = ",", col.names = setHeader, append = T, row.names = F)

