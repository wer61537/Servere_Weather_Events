#remove all objects just to be safe
rm(list = ls(all = TRUE))

#Setting up the environment
#get needed libraries
library(plyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(knitr, warn.conflicts = FALSE)
library(stringr, warn.conflicts = FALSE)
library(gridExtra, warn.conflicts = FALSE)
library(grid, warn.conflicts = FALSE)


# Data Processing
# Documentation is at https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf.
## Load the data
### download the file, if necessary
if (!file.exists("./data/repdata-data-StormData.csv.bz2"))
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
                "./data/repdata-data-StormData.csv.bz2")


### read in the dataset, if necessary
if (!exists("storm.data")){
  cat("Reading in the storm data file ... ")
  ## a bit pokey, takes 2 minutes to load
  ## 902,297 rows, 37 columns
  #not all columns are needed.  focus will be on:
  # state, beg_date, evtype, fatalaties, injuries,
  # propdmg, prodmgexp, cropdmg, cropdmgexp
  #  see http://stackoverflow.com/questions/5788117/only-read-limited-number-of-columns-in-r
  #columns from documentation are
  #STATE__,	BGN_DATE,	BGN_TIME,	TIME_ZONE,	COUNTY,	COUNTYNAME,	STATE,	EVTYPE,	BGN_RANGE,	BGN_AZI,	
  #BGN_LOCATI,	END_DATE,	END_TIME,	COUNTY_END,	COUNTYENDN,	END_RANGE,	END_AZI,	END_LOCATI,	LENGTH,	WIDTH,
  #F,	MAG,	FATALITIES,	INJURIES,	PROPDMG,	PROPDMGEXP,	CROPDMG,	CROPDMGEXP,	WFO,	STATEOFFIC,	ZONENAMES,
  # LATITUDE,	LONGITUDE,	LATITUDE_E,	LONGITUDE_,	REMARKS,	REFNUM
  
  # BGN_DATE (start of severe weather)
  # STATE 
  # EVTYPE (event type)
  # FATALITIES (no. of fatalities)
  # INJURIES (no. of injuries)
  # PROPDMG (damage cost on property)
  # PROPDMGEXP (factor codes K, M, B, to be applied on PROPDMG)
  # CROPDMG (damage cost on crops)
  # CROPDMGEXP (factor codes K, M, B, to be applied on CROPDMG)
  
  # using null prevents readin the column
  storm.data <- read.csv("./data/repdata-data-StormData.csv.bz2",
                         colClasses=c("NULL", 
                                      "character", #bgn_date
                                      rep("NULL",4),
                                      rep("character",2),  #state, evtype
                                      rep("NULL",14),  
                                      rep("numeric", 3), #fatalities, injuries, propdmg
                                      "character",  #propdmgexp
                                      "numeric",    #cropdmg
                                      "character",  #cropdmgexp
                                      rep("NULL",3),
                                      rep("NULL",2),  #latitude and #longitude, toogle on if needed
                                      rep("NULL",4)
                                      )
                        )
  cat("Storm data loaded!")
} 
## structure of data
str(storm.data)
#Tidy the data
#check to see if the variable names are "tidy"
if(grep("-",names(storm.data))>0){
  names(storm.data) <- gsub("-", "_", names(storm.data)) 
}

#check for missing data and remove any
if(sum(is.na(storm.data))>0){
  storm.data <- storm.data[complete.cases(storm.data), ]
}

## convert to upper case, remove multiple spaces and any preceding and trailing spaces
if (sum(!(grepl("^[[:upper:]]+$", storm.data$EVTYPE)))>0){
    storm.data$EVTYPE<-toupper(str_trim(gsub(pattern = "\\s\\s+", replacement = " ", storm.data$EVTYPE),side="both"))
}

if (sum(!(grepl("^[[:upper:]]+$", storm.data$PROPDMGEXP)))>0){
  storm.data$PROPDMGEXP<-toupper(str_trim(gsub(pattern = "\\s\\s+", replacement = " ", storm.data$PROPDMGEXP),side="both"))
}


if (sum(!(grepl("^[[:upper:]]+$", storm.data$CROPDMGEXP)))>0){
  storm.data$CROPDMGEXP<-toupper(str_trim(gsub(pattern = "\\s\\s+", replacement = " ", storm.data$CROPDMGEXP),side="both"))
}

storm.data$EVTYPE[(storm.data$EVTYPE)=="?"]<- "Unknown"

#fatalities and injuries
sort(unique(storm.data$FATALITIES))
sort(unique(storm.data$INJURIES))
#data seems fine
pers<-aggregate(cbind(storm.data$FATALITIES, storm.data$INJURIES)~storm.data$EVTYPE, data=storm.data, FUN=sum, na.rm=TRUE)
names(pers) =c("Event","Fatalities","Injuries")
pers2<-pers[!with(pers,Fatalities==0 & Injuries==0),]
pers2$Total_Pers_Cost<-pers2$Fatalities + pers2$Injuries

#select top 20
pers2<-pers2[ order(-pers2[,4], pers2[,1]), ]
pers2.top <- pers2[1:10,]
str(pers2.top)
pers2.top

dth <- ggplot(pers2.top, aes(x=Event,y=Fatalities )) + 
  geom_bar(stat="identity") + coord_flip() + 
  labs(title="Deaths Caused by Weather Events", x = "Event", y="Number of Deaths")
print(dth)

inj <- ggplot(pers2.top, aes(x=Event,y=Injuries )) + 
  geom_bar(stat="identity") + coord_flip() + 
  labs(title="Injuries Caused by Weather Events", x = "Event", y="Number of Injuries")
print(inj)

#plot_grid(dth, inj, labels = c("A","B"))

grid.arrange(dth, inj, nrow=2)

#economic cost
#check DMGEXP columns 
# The cost of damages is the product of PROPDMG * PROPDMGEXP and CROPDMG * CROPDMGEXP.
# the DMGEX" columns have the magnitude of the DMG.  So, PROPDMG =25 and PROPDMEXP =K 
# would be 25,000.

sort(unique(storm.data$PROPDMGEXP))
sort(unique(storm.data$CROPDMGEXP))
#convert to char
storm.data$PROPDMGEXP <- as.character(storm.data$PROPDMGEXP)
storm.data$CROPDMGEXP <- as.character(storm.data$CROPDMGEXP)


#take blank as a 1
storm.data$PROPDMGEXP[(storm.data$PROPDMGEXP)=="B"]<- 9
storm.data$CROPDMGEXP[(storm.data$CROPDMGEXP)=="B"]<- 9

storm.data$PROPDMGEXP[(storm.data$PROPDMGEXP)=="M"]<- 6
storm.data$CROPDMGEXP[(storm.data$CROPDMGEXP)=="M"]<- 6

storm.data$PROPDMGEXP[(storm.data$PROPDMGEXP)=="K"]<- 3
storm.data$CROPDMGEXP[(storm.data$CROPDMGEXP)=="K"]<- 3

storm.data$PROPDMGEXP[(storm.data$PROPDMGEXP)=="H"]<- 2
storm.data$CROPDMGEXP[(storm.data$CROPDMGEXP)=="H"]<- 2

sort(unique(storm.data$PROPDMGEXP))
sort(unique(storm.data$CROPDMGEXP))

#back to numeric
storm.data$PROPDMGEXP <- as.numeric(storm.data$PROPDMGEXP)
storm.data$CROPDMGEXP <- as.numeric(storm.data$CROPDMGEXP)

#create new vars to costs (DMG*10^DMGEP)
storm.data$PROPCOST <- storm.data$PROPDMG*10^storm.data$PROPDMGEXP
storm.data$CROPCOST <- storm.data$CROPDMG*10^storm.data$CROPDMGEXP

#clean up variations in names
sort(unique(storm.data$EVTYPE))

#which events had fatalities, injuries, crop or property damage and crop or property expense
#calculate mean for each activity, subject combination
econ<-aggregate(cbind(storm.data$PROPCOST, storm.data$CROPCOST)~storm.data$EVTYPE, data=storm.data, FUN=sum, na.rm=TRUE)
names(econ) =c("Event","Property","Crop")

econ2<-econ[!with(econ,Property==0.000000e+00 & Crop==0),]
names(econ2)
str(econ2)

econ2$Total_Econ_Cost<-econ2$Property + econ2$Crop

econ2<-econ2[ order(-econ2[,4], econ2[,1]), ][1:10,]
econ2

econ2 %>% group_by(Event) %>% mutate(cumsum = cumsum(Total_Econ_Cost))

prop2<-econ2[ order(-econ2[,2], econ2[,1]), ][1:10,]
crop2<-econ2[ order(-econ2[,3], econ2[,1]), ][1:10,]


totl <- ggplot(econ2, aes(x=Event,y=Total_Econ_Cost )) + 
  geom_bar(stat="identity") + coord_flip() + 
  labs(title="Total Economic Cost Caused by Weather Events", x = "Event", y="Cost")
print(totl)


prop <- ggplot(prop2, aes(x=Event,y=Property )) + 
  geom_bar(stat="identity") + coord_flip() + 
  labs(title="Cost of Property Damage Caused by Weather Events", x = "Event", y="Cost")
print(prop)

crop <- ggplot(crop2, aes(x=Event,y=Crop )) + 
  geom_bar(stat="identity") + coord_flip() + 
  labs(title="Cost of Crop Damage Caused by Weather Events", x = "Event", y="Cost")
print(crop)

grid.arrange(totl,prop, crop, nrow=3)

grid.table(econ2)

#The primary questions are
#Does the analysis address the question of which types of events are most harmful to population health?
#Does the analysis address the question of which types of events have the greatest economic consequences?

econ2[, ] <- formatC(as.numeric(econ2[,c('Property','Cost','Total_Econ_Cost') ]), big.mark = ',', digits = 0, format = 'f')