## Data Analysis on Registered clinical trials

setwd("C:/Users/user/Documents/R/Clinicals")

#install.packages("XML")
library("XML")
library(ggplot2)
library(dbplyr)
library(tidyverse)
library(lubridate)
library(scales)

#Load Clinical Trials in Kenya Data Set
clinicals <- xmlToDataFrame("ICTRP-Results.xml")

#Transform Date Column from character to Date variable
class(clinicals$Date_registration) ##'character'
#using the lubridate library
clinicals <- clinicals %>%
  mutate(Date_registration = lubridate::dmy(Date_registration))

class(clinicals$Date_registration)##'Date'



#Now, What do i WANT to do with the data set?

#create a count column
count <- c(1:1175)
clinicals <- cbind(clinicals, count)

# Asses The Phases of the clinical trials.
# First need to clean the data
# get the Phase column
Phase <- clinicals$Phase
#set to lower case
Phase <- tolower(Phase)
#Remove hyphens
Phase <- gsub("-/@#$:", "", Phase )
#Remove \n
Phase <- gsub("\n", "", Phase)


##Phase 4
Phase <- gsub('phase-4','phase 4', Phase)
Phase <- gsub('phase iv','phase 4', Phase)
Phase <- gsub('phase 1v','phase 4', Phase)
Phase <- gsub('iv','phase 4', Phase)

##Phase3
Phase <- gsub('phase-3','phase 3', Phase)
Phase <- gsub('phase iii','phase 3', Phase)


#Phase2
Phase <- gsub('phase-2','phase 2', Phase)
Phase <- gsub('phase ii','phase 2', Phase)
Phase <- gsub('ii','phase 2', Phase)
Phase <- gsub('phase 2i','phase 2', Phase)
Phase <- gsub('phase2/phase 2i','phase 2', Phase)
Phase <- gsub ('phase 2/phase 2','phase 2', Phase)

#Phase1
Phase <- gsub('phase-1','phase 1', Phase)
Phase <- gsub('phase i','phase 1', Phase)


#Phase0
Phase <- gsub('phase-0','phase 0', Phase)
Phase <- gsub('0','phase 0', Phase)
Phase <- gsub('phase phase 0','phase 0', Phase)

#Phase2/3
Phase <- gsub('phase 2/ phase 3','phase 2 / phase 3', Phase)
Phase <- gsub('phase 2/phase 3','phase 2 / phase 3', Phase)

#Phase1/2
Phase <- gsub('phase 1/phase 2','phase 1 / phase 2', Phase)
Phase <- gsub('phase 1/ phase 2','phase 1 / phase 2', Phase)

#Replace the long texts to empty strings
Phase <- gsub('explanatory', '', Phase)
Phase <- gsub('trials','', Phase)
Phase <- gsub('[()]', '', Phase)
Phase <- gsub('human', '', Phase)
Phase <- gsub('pharmacology', '', Phase)
Phase <- gsub('confirmatory', '', Phase)
Phase <- gsub('exploratory', '', Phase)
Phase <- gsub('therapeutic', '', Phase)
Phase <- gsub('use', '', Phase)
Phase <- gsub('no', '', Phase)
Phase <- gsub('n/a', '', Phase)

#Substitute the remaining patterns
Phase <- gsub('phase 1: yes', 'yes1', Phase)
Phase <- gsub('phase 2: yes', 'yes2', Phase)
Phase <- gsub('phase 3: yes', 'yes3', Phase)
Phase <- gsub('phase 4: yes', 'yes4', Phase)

Phase <- gsub('phase 1:', '', Phase)
Phase <- gsub('phase 2:', '', Phase)
Phase <- gsub('phase 3:', '', Phase)
Phase <- gsub('phase 4:', '', Phase)

Phase <- gsub("-","", Phase)
Phase <- gsub("[[:space:]]", "", Phase)

Phase <- gsub("yes1", "phase1", Phase)
Phase <- gsub("yes2", "phase2", Phase)
Phase <- gsub("yes3", "phase3", Phase)
Phase <- gsub("yes4", "phase4", Phase)

Phase <- gsub("/","", Phase)
Phase <- gsub("[[:space:]]", "", Phase)

Phase <- gsub('postmarketingsurveillance', 'phase4', Phase)
Phase <- gsub('tspecified','', Phase)
Phase <- gsub('tapplicable','Not_App', Phase)#not_applicable

Phase <- gsub('3','phase3', Phase)
Phase <- gsub('2','phase2', Phase)

Phase <- gsub('phasephase','phase', Phase)
Phase <- gsub('phase', 'p', Phase)

clinicals["fix_Phase"] <- Phase


#Plot the Phases of the clinical trials
#visualize remaining errors
# ggplot(data = clinicals,
#        aes(x = count, y = fix_Phase))+
#   geom_bar(stat = "identity")



#Obtain Clinical Trials from 2022
this_year <- clinicals[clinicals$Date_registration >= "2022-01-01" & clinicals$Date_registration <= "2022-09-30", ]
#delete last 3 rows, they contain NA values
this_year <- this_year[-c(74,75,76),]

#create a count column, to enable summation? 
count_a <- rep(1,times =73) #add it to this_year table
this_year <- cbind(this_year, count_a)

# Create a separate data frame, That contains This year's clinical trials and the counta column
Phases_data <- this_year[ , c("fix_Phase", "count_a")]

#Change all empty cells to NA
Phases_data [Phases_data == ""] <- NA

Phases_data <- drop_na(Phases_data)
#Then drop all Rows with NA variable
Phases_data <- na.omit(Phases_data)


#Now, record each observation of phases into their own row
Phases_data1 <- Phases_data %>% 
  group_by(fix_Phase) %>% 
  summarise_each(funs(sum))


  ggplot(data = Phases_data1,
         aes(x =count_a , y =fix_Phase, label = round (count_a, 1)))+
    geom_point()+
    geom_segment(aes(y=fix_Phase, yend=fix_Phase, x=0, xend=count_a),
                 size = 1,
                 color = 'grey')+
    geom_point(
      size = 6,
      color = '#5D3FD3'
    )+
    geom_text(aes(label=count_a),color='white', size = 3.5)+
    theme_light()+
    theme (
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      axis.ticks.x = element_blank()
    )+
    xlab("")+
    ylab("Phases")+
    ggtitle("Clinical Trials")+
    labs (subtitle = '01-Jan to 30-Sep 2022')+
    scale_x_continuous(breaks = breaks_width(5))
  
summary(Phases_data1$count_a)
sum(Phases_data1$count_a)
