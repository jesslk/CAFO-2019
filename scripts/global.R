################### read dataset #######################
#dataset <- read.csv("./CAFO_all_data_new_June18.csv", 
#                    header = T, stringsAsFactors = FALSE)
# dataset <- read.csv("./CAFO_All_data_new_July26th.csv",
#                     header = T, stringsAsFactors = FALSE)

## CAFO_All_data_new_Aug17.csv: add RefID #4000 study 
##                             & change "Specific IgE to Common Allergens >0.35IU/mL" classified as upper and lower respiratory to "upper respiratory"

#dataset <- read.csv("./CAFO_All_data_new_Aug17.csv",
#                     header = T, stringsAsFactors = FALSE)

## CAFO_All_data_new_Aug21.csv: add Q fever into RefID #81 & RefID #203 ROB studies
dataset <- read.csv("../datasets/CAFO_All_data_new_Aug21.csv",
                    header = T, stringsAsFactors = FALSE)
### Add studies type into dataset 
### case control & cross-sectional studies:   
###                         #9, #81, #203, #690, #713, #743, #288, #648, #2417,#4000  
### cohort type studies:  
###                         #327, #452, #1552, #795, #1187
### experiment: 
###                        #775  
dataset$studyType <- rep("", nrow(dataset)) 
caseControlIndex <- which(dataset$Refid %in% c(9, 81, 203, 690, 713, 743, 288, 
                                               648, 2417, 4000, 1234))
dataset$studyType[caseControlIndex] <- "Case control and cross sectional studies"
cohortTypeIndex <- which(dataset$Refid %in% c(327, 452, 1552, 795, 1187))  
dataset$studyType[cohortTypeIndex] <- "Cohort studies"
experimentIndex <- which(dataset$Refid == 775)  
dataset$studyType[experimentIndex] <- "Experimental studies"

### add a column studyID: 
###              (RefID#775 --> 1); (RefID #81, RefID 203 -- >2); (RefID 288, RefID 648, RefID 743 --> 3);
###              (RefID#690 --> 4); (RefID #9 --> 5); (RefID 713 --> 6); (RefID 2417 -->7); (RefID 1187 -->8)
###              (RefID 795 --> 9); (RefID 327, RefID 452, RefID 1552 --> 10)
###              (RefID 4000 --> 11)
dataset$studyID <- rep(NA, nrow(dataset))
idx_1 <- which(dataset$Refid == 775)
dataset$studyID[idx_1] <- 1
idx_2 <- which(dataset$Refid %in% c(81, 203))
dataset$studyID[idx_2] <- 2
idx_3 <- which(dataset$Refid %in% c(288, 648, 743))
dataset$studyID[idx_3] <- 3
idx_4 <- which(dataset$Refid == 690)
dataset$studyID[idx_4] <- 4
idx_5 <- which(dataset$Refid == 9)
dataset$studyID[idx_5] <- 5
idx_6 <- which(dataset$Refid == 713)
dataset$studyID[idx_6] <- 6
idx_7 <- which(dataset$Refid == 2417)
dataset$studyID[idx_7] <- 7
idx_8 <- which(dataset$Refid == 1187)
dataset$studyID[idx_8] <- 8
idx_9 <- which(dataset$Refid == 795)
dataset$studyID[idx_9] <- 9
idx_10 <- which(dataset$Refid %in% c(327, 452, 1552))
dataset$studyID[idx_10] <- 10
idx_11 <- which(dataset$Refid %in% c(4000))
dataset$studyID[idx_11] <- 11

## copy ROB of measurement intervention for Cohort studies into ROB of measurement exposure for case control   
ROB_interventn_id <- which(dataset$ROB_measurementInterventions_paige != "")
dataset$ROB_measurementExposure_paige[ROB_interventn_id] <- dataset$ROB_measurementInterventions_paige[ROB_interventn_id]  
dataset$ROB_measurementExposure_second[ROB_interventn_id] <- dataset$ROB_measurementIntervention_second[ROB_interventn_id]
dataset$Description_measurementExpo_paige[ROB_interventn_id] <- dataset$Description_MeasurementIntervention_paige[ROB_interventn_id]


## add two columns: author(paperInfo) & paper published year(paperYear) 
dataset$paperInfo <- rep("", nrow(dataset))
dataset$paperYear <- rep(NA, nrow(dataset))
idx_9 <- which(dataset$Refid == 9)
dataset$paperInfo[idx_9] <- "Schinasi et al. 2014"
dataset$paperYear[idx_9] <- 2014
idx_81 <- which(dataset$Refid == 81)
dataset$paperInfo[idx_81] <- "Smit et al. 2013"
dataset$paperYear[idx_81] <- 2013
idx_203 <- which(dataset$Refid == 203)
dataset$paperInfo[idx_203] <- "Smit et al. 2012"
dataset$paperYear[idx_203] <- 2012
idx_288 <- which(dataset$Refid == 288)
dataset$paperInfo[idx_288] <- "Schulze et al. 2011"
dataset$paperYear[idx_288] <- 2011
idx_327 <- which(dataset$Refid == 327)
dataset$paperInfo[idx_327] <- "Schinasi et al. 2011"
dataset$paperYear[idx_327] <- 2011
idx_452 <- which(dataset$Refid == 452)
dataset$paperInfo[idx_452] <- "Horton et al. 2009"
dataset$paperYear[idx_452] <- 2009
idx_648 <- which(dataset$Refid == 648)
dataset$paperInfo[idx_648] <- "Radon et al. 2007"
dataset$paperYear[idx_648] <- 2007
idx_690 <- which(dataset$Refid == 690)
dataset$paperInfo[idx_690] <- "Hoopmann et al. 2006"
dataset$paperYear[idx_690] <- 2006
idx_713 <- which(dataset$Refid == 713)
dataset$paperInfo[idx_713] <- "Mirabelli et al. 2006"
dataset$paperYear[idx_713] <- 2006
idx_743 <- which(dataset$Refid == 743)
dataset$paperInfo[idx_743] <- "Radon et al. 2005"
dataset$paperYear[idx_743] <- 2005
idx_775 <- which(dataset$Refid == 775)
dataset$paperInfo[idx_775] <- "Schiffman et al. 2005"
dataset$paperYear[idx_775] <- 2005
idx_795 <- which(dataset$Refid == 795)
dataset$paperInfo[idx_795] <- "Avery et al. 2004"
dataset$paperYear[idx_795] <- 2004
idx_1187 <- which(dataset$Refid == 1187)
dataset$paperInfo[idx_1187] <- "Schiffman et al. 1995"
dataset$paperYear[idx_1187] <- 1995
idx_1552 <- which(dataset$Refid == 1552)
dataset$paperInfo[idx_1552] <- "Wing et al. 2013"
dataset$paperYear[idx_1552] <- 2013
idx_2417 <- which(dataset$Refid == 2417)
dataset$paperInfo[idx_2417] <- "Bullers et al. 2005"
dataset$paperYear[idx_2417] <- 2005
idx_4000 <- which(dataset$Refid == 4000)
dataset$paperInfo[idx_4000] <- "Feingold et al. 2012"
dataset$paperYear[idx_4000] <- 2012 

 
## add a column: abbreviation for ROB type: 
dataset$abbROB <- rep("",nrow(dataset))
idx_SEOO <- which(dataset$ROBNRSI_K == "SUBJECTIVE exposures / OBJECTIVE outcomes")
dataset$abbROB[idx_SEOO] <- "SE/OO"
idx_OEOO <- which(dataset$ROBNRSI_K %in% c("OBJECTIVE exposures / OBJECTIVE outcomes","OBJECTIVE EXPOSURE/OBJECTIVE OUTCOME"))
dataset$abbROB[idx_OEOO] <- "OE/OO"
idx_OESO <- which(dataset$ROBNRSI_K %in% c("OBJECTIVE exposures / SUBJECTIVE outcomes","OBJECTIVE EXPOSURE/SUBJECTIVE OUTCOME"))
dataset$abbROB[idx_OESO] <- "OE/SO"
idx_SESO <- which(dataset$ROBNRSI_K == "SUBJECTIVE exposures / SUBJECTIVE outcomes")
dataset$abbROB[idx_SESO] <- "SE/SO"


## risk of bias plot color 
col_ROB <- c("#fef0d9","#1a9850", "#91cf60","#fee08b","#fc8d59","#d73027")
names(col_ROB) <- c("Uncertain","Low", "Moderate", "High","Serious", "Critical")

## general setting for forest & ROB plot 
## number of Row for height display on the screen --> for output$forestROBPlot renderPlot options 
numRow <- 21
heightDefault <- 450
maxHeight <- 1400 


