select_df <- function(clas, expoBoard, expoNarrow){
  ## select subdataset to output 
  ## Args: outcome categorized class, exposure broad group, exposure narrow group, study type 
  aux <- dataset %>% 
    filter(Categorized.class %in% clas, Expo.Very.board %in% expoBoard, Expo.BitNarrow %in% expoNarrow) %>%           
    select(Refid, paperInfo, paperYear,
           Categorized.class, 
           Outcome.variable, 
           Expo.Very.board, Expo.BitNarrow,
           Exposure.measure, 
           Subcategory, 
           Effect.measure, Effect.measure.1, 
           Lower, Upper, 
           studyType,
           ROBNRSI_K,
           abbROB,
           studyID,
           ROB_confounding_paige, ROB_confounding_second, Description_Confounding_paige,
           ROB_selection_paige, ROB_selection_second, Description_selection_paige,
           ROB_measurementExposure_paige, ROB_measurementExposure_second, Description_measurementExpo_paige,
           ROB_missingData_paige, ROB_missingData_second, Description_missingData_paige,
           ROB_measureOutcome_paige, ROB_measureOutcome_second, Description_MeasurementOutcome_paige,
           ROB_SelectionofReportedResult_paige, ROB_SelectionofReportedResult_second, GiveSupportforJudgment_paige,
           ROB_overall_paige, ROB_overall_second, Description_ovrall_paige
    ) 
  return(aux)
}

getROBClass_nonExpr <- function(d){
  ## get ROB description display dataset for non experiment studies 
  d %>%
    select(
      studyID,
      abbROB,
      studyType,
      Description_Confounding_paige,
      Description_selection_paige,
      Description_measurementExpo_paige,
      Description_missingData_paige,
      Description_MeasurementOutcome_paige,
      GiveSupportforJudgment_paige,
      Description_ovrall_paige
    ) %>%
    group_by(studyID, abbROB) %>%
    slice(1)  ## within each group(study ID, abbROB), select the first 1 record
 #   unique()
   
}

getROBClass_Expr <- function(d){
  ## get ROB description display dataset for experiment study case 
  d %>%
    select(studyID,
           abbROB,
           studyType,
           Rationale_Allocationmethod_paige,
           Rationale_Allocation_concealment_paige,
           Rationale_Blindingowners_personnel_paige,
           RationaleBlindingoutcomeassessors_paige,
           Rationale_Incompleteoutcomedata_paige,
           RationaleSelectivereporting_paige,
           Rationale_Other_sources_of_bias_paige    
    ) %>%
    group_by(studyID, abbROB) %>%
    unique()
}