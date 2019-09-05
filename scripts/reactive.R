## get exposure variable categorized in broad way: direct/surrogate 
expo_var_board_str <- reactive({
  if(is.null(input$class)) return(NULL)
  dataset %>%
    filter(Categorized.class == input$class) %>%
    select(Expo.Very.board) %>%
    unique() %>%
    arrange() %>%
    .[, "Expo.Very.board"]
})

## get exposure variable categorized in a bit narrow way
expo_var_naro_str <- reactive({
  #  if(is.null(input$class) || is.null(input$expo_var_board)) return(NULL)
  dataset %>%
    filter(Categorized.class == input$class, Expo.Very.board %in% input$expo_var_board) %>%
    select(Expo.BitNarrow) %>%
    unique() %>%
    arrange() %>%
    .[, "Expo.BitNarrow"]
  
})

## get effect measure method based previous queries
study_measure_type_str <- reactive({
  #     if(is.null(input$class) || is.null(input$expo_var_board) || is.null(input$expo_var_naro)) return(NULL)
  dataset %>%
    filter(Categorized.class %in% input$class, Expo.Very.board %in% input$expo_var_board, 
           Expo.BitNarrow %in% input$expo_var_naro) %>%
    select(Effect.measure) %>%
    unique() %>%
    arrange() %>%
    .[, "Effect.measure"]
})


measureMethod <- reactive({
  ## get Effect measure method 
#   if(is.null(input$effect_measure_method)) return()
   measure.Method <- as.character(input$effect_measure_method)
   return(measure.Method)
})


db <- reactive({
  ## extract dataset based on all four queries
#    if(is.null(input$class) || is.null(input$expo_var_board) || is.null(input$expo_var_naro)) return()
  
    DB <- select_df(input$class, input$expo_var_board, input$expo_var_naro)   
    df_tmp <- DB   
    df <- df_tmp %>%
      filter(Effect.measure == measureMethod())
    
    ## arrange the order of dataset: for each outcome & corresponding exposure group, arrange the dataset by putting the reference at the first place if effect measure is OR
    measure_method <- measureMethod()
    if( measure_method == "OR"){
      df <- df %>%
        group_by(paperYear, Outcome.variable, Exposure.measure) #%>%
     #   arrange(abs(as.numeric(Effect.measure.1) - 1.0))    
    }else{
      df <- df %>%
        group_by(paperYear, Outcome.variable, Exposure.measure) %>%
        arrange(paperYear)
    }
    
    return(df)
})


studyType <- reactive({
  d <- db()
#  if(nrow(d) == 0) return(NULL)
  return(unique(d$studyType))
})

isExperimt <- reactive({
  ## Only mixed cases return TRUE
  ## Otherwise: return FALSE: i.e. not including experimental studies or only experimental studies 
  df_tmp <- db()
#  if(nrow(df_tmp) == 0) return(NULL)
  study_type <- studyType()
  if("Experimental studies" %in% study_type){
    if(length(study_type) > 1 ){
      ## mixed case 
      return(TRUE)
    }else{
      ## only experimental case 
      return(FALSE)
    }
  }else{
    return(FALSE)
  }
})

getOnlyExperimtDB <- reactive({
#  if(is.null(input$class) || is.null(input$expo_var_board) || is.null(input$expo_var_naro) || is.null(input$effect_measure_method)) return()
  dataset %>% 
    filter(Categorized.class %in% input$class, Expo.Very.board %in% input$expo_var_board, 
           Expo.BitNarrow %in% input$expo_var_naro, Effect.measure %in% input$effect_measure_method) %>%           
    select(Refid, paperInfo, paperYear,
           Outcome.variable, 
           Exposure.measure, 
           Subcategory, 
           Effect.measure, Effect.measure.1, 
           Lower, Upper, 
           studyType,
           ROBNRSI_K,
           abbROB,
           studyID,
           ROB_randomsequencegeneration_paige,ROB_randomsequencegeneration_second,Rationale_Allocationmethod_paige,
           ROBAllocationconcealment_paige,ROBAllocationconcealment_second,Rationale_Allocation_concealment_paige,
           ROB_Blindingowners_personnel_paige,ROB_Blindingowners_personnel_second,Rationale_Blindingowners_personnel_paige,
           ROB_Blindingoutcomeassessors_paige,ROB_Blindingoutcomeassessors_second,RationaleBlindingoutcomeassessors_paige,
           ROB_Incompleteoutcomedata_paige,ROB_Incompleteoutcomedata_second,Rationale_Incompleteoutcomedata_paige,
           ROBSelectivereporting_paige,ROBSelectivereporting_second,RationaleSelectivereporting_paige,
           ROB_Other_sources_of_bias_paige,ROB_Other_sources_of_bias_second,Rationale_Other_sources_of_bias_paige    
    )
})
