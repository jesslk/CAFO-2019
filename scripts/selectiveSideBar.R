###################### Select input sidebar panel #######################
### get class_var   
class_var <- unique(dataset$Categorized.class)
class_var <- sort(class_var)
output$class <- renderUI({
  if(is.null(class_var)) return(NULL)
  selectInput("class",
              "Outcome class",
              choices = class_var,
              selected = class_var[1])
})

## get exposure variable categorized in broad way: direct/surrogate 
output$expo_var_board <- renderUI({  ## Note: not allowed multiple choices 
#  if(is.null(expo_var_board_str())) return(NULL)
  selectInput("expo_var_board",
              "Broad grouped Exposure variable",
              choices = expo_var_board_str(),
              selected = expo_var_board_str()[1])
})

## get exposure variable categorized in a bit narrow way
output$expo_var_naro <- renderUI({
#  if(is.null(expo_var_naro_str())) return(NULL)
  selectInput("expo_var_naro",
              "Narrow grouped exposure variable(s)",
              choices = expo_var_naro_str(),
              multiple = T,
              selected = expo_var_naro_str()[1])
}) 

## get effect measure method based previous queries
output$effect_measure_method <- renderUI({
#  if(is.null(study_measure_type_str())) return(NULL)
  selectInput("effect_measure_method",
              "Effect size (ES) measure method",
              choices = study_measure_type_str(),
              selected = study_measure_type_str()[1]
  )
}) 

## display experiment study ROB if experiment mixed with other study type
output$experimentGoButton <- renderUI({
  if(isExperimt()){
    actionButton("experimtButton", "Experimental", class = "btn-block btn-info")
  }
})


