getInputsForForestplot <- function(d, measure_method, numROB = 6){
  ##### Argus: d -- > selected dataset 
  #####        measure_method: selected measure method 
  #####        numROB: number of columns on the RHS shown on the Foresplot 
  ##### Return: list for passing into forestPlot function 
  
  ## Extract ROB matrix prepaired for forest plot 
  M_first <- d%>%
    select(starts_with("ROB")) %>% 
    select(ends_with("paige"))
  
  ## extract column starts with ROB 
  start_idx <- grep("ROB", names(M_first))[1]
  M_first <- M_first[, start_idx:ncol(M_first)]
  
  M_second <- d%>%
    select(starts_with("ROB")) %>%
    select(ends_with("second"))
  
  start_idx_second <- grep("ROB", names(M_second))[1]
  M_second <- M_second[, start_idx_second:ncol(M_second)]
  
  M_first <- as.matrix(M_first)
  M_second <- as.matrix(M_second)
  
  
  ### Extract Effect size, corresponding lower/upper bound 
  mean <- as.numeric(d$Effect.measure.1)
  lower <- as.numeric(d$Lower)
  upper <- as.numeric(d$Upper)
  
#   ## for forest plot x axis trucated 
#   qu_min_max = summary(c(upper, lower), na.rm  = T)[c(1,6)]
#   if(all(is.na(upper))){
#     clip <- range(mean)
#   }else{
#     if(qu_min_max[2] > 10){
#       qu_min_max[2] = 10
#     }
#     clip = range(c(qu_min_max, mean), na.rm = T)
#   }
  
  ### prepare for LHS on the forestplot, i.e. labeltext matrix for passing into forestplot  
  ### Also put outcome variable/exposure variable category used in ROB into LHS shown in forestplot 
  if(measure_method == "OR" || measure_method == "beta" || measure_method == "PR"){     
    if(any(d$Subcategory != "")){
      ## labeltext should be matrix form for passing into Forestplot function; 1st row in labeltext matrix is summary(row name)   
      labeltext <- cbind(d$Refid, d$Outcome.variable, d$Exposure.measure, d$Subcategory, mean, lower,  upper)
      labeltext <- rbind(c("Refid","Outcome variable",  "Exposure variable", "Subcategory", "ES", "Lower","Upper"),labeltext)
    }else{
      ## no subcategory --> don't extract subcategory 
      labeltext <- cbind(d$Refid, d$Outcome.variable, d$Exposure.measure, mean, lower,  upper)
      labeltext <- rbind(c("Refid",  "Outcome variable",  "Exposure variable","ES", "Lower","Upper"),labeltext)
    }
  }
  
  if(measure_method == "Mean difference" || measure_method == "beta p value" || measure_method == "OR p value"){
    ## don't extract lower/upper in this case 
    if(any(d$Subcategory != "")){
      labeltext <- cbind(d$Refid, d$Outcome.variable, d$Exposure.measure, d$Subcategory, mean)
      labeltext <- rbind(c("Refid","Outcome variable",  "Exposure variable", "Subcategory","ES"),labeltext)
    }else{
      labeltext <- cbind(d$Refid, d$Outcome.variable, d$Exposure.measure,mean)
      labeltext <- rbind(c("Refid","Outcome variable",  "Exposure variable", "ES"),labeltext)
    }
  }
  
  ## is.summary: vector label if certain is for summary or not--> passing into Forestplot  for assigning different font/format 
  is.summary <- rep(FALSE, nrow(d))
  ## 1st row is summary 
  is.summary <- c(TRUE, is.summary)
  
  ## due to 1st row is summary, adjust content in other columns in LHS 
  M_first <- rbind(rep("", numROB), M_first)
  M_second <- rbind(rep("", numROB), M_second)
  lower <- c(NA, lower)
  upper <- c(NA, upper)
  mean <- c(NA, mean)
  lower <- round(lower, digits = 3)
  upper <- round(upper, digits = 3)
  mean <- round(mean, digits = 3)
  
  return(list(labeltext = labeltext, is.summary = is.summary, 
              M_first = M_first, M_second = M_second, 
              mean = mean, lower = lower, upper = upper ))
}
