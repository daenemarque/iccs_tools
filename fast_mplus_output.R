#run model and return fit values, yx standardized parameters and R2
fast_mplus_output <- function(model, inpfile) {
  results <- mplusModeler(model,
               modelout = inpfile,
               run = TRUE, 
               quiet = FALSE)
  mplus_output_list <- list(input = inpfile, 
                            fits = results$results$summaries[c("CFI_Mean","RMSEA_Mean", "SRMR_Mean")],
                            sdtyx = results$results$parameters$stdyx.standardized,
                            R2 = results$results$parameters$r2)
  return(mplus_output_list)
}

