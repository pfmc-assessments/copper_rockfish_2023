

get_model_quants <- function(repfile, write_csv = TRUE){


  x <- r4ss::SSsummarize(list(repfile), verbose = FALSE)
  
  quants <- cbind(
    as.numeric(x$likelihoods[x$likelihoods$Label == "TOTAL",1]), 
    as.numeric(x$likelihoods[x$likelihoods$Label == "Survey",1]), 
    as.numeric(x$likelihoods[x$likelihoods$Label == "Length_comp",1]),
    as.numeric(x$likelihoods[x$likelihoods$Label == "Age_comp",1]), 
    as.numeric(x$pars[x$pars$Label == "SR_LN(R0)", 1]), 
    as.numeric(x$SpawnBio[x$SpawnBio$Label == "SSB_Virgin", 1]),
    as.numeric(x$SpawnBio[x$SpawnBio$Label == "SSB_2023", 1]),
    as.numeric(x$Bratio[x$Bratio$Label == "Bratio_2023", 1]),
    as.numeric(x$pars[x$pars$Label == "NatM_uniform_Fem_GP_1", 1]),
    as.numeric(x$pars[x$pars$Label == "L_at_Amax_Fem_GP_1", 1]),
    as.numeric(x$pars[x$pars$Label == "NatM_uniform_Mal_GP_1", 1]),
    as.numeric(x$pars[x$pars$Label == "L_at_Amax_Mal_GP_1", 1])
  )
  
  colnames(quants) <- c(
    "Total NLL",
    "Survey NLL",
    "Length NLL",
    "Age NLL",
    "log(R0)",
    "SB Virgin",
    "SB 2023",
    "Fraction Unfished 2023",
    "Natural Mortality - Female",
    "Length at Amax - Female",
    "Natural Mortality - Male",
    "Length at Amax - Male"
    )
  
  if(write_csv){
   write.csv(quants, file = file.path(repfile$inputs$dir, "model_quants.csv"),
             row.names = FALSE) 
  }
  return(quants)
}