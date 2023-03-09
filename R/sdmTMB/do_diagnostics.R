do_diagnostics <- function(dir, data){
 
  write.csv(tidy(fit), 
            file = file.path(dir, "parameters.csv")) 
  
  write.csv(rbind(c("pos_def_hessian", fit$pos_def_hessian),
                  c("bad_eig", fit$bad_eig)), 
            file = file.path(dir, paste0("pos_def_hessian_", fit$pos_def_hessian, ".csv")))
  
  get_diag_tables(
    fit = fit, 
    dir = dir
  )
  
  fit$data$residuals <- residuals(fit)
  
  plot_qq(data = fit, 
          dir = dir)
  
  plot_residuals(
    data = fit, 
    dir = dir, 
    nrow = 3, ncol = 4)
  
  plot_fixed_effects_para(
    data = fit, 
    dir = dir) 
  
  save(data, file = file.path(dir, "fit.rdata"))
  
}