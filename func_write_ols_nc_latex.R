# funciton for writing OLS ANOVA tables into LaTeX tables with no constant----
# output can include more regression results as defined by user
write_latex_ols_nc <- function(reg_obj        # regression object
                            , name_tex      # name for latex table 
                            , name_var       # names of variables
                            , dec_sums=2    # number of decimals for SSE, SSR, SST
                            , dec_rse = 3   # number of decimals for RSE
                            , dec_r = 4     # number of decimals for R2 and AdjR2
                            , dec_co = 4){
  
  # calculatin and rounding variables for output
  sse <- round(sum(residuals(reg_obj)^2),dec_sums) # sum of squares error
  ssr <- round(sum((predict(reg_obj)-mean(predict(reg_obj)+residuals(reg_obj)))^2),dec_sums) # sum of squares regression
  sst <- round(sum((predict(reg_obj)+residuals(reg_obj)-mean(predict(reg_obj)+residuals(reg_obj)))^2),dec_sums) # sum of squares regression
  dof <- as.numeric(unlist(summary(reg_obj)["df"])["df2"])
  rse <- round(sqrt(sse/dof), dec_rse)
  r2 <- round(1 - sse/sst,dec_r)
  adjr2 <- round(1 - (1-r2)*(length(residuals(reg_obj)-1))/dof,dec_r)
  
  # creating file
  file.create(name_tex)
  sink(name_tex)
  
  # writing date, time, and comments
  time <- Sys.time()
  cat(c("%", format(time) ), sep="\t")
  cat(c("\n"))
  cat(c("% output from R regression"))
  cat(c("\n"))
  
  # writing initial data
  
  # writing line 1
  cat(c("SSE", sse,  "",  "DoF",  dof), sep = "\t&\t")
  cat(c("\\\\", "\n"))
  
  # writing line 2
  cat(c("SSR", ssr,  "",  "R2",  r2), sep = "\t&\t")
  cat(c("\\\\", "\n"))
  
  # writing line 3
  cat(c("SST", sst,  "",  "Adj.R2",  adjr2), sep = "\t&\t")
  cat(c("\\\\", "\n"))
  
  # writing line 4
  cat(c("RSE", rse,  "",  "",  ""), sep = "\t&\t")
  cat(c("\\\\", "\n"))
  
  cat("\\hline") # end of upper portion of table
  cat(c("\n"))
  
  # writing column names
  cat(c("Variable", "Est. Coeff.",  "Std. Error",  "t Stat.",  "Pr($>|t|$)"), sep = "\t&\t")
  cat(c("\\\\", "\n"))
  
  cat("\\hline") # begining of lower seciton of table
  cat(c("\n"))
  
  # putting in columns for coefficients and calcs
  for(i in 1:length(name_var)){
    
    cat(c(name_var[i], format(coefficients(summary(reg_obj))[i],scientific=TRUE,digits=dec_co), format(coefficients(summary(reg_obj))[i+length(name_var)],scientific=TRUE,digits=dec_co), format(coefficients(summary(reg_obj))[i+2*length(name_var)],scientific=TRUE,digits=dec_co), format(coefficients(summary(reg_obj))[i+3*length(name_var)],scientific=TRUE,digits=dec_co)), sep="\t&\t")
    cat(c("\\\\", "\n"))
  }
  
  # closing connection so output is no longer written to file
  closeAllConnections()
}

