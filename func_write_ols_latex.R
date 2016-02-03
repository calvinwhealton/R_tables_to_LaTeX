# funciton for writing OLS ANOVA tables into LaTeX tables
# output can include more regression results as defined by user
write_latex_ols <- function(reg_obj         # regression object
                            , name_tex      # name for latex table 
                            , name_var=NULL # names of variables
                            , caption=NULL  # caption for table, long caption
                            , capShort=NULL # short caption
                            , comments=NULL # additional comments for the table
                            , label = NULL  # label for the table
                            , dec_sums=2    # number of decimals for SSE, SSR, SST
                            , dec_rse = 3   # number of decimals for RSE
                            , dec_r = 4     # number of decimals for R2 and AdjR2
                            , dec_co = 4    # number of decimals for coefficients
                            ){
  
  # calculatin and rounding variables for output
  sse <- round(sum(residuals(reg_obj)^2),dec_sums) # sum of squares error
  ssr <- round(sum((predict(reg_obj)-mean(predict(reg_obj)))^2),dec_sums) # sum of squares regression
  sst <- round(sse+ssr,dec_sums)
  dof <- as.numeric(unlist(summary(reg_obj)["df"])["df2"])
  rse <- round(sqrt(sse/dof), dec_rse)
  r2 <- round(as.numeric(unlist(summary(reg_obj)["r.squared"])),dec_r)
  adjr2 <- round(as.numeric(unlist(summary(reg_obj)["adj.r.squared"])),dec_r)
  
  # creating file
  file.create(name_tex)
  sink(name_tex)
  
  # writing date, time, and comments
  time <- Sys.time()
  cat(c("%", format(time),"\n" ), sep="\t")
  cat(c("% output from R regression\n"))
  # printing additional comments
  if(length(comments != 0)){
    cat(c("%",comments,"\n"),sep=" ")
  }

  # writing caption and opening information
  if(length(caption) != 0){
    cat("\\begin{table}[h!]\n") # controling location of table
    # printing caption, option for both short and long captions
    if(length(capShort) == 0){
      cat(c("\\caption{",caption,"}"),sep='')
    }else{
      cat(c("\\caption[",capShort,']{',caption,"}"),sep='')
    }
    
    # adding label
    if(length(label) != 0){
      cat("\\label{",label,"}",sep='')
    }
    cat("\\begin{center}\n") # centering
    cat("\\begin{tabular}{l l l l l}\n") # justificaiton of columns
    
    cat("\n")
    cat("\\hline\n")
  }

  # writing data upper portion of table
  
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
  
  cat("\\hline\n") # end of upper portion of table
  
  # writing column names
  cat(c("Variable", "Est. Coeff.",  "Std. Error",  "t Stat.",  "Pr($>|t|$)"), sep = "\t&\t")
  cat(c("\\\\", "\n"))
  
  cat("\\hline\n") # begining of lower seciton of table
  
  # setting names as variables if names are not defined by user
  if(length(name_var) == 0){
    name_var <- names(coefficients(reg_obj))
  }
  
  # putting in columns for coefficients and calcs
  for(i in 1:length(name_var)){
    cat(c(name_var[i], format(coefficients(summary(reg_obj))[i],scientific=TRUE,digits=dec_co), format(coefficients(summary(reg_obj))[i+length(name_var)],scientific=TRUE,digits=dec_co), format(coefficients(summary(reg_obj))[i+2*length(name_var)],scientific=TRUE,digits=dec_co), format(coefficients(summary(reg_obj))[i+3*length(name_var)],scientific=TRUE,digits=dec_co)), sep="\t&\t")
    cat(c("\\\\", "\n"))
  }
  
  # adding statements to end table
  if(length(caption) != 0){
    # closing information for table
    cat("\\hline")
    cat("\\end{tabular}\n")
    cat("\\end{center}\n")
    cat("\\end{table}")
  }
  
  # closing connection so output is no longer written to file
  closeAllConnections()
}

