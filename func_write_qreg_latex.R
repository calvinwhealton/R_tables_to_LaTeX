# funciton for writing OLS ANOVA tables into LaTeX tables----
# output can include more regression results as defined by user
write_latex_qreg <- function(reg_obj        # regression object
                            , xs            # columns for x vectors
                            , ys            # column for y vector
                            , name_tex      # name for latex table 
                            , name_var=NULL # names of variables
                            , caption=NULL  # caption for table, long caption
                            , capShort=NULL # short caption
                            , comments=NULL # additional comments for table
                            , label=NULL    # table for table
                            , RegQ = 0.5    # quantile regressed, median default
                            , dec_ae = 3    # number of decimals for RSE
                            , dec_r = 4     # number of decimals for R2 and AdjR2
                            , dec_co = 4    # number of decimals for coefficients
                            , boots = 5000  # number of bootstrap replicates
                            , noCon = FALSE # include constant
                            ){
  
  # setting names as variables if names are not defined by user
  if(length(name_var) == 0){
    name_var <- names(coefficients(reg_obj))
  }
  
  # regression output
  dof <- length(residuals(reg_obj))-length(name_var) # degrees of freedom
  pseudosse <- sum(residuals(reg_obj)*(RegQ-(residuals(reg_obj)<0))) # pseudo sse, assumed median regression
  const_mod <- rq(ys~1, tau=0.5) # fitting constant model
  pseudosst <- sum(residuals(const_mod)*(RegQ-(residuals(const_mod)<0))) # pseudo sst
  r2 <- round((1 - pseudosse/pseudosst) ,dec_r)
  
  # quantile output calculations
  quants <- round(as.numeric(quantile(abs(residuals(reg_obj)),c(0.1,0.25,0.5,0.75,0.9))),dec_ae)
  mae <- round(mean(abs(residuals(reg_obj))),dec_ae)
  
  # anova lower half of table
  set.seed(1)
  if(noCon == TRUE){
    anova_mat <- matrix(0,nrow = ncol(xs),ncol=4)
    boot_coeff <- boot.rq(xs, ys, tau = RegQ, R = boots, bsmethod = "xy")
  }else{
    anova_mat <- matrix(0,nrow = ncol(xs)+1,ncol=4)
    boot_coeff <- boot.rq(cbind(1,xs), ys, tau = RegQ, R = boots, bsmethod = "xy")
  }
  coeff_se <- sqrt(diag(cov(boot_coeff)))
  
  for(i in 1:nrow(anova_mat)){
    anova_mat[i,1] <- coefficients(reg_obj)[i] # coefficient
    anova_mat[i,2] <- coeff_se[i] # standard error
    anova_mat[i,3] <- anova_mat[i,1]/anova_mat[i,2] # t statistic
    anova_mat[i,4] <- pnorm(-1*abs(anova_mat[i,3]))*2 # p value
  }
  
  # creating file
  file.create(name_tex)
  sink(name_tex)
  
  # writing date, time, and comments
  time <- Sys.time()
  cat(c("%", format(time) ), sep="\t")
  cat(c("\n"))
  cat("% output from R quantile regression\n")
  cat(c("% Quantile of interest is ", RegQ), sep='')
  cat(c("\n"))
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
  
  
  # writing initial data
  
  # writing line 1----
  cat(c("AE 0.10", quants[1],  "",  "DoF", dof), sep = "\t&\t")
  cat(c("\\\\", "\n"))
  
  # writing line 2----
  cat(c("AE 0.25", quants[2],  "",  "MAE",  mae), sep = "\t&\t")
  cat(c("\\\\", "\n"))
 
  # writing line 3----
  cat(c("AE 0.50",  quants[3],  "",  "pseudoR2",  r2), sep = "\t&\t")
  cat(c("\\\\", "\n"))
  
  # writing line 4----
  cat(c("AE 0.75", quants[4],  "",  "Reps.",  boots), sep = "\t&\t")
  cat(c("\\\\", "\n"))
      
  # writing line 5----
  cat(c("AE 0.90", quants[5],  "",  "",  ""), sep = "\t&\t")
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
    
    cat(c(name_var[i], format(anova_mat[i,1],scientific=TRUE,digits=dec_co), format(anova_mat[i,2],scientific=TRUE,digits=dec_co), format(anova_mat[i,3],scientific=TRUE,digits=dec_co), format(anova_mat[i,4],scientific=TRUE,digits=dec_co)), sep="\t&\t")
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