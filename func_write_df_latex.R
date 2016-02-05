# funciton for writing dataframe into LaTeX tables----
write_latex_df <- function(df              # dataframe
                            , name_tex      # name for LaTeX table 
                            , name_var=NULL # names of variables
                            , caption=NULL  # caption for table, long caption
                            , capShort=NULL # short caption
                            , comments=NULL # additional comments
                            , label=NULL    # label for the table
                            , dec_sf=2      # number of decimals
                            ){
  
  # creating file
  file.create(name_tex)
  sink(name_tex)
  
  # writing date, time, and comments
  time <- Sys.time()
  cat(c("%", format(time) ), sep="\t")
  cat(c("\n"))
  cat(c("% R data frame"))
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
    cat(c("\\begin{tabular}{",rep("l ",ncol(df)),"}\n"),sep='') # justificaiton of columns
    
    cat("\n")
    cat("\\hline\n")
  }
  
  # setting names as variables if names are not defined by user
  if(length(name_var) == 0){
    name_var <- names(df)
  }
  
  # writing data
  # writing column names
  cat(name_var, sep = "\t&\t")
  cat(c("\\\\", "\n"))
  
  cat("\\hline") # begining of lower seciton of table
  cat(c("\n"))
  
  if(length(dec_sf) == 1){
    dec_sf <- rep(dec_sf,length(name_var))
  }
  
  # writing the values
  for(i in 1:nrow(df)){
    for(j in 1:ncol(df)){
      cat(format(df[i,j],scientific=TRUE,digits=dec_sf[j]))
      if(j != ncol(df)){
        cat("\t&\t") # spacing with a tab
      }
    }
      cat(c("\\\\", "\n")) # returning to new line
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

