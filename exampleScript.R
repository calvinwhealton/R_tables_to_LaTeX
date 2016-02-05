# setting working directory
setwd('/Users/calvinwhealton/GitHub/R_tables_to_LaTeX')

# loading librarys
library('quantreg')

# loading source files
source('func_write_ols_latex.R')
source('func_write_nls_latex.R')
source('func_write_qreg_latex.R')
source('func_write_df_latex.R')

# tables using lm() for ordinary least squares regression (OLS)----
# generating some random data for the OLS model
set.seed(10)
x1 <- runif(100,0,100) # first predictor
x2 <- runif(100,0,100) # second predictor
y <- 5 + 0.5*x1 + 2*x2 + rnorm(100,0,1)
exData <- as.data.frame(cbind(y,x1,x2))

# ordinary least squares (OLS) model
ols.mod <- lm(y ~ x1 + x2, data=exData)
write_latex_ols(reg_obj=ols.mod       # regression object
                , name_tex='ols1.tex' # name for latex table 
)

write_latex_ols(reg_obj=ols.mod                # regression object
                , name_tex='ols2.tex'          # name for latex table 
                , name_var=c('b0','bx1','bx2') # names of variables
                , caption="A table that is entirely written from R including captions (long and short) and label."  # caption for table, long caption
                , capShort="OLS table all from R"   # short caption
                , comments="This is an example for writing an entire table from R"# additional comments for the table
                , label = "ols2"               # label for the table
)


ols.mod.noCon <- lm(y ~ x1 + x2 -1, data=exData)
write_latex_ols(reg_obj=ols.mod.noCon                # regression object
                , name_tex='ols3.tex'                # name for latex table 
                , name_var=c('cx1','cx2')            # names of variables
                , caption="A table that is entirely written from R including captions (long and short) and label. There is no constant in this model."  # caption for table, long caption
                , capShort="OLS table all from R, no Constant"                       # short caption
                , comments="This is an example for writing an entire table from R"   # additional comments for the table
                , label = "ols3"                     # label for the table
                , noCon = TRUE                       # no constant in regression is true
)

# tables using nls() for nonlinear least squares regression (NLS)----
# generating some random data for the OLS model
set.seed(10)
x1 <- runif(10,0,100) # first predictor
x2 <- runif(10,1,100) # second predictor
y <- 5 + exp(0.1*x1) + 2*log(x2) + rnorm(100,0,1)
exData <- as.data.frame(cbind(y,x1,x2))

# nonlinear least squares (NLS) model
nls.mod <- nls(y ~ x1 + x2, data=exData)
write_latex_nls(reg_obj=nls.mod       # regression object
                , name_tex='nls1.tex' # name for latex table 
)

write_latex_nls(reg_obj=nls.mod                # regression object
                , name_tex='nls2.tex'          # name for latex table 
                , name_var=c('$\\mu$','$\\alpha$','$\\beta$') # names of variables
                , caption="Nonlinear regression table written from R. Model is $y = \\mu + \\exp(\\alpha x_1) + \\beta \\ln(x_2)$. The caption includes mathematical notation."  # caption for table, long caption
                , capShort="NLS table all from R"   # short caption
                , comments="This is an example for writing an entire table from R including mathematical notation"# additional comments for the table
                , label = "nls2"               # label for the table
)



# tables using rq() for quantile regression----
# generating some random data for the quantile model
set.seed(10)
x1 <- runif(100,0,100) # first predictor
x2 <- runif(100,0,100) # second predictor
y <- 5 + 0.5*x1 + 2*x2 + rnorm(100,0,1)
exData <- as.data.frame(cbind(y,x1,x2))

# quantile regression model
qreg.mod <- rq(y ~ x1 + x2, data=exData)
write_latex_qreg(reg_obj=qreg.mod       # regression object
                ,name_tex='qreg1.tex' # name for latex table 
                ,ys = y
                ,xs = cbind(x1,x2)
)

write_latex_qreg(reg_obj=qreg.mod# regression object
                 ,ys = y
                 ,xs = cbind(x1,x2)
                , name_tex='qreg2.tex'          # name for latex table 
                , name_var=c('b0','bx1','bx2') # names of variables
                , caption="A quantile regression table from R."  # caption for table, long caption
                , capShort="Quantile regression table all from R"   # short caption
                , comments="This is an example for writing an entire table from R"# additional comments for the table
                , label = "qreg"               # label for the table
)


# tables for data frames----
# generating some random data for the quantile model
set.seed(10)
x1 <- runif(10,0,100) # first predictor
x2 <- runif(10,0,100) # second predictor
y <- 5 + 0.5*x1 + 2*x2 + rnorm(10,0,1)
exData <- as.data.frame(cbind(y,x1,x2))

# writing LaTeX tables
write_latex_df(exData
                , 'df1.tex'
)
write_latex_df(exData
               , 'df2.tex'
               , name_var=c('A','B','C') # names of variables
               , caption='A data frame with different numbers of significant figures. There is no short caption.'  # caption for table, long caption
               , label='df2'    # label for the table
               , dec_sf=c(2,4,6)      # number of decimals
)