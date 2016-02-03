# setting working directory
setwd('/Users/calvinwhealton/GitHub/R_tables_to_LaTeX')

# loading librarys
library('quantreg')

# loading source files
source('func_write_ols_latex.R')

# generating some random data
set.seed(10)
x1 <- runif(100,0,10) # first predictor
x2 <- runif(100,0,10) # second predictor
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
