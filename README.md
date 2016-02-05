# R_tables_to_LaTeX

This repository contains code used to write tables, specifically regression tables and data frames, from R to LaTeX. They are designed to be easily hacked to change formatting and layout as the user desires. I wrote the early versions of most of these functions when I was working on my Master's thesis, which had many pages of regression tables and datasets. Rerunning code and knowing that all the tables would update was nice.

There are functions for linear models (models fit with `lm()`), nonlienar least squares models (models fit with `nls()`), and linear quantile regression models (models fit with `rq()` in the `quantreg` package). Code for writing data frames is for simple data frames. See 'exampleScript.R' for examples of the different types of tables that can be written and 'sampleFile.tex' and 'sampleFile.pdf' for how the tables are included in a LaTeX document. Most of the functions have options to include long and short captions, as illustrated in the sample files.

This code is offered under the MIT license. If you have comments/suggestions on other features or if you want some modifications that you made to these functions to be included, then please contact me.
