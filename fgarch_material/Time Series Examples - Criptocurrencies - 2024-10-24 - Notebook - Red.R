############################################################## References ##################################################################
#
# Peter J. Brockwell and Richard A. Davis - Introduction to Time Series and Forecasting (Third Edition)
# Springer Texts in Statistics - Springer Verlag
#
# Robert H. Shumway, David S. Stoffer - Time Series Analysis and Its Applications (with R Examples) 4th Edition
# Springer Texts in Statistics - Springer Verlag
# https://www.stat.pitt.edu/stoffer/tsa4/tsa4.pdf
#
# Rob J Hyndman and George Athanasopoulos - Forecasting: Principles and Practice
# Monash Univeristy, Australia
# https://otexts.com/fpp2/
#
# Stephane Guerrier, Roberto Molinari, Haotian Xu and Yuming Zhang - Applied Time Series Analysis with R
# https://smac-group.github.io/ts/index.html
#

############################################################################################################################################
################################################################## RENV ###############################################################
############################################################################################################################################
#renv::init()
#renv::snapshot()
renv::activate()
############################################################################################################################################
################################################################## Libraries ###############################################################
############################################################################################################################################
# installed.packages()
# Reading libraries
# https://www.r-project.org/other-docs.html
library(base)
# https://rdrr.io/r/#base
# https://www.math.ucla.edu/~anderson/rw1001/library/base/html/00Index.html
#
library(utils)
#
library(stats)
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/00Index.html
#
# library(boot)
if (!require("boot", quietly = TRUE)) install.packages("boot")
#
############################################################################################################################################
# library(zoo)
# https://cran.r-project.org/web/packages/zoo/zoo.pdf
if (!require("zoo", quietly = TRUE)) install.packages("zoo")
#
# library(quantmod)
# https://cran.r-project.org/web/packages/quantmod/quantmod.pdf
# https://www.rdocumentation.org/packages/quantmod/versions/0.4-15/topics/chartSeries
# http://www.quantmod.com/
if (!require("quantmod", quietly = TRUE)) install.packages("quantmod")
#
# library(xts)
if (!require("xts", quietly = TRUE)) install.packages("xts")
#
# library(tibble)
if (!require("tibble", quietly = TRUE)) install.packages("tibble")
#
# library(dplyr)
if (!require("dbplyr", quietly = TRUE)) install.packages("dbplyr")
#
# library(dbplyr)
if (!require("dbplyr", quietly = TRUE)) install.packages("dbplyr")
#
# library(numbers)
if (!require("numbers", quietly = TRUE)) install.packages("numbers")
#
# library(ggplot2)
if (!require("ggplot2", quietly = TRUE)) install.packages("ggplot2")
#
# library(TSA)
# if (!require("TSA", quietly = TRUE)) install.packages("TSA")
# It appears that loading the TSA package interferes with the proper functioning of the FitAR::LjungBoxTest() function.
# 
# library(urca)
# https://cran.r-project.org/web/packages/urca/urca.pdf
if (!require("urca", quietly = TRUE)) install.packages("urca")
#
############################################################################################################################################
# # library(remotes)
# if (!require("remotes", quietly = TRUE)) install.packages("remotes")
# #
# library(FitAR)
# if (!require("FitAR", quietly = TRUE)) remotes::install_github("cran/FitAR")
if (!require("FitAR", quietly = TRUE)) install.packages("FitAR")
#
# library(portes)
if (!require("portes", quietly = TRUE)) install.packages("portes")
#
############################################################################################################################################
# library(DescTools)
if (!require("DescTools", quietly = TRUE)) install.packages("DescTools")
#
# library(lmtest)
# https://cran.r-project.org/web/packages/lmtest/lmtest.pdf
# https://cran.r-project.org/web/packages/lmtest/vignettes/lmtest-intro.pdf
if (!require("lmtest", quietly = TRUE)) install.packages("lmtest")
#
# library(skedastic)
# https://cran.r-project.org/web/packages/skedastic/skedastic.pdf
# https://www.rdocumentation.org/packages/skedastic/versions/2.0.1
if (!require("skedastic", quietly = TRUE)) install.packages("skedastic")
#
# library(olsrr)
# https://cran.r-project.org/web/packages/olsrr/olsrr.pdf
# https://cran.r-project.org/web/packages/olsrr/vignettes/intro.html
# https://cran.r-project.org/web/packages/olsrr/vignettes/heteroskedasticity.html
if (!require("olsrr", quietly = TRUE)) install.packages("olsrr")
#
# library(whitestrap)
# https://cran.r-project.org/web/packages/whitestrap/whitestrap.pdf
# https://cran.r-project.org/web/packages/whitestrap/readme/README.html
if (!require("whitestrap", quietly = TRUE)) install.packages("whitestrap")
###########################################################################################################################################
# library(tseries)
# https://cran.r-project.org/web/packages/tseries/tseries.pdf
# https://rdrr.io/cran/tseries/man/garch.html
if (!require("tseries", quietly = TRUE)) install.packages("tseries")
# #
# library(fGarch)
# https://cran.r-project.org/web/packages/fGarch/fGarch.pdf
if (!require("fGarch", quietly = TRUE)) install.packages("fGarch")
# #
# # library(rugarch)
# # https://cran.r-project.org/web/packages/rugarch/rugarch.pdf
# # https://cran.r-project.org/web/packages/rugarch/vignettes/Introduction_to_the_rugarch_package.pdf
# if (!require("rugarch", quietly = TRUE)) install.packages("rugarch")
# #
# # library(rmugarch)
# # https://cran.r-project.org/web/packages/rmgarch/rmgarch.pdf
# # https://cran.r-project.org/web/packages/rmgarch/vignettes/The_rmgarch_models.pdf
# if (!require("rmgarch", quietly = TRUE)) install.packages("rmgarch")
############################################################################################################################################
# # library(stats4)
# if (!require("stats4", quietly = TRUE)) install.packages("stats4")
# #
# # library(withr)
# if (!require("withr", quietly = TRUE)) install.packages("withr")
# #
# # library(readxl) 
# if (!require("readxl", quietly = TRUE)) install.packages("withr")
# #
# # library(xlsx)
# #
# # library("writexl") 
# if (!require("writexl", quietly = TRUE)) install.packages("writexl")
# #
# # library(forecast)
# # https://cran.r-project.org/web/packages/forecast/forecast.pdf
# if (!require("forecast", quietly = TRUE)) install.packages("forecast")
# #
# # library(tsibble)
# if (!require("tsibble", quietly = TRUE)) install.packages("tsibble")
# #
# # library(fabletools)
# if (!require("fabletools", quietly = TRUE)) install.packages("fabletools")
# #
# # library(fable)
# if (!require("fable", quietly = TRUE)) install.packages("fable")
# #
# # library(feasts)
# if (!require("feasts", quietly = TRUE)) install.packages("feasts")
# #
# # library(graphics)
# if (!require("graphics", quietly = TRUE)) install.packages("graphics")
# #
# # library(qqplotr)
# # https://cran.r-project.org/web/packages/qqplotr/qqplotr.pdf
# if (!require("qqplotr", quietly = TRUE)) install.packages("qqplotr")
# #
# # library(timeDate)
# if (!require("timeDate", quietly = TRUE)) install.packages("timeDate")
# #
# # library(timeSeries)
# if (!require("timeSeries", quietly = TRUE)) install.packages("timeSeries")
# #
# # library(fBasics)
# if (!require("fBasics", quietly = TRUE)) install.packages("fBasics")
# #
# # library("data.table")
# if (!require("data.table", quietly = TRUE)) install.packages("data.table")
# #
# # library(reshape2)
# if (!require("reshape2", quietly = TRUE)) install.packages("reshape2")
# #
# # library(tidyverse)
# if (!require("tidyverse", quietly = TRUE)) install.packages("tidyverse")
# #
# # library(crayon)
# if (!require("crayon", quietly = TRUE)) install.packages("crayon")
# #
# # library(scales)
# if (!require("scales", quietly = TRUE)) install.packages("scales") 
# #
# # library(survival)
# if (!require("survival", quietly = TRUE)) install.packages("survival") 
# #
# # library(MASS)
# if (!require("MASS", quietly = TRUE)) install.packages("MASS") 
# #
# # library(fitdistrplus)
# # https://cran.r-project.org/web/packages/fitdistrplus/fitdistrplus.pdf
# # https://cran.r-project.org/web/packages/fitdistrplus/vignettes/paper2JSS.pdf
# if (!require("fitdistrplus", quietly = TRUE)) install.packages("fitdistrplus") 
# #
# # library(lattice)
# if (!require("lattice", quietly = TRUE)) install.packages("lattice") 
# #
# # library(extraDistr)
# if (!require("extraDistr", quietly = TRUE)) install.packages("extraDistr") 
# #
# # library(gamlss.dist)
# if (!require("gamlss.dist", quietly = TRUE)) install.packages("gamlss.dist") 
# #
# # library(glogis)
# if (!require("glogis", quietly = TRUE)) install.packages("glogis")
# #
# # library(nleqslv)
# # https://cran.r-project.org/web/packages/nleqslv/nleqslv.pdf
# if (!require("nleqslv", quietly = TRUE)) install.packages("nleqslv") 
# #
# # library(NlcOptim)
# if (!require("NlcOptim", quietly = TRUE)) install.packages("NlcOptim") 
# #
# # library(pracma)
# if (!require("pracma", quietly = TRUE)) install.packages("pracma") 
# #
# # library(reshape2)
# if (!require("reshape2", quietly = TRUE)) install.packages("reshape2") 
# #
# # library(tidyverse)
# if (!require("tidyverse", quietly = TRUE)) install.packages("tidyverse") 
# #
# # library(scales)
# if (!require("scales", quietly = TRUE)) install.packages("scales") 
# #
# # library(grid)
# if (!require("grid", quietly = TRUE)) install.packages("grid")
# #
# # library(gridExtra)
# if (!require("gridExtra", quietly = TRUE)) install.packages("gridExtra") 
# #
# # library(gridSVG)
# if (!require("gridSVG", quietly = TRUE)) install.packages("gridSVG")
# #
# # library(moments)
# if (!require("moments", quietly = TRUE)) install.packages("moments") 
# #
# # library(TTR)
# if (!require("TTR", quietly = TRUE)) install.packages("TTR") 
# #
# # library(lubridate)
# if (!require("lubridate", quietly = TRUE)) install.packages("lubridate") 
# #
# # library(strucchange)
# if (!require("strucchange", quietly = TRUE)) install.packages("strucchange")
# #
# # library(broom)
# if (!require("broom", quietly = TRUE)) install.packages("broom")
# #
# # library(rlang)
# if (!require("rlang", quietly = TRUE)) install.packages("rlang")
# #
# # library(timechange)
# if (!require("timechange", quietly = TRUE)) install.packages("timechange")
# #
# # library(leaps)
# if (!require("leaps", quietly = TRUE)) install.packages("leaps")
# #
# # library(ltsa)
# if (!require("ltsa", quietly = TRUE)) install.packages("ltsa")
# #
# # library(bestglm)
# if (!require("bestglm", quietly = TRUE)) install.packages("bestglm")
# #
# # library(lemon)
# if (!require("lemon", quietly = TRUE)) install.packages("lemon")
# #
# # library(nortest)
# if (!require("nortest", quietly = TRUE)) install.packages("nortest")
# #
# # library(EnvStats)
# if (!require("EnvStats", quietly = TRUE)) install.packages("EnvStats")
# #
# # library(car)
# # https://cran.r-project.org/web/packages/car/car.pdf
# if (!require("car", quietly = TRUE)) install.packages("car")
# #
# # library(rlist)
# if (!require("rlist", quietly = TRUE)) install.packages("rlist")
# #
# # library(goftest)
# # https://cran.r-project.org/web/packages/goftest/goftest.pdf
# if (!require("goftest", quietly = TRUE)) install.packages("goftest")
# ############################################################################################################################################
# # library(BiocManager)
# if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")
# #
# # library(BiocGenerics)
# if (!require("BiocGenerics", quietly = TRUE)) BiocManager::install("BiocGenerics")
# #
# # library(Biobase)
# if (!require("Biobase", quietly = TRUE)) BiocManager::install("Biobase")
# #
# # library(S4Vectors)
# if (!require("S4Vectors", quietly = TRUE)) BiocManager::install("S4Vectors")
# #
# # library(IRanges)
# if (!require("IRanges", quietly = TRUE)) BiocManager::install("IRanges")
# #
# # library(AnnotationDbi)
# if (!require("AnnotationDbi", quietly = TRUE)) BiocManager::install("AnnotationDbi")
# #
# # library(GO.db)
# if (!require("GO.db", quietly = TRUE)) BiocManager::install("GO.db")
# #
# # library(dynamicTreeCut)
# if (!require("dynamicTreeCut", quietly = TRUE)) BiocManager::install("dynamicTreeCut")
# #
# # library(fastcluster)
# if (!require("fastcluster", quietly = TRUE)) BiocManager::install("fastcluster")
# #
# # library(WGCNA)
# if (!require("WGCNA", quietly = TRUE)) BiocManager::install("WGCNA")
# ############################################################################################################################################
# # library(KFAS)
# if (!require("KFAS", quietly = TRUE)) install.packages("KFAS")
# #
# # library(MARSS)
# # RShowDoc("Chapter_inits.R",package="MARSS")
# if (!require("MARSS", quietly = TRUE)) install.packages("MARSS")
# #
# # library(gtable)
# if (!require("gtable", quietly = TRUE)) install.packages("gtable")
# #
# # library(sandwich)
# if (!require("sandwich", quietly = TRUE)) install.packages("sandwich")
# #
# # library(testcorr)
# if (!require("testcorr", quietly = TRUE)) install.packages("testcorr")
# #
# # library(gplots)
# if (!require("gplots", quietly = TRUE)) install.packages("gplots")
# #
# # library(aTSA)
# if (!require("aTSA", quietly = TRUE)) install.packages("aTSA")
# #
# # library(sgt)
# # https://cran.r-project.org/web/packages/sgt/sgt.pdf
# # https://cran.r-project.org/web/packages/sgt/vignettes/sgt.pdf
# if (!require("sgt", quietly = TRUE)) install.packages("sgt")
# ############################################################################################################################################
# # library(parallel)
# if (!require("parallel", quietly = TRUE)) install.packages("parallel")
# #
# # library(snow)
# if (!require("snow", quietly = TRUE)) install.packages("snow")
# #
# # library(doParallel)
# if (!require("doParallel", quietly = TRUE)) install.packages("doParallel")
# #
# # library(foreach)
# if (!require("foreach", quietly = TRUE)) install.packages("foreach")
# #
# # library(iterators)
# if (!require("iterators", quietly = TRUE)) install.packages("iterators")
# ###############################################################################################################################################
# # library(conflicted)
# if (!require("conflicted", quietly = TRUE)) install.packages("conflicted")
# ###############################################################################################################################################
# # library(fOptions)
# # http://cran.nexr.com/web/packages/fOptions/fOptions.pdf
# # if (!require("fOptions", quietly = TRUE)) install.packages("fOptions")
############################################################################################################################################
############################################################################################################################################
############################################################################################################################################
############################################################################################################################################
############################################################################################################################################
############################################################################################################################################
########################################################### Environmental Setting ##########################################################
# Removing all items in Global Environment
rm(list=ls())
#
# Closing all video devices.
graphics.off()
#
# Clearing all Plots
# try(dev.off(dev.list()["RStudioGD"]), silent=TRUE)
try(dev.off(),silent=TRUE)
#
# Storing options' default values.
def_options <- options()
# Resetting options to default values.
options(def_options)
#
# Changing digits option
# show(def_options$digits)
# or
# getOption("digits")
# # 7
# options(digits=22)
# getOption("digits")
# 22
# Restoring digits option to default value
# options(digits=7)
# or
# options(digits=def_options$digits)
# getOption("digits")
# 7
#
# Showing data frames with a large number of rows.
# show(def_options$max.print)
# or 
# getOption("max.print")
# 1000
# options(max.print=10000)
# getOption("max.print")
#
# Restoring max.print option to default value
# options(max.print=1000)
# or
# options(max.print=def_options$max.print)
# getOption("max.print")
# 1000
#
# Setting the current directory as the work directory. 
WD <- dirname(rstudioapi::getSourceEditorContext()$path)
show(WD)
setwd(WD)
dir()
#
# Loading the system directory
Sys.getenv('PATH')
#
# Adding the data directory to the system directory
# library(withr)
# WITHR <- with_path("C:/Users/rober/Documents/My Documents - Notebook (local)/My Classes/MPSMF/R - Scripts & Data/Data", Sys.getenv('PATH'), action="suffix")
# show(WITHR)
#
# Clearing the console.
cat("\014")
#
############################################################################################################################################
#### Functions #############################################################################################################################
# Clearing the Console
# cls <- function() cat(rep("\n",100))
# cls()
#
na.rm <- function(x){as.vector(x[!is.na(as.vector(x))])}
#
############################################################################################################################################
########## This section only illustrates file input/output techniques and the financial data candlestick plot. It can be skipped ###########
############################################################################################################################################
BTC_df <- utils::read.csv("BTC-USD_Data.csv", header=TRUE)
class(BTC_df)
# [1] "data.frame"
head(BTC_df)
#         Date    Open    High     Low   Close Adj.Close      Volume
# 1 2018-01-01 14112.2 14112.2 13154.7 13657.2   13657.2 10291200000
# 2 2018-01-02 13625.0 15444.6 13163.6 14982.1   14982.1 16846600192
# 3 2018-01-03 14978.2 15572.8 14844.5 15201.0   15201.0 16871900160
# 4 2018-01-04 15270.7 15739.7 14522.2 15599.2   15599.2 21783199744
# 5 2018-01-05 15477.2 17705.2 15202.8 17429.5   17429.5 23840899072
# 6 2018-01-06 17462.1 17712.4 16764.6 17527.0   17527.0 18314600448
tail(BTC_df)
#            Date     Open     High      Low    Close Adj.Close      Volume
# 2065 2023-08-27 26008.24 26165.37 25965.10 26089.69  26089.69  6913768611
# 2066 2023-08-28 26089.62 26198.58 25880.60 26106.15  26106.15 11002805166
# 2067 2023-08-29 26102.49 28089.34 25912.63 27727.39  27727.39 29368391712
# 2068 2023-08-30 27726.08 27760.16 27069.21 27297.27  27297.27 16343655235
# 2069 2023-08-31 27301.93 27456.08 25752.93 25931.47  25931.47 20181001451
# 2070 2023-09-01 25934.02 26125.87 25362.61 25800.72  25800.72 17202862221
#
# We check whether the Date column is in "Date" format. In case it is not, we change the format to "Date".
class(BTC_df$Date)
# [1] "character"
BTC_df$Date <- base::as.Date(BTC_df$Date, format="%Y-%m-%d")
class(BTC_df$Date)
# [1] "Date"
head(BTC_df$Date)
# [1] "2018-01-01" "2018-01-02" "2018-01-03" "2018-01-04" "2018-01-05" "2018-01-06"
tail(BTC_df$Dat)
# [1] "2023-08-27" "2023-08-28" "2023-08-29" "2023-08-30" "2023-08-31" "2023-09-01"
#
# We convert the BTC_df data.frame class object into a zoo class object to draw a candlestick plot.
# library(zoo)
BTC_zoo <- zoo::read.zoo(BTC_df)
class(BTC_zoo)
# [1] "zoo"
head(BTC_zoo, 3)
#               Open    High     Low   Close Adj.Close      Volume
# 2018-01-01 14112.2 14112.2 13154.7 13657.2   13657.2 10291200000
# 2018-01-02 13625.0 15444.6 13163.6 14982.1   14982.1 16846600192
# 2018-01-03 14978.2 15572.8 14844.5 15201.0   15201.0 16871900160
tail(BTC_zoo, 3)
#                Open     High      Low    Close Adj.Close      Volume
# 2023-08-30 27726.08 27760.16 27069.21 27297.27  27297.27 16343655235
# 2023-08-31 27301.93 27456.08 25752.93 25931.47  25931.47 20181001451
# 2023-09-01 25934.02 26125.87 25362.61 25800.72  25800.72 17202862221
#
# library(quantmod)
quantmod::chartSeries(BTC_zoo, type="auto", theme=chartTheme('white'))
#
quantmod::chartSeries(BTC_zoo, type="auto", subset="2018-04-17::2023-05-31", theme=chartTheme('white'))
#
############################################################################################################################################
# We might also convert the BTC_zoo zoo class object into an xts class object to draw a candlestick plot.
# library(xts)
BTC_xts <- xts::as.xts(BTC_zoo)
class(BTC_xts)
# [1] "xts" "zoo"
head(BTC_xts, 3)
#               Open    High     Low   Close Adj.Close      Volume
# 2018-01-01 14112.2 14112.2 13154.7 13657.2   13657.2 10291200000
# 2018-01-02 13625.0 15444.6 13163.6 14982.1   14982.1 16846600192
# 2018-01-03 14978.2 15572.8 14844.5 15201.0   15201.0 16871900160
tail(BTC_xts, 3)
#                Open     High      Low    Close Adj.Close      Volume
# 2023-08-30 27726.08 27760.16 27069.21 27297.27  27297.27 16343655235
# 2023-08-31 27301.93 27456.08 25752.93 25931.47  25931.47 20181001451
# 2023-09-01 25934.02 26125.87 25362.61 25800.72  25800.72 17202862221
#
# library(quantmod)
quantmod::chartSeries(BTC_xts, type="auto", theme=chartTheme('white'))
#
quantmod::chartSeries(BTC_xts, type="auto", subset="2018-04-17::2023-05-31", theme=chartTheme('white'))
#
############################################################################################################################################
# The plot is a (Japanese) candlestick chart. 
# Each candlestick represents an asset trading unit of time (generally a day).
# Similarly to a box plot, each candlestick is built with a body and two whiskers, which are called shadows or wicks in this context.
# The lowest [resp. highest] point of the bottom [resp. top] shadow represents the lowest [resp. highest] value of the asset price 
# in the unit time of trading.
# The color of the candlestick body is white or green [resp. black or red] for an open price lower [resp. higher] than the close price.
# The lowest [resp. highest] point of the body represents the open or close [resp. close or open] asset price according to whether 
# the open asset price is lower or higher than the close price, that is the candlestick is white or green [resp. black or red].
# The length of the body and shadows are proportional to the asset price changes, with respect to a unit measure.
# Each bar in the bottom of the plot represents the asset trading volume in the unit of time. This is the amount of shares of stock traded 
# in the unit of time. The higher the volume bar, the more actively the stock is traded. The color of the volume bar is white or green 
# [resp. black or red] according to whether the color of the overhead candle is white or green [resp. black or red].
#
# Note that to convert the BTC_df data.frame class object into a zoo class object and thereby into an xts class object, we should not add an 
# index column in front of the BTC_df$Date column before the conversion.
#
# We create a data frame from the BTC xts object and save it as csv file
BTC_df <- data.frame(BTC_xts)
# Class and structure of BTC_df.
class(BTC_df)
head(BTC_df, 3)
#               Open    High     Low   Close Adj.Close      Volume
# 2018-01-01 14112.2 14112.2 13154.7 13657.2   13657.2 10291200000
# 2018-01-02 13625.0 15444.6 13163.6 14982.1   14982.1 16846600192
# 2018-01-03 14978.2 15572.8 14844.5 15201.0   15201.0 16871900160
tail(BTC_df, 3)
#                Open     High      Low    Close Adj.Close      Volume
# 2023-08-30 27726.08 27760.16 27069.21 27297.27  27297.27 16343655235
# 2023-08-31 27301.93 27456.08 25752.93 25931.47  25931.47 20181001451
# 2023-09-01 25934.02 26125.87 25362.61 25800.72  25800.72 17202862221
#
# We add an index column to index the daily data by replicating the new row names,
# library(tibble)
BTC_df <- tibble::add_column(BTC_df, t=1:nrow(BTC_df), Date=rownames(BTC_df), .before=1)
rownames(BTC_df) <- NULL
head(BTC_df, 3)
#   t       Date    Open    High     Low   Close Adj.Close      Volume
# 1 1 2018-01-01 14112.2 14112.2 13154.7 13657.2   13657.2 10291200000
# 2 2 2018-01-02 13625.0 15444.6 13163.6 14982.1   14982.1 16846600192
# 3 3 2018-01-03 14978.2 15572.8 14844.5 15201.0   15201.0 16871900160
tail(BTC_df, 3)
#         t       Date     Open     High      Low    Close Adj.Close      Volume
# 2068 2068 2023-08-30 27726.08 27760.16 27069.21 27297.27  27297.27 16343655235
# 2069 2069 2023-08-31 27301.93 27456.08 25752.93 25931.47  25931.47 20181001451
# 2070 2070 2023-09-01 25934.02 26125.87 25362.61 25800.72  25800.72 17202862221
class(BTC_df$Date)
# [1] "character"
class(BTC_df$Adj.Close)
# [1] "numeric"
# Note that the column Date is a character class object. The other columns are numeric class objects.
BTC_df$Date <- as.Date(BTC_df$Date, format="%Y-%m-%d")
class(BTC_df$Date)
# We save BTC_df as cvs file
# [1] "Date"
utils::write.csv(BTC_df, "BTC.csv", row.names=FALSE)
# We remove the BTC_df data frame from the environment
rm(BTC_df)
head(BTC_df)
# Error: object 'BTC_df' not found
# Thereafter, we read the file BTC.csv as a data.frame object.
BTC_df <- read.csv("BTC.csv", header=TRUE)
class(BTC_df)
# [1] "data.frame"
head(BTC_df, 3)
#   t       Date    Open    High     Low   Close Adj.Close      Volume
# 1 1 2018-01-01 14112.2 14112.2 13154.7 13657.2   13657.2 10291200000
# 2 2 2018-01-02 13625.0 15444.6 13163.6 14982.1   14982.1 16846600192
# 3 3 2018-01-03 14978.2 15572.8 14844.5 15201.0   15201.0 16871900160
tail(BTC_df, 3)
#         t       Date     Open     High      Low    Close Adj.Close      Volume
# 2068 2068 2023-08-30 27726.08 27760.16 27069.21 27297.27  27297.27 16343655235
# 2069 2069 2023-08-31 27301.93 27456.08 25752.93 25931.47  25931.47 20181001451
# 2070 2070 2023-09-01 25934.02 26125.87 25362.61 25800.72  25800.72 17202862221
class(BTC_df$Date)
# [1] "character"
class(BTC_df$Adj.Close)
# [1] "numeric"
# Note that the column Date is again a character class object. The other columns are numeric class objects.
BTC_df$Date <- as.Date(BTC_df$Date, format="%Y-%m-%d")
class(BTC_df$Date)
# [1] "Date"
#
# Alternatively, we can save the BTC Data xts object directly as cvs file by the write.zoo command
zoo::write.zoo(BTC_xts, file="BTC_bis.csv", index.name="Date", row.names=FALSE, col.names=TRUE, sep=",")
# We remove the BTC_df data frame from the environment.
rm(BTC_df)
head(BTC_df)
# Error: object 'BTC_df' not found
# Thereafter, we read the file BTC_bis.csv as a data.frame object.
BTC_df <- read.csv("BTC_bis.csv", header=TRUE)
# Class and structure of data.
class(BTC_df)
# [1] "data.frame"
head(BTC_df, 3)
#         Date    Open    High     Low   Close Adj.Close      Volume
# 1 2018-01-01 14112.2 14112.2 13154.7 13657.2   13657.2 10291200000
# 2 2018-01-02 13625.0 15444.6 13163.6 14982.1   14982.1 16846600192
# 3 2018-01-03 14978.2 15572.8 14844.5 15201.0   15201.0 16871900160
tail(BTC_df, 3)
#            Date     Open     High      Low    Close Adj.Close      Volume
# 2068 2023-08-30 27726.08 27760.16 27069.21 27297.27  27297.27 16343655235
# 2069 2023-08-31 27301.93 27456.08 25752.93 25931.47  25931.47 20181001451
# 2070 2023-09-01 25934.02 26125.87 25362.61 25800.72  25800.72 17202862221
class(BTC_df$Date)
# [1] "character"
class(BTC_df$Adj.Close)
# [1] "numeric"
# Note that also in this case the column Date is a character class object. The other columns are class numeric objects.
BTC_df$Date <- as.Date(BTC_df$Date, format="%Y-%m-%d")
class(BTC_df$Date)
# [1] "Date"
############################################################################################################################################
############################################################################################################################################
##################### Scatter and Line plot of the Bitcoin Daily Adjusted Close Price from Apr-04-2017 to May-31-2023. #####################
detach("package:fGarch", unload=TRUE)
BTC_df <- read.csv("BTC-USD_Data.csv", header=TRUE)
class(BTC_df)
# [1] "data.frame"
head(BTC_df, 3)
#         Date    Open    High     Low   Close Adj.Close      Volume
# 1 2018-01-01 14112.2 14112.2 13154.7 13657.2   13657.2 10291200000
# 2 2018-01-02 13625.0 15444.6 13163.6 14982.1   14982.1 16846600192
# 3 2018-01-03 14978.2 15572.8 14844.5 15201.0   15201.0 16871900160
tail(BTC_df, 3)
#            Date     Open     High      Low    Close Adj.Close      Volume
# 2068 2023-08-30 27726.08 27760.16 27069.21 27297.27  27297.27 16343655235
# 2069 2023-08-31 27301.93 27456.08 25752.93 25931.47  25931.47 20181001451
# 2070 2023-09-01 25934.02 26125.87 25362.61 25800.72  25800.72 17202862221
# We check whether the Date column is in "Date" format. In case it is not, we change the format to "Date".
class(BTC_df$Date)
# [1] "character"
BTC_df$Date <- as.Date(BTC_df$Date, format="%Y-%m-%d")
class(BTC_df$Date)
# [1] "Date"
class(BTC_df$Adj.Close)
# [1] "numeric"
# We reduce the data frame from Apr-17-2018 to May-31-2023 for compatibility with another data set not used here.
BTC_red_df <- BTC_df[which(BTC_df$Date=="2018-04-17"):which(BTC_df$Date=="2023-05-31"),]
head(BTC_red_df, 3)
#           Date    Open    High     Low   Close Adj.Close     Volume
# 107 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09 6900879872
# 108 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42 6529909760
# 109 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31 7063209984
tail(BTC_red_df, 3)
#            Date     Open     High      Low    Close Adj.Close      Volume
# 1975 2023-05-29 28075.59 28432.04 27563.88 27745.88  27745.88 15181308984
# 1976 2023-05-30 27745.12 28044.76 27588.50 27702.35  27702.35 13251081851
# 1977 2023-05-31 27700.53 27831.68 26866.45 27219.66  27219.66 15656371534
row.names(BTC_red_df) <- NULL
head(BTC_red_df, 3)
#         Date    Open    High     Low   Close Adj.Close     Volume
# 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09 6900879872
# 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42 6529909760
# 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31 7063209984
tail(BTC_red_df, 3)
#            Date     Open     High      Low    Close Adj.Close      Volume
# 1869 2023-05-29 28075.59 28432.04 27563.88 27745.88  27745.88 15181308984
# 1870 2023-05-30 27745.12 28044.76 27588.50 27702.35  27702.35 13251081851
# 1871 2023-05-31 27700.53 27831.68 26866.45 27219.66  27219.66 15656371534
#
# We add an index column to index the daily data by replicating the new row names,
# library(tibble)
BTC_red_df <- tibble::add_column(BTC_red_df, t=1:nrow(BTC_red_df), .before=1)
head(BTC_red_df, 3)
#   t       Date    Open    High     Low   Close Adj.Close     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31 7063209984
tail(BTC_red_df, 3)
#         t       Date     Open     High      Low    Close Adj.Close      Volume
# 1869 1869 2023-05-29 28075.59 28432.04 27563.88 27745.88  27745.88 15181308984
# 1870 1870 2023-05-30 27745.12 28044.76 27588.50 27702.35  27702.35 13251081851
# 1871 1871 2023-05-31 27700.53 27831.68 26866.45 27219.66  27219.66 15656371534
#
# We draw a scatter and a line plot of the Bitcoin Daily Adjusted Close Price from Apr-17-2018 to May-31-2023.
# The scatter plot
Data_df <- BTC_red_df
head(Data_df)
# library(dplyr)
Data_df <- dplyr::rename(Data_df, x=t, y=Adj.Close)
sum(is.na(Data_df$y)) # We check whether we have NA in the BTC adjusted close price data set and how many NA we have.
# 0
DS_length <- length(Data_df$y)
show(DS_length)
First_Day <- as.character(Data_df$Date[1])
Last_Day <- as.character(Data_df$Date[DS_length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Bitcoin Daily Adjusted Close Price from ", .(First_Day), " to ", .(Last_Day), sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Data set length - ", .(DS_length), " sample points. Data by courtesy of Yahoo Finance - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("")
# library(numbers)
# numbers::primeFactors(DS_length-1)
x_breaks_num <- 34 # (deduced from primeFactors(DS_length-1))
x_breaks_low <- Data_df$x[1]
x_breaks_up <- Data_df$x[DS_length]
x_binwidth <- ceiling((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Date[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("adjusted close prices (US $)")
y_max <- max(na.rm(Data_df$y))
y_min <- min(na.rm(Data_df$y))
as.numeric(floor(y_max-y_min))
y_breaks_num <- 10
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor((y_min/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((y_max/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth), digits=3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.5
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
point_b <- bquote("daily adj. close prices")
line_red  <- bquote("LOESS curve")
line_green  <- bquote("regression line")
leg_labs <- c(point_b, line_red, line_green)
leg_cols <- c("point_b"="blue", "line_red"="red", "line_green"="green")
leg_breaks <- c("point_b", "line_red", "line_green")
# library(ggplot2)
BTC_Adj.Close_sp <- ggplot2::ggplot(Data_df) +
  geom_smooth(aes(x=x, y=y, color="line_green"), method="lm", formula=y ~ x, alpha=1, lwd=0.8, linetype="solid",
              se=FALSE, fullrange=FALSE) +
  geom_smooth(aes(x=x, y=y, color="line_red"), method="loess", formula=y ~ x, alpha=1, lwd=0.8, linetype="dashed",
              se=FALSE, fullrange=FALSE) +
  geom_point(alpha=1, size=0.6, shape=19, aes(x=x, y=y, color="point_b")) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_Adj.Close_sp)
#
# The line plot
line_blue  <- bquote("daily adj. close prices")
line_red  <- bquote("LOESS curve")
line_green  <- bquote("regression line")
leg_labs <- c(line_blue, line_red, line_green)
leg_cols <- c("line_blue"="blue", "line_red"="red", "line_green"="green")
leg_breaks <- c("line_blue", "line_red", "line_green")
BTC_Adj.Close_lp <- ggplot2::ggplot(Data_df) +
  geom_smooth(aes(x=x, y=y, color="line_green"), method="lm", formula=y ~ x, alpha=1, lwd=0.8, linetype="solid",
              se=FALSE, fullrange=FALSE) +
  geom_smooth(aes(x=x, y=y, color="line_red"), method="loess", formula=y ~ x, alpha=1, lwd=0.8, linetype="dashed",
              se=FALSE, fullrange=FALSE) +
  geom_line(aes(x=x, y=y, color="line_blue", group=1), alpha=1, lwd=0.6, linetype="solid") +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_Adj.Close_lp)
#
# With the goal of building a model for forecasting we split the data set in a training set, about $92\%$ of the data set, and a test set, 
# about $8\%$ of the data set. The training set is chosen considering the ratio between the number of data before the date "2023-01-01" and
# the full data set.
length(BTC_red_df$Date[which(BTC_red_df$Date<as.Date("2023-01-01"))])/length(BTC_red_df$Date)
# [1] 0.9192945
# We consider the scatter plot of the Bitcoin daily adjusted close prices training set and the test set
Data_df <- BTC_red_df
head(Data_df, 3)
#   t       Date    Open    High     Low   Close Adj.Close     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31 7063209984
tail(Data_df, 3)
#         t       Date     Open     High      Low    Close Adj.Close      Volume
# 1869 1869 2023-05-29 28075.59 28432.04 27563.88 27745.88  27745.88 15181308984
# 1870 1870 2023-05-30 27745.12 28044.76 27588.50 27702.35  27702.35 13251081851
# 1871 1871 2023-05-31 27700.53 27831.68 26866.45 27219.66  27219.66 15656371534
Data_df <- dplyr::rename(Data_df, x=t, y=Adj.Close)
head(Data_df, 3)
#   x       Date    Open    High     Low   Close       y     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09 7902.09 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42 8163.42 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31 8294.31 7063209984
DS_length <- length(Data_df$y)
show(DS_length)
# 1871
First_Day <- as.character(Data_df$Date[1])
show(First_Day)
# "2018-04-17"
Last_Day <- as.character(Data_df$Date[DS_length])
show(Last_Day)
# "2023-05-31"
TrnS_Last_Day <- as.character(Data_df$Date[which(Data_df$Date==as.Date("2023-01-01"))-1])
show(TrnS_Last_Day)
# "2022-12-31"
TrnS_length <- length(Data_df$Date[which(Data_df$Date<as.Date("2023-01-01"))])
show(TrnS_length)
# 1720
TstS_First_Day <- as.character(Data_df$Date[which(Data_df$Date==as.Date("2023-01-01"))])
show(TstS_First_Day)
# "2023-01-01"
TstS_length <- length(Data_df$Date[which(Data_df$Date>=as.Date("2023-01-01"))])
show(TstS_length)
# 151
TstS_length == DS_length-TrnS_length
# TRUE
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Bitcoin Daily Adjusted Close Price - Training and Test Sets - from ", .(First_Day), " to ", .(Last_Day), sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points, from ", .(First_Day), " to ", .(TrnS_Last_Day),". Test set length ", .(TstS_length), " sample points, from ", .(TstS_First_Day), " to ", .(Last_Day),". Data by courtesy of Yahoo Finance - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("")
# numbers::primeFactors(length-1)
x_breaks_num <- 34 # (deduced from primeFactors(length-1))
x_breaks_low <- Data_df$x[1]
x_breaks_up <- Data_df$x[DS_length]
x_binwidth <- ceiling((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Date[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("adjusted close prices (US $)")
y_max <- max(na.rm(Data_df$y))
y_min <- min(na.rm(Data_df$y))
as.numeric(floor(y_max-y_min))
y_breaks_num <- 10
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor((y_min/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((y_max/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth), digits=3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.5
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
point_black <- bquote("daily adj. close prices - training set")
point_b <- bquote("daily adj. close prices - test set")
line_green <- bquote("regression line (training set)")
line_red <- bquote("LOESS curve (training set)")
leg_point_labs <- c(point_black, point_b)
leg_point_cols <- c("point_black"="black", "point_b"="blue")
leg_point_breaks <- c("point_black", "point_b")
leg_line_labs <- c(line_green, line_red)
leg_line_cols <- c("line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_green", "line_red")
leg_col_labs   <- c(leg_point_labs,leg_line_labs)
leg_col_cols   <- c(leg_point_cols,leg_line_cols)
leg_col_breaks <- c(leg_point_breaks,leg_line_breaks)
BTC_Adj.Close_TrnS_TstS_sp <- ggplot(Data_df, aes(x=x)) +
  geom_vline(xintercept=Data_df$x[TrnS_length], lwd=0.5, color="black") +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_green"), method="lm" , formula=y ~ x, 
              alpha=1, lwd=0.9, linetype="solid", se=FALSE, fullrange=FALSE) +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_red"), method="loess", formula=y ~ x, 
              alpha=1, lwd=0.9, linetype="dashed", se=FALSE, fullrange=FALSE) +
  geom_point(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="point_black"), alpha=1, size=0.7, shape=19) + 
  geom_point(data=subset(Data_df, Data_df$x>x[TrnS_length]), aes(y=y, color="point_b"), alpha=1, size=0.7, shape=19) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  guides(colour=guide_legend(override.aes=list(shape=c(19, 19, NA, NA), linetype=c("blank", "blank", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_Adj.Close_TrnS_TstS_sp)
#
# The line plot
BTC_Adj.Close_TrnS_TstS_lp <- ggplot(Data_df, aes(x=x)) +
  geom_vline(xintercept=Data_df$x[TrnS_length], lwd=0.5, color="black") +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_green"), method="lm" , formula=y ~ x, 
              alpha=1, lwd=0.9, linetype="solid", se=FALSE, fullrange=FALSE) +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_red"), method="loess", formula=y ~ x,
              alpha=1, lwd=0.9, linetype="dashed", se=FALSE, fullrange=FALSE) +
  geom_line(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="point_black"), alpha=1, lwd=0.7, linetype="solid") +
  geom_line(data=subset(Data_df, Data_df$x>x[TrnS_length]), aes(y=y, color="point_b"), alpha=1, lwd=0.7, linetype="solid") +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_Adj.Close_TrnS_TstS_lp)
#
# Upon examining the scatter and line plots, we observe an increasing trend accompanied by some sharp declines. The overall trend does not 
# appear linear when we compare the LOESS curve to the regression line. If it weren't for the significant drop starting in October 2021, the 
# trend would seem exponential, aligning with predictions from the geometric Brownian motion model.
# We lack visual evidence of seasonality. Generally, stock market time series do not exhibit a pronounced seasonal component due to the
# inherent nature of the stock market, making it challenging to define a seasonal period. While stock markets are typically closed on 
# Saturdays and Sundays, they also shut down for national and some local holidays. This complicates the management of a standard five-day 
# trading period. Additionally, stock price movements are driven by traders' continuous buying and selling, influenced by their expectations
# of future stock returns, which are shaped by unpredictable economic, political, and social news. Therefore, it's difficult to identify a seasonal 
# mechanism for stock price movements. 
# However, we can observe some evidence of hourly seasonality on trading days, but this would require intra-day data for analysis. 
# Furthermore, the distribution of training set points around the LOESS curve does not appear homogeneous along the path, indicating wisual
# evidence of heteroscedasticity.
############################################################################################################################################
# For simplicity, we build a data frame containing only the training set data before pursuing a quantitative analysis.
BTC_train_df <- BTC_red_df[which(BTC_red_df$Date<as.Date("2023-01-01")),]
head(BTC_train_df, 3)
#   t       Date    Open    High     Low   Close Adj.Close     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31 7063209984
tail(BTC_train_df, 3)
#         t       Date     Open     High      Low    Close Adj.Close      Volume
# 1718 1718 2022-12-29 16552.32 16651.76 16508.68 16642.34  16642.34 14472237479
# 1719 1719 2022-12-30 16641.33 16643.43 16408.47 16602.59  16602.59 15929162910
# 1720 1720 2022-12-31 16603.67 16628.99 16517.52 16547.50  16547.50 11239186456
#
# We consider the autocorrelograms of the training set. Of course, due to the clear trend, we expect a strong visual evidence for 
# autocorrelation.
# Autocorrelogram of the training set of the Bitcoin daily adjusted close prices.
# library(TSA)
Data_df <- BTC_train_df
y <- Data_df$Adj.Close
TrnS_length <- length(y)
T <- length(y)
# max_lag <- ceiling(10*log10(T))    # Default
# max_lag <- ceiling(sqrt(n)+45)     # Box-Jenkins
max_lag <- ceiling(min(10,T/4))      # Hyndman (for data without seasonality)
# max_lag <- ceiling(min(2*12,T/5))  # Hyndman (for data with seasonality, see https://robjhyndman.com/hyndsight/ljung-box-test/
Aut_Fun_y <- stats::acf(y, lag.max=max_lag, type="correlation", plot=FALSE)
# Aut_Fun_y <- TSA::acf(y, lag.max=max_lag, type="correlation", plot=TRUE)
ci_090 <- qnorm((1+0.90)/2)/sqrt(T)
ci_095 <- qnorm((1+0.95)/2)/sqrt(T)
ci_099 <- qnorm((1+0.99)/2)/sqrt(T)
Aut_Fun_y_df <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
First_Day <- as.character(Data_df$Date[1])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Autocorrelogram of the Bitcoin Daily Adjusted Close Price Training Set - from ", .(First_Day), " to ", .(Last_Day), sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points. Lags ", .(max_lag), ". Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("lags")
x_breaks_num <- max_lag
x_binwidth <- 1
x_breaks <- Aut_Fun_y_df$lag
x_labs <- format(x_breaks, scientific=FALSE)
Aut_Fun_y_plot <- ggplot(Aut_Fun_y_df, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), lwd=1, col="black") +
  geom_hline(aes(yintercept=-ci_090, color="CI_090"), lwd=0.9, lty=3, show.legend=TRUE) +
  geom_hline(aes(yintercept=ci_090, color="CI_090"), lwd=0.9, lty=3) +
  geom_hline(aes(yintercept=ci_095, color="CI_095"), lwd=0.8, lty=2, show.legend=TRUE) + 
  geom_hline(aes(yintercept=-ci_095, color="CI_095"), lwd=0.8, lty=2) +
  geom_hline(aes(yintercept=-ci_099, color="CI_099"), lwd=0.8, lty=4, show.legend=TRUE) +
  geom_hline(aes(yintercept=ci_099, color="CI_099"), lwd=0.8, lty=4) +
  scale_x_continuous(name= "lag", breaks=x_breaks, label=x_labs) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis=sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"), values=c(CI_090="green", CI_095="blue", CI_099="red"),
                     guide=guide_legend(override.aes=list(linetype=c("dotted", "dashed", "dotdash")))) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=0, vjust=1, hjust=0.5),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(Aut_Fun_y_plot)
#
# The autocorrelogram provides visual evidence for autocorrelation in a typical form due to non-stationarity.
#
# Partial autocorrelogram of the training set of the NASDAQ Composite daily adjusted close prices.
Data_df <- BTC_train_df
y <- Data_df$Adj.Close
length <- length(y)
T <- length(y)
# max_lag <- ceiling(10*log10(T))    # Default
# max_lag <- ceiling(sqrt(n)+45)     # Box-Jenkins
max_lag <- ceiling(min(10,T/4))      # Hyndman (for data without seasonality)
# max_lag <- ceiling(min(2*12,T/5))  # Hyndman (for data with seasonality, see https://robjhyndman.com/hyndsight/ljung-box-test/)
Part_Aut_Fun_y <- stats::pacf(y, lag.max=max_lag, type="correlation", plot=FALSE)
ci_090 <- qnorm((1+0.90)/2)/sqrt(T)
ci_950 <- qnorm((1+0.95)/2)/sqrt(T)
ci_990 <- qnorm((1+0.99)/2)/sqrt(T)
Part_Aut_Fun_y_df <- data.frame(lag=Part_Aut_Fun_y$lag, pacf=Part_Aut_Fun_y$acf)
First_Day <- as.character(Data_df$Date[1])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Partial Autocorrelogram of the Bitcoin Daily Adjusted Close Price Training Set - from ", .(First_Day), " to ", .(Last_Day), sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points. Lags ", .(max_lag), ".  Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("lags")
x_breaks_num <- max_lag
x_binwidth <- 1
x_breaks <- Aut_Fun_y_df$lag
x_labs <- format(x_breaks, scientific=FALSE)
Part_Aut_Fun_y_plot <-ggplot(Part_Aut_Fun_y_df, aes(x=lag, y=pacf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=pacf), lwd=1, col="black") +
  geom_hline(aes(yintercept=-ci_090, color="CI_090"), lwd=0.9, lty=3, show.legend=TRUE) +
  geom_hline(aes(yintercept=ci_090, color="CI_090"), lwd=0.9, lty=3) +
  geom_hline(aes(yintercept=ci_095, color="CI_095"), lwd=0.8, lty=2, show.legend=TRUE) + 
  geom_hline(aes(yintercept=-ci_095, color="CI_095"), lwd=0.8, lty=2) +
  geom_hline(aes(yintercept=-ci_099, color="CI_099"), lwd=0.8, lty=4, show.legend=TRUE) +
  geom_hline(aes(yintercept=ci_099, color="CI_099"), lwd=0.8, lty=4) +
  scale_x_continuous(name= "lag", breaks=x_breaks, label=x_labs) +
  scale_y_continuous(name="pacf value", breaks=waiver(), labels=NULL,
                     sec.axis=sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"), values=c(CI_090="green", CI_095="blue", CI_099="red"),
                     guide=guide_legend(override.aes=list(linetype=c("dotted", "dashed", "dotdash")))) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=0, vjust=1, hjust=0.5),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(Part_Aut_Fun_y_plot)
#
# The partial autocorrelogram reveals that the time series autocorrelation is mainly due to a strong correlation between the time series and 
# its one-lagged copy. Therefore, combining autocorrelogram and partial autocorrelogram provides visual evidence, an actual signature, for 
# the presence of a random walk component (unit root).
############################################################################################################################################
# We apply the Augmented Dickey-Fuller (ADF) test to validate (not reject) the null hypothesis of a stochastic trend component (random
# walk) in the time series generating process against the alternative hypothesis that the time series can be thought of as a path of an 
# autoregressive process, possibly with drift and linear trend. Furthermore, we apply the Kwiatowski-Phillips-Schmidt-Shin (KPSS) test to
# reject the null hypothesis that the time series is generated by an autoregressive process, possibly with drift and linear trend, against 
# the alternative that the time series can be thought of as a path of a process with a stochastic trend component.
#
# More precisely, the ADF test assumes that the time series is generated by a stochastic process with a random walk component. This null 
# hypothesis leads to refer to the ADF test as a unit root test. In addition, three alternative hypotheses are considered:
# 1) the time series can be thought of as a path of an autoregressive process, with no drift and no linear trend;
# 2) the time series can be thought of as a path of an autoregressive process, with drift and no linear trend;
# 3) the time series can be thought of as a path of an autoregressive process, with drift and linear trend.
# For more details, see Equations (12.65)-(12.67) in Essentials of Time Series Analysis.
#
# For optimal performance of the ADF test, the choice of the maximum number of lags in the linear models used for the test, described in
# Equations (12.65)-(12.67), is crucial. If the maximum number of lags is too small, then the linear model's residuals will likely be
# affected by autocorrelation, which biases the test. On the contrary, if the maximum number of lags is too large, then the power of the 
# test will suffer. Typically, two rules of thumb are used to determine a small and a large number of lags, respectively. In addition, AIC 
# and BIC can be used to determine a more optimal number of lags. Nevertheless, an important issue is that the residuals of the linear model
# used for the test should not show autocorrelation. 
# We start testing the unit root hypothesis against the alternative hypothesis of autoregressive process with drift and linear trend by 
# choosing a fixed number of lags. Then, we will select the number of lags by the Akaike and the Bayes information criteria while 
# considering the issue of no autocorrelation in the linear model's residuals for the test.
#
Data_df <- BTC_train_df
y <- Data_df$Adj.Close
length(y)
# [1] 1720
#
# First, we consider the ADF test with 0 lags, which is actually the original Dickey-Fuller test.
lag_num <- 0
# library(urca)
y_ADF_ur.df_trend_0_lags <- urca::ur.df(y, type="trend", lags=lag_num, selectlags="Fixed")
class(y_ADF_ur.df_trend_0_lags)
lm(formula=y_ADF_ur.df_trend_0_lags@testreg[["terms"]])
# Call: lm(formula=y_ADF_ur.df_trend_0_lags@testreg[["terms"]])
# Coefficients: (Intercept)      z.lag.1           tt  
#                 38.23063     -0.00219         0.01480  
nobs(lm(formula=y_ADF_ur.df_trend_0_lags@testreg[["terms"]]))
# 1719
n_obs <- length(y)-(lag_num+1)
show(n_obs)
# 1719
n_coeffs <- nrow(y_ADF_ur.df_trend_0_lags@testreg[["coefficients"]])
show(n_coeffs)
# 3
df.residual(lm(formula=y_ADF_ur.df_trend_0_lags@testreg[["terms"]])) 
# 1716
df_res <- n_obs-n_coeffs
show(df_res)
# 1716
summary(y_ADF_ur.df_trend_0_lags)
############################################### 
# Augmented Dickey-Fuller Test Unit Root Test # 
############################################### 
# Test regression trend 
# Call: lm(formula=z.diff ~ z.lag.1 + 1 + tt)
#
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7484.7 -229.9  -21.3   213.3  7324.8
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept) 38.230627  49.614200   0.771    0.441
# z.lag.1    -0.002190   0.001906 -1.149    0.251
# tt           0.014800   0.066420   0.223    0.824
# 
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 1027 on 1716 degrees of freedom
# Multiple R-squared:  0.001063,	Adjusted R-squared: -0.0001008 
# F-statistic: 0.9134 on 2 and 1716 DF,  p-value: 0.4013
# 
# Value of test-statistic is: -1.1489 0.6227 0.9134 
# Critical values for test statistics: 
#       1pct  5pct 10pct
# tau3 -3.96 -3.41 -3.12
# phi2  6.09  4.68  4.03
# phi3  8.27  6.25  5.34
#
# The null hypothesis cannot be rejected against the three alternatives at the $10\%$ significance level.
# However, to validate the test, we need to check the possible presence of autocorrelation in the residuals of the linear model used for the DF
# test.
#
y_res <- as.vector(y_ADF_ur.df_trend_0_lags@testreg[["residuals"]])
n_obs <- nobs(lm(formula=y_ADF_ur.df_trend_0_lags@testreg[["terms"]]))
show(n_obs)
# [1] 1719
max_lag <- ceiling(min(10, n_obs/4))    # Hyndman (for data without seasonality, see https://robjhyndman.com/hyndsight/ljung-box-test/)
show(max_lag)
# 10
n_coeffs <- nrow(y_ADF_ur.df_trend_0_lags@testreg[["coefficients"]])
show(n_coeffs)
# 3
n_pars <- n_coeffs
show(n_pars)
# 3
fit_df <- n_pars
y_res_LB <- Box.test(y_res, lag=max_lag, fitdf=fit_df, type="Ljung-Box")
show(y_res_LB)
# Box-Ljung test
# data:  y_res
# X-squared=26.923, df=7, p-value=0.000344
#
# Note that, in general, the fitdf option should be given the following value.
LB_fit_df <- min(min(max_lag, n_pars), max_lag-1)
show(LB_fit_df)
# 3
# However, if n_pars < max_lag, then LB_fit_df=n_pars.
Box.test(y_res, lag=max_lag, fitdf=LB_fit_df, type="Ljung-Box")
# Box-Ljung test
# data:  y_res
# X-squared=26.923, df=7, p-value=0.000344
#
# Also, note also that R. Hyndman proposes the following modification of the lag option instead. 
H_max_lag <- max(max_lag,(n_pars+3))
show(H_max_lag)
# 10
# However, if n_pars+3 <= max_lag, then H_max_lag=max_lag.
Box.test(y_res, lag=H_max_lag, fitdf=fit_df, type="Ljung-Box")
# Box-Ljung test
# data:  y_res
# X-squared=26.923, df=7, p-value=0.000344
#
# The function FitAR::LjungBoxTest() switches from the value n_pars to LB_fit_df automatically. Interestingly, FitAR::LjungBoxTest() yields
# also the results of the Ljung-Box test for lags smaller than max_lag. This form of the Ljung_box test test is described in detail in Wei 
# (2006, p.153, eqn. 7.5.1). The df are given by h-k, where h is the lag, running from StartLag to lag.max, and k	is the number of ARMA 
# parameters, default k=0. When h-k < 1, it is reset to 1. This raises no problems, since the test is conservative in this case.
# library(FitAR)
FitAR::LjungBoxTest(y_res, k=n_pars, lag.max=max_lag, StartLag=1, SquaredQ=FALSE)
#  m   Qm     pvalue
#  1  1.66 0.1979303177
#  2  1.78 0.1826815365
#  3  2.53 0.1113477692
#  4  4.98 0.0256848472
#  5  5.09 0.0785034768
#  6  6.48 0.0904596081
#  7 10.17 0.0376098811
#  8 12.92 0.0241576973
#  9 26.35 0.0001915471
# 10 26.92 0.0003440490
#
# A loop-cycle accounting of the LB_fit_df assignment can replicate the FitAR::LjungBoxTest() function yielding.
n_pars_seq <- rep(NA,max_lag)
for(l in 1:max_lag){
  if(l-n_pars<=0) n_pars_seq[l] <- l-1
  else n_pars_seq[l] <- n_pars
}
show(n_pars_seq)
# 0 1 2 3 3 3 3 3 3 3
#
for(l in 1:max_lag){
  show(Box.test(y_res, lag=l,   fitdf=n_pars_seq[l], type="Ljung-Box"))
}
# Box-Ljung test
# data:  y_res
# X-squared=1.6576, df=1, p-value=0.1979
# X-squared=1.7757, df=1, p-value=0.1827
# X-squared=2.5350, df=1, p-value=0.1113
# X-squared=4.9771, df=1, p-value=0.02568
# X-squared=5.0892, df=2, p-value=0.0785
# X-squared=6.4799, df=3, p-value=0.09046
# X-squared=10.173, df=4, p-value=0.03761
# X-squared=12.918, df=5, p-value=0.02416
# X-squared=26.350, df=6, p-value=0.0001915
# X-squared=26.923, df=7, p-value=0.000344
#
# # The portes::LjungBox() function also allows to consider several lags at once
# # library(portes)
# portes::LjungBox(y_res, lags=seq(from=1, to=max_lag, by=1), fitdf=n_pars, sqrd.res=FALSE)
# # lags statistic df      p-value
# #  1  1.657580  0           NA
# #  2  1.775673  0           NA
# #  3  2.534987  0 0.0000000000
# #  4  4.977111  1 0.0256848472
# #  5  5.089225  2 0.0785034768
# #  6  6.479874  3 0.0904596081
# #  7 10.173148  4 0.0376098811
# #  8 12.918180  5 0.0241576973
# #  9 26.350358  6 0.0001915471
# # 10 26.923243  7 0.0003440490
# #
# # However, the portes::LjungBox() function does not calculate the p-values corresponding to the lags having a non-positive difference with 
# # max_lag. This issue can be overcome by redefining the lag sequence.
# lag_seq <- rep(NA,max_lag)
# for(l in 1:max_lag){
#   if(l-n_pars<=0) lag_seq[l] <- n_pars+1
#   else lag_seq[l] <- l
# }
# show(lag_seq)
# # 4  4  4  4  5  6  7  8  9 10
# #
# portes::LjungBox(y_res, lags=lag_seq, fitdf=n_pars, sqrd.res=FALSE)
# # lags statistic df      p-value
# #  4   4.977111  1 0.0256848472
# #  4   4.977111  1 0.0256848472
# #  4   4.977111  1 0.0256848472
# #  4   4.977111  1 0.0256848472
# #  5   5.089225  2 0.0785034768
# #  6   6.479874  3 0.0904596081
# #  7  10.173148  4 0.0376098811
# #  8  12.918180  5 0.0241576973
# #  9  26.350358  6 0.0001915471
# # 10  26.923243  7 0.0003440490
# #
# # Note that applying the stats::Box.test() function in a loop
# for(l in 1:max_lag){
#   show(Box.test(y_res, lag=lag_seq[l],   fitdf=n_pars, type="Ljung-Box"))
# }
# # Box-Ljung test
# # data:  y_res
# # X-squared = 4.9771, df = 1, p-value = 0.02568
# # X-squared = 4.9771, df = 1, p-value = 0.02568
# # X-squared = 4.9771, df = 1, p-value = 0.02568
# # X-squared = 4.9771, df = 1, p-value = 0.02568
# # X-squared = 5.0892, df = 2, p-value = 0.0785
# # X-squared = 6.4799, df = 3, p-value = 0.09046
# # X-squared = 10.173, df = 4, p-value = 0.03761
# # X-squared = 12.918, df = 5, p-value = 0.02416
# # X-squared = 26.350, df = 6, p-value = 0.0001915
# # X-squared = 26.923, df = 7, p-value = 0.000344
# #
# # Another redefinition of the lag sequence was proposed by R. Hyndman (see forecast::checkresiduals() function, see also 
# # https://robjhyndman.com/hyndsight/ljung_box_df.html).
# lag_seq <- rep(NA,max_lag)
# for(l in 1:max_lag){
#   if(l-n_pars<=2) lag_seq[l] <- max(l,(n_pars+3)) # Hyndman
#   else lag_seq[l] <- l
# }
# show(lag_seq)
# # 6  6  6  6  6  6  7  8  9 10
# # library(portes)
# portes::LjungBox(y_res, lags=lag_seq, fitdf=n_pars, sqrd.res=FALSE)
# # lags statistic df      p-value
# #  6  6.479874  3 0.0904596081
# #  6  6.479874  3 0.0904596081
# #  6  6.479874  3 0.0904596081
# #  6  6.479874  3 0.0904596081
# #  6  6.479874  3 0.0904596081
# #  6  6.479874  3 0.0904596081
# #  7 10.173148  4 0.0376098811
# #  8 12.918180  5 0.0241576973
# #  9 26.350358  6 0.0001915471
# # 10 26.923243  7 0.0003440490
# #
# # In the same way,
# for(l in 1:max_lag){
#   show(Box.test(y_res, lag=lag_seq[l],   fitdf=n_pars, type="Ljung-Box"))
# }
# # Box-Ljung test
# # data:  y_res
# # X-squared = 6.4799, df = 3, p-value = 0.09046
# # X-squared = 6.4799, df = 3, p-value = 0.09046
# # X-squared = 6.4799, df = 3, p-value = 0.09046
# # X-squared = 6.4799, df = 3, p-value = 0.09046
# # X-squared = 6.4799, df = 3, p-value = 0.09046
# # X-squared = 6.4799, df = 3, p-value = 0.09046
# # X-squared = 10.173, df = 4, p-value = 0.03761
# # X-squared = 12.918, df = 5, p-value = 0.02416
# # X-squared = 26.350, df = 6, p-value = 0.0001915
# # X-squared = 26.923, df = 7, p-value = 0.000344
# #
# According to all versions of the Ljung-Box test shown above, we should reject the null hypothesis of no autocorrelation in the residuals
# of the linear model used for the DF test at the $1\%$ significance level. However, comparing the computational tests with the residuals
# autocorrelogram, we think that the version of the Ljung-Box test applied by the function FitAR::LjungBoxTest() appears to be closer to 
# the visual evidence than the others.
#
# We plot the autocorrelograms of the residuals of the linear model used for the DF test.
y_res <- as.vector(y_ADF_ur.df_trend_0_lags@testreg[["residuals"]])
n_obs <- nobs(lm(formula=y_ADF_ur.df_trend_0_lags@testreg[["terms"]]))
T <- n_obs
# max_lag <- ceiling(10*log10(T))    # Default
# max_lag <- ceiling(sqrt(n)+45)     # Box-Jenkins
max_lag <- ceiling(min(10, T/4))     # Hyndman (for data without seasonality)
# max_lag <- ceiling(min(2*12, T/5)) # Hyndman (for data with seasonality, see https://robjHyndman.com/hy_resndsight/ljung-box-test/)
# Aut_Fun_y <- stats::acf(y, lag.max=max_lag, type="correlation", plot=FALSE)
Aut_Fun_y_res <- TSA::acf(y_res, lag.max=max_lag, type= "correlation", plot=FALSE)
ci_090 <- qnorm((1+0.90)/2)/sqrt(T)
ci_95 <- qnorm((1+0.95)/2)/sqrt(T)
ci_99 <- qnorm((1+0.99)/2)/sqrt(T)
Aut_Fun_y_res <- data.frame(lag=Aut_Fun_y_res$lag, acf=Aut_Fun_y_res$acf)
# First_Date <- paste(Data_df$Month[1],Data_df$year[1])
# Last_Date <- paste(Data_df$Month[T],Data_df$year[T])
First_Date <- as.character(Data_df$Date[1])
Last_Date <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025", 
                             paste("Autocorrelogram of the Residuals of the Linear Model for the DF Test (ADF Test with 0 Lags) on the Bitcoin Daily Adjusted Close from ", .(First_Date), " to ", .(Last_Date))))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Data set size ",.(T)," sample points. Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("lags")
x_breaks_num <- max_lag
x_binwidth <- 1
x_breaks <- Aut_Fun_y_res$lag
x_labs <- format(x_breaks, scientific=FALSE)
Plot_Aut_Fun_y_res <- ggplot(Aut_Fun_y_res, aes(x=lag, y=acf))+
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), linewidth=1, col= "black") +
  # geom_col(mapping=NULL, data=NULL, position= "dodge", width=0.1, col= "black", inherit.aes=TRUE)+
  geom_hline(aes(yintercept=-ci_090, color= "CI_090"), show.legend=TRUE, lwd=0.9, lty=3) +
  geom_hline(aes(yintercept=ci_090, color= "CI_090"), lwd=0.9, lty=3) +
  geom_hline(aes(yintercept=ci_95, color= "CI_95"), show.legend=TRUE, lwd=0.8, lty=2)+
  geom_hline(aes(yintercept=-ci_95, color= "CI_95"), lwd=0.8, lty=2) +
  geom_hline(aes(yintercept=-ci_99, color= "CI_99"), show.legend=TRUE, lwd=0.8, lty=4) +
  geom_hline(aes(yintercept=ci_99, color= "CI_99"), lwd=0.8, lty=4) +
  scale_x_continuous(name= "lag", breaks=x_breaks, label=x_labs) +
  scale_y_continuous(name= "acf value", breaks=waiver(), labels=NULL,
                     sec.axis=sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name= "Conf. Inter.", labels=c("90%","95%","99%"), values=c(CI_090="green", CI_95="blue", CI_99="red"),
                     guide=guide_legend(override.aes=list(linetype=c("dotted", "dashed", "dotdash")))) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
  # theme(plot.title=element_blank(), 
  #       plot.subtitle=element_blank(),
  #       plot.caption=element_text(hjust=1.0),
  #       legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(Plot_Aut_Fun_y_res)
#
# The visual evidence from the autocorrelogram of the residuals of the linear model used for the DF test confirms the rejection of the null 
# hypothesis of no autocorrelation.
#
# We also consider the Breusch-Godfrey test for autocorrelation.
# Note that the Breusch-Godfrey test applies to the residuals from a linear regression. Therefore, applying the Breusch-Godfrey test to the 
# residuals of the linear model used for the DF test is pretty natural. Eventually, the test can be applied to the linear model
lm(formula=y_ADF_ur.df_trend_0_lags@testreg[["terms"]])
# Call: lm(formula = y_ADF_ur.df_trend_0_lags@testreg[["terms"]])
# Coefficients: (Intercept)      z.lag.1           tt  
#                38.23063       -0.00219      0.01480
#
lmtest::bgtest(lm(formula=y_ADF_ur.df_trend_0_lags@testreg[["terms"]]), order=10, type="Chisq", fill=NA)
# Breusch-Godfrey test for serial correlation of order up to 10
# data: lm(formula = y_ADF_ur.df_trend_0_lags@testreg[["terms"]])
# LM test = 26.997, df = 10, p-value = 0.002608
# 
# This version of the test uses the Lagrange Multipliers (LM) statistics $nR^{2}$, where $n$ is the number of the observations and $R^{2}$
# is the coefficient of determination of the linear regression used in the Breusch_Godfrey test 
# (see https://real-statistics.com/multiple-regression/autocorrelation/breusch-godfrey-test/).
# The LM statistic is asymptotically $\Chi^{p}$-distributed, where $p$ is the order of the correlation that we want to test. In this case
# $p=10$, since we are considering up to 10 lags in analyzing the autocorrelation of the residuals. Another version of the Breusch-Godfrey 
# test uses the F statistic.
lmtest::bgtest(lm(formula=y_ADF_ur.df_trend_0_lags@testreg[["terms"]]), order=10, type="F", fill=NA)
# Breusch-Godfrey test for serial correlation of order up to 10
# data:  lm(formula = y_ADF_ur.df_trend_0_lags@testreg[["terms"]])
# LM test = 2.7221, df1 = 10, df2 = 1696, p-value = 0.00254
#
# In both cases the null of no autocorrelation is rejected at the $1\%$ significance level.
#
# It may be interesting to show how the linear model used in the DF test is built.
# From the time series $\left(y_{t}\right)_{t=1}^{T}$, 
y <- Data_df$Adj.Close
head(y)
# 7902.09 8163.42 8294.31 8845.83 8895.58 8802.46
length(y)
# 1720
# We define the differenced time series $\left(z_{t}\right)_{t=1}^{T-1}$, given by $z_{t}\overset{\text{def}}{=}\left(y_{t+1}-y_{t}\right), for
# every $t=1,\dots,T-1$.
z=diff(y,differences=1)
head(z)
# 261.33008 130.88965 551.52051  49.75000 -93.12012 128.41992
length(z)
# 1719
# Note that the differenced time series $\left(z_{t}\right)_{t=1}^{T-1}$ is one term shorter than the original time series. 
# $\left(y_{t}\right)_{t=1}^{T}$.  
# We define the time variable in the regression
tt=c(1:(length(y)-1))
# We define the one-lag lagged time series $\left(y^{\left(lag1\right)}_{t}\right)_{t=1}^{T-1}$, given by 
# $y^{\left(lag1\right)_{t}\overset{\text{def}}{=}y_{t-1}, for every $t=1,\dots,T-1$
y_lag_1=y[-length(y)]
#
# The data frame used in the DF test, whose residuals are tested in the Breusch-Godfrey test is the given by.
DF_LM_df <- data.frame(z, tt, y_lag_1)
head(DF_LM_df, 3)
#          z tt y_lag_1
# 1 261.3301  1 7902.09
# 2 130.8896  2 8163.42
# 3 551.5205  3 8294.31
tail(DF_LM_df, 3)
#              z   tt  y_lag_1
# 1717  89.76953 1717 16552.57
# 1718 -39.75586 1718 16642.34
# 1719 -55.08984 1719 16602.59
#
# The linear regression used in the DF test is
lm(z~tt+y_lag_1, data=DF_LM_df)
# Call: lm(formula = z ~ tt + y_lag_1, data = DF_LM_df)
# Coefficients: (Intercept)           tt      y_lag_1
#                  38.23063      0.01480     -0.00219  
#
# Note that the coefficients of the linear regression are exactly the same as the coefficients in the linear regression used in the DF test.
# Clearly,
lmtest::bgtest(lm(z~tt+y_lag_1, data=DF_LM_df), order=10, type="Chisq", fill=NA)
# Breusch-Godfrey test for serial correlation of order up to 10
# data:  lm(z ~ tt + y_lag_1, data = BG_df)
# LM test = 26.997, df = 10, p-value = 0.002608
#
# Note also that the forecast::checkresiduals() function should be able to perform both the Ljung-Box and Breusch-Godfrey test. However,
# there are slightly differences in the results.
forecast::checkresiduals(lm(z~tt+y_lag_1, data=DF_LM_df), lag=10, test="LB", plot=TRUE)
# Ljung-Box test
# data:  Residuals
# Q* = 26.923, df = 10, p-value = 0.002678
# Model df: 0.   Total lags used: 10
#
forecast::checkresiduals(lm(z~tt+y_lag_1, data=DF_LM_df), lag=10, test="BG", plot=TRUE)
# Breusch-Godfrey test for serial correlation of order up to 10
# data:  Residuals
# LM test = 27.225, df = 10, p-value = 0.002399
#
# In particular, the Ljung-Box test executed by the forecast::checkresiduals() function does not account of the 3 degrees of freedom used by the 
# linear model model. In fact,
Box.test(y_res, lag=l,   fitdf=0, type="Ljung-Box")
# X-squared = 26.923, df = 10, p-value = 0.002678
#
# In general, we have discovered some potential issues when using the forecast::checkresiduals() function. Therefore, we advise using it 
# with caution.
#
# From the computational and visual evidences we can conclude that we have to reject at the $1\%$ significance level the null hypothesis of 
# no autocorrelation in the residuals of the linear regression used in the DF test.  
# 
# The main issue with the above autocorrelation analysis on the residuals of the linear model used in the DF test is that the tests we have
# applied loose power when the residuals are affected by conditional heteroscedasticity. Dalla, Giraitis, and Phillips (see Dalla V., 
# Giraitis L., Phillips, P.C.B., Robust Tests for White Noise and Cross-Correlation, Cambridge University Press, 21 September 2020, 
# https://www.cambridge.org/core/journals/econometric-theory/article/robust-tests-for-white-noise-and-crosscorrelation/4D77C12C52433F4C6735E584C779403A)
# have provided robust versions of autocorrelation tests to overcome this issue. Moreover, they have built suitable R functions to perform
# these robust versions of autocorrelation tests (see https://cran.r-project.org/web/packages/testcorr/vignettes/testcorr.pdf; see also
# https://cran.r-project.org/web/packages/testcorr/testcorr.pdf). A line plot of the residuals yields clear visual evidence for conditional
# heteroscedasticity.
head(BTC_train_df, 3)
#   t       Date    Open    High     Low   Close Adj.Close     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31 7063209984
tail(BTC_train_df, 3)
#         t       Date     Open     High      Low    Close Adj.Close      Volume
# 1718 1718 2022-12-29 16552.32 16651.76 16508.68 16642.34  16642.34 14472237479
# 1719 1719 2022-12-30 16641.33 16643.43 16408.47 16602.59  16602.59 15929162910
# 1720 1720 2022-12-31 16603.67 16628.99 16517.52 16547.50  16547.50 11239186456
#
Data_df <- BTC_train_df
head(Data_df)
Data_df <- add_column(Data_df, DF_y_res=c(NA,as.vector(y_ADF_ur.df_trend_0_lags@testreg[["residuals"]])), .after="Adj.Close")
head(Data_df, 3)
#   t       Date    Open    High     Low   Close Adj.Close     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31 7063209984
Data_df <- dplyr::rename(Data_df, x=t, y=DF_y_res)
TrnS_length <- length(Data_df$Adj.Close)
show(TrnS_length)
# 1720
First_Day <- as.character(Data_df$Date[2])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Residuals of the Linear Model for the DF Test on the Bitcoin Daily Adjusted Close Price Training Set - from ", .(First_Day), " to ", .(Last_Day), sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points. Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("")
# numbers::primeFactors(TrnS_length)
x_breaks_num <- 40 # (deduced from primeFactors(TrnS_length-2))
x_breaks_low <- Data_df$x[1]
x_breaks_up <- Data_df$x[(TrnS_length)]
x_binwidth <- ceiling((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Date[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("residuals of the linear model")
y_max <- max(na.rm(Data_df$y))
y_min <- min(na.rm(Data_df$y))
as.numeric(floor(y_max-y_min))
y_breaks_num <- 10
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor((y_min/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((y_max/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth), digits=3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.5
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
point_black <- bquote("GARCH(1,1) stand. residuals")
line_green <- bquote("regression line")
line_red <- bquote("LOESS curve")
leg_point_labs <- c(point_black)
leg_point_cols <- c("point_black"="black")
leg_point_breaks <- c("point_black")
leg_line_labs <- c(line_green, line_red)
leg_line_cols <- c("line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_green", "line_red")
leg_col_labs   <- c(leg_point_labs,leg_line_labs)
leg_col_cols   <- c(leg_point_cols,leg_line_cols)
leg_col_breaks <- c(leg_point_breaks,leg_line_breaks)
BTC_ACP_TrnS_DF_res_sp <- ggplot(Data_df, aes(x=x)) +
  geom_smooth(aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_point(aes(y=y, color="point_black"), alpha=1, size=0.7, shape=19, na.rm=TRUE) + 
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  guides(colour=guide_legend(override.aes=list(shape=c(19, NA, NA), linetype=c("blank", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_ACP_TrnS_DF_res_sp)
#
# The line plot
BTC_ACP_TrnS_DF_res_lp <- ggplot(Data_df, aes(x=x)) +
  geom_smooth(aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_line(aes(y=y, color="point_black"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_ACP_TrnS_DF_res_lp)
#
# Therefore, we consider the application of the testcorr::ac.test() function.
# library(testcorr)
y_res <- as.vector(y_ADF_ur.df_trend_0_lags@testreg[["residuals"]])
testcorr::ac.test(y_res, max.lag = 10, alpha = 0.05, lambda = 2.576, plot = TRUE, table = TRUE, var.name = NULL, scale.font = 1)
#   | Lag|     AC|  Stand. CB(95%)|  Robust CB(95%)| Lag|      t| p-value| t-tilde| p-value| Lag|     LB| p-value| Q-tilde| p-value|
#   |---:|------:|---------------:|---------------:|---:|------:|-------:|-------:|-------:|---:|------:|-------:|-------:|-------:|
#   |   1| -0.031| (-0.047, 0.047)| (-0.072, 0.072)|   1| -1.286|   0.198|  -0.848|   0.397|   1|  1.658|   0.198|   0.719|   0.397|
#   |   2|  0.008| (-0.047, 0.047)| (-0.077, 0.077)|   2|  0.343|   0.731|   0.211|   0.833|   2|  1.776|   0.412|   0.763|   0.683|
#   |   3|  0.021| (-0.047, 0.047)| (-0.072, 0.072)|   3|  0.870|   0.384|   0.574|   0.566|   3|  2.535|   0.469|   1.093|   0.779|
#   |   4|  0.038| (-0.047, 0.047)| (-0.086, 0.086)|   4|  1.560|   0.119|   0.860|   0.390|   4|  4.977|   0.290|   1.833|   0.766|
#   |   5|  0.008| (-0.047, 0.047)| (-0.080, 0.080)|   5|  0.334|   0.738|   0.197|   0.844|   5|  5.089|   0.405|   1.871|   0.867|
#   |   6|  0.028| (-0.047, 0.047)| (-0.081, 0.081)|   6|  1.177|   0.239|   0.686|   0.493|   6|  6.480|   0.372|   2.342|   0.886|
#   |   7| -0.046| (-0.047, 0.047)| (-0.089, 0.089)|   7| -1.917|   0.055|  -1.018|   0.309|   7| 10.173|   0.179|   3.378|   0.848|
#   |   8| -0.040| (-0.047, 0.047)| (-0.091, 0.091)|   8| -1.652|   0.099|  -0.855|   0.393|   8| 12.918|   0.115|   4.109|   0.847|
#   |   9|  0.088| (-0.047, 0.047)| (-0.083, 0.083)|   9|  3.653|   0.000|   2.073|   0.038|   9| 26.350|   0.002|   8.408|   0.494|
#   |  10|  0.018| (-0.047, 0.047)| (-0.074, 0.074)|  10|  0.754|   0.451|   0.483|   0.629|  10| 26.923|   0.003|   8.641|   0.567|
#
# From the above table and the associated plot, we have computational evidence that the robust Q-tilde statistic does not reject the
# null hypothesis of autocorrelation at the $5\%$ significance level. Nevertheless, the robust $95\%$ confidence bands yield visual evidence 
# for rejecting the null hypothesis at the $5\%$ significance level. Note that, regarding the Ljung-Box statistic, the test is executed  
# under the option fitdf=0. In fact,
for(l in 1:max_lag){
  show(Box.test(y_res, lag=l,   fitdf=0, type="Ljung-Box"))
}
# Box-Ljung test
# data:  y_res
# X-squared = 1.6576, df =  1, p-value = 0.1979
# X-squared = 1.7757, df =  2, p-value = 0.4115
# X-squared = 2.5350, df =  3, p-value = 0.469
# X-squared = 4.9771, df =  4, p-value = 0.2897
# X-squared = 5.0892, df =  5, p-value = 0.4051
# X-squared = 6.4799, df =  6, p-value = 0.3716
# X-squared = 10.173, df =  7, p-value = 0.179
# X-squared = 12.918, df =  8, p-value = 0.1147
# X-squared = 26.350, df =  9, p-value = 0.00179
# X-squared = 26.923, df = 10, p-value = 0.002678
#
# Cause the mixed evidences about the possible autocorrelation in the residuals of the linear model used for the DF test, we consider the
# ADF test in which we use a linear model with a large number of lags (with "long" lags), according to the Schwert formula (1989).
long_lags <- floor(12*(length(y)/100)^(1/4)) # the Schwert formula
show(long_lags)
# 24
y_ADF_ur.df_trend_long_lags <- ur.df(y, type="trend", lags=long_lags, selectlags="Fixed")
lm(formula=y_ADF_ur.df_trend_long_lags@testreg[["terms"]])
# Call: lm(formula=y_ADF_ur.df_trend_long_lags@testreg[["terms"]])
# Coefficients: (Intercept)       z.lag.1            tt       z.diff.lag1   z.diff.lag2   z.diff.lag3   z.diff.lag4   z.diff.lag5    
#                34.771230     -0.002470         0.024961    -0.019871      0.012292      0.030042      0.026443      0.014041       
#                                                             z.diff.lag6   z.diff.lag7   z.diff.lag8   z.diff.lag9   z.diff.lag10
#                                                               0.032416    -0.057108    -0.038416      0.088120      0.033385 
#                                                              z.diff.lag11  z.diff.lag12  z.diff.lag13  z.diff.lag14  z.diff.lag15  
#                                                             -0.001773    -0.056122      0.036488    -0.003435    -0.029077
#                                                              z.diff.lag16  z.diff.lag17  z.diff.lag18  z.diff.lag19  z.diff.lag20
#                                                              -0.015045      0.025051   -0.007098    -0.061448      0.050838 
#                                                              z.diff.lag21  z.diff.lag22  z.diff.lag23  z.diff.lag24  
#                                                              -0.011420      0.015215    -0.045323      0.077784  
lag_num <- long_lags
show(lag_num)
# 24
nobs(lm(formula=y_ADF_ur.df_trend_long_lags@testreg[["terms"]]))
# 1695
n_obs <- length(y)-(lag_num+1)
show(n_obs)
# 1695
n_coeffs <- nrow(y_ADF_ur.df_trend_long_lags@testreg[["coefficients"]])
show(n_coeffs)
# 27
df.residual(lm(formula=y_ADF_ur.df_trend_long_lags@testreg[["terms"]]))
# 1668
df_res <- n_obs-n_coeffs
show(df_res)
# 1668
summary(y_ADF_ur.df_trend_long_lags)
# ############################################### 
# # Augmented Dickey-Fuller Test Unit Root Test # 
# ############################################### 
# Test regression trend 
# Call: lm(formula=z.diff ~ z.lag.1 + 1 + tt + z.diff.lag)
#
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6599.9 -224.4  -17.4   219.6  7098.3 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  34.771230  50.846285   0.684 0.494165    
# z.lag.1      -0.002470   0.001964  -1.258 0.208748    
# tt            0.024961   0.069194   0.361 0.718341    
# z.diff.lag1  -0.019871   0.024434  -0.813 0.416191    
# z.diff.lag2   0.012292   0.024408   0.504 0.614594    
# z.diff.lag3   0.030042   0.024409   1.231 0.218581    
# z.diff.lag4   0.026443   0.024417   1.083 0.278967    
# z.diff.lag5   0.014041   0.024397   0.576 0.565015    
# z.diff.lag6   0.032416   0.024346   1.331 0.183221    
# z.diff.lag7  -0.057108   0.024358  -2.345 0.019169 *  
# z.diff.lag8  -0.038416   0.024393  -1.575 0.115479    
# z.diff.lag9   0.088120   0.024406   3.611 0.000315 ***
# z.diff.lag10  0.033385   0.024488   1.363 0.172964    
# z.diff.lag11 -0.001773   0.024501  -0.072 0.942318    
# z.diff.lag12 -0.056122   0.024490  -2.292 0.022053 *  
# z.diff.lag13  0.036488   0.024484   1.490 0.136336    
# z.diff.lag14 -0.003435   0.024499  -0.140 0.888504    
# z.diff.lag15 -0.029077   0.024488  -1.187 0.235242    
# z.diff.lag16 -0.015045   0.024415  -0.616 0.537819    
# z.diff.lag17  0.025051   0.024390   1.027 0.304535    
# z.diff.lag18 -0.007098   0.024349  -0.292 0.770687    
# z.diff.lag19 -0.061448   0.024344  -2.524 0.011689 *  
# z.diff.lag20  0.050838   0.024389   2.084 0.037269 *  
# z.diff.lag21 -0.011420   0.024414  -0.468 0.640022    
# z.diff.lag22  0.015215   0.024407   0.623 0.533098    
# z.diff.lag23 -0.045323   0.024409  -1.857 0.063508 .  
# z.diff.lag24  0.077784   0.024428   3.184 0.001478 ** 
#  ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1021 on 1668 degrees of freedom
# Multiple R-squared:  0.0381,	Adjusted R-squared:  0.0231 
# F-statistic: 2.541 on 26 and 1668 DF,  p-value: 3.176e-05
# 
# Value of test-statistic is: -1.2575 0.6874 1.0149 
# Critical values for test statistics: 
#   1pct  5pct 10pct
# tau3 -3.96 -3.41 -3.12
# phi2  6.09  4.68  4.03
# phi3  8.27  6.25  5.34
#
# The impossibility of rejecting the null hypothesis against the three alternatives at $10\%$ significance level is confirmed. However, 
# to validate the test, we need to check again the possible presence of autocorrelation in the residuals of the model used for the test.
y_res <- as.vector(y_ADF_ur.df_trend_long_lags@testreg[["residuals"]])
max_lag <- ceiling(min(10, n_obs/4))    # Hyndman (for data without seasonality, see https://robjHyndman.com/hy_resndsight/ljung-box-test/)
n_pars <- n_coeffs
fit_df <- n_pars
Box.test(y_res, lag=max_lag, fitdf=fit_df, type="Ljung-Box")
# Box-Ljung test 
# data:  y_res
# X-squared=0.27571, df=-17, p-value=NA
# Warning message: In pchisq(STATISTIC, lag - fitdf) : NaNs produced
#
# This is just to shows that to perform the Ljung-Box test accounting of the presence of many parameters estimated by the model it is 
# necessary to give the fitdf option the following value.
LB_fit_df <- min(min(max_lag, n_pars), max_lag-1)
show(LB_fit_df)
# 9
Box.test(y_res, lag=max_lag, fitdf=LB_fit_df, type="Ljung-Box")
# Box-Ljung test
# data:  y_res
# X-squared=0.27571, df=1, p-value=0.5995
#
# The result of the test is confirmed by applying the FitAR::LjungBoxTest() function.
FitAR::LjungBoxTest(y_res, k=n_pars, lag.max=max_lag, StartLag=1, SquaredQ=FALSE)
#  m   Qm    pvalue
#  1 0.00 0.9903727
#  2 0.00 0.9654270
#  3 0.03 0.8615421
#  4 0.04 0.8407428
#  5 0.10 0.7510447
#  6 0.12 0.7280017
#  7 0.17 0.6799408
#  8 0.25 0.6148277
#  9 0.28 0.5995608
# 10 0.28 0.5995240
# 
# This result can be also obtained by the following loops.
n_pars_seq <- rep(NA,max_lag)
for(l in 1:max_lag){
  if(l-n_pars<0) n_pars_seq[l] <- l-1
  else n_pars_seq[l] <- n_pars
}
show(n_pars_seq)
# 0 1 2 3 4 5 6 7 8 9
#
# In the same way,
for(l in 1:max_lag){
  show(Box.test(y_res, lag=l,  fitdf=n_pars_seq[l], type="Ljung-Box"))
}
# Box-Ljung test
# data:  y_res
# X-squared=0.0001456, df=1, p-value=0.9904
# X-squared=0.0018787, df=1, p-value=0.9654
# X-squared=0.030419,  df=1, p-value=0.8615
# X-squared=0.040378,  df=1, p-value=0.8407
# X-squared=0.10066,   df=1, p-value=0.7510
# X-squared=0.12095,   df=1, p-value=0.7280
# X-squared=0.17019,   df=1, p-value=0.6799
# X-squared=0.2532,    df=1, p-value=0.6148
# X-squared=0.27566,   df=1, p-value=0.5996
# X-squared=0.27571,   df=1, p-value=0.5995
#
# From the above tests it turns out that we cannot reject the null hypothesis of no autocorrelation at any standard significance level.
#
# The portes::LjungBox() function fails in performing the test under the defauls options.
portes::LjungBox(y_res, lags=seq(1, max_lag, by=1), fitdf=n_pars, sqrd.res=FALSE)
# lags   statistic df p-value
#  1 0.0001455969  0      NA
#  2 0.0018787388  0      NA
#  3 0.0304193459  0      NA
#  4 0.0403782529  0      NA
#  5 0.1006552948  0      NA
#  6 0.1209543209  0      NA
#  7 0.1701924843  0      NA
#  8 0.2532044565  0      NA
#  9 0.2756588713  0      NA
# 10 0.2757143655  0      NA
#
# Again, the portes::LjungBox() function does not calculate the p-values corresponding to the lags having a non-positive difference with 
# max_lag. This issue can be overcome by redefining the lag sequence as proposed by R. Hyndman (see forecast::checkresiduals() function, see 
# also https://robjhyndman.com/hyndsight/ljung_box_df.html).
lag_seq <- rep(NA,max_lag)
for(l in 1:max_lag){
  if(l-n_pars<=2) lag_seq[l] <- max(l,(n_pars+3)) # Hyndman
  else lag_seq[l] <- l
}
# However, the result seems to be not valuable.
show(lag_seq)
# 30 30 30 30 30 30 30 30 30 30
#
portes::LjungBox(y_res, lags=lag_seq, fitdf=n_pars, sqrd.res=FALSE)
# lags statistic df      p-value
# 30    19.226  3 0.0002455019
# 30    19.226  3 0.0002455019
# 30    19.226  3 0.0002455019
# 30    19.226  3 0.0002455019
# 30    19.226  3 0.0002455019
# 30    19.226  3 0.0002455019
# 30    19.226  3 0.0002455019
# 30    19.226  3 0.0002455019
# 30    19.226  3 0.0002455019
# 30    19.226  3 0.0002455019
#
# Actually, performing the Ljung-Box test under Hyndman's lag=H_max_lag options returns a results which contradicts what found above leading
# to the rejection of the null hypothesis of non autocorrelation at the $1\%$ significance level.
H_max_lag <- max(max_lag,(n_pars+3))
show(H_max_lag)
# 30
Box.test(y_res, lag=H_max_lag, fitdf=fit_df, type="Ljung-Box")
# Box-Ljung test
# data:  y_res
# X-squared = 19.226, df = 3, p-value = 0.0002455
#
# The function forecast::checkresiduals() aso yields contradictory resuls.
forecast::checkresiduals(lm(formula=y_ADF_ur.df_trend_long_lags@testreg[["terms"]]), lag=10, test="LB", plot=TRUE)
# Ljung-Box test
# data:  Residuals
# Q* = 0.27571, df = 10, p-value = 1
# Model df: 0.   Total lags used: 10
#
forecast::checkresiduals(lm(formula=y_ADF_ur.df_trend_long_lags@testreg[["terms"]]), lag=10, test="BG", plot=TRUE)
# Breusch-Godfrey test for serial correlation of order up to 10
# data:  Residuals
# LM test = 40.349, df = 10, p-value = 0.00001471

# We consider the autocorrelogram o the residuals
y_res <- as.vector(y_ADF_ur.df_trend_long_lags@testreg[["residuals"]])
T <- n_obs
# max_lag <- ceiling(10*log10(T))    # Default
# max_lag <- ceiling(sqrt(n)+45)     # Box-Jenkins
max_lag <- ceiling(min(10, T/4))     # Hyndman (for data without seasonality)
# max_lag <- ceiling(min(2*12, T/5)) # Hyndman (for data with seasonality, see https://robjHyndman.com/hy_resndsight/ljung-box-test/)
# Aut_Fun_y <- stats::acf(y, lag.max=max_lag, type="correlation", plot=FALSE)
Aut_Fun_y_res <- TSA::acf(y_res, lag.max=max_lag, type= "correlation", plot=FALSE)
ci_090 <- qnorm((1+0.90)/2)/sqrt(T)
ci_95 <- qnorm((1+0.95)/2)/sqrt(T)
ci_99 <- qnorm((1+0.99)/2)/sqrt(T)
Aut_Fun_y_res <- data.frame(lag=Aut_Fun_y_res$lag, acf=Aut_Fun_y_res$acf)
# First_Date <- paste(Data_df$Month[1],Data_df$year[1])
# Last_Date <- paste(Data_df$Month[T],Data_df$year[T])
First_Date <- as.character(Data_df$Date[1])
Last_Date <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025", 
                             paste("Autocorrelogram of the Residuals of the Linear Model with", " \"Long\" ", "Lags for the ADF Test for the Bitcoin Daily Adjusted Close from ", .(First_Date), " to ", .(Last_Date))))
# link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
# subtitle_content <- bquote(paste("Data set size ",.(TrnS_length)," sample points. Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("lags")
x_breaks_num <- max_lag
x_binwidth <- 1
x_breaks <- Aut_Fun_y_res$lag
x_labs <- format(x_breaks, scientific=FALSE)
Plot_Aut_Fun_y_res <- ggplot(Aut_Fun_y_res, aes(x=lag, y=acf))+
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), linewidth=1, col= "black") +
  # geom_col(mapping=NULL, data=NULL, position= "dodge", width=0.1, col= "black", inherit.aes=TRUE)+
  geom_hline(aes(yintercept=-ci_090, color= "CI_090"), show.legend=TRUE, lwd=0.9, lty=3) +
  geom_hline(aes(yintercept=ci_090, color= "CI_090"), lwd=0.9, lty=3) +
  geom_hline(aes(yintercept=ci_95, color= "CI_95"), show.legend=TRUE, lwd=0.8, lty=2)+
  geom_hline(aes(yintercept=-ci_95, color= "CI_95"), lwd=0.8, lty=2) +
  geom_hline(aes(yintercept=-ci_99, color= "CI_99"), show.legend=TRUE, lwd=0.8, lty=4) +
  geom_hline(aes(yintercept=ci_99, color= "CI_99"), lwd=0.8, lty=4) +
  scale_x_continuous(name= "lag", breaks=x_breaks, label=x_labs) +
  scale_y_continuous(name= "acf value", breaks=waiver(), labels=NULL,
                     sec.axis=sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name= "Conf. Inter.", labels=c("90%","95%","99%"), values=c(CI_090="green", CI_95="blue", CI_99="red"),
                     guide=guide_legend(override.aes=list(linetype=c("dotted", "dashed", "dotdash")))) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
  # theme(plot.title=element_blank(), 
  #       plot.subtitle=element_blank(),
  #       plot.caption=element_text(hjust=1.0),
  #       legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(Plot_Aut_Fun_y_res)
#
# From the autocorrelogram we have clear visual evidence of no autocorrelation in the residuals.
#
# We again consider the Breusch-Godfrey test for autocorrelation. Applying the test to the residuals of the linear model used for the ADF 
# test also seems natural in this case. Hence, we apply the test to this linear model similarly to what we have done above.
lm(formula=y_ADF_ur.df_trend_long_lags@testreg[["terms"]])
# Call: lm(formula = y_ADF_ur.df_trend_long_lags@testreg[["terms"]])
# Coefficients:  (Intercept)     z.lag.1            tt     z.diff.lag1   z.diff.lag2   z.diff.lag3   z.diff.lag4   z.diff.lag5   z.diff.lag6   
#                 34.771230     -0.002470      0.024961     -0.019871      0.012292      0.030042      0.026443      0.014041      0.032416     
#                                                          z.diff.lag7   z.diff.lag8   z.diff.lag9  z.diff.lag10  z.diff.lag11  z.diff.lag12
#                                                           -0.057108    -0.038416      0.088120      0.033385     -0.001773     -0.056122
#                                                         z.diff.lag13  z.diff.lag14  z.diff.lag15  z.diff.lag16  z.diff.lag17  z.diff.lag18 
#                                                            0.036488     -0.003435     -0.029077     -0.015045      0.025051     -0.007098 
#                                                         z.diff.lag19  z.diff.lag20  z.diff.lag21  z.diff.lag22  z.diff.lag23  z.diff.lag24  
#                                                           -0.061448      0.050838     -0.011420      0.015215     -0.045323      0.077784
# From the autocorrelogram we have no visual evidence for autocorrelation till the 10th lag. Therefore, we apply the Breusch-Godfrey test to
# check the presence of autocorrelation till the 10th order.
lmtest::bgtest(lm(formula=y_ADF_ur.df_trend_long_lags@testreg[["terms"]]), order=10, type="Chisq", fill=NA)
# Breusch-Godfrey test for serial correlation of order up to 10
# data:  lm(formula = y_ADF_ur.df_trend_long_lags@testreg[["terms"]])
# LM test = 43.274, df = 10, p-value = 4.443e-06
#
# The Breusch-Godfrey test rejects the null of no autocorrelation at the $1\%$ significance level. This contradicts the computational
# evidence from the Ljung-Box test and the visual evidence from the autocorrelogram. Note also that the lmtest::bgtest() function yields
# a slightly different result from the forecast::checkresiduals() function.
#
# We consider the handmade linear model with "long" lags for the ADF test and the corresponding handmade linear model for the 
# Breusch-Godfrey test on the residuals of the former.
Data_df <- BTC_train_df
head(Data_df)
y <- Data_df$Adj.Close
length(y)
# 1720
z <- c(diff(y,differences=1))
length(z)
# 1719
tt <- c(1:(length(y)-1))
length(tt)
# 1719
y_lag_1 <- y[-length(y)]
length(y_lag_1)
# 1719
y_lag_2 <- c(NA,y_lag_1[-length(y_lag_1)])
y_lag_3 <- c(NA,y_lag_2[-length(y_lag_2)])
y_lag_4 <- c(NA,y_lag_3[-length(y_lag_3)])
y_lag_5 <- c(NA,y_lag_4[-length(y_lag_4)])
y_lag_6 <- c(NA,y_lag_5[-length(y_lag_5)])
y_lag_7 <- c(NA,y_lag_6[-length(y_lag_6)])
y_lag_8 <- c(NA,y_lag_7[-length(y_lag_7)])
y_lag_9 <- c(NA,y_lag_8[-length(y_lag_8)])
y_lag_10 <- c(NA,y_lag_9[-length(y_lag_9)])
y_lag_11 <- c(NA,y_lag_10[-length(y_lag_10)])
y_lag_12 <- c(NA,y_lag_11[-length(y_lag_11)])
y_lag_13 <- c(NA,y_lag_12[-length(y_lag_12)])
y_lag_14 <- c(NA,y_lag_13[-length(y_lag_13)])
y_lag_15 <- c(NA,y_lag_14[-length(y_lag_14)])
y_lag_16 <- c(NA,y_lag_15[-length(y_lag_15)])
y_lag_17 <- c(NA,y_lag_16[-length(y_lag_16)])
y_lag_18 <- c(NA,y_lag_17[-length(y_lag_17)])
y_lag_19 <- c(NA,y_lag_18[-length(y_lag_18)])
y_lag_20 <- c(NA,y_lag_19[-length(y_lag_19)])
y_lag_21 <- c(NA,y_lag_20[-length(y_lag_20)])
y_lag_22 <- c(NA,y_lag_21[-length(y_lag_21)])
y_lag_23 <- c(NA,y_lag_22[-length(y_lag_22)])
y_lag_24 <- c(NA,y_lag_23[-length(y_lag_23)])
#
z_diff_lag_1 <- c(NA,diff(y_lag_1, differences=1))
length(z_diff_lag_1)
# 1719
z_diff_lag_2 <- c(NA,diff(y_lag_2, differences=1))
z_diff_lag_3 <- c(NA,diff(y_lag_3, differences=1))
z_diff_lag_4 <- c(NA,diff(y_lag_4, differences=1))
z_diff_lag_5 <- c(NA,diff(y_lag_5, differences=1))
z_diff_lag_6 <- c(NA,diff(y_lag_6, differences=1))
z_diff_lag_7 <- c(NA,diff(y_lag_7, differences=1))
z_diff_lag_8 <- c(NA,diff(y_lag_8, differences=1))
z_diff_lag_9 <- c(NA,diff(y_lag_9, differences=1))
z_diff_lag_10 <- c(NA,diff(y_lag_10, differences=1))
z_diff_lag_11 <- c(NA,diff(y_lag_11, differences=1))
z_diff_lag_12 <- c(NA,diff(y_lag_12, differences=1))
z_diff_lag_13 <- c(NA,diff(y_lag_13, differences=1))
z_diff_lag_14 <- c(NA,diff(y_lag_14, differences=1))
z_diff_lag_15 <- c(NA,diff(y_lag_15, differences=1))
z_diff_lag_16 <- c(NA,diff(y_lag_16, differences=1))
z_diff_lag_17 <- c(NA,diff(y_lag_17, differences=1))
z_diff_lag_18 <- c(NA,diff(y_lag_18, differences=1))
z_diff_lag_19 <- c(NA,diff(y_lag_19, differences=1))
z_diff_lag_20 <- c(NA,diff(y_lag_20, differences=1))
z_diff_lag_21 <- c(NA,diff(y_lag_21, differences=1))
z_diff_lag_22 <- c(NA,diff(y_lag_22, differences=1))
z_diff_lag_23 <- c(NA,diff(y_lag_23, differences=1))
z_diff_lag_24 <- c(NA,diff(y_lag_24, differences=1))
#
ADF_LM_df <- data.frame(z, tt, y_lag_1, z_diff_lag_1, z_diff_lag_2, z_diff_lag_3, z_diff_lag_4, z_diff_lag_5, z_diff_lag_6, z_diff_lag_7, 
                        z_diff_lag_8, z_diff_lag_9, z_diff_lag_10, z_diff_lag_11, z_diff_lag_12, z_diff_lag_13, z_diff_lag_14, z_diff_lag_15,
                        z_diff_lag_16, z_diff_lag_17, z_diff_lag_18, z_diff_lag_19, z_diff_lag_20, z_diff_lag_21, z_diff_lag_22, z_diff_lag_23,
                        z_diff_lag_24)
head(ADF_LM_df, 3)
#          z tt y_lag_1 z_diff_lag_1 z_diff_lag_2 z_diff_lag_3 z_diff_lag_4 z_diff_lag_5 z_diff_lag_6 z_diff_lag_7 z_diff_lag_8 z_diff_lag_9
# 1 261.3301  1 7902.09           NA           NA           NA           NA           NA           NA           NA           NA           NA
# 2 130.8896  2 8163.42     261.3301           NA           NA           NA           NA           NA           NA           NA           NA
# 3 551.5205  3 8294.31     130.8896     261.3301           NA           NA           NA           NA           NA           NA           NA
#   z_diff_lag_10 z_diff_lag_11 z_diff_lag_12 z_diff_lag_13 z_diff_lag_14 z_diff_lag_15 z_diff_lag_16 z_diff_lag_17 z_diff_lag_18 z_diff_lag_19
# 1            NA            NA            NA            NA            NA            NA            NA            NA            NA            NA
# 2            NA            NA            NA            NA            NA            NA            NA            NA            NA            NA
# 3            NA            NA            NA            NA            NA            NA            NA            NA            NA            NA
#   z_diff_lag_20 z_diff_lag_21 z_diff_lag_22 z_diff_lag_23 z_diff_lag_24
# 1            NA            NA            NA            NA            NA
# 2            NA            NA            NA            NA            NA
# 3            NA            NA            NA            NA            NA
tail(ADF_LM_df, 3)
#              z   tt  y_lag_1 z_diff_lag_1 z_diff_lag_2 z_diff_lag_3 z_diff_lag_4 z_diff_lag_5 z_diff_lag_6 z_diff_lag_7 z_diff_lag_8
# 1717  89.76953 1717 16552.57   -164.60156   -202.63086     77.81836    -5.769531    50.802734   -33.388672     12.80664    -88.76953
# 1718 -39.75586 1718 16642.34     89.76953   -164.60156   -202.63086    77.818360    -5.769531    50.802734    -33.38867     12.80664
# 1719 -55.08984 1719 16602.59    -39.75586     89.76953   -164.60156  -202.630860    77.818360    -5.769531     50.80273    -33.38867
#      z_diff_lag_9 z_diff_lag_10 z_diff_lag_11 z_diff_lag_12 z_diff_lag_13 z_diff_lag_14 z_diff_lag_15 z_diff_lag_16 z_diff_lag_17 z_diff_lag_18
# 1717    466.62500    -318.29688     -37.11523     147.60742    -717.38086     -450.7852      34.33203     574.88086     102.24414     -24.53125
# 1718    -88.76953     466.62500    -318.29688     -37.11523     147.60742     -717.3809    -450.78516      34.33203     574.88086     102.24414
# 1719     12.80664     -88.76953     466.62500    -318.29688     -37.11523      147.6074    -717.38086    -450.78516      34.33203     574.88086
#      z_diff_lag_19 z_diff_lag_20 z_diff_lag_21 z_diff_lag_22 z_diff_lag_23 z_diff_lag_24
# 1717     -4.427735   -100.322265    385.347656     -241.3770      114.6777     -155.6602
# 1718    -24.531250     -4.427735   -100.322265      385.3477     -241.3770      114.6777
# 1719    102.244141    -24.531250     -4.427735     -100.3223      385.3477     -241.3770
#
lm(z~tt+y_lag_1+z_diff_lag_1+z_diff_lag_2+z_diff_lag_3+z_diff_lag_4+z_diff_lag_5+z_diff_lag_6+z_diff_lag_7+z_diff_lag_8+z_diff_lag_9+z_diff_lag_10
   +z_diff_lag_11+z_diff_lag_12+z_diff_lag_13+z_diff_lag_14+z_diff_lag_15+z_diff_lag_16+z_diff_lag_17+z_diff_lag_18+z_diff_lag_19+z_diff_lag_20
   +z_diff_lag_21+z_diff_lag_22+z_diff_lag_23+z_diff_lag_24, data=ADF_LM_df)
# Call: lm(formula = z ~ tt + y_lag_1 + z_diff_lag_1 + z_diff_lag_2 + z_diff_lag_3 + z_diff_lag_4 + z_diff_lag_5 + z_diff_lag_6 + 
#                        + z_diff_lag_7 + z_diff_lag_8 + z_diff_lag_9 + z_diff_lag_10 + z_diff_lag_11 + z_diff_lag_12 + z_diff_lag_13 
#                        + z_diff_lag_14 + z_diff_lag_15 + z_diff_lag_16 + z_diff_lag_17 + z_diff_lag_18 + z_diff_lag_19 + z_diff_lag_20
#                        + z_diff_lag_21 + z_diff_lag_22 + z_diff_lag_23 + z_diff_lag_24, data = ADF_LM_df)
# 
# Coefficients: (Intercept)      tt      y_lag_1    z_diff_lag_1   z_diff_lag_2   z_diff_lag_3   z_diff_lag_4   z_diff_lag_5  z_diff_lag_6   
#                34.771230    0.024961  -0.002470    -0.019871       0.012292       0.030042       0.026443       0.014041     0.032416   
#                                                    z_diff_lag_7   z_diff_lag_8   z_diff_lag_9  z_diff_lag_10  z_diff_lag_11  z_diff_lag_12   
#                                                    -0.057108      -0.038416       0.088120       0.033385      -0.001773      -0.056122    
#                                                    z_diff_lag_13  z_diff_lag_14  z_diff_lag_15 z_diff_lag_16  z_diff_lag_17  z_diff_lag_18 
#                                                     0.036488      -0.003435      -0.029077       -0.015045       0.025051      -0.007098
#                                                    z_diff_lag_19  z_diff_lag_20  z_diff_lag_21  z_diff_lag_22  z_diff_lag_23  z_diff_lag_24  
#                                                    -0.061448       0.050838       -0.011420       0.015215      -0.045323       0.077784 
# 
# The coefficients of the handmade linear model are the same as the coefficients of the linear model built by the ur.df() function.
# Applying the Breusch-Godfrey test on the handmade linear model we clearly obtain the same result
lmtest::bgtest(lm(z~tt+y_lag_1+z_diff_lag_1+z_diff_lag_2+z_diff_lag_3+z_diff_lag_4+z_diff_lag_5+z_diff_lag_6+z_diff_lag_7+z_diff_lag_8+z_diff_lag_9+z_diff_lag_10
                  +z_diff_lag_11+z_diff_lag_12+z_diff_lag_13+z_diff_lag_14+z_diff_lag_15+z_diff_lag_16+z_diff_lag_17+z_diff_lag_18+z_diff_lag_19+z_diff_lag_20
                  +z_diff_lag_21+z_diff_lag_22+z_diff_lag_23+z_diff_lag_24, data=ADF_LM_df), order=10, type="Chisq", fill=NA)
# Breusch-Godfrey test for serial correlation of order up to 10
# data:  lm(z ~ tt + y_lag_1 + z_diff_lag_1 + z_diff_lag_2 + z_diff_lag_3 + z_diff_lag_4 + z_diff_lag_5 + z_diff_lag_6 + z_diff_lag_7 
#               + z_diff_lag_8 + z_diff_lag_9 + z_diff_lag_10 + z_diff_lag_11 + z_diff_lag_12 + z_diff_lag_13 + z_diff_lag_14 + z_diff_lag_15
#               + z_diff_lag_16 + z_diff_lag_17 + z_diff_lag_18 + z_diff_lag_19 + z_diff_lag_20 + z_diff_lag_21 + z_diff_lag_22 + z_diff_lag_23
#               + z_diff_lag_24, data = ADF_LM_df)
# LM test = 43.274, df = 10, p-value = 0.000004443
#
# The question arises: how come the Breusch-Godfrey test yields such a contradictory result compared to the Ljung-Box test and the visual 
# evidence? 
#
# We consider again the case with 0 lags.
lmtest::bgtest(lm(z~tt+y_lag_1, data=ADF_LM_df), order=10, type="Chisq", fill=NA)
# LM test = 26.997, df = 10, p-value = 0.002608
res <- residuals(lm(z~tt+y_lag_1, data=ADF_LM_df))
res_lag_1 <- c(NA,res[-length(res)])
res_lag_2 <- c(NA,res_lag_1[-length(res_lag_1)])
res_lag_3 <- c(NA,res_lag_2[-length(res_lag_2)])
res_lag_4 <- c(NA,res_lag_3[-length(res_lag_3)])
res_lag_5 <- c(NA,res_lag_4[-length(res_lag_4)])
res_lag_6 <- c(NA,res_lag_5[-length(res_lag_5)])
res_lag_7 <- c(NA,res_lag_6[-length(res_lag_6)])
res_lag_8 <- c(NA,res_lag_7[-length(res_lag_7)])
res_lag_9 <- c(NA,res_lag_8[-length(res_lag_8)])
res_lag_10 <- c(NA,res_lag_9[-length(res_lag_9)])
#
BG_LM_0 <- lm(res~tt+y_lag_1+res_lag_1+res_lag_2+res_lag_3+res_lag_4+res_lag_5+res_lag_6+res_lag_7+res_lag_8+res_lag_9+res_lag_10, data=ADF_LM_df)
summary(BG_LM_0)
# Call: lm(formula = res ~ tt + y_lag_1 + res_lag_1 + res_lag_2 + res_lag_3 + res_lag_4 + res_lag_5 + res_lag_6 + res_lag_7 + res_lag_8 + 
#                    res_lag_9 + res_lag_10, data = ADF_LM_df)
# 
# Residuals:   Min    1Q      Medi     3Q     Max 
#           -7430.2  -227.2   -13.8   216.2  7186.6 
# 
# Coefficients: Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.3362084 50.0532110  -0.027 0.978706    
# tt           0.0160050  0.0676918   0.236 0.813120    
# y_lag_1     -0.0006192  0.0019631  -0.315 0.752479    
# res_lag_1   -0.0304383  0.0243418  -1.250 0.211306    
# res_lag_2    0.0135523  0.0242760   0.558 0.576741    
# res_lag_3    0.0238150  0.0242441   0.982 0.326091    
# res_lag_4    0.0406418  0.0242112   1.679 0.093407 .  
# res_lag_5    0.0093715  0.0242275   0.387 0.698942    
# res_lag_6    0.0241494  0.0242285   0.997 0.319035    
# res_lag_7   -0.0482793  0.0242217  -1.993 0.046397 *  
# res_lag_8   -0.0422003  0.0242455  -1.741 0.081945 .  
# res_lag_9    0.0861727  0.0242666   3.551 0.000394 ***
# res_lag_10   0.0250639  0.0243391   1.030 0.303261    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1024 on 1696 degrees of freedom (10 observations deleted due to missingness)
# Multiple R-squared:  0.0158,	Adjusted R-squared:  0.008833 
# F-statistic: 2.268 on 12 and 1696 DF,  p-value: 0.007514
#
# Note that the most significant coefficients of the regressed residuals correspond to the lags of the highest spikes in the autocorrelogram
# of the residuals. 
# We evaluate the LM statistic and the corresponding p-value
show(c(nobs(BG_LM_0)*summary(BG_LM_0)$r.squared, pchisq(q=nobs(BG_LM_0)*summary(BG_LM_0)$r.squared, d=10, lower.tail=FALSE)))
# 26.996206410  0.002607942
# Recall that the Breusch-Gofrey test yields
# LM test = 26.997, df = 10, p-value = 0.002608
#
# We suppress the lagged residuals from the model (to have a comparable model we also suppress the first 10 data to have the same number of
# observations).
BG_LM_0_bis <- lm(res[-c(1:10)]~tt[-c(1:10)]+y_lag_1[-c(1:10)], data=ADF_LM_df)
summary(BG_LM_0_bis)
# Call: lm(formula = res[-c(1:10)] ~ tt[-c(1:10)] + y_lag_1[-c(1:10)],  data = ADF_LM_df)
# 
# Residuals:  Min      1Q    Median   3Q     Max 
#          -7484.1  -230.7   -20.5   212.4  7325.3 
#
# Coefficients:      Estimate Std. Error t value  Pr(>|t|)
# (Intercept)       -2.109e+00  5.030e+01  -0.042    0.967
# tt[-c(1:10)]       2.299e-03  6.713e-02   0.034    0.973
# y_lag_1[-c(1:10)] -1.913e-05  1.912e-03  -0.010    0.992
# 
# Residual standard error: 1029 on 1706 degrees of freedom
# Multiple R-squared:  8.511e-07,	Adjusted R-squared:  -0.001171 
# F-statistic: 0.000726 on 2 and 1706 DF,  p-value: 0.9993
#
# We evaluate the LM statistic.
show(nobs(BG_LM_0_bis)*summary(BG_LM_0_bis)$r.squared)
# 0.001454515
#
# Now, the case with "long" lags.
lmtest::bgtest(lm(z~tt+y_lag_1+z_diff_lag_1+z_diff_lag_2+z_diff_lag_3+z_diff_lag_4+z_diff_lag_5+z_diff_lag_6+z_diff_lag_7+z_diff_lag_8+z_diff_lag_9+z_diff_lag_10
                  +z_diff_lag_11+z_diff_lag_12+z_diff_lag_13+z_diff_lag_14+z_diff_lag_15+z_diff_lag_16+z_diff_lag_17+z_diff_lag_18+z_diff_lag_19+z_diff_lag_20
                  +z_diff_lag_21+z_diff_lag_22+z_diff_lag_23+z_diff_lag_24, data=ADF_LM_df), order=10, type="Chisq", fill=NA)
# LM test = 43.274, df = 10, p-value = 4.443e-06
res <- residuals(lm(z~tt+y_lag_1+z_diff_lag_1+z_diff_lag_2+z_diff_lag_3+z_diff_lag_4+z_diff_lag_5+z_diff_lag_6+z_diff_lag_7+z_diff_lag_8+z_diff_lag_9+z_diff_lag_10
                                   +z_diff_lag_11+z_diff_lag_12+z_diff_lag_13+z_diff_lag_14+z_diff_lag_15+z_diff_lag_16+z_diff_lag_17+z_diff_lag_18+z_diff_lag_19+z_diff_lag_20
                                   +z_diff_lag_21+z_diff_lag_22+z_diff_lag_23+z_diff_lag_24, data=ADF_LM_df))
res_lag_1 <- c(NA,res[-length(res)])
res_lag_2 <- c(NA,res_lag_1[-length(res_lag_1)])
res_lag_3 <- c(NA,res_lag_2[-length(res_lag_2)])
res_lag_4 <- c(NA,res_lag_3[-length(res_lag_3)])
res_lag_5 <- c(NA,res_lag_4[-length(res_lag_4)])
res_lag_6 <- c(NA,res_lag_5[-length(res_lag_5)])
res_lag_7 <- c(NA,res_lag_6[-length(res_lag_6)])
res_lag_8 <- c(NA,res_lag_7[-length(res_lag_7)])
res_lag_9 <- c(NA,res_lag_8[-length(res_lag_8)])
res_lag_10 <- c(NA,res_lag_9[-length(res_lag_9)])
#
length(res)
# 1695
length(tt)
# 1719
BG_LM_24 <- lm(res~tt[-c(1:24)]+y_lag_1[-c(1:24)]+z_diff_lag_1[-c(1:24)]+z_diff_lag_2[-c(1:24)]+z_diff_lag_3[-c(1:24)]+z_diff_lag_4[-c(1:24)]
               +z_diff_lag_5[-c(1:24)]+z_diff_lag_6[-c(1:24)]+z_diff_lag_7[-c(1:24)]+z_diff_lag_8[-c(1:24)]+z_diff_lag_9[-c(1:24)]
               +z_diff_lag_10[-c(1:24)]+z_diff_lag_11[-c(1:24)]+z_diff_lag_12[-c(1:24)]+z_diff_lag_13[-c(1:24)]+z_diff_lag_14[-c(1:24)]
               +z_diff_lag_15[-c(1:24)]+z_diff_lag_16[-c(1:24)]+z_diff_lag_17[-c(1:24)]+z_diff_lag_18[-c(1:24)]+z_diff_lag_19[-c(1:24)]
               +z_diff_lag_20[-c(1:24)]+z_diff_lag_21[-c(1:24)]+z_diff_lag_22[-c(1:24)]+z_diff_lag_23[-c(1:24)]+z_diff_lag_24[-c(1:24)]
               +res_lag_1+res_lag_2+res_lag_3+res_lag_4+res_lag_5+res_lag_6+res_lag_7+res_lag_8+res_lag_9+res_lag_10, data=ADF_LM_df)
summary(BG_LM_24)
# Call: lm(formula = res ~ tt[-c(1:24)] + y_lag_1[-c(1:24)] + z_diff_lag_1[-c(1:24)] + z_diff_lag_2[-c(1:24)] + z_diff_lag_3[-c(1:24)] 
#                          + z_diff_lag_4[-c(1:24)] + z_diff_lag_5[-c(1:24)] + z_diff_lag_6[-c(1:24)] + z_diff_lag_7[-c(1:24)]
#                          + z_diff_lag_8[-c(1:24)] + z_diff_lag_9[-c(1:24)] + z_diff_lag_10[-c(1:24)] + z_diff_lag_11[-c(1:24)] 
#                          + z_diff_lag_12[-c(1:24)] + z_diff_lag_13[-c(1:24)] + z_diff_lag_14[-c(1:24)] + z_diff_lag_15[-c(1:24)] 
#                          + z_diff_lag_16[-c(1:24)] + z_diff_lag_17[-c(1:24)] + z_diff_lag_18[-c(1:24)] + z_diff_lag_19[-c(1:24)]
#                          + z_diff_lag_20[-c(1:24)] + z_diff_lag_21[-c(1:24)] + z_diff_lag_22[-c(1:24)] + z_diff_lag_23[-c(1:24)]  
#                          + z_diff_lag_24[-c(1:24)] + res_lag_1 + res_lag_2 + res_lag_3 + res_lag_4 + res_lag_5 + res_lag_6 + res_lag_7 
#                          + res_lag_8 + res_lag_9 + res_lag_10, data = ADF_LM_df)
# 
# Residuals:  Min      1Q    Median    3Q     Max 
#           -6394.3  -246.4   -15.6   217.0  7000.1 
# 
# Coefficients:            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             -2.180e+02  8.457e+01  -2.578 0.010017 *  
# tt[-c(1:24)]            -1.228e-01  7.822e-02  -1.570 0.116609    
# y_lag_1[-c(1:24)]        1.412e-02  4.714e-03   2.994 0.002793 ** 
# z_diff_lag_1[-c(1:24)]   1.694e+00  4.771e-01   3.551 0.000395 ***
# z_diff_lag_2[-c(1:24)]   6.556e-01  4.780e-01   1.372 0.170359    
# z_diff_lag_3[-c(1:24)]   1.407e+00  4.559e-01   3.087 0.002054 ** 
# z_diff_lag_4[-c(1:24)]   1.084e+00  4.567e-01   2.374 0.017726 *  
# z_diff_lag_5[-c(1:24)]  -1.298e+00  4.212e-01  -3.082 0.002090 ** 
# z_diff_lag_6[-c(1:24)]   1.749e-01  4.211e-01   0.415 0.677994    
# z_diff_lag_7[-c(1:24)]   4.191e-01  3.709e-01   1.130 0.258637    
# z_diff_lag_8[-c(1:24)]   9.551e-02  3.781e-01   0.253 0.800611    
# z_diff_lag_9[-c(1:24)]   1.348e+00  3.827e-01   3.522 0.000440 ***
# z_diff_lag_10[-c(1:24)]  2.127e-01  3.228e-01   0.659 0.510027    
# z_diff_lag_11[-c(1:24)]  1.296e-02  7.039e-02   0.184 0.853919    
# z_diff_lag_12[-c(1:24)] -2.200e-01  7.004e-02  -3.141 0.001711 ** 
# z_diff_lag_13[-c(1:24)] -1.454e-01  7.150e-02  -2.034 0.042071 *  
# z_diff_lag_14[-c(1:24)]  6.512e-02  5.336e-02   1.220 0.222467    
# z_diff_lag_15[-c(1:24)]  6.148e-02  4.891e-02   1.257 0.208969    
# z_diff_lag_16[-c(1:24)]  7.293e-02  4.755e-02   1.534 0.125290    
# z_diff_lag_17[-c(1:24)] -2.153e-02  4.333e-02  -0.497 0.619312    
# z_diff_lag_18[-c(1:24)] -3.423e-02  4.019e-02  -0.852 0.394600    
# z_diff_lag_19[-c(1:24)]  4.403e-03  3.719e-02   0.118 0.905750    
# z_diff_lag_20[-c(1:24)]  3.478e-02  3.847e-02   0.904 0.366048    
# z_diff_lag_21[-c(1:24)]  1.010e-03  3.660e-02   0.028 0.977977    
# z_diff_lag_22[-c(1:24)]  9.492e-02  3.632e-02   2.614 0.009038 ** 
# z_diff_lag_23[-c(1:24)] -3.196e-02  3.629e-02  -0.881 0.378618    
# z_diff_lag_24[-c(1:24)] -3.091e-02  3.737e-02  -0.827 0.408278    
# res_lag_1               -1.704e+00  4.795e-01  -3.553 0.000391 ***
# res_lag_2               -6.317e-01  4.762e-01  -1.327 0.184844    
# res_lag_3               -1.436e+00  4.545e-01  -3.159 0.001613 ** 
# res_lag_4               -1.117e+00  4.543e-01  -2.459 0.014015 *  
# res_lag_5                1.240e+00  4.120e-01   3.011 0.002645 ** 
# res_lag_6               -3.115e-01  4.127e-01  -0.755 0.450572    
# res_lag_7               -5.458e-01  3.728e-01  -1.464 0.143364    
# res_lag_8               -2.056e-02  3.717e-01  -0.055 0.955893    
# res_lag_9               -1.297e+00  3.688e-01  -3.518 0.000446 ***
# res_lag_10              -2.750e-01  3.169e-01  -0.868 0.385641    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1014 on 1648 degrees of freedom (10 observations deleted due to missingness)
# Multiple R-squared:  0.02568,	Adjusted R-squared:  0.004398 
# F-statistic: 1.207 on 36 and 1648 DF,  p-value: 0.1874
#
# We evaluate the LM statistic and the corresponding p-value.
show(c(nobs(BG_LM_24)*summary(BG_LM_24)$r.squared, pchisq(q=nobs(BG_LM_24)*summary(BG_LM_24)$r.squared, d=10, lower.tail=FALSE)))
# [1] 43.27383974607  0.00000444252
# Recall that the Breusch-Gofrey test yields
# LM test = 43.274, df = 10, p-value = 4.443e-06
#
# We suppress the lagged residuals from the model (to have a comparable model we also suppress the first 10 data of the residuals to have
# the same number of observations).
BG_LM_24_bis <- lm(res[-c(1:10)]~tt[-c(1:34)]+y_lag_1[-c(1:34)]+z_diff_lag_1[-c(1:34)]+z_diff_lag_2[-c(1:34)]+z_diff_lag_3[-c(1:34)]+z_diff_lag_4[-c(1:34)]
                   +z_diff_lag_5[-c(1:34)]+z_diff_lag_6[-c(1:34)]+z_diff_lag_7[-c(1:34)]+z_diff_lag_8[-c(1:34)]+z_diff_lag_9[-c(1:34)]
                   +z_diff_lag_10[-c(1:34)]+z_diff_lag_11[-c(1:34)]+z_diff_lag_12[-c(1:34)]+z_diff_lag_13[-c(1:34)]+z_diff_lag_14[-c(1:34)]
                   +z_diff_lag_15[-c(1:34)]+z_diff_lag_16[-c(1:34)]+z_diff_lag_17[-c(1:34)]+z_diff_lag_18[-c(1:34)]+z_diff_lag_19[-c(1:34)]
                   +z_diff_lag_20[-c(1:34)]+z_diff_lag_21[-c(1:34)]+z_diff_lag_22[-c(1:34)]+z_diff_lag_23[-c(1:34)]+z_diff_lag_24[-c(1:34)],
                   data=ADF_LM_df)
summary(BG_LM_24_bis)
# Call: lm(formula = res[-c(1:10)] ~ tt[-c(1:34)] + y_lag_1[-c(1:34)] + z_diff_lag_1[-c(1:34)] + z_diff_lag_2[-c(1:34)] 
#                                    + z_diff_lag_3[-c(1:34)] + z_diff_lag_4[-c(1:34)] + z_diff_lag_5[-c(1:34)] + z_diff_lag_6[-c(1:34)]
#                                    + z_diff_lag_7[-c(1:34)] + z_diff_lag_8[-c(1:34)] + z_diff_lag_9[-c(1:34)] + z_diff_lag_10[-c(1:34)]
#                                    + z_diff_lag_11[-c(1:34)] + z_diff_lag_12[-c(1:34)] + z_diff_lag_13[-c(1:34)] + z_diff_lag_14[-c(1:34)]
#                                    + z_diff_lag_15[-c(1:34)] + z_diff_lag_16[-c(1:34)] + z_diff_lag_17[-c(1:34)] + z_diff_lag_18[-c(1:34)]
#                                    + z_diff_lag_19[-c(1:34)] + z_diff_lag_20[-c(1:34)] + z_diff_lag_21[-c(1:34)] + z_diff_lag_22[-c(1:34)]
#                                    + z_diff_lag_23[-c(1:34)] + z_diff_lag_24[-c(1:34)], data = ADF_LM_df)
# 
# Residuals:  Min      1Q    Median   3Q     Max 
#          -6599.9  -226.6   -17.3   220.2  7099.3 
# 
# Coefficients:            Estimate Std. Error t value Pr(>|t|)
# (Intercept)              1.150e-01  5.161e+01   0.002    0.998
# tt[-c(1:34)]            -1.435e-04  6.999e-02  -0.002    0.998
# y_lag_1[-c(1:34)]        1.809e-06  1.972e-03   0.001    0.999
# z_diff_lag_1[-c(1:34)]  -2.490e-05  2.451e-02  -0.001    0.999
# z_diff_lag_2[-c(1:34)]   8.377e-05  2.449e-02   0.003    0.997
# z_diff_lag_3[-c(1:34)]   4.781e-05  2.449e-02   0.002    0.998
# z_diff_lag_4[-c(1:34)]  -1.875e-05  2.449e-02  -0.001    0.999
# z_diff_lag_5[-c(1:34)]  -2.653e-05  2.448e-02  -0.001    0.999
# z_diff_lag_6[-c(1:34)]  -3.767e-05  2.442e-02  -0.002    0.999
# z_diff_lag_7[-c(1:34)]  -8.595e-05  2.444e-02  -0.004    0.997
# z_diff_lag_8[-c(1:34)]  -5.750e-05  2.447e-02  -0.002    0.998
# z_diff_lag_9[-c(1:34)]   4.274e-05  2.448e-02   0.002    0.999
# z_diff_lag_10[-c(1:34)] -6.921e-05  2.457e-02  -0.003    0.998
# z_diff_lag_11[-c(1:34)]  2.606e-06  2.458e-02   0.000    1.000
# z_diff_lag_12[-c(1:34)]  9.709e-05  2.457e-02   0.004    0.997
# z_diff_lag_13[-c(1:34)]  1.451e-04  2.456e-02   0.006    0.995
# z_diff_lag_14[-c(1:34)]  4.948e-05  2.457e-02   0.002    0.998
# z_diff_lag_15[-c(1:34)] -8.264e-05  2.456e-02  -0.003    0.997
# z_diff_lag_16[-c(1:34)]  2.203e-05  2.449e-02   0.001    0.999
# z_diff_lag_17[-c(1:34)] -1.419e-04  2.447e-02  -0.006    0.995
# z_diff_lag_18[-c(1:34)]  1.386e-04  2.443e-02   0.006    0.995
# z_diff_lag_19[-c(1:34)] -9.517e-05  2.443e-02  -0.004    0.997
# z_diff_lag_20[-c(1:34)]  2.600e-05  2.447e-02   0.001    0.999
# z_diff_lag_21[-c(1:34)]  1.001e-05  2.450e-02   0.000    1.000
# z_diff_lag_22[-c(1:34)] -7.539e-05  2.449e-02  -0.003    0.998
# z_diff_lag_23[-c(1:34)]  1.176e-05  2.449e-02   0.000    1.000
# z_diff_lag_24[-c(1:34)] -3.747e-06  2.451e-02   0.000    1.000
# 
# Residual standard error: 1024 on 1658 degrees of freedom
# Multiple R-squared:  1.313e-07,	Adjusted R-squared:  -0.01568 
# F-statistic: 8.375e-06 on 26 and 1658 DF,  p-value: 1
#
# We evaluate the LM statistic and the corresponding p-value.
show(c(nobs(BG_LM_24_bis)*summary(BG_LM_24_bis)$r.squared, pchisq(q=nobs(BG_LM_24_bis)*summary(BG_LM_24_bis)$r.squared, d=10, lower.tail=FALSE)))
# 0.0002212884 1.0000000000
#
# In light of what is shown above, we conjecture that the rejection of the null hypothesis of no correlation up the 10th order in the 
# residuals of the linear model used for the ADF test with "long" lags occurs because the introduction of the lagged residuals in the linear
# model used for the Breusch-Godfrey test alters significantly the correlation of the residuals with the many independent variables of the
# model. Introducing the lagged residuals seems to make some independent variables endogenous which renders the Breusch-Godfrey test not 
# applicable. Moreover, a plot of the residuals shows a clear visual evidence for heteroscedasticity.
head(BTC_train_df, 3)
#   t       Date    Open    High     Low   Close Adj.Close     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31 7063209984
tail(BTC_train_df, 3)
#         t       Date     Open     High      Low    Close Adj.Close      Volume
# 1718 1718 2022-12-29 16552.32 16651.76 16508.68 16642.34  16642.34 14472237479
# 1719 1719 2022-12-30 16641.33 16643.43 16408.47 16602.59  16602.59 15929162910
# 1720 1720 2022-12-31 16603.67 16628.99 16517.52 16547.50  16547.50 11239186456
#
Data_df <- BTC_train_df
head(Data_df, 3)
#   t       Date    Open    High     Low   Close Adj.Close     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31 7063209984
length(as.vector(y_ADF_ur.df_trend_long_lags@testreg[["residuals"]]))
# 1695
Data_df <- add_column(Data_df, ADF_Long_lag_y_res=c(rep(NA,25),as.vector(y_ADF_ur.df_trend_long_lags@testreg[["residuals"]])), .after="Adj.Close")
head(Data_df, 3)
#   t       Date    Open    High     Low   Close Adj.Close ADF_Long_lag_y_res     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09                 NA 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42                 NA 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31                 NA 7063209984
tail(Data_df, 3)
#         t       Date     Open     High      Low    Close Adj.Close ADF_Long_lag_y_res      Volume
# 1718 1718 2022-12-29 16552.32 16651.76 16508.68 16642.34  16642.34           87.22173 14472237479
# 1719 1719 2022-12-30 16641.33 16643.43 16408.47 16602.59  16602.59         -144.11835 15929162910
# 1720 1720 2022-12-31 16603.67 16628.99 16517.52 16547.50  16547.50          -75.15285 11239186456
Data_df <- dplyr::rename(Data_df, x=t, y=ADF_Long_lag_y_res)
head(Data_df, 3)
#   x       Date    Open    High     Low   Close Adj.Close  y     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09 NA 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42 NA 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31 NA 7063209984
TrnS_length <- length(Data_df$Adj.Close)
show(TrnS_length)
# 1720
First_Day <- as.character(Data_df$Date[2])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Residuals of the Linear Model with", " \"Long\" ", "Lags for the ADF Test on the Bitcoin Daily Adjusted Close Price Training Set - from ", .(First_Day), " to ", .(Last_Day), sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points. Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("")
# numbers::primeFactors(TrnS_length)
x_breaks_num <- 40 # (deduced from primeFactors(TrnS_length-2))
x_breaks_low <- Data_df$x[1]
x_breaks_up <- Data_df$x[(TrnS_length)]
x_binwidth <- ceiling((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Date[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("residuals of the linear model")
y_max <- max(na.rm(Data_df$y))
y_min <- min(na.rm(Data_df$y))
as.numeric(floor(y_max-y_min))
y_breaks_num <- 10
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor((y_min/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((y_max/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth), digits=3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.5
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
point_black <- bquote("GARCH(1,1) stand. residuals")
line_green <- bquote("regression line")
line_red <- bquote("LOESS curve")
leg_point_labs <- c(point_black)
leg_point_cols <- c("point_black"="black")
leg_point_breaks <- c("point_black")
leg_line_labs <- c(line_green, line_red)
leg_line_cols <- c("line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_green", "line_red")
leg_col_labs   <- c(leg_point_labs,leg_line_labs)
leg_col_cols   <- c(leg_point_cols,leg_line_cols)
leg_col_breaks <- c(leg_point_breaks,leg_line_breaks)
BTC_ACP_TrnS_Long_Lags_ADF_res_sp <- ggplot(Data_df, aes(x=x)) +
  geom_smooth(aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_point(aes(y=y, color="point_black"), alpha=1, size=0.7, shape=19, na.rm=TRUE) + 
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  guides(colour=guide_legend(override.aes=list(shape=c(19, NA, NA), linetype=c("blank", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_ACP_TrnS_Long_Lags_ADF_res_sp)
#
# The line plot
BTC_ACP_TrnS_Long_Lags_ADF_res_lp <- ggplot(Data_df, aes(x=x)) +
  geom_smooth(aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_line(aes(y=y, color="point_black"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_ACP_TrnS_Long_Lags_ADF_res_lp)
#
# However, as also suggested by Baum and Schaffer (see Baum, C.F. & Schaffer, M.E., A General Approach to Testing for Autocorrelation, Stata 
# Conference, New Orleans, July 2013, slide 11/44) the Breusch-Godfrey can be applied to a singe time series by regressing the series on a 
# constant. In this case, the regressor (the unit vector) is strictly exogenous. Eventually, creating a fictitious linear regression by 
# setting 
# 
y_res <- as.vector(y_ADF_ur.df_trend_long_lags@testreg[["residuals"]])
lmtest::bgtest(lm(y_res~1), order=10, type="Chisq")
# Breusch-Godfrey test for serial correlation of order up to 10
# data:  triv_y_lm
# LM test = 0.27544, df = 10, p-value = 1
#
lmtest::bgtest(lm(y_res~1), order=10, type="F")
# Breusch-Godfrey test for serial correlation of order up to 10
# data:  triv_y_lm
# LM test = 0.027369, df1 = 10, df2 = 1684, p-value = 1
#
# In addition, the robust version of the test firmly confirm the lack of autocorrelation in the residuals of the linear model with "long" 
# lags used for the ADF test.
y_res <- as.vector(y_ADF_ur.df_trend_long_lags@testreg[["residuals"]])
testcorr::ac.test(y_res, max.lag = 10, alpha = 0.05, lambda = 2.576, plot = TRUE, table = TRUE, var.name = NULL, scale.font = 1)
#   | Lag|     AC|  Stand. CB(95%)|  Robust CB(95%)| Lag|      t| p-value| t-tilde| p-value| Lag|    LB| p-value| Q-tilde| p-value|
#   |---:|------:|---------------:|---------------:|---:|------:|-------:|-------:|-------:|---:|-----:|-------:|-------:|-------:|
#   |   1|  0.000| (-0.048, 0.048)| (-0.074, 0.074)|   1| -0.012|   0.990|  -0.008|   0.994|   1| 0.000|   0.990|   0.000|   0.994|
#   |   2| -0.001| (-0.048, 0.048)| (-0.076, 0.076)|   2| -0.042|   0.967|  -0.026|   0.979|   2| 0.002|   0.999|   0.001|   1.000|
#   |   3| -0.004| (-0.048, 0.048)| (-0.069, 0.069)|   3| -0.169|   0.866|  -0.116|   0.908|   3| 0.030|   0.999|   0.014|   1.000|
#   |   4| -0.002| (-0.048, 0.048)| (-0.084, 0.084)|   4| -0.100|   0.921|  -0.057|   0.955|   4| 0.040|   1.000|   0.017|   1.000|
#   |   5|  0.006| (-0.048, 0.048)| (-0.079, 0.079)|   5|  0.245|   0.806|   0.148|   0.883|   5| 0.101|   1.000|   0.039|   1.000|
#   |   6| -0.003| (-0.048, 0.048)| (-0.082, 0.082)|   6| -0.142|   0.887|  -0.082|   0.934|   6| 0.121|   1.000|   0.046|   1.000|
#   |   7| -0.005| (-0.048, 0.048)| (-0.085, 0.085)|   7| -0.221|   0.825|  -0.124|   0.901|   7| 0.170|   1.000|   0.061|   1.000|
#   |   8|  0.007| (-0.048, 0.048)| (-0.089, 0.089)|   8|  0.287|   0.774|   0.154|   0.878|   8| 0.253|   1.000|   0.085|   1.000|
#   |   9| -0.004| (-0.048, 0.048)| (-0.078, 0.078)|   9| -0.149|   0.881|  -0.091|   0.927|   9| 0.276|   1.000|   0.094|   1.000|
#   |  10|  0.000| (-0.048, 0.048)| (-0.076, 0.076)|  10|  0.007|   0.994|   0.005|   0.996|  10| 0.276|   1.000|   0.094|   1.000|
#
# From what we have shown above, we can conclude that the residuals in the linear model with "long" lags used for the ADF test do not show 
# evidence for autocorrelation. Hence, we cannot reject the null hypothesis of the presence of a unit root in the Bitcoin adjusted 
# close price time series.
# More generally, regarding the statistical investigation approach, the above results highlight how important it is to check the validity of
# the hypotheses that allow the successful application of statistical tests and the need to combine different tests to accumulate clear 
# evidence regarding the desired result. 
# 
# We continue the analysis of the ADF test by considering the ADF test with a small number of lags (with "short" lags). However, in what
# follows, we will no longer use the functions portes::LjungBox(), forecst::checkresiduals(), stats::Box.test() with Hyndman's lag option 
# H_max_lag = max(max_lag,(n_pars+3))
#
short_lags <- floor(4*(length(y)/100)^(1/4))
y_ADF_ur.df_trend_short_lags <- ur.df(y, type="trend", lags=short_lags, selectlags="Fixed")
lm(formula=y_ADF_ur.df_trend_short_lags@testreg[["terms"]])
# Call: lm(formula=y_ADF_ur.df_trend_short_lags@testreg[["terms"]])
# Coefficients:
#   (Intercept)      z.lag.1         tt      z.diff.lag1  z.diff.lag2  z.diff.lag3  z.diff.lag4  z.diff.lag5  z.diff.lag6  z.diff.lag7  
#    36.801945     -0.002132     0.014606   -0.033085     0.007996     0.024106     0.041637     0.012625     0.026784   -0.047602     
#                                            z.diff.lag8
#                                            -0.045321 
nobs(lm(formula=y_ADF_ur.df_trend_short_lags@testreg[["terms"]]))
# 1711
n_obs <- length(y)-(short_lags+1)
show(n_obs)
# 1711
n_coeffs <- nrow(y_ADF_ur.df_trend_short_lags@testreg[["coefficients"]])
show(n_coeffs)
# 11
df.residual(lm(formula=y_ADF_ur.df_trend_short_lags@testreg[["terms"]]))
# 1700
df_res <- n_obs-n_coeffs
show(df_res)
# 1700
summary(y_ADF_ur.df_trend_short_lags)
############################################### 
# Augmented Dickey-Fuller Test Unit Root Test # 
############################################### 
# Test regression trend 
# Call: lm(formula=z.diff ~ z.lag.1 + 1 + tt + z.diff.lag)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7451.0 -223.7  -18.8   204.1  7186.7 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept) 36.801945  50.117357   0.734   0.4629  
#   z.lag.1   -0.002132   0.001928  -1.106   0.2691  
#     tt       0.014606   0.067447   0.217   0.8286  
# z.diff.lag1 -0.033085   0.024244  -1.365   0.1725  
# z.diff.lag2  0.007996   0.024222   0.330   0.7414  
# z.diff.lag3  0.024106   0.024217   0.995   0.3197  
# z.diff.lag4  0.041637   0.024223   1.719   0.0858 .
# z.diff.lag5  0.012625   0.024226   0.521   0.6024  
# z.diff.lag6  0.026784   0.024222   1.106   0.2690  
# z.diff.lag7 -0.047602   0.024231 - 1.965   0.0496 *
# z.diff.lag8 -0.045321   0.024241  -1.870   0.0617 .
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1027 on 1700 degrees of freedom
# Multiple R-squared:  0.009067,	Adjusted R-squared:  0.003238 
# F-statistic: 1.556 on 10 and 1700 DF,  p-value: 0.1142
# 
# Value of test-statistic is: -1.1056 0.5805 0.8535 
# 
# Critical values for test statistics: 
#       1pct  5pct 10pct
# tau3 -3.96 -3.41 -3.12
# phi2  6.09  4.68  4.03
# phi3  8.27  6.25  5.34
#
# The impossibility of rejecting the null hypothesis against the three alternatives at $10\%$ significance level is confirmed.
# However, to validate the test, we need to check again the possible presence of autocorrelation in the residuals of the linear model used
# for the ADF test.
#
y_res <- as.vector(y_ADF_ur.df_trend_short_lags@testreg[["residuals"]])
max_lag <- ceiling(min(10, n_obs/4))    # Hyndman (for data without seasonality, see https://robjHyndman.com/hy_resndsight/ljung-box-test/)
n_pars <- n_coeffs
fit_df <- n_pars
LB_fit_df <- min(min(max_lag, n_pars), max_lag-1)
show(LB_fit_df)
# 9
y_res_LB <- Box.test(y_res, lag=max_lag, fitdf=LB_fit_df, type="Ljung-Box")
show(y_res_LB)
# Box-Ljung test
# data:  y_res
# X-squared=13.699, df=1, p-value=0.0002146
#
FitAR::LjungBoxTest(y_res, k=n_pars, lag.max=max_lag, StartLag=1, SquaredQ=FALSE)
#  m    Qm       pvalue
#  1  0.03 0.8674283701
#  2  0.07 0.7963572633
#  3  0.07 0.7960059750
#  4  0.09 0.7613210413
#  5  0.14 0.7033825779
#  6  0.14 0.7033713326
#  7  0.16 0.6886836091
#  8  0.17 0.6781488854
#  9 12.74 0.0003573020
# 10 13.70 0.0002146015
#
n_pars_seq <- rep(NA,max_lag)
for(l in 1:max_lag){
  if(l-n_pars<0) n_pars_seq[l] <- l-1
  else n_pars_seq[l] <- n_pars
}
show(n_pars_seq)
# 0 1 2 3 4 5 6 7 8 9
#
Box_test_ls <- list()
for(l in 1:max_lag){
  Box_test_ls[[l]] <- Box.test(y_res, lag=l,   fitdf=n_pars_seq[l], type="Ljung-Box")
  show(Box_test_ls[[l]])
}
# Box-Ljung test
# data:  y_res
# X-squared=0.027864, df=1, p-value=0.8674
# X-squared=0.066597, df=1, p-value=0.7964
# X-squared=0.066832, df=1, p-value=0.796
# X-squared=0.092262, df=1, p-value=0.7613
# X-squared=0.14498,  df=1, p-value=0.7034
# X-squared=0.14499,  df=1, p-value=0.7034
# X-squared=0.16051,  df=1, p-value=0.6887
# X-squared=0.17222,  df=1, p-value=0.6781
# X-squared=12.7430   df=1, p-value=0.0003573
# X-squared=13.6990,  df=1, p-value=0.0002146
#
# We plot the autocorrelogram
y_res <- as.vector(y_ADF_ur.df_trend_short_lags@testreg[["residuals"]])
T <- n_obs
# max_lag <- ceiling(10*log10(T))    # Default
# max_lag <- ceiling(sqrt(n)+45)     # Box-Jenkins
max_lag <- ceiling(min(10, T/4))     # Hyndman (for data without seasonality)
# max_lag <- ceiling(min(2*12, T/5)) # Hyndman (for data with seasonality, see https://robjHyndman.com/hy_resndsight/ljung-box-test/)
# Aut_Fun_y <- stats::acf(y, lag.max=max_lag, type="correlation", plot=FALSE)
Aut_Fun_y_res <- TSA::acf(y_res, lag.max=max_lag, type= "correlation", plot=FALSE)
ci_090 <- qnorm((1+0.90)/2)/sqrt(T)
ci_95 <- qnorm((1+0.95)/2)/sqrt(T)
ci_99 <- qnorm((1+0.99)/2)/sqrt(T)
Aut_Fun_y_res <- data.frame(lag=Aut_Fun_y_res$lag, acf=Aut_Fun_y_res$acf)
# First_Date <- paste(Data_df$Month[1],Data_df$year[1])
# Last_Date <- paste(Data_df$Month[T],Data_df$year[T])
First_Date <- as.character(Data_df$Date[1])
Last_Date <-  as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025", 
                             paste("Autocorrelogram of the Residuals of the Linear Model with", " \"Short\" ", "Lags for the ADF Test for the Bitcoin Daily Adjusted Close from ", .(First_Date), " to ", .(Last_Date))))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Data set size ",.(TrnS_length)," sample points. Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("lags")
x_breaks_num <- max_lag
x_binwidth <- 1
x_breaks <- Aut_Fun_y_res$lag
x_labs <- format(x_breaks, scientific=FALSE)
Plot_Aut_Fun_y_res <- ggplot(Aut_Fun_y_res, aes(x=lag, y=acf))+
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), linewidth=1, col= "black") +
  # geom_col(mapping=NULL, data=NULL, position= "dodge", width=0.1, col= "black", inherit.aes=TRUE)+
  geom_hline(aes(yintercept=-ci_090, color= "CI_090"), show.legend=TRUE, lwd=0.9, lty=3) +
  geom_hline(aes(yintercept=ci_090, color= "CI_090"), lwd=0.9, lty=3) +
  geom_hline(aes(yintercept=ci_95, color= "CI_95"), show.legend=TRUE, lwd=0.8, lty=2)+
  geom_hline(aes(yintercept=-ci_95, color= "CI_95"), lwd=0.8, lty=2) +
  geom_hline(aes(yintercept=-ci_99, color= "CI_99"), show.legend=TRUE, lwd=0.8, lty=4) +
  geom_hline(aes(yintercept=ci_99, color= "CI_99"), lwd=0.8, lty=4) +
  scale_x_continuous(name= "lag", breaks=x_breaks, label=x_labs) +
  scale_y_continuous(name= "acf value", breaks=waiver(), labels=NULL,
                     sec.axis=sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name= "Conf. Inter.", labels=c("90%","95%","99%"), values=c(CI_090="green", CI_95="blue", CI_99="red"),
                     guide=guide_legend(override.aes=list(linetype=c("dotted", "dashed", "dotdash")))) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
  # theme(plot.title=element_blank(), 
  #       plot.subtitle=element_blank(),
  #       plot.caption=element_text(hjust=1.0),
  #       legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(Plot_Aut_Fun_y_res)
#
# The Visual evidence from the autocorrelogram supports the computational result of the Ljung-Box test with the conservative correction for 
# the difference between the lag and fitdf parameters.
#
# We again consider the Breusch-Godfrey test for autocorrelation. Applying the test to the residuals of the linear model used for the ADF 
# test also seems natural in this case. Hence, we apply the test to this linear model similarly to what we have done above.
lm(formula=y_ADF_ur.df_trend_short_lags@testreg[["terms"]])
# Call: lm(formula=y_ADF_ur.df_trend_short_lags@testreg[["terms"]])
# Coefficients:
#   (Intercept)      z.lag.1         tt      z.diff.lag1  z.diff.lag2  z.diff.lag3  z.diff.lag4  z.diff.lag5  z.diff.lag6  z.diff.lag7  
#    36.801945     -0.002132     0.014606   -0.033085     0.007996     0.024106     0.041637     0.012625     0.026784   -0.047602     
#                                            z.diff.lag8
#                                            -0.045321 
#
lmtest::bgtest(lm(formula=y_ADF_ur.df_trend_short_lags@testreg[["terms"]]), order=10, type="Chisq")
# Breusch-Godfrey test for serial correlation of order up to 10
# data:  lm(formula = y_ADF_ur.df_trend_short_lags@testreg[["terms"]])
# LM test = 21.584, df = 10, p-value = 0.01737
#
# The result of the test confirms the rejection of the null hypothesis of no autocorrelation at the $5\%$ significance level. On the other
# hand, considering again the residuals of the linear model used for the ADF test, setting the fictitious linear regression against the 
# units vector, and reapplying the Breusch-Godfrey test we obtain
y_res <- as.vector(y_ADF_ur.df_trend_short_lags@testreg[["residuals"]])
lmtest::bgtest(lm(y_res~1), order=10, type="Chisq")
# Breusch-Godfrey test for serial correlation of order up to 10
# data:  triv_y_lm
# LM test = 13.551, df = 10, p-value = 0.1945
# 
# The result contradicts the rejection of the null-hypothesis. However, a plot of the residuals shows a clear visual evidence for 
# heteroscedasticity and advocates the robust tests for autocorrelation.
head(BTC_train_df, 3)
#   t       Date    Open    High     Low   Close Adj.Close     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31 7063209984
tail(BTC_train_df, 3)
#         t       Date     Open     High      Low    Close Adj.Close      Volume
# 1718 1718 2022-12-29 16552.32 16651.76 16508.68 16642.34  16642.34 14472237479
# 1719 1719 2022-12-30 16641.33 16643.43 16408.47 16602.59  16602.59 15929162910
# 1720 1720 2022-12-31 16603.67 16628.99 16517.52 16547.50  16547.50 11239186456
#
Data_df <- BTC_train_df
head(Data_df, 3)
#   t       Date    Open    High     Low   Close Adj.Close     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31 7063209984
length(as.vector(y_ADF_ur.df_trend_short_lags@testreg[["residuals"]]))
# 1711
Data_df <- add_column(Data_df, ADF_Long_lag_y_res=c(rep(NA,9),as.vector(y_ADF_ur.df_trend_short_lags@testreg[["residuals"]])), .after="Adj.Close")
head(Data_df, 3)
#   t       Date    Open    High     Low   Close Adj.Close ADF_Long_lag_y_res     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09                 NA 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42                 NA 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31                 NA 7063209984
Data_df <- dplyr::rename(Data_df, x=t, y=ADF_Long_lag_y_res)
head(Data_df, 3)
#   x       Date    Open    High     Low   Close Adj.Close  y     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09 NA 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42 NA 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31 NA 7063209984
TrnS_length <- length(Data_df$Adj.Close)
show(TrnS_length)
# 1720
First_Day <- as.character(Data_df$Date[2])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Residuals of the Linear Model with", " \"Short\" ", "Lags for the ADF Test on the Bitcoin Daily Adjusted Close Price Training Set - from ", .(First_Day), " to ", .(Last_Day), sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points. Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("")
# numbers::primeFactors(TrnS_length)
x_breaks_num <- 40 # (deduced from primeFactors(TrnS_length-2))
x_breaks_low <- Data_df$x[1]
x_breaks_up <- Data_df$x[(TrnS_length)]
x_binwidth <- ceiling((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Date[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("residuals of the linear model")
y_max <- max(na.rm(Data_df$y))
y_min <- min(na.rm(Data_df$y))
as.numeric(floor(y_max-y_min))
y_breaks_num <- 10
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor((y_min/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((y_max/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth), digits=3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.5
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
point_black <- bquote("GARCH(1,1) stand. residuals")
line_green <- bquote("regression line")
line_red <- bquote("LOESS curve")
leg_point_labs <- c(point_black)
leg_point_cols <- c("point_black"="black")
leg_point_breaks <- c("point_black")
leg_line_labs <- c(line_green, line_red)
leg_line_cols <- c("line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_green", "line_red")
leg_col_labs   <- c(leg_point_labs,leg_line_labs)
leg_col_cols   <- c(leg_point_cols,leg_line_cols)
leg_col_breaks <- c(leg_point_breaks,leg_line_breaks)
BTC_ACP_TrnS_Short_Lags_ADF_res_sp <- ggplot(Data_df, aes(x=x)) +
  geom_smooth(aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_point(aes(y=y, color="point_black"), alpha=1, size=0.7, shape=19, na.rm=TRUE) + 
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  guides(colour=guide_legend(override.aes=list(shape=c(19, NA, NA), linetype=c("blank", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_ACP_TrnS_Short_Lags_ADF_res_sp)
#
# The line plot
BTC_ACP_TrnS_Short_Lags_ADF_res_lp <- ggplot(Data_df, aes(x=x)) +
  geom_smooth(aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_line(aes(y=y, color="point_black"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_ACP_TrnS_Short_Lags_ADF_res_lp)
#
y_res <- as.vector(y_ADF_ur.df_trend_short_lags@testreg[["residuals"]])
testcorr::ac.test(y_res, max.lag = 10, alpha = 0.05, lambda = 2.576, plot = TRUE, table = TRUE, var.name = NULL, scale.font = 1)
# Tests for zero autocorrelation of x
#   | Lag|     AC|  Stand. CB(95%)|  Robust CB(95%)| Lag|      t| p-value| t-tilde| p-value| Lag|     LB| p-value| Q-tilde| p-value|
#   |---:|------:|---------------:|---------------:|---:|------:|-------:|-------:|-------:|---:|------:|-------:|-------:|-------:|
#   |   1|  0.004| (-0.047, 0.047)| (-0.072, 0.072)|   1|  0.167|   0.868|   0.110|   0.913|   1|  0.028|   0.867|   0.012|   0.913|
#   |   2|  0.005| (-0.047, 0.047)| (-0.077, 0.077)|   2|  0.197|   0.844|   0.121|   0.904|   2|  0.067|   0.967|   0.027|   0.987|
#   |   3|  0.000| (-0.047, 0.047)| (-0.070, 0.070)|   3| -0.015|   0.988|  -0.010|   0.992|   3|  0.067|   0.995|   0.027|   0.999|
#   |   4| -0.004| (-0.047, 0.047)| (-0.085, 0.085)|   4| -0.159|   0.874|  -0.089|   0.929|   4|  0.092|   0.999|   0.035|   1.000|
#   |   5| -0.006| (-0.047, 0.047)| (-0.080, 0.080)|   5| -0.229|   0.819|  -0.136|   0.892|   5|  0.145|   1.000|   0.053|   1.000|
#   |   6|  0.000| (-0.047, 0.047)| (-0.081, 0.081)|   6|  0.003|   0.997|   0.002|   0.998|   6|  0.145|   1.000|   0.053|   1.000|
#   |   7| -0.003| (-0.047, 0.047)| (-0.090, 0.090)|   7| -0.124|   0.901|  -0.065|   0.948|   7|  0.161|   1.000|   0.058|   1.000|
#   |   8|  0.003| (-0.047, 0.047)| (-0.090, 0.090)|   8|  0.108|   0.914|   0.057|   0.955|   8|  0.172|   1.000|   0.061|   1.000|
#   |   9|  0.085| (-0.047, 0.047)| (-0.082, 0.082)|   9|  3.534|   0.000|   2.042|   0.041|   9| 12.743|   0.175|   4.231|   0.896|
#   |  10|  0.024| (-0.047, 0.047)| (-0.074, 0.074)|  10|  0.974|   0.330|   0.625|   0.532|  10| 13.699|   0.187|   4.622|   0.915|
# 
# The visual evidence from the robust confidence bands contradicts the computational evidence of the robust Q-tilde statistic. However, it
# seems that the test does not account for the degrees of freedom the linear model uses. Eventually, the results of the Ljung-Box test are 
# the same as those obtained by the stats::Box.test() function with the fitdf=0 option.
Box_test_ls <- list()
for(l in 1:max_lag){
  Box_test_ls[[l]] <- Box.test(y_res, lag=l,   fitdf=0, type="Ljung-Box")
  show(Box_test_ls[[l]])
}
# Box-Ljung test
# data:  y_res
# X-squared = 0.027864, df = 1,  p-value = 0.8674
# X-squared = 0.066597, df = 2,  p-value = 0.9672
# X-squared = 0.066832, df = 3,  p-value = 0.9955
# X-squared = 0.092262, df = 4,  p-value = 0.999
# X-squared = 0.14498,  df = 5,  p-value = 0.9996
# X-squared = 0.14499,  df = 6,  p-value = 0.9999
# X-squared = 0.16051,  df = 7,  p-value = 1
# X-squared = 0.17222,  df = 8,  p-value = 1
# X-squared = 12.7430,  df = 9,  p-value = 0.1746
# X-squared = 13.6990,  df = 10, p-value = 0.1872
#
# For this reason, we exam other linear models for the ADF test.
# We consider the ADF test with an optimal number of lags according to the AIC.
#
y_ADF_ur.df_trend_AIC_lags <- ur.df(y, type="trend", lags=long_lags, selectlags="AIC")
lm(formula=y_ADF_ur.df_trend_AIC_lags@testreg[["terms"]])
# Call: lm(formula=y_ADF_ur.df_trend_long_lags@testreg[["terms"]])
# Coefficients: (Intercept)       z.lag.1            tt       z.diff.lag1   z.diff.lag2   z.diff.lag3   z.diff.lag4   z.diff.lag5    
#                34.771230     -0.002470         0.024961    -0.019871      0.012292      0.030042      0.026443      0.014041       
#                                                             z.diff.lag6   z.diff.lag7   z.diff.lag8   z.diff.lag9   z.diff.lag10
#                                                               0.032416    -0.057108    -0.038416      0.088120      0.033385 
#                                                              z.diff.lag11  z.diff.lag12  z.diff.lag13  z.diff.lag14  z.diff.lag15  
#                                                             -0.001773    -0.056122      0.036488    -0.003435    -0.029077
#                                                              z.diff.lag16  z.diff.lag17  z.diff.lag18  z.diff.lag19  z.diff.lag20
#                                                              -0.015045      0.025051   -0.007098    -0.061448      0.050838 
#                                                              z.diff.lag21  z.diff.lag22  z.diff.lag23  z.diff.lag24  
#                                                              -0.011420      0.015215    -0.045323      0.077784  
#
# It turns out that the AIC-selected best linear model to perform the ADF test on the Bitcoin Adjusted Price is the linear model with the
# number of lags given by the Schwert formula.
#
# We also consider the ADF test with an optimal number of lags according to the BIC.
y_ADF_ur.df_trend_BIC_lags <- ur.df(y, type="trend", lags=long_lags, selectlags="BIC")
lm(formula=y_ADF_ur.df_trend_BIC_lags@testreg[["terms"]])
# Call: lm(formula=y_ADF_ur.df_trend_BIC_lags@testreg[["terms"]])
# Coefficients: (Intercept)      z.lag.1           tt   z.diff.lag  
#                38.917248   -0.002063     0.011021   -0.030747 
# 
# The BIC-selected best linear model to perform the ADF test on the Bitcoin Adjusted Price is the linear model with one lag.
#
# One might wonder how to determine the minimum AIC or BIC from an ur.df object. A possible solution is in waht follows. 
# Preliminary, note that, as the number of lags increases, the linear models to compare with the AIC or BIC have to be estimated with a
# smaller number of observation. This affects the values of the corresponding AIC and BIC. Therefore, we need to level the number of 
# observations to the number of observations available for the model with the largest number of lags. To this, we cut a suitable number of 
# initial observations for the models with smaller numbers of lags than the largest.
#
head(Data_df)
y <- Data_df$Adj.Close
length(y)
# [1] 1720
sum(is.na(y))
# [1] 0
#
long_lags <- floor(12*(length(y)/100)^(1/4))  # Fixing the maximum number of lags with the Schwert formula
n_obs <- length(y)-(long_lags+1)
show(n_obs)
# [1] 1695
y_ADF_ur.df_trend_lags_ls <- list()                     # Creating an empty list
y_ADF_ur.df_trend_lags_AIC_vec <- rep(NA,(long_lags+1)) # Creating an empty vector to store AIC for different lags
y_ADF_ur.df_trend_lags_BIC_vec <- rep(NA,(long_lags+1)) # Creating an empty vector to store BIC for different lags
for (l in 0:long_lags){
  y_ADF_ur.df_trend_lags_ls[[l+1]] <- ur.df(y[-c(1:(long_lags-l+1))], type="trend", lags=l, selectlags="Fixed")
  show(y_ADF_ur.df_trend_lags_ls[[l+1]])
  show(nobs(lm(formula=y_ADF_ur.df_trend_lags_ls[[l+1]]@testreg[["terms"]])))
  y_ADF_ur.df_trend_lags_AIC_vec[l+1] <- AIC(lm(formula=y_ADF_ur.df_trend_lags_ls[[l+1]]@testreg[["terms"]]))
  show(y_ADF_ur.df_trend_lags_AIC_vec[l+1])
  y_ADF_ur.df_trend_lags_BIC_vec[l+1] <- BIC(lm(formula=y_ADF_ur.df_trend_lags_ls[[l+1]]@testreg[["terms"]]))
  show(y_ADF_ur.df_trend_lags_BIC_vec[l+1])
}
show(y_ADF_ur.df_trend_lags_AIC_vec)
# 28328.01 28328.41 28330.33 28331.54 28330.99 28332.81 28333.45 28331.86 28330.43 28320.19 28321.13 28322.83 28318.14 28318.36 28320.34
# 28321.45 28323.02 28324.38 28326.37 28321.43 28318.79 28320.61 28322.10 28320.27 28312.00
#
show(y_ADF_ur.df_trend_lags_BIC_vec)
# 28349.75 28355.58 28362.94 28369.58 28374.47 28381.72 28387.80 28391.64 28395.65 28390.84 28397.22 28404.35 28405.09 28410.75 28418.17
# 28424.71 28431.72 28438.51 28445.94 28446.43 28449.23 28456.48 28463.40 28467.01 28464.18
#
# We draw an "elbow" plot to show the AIC and BIC values as functions of the lags.
def_margins <- par("mar")
par(mfrow=c(2,1), mar=c(2.5,2.5,4.0,1.0))
plot(y_ADF_ur.df_trend_lags_AIC_vec, type="b", pch=19, col=4)
plot(y_ADF_ur.df_trend_lags_BIC_vec, type="b", pch=19, col=4)
par(mfrow=c(1,1), mar=def_margins)
#
# We compute the minimum values of AIC and BIC and show the lag where the minimum values are attained and the minimum values themselves.
min_AIC_lag <- which(y_ADF_ur.df_trend_lags_AIC_vec==min(y_ADF_ur.df_trend_lags_AIC_vec))
show(c((min_AIC_lag-1),y_ADF_ur.df_trend_lags_AIC_vec[min_AIC_lag]))
# 24 28312
#
min_BIC_lag <- which(y_ADF_ur.df_trend_lags_BIC_vec==min(y_ADF_ur.df_trend_lags_BIC_vec))
show(c((min_BIC_lag-1),y_ADF_ur.df_trend_lags_BIC_vec[min_AIC_lag]))
# 0.00 28464.18
#
# We sort the AIC and BIC values in increasing order.
AIC_lag_sort <- sort(y_ADF_ur.df_trend_lags_AIC_vec, index.return=TRUE, decreasing=FALSE)
show(AIC_lag_sort)
# $x (the sorted values)
# 28312.00 28318.14 28318.36 28318.79 28320.19 28320.27 28320.34 28320.61 28321.13 28321.43 28321.45 28322.10 28322.83 28323.02 28324.38
# 28326.37 28328.01 28328.41 28330.33 28330.43 28330.99 28331.54 28331.86 28332.81 28333.45
#
# $ix (the lags of the sorted values)
# 25 13 14 21 10 24 15 22 11 20 16 23 12 17 18 19  1  2  3  9  5  4  8  6  7 
#
BIC_lag_sort <- sort(y_ADF_ur.df_trend_lags_BIC_vec, index.return=TRUE, decreasing=FALSE)
show(BIC_lag_sort)
# $x (the sorted values)
# 28349.75 28355.58 28362.94 28369.58 28374.47 28381.72 28387.80 28390.84 28391.64 28395.65 28397.22 28404.35 28405.09 28410.75 28418.17
# 28424.71 28431.72 28438.51 28445.94 28446.43 28449.23 28456.48 28463.40 28464.18 28467.01
# 
# $ix (the lags of the sorted values)
# 1  2  3  4  5  6  7 10  8  9 11 12 13 14 15 16 17 18 19 20 21 22 23 25 24
#
# Since AIC and BIC values contrast somewhat with each other, we try to determine an optimal combination mixing them by summing their 
# positions in the sorted sequences.
AIC_BIC_pos_pnt <- vector(mode="integer", length=(long_lags+1))
for(p in 1:(long_lags+1)){
  AIC_BIC_pos_pnt[p] <- (which(AIC_lag_sort$ix==p)+which(BIC_lag_sort$ix==p))
}
show(AIC_BIC_pos_pnt)
# 18 20 22 26 26 30 32 32 30 13 20 25 15 17 22 27 31 33 35 30 25 30 35 31 25
#
# Then we choose the number of lags which produces the smallest sum.
AIC_BIC_pos_pnt_sort <- sort(AIC_BIC_pos_pnt, index.return=TRUE, decreasing=FALSE)
show(AIC_BIC_pos_pnt_sort)
# $x (the sums sorted in increasing order)
# 13 15 17 18 20 20 22 22 25 25 25 26 26 27 30 30 30 30 31 31 32 32 33 35 35
# 
# $ix (the lags of the sorted sums)
# 10 13 14  1  2 11  3 15 12 21 25  4  5 16  6  9 20 22 17 24  7  8 18 19 23
# 
# The "mixed" optimal combination is achieved at p=10 which corresponds to nine lags (p=1 corresponds to zero lags).
#
# Note that in the "elbow" plot at the value p=10 of the lag variable both AIC and BIC paths attain a local minimum (elbow).
# In light of what we have presented above, we perform the ADF test considering nine lags.
l <- 9
y_ADF_ur.df_trend_9_lags <- ur.df(y, type="trend", lags=l, selectlags="Fixed")
lm(formula=y_ADF_ur.df_trend_9_lags@testreg[["terms"]])
# Call: lm(formula=y_ADF_ur.df_trend_9_lags@testreg[["terms"]])
# Coefficients:  (Intercept)    z.lag.1        tt       z.diff.lag1  z.diff.lag2  z.diff.lag3  z.diff.lag4  z.diff.lag5  z.diff.lag6   
#                 33.086555   -0.002487     0.026755   -0.028698     0.012242     0.022141     0.040945     0.009404     0.024969      
#                                                       z.diff.lag7  z.diff.lag8  z.diff.lag9  
#                                                      -0.047953   -0.042174      0.085065  
nobs(lm(formula=y_ADF_ur.df_trend_9_lags@testreg[["terms"]]))
# 1710
n_obs <- length(y)-(l+1)
show(n_obs)
# 1710
n_coeffs <- nrow(y_ADF_ur.df_trend_9_lags@testreg[["coefficients"]])
show(n_coeffs)
# 12
df.residual(lm(formula=y_ADF_ur.df_trend_9_lags@testreg[["terms"]])) 
# 1698
df_res <- n_obs-n_coeffs
show(df_res)
# 1698
summary(y_ADF_ur.df_trend_9_lags)
############################################### 
# Augmented Dickey-Fuller Test Unit Root Test # 
############################################### 
# Call: lm(formula=z.diff ~ z.lag.1 + 1 + tt + z.diff.lag)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7468.1 -225.7  -15.0   215.0  7208.3 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 33.086555  50.026146   0.661 0.508455    
# z.lag.1    -0.002487   0.001925 -1.292 0.196541    
# tt           0.026755   0.067368   0.397 0.691307    
# z.diff.lag1 -0.028698   0.024204  -1.186 0.235909    
# z.diff.lag2  0.012242   0.024183   0.506 0.612768    
# z.diff.lag3  0.022141   0.024148   0.917 0.359334    
# z.diff.lag4  0.040945   0.024149   1.695 0.090164 .  
# z.diff.lag5  0.009404   0.024169   0.389 0.697247    
# z.diff.lag6  0.024969   0.024154   1.034 0.301406    
# z.diff.lag7 -0.047953   0.024156  -1.985 0.047294 *  
# z.diff.lag8 -0.042174   0.024184  -1.744 0.081360 .  
# z.diff.lag9  0.085065   0.024192   3.516 0.000449 ***
#  ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1024 on 1698 degrees of freedom
# Multiple R-squared:  0.01621,	Adjusted R-squared:  0.009839 
# F-statistic: 2.544 on 11 and 1698 DF,  p-value: 0.003441
# 
# Value of test-statistic is: -1.292 0.6939 1.0282 
# 
# Critical values for test statistics: 
#       1pct  5pct 10pct
# tau3 -3.96 -3.41 -3.12
# phi2  6.09  4.68  4.03
# phi3  8.27  6.25  5.34
#
# The null hypothesis cannot be rejected against the three alternatives at the $10\%$ significance level.
# However, to validate the test, we need to check the possible presence of autocorrelation in the residuals of the model used for the test.
y_res <- as.vector(y_ADF_ur.df_trend_9_lags@testreg[["residuals"]])
nobs(lm(formula=y_ADF_ur.df_trend_9_lags@testreg[["terms"]]))
max_lag <- ceiling(min(10, n_obs/4))    # Hyndman (for data without seasonality, see https://robjHyndman.com/hy_resndsight/ljung-box-test/)
show(max_lag)
# 10
n_coeffs <- nrow(y_ADF_ur.df_trend_9_lags@testreg[["coefficients"]])
show(n_coeffs)
# 12
n_pars <- n_coeffs
show(n_pars)
# 12
fit_df <- n_pars
LB_fit_df <- min(min(max_lag, n_pars), max_lag-1)
show(LB_fit_df)
# 9
y_res_LB <- Box.test(y_res, lag=max_lag, fitdf=LB_fit_df, type="Ljung-Box")
show(y_res_LB)
# Box-Ljung test
# data:  y_res
# X-squared=1.9316, df=1, p-value=0.1646
#
FitAR::LjungBoxTest(y_res, k=n_pars, lag.max=max_lag, StartLag=1, SquaredQ=FALSE)
#  m  Qm    pvalue
#  1 0.01 0.9320035
#  2 0.01 0.9316027
#  3 0.10 0.7519338
#  4 0.16 0.6928563
#  5 0.16 0.6871187
#  6 0.19 0.6630801
#  7 0.19 0.6611990
#  8 0.20 0.6577588
#  9 0.20 0.6541153
# 10 1.93 0.1645825
#
n_pars_seq <- rep(NA,max_lag)
for(l in 1:max_lag){
  if(l-n_pars<0) n_pars_seq[l] <- l-1
  else n_pars_seq[l] <- n_pars
}
show(n_pars_seq)
# 0 1 2 3 4 5 6 7 8 9
#
Box_test_ls <- list()
for(l in 1:max_lag){
  Box_test_ls[[l]] <- Box.test(y_res, lag=l,   fitdf=n_pars_seq[l], type="Ljung-Box")
  show(Box_test_ls[[l]])
}
# Box-Ljung test
# data:  y_res
# X-squared=0.0072802, df=1, p-value=0.932
# X-squared=0.0073665, df=1, p-value=0.9316
# X-squared=0.099913,  df=1, p-value=0.7519
# X-squared=0.15601,   df=1, p-value=0.6929
# X-squared=0.16222,   df=1, p-value=0.6871
# X-squared=0.1898,    df=1, p-value=0.6631
# X-squared=0.19207,   df=1, p-value=0.6612
# X-squared=0.19626,   df=1, p-value=0.6578
# X-squared=0.20075,   df=1, p-value=0.6541
# X-squared=1.9316,    df=1, p-value=0.1646
#
y_res <- as.vector(y_ADF_ur.df_trend_9_lags@testreg[["residuals"]])
T <- n_obs
# max_lag <- ceiling(10*log10(T))    # Default
# max_lag <- ceiling(sqrt(n)+45)     # Box-Jenkins
max_lag <- ceiling(min(10, T/4))     # Hyndman (for data without seasonality)
# max_lag <- ceiling(min(2*12, T/5)) # Hyndman (for data with seasonality, see https://robjHyndman.com/hy_resndsight/ljung-box-test/)
# Aut_Fun_y <- stats::acf(y, lag.max=max_lag, type="correlation", plot=FALSE)
Aut_Fun_y_res <- TSA::acf(y_res, lag.max=max_lag, type= "correlation", plot=FALSE)
ci_090 <- qnorm((1+0.90)/2)/sqrt(T)
ci_95 <- qnorm((1+0.95)/2)/sqrt(T)
ci_99 <- qnorm((1+0.99)/2)/sqrt(T)
Aut_Fun_y_res <- data.frame(lag=Aut_Fun_y_res$lag, acf=Aut_Fun_y_res$acf)
# First_Date <- paste(Data_df$Month[1],Data_df$y_resear[1])
# Last_Date <- paste(Data_df$Month[T],Data_df$y_resear[T])
First_Date <- as.character(Data_df$Date[1])
Last_Date <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025", 
                             paste("Autocorrelogram of the Residuals of the Linear Model with AIC-BIC Selected Nine (9) Lags for the ADF Test for the Bitcoin Daily Adjusted Close from ", .(First_Date), " to ", .(Last_Date))))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Data set size ",.(TrnS_length)," sample points. Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("lags")
x_breaks_num <- max_lag
x_binwidth <- 1
x_breaks <- Aut_Fun_y_res$lag
x_labs <- format(x_breaks, scientific=FALSE)
Plot_Aut_Fun_y_res <- ggplot(Aut_Fun_y_res, aes(x=lag, y=acf))+
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), linewidth=1, col= "black") +
  # geom_col(mapping=NULL, data=NULL, position= "dodge", width=0.1, col= "black", inherit.aes=TRUE)+
  geom_hline(aes(yintercept=-ci_090, color= "CI_090"), show.legend=TRUE, lwd=0.9, lty=3) +
  geom_hline(aes(yintercept=ci_090, color= "CI_090"), lwd=0.9, lty=3) +
  geom_hline(aes(yintercept=ci_95, color= "CI_95"), show.legend=TRUE, lwd=0.8, lty=2)+
  geom_hline(aes(yintercept=-ci_95, color= "CI_95"), lwd=0.8, lty=2) +
  geom_hline(aes(yintercept=-ci_99, color= "CI_99"), show.legend=TRUE, lwd=0.8, lty=4) +
  geom_hline(aes(yintercept=ci_99, color= "CI_99"), lwd=0.8, lty=4) +
  scale_x_continuous(name= "lag", breaks=x_breaks, label=x_labs) +
  scale_y_continuous(name= "acf value", breaks=waiver(), labels=NULL,
                     sec.axis=sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name= "Conf. Inter.", labels=c("90%","95%","99%"), values=c(CI_090="green", CI_95="blue", CI_99="red"),
                     guide=guide_legend(override.aes=list(linetype=c("dotted", "dashed", "dotdash")))) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
  # theme(plot.title=element_blank(), 
  #       plot.subtitle=element_blank(),
  #       plot.caption=element_text(hjust=1.0),
  #       legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(Plot_Aut_Fun_y_res)
#
# We consider the Breusch-Godfrey test for autocorrelation.
lm(formula=y_ADF_ur.df_trend_9_lags@testreg[["terms"]])
# Call: lm(formula=y_ADF_ur.df_trend_short_lags@testreg[["terms"]])
# Coefficients:
#   (Intercept)      z.lag.1         tt      z.diff.lag1  z.diff.lag2  z.diff.lag3  z.diff.lag4  z.diff.lag5  z.diff.lag6  z.diff.lag7  
#    33.086555      -0.002487     0.026755   -0.028698     0.012242     0.022141     0.040945     0.009404     0.024969    -0.047953     
#                                            z.diff.lag8  z.diff.lag9  
#                                             -0.042174    0.085065
#
lmtest::bgtest(lm(formula=y_ADF_ur.df_trend_9_lags@testreg[["terms"]]), order=10, type="Chisq")
# Breusch-Godfrey test for serial correlation of order up to 10
# data:  lm(formula = y_ADF_ur.df_trend_short_lags@testreg[["terms"]])
# LM test = 13.982, df = 10, p-value = 0.1738
#
lmtest::bgtest(lm(formula=y_ADF_ur.df_trend_9_lags@testreg[["terms"]]), order=10, type="F")
# Breusch-Godfrey test for serial correlation of order up to 10
# data:  lm(formula = y_ADF_ur.df_trend_9_lags@testreg[["terms"]])
# LM test = 1.3916, df1 = 10, df2 = 1688, p-value = 0.178
#
# The result of the two versions of the Breusch-Godfrey test confirms the non-rejection of the null hypothesis of no autocorrelation at the
# $10\%$ significance level.
# 
# Nevertheless, a plot of the residuals shows a clear visual evidence for heteroscedasticity and advocates the robust tests for 
# autocorrelation.
head(BTC_train_df)
tail(BTC_train_df)
Data_df <- BTC_train_df
head(Data_df)
length(as.vector(y_ADF_ur.df_trend_9_lags@testreg[["residuals"]]))
# 1711
Data_df <- add_column(Data_df, ADF_Long_lag_y_res=c(rep(NA,10),as.vector(y_ADF_ur.df_trend_9_lags@testreg[["residuals"]])), .after="Adj.Close")
head(Data_df)
Data_df <- dplyr::rename(Data_df, x=t, y=ADF_Long_lag_y_res)
TrnS_length <- length(Data_df$Adj.Close)
show(TrnS_length)
# 1720
First_Day <- as.character(Data_df$Date[2])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Residuals of the Linear Model with AIC-BIC Selected Nine (9) Lags for the ADF Test on the Bitcoin Daily Adjusted Close Price Training Set - from ", .(First_Day), " to ", .(Last_Day), sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points. Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("")
# numbers::primeFactors(TrnS_length)
x_breaks_num <- 40 # (deduced from primeFactors(TrnS_length-2))
x_breaks_low <- Data_df$x[1]
x_breaks_up <- Data_df$x[(TrnS_length)]
x_binwidth <- ceiling((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Date[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("residuals of the linear model")
y_max <- max(na.rm(Data_df$y))
y_min <- min(na.rm(Data_df$y))
as.numeric(floor(y_max-y_min))
y_breaks_num <- 10
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor((y_min/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((y_max/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth), digits=3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.5
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
point_black <- bquote("GARCH(1,1) stand. residuals")
line_green <- bquote("regression line")
line_red <- bquote("LOESS curve")
leg_point_labs <- c(point_black)
leg_point_cols <- c("point_black"="black")
leg_point_breaks <- c("point_black")
leg_line_labs <- c(line_green, line_red)
leg_line_cols <- c("line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_green", "line_red")
leg_col_labs   <- c(leg_point_labs,leg_line_labs)
leg_col_cols   <- c(leg_point_cols,leg_line_cols)
leg_col_breaks <- c(leg_point_breaks,leg_line_breaks)
BTC_ACP_TrnS_9_Lags_ADF_res_sp <- ggplot(Data_df, aes(x=x)) +
  geom_smooth(aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_point(aes(y=y, color="point_black"), alpha=1, size=0.7, shape=19, na.rm=TRUE) + 
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  guides(colour=guide_legend(override.aes=list(shape=c(19, NA, NA), linetype=c("blank", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_ACP_TrnS_9_Lags_ADF_res_sp)
#
# The line plot
BTC_ACP_TrnS_9_Lags_ADF_res_lp <- ggplot(Data_df, aes(x=x)) +
  geom_smooth(aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_line(aes(y=y, color="point_black"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_ACP_TrnS_9_Lags_ADF_res_lp)
#
y_res <- as.vector(y_ADF_ur.df_trend_9_lags@testreg[["residuals"]])
testcorr::ac.test(y_res, max.lag = 10, alpha = 0.10, lambda = 2.576, plot = TRUE, table = TRUE, var.name = NULL, scale.font = 1)
# Tests for zero autocorrelation of x
#   | Lag|     AC|  Stand. CB(90%)|  Robust CB(90%)| Lag|      t| p-value| t-tilde| p-value| Lag|    LB| p-value| Q-tilde| p-value|
#   |---:|------:|---------------:|---------------:|---:|------:|-------:|-------:|-------:|---:|-----:|-------:|-------:|-------:|
#   |   1| -0.002| (-0.040, 0.040)| (-0.061, 0.061)|   1| -0.085|   0.932|  -0.056|   0.956|   1| 0.007|   0.932|   0.003|   0.956|
#   |   2|  0.000| (-0.040, 0.040)| (-0.064, 0.064)|   2| -0.009|   0.993|  -0.006|   0.995|   2| 0.007|   0.996|   0.003|   0.998|
#   |   3|  0.007| (-0.040, 0.040)| (-0.059, 0.059)|   3|  0.304|   0.761|   0.205|   0.837|   3| 0.100|   0.992|   0.045|   0.997|
#   |   4| -0.006| (-0.040, 0.040)| (-0.072, 0.072)|   4| -0.236|   0.813|  -0.130|   0.896|   4| 0.156|   0.997|   0.062|   1.000|
#   |   5| -0.002| (-0.040, 0.040)| (-0.067, 0.067)|   5| -0.079|   0.937|  -0.047|   0.963|   5| 0.162|   0.999|   0.064|   1.000|
#   |   6|  0.004| (-0.040, 0.040)| (-0.068, 0.068)|   6|  0.166|   0.868|   0.097|   0.923|   6| 0.190|   1.000|   0.074|   1.000|
#   |   7| -0.001| (-0.040, 0.040)| (-0.075, 0.075)|   7| -0.047|   0.962|  -0.025|   0.980|   7| 0.192|   1.000|   0.074|   1.000|
#   |   8| -0.002| (-0.040, 0.040)| (-0.076, 0.076)|   8| -0.065|   0.949|  -0.034|   0.973|   8| 0.196|   1.000|   0.075|   1.000|
#   |   9|  0.002| (-0.040, 0.040)| (-0.066, 0.066)|   9|  0.067|   0.947|   0.040|   0.968|   9| 0.201|   1.000|   0.077|   1.000|
#   |  10|  0.032| (-0.040, 0.040)| (-0.062, 0.062)|  10|  1.311|   0.190|   0.839|   0.401|  10| 1.932|   0.997|   0.781|   1.000|
#
# The robust autocorrelation test does not reject the null hypothesis of no autocorrelation at the $10\%$ significance level. Therefore,
# the linear model with nine lags for the ADF test appears to be a good combination in terms of model parsimony and lack of autocorrelation
# in the residuals.
#
# In the end, we check closer the behavior of the linear model for the Breusch_Godfrey test.
# We define the variables
y <- Data_df$Adj.Close
length(y)
# 1720
z <- c(diff(y,differences=1))
length(z)
# 1719
tt <- c(1:(length(y)-1))
length(tt)
# 1719
y_lag_1 <- y[-length(y)]
length(y_lag_1)
# 1719
y_lag_2 <- c(NA,y_lag_1[-length(y_lag_1)])
y_lag_3 <- c(NA,y_lag_2[-length(y_lag_2)])
y_lag_4 <- c(NA,y_lag_3[-length(y_lag_3)])
y_lag_5 <- c(NA,y_lag_4[-length(y_lag_4)])
y_lag_6 <- c(NA,y_lag_5[-length(y_lag_5)])
y_lag_7 <- c(NA,y_lag_6[-length(y_lag_6)])
y_lag_8 <- c(NA,y_lag_7[-length(y_lag_7)])
y_lag_9 <- c(NA,y_lag_8[-length(y_lag_8)])
y_lag_10 <- c(NA,y_lag_9[-length(y_lag_9)])
y_lag_11 <- c(NA,y_lag_10[-length(y_lag_10)])
y_lag_12 <- c(NA,y_lag_11[-length(y_lag_11)])
y_lag_13 <- c(NA,y_lag_12[-length(y_lag_12)])
y_lag_14 <- c(NA,y_lag_13[-length(y_lag_13)])
y_lag_15 <- c(NA,y_lag_14[-length(y_lag_14)])
y_lag_16 <- c(NA,y_lag_15[-length(y_lag_15)])
y_lag_17 <- c(NA,y_lag_16[-length(y_lag_16)])
y_lag_18 <- c(NA,y_lag_17[-length(y_lag_17)])
y_lag_19 <- c(NA,y_lag_18[-length(y_lag_18)])
y_lag_20 <- c(NA,y_lag_19[-length(y_lag_19)])
y_lag_21 <- c(NA,y_lag_20[-length(y_lag_20)])
y_lag_22 <- c(NA,y_lag_21[-length(y_lag_21)])
y_lag_23 <- c(NA,y_lag_22[-length(y_lag_22)])
y_lag_24 <- c(NA,y_lag_23[-length(y_lag_23)])
#
z_diff_lag_1 <- c(NA,diff(y_lag_1, differences=1))
length(z_diff_lag_1)
# 1719
z_diff_lag_2 <- c(NA,diff(y_lag_2, differences=1))
z_diff_lag_3 <- c(NA,diff(y_lag_3, differences=1))
z_diff_lag_4 <- c(NA,diff(y_lag_4, differences=1))
z_diff_lag_5 <- c(NA,diff(y_lag_5, differences=1))
z_diff_lag_6 <- c(NA,diff(y_lag_6, differences=1))
z_diff_lag_7 <- c(NA,diff(y_lag_7, differences=1))
z_diff_lag_8 <- c(NA,diff(y_lag_8, differences=1))
z_diff_lag_9 <- c(NA,diff(y_lag_9, differences=1))
z_diff_lag_10 <- c(NA,diff(y_lag_10, differences=1))
z_diff_lag_11 <- c(NA,diff(y_lag_11, differences=1))
z_diff_lag_12 <- c(NA,diff(y_lag_12, differences=1))
z_diff_lag_13 <- c(NA,diff(y_lag_13, differences=1))
z_diff_lag_14 <- c(NA,diff(y_lag_14, differences=1))
z_diff_lag_15 <- c(NA,diff(y_lag_15, differences=1))
z_diff_lag_16 <- c(NA,diff(y_lag_16, differences=1))
z_diff_lag_17 <- c(NA,diff(y_lag_17, differences=1))
z_diff_lag_18 <- c(NA,diff(y_lag_18, differences=1))
z_diff_lag_19 <- c(NA,diff(y_lag_19, differences=1))
z_diff_lag_20 <- c(NA,diff(y_lag_20, differences=1))
z_diff_lag_21 <- c(NA,diff(y_lag_21, differences=1))
z_diff_lag_22 <- c(NA,diff(y_lag_22, differences=1))
z_diff_lag_23 <- c(NA,diff(y_lag_23, differences=1))
z_diff_lag_24 <- c(NA,diff(y_lag_24, differences=1))
#
# We consider the residuals of the linear model selected with the AIC-BIC criterion and used for the ADF test and build the lagged residuals
res <- residuals(lm(z~tt+y_lag_1+z_diff_lag_1+z_diff_lag_2+z_diff_lag_3+z_diff_lag_4+z_diff_lag_5+z_diff_lag_6+z_diff_lag_7+z_diff_lag_8
                    +z_diff_lag_9, data=ADF_LM_df))
res_lag_1 <- c(NA,res[-length(res)])
res_lag_2 <- c(NA,res_lag_1[-length(res_lag_1)])
res_lag_3 <- c(NA,res_lag_2[-length(res_lag_2)])
res_lag_4 <- c(NA,res_lag_3[-length(res_lag_3)])
res_lag_5 <- c(NA,res_lag_4[-length(res_lag_4)])
res_lag_6 <- c(NA,res_lag_5[-length(res_lag_5)])
res_lag_7 <- c(NA,res_lag_6[-length(res_lag_6)])
res_lag_8 <- c(NA,res_lag_7[-length(res_lag_7)])
res_lag_9 <- c(NA,res_lag_8[-length(res_lag_8)])
res_lag_10 <- c(NA,res_lag_9[-length(res_lag_9)])
# 
# We regress the residuals on the linear model for the Breusch-Godfrey test.
BG_LM_9 <- lm(res[-c(1:10)]~tt[-c(1:19)]+y_lag_1[-c(1:19)]+z_diff_lag_1[-c(1:19)]+z_diff_lag_2[-c(1:19)]+z_diff_lag_3[-c(1:19)]    
                            +z_diff_lag_4[-c(1:19)]+z_diff_lag_5[-c(1:19)]+z_diff_lag_6[-c(1:19)]+z_diff_lag_7[-c(1:19)]
                            +z_diff_lag_8[-c(1:19)]+z_diff_lag_9[-c(1:19)]+res_lag_1[-c(1:10)]+res_lag_2[-c(1:10)]+res_lag_3[-c(1:10)]
                            +res_lag_4[-c(1:10)]+res_lag_5[-c(1:10)]+res_lag_6[-c(1:10)]+res_lag_7[-c(1:10)]+res_lag_8[-c(1:10)]
                            +res_lag_9[-c(1:10)]+res_lag_10[-c(1:10)], data=ADF_LM_df)
summary(BG_LM_9)
# Call: lm(formula = res[-c(1:10)] ~ tt[-c(1:19)] + y_lag_1[-c(1:19)] + [-c(1:19)] + z_diff_lag_2[-c(1:19)] + z_diff_lag_3[-c(1:19)] 
#                                  + z_diff_lag_4[-c(1:19)] + z_diff_lag_5[-c(1:19)] + z_diff_lag_6[-c(1:19)] + z_diff_lag_7[-c(1:19)] 
#                                  + z_diff_lag_8[-c(1:19)] + z_diff_lag_9[-c(1:19)] + res_lag_1[-c(1:10)] + res_lag_2[-c(1:10)] 
#                                  + res_lag_3[-c(1:10)] + res_lag_4[-c(1:10)] + res_lag_5[-c(1:10)] + res_lag_6[-c(1:10)] 
#                                  + res_lag_7[-c(1:10)] + res_lag_8[-c(1:10)] + res_lag_9[-c(1:10)] + res_lag_10[-c(1:10)], 
#                                    data = ADF_LM_df)
# 
# Residuals:  Min      1Q     Median   3Q     Max 
#           -7020.8  -223.2   -16.0   220.9  6882.5 
# 
# Coefficients:            Estimate Std. Error t value Pr(>|t|)   
# (Intercept)            287.387564 122.650968   2.343  0.01924 * 
# tt[-c(1:19)]             0.221153   0.106860   2.070  0.03865 * 
# y_lag_1[-c(1:19)]       -0.021166   0.008298  -2.551  0.01084 * 
# z_diff_lag_1[-c(1:19)]  -7.911112   3.050619  -2.593  0.00959 **
# z_diff_lag_2[-c(1:19)]   0.509929   0.677571   0.753  0.45181   
# z_diff_lag_3[-c(1:19)]   0.338392   0.657224   0.515  0.60670   
# z_diff_lag_4[-c(1:19)]   0.879753   0.527617   1.667  0.09562 . 
# z_diff_lag_5[-c(1:19)]   0.371939   0.499081   0.745  0.45623   
# z_diff_lag_6[-c(1:19)]  -0.416271   0.440784  -0.944  0.34511   
# z_diff_lag_7[-c(1:19)]  -0.224748   0.426003  -0.528  0.59787   
# z_diff_lag_8[-c(1:19)]  -0.834868   0.505962  -1.650  0.09912 . 
# z_diff_lag_9[-c(1:19)]  -0.727874   0.381939  -1.906  0.05686 . 
# res_lag_1[-c(1:10)]      7.932660   3.057703   2.594  0.00956 **
# res_lag_2[-c(1:10)]     -0.733062   0.698861  -1.049  0.29436   
# res_lag_3[-c(1:10)]     -0.212183   0.623845  -0.340  0.73381   
# res_lag_4[-c(1:10)]     -0.711516   0.489204  -1.454  0.14601   
# res_lag_5[-c(1:10)]     -0.040490   0.441376  -0.092  0.92692   
# res_lag_6[-c(1:10)]      0.460230   0.426974   1.078  0.28124   
# res_lag_7[-c(1:10)]      0.376904   0.414680   0.909  0.36353   
# res_lag_8[-c(1:10)]      0.401888   0.399667   1.006  0.31477   
# res_lag_9[-c(1:10)]      0.423843   0.324198   1.307  0.19127   
# res_lag_10[-c(1:10)]     0.751552   0.286617   2.622  0.00882 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1024 on 1678 degrees of freedom
# Multiple R-squared:  0.01097,	Adjusted R-squared:  -0.001411 
# F-statistic: 0.886 on 21 and 1678 DF,  p-value: 0.6103
#
# We evaluate the LM statistic and the corresponding p-value.
show(c(nobs(BG_LM_9)*summary(BG_LM_9)$r.squared, pchisq(q=nobs(BG_LM_9)*summary(BG_LM_9)$r.squared, d=10, lower.tail=FALSE)))
# 18.64296834  0.04503899
#
# Note that the direct application of the test yields
lmtest::bgtest(lm(z~tt+y_lag_1+z_diff_lag_1+z_diff_lag_2+z_diff_lag_3+z_diff_lag_4+z_diff_lag_5+z_diff_lag_6+z_diff_lag_7+z_diff_lag_8
                  +z_diff_lag_9), order=10, type="Chisq", fill=NA)
# LM test = 18.643, df = 10, p-value = 0.04504
#
# We suppress the lagged residuals from the model.
# BG_LM_9_bis <- lm(res[-c(1:10)]~tt[-c(1:19)]+y_lag_1[-c(1:19)]+z_diff_lag_1[-c(1:19)]+z_diff_lag_2[-c(1:19)]+z_diff_lag_3[-c(1:19)]    
#                   +z_diff_lag_4[-c(1:19)]+z_diff_lag_5[-c(1:19)]+z_diff_lag_6[-c(1:19)]+z_diff_lag_7[-c(1:19)]+z_diff_lag_8[-c(1:19)]
#                   +z_diff_lag_9[-c(1:19)], data=ADF_LM_df)
# summary(BG_LM_9_bis)
# Call: lm(formula = res[-c(1:10)] ~ tt[-c(1:19)] + y_lag_1[-c(1:19)] + z_diff_lag_1[-c(1:19)] + z_diff_lag_2[-c(1:19)]
#                                    + z_diff_lag_3[-c(1:19)] + z_diff_lag_4[-c(1:19)] + z_diff_lag_5[-c(1:19)] + z_diff_lag_6[-c(1:19)]
#                                    + z_diff_lag_7[-c(1:19)] + z_diff_lag_8[-c(1:19)] + z_diff_lag_9[-c(1:19)], data = ADF_LM_df)
# 
# Residuals:  Min      1Q    Median   3Q     Max 
#          -7467.3  -226.9   -15.1   215.1  7208.5 
# 
# Coefficients: Estimate   Std. Error t value Pr(>|t|)
# (Intercept)            -0.633253263 50.743732211  -0.012    0.990
# tt[-c(1:19)]            0.000724328  0.068128932   0.011    0.992
# y_lag_1[-c(1:19)]      -0.000007241  0.001932670  -0.004    0.997
# z_diff_lag_1[-c(1:19)]  0.000110703  0.024276266   0.005    0.996
# z_diff_lag_2[-c(1:19)] -0.000161640  0.024259924  -0.007    0.995
# z_diff_lag_3[-c(1:19)]  0.000340716  0.024228711   0.014    0.989
# z_diff_lag_4[-c(1:19)] -0.000062630  0.024228233  -0.003    0.998
# z_diff_lag_5[-c(1:19)] -0.000206638  0.024248374  -0.009    0.993
# z_diff_lag_6[-c(1:19)]  0.000115066  0.024233025   0.005    0.996
# z_diff_lag_7[-c(1:19)]  0.000050756  0.024236438   0.002    0.998
# z_diff_lag_8[-c(1:19)]  0.000146780  0.024263977   0.006    0.995
# z_diff_lag_9[-c(1:19)] -0.000207143  0.024271767  -0.009    0.993
# 
# Residual standard error: 1027 on 1688 degrees of freedom
# Multiple R-squared:  3.649e-07,	Adjusted R-squared:  -0.006516 
# F-statistic: 5.599e-05 on 11 and 1688 DF,  p-value: 1
#
# We evaluate the LM statistic and the corresponding p-value.
# show(c(nobs(BG_LM_9_bis)*summary(BG_LM_9_bis)$r.squared, pchisq(q=nobs(BG_LM_9_bis)*summary(BG_LM_9_bis)$r.squared, d=10, lower.tail=FALSE)))
# 0.0006202932 1.0000000000
#
# Also in this case, the introduction of the lagged residuals makes some exogenous variables endogenous. However, this is insufficient to 
# reject the null hypothesis of no autocorrelation at nearly the $5\%$ significance level in the residuals of the linear model with nine
# lags used for the ADF test. However, combining this result with the robust approach, we can conclude in favor of the null hypothesis of no
# autocorrelation at the $10\%$ significance level. This validates the non-rejection of the unit root component in the Bitcoin adjusted 
# close price time series at the $10\%$ significance level.
#
############################################################################################################################################
############################################################################################################################################
# We consider the *KPSS* test, which assumes the null hypothesis that the time series can be considered a path of an autoregressive process. 
# In terms of the null hypothesis, the test allows us to specify an autoregressive process with drift, type="mu", or an autoregressive 
# process with drift and linear trend, type="tau".
#   
# Focusing on the type "tau", the *KPSS* test contained in the library *urca*, also allows different possibilities for the number of lags.
# More specifically, we have
#
head(Data_df)
y <- Data_df$Adj.Close
#
y_KPSS_ur_tau_nil <- ur.kpss(y, type="tau", lags="nil")
summary(y_KPSS_ur_tau_nil)
####################### 
# KPSS Unit Root Test # 
####################### 
# Test is of type: tau with 0 lags. 
# Value of test-statistic is: 15.2432 
# Critical value for a significance level of: 
#                 10pct  5pct 2.5pct  1pct
# critical values 0.119 0.146  0.176 0.216
#
# When the null hypothesis consists of an autoregressive model with drift, linear trend, and 0 lags, we have a rejection of the null 
# hypothesis in favor of the alternative at $1\%$ significance level.
#
y_KPSS_ur_tau_short <- ur.kpss(y, type="tau", lags="short")
summary(y_KPSS_ur_tau_short)
####################### 
# KPSS Unit Root Test # 
####################### 
# Test is of type: tau with 8 lags. 
# Value of test-statistic is: 1.7148 
# Critical value for a significance level of: 
#                 10pct  5pct 2.5pct  1pct
# critical values 0.119 0.146  0.176 0.216
#
# When the null hypothesis consists of an autoregressive model with drift, linear trend, and a small number of lags, we still have a 
# rejection of the null hypothesis in favor of the alternative at $1\%$ significance level, but the rejection is weaker than the case with 
# zero lags.
#
y_KPSS_ur_tau_long <- ur.kpss(y, type="tau", lags="long") # the Schwert formula
summary(y_KPSS_ur_tau_long)
####################### 
# KPSS Unit Root Test # 
####################### 
# Test is of type: tau with 24 lags. 
# Value of test-statistic is: 0.6324 
# Critical value for a significance level of: 
#                 10pct  5pct 2.5pct  1pct
# critical values 0.119 0.146  0.176 0.216
#
# When the null hypothesis consists of an autoregressive model with drift, linear trend, and a large number of lags, we still have a
# rejection of the null hypothesis in favor of the alternative at $1\%$ significance level, but the rejection is weaker than the case with
# a small number of lags.
#
# On increasing the number of lags of the lagged autoregressive model with drift and linear trend considered as the null hypothesis,
# rejecting the null hypothesis in favor of the alternative at $1\%$ significance level becomes weaker but cannot be avoided.
#
# In light of the results of the *ADF* and *KPSS* we have to think of the Bitcoin Daily Adjusted Close Price as a path of a process containing a 
# random walk component.
############################################################################################################################################
# In the presence of a random walk component, the strategy to render a time series stationary is differencing it. However, in the case of a
# time series representing a stock price in a financial market, the standard procedure prescribes applying the logarithm transformation
# before differencing. This is because by differencing the logarithm transformation of a stock price time series, we obtain the time series
# of the logarithm returns of the stock, which is the time series of genuine interest for a financial analyst.
# Note that by the logarithm transformation, we eliminate a likely exponential deterministic trend, which is evidenced by the structure 
# of the first part of the LOESS curve in the scatter and line plot of the data. This exponential trend is a natural component of the price
# of financial assets due to the time value of the money. On the other hand, by differencing, we eliminate a random walk component in the 
# log-transformed stock price. A random walk is also a natural component of a stock price time series since the stock prices in financial 
# markets are continuously updated by the buying and selling activity due to the erratic arrival of price-sensitive news, which modifies the 
# investors' perception of the value of the stocks. Last, but not the least, a logarithm transformation, which is a particular Box-Cox 
# transformation can significantly reduce the heteroscedasticity in the time series.
# We consider logarithm transformation and differencing separately, but it is pretty standard practice to consider the two transformations
# at once.
#
############################ Logarithm transformation of the Bitcoin Daily Adjusted Close Price  ###########################################
# We create the logarithm transformation of the Adj.Close column in the BTC_red_df data frame adding it to the BTC_red_df data frame itself.
head(BTC_red_df)
BTC_red_df <- add_column(BTC_red_df, Adj.Close_log=log(BTC_red_df$Adj.Close), .after="Adj.Close")
head(BTC_red_df)
tail(BTC_red_df)
# Hence we consider the scatter and line plot of the log-transformed training and test set
Data_df <- BTC_red_df
head(Data_df)
tail(Data_df)
Data_df <- dplyr::rename(Data_df, x=t, y=Adj.Close_log)
head(Data_df)
DS_length <- length(Data_df$y)
show(DS_length)
# 1871
First_Day <- as.character(Data_df$Date[1])
show(First_Day)
# "2018-04-17"
Last_Day <- as.character(Data_df$Date[DS_length])
show(Last_Day)
# "2023-05-31"
TrnS_Last_Day <- as.character(Data_df$Date[which(Data_df$Date==as.Date("2023-01-01"))-1])
show(TrnS_Last_Day)
# "2022-12-31"
TrnS_length <- length(Data_df$Date[which(Data_df$Date<as.Date("2023-01-01"))])
show(TrnS_length)
# 1720
TstS_First_Day <- as.character(Data_df$Date[which(Data_df$Date==as.Date("2023-01-01"))])
show(TstS_First_Day)
# "2023-01-01"
TstS_length <- length(Data_df$Date[which(Data_df$Date>=as.Date("2023-01-01"))])
show(TstS_length)
# 151
TstS_length == DS_length-TrnS_length
# TRUE
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Bitcoin Daily Adjusted Close Price Logarithm - Training and Test Sets - from ", .(First_Day), " to ", .(Last_Day), sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points, from ", .(First_Day), " to ", .(TrnS_Last_Day),". Test set length ", .(TstS_length), " sample points, from ", .(TstS_First_Day), " to ", .(Last_Day),". Data by courtesy of Yahoo Finance - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("")
# numbers::primeFactors(DS_length-1)
x_breaks_num <- 34 # (deduced from primeFactors(DS_length-1))
x_breaks_low <- Data_df$x[1]
x_breaks_up <- Data_df$x[DS_length]
x_binwidth <- ceiling((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Date[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("daily adjusted close price logarithms (US $)")
y_max <- max(na.rm(Data_df$y))
y_min <- min(na.rm(Data_df$y))
as.numeric(floor(y_max-y_min))
y_breaks_num <- 10
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor((y_min/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((y_max/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth), digits=3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.5
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
point_black <- bquote("daily close price logs (US $) training set")
point_b <- bquote("daily close price logs (US $) test set")
line_green <- bquote("regression line (training set)")
line_red <- bquote("LOESS curve (training set)")
leg_point_labs <- c(point_black, point_b)
leg_point_cols <- c("point_black"="black", "point_b"="blue")
leg_point_breaks <- c("point_black", "point_b")
leg_line_labs <- c(line_green, line_red)
leg_line_cols <- c("line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_green", "line_red")
leg_col_labs   <- c(leg_point_labs,leg_line_labs)
leg_col_cols   <- c(leg_point_cols,leg_line_cols)
leg_col_breaks <- c(leg_point_breaks,leg_line_breaks)
BTC_Adj.Close_log_TrnS_TstS_sp <- ggplot(Data_df, aes(x=x)) +
  geom_vline(xintercept=Data_df$x[TrnS_length], lwd=0.5, color="black") +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", 
              method="lm" , formula=y ~ x, se=FALSE) +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", 
              method="loess", formula=y ~ x, se=FALSE) +
  geom_point(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="point_black"), alpha=1, size=0.7, shape=19) + 
  geom_point(data=subset(Data_df, Data_df$x>x[TrnS_length]), aes(y=y, color="point_b"), alpha=1, size=0.7, shape=19) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  guides(colour=guide_legend(override.aes=list(shape=c(19, 19, NA, NA), linetype=c("blank", "blank", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_Adj.Close_log_TrnS_TstS_sp)
#
# The line plot
BTC_Adj.Close_log_TrnS_TstS_lp <- ggplot(Data_df, aes(x=x)) +
  geom_vline(xintercept=Data_df$x[TrnS_length], lwd=0.5, color="black") +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", 
              method="lm" , formula=y ~ x, se=FALSE) +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", 
              method="loess", formula=y ~ x, se=FALSE) +
  geom_line(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="point_black"), alpha=1, lwd=0.7, linetype="solid") +
  geom_line(data=subset(Data_df, Data_df$x>x[TrnS_length]), aes(y=y, color="point_b"), alpha=1, lwd=0.7, linetype="solid") +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_Adj.Close_log_TrnS_TstS_lp)
#
# From the inspection of the scatter and line plot of the daily adjusted close price logarithm, we have visual evidence of an increasing 
# trend with some falls, milder than the sharp falls of the daily adjusted close price. Clearly, the lack of evidence for seasonality is 
# confirmed. Comparing the LOESS with the regression line, the overall trend does not appear to be linear. Neither does it appear to be 
# exponential. The spread of the points of the training set around the LOESS seems to be more homogeneous throughout the LOESS path. 
# Uncertain visual evidence for heteroscedasticity.
############################################################################################################################################
# We consider again the data frame containing only the training set data.
BTC_train_df <- BTC_red_df[which(BTC_red_df$Date<as.Date("2023-01-01")),]
head(BTC_train_df)
tail(BTC_train_df)
#
# We consider the autocorrelograms of the daily adjusted close price logarithm training set. Of course, due to the clear trend, we expect a
# strong visual evidence for autocorrelation.
# Autocorrelogram of the training set of the Bitcoin daily adjusted close price logarithm.
Data_df <- BTC_train_df
y <- Data_df$Adj.Close_log
length <- length(y)
T <- length(y)
# max_lag <- ceiling(10*log10(T))    # Default
# max_lag <- ceiling(sqrt(n)+45)     # Box-Jenkins
max_lag <- ceiling(min(10,T/4))      # Hyndman (for data without seasonality)
# max_lag <- ceiling(min(2*12,T/5))  # Hyndman (for data with seasonality, see https://robjHyndman.com/hy_resndsight/ljung-box-test/)
Aut_Fun_y <- stats::acf(y, lag.max=max_lag, type="correlation", plot=FALSE)
# Aut_Fun_y <- TSA::acf(y, lag.max=max_lag, type="correlation", plot=TRUE)
ci_090 <- qnorm((1+0.90)/2)/sqrt(T)
ci_95 <- qnorm((1+0.95)/2)/sqrt(T)
ci_99 <- qnorm((1+0.99)/2)/sqrt(T)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
First_Day <- as.character(Data_df$Date[1])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Autocorrelogram of the Bitcoin Daily Adjusted Close Price Logarithm - Training Set - from ", .(First_Day), " to ", .(Last_Day), sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("path length ", .(length), " sample points,  ", "lags ", .(max_lag), ".  Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("lags")
x_breaks_num <- max_lag
x_binwidth <- 1
x_breaks <- Aut_Fun_y_df$lag
x_labs <- format(x_breaks, scientific=FALSE)
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), lwd=1, col="black") +
  geom_hline(aes(yintercept=-ci_090, color="CI_090"), lwd=0.9, lty=3, show.legend=TRUE) +
  geom_hline(aes(yintercept=ci_090, color="CI_090"), lwd=0.9, lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), lwd=0.8, lty=2, show.legend=TRUE) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lwd=0.8, lty=2) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), lwd=0.8, lty=4, show.legend=TRUE) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lwd=0.8, lty=4) +
  scale_x_continuous(name= "lag", breaks=x_breaks, label=x_labs) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis=sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"), values=c(CI_090="green", CI_95="blue", CI_99="red"),
                     guide=guide_legend(override.aes=list(linetype=c("dotted", "dashed", "dotdash")))) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=0, vjust=1, hjust=0.5),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
#
# The autocorrelogram provides a very strong visual evidence for non-stationarity.
#
# Partial autocorrelogram of the training set of the NASDAQ Composite daily adjusted close prices.
Data_df <- BTC_train_df
y <- Data_df$Adj.Close_log
length <- length(y)
T <- length(y)
# max_lag <- ceiling(10*log10(T))    # Default
# max_lag <- ceiling(sqrt(n)+45)     # Box-Jenkins
max_lag <- ceiling(min(10,T/4))      # Hyndman (for data without seasonality)
# max_lag <- ceiling(min(2*12,T/5))  # Hyndman (for data with seasonality, see https://robjHyndman.com/hy_resndsight/ljung-box-test/)
Part_Aut_Fun_y <- stats::pacf(y, lag.max=max_lag, type="correlation", plot=FALSE)
ci_090 <- qnorm((1+0.90)/2)/sqrt(T)
ci_95 <- qnorm((1+0.95)/2)/sqrt(T)
ci_99 <- qnorm((1+0.99)/2)/sqrt(T)
Plot_Part_Aut_Fun_y <- data.frame(lag=Part_Aut_Fun_y$lag, pacf=Part_Aut_Fun_y$acf)
First_Day <- as.character(Data_df$Date[1])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Partial Autocorrelogram of the Bitcoin Daily Adjusted Close Price Logarithm - Training Set - from ", .(First_Day), " to ", .(Last_Day), sep="")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points,  ", "lags ", .(max_lag), ".  Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("lags")
x_breaks_num <- max_lag
x_binwidth <- 1
x_breaks <- Aut_Fun_y_df$lag
x_labs <- format(x_breaks, scientific=FALSE)
ggplot(Plot_Part_Aut_Fun_y, aes(x=lag, y=pacf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=pacf), size=1, col="black") +
  geom_hline(aes(yintercept=-ci_090, color="CI_090"), lwd=0.9, lty=3, show.legend=TRUE) +
  geom_hline(aes(yintercept=ci_090, color="CI_090"), lwd=0.9, lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), lwd=0.8, lty=2, show.legend=TRUE) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lwd=0.8, lty=2) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), lwd=0.8, lty=4, show.legend=TRUE) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lwd=0.8, lty=4) +
  scale_x_continuous(name= "lag", breaks=x_breaks, label=x_labs) +
  scale_y_continuous(name="pacf value", breaks=waiver(), labels=NULL,
                     sec.axis=sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"), values=c(CI_090="green", CI_95="blue", CI_99="red"),
                     guide=guide_legend(override.aes=list(linetype=c("dotted", "dashed", "dotdash")))) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=0, vjust=1, hjust=0.5),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
#
# The partial autocorrelogram provides visual evidence for a unit root.
#
# Despite not necessary, due to the strong visual evidence from the autocorrelograms, we consider the Ljung-Box test
# Ljung-Box test
Data_df <- BTC_train_df
head(Data_df)
y <- Data_df$Adj.Close_log
T <- length(y)
max_lag <- ceiling(min(10,T/4)) # Hyndman (for data without seasonality, see https://robjHyndman.com/hy_resndsight/ljung-box-test/)
show(max_lag)
# 10
n_pars <- 0
show(n_pars)
# 0
fit_df <- n_pars
y_LB <- Box.test(y, lag=max_lag, fitdf=fit_df, type="Ljung-Box")
show(y_LB)
# Box-Ljung test
# data:  y
# X-squared=17062, df=10, p-value < 2.2e-16
#
FitAR::LjungBoxTest(y, k=n_pars, lag.max=max_lag, StartLag=1, SquaredQ=FALSE)
#  m       Qm pvalue
#  1  1715.50      0
#  2  3428.17      0
#  3  5138.18      0
#  4  6845.14      0
#  5  8548.98      0
#  6 10249.56      0
#  7 11947.26      0
#  8 13642.12      0
#  9 15334.16      0
# 10 17058.69      0
#
n_pars_seq <- rep(NA,max_lag)
for(l in 1:max_lag){
  if(l-n_pars<0) n_pars_seq[l] <- l-1
  else n_pars_seq[l] <- n_pars
}
show(n_pars_seq)
# 0 0 0 0 0 0 0 0 0 0
#
Box_test_ls <- list()
for(l in 1:max_lag){
  Box_test_ls[[l]] <- Box.test(y, lag=l,   fitdf=n_pars_seq[l], type="Ljung-Box")
  show(Box_test_ls[[l]])
}
# Box-Ljung test
# data:  y
# X-squared = 1719, df = 1, p-value < 0.00000000000000022
# X-squared = 3435.5, df = 2, p-value < 0.00000000000000022
# X-squared = 5149.2, df = 3, p-value < 0.00000000000000022
# X-squared = 6860.2, df = 4, p-value < 0.00000000000000022
# X-squared = 8568.2, df = 5, p-value < 0.00000000000000022
# X-squared = 10273, df = 6, p-value < 0.00000000000000022
# X-squared = 11975, df = 7, p-value < 0.00000000000000022
# X-squared = 13673, df = 8, p-value < 0.00000000000000022
# X-squared = 15369, df = 9, p-value < 0.00000000000000022
# X-squared = 17062, df = 10, p-value < 0.00000000000000022
#
# The Ljung-Box test rejects the null hypothesis of no autocorrelation in the Bitcoin daily adjusted close price logarithms at the $1\%$
# significance level.
############################################################################################################################################
# Also in this case, we should check whether the Bitcoin adjusted close price logarithm time series contains a unit root (a random walk 
# component). However, we apply here a more direct two step procedure. First, we evaluate the optimal lag for the ADF test by applying the
# AIC+BIC information criterion. Second, we check the validity of the test by evaluating the autocorrelation of the residuals of the linear
# model used for the ADF test.
head(BTC_train_df)
Data_df <- BTC_train_df
y <- Data_df$Adj.Close_log
length(y)
# 1720
#
sum(is.na(y))
# 0
#
long_lags <- floor(12*(length(y)/100)^(1/4))  # Fixing the maximum number of lags with the Schwert formula
n_obs <- length(y)-(long_lags+1)
show(n_obs)
y_ADF_ur.df_trend_lags_ls <- list()                     # Creating an empty list
y_ADF_ur.df_trend_lags_AIC_vec <- rep(NA,(long_lags+1)) # Creating an empty vector to store AIC for different lags
y_ADF_ur.df_trend_lags_BIC_vec <- rep(NA,(long_lags+1)) # Creating an empty vector to store BIC for different lags
for (l in 0:long_lags){
  y_ADF_ur.df_trend_lags_ls[[l+1]] <- ur.df(y[-c(1:(long_lags-l+1))], type="trend", lags=l, selectlags="Fixed")
  show(y_ADF_ur.df_trend_lags_ls[[l+1]])
  show(nobs(lm(formula=y_ADF_ur.df_trend_lags_ls[[l+1]]@testreg[["terms"]])))
  y_ADF_ur.df_trend_lags_AIC_vec[l+1] <- AIC(lm(formula=y_ADF_ur.df_trend_lags_ls[[l+1]]@testreg[["terms"]]))
  show(y_ADF_ur.df_trend_lags_AIC_vec[l+1])
  y_ADF_ur.df_trend_lags_BIC_vec[l+1] <- BIC(lm(formula=y_ADF_ur.df_trend_lags_ls[[l+1]]@testreg[["terms"]]))
  show(y_ADF_ur.df_trend_lags_BIC_vec[l+1])
}
show(y_ADF_ur.df_trend_lags_AIC_vec)
# -6317.424 -6320.133 -6321.233 -6319.247 -6321.624 -6320.131 -6318.854 -6318.405 -6318.664 -6316.776 -6317.144 -6315.445 -6313.452 -6313.363
# -6311.372 -6309.389 -6308.079 -6307.660 -6305.903 -6304.057 -6302.123 -6302.123 -6300.530 -6301.362 -6301.821
#
show(y_ADF_ur.df_trend_lags_BIC_vec)
# -6295.685 -6292.959 -6288.624 -6281.203 -6278.145 -6271.218 -6264.506 -6258.621 -6253.445 -6246.123 -6241.056 -6233.922 -6226.495 -6220.971
# -6213.544 -6206.127 -6199.382 -6193.528 -6186.336 -6179.055 -6171.686 -6166.252 -6159.224 -6154.621 -6149.645
#
# We draw an "elbow" plot to show the AIC and BIC values as functions of the lags.
def_margins <- par("mar")
par(mfrow=c(2,1), mar=c(2.5,2.5,4.0,1.0))
plot(y_ADF_ur.df_trend_lags_AIC_vec, type="b", pch=19, col=4)
plot(y_ADF_ur.df_trend_lags_BIC_vec, type="b", pch=19, col=4)
par(mfrow=c(1,1), mar=def_margins)
#
# We compute the minimum values of AIC and BIC and show the lag where the minimum values are attained and and the minimum values themselves.
min_AIC_lag <- which(y_ADF_ur.df_trend_lags_AIC_vec==min(y_ADF_ur.df_trend_lags_AIC_vec))
show(c((min_AIC_lag-1),y_ADF_ur.df_trend_lags_AIC_vec[min_AIC_lag]))
# 4.000 -6321.624
#
min_BIC_lag <- which(y_ADF_ur.df_trend_lags_BIC_vec==min(y_ADF_ur.df_trend_lags_BIC_vec))
show(c((min_BIC_lag-1),y_ADF_ur.df_trend_lags_BIC_vec[min_AIC_lag]))
# 0.000 -6278.145
#
# We sort the AIC and BIC values in increasing order.
AIC_lag_sort <- sort(y_ADF_ur.df_trend_lags_AIC_vec, index.return=TRUE, decreasing=FALSE)
show(AIC_lag_sort)
# $x (the sorted values)
# -6321.624 -6321.233 -6320.133 -6320.131 -6319.247 -6318.854 -6318.664 -6318.405 -6317.424 -6317.144 -6316.776 -6315.445 -6313.452 -6313.363
# -6311.372 -6309.389 -6308.079 -6307.660 -6305.903 -6304.057 -6302.123 -6302.123 -6301.821 -6301.362 -6300.530
#
# $ix (the lags of the sorted values)
# 5  3  2  6  4  7  9  8  1 11 10 12 13 14 15 16 17 18 19 20 22 21 25 24 23 
#
BIC_lag_sort <- sort(y_ADF_ur.df_trend_lags_BIC_vec, index.return=TRUE, decreasing=FALSE)
show(BIC_lag_sort)
# $x (the sorted values)
# -6295.685 -6292.959 -6288.624 -6281.203 -6278.145 -6271.218 -6264.506 -6258.621 -6253.445 -6246.123 -6241.056 -6233.922 -6226.495 -6220.971
# -6213.544 -6206.127 -6199.382 -6193.528 -6186.336 -6179.055 -6171.686 -6166.252 -6159.224 -6154.621 -6149.645
# 
# $ix (the lags of the sorted values)
# 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
#
# Since AIC and BIC values contrast somewhat with each other, we try to determine an optimal combination mixing them by summing their 
# positions in the sorted sequences.
AIC_BIC_pos_pnt <- vector(mode="integer", length=(long_lags+1))
for(p in 1:(long_lags+1)){
  AIC_BIC_pos_pnt[p] <- (which(AIC_lag_sort$ix==p)+which(BIC_lag_sort$ix==p))
}
show(AIC_BIC_pos_pnt)
# 10  5  5  9  6 10 13 16 16 21 21 24 26 28 30 32 34 36 38 40 43 43 48 48 48
#
# Then we choose the number of lags which produces the smallest sum.
AIC_BIC_pos_pnt_sort <- sort(AIC_BIC_pos_pnt, index.return=TRUE, decreasing=FALSE)
show(AIC_BIC_pos_pnt_sort)
# $x
# 5  5  6  9 10 10 13 16 16 21 21 24 26 28 30 32 34 36 38 40 43 43 48 48 48
# 
# $ix
# 2  3  5  4  1  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
#
# The "mixed" optimal combination is achieved at p=2 which corresponds to one lag (p=1 corresponds to zero lags).
# Note that in the "elbow" plot at the value p=10 of the lag variable both AIC and BIC paths attain a local minimum (elbow).
# AS a consequence we test test for a unit root applying an ADF test wti one lag.
l <- 1
y_ADF_ur.df_trend_1_lags <- ur.df(y, type="trend", lags=l, selectlags="Fixed")
lm(formula=y_ADF_ur.df_trend_1_lags@testreg[["terms"]])
# Call: lm(formula = y_ADF_ur.df_trend_1_lags@testreg[["terms"]])
# 
# Coefficients:  (Intercept)     z.lag.1           tt   z.diff.lag  
#                 38.635382    -0.002069     0.011412    -0.031146  
nobs(lm(formula=y_ADF_ur.df_trend_1_lags@testreg[["terms"]]))
# 1718
n_obs <- length(y)-(l+1)
show(n_obs)
# 1718
n_coeffs <- nrow(y_ADF_ur.df_trend_1_lags@testreg[["coefficients"]])
show(n_coeffs)
# 4
df.residual(lm(formula=y_ADF_ur.df_trend_1_lags@testreg[["terms"]])) 
# 1714
df_res <- n_obs-n_coeffs
show(df_res)
# 1714
summary(y_ADF_ur.df_trend_1_lags)
############################################### 
# Augmented Dickey-Fuller Test Unit Root Test # 
############################################### 
# Call: lm(formula = z.diff ~ z.lag.1 + 1 + tt + z.diff.lag)
# 
# Residuals: Min      1Q     Median   3Q     Max 
#          -7461.8  -226.0   -19.3   207.6  7311.9 
# 
# Coefficients: Estimate Std. Error t value Pr(>|t|)
# (Intercept) 38.635382  49.680176   0.778    0.437
# z.lag.1     -0.002069   0.001909  -1.084    0.279
# tt           0.011412   0.066550   0.171    0.864
# z.diff.lag  -0.031146   0.024165  -1.289    0.198
# 
# Residual standard error: 1027 on 1714 degrees of freedom
# Multiple R-squared:  0.002026,	Adjusted R-squared:  0.0002797 
# F-statistic:  1.16 on 3 and 1714 DF,  p-value: 0.3236
# 
# Value of test-statistic is: -1.084 0.5808 0.8505 
# Critical values for test statistics: 
#       1pct  5pct 10pct
# tau3 -3.96 -3.41 -3.12
# phi2  6.09  4.68  4.03
# phi3  8.27  6.25  5.34
#
# The null hypothesis cannot be rejected against the three alternatives at the $10\%$ significance level.
# However, to validate the test, we need to check the possible presence of autocorrelation in the residuals of the model used for the test.
y_res <- as.vector(y_ADF_ur.df_trend_1_lags@testreg[["residuals"]])
nobs(lm(formula=y_ADF_ur.df_trend_1_lags@testreg[["terms"]]))
# 1718
max_lag <- ceiling(min(10, n_obs/4))    # Hyndman (for data without seasonality, see https://robjHyndman.com/hy_resndsight/ljung-box-test/)
show(max_lag)
# 10
n_coeffs <- nrow(y_ADF_ur.df_trend_1_lags@testreg[["coefficients"]])
show(n_coeffs)
# 4
n_pars <- n_coeffs
show(n_pars)
# 4
fit_df <- n_pars
LB_fit_df <- min(min(max_lag, n_pars), max_lag-1)
show(LB_fit_df)
# 4
y_res_LB <- Box.test(y_res, lag=max_lag, fitdf=LB_fit_df, type="Ljung-Box")
show(y_res_LB)
# Box-Ljung test
# data:  y_res
# X-squared = 25.312, df = 6, p-value = 0.0002988
#
FitAR::LjungBoxTest(y_res, k=n_pars, lag.max=max_lag, StartLag=1, SquaredQ=FALSE)
#  m    Qm      pvalue
#  1  0.01 0.925826664
#  2  3.24 0.071789574
#  3  3.25 0.071301546
#  4  8.55 0.003456224
#  5  8.97 0.002741267
#  6  9.76 0.007588422
#  7 11.52 0.009205493
#  8 13.40 0.009492413
#  9 13.68 0.017770112
# 10 15.84 0.014633937
#
n_pars_seq <- rep(NA,max_lag)
for(l in 1:max_lag){
  if(l-n_pars<=0) n_pars_seq[l] <- l-1
  else n_pars_seq[l] <- n_pars
}
show(n_pars_seq)
# 0 1 2 4 4 4 4 4 4 4
#
Box_test_ls <- list()
for(l in 1:max_lag){
  Box_test_ls[[l]] <- Box.test(y_res, lag=l,   fitdf=n_pars_seq[l], type="Ljung-Box")
  show(Box_test_ls[[l]])
}
# Box-Ljung test
# data:  y_res
# X-squared = 0.008667, df = 1, p-value = 0.9258
# X-squared = 3.2416,   df = 1, p-value = 0.07179
# X-squared = 3.2528,   df = 1, p-value = 0.0713
# X-squared = 8.5495,   df = 1, p-value = 0.003456
# X-squared = 8.9721,   df = 1, p-value = 0.002741
# X-squared = 9.7623,   df = 2, p-value = 0.007588
# X-squared = 11.524,   df = 3, p-value = 0.009205
# X-squared = 13.397,   df = 4, p-value = 0.009492
# X-squared = 13.681,   df = 5, p-value = 0.01777
# X-squared = 15.841,   df = 6, p-value = 0.01463
#
y_res <- as.vector(y_ADF_ur.df_trend_1_lags@testreg[["residuals"]])
T <- n_obs
# max_lag <- ceiling(10*log10(T))    # Default
# max_lag <- ceiling(sqrt(n)+45)     # Box-Jenkins
max_lag <- ceiling(min(10, T/4))     # Hyndman (for data without seasonality)
# max_lag <- ceiling(min(2*12, T/5)) # Hyndman (for data with seasonality, see https://robjHyndman.com/hy_resndsight/ljung-box-test/)
# Aut_Fun_y <- stats::acf(y, lag.max=max_lag, type="correlation", plot=FALSE)
Aut_Fun_y_res <- TSA::acf(y_res, lag.max=max_lag, type= "correlation", plot=FALSE)
ci_090 <- qnorm((1+0.90)/2)/sqrt(T)
ci_95 <- qnorm((1+0.95)/2)/sqrt(T)
ci_99 <- qnorm((1+0.99)/2)/sqrt(T)
Aut_Fun_y_res <- data.frame(lag=Aut_Fun_y_res$lag, acf=Aut_Fun_y_res$acf)
# First_Date <- paste(Data_df$Month[1],Data_df$y_resear[1])
# Last_Date <- paste(Data_df$Month[T],Data_df$y_resear[T])
First_Date <- as.character(Data_df$Date[1])
Last_Date <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025", 
                             paste("Autocorrelogram of the Residuals of the Linear Model with AIC-BIC Selected One (1) Lag for the ADF Test for the Bitcoin Daily Adjusted Close Logarithm from ", .(First_Date), " to ", .(Last_Date))))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Data set size ",.(TrnS_length)," sample points. Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("lags")
x_breaks_num <- max_lag
x_binwidth <- 1
x_breaks <- Aut_Fun_y_res$lag
x_labs <- format(x_breaks, scientific=FALSE)
Plot_Aut_Fun_y_res <- ggplot(Aut_Fun_y_res, aes(x=lag, y=acf))+
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), linewidth=1, col= "black") +
  # geom_col(mapping=NULL, data=NULL, position= "dodge", width=0.1, col= "black", inherit.aes=TRUE)+
  geom_hline(aes(yintercept=-ci_090, color= "CI_090"), show.legend=TRUE, lwd=0.9, lty=3) +
  geom_hline(aes(yintercept=ci_090, color= "CI_090"), lwd=0.9, lty=3) +
  geom_hline(aes(yintercept=ci_95, color= "CI_95"), show.legend=TRUE, lwd=0.8, lty=2)+
  geom_hline(aes(yintercept=-ci_95, color= "CI_95"), lwd=0.8, lty=2) +
  geom_hline(aes(yintercept=-ci_99, color= "CI_99"), show.legend=TRUE, lwd=0.8, lty=4) +
  geom_hline(aes(yintercept=ci_99, color= "CI_99"), lwd=0.8, lty=4) +
  scale_x_continuous(name= "lag", breaks=x_breaks, label=x_labs) +
  scale_y_continuous(name= "acf value", breaks=waiver(), labels=NULL,
                     sec.axis=sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name= "Conf. Inter.", labels=c("90%","95%","99%"), values=c(CI_090="green", CI_95="blue", CI_99="red"),
                     guide=guide_legend(override.aes=list(linetype=c("dotted", "dashed", "dotdash")))) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
# theme(plot.title=element_blank(), 
#       plot.subtitle=element_blank(),
#       plot.caption=element_text(hjust=1.0),
#       legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(Plot_Aut_Fun_y_res)
#
# From the autocorrelogram we have visual evidence for autocorrelation at the $5\%$ significance level. Not at the $1\%$ significance level,
# though. On the other hand, a plot of the residuals shows a clear visual evidence for heteroscedasticity and advocates the robust tests for 
# autocorrelation.
head(BTC_train_df)
tail(BTC_train_df)
Data_df <- BTC_train_df
head(Data_df)
length(as.vector(y_ADF_ur.df_trend_1_lags@testreg[["residuals"]]))
# 1711
Data_df <- add_column(Data_df, ADF_Long_lag_y_res=c(rep(NA,10),as.vector(y_ADF_ur.df_trend_9_lags@testreg[["residuals"]])), .after="Adj.Close")
head(Data_df)
Data_df <- dplyr::rename(Data_df, x=t, y=ADF_Long_lag_y_res)
TrnS_length <- length(Data_df$Adj.Close)
show(TrnS_length)
# 1720
First_Day <- as.character(Data_df$Date[2])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Residuals of the Linear Model with AIC-BIC Selected One (1) Lag for the ADF Test on the Bitcoin Daily Adjusted Close Price Logarithm - Training Set - from ", .(First_Day), " to ", .(Last_Day), sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points. Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("")
# numbers::primeFactors(TrnS_length)
x_breaks_num <- 40 # (deduced from primeFactors(TrnS_length-2))
x_breaks_low <- Data_df$x[1]
x_breaks_up <- Data_df$x[(TrnS_length)]
x_binwidth <- ceiling((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Date[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("residuals of the linear model")
y_max <- max(na.rm(Data_df$y))
y_min <- min(na.rm(Data_df$y))
as.numeric(floor(y_max-y_min))
y_breaks_num <- 10
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor((y_min/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((y_max/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth), digits=3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.5
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
point_black <- bquote("GARCH(1,1) stand. residuals")
line_green <- bquote("regression line")
line_red <- bquote("LOESS curve")
leg_point_labs <- c(point_black)
leg_point_cols <- c("point_black"="black")
leg_point_breaks <- c("point_black")
leg_line_labs <- c(line_green, line_red)
leg_line_cols <- c("line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_green", "line_red")
leg_col_labs   <- c(leg_point_labs,leg_line_labs)
leg_col_cols   <- c(leg_point_cols,leg_line_cols)
leg_col_breaks <- c(leg_point_breaks,leg_line_breaks)
BTC_ACPL_TrnS_1_Lags_ADF_res_sp <- ggplot(Data_df, aes(x=x)) +
  geom_smooth(aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_point(aes(y=y, color="point_black"), alpha=1, size=0.7, shape=19, na.rm=TRUE) + 
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  guides(colour=guide_legend(override.aes=list(shape=c(19, NA, NA), linetype=c("blank", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_ACPL_TrnS_1_Lags_ADF_res_sp)
#
# The line plot
BTC_ACPL_TrnS_1_Lags_ADF_res_lp <- ggplot(Data_df, aes(x=x)) +
  geom_smooth(aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_line(aes(y=y, color="point_black"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_ACPL_TrnS_1_Lags_ADF_res_lp)
#
y_res <- as.vector(y_ADF_ur.df_trend_1_lags@testreg[["residuals"]])
testcorr::ac.test(y_res, max.lag = 10, alpha = 0.10, lambda = 2.576, plot = TRUE, table = TRUE, var.name = NULL, scale.font = 1)
# Tests for zero autocorrelation of x
#   | Lag|     AC|  Stand. CB(90%)|  Robust CB(90%)| Lag|      t| p-value| t-tilde| p-value| Lag|     LB| p-value| Q-tilde| p-value|
#   |---:|------:|---------------:|---------------:|---:|------:|-------:|-------:|-------:|---:|------:|-------:|-------:|-------:|
#   |   1|  0.002| (-0.040, 0.040)| (-0.051, 0.051)|   1|  0.093|   0.926|   0.072|   0.942|   1|  0.009|   0.926|   0.005|   0.942|
#   |   2|  0.043| (-0.040, 0.040)| (-0.046, 0.046)|   2|  1.796|   0.073|   1.536|   0.125|   2|  3.242|   0.198|   2.365|   0.307|
#   |   3|  0.003| (-0.040, 0.040)| (-0.043, 0.043)|   3|  0.106|   0.916|   0.098|   0.922|   3|  3.253|   0.354|   2.374|   0.498|
#   |   4|  0.055| (-0.040, 0.040)| (-0.058, 0.058)|   4|  2.297|   0.022|   1.585|   0.113|   4|  8.549|   0.073|   4.196|   0.380|
#   |   5|  0.016| (-0.040, 0.040)| (-0.045, 0.045)|   5|  0.649|   0.516|   0.576|   0.564|   5|  8.972|   0.110|   4.529|   0.476|
#   |   6|  0.021| (-0.040, 0.040)| (-0.042, 0.042)|   6|  0.887|   0.375|   0.832|   0.406|   6|  9.762|   0.135|   5.220|   0.516|
#   |   7| -0.032| (-0.040, 0.040)| (-0.068, 0.068)|   7| -1.324|   0.186|  -0.773|   0.439|   7| 11.524|   0.117|   5.818|   0.561|
#   |   8| -0.033| (-0.040, 0.040)| (-0.041, 0.041)|   8| -1.364|   0.172|  -1.319|   0.187|   8| 13.397|   0.099|   7.557|   0.478|
#   |   9|  0.013| (-0.040, 0.040)| (-0.039, 0.039)|   9|  0.531|   0.595|   0.538|   0.591|   9| 13.681|   0.134|   7.846|   0.550|
#   |  10|  0.035| (-0.040, 0.040)| (-0.042, 0.042)|  10|  1.465|   0.143|   1.385|   0.166|  10| 15.841|   0.104|   9.765|   0.461|
#
# The robust autocorrelation test does not reject the null hypothesis of no autocorrelation at the $10\%$ significance level. Therefore,
# the linear model with one lag for the ADF test appears to be a good combination in terms of model parsimony and lack of autocorrelation
# in the residuals.
#
# Now, we consider the *KPSS* test, which assumes the null hypothesis that the time series can be considered a path of an autoregressive 
# process. In terms of the null hypothesis, the test allows us to specify an autoregressive process with drift, type="mu", or an 
# autoregressive process with drift and linear trend, type="tau".
#   
# Focusing on the type "tau", the *KPSS* test contained in the library *urca*, also allows different possibilities for the number of lags.
# More specifically, we have
#
head(Data_df)
y <- Data_df$Adj.Close_log
#
y_KPSS_ur_tau_nil <- ur.kpss(y, type="tau", lags="nil")
summary(y_KPSS_ur_tau_nil)
####################### 
# KPSS Unit Root Test # 
####################### 
# Test is of type: tau with 0 lags. 
# Value of test-statistic is: 15.7356 
# Critical value for a significance level of: 
#                 10pct  5pct 2.5pct  1pct
# critical values 0.119 0.146  0.176 0.216
#
# When the null hypothesis consists of an autoregressive model with drift, linear trend, and 0 lags, we have a rejection of the null 
# hypothesis in favor of the alternative at $1\%$ significance level.
#
y_KPSS_ur_tau_short <- ur.kpss(y, type="tau", lags="short")
summary(y_KPSS_ur_tau_short)
####################### 
# KPSS Unit Root Test # 
####################### 
# 
# Test is of type: tau with 8 lags. 
# Value of test-statistic is: 1.7694  
# Critical value for a significance level of: 
#                 10pct  5pct 2.5pct  1pct
# critical values 0.119 0.146  0.176 0.216
#
# When the null hypothesis consists of an autoregressive model with drift, linear trend, and a small number of lags, we still have a 
# rejection of the null hypothesis in favor of the alternative at $1\%$ significance level, but the rejection is weaker than the case with 
# zero lags.
#
y_KPSS_ur_tau_long <- ur.kpss(y, type="tau", lags="long") # the Schwert formula
summary(y_KPSS_ur_tau_long)
####################### 
# KPSS Unit Root Test # 
####################### 
# Test is of type: tau with 24 lags. 
# Value of test-statistic is: 0.6523 
# Critical value for a significance level of: 
#                 10pct  5pct 2.5pct  1pct
# critical values 0.119 0.146  0.176 0.216
#
# When the null hypothesis consists of an autoregressive model with drift, linear trend, and a large number of lags, we still have a
# rejection of the null hypothesis in favor of the alternative at $1\%$ significance level, but the rejection is weaker than the case with
# a small number of lags.
#
# On increasing the number of lags of the lagged autoregressive model with drift and linear trend considered as the null hypothesis,
# rejecting the null hypothesis in favor of the alternative at $1\%$ significance level becomes weaker but cannot be avoided.
#
# In light of the results of the *ADF* and *KPSS* we have to think of the Bitcoin daily adjusted close price logarithm as a path of a 
# process containing a random walk component.
############################################################################################################################################
############################################################################################################################################
############################################################################################################################################
# Given the above results, we consider the Bitcoin daily adjusted close price differences, the daily logarithm returns, and the daily 
# logarithm return percentage. The latter is usually the standard time series of interest for financial analysts. This is for two
# main reasons: from the economic point of view, it is more convenient to think in terms of percentages, and from the computational point of
# view, computations with too-small values are avoided.
head(BTC_red_df, 3)
#   t       Date    Open    High     Low   Close Adj.Close Adj.Close_log     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09       8.974883 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42       9.007418 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31       9.023325 7063209984
tail(BTC_red_df, 3)
#         t       Date     Open     High      Low    Close Adj.Close Adj.Close_log      Volume
# 1869 1869 2023-05-29 28075.59 28432.04 27563.88 27745.88  27745.88       10.23084 15181308984
# 1870 1870 2023-05-30 27745.12 28044.76 27588.50 27702.35  27702.35       10.22927 13251081851
# 1871 1871 2023-05-31 27700.53 27831.68 26866.45 27219.66  27219.66       10.21169 15656371534
BTC_red_df <- add_column(BTC_red_df, Adj.Close_diff=c(NA,diff(BTC_red_df$Adj.Close, lag=1, difference=1)), .after="Adj.Close")
BTC_red_df <- add_column(BTC_red_df, log_ret=c(NA,diff(BTC_red_df$Adj.Close_log, lag=1, difference=1)), .after="Adj.Close_log")
BTC_red_df <- add_column(BTC_red_df, log_ret_perc=100*BTC_red_df$log_ret, .after="log_ret")
head(BTC_red_df, 3)
#   t       Date    Open    High     Low   Close Adj.Close Adj.Close_diff Adj.Close_log    log_ret log_ret_perc     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09             NA      8.974883         NA           NA 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42       261.3301      9.007418 0.03253593     3.253593 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31       130.8896      9.023325 0.01590650     1.590650 7063209984
tail(BTC_red_df, 3)
#         t       Date     Open     High      Low    Close Adj.Close Adj.Close_diff Adj.Close_log      log_ret log_ret_perc      Volume
# 1869 1869 2023-05-29 28075.59 28432.04 27563.88 27745.88  27745.88     -339.76172      10.23084 -0.012171112   -1.2171112 15181308984
# 1870 1870 2023-05-30 27745.12 28044.76 27588.50 27702.35  27702.35      -43.53516      10.22927 -0.001570299   -0.1570299 13251081851
# 1871 1871 2023-05-31 27700.53 27831.68 26866.45 27219.66  27219.66     -482.69141      10.21169 -0.017577793   -1.7577793 15656371534
############################################################################################################################################
# We consider the scatter plot of the Bitcoin daily adjusted close price differences training set and the test set
Data_df <- BTC_red_df
head(Data_df, 3)
#   t       Date    Open    High     Low   Close Adj.Close Adj.Close_diff Adj.Close_log    log_ret log_ret_perc     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09             NA      8.974883         NA           NA 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42       261.3301      9.007418 0.03253593     3.253593 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31       130.8896      9.023325 0.01590650     1.590650 7063209984
Data_df <- dplyr::rename(Data_df, x=t, y=Adj.Close_diff)
head(Data_df, 3)
#   x       Date    Open    High     Low   Close Adj.Close        y Adj.Close_log    log_ret log_ret_perc     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09       NA      8.974883         NA           NA 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42 261.3301      9.007418 0.03253593     3.253593 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31 130.8896      9.023325 0.01590650     1.590650 7063209984
DS_length <- length(na.rm(Data_df$y))
show(DS_length)
# 1870
TrnS_First_Day <- as.character(Data_df$Date[min(which(!is.na(Data_df$y)))])
show(TrnS_First_Day)
# "2018-04-18"
TrnS_Last_Day <- as.character(Data_df$Date[which(Data_df$Date==as.Date("2023-01-01"))-1])
show(TrnS_Last_Day)
# "2022-12-31"
TrnS_length <- length(na.rm(Data_df$y[which(Data_df$Date<as.Date("2023-01-01"))]))
show(TrnS_length)
# 1719
TstS_length <- DS_length-TrnS_length
show(TstS_length)
# 151
TstS_First_Day <- as.character(Data_df$Date[which(Data_df$Date==as.Date("2023-01-01"))])
show(TstS_First_Day)
# "2023-01-01"
TstS_length <- length(na.rm(Data_df$Date[which(Data_df$Date>=as.Date("2023-01-01"))]))
show(TstS_length)
# 151
TstS_length == DS_length-TrnS_length
# TRUE
First_Day <- as.character(Data_df$Date[1])
show(First_Day)
# "2018-04-17"
Last_Day <- as.character(Data_df$Date[DS_length])
show(Last_Day)
# "2023-05-31"
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Bitcoin Daily Adjusted Close Price Differences - Training and Test Sets - from ", .(First_Day), " to ", .(Last_Day), sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points, from ", .(TrnS_First_Day), " to ", .(TrnS_Last_Day),". Test set length ", .(TstS_length), " sample points, from ", .(TstS_First_Day), " to ", .(Last_Day),". Data by courtesy of Yahoo Finance - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("")
# numbers::primeFactors(length-1)
x_breaks_num <- 34 # (deduced from primeFactors(length-1))
x_breaks_low <- Data_df$x[1]
x_breaks_up <- Data_df$x[DS_length]
x_binwidth <- ceiling((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Date[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("daily adjusted close price differences (US $)")
y_max <- max(na.rm(Data_df$y))
y_min <- min(na.rm(Data_df$y))
as.numeric(floor(y_max-y_min))
y_breaks_num <- 10
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor((y_min/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((y_max/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth), digits=3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.5
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
point_black <- bquote("daily adj. close prices - training set")
point_b <- bquote("daily adj. close prices - test set")
line_green <- bquote("regression line (training set)")
line_red <- bquote("LOESS curve (training set)")
leg_point_labs <- c(point_black, point_b)
leg_point_cols <- c("point_black"="black", "point_b"="blue")
leg_point_breaks <- c("point_black", "point_b")
leg_line_labs <- c(line_green, line_red)
leg_line_cols <- c("line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_green", "line_red")
leg_col_labs   <- c(leg_point_labs,leg_line_labs)
leg_col_cols   <- c(leg_point_cols,leg_line_cols)
leg_col_breaks <- c(leg_point_breaks,leg_line_breaks)
BTC_Adj.Close_diff_TrnS_TstS_sp <- ggplot(Data_df, aes(x=x)) +
  geom_vline(xintercept=Data_df$x[TrnS_length], lwd=0.5, color="black") +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", 
              method="lm" , formula=y ~ x, se=FALSE, na.rm=TRUE) +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", 
              method="loess", formula=y ~ x, se=FALSE, na.rm=TRUE) +
  geom_point(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="point_black"), alpha=1, size=0.6, shape=19, na.rm=TRUE) + 
  geom_point(data=subset(Data_df, Data_df$x>x[TrnS_length]), aes(y=y, color="point_b"), alpha=1, size=0.6, shape=19, na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  guides(colour=guide_legend(override.aes=list(shape=c(19, 19, NA, NA), linetype=c("blank", "blank", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_Adj.Close_diff_TrnS_TstS_sp)
#
# The line plot
BTC_Adj.Close_diff_TrnS_TstS_lp <- ggplot(Data_df, aes(x=x)) +
  geom_vline(xintercept=Data_df$x[TrnS_length], lwd=0.5, color="black") +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", 
              method="lm" , formula=y ~ x, se=FALSE, na.rm=TRUE) +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", 
              method="loess", formula=y ~ x, se=FALSE, na.rm=TRUE) +
  geom_line(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="point_black"), alpha=1, size=0.5, linetype="solid", na.rm=TRUE) +
  geom_line(data=subset(Data_df, Data_df$x>x[TrnS_length]), aes(y=y, color="point_b"), alpha=1, size=0.5, linetype="solid", na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_Adj.Close_diff_TrnS_TstS_lp)
#
# We also consider the scatter plot of the Bitcoin daily logarithm return training and test sets.
Data_df <- BTC_red_df
head(Data_df, 3)
#   t       Date    Open    High     Low   Close Adj.Close Adj.Close_diff Adj.Close_log    log_ret log_ret_perc     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09             NA      8.974883         NA           NA 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42       261.3301      9.007418 0.03253593     3.253593 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31       130.8896      9.023325 0.01590650     1.590650 7063209984
tail(Data_df, 3)
#         t       Date     Open     High      Low    Close Adj.Close Adj.Close_diff Adj.Close_log      log_ret log_ret_perc      Volume
# 1869 1869 2023-05-29 28075.59 28432.04 27563.88 27745.88  27745.88     -339.76172      10.23084 -0.012171112   -1.2171112 15181308984
# 1870 1870 2023-05-30 27745.12 28044.76 27588.50 27702.35  27702.35      -43.53516      10.22927 -0.001570299   -0.1570299 13251081851
# 1871 1871 2023-05-31 27700.53 27831.68 26866.45 27219.66  27219.66     -482.69141      10.21169 -0.017577793   -1.7577793 15656371534
Data_df <- dplyr::rename(Data_df, x=t, y=log_ret)
head(Data_df, 3)
#   x       Date    Open    High     Low   Close Adj.Close Adj.Close_diff Adj.Close_log    log_ret        y     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09             NA      8.974883         NA       NA 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42       261.3301      9.007418 0.03253593 3.253593 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31       130.8896      9.023325 0.01590650 1.590650 7063209984
DS_length <- length(na.rm(Data_df$y))
show(DS_length)
# 1870
TrnS_First_Day <- as.character(Data_df$Date[min(which(!is.na(Data_df$y)))])
show(TrnS_First_Day)
# "2018-04-18"
TrnS_Last_Day <- as.character(Data_df$Date[which(Data_df$Date==as.Date("2023-01-01"))-1])
show(TrnS_Last_Day)
# "2022-12-31"
TrnS_length <- length(na.rm(Data_df$y[which(Data_df$Date<as.Date("2023-01-01"))]))
show(TrnS_length)
# 1719
TstS_length <- DS_length-TrnS_length
show(TstS_length)
# 151
TstS_First_Day <- as.character(Data_df$Date[which(Data_df$Date==as.Date("2023-01-01"))])
show(TstS_First_Day)
# "2023-01-01"
TstS_length <- length(na.rm(Data_df$Date[which(Data_df$Date>=as.Date("2023-01-01"))]))
show(TstS_length)
# 151
TstS_length == DS_length-TrnS_length
# TRUE
First_Day <- as.character(Data_df$Date[1])
show(First_Day)
# "2018-04-17"
Last_Day <- as.character(Data_df$Date[DS_length])
show(Last_Day)
# "2023-05-31"
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Bitcoin Daily Logarithm Returns - Training and Test Sets - from ", .(First_Day), " to ", .(Last_Day), sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points, from ", .(TrnS_First_Day), " to ", .(TrnS_Last_Day),". Test set length ", .(TstS_length), " sample points, from ", .(TstS_First_Day), " to ", .(Last_Day),". Data by courtesy of Yahoo Finance - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("")
# numbers::primeFactors(length-1)
x_breaks_num <- 34 # (deduced from primeFactors(length-1))
x_breaks_low <- Data_df$x[1]
x_breaks_up <- Data_df$x[DS_length]
x_binwidth <- ceiling((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Date[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("daily logarithm returns (US $)")
y_max <- max(na.rm(Data_df$y))
y_min <- min(na.rm(Data_df$y))
as.numeric(floor(y_max-y_min))
y_breaks_num <- 10
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor((y_min/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((y_max/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth), digits=3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.5
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
point_black <- bquote("daily log. ret. (US $) training set")
point_b <- bquote("daily log. ret. (US $) test set")
line_green <- bquote("regression line (training set)")
line_red <- bquote("LOESS curve (training set)")
leg_point_labs <- c(point_black, point_b)
leg_point_cols <- c("point_black"="black", "point_b"="blue")
leg_point_breaks <- c("point_black", "point_b")
leg_line_labs <- c(line_green, line_red)
leg_line_cols <- c("line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_green", "line_red")
leg_col_labs   <- c(leg_point_labs,leg_line_labs)
leg_col_cols   <- c(leg_point_cols,leg_line_cols)
leg_col_breaks <- c(leg_point_breaks,leg_line_breaks)
BTC_log_ret_TrnS_TstS_sp <- ggplot(Data_df, aes(x=x)) +
  geom_vline(xintercept=Data_df$x[TrnS_length], lwd=0.5, color="black") +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", 
              method="lm" , formula=y ~ x, se=FALSE, na.rm=TRUE) +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", 
              method="loess", formula=y ~ x, se=FALSE, na.rm=TRUE) +
  geom_point(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="point_black"), alpha=1, size=0.6, shape=19, na.rm=TRUE) + 
  geom_point(data=subset(Data_df, Data_df$x>x[TrnS_length]), aes(y=y, color="point_b"), alpha=1, size=0.6, shape=19, na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  guides(colour=guide_legend(override.aes=list(shape=c(19, 19, NA, NA), linetype=c("blank", "blank", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_log_ret_TrnS_TstS_sp)
#
# The line plot
BTC_log_ret_TrnS_TstS_lp <- ggplot(Data_df, aes(x=x)) +
  geom_vline(xintercept=Data_df$x[TrnS_length], lwd=0.5, color="black") +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", 
              method="lm" , formula=y ~ x, se=FALSE, na.rm=TRUE) +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", 
              method="loess", formula=y ~ x, se=FALSE, na.rm=TRUE) +
  geom_line(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="point_black"), alpha=1, size=0.5, linetype="solid", na.rm=TRUE) +
  geom_line(data=subset(Data_df, Data_df$x>x[TrnS_length]), aes(y=y, color="point_b"), alpha=1, size=0.5, linetype="solid", na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_log_ret_TrnS_TstS_lp)
#
# In the end, we also draw the scatter and line plot of the Bitcoin daily logarithm return percentages training and test sets.
Data_df <- BTC_red_df
head(Data_df, 3)
#   t       Date    Open    High     Low   Close Adj.Close Adj.Close_diff Adj.Close_log    log_ret log_ret_perc     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09             NA      8.974883         NA           NA 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42       261.3301      9.007418 0.03253593     3.253593 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31       130.8896      9.023325 0.01590650     1.590650 7063209984
tail(Data_df, 3)
#         t       Date     Open     High      Low    Close Adj.Close Adj.Close_diff Adj.Close_log      log_ret log_ret_perc      Volume
# 1869 1869 2023-05-29 28075.59 28432.04 27563.88 27745.88  27745.88     -339.76172      10.23084 -0.012171112   -1.2171112 15181308984
# 1870 1870 2023-05-30 27745.12 28044.76 27588.50 27702.35  27702.35      -43.53516      10.22927 -0.001570299   -0.1570299 13251081851
# 1871 1871 2023-05-31 27700.53 27831.68 26866.45 27219.66  27219.66     -482.69141      10.21169 -0.017577793   -1.7577793 15656371534
Data_df <- dplyr::rename(Data_df, x=t, y=log_ret_perc)
head(Data_df, 3)
#   x       Date    Open    High     Low   Close Adj.Close Adj.Close_diff Adj.Close_log    log_ret        y     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09             NA      8.974883         NA       NA 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42       261.3301      9.007418 0.03253593 3.253593 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31       130.8896      9.023325 0.01590650 1.590650 7063209984
DS_length <- length(na.rm(Data_df$y))
show(DS_length)
# 1870
TrnS_First_Day <- as.character(Data_df$Date[min(which(!is.na(Data_df$y)))])
show(TrnS_First_Day)
# "2018-04-18"
TrnS_Last_Day <- as.character(Data_df$Date[which(Data_df$Date==as.Date("2023-01-01"))-1])
show(TrnS_Last_Day)
# "2022-12-31"
TrnS_length <- length(na.rm(Data_df$y[which(Data_df$Date<as.Date("2023-01-01"))]))
show(TrnS_length)
# 1719
TstS_length <- DS_length-TrnS_length
show(TstS_length)
# 151
TstS_First_Day <- as.character(Data_df$Date[which(Data_df$Date==as.Date("2023-01-01"))])
show(TstS_First_Day)
# "2023-01-01"
TstS_length <- length(na.rm(Data_df$Date[which(Data_df$Date>=as.Date("2023-01-01"))]))
show(TstS_length)
# 151
TstS_length == DS_length-TrnS_length
# TRUE
First_Day <- as.character(Data_df$Date[1])
show(First_Day)
# "2018-04-17"
Last_Day <- as.character(Data_df$Date[DS_length])
show(Last_Day)
# "2023-05-31"
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Bitcoin Daily Logarithm Return Percentage - Training and Test Sets - from ", .(First_Day), " to ", .(Last_Day), sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points, from ", .(TrnS_First_Day), " to ", .(TrnS_Last_Day),". Test set length ", .(TstS_length), " sample points, from ", .(TstS_First_Day), " to ", .(Last_Day),". Data by courtesy of Yahoo Finance - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("")
# numbers::primeFactors(length-1)
x_breaks_num <- 34 # (deduced from primeFactors(length-1))
x_breaks_low <- Data_df$x[1]
x_breaks_up <- Data_df$x[DS_length]
x_binwidth <- ceiling((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Date[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("daily percentage logaarithm returns (US $)")
y_max <- max(na.rm(Data_df$y))
y_min <- min(na.rm(Data_df$y))
as.numeric(floor(y_max-y_min))
y_breaks_num <- 10
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor((y_min/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((y_max/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth), digits=3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.5
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
point_black <- bquote("daily perc. log. ret. - training set")
point_b <- bquote("daily perc. log. ret. - test set")
line_green <- bquote("regression line (training set)")
line_red <- bquote("LOESS curve (training set)")
leg_point_labs <- c(point_black, point_b)
leg_point_cols <- c("point_black"="black", "point_b"="blue")
leg_point_breaks <- c("point_black", "point_b")
leg_line_labs <- c(line_green, line_red)
leg_line_cols <- c("line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_green", "line_red")
leg_col_labs   <- c(leg_point_labs,leg_line_labs)
leg_col_cols   <- c(leg_point_cols,leg_line_cols)
leg_col_breaks <- c(leg_point_breaks,leg_line_breaks)
BTC_log_retperc_TrnS_TstS_sp <- ggplot(Data_df, aes(x=x)) +
  geom_vline(xintercept=Data_df$x[TrnS_length], lwd=0.5, color="black") +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", 
              method="lm" , formula=y ~ x, se=FALSE, na.rm=TRUE) +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", 
              method="loess", formula=y ~ x, se=FALSE, na.rm=TRUE) +
  geom_point(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="point_black"), alpha=1, size=0.6, shape=19, na.rm=TRUE) + 
  geom_point(data=subset(Data_df, Data_df$x>x[TrnS_length]), aes(y=y, color="point_b"), alpha=1, size=0.6, shape=19, na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  guides(colour=guide_legend(override.aes=list(shape=c(19, 19, NA, NA), linetype=c("blank", "blank", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_log_retperc_TrnS_TstS_sp)
#
# The line plot
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Bitcoin Daily Logarithm Return Percentage - Training and Test Sets - from ", .(First_Day), " to ", .(Last_Day), sep="")))
BTC_log_retperc_TrnS_TstS_lp <- ggplot(Data_df, aes(x=x)) +
  geom_vline(xintercept=Data_df$x[TrnS_length], lwd=0.5, color="black") +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", 
              method="lm" , formula=y ~ x, se=FALSE, na.rm=TRUE) +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", 
              method="loess", formula=y ~ x, se=FALSE, na.rm=TRUE) +
  geom_line(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="point_black"), alpha=1, size=0.5, linetype="solid", na.rm=TRUE) +
  geom_line(data=subset(Data_df, Data_df$x>x[TrnS_length]), aes(y=y, color="point_b"), alpha=1, size=0.5, linetype="solid", na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_log_retperc_TrnS_TstS_lp)
############################################################################################################################################
############################################################################################################################################
############################################################################################################################################
# We focus on the analysis of the Bitcoin daily logarithm return percentage.
# We consider again the data frame containing only the training set data.
BTC_train_df <- BTC_red_df[which(BTC_red_df$Date<as.Date("2023-01-01")),]
head(BTC_train_df, 3)
#   t       Date    Open    High     Low   Close Adj.Close Adj.Close_diff Adj.Close_log    log_ret log_ret_perc     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09             NA      8.974883         NA           NA 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42       261.3301      9.007418 0.03253593     3.253593 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31       130.8896      9.023325 0.01590650     1.590650 7063209984
tail(BTC_train_df, 3)
#         t       Date     Open     High      Low    Close Adj.Close Adj.Close_diff Adj.Close_log      log_ret log_ret_perc      Volume
# 1718 1718 2022-12-29 16552.32 16651.76 16508.68 16642.34  16642.34       89.76953      9.719705  0.005408645    0.5408645 14472237479
# 1719 1719 2022-12-30 16641.33 16643.43 16408.47 16602.59  16602.59      -39.75586      9.717314 -0.002391696   -0.2391696 15929162910
# 1720 1720 2022-12-31 16603.67 16628.99 16517.52 16547.50  16547.50      -55.08984      9.713990 -0.003323666   -0.3323666 11239186456
#
# From the scatter and line plot, the stationarity in the mean of the Bitcoin daily logarithm return percentage time series looks pretty
# evident. It also seems to be evident that the time series is conditionally heteroscedastic. However, we check the heteroscedasticity by
# applying the Breusch-Pagan and White test.
# We start with introducing the linear model used for the Breusch-Pagan test.
Data_df <- BTC_train_df
head(Data_df, 3)
#   t       Date    Open    High     Low   Close Adj.Close Adj.Close_diff Adj.Close_log    log_ret log_ret_perc     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09             NA      8.974883         NA           NA 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42       261.3301      9.007418 0.03253593     3.253593 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31       130.8896      9.023325 0.01590650     1.590650 7063209984
y <- na.rm(Data_df$log_ret_perc)
x <- 1:length(y)
BP_lm <- lm(y~x)
BP_lm_res <- BP_lm[["residuals"]]
#
# We consider the possible heteroscedasticity of the residuals in the linear model used for the Breusch-Pagan test.
def_margins <- par("mar")
par(mfrow=c(2,1), mar=c(2.5,2.5,4.0,1.0))
plot(BP_lm,1)
plot(BP_lm,3)
par(mfrow=c(1,1), mar=def_margins)
#
# From the Residuals vs Fitted plot, we do not have visual evidence for heteroscedasticity in the residuals. The LOESS curve appears to be 
# flat and the spread of the residuals around the LOESS curve appears to be rather homogeneous. The visual evidence from the Scale-Location
# plot essentially confirms the visual evidence from the Residual vs Fitted plot: an almost flat horizontal LOESS curve (but not as flat as
# the LOESS of the Residuals vs Fitted plot) suggests the absence of non linear forms of heteroscedasticity in the residual time series. 
# However, from the Scale-Location plot we have also visual evidence for conditional heteroscedasticity: the points with similar spread 
# around the LOESS curve appears often very close to each other.
# 
# We check the kurtosis of the residuals in the linear model used for the Breusch-Pagan test.
# library(DescTools)
DescTools::Kurt(BP_lm_res, weights=NULL, method=2, conf.level=0.99, ci.type="classic") 
#   kurtosis    lwr.ci    upr.ci
#  16.0928885 -0.3039169  0.3039169 
#
# The estimated value of the excess kurtosis of the standardized residuals of the estimated GARCH(1,1) model severely conflicts with a 
# possible Gaussian distribution of the residuals at the $1\%$ significance level. We proceed with other non-parametric tests
set.seed(12345)
DescTools::Kurt(BP_lm_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.95, ci.type="norm", R=5000) 
#   kurt      lwr.ci    upr.ci 
# 16.092889 -1.491741 37.082153 
#
set.seed(12345)
DescTools::Kurt(BP_lm_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.95, ci.type="basic", R=5000) 
#   kurt      lwr.ci    upr.ci 
# 16.092889 -1.030256 29.549973 
#
set.seed(12345)
DescTools::Kurt(BP_lm_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type="perc", R=5000) 
#   kurt      lwr.ci    upr.ci 
# 16.092889  2.353563 37.370565 
# 
set.seed(12345)
DescTools::Kurt(BP_lm_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type="bca", R=5000) 
#   kurt      lwr.ci    upr.ci 
# 16.092889  2.765649 44.163968 
# Warning message: In norm.inter(t, adj.alpha) : extreme order statistics used as endpoints
#
set.seed(23451)
DescTools::Kurt(BP_lm_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type="bca", R=50000)
#   kurt     lwr.ci   upr.ci 
# 16.09289  2.79176 46.63326  
# Warning message: In norm.inter(t, adj.alpha) : extreme order statistics used as endpoints
#
set.seed(12345)
DescTools::Kurt(BP_lm_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.95, ci.type="bca", R=5000) 
#   kurt      lwr.ci    upr.ci 
# 16.092889  2.927865 40.981653
#
# The bootstrapped confidence intervals of type "norm" and "basic" at the $95\%$ [resp. $99\%$] confidence level does contain zero. Hence, 
# we cannot reject the null hypothesis of mesokurtic residuals in the linear model for the Breusch-Pagan test at the $5\%$ significance 
# level. On the other hand, the $99\%$ bootstrapped confidence intervals of type "perc" and "bca" do not contain zero (the $99\%$ confidence
# intervals of type "bca" use extreme order statistics as endpoints, though) and so does the $95\%$ bootstrapped confidence interval of type
# "bca". Therefore, referring to these confidence intervals, we must reject the null hypothesis of mesokurtic residuals in the linear model 
# for the Breusch-Pagan test at least the $5\%$ significance level. 
# In light of this, we execute the Breusch-Pagan and White test in the Koenker (studentised) modification.
#
# library(lmtest)
lmtest::bptest(BP_lm, varformula=NULL, studentize=TRUE, data=NULL)
# studentized Breusch-Pagan test
# data:  BP_lm
# BP = 0.018308, df = 1, p-value = 0.8924
#
# library(skedastic)
skedastic::breusch_pagan(BP_lm, koenker=TRUE)
# statistic  p.value  parameter       method         alternative
#   0.0183    0.892      1    Koenker (studentised)    greater     
#
# library(olsrr)
olsrr::ols_test_score(BP_lm, fitted_values=TRUE, rhs=FALSE)
# Score Test for Heteroskedasticity
# ---------------------------------
#   Ho: Variance is homogenous
#   Ha: Variance is not homogenous
# 
# Variables: fitted values of y 
# Test Summary          
# -----------------------------
# DF           =   1 
# Chi2         =   0.01830807 
# Prob > Chi2  =   0.8923689 
#
# Confirming the visual evidence from the Residuals vs Fitted plot, we cannot reject the null of homoscedasticity at the %10\%$ significance
# level. 
# We consider the White test.
#
Data_df <- BTC_train_df
head(Data_df, 3)
#   t       Date    Open    High     Low   Close Adj.Close Adj.Close_diff Adj.Close_log    log_ret log_ret_perc     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09             NA      8.974883         NA           NA 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42       261.3301      9.007418 0.03253593     3.253593 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31       130.8896      9.023325 0.01590650     1.590650 7063209984
tail(Data_df, 3)
#         t       Date     Open     High      Low    Close Adj.Close Adj.Close_diff Adj.Close_log      log_ret log_ret_perc      Volume
# 1718 1718 2022-12-29 16552.32 16651.76 16508.68 16642.34  16642.34       89.76953      9.719705  0.005408645    0.5408645 14472237479
# 1719 1719 2022-12-30 16641.33 16643.43 16408.47 16602.59  16602.59      -39.75586      9.717314 -0.002391696   -0.2391696 15929162910
# 1720 1720 2022-12-31 16603.67 16628.99 16517.52 16547.50  16547.50      -55.08984      9.713990 -0.003323666   -0.3323666 11239186456
y <- na.rm(Data_df$log_ret_perc)
x <- 1:length(y)
W_lm <- lm(y~x+I(x^2))
#
lmtest::bptest(BP_lm, W_lm, studentize=TRUE, data=NULL)
# studentized Breusch-Pagan (White) test
# data: BP_lm, W_lm
# BP = 3.314, df = 2, p-value = 0.1907
#
#
lmtest::bptest(y~x, y~x+I(x^2), studentize=TRUE, data=NULL)
# studentized Breusch-Pagan (White) test
# data:  y ~ x
# BP = 3.314, df = 2, p-value = 0.1907
#
#
skedastic::white(BP_lm, interactions=FALSE, statonly=FALSE)
# statistic  p.value  parameter       method    alternative
#    3.31     0.191      2         White's Test   greater    
#
# library(whitestrap)
whitestrap::white_test(BP_lm)
# White's test results
# Null hypothesis: Homoskedasticity of the residuals
# Alternative hypothesis: Heteroskedasticity of the residuals
# Test Statistic: 3.31
# P-value: 0.190708
#
# The White test cannot reject the null of homoscedasticity at the $10\%$ significance level.
#
# Autocorrelogram of the training set of the Bitcoin daily logarithm return percentage.
Data_df <- BTC_train_df
y <- na.rm(Data_df$log_ret_perc)
length <- length(y)
T <- length(y)
# max_lag <- ceiling(10*log10(T))    # Default
# max_lag <- ceiling(sqrt(n)+45)     # Box-Jenkins
max_lag <- ceiling(min(10,T/4))      # Hyndman (for data without seasonality)
# max_lag <- ceiling(min(2*12,T/5))  # Hyndman (for data with seasonality, see https://robjHyndman.com/hy_resndsight/ljung-box-test/)
# Aut_Fun_y <- stats::acf(y, lag.max=max_lag, type="correlation", plot=FALSE)
Aut_Fun_y <- TSA::acf(y, lag.max=max_lag, type="correlation", plot=TRUE)
ci_090 <- qnorm((1+0.90)/2)/sqrt(T)
ci_95 <- qnorm((1+0.95)/2)/sqrt(T)
ci_99 <- qnorm((1+0.99)/2)/sqrt(T)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
First_Day <- as.character(Data_df$Date[1])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Autocorrelogram of the Bitcoin Daily Logarithm Return Percentage - Training Set - from ", .(First_Day), " to ", .(Last_Day), sep="")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points,  ", "lags ", .(max_lag), ".  Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("lags")
x_breaks_num <- max_lag
x_binwidth <- 1
x_breaks <- Aut_Fun_y_df$lag
x_labs <- format(x_breaks, scientific=FALSE)
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), lwd=1, col="black") +
  geom_hline(aes(yintercept=-ci_090, color="CI_090"), lwd=0.9, lty=3, show.legend=TRUE) +
  geom_hline(aes(yintercept=ci_090, color="CI_090"), lwd=0.9, lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), lwd=0.8, lty=2, show.legend=TRUE) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lwd=0.8, lty=2) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), lwd=0.8, lty=4, show.legend=TRUE) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lwd=0.8, lty=4) +
  scale_x_continuous(name= "lag", breaks=x_breaks, label=x_labs) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis=sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"), values=c(CI_090="green", CI_95="blue", CI_99="red"),
                     guide=guide_legend(override.aes=list(linetype=c("dotted", "dashed", "dotdash")))) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=0, vjust=1, hjust=0.5),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
#
# The autocorrelogram provides visual evidence for autocorrelation at the $5\%$ significance level, but no evidence for autocorrelation 
# at the $1\%$ significance level.
#
# Partial autocorrelogram of the training set of the Bitcoin daily logarithm return percentage.
Data_df <- BTC_train_df
y <- na.rm(Data_df$log_ret_perc)
length <- length(y)
T <- length(y)
# max_lag <- ceiling(10*log10(T))    # Default
# max_lag <- ceiling(sqrt(n)+45)     # Box-Jenkins
max_lag <- ceiling(min(10,T/4))      # Hyndman (for data without seasonality)
# max_lag <- ceiling(min(2*12,T/5))  # Hyndman (for data with seasonality, see https://robjHyndman.com/hy_resndsight/ljung-box-test/)
Part_Aut_Fun_y <- stats::pacf(y, lag.max=max_lag, type="correlation", plot=FALSE)
ci_090 <- qnorm((1+0.90)/2)/sqrt(T)
ci_95 <- qnorm((1+0.95)/2)/sqrt(T)
ci_99 <- qnorm((1+0.99)/2)/sqrt(T)
Plot_Part_Aut_Fun_y <- data.frame(lag=Part_Aut_Fun_y$lag, pacf=Part_Aut_Fun_y$acf)
First_Day <- as.character(Data_df$Date[1])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Partial Autocorrelogram of the Bitcoin Daily Logarithm Return Percentage - Training Set - from ", .(First_Day), " to ", .(Last_Day), sep="")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points,  ", "lags ", .(max_lag), ".  Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("lags")
x_breaks_num <- max_lag
x_binwidth <- 1
x_breaks <- Aut_Fun_y_df$lag
x_labs <- format(x_breaks, scientific=FALSE)
ggplot(Plot_Part_Aut_Fun_y, aes(x=lag, y=pacf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=pacf), size=1, col="black") +
  geom_hline(aes(yintercept=-ci_090, color="CI_090"), lwd=0.9, lty=3, show.legend=TRUE) +
  geom_hline(aes(yintercept=ci_090, color="CI_090"), lwd=0.9, lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), lwd=0.8, lty=2, show.legend=TRUE) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lwd=0.8, lty=2) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), lwd=0.8, lty=4, show.legend=TRUE) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lwd=0.8, lty=4) +
  scale_x_continuous(name= "lag", breaks=x_breaks, label=x_labs) +
  scale_y_continuous(name="pacf value", breaks=waiver(), labels=NULL,
                     sec.axis=sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"), values=c(CI_090="green", CI_95="blue", CI_99="red"),
                     guide=guide_legend(override.aes=list(linetype=c("dotted", "dashed", "dotdash")))) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=0, vjust=1, hjust=0.5),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
#
# The partial autocorrelogram also provides visual evidence for autocorrelation at the $5\%$ significance level, but no evidence for 
# autocorrelation at the $1\%$ significance level.
#
# We consider the Ljung-Box test
# Ljung-Box test
Data_df <- BTC_train_df
y <- na.rm(Data_df$log_ret_perc)
T <- length(y)
show(T)
max_lag <- ceiling(min(10,T/4)) # Hyndman - https://robjhyndman.com/hyndsight/ljung-box-test/
show(max_lag)
# 10
n_pars <- 0
show(n_pars)
# 0
fit_df <- n_pars
#
detach("package:TSA", unload=TRUE)
FitAR::LjungBoxTest(y, k=n_pars, lag.max=max_lag, StartLag=1, SquaredQ=FALSE)
#  m    Qm      pvalue
#  1  5.54 0.018582786
#  2  9.27 0.009722311
#  3  9.29 0.025663343
#  4 14.28 0.006455679
#  5 14.48 0.012853974
#  6 15.38 0.017526176
#  7 17.28 0.015668649
#  8 19.02 0.014738216
#  9 19.25 0.023154683
# 10 21.23 0.019559448
#
lag_seq <- seq(from=1, to=max_lag, by=1)
for(l in lag_seq){
show(Box.test(y, lag=lag_seq[l], fitdf=fit_df, type="Ljung-Box"))
}
# Box-Ljung test
# data:  y
# X-squared = 5.5404, df =  1, p-value = 0.01858
# X-squared = 9.2667, df =  2, p-value = 0.009722
# X-squared = 9.2909, df =  3, p-value = 0.02566
# X-squared = 14.279, df =  4, p-value = 0.006456
# X-squared = 14.476, df =  5, p-value = 0.01285
# X-squared = 15.376, df =  6, p-value = 0.01753
# X-squared = 17.281, df =  7, p-value = 0.01567
# X-squared = 19.023, df =  8, p-value = 0.01474
# X-squared = 19.249, df =  9, p-value = 0.02315
# X-squared = 21.228, df = 10, p-value = 0.01956
#
# WE also consider the Breusch-Godfrey test applied on the fictitious linear regression.
lmtest::bgtest(lm(y~1), order=10, type="Chisq")
# Breusch-Godfrey test for serial correlation of order up to 10
# data:  lm(y ~ 1)
# LM test = 21.409, df = 10, p-value = 0.01841
#
# Note that the Breusch-Godfrey test statistic and p-value are very close to the Ljung-Box statistic and p-value.
#
# The Ljung-Box and  Breusch-Godfrey tests confirm the rejection of the null hypothesis of no autocorrelation in the Bitcoin daily logarithm 
# return percentage at the $5\%$ significance level and the non-rejection of the null hypothesis at the the $1\%$ significance level. In this
# case, since there are zero degrees of freedom fitted by the model, the robust version of the autocorrelation test can be fully considered 
# in both the confidence bands determination and the computational results. 
testcorr::ac.test(y, max.lag = 10, alpha = 0.05, lambda = 2.576, plot = TRUE, table = TRUE, var.name = NULL, scale.font = 1)
# Tests for zero autocorrelation of x
#   | Lag|     AC|  Stand. CB(95%)|  Robust CB(95%)| Lag|      t| p-value| t-tilde| p-value| Lag|     LB| p-value| Q-tilde| p-value|
#   |---:|------:|---------------:|---------------:|---:|------:|-------:|-------:|-------:|---:|------:|-------:|-------:|-------:|
#   |   1| -0.057| (-0.047, 0.047)| (-0.067, 0.067)|   1| -2.352|   0.019|  -1.653|   0.098|   1|  5.540|   0.019|   2.733|   0.098|
#   |   2|  0.047| (-0.047, 0.047)| (-0.057, 0.057)|   2|  1.928|   0.054|   1.610|   0.107|   2|  9.267|   0.010|   5.326|   0.070|
#   |   3| -0.004| (-0.047, 0.047)| (-0.051, 0.051)|   3| -0.155|   0.877|  -0.143|   0.886|   3|  9.291|   0.026|   5.347|   0.148|
#   |   4|  0.054| (-0.047, 0.047)| (-0.068, 0.068)|   4|  2.230|   0.026|   1.546|   0.122|   4| 14.279|   0.006|   7.071|   0.132|
#   |   5|  0.011| (-0.047, 0.047)| (-0.054, 0.054)|   5|  0.443|   0.658|   0.389|   0.697|   5| 14.476|   0.013|   7.222|   0.205|
#   |   6|  0.023| (-0.047, 0.047)| (-0.052, 0.052)|   6|  0.947|   0.344|   0.866|   0.386|   6| 15.376|   0.018|   7.973|   0.240|
#   |   7| -0.033| (-0.047, 0.047)| (-0.081, 0.081)|   7| -1.377|   0.169|  -0.800|   0.424|   7| 17.281|   0.016|   8.613|   0.282|
#   |   8| -0.032| (-0.047, 0.047)| (-0.049, 0.049)|   8| -1.316|   0.188|  -1.273|   0.203|   8| 19.023|   0.015|  10.235|   0.249|
#   |   9|  0.011| (-0.047, 0.047)| (-0.047, 0.047)|   9|  0.474|   0.635|   0.478|   0.632|   9| 19.249|   0.023|  10.463|   0.314|
#   |  10|  0.034| (-0.047, 0.047)| (-0.050, 0.050)|  10|  1.402|   0.161|   1.327|   0.185|  10| 21.228|   0.020|  12.223|   0.270|
#
# From the robust autocorrelation tests, we obtain the non-rejection of the null hypothesis of no autocorrelation in the Bitcoin daily
# logarithm return percentage at the $5\%$ significance level.
############################################################################################################################################
# We consider also the mean of the logarithm return percentage
Data_df <- BTC_train_df
head(Data_df, 3)
#   t       Date    Open    High     Low   Close Adj.Close Adj.Close_diff Adj.Close_log    log_ret log_ret_perc     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09             NA      8.974883         NA           NA 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42       261.3301      9.007418 0.03253593     3.253593 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31       130.8896      9.023325 0.01590650     1.590650 7063209984
y <- na.rm(Data_df$log_ret_perc)
mean(y)
# 0.04299637
# We wonder whether the mean is significantly different from zero. To this we determine the bootstrapped confidence intervals.
Boot_Data_df <- data.frame(k=1:length(y), y=y)
head(Boot_Data_df,3)
#   k        y
# 1 1 3.253593
# 2 2 1.590650
# 3 3 6.437648
boot_mean <- function(Boot_Data_df, k){
  d <- Boot_Data_df[k,]
  return(mean(d$y))
}
boot_mean(Boot_Data_df)
# 0.04299637
# turn off set.seed() if you want the results to vary
# library(boot)
set.seed(12345)
booted_mean <- boot::boot(Boot_Data_df, boot_mean, R=5000)
class(booted_mean)
# [1] "boot"
show(booted_mean)
# ORDINARY NONPARAMETRIC BOOTSTRAP
# Call: boot(data=d, statistic=boot_mean, R=5000)
# Bootstrap Statistics:   original         bias  std. error
#                   t1* 0.04299637 -0.002449327  0.09017207
#
summary(booted_mean)
#      R  riginal      bootBias      bootSE   bootMed
# 1 5000 0.042996    -0.0024493    0.090172   0.03956
#
mean(booted_mean$t) - booted_mean$t0
# -0.002449327
sd(booted_mean$t)
# 0.09017207
plot(booted_mean)
#
booted_mean.ci <- boot::boot.ci(boot.out=booted_mean, conf=0.80, type=c("norm", "basic", "perc", "bca"))
show(booted_mean.ci)
# 
# BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
# Based on 5000 bootstrap replicates
# CALL:  boot.ci(boot.out=booted_mean, conf=0.8, type=c("norm", "basic", "perc", "bca"))
# Intervals: Level        Normal                Basic            Percentile               BCa        
#             80%   (-0.0701,  0.1610)   (-0.0722,  0.1609)  (-0.0749,  0.1582)   (-0.0695,  0.1631)
# Calculations and Intervals on Original Scale
#
# In light of the results of the t-test and the bootstrap confidence intervals, we cannot reject the null hypothesis that the true value
# of the mean of logarithm returns is zero at the $20\%$ significance level.
#
# We consider the Ljung-Box test for the daily squared logarithm return percentage. In the packages *FitAR* and *portes*, the tests can be
# executed on the logarithm return percentage themselves, just selecting the option *SquaredQ=TRUE* or *sqrd.res=TRUE*, respectively.
library(FitAR)
FitAR::LjungBoxTest(y, k=n_pars, lag.max=max_lag, StartLag=1, SquaredQ=TRUE)
#  m    Qm        pvalue
#  1  5.56 0.01842002967
#  2  6.55 0.03783450484
#  3  6.71 0.08170002948
#  4 12.89 0.01184228888
#  5 13.34 0.02035306778
#  6 13.54 0.03518761614
#  7 34.01 0.00001716910
#  8 34.04 0.00004003634
#  9 34.04 0.00008804084
# 10 34.11 0.00017668790
#
lag_seq <- seq(from=1, to=max_lag, by=1)
portes::LjungBox(y, lags=lag_seq, fitdf=n_pars, sqrd.res=TRUE)
# lags statistic df       p-value
#  1  5.629137   1 0.01766432157
#  2  6.597931   2 0.03692133588
#  3  6.755870   3 0.08009978681
#  4 12.850575   4 0.01202946560
#  5 13.309591   5 0.02064401126
#  6 13.515184   6 0.03554644705
#  7 34.256082   7 0.00001542418
#  8 34.285680   8 0.00003605853
#  9 34.286614   9 0.00007955674
# 10 34.362220  10 0.00016033337
#
# Somewhat surprisingly, there are small differences in the values of the statistics.
# We execute the Ljung-Box test in the stats package. In this case we have to consider the squared residual.
for(l in 1:max_lag){
  show(stats::Box.test(y^2, lag=l,   fitdf=n_pars, type="Ljung-Box"))
}
# Box-Ljung test
# data:  y^2
# X-squared=5.6291,  df= 1, p-value=0.01766
# X-squared=6.5979,  df= 2, p-value=0.03692
# X-squared=6.7559,  df= 3, p-value=0.0801
# X-squared=12.851,  df= 4, p-value=0.01203
# X-squared=13.31,   df= 5, p-value=0.02064
# X-squared=13.515,  df= 6, p-value=0.03555
# X-squared=34.256,  df= 7, p-value=0.00001542
# X-squared=34.286,  df= 8, p-value=0.00003606
# X-squared=34.287,  df= 9, p-value=0.00007956
# X-squared=34.362,  df=10, p-value=0.0001603
#
# Autocorrelogram of the training set of the Bitcoin daily squared logarithm return percentage.
Data_df <- BTC_train_df
y <- na.rm(Data_df$log_ret_perc)
y <- y^2
length <- length(y)
T <- length(y)
# max_lag <- ceiling(10*log10(T))    # Default
# max_lag <- ceiling(sqrt(n)+45)     # Box-Jenkins
max_lag <- ceiling(min(10,T/4))      # Hyndman (for data without seasonality)
# max_lag <- ceiling(min(2*12,T/5))  # Hyndman (for data with seasonality, see https://robjHyndman.com/hy_resndsight/ljung-box-test/)
# Aut_Fun_y <- stats::acf(y, lag.max=max_lag, type="correlation", plot=FALSE)
Aut_Fun_y <- TSA::acf(y, lag.max=max_lag, type="correlation", plot=TRUE)
ci_090 <- qnorm((1+0.90)/2)/sqrt(T)
ci_95 <- qnorm((1+0.95)/2)/sqrt(T)
ci_99 <- qnorm((1+0.99)/2)/sqrt(T)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
First_Day <- as.character(Data_df$Date[1])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Autocorrelogram of the Bitcoin Daily Squared Logarithm Return Percentage - Training Set - from ", .(First_Day), " to ", .(Last_Day), sep="")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points,  ", "lags ", .(max_lag), ".  Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("lags")
x_breaks_num <- max_lag
x_binwidth <- 1
x_breaks <- Aut_Fun_y_df$lag
x_labs <- format(x_breaks, scientific=FALSE)
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), lwd=1, col="black") +
  geom_hline(aes(yintercept=-ci_090, color="CI_090"), lwd=0.9, lty=3, show.legend=TRUE) +
  geom_hline(aes(yintercept=ci_090, color="CI_090"), lwd=0.9, lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), lwd=0.8, lty=2, show.legend=TRUE) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lwd=0.8, lty=2) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), lwd=0.8, lty=4, show.legend=TRUE) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lwd=0.8, lty=4) +
  scale_x_continuous(name= "lag", breaks=x_breaks, label=x_labs) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis=sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"), values=c(CI_090="green", CI_95="blue", CI_99="red"),
                     guide=guide_legend(override.aes=list(linetype=c("dotted", "dashed", "dotdash")))) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=0, vjust=1, hjust=0.5),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
#
# The autocorrelogram provides visual evidence for autocorrelation at the $1\%$ significance level.
#
# Partial autocorrelogram of the training set of the Bitcoin daily squared logarithm return percentage.
Data_df <- BTC_train_df
y <- na.rm(Data_df$log_ret_perc)
y <- y^2
length <- length(y)
T <- length(y)
# max_lag <- ceiling(10*log10(T))    # Default
# max_lag <- ceiling(sqrt(n)+45)     # Box-Jenkins
max_lag <- ceiling(min(10,T/4))      # Hyndman (for data without seasonality)
# max_lag <- ceiling(min(2*12,T/5))  # Hyndman (for data with seasonality, see https://robjHyndman.com/hy_resndsight/ljung-box-test/)
Part_Aut_Fun_y <- stats::pacf(y, lag.max=max_lag, type="correlation", plot=FALSE)
ci_090 <- qnorm((1+0.90)/2)/sqrt(T)
ci_95 <- qnorm((1+0.95)/2)/sqrt(T)
ci_99 <- qnorm((1+0.99)/2)/sqrt(T)
Plot_Part_Aut_Fun_y <- data.frame(lag=Part_Aut_Fun_y$lag, pacf=Part_Aut_Fun_y$acf)
First_Day <- as.character(Data_df$Date[1])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Partial Autocorrelogram of the Bitcoin Daily Squared Logarithm Return Percentage - Training Set - from ", .(First_Day), " to ", .(Last_Day), sep="")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points,  ", "lags ", .(max_lag), ".  Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("lags")
x_breaks_num <- max_lag
x_binwidth <- 1
x_breaks <- Aut_Fun_y_df$lag
x_labs <- format(x_breaks, scientific=FALSE)
ggplot(Plot_Part_Aut_Fun_y, aes(x=lag, y=pacf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=pacf), size=1, col="black") +
  geom_hline(aes(yintercept=-ci_090, color="CI_090"), lwd=0.9, lty=3, show.legend=TRUE) +
  geom_hline(aes(yintercept=ci_090, color="CI_090"), lwd=0.9, lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), lwd=0.8, lty=2, show.legend=TRUE) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lwd=0.8, lty=2) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), lwd=0.8, lty=4, show.legend=TRUE) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lwd=0.8, lty=4) +
  scale_x_continuous(name= "lag", breaks=x_breaks, label=x_labs) +
  scale_y_continuous(name="pacf value", breaks=waiver(), labels=NULL,
                     sec.axis=sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"), values=c(CI_090="green", CI_95="blue", CI_99="red"),
                     guide=guide_legend(override.aes=list(linetype=c("dotted", "dashed", "dotdash")))) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=0, vjust=1, hjust=0.5),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
#
# The partial autocorrelogram also provides visual evidence for autocorrelation at the $1\%$ significance level.
# We plot the daily squared logarithm return percentage.
Data_df <- BTC_red_df
head(Data_df)
tail(Data_df)
Data_df <- add_column(Data_df, sqrd.log_ret_perc=Data_df$log_ret_perc^2, .after="log_ret_perc")
head(Data_df)
Data_df <- dplyr::rename(Data_df, x=t, y=sqrd.log_ret_perc)
head(Data_df)
DS_length <- length(na.rm(Data_df$y))
show(DS_length)
# 1870
TrnS_First_Day <- as.character(Data_df$Date[min(which(!is.na(Data_df$y)))])
show(TrnS_First_Day)
# "2018-04-18"
TrnS_Last_Day <- as.character(Data_df$Date[which(Data_df$Date==as.Date("2023-01-01"))-1])
show(TrnS_Last_Day)
# "2022-12-31"
TrnS_length <- length(na.rm(Data_df$y[which(Data_df$Date<as.Date("2023-01-01"))]))
show(TrnS_length)
# 1719
TstS_length <- DS_length-TrnS_length
show(TstS_length)
# 151
TstS_First_Day <- as.character(Data_df$Date[which(Data_df$Date==as.Date("2023-01-01"))])
show(TstS_First_Day)
# "2023-01-01"
TstS_length <- length(na.rm(Data_df$Date[which(Data_df$Date>=as.Date("2023-01-01"))]))
show(TstS_length)
# 151
TstS_length == DS_length-TrnS_length
# TRUE
First_Day <- as.character(Data_df$Date[1])
show(First_Day)
# "2018-04-17"
Last_Day <- as.character(Data_df$Date[DS_length])
show(Last_Day)
# "2023-05-31"
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Bitcoin Daily Squared Logarithm Return Percentage - Training and Test Sets - from ", .(First_Day), " to ", .(Last_Day), sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points, from ", .(TrnS_First_Day), " to ", .(TrnS_Last_Day),". Test set length ", .(TstS_length), " sample points, from ", .(TstS_First_Day), " to ", .(Last_Day),". Data by courtesy of Yahoo Finance - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("")
# numbers::primeFactors(length-1)
x_breaks_num <- 34 # (deduced from primeFactors(length-1))
x_breaks_low <- Data_df$x[1]
x_breaks_up <- Data_df$x[DS_length]
x_binwidth <- ceiling((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Date[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("daily squared logarithm return percentage")
y_max <- max(na.rm(Data_df$y))
y_min <- min(na.rm(Data_df$y))
as.numeric(floor(y_max-y_min))
y_breaks_num <- 10
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor((y_min/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((y_max/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth), digits=3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.0
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
point_black <- bquote("daily perc. log. ret. - training set")
point_b <- bquote("daily perc. log. ret. - test set")
line_green <- bquote("regression line (training set)")
line_red <- bquote("LOESS curve (training set)")
leg_point_labs <- c(point_black, point_b)
leg_point_cols <- c("point_black"="black", "point_b"="blue")
leg_point_breaks <- c("point_black", "point_b")
leg_line_labs <- c(line_green, line_red)
leg_line_cols <- c("line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_green", "line_red")
leg_col_labs   <- c(leg_point_labs,leg_line_labs)
leg_col_cols   <- c(leg_point_cols,leg_line_cols)
leg_col_breaks <- c(leg_point_breaks,leg_line_breaks)
BTC_sqrd.log_retperc_TrnS_TstS_sp <- ggplot(Data_df, aes(x=x)) +
  geom_vline(xintercept=Data_df$x[TrnS_length], lwd=0.5, color="black") +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", 
              method="lm" , formula=y ~ x, se=FALSE, na.rm=TRUE) +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", 
              method="loess", formula=y ~ x, se=FALSE, na.rm=TRUE) +
  geom_point(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="point_black"), alpha=1, size=0.6, shape=19, na.rm=TRUE) + 
  geom_point(data=subset(Data_df, Data_df$x>x[TrnS_length]), aes(y=y, color="point_b"), alpha=1, size=0.6, shape=19, na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  guides(colour=guide_legend(override.aes=list(shape=c(19, 19, NA, NA), linetype=c("blank", "blank", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_sqrd.log_retperc_TrnS_TstS_sp)
#
# The line plot
BTC_sqrd.log_retperc_TrnS_TstS_lp <- ggplot(Data_df, aes(x=x)) +
  geom_vline(xintercept=Data_df$x[TrnS_length], lwd=0.5, color="black") +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", 
              method="lm" , formula=y ~ x, se=FALSE, na.rm=TRUE) +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", 
              method="loess", formula=y ~ x, se=FALSE, na.rm=TRUE) +
  geom_line(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="point_black"), alpha=1, size=0.5, linetype="solid", na.rm=TRUE) +
  geom_line(data=subset(Data_df, Data_df$x>x[TrnS_length]), aes(y=y, color="point_b"), alpha=1, size=0.5, linetype="solid", na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_sqrd.log_retperc_TrnS_TstS_lp)
#
# Due to the quite evident presence of conditional heteroscedasticity in the daily squared logarithm return percentage, we consider the
# robust autocorrelation tests
Data_df <- dplyr::rename(Data_df, t=x, sqrd.log_ret_perc=y)
y <- na.rm(Data_df$sqrd.log_ret_perc)
testcorr::ac.test(y, max.lag = 10, alpha = 0.05, lambda = 2.576, plot = TRUE, table = TRUE, var.name = NULL, scale.font = 1)
# Tests for zero autocorrelation of x
#   | Lag|    AC|  Stand. CB(95%)|  Robust CB(95%)| Lag|     t| p-value| t-tilde| p-value| Lag|     LB| p-value| Q-tilde| p-value|
#   |---:|-----:|---------------:|---------------:|---:|-----:|-------:|-------:|-------:|---:|------:|-------:|-------:|-------:|
#   |   1| 0.059| (-0.045, 0.045)| (-0.081, 0.081)|   1| 2.542|   0.011|   1.431|   0.152|   1|  6.472|   0.011|   2.047|   0.152|
#   |   2| 0.025| (-0.045, 0.045)| (-0.027, 0.027)|   2| 1.079|   0.281|   1.788|   0.074|   2|  7.638|   0.022|   5.245|   0.073|
#   |   3| 0.011| (-0.045, 0.045)| (-0.015, 0.015)|   3| 0.459|   0.646|   1.429|   0.153|   3|  7.850|   0.049|   7.286|   0.063|
#   |   4| 0.061| (-0.045, 0.045)| (-0.060, 0.060)|   4| 2.658|   0.008|   2.003|   0.045|   4| 14.937|   0.005|  11.297|   0.023|
#   |   5| 0.018| (-0.045, 0.045)| (-0.016, 0.016)|   5| 0.772|   0.440|   2.236|   0.025|   5| 15.536|   0.008|  16.295|   0.006|
#   |   6| 0.013| (-0.045, 0.045)| (-0.020, 0.020)|   6| 0.548|   0.584|   1.235|   0.217|   6| 15.837|   0.015|  17.821|   0.007|
#   |   7| 0.110| (-0.045, 0.045)| (-0.184, 0.184)|   7| 4.764|   0.000|   1.174|   0.240|   7| 38.643|   0.000|  19.200|   0.008|
#   |   8| 0.006| (-0.045, 0.045)| (-0.018, 0.018)|   8| 0.249|   0.804|   0.636|   0.525|   8| 38.705|   0.000|  19.604|   0.012|
#   |   9| 0.001| (-0.045, 0.045)| (-0.017, 0.017)|   9| 0.027|   0.978|   0.074|   0.941|   9| 38.706|   0.000|  19.610|   0.020|
#   |  10| 0.008| (-0.045, 0.045)| (-0.017, 0.017)|  10| 0.356|   0.722|   0.953|   0.340|  10| 38.833|   0.000|  20.518|   0.025|
# 
# The robust tests confirm the presence of autocorrelation in the daily squared logarithm return percentage.
# Note that some authors choose to consider the absolute logarithm return percentage, rather than the squared logarithm return percentage.
Data_df <- BTC_train_df
y <- na.rm(Data_df$log_ret_perc)
FitAR::LjungBoxTest(abs(y), k=n_pars, lag.max=max_lag, StartLag=1, SquaredQ=FALSE)
#  m     Qm             pvalue
#  1  26.60 0.0000002507643296
#  2  40.26 0.0000000018138009
#  3  48.39 0.0000000001757383
#  4  85.28 0.0000000000000000
#  5  98.03 0.0000000000000000
#  6 112.28 0.0000000000000000
#  7 149.92 0.0000000000000000
#  8 161.47 0.0000000000000000
#  9 165.41 0.0000000000000000
# 10 168.59 0.0000000000000000
#
lag_seq <- seq(from=1, to=max_lag, by=1)
portes::LjungBox(abs(y), lags=lag_seq, fitdf=n_pars, sqrd.res=FALSE)
# lags statistic df            p-value
#  1  26.59599  1 0.0000002507643296
#  2  40.25568  2 0.0000000018138009
#  3  48.39192  3 0.0000000001757383
#  4  85.27577  4 0.0000000000000000
#  5  98.03466  5 0.0000000000000000
#  6 112.28348  6 0.0000000000000000
#  7 149.91924  7 0.0000000000000000
#  8 161.46735  8 0.0000000000000000
#  9 165.40627  9 0.0000000000000000
# 10 168.59421 10 0.0000000000000000
#
for(l in 1:max_lag){
  LB_fit_df <- min(min(l, n_pars), l-1)
  show(Box.test(abs(y), lag=l, fitdf=LB_fit_df, type="Ljung-Box"))
}
# Box-Ljung test
# data:  abs(y)
# X-squared = 26.596, df =  1, p-value = 0.0000002508
# X-squared = 40.256, df =  2, p-value = 0.000000001814
# X-squared = 48.392, df =  3, p-value = 0.0000000001757
# X-squared = 85.276, df =  4, p-value < 0.00000000000000022
# X-squared = 98.035, df =  5, p-value < 0.00000000000000022
# X-squared = 112.28, df =  6, p-value < 0.00000000000000022
# X-squared = 149.92, df =  7, p-value < 0.00000000000000022
# X-squared = 161.47, df =  8, p-value < 0.00000000000000022
# X-squared = 165.41, df =  9, p-value < 0.00000000000000022
# X-squared = 168.59, df = 10, p-value < 0.00000000000000022
# 
# Autocorrelogram of the training set of the Bitcoin daily absolute logarithm return percentage.
Data_df <- BTC_train_df
y <- na.rm(Data_df$log_ret_perc)
y <- abs(y)
length <- length(y)
T <- length(y)
# max_lag <- ceiling(10*log10(T))    # Default
# max_lag <- ceiling(sqrt(n)+45)     # Box-Jenkins
max_lag <- ceiling(min(10,T/4))      # Hyndman (for data without seasonality)
# max_lag <- ceiling(min(2*12,T/5))  # Hyndman (for data with seasonality, see https://robjHyndman.com/hy_resndsight/ljung-box-test/)
# Aut_Fun_y <- stats::acf(y, lag.max=max_lag, type="correlation", plot=FALSE)
Aut_Fun_y <- TSA::acf(y, lag.max=max_lag, type="correlation", plot=TRUE)
ci_090 <- qnorm((1+0.90)/2)/sqrt(T)
ci_95 <- qnorm((1+0.95)/2)/sqrt(T)
ci_99 <- qnorm((1+0.99)/2)/sqrt(T)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
First_Day <- as.character(Data_df$Date[1])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Autocorrelogram of the Bitcoin Daily Absolute Logarithm Return Percentage - Training Set - from ", .(First_Day), " to ", .(Last_Day), sep="")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points,  ", "lags ", .(max_lag), ".  Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("lags")
x_breaks_num <- max_lag
x_binwidth <- 1
x_breaks <- Aut_Fun_y_df$lag
x_labs <- format(x_breaks, scientific=FALSE)
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), lwd=1, col="black") +
  geom_hline(aes(yintercept=-ci_090, color="CI_090"), lwd=0.9, lty=3, show.legend=TRUE) +
  geom_hline(aes(yintercept=ci_090, color="CI_090"), lwd=0.9, lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), lwd=0.8, lty=2, show.legend=TRUE) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lwd=0.8, lty=2) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), lwd=0.8, lty=4, show.legend=TRUE) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lwd=0.8, lty=4) +
  scale_x_continuous(name= "lag", breaks=x_breaks, label=x_labs) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis=sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"), values=c(CI_090="green", CI_95="blue", CI_99="red"),
                     guide=guide_legend(override.aes=list(linetype=c("dotted", "dashed", "dotdash")))) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=0, vjust=1, hjust=0.5),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
#
# The autocorrelogram yields visual evidence for autocorrelation at the $1\%$ significance level.
# Partial autocorrelogram of the training set of the Bitcoin daily absolute logarithm return percentage.
Data_df <- BTC_train_df
y <- na.rm(Data_df$log_ret_perc)
y <- abs(y)
length <- length(y)
T <- length(y)
# max_lag <- ceiling(10*log10(T))    # Default
# max_lag <- ceiling(sqrt(n)+45)     # Box-Jenkins
max_lag <- ceiling(min(10,T/4))      # Hyndman (for data without seasonality)
# max_lag <- ceiling(min(2*12,T/5))  # Hyndman (for data with seasonality, see https://robjHyndman.com/hy_resndsight/ljung-box-test/)
Part_Aut_Fun_y <- stats::pacf(y, lag.max=max_lag, type="correlation", plot=FALSE)
ci_090 <- qnorm((1+0.90)/2)/sqrt(T)
ci_95 <- qnorm((1+0.95)/2)/sqrt(T)
ci_99 <- qnorm((1+0.99)/2)/sqrt(T)
Plot_Part_Aut_Fun_y <- data.frame(lag=Part_Aut_Fun_y$lag, pacf=Part_Aut_Fun_y$acf)
First_Day <- as.character(Data_df$Date[1])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Partial Autocorrelogram of the Bitcoin Daily Absolute Logarithm Return Percentage - Training Set - from ", .(First_Day), " to ", .(Last_Day), sep="")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points,  ", "lags ", .(max_lag), ".  Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("lags")
x_breaks_num <- max_lag
x_binwidth <- 1
x_breaks <- Aut_Fun_y_df$lag
x_labs <- format(x_breaks, scientific=FALSE)
ggplot(Plot_Part_Aut_Fun_y, aes(x=lag, y=pacf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=pacf), size=1, col="black") +
  geom_hline(aes(yintercept=-ci_090, color="CI_090"), lwd=0.9, lty=3, show.legend=TRUE) +
  geom_hline(aes(yintercept=ci_090, color="CI_090"), lwd=0.9, lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), lwd=0.8, lty=2, show.legend=TRUE) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lwd=0.8, lty=2) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), lwd=0.8, lty=4, show.legend=TRUE) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lwd=0.8, lty=4) +
  scale_x_continuous(name= "lag", breaks=x_breaks, label=x_labs) +
  scale_y_continuous(name="pacf value", breaks=waiver(), labels=NULL,
                     sec.axis=sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"), values=c(CI_090="green", CI_95="blue", CI_99="red"),
                     guide=guide_legend(override.aes=list(linetype=c("dotted", "dashed", "dotdash")))) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=0, vjust=1, hjust=0.5),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
#
# The partial autocorrelogram also yields visual evidence for autocorrelation at the $1\%$ significance level.
#
# We plot the Bitcoin daily squared absolute logarithm return percentage.
Data_df <- BTC_red_df
head(Data_df)
tail(Data_df)
Data_df <- add_column(Data_df, abs.log_ret_perc=abs(Data_df$log_ret_perc), .after="log_ret_perc")
head(Data_df)
Data_df <- dplyr::rename(Data_df, x=t, y=abs.log_ret_perc)
head(Data_df)
DS_length <- length(na.rm(Data_df$y))
show(DS_length)
# 1870
TrnS_First_Day <- as.character(Data_df$Date[min(which(!is.na(Data_df$y)))])
show(TrnS_First_Day)
# "2018-04-18"
TrnS_Last_Day <- as.character(Data_df$Date[which(Data_df$Date==as.Date("2023-01-01"))-1])
show(TrnS_Last_Day)
# "2022-12-31"
TrnS_length <- length(na.rm(Data_df$y[which(Data_df$Date<as.Date("2023-01-01"))]))
show(TrnS_length)
# 1719
TstS_length <- DS_length-TrnS_length
show(TstS_length)
# 151
TstS_First_Day <- as.character(Data_df$Date[which(Data_df$Date==as.Date("2023-01-01"))])
show(TstS_First_Day)
# "2023-01-01"
TstS_length <- length(na.rm(Data_df$Date[which(Data_df$Date>=as.Date("2023-01-01"))]))
show(TstS_length)
# 151
TstS_length == DS_length-TrnS_length
# TRUE
First_Day <- as.character(Data_df$Date[1])
show(First_Day)
# "2018-04-17"
Last_Day <- as.character(Data_df$Date[DS_length])
show(Last_Day)
# "2023-05-31"
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Bitcoin Daily Absolute Logarithm Return Percentage - Training and Test Sets - from ", .(First_Day), " to ", .(Last_Day), sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points, from ", .(TrnS_First_Day), " to ", .(TrnS_Last_Day),". Test set length ", .(TstS_length), " sample points, from ", .(TstS_First_Day), " to ", .(Last_Day),". Data by courtesy of Yahoo Finance - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("")
# numbers::primeFactors(length-1)
x_breaks_num <- 34 # (deduced from primeFactors(length-1))
x_breaks_low <- Data_df$x[1]
x_breaks_up <- Data_df$x[DS_length]
x_binwidth <- ceiling((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Date[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("daily absolute pergentage logarithm returns (US $)")
y_max <- max(na.rm(Data_df$y))
y_min <- min(na.rm(Data_df$y))
as.numeric(floor(y_max-y_min))
y_breaks_num <- 10
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor((y_min/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((y_max/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth), digits=3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.0
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
point_black <- bquote("daily perc. log. ret. - training set")
point_b <- bquote("daily perc. log. ret. - test set")
line_green <- bquote("regression line (training set)")
line_red <- bquote("LOESS curve (training set)")
leg_point_labs <- c(point_black, point_b)
leg_point_cols <- c("point_black"="black", "point_b"="blue")
leg_point_breaks <- c("point_black", "point_b")
leg_line_labs <- c(line_green, line_red)
leg_line_cols <- c("line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_green", "line_red")
leg_col_labs   <- c(leg_point_labs,leg_line_labs)
leg_col_cols   <- c(leg_point_cols,leg_line_cols)
leg_col_breaks <- c(leg_point_breaks,leg_line_breaks)
BTC_abs.log_retperc_TrnS_TstS_sp <- ggplot(Data_df, aes(x=x)) +
  geom_vline(xintercept=Data_df$x[TrnS_length], lwd=0.5, color="black") +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", 
              method="lm" , formula=y ~ x, se=FALSE, na.rm=TRUE) +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", 
              method="loess", formula=y ~ x, se=FALSE, na.rm=TRUE) +
  geom_point(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="point_black"), alpha=1, size=0.6, shape=19, na.rm=TRUE) + 
  geom_point(data=subset(Data_df, Data_df$x>x[TrnS_length]), aes(y=y, color="point_b"), alpha=1, size=0.6, shape=19, na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  guides(colour=guide_legend(override.aes=list(shape=c(19, 19, NA, NA), linetype=c("blank", "blank", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_abs.log_retperc_TrnS_TstS_sp)
#
# The line plot
BTC_abs.log_retperc_TrnS_TstS_lp <- ggplot(Data_df, aes(x=x)) +
  geom_vline(xintercept=Data_df$x[TrnS_length], lwd=0.5, color="black") +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", 
              method="lm" , formula=y ~ x, se=FALSE, na.rm=TRUE) +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", 
              method="loess", formula=y ~ x, se=FALSE, na.rm=TRUE) +
  geom_line(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="point_black"), alpha=1, size=0.5, linetype="solid", na.rm=TRUE) +
  geom_line(data=subset(Data_df, Data_df$x>x[TrnS_length]), aes(y=y, color="point_b"), alpha=1, size=0.5, linetype="solid", na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_abs.log_retperc_TrnS_TstS_lp)
#
# Due to the quite evident presence of conditional heteroscedasticity in the daily absolute logarithm return percentage, we consider the
# robust autocorrelation tests
Data_df <- dplyr::rename(Data_df, t=x, abs.log_ret_perc=y)
y <- na.rm(Data_df$abs.log_ret_perc)
testcorr::ac.test(y, max.lag = 10, alpha = 0.10, lambda = 2.576, plot = TRUE, table = TRUE, var.name = NULL, scale.font = 1)
# Tests for zero autocorrelation of x
#   | Lag|    AC|  Stand. CB(90%)|  Robust CB(90%)| Lag|     t| p-value| t-tilde| p-value| Lag|      LB| p-value| Q-tilde| p-value|
#   |---:|-----:|---------------:|---------------:|---:|-----:|-------:|-------:|-------:|---:|-------:|-------:|-------:|-------:|
#   |   1| 0.132| (-0.038, 0.038)| (-0.062, 0.062)|   1| 5.692|   0.000|   3.504|   0.000|   1|  32.452|   0.000|  12.278|   0.000|
#   |   2| 0.093| (-0.038, 0.038)| (-0.045, 0.045)|   2| 4.026|   0.000|   3.389|   0.001|   2|  48.692|   0.000|  23.766|   0.000|
#   |   3| 0.068| (-0.038, 0.038)| (-0.037, 0.037)|   3| 2.955|   0.003|   3.066|   0.002|   3|  57.447|   0.000|  33.168|   0.000|
#   |   4| 0.150| (-0.038, 0.038)| (-0.058, 0.058)|   4| 6.492|   0.000|   4.228|   0.000|   4|  99.730|   0.000|  51.045|   0.000|
#   |   5| 0.090| (-0.038, 0.038)| (-0.040, 0.040)|   5| 3.872|   0.000|   3.727|   0.000|   5| 114.780|   0.000|  64.937|   0.000|
#   |   6| 0.097| (-0.038, 0.038)| (-0.041, 0.041)|   6| 4.202|   0.000|   3.853|   0.000|   6| 132.516|   0.000|  74.314|   0.000|
#   |   7| 0.148| (-0.038, 0.038)| (-0.082, 0.082)|   7| 6.399|   0.000|   2.959|   0.003|   7| 173.664|   0.000|  83.070|   0.000|
#   |   8| 0.086| (-0.038, 0.038)| (-0.040, 0.040)|   8| 3.705|   0.000|   3.557|   0.000|   8| 187.467|   0.000|  92.504|   0.000|
#   |   9| 0.052| (-0.038, 0.038)| (-0.037, 0.037)|   9| 2.244|   0.025|   2.313|   0.021|   9| 192.531|   0.000|  97.855|   0.000|
#   |  10| 0.051| (-0.038, 0.038)| (-0.036, 0.036)|  10| 2.219|   0.027|   2.369|   0.018|  10| 197.485|   0.000| 103.468|   0.000|
# 
# The robust tests confirm the presence of autocorrelation in the Bitcoin daily absolute logarithm return percentage at the $1\%$ 
# significance level.
############################################################################################################################################
############################################################################################################################################
############################################################################################################################################
# Thanks to the non-rejection of the null of no autocorrelation at the $5\%$ significance level in the Bitcoin daily percentage logarithm 
# returns and the rejection of the null of no autocorrelation in the squared and absolute logarithm return percentage at the $5\%$ and 
# $1\%$ significance levels, respectively, we can consider an GARCH model for the Bitcoin daily logarithm return percentage. To this, we
# will use the two popular packages, *tseries* (Trapletti and Hornik, 2018) and *fGarch* (Wuertz and Chalabi, 2016) 
# (see https://cran.r-project.org/web/packages/tseries/tseries.pdf and https://cran.r-project.org/web/packages/fGarch/fGarch.pdf), 
# in which the estimation of the GARCH model is treated, showing similarities and differences, for a better understanding of their use.
# (see also  https://www.math.pku.edu.cn/teachers/heyb/TimeSeries/lectures/garch.pdf). In a forthcoming developments of these notes we will
# introduce also the package *rugarch* (Ghalanos, 2017) (see https://cran.r-project.org/web/packages/rugarch/rugarch.pdf, and
# https://cran.r-project.org/web/packages/rugarch/vignettes/Introduction_to_the_rugarch_package.pdf). In the end, we refer to  
# https://www.erfin.org/journal/index.php/erfin/article/download/64/40/ for a survey on all the above packages.
# 
# Let us start with the *tseries* package.
# library(tseries)
# library(crayon)
Data_df <- BTC_train_df
head(Data_df, 3)
#   t       Date    Open    High     Low   Close Adj.Close Adj.Close_diff Adj.Close_log    log_ret log_ret_perc     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09             NA      8.974883         NA           NA 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42       261.3301      9.007418 0.03253593     3.253593 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31       130.8896      9.023325 0.01590650     1.590650 7063209984
tail(Data_df, 3)
#         t       Date     Open     High      Low    Close Adj.Close Adj.Close_diff Adj.Close_log      log_ret log_ret_perc      Volume
# 1718 1718 2022-12-29 16552.32 16651.76 16508.68 16642.34  16642.34       89.76953      9.719705  0.005408645    0.5408645 14472237479
# 1719 1719 2022-12-30 16641.33 16643.43 16408.47 16602.59  16602.59      -39.75586      9.717314 -0.002391696   -0.2391696 15929162910
# 1720 1720 2022-12-31 16603.67 16628.99 16517.52 16547.50  16547.50      -55.08984      9.713990 -0.003323666   -0.3323666 11239186456
y <- na.rm(Data_df$log_ret_perc)
head(y)
# 3.2535928  1.5906496  6.4376485  0.5608363 -1.0523309  1.4483698
T <- length(y)
show(T)
# 1719
cons <- file("tseries_GARCH_log_retperc_fit.log")
sink(cons, append=TRUE, type=c("output","message"), split=TRUE)
GARCH_log_ret_perc <-list() # Initializing an empty list where to save the candidate GARCH(p,q) models on varying of the parameters p and q.
cn <- 1                     # Setting a counter to identify the model                              
for (p in 1:3){             # Looping over p
  for(q in 1:3){            # Looping over p
    # ERROR and WARNINGS HANDLING
    tryCatch({
      GARCH_log_ret_perc[[cn]] <- tseries::garch(y, order=c(p,q), series=NULL, coef=NULL, maxiter=200,
                                            grad="analytical", trace=TRUE, eps=NULL, 
                                            abstol=max(1e-20, .Machine$double.eps^2),
                                            reltol=max(1e-10, .Machine$double.eps^(2/3)), 
                                            xtol=sqrt(.Machine$double.eps),
                                            falsetol=1e2 * .Machine$double.eps)
      show(GARCH_log_ret_perc[[cn]])
      cn <- cn+1
      cat("  \n","  \n")
    }, error=function(e){
      cat(red(sprintf("caught Error: %s", e)))
      cat(red("GARCH parameter p=", p), red("GARCH parameter q=", q))
      cat("\n")
      traceback(1, max.lines=1)
      cat("  \n","  \n")
    }, warning=function (w){
      cat(yellow(sprintf("caught Warning: %s", w)))
      cat(yellow("GARCH parameter p=", p), yellow("GARCH parameter q=", q))
      cat("\n")
      traceback(1, max.lines=1)
      cat("  \n","  \n")
    }
    )
  }
}
closeAllConnections()
# Note that the estimation procedure does not always succeed in converging. Opening the file "tseries_GARCH_log_retperc_fit.log" generated
# by the estimation procedure, we can see that we have got seven ** FALSE CONVERGENCE ** messages, one ** RELATIVE FUNCTION CONVERGENCE **
# message and one ** X- AND RELATIVE FUNCTION CONVERGENCE ** message. This binds the choice of the model. However, to present the general
# model selection procedure, we ignore the ** FALSE CONVERGENCE ** message. Hence, for each of the estimated models we compute the AIC and
# BIC.
#
# Preliminary, we extract the likelihood and the order from each model 
GARCH_log_retperc_Likeli <- sapply(GARCH_log_ret_perc, function(x) x$n.likeli)
show(GARCH_log_retperc_Likeli)
# 3073.048 3070.467 3085.484 3071.646 3107.232 3100.889 3101.715 3100.151 3096.016
#
GARCH_log_retperc_order <- sapply(GARCH_log_ret_perc, function(x) x$order)
show(GARCH_log_retperc_order)
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
#  p    1    1    1    2    2    2    3    3    3
#  q    1    2    3    1    2    3    1    2    3
#
# Hence, we compute the AIC and sort the model counters according the decreasing AIC
GARCH_log_retperc_AIC <- 2*colSums(GARCH_log_retperc_order)-log(GARCH_log_retperc_Likeli^2)
show(GARCH_log_retperc_AIC)
# -12.060851 -10.059170  -8.068927 -10.059938  -8.082975  -6.078888  -8.079421  -6.078412  -4.075743
#
GARCH_log_retperc_sort_AIC <- sort(GARCH_log_retperc_AIC, decreasing=FALSE)
show(GARCH_log_retperc_sort_AIC)
# -12.060851 -10.059938 -10.059170  -8.082975  -8.079421  -8.068927  -6.078888  -6.078412  -4.075743
#
GARCH_log_retperc_incr_AIC <- match(GARCH_log_retperc_AIC, GARCH_log_retperc_sort_AIC)
show(GARCH_log_retperc_incr_AIC)
# 1 3 6 2 4 7 5 8 9
#
# We compute the BIC and sort the model counters according the decreasing BIC
GARCH_log_retperc_BIC <- log(T)*colSums(GARCH_log_retperc_order)-log(GARCH_log_retperc_Likeli^2)
show(GARCH_log_retperc_BIC)
# -1.161855  6.289324 13.729065  6.288556 13.715017 21.168602 13.718571 21.169078 28.621245
#
GARCH_log_retperc_sort_BIC <- sort(GARCH_log_retperc_BIC, decreasing=FALSE)
show(GARCH_log_retperc_sort_BIC)
# -1.161855  6.288556  6.289324 13.715017 13.718571 13.729065 21.168602 21.169078 28.621245
#
GARCH_log_retperc_incr_BIC <- match(GARCH_log_retperc_BIC, GARCH_log_retperc_sort_BIC)
show(GARCH_log_retperc_incr_BIC)
# 1 3 6 2 4 7 5 8 9
#
# Note that, in the case considered, sorting according to the AIC or BIC produces the same result.
# Now, we estimate again the sorted models until we find the first for which the estimation procedure converges.
# We start by checking the estimation procedure for the model of counter cn=1.
cn_sort_AIC <- GARCH_log_retperc_incr_AIC # Setting a counter according to increasing AIC.
show(cn_sort_AIC)
# 1 3 6 2 4 7 5 8 9
#
y <- na.rm(Data_df$log_ret_perc)
garch.control <- tseries::garch.control(trace=TRUE, start=NULL,  coef=NULL, maxiter=200, grad="analytical",  eps=NULL, abstol=max(1e-20, 
                                        .Machine$double.eps^2), reltol=max(1e-10, .Machine$double.eps^(2/3)), xtol=sqrt(.Machine$double.eps),
                                        falsetol=1e2*.Machine$double.eps)
tseries::garch(y, order=c(GARCH_log_ret_perc[[cn_sort_AIC[1]]]$order[1], GARCH_log_ret_perc[[cn_sort_AIC[1]]]$order[2]), series=NULL,
               control=garch.control)
# ***** ESTIMATION WITH ANALYTICAL GRADIENT ***** 
#   I     INITIAL X(I)        D(I)
#   1     1.262054e+01     1.000e+00
#   2     5.000000e-02     1.000e+00
#   3     5.000000e-02     1.000e+00
# 
#  IT   NF      F       RELDF    PRELDF    RELDX   STPPAR   D*STEP   NPRELDF
#  0    1  3.117e+03
#  1    4  3.117e+03  6.47e-05  2.72e-04  8.6e-04  1.8e+03  2.2e-02  2.41e-01
#  2    5  3.117e+03  4.62e-07  6.26e-05  8.5e-04  2.0e+00  2.2e-02  7.49e-03
#  3    6  3.117e+03  2.35e-05  2.36e-05  4.3e-04  2.1e+00  1.1e-02  1.30e-03
#  4    8  3.117e+03  3.18e-06  5.43e-06  1.1e-04  1.6e+01  2.9e-03  9.08e-04
#  5    9  3.117e+03  9.88e-07  1.99e-06  1.1e-04  1.5e+01  2.9e-03  1.11e-04
#  6   15  3.116e+03  1.94e-04  9.65e-05  3.2e-02  0.0e+00  7.8e-01  9.65e-05
#  7   17  3.088e+03  9.18e-03  1.08e-03  5.4e-01  0.0e+00  8.3e+00  1.08e-03
#  8   23  3.088e+03  6.71e-05  3.39e-04  1.6e-03  4.0e+00  1.3e-02  9.95e-01
#  9   24  3.087e+03  1.13e-04  1.16e-04  1.7e-03  2.0e+00  1.3e-02  9.35e-01
# 10   25  3.087e+03  7.71e-05  8.83e-05  3.6e-03  2.0e+00  2.7e-02  9.68e-01
# 11   29  3.078e+03  2.78e-03  6.29e-03  5.8e-01  2.0e+00  2.6e+00  9.89e-01
# 12   33  3.077e+03  4.45e-04  1.84e-03  6.7e-03  6.2e+00  1.4e-02  6.85e-03
# 13   34  3.076e+03  3.40e-04  3.44e-04  6.5e-03  2.0e+00  1.4e-02  7.44e-03
# 14   35  3.075e+03  3.21e-04  3.40e-04  1.2e-02  2.0e+00  2.8e-02  5.29e-03
# 15   36  3.074e+03  2.87e-04  3.58e-04  2.7e-02  1.9e+00  5.6e-02  2.79e-03
# 16   37  3.074e+03  1.23e-04  1.47e-04  2.6e-02  1.7e+00  5.6e-02  9.01e-04
# 17   38  3.073e+03  2.11e-04  2.37e-04  4.8e-02  2.3e-01  1.1e-01  2.44e-04
# 18   39  3.073e+03  6.61e-06  2.42e-05  4.0e-02  0.0e+00  1.0e-01  2.42e-05
# 19   40  3.073e+03  1.03e-05  1.02e-05  9.1e-03  0.0e+00  2.4e-02  1.02e-05
# 20   41  3.073e+03  9.95e-08  7.82e-08  4.4e-04  0.0e+00  1.1e-03  7.82e-08
# 21   42  3.073e+03  4.72e-10  1.15e-11  9.2e-06  0.0e+00  2.4e-05  1.15e-11
# 22   43  3.073e+03  4.87e-11  2.58e-14  9.7e-07  0.0e+00  2.5e-06  2.58e-14
# 23   44  3.073e+03  3.06e-12  9.53e-17  6.1e-08  0.0e+00  1.6e-07  9.53e-17
# 24   45  3.073e+03 -2.37e-15  7.39e-21  7.9e-11  0.0e+00  2.2e-10  7.39e-21
# 
# ***** X- AND RELATIVE FUNCTION CONVERGENCE *****
#   
# FUNCTION     3.073048e+03   RELDX        7.860e-11
# FUNC. EVALS      45         GRAD. EVALS      24
# PRELDF       7.392e-21      NPRELDF      7.392e-21
# 
# I      FINAL X(I)        D(I)          G(I)
# 
# 1    1.272503e+00     1.000e+00     8.383e-08
# 2    9.083005e-02     1.000e+00     6.405e-07
# 3    8.251501e-01     1.000e+00     1.116e-06
# 
# Call: tseries::garch(x=y, order=c(GARCH_log_ret_perc[[cn_sort_AIC[1]]]$order[1], GARCH_log_ret_perc[[cn_sort_AIC[1]]]$order[2]), 
#                      Series=NULL, control=garch.control())
# 
# Coefficient(s):
#   a0       a1       b1  
# 1.27250  0.09083  0.82515  
#
# We check the estimation procedure for the model of counter cn=2
y <- na.rm(Data_df$log_ret_perc)
tseries::garch(y, order=c(GARCH_log_ret_perc[[cn_sort_AIC[2]]]$order[1], GARCH_log_ret_perc[[cn_sort_AIC[2]]]$order[2]), series=NULL, 
               control=garch.control())
# ***** ESTIMATION WITH ANALYTICAL GRADIENT ***** 
# I     INITIAL X(I)        D(I)
# 1     1.121826e+01     1.000e+00
# 2     5.000000e-02     1.000e+00
# 3     5.000000e-02     1.000e+00
# 4     5.000000e-02     1.000e+00
# 5     5.000000e-02     1.000e+00
# 
#  IT   NF      F       RELDF    PRELDF    RELDX   STPPAR   D*STEP   NPRELDF
#  0    1  3.108e+03
#  1    3  3.108e+03  7.59e-06  1.94e-03  3.8e-03  6.1e+02  1.0e-01  5.88e-01
#  2    4  3.107e+03  5.17e-04  8.31e-04  1.6e-03  2.3e+00  5.0e-02  1.69e-01
#  3    6  3.106e+03  9.50e-05  2.22e-04  5.4e-04  3.3e+00  1.5e-02  2.54e-01
#  4    7  3.106e+03  3.91e-06  4.44e-05  3.9e-04  2.0e+00  1.5e-02  1.14e-02
#  5    8  3.106e+03  1.42e-05  1.73e-05  2.1e-04  2.0e+00  7.3e-03  2.61e-03
#  6   10  3.106e+03  2.92e-06  6.95e-06  1.3e-04  2.5e+00  3.6e-03  2.96e-03
#  7   11  3.106e+03  4.04e-06  4.90e-06  1.0e-04  2.0e+00  3.6e-03  2.27e-03
#  8   12  3.106e+03  2.82e-06  3.23e-06  1.4e-04  2.0e+00  3.6e-03  1.46e-03
#  9   15  3.106e+03  2.79e-05  4.35e-05  2.2e-03  1.9e+00  4.9e-02  1.43e-03
# 10   16  3.106e+03  5.93e-05  6.12e-05  2.1e-03  2.0e+00  4.9e-02  1.85e-02
# 11   17  3.106e+03  3.67e-05  5.11e-05  2.2e-03  2.0e+00  4.9e-02  1.82e-02
# 12   20  3.105e+03  2.13e-04  3.26e-04  1.5e-02  2.0e+00  3.3e-01  3.29e-02
# 13   21  3.105e+03  2.29e-04  4.91e-04  1.5e-02  2.0e+00  3.3e-01  1.28e-01
# 14   23  3.103e+03  5.73e-04  4.95e-04  1.6e-02  2.0e+00  3.3e-01  6.57e-02
# 15   24  3.102e+03  3.34e-04  5.07e-04  1.6e-02  2.0e+00  3.3e-01  1.61e-01
# 16   26  3.098e+03  1.11e-03  9.84e-04  3.4e-02  2.0e+00  6.5e-01  2.17e-01
# 17   28  3.098e+03  2.03e-04  2.10e-04  7.2e-03  2.0e+00  1.3e-01  1.25e+00
# 18   30  3.096e+03  4.11e-04  4.00e-04  1.5e-02  2.0e+00  2.6e-01  1.23e+00
# 19   32  3.096e+03  1.03e-04  1.18e-04  2.9e-03  2.0e+00  5.2e-02  1.31e+01
# 20   36  3.093e+03  9.30e-04  8.89e-04  3.2e-02  2.0e+00  5.4e-01  1.55e+01
# 21   38  3.093e+03  1.93e-04  1.92e-04  6.6e-03  2.0e+00  1.1e-01  1.19e+03
# 22   40  3.091e+03  3.86e-04  3.87e-04  1.4e-02  2.0e+00  2.1e-01  2.98e+04
# 23   42  3.091e+03  7.96e-05  7.82e-05  2.7e-03  2.0e+00  4.3e-02  2.06e+04
# 24   44  3.091e+03  1.61e-04  1.59e-04  5.5e-03  2.0e+00  8.6e-02  1.09e+05
# 25   47  3.088e+03  8.62e-04  8.83e-04  3.2e-02  2.0e+00  4.8e-01  2.76e+05
# 26   49  3.087e+03  2.09e-04  1.96e-04  6.6e-03  2.0e+00  9.5e-02  3.77e+05
# 27   51  3.086e+03  3.85e-04  3.87e-04  1.4e-02  2.0e+00  1.9e-01  7.96e+05
# 28   53  3.086e+03  7.75e-05  7.75e-05  2.7e-03  2.0e+00  3.8e-02  7.43e+05
# 29   55  3.085e+03  1.52e-04  1.53e-04  5.5e-03  2.0e+00  7.6e-02  7.91e+05
# 30   59  3.085e+03  4.24e-07  4.24e-07  7.9e-06  1.6e+01  1.5e-04  7.94e+05
# 31   61  3.085e+03  7.72e-07  7.73e-07  1.7e-05  3.1e+00  3.0e-04  7.96e+05
# 32   63  3.085e+03  1.45e-07  1.45e-07  3.7e-06  6.0e+01  6.1e-05  7.96e+05
# 33   65  3.085e+03  2.88e-08  2.88e-08  7.6e-07  3.2e+02  1.2e-05  7.95e+05
# 34   67  3.085e+03  5.73e-08  5.73e-08  1.5e-06  4.1e+01  2.4e-05  7.95e+05
# 35   69  3.085e+03  1.13e-07  1.14e-07  3.1e-06  2.2e+01  4.9e-05  7.95e+05
# 36   71  3.085e+03  2.25e-08  2.26e-08  6.2e-07  4.5e+02  9.7e-06  7.95e+05
# 37   73  3.085e+03  4.50e-09  4.51e-09  1.2e-07  2.3e+03  1.9e-06  7.95e+05
# 38   75  3.085e+03  9.00e-09  9.01e-09  2.5e-07  2.9e+02  3.9e-06  7.95e+05
# 39   77  3.085e+03  1.80e-08  1.80e-08  5.0e-07  1.4e+02  7.8e-06  7.95e+05
# 40   79  3.085e+03  3.59e-09  3.60e-09  9.9e-08  2.9e+03  1.6e-06  7.95e+05
# 41   81  3.085e+03  7.18e-10  7.19e-10  2.0e-08  1.5e+04  3.1e-07  7.95e+05
# 42   83  3.085e+03  1.44e-10  1.44e-10  4.0e-09  7.3e+04  6.2e-08  7.95e+05
# 43   85  3.085e+03  2.87e-10  2.88e-10  7.9e-09  9.1e+03  1.2e-07  7.95e+05
# 44   87  3.085e+03  5.74e-11  5.75e-11  1.6e-09  2.0e+00  2.5e-08 -1.98e-03
# 45   90  3.085e+03  4.59e-10  4.60e-10  1.3e-08  5.7e+03  2.0e-07  7.95e+05
# 46   93  3.085e+03  9.19e-12  9.20e-12  2.5e-10  2.0e+00  4.0e-09 -1.98e-03
# 47   96  3.085e+03  1.86e-13  1.84e-13  5.1e-12  2.0e+00  8.0e-11 -1.98e-03
# 48   98  3.085e+03  3.67e-13  3.68e-13  1.0e-11  2.0e+00  1.6e-10 -1.98e-03
# 49  100  3.085e+03  7.27e-14  7.36e-14  2.0e-12  2.0e+00  3.2e-11 -1.98e-03
# 50  102  3.085e+03  1.48e-13  1.47e-13  4.1e-12  2.0e+00  6.4e-11 -1.98e-03
# 51  104  3.085e+03  2.74e-14  2.95e-14  8.1e-13  2.0e+00  1.3e-11 -1.98e-03
# 52  106  3.085e+03  6.12e-14  5.89e-14  1.6e-12  2.0e+00  2.6e-11 -1.98e-03
# 53  108  3.085e+03  9.58e-15  1.18e-14  3.3e-13  2.0e+00  5.1e-12 -1.98e-03
# 54  110  3.085e+03  2.49e-14  2.36e-14  6.5e-13  2.0e+00  1.0e-11 -1.98e-03
# 55  112  3.085e+03  4.27e-15  4.71e-15  1.3e-13  2.0e+00  2.0e-12 -1.98e-03
# 56  114  3.085e+03  1.09e-14  9.43e-15  2.6e-13  2.0e+00  4.1e-12 -1.98e-03
# 57  116  3.085e+03  1.74e-14  1.89e-14  5.2e-13  2.0e+00  8.2e-12 -1.98e-03
# 58  118  3.085e+03  2.80e-15  3.77e-15  1.0e-13  2.0e+00  1.6e-12 -1.98e-03
# 59  120  3.085e+03  4.42e-16  7.54e-16  2.1e-14  2.0e+00  3.3e-13 -1.98e-03
# 60  122  3.085e+03  2.06e-15  1.51e-15  4.2e-14  2.0e+00  6.5e-13 -1.98e-03
# 61  124  3.085e+03  3.83e-15  3.02e-15  8.3e-14  2.0e+00  1.3e-12 -1.98e-03
# 62  126  3.085e+03 -3.24e+06  6.03e-16  1.7e-14  2.0e+00  2.6e-13 -1.98e-03
# 
# ***** FALSE CONVERGENCE *****
#   
# FUNCTION     3.085484e+03   RELDX        1.668e-14
# FUNC. EVALS     126         GRAD. EVALS      62
# PRELDF       6.032e-16      NPRELDF     -1.978e-03
# 
# I      FINAL X(I)        D(I)          G(I)
# 1    6.816715e+00     1.000e+00     6.169e+00
# 2    5.323327e-02     1.000e+00     1.448e+00
# 3    1.905442e-14     1.000e+00     2.861e+00
# 4    1.311923e-01     1.000e+00    -7.789e-01
# 5    3.703548e-01     1.000e+00     1.281e+00
# 
# Call: tseries::garch(x=y, order=c(GARCH_log_ret_perc[[cn_sort_AIC[2]]]$order[1], GARCH_log_ret_perc[[cn_sort_AIC[2]]]$order[2]), 
#                      series=NULL, control=garch.control())
# 
# Coefficient(s):
#   a0         a1         a2         a3         b1  
# 6.817e+00  5.323e-02  1.905e-14  1.312e-01  3.704e-01
#
# We check the estimation procedure for the model of counter cn=3
y <- na.rm(Data_df$log_ret_perc)
tseries::garch(y, order=c(GARCH_log_ret_perc[[cn_sort_AIC[3]]]$order[1], GARCH_log_ret_perc[[cn_sort_AIC[3]]]$order[2]), series=NULL, 
               control=garch.control)
# ***** ESTIMATION WITH ANALYTICAL GRADIENT ***** 
# I     INITIAL X(I)        D(I)
# 1     1.051712e+01     1.000e+00
# 2     5.000000e-02     1.000e+00
# 3     5.000000e-02     1.000e+00
# 4     5.000000e-02     1.000e+00
# 5     5.000000e-02     1.000e+00
# 6     5.000000e-02     1.000e+00
# 
#  IT   NF      F       RELDF    PRELDF    RELDX   STPPAR   D*STEP   NPRELDF
#  0    1  3.106e+03
#  1    4  3.104e+03  4.31e-04  8.41e-04  1.0e-03  2.9e+03  3.0e-02  1.22e+00
#  2    5  3.104e+03  1.26e-05  3.49e-04  1.0e-03  3.1e+00  3.0e-02  3.78e-01
#  3    6  3.104e+03  2.01e-04  2.46e-04  3.6e-04  2.3e+00  1.5e-02  6.19e-02
#  4    9  3.102e+03  3.89e-04  3.39e-04  1.8e-03  2.0e+00  6.0e-02  7.02e-02
#  5   11  3.102e+03  1.56e-04  1.11e-04  3.8e-04  2.0e+00  1.2e-02  1.56e+00
#  6   12  3.101e+03  2.27e-04  2.17e-04  7.1e-04  2.0e+00  2.4e-02  1.25e+01
#  7   14  3.101e+03  6.30e-05  6.21e-05  1.9e-04  4.3e+00  4.8e-03  1.22e+01
#  8   16  3.101e+03  1.12e-05  1.12e-05  4.0e-05  4.9e+01  9.6e-04  1.58e+01
#  9   18  3.101e+03  2.19e-05  2.19e-05  8.0e-05  8.8e+00  1.9e-03  1.71e+01
# 10   20  3.101e+03  4.33e-06  4.33e-06  1.6e-05  3.2e+02  3.8e-04  1.73e+01
# 11   22  3.101e+03  8.62e-06  8.62e-06  3.1e-05  4.8e+01  7.7e-04  1.98e+01
# 12   24  3.101e+03  1.72e-06  1.72e-06  6.2e-06  1.3e+03  1.5e-04  2.02e+01
# 13   26  3.101e+03  3.43e-06  3.43e-06  1.2e-05  1.7e+02  3.1e-04  2.20e+01
# 14   28  3.101e+03  6.85e-06  6.85e-06  2.5e-05  9.7e+01  6.2e-04  2.23e+01
# 15   30  3.101e+03  1.37e-06  1.37e-06  4.9e-06  2.5e+03  1.2e-04  2.29e+01
# 16   32  3.101e+03  2.74e-07  2.74e-07  9.9e-07  1.3e+04  2.5e-05  2.54e+01
# 17   34  3.101e+03  5.47e-07  5.47e-07  2.0e-06  1.7e+03  4.9e-05  2.60e+01
# 18   36  3.101e+03  1.09e-06  1.09e-06  4.0e-06  8.7e+02  9.8e-05  2.61e+01
# 19   39  3.101e+03  2.19e-08  2.19e-08  7.9e-08  1.8e+05  2.0e-06  2.62e+01
# 20   41  3.101e+03  4.38e-08  4.38e-08  1.6e-07  2.3e+04  3.9e-06  2.68e+01
# 21   43  3.101e+03  8.75e-08  8.75e-08  3.2e-07  1.1e+04  7.9e-06  2.68e+01
# 22   45  3.101e+03  1.75e-08  1.75e-08  6.3e-08  2.3e+05  1.6e-06  2.68e+01
# 23   47  3.101e+03  3.50e-09  3.50e-09  1.3e-08  1.1e+06  3.2e-07  2.68e+01
# 24   49  3.101e+03  7.00e-10  7.00e-10  2.5e-09  5.7e+06  6.3e-08  2.68e+01
# 25   51  3.101e+03  1.40e-10  1.40e-10  5.1e-10  2.9e+07  1.3e-08  2.68e+01
# 26   53  3.101e+03  2.80e-10  2.80e-10  1.0e-09  3.6e+06  2.5e-08  2.68e+01
# 27   55  3.101e+03  5.60e-10  5.60e-10  2.0e-09  1.8e+06  5.0e-08  2.68e+01
# 28   58  3.101e+03  1.12e-11  1.12e-11  4.0e-11  2.0e+00  1.0e-09 -9.04e-03
# 29   60  3.101e+03  2.24e-11  2.24e-11  8.1e-11  2.0e+00  2.0e-09 -9.04e-03
# 30   63  3.101e+03  4.49e-13  4.48e-13  1.6e-12  2.0e+00  4.0e-11 -9.04e-03
# 31   65  3.101e+03  8.95e-13  8.96e-13  3.2e-12  2.0e+00  8.1e-11 -9.04e-03
# 32   67  3.101e+03  1.79e-13  1.79e-13  6.5e-13  2.0e+00  1.6e-11 -9.04e-03
# 33   70  3.101e+03  1.44e-12  1.43e-12  5.2e-12  2.0e+00  1.3e-10 -9.04e-03
# 34   73  3.101e+03  2.92e-14  2.87e-14  1.0e-13  2.0e+00  2.6e-12 -9.04e-03
# 35   75  3.101e+03  5.70e-14  5.74e-14  2.1e-13  2.0e+00  5.2e-12 -9.04e-03
# 36   77  3.101e+03  1.04e-14  1.15e-14  4.1e-14  2.0e+00  1.0e-12 -9.04e-03
# 37   79  3.101e+03  2.39e-14  2.29e-14  8.3e-14  2.0e+00  2.1e-12 -9.04e-03
# 38   81  3.101e+03  4.55e-15  4.59e-15  1.7e-14  2.0e+00  4.1e-13 -9.04e-03
# 39   83  3.101e+03  9.53e-15  9.18e-15  3.3e-14  2.0e+00  8.3e-13 -9.03e-03
# 40   85  3.101e+03  1.98e-14  1.84e-14  6.6e-14  2.0e+00  1.7e-12 -9.04e-03
# 41   87  3.101e+03  2.05e-15  3.67e-15  1.3e-14  2.0e+00  3.3e-13 -9.04e-03
# 42   89  3.101e+03  5.87e-16  7.34e-16  2.6e-15  2.0e+00  6.6e-14 -9.04e-03
# 43   91  3.101e+03  1.47e-15  1.47e-15  5.3e-15  2.0e+00  1.3e-13 -9.04e-03
# 44   92  3.101e+03 -3.22e+06  2.94e-15  1.1e-14  2.0e+00  2.6e-13 -9.03e-03
# 
# ***** FALSE CONVERGENCE *****
#   
# FUNCTION     3.100889e+03   RELDX        1.060e-14
# FUNC. EVALS      92         GRAD. EVALS      44
# PRELDF       2.937e-15      NPRELDF     -9.031e-03
# 
# I      FINAL X(I)        D(I)          G(I)
# 
# 1    1.049981e+01     1.000e+00     3.829e+00
# 2    4.161838e-02     1.000e+00     1.214e+00
# 3    8.872517e-02     1.000e+00    -8.654e+00
# 4    1.435550e-02     1.000e+00     4.079e+00
# 5    1.625536e-13     1.000e+00     2.901e+01
# 6    1.474573e-01     1.000e+00    -1.543e+01
# 
# Call: tseries::garch(x=y, order=c(GARCH_log_ret_perc[[cn_sort_AIC[3]]]$order[1], GARCH_log_ret_perc[[cn_sort_AIC[3]]]$order[2]), 
#                      series=NULL, control=garch.control())
# 
# Coefficient(s):
#   a0         a1         a2         a3         b1         b2  
# 1.050e+01  4.162e-02  8.873e-02  1.436e-02  1.626e-13  1.475e-01 
#
# Therefore, we select the model of counter cn_sort_AIC=1, that is a GARCH(1,1) model.
GARCH_1_1 <- GARCH_log_ret_perc[[1]]
show(GARCH_1_1)
# Call: tseries::garch(x=y, order=c(p, q), series=NULL, coef=NULL, maxiter=200, grad="analytical", trace=TRUE, eps=NULL,
#                      abstol=max(1e-20, .Machine$double.eps^2), reltol=max(1e-10, .Machine$double.eps^(2/3)), 
#                      xtol=sqrt(.Machine$double.eps), falsetol=100 *.Machine$double.eps)
# 
# Coefficient(s):
#      a0       a1       b1  
# 1.27250  0.09083  0.82515
#
summary(GARCH_1_1)
# Call: tseries::garch(x=y, order=c(p, q), series=NULL, coef=NULL,     maxiter=200, grad="analytical", trace=TRUE, eps=NULL,     abstol=max(1e-20, .Machine$double.eps^2), reltol=max(1e-10,         .Machine$double.eps^(2/3)), xtol=sqrt(.Machine$double.eps),     falsetol=100 * .Machine$double.eps)
# Model: GARCH(1,1)
# Residuals: Min        1Q     Median        3Q       Max 
#        -12.68034  -0.40544   0.02741   0.44842   5.71992 
# 
# Coefficient(s): Estimate  Std. Error  t value Pr(>|t|)    
#           a0   1.27250     0.17990    7.074   1.51e-12 ***
#           a1   0.09083     0.01108    8.196   2.22e-16 ***
#           b1   0.82515     0.02166   38.101    < 2e-16 ***
#   ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Diagnostic Tests: 
# Jarque Bera Test
# data:  Residuals
# X-squared=22642, df=2, p-value < 2.2e-16
# 
# Box-Ljung test
# data:  Squared.Residuals
# X-squared=0.058415, df=1, p-value=0.809
#
# Note that from the diagnostic tests executed within the procedure, we have computational evidence that the residuals of the estimated
# GARCH(1,1) model are not Gaussian distributed. Still, the squared residuals appear to be uncorrelated. This means that the GARCH(1,1)
# model should have been able to account for the conditional heteroscedasticity of the Bitcoin daily logarithm return percentage training
# set. The estimation procedure also produces some plots.
def_margins <- par("mar")
mar=c(2.5,2.5,4.0,1.0)
plot(GARCH_1_1)
par(mar=def_margins)
#
# Since corresponding to cn_sort_AIC=1 we have the best AIC and BIC, before checking the model's residuals, which is a necessary step for 
# the validation of the model, it is unnecessary to consider also the cases cn_sort_AIC=2 and cn_sort_AIC=3. We just showed them for 
# illustrative purposes. However, due to the false convergence result, we already know that we have to discharge them as possible
# alternative models, even if the validation of the GARCH(1,1) model corresponding to the counter cn=1 will fail.
#
# After having determined the best model for which we have convergence in the parameter estimation, we store the estimated coefficients
a0 <- as.numeric(GARCH_1_1$coef[1])
a1 <- as.numeric(GARCH_1_1$coef[2])
b1 <- as.numeric(GARCH_1_1$coef[3])
show(c(a0, a1, b1))
#  1.27250304 0.09083005 0.82515011
#
# Note that we have
a1+b1 < 1 
# TRUE
#
# Hence, the estimated GARCH(1,1) model is stationary.
# We compute the long run variance of the model
long_run_var <- a0/(1-(a1+b1))
show(long_run_var)
# 15.14527
#
# We consider the model's residuals
head(GARCH_1_1[["residuals"]],20)
#         NA  0.4144350  1.7419601  0.1388852 -0.2739446  0.3934785  2.3160671 -2.1750189  0.9879875 -0.6733973  0.8577650  0.1677307
# -0.4519716 -0.3269755  0.3293429  1.4429839 -0.1135800  0.4322909 -0.5793139 -0.8429184
#
mean(na.rm(GARCH_1_1[["residuals"]]))
# 0.009702442
#
var(na.rm(GARCH_1_1[["residuals"]]))
# 1.001107
#
# Despite not being clearly documented (?!), the tseries::garch() function computes the standardized residuals of the model.
# The standardized residuals are defined as the states of the model divided the conditional standard deviation. This definition comes from
# the stochastic equation
# $Z_{t}=\sigma_{t}W_{t}$, where $t=1,\dots,T$
# Given the sequence $\left(Z_{t}\left(\omega\right)\right)_{t=1}^{T}$ of the observed states of the process and computing the realizations 
# of the conditional variance of the process from the stochastic equation
# $\sigma_{t}^{2}=a_{0} + a_{1}Z_{t-1}^{2} + b_{1}\sigma_{t-1}^2, where $t=1,\dots,T$,
# we can write
# $\frac{Z_{t}\left(\omega\right)}{\hat{\sigma}_{t\mid\t-1}\left(\omega\right)}=W_{t}\left(\omega\right), where $t=1,\dots,T$.
# Then, the elements of the sequence $\left(W_{t}\left(\omega\right)\right)_{t=1}^{T} are the standardized residuals of the model.
# The main issue to compute the conditional variance of the process is its initialization, that is the determination of the initial value
# $\hat{\sigma}_{0}^{2}$. This issue is addressed, for instance, in the paper
# https://www.researchgate.net/publication/237530561_VARIANCE_INITIALISATION_IN_GARCH_ESTIMATION
# There are several ways to initialize the conditional variance:
# 1) the value $\hat{\sigma}_{0}^{2}$ can be chosen as the unconditional sample variance;
# $\hat{\sigma}_{0}^{2}=\frac{1}{T}\sum_{t=1}^{T}Z_{t}^2\left(\omega\right)$.
# In our code chunk,
Data_df <- BTC_train_df
head(Data_df, 3)
#   t       Date    Open    High     Low   Close Adj.Close Adj.Close_diff Adj.Close_log    log_ret log_ret_perc     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09             NA      8.974883         NA           NA 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42       261.3301      9.007418 0.03253593     3.253593 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31       130.8896      9.023325 0.01590650     1.590650 7063209984
tail(Data_df, 3)
#         t       Date     Open     High      Low    Close Adj.Close Adj.Close_diff Adj.Close_log      log_ret log_ret_perc      Volume
# 1718 1718 2022-12-29 16552.32 16651.76 16508.68 16642.34  16642.34       89.76953      9.719705  0.005408645    0.5408645 14472237479
# 1719 1719 2022-12-30 16641.33 16643.43 16408.47 16602.59  16602.59      -39.75586      9.717314 -0.002391696   -0.2391696 15929162910
# 1720 1720 2022-12-31 16603.67 16628.99 16517.52 16547.50  16547.50      -55.08984      9.713990 -0.003323666   -0.3323666 11239186456
y <- na.rm(Data_df$log_ret_perc)
T <- length(y)
GARCH_sigma0 <- (1/T)*sum(y^2)
show(GARCH_sigma0)
# 14.01651
#
# 2) the value $\hat{\sigma}_{0}^{2}$ can be chosen as the variance of the first ten observations;
# $\hat{\sigma}_{0}^{2}=\frac{1}{10}\sum_{t=1}^{10}Z_{t}^2\left(\omega\right)$.
# In our code chunk,
GARCH_sigma0 <- (1/10)*sum(y[1:10]^2)
show(GARCH_sigma0)
# 24.39334
#
# 3) the value $\hat{\sigma}_{0}^{2}$ can be chosen as the square of the first observation;
# $\hat{\sigma}_{0}^{2}=Z_{1}\left(\omega\right)^2
GARCH_sigma0 <- y[1]^2
show(GARCH_sigma0)
# 10.58587
#
# 4) the value $\hat{\sigma}_{0}^{2}$ can be chosen as the exponential smoothing backcast with parameter $\lambda=0.7$;
# $\hat{\sigma}_{0}^{2}=\lambda^{T}\frac{1}{T}\sum_{t=1}^{T}Z_{t}^2\left(\omega\right)
#                       +(1-\lambda)\sum_{t=0}^{T-1}\lambda^{t}Z_{t+1}^2\left(\omega\right)$
# In our code chunk,
GARCH_sigma0 <- ((0.7)^T)*(1/T)*sum(y^2)+(1-0.7)*sum((0.7)^(0:(T-1))*y^2)
show(GARCH_sigma0)
# 15.23773
#
# 5) the value $\hat{\sigma}_{0}^{2}$ can be chosen as the tong run variance;
# $\hat{\sigma}_{0}^{2}=\frac{a_{0}}{1-\left(a_{1}+b_{1}\right)}
# In our code chunk,
GARCH_sigma0 <- a0/(1-(a1+b1))
show(GARCH_sigma0)
# 15.14527
#
# 6) the value $\hat{\sigma}_{0}^{2}$ becomes an additional parameter with respect to which the log-likelihood is optimized;
#
# 7) the smoothing parameter $\lambda$ becomes an additional parameter with respect to which the log-likelihood is optimized.
#
# Now, we have
GARCH_1_1_resid <- GARCH_1_1[["residuals"]]
head(GARCH_1_1_resid, 20)
# [1]         NA  0.4144350  1.7419601  0.1388852 -0.2739446  0.3934785  2.3160671 -2.1750189  0.9879875 -0.6733973  0.8577650  0.1677307
# [13] -0.4519716 -0.3269755  0.3293429  1.4429839 -0.1135800  0.4322909 -0.5793139 -0.8429184
#
# Then, we write the procedure to generate the path of the conditional variance from the initial value $\hat{\sigma}_{0}^{2}$.
# Consider that, due to the way R indexes vectors, we have to shift the starting time from t=0 to t=1.
GARCH_1_1_cond_var_est <- vector(mode="numeric", length=T)
GARCH_1_1_cond_var_est[1] <- a0/(1-(a1+b1))
for(t in 2:T){
  GARCH_1_1_cond_var_est[t] <- a0 + a1*y[t-1]^2 + b1*GARCH_1_1_cond_var_est[t-1]
}
head(GARCH_1_1_cond_var_est,20)
# [1] 15.14527 14.73114 13.65772 16.30647 14.75636 13.54930 12.64325 17.86522 23.69050 22.92115 21.12997 20.12000 17.92594 16.39670 14.96147
# [16] 13.76536 15.23439 13.86101 12.94519 12.34884
#
# From this, we obtain the path of the conditional standard deviation.
head(sqrt(GARCH_1_1_cond_var_est),20)
# [1] 3.891692 3.838116 3.695635 4.038127 3.841400 3.680937 3.555735 4.226727 4.867289 4.787604 4.596735 4.485532 4.233903 4.049284 3.868006
# [16] 3.710170 3.903125 3.723038 3.597943 3.514091
#
# In the end, we obtain the standardized residuals.
GARCH_1_1_stand_res <- y/sqrt(GARCH_1_1_cond_var_est)
head(GARCH_1_1_stand_res,20)
# [1]  0.8360355  0.4144350  1.7419601  0.1388852 -0.2739446  0.3934785  2.3160671 -2.1750189  0.9879875 -0.6733973  0.8577650  0.1677307
# [13] -0.4519716 -0.3269755  0.3293429  1.4429839 -0.1135800  0.4322909 -0.5793139 -0.8429184
#
# To be compared with
head(GARCH_1_1[["residuals"]],20)
# [1]         NA  0.4144350  1.7419601  0.1388852 -0.2739446  0.3934785  2.3160671 -2.1750189  0.9879875 -0.6733973  0.8577650  0.1677307
# [13] -0.4519716 -0.3269755  0.3293429  1.4429839 -0.1135800  0.4322909 -0.5793139 -0.8429184
#
# Actually, we have
identical(GARCH_1_1_stand_res[2:T],GARCH_1_1[["residuals"]][2:T])
# TRUE
#
# Therefore the vector GARCH_1_1[["residuals"]] yields the standardized residuals of the model. 
# Note that the tseries::garch() function also yields the sequence of the conditional standard deviation. In fact, we have
head(GARCH_1_1[["fitted.values"]], 20)
#           sigt     -sigt
#  [1,]       NA        NA
#  [2,] 3.838116 -3.838116
#  [3,] 3.695635 -3.695635
#  [4,] 4.038127 -4.038127
#  [5,] 3.841400 -3.841400
#  [6,] 3.680937 -3.680937
#  [7,] 3.555735 -3.555735
#  [8,] 4.226727 -4.226727
#  [9,] 4.867289 -4.867289
# [10,] 4.787604 -4.787604
# [11,] 4.596735 -4.596735
# [12,] 4.485532 -4.485532
# [13,] 4.233903 -4.233903
# [14,] 4.049284 -4.049284
# [15,] 3.868006 -3.868006
# [16,] 3.710170 -3.710170
# [17,] 3.903125 -3.903125
# [18,] 3.723038 -3.723038
# [19,] 3.597943 -3.597943
# [20,] 3.514091 -3.514091
#
# From which,
identical(sqrt(GARCH_1_1_cond_var_est)[2:T], GARCH_1_1[["fitted.values"]][2:T,1])
# TRUE
#
# Note that the GARCH(1,1) model is estimated by the function tseries::garch() under the assumption that the innovation 
# $\left(W_{t}\right)_{t=1}^{T}\equiv W$ is a standard Gaussian strong white noise. In fact, the model is estimated using a Gaussian 
# likelihood function. Consequently, the $68.3\%$ of the realizations $\left(W_{t}\left(\omega\right)\right)_{t=1}^{T}$ of the innovation 
# $W$ are expected in the interval $\left[-1,1\right]$. It follows that the $68.3\%$ of the states of the process 
# $\left(Z_{t}\right)_{t=1}^{T}$ are expected in the interval $\left[-\sigma_{t},\sigma_{t}\right]$. In our code chunk this interval is 
# given by GARCH_1_1[["fitted.values"]][2:T].
#
# To validate the model, we need to check the standardized residuals. We start with plotting them.
head(BTC_train_df, 3)
#   t       Date    Open    High     Low   Close Adj.Close Adj.Close_diff Adj.Close_log    log_ret log_ret_perc     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09             NA      8.974883         NA           NA 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42       261.3301      9.007418 0.03253593     3.253593 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31       130.8896      9.023325 0.01590650     1.590650 7063209984
tail(BTC_train_df, 3)
#         t       Date     Open     High      Low    Close Adj.Close Adj.Close_diff Adj.Close_log      log_ret log_ret_perc      Volume
# 1718 1718 2022-12-29 16552.32 16651.76 16508.68 16642.34  16642.34       89.76953      9.719705  0.005408645    0.5408645 14472237479
# 1719 1719 2022-12-30 16641.33 16643.43 16408.47 16602.59  16602.59      -39.75586      9.717314 -0.002391696   -0.2391696 15929162910
# 1720 1720 2022-12-31 16603.67 16628.99 16517.52 16547.50  16547.50      -55.08984      9.713990 -0.003323666   -0.3323666 11239186456
#
BTC_train_df <- add_column(BTC_train_df, GARCH_1_1_stand_res=c(NA,GARCH_1_1[["residuals"]]), .after="log_ret_perc")
head(BTC_train_df, 3)
#   t       Date    Open    High     Low   Close Adj.Close Adj.Close_diff Adj.Close_log    log_ret log_ret_perc GARCH_1_1_stand_res     Volume
# 1 1 2018-04-17 8071.66 8285.96 7881.72 7902.09   7902.09             NA      8.974883         NA           NA                  NA 6900879872
# 2 2 2018-04-18 7944.43 8197.80 7886.01 8163.42   8163.42       261.3301      9.007418 0.03253593     3.253593                  NA 6529909760
# 3 3 2018-04-19 8159.27 8298.69 8138.78 8294.31   8294.31       130.8896      9.023325 0.01590650     1.590650            0.414435 7063209984
Data_df <- BTC_train_df
head(Data_df)
Data_df <- dplyr::rename(Data_df, x=t, y=GARCH_1_1_stand_res)
head(Data_df)
TrnS_length <- length(na.rm(Data_df$y[which(Data_df$Date<as.Date("2023-01-01"))]))
show(TrnS_length)
# 1718
First_Day <- as.character(Data_df$Date[3])
Last_Day <- as.character(Data_df$Date[(TrnS_length+2)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Standardized Residuals of the tseries::garch() Fitted GARCH(1,1) Model for the Bitcoin Daily Logarithm Return Percentage - Training Set - from ", .(First_Day), " to ", .(Last_Day), sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points. Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("")
# numbers::primeFactors(TrnS_length-2)
x_breaks_num <- 44 # (deduced from primeFactors(TrnS_length-2))
x_breaks_low <- Data_df$x[3]
x_breaks_up <- Data_df$x[(TrnS_length+2)]
x_binwidth <- ceiling((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Date[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("standardized residuals (US $)")
y_max <- max(na.rm(Data_df$y))
y_min <- min(na.rm(Data_df$y))
as.numeric(floor(y_max-y_min))
y_breaks_num <- 10
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor((y_min/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((y_max/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth), digits=3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.5
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
point_black <- bquote("GARCH(1,1) stand. residuals")
line_green <- bquote("regression line")
line_red <- bquote("LOESS curve")
leg_point_labs <- c(point_black)
leg_point_cols <- c("point_black"="black")
leg_point_breaks <- c("point_black")
leg_line_labs <- c(line_green, line_red)
leg_line_cols <- c("line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_green", "line_red")
leg_col_labs   <- c(leg_point_labs,leg_line_labs)
leg_col_cols   <- c(leg_point_cols,leg_line_cols)
leg_col_breaks <- c(leg_point_breaks,leg_line_breaks)
BTC_GARCH_1_1_stand_res_TrnS_sp <- ggplot(Data_df, aes(x=x)) +
  geom_smooth(aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_point(aes(y=y, color="point_black"), alpha=1, size=0.7, shape=19, na.rm=TRUE) + 
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  guides(colour=guide_legend(override.aes=list(shape=c(19, NA, NA), linetype=c("blank", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_GARCH_1_1_stand_res_TrnS_sp)
#
# The line plot
BTC_GARCH_1_1_stand_res_TrnS_lp <- ggplot(Data_df, aes(x=x)) +
  geom_smooth(aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_line(aes(y=y, color="point_black"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_GARCH_1_1_stand_res_TrnS_lp)
#
# We superimpose the conditional standard deviation of the tseries::garch() fitted GARCH(1,1) model to the plots of the Bitcoin daily logarithm return percentage.
# The scatter plot.
head(BTC_train_df)
tail(BTC_train_df)
BTC_train_df <- add_column(BTC_train_df, GARCH_1_1_cond_stand_dev=c(NA,GARCH_1_1[["fitted.values"]][,1]), .after="GARCH_1_1_stand_res")
head(BTC_train_df)
Data_df <- BTC_train_df
head(Data_df)
Data_df <- dplyr::rename(Data_df, x=t, y=log_ret_perc, z=GARCH_1_1_cond_stand_dev)
head(Data_df)
tail(Data_df)
First_Day <- as.character(Data_df$Date[3])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Standardized Residuals and Conditional Standard Deviation of the tseries::garch() Fitted GARCH(1,1) Model for the Bitcoin Daily Logarithm Return Percentage - Training Set - from ", .(First_Day), " to ", .(Last_Day), sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points. Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("")
# numbers::primeFactors(TrnS_length-2)
x_breaks_num <- 44 # (deduced from primeFactors(TrnS_length-2))
x_breaks_low <- Data_df$x[3]
x_breaks_up <- Data_df$x[nrow(Data_df)]
x_binwidth <- ceiling((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Date[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("standardizedd residuals (US $)")
y_max <- max(na.rm(Data_df$y))
y_min <- min(na.rm(Data_df$y))
as.numeric(floor(y_max-y_min))
y_breaks_num <- 10
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor((y_min/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((y_max/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth), digits=3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.5
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
point_black <- bquote("GARCH(1,1) stand. residuals")
leg_point_labs <- c(point_black)
leg_point_cols <- c("point_black"="black")
leg_point_breaks <- c("point_black")
line_magenta <- bquote("GARCH(1,1) cond. stand. dev.")
line_green <- bquote("regression line")
line_red <- bquote("LOESS curve")
leg_line_labs <- c(line_magenta, line_green, line_red)
leg_line_cols <- c("line_magenta"="magenta", "line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_magenta", "line_green", "line_red")
leg_col_labs   <- c(leg_point_labs,leg_line_labs)
leg_col_cols   <- c(leg_point_cols,leg_line_cols)
leg_col_breaks <- c(leg_point_breaks,leg_line_breaks)
BTC_perc_log_ret_GARCH_1_1_cond_stand_dev_TrnS_sp <- ggplot(Data_df, aes(x=x)) +
  geom_smooth(aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_point(aes(y=y, color="point_black"), alpha=1, size=0.7, shape=19, na.rm=TRUE) + 
  geom_line(aes(y=z, color="line_magenta"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  geom_line(aes(y=-z, color="line_magenta"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  guides(colour=guide_legend(override.aes=list(shape=c(19, NA, NA, NA), linetype=c("blank", "solid", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_perc_log_ret_GARCH_1_1_cond_stand_dev_TrnS_sp)
#
# The line plot.
line_black <- bquote("perc. log. returns")
line_magenta <- bquote("GARCH(1,1) cond. stand. dev.")
line_green <- bquote("regression line")
line_red <- bquote("LOESS curve")
leg_line_labs <- c(line_black, line_magenta, line_green, line_red)
leg_line_cols <- c("line_black"="black", "line_magenta"="magenta", "line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_black", "line_magenta", "line_green", "line_red")
BTC_perc_log_ret_GARCH_1_1_cond_stand_dev_TrnS_lp <- ggplot(Data_df, aes(x=x)) +
  geom_smooth(aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_line(aes(y=y, color="line_black"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  geom_line(aes(y=z, color="line_magenta"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  geom_line(aes(y=-z, color="line_magenta"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="", labels=leg_line_labs, values=leg_line_cols, breaks=leg_line_breaks) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_perc_log_ret_GARCH_1_1_cond_stand_dev_TrnS_lp)
# 
# Note that we have
length(which(abs(BTC_train_df$log_ret_perc[-c(1,2)])<BTC_train_df$GARCH_1_1_cond_stand_dev[-c(1,2)]))/length(BTC_train_df$log_ret_perc[-c(1,2)])
# 0.782305
# This shows that more than the $68.3\%$ of the states of the process $\left(Z_{t}\right)_{t=1}^{T}$ are actually in the interval 
# $\left[-\sigma_{t},\sigma_{t}\right]$
# 
# As we mentioned above, the GARCH(1,1) model is estimated by the function tseries::garch() under the assumption that the innovation 
# $\left(W_{t}\right)_{t=1}^{T}\equiv W$ is a standard Gaussian strong white noise. Therefore, we should expect that the standardized 
# residuals of the GARCH(1,1) model for the Bitcoin daily logarithm return percentage satisfy the following conditions:
# 1) the standardized residuals are stationary at the $5\%$ significance level, at least;
# 2) the standardized residuals have mean zero and variance one at the $5\%$ significance level, at least;
# 3) the standardized residuals are uncorrelated at the $5\%$ significance level, at least; 
# 4) the standardized residuals are homoscedastic at the $5\%$ significance level, at least; 
# 5) the standardized residuals are Gaussian at the $5\%$ significance level, at least.
#
# From the scatter and line plot, we have clear visual evidence for stationarity in the standardized residuals of the GARCH(1,1) model.
# Hence, we apply to the standardized residuals the ADF test with 0 lags, that is, the DF test, and consider the alternative hypothesis 
# of type "none" (no drift and no linear trend).
Data_df <- BTC_train_df
y <- na.rm(Data_df$GARCH_1_1_stand_res)
l <- 0
y_ADF_ur.df_none_0_lags <- ur.df(y, type="none", lags=l, selectlags="Fixed")
summary(y_ADF_ur.df_none_0_lags)
############################################### 
# Augmented Dickey-Fuller Test Unit Root Test # 
############################################### 
# Test regression none 
# Call: lm(formula=z.diff ~ z.lag.1 - 1)
# 
# Residuals: Min        1Q     Median     3Q      Max 
#          -12.6804  -0.4062   0.0256   0.4484   5.7180 
# 
# Coefficients: Estimate   Std. Error t value Pr(>|t|)    
#       z.lag.1 -0.99597    0.02414  -41.26   <2e-16 ***
#   ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.001 on 1716 degrees of freedom
# Multiple R-squared:  0.498,	Adjusted R-squared:  0.4977 
# F-statistic:  1702 on 1 and 1716 DF,  p-value: < 2.2e-16
# 
# Value of test-statistic is: -41.2597 
# Critical values for test statistics: 1pct  5pct 10pct
#                                tau1 -2.58 -1.95 -1.62
# 
# From the DF test we have a rejection of the null hypothesis of a unit root at the $1\%$ significance level.
# Moreover, considering the residuals of the linear model used for the DF test, we have
y_res <- as.vector(y_ADF_ur.df_none_0_lags@testreg[["residuals"]])
head(y_res, 20)
# 1.7403058  0.1318592 -0.2745083  0.3945865  2.3144850 -2.1844087  0.9967936 -0.6774021  0.8605014  0.1642729 -0.4526543 -0.3251546
# 0.3306633  1.4416539 -0.1194029  0.4327498 -0.5810568 -0.8405774 -0.4208144  0.2877424
n_obs <- nobs(lm(formula=y_ADF_ur.df_none_0_lags@testreg[["terms"]]))
show(n_obs)
# 1717
max_lag <- ceiling(min(10, n_obs/4))    # Hyndman (for data without seasonality)
show(max_lag)
# 10
n_coeffs <- nrow(y_ADF_ur.df_trend_0_lags@testreg[["coefficients"]])
show(n_coeffs)
# 3
n_pars <- n_coeffs
show(n_pars)
# 3
y_res_LB <- Box.test(y_res, lag=max_lag, fitdf=n_pars, type="Ljung-Box")
show(y_res_LB)
# Box-Ljung test
# data:  y_res
# X-squared = 8.4728, df = 7, p-value = 0.2928
#
# The null hypothesis of no autocorrelation in the residuals of the linear model used for the DF test cannot be rejected at the $10\%$ 
# significance level. This validates the rejection of the null hypothesis of a unit root in the residuals of the tseries::garch() fitted 
# GARCH(1,1) model for the Bitcoin daily logarithm return percentage. The execution of the robust autocorrelation test confirms this 
# finding.
testcorr::ac.test(y_res, max.lag = 10, alpha = 0.10, lambda = 2.576, plot = TRUE, table = TRUE, var.name = NULL, scale.font = 1)
# Tests for zero autocorrelation of x
#   | Lag|     AC|  Stand. CB(90%)|  Robust CB(90%)| Lag|      t| p-value| t-tilde| p-value| Lag|    LB| p-value| Q-tilde| p-value|
#   |---:|------:|---------------:|---------------:|---:|------:|-------:|-------:|-------:|---:|-----:|-------:|-------:|-------:|
#   |   1| -0.001| (-0.040, 0.040)| (-0.037, 0.037)|   1| -0.025|   0.980|  -0.027|   0.979|   1| 0.001|   0.980|   0.001|   0.979|
#   |   2|  0.028| (-0.040, 0.040)| (-0.035, 0.035)|   2|  1.152|   0.249|   1.295|   0.195|   2| 1.330|   0.514|   1.679|   0.432|
#   |   3|  0.022| (-0.040, 0.040)| (-0.033, 0.033)|   3|  0.916|   0.360|   1.115|   0.265|   3| 2.172|   0.537|   2.578|   0.461|
#   |   4|  0.035| (-0.040, 0.040)| (-0.051, 0.051)|   4|  1.437|   0.151|   1.114|   0.265|   4| 4.245|   0.374|   3.819|   0.431|
#   |   5|  0.024| (-0.040, 0.040)| (-0.038, 0.038)|   5|  1.008|   0.314|   1.048|   0.295|   5| 5.265|   0.384|   4.917|   0.426|
#   |   6|  0.009| (-0.040, 0.040)| (-0.035, 0.035)|   6|  0.364|   0.716|   0.407|   0.684|   6| 5.398|   0.494|   5.083|   0.533|
#   |   7| -0.011| (-0.040, 0.040)| (-0.045, 0.045)|   7| -0.459|   0.646|  -0.405|   0.685|   7| 5.610|   0.586|   5.247|   0.630|
#   |   8| -0.024| (-0.040, 0.040)| (-0.037, 0.037)|   8| -1.007|   0.314|  -1.087|   0.277|   8| 6.629|   0.577|   6.429|   0.599|
#   |   9|  0.021| (-0.040, 0.040)| (-0.035, 0.035)|   9|  0.871|   0.384|   0.979|   0.327|   9| 7.392|   0.596|   7.388|   0.597|
#   |  10|  0.025| (-0.040, 0.040)| (-0.036, 0.036)|  10|  1.036|   0.300|   1.143|   0.253|  10| 8.473|   0.583|   8.694|   0.561|
#
# We consider also the KPSS test with null hypothesis of type "mu"
Data_df <- BTC_train_df
y <- na.rm(Data_df$GARCH_1_1_stand_res)
l <- 0
y_KPSS_ur.kpss_mu_0_lags <- ur.kpss(y, type="mu", use.lag=0)
summary(y_KPSS_ur.kpss_mu_0_lags)
####################### 
# KPSS Unit Root Test # 
####################### 
# Test is of type: mu with 0 lags. 
# 
# Value of test-statistic is: 0.2375 
# 
# Critical value for a significance level of: 10pct  5pct 2.5pct  1pct
#                             critical values 0.347 0.463  0.574 0.739
#
# In this case we cannot reject the null hypothesis that the standardized residuals are stationary at the $1\%$ significance level.
# Moreover, considering the residuals of the linear model used for the KPSS test, we have 
y_res <- as.vector(y_KPSS_ur.kpss_mu_0_lags@res)
head(y_res, 20)
#  0.4047390  1.7322756  0.1291854 -0.2836504  0.3837788  2.3063703 -2.1847662  0.9783152 -0.6831180  0.8480818  0.1580315 -0.4616800
# -0.3366807  0.3196416  1.4332804 -0.1232832  0.4225891 -0.5890150 -0.8526172 -0.4339178
#
# and
y_res_LB <- Box.test(y_res, lag=max_lag, fitdf=2, type="Ljung-Box")
show(y_res_LB)
# Box-Ljung test
# data:  y_res
# X-squared=8.6429, df=8, p-value=0.3733
#
# Also in this case, we cannot reject the null hypothesis that the residuals of the linear model used for the KPSS test are uncorrelated at 
# the $10\%$ significance level. This validates the non rejection of the null hypothesis of stationarity from the KPSS test.
#
# Now, we consider whether we can assess that the standardized residuals have mean zero and variance one at the $5\%$ significance level. 
# Since we have not checked yet the residuals empirical distribution we apply non parametric tests.
Data_df <- BTC_train_df
y <- na.rm(Data_df$GARCH_1_1_stand_res)
# We have
show(c(mean(y), var(y)))
# 0.00970241 1.00110974
#
# We wonder whether the mean is significantly different from zero. To this we determine the bootstrapped confidence intervals.
d <- data.frame(k=1:length(y), y=y)
head(d)
boot_mean <- function(d, k){
  d2 <- d[k,]
  return(mean(d2$y))
}
boot_mean(d)
# 0.00970241
# change or turn off set.seed() if you want the results to vary
set.seed(12345)
booted_mean <- boot(d, boot_mean, R=5000)
show(booted_mean)
# ORDINARY NONPARAMETRIC BOOTSTRAP
# Call: boot(data=d, statistic=boot_mean, R=5000)
# Bootstrap Statistics :
#     original        bias     std. error
# t1* 0.00970241 -1.220604e-05  0.02462959
#
summary(booted_mean)
#    R   original      bootBias     bootSE    bootMed
# 1 5000 0.0097024   -1.2206e-05    0.02463  0.0098051
#
booted_mean.ci <- boot.ci(boot.out=booted_mean, conf=0.80, type=c("norm", "basic", "perc", "bca"))
show(booted_mean.ci)
# 
# BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
# Based on 5000 bootstrap replicates
# CALL:  boot.ci(boot.out=booted_mean, conf=0.8, type=c("norm", "basic", "perc", "bca"))
# Intervals: Level      Normal                 Basic         
#             80%   (-0.0218,  0.0413)   (-0.0213,  0.0418)  
#            Level     Percentile               BCa          
#             80%   (-0.0224,  0.0407)   (-0.0229,  0.0403)  
# Calculations and Intervals on Original Scale
#
# Alternatively,
set.seed(12345)
DescTools::BootCI(x=y, y=NULL, FUN=mean, na.rm=TRUE, conf.level=0.80, bci.method="norm", R=5000) 
#     mean      lwr.ci      upr.ci 
# 0.00970241 -0.02184947  0.04127871
set.seed(12345)
DescTools::BootCI(x=y, y=NULL, FUN=mean, na.rm=TRUE, conf.level=0.80, bci.method="basic", R=5000) 
#     mean      lwr.ci      upr.ci 
# 0.00970241 -0.02133602  0.04180539
set.seed(12345)
DescTools::BootCI(x=y, y=NULL, FUN=mean, na.rm=TRUE, conf.level=0.80, bci.method="perc", R=5000) 
#     mean      lwr.ci      upr.ci 
# 0.00970241 -0.02240057  0.04074084 
set.seed(12345)
DescTools::BootCI(x=y, y=NULL, FUN=mean, na.rm=TRUE, conf.level=0.80, bci.method="bca", R=5000) 
#    mean       lwr.ci      upr.ci 
# 0.00970241 -0.02292493  0.04028959
#
# In light of the bootstrapped confidence intervals, we cannot reject the null hypothesis that the true value of the mean of the residuals 
# of the GARCH(1,1) model for the daily logarithm return percentage training set is zero at the $20\%$ significance level. 
#
boot_var <- function(d, k){
  d2 <- d[k,]
  return(var(d2$y))
}
boot_var(d)
# 1.00111
# change or turn off set.seed() if you want the results to vary
set.seed(12345)
booted_var <- boot(d, boot_var, R=5000)
show(booted_var)
# ORDINARY NONPARAMETRIC BOOTSTRAP
# Call: boot(data=d, statistic=boot_var, R=5000)
# Bootstrap Statistics :
#     original      bias      std. error
# t1* 1.00111   0.0007139666   0.1062945
#
summary(booted_var)
#    R   original  bootBias     bootSE    bootMed
# 1 5000 1.0011   0.00071397    0.10629    0.9897
#
booted_var.ci <- boot.ci(boot.out=booted_var, conf=0.80, type=c("norm", "basic", "perc", "bca"))
show(booted_var.ci)
# 
# BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
# Based on 5000 bootstrap replicates
# CALL:  boot.ci(boot.out=booted_mean, conf=0.8, type=c("norm", "basic", "perc", "bca"))
# Intervals: Level      Normal            Basic         
#             80%   (0.864,  1.137)   (0.864,  1.128)  
#            Level     Percentile          BCa          
#             80%   (0.874,  1.138)   (0.901,  1.208)  
# Calculations and Intervals on Original Scale
#
# Alternatively,
set.seed(12345)
DescTools::BootCI(x=y, y=NULL, FUN=var, na.rm=TRUE, conf.level=0.80, bci.method="norm", R=5000) 
#    var      lwr.ci      upr.ci 
# 1.0011097  0.8641739   1.1366177 
set.seed(12345)
DescTools::BootCI(x=y, y=NULL, FUN=var, na.rm=TRUE, conf.level=0.80, bci.method="basic", R=5000) 
#    var      lwr.ci      upr.ci 
# 1.0011097  0.8639103   1.1279049
set.seed(12345)
DescTools::BootCI(x=y, y=NULL, FUN=var, na.rm=TRUE, conf.level=0.80, bci.method="perc", R=5000) 
#    var      lwr.ci     upr.ci 
# 1.0011097  0.8743146  1.1383092
set.seed(12345)
DescTools::BootCI(x=y, y=NULL, FUN=var, na.rm=TRUE, conf.level=0.80, bci.method="bca", R=5000) 
#    var      lwr.ci      upr.ci 
# 1.0011097  0.9013402  1.2076123
#
# In light of the bootstrapped confidence intervals, we cannot reject the null hypothesis that the true value of the variance of the 
# residuals of the GARCH(1,1) model for the daily logarithm return percentage training set is zero at the $20\%$ significance level. 
#
# We check the possible autocorrelation.
# Autocorrelogram of the standardized residuals of the GARCH(1,1) model for the Bitcoin daily logarithm return percentage training set.
Data_df <- BTC_train_df
y <- na.rm(Data_df$GARCH_1_1_stand_res)
length <- length(y)
T <- length(y)
# max_lag <- ceiling(10*log10(T))    # Default
# max_lag <- ceiling(sqrt(n)+45)     # Box-Jenkins
max_lag <- ceiling(min(10,T/4))      # Hyndman (for data without seasonality)
# max_lag <- ceiling(min(2*12,T/5))  # Hyndman (for data with seasonality, see https://robjHyndman.com/hy_resndsight/ljung-box-test/)
# Aut_Fun_y <- stats::acf(y, lag.max=max_lag, type="correlation", plot=FALSE)
Aut_Fun_y <- TSA::acf(y, lag.max=max_lag, type="correlation", plot=TRUE)
ci_090 <- qnorm((1+0.90)/2)/sqrt(T)
ci_95 <- qnorm((1+0.95)/2)/sqrt(T)
ci_99 <- qnorm((1+0.99)/2)/sqrt(T)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
First_Day <- as.character(Data_df$Date[1])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Autocorrelogram of the Standardized Residuals of the tseries::garch() Fitted GARCH(1,1) Model for the Bitcoin Daily Logarithm Return Percentage - Training Set - from ", .(First_Day), " to ", .(Last_Day), sep="")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points,  ", "lags ", .(max_lag), ".  Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("lags")
x_breaks_num <- max_lag
x_binwidth <- 1
x_breaks <- Aut_Fun_y_df$lag
x_labs <- format(x_breaks, scientific=FALSE)
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), lwd=1, col="black") +
  geom_hline(aes(yintercept=-ci_090, color="CI_090"), lwd=0.9, lty=3, show.legend=TRUE) +
  geom_hline(aes(yintercept=ci_090, color="CI_090"), lwd=0.9, lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), lwd=0.8, lty=2, show.legend=TRUE) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lwd=0.8, lty=2) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), lwd=0.8, lty=4, show.legend=TRUE) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lwd=0.8, lty=4) +
  scale_x_continuous(name= "lag", breaks=x_breaks, label=x_labs) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis=sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"), values=c(CI_090="green", CI_95="blue", CI_99="red"),
                     guide=guide_legend(override.aes=list(linetype=c("dotted", "dashed", "dotdash")))) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=0, vjust=1, hjust=0.5),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
#
# The autocorrelogram provides visual evidence for the lack of autocorrelation at the $10\%$ significance level.
#
# Partial autocorrelogram of the standardized residuals of the tseries::garch() fitted GARCH(1,1) model for the Bitcoin daily percentage 
# logarithm returns training
# set.
Data_df <- BTC_train_df
y <- na.rm(Data_df$GARCH_1_1_stand_res)
length <- length(y)
T <- length(y)
# max_lag <- ceiling(10*log10(T))    # Default
# max_lag <- ceiling(sqrt(n)+45)     # Box-Jenkins
max_lag <- ceiling(min(10,T/4))      # Hyndman (for data without seasonality)
# max_lag <- ceiling(min(2*12,T/5))  # Hyndman (for data with seasonality, see https://robjHyndman.com/hy_resndsight/ljung-box-test/)
Part_Aut_Fun_y <- stats::pacf(y, lag.max=max_lag, type="correlation", plot=FALSE)
ci_090 <- qnorm((1+0.90)/2)/sqrt(T)
ci_95 <- qnorm((1+0.95)/2)/sqrt(T)
ci_99 <- qnorm((1+0.99)/2)/sqrt(T)
Plot_Part_Aut_Fun_y <- data.frame(lag=Part_Aut_Fun_y$lag, pacf=Part_Aut_Fun_y$acf)
First_Day <- as.character(Data_df$Date[1])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Partial Autocorrelogram of the Standardized Residuals of the tseries::garch() Fitted GARCH(1,1) Model for the Bitcoin Daily Logarithm Return Percentage - Training Set - from ", .(First_Day), " to ", .(Last_Day), sep="")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points,  ", "lags ", .(max_lag), ".  Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("lags")
x_breaks_num <- max_lag
x_binwidth <- 1
x_breaks <- Aut_Fun_y_df$lag
x_labs <- format(x_breaks, scientific=FALSE)
ggplot(Plot_Part_Aut_Fun_y, aes(x=lag, y=pacf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=pacf), size=1, col="black") +
  geom_hline(aes(yintercept=-ci_090, color="CI_090"), lwd=0.9, lty=3, show.legend=TRUE) +
  geom_hline(aes(yintercept=ci_090, color="CI_090"), lwd=0.9, lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), lwd=0.8, lty=2, show.legend=TRUE) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lwd=0.8, lty=2) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), lwd=0.8, lty=4, show.legend=TRUE) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lwd=0.8, lty=4) +
  scale_x_continuous(name= "lag", breaks=x_breaks, label=x_labs) +
  scale_y_continuous(name="pacf value", breaks=waiver(), labels=NULL,
                     sec.axis=sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"), values=c(CI_090="green", CI_95="blue", CI_99="red"),
                     guide=guide_legend(override.aes=list(linetype=c("dotted", "dashed", "dotdash")))) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=0, vjust=1, hjust=0.5),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
#
# The partial autocorrelogram also provides visual evidence for the lack of autocorrelation at the $10\%$ significance level.
Data_df <- BTC_train_df
y <- na.rm(Data_df$GARCH_1_1_stand_res)
FitAR::LjungBoxTest(y, k=n_pars, lag.max=max_lag, StartLag=1, SquaredQ=FALSE)
# Warning message: In (ra^2)/(n - (1:lag.max)): longer object length is not a multiple of shorter object length
# m   Qm    pvalue
# 1 1.34 0.2466391
# 2 2.20 0.3331689
# 3 4.30 0.2304137
# 4 5.38 0.2500391
# 5 5.50 0.3574667
# 6 5.71 0.4564825
# 7 6.74 0.4566020
# 8 7.52 0.4820273
# 9 8.61 0.4739354
# 10 9.96 0.4439883
#
lag_seq <- seq(from=1, to=max_lag, by=1)
y_LB_portes <- portes::LjungBox(y, lags=lag_seq, fitdf=n_pars, sqrd.res=FALSE)
show(y_LB_portes)
# lags statistic df  p-value
# 1 0.02678663  1 0.8699939
# 2 1.36980990  2 0.5041381
# 3 2.22627927  3 0.5267907
# 4 4.33371173  4 0.3627227
# 5 5.41476737  5 0.3673825
# 6 5.53436940  6 0.4773125
# 7 5.73981616  7 0.5704368
# 8 6.76925181  8 0.5617211
# 9 7.54802577  9 0.5802549
# 10 8.64274268 10 0.5663105
#
Box_test_ABS_ls <- list()
for(l in 1:max_lag){
  LB_fit_df <- min(min(l, n_pars), l-1)
  show(Box.test(y, lag=l, fitdf=LB_fit_df, type="Ljung-Box"))
 }
# Box-Ljung test
# data:  y
# X-squared=0.026787, df=1, p-value=0.87
# X-squared=1.3698,   df=2, p-value=0.5041
# X-squared=2.2263,   df=3, p-value=0.5268
# X-squared=4.3337,   df=4, p-value=0.3627
# X-squared=5.4148,   df=5, p-value=0.3674
# X-squared=5.5344,   df=6, p-value=0.4773
# X-squared=5.7398,   df=7, p-value=0.5704
# X-squared=6.7693,   df=8, p-value=0.5617
# X-squared=7.548,    df=9, p-value=0.5803
# X-squared=8.6427,  df=10, p-value=0.5663
#
# The Ljung-Box test confirms the lack of autocorrelation at the $10\%$ significance level.
#
# We also consider the Breusch-Godfrey test for autocorrelation.
# Note that the Breusch-Godfrey test test applies on the residuals from a linear regression.
# Therefore, we can apply it on the trivial linear regression of the standardized residuals of the GARCH(1,1) model for the Bitcoin daily 
# logarithm return percentage training set.
Data_df <- BTC_train_df
y <- na.rm(Data_df$GARCH_1_1_stand_res)
triv_y_lm <- lm(y~0)
# Note that
identical(y,as.vector(triv_y_lm[["residuals"]]))
# TRUE
#
y_BG_Chisq <- lmtest::bgtest(triv_y_lm, order=10, type="Chisq")
show(y_BG_Chisq)
# Breusch-Godfrey test for serial correlation of order up to 10
# data:  triv_y_lm
# LM test=8.6728, df=10, p-value=0.5634
#
y_BG_F <- lmtest::bgtest(triv_y_lm, order=10, type="F")
show(y_BG_F)
# Breusch-Godfrey test for serial correlation of order up to 7
# data:  triv_y_lm
# LM test=0.86661, df1=10, df2=1708, p-value=0.5642
#
# The Breusch-Godfrey test does not allow to reject the null hypothesis that the standardized residuals of the GARCH(1,1) model for the 
# Bitcoin daily logarithm return percentage training set are uncorrelated at the $10\%$ significance level 

# It is interesting to note that considering the absolute standardized residuals, we obtain
Data_df <- BTC_train_df
y <- na.rm(abs(Data_df$GARCH_1_1_stand_res))
length <- length(y)
T <- length(y)
# max_lag <- ceiling(10*log10(T))    # Default
# max_lag <- ceiling(sqrt(n)+45)     # Box-Jenkins
max_lag <- ceiling(min(10,T/4))      # Hyndman (for data without seasonality)
# max_lag <- ceiling(min(2*12,T/5))  # Hyndman (for data with seasonality, see https://robjhyndman.com/hyndsight/ljung-box-test/)
# Aut_Fun_y <- stats::acf(y, lag.max=max_lag, type="correlation", plot=FALSE)
Aut_Fun_y <- TSA::acf(y, lag.max=max_lag, type="correlation", plot=TRUE)
ci_090 <- qnorm((1+0.90)/2)/sqrt(T)
ci_95 <- qnorm((1+0.95)/2)/sqrt(T)
ci_99 <- qnorm((1+0.99)/2)/sqrt(T)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
First_Day <- as.character(Data_df$Date[1])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Autocorrelogram of the Absolute Standardized Residuals of the tseries::garch() Fitted GARCH(1,1) Model for the Bitcoin Daily Logarithm Return Percentage - Training Set - from ", .(First_Day), " to ", .(Last_Day), sep="")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points,  ", "lags ", .(max_lag), ".  Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("lags")
x_breaks_num <- max_lag
x_binwidth <- 1
x_breaks <- Aut_Fun_y_df$lag
x_labs <- format(x_breaks, scientific=FALSE)
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), lwd=1, col="black") +
  geom_hline(aes(yintercept=-ci_090, color="CI_090"), lwd=0.9, lty=3, show.legend=TRUE) +
  geom_hline(aes(yintercept=ci_090, color="CI_090"), lwd=0.9, lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), lwd=0.8, lty=2, show.legend=TRUE) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lwd=0.8, lty=2) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), lwd=0.8, lty=4, show.legend=TRUE) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lwd=0.8, lty=4) +
  scale_x_continuous(name= "lag", breaks=x_breaks, label=x_labs) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis=sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"), values=c(CI_090="green", CI_95="blue", CI_99="red"),
                     guide=guide_legend(override.aes=list(linetype=c("dotted", "dashed", "dotdash")))) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=0, vjust=1, hjust=0.5),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
#
# We have visual evidence for the lack of autocorrelation at the $1\%$ significance level, not at the $5\%$ significance level, though.
Data_df <- BTC_train_df
y <- na.rm(abs(Data_df$GARCH_1_1_stand_res))
length <- length(y)
T <- length(y)
# max_lag <- ceiling(10*log10(T))    # Default
# max_lag <- ceiling(sqrt(n)+45)     # Box-Jenkins
max_lag <- ceiling(min(10,T/4))      # Hyndman (for data without seasonality)
# max_lag <- ceiling(min(2*12,T/5))  # Hyndman (for data with seasonality, see https://robjHyndman.com/hy_resndsight/ljung-box-test/)
Part_Aut_Fun_y <- stats::pacf(y, lag.max=max_lag, type="correlation", plot=FALSE)
ci_090 <- qnorm((1+0.90)/2)/sqrt(T)
ci_95 <- qnorm((1+0.95)/2)/sqrt(T)
ci_99 <- qnorm((1+0.99)/2)/sqrt(T)
Plot_Part_Aut_Fun_y <- data.frame(lag=Part_Aut_Fun_y$lag, pacf=Part_Aut_Fun_y$acf)
First_Day <- as.character(Data_df$Date[1])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Partial Autocorrelogram of the Absolute Standardized Residuals of the tseries::garch() Fitted GARCH(1,1) Model for the Bitcoin Daily Logarithm Return Percentage - Training Set - from ", .(First_Day), " to ", .(Last_Day), sep="")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points,  ", "lags ", .(max_lag), ".  Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("lags")
x_breaks_num <- max_lag
x_binwidth <- 1
x_breaks <- Aut_Fun_y_df$lag
x_labs <- format(x_breaks, scientific=FALSE)
ggplot(Plot_Part_Aut_Fun_y, aes(x=lag, y=pacf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=pacf), size=1, col="black") +
  geom_hline(aes(yintercept=-ci_090, color="CI_090"), lwd=0.9, lty=3, show.legend=TRUE) +
  geom_hline(aes(yintercept=ci_090, color="CI_090"), lwd=0.9, lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), lwd=0.8, lty=2, show.legend=TRUE) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lwd=0.8, lty=2) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), lwd=0.8, lty=4, show.legend=TRUE) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lwd=0.8, lty=4) +
  scale_x_continuous(name= "lag", breaks=x_breaks, label=x_labs) +
  scale_y_continuous(name="pacf value", breaks=waiver(), labels=NULL,
                     sec.axis=sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"), values=c(CI_090="green", CI_95="blue", CI_99="red"),
                     guide=guide_legend(override.aes=list(linetype=c("dotted", "dashed", "dotdash")))) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=0, vjust=1, hjust=0.5),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
#
# We have visual evidence for autocorrelation at the $1\%$ significance level.
#
Data_df <- BTC_train_df
y <- na.rm(Data_df$GARCH_1_1_stand_res)
FitAR::LjungBoxTest(abs(y), k=n_pars, lag.max=max_lag, StartLag=1, SquaredQ=FALSE)
# Warning message: In (ra^2)/(n - (1:lag.max)): longer object length is not a multiple of shorter object length
#  m   Qm     pvalue
#  1  0.22 0.63780801
#  2  1.82 0.40243890
#  3  8.13 0.04342288
#  4  8.19 0.08478211
#  5  8.54 0.12897248
#  6 15.02 0.02011295
#  7 16.63 0.01994940
#  8 16.68 0.03358931
#  9 16.93 0.04986177
# 10 17.15 0.07110163
#
lag_seq <- seq(from=1, to=max_lag, by=1)
y_LB_portes <- portes::LjungBox(abs(y), lags=lag_seq, fitdf=n_pars, sqrd.res=FALSE)
show(y_LB_portes)
# lags  statistic df    p-value
#  1  0.2758388  1 0.59944170
#  2  0.4975883  2 0.77974046
#  3  2.0973241  3 0.55245430
#  4  8.4094103  4 0.07768119
#  5  8.4729327  5 0.13202573
#  6  8.8188192  6 0.18402687
#  7 15.3031548  7 0.03230391
#  8 16.9148511  8 0.03100804
#  9 16.9681682  9 0.04921680
# 10 17.2133319 10 0.06977477
#
Box_test_ABS_ls <- list()
for(l in 1:max_lag){
  Box_test_ls[[l]] <- Box.test(abs(y), lag=l,   fitdf=LB_fit_df, type="Ljung-Box")
  show(Box_test_ls[[l]])
}
# Box-Ljung test
# data:  abs(y)
# X-squared=0.27584, df=1, p-value=0.5994
# X-squared=0.49759, df=2, p-value=0.7797
# X-squared=2.0973,  df=3, p-value=0.5525
# X-squared=8.4094,  df=4, p-value=0.07768
# X-squared=8.4729,  df=5, p-value=0.132
# X-squared=8.8188,  df=6, p-value=0.184
# X-squared=15.303,  df=7, p-value=0.0323
# X-squared=16.915,  df=8, p-value=0.03101
# X-squared=16.968,  df=9, p-value=0.04922
# X-squared=17.213, df=10, p-value=0.06977
#
# We have computational evidence for the lack of autocorrelation at the $5\5$ significance level.
#
# Although the results of the visual and computational tests are somewhat ambiguous regarding the absolute residuals, the GARCH(1,1) model
# appears to have eliminated most of the autocorrelation in the logarithm percentage returns.
#
# We test the homoscedasticity in the standardized residuals of the GARCH(1,1) model for the Bitcoin daily logarithm return percentage
# training set.
Data_df <- BTC_train_df
head(Data_df)
y <- na.rm(abs(Data_df$GARCH_1_1_stand_res))
head(y,20)
# [1] 0.4144350 1.7419601 0.1388852 0.2739446 0.3934785 2.3160671 2.1750189 0.9879875 0.6733973 0.8577650 0.1677307 0.4519716 0.3269755 0.3293429
# [15] 1.4429839 0.1135800 0.4322909 0.5793139 0.8429184 0.4242169
#
# We start with introducing the linear model used for the Breusch-Pagan test.
x <- 1:length(y)
BP_lm <- lm(y~x)
BP_lm_res <- BP_lm[["residuals"]]
#
# We consider the possible heteroscedasticity of the residuals in the linear model used for the Breusch-Pagan test.
def_margins <- par("mar")
par(mfrow=c(2,1), mar=c(2.5,2.5,4.0,1.0))
plot(BP_lm,1)
plot(BP_lm,3)
par(mfrow=c(1,1), mar=def_margins)
#
# From the Residual vs Fitted plot, we do not have visual evidence for heteroscedasticity in the residuals. We have strong visual evidence
# for skewness, though. The LOESS curve appears to be flat and the spread of the residuals around the LOESS curve appears to be rather
# homogeneous. The visual evidence from the Scale-Location plot essentially confirms the visual evidence from the Residual vs Fitted plot:
# an almost flat horizontal LOESS curve suggests the absence of non linear forms of heteroscedasticity in the residual time series.
# We check the kurtosis of the residuals in the linear model used for the Breusch-Pagan test.
DescTools::Kurt(BP_lm_res, weights=NULL, method=2, conf.level=0.99, ci.type="classic") 
#   kurtosis    lwr.ci    upr.ci
#  42.940071 -0.304005  0.304005 
#
# The estimated value of the excess kurtosis of the standardized residuals of the estimated GARCH(1,1) model severely conflicts with a 
# possible Gaussian distribution of the residuals at the $1\%$ significance level. We proceed with other non-parametric tests
set.seed(12345)
DescTools::Kurt(BP_lm_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.95, ci.type="norm", R=5000) 
#   kurt      lwr.ci     upr.ci 
# 42.940071  2.919865  96.514725 
#
set.seed(12345)
DescTools::Kurt(BP_lm_res, weights=NULL,  na.rm=TRUE, method=2, conf.level=0.99, ci.type="norm", R=5000) 
#
#   kurt     lwr.ci    upr.ci 
# 42.94007 -11.78495 111.21954 
#
set.seed(12345)
DescTools::Kurt(BP_lm_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type="basic", R=5000) 
#    kurt     lwr.ci     upr.ci 
# 42.940071  4.599081  81.370910 
#
set.seed(12345)
DescTools::Kurt(BP_lm_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type="perc", R=5000) 
#     kurt     lwr.ci     upr.ci 
# 42.940071  4.509232  81.281062 
# 
set.seed(12345)
DescTools::Kurt(BP_lm_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type="bca", R=5000) 
#     kurt     lwr.ci    upr.ci 
# 42.940071  5.289447  92.883635 
# Warning message: In norm.inter(t, adj.alpha) : extreme order statistics used as endpoints
#
set.seed(12345)
DescTools::Kurt(BP_lm_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type="bca", R=50000)
#     kurt     lwr.ci    upr.ci 
# 42.940071  5.355628  97.828112  
# Warning message: In norm.inter(t, adj.alpha) : extreme order statistics used as endpoints
#
# The bootstrapped confidence intervals of type "norm" at the $95\%$ [resp. $99\%$] confidence level does not [resp. does] contain zero.
# Hence, we must [resp. we cannot] reject the null hypothesis of mesokurtic residuals in the linear model for the Breusch-Pagan test at the
# $5\%$ [resp. $1\%$] significance level. On the other hand, the bootstrapped confidence intervals of type "basic", "perc", and "bca" do not
# contain zero. Hence, referring to these confidence intervals, we must reject the null hypothesis of mesokurtic residuals in the linear 
# model for the Breusch-Pagan testat the $1\%$ significance level. In light of this, we execute the Breusch-Pagan and White test in the
# Koenker (studentised) modification.
#
#
lmtest::bptest(BP_lm, varformula=NULL, studentize=TRUE, data=NULL)
# studentized Breusch-Pagan test
# data:  BP_lm
# BP=0.11433, df=1, p-value=0.7353
#
#
skedastic::breusch_pagan(BP_lm, koenker=TRUE)
# statistic  p.value  parameter       method         alternative
#  0.114      0.735      1    Koenker (studentised)    greater     
#
#
olsrr::ols_test_score(BP_lm, fitted_values=TRUE, rhs=FALSE)
# Score Test for Heteroskedasticity
# ---------------------------------
#   Ho: Variance is homogenous
#   Ha: Variance is not homogenous
# 
# Variables: fitted values of y 
# Test Summary          
# -----------------------------
# DF           =   1 
# Chi2         =   0.1143323 
# Prob > Chi2  =   0.7352648  
#
# We cannot reject the null of homoscedasticity at the %10\%$ significance level. 
# We consider the White test.
#
Data_df <- BTC_train_df
y <- na.rm(abs(Data_df$GARCH_1_1_stand_res))
head(y,20)
# [1] 0.4144350 1.7419601 0.1388852 0.2739446 0.3934785 2.3160671 2.1750189 0.9879875 0.6733973 0.8577650 0.1677307 0.4519716 0.3269755 0.3293429
# [15] 1.4429839 0.1135800 0.4322909 0.5793139 0.8429184 0.4242169
x <- 1:length(y)
BP_lm <- lm(y~x)
BP_lm_res <- BP_lm[["residuals"]]
z <- BP_lm_res^2
Het_Data <- data.frame(x,y,z)
head(Het_Data)
W_lm <- lm(z~x+I(x^2))
#
lmtest::bptest(BP_lm, W_lm, studentize=TRUE, data=NULL)
# studentized Breusch-Pagan (White) test
# data: BP_lm, W_lm
# BP=1.0447, df=2, p-value=0.5931
#
#
lmtest::bptest(y~x, z~x+I(x^2), studentize=TRUE, data=Het_Data)
# studentized Breusch-Pagan (White) test
# data:  y ~ x
# BP=1.0447, df=2, p-value=0.5931
#
#
skedastic::white(BP_lm, interactions=FALSE, statonly=FALSE)
# statistic  p.value  parameter       method    alternative
#   1.04      0.593      2         White's Test   greater    
#
# library(whitestrap)
white_test(BP_lm)
# White's test results
# Null hypothesis: Homoskedasticity of the residuals
# Alternative hypothesis: Heteroskedasticity of the residuals
# Test Statistic: 1.04
# P-value: 0.59311
#
# Confirming the visual evidence of the scale location plot, the White test does not allow us to reject the null of homoscedasticity at the
# $10\%$ significance level. Due to the sounding results of the studentized Breusch-Pagan and White test, we cannot reject the null of 
# homoscedasticity at the $10\%$ significance level.
# Recall that the Breusch-Pagan test is a test for linear forms of heteroscedasticity, e.g., as y-hat goes up, the error variance goes up. 
# Even in the studentized form, the test does not work well for non-linear forms of heteroscedasticity (where error variance gets larger as
# the explanatory variable gets more extreme in either direction). The White test is more reliable for such cases 
# (see https://www3.nd.edu/~rwilliam/stats2/l25.pdf).
#
# With the goal of testing for Gaussianity of the standardized residual distribution we consider the issue of the possible skewness and
# kurtosis of the standardized residuals. First, the skewness
Data_df <- BTC_train_df
y <- na.rm(Data_df$GARCH_1_1_stand_res)
head(y,20)
# [1]  0.4144350  1.7419601  0.1388852 -0.2739446  0.3934785  2.3160671 -2.1750189  0.9879875 -0.6733973  0.8577650  0.1677307 -0.4519716
# [13] -0.3269755  0.3293429  1.4429839 -0.1135800  0.4322909 -0.5793139 -0.8429184 -0.4242169
#
DescTools::Skew(y, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type="classic") 
#      skew       lwr.ci       upr.ci 
#  -1.2404212  -0.1520906    0.1520906
#
# The estimated value of the skewness of the standardized residuals of the estimated GARCH(1,1) model conflicts with a possible Gaussian 
# distribution of the standardized residuals at the $1/%$ significance level. However, the bootstrapped confidence intervals do not allow
# us to exclude that an unskewed distribution generates the standardized residuals at the $10\%$ significance level. In fact, we have
set.seed(12345)
DescTools::Skew(y, weights=NULL, na.rm=TRUE, method=2, conf.level=0.85, ci.type="norm", R=5000) 
#     skew         lwr.ci       upr.ci 
# -1.240421169 -2.692459803  0.006904769 
#
set.seed(12345)
DescTools::Skew(y, weights=NULL, na.rm=TRUE, method=2, conf.level=0.85, ci.type="basic", R=5000) 
#     skew       lwr.ci       upr.ci 
# -1.24042117 -2.55686133  0.03078115
#
set.seed(12345)
DescTools::Skew(y, weights=NULL, na.rm=TRUE, method=2, conf.level=0.85, ci.type="perc", R=5000) 
#     skew       lwr.ci      upr.ci 
# -1.24044585 -2.51166265  0.07598597
#
set.seed(12345)
DescTools::Skew(y, weights=NULL, na.rm=TRUE, method=2, conf.level=0.85, ci.type="bca", R=5000) 
#     skew       lwr.ci      upr.ci 
# -1.24042117 -2.51162349  0.07601899
#
set.seed(12345)
DescTools::Skew(y, weights=NULL, na.rm=TRUE, method=2, conf.level=0.90, ci.type="bca", R=5000) 
#      skew       lwr.ci       upr.ci 
# -1.240421169 -3.376905674  0.001312122 
#
# Since the all bootstrapped confidence intervals at the $90\%$ confidence level contains zero, we cannot reject the null hypothesis of 
# unskewed standardized residuals at the $10\%$ significance level.
#
# Second, the kurtosis
DescTools::Kurt(y, weights=NULL, method=2, conf.level=0.99, ci.type="classic") 
#   kurtosis    lwr.ci    upr.ci
#  17.666383  -0.304005  0.304005
#
# The estimated value of the excess kurtosis of the standardized residuals of the tseries::garch() fitted GARCH(1,1) model severely 
# conflicts with a possible Gaussian distribution of the standardized residuals at the $1\%$ significance level. The bootstrapped confidence
# intervals of type "norm" and "basic" do not allow to exclude that a mesokurtic distribution generates the standardized residuals at the 
# $5\%$ significance level. In contrast, the bootstrapped confidence intervals of type "perc" and "bca" support rejecting the null 
# hypothesis that a mesokurtic distribution might generate the standardized residuals in favor of a leptokurtic distribution at the $1\%$ 
# significance level.
# 
set.seed(12345)
DescTools::Kurt(y, weights=NULL, method=2, conf.level=0.95, ci.type="norm", R=5000) 
#   kurt       lwr.ci     upr.ci 
# 17.6663826 -0.9447927 39.8059389 
#
set.seed(12345)
DescTools::Kurt(y, weights=NULL, na.rm=TRUE, method=2, conf.level=0.95, ci.type="basic", R=5000) 
#    kurt     lwr.ci     upr.ci 
# 17.666383 -0.375733  32.423954 
#
set.seed(12345)
DescTools::Kurt(y, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type="perc", R=5000) 
#     kurt     lwr.ci    upr.ci 
# 17.666383  2.600465  40.019858
# 
set.seed(12345)
DescTools::Kurt(y, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type="bca", R=5000) 
#     kurt     lwr.ci    upr.ci 
# 17.666383  3.044805  44.719127 
# Warning message: In norm.inter(t, adj.alpha) : extreme order statistics used as endpoints
#
set.seed(12345)
DescTools::Kurt(y, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type="bca", R=50000)
#     kurt     lwr.ci    upr.ci 
# 17.666383  3.069008  49.834294 
# Warning message: In norm.inter(t, adj.alpha) : extreme order statistics used as endpoints
#
# The bootstrapped confidence intervals of type "norm" and "basic" at the $95\%$ confidence level contain zero. Hence, we cannot reject the
# null hypothesis of mesokurtic standardized residuals at the $5\%$ significance level. On the other hand, The bootstrapped confidence 
# intervals of type "perc" and "bca" at the $99\%$ confidence level do not contain zero. Although, the "bca" type uses the extreme order 
# statistics as endpoints, even with a very large number of bootstrap replicates. Hence, we must reject the null hypothesis of mesokurtic
# standardized residuals at the $1\%$ significance level.
#
# In light of the results of the non-parametric test for skewness and kurtosis on the standardized residuals of the Bitcoin daily percentage
# logarithm returns training set, we must reject at the $1\%$ significance level that the standardized residuals might be Gaussian 
# distributed. The following normality tests strengthen this evidence.
#
Data_df <- BTC_train_df
y <- na.rm(Data_df$GARCH_1_1_stand_res)
head(y,20)
#  0.4144350  1.7419601  0.1388852 -0.2739446  0.3934785  2.3160671 -2.1750189  0.9879875 -0.6733973  0.8577650  0.1677307 -0.4519716
# -0.3269755  0.3293429  1.4429839 -0.1135800  0.4322909 -0.5793139 -0.8429184 -0.4242169
#
# Shapiro-Wilks (*SW*) test.
stats::shapiro.test(y)
# Shapiro-Wilk normality test
# data:  y
# W=0.89821, p-value < 2.2e-16
#
# D'Agostino Pearson (*DP*) test.
# library(fBasics)
fBasics::dagoTest(y)
# D'Agostino Normality Test
# Test Results:
#   STATISTIC:
#     Chi2 | Omnibus: 692.649
#     Z3  | Skewness: -16.7488
#     Z4  | Kurtosis:  20.3009
#   P VALUE:
#     Omnibus  Test: 2.2e-16
#     Skewness Test: 2.2e-16 
#     Kurtosis Test: 2.2e-16
#
# Anderson-Darling (AD) test.
# library(nortest)
nortest::ad.test(y)
# Anderson-Darling normality test
# data:  y
# A=30.687, p-value < 2.2e-16
#
# Jarque-Bera (*JB*) test.
tseries::jarque.bera.test(y)
# Jarque Bera Test
# data:  y
# X-squared=22642, df=2, p-value < 2.2e-16
################################################################################################################################################
# Despite we must reject the null hypothesis that the standardized residuals of the Bitcoin daily logarithm return percentage training set 
# are Gaussian distributed at the $1\%$ significance level, the rather good results in terms of stationarity, lack of autocorrelation, and 
# homoscedasticity, advocate the attempt to determine their distribution by means of a parametric approach.
# 
# We start with considering a Cullen-Frey graph.
Data_df <- BTC_train_df
y <- na.rm(Data_df$GARCH_1_1_stand_res)
head(y,20)
#  0.4144350  1.7419601  0.1388852 -0.2739446  0.3934785  2.3160671 -2.1750189  0.9879875 -0.6733973  0.8577650  0.1677307 -0.4519716
# -0.3269755  0.3293429  1.4429839 -0.1135800  0.4322909 -0.5793139 -0.8429184 -0.4242169
#
# We have already computed the confidence intervals for the mean and the variance of the standardized residuals of the Bitcoin daily
# logarithm return percentage training set and we have found that they are actually standardized at the $20\%$ significance level.
# Therefore, we apply directly on them the procedures to determine their possible distribution.
#
# We apply a Cullen-Frey graph on the standardized residuals
# library(survival)
# library(MASS)
# library(fitdistrplus)
set.seed(12345)
fitdistrplus::descdist(y, discrete=FALSE, method= "sample", graph=TRUE, boot=5000)
# summary statistics
# ------
# min:  -12.68034   max:  5.719923 
# median:  0.02740622 
# mean:  0.009702442 
# sample sd:  1.000262 
# sample skewness:  -1.239338 
# sample kurtosis:  20.61152 
#
# The Cullen-Frey graph appears to be split into three areas. This is likely due to the presence of some outliers in the data set. However, 
# we have a visual insight that the data set is possibly skewed Student distributed. We will explore this possibility in more detail.
# We compute the empirical quantiles, empirical density function, and empirical distribution function of the standardized residuals.
# library(EnvStats)
y_qemp <- EnvStats::qemp(stats::ppoints(y), y) # Empirical quantiles of the data set y.
y_demp <- EnvStats::demp(y_qemp, y)     # Empirical probability density of the data set y.
y_pemp <- EnvStats::pemp(y_qemp, y)     # Empirical distribution function of the data set y.  
x <- y_qemp
y_d <- y_demp
y_p <- y_pemp
#
# With reference to the fGarch library, we plot the histogram of the standardized residuals together with the empirical density function, 
# the Standard Gaussian Distribution Density function, the generalized Error Distribution Density Function (GED) with mean parameter, mean=0, standard deviation 
# parameter, sd=1, and shape parameter, nu=1, and the standardized Student-t distribution (STD) with mean parameter, 
# mean=0, standard deviation parameter, sd=1, and degrees of freedom (shape) parameter, nu=3.
# Note that the standardized GED [resp. STD] is defined so that for a given standard deviation parameter, sd, it has the same variance, sd^2, 
# for all values of the shape parameter [resp. degrees of freedom parameter]. For comparison, the variance of the usual Student-t 
# distribution is nu/(nu-2), where nu is the degrees of freedom. The usual Student-t distribution is obtained by setting 
# sd=sqrt(nu/(nu - 2)). (see Wuertz D., Chalabi Y. and Luksan L. - Parameter estimation of ARMA models with GARCH/APARCH errors: An R and
# SPlus software implementation, Preprint, 41 pages, https://github.com/GeoBosh/fGarchDoc/blob/master/WurtzEtAlGarch.pdf).
#
# library(fGarch)
mean <- 0
sd <- 1
GED_nu <- 1
STD_nu <- 3
GED_leg <- bquote(paste("Generalized Error Distribution Density Function: mean=", .(mean),", standard deviation=", .(sd), ", shape=", .(GED_nu)))
STD_leg <- bquote(paste("Standardized Student-t Distribution: mean=", .(mean),", standard deviation=", .(sd), ", degrees of freedom=", .(STD_nu)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(y, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the tseries::garch() Fitted GARCH(1,1) Model for the Bitcoin Daily Logarithm Return Percentage  Training Set", 
     xlab= "Standardized Residuals", ylab= "Histogram & Density Functions Values")
# lines(x, y_d, lwd=2, col= "darkblue")
lines(density(y), lwd=2, col= "darkblue")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "red")
lines(x, fGarch::dged(x, mean=0, sd=1, nu=1, log=FALSE), lwd=2, col= "magenta")
lines(x, fGarch::dstd(x, mean=0, sd=1, nu=3), lwd=2, col= "darkgreen")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Distribution Density Function", GED_leg, STD_leg), 
       col=c("darkblue", "red","magenta","darkgreen"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
#
# We also compare the empirical distribution function of the standardized residuals with the distribution functions of the GED and STD.
mean <- 0
sd <- 1
GED_nu <- 1
STD_nu <- 3
GED_leg <- bquote(paste("Generalized Error Distribution Density Function: mean=", .(mean),", standard deviation=", .(sd), ", shape=", .(GED_nu)))
STD_leg <- bquote(paste("Standardized Student-t Distribution: mean=", .(mean),", standard deviation=", .(sd), ", degrees of freedom=", .(STD_nu)))
dev.new()
EnvStats::ecdfPlot(y, discrete=TRUE, prob.method= "emp.probs", type= "s", plot.it=TRUE, 
                   add=FALSE, ecdf.col= "cyan", ecdf.lwd=2, ecdf.lty=1, curve.fill=TRUE,  
                   main= "Empirical Distribution Function of the Standardized Residuals of the tseries::garch() Fitted GARCH(1,1) Model for the Bitcoin Daily Logarithm Return Percentage  Training Set", 
                    xlab= "Standardized Residuals", ylab= "Probability Distribution", xlim=c(x[1]-1.0, x[length(x)]+1.0))
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "red")
lines(x, fGarch::pged(x, mean=0, sd=1, nu=1), lwd=2, col= "magenta")
lines(x, fGarch::pstd(x, mean=0, sd=1, nu=3), lwd=2, col= "darkgreen")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Distribution Density Function", GED_leg, STD_leg), 
       col=c("darkblue", "red","magenta","darkgreen"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
#
dev.off()
#
# Alternatively
mean <- 0
sd <- 1
GED_nu <- 1
STD_nu <- 3
GED_leg <- bquote(paste("Generalized Error Distribution Density Function: mean=", .(mean),", standard deviation=", .(sd), ", shape=", .(GED_nu)))
STD_leg <- bquote(paste("Standardized Student-t Distribution: mean=", .(mean),", standard deviation=", .(sd), ", degrees of freedom=", .(STD_nu)))
dev.new()
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the tseries::garch() Fitted GARCH(1,1) Model for the Bitcoin Daily Logarithm Return Percentage  Training Set", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "red")
lines(x, fGarch::pged(x, mean=0, sd=1, nu=1), lwd=2, col= "magenta")
lines(x, fGarch::pstd(x, mean=0, sd=1, nu=3), lwd=2, col= "darkgreen")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Distribution Density Function", GED_leg, STD_leg), 
       col=c("darkblue", "red","magenta","darkgreen"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
#
dev.off()
#
# In light of the presented plots, we try to estimate the shape parameter of the GED and the degrees of freedom parameter of the STD to
# better adapt them to the empirical distribution of the standardized residuals.
# For this task, the fGarch package provides two functions fGarch::gedFit() and fGarch::stdFit(). However, these functions estimate the 
# whole set of the parameters of the theoretical distribution to adapt it to the empirical distribution and do not allow for the estimation 
# of only a subset of the parameters. Furthermore, in the case of the GED, the estimation procedure is unreliable due to a false convergence 
# issue. In fact,
gedFit_x <- fGarch::gedFit(x)
show(gedFit_x)
# $par
# mean         sd         nu 
# 0.03019467 0.97226695 0.88342822 
# 
# $objective
# [1] 2202.351
# 
# $convergence
# [1] 1
# 
# $iterations
# [1] 16
# 
# $evaluations
# function gradient 
# 39       58 
# 
# $message
# [1] "false convergence (8)"
#
stdFit_x <- fGarch::stdFit(x)
show(stdFit_x)
# $par
# mean         sd         nu 
# 0.02281734 1.17772637 2.65330714 
# 
# $objective
# [1] 2207.216
# 
# $convergence
# [1] 0
# 
# $iterations
# [1] 80
# 
# $evaluations
# function gradient 
# 90      259 
# 
# $message
# [1] "relative convergence (4)"
#
# The fitdistrplus package provides a more general fitdistrplus::fitdist() function that can be fed by the density of the distributions GED
# and SDT provided by the fGarch package and is flexible enough to allow the estimation of only a subset of the parameters.
fitdist_ged <- fitdistrplus::fitdist(y, dged, start=list(nu=1), fix.arg=list(mean=0, sd=1), method= "mle")
summary(fitdist_ged)
# Fitting of the distribution ' ged ' by maximum likelihood 
# Parameters : estimate  Std. Error
#           nu 0.8742739 0.03194005
# Fixed parameters: value
#           mean     0
#           sd       1
# Loglikelihood:  -2201.736   AIC:  4405.472   BIC:  4410.921 
#
# Moreover, by means of the object generated by the fitdistrplus::fitdist() function we can also evaluate the uncertainty in estimated 
# parameters of the fitted distribution by means of the fitdistrplus::bootdist() function.
set.seed(12345)
fitdist_ged_bd <- bootdist(fitdist_ged, niter=1000)
summary(fitdist_ged_bd)
# Parametric bootstrap medians and 95% percentile CI 
#     Median      2.5%     97.5% 
#   0.8749224 0.8125824 0.9445581
#
# For cross-checking the result of the fitdistrplus::fitdist() function, we also consider more estimates of the GED shape parameter by 
# tackling the log-likelihood function direct maximization. We start writing the log-likelihood function to be maximized. Note that we again
# use the GED density provided by the fGarch package.
opt_ged_minus_logLik <- function(x) -sum(log(dged(y, mean=0, sd=1, nu=x)))
# Hence, we tackle the optimization of the log-likelihood function by means of the function stats::optimize().
opt_ged_result <- stats::optimize(f=opt_ged_minus_logLik, interval=c(0,1), maximum=FALSE, tol=1e-09)
show(opt_ged_result)
# $minimum 0.8742729
# 
# $objective 2201.736
#
# We also show how to apply the rather powerful pracma::fminunc() and pracma::fmincon() functions conceived to optimize multivariate
# functions.
# First, we rewrite the log-likelihood of the GED, fictitiously transformed into a multivariate function by adding a quadratic
# term.
fmin_minus_logLik <- function(x) x[1]^2-sum(log(dged(y, mean=0, sd=1, nu=x[2])))
#
# Second, we fix the initial points of the unconstrained maximization procedure, that we choose as the median provided by the 
# fitdistrplus::bootdist() function.
nu0 <- as.vector(fitdist_ged_bd[["CI"]][["Median"]])
show(nu0)
# 0.8749224
# Then, we launch the unconstrained maximization procedure.
# library(NlcOptim)
# library(pracma)
fminunc_result <- pracma::fminunc(fn=fmin_minus_logLik, x0=c(0, nu0), tol=1e-08)
show(fminunc_result)
# $par   x[1]    x[2]=nu
# [1] 0.0000000 0.8742729
# 
# $value
# [1] 2201.736
# 
# $counts
# function gradient 
# 15        5 
# 
# $convergence
# [1] 0
# 
# $message
# [1] "Rvmminu converged"
#
# In the end, we consider the constrained optimization procedure where (0,nu0) is the starting point and we use the confidence interval 
# endpoints provided by the fitdistrplus::bootdist() function to build the multivariate constraint.
nu0 <- as.vector(fitdist_ged_bd[["CI"]][["Median"]])
nu_min <- as.vector(fitdist_ged_bd[["CI"]][["2.5%"]])
nu_max <- as.vector(fitdist_ged_bd[["CI"]][["97.5%"]])
show(c(nu0,nu_min,nu_max))
# 0.8749224 0.8125824 0.9445581
#
fmincon_result <- pracma::fmincon(fn=fmin_minus_logLik, x0=c(0, nu0), lb=c(-1, nu_min), ub=c(1, nu_max), tol=1e-06, maxfeval=10000, maxiter=5000) 
# fmincon_result <- pracma::fmincon(fn=minus_logLik, x0=c(0, nu0), lb=c(-1, nu_min), ub=c(1, nu_max), tol=1e-06, maxfeval=1e+09, maxiter=1e+09)   
show(fmincon_result)
# $par   x[1]    x[2]=nu
# [1] 0.0000000 0.8742729
# 
# $value
# [1] 2201.736
# 
# $convergence
# [1] 0
# 
# $info$grad
# [,1]
# [1,] 0.0000000000
# [2,] 0.0004882812
# 
# $info$hessian
# [,1]     [,2]
# [1,]    1   0.0000
# [2,]    0 981.0405
#
# Now, we apply the above procedure to the STD distribution. 
fitdist_std <- fitdistrplus::fitdist(y, dstd, start=list(nu=3), fix.arg=list(mean=0, sd=1), method= "mle")
summary(fitdist_std)
# Parameters:  estimate Std. Error
#          nu 3.128909  0.1105751
# Fixed parameters: value
#            mean     0
#            sd       1
# Loglikelihood:  -2208.051   AIC:  4418.101   BIC:  4423.55
#
set.seed(12345)
fitdist_std_bd <- bootdist(fitdist_std, niter=1000)
summary(fitdist_std_bd)
# Parametric bootstrap medians and 95% percentile CI 
#    Median     2.5%    97.5% 
#   3.133358 2.945289 3.380252 
#
# We write the log-likelihood function to be maximized. Note that we again use the GED density provided by the fGarch package.
opt_std_minus_logLik <- function(x) -sum(log(dstd(y, mean=0, sd=1, nu=x)))
opt_std_result <- stats::optimize(f=opt_std_minus_logLik, interval=c(2,100), maximum=FALSE, tol=1e-09)   # the minimization procedure where nu is the starting point.
show(opt_std_result)
# $minimum 3.128966
# 
# $objective 2208.051
#
# We rewrite the log-likelihood of the GED, fictitiously transformed into a multivariate function by adding a quadratic term.
fmin_minus_logLik <- function(x) x[1]^2-sum(log(dstd(y, mean=0, sd=1, nu=x[2])))
#
# We fix the initial points of the unconstrained maximization procedure
nu0 <- as.vector(fitdist_std_bd[["CI"]][["Median"]])
show(nu0)
# 3.133358
#
# We consider the unconstrained minimization procedure where (0,nu0) is the starting point.
fminunc_result <- pracma::fminunc(fn=fmin_minus_logLik, x0=c(0, nu0), tol=1e-08)
show(fminunc_result)
# $par  x[1]    x[2]=nu
# [1] 0.000000 3.128966
# 
# $value
# [1] 2208.051
# 
# $counts
# function gradient 
# 19        4 
# 
# $convergence
# [1] 0
# 
# $message
# [1] "Rvmminu converged"
#
# We consider the constrained optimization procedure where (0,nu0) is the starting point and we use the confidence interval endpoints 
# provided by the fitdistrplus::bootdist() function to build the multivariate constraint.
nu0 <- as.vector(fitdist_std_bd[["CI"]][["Median"]])
nu_min <- as.vector(fitdist_std_bd[["CI"]][["2.5%"]])
nu_max <- as.vector(fitdist_std_bd[["CI"]][["97.5%"]])
show(c(nu0,nu_min,nu_max))
# 3.133358 2.945289 3.380252
#
fmincon_result <- pracma::fmincon(fn=fmin_minus_logLik, x0=c(0, nu0), lb=c(-1, nu_min), ub=c(1, nu_max), tol=1e-06, maxfeval=10000, maxiter=5000) 
# fmincon_result <- pracma::fmincon(fn=minus_logLik, x0=c(0, nu0), lb=c(-1, nu_min), ub=c(1, nu_max), tol=1e-06, maxfeval=1e+09, maxiter=1e+09)   
show(fmincon_result)
# $par  x[1]    x[2]=nu
# [1] 0.000000 3.128966
# 
# $value
# [1] 2208.051
# 
# $convergence
# [1] 0
# 
# $info$grad
# [,1]
# [1,] 0.00000000
# [2,] 0.00100458
# 
# $info$hessian
# [,1]     [,2]
# [1,]    1  0.00000
# [2,]    0 81.99643
#
############################################################################################################################################
# From the above results, we select for the GED the parameter nu=0.8742729. It is the optimum point commonly estimated by the optimization 
# procedures stats::optimize(), pracma::fminuncn(), and pracma::fmincon(), and it is very similar to the optimum point nu=0.8742739 
# estimated by the fitdistrplus::fitdist() procedure with the same loglikelihood loglik=-2201.736, till the third decimal digit. Furthermore,
# the parameter nu=0.8742729 falls within the $95\%$ confidence interval [0.8125824 0.9445581] of the parameter true value estimated by the
# fitdistrplus::bootdist() bootstrap procedure.
# 
# Setting
logLik <- -opt_ged_result[["objective"]] # the minimized negative log-likelihood
n <- length(y)
k <- 1
AIC <- 2*k-2*logLik
BIC <- k*log(n)-2*logLik
AICc <- AIC + 2*k*((k+1)/(n-k-1))
# we have
show(c(logLik, AIC, BIC, AICc))
#   logLik      AIC       BIC      AICc
# -2201.736  4405.472  4410.921  4405.475
#
# Similarly, we select for the STD the parameter nu=3.128966. It is the optimum point commonly estimated by the optimization procedures
# stats::optimize(), pracma::fminuncn(), and pracma::fmincon(), and it is very similar to the optimum points nu=3.128909 estimated by the 
# fitdistrplus::fitdist() procedures, respectively, with the same loglikelihood loglik=-2208.051, till the third decimal digit. Furthermore, 
# the parameter nu=3.128975 falls within the $95\%$ confidence interval [2.945289 3.380252] of the parameter true value estimated by the 
# fitdistrplus::bootdist() bootstrap procedure.
# Setting
logLik <- -opt_std_result[["objective"]] # the minimized negative log-likelihood
n <- length(y)
k <- 1
AIC <- 2*k-2*logLik
BIC <- k*log(n)-2*logLik
AICc <- AIC + 2*k*((k+1)/(n-k-1))
# we have
show(c(logLik, AIC, BIC, AICc))
#   logLik      AIC       BIC      AICc
# -2208.051  4418.101  4423.550  4418.104
#
# We plot the histogram and the empirical density function of the standardized residuals together with the density function of the estimated 
# GED and SDT.
mean <- 0
sd <- 1
GED_nu <- opt_ged_result[["minimum"]]
STD_nu <- opt_std_result[["minimum"]]
GED_leg <- bquote(paste("Generalized Error Distribution Density Function: mean=", .(mean),", standard deviation=", .(sd), ", shape=", .(GED_nu)))
STD_leg <- bquote(paste("Standardized Student-t Distribution: mean=", .(mean),", standard deviation=", .(sd), ", degrees of freedom=", .(STD_nu)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(y, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the tseries::garch() Fitted GARCH(1,1) Model for the Bitcoin Daily Logarithm Return Percentage  Training Set", 
     xlab= "Standardized Residuals", ylab= "Histogram & Density Functions Values")
# lines(x, y_d, lwd=2, col= "darkblue")
lines(density(y), lwd=2, col= "darkblue")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "red")
lines(x, fGarch::dged(x, mean=0, sd=1, nu=1, log=FALSE), lwd=2, col= "magenta")
lines(x, fGarch::dstd(x, mean=0, sd=1, nu=3), lwd=2, col= "darkgreen")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Distribution Density Function", GED_leg, STD_leg), 
       col=c("darkblue", "red","magenta","darkgreen"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
#
# We also compare the empirical distribution function of the standardized residuals with the distribution functions of the estimated GED and
# STD.
mean <- 0
sd <- 1
GED_nu <- opt_ged_result[["minimum"]]
STD_nu <- opt_std_result[["minimum"]]
GED_leg <- bquote(paste("Generalized Error Distribution Density Function: mean=", .(mean),", standard deviation=", .(sd), ", shape=", .(GED_nu)))
STD_leg <- bquote(paste("Standardized Student-t Distribution: mean=", .(mean),", standard deviation=", .(sd), ", degrees of freedom=", .(STD_nu)))
dev.new()
EnvStats::ecdfPlot(y, discrete=TRUE, prob.method= "emp.probs", type= "s", plot.it=TRUE, 
                   add=FALSE, ecdf.col= "cyan", ecdf.lwd=2, ecdf.lty=1, curve.fill=TRUE,  
                   main= "Empirical Distribution Function of the Standardized Residuals of the tseries::garch() Fitted GARCH(1,1) Model for the Bitcoin Daily Logarithm Return Percentage  Training Set", 
                   xlab= "Standardized Residuals", ylab= "Probability Distribution", xlim=c(x[1]-1.0, x[length(x)]+1.0))
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "red")
lines(x, fGarch::pged(x, mean=0, sd=1, nu=1), lwd=2, col= "magenta")
lines(x, fGarch::pstd(x, mean=0, sd=1, nu=3), lwd=2, col= "darkgreen")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Distribution Density Function", GED_leg, STD_leg), 
       col=c("darkblue", "red","magenta","darkgreen"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
#
dev.off()
#
# Alternatively
mean <- 0
sd <- 1
GED_nu <- opt_ged_result[["minimum"]]
STD_nu <- opt_std_result[["minimum"]]
GED_leg <- bquote(paste("Generalized Error Distribution Density Function: mean=", .(mean),", standard deviation=", .(sd), ", shape=", .(GED_nu)))
STD_leg <- bquote(paste("Standardized Student-t Distribution: mean=", .(mean),", standard deviation=", .(sd), ", degrees of freedom=", .(STD_nu)))
dev.new()
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the tseries::garch() Fitted GARCH(1,1) Model for the Bitcoin Daily Logarithm Return Percentage  Training Set", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "red")
lines(x, fGarch::pged(x, mean=0, sd=1, nu=1), lwd=2, col= "magenta")
lines(x, fGarch::pstd(x, mean=0, sd=1, nu=3), lwd=2, col= "darkgreen")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Distribution Density Function", GED_leg, STD_leg), 
       col=c("darkblue", "red","magenta","darkgreen"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
#
dev.off()
#
# We build the Q-Q plot of the standardized residuals of the tseries::garch() fitted GARCH(1,1) model for the Bitcoin daily percentage 
# logarithm returns training set against the corresponding quantiles of the estimated generalized GED and STD by using the library 
# *qqplotr*, which extends some functionality of the library *ggplot2*. The library *qqplotr* allows to draw also P-P plots.
# 
# First, we build a suitable data frame.
Data_df <- BTC_train_df
y <- na.rm(Data_df$GARCH_1_1_stand_res)
head(y,20)
#  0.4144350  1.7419601  0.1388852 -0.2739446  0.3934785  2.3160671 -2.1750189  0.9879875 -0.6733973  0.8577650  0.1677307 -0.4519716
# -0.3269755  0.3293429  1.4429839 -0.1135800  0.4322909 -0.5793139 -0.8429184 -0.4242169
y_qemp <- EnvStats::qemp(ppoints(length(y)), y)
mean <- 0
sd <- 1
nu  <- GED_nu 
distr <- "ged"
distr_pars <- list(mean=0, sd=1, nu=nu)
quants <- fGarch::qged(ppoints(length(y)), mean=0, sd=1, nu=nu)
QQ_plot_df <- data.frame(T=1:length(y), Q=quants, X=y, Y=y_qemp)
head(QQ_plot_df)
# Second we draw the Q-Q plot of the residuals.
# library(qqplotr)
Data_df <- QQ_plot_df
length <- nrow(Data_df)
quart_probs <- c(0.25,0.75)
quart_X <- as.vector(quantile(QQ_plot_df$X, quart_probs))
quart_Q <- qged(quart_probs, mean=0, sd=1, nu=nu)
slope <- diff(quart_X)/diff(quart_Q)
intercept <- quart_X[1]-slope*quart_Q[1]
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Q-Q plot of the Standardized Residuals of the tseries::garch() fitted GARCH(1,1) Model for the Bitcoin Daily Logarithm Return Percentage  Training Set Against the Generalized Error Distribution Density Function")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Data set size ", .(length), " sample points; Generalized Error Distribution density function: mean ", .(mean), ", standard deviation ", .(sd), ", shape ", .(nu), "."~~"Data by courtesy of Yahoo Finance US - ",.(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("Theoretical Quantiles")
y_name <- bquote("Sample Quantiles")
x_breaks_min <- floor(Data_df$Q[1])
x_breaks_max <- ceiling(Data_df$Q[length])
x_breaks <- seq(from=x_breaks_min, to=x_breaks_max, by=0.5)
x_labs <- format(x_breaks, scientific=FALSE)
y_breaks_num <- length(x_breaks)
y_binwidth <- round((max(Data_df$Y)-min(Data_df$Y))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$Y)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$Y)/y_binwidth))*y_binwidth
y_breaks <- c(round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3))
y_labs <- format(y_breaks, scientific=FALSE)
y1_shape <- bquote("Q-Q plot")
y1_fill <- bquote("90% confidence intervals")
y2_fill <- bquote("95% confidence intervals")
y1_col <- bquote("interquartile line")
y2_col <- bquote("regression line")
y3_col <- bquote("y=x line")
leg_shape_labs <- y1_shape
leg_fill_labs <- c(y1_fill, y2_fill)
leg_col_labs <- c(y1_col, y2_col, y3_col)
leg_shape_cols <- c("y1_shape"=19)
leg_fill_cols <- c("y1_fill"="chartreuse1", "y2_fill"="deepskyblue1")
leg_col_cols <- c("y1_col"="cyan", "y2_col"="red", "y3_col"="black")
leg_shape_sort <- "y1_shape"
leg_fill_sort <- c("y1_fill", "y2_fill")
leg_col_sort <- c("y1_col", "y2_col", "y3_col")
Stand_Res_ged_QQ_plot <- ggplot(Data_df, aes(sample=X)) +
  qqplotr::stat_qq_band(aes(fill= "y2_fill"), distribution=distr, dparams=distr_pars, conf=0.95) +
  qqplotr::stat_qq_band(aes(fill= "y1_fill"), distribution=distr, dparams=distr_pars, conf=0.90) +
  geom_abline(aes(slope=slope, intercept=intercept, colour= "y1_col"), linewidth=0.8, linetype= "solid", show.legend=FALSE)+
  stat_smooth(aes(x=Q, y=Y, colour= "y2_col", group=1), inherit.aes=FALSE, method= "lm" , formula=y~x, alpha=1, linewidth=0.8, linetype= "solid",
              se=FALSE, fullrange=FALSE)+
  geom_abline(aes(slope=1, intercept=0, colour= "y3_col"), linewidth=0.8, linetype= "solid", show.legend=FALSE) +
  qqplotr::stat_qq_point(aes(shape= "y1_shape"), distribution=distr, dparams=distr_pars, colour= "black", alpha=1, size=1.0) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_shape_manual(name= "", labels=leg_shape_labs, values=leg_shape_cols, breaks=leg_shape_sort) +
  scale_fill_manual(name= "", labels=leg_fill_labs, values=leg_fill_cols, breaks=leg_fill_sort) +
  scale_colour_manual(name= "", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_sort) +
  guides(shape=guide_legend(order=1), fill=guide_legend(order=2), colour=guide_legend(order=3)) +
  theme(plot.title=element_text(hjust=0.5, size=11), 
        plot.subtitle=element_text(hjust=0.5, size=10),
        axis.text.x=element_text(angle=0, vjust=1),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(Stand_Res_ged_QQ_plot)
#
# P-P plot of the empirical distribution function of the standardized residuals of the tseries::garch() fitted GARCH(1,1) model for the 
# Bitcoin daily logarithm return percentage training set against the estimated GED function.
# First, we build a suitable data frame.
y_qemp <- qemp(ppoints(length(y)), y)
y_pemp <- pemp(y_qemp, y)
mean <- 0
sd <- 1
nu  <- GED_nu 
distr <- "ged"
distr_pars <- list(mean=0, sd=1, nu=nu)
quants <- qged(ppoints(length(y)), mean=0, sd=1, nu=nu)
probs <- pged(quants, mean=0, sd=1, nu=nu)
PP_plot_df <- data.frame(T=1:length(y), P=probs, X=y, Y=y_pemp)
head(PP_plot_df)
# Second we draw the P-P plot of the standardized residuals.
Data_df <- PP_plot_df
length <- nrow(PP_plot_df)
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("P-P plot of the Standardized Residuals of the tseries::garch() Fitted GARCH(1,1) Model for the Bitcoin Daily Logarithm Returns Training Set Against the Estimated Generalized Logistic Distribution")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Data set size ", .(length), " sample points; estimated generalized logistic density function: mean ", .(mean), ", standard deviation ", .(sd), ", shape ", .(nu), "."~~"Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("Theoretical Probabilities")
y_name <- bquote("Sample Probabilities")
x_breaks_min <- floor(Data_df$P[1])
x_breaks_max <- ceiling(Data_df$P[length])
x_breaks <- seq(from=x_breaks_min, to=x_breaks_max, by=0.5)
x_labs <- format(x_breaks, scientific=FALSE)
J <- 0
x_lims <- c(x_breaks_min-J*x_binwidth, x_breaks_max+J*x_binwidth)
y_breaks_num <- length(x_breaks)
y_binwidth <- round((max(Data_df$Y)-min(Data_df$Y))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$Y)/y_binwidth))*y_binwidth
y_breaks_up <- floor((max(Data_df$Y)/y_binwidth))*y_binwidth
y_breaks <- c(round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3))
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.3
y_lims <- c(y_breaks_low-K*y_binwidth, y_breaks_up+K*y_binwidth)
y1_shape <- bquote("P-P plot")
y1_fill <- bquote("90% confidence intervals")
y2_fill <- bquote("95% confidence intervals")
y1_col <- bquote("y=x line")
y2_col <- bquote("regression line")
leg_shape_labs <- y1_shape
leg_fill_labs <- c(y1_fill, y2_fill)
leg_col_labs <- c(y1_col, y2_col)
leg_shape_cols <- c("y1_shape"=19)
leg_fill_cols <- c("y1_fill"="chartreuse1", "y2_fill"="deepskyblue1")
leg_col_cols <- c("y1_col"="black", "y2_col"="red")
leg_shape_sort <- "y1_shape"
leg_fill_sort <- c("y1_fill", "y2_fill")
leg_col_sort <- c("y1_col", "y2_col")
Stand_Res_ged_PP_plot <- ggplot(Data_df, aes(sample=X)) +
  qqplotr::stat_pp_band(aes(fill= "y2_fill"), distribution=distr, dparams=distr_pars, conf=0.95) +
  qqplotr::stat_pp_band(aes(fill= "y1_fill"), distribution=distr, dparams=distr_pars, conf=0.90) +
  qqplotr::stat_pp_line(aes(colour= "y1_col"), geom="path", position="identity", colour= "black") +
  stat_smooth(aes(x=P, y=Y, colour= "y2_col"), inherit.aes=FALSE, method= "lm", formula=y~x, alpha=1, linewidth=0.8, linetype= "solid", se=FALSE, fullrange=FALSE) +
  qqplotr::stat_pp_point(aes(shape= "y1_shape"), distribution=distr, dparams=distr_pars, colour= "black", alpha=1, size=1.0) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_shape_manual(name= "", labels=leg_shape_labs, values=leg_shape_cols, breaks=leg_shape_sort) +
  scale_fill_manual(name= "", labels=leg_fill_labs, values=leg_fill_cols, breaks=leg_fill_sort) +
  scale_colour_manual(name= "", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_sort) +
  guides(shape=guide_legend(order=1), fill=guide_legend(order=2), colour=guide_legend(order=3)) +
  theme(plot.title=element_text(hjust=0.5, size=11), 
        plot.subtitle=element_text(hjust=0.5, size=10),
        axis.text.x=element_text(angle=0, vjust=1),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(Stand_Res_ged_PP_plot)
#
# We modify the code chunk for the estimated generalized Student-t distribution
Data_df <- BTC_train_df
y <- na.rm(Data_df$GARCH_1_1_stand_res)
head(y,20)
#  0.4144414  1.7419780  0.1388878 -0.2739479  0.3934812  2.3160727 -2.1750637  0.9880176 -0.6734156  0.8577843  0.1677339 -0.4519775
# -0.3269782  0.3293440  1.4429828 -0.1135808  0.4322915 -0.5793126 -0.8429148 -0.4242154
y_qemp <- qemp(ppoints(length(y)), y)
mean <- 0
sd <- 1
nu  <- STD_nu 
distr <- "std"
distr_pars <- list(mean=0, sd=1, nu=nu)
quants <- qstd(ppoints(length(y)), mean=0, sd=1, nu=nu)
QQ_plot_df <- data.frame(T=1:length(y), Q=quants, X=y, Y=y_qemp)
head(QQ_plot_df)
# Second we draw the Q-Q plot of the standardized residuals.
Data_df <- QQ_plot_df
length <- nrow(Data_df)
quart_probs <- c(0.25,0.75)
quart_X <- as.vector(quantile(QQ_plot_df$X, quart_probs))
quart_Q <- qstd(quart_probs, mean=0, sd=1, nu=nu)
slope <- diff(quart_X)/diff(quart_Q)
intercept <- quart_X[1]-slope*quart_Q[1]
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Q-Q plot of the Standardized Residuals of the tseries::garch() Fitted GARCH(1,1) Model for the Bitcoin Daily Logarithm Returns Training Set Against the Generalized Student Distribution")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Data set size ", .(length), " sample points; generalized Student density function: mean ", .(mean), ", standard deviation ", .(sd), ", degrees of freedom ", .(nu), "."~~"Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("Theoretical Quantiles")
y_name <- bquote("Sample Quantiles")
x_breaks_min <- floor(Data_df$Q[1])
x_breaks_max <- ceiling(Data_df$Q[length])
x_breaks <- seq(from=x_breaks_min, to=x_breaks_max, by=0.5)
x_labs <- format(x_breaks, scientific=FALSE)
y_breaks_num <- length(x_breaks)
y_binwidth <- round((max(Data_df$Y)-min(Data_df$Y))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$Y)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$Y)/y_binwidth))*y_binwidth
y_breaks <- c(round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3))
y_labs <- format(y_breaks, scientific=FALSE)
y1_shape <- bquote("Q-Q plot")
y1_fill <- bquote("90% confidence intervals")
y2_fill <- bquote("95% confidence intervals")
y1_col <- bquote("interquartile line")
y2_col <- bquote("regression line")
y3_col <- bquote("y=x line")
leg_shape_labs <- y1_shape
leg_fill_labs <- c(y1_fill, y2_fill)
leg_col_labs <- c(y1_col, y2_col, y3_col)
leg_shape_cols <- c("y1_shape"=19)
leg_fill_cols <- c("y1_fill"="chartreuse1", "y2_fill"="deepskyblue1")
leg_col_cols <- c("y1_col"="cyan", "y2_col"="red", "y3_col"="black")
leg_shape_sort <- "y1_shape"
leg_fill_sort <- c("y1_fill", "y2_fill")
leg_col_sort <- c("y1_col", "y2_col", "y3_col")
Stand_Res_std_QQ_plot <- ggplot(Data_df, aes(sample=X)) +
  qqplotr::stat_qq_band(aes(fill= "y2_fill"), distribution=distr, dparams=distr_pars, conf=0.95) +
  qqplotr::stat_qq_band(aes(fill= "y1_fill"), distribution=distr, dparams=distr_pars, conf=0.90) +
  geom_abline(aes(slope=slope, intercept=intercept, colour= "y1_col"), linewidth=0.8, linetype= "solid", show.legend=FALSE)+
  stat_smooth(aes(x=Q, y=Y, colour= "y2_col", group=1), inherit.aes=FALSE, method= "lm" , formula=y~x, alpha=1, linewidth=0.8, linetype= "solid",
              se=FALSE, fullrange=FALSE)+
  geom_abline(aes(slope=1, intercept=0, colour= "y3_col"), linewidth=0.8, linetype= "solid", show.legend=FALSE) +
  qqplotr::stat_qq_point(aes(shape= "y1_shape"), distribution=distr, dparams=distr_pars, colour= "black", alpha=1, size=1.0) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_shape_manual(name= "", labels=leg_shape_labs, values=leg_shape_cols, breaks=leg_shape_sort) +
  scale_fill_manual(name= "", labels=leg_fill_labs, values=leg_fill_cols, breaks=leg_fill_sort) +
  scale_colour_manual(name= "", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_sort) +
  guides(shape=guide_legend(order=1), fill=guide_legend(order=2), colour=guide_legend(order=3)) +
  theme(plot.title=element_text(hjust=0.5, size=11), 
        plot.subtitle=element_text(hjust=0.5, size=10),
        axis.text.x=element_text(angle=0, vjust=1),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(Stand_Res_std_QQ_plot)
#
# P-P plot of the standardized residuals of the GARCH(1,1) model for the Bitcoin daily logarithm return percentage training set against the
# estimated standardized Student distribution.
# As before, we start by building a suitable data frame
y_pemp <- pstd(y_qemp, y)
y_pemp <- pemp(y_qemp, y)
mean <- 0
sd <- 1
nu  <- STD_nu 
distr <- "std"
distr_pars <- list(mean=0, sd=1, nu=nu)
quants <- qstd(ppoints(length(y)), mean=0, sd=1, nu=nu)
probs <- pstd(quants, mean=0, sd=1, nu=nu)
PP_plot_df <- data.frame(T=1:length(y), P=probs, X=y, Y=y_pemp)
head(PP_plot_df)
# Second we draw the P-P plot of the standardized residuals.
Data_df <- PP_plot_df
length <- nrow(PP_plot_df)
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("P-P plot of the Standardized Residuals of the GARCH(1,1) Model for the Bitcoin Daily Logarithm Returns Training Set Against the Estimated Generalized Student Distribution")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Data set size ", .(length), " sample points; estimated generalized Student density function: mean ", .(mean), ", standard deviation ", .(sd), ", degrees of freedom ", .(nu), "."~~"Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("Theoretical Probabilities")
y_name <- bquote("Sample Probabilities")
x_breaks_min <- floor(Data_df$P[1])
x_breaks_max <- ceiling(Data_df$P[length])
x_breaks <- seq(from=x_breaks_min, to=x_breaks_max, by=0.5)
x_labs <- format(x_breaks, scientific=FALSE)
J <- 0
x_lims <- c(x_breaks_min-J*x_binwidth, x_breaks_max+J*x_binwidth)
y_breaks_num <- length(x_breaks)
y_binwidth <- round((max(Data_df$Y)-min(Data_df$Y))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$Y)/y_binwidth))*y_binwidth
y_breaks_up <- floor((max(Data_df$Y)/y_binwidth))*y_binwidth
y_breaks <- c(round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3))
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.3
y_lims <- c(y_breaks_low-K*y_binwidth, y_breaks_up+K*y_binwidth)
y1_shape <- bquote("P-P plot")
y1_fill <- bquote("90% confidence intervals")
y2_fill <- bquote("95% confidence intervals")
y1_col <- bquote("y=x line")
y2_col <- bquote("regression line")
leg_shape_labs <- y1_shape
leg_fill_labs <- c(y1_fill, y2_fill)
leg_col_labs <- c(y1_col, y2_col)
leg_shape_cols <- c("y1_shape"=19)
leg_fill_cols <- c("y1_fill"="chartreuse1", "y2_fill"="deepskyblue1")
leg_col_cols <- c("y1_col"="black", "y2_col"="red")
leg_shape_sort <- "y1_shape"
leg_fill_sort <- c("y1_fill", "y2_fill")
leg_col_sort <- c("y1_col", "y2_col")
Stand_Res_std_PP_plot <- ggplot(Data_df, aes(sample=X)) +
  qqplotr::stat_pp_band(aes(fill= "y2_fill"), distribution=distr, dparams=distr_pars, conf=0.95) +
  qqplotr::stat_pp_band(aes(fill= "y1_fill"), distribution=distr, dparams=distr_pars, conf=0.90) +
  qqplotr::stat_pp_line(aes(colour= "y1_col"), geom="path", position="identity", colour= "black") +
  stat_smooth(aes(x=P, y=Y, colour= "y2_col"), inherit.aes=FALSE, method= "lm", formula=y~x, alpha=1, linewidth=0.8, linetype= "solid", se=FALSE, fullrange=FALSE) +
  qqplotr::stat_pp_point(aes(shape= "y1_shape"), distribution=distr, dparams=distr_pars, colour= "black", alpha=1, size=1.0) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_shape_manual(name= "", labels=leg_shape_labs, values=leg_shape_cols, breaks=leg_shape_sort) +
  scale_fill_manual(name= "", labels=leg_fill_labs, values=leg_fill_cols, breaks=leg_fill_sort) +
  scale_colour_manual(name= "", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_sort) +
  guides(shape=guide_legend(order=1), fill=guide_legend(order=2), colour=guide_legend(order=3)) +
  theme(plot.title=element_text(hjust=0.5, size=11), 
        plot.subtitle=element_text(hjust=0.5, size=10),
        axis.text.x=element_text(angle=0, vjust=1),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(Stand_Res_std_PP_plot)
#
# From the Q-Q and P-P plots, we have visual evidence of a better fit of the empirical distribution with the GED rather than 
# the STD distribution.
#
# We consider the standard goodness of fit tests.
# The Kolmogorov-Smirnov test in the library *stats*
Data_df <- BTC_train_df
y <- na.rm(Data_df$GARCH_1_1_stand_res)
head(y,20)
# [1]  0.4144350  1.7419601  0.1388852 -0.2739446  0.3934785  2.3160671 -2.1750189  0.9879875 -0.6733973  0.8577650  0.1677307 -0.4519716
# [13] -0.3269755  0.3293429  1.4429839 -0.1135800  0.4322909 -0.5793139 -0.8429184 -0.4242169
mean <- 0
sd <- 1
nu  <- GED_nu 
stats::ks.test(y, y="pged", mean=0, sd=1, nu=nu, alternative= "two.sided")
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  y
# D=0.025228, p-value=0.2242
# alternative hypothesis: two-sided
#
mean <- 0
sd <- 1
nu  <- STD_nu 
stats::ks.test(y, y="pstd", mean=0, sd=1, nu=nu, alternative= "two.sided")
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  y
# D=0.03491, p-value=0.03037
# alternative hypothesis: two-sided
#
# The Kolmgorov-Smirnov test cannot reject the null hypothesis that the standardized residuals of the tseries::garch() fitted GARCH(1,1) 
# model have the estimated generalized Error Distribution Density Function at the $10\%$ significance level, but rejects the null hypothesis
# that the standardized residuals have the estimated standardized Student distribution at the $5\%$ significance level.
# 
# Another application of the Kolmogorov-Smirnov test can be derived using the possibility of comparing two empirical distributions offered 
# by the function stats::ks.test().
mean <- 0
sd <- 1
nu  <- GED_nu 
KS_ged_mat_np <- matrix(NA, nrow=10000, ncol=2)
for(k in 1:10000){
  set.seed(k)
  y_rged <- rged(n=length(y), mean=0, sd=1, nu=nu)
  KS_ged <- stats::ks.test(x=y, y=y_rged, alternative="two.sided")
  KS_ged_mat_np[k,1] <- k
  KS_ged_mat_np[k,2] <- KS_ged[["p.value"]]}
summary(KS_ged_mat_np[,2])
#    Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0000675 0.1841870 0.4107762 0.4340310 0.6548644 0.9989504 
quantile(KS_ged_mat_np[,2], na.rm=TRUE, probs=c(0.01,0.05,0.1))
#        1%         5%        10%
#   0.01167807 0.04390009 0.07569350
#
# In the $5\%$ of cases on 10000 random vectors sampled by the GED, the null hypothesis that the GED fits the 
# distribution of the standardized residuals of the tseries::garch() fitted GARCH(1,1) model is rejected at the $5\%$ significance level, 
# not at the $1\%$ significance level, though. In the $10\%$ of cases on 10000 random vectors sampled by the GED, the null 
# hypothesis that the GED fits the standardized residuals empirical distribution is not rejected at the $5\%$ significance level,
#
mean <- 0
sd <- 1
nu  <- STD_nu 
KS_std_mat_np <- matrix(NA, nrow=10000, ncol=2)
for(k in 1:10000){
  set.seed(k)
  y_rstd <- rstd(n=length(y),  mean=0, sd=1, nu=nu)
  KS_std <- stats::ks.test(x=y, y=y_rstd, alternative="two.sided")
  KS_std_mat_np[k,1] <- k
  KS_std_mat_np[k,2] <- KS_std[["p.value"]]}
summary(KS_std_mat_np[,2])
#    Min.     1st Qu.   Median     Mean    3rd Qu.    Max. 
# 0.0000261 0.0693254 0.1982800 0.2557522 0.3870047 0.9837664  
quantile(KS_std_mat_np[,2], na.rm=TRUE, probs=c(0.01,0.05,0.1,0.15,0.20))
#     1%          5%         10%         15%         20% 
# 0.002251536 0.010461490 0.022050476 0.036270374 0.052887738 
#
# In the $15\%$ of cases on 10000 random vectors sampled by the STD distribution, the null hypothesis that the STD distribution fits the 
# distribution of the standardized residuals of the tseries::garch() fitted GARCH(1,1) model is rejected at the $5\%$ significance level, 
# not at the $1\%$ significance level, though. In the $20\%$ of cases on 10000 random vectors sampled by the STD distribution, the null 
# hypothesis that the STD distribution fits the standardized residuals empirical distribution is not rejected at the $5\%$ significance level,
#
# library(goftest)
# The Cramer-Von Mises test in the library *goftest*.
# This function performs the Cramer-Von Mises test of goodness-of-fit to the distribution specified by the argument null. It is assumed that
# the values in x are independent and identically distributed random values, with some cumulative distribution function F. The null 
# hypothesis is that F is the function specified by the argument null, while the alternative hypothesis is that F is some other function.
#
mean <- 0
sd <- 1
nu  <- GED_nu 
goftest::cvm.test(y, null="pged", mean=0, sd=1, nu=nu, estimated=FALSE)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘pged’
# with parameters mean=0, sd=1, nu=0.8742741
# Parameters assumed to be fixed
# data:  y
# omega2=0.23072, p-value=0.2151
#
mean <- 0
sd <- 1
nu  <- STD_nu 
goftest::cvm.test(y, null="pstd", mean=0, sd=1, nu=nu, estimated=FALSE)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘pstd’
# with parameters mean=0, sd=1, nu=3.128975
# Parameters assumed to be fixed
# data:  y
# omega2=0.33769, p-value=0.1062
#
# By default, the Cramer von Mises test assumes that all the parameters of the null distribution are known in advance (a simple null 
# hypothesis). This test does not account for the effect of estimating the parameters.
# If the parameters of the distribution were estimated (that is, if they were calculated from the same data x), then this should be 
# indicated by setting the argument estimated=TRUE. The test will then use the method of Braun (1980) to adjust for the effect of parameter
# estimation. Note that Braun's method involves randomly dividing the data into two equally-sized subsets, so the p-value is not exactly the
# same if the test is repeated. This technique is expected to work well when the number of observations in x is large. However, we approach 
# this version of the test with a technique similar to that we have used in the Kolmogorov-Smirnov test with random sampling. 
#
mean <- 0
sd <- 1
nu  <- GED_nu 
CVM_ged_mat_np <- matrix(NA, nrow=10000, ncol=2)
for(k in 1:10000){
  set.seed(k)
  CVM_ged <- goftest::cvm.test(x=y, null="pged", mean=0, sd=1, nu=nu, estimated=TRUE)
  CVM_ged_mat_np[k,1] <- k
  CVM_ged_mat_np[k,2] <- CVM_ged[["p.value"]]}
summary(CVM_ged_mat_np[,2])
#    Min.    1st Qu.    Median     Mean    3rd Qu.     Max. 
# 0.0002005 0.2559839 0.5039278 0.5037629 0.7534131 0.9999353 
quantile(CVM_ged_mat_np[,2], na.rm=TRUE, probs=c(0.01,0.05))
#    1%         5% 
# 0.01208326 0.05426212
#
# In the $1\%$ of cases on 10000 random vectors sampled by the STD distribution, the null hypothesis that the GED fits the
# distribution of the standardized residuals of the tseries::garch() fitted GARCH(1,1) model is rejected at the $5\%$ significance level,
# not at the $1\%$ significance level, though.
#
mean <- 0
sd <- 1
nu  <- STD_nu 
CVM_std_mat_np <- matrix(NA, nrow=10000, ncol=2)
for(k in 1:10000){
  set.seed(k)
  CVM_std <- goftest::cvm.test(x=y, null="pstd", mean=0, sd=1, nu=nu, estimated=TRUE)
  CVM_std_mat_np[k,1] <- k
  CVM_std_mat_np[k,2] <- CVM_std[["p.value"]]}
summary(CVM_std_mat_np[,2])
#    Min.    1st Qu.    Median     Mean     3rd Qu.    Max. 
# 0.0001871 0.2583397 0.5037986 0.5024513 0.7489603 0.9999194 
quantile(CVM_std_mat_np[,2], na.rm=TRUE, probs=c(0.01,0.05))
#     1%         5%      
# 0.01209338 0.05331370
#
# In the $1\%$ of cases on 10000 random vectors sampled by the STD distribution, the null hypothesis that the GED fits the
# distribution of the standardized residuals of the tseries::garch() fitted GARCH(1,1) model is rejected at the $5\%$ significance level, 
# not at the $1\%$ significance level, though.
#
# The Anderson-Darling test in the library *goftest*.
mean <- 0
sd <- 1
nu  <- GED_nu 
goftest::ad.test(y, null="pged", mean=0, sd=1, nu=nu, estimated=FALSE)
#
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘pged’
# with parameters mean=0, sd=1, nu=0.87427285949957
# Parameters assumed to be fixed
# data:  y
# An=1.2966, p-value=0.2334
#
mean <- 0
sd <- 1
nu  <- STD_nu 
goftest::ad.test(y, null="pstd", mean=0, sd=1, nu=nu, estimated=FALSE)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘pstd’
# with parameters mean=0, sd=1, nu=3.12896640299425
# Parameters assumed to be fixed
# data:  y
# An=2.1305, p-value=0.07796
#
# By default, also the Anderson Darling test assumes that all the parameters of the null distribution are known in advance (a simple null 
# hypothesis). This test does not account for the effect of estimating the parameters.
# If the parameters of the distribution were estimated (that is, if they were calculated from the same data x), then this should be 
# indicated by setting the argument estimated=TRUE. The test will then use the method of Braun (1980) to adjust for the effect of parameter
# estimation. Note that Braun's method involves randomly dividing the data into two equally-sized subsets, so the p-value is not exactly the
# same if the test is repeated. This technique is expected to work well when the number of observations in x is large. However, we approach 
# this version of the test with the same technique that we have used in the Cramer von Mises test. 
#
mean <- 0
sd <- 1
nu  <- GED_nu 
AD_ged_mat_np <- matrix(NA, nrow=10000, ncol=2)
for(k in 1:10000){
  set.seed(k)
  AD_ged <- goftest::ad.test(x=y, null="pged", mean=0, sd=1, nu=nu, estimated=TRUE)
  AD_ged_mat_np[k,1] <- k
  AD_ged_mat_np[k,2] <- AD_ged[["p.value"]]}
summary(AD_ged_mat_np[,2])
#    Min.    1st Qu.    Median     Mean    3rd Qu.     Max. 
# 0.0006379 0.2842479 0.5430350 0.5290502 0.7805338 0.9999868 
quantile(AD_ged_mat_np[,2], na.rm=TRUE, probs=c(0.01,0.05))
#      1%         5% 
# 0.01443705 0.06348761
#
# In the $1\%$ of cases on 10000 random vectors sampled by the STD distribution, the null hypothesis that the GED fits the
# distribution of the standardized residuals of the tseries::garch() fitted GARCH(1,1) model is rejected at the $5\%$ significance level,
# not at the $1\%$ significance level, though.
#
mean <- 0
sd <- 1
nu  <- STD_nu 
AD_std_mat_np <- matrix(NA, nrow=10000, ncol=2)
for(k in 1:10000){
  set.seed(k)
  AD_std <- goftest::ad.test(x=y, null="pstd", mean=0, sd=1, nu=nu, estimated=TRUE)
  AD_std_mat_np[k,1] <- k
  AD_std_mat_np[k,2] <- AD_std[["p.value"]]}
summary(AD_std_mat_np[,2])
#    Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0005864 0.2260114 0.4556840 0.4678446 0.7012909 0.9987752 
quantile(AD_std_mat_np[,2], na.rm=TRUE, probs=c(0.01,0.05,0.1,0.15,0.20))
#     1%          5%         10%       
# 0.008628736 0.044969446 0.088366621

# In the $5\%$ of cases on 10000 random vectors sampled by the STD distribution, the null hypothesis that the GED fits the
# standardized residuals empirical distribution is rejected at the $5\%$ significance level, not at the $1\%$ significance level, though.
#
# The goodness of fit tests yield computational evidence that the estimated GED fits the empirical distribution of the standardized 
# residuals of the tseries::garch() fitted GARCH(1,1) model slightly better than the estimated STD. Overall, the Q-Q plots, P-P plots, 
# and Kolmogorov-Smirnov tests highlight a failure of the estimated STD in the central part of the distribution, where the estimated GED 
# performs better. However, the estimated distribution of the standardized residuals of the tseries::garch() fitted GARCH(1,1) model for 
# the Bitcoin daily logarithm return percentage training set built using the tseries::garch() function is definitively far from being 
# Gaussian. This is a non-negligible problem for the model validation that we should tackle. Although we could carry out a handmade
# prediction procedure based on the estimated model and standardized residual distribution, the coefficients of the GARCH(1,1) model remain 
# estimated under the assumption of Gaussian-distributed standardized residuals. This renders their estimation less reliable than desirable.
###########################################################################################################################################
###########################################################################################################################################
###########################################################################################################################################
###########################################################################################################################################
############################################################################################################################################
############################################################################################################################################
# The tseries::garch() function, although appreciable for its simplicity, does not allow us to deal with innovation distributions other than 
# the Gaussian distribution (?!). Such a possibility seems to be offered by the function fGarch::garchFit(), which includes options for 
# choosing a GED or an STD or even their skewed modifications as the innovation distribution. Therefore, we start exploring the features
# of the fGarch::garchFit() function by re-estimating the GARCH(1,1).
# First, we consider the options of fGarch::garchFit() which replicates the results of the tseries::garch() function.
Data_df <- BTC_train_df
head(Data_df)
tail(Data_df)
y <- na.rm(Data_df$log_ret_perc)
head(y,20)
#  3.2535928  1.5906496  6.4376485  0.5608363 -1.0523309  1.4483698  8.2353199 -9.1932107  4.8088210 -3.2239593  3.9429183  0.7523614
# -1.9136041 -1.3240167  1.2739001  5.3537158 -0.4433170  1.6094355 -2.0843383 -2.9620923
# 
# The cond.dist="norm" option calls for using a Gaussian standard distribution as the innovation distribution. The algorithm="lbfgsb" 
# option invokes the limited-memory Broyden–Fletcher–Goldfarb–Shanno algorithm with box constraints for the model estimation.
fGARCH_1_1 <- fGarch::garchFit(formula=~garch(1,1), data=y, init.rec="mci", cond.dist="norm", include.mean=FALSE, include.skew=FALSE, 
                               include.shape=FALSE, trace=TRUE, algorithm="lbfgsb")
# Extracted from the output.
# Series Initialization:
# ARMA Model:                arma
# Formula Mean:              ~ arma(0, 0)
# GARCH Model:               garch
# Formula Variance:          ~ garch(1, 1)
# ARMA Order:                0 0
# Max ARMA Order:            0
# GARCH Order:               1 1
# Max GARCH Order:           1
# Maximum Order:             1
# Conditional Dist:          norm
# h.start:                   2
# llh.start:                 1
# Length of Series:          1719
# Recursion Init:            mci
# Series Scale:              3.744706
# 
# Parameter Initialization:
# Initial Parameters:          $params
# Limits of Transformations:   $U, $V
# Which Parameters are Fixed?  $includes
# Parameter Matrix:
#              U           V      params includes
# mu     -0.11481907   0.1148191    0.0    FALSE
# omega   0.00000100 100.0000000    0.1     TRUE
# alpha1  0.00000001   1.0000000    0.1     TRUE
# gamma1 -0.99999999   1.0000000    0.1    FALSE
# beta1   0.00000001   1.0000000    0.8     TRUE
# delta   0.00000000   2.0000000    2.0    FALSE
# skew    0.10000000  10.0000000    1.0    FALSE
# shape   1.00000000  10.0000000    4.0    FALSE
# Index List of Parameters to be Optimized:
#   omega alpha1  beta1 
#     2      3      5 
# Persistence:                  0.9 
# 
# --- START OF TRACE ---
#   Selected Algorithm: lbfgsb 
# 
# R coded optim[L-BFGS-B] Solver: 
# final  value 2384.726788 
# converged
# 
# Final Estimate of the Negative LLH: 4654.397  norm LLH: 2.707619 
#   omega     alpha1      beta1 
# 1.27007192 0.09084851 0.82533630 
# 
# R-optimhess Difference Approximated Hessian Matrix:
#           omega     alpha1     beta1
# omega   -197.5147  -1610.888  -2400.93
# alpha1 -1610.8885 -19996.159 -22922.24
# beta1  -2400.9298 -22922.241 -32251.41
# 
# --- END OF TRACE ---
#
summary(fGARCH_1_1)
# Extracted from the output.
# Title: GARCH Modelling 
# Call: fGarch::garchFit(formula=~garch(1, 1), data=y, init.rec="mci", cond.dist="norm", include.mean=FALSE, include.skew=FALSE,
#                        include.shape=FALSE, trace=TRUE, algorithm="lbfgsb") 
# Conditional Distribution: norm 
# Coefficient(s): omega    alpha1     beta1  
#               1.270072  0.090849  0.825336  
# 
# Std. Errors: based on Hessian 
# Error Analysis: Estimate  Std. Error  t value Pr(>|t|)    
#         omega    1.27007     0.24757    5.130 2.90e-07 ***
#         alpha1   0.09085     0.01763    5.154 2.55e-07 ***
#         beta1    0.82534     0.02636   31.309  < 2e-16 ***
#   ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Log Likelihood: -4654.397    normalized:  -2.707619 
# 
# Standardised Residuals Tests:    
#                                  Statistic   p-Value
# Jarque-Bera Test   R    Chi^2  2.263033e+04 0.0000000
# Shapiro-Wilk Test  R    W      8.983107e-01 0.0000000
# Ljung-Box Test     R    Q(10)  8.792208e+00 0.5519309
# Ljung-Box Test     R    Q(15)  1.071485e+01 0.7725338
# Ljung-Box Test     R    Q(20)  1.290943e+01 0.8812361
# Ljung-Box Test     R^2  Q(10)  3.688948e+00 0.9602901
# Ljung-Box Test     R^2  Q(15)  4.044115e+00 0.9975893
# Ljung-Box Test     R^2  Q(20)  5.057586e+00 0.9996969
# LM Arch Test       R    TR^2   3.680448e+00 0.9885371
#
# Information Criterion Statistics:
#   AIC      BIC      SIC     HQIC 
# 5.418728 5.428238 5.418722 5.422247
#
# Uncomment and execute the following line
# plot(fGARCH_1_1)
# From the autocorrelograms of the standardized and squared standardized residuals, we have no visual evidence of autocorrelation at the 
# $5\%$ significance level. However, the Q-Q plot highlights the poor fit between the standardized residuals and the quantiles of the
# hypothesized standard Gaussian distribution of the innovation.
#
# Note that the estimated parameters of the GARCH(1,1) model by the function fGarch::garchFit() are very similar to the estimated parameters
# by the function tseries::garch(). Eventually, we have
a0 <- as.numeric(GARCH_1_1$coef[1])
a1 <- as.numeric(GARCH_1_1$coef[2])
b1 <- as.numeric(GARCH_1_1$coef[3])
show(c(a0, a1, b1))
#  1.27250304 0.09083005 0.82515011
#
omega  <- as.numeric(fGARCH_1_1@fit[["par"]][["omega"]])
alpha1 <- as.numeric(fGARCH_1_1@fit[["par"]][["alpha1"]])
beta1  <- as.numeric(fGARCH_1_1@fit[["par"]][["beta1"]])
show(c(omega, alpha1, beta1))
# 1.27007192 0.09084851 0.82533630
#
# Therefore,
show(c(abs(a0-omega),abs(a1-alpha1),abs(b1-beta1)))
# 0.00243112277 0.00001846604 0.00018619234
#
# Thus, the coefficient estimates differ by much less than the corresponding standard errors computed by both the tseries::garch() and
# fGarch::garchFit() functions (see summaries) reported below.
#     a0_se  0.17990      a1_se   0.01108     b1_se  0.02166
#  omega_se  0.24757  alpha1_se   0.01763  beta1_se  0.02636 
#
# Note also that the coefficient estimates of the fGARCH_1_1 model can also be obtained by applying the function fGarch::coef(). 
# Similarly, the functions fGarch::residuals() and fGarch::fitted() return the residuals and the fitted values of the model. In fact, 
omega_bis <- as.numeric(fGarch::coef(fGARCH_1_1)[1])
alpha1_bis <- as.numeric(fGarch::coef(fGARCH_1_1)[2])
beta1_bis <- as.numeric(fGarch::coef(fGARCH_1_1)[3])
show(c(omega_bis, alpha1_bis, beta1_bis))
# 1.27007192 0.09084851 0.82533630
#
identical(c(omega, alpha1, beta1),c(omega_bis, alpha1_bis, beta1_bis))
# TRUE
#
fGARCH_1_1_resid <- fGARCH_1_1@residuals
head(fGARCH_1_1_resid,20)
#  3.2535928  1.5906496  6.4376485  0.5608363 -1.0523309  1.4483698  8.2353199 -9.1932107  4.8088210 -3.2239593  3.9429183  0.7523614
# -1.9136041 -1.3240167  1.2739001  5.3537158 -0.4433170  1.6094355 -2.0843383 -2.9620923
#
fGARCH_1_1_resid_bis <- fGarch::residuals(fGARCH_1_1, standardize=FALSE)
head(fGARCH_1_1_resid_bis,20)
#  3.2535928  1.5906496  6.4376485  0.5608363 -1.0523309  1.4483698  8.2353199 -9.1932107  4.8088210 -3.2239593  3.9429183  0.7523614
# -1.9136041 -1.3240167  1.2739001  5.3537158 -0.4433170  1.6094355 -2.0843383 -2.9620923
#
identical(fGARCH_1_1_resid,fGARCH_1_1_resid_bis)
# TRUE
#
fGARCH_1_1_fit <- fGARCH_1_1@fitted
head(fGARCH_1_1_fit,20)
# 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
# 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 
# 
fGARCH_1_1_fit_bis <- fGarch::fitted(fGARCH_1_1)
head(fGARCH_1_1_fit_bis,20)
# 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
# 0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 
#
identical(fGARCH_1_1_fit,fGARCH_1_1_fit_bis)
# TRUE
#
# The fitted values being all zero might appear to be a somewhat surprising result. However, recall that for any GARCH(p,q) process with a
# state process $\left(Z_{t}\right)_{t\in\mathbb{N}_{0}$ and available information $\left(\mathcal{F}_{t}\right)_{t\in\mathbb{N}_{0}$ we 
# always have $\mathbf{E}\left[Z_{t}\mid\mathcal{F}_{t-s}\right]=0$, for every $t\in\mathbb{N}$ and every $s=1,\dots,t$,
# Therefore, we should have expected the fitted values of the states y=na.rm(Data_df$log_ret_perc) are all zero. On the other hand, we have
# $Res\left(Z_{t}\right)=Z_{t}-\mathbf{E}\left[Z_{t}\mid\mathcal{F}_{t-1}\right]=Z_{t}$, for every $t\in\mathbb{N}$.
# Therefore, we should expect that the residuals of the states y=na.rm(Data_df$log_ret_perc) coincide with the states themselves. 
# Nevertheless, we have
fGARCH_1_1_resid==y
# [1]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [24]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [47]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [70]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [93]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [116]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [139]  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
# [162]  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [185]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [208]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [231]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [254]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [277]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [300]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE
# [323]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [346]  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
# [369]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [392]  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE
# [415]  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE
# [438]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [461]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [484]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [507]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [530]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [553]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [576]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [599] FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [622]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [645]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [668] FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [691]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [714]  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE
# [737]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [760]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [783]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [806]  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE
# [829]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [852]  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [875]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [898]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [921]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [944]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE
# [967]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [990]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE
# [ reached getOption("max.print") -- omitted 719 entries ]
# 
# This because
format(round(fGARCH_1_1_resid[11],16), nsmall=16)
"3.9429182854110674"
format(round(y[11],16), nsmall=16)
"3.9429182854110678"
#
# However, rounding up to the 13th decimal digit,
identical(round(fGARCH_1_1_resid,13),round(y,13))
# TRUE
#
# Note that the object GARCH_1_1[["fitted.values"]] (presented above) despite the similar denomination of 
# fGARCH_1_1@fitted has the the rather different role of a confidence band.
#
# Another issue is what the standardized residuals of the fGarch::garchFit() fitted GARCH(1,1) (from now on fGARCH_1_1) model are precisely.
# We have
fGARCH_1_1_stand_res <- fGarch::residuals(fGARCH_1_1, standardize=TRUE)
head(fGARCH_1_1_stand_res,20)
#  0.8661082  0.4269719  1.7886102  0.1414207 -0.2784887  0.3993254  2.3464161 -2.1914573  0.9925632 -0.6760192  0.8607116  0.1682222
# -0.4531814 -0.3277551  0.3300456  1.4457249 -0.1137383  0.4328310 -0.5799489 -0.8437155
#
# Therefore, the standardized residuals appears to be rather similar to the residuals of the tseries::garch() fitted GARCH(1,1) (from now on
# GARCH_1_1) model.
head(GARCH_1_1_stand_res,20)
#  0.8360355  0.4144350  1.7419601  0.1388852 -0.2739446  0.3934785  2.3160671 -2.1750189  0.9879875 -0.6733973  0.8577650  0.1677307
# -0.4519716 -0.3269755  0.3293429  1.4429839 -0.1135800  0.4322909 -0.5793139 -0.8429184
#
# To solve this issue we consider the generation of the path of the positive process $\sigma_{t}$ in the fGARCH_1_1 model.
# First, we recall the basic variables.
Data_df <- BTC_train_df
head(Data_df)
tail(Data_df)
y <- na.rm(Data_df$log_ret_perc)
T <- length(y)
show(T)
# 1719
omega  <- as.numeric(fGARCH_1_1@fit[["par"]][["omega"]])
alpha1 <- as.numeric(fGARCH_1_1@fit[["par"]][["alpha1"]])
beta1  <- as.numeric(fGARCH_1_1@fit[["par"]][["beta1"]])
show(c(omega, alpha1, beta1))
# 1.27007192 0.09084851 0.82533630
#
# Second, from (22) p. 15 and (23) p. 16 of the paper https://www.math.pku.edu.cn/teachers/heyb/TimeSeries/lectures/garch.pdf, we have found 
# that the conditional variance should be initialized by setting
fGARCH_1_1_sigma0 <- omega + (alpha1+beta1)*(1/T)*sum(y^2)
show(fGARCH_1_1_sigma0)
# 14.11179
#
# Hence, we write the generation procedure.
fGARCH_1_1_cond_var_est <- vector(mode="numeric", length=T)
fGARCH_1_1_cond_var_est[1] <- fGARCH_1_1_sigma0
for(t in 2:T){
  fGARCH_1_1_cond_var_est[t] <- omega + alpha1*y[t-1]^2 + beta1*fGARCH_1_1_cond_var_est[t-1]
}
# The conditional variance, is then given by
head(fGARCH_1_1_cond_var_est,20)
# 14.11179 13.87875 12.95457 15.72701 14.27872 13.15543 12.31830 17.59821 23.47258 22.74370 20.98554 20.00259 17.83036 16.31879 14.89782
# 13.71322 15.19201 13.82644 12.91686 12.32552
#
# Consequently, the conditional standard deviation, is
head(sqrt(fGARCH_1_1_cond_var_est),20)
# 3.756566 3.725420 3.599246 3.965730 3.778720 3.627041 3.509744 4.195021 4.844851 4.769035 4.580998 4.472425 4.222601 4.039652 3.859770
# 3.703136 3.897693 3.718393 3.594004 3.510771
#
# Note that the objects fGARCH_1_1@h.t and fGARCH_1_1@sigma.t or equivalently the extractor functions fGarch::volatility( , type="h") and
# fGarch::volatility( , type="sigma") are supposed to return the conditional variance and the conditional standard deviation from the
# fGARCH_1_1 model, respectively. However, we have
head(fGARCH_1_1@h.t, 20) 
# or equivalently
head(fGarch::volatility(fGARCH_1_1, type="h"), 20)
# 14.11179 13.87875 12.95457 15.72701 14.27872 13.15543 12.31830 17.59821 23.47258 22.74370 20.98554 20.00259 17.83036 16.31879 14.89782
# 13.71322 15.19201 13.82644 12.91686 12.32552
#
# and eventually,
identical(round(fGARCH_1_1_cond_var_est,12),round(fGARCH_1_1@h.t,12))
# TRUE
#
# and
head(fGARCH_1_1@sigma.t, 20)
# or equivalently
head(fGarch::volatility(fGARCH_1_1, type="sigma"), 20)
# 3.756566 3.725420 3.599246 3.965730 3.778720 3.627041 3.509744 4.195021 4.844851 4.769035 4.580998 4.472425 4.222601 4.039652 3.859770
# 3.703136 3.897693 3.718393 3.594004 3.510771
# 
# and eventually,
identical(round(sqrt(fGARCH_1_1_cond_var_est),13),round(fGARCH_1_1@sigma.t,13))
# TRUE
#
# Now, referring the defining stochastic equation of the GARCH  models
# $\Z_{t}=\sigma_{t}W_{t}$,
# to the path of the positive process $\sigma_{t}$ that we have determined, we obtain
head(y/fGARCH_1_1@sigma.t, 20)
#  0.8661082  0.4269719  1.7886102  0.1414207 -0.2784887  0.3993254  2.3464161 -2.1914573  0.9925632 -0.6760192  0.8607116  0.1682222
# -0.4531814 -0.3277551  0.3300456  1.4457249 -0.1137383  0.4328310 -0.5799489 -0.8437155
#
# This should be compared with
head(fGARCH_1_1_stand_res, 20)
#  0.8661082  0.4269719  1.7886102  0.1414207 -0.2784887  0.3993254  2.3464161 -2.1914573  0.9925632 -0.6760192  0.8607116  0.1682222
# -0.4531814 -0.3277551  0.3300456  1.4457249 -0.1137383  0.4328310 -0.5799489 -0.8437155
#
# Eventually,
identical(round(y/fGARCH_1_1@sigma.t, 13), round(fGARCH_1_1_stand_res, 13))
# TRUE
#
# Similarly,
head(GARCH_1_1_stand_res, 20)
#  0.8360355   0.4144350  1.7419601  0.1388852 -0.2739446  0.3934785  2.3160671 -2.1750189  0.9879875 -0.6733973  0.8577650  0.1677307 
# -0.4519716  -0.3269755  0.3293429  1.4429839 -0.1135800  0.4322909 -0.5793139 -0.8429184 -0.4242169
#
# Therefore, also the vector fGARCH_1_1_stand_res is just the path of the noise process $W_{t}$ which contributes jointly to 
# the path of the conditional standard deviation $\hat{sigma}_{t\mid t-1}$ to the realization of the states of the process.
#
# To summarize, the standardized residuals of the fGARCH_1_1 model are analogous to the residuals of the GARCH_1_1 model. In particular,
# from the summary of the fGARCH_1_1 model, we can realize that the standardized residuals of the fGARCH_1_1 model fail to fit the standard
# Gaussian distribution.
# Standardised Residuals Tests:    Statistic   p-Value
# Jarque-Bera Test   R    Chi^2  2.263033e+04 0.0000000
# Shapiro-Wilk Test  R    W      8.983107e-01 0.0000000
# Ljung-Box Test     R    Q(10)  8.792208e+00 0.5519309
# Ljung-Box Test     R    Q(15)  1.071485e+01 0.7725338
# Ljung-Box Test     R    Q(20)  1.290943e+01 0.8812361
# Ljung-Box Test     R^2  Q(10)  3.688948e+00 0.9602901
# Ljung-Box Test     R^2  Q(15)  4.044115e+00 0.9975893
# Ljung-Box Test     R^2  Q(20)  5.057586e+00 0.9996969
# LM Arch Test       R    TR^2   3.680448e+00 0.9885371
#
# Note that the Ljung-Box Test on the standardized residuals and squared standardized residuals are executed setting to zero the number of
# parameters estimated by the model (?!).
y <- fGARCH_1_1_stand_res
head(y,20)
#  0.8661082  0.4269719  1.7886102  0.1414207 -0.2784887  0.3993254  2.3464161 -2.1914573  0.9925632 -0.6760192  0.8607116  0.1682222
# -0.4531814 -0.3277551  0.3300456  1.4457249 -0.1137383  0.4328310 -0.5799489 -0.8437155
#
max_lag <- ceiling(min(10, n_obs/4))    # Hyndman (for data without seasonality)
show(max_lag)
# 10
n_coeffs <- nrow(y_ADF_ur.df_trend_0_lags@testreg[["coefficients"]])
show(n_coeffs)
# 3
n_pars <- n_coeffs
show(n_pars)
# 3
Box.test(y, lag=max_lag, fitdf=n_pars, type="Ljung-Box")
# Box-Ljung test, data:  y
# X-squared=8.7922, df=7, p-value=0.2679
#
Box.test(y, lag=max_lag, fitdf=0, type="Ljung-Box")
# Box-Ljung test, data:  y
# X-squared=8.7922, df=10, p-value=0.5519
#
Box.test(y^2, lag=max_lag, fitdf=n_pars, type="Ljung-Box")
# Box-Ljung test, data:  y^2
# X-squared=3.6889, df=7, p-value=0.8148
#
Box.test(y^2, lag=max_lag, fitdf=0, type="Ljung-Box")
# Box-Ljung test, data:  y^2
# X-squared=3.6889, df=10, p-value=0.9603
#
# The lack of autocorrelation in the stardardized residuals and squared standardized residuals can also be grasped by the draft plots 
# corresponding to the selection 10 and 11 of
# plot(fGARCH_1_1)
# Uncomment and execute the above line
#
# the lack of Gaussianity in the empirical distribution of the standardized residuals can also be grasped by the draft plot corresponding to
# the selection 13 of
# plot(fGARCH_1_1)
# Uncomment and execute the above line
#
# Again, given the default options, the GARCH(1,1) model is estimated by the fGarch::garchFit() function  under the assumption that the 
# innovation $\left(W_{t}\right)_{t=1}^{T}\equiv W$ is a Standard Gaussian Distributed (SGD) strong white noise. Consequently, to validate
# the model we should check that the standardized residuals are standard Gaussian distributed. We start with plotting them.
head(BTC_train_df)
tail(BTC_train_df)
BTC_train_df <- add_column(BTC_train_df, 
                           fGARCH_1_1_stand_res=c(NA,fGARCH_1_1_stand_res), fGARCH_1_1_cond_stand_dev=c(NA,fGARCH_1_1@sigma.t),
                           .after="GARCH_1_1_cond_stand_dev")
head(BTC_train_df)
# As we did above, we draw the scatter and line plot of the the standardized residuals of the GARCH(1,1) model for the Bitcoin daily
# logarithm return percentage estimated by the fGarch::garchFit() function given the default options. The plots will be very similar to the 
# ones presented above, as the values of the standardized residuals estimated by the tseries::garch() function and
# fGarch::garchFit() function given the default options are very similar.
Data_df <- BTC_train_df
head(Data_df)
tail(Data_df)
Data_df <- dplyr::rename(Data_df, x=t, y=fGARCH_1_1_stand_res)
TrnS_length <- length(na.rm(Data_df$y[which(Data_df$Date<as.Date("2023-01-01"))]))
show(TrnS_length)
# 1719
First_Day <- as.character(Data_df$Date[2])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Standardized Residuals of the fGarch::garchFit() Fitted GARCH(1,1) Model with SGD Innovation for the Bitcoin Daily Logarithm Return Percentage - Training Set - from ", .(First_Day), " to ", .(Last_Day), sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points. Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("")
# numbers::primeFactors(TrnS_length-2)
x_breaks_num <- 44 # (deduced from primeFactors(TrnS_length-2))
x_breaks_low <- Data_df$x[2]
x_breaks_up <- Data_df$x[nrow(Data_df)]
x_binwidth <- ceiling((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Date[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("standardized residuals (US $)")
y_max <- max(na.rm(Data_df$y))
y_min <- min(na.rm(Data_df$y))
as.numeric(floor(y_max-y_min))
y_breaks_num <- 10
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor((y_min/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((y_max/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth), digits=3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.5
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
point_black <- bquote("GARCH(1,1) stand. residuals")
line_green <- bquote("regression line")
line_red <- bquote("LOESS curve")
leg_point_labs <- c(point_black)
leg_point_cols <- c("point_black"="black")
leg_point_breaks <- c("point_black")
leg_line_labs <- c(line_green, line_red)
leg_line_cols <- c("line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_green", "line_red")
leg_col_labs   <- c(leg_point_labs,leg_line_labs)
leg_col_cols   <- c(leg_point_cols,leg_line_cols)
leg_col_breaks <- c(leg_point_breaks,leg_line_breaks)
BTC_fGARCH_1_1_stand_res_TrnS_sp <- ggplot(Data_df, aes(x=x)) +
  geom_smooth(aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_point(aes(y=y, color="point_black"), alpha=1, size=0.7, shape=19, na.rm=TRUE) + 
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  guides(colour=guide_legend(override.aes=list(shape=c(19, NA, NA), linetype=c("blank", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_fGARCH_1_1_stand_res_TrnS_sp)
#
# The line plot
BTC_fGARCH_1_1_stand_res_TrnS_lp <- ggplot(Data_df, aes(x=x)) +
  geom_smooth(aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_line(aes(y=y, color="point_black"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_fGARCH_1_1_stand_res_TrnS_lp)
#
# As we did above, we superimpose the conditional standard deviation of the GARCH(1,1) model estimated by the fGarch::garchFit() function to
# the plots of the Bitcoin daily logarithm return percentage.
# The scatter plot.
head(BTC_train_df)
tail(BTC_train_df)
Data_df <- BTC_train_df
head(Data_df)
tail(Data_df)
Data_df <- dplyr::rename(Data_df, x=t, y=log_ret_perc, z=fGARCH_1_1_cond_stand_dev)
head(Data_df)
tail(Data_df)
First_Day <- as.character(Data_df$Date[3])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Bitcoin Daily Logarithm Return Percentage and Conditional Standard Deviation of the fGarch::garchFit() Fitted GARCH(1,1) Model with SGD Innovation - Training Set - from ", .(First_Day), " to ", .(Last_Day), sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points. Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("")
# numbers::primeFactors(TrnS_length-2)
x_breaks_num <- 44 # (deduced from primeFactors(TrnS_length-2))
x_breaks_low <- Data_df$x[2]
x_breaks_up <- Data_df$x[nrow(Data_df)]
x_binwidth <- ceiling((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Date[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("daily logarithm return percentage")
y_max <- max(na.rm(Data_df$y))
y_min <- min(na.rm(Data_df$y))
as.numeric(floor(y_max-y_min))
y_breaks_num <- 10
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor((y_min/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((y_max/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth), digits=3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.5
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
point_black <- bquote("GARCH(1,1) stand. residuals")
leg_point_labs <- c(point_black)
leg_point_cols <- c("point_black"="black")
leg_point_breaks <- c("point_black")
line_magenta <- bquote("GARCH(1,1) cond. stand. dev.")
line_green <- bquote("regression line")
line_red <- bquote("LOESS curve")
leg_line_labs <- c(line_magenta, line_green, line_red)
leg_line_cols <- c("line_magenta"="magenta", "line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_magenta", "line_green", "line_red")
leg_col_labs   <- c(leg_point_labs,leg_line_labs)
leg_col_cols   <- c(leg_point_cols,leg_line_cols)
leg_col_breaks <- c(leg_point_breaks,leg_line_breaks)
BTC_perc_log_ret_fGARCH_1_1_cond_stand_dev_TrnS_sp <- ggplot(Data_df, aes(x=x)) +
  geom_smooth(aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_point(aes(y=y, color="point_black"), alpha=1, size=0.7, shape=19, na.rm=TRUE) + 
  geom_line(aes(y=z, color="line_magenta"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  geom_line(aes(y=-z, color="line_magenta"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  guides(colour=guide_legend(override.aes=list(shape=c(19, NA, NA, NA), linetype=c("blank", "solid", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_perc_log_ret_fGARCH_1_1_cond_stand_dev_TrnS_sp)
#
# The line plot.
line_black <- bquote("perc. log. returns")
line_magenta <- bquote("GARCH(1,1) cond. stand. dev.")
line_green <- bquote("regression line")
line_red <- bquote("LOESS curve")
leg_line_labs <- c(line_black, line_magenta, line_green, line_red)
leg_line_cols <- c("line_black"="black", "line_magenta"="magenta", "line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_black", "line_magenta", "line_green", "line_red")
BTC_perc_log_ret_fGARCH_1_1_cond_stand_dev_TrnS_lp <- ggplot(Data_df, aes(x=x)) +
  geom_smooth(aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_line(aes(y=y, color="line_black"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  geom_line(aes(y=z, color="line_magenta"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  geom_line(aes(y=-z, color="line_magenta"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="", labels=leg_line_labs, values=leg_line_cols, breaks=leg_line_breaks) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_perc_log_ret_fGARCH_1_1_cond_stand_dev_TrnS_lp)
# 
# From the computed values and plots of the standardized residuals and conditional standard deviation of the GARCH(1,1) models for the 
# Bitcoin daily percentage results estimated by the tseries:garch() function and fGarch:garchFit() function with the default options, it is
# possible to grasp that the two models are very similar. The only difference seems to be in the initialization of the conditional variance.
# In particular, both models are estimated under the assumption of Standard Gaussian Distributed (SGD) innovation. We have shown that the
# analysis of the standardized residuals of the model estimated by the tseries:garch() function contradicts this assumption. We had better 
# think that the standardized residuals are GED or STD distributed. This is clearly true for the standardized residuals of the model 
# estimated by the fGarch:garchFit() function. We do not repeat the long and almost identical analysis, though. On the other hand, suitably
# selecting some options, the fGarch:garchFit() function offers the possibility of estimating the GARCH(1,1) models under the assumption of 
# innovation distribution other than Gaussian. Therefore, we will explore this possibility to check whether it may lead to a more accurate
# estimation.
############################################################################################################################################
# We now estimate the GARCH(1,1) models showing how the options of the fGarch:garchFit() function allow us to consider the possibility that 
# the innovation distribution is not Gaussian.
Data_df <- BTC_train_df
head(Data_df)
tail(Data_df)
y <- na.rm(Data_df$log_ret_perc)
head(y,20)
#  3.2535928  1.5906496  6.4376485  0.5608363 -1.0523309  1.4483698  8.2353199 -9.1932107  4.8088210 -3.2239593  3.9429183  0.7523614
# -1.9136041 -1.3240167  1.2739001  5.3537158 -0.4433170  1.6094355 -2.0843383 -2.9620923
# 
# We start with considering the nonlinear minimization subject to box constraints "nlminb" algorithm. The options cond.dist="ged" and 
# include.shape=NULL consider a GED innovation and invoke the estimation of the shape parameter.
fGARCH_1_1_ged_shpN_nlminb <- fGarch::garchFit(formula=~garch(1,1), data=y, init.rec="mci", cond.dist="ged", include.mean=FALSE,  
                                               include.skew=FALSE, include.shape=NULL, trace=TRUE, algorithm="nlminb")
# Extract from the trace output.
# Series Initialization:
#   ARMA Model:                arma
# Formula Mean:              ~ arma(0, 0)
# GARCH Model:               garch
# Formula Variance:          ~ garch(1, 1)
# ARMA Order:                0 0
# Max ARMA Order:            0
# GARCH Order:               1 1
# Max GARCH Order:           1
# Maximum Order:             1
# Conditional Dist:          ged
# h.start:                   2
# llh.start:                 1
# Length of Series:          1719
# Recursion Init:            mci
# Series Scale:              3.744706
# 
# Parameter Initialization:
#   Initial Parameters:          $params
# Limits of Transformations:   $U, $V
# Which Parameters are Fixed?  $includes
# Parameter Matrix:
#   U           V params includes
# mu     -0.11481907   0.1148191    0.0    FALSE
# omega   0.00000100 100.0000000    0.1     TRUE
# alpha1  0.00000001   1.0000000    0.1     TRUE
# gamma1 -0.99999999   1.0000000    0.1    FALSE
# beta1   0.00000001   1.0000000    0.8     TRUE
# delta   0.00000000   2.0000000    2.0    FALSE
# skew    0.10000000  10.0000000    1.0    FALSE
# shape   1.00000000  10.0000000    4.0     TRUE
# Index List of Parameters to be Optimized:
#   omega alpha1  beta1  shape 
#     2      3      5      8 
# Persistence:                  0.9 
# 
# 
# --- START OF TRACE ---
#   Selected Algorithm: nlminb 
# 
# R coded nlminb Solver: 
#   
#   0: 1.1000000e+99: 0.100000 0.100000 0.800000  4.00000
# 1: 1.1000000e+99: 0.100000 0.100000 0.800000  4.00000
# 
# Final Estimate of the Negative LLH:  15158673573804500815488446204484204406004042024404046082422606242064226020486482682066620800844246624486    norm LLH:  8818309234324899608026462422444062044626824822868266440026660468826444642688486204680484246640060004 
#   omega   alpha1    beta1    shape 
# 1.402282 0.100000 0.800000 4.000000 
# 
# R-optimhess Difference Approximated Hessian Matrix:
#   omega
# omega     -4943885520426178748046424880664822866886288682886680428686868202600202684884848682600088666686880442402
# alpha1  -201221340853522147200048248620880204848020446066806062042466020800680868242664424282280862680824820660264
# beta1   -782630796560580336320860422882420448228068602008064284886628208406422008840242422664248620688868648248664
# shape  -3424376615469783005808466864488040082840606040222420664068426080420624642402002606666644866802686622082482
# alpha1
# omega    -201221340853522147200048248620880204848020446066806062042466020800680868242664424282280862680824820660264
# alpha1  -5405147993958730610468460684080608004882244486886428288204228226484644088820828662220004880484840662420462
# beta1  -15688369046096696381840846280864224048604680240266266008448288048440086088088220622206002206888662266280222
# shape  -61018382598994881284024008600626480642664840022440060882646884684820026042062464682804024802626444088042248
# beta1
# omega    -782630796560580336320860422882420448228068602008064284886628208406422008840242422664248620688868648248664
# alpha1 -15688369046096696381840846280864224048604680240266266008448288048440086088088220622206002206888662266280222
# beta1  -30052175913598978352042084684004046860286686846064204226824262086000888040806040086822446844242208886204868
# shape  -87226034679848494872204240282286422426480204246808460064464824408860062824084284220080822604000066442640602
# shape
# omega    -3424376615469783005808466864488040082840606040222420664068426080420624642402002606666644866802686622082482
# alpha1  -61018382598994881284024008600626480642664840022440060882646884684820026042062464682804024802626444088042248
# beta1   -87226034679848494872204240282286422426480204246808460064464824408860062824084284220080822604000066442640602
# shape  -167087613170133353920220424428424262888620424426484028020088048206684800262002400202000420884202282600248600
# 
# --- END OF TRACE ---
#   
#   Error in solve.default(fit$hessian) : 
#   system is computationally singular: reciprocal condition number = 5.99442e-19
#
summary(fGARCH_1_1_ged_shpN_nlminb)
# Error in h(simpleError(msg, call)) : 
#   error in evaluating the argument 'object' in selecting a method for function 'summary': object 'fGARCH_1_1_ged_shpN_nlminb' not found
#
# The estimation algorithm clearly yields error. We add the Nelder-Mead algorithm to the estimation procedure.
fGARCH_1_1_ged_shpN_nlminb_nm <- fGarch::garchFit(formula=~garch(1,1), data=y, init.rec="mci", cond.dist="ged", include.mean=FALSE,  
                                               include.skew=FALSE, include.shape=NULL, trace=TRUE, algorithm="nlminb+nm")
# Series Initialization:
#   ARMA Model:                arma
# Formula Mean:              ~ arma(0, 0)
# GARCH Model:               garch
# Formula Variance:          ~ garch(1, 1)
# ARMA Order:                0 0
# Max ARMA Order:            0
# GARCH Order:               1 1
# Max GARCH Order:           1
# Maximum Order:             1
# Conditional Dist:          ged
# h.start:                   2
# llh.start:                 1
# Length of Series:          1719
# Recursion Init:            mci
# Series Scale:              3.744706
# 
# Parameter Initialization:
#   Initial Parameters:          $params
# Limits of Transformations:   $U, $V
# Which Parameters are Fixed?  $includes
# Parameter Matrix:
#   U           V       params   includes
# mu     -0.11481907   0.1148191    0.0    FALSE
# omega   0.00000100 100.0000000    0.1     TRUE
# alpha1  0.00000001   1.0000000    0.1     TRUE
# gamma1 -0.99999999   1.0000000    0.1    FALSE
# beta1   0.00000001   1.0000000    0.8     TRUE
# delta   0.00000000   2.0000000    2.0    FALSE
# skew    0.10000000  10.0000000    1.0    FALSE
# shape   1.00000000  10.0000000    4.0     TRUE
# Index List of Parameters to be Optimized:
#   omega alpha1  beta1  shape 
#     2      3      5      8 
# Persistence:                  0.9 
# 
# --- START OF TRACE ---
#   Selected Algorithm: nlminb+nm 
# 
# R coded nlminb Solver: 
#   
#   0: 1.1000000e+99: 0.100000 0.100000 0.800000  4.00000
# 1: 1.1000000e+99: 0.100000 0.100000 0.800000  4.00000
# 
# R coded Nelder-Mead Hybrid Solver: 
#   
#   Nelder-Mead direct search function minimizer
# function value for initial parameters = 30.912681
# Scaled convergence tolerance is 3.09127e-10
# Stepsize computed as 0.100000
# BUILD              5 45.259256 30.912681
# EXTENSION          7 41.144778 0.000000
# LO-REDUCTION       9 37.404343 0.000000
# LO-REDUCTION      11 34.003949 0.000000
# LO-REDUCTION      13 30.912681 0.000000
# Exiting from Nelder Mead minimizer
# 15 function evaluations used
# 
# Final Estimate of the Negative LLH:  4956.208    norm LLH:  2.883193 
#   omega    alpha1     beta1     shape 
# 4.9846751 0.1161719 0.8168750 1.9750000 
# 
# R-optimhess Difference Approximated Hessian Matrix:
#   omega      alpha1       beta1       shape
# omega     6.153787    39.51659   -267.6671   -5.475413
# alpha1   39.516595   609.17058  -3856.7771  -85.535192
# beta1  -267.667128 -3856.77711 -26070.6330 -217.070368
# shape    -5.475413   -85.53519   -217.0704  102.517131
# 
# Warning message: In sqrt(diag(fit$cvar)) : NaNs produced
# 
summary(fGARCH_1_1_ged_shpN_nlminb_nm)
# Extract from the summary output.
# Title: GARCH Modelling 
# Call: fGarch::garchFit(formula = ~garch(1, 1), data = y, init.rec = "mci", cond.dist = "ged", include.mean = FALSE, include.skew = FALSE, 
#                        include.shape = NULL, trace = TRUE, algorithm = "nlminb+nm") 
# 
# Conditional Distribution: ged 
# 
# Coefficient(s): omega   alpha1    beta1    shape  
#                4.98468  0.11617  0.81687  1.97500  
# 
# Std. Errors: based on Hessian 
# 
# Error Analysis: Estimate  Std. Error  t value            Pr(>|t|)    
#         omega   4.984675      NaN      NaN                 NaN    
#        alpha1   0.116172      NaN      NaN                 NaN    
#         beta1   0.816875    0.004191    194.9 <0.0000000000000002 ***
#         shape   1.975000      NaN      NaN                 NaN    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Log Likelihood: -4956.208    normalized:  -2.883193 
# 
# Standardised Residuals Tests:
#                                   Statistic   p-Value
# Jarque-Bera Test   R    Chi^2  22622.4858041 0.0000000
# Shapiro-Wilk Test  R    W          0.9016892 0.0000000
# Ljung-Box Test     R    Q(10)     10.5784629 0.3912878
# Ljung-Box Test     R    Q(15)     12.3719770 0.6506830
# Ljung-Box Test     R    Q(20)     14.3949542 0.8099197
# Ljung-Box Test     R^2  Q(10)      4.6519698 0.9131825
# Ljung-Box Test     R^2  Q(15)      5.2318064 0.9899751
# Ljung-Box Test     R^2  Q(20)      6.2056106 0.9985898
# LM Arch Test       R    TR^2       4.7582823 0.9655690
# 
# Information Criterion Statistics:
#   AIC      BIC      SIC     HQIC 
# 5.771039 5.783720 5.771028 5.775731
# 
# The estimation procedure allows to get parameters estimates, but the obtained results appear somewhat lacking. This is confirmed by the
# visual evidence from Plots 13
# plot(fGARCH_1_1_ged_shpN_nlminb_nm)
# Uncomment and execute the above line
#
# Hence, we consider the Limited-memory Broyden–Fletcher–Goldfarb–Shanno algorithm with box constraints "lbfgsb" algorithm.
fGARCH_1_1_ged_shpN_lbfgsb <- fGarch::garchFit(formula=~garch(1,1), data=y, init.rec="mci", cond.dist="ged", include.mean=FALSE,
                                               include.skew=FALSE, include.shape=NULL, trace=TRUE, algorithm="lbfgsb")
# Extract from the trace output.
# Series Initialization:
# ARMA Model:                arma
# Formula Mean:              ~ arma(0, 0)
# GARCH Model:               garch
# Formula Variance:          ~ garch(1, 1)
# ARMA Order:                0 0
# Max ARMA Order:            0
# GARCH Order:               1 1
# Max GARCH Order:           1
# Maximum Order:             1
# Conditional Dist:          ged
# h.start:                   2
# llh.start:                 1
# Length of Series:          1719
# Recursion Init:            mci
# Series Scale:              3.744706
# 
# Parameter Initialization:
# Initial Parameters:          $params
# Limits of Transformations:   $U, $V
# Which Parameters are Fixed?  $includes
# Parameter Matrix:
#              U           V      params includes
# mu     -0.11481907   0.1148191    0.0    FALSE
# omega   0.00000100 100.0000000    0.1     TRUE
# alpha1  0.00000001   1.0000000    0.1     TRUE
# gamma1 -0.99999999   1.0000000    0.1    FALSE
# beta1   0.00000001   1.0000000    0.8     TRUE
# delta   0.00000000   2.0000000    2.0    FALSE
# skew    0.10000000  10.0000000    1.0    FALSE
# shape   1.00000000  10.0000000    4.0    TRUE
# Index List of Parameters to be Optimized:
# omega alpha1  beta1  shape 
#   2      3      5      8 
# Persistence:                  0.9 
# 
# --- START OF TRACE ---
#   Selected Algorithm: lbfgsb 
# 
# R coded optim[L-BFGS-B] Solver: 
# iter   10 value 2183.975912
# iter   20 value 2145.633236
#    final  value 2145.601896 
# stopped after 25 iterations
# 
# Final Estimate of the Negative LLH: 4415.272  norm LLH: 2.568512 
#   omega    alpha1     beta1     shape 
# 0.3792814 0.0750745 0.8942373 1.0000000 
# 
# R-optimhess Difference Approximated Hessian Matrix:
#           omega     alpha1      beta1      shape
# omega   -438.0519  -3066.057  -4206.966  -129.8367
# alpha1 -3066.0574 -33474.628 -37011.953 -1677.3608
# beta1  -4206.9656 -37011.953 -47205.418 -1822.7623
# shape   -129.8367  -1677.361  -1822.762  -675.1041
# 
# --- END OF TRACE ---
#
summary(fGARCH_1_1_ged_shpN_lbfgsb)
# Extract from the summary output.
# Title: GARCH Modelling 
# Call: fGarch::garchFit(formula=~garch(1,1), data=y, init.rec="mci", cond.dist="ged", include.mean=FALSE, include.skew=FALSE, 
#                        include.shape=NULL, trace=TRUE, algorithm="lbfgsb") 
# Conditional Distribution: ged 
# Coefficient(s):  omega    alpha1     beta1     shape  
#                0.379281  0.075074  0.894237  1.000000
#
# Std. Errors: based on Hessian 
# Error Analysis: Estimate  Std. Error  t value Pr(>|t|)    
#        omega    0.37928     0.14094    2.691  0.00712 ** 
#        alpha1   0.07507     0.01673    4.486 7.25e-06 ***
#        beta1    0.89424     0.02223   40.224  < 2e-16 ***
#        shape    1.00000     0.04140   24.152  < 2e-16 ***
#   ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Log Likelihood: -4415.272    normalized:  -2.568512 
# 
# Standardised Residuals Tests:  Statistic   p-Value
# Jarque-Bera Test   R    Chi^2  2.702746e+04 0.0000000
# Shapiro-Wilk Test  R    W      8.846426e-01 0.0000000
# Ljung-Box Test     R    Q(10)  9.131429e+00 0.5196776
# Ljung-Box Test     R    Q(15)  1.162095e+01 0.7074641
# Ljung-Box Test     R    Q(20)  1.425941e+01 0.8171032
# Ljung-Box Test     R^2  Q(10)  3.425949e+00 0.9695531
# Ljung-Box Test     R^2  Q(15)  4.034650e+00 0.9976217
# Ljung-Box Test     R^2  Q(20)  5.392945e+00 0.9995036
# LM Arch Test       R    TR^2   3.681881e+00 0.9885171
# 
# Information Criterion Statistics:
#   AIC      BIC      SIC     HQIC 
# 5.141677 5.154358 5.141667 5.146369 
#
# The option include.shape=NULL would allegedly lead to an estimate of the shape parameter. Eventually, the shape parameter seems to move
# from the initial value shape=4.0 to the value shape=1.0, the left endpoint of the interval [1.0  10.0], where the shape seems to be
# constrained to vary. On the other hand, the estimation procedure stops after 25 iterations, signaling no convergence. In light of this, 
# we cannot validate the estimated model. After some failed attempts to modify the include.shape option and introduce the further 
# shape=GED_nu option, which sets the initial value of the shape to the value GED_nu=0.8742729 previously estimated, we have found that the 
# difficulty of successfully estimating the shape parameter can be overcome by changing the estimation algorithm.
#
# plot(fGARCH_1_1_ged_shpN_lbfgsb)
# Uncomment and execute the above line
# From the Q-Q plot, we have visual evidence of some improvement in the fit between the standardized residuals and the quantiles of the
# hypothesized GED of the innovation. However, the likely incorrect shape parameter estimation might prevent a better fit at the tails of 
# the Q-Q plot.
#
# Combining the options shape=GED_nu, include.shape=FALSE, and algorithm="lbfgsb" should fix the shape parameter at the value 
# GED_nu=0.8742729 and estimate the other parameters.
fGARCH_1_1_ged_shpF_lbfgsb <- fGarch::garchFit(formula=~garch(1,1), data=y, init.rec="mci", cond.dist="ged", shape=GED_nu,
                                               include.mean=FALSE, include.skew=FALSE, include.shape=FALSE, trace=TRUE, algorithm="lbfgsb")
# Extract from the trace output.
# Series Initialization:
# ARMA Model:                arma
# Formula Mean:              ~ arma(0, 0)
# GARCH Model:               garch
# Formula Variance:          ~ garch(1, 1)
# ARMA Order:                0 0
# Max ARMA Order:            0
# GARCH Order:               1 1
# Max GARCH Order:           1
# Maximum Order:             1
# Conditional Dist:          ged
# h.start:                   2
# llh.start:                 1
# Length of Series:          1719
# Recursion Init:            mci
# Series Scale:              3.744706
# 
# Parameter Initialization:
# Initial Parameters:          $params
# Limits of Transformations:   $U, $V
# Which Parameters are Fixed?  $includes
# Parameter Matrix:
#              U           V       params  includes
# mu     -0.11481907   0.1148191 0.0000000    FALSE
# omega   0.00000100 100.0000000 0.1000000     TRUE
# alpha1  0.00000001   1.0000000 0.1000000     TRUE
# gamma1 -0.99999999   1.0000000 0.1000000    FALSE
# beta1   0.00000001   1.0000000 0.8000000     TRUE
# delta   0.00000000   2.0000000 2.0000000    FALSE
# skew    0.10000000  10.0000000 1.0000000    FALSE
# shape   1.00000000  10.0000000 0.8742729    FALSE
# Index List of Parameters to be Optimized:
#   omega alpha1  beta1 
#    2      3      5 
# Persistence:                  0.9 
# 
# --- START OF TRACE ---
#   Selected Algorithm: lbfgsb 
# 
# R coded optim[L-BFGS-B] Solver: 
# iter   10 value 2141.306044
#    final  value 2141.201421 
# converged
# 
# Final Estimate of the Negative LLH: 4410.871  norm LLH: 2.565952 
# omega     alpha1      beta1 
# 0.33912111 0.08157755 0.89939199 
# 
# R-optimhess Difference Approximated Hessian Matrix:
#            omega    alpha1      beta1
# omega   -384.2906  -2605.60  -3879.742
# alpha1 -2605.6000 -27680.23 -33498.961
# beta1  -3879.7419 -33498.96 -46455.613
# 
# --- END OF TRACE ---
#
summary(fGARCH_1_1_ged_shpF_lbfgsb)
# Extract from the summary output.
# Title: GARCH Modelling 
# Call: fGarch::garchFit(formula=~garch(1, 1), data=y, init.rec="mci", shape=GED_nu, cond.dist="ged", include.mean=FALSE,  
#                        include.skew=FALSE, include.shape=FALSE, trace=TRUE, algorithm="lbfgsb") 
# 
# Conditional Distribution: ged 
# Coefficient(s):
#   omega    alpha1     beta1  
# 0.339121  0.081578  0.899392  
# 
# Std. Errors: based on Hessian 
# Error Analysis: Estimate  Std. Error  t value Pr(>|t|)    
#         omega    0.33912     0.14170    2.393   0.0167 *  
#         alpha1   0.08158     0.01853    4.402 1.07e-05 ***
#         beta1    0.89939     0.02172   41.402  < 2e-16 ***
#   ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Log Likelihood:   -4410.871    normalized:  -2.565952 
# 
# Standardised Residuals Tests:
#                                 Statistic   p-Value
# Jarque-Bera Test   R    Chi^2  2.785661e+04 0.0000000
# Shapiro-Wilk Test  R    W      8.816101e-01 0.0000000
# Ljung-Box Test     R    Q(10)  9.308394e+00 0.5031073
# Ljung-Box Test     R    Q(15)  1.189688e+01 0.6868170
# Ljung-Box Test     R    Q(20)  1.465091e+01 0.7960221
# Ljung-Box Test     R^2  Q(10)  3.388936e+00 0.9707356
# Ljung-Box Test     R^2  Q(15)  4.050387e+00 0.9975677
# Ljung-Box Test     R^2  Q(20)  5.476311e+00 0.9994423
# LM Arch Test       R    TR^2   3.686289e+00 0.9884553
# 
# Information Criterion Statistics:
#   AIC      BIC      SIC     HQIC 
# 5.135394 5.144905 5.135388 5.138913 
#
# In this case, the estimation procedure seems to converge with a slightly improvement in all the values of the information criteria. 
# Note that the estimated values for the parameters omega=0.33912, alpha1=0.08158, and beta1=0.89939, under the assumption of GED
# distributed innovation, are not significantly close to the corresponding estimated values for the parameters omega=1.27007192,  
# alpha1=0.09084851, and beta1=0.82533630, obtained under the assumption of Gaussian innovation. In fact, we have
# omega:  abs(0.33912-1.27007192)=0.9309519  > std_err=0.14170
# alpha1: abs(0.08158-0.09084851)=0.00926851 < std_err=0.01853
# beta1:  abs(0.89939-0.82533630)=0.0740537  > std_err=0.02172
#
# From the Q-Q plot we also have visual evidence that the above combination of options improves significantly the fit between the
# standardized residuals of the estimated model and the quantiles of the hypothesized GED of the innovation.
# plot(fGARCH_1_1_ged_shpF_lbfgsb)
# Uncomment and execute the above line
#
# Combining the options include.shape=NULL and algorithm="lbfgsb+mn", which invokes the Limited-memory Broyden–Fletcher–Goldfarb–Shanno 
# algorithm with box constraints plus the Nelder-Mead algorithm, allows the estimation of the shape parameters.
fGARCH_1_1_ged_shpN_lbfgsb_nm <- fGarch::garchFit(formula=~garch(1,1), data=y, init.rec="mci", cond.dist="ged", include.mean=FALSE,  
                                                  include.skew=FALSE, include.shape=NULL, trace=TRUE, algorithm="lbfgsb+nm")
# Extract from the trace output.
# Series Initialization:
# ARMA Model:                arma
# Formula Mean:              ~ arma(0, 0)
# GARCH Model:               garch
# Formula Variance:          ~ garch(1, 1)
# ARMA Order:                0 0
# Max ARMA Order:            0
# GARCH Order:               1 1
# Max GARCH Order:           1
# Maximum Order:             1
# Conditional Dist:          ged
# h.start:                   2
# llh.start:                 1
# Length of Series:          1719
# Recursion Init:            mci
# Series Scale:              3.744706
# 
# Parameter Initialization:
# Initial Parameters:          $params
# Limits of Transformations:   $U, $V
# Which Parameters are Fixed?  $includes
# Parameter Matrix:
#              U           V     params includes
# mu     -0.11481907   0.1148191    0.0    FALSE
# omega   0.00000100 100.0000000    0.1     TRUE
# alpha1  0.00000001   1.0000000    0.1     TRUE
# gamma1 -0.99999999   1.0000000    0.1    FALSE
# beta1   0.00000001   1.0000000    0.8     TRUE
# delta   0.00000000   2.0000000    2.0    FALSE
# skew    0.10000000  10.0000000    1.0    FALSE
# shape   1.00000000  10.0000000    4.0     TRUE
# Index List of Parameters to be Optimized:
#   omega alpha1  beta1  shape 
#     2      3      5      8 
# Persistence:                  0.9 
# 
# --- START OF TRACE ---
#   Selected Algorithm: lbfgsb+nm 
#
# R coded optim[L-BFGS-B] Solver: 
# iter   10 value 2183.975912
# iter   20 value 2145.633236
# final  value 2145.601896 
# stopped after 25 iterations
# 
# R coded Nelder-Mead Hybrid Solver: 
# Nelder-Mead direct search function minimizer
# function value for initial parameters=1.000000
# Scaled convergence tolerance is 1e-11
# Stepsize computed as 0.100000
# BUILD              5 1.290701 1.000000
# LO-REDUCTION       7 1.059806 1.000000
# HI-REDUCTION       9 1.024337 1.000000
# HI-REDUCTION      11 1.009785 1.000000
# HI-REDUCTION      13 1.004947 1.000000
# LO-REDUCTION      15 1.004494 1.000000
# REFLECTION        17 1.003289 0.999535
# REFLECTION        19 1.000406 0.999042
# REFLECTION        21 1.000005 0.998748
# HI-REDUCTION      23 1.000000 0.998748
# LO-REDUCTION      25 0.999535 0.998427
# REFLECTION        27 0.999042 0.998193
# HI-REDUCTION      29 0.998912 0.997975
# HI-REDUCTION      31 0.998748 0.997975
# LO-REDUCTION      33 0.998427 0.997960
# HI-REDUCTION      35 0.998301 0.997960
# HI-REDUCTION      37 0.998193 0.997960
# HI-REDUCTION      39 0.998078 0.997960
# HI-REDUCTION      41 0.998040 0.997960
# LO-REDUCTION      43 0.997977 0.997926
# HI-REDUCTION      45 0.997975 0.997926
# HI-REDUCTION      47 0.997973 0.997923
# HI-REDUCTION      49 0.997960 0.997923
# LO-REDUCTION      51 0.997932 0.997921
# HI-REDUCTION      53 0.997931 0.997920
# HI-REDUCTION      55 0.997926 0.997920
# HI-REDUCTION      57 0.997923 0.997917
# HI-REDUCTION      59 0.997921 0.997917
# HI-REDUCTION      61 0.997920 0.997917
# LO-REDUCTION      63 0.997920 0.997917
# LO-REDUCTION      65 0.997917 0.997917
# REFLECTION        67 0.997917 0.997916
# HI-REDUCTION      69 0.997917 0.997916
# HI-REDUCTION      71 0.997917 0.997916
# HI-REDUCTION      73 0.997917 0.997916
# LO-REDUCTION      75 0.997916 0.997916
# HI-REDUCTION      77 0.997916 0.997916
# LO-REDUCTION      79 0.997916 0.997916
# LO-REDUCTION      81 0.997916 0.997916
# HI-REDUCTION      83 0.997916 0.997916
# REFLECTION        85 0.997916 0.997916
# REFLECTION        87 0.997916 0.997916
# HI-REDUCTION      89 0.997916 0.997916
# REFLECTION        91 0.997916 0.997916
# REFLECTION        93 0.997916 0.997916
# LO-REDUCTION      95 0.997916 0.997916
# EXTENSION         97 0.997916 0.997916
# LO-REDUCTION      99 0.997916 0.997916
# EXTENSION        101 0.997916 0.997916
# LO-REDUCTION     103 0.997916 0.997916
# LO-REDUCTION     105 0.997916 0.997916
# REFLECTION       107 0.997916 0.997916
# EXTENSION        109 0.997916 0.997915
# LO-REDUCTION     111 0.997916 0.997915
# LO-REDUCTION     113 0.997916 0.997915
# LO-REDUCTION     115 0.997916 0.997915
# EXTENSION        117 0.997916 0.997915
# EXTENSION        119 0.997915 0.997915
# LO-REDUCTION     121 0.997915 0.997915
# LO-REDUCTION     123 0.997915 0.997915
# LO-REDUCTION     125 0.997915 0.997915
# LO-REDUCTION     127 0.997915 0.997915
# REFLECTION       129 0.997915 0.997915
# LO-REDUCTION     131 0.997915 0.997915
# HI-REDUCTION     133 0.997915 0.997915
# REFLECTION       135 0.997915 0.997915
# REFLECTION       137 0.997915 0.997915
# LO-REDUCTION     139 0.997915 0.997915
# LO-REDUCTION     141 0.997915 0.997915
# EXTENSION        143 0.997915 0.997915
# LO-REDUCTION     145 0.997915 0.997915
# LO-REDUCTION     147 0.997915 0.997915
# LO-REDUCTION     149 0.997915 0.997915
# REFLECTION       151 0.997915 0.997915
# LO-REDUCTION     153 0.997915 0.997915
# HI-REDUCTION     155 0.997915 0.997915
# LO-REDUCTION     157 0.997915 0.997915
# EXTENSION        159 0.997915 0.997915
# LO-REDUCTION     161 0.997915 0.997915
# LO-REDUCTION     163 0.997915 0.997915
# LO-REDUCTION     165 0.997915 0.997915
# LO-REDUCTION     167 0.997915 0.997915
# LO-REDUCTION     169 0.997915 0.997915
# HI-REDUCTION     171 0.997915 0.997915
# LO-REDUCTION     173 0.997915 0.997915
# REFLECTION       175 0.997915 0.997915
# LO-REDUCTION     177 0.997915 0.997915
# LO-REDUCTION     179 0.997915 0.997915
# LO-REDUCTION     181 0.997915 0.997915
# EXTENSION        183 0.997915 0.997915
# HI-REDUCTION     185 0.997915 0.997915
# LO-REDUCTION     187 0.997915 0.997915
# LO-REDUCTION     189 0.997915 0.997915
# LO-REDUCTION     191 0.997915 0.997915
# LO-REDUCTION     193 0.997915 0.997915
# EXTENSION        195 0.997915 0.997915
# LO-REDUCTION     197 0.997915 0.997915
# LO-REDUCTION     199 0.997915 0.997915
# EXTENSION        201 0.997915 0.997915
# EXTENSION        203 0.997915 0.997915
# LO-REDUCTION     205 0.997915 0.997915
# REFLECTION       207 0.997915 0.997915
# EXTENSION        209 0.997915 0.997915
# LO-REDUCTION     211 0.997915 0.997915
# HI-REDUCTION     213 0.997915 0.997915
# LO-REDUCTION     215 0.997915 0.997915
# EXTENSION        217 0.997915 0.997915
# HI-REDUCTION     219 0.997915 0.997915
# LO-REDUCTION     221 0.997915 0.997915
# LO-REDUCTION     223 0.997915 0.997915
# EXTENSION        225 0.997915 0.997915
# LO-REDUCTION     227 0.997915 0.997915
# EXTENSION        229 0.997915 0.997915
# LO-REDUCTION     231 0.997915 0.997915
# LO-REDUCTION     233 0.997915 0.997915
# REFLECTION       235 0.997915 0.997915
# REFLECTION       237 0.997915 0.997915
# LO-REDUCTION     239 0.997915 0.997915
# EXTENSION        241 0.997915 0.997915
# LO-REDUCTION     243 0.997915 0.997915
# LO-REDUCTION     245 0.997915 0.997915
# LO-REDUCTION     247 0.997915 0.997915
# EXTENSION        249 0.997915 0.997915
# LO-REDUCTION     251 0.997915 0.997915
# EXTENSION        253 0.997915 0.997915
# LO-REDUCTION     255 0.997915 0.997915
# LO-REDUCTION     257 0.997915 0.997915
# LO-REDUCTION     259 0.997915 0.997915
# LO-REDUCTION     261 0.997915 0.997915
# EXTENSION        263 0.997915 0.997915
# EXTENSION        265 0.997915 0.997915
# LO-REDUCTION     267 0.997915 0.997915
# REFLECTION       269 0.997915 0.997915
# LO-REDUCTION     271 0.997915 0.997915
# LO-REDUCTION     273 0.997915 0.997915
# LO-REDUCTION     275 0.997915 0.997915
# LO-REDUCTION     277 0.997915 0.997915
# EXTENSION        279 0.997915 0.997915
# LO-REDUCTION     281 0.997915 0.997915
# LO-REDUCTION     283 0.997915 0.997915
# EXTENSION        285 0.997915 0.997915
# EXTENSION        287 0.997915 0.997915
# LO-REDUCTION     289 0.997915 0.997915
# LO-REDUCTION     291 0.997915 0.997915
# REFLECTION       293 0.997915 0.997915
# Exiting from Nelder Mead minimizer
# 295 function evaluations used
# 
# Final Estimate of the Negative LLH: 4410.799  norm LLH: 2.56591 
#   omega     alpha1      beta1      shape 
# 0.34079449 0.08046777 0.89917369 0.88780427 
# 
# R-optimhess Difference Approximated Hessian Matrix:
#           omega     alpha1      beta1      shape
# omega   -394.7368  -2685.631  -3952.109  -221.8304
# alpha1 -2685.6312 -28609.574 -34203.893 -2469.1411
# beta1  -3952.1091 -34203.893 -46878.309 -2980.3783
# shape   -221.8304  -2469.141  -2980.378 -1000.9125
# 
# --- END OF TRACE ---
#
summary(fGARCH_1_1_ged_shpN_lbfgsb_nm)
# Extract from the summary output.
# Title: GARCH Modelling 
# Call: fGarch::garchFit(formula=~garch(1, 1), data=y, init.rec="mci", cond.dist="ged", include.mean=FALSE, include.skew=FALSE, 
#                        include.shape=NULL, trace=TRUE, algorithm="lbfgsb+nm") 
# 
# Conditional Distribution: ged 
# Coefficient(s):
#   omega    alpha1     beta1     shape  
# 0.340794  0.080468  0.899174  0.887804  
# 
# Std. Errors: based on Hessian 
# Error Analysis:
#           Estimate  Std. Error  t value Pr(>|t|)    
#   omega    0.34079     0.14059    2.424   0.0153 *  
#   alpha1   0.08047     0.01836    4.383 1.17e-05 ***
#   beta1    0.89917     0.02169   41.449  < 2e-16 ***
#   shape    0.88780     0.03571   24.860  < 2e-16 ***
#   ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Log Likelihood: -4410.799    normalized:  -2.56591 
# 
# Standardised Residuals Tests:
#                                 Statistic   p-Value
# Jarque-Bera Test   R    Chi^2  2.782087e+04 0.0000000
# Shapiro-Wilk Test  R    W      8.818707e-01 0.0000000
# Ljung-Box Test     R    Q(10)  9.292729e+00 0.5045662
# Ljung-Box Test     R    Q(15)  1.187352e+01 0.6885761
# Ljung-Box Test     R    Q(20)  1.461694e+01 0.7978907
# Ljung-Box Test     R^2  Q(10)  3.389556e+00 0.9707160
# Ljung-Box Test     R^2  Q(15)  4.045833e+00 0.9975834
# Ljung-Box Test     R^2  Q(20)  5.464050e+00 0.9994517
# LM Arch Test       R    TR^2   3.683053e+00 0.9885007
# 
# Information Criterion Statistics:
#   AIC      BIC      SIC     HQIC 
# 5.136473 5.149154 5.136462 5.141165
#
# Note that the estimated value of the state parameter is shape=0.88780 with a standard error std_err=0.03571. Therefore, we have
# abs(0.88780-0.8742729)=0.0135271<0.03571=shp_std_err. Hence, the estimated shape parameter differs from the formerly estimated parameter
# GED_nu=0.8742729 for less than the standard error. Regarding the values of the information criteria, we have a slight worsening compared
# to the former model. However, successfully estimating the shape parameter seems to be a reasonable compensation.
# Note also that the parameters omega, alpha1, and beta1 estimated in this case are very similar to the parameters estimated in the former
# case. In fact, the absolute value of the difference between the estimates of all parameters omega, alpha1, and beta1 is smaller than the
# corresponding minimum standard error.
# omega:  abs(0.34079-0.33912)=0.00167<0.14059=min(0.14059,0.14170);
# alpha1: abs(0.08047-0.08158)=0.00111<0.01836=min(0.01836,0.01853);
# beta1:  abs(0.89917-0.89424)=0.00493<0.02169=min(0.02223,0.02169).
#
# From the Q-Q plot, we also have visual evidence of a slight worsening in the fit between the standardized residuals of the estimated model
# and the quantiles of the hypothesized GED of the innovation.
# plot(fGARCH_1_1_ged_shpN_lbfgsb_nm)
# Uncomment and execute the above line
#
# We consider combining the options shape=GED_nu, include.shape=NULL, algorithm="lbfgsb+nm". These should lead to an estimation of the 
# shape parameter starting from the value GED_nu, by meas of the "lbfgsb+nm" algorithm.
fGARCH_1_1_ged_initshp_shpN_lbfgsb_nm <- fGarch::garchFit(formula=~garch(1,1), data=y, init.rec="mci", cond.dist="ged", 
                                                          shape=GED_nu, include.mean=FALSE, include.skew=FALSE, include.shape=NULL, 
                                                          trace=TRUE, algorithm="lbfgsb+nm")
# Extrac from the trace output.
# Series Initialization:
# ARMA Model:                arma
# Formula Mean:              ~ arma(0, 0)
# GARCH Model:               garch
# Formula Variance:          ~ garch(1, 1)
# ARMA Order:                0 0
# Max ARMA Order:            0
# GARCH Order:               1 1
# Max GARCH Order:           1
# Maximum Order:             1
# Conditional Dist:          ged
# h.start:                   2
# llh.start:                 1
# Length of Series:          1719
# Recursion Init:            mci
# Series Scale:              3.744706
# 
# Parameter Initialization:
# Initial Parameters:          $params
# Limits of Transformations:   $U, $V
# Which Parameters are Fixed?  $includes
# Parameter Matrix:
#              U           V       params  includes
# mu     -0.11481907   0.1148191 0.0000000    FALSE
# omega   0.00000100 100.0000000 0.1000000     TRUE
# alpha1  0.00000001   1.0000000 0.1000000     TRUE
# gamma1 -0.99999999   1.0000000 0.1000000    FALSE
# beta1   0.00000001   1.0000000 0.8000000     TRUE
# delta   0.00000000   2.0000000 2.0000000    FALSE
# skew    0.10000000  10.0000000 1.0000000    FALSE
# shape   1.00000000  10.0000000 0.8742729     TRUE
# Index List of Parameters to be Optimized:
#   omega alpha1  beta1  shape 
#     2      3      5      8 
# Persistence:                  0.9 
# 
# --- START OF TRACE ---
#   Selected Algorithm: lbfgsb+nm 
# 
# R coded optim[L-BFGS-B] Solver: 
#   iter   10 value 2145.604098
#      final  value 2145.601895 
# converged
# 
# R coded Nelder-Mead Hybrid Solver: 
# Nelder-Mead direct search function minimizer
# function value for initial parameters = 1.000000
# Scaled convergence tolerance is 1e-11
# Stepsize computed as 0.100000
# BUILD              5 1.290719 1.000000
# LO-REDUCTION       7 1.059806 1.000000
# HI-REDUCTION       9 1.024338 1.000000
# HI-REDUCTION      11 1.009785 1.000000
# HI-REDUCTION      13 1.004947 1.000000
# LO-REDUCTION      15 1.004494 1.000000
# REFLECTION        17 1.003289 0.999535
# REFLECTION        19 1.000406 0.999042
# REFLECTION        21 1.000004 0.998748
# HI-REDUCTION      23 1.000000 0.998748
# LO-REDUCTION      25 0.999535 0.998427
# REFLECTION        27 0.999042 0.998193
# HI-REDUCTION      29 0.998912 0.997975
# HI-REDUCTION      31 0.998748 0.997975
# LO-REDUCTION      33 0.998427 0.997960
# HI-REDUCTION      35 0.998301 0.997960
# HI-REDUCTION      37 0.998193 0.997960
# HI-REDUCTION      39 0.998078 0.997960
# HI-REDUCTION      41 0.998040 0.997960
# LO-REDUCTION      43 0.997977 0.997926
# HI-REDUCTION      45 0.997975 0.997926
# HI-REDUCTION      47 0.997973 0.997923
# HI-REDUCTION      49 0.997960 0.997923
# LO-REDUCTION      51 0.997932 0.997921
# HI-REDUCTION      53 0.997931 0.997920
# HI-REDUCTION      55 0.997926 0.997920
# HI-REDUCTION      57 0.997923 0.997917
# HI-REDUCTION      59 0.997921 0.997917
# HI-REDUCTION      61 0.997920 0.997917
# LO-REDUCTION      63 0.997920 0.997917
# LO-REDUCTION      65 0.997917 0.997917
# REFLECTION        67 0.997917 0.997916
# HI-REDUCTION      69 0.997917 0.997916
# HI-REDUCTION      71 0.997917 0.997916
# HI-REDUCTION      73 0.997917 0.997916
# LO-REDUCTION      75 0.997916 0.997916
# HI-REDUCTION      77 0.997916 0.997916
# LO-REDUCTION      79 0.997916 0.997916
# LO-REDUCTION      81 0.997916 0.997916
# HI-REDUCTION      83 0.997916 0.997916
# REFLECTION        85 0.997916 0.997916
# REFLECTION        87 0.997916 0.997916
# HI-REDUCTION      89 0.997916 0.997916
# REFLECTION        91 0.997916 0.997916
# REFLECTION        93 0.997916 0.997916
# LO-REDUCTION      95 0.997916 0.997916
# EXTENSION         97 0.997916 0.997916
# LO-REDUCTION      99 0.997916 0.997916
# EXTENSION        101 0.997916 0.997916
# LO-REDUCTION     103 0.997916 0.997916
# LO-REDUCTION     105 0.997916 0.997916
# REFLECTION       107 0.997916 0.997916
# EXTENSION        109 0.997916 0.997915
# LO-REDUCTION     111 0.997916 0.997915
# LO-REDUCTION     113 0.997916 0.997915
# LO-REDUCTION     115 0.997916 0.997915
# EXTENSION        117 0.997916 0.997915
# EXTENSION        119 0.997915 0.997915
# LO-REDUCTION     121 0.997915 0.997915
# LO-REDUCTION     123 0.997915 0.997915
# LO-REDUCTION     125 0.997915 0.997915
# LO-REDUCTION     127 0.997915 0.997915
# REFLECTION       129 0.997915 0.997915
# LO-REDUCTION     131 0.997915 0.997915
# HI-REDUCTION     133 0.997915 0.997915
# REFLECTION       135 0.997915 0.997915
# REFLECTION       137 0.997915 0.997915
# LO-REDUCTION     139 0.997915 0.997915
# LO-REDUCTION     141 0.997915 0.997915
# EXTENSION        143 0.997915 0.997915
# LO-REDUCTION     145 0.997915 0.997915
# LO-REDUCTION     147 0.997915 0.997915
# LO-REDUCTION     149 0.997915 0.997915
# REFLECTION       151 0.997915 0.997915
# LO-REDUCTION     153 0.997915 0.997915
# LO-REDUCTION     155 0.997915 0.997915
# HI-REDUCTION     157 0.997915 0.997915
# REFLECTION       159 0.997915 0.997915
# REFLECTION       161 0.997915 0.997915
# REFLECTION       163 0.997915 0.997915
# LO-REDUCTION     165 0.997915 0.997915
# LO-REDUCTION     167 0.997915 0.997915
# REFLECTION       169 0.997915 0.997915
# LO-REDUCTION     171 0.997915 0.997915
# HI-REDUCTION     173 0.997915 0.997915
# EXTENSION        175 0.997915 0.997915
# LO-REDUCTION     177 0.997915 0.997915
# LO-REDUCTION     179 0.997915 0.997915
# EXTENSION        181 0.997915 0.997915
# REFLECTION       183 0.997915 0.997915
# EXTENSION        185 0.997915 0.997915
# REFLECTION       187 0.997915 0.997915
# LO-REDUCTION     189 0.997915 0.997915
# LO-REDUCTION     191 0.997915 0.997915
# LO-REDUCTION     193 0.997915 0.997915
# HI-REDUCTION     195 0.997915 0.997915
# LO-REDUCTION     197 0.997915 0.997915
# LO-REDUCTION     199 0.997915 0.997915
# REFLECTION       201 0.997915 0.997915
# REFLECTION       203 0.997915 0.997915
# HI-REDUCTION     205 0.997915 0.997915
# HI-REDUCTION     207 0.997915 0.997915
# REFLECTION       209 0.997915 0.997915
# HI-REDUCTION     211 0.997915 0.997915
# HI-REDUCTION     213 0.997915 0.997915
# REFLECTION       215 0.997915 0.997915
# Exiting from Nelder Mead minimizer
# 217 function evaluations used
# 
# Final Estimate of the Negative LLH: 4410.799  norm LLH: 2.56591 
# omega     alpha1      beta1      shape 
# 0.34079130 0.08046938 0.89917383 0.88779372 
# 
# R-optimhess Difference Approximated Hessian Matrix:
#           omega     alpha1      beta1      shape
# omega   -394.7252  -2685.529  -3952.034  -221.8389
# alpha1 -2685.5292 -28608.302 -34203.110 -2469.2085
# beta1  -3952.0345 -34203.110 -46878.107 -2980.5094
# shape   -221.8389  -2469.209  -2980.509 -1000.9491
# attr(,"time")
# Time difference of 0.04401994 secs
# 
# --- END OF TRACE ---
#
summary(fGARCH_1_1_ged_initshp_shpN_lbfgsb_nm)
# Extract from the summary output.
# Title: GARCH Modelling 
# Call: fGarch::garchFit(formula=~garch(1, 1), data=y, init.rec"mci", shape=GED_nu, cond.dist="ged", include.mean=FALSE, include.skew=FALSE, 
#                        include.shape=NULL, trace=TRUE, algorithm="lbfgsb+nm") 
# 
# Conditional Distribution: ged 
# Coefficient(s):
#   omega    alpha1     beta1     shape  
# 0.340791  0.080469  0.899174  0.887794  
# 
# Std. Errors: based on Hessian 
# Error Analysis: Estimate  Std. Error  t value Pr(>|t|)    
#         omega    0.34079     0.14059    2.424   0.0153 *  
#         alpha1   0.08047     0.01836    4.383 1.17e-05 ***
#         beta1    0.89917     0.02169   41.449  < 2e-16 ***
#         shape    0.88779     0.03571   24.860  < 2e-16 ***
#   ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Log Likelihood: -4410.799    normalized:  -2.56591 
# 
# Standardised Residuals Tests:
#                                  Statistic   p-Value
# Jarque-Bera Test   R    Chi^2  2.782089e+04 0.0000000
# Shapiro-Wilk Test  R    W      8.818703e-01 0.0000000
# Ljung-Box Test     R    Q(10)  9.292750e+00 0.5045643
# Ljung-Box Test     R    Q(15)  1.187355e+01 0.6885738
# Ljung-Box Test     R    Q(20)  1.461699e+01 0.7978881
# Ljung-Box Test     R^2  Q(10)  3.389558e+00 0.9707160
# Ljung-Box Test     R^2  Q(15)  4.045843e+00 0.9975834
# Ljung-Box Test     R^2  Q(20)  5.464074e+00 0.9994517
# LM Arch Test       R    TR^2   3.683061e+00 0.9885006
# 
# Information Criterion Statistics:
#   AIC      BIC      SIC     HQIC 
# 5.136473 5.149154 5.136462 5.141165 
#
# Invoking the shape=GED_nu option does not seem to improve the performances of the estimated model.
#
# From the Q-Q plot, we also have visual evidence that invoking the shape=GED_nu option does not seem to improve the performances of the 
# estimated model.
# plot(fGARCH_1_1_ged_initshp_shpN_lbfgsb_nm)
# Uncomment and execute the above line
#
# Overall, the best model appears to be estimated invoking the options include.shape=NULL and algorithm="lbfgsb+nm".
#
# We plot the standardized residuals and the conditional standard deviation estimated by the
# fGarch::garchFit(..., include.shape=NULL, algorithm="lbfgsb+nm") function. We have
# 
fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res <- fGarch::residuals(fGARCH_1_1_ged_shpN_lbfgsb_nm, standardize=TRUE)
head(fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res, 20)
#  0.8673333  0.4274806  1.7858863  0.1431021 -0.2794854  0.3989901  2.3425966 -2.2355670  1.0172507 -0.6827799  0.8558484  0.1654581
# -0.4392640 -0.3147066  0.3145931  1.3725041 -0.1097089  0.4150189 -0.5556425 -0.8109007
#
fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_stand_dev <- fGARCH_1_1_ged_initshp_shpN_lbfgsb_nm@sigma.t
head(fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_stand_dev, 20)
# 3.751263 3.720992 3.604741 3.919147 3.765256 3.630100 3.515477 4.112271 4.727303 4.721845 4.607060 4.547173 4.356416 4.207173 4.049384
# 3.900717 4.040877 3.878006 3.751246 3.652865
#
fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_var <- fGARCH_1_1_ged_initshp_shpN_lbfgsb_nm@h.t
head(fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_var, 20)
#
head(BTC_train_df)
tail(BTC_train_df)
BTC_train_df <- add_column(BTC_train_df, 
                           fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res=c(NA,fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res), 
                           fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_stand_dev=c(NA,fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_stand_dev),
                           fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_var=c(NA,fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_var),
                           .after="fGARCH_1_1_cond_stand_dev")
head(BTC_train_df)
Data_df <- BTC_train_df
head(Data_df)
Data_df <- dplyr::rename(Data_df, x=t, y=fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res)
head(Data_df)
TrnS_length <- length(na.rm(Data_df$y[which(Data_df$Date<as.Date("2023-01-01"))]))
show(TrnS_length)
# 1719
First_Day <- as.character(Data_df$Date[2])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Standardized Residuals of the fGarch::garchFit() Fitted GARCH(1,1) Model with GED Innovation for the Bitcoin Daily Logarithm Return Percentage - Training Set - from ", .(First_Day), " to ", .(Last_Day), sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points. Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("")
# numbers::primeFactors(TrnS_length-2)
x_breaks_num <- 44 # (deduced from primeFactors(TrnS_length-2))
x_breaks_low <- Data_df$x[2]
x_breaks_up <- Data_df$x[nrow(Data_df)]
x_binwidth <- ceiling((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Date[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("standardized residuals (US $)")
y_max <- max(na.rm(Data_df$y))
y_min <- min(na.rm(Data_df$y))
as.numeric(floor(y_max-y_min))
y_breaks_num <- 10
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor((y_min/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((y_max/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth), digits=3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.5
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
point_black <- bquote("GARCH(1,1) stand. residuals")
line_green <- bquote("regression line")
line_red <- bquote("LOESS curve")
leg_point_labs <- c(point_black)
leg_point_cols <- c("point_black"="black")
leg_point_breaks <- c("point_black")
leg_line_labs <- c(line_green, line_red)
leg_line_cols <- c("line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_green", "line_red")
leg_col_labs   <- c(leg_point_labs,leg_line_labs)
leg_col_cols   <- c(leg_point_cols,leg_line_cols)
leg_col_breaks <- c(leg_point_breaks,leg_line_breaks)
BTC_fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res_TrnS_sp <- ggplot(Data_df, aes(x=x)) +
  geom_smooth(aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_point(aes(y=y, color="point_black"), alpha=1, size=0.7, shape=19, na.rm=TRUE) + 
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  guides(colour=guide_legend(override.aes=list(shape=c(19, NA, NA), linetype=c("blank", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res_TrnS_sp)
#
# The line plot
BTC_fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res_TrnS_lp <- ggplot(Data_df, aes(x=x)) +
  geom_smooth(aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_line(aes(y=y, color="point_black"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res_TrnS_lp)
#
# We superimpose the conditional standard deviation of the GARCH(1,1) model estimated by the 
# fGarch::garchFit(..., include.shape=NULL, algorithm="lbfgsb+nm") function to the plots of the Bitcoin daily logarithm return percentage.
# The scatter plot.
head(BTC_train_df)
tail(BTC_train_df)
Data_df <- BTC_train_df
head(Data_df)
tail(Data_df)
Data_df <- dplyr::rename(Data_df, x=t, y=log_ret_perc, z=fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_stand_dev)
head(Data_df)
tail(Data_df)
First_Day <- as.character(Data_df$Date[3])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Bitcoin Daily Logarithm Return Percentage and Conditional Standard Deviation of the fGarch::garchFit() Fitted GARCH(1,1) Model with GED Innovation - Training Set - from ", .(First_Day), " to ", .(Last_Day), sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points. Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("")
# numbers::primeFactors(TrnS_length-2)
x_breaks_num <- 44 # (deduced from primeFactors(TrnS_length-2))
x_breaks_low <- Data_df$x[2]
x_breaks_up <- Data_df$x[nrow(Data_df)]
x_binwidth <- ceiling((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Date[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("daily logarithm return percentage")
y_max <- max(na.rm(Data_df$y))
y_min <- min(na.rm(Data_df$y))
as.numeric(floor(y_max-y_min))
y_breaks_num <- 10
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor((y_min/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((y_max/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth), digits=3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.5
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
point_black <- bquote("GARCH(1,1) stand. residuals")
leg_point_labs <- c(point_black)
leg_point_cols <- c("point_black"="black")
leg_point_breaks <- c("point_black")
line_magenta <- bquote("GARCH(1,1) cond. stand. dev.")
line_green <- bquote("regression line")
line_red <- bquote("LOESS curve")
leg_line_labs <- c(line_magenta, line_green, line_red)
leg_line_cols <- c("line_magenta"="magenta", "line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_magenta", "line_green", "line_red")
leg_col_labs   <- c(leg_point_labs,leg_line_labs)
leg_col_cols   <- c(leg_point_cols,leg_line_cols)
leg_col_breaks <- c(leg_point_breaks,leg_line_breaks)
BTC_perc_log_ret_fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_stand_dev_TrnS_sp <- ggplot(Data_df, aes(x=x)) +
  geom_smooth(aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_point(aes(y=y, color="point_black"), alpha=1, size=0.7, shape=19, na.rm=TRUE) + 
  geom_line(aes(y=z, color="line_magenta"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  geom_line(aes(y=-z, color="line_magenta"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  guides(colour=guide_legend(override.aes=list(shape=c(19, NA, NA, NA), linetype=c("blank", "solid", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_perc_log_ret_fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_stand_dev_TrnS_sp)
#
# The line plot.
line_black <- bquote("perc. log. returns")
line_magenta <- bquote("GARCH(1,1) cond. stand. dev.")
line_green <- bquote("regression line")
line_red <- bquote("LOESS curve")
leg_line_labs <- c(line_black, line_magenta, line_green, line_red)
leg_line_cols <- c("line_black"="black", "line_magenta"="magenta", "line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_black", "line_magenta", "line_green", "line_red")
BTC_perc_log_ret_fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_stand_dev_TrnS_lp <- ggplot(Data_df, aes(x=x)) +
  geom_smooth(aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_line(aes(y=y, color="line_black"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  geom_line(aes(y=z, color="line_magenta"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  geom_line(aes(y=-z, color="line_magenta"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="", labels=leg_line_labs, values=leg_line_cols, breaks=leg_line_breaks) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_perc_log_ret_fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_stand_dev_TrnS_lp)
#
# Note that we have
length(which(abs(BTC_train_df$log_ret_perc[-c(1,2)])<BTC_train_df$fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_stand_dev[-c(1,2)]))/length(BTC_train_df$log_ret_perc[-c(1,2)])
# 0.7718277
#
# We plot the histogram of the standardized residuals together with the empirical density function, the Standard Gaussian Distribution Density function, 
# the generalized Error Distribution Density Function (GED) with mean parameter, mean=0, standard deviation parameter, sd=1, and shape parameter,
# nu=fGARCH_1_1_ged_initshp_shpN_lbfgsb_nm@fit[["par"]][["shape"]]=0.8877937.
y <- fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res
y_qemp <- EnvStats::qemp(stats::ppoints(y), y) # Empirical quantiles of the data set y.
y_demp <- EnvStats::demp(y_qemp, y)            # Empirical probability density of the data set y.
y_pemp <- EnvStats::pemp(y_qemp, y)            # Empirical distribution function of the data set y.  
x <- y_qemp
y_d <- y_demp
y_p <- y_pemp
mean <- 0
sd <- 1
GED_nu <- fGARCH_1_1_ged_initshp_shpN_lbfgsb_nm@fit[["par"]][["shape"]]
GED_leg <- bquote(paste("Generalized Error Distribution Density Function: mean=", .(mean),", standard deviation=", .(sd), ", shape=", .(GED_nu)))
GED_dens_tit <- "Density Histogram and Empirical Density Function of the Standardized Residuals of the fGarch::garchFit() Fitted GARCH(1,1) Model with GED innovation"
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(y, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main="", xlab= "Standardized Residuals", ylab= "Histogram & Density Functions Values")
# lines(x, y_d, lwd=2, col= "darkblue")
lines(density(y), lwd=2, col= "darkblue")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "red")
lines(x, fGarch::dged(x, mean=0, sd=1, nu=1, log=FALSE), lwd=2, col= "magenta")
title(main=list(GED_dens_tit, cex=1.0))
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Distribution Density Function", GED_leg), 
       col=c("darkblue", "red","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
#
# We also compare the empirical distribution function of the standardized residuals with the distribution functions of the GED and STD.
dev.new()
GED_distr_tit <- "Empirical Distribution Function of the Standardized Residuals of the fGarch::garchFit() Fitted GARCH(1,1) Model with GED innovation"
EnvStats::ecdfPlot(y, discrete=TRUE, prob.method= "emp.probs", type= "s", plot.it=TRUE, 
         add=FALSE, ecdf.col= "cyan", ecdf.lwd=2, ecdf.lty=1, curve.fill=TRUE, main="", 
         xlab= "Standardized Residuals", ylab= "Probability Distribution", xlim=c(x[1]-1.0, x[length(x)]+1.0))
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "red")
lines(x, fGarch::pged(x, mean=0, sd=1, nu=1), lwd=2, col= "magenta")
title(main=list(GED_distr_tit, cex=1.0))
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Distribution Density Function", GED_leg), 
       col=c("darkblue", "red","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
#
dev.off()
#
# We build the Q-Q plot of the standardized residuals of the GARCH(1,1) model for the Bitcoin daily logarithm return percentage training 
# set against the corresponding quantiles of the estimated generalized GED.
# 
# First, we build a suitable data frame.
y <- fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res
head(y,20)
#  0.8673333  0.4274806  1.7858863  0.1431021 -0.2794854  0.3989901  2.3425966 -2.2355670  1.0172507 -0.6827799  0.8558484  0.1654581
# -0.4392640 -0.3147066  0.3145931  1.3725041 -0.1097089  0.4150189 -0.5556425 -0.8109007
y_qemp <- qemp(ppoints(length(y)), y)
mean <- 0
sd <- 1
nu  <- GED_nu 
distr <- "ged"
distr_pars <- list(mean=0, sd=1, nu=nu)
quants <- qged(ppoints(length(y)), mean=0, sd=1, nu=nu)
QQ_plot_df <- data.frame(T=1:length(y), Q=quants, X=y, Y=y_qemp)
head(QQ_plot_df)
# Second we draw the Q-Q plot of the residuals.
Data_df <- QQ_plot_df
length <- nrow(Data_df)
quart_probs <- c(0.25,0.75)
quart_X <- as.vector(quantile(QQ_plot_df$X, quart_probs))
quart_Q <- qged(quart_probs, mean=0, sd=1, nu=nu)
slope <- diff(quart_X)/diff(quart_Q)
intercept <- quart_X[1]-slope*quart_Q[1]
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Q-Q plot of the Standardized Residuals of the fGarch::garchFit() Fitted GARCH(1,1) Model with GED Innovation for the Bitcoin Daily Logarithm Return Percentage  Training Set Against the Estimated GED")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Data set size ", .(length), " sample points; GED density function: mean ", .(mean), ", standard deviation ", .(sd), ", shape ", .(nu), "."~~"Data by courtesy of Yahoo Finance US - ",.(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("Theoretical Quantiles")
y_name <- bquote("Sample Quantiles")
x_breaks_min <- floor(Data_df$Q[1])
x_breaks_max <- ceiling(Data_df$Q[length])
x_breaks <- seq(from=x_breaks_min, to=x_breaks_max, by=0.5)
x_labs <- format(x_breaks, scientific=FALSE)
y_breaks_num <- length(x_breaks)
y_binwidth <- round((max(Data_df$Y)-min(Data_df$Y))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$Y)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$Y)/y_binwidth))*y_binwidth
y_breaks <- c(round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3))
y_labs <- format(y_breaks, scientific=FALSE)
y1_shape <- bquote("Q-Q plot")
y1_fill <- bquote("90% confidence intervals")
y2_fill <- bquote("95% confidence intervals")
y1_col <- bquote("interquartile line")
y2_col <- bquote("regression line")
y3_col <- bquote("y=x line")
leg_shape_labs <- y1_shape
leg_fill_labs <- c(y1_fill, y2_fill)
leg_col_labs <- c(y1_col, y2_col, y3_col)
leg_shape_cols <- c("y1_shape"=19)
leg_fill_cols <- c("y1_fill"="chartreuse1", "y2_fill"="deepskyblue1")
leg_col_cols <- c("y1_col"="cyan", "y2_col"="red", "y3_col"="black")
leg_shape_sort <- "y1_shape"
leg_fill_sort <- c("y1_fill", "y2_fill")
leg_col_sort <- c("y1_col", "y2_col", "y3_col")
Stand_Res_ged_QQ_plot <- ggplot(Data_df, aes(sample=X)) +
  qqplotr::stat_qq_band(aes(fill= "y2_fill"), distribution=distr, dparams=distr_pars, conf=0.95) +
  qqplotr::stat_qq_band(aes(fill= "y1_fill"), distribution=distr, dparams=distr_pars, conf=0.90) +
  geom_abline(aes(slope=slope, intercept=intercept, colour= "y1_col"), linewidth=0.8, linetype= "solid", show.legend=FALSE)+
  stat_smooth(aes(x=Q, y=Y, colour= "y2_col", group=1), inherit.aes=FALSE, method= "lm" , formula=y~x, alpha=1, linewidth=0.8, linetype= "solid",
              se=FALSE, fullrange=FALSE)+
  geom_abline(aes(slope=1, intercept=0, colour= "y3_col"), linewidth=0.8, linetype= "solid", show.legend=FALSE) +
  qqplotr::stat_qq_point(aes(shape= "y1_shape"), distribution=distr, dparams=distr_pars, colour= "black", alpha=1, size=1.0) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_shape_manual(name= "", labels=leg_shape_labs, values=leg_shape_cols, breaks=leg_shape_sort) +
  scale_fill_manual(name= "", labels=leg_fill_labs, values=leg_fill_cols, breaks=leg_fill_sort) +
  scale_colour_manual(name= "", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_sort) +
  guides(shape=guide_legend(order=1), fill=guide_legend(order=2), colour=guide_legend(order=3)) +
  theme(plot.title=element_text(hjust=0.5, size=11), 
        plot.subtitle=element_text(hjust=0.5, size=10),
        axis.text.x=element_text(angle=0, vjust=1),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(Stand_Res_ged_QQ_plot)
#
# P-P plot of the empirical probability distribution of the standardized residuals of the GARCH(1,1) model for the Bitcoin daily percentage
# logarithm returns training set estimated by the fGarch::garcgFit() function against the estimated GED function.
# First, we build a suitable data frame.
y_qemp <- qemp(ppoints(length(y)), y)
y_pemp <- pemp(y_qemp, y)
mean <- 0
sd <- 1
nu  <- GED_nu 
distr <- "ged"
distr_pars <- list(mean=0, sd=1, nu=nu)
quants <- qged(ppoints(length(y)), mean=0, sd=1, nu=nu)
probs <- pged(quants, mean=0, sd=1, nu=nu)
PP_plot_df <- data.frame(T=1:length(y), P=probs, X=y, Y=y_pemp)
head(PP_plot_df)
# Second we draw the P-P plot of the standardized residuals.
Data_df <- PP_plot_df
length <- nrow(PP_plot_df)
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("P-P plot of the Empirical Probability of the Standardized Residuals of the fGarcg::garchFit() Fitted GARCH(1,1) Model with GED Innovation for the Bitcoin Daily Logarithm Returns Training Set Against the Estimated GED")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Data set size ", .(length), " sample points; GED density function: mean ", .(mean), ", standard deviation ", .(sd), ", shape ", .(nu), "."~~"Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("Theoretical Probabilities")
y_name <- bquote("Sample Probabilities")
x_breaks_min <- floor(Data_df$P[1])
x_breaks_max <- ceiling(Data_df$P[length])
x_breaks <- seq(from=x_breaks_min, to=x_breaks_max, by=0.5)
x_labs <- format(x_breaks, scientific=FALSE)
J <- 0
x_lims <- c(x_breaks_min-J*x_binwidth, x_breaks_max+J*x_binwidth)
y_breaks_num <- length(x_breaks)
y_binwidth <- round((max(Data_df$Y)-min(Data_df$Y))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$Y)/y_binwidth))*y_binwidth
y_breaks_up <- floor((max(Data_df$Y)/y_binwidth))*y_binwidth
y_breaks <- c(round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3))
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.3
y_lims <- c(y_breaks_low-K*y_binwidth, y_breaks_up+K*y_binwidth)
y1_shape <- bquote("P-P plot")
y1_fill <- bquote("90% confidence intervals")
y2_fill <- bquote("95% confidence intervals")
y1_col <- bquote("y=x line")
y2_col <- bquote("regression line")
leg_shape_labs <- y1_shape
leg_fill_labs <- c(y1_fill, y2_fill)
leg_col_labs <- c(y1_col, y2_col)
leg_shape_cols <- c("y1_shape"=19)
leg_fill_cols <- c("y1_fill"="chartreuse1", "y2_fill"="deepskyblue1")
leg_col_cols <- c("y1_col"="black", "y2_col"="red")
leg_shape_sort <- "y1_shape"
leg_fill_sort <- c("y1_fill", "y2_fill")
leg_col_sort <- c("y1_col", "y2_col")
Stand_Res_ged_PP_plot <- ggplot(Data_df, aes(sample=X)) +
  qqplotr::stat_pp_band(aes(fill= "y2_fill"), distribution=distr, dparams=distr_pars, conf=0.95) +
  qqplotr::stat_pp_band(aes(fill= "y1_fill"), distribution=distr, dparams=distr_pars, conf=0.90) +
  qqplotr::stat_pp_line(aes(colour= "y1_col"), geom="path", position="identity", colour= "black") +
  stat_smooth(aes(x=P, y=Y, colour= "y2_col"), inherit.aes=FALSE, method= "lm", formula=y~x, alpha=1, linewidth=0.8, linetype= "solid", se=FALSE, fullrange=FALSE) +
  qqplotr::stat_pp_point(aes(shape= "y1_shape"), distribution=distr, dparams=distr_pars, colour= "black", alpha=1, size=1.0) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_shape_manual(name= "", labels=leg_shape_labs, values=leg_shape_cols, breaks=leg_shape_sort) +
  scale_fill_manual(name= "", labels=leg_fill_labs, values=leg_fill_cols, breaks=leg_fill_sort) +
  scale_colour_manual(name= "", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_sort) +
  guides(shape=guide_legend(order=1), fill=guide_legend(order=2), colour=guide_legend(order=3)) +
  theme(plot.title=element_text(hjust=0.5, size=11), 
        plot.subtitle=element_text(hjust=0.5, size=10),
        axis.text.x=element_text(angle=0, vjust=1),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(Stand_Res_ged_PP_plot)
#
# In light of the presented plots, we consider the cross-checking of the estimated value of the GED shape parameter over the standardized 
# residuals. Of course, we use other functions than the fGarch::gedFit() function. We start with applying the fitdistrplus::fitdist() 
# function although fed by the fGarch::dged() function.
fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res_fitdist_ged <- fitdistrplus::fitdist(y, dged, start=list(nu=1), fix.arg=list(mean=0, sd=1), 
                                                                             method= "mle")
summary(fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res_fitdist_ged)
# Fitting of the distribution ' ged ' by maximum likelihood 
# Parameters : estimate  Std. Error
#           nu 0.887797 0.03160786
# Fixed parameters: value
#           mean     0
#           sd       1
# Loglikelihood:  -2258.638   AIC:  4519.276   BIC:  4524.725  
#
# Note that we have
show(abs(GED_nu-fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res_fitdist_ged[["estimate"]][["nu"]]))
# 0.00000324423
# much smaller than the estimate standard error. We evaluate the uncertainty in estimated parameter
set.seed(12345)
fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res_fitdist_ged_bd <- fitdistrplus::bootdist(fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res_fitdist_ged, niter=1000)
summary(fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res_fitdist_ged_bd)
# Parametric bootstrap medians and 95% percentile CI 
#     Median      2.5%     97.5% 
#   0.8888134 0.8209580 0.9589805
#
# For a further cross-checking, we also consider the estimates of the GED shape parameter by tackling the log-likelihood function direct
# maximization. To this we need to write the log-likelihood function to be maximized. Note that we again use the GED density provided by the
# fGarch package.
opt_ged_minus_logLik <- function(x) -sum(log(dged(y, mean=0, sd=1, nu=x)))
# Hence, we tackle the optimization of the log-likelihood function by means of the function stats::optimize().
opt_ged_result <- stats::optimize(f=opt_ged_minus_logLik, interval=c(0,1), maximum=FALSE, tol=1e-09)
show(opt_ged_result)
# $minimum 0.8877968
# 
# $objective 2258.638
#
# Note that, setting
logLik <- -opt_ged_minus_logLik(GED_nu) 
n <- length(y)
k <- 1
AIC <- 2*k-2*logLik
BIC <- k*log(n)-2*logLik
AICc <- AIC + 2*k*((k+1)/(n-k-1))
# we obtain
show(c(logLik, AIC, BIC, AICc))
#   logLik      AIC       BIC      AICc
# -2258.638  4519.276  4524.725  4519.278
#
# We show again how to apply pracma::fminunc() and pracma::fmincon() functions conceived to optimize multivariate functions. As above, we
# rewrite the log-likelihood of the GED, fictitiously transformed into a multivariate function by adding a quadratic term.
fmin_minus_logLik <- function(x) x[1]^2-sum(log(dged(y, mean=0, sd=1, nu=x[2])))
#
# Second, we fix the initial points of the unconstrained maximization procedure, that we choose as the median provided by the 
# fitdistrplus::bootdist() function.
nu0 <- as.vector(fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res_fitdist_ged_bd[["CI"]][["Median"]])
show(nu0)
# 0.8888134
# Then, we launch the unconstrained maximization procedure.
fminunc_result <- pracma::fminunc(fn=fmin_minus_logLik, x0=c(0, nu0), tol=1e-08)
show(fminunc_result)
# Error in if (f < fmin) { : missing value where TRUE/FALSE needed
# In addition: Warning message: In log(dged(y, mean = 0, sd = 1, nu = x[2])) : NaNs produced
#
# The procedure returns an error, this is likely due to the choice of the starting point of the minimization procedure. Actually, with a 
# different choice we have
fminunc_result <- pracma::fminunc(fn=fmin_minus_logLik, x0=c(0, 0.8), tol=1e-08)
show(fminunc_result)
# $par x[1]    x[2]=nu
# [1] 0.0000000 0.8877968
# 
# $value
# [1] 2258.638
# 
# $counts
# function gradient 
# 13        9 
# 
# $convergence
# [1] 2
# 
# $message
# [1] "Small gradient norm"
#
# which still returns some convergence problem. However, with another choice we have
fminunc_result <- pracma::fminunc(fn=fmin_minus_logLik, x0=c(0, 0.9), tol=1e-08)
show(fminunc_result)
# $par x[1]    x[2]=nu
# [1] 0.0000000 0.8877968
# 
# $value
# [1] 2258.638
# 
# $counts
# function gradient 
# 17        6 
# 
# $convergence
# [1] 0
# 
# $message
# [1] "Rvmminu converged"
#
# The result of the minimization is the same, but we no longer have the convergence problem. 
# In the end, we consider the constrained optimization procedure where (0,nu0) is the starting point and we use the confidence interval 
# endpoints provided by the fitdistrplus::bootdist() function to build the multivariate constraint.
nu0 <- as.vector(fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res_fitdist_ged_bd[["CI"]][["Median"]])
nu_min <- as.vector(fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res_fitdist_ged_bd[["CI"]][["2.5%"]])
nu_max <- as.vector(fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res_fitdist_ged_bd[["CI"]][["97.5%"]])
show(c(nu0,nu_min,nu_max))
# 0.8888134 0.8209580 0.9589805
#
fmincon_result <- pracma::fmincon(fn=fmin_minus_logLik, x0=c(0, nu0), lb=c(-1, nu_min), ub=c(1, nu_max), tol=1e-06, maxfeval=10000, maxiter=5000) 
show(fmincon_result)
# Extract from the output.
# $par   x[1]    x[2]=nu
# [1] 0.0000000 0.8877968
# 
# $value
# [1] 2258.638
# 
# $convergence
# [1] 0
# 
# $info$grad
# [,1]
# [1,] 0.0000000000
# [2,] 0.0001220703
# 
# $info$hessian
# [,1]     [,2]
# [1,]    1    0.000
# [2,]    0 1001.669
#
# The constrained optimization procedure does not return convergence problems.
# To summarize having estimated a GARCH(1,1) model with GED innovation shape parameter GED_nu=0.8877937 appears to be coherent with the 
# subsequent checks performed by the optimization procedures.
#
# It is also interesting to observe that removing the constraints fix.arg=list(mean=0, sd=1) the optimization procedure still essentially
# confirms the result of a standardized GED with shape parameter 0.8877937. In fact,
fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res_fitdist_unc_ged <- fitdistrplus::fitdist(y, dged, start=list(mean=0, sd=1,nu=1), method= "mle")
summary(fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res_fitdist_unc_ged)
# Fitting of the distribution ' ged ' by maximum likelihood 
# Parameters: estimate  Std. Error
#        mean 0.0322170 0.009594909
#        sd   1.0027808 0.028925438
#        nu   0.8817457 0.035403862
# Loglikelihood:  -2257.035   AIC:  4520.07   BIC:  4536.418 
# Correlation matrix:   mean          sd          nu
#              mean  1.00000000  0.01503907 -0.03167945
#              sd    0.01503907  1.00000000 -0.45469644
#              nu   -0.03167945 -0.45469644  1.00000000
set.seed(12345)
fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res_fitdist_unc_ged_bd <- fitdistrplus::bootdist(fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res_fitdist_unc_ged, niter=1000)
summary(fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res_fitdist_unc_ged_bd)
# Parametric bootstrap medians and 95% percentile CI 
#        Median        2.5%      97.5%
# mean 0.03284073 0.004729641 0.06171873
# sd   1.00206380 0.943246779 1.06333465
# nu   0.88053754 0.814144249 0.97066364
# 
# Note that despite the bootstrapped 95% percentile CI for the mean does not contain the value zero, the bootstrapped 95% percentile CI for
# the standard deviation and shape contain the values one and 0.8877937, respectively.
# The unconstrained minimization of the log-likelihood of the unconstrained ged suffers for the choine of the initial point.
fmin_minus_unc_ged_logLik <- function(x) -sum(log(dged(y, mean=x[1], sd=x[2], nu=x[3])))
fminunc_result <- pracma::fminunc(fn=fmin_minus_unc_ged_logLik, x0=c(0.03, 1.00, 0.881), tol=1e-08)
show(fminunc_result)
# $par
# [1] 0.03221878 1.00248686 0.88198606
# 
# $value
# [1] 2257.035
# 
# $counts
# function gradient 
# 63        9 
# 
# $convergence
# [1] 0
# 
# $message
# [1] "Rvmminu converged"
#
mean0 <- as.vector(fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res_fitdist_unc_ged_bd[["CI"]][[1,1]])
mean_min <- as.vector(fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res_fitdist_unc_ged_bd[["CI"]][[1,2]])
mean_max <- as.vector(fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res_fitdist_unc_ged_bd[["CI"]][[1,3]])
show(c(mean0,mean_min,mean_max))
# 0.032840730 0.004729641 0.061718731
#
sd0 <- as.vector(fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res_fitdist_unc_ged_bd[["CI"]][[2,1]])
sd_min <- as.vector(fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res_fitdist_unc_ged_bd[["CI"]][[2,2]])
sd_max <- as.vector(fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res_fitdist_unc_ged_bd[["CI"]][[2,3]])
show(c(sd0,sd_min,sd_max))
# 1.0020638 0.9432468 1.0633346
#
nu0 <- as.vector(fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res_fitdist_unc_ged_bd[["CI"]][[3,1]])
nu_min <- as.vector(fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res_fitdist_unc_ged_bd[["CI"]][[3,2]])
nu_max <- as.vector(fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res_fitdist_unc_ged_bd[["CI"]][[3,3]])
show(c(nu0,nu_min,nu_max))
# 0.8805375 0.8141442 0.9706636
#
fmincon_result <- pracma::fmincon(fn=fmin_minus_unc_ged_logLik, x0=c(mean0, sd0, nu0), lb=c(mean_min, sd_min, nu_min), ub=c(mean_max, sd_max, nu_max),
                                  tol=1e-06, maxfeval=10000, maxiter=5000) 
show(fmincon_result)
# $par
# [1] 0.03178257 1.00245213 0.88203711
# 
# $value
# [1] 2257.035
# 
# $convergence
# [1] 0
# 
# $info
# $info$lambda
# $info$lambda$lower
# [,1]
# [1,]    0
# [2,]    0
# [3,]    0
# 
# $info$lambda$upper
# [,1]
# [1,]    0
# [2,]    0
# [3,]    0
# 
# $info$grad
# [,1]
# [1,] -16.1932826845
# [2,]  -0.0007915171
# [3,]   0.0009132889
# 
# $info$hessian
# [,1]       [,2]      [,3]
# [1,] 375648279.79 -30290.485 37444.609
# [2,]    -30290.48   1451.880  -213.298
# [3,]     37444.61   -213.298  1349.945
############################################################################################################################################
# In the end we consider the standard goodness of fit tests.
# The Kolmogorov-Smirnov test in the library *stats*
y <- fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res
head(y,20)
# [1]  0.8673333  0.4274806  1.7858863  0.1431021 -0.2794854  0.3989901  2.3425966 -2.2355670  1.0172507 -0.6827799  0.8558484  0.1654581
# [13] -0.4392640 -0.3147066  0.3145931  1.3725041 -0.1097089  0.4150189 -0.5556425 -0.8109007
mean <- 0
sd <- 1
nu  <- GED_nu 
stats::ks.test(y, y="pged", mean=0, sd=1, nu=nu, alternative= "two.sided")
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  y
# D = 0.025775, p-value = 0.2035
# alternative hypothesis: two-sided
#
# The Kolmgorov-Smirnov test cannot reject the null hypothesis that the standardized residuals of the GARCH(1,1) model have the estimated
# GED at the $10\%$ significance level.
# 
# Another application of the Kolmogorov-Smirnov test can be derived using the possibility of comparing two empirical distributions offered 
# by the function stats::ks.test().
mean <- 0
sd <- 1
nu  <- GED_nu 
KS_ged_mat_np <- matrix(NA, nrow=10000, ncol=2)
for(k in 1:10000){
  set.seed(k)
  y_rged <- rged(n=length(y), mean=0, sd=1, nu=nu)
  KS_ged <- stats::ks.test(x=y, y=y_rged, alternative="two.sided")
  KS_ged_mat_np[k,1] <- k
  KS_ged_mat_np[k,2] <- KS_ged[["p.value"]]}
summary(KS_ged_mat_np[,2])
#    Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0000923 0.1468049 0.3424329 0.3908007 0.5978583 0.9989554 
quantile(KS_ged_mat_np[,2], na.rm=TRUE, probs=c(0.01,0.05,0.1))
#        1%         5%          10%
#   0.008392775 0.029900467 0.058067781
#
# In the $5\%$ of cases on 10000 random vectors sampled by the GED, the null hypothesis that the GED fits the standardized residuals
# empirical distribution is rejected at the $5\%$ significance level, not at the $1\%$ significance level, though. In the $10\%$ of cases
# on 10000 random vectors sampled by the GED, the null hypothesis that the GED fits the standardized residuals empirical distribution is not
# rejected at the $5\%$ significance level,
#
# The Cramer-Von Mises test in the library *goftest*.
# This function performs the Cramer-Von Mises test of goodness-of-fit to the distribution specified by the argument null. It is assumed that
# the values in x are independent and identically distributed random values, with some cumulative distribution function F. The null 
# hypothesis that F is the function specified by the argument null, while the alternative hypothesis is that F is some other function.
#
mean <- 0
sd <- 1
nu  <- GED_nu 
goftest::cvm.test(y, null="pged", mean=0, sd=1, nu=nu, estimated=FALSE)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘pged’
# with parameters mean = 0, sd = 1, nu = 0.887793724741315
# Parameters assumed to be fixed
# data:  y
# omega2 = 0.24542, p-value = 0.1943
#
# By default, the Cramer von Mises test assumes that all the parameters of the null distribution are known in advance (a simple null 
# hypothesis). This test does not account for the effect of estimating the parameters.
# If the parameters of the distribution were estimated (that is, if they were calculated from the same data x), then this should be 
# indicated by setting the argument estimated=TRUE. The test will then use the method of Braun (1980) to adjust for the effect of parameter
# estimation. Note that Braun's method involves randomly dividing the data into two equally-sized subsets, so the p-value is not exactly the
# same if the test is repeated. This technique is expected to work well when the number of observations in x is large. However, we approach 
# this version of the test with a technique similar to that we have used in the Kolmogorov-Smirnov test with random sampling. 
#
mean <- 0
sd <- 1
nu  <- GED_nu 
CVM_ged_mat_np <- matrix(NA, nrow=10000, ncol=2)
for(k in 1:10000){
  set.seed(k)
  CVM_ged <- goftest::cvm.test(x=y, null="pged", mean=0, sd=1, nu=nu, estimated=TRUE)
  CVM_ged_mat_np[k,1] <- k
  CVM_ged_mat_np[k,2] <- CVM_ged[["p.value"]]}
summary(CVM_ged_mat_np[,2])
#    Min.    1st Qu.    Median     Mean    3rd Qu.     Max. 
# 0.0000265 0.2359329 0.4828958 0.4880233 0.7360192 0.9997293  
quantile(CVM_ged_mat_np[,2], na.rm=TRUE, probs=c(0.01,0.05,0.1))
#    1%           5%          10% 
# 0.009668316 0.048038362 0.096043813 
#
# In the $5\%$ of cases on 10000 random vectors sampled by the GED, the null hypothesis that the GED fits the standardized residuals 
# empirical distribution is rejected at the $5\%$ significance level, not at the $1\%$ significance level, though. In the $10\%$ of cases 
# on 10000 random vectors sampled by the GED, the null hypothesis that the GED fits the standardized residuals empirical distribution cannot
# be rejected at the $5\%$ significance level.
#
# The Anderson-Darling test in the library *goftest*.
mean <- 0
sd <- 1
nu  <- GED_nu 
goftest::ad.test(y, null="pged", mean=0, sd=1, nu=nu, estimated=FALSE)
#
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘pged’
# with parameters mean = 0, sd = 1, nu = 0.887793724741315
# Parameters assumed to be fixed
# data:  y
# An = 1.3183, p-value = 0.2265
#
# By default, also the Anderson Darling test assumes that all the parameters of the null distribution are known in advance (a simple null 
# hypothesis). This test does not account for the effect of estimating the parameters.
# If the parameters of the distribution were estimated (that is, if they were calculated from the same data x), then this should be 
# indicated by setting the argument estimated=TRUE. The test will then use the method of Braun (1980) to adjust for the effect of parameter
# estimation. Note that Braun's method involves randomly dividing the data into two equally-sized subsets, so the p-value is not exactly the
# same if the test is repeated. This technique is expected to work well when the number of observations in x is large. However, we approach 
# this version of the test with the same technique that we have used in the Cramer von Mises test. 
#
mean <- 0
sd <- 1
nu  <- GED_nu 
AD_ged_mat_np <- matrix(NA, nrow=10000, ncol=2)
for(k in 1:10000){
  set.seed(k)
  AD_ged <- goftest::ad.test(x=y, null="pged", mean=0, sd=1, nu=nu, estimated=TRUE)
  AD_ged_mat_np[k,1] <- k
  AD_ged_mat_np[k,2] <- AD_ged[["p.value"]]}
summary(AD_ged_mat_np[,2])
#    Min.    1st Qu.    Median     Mean    3rd Qu.     Max. 
# 0.0005855 0.2524341 0.5062057 0.5036601 0.7520522 0.9997453 
quantile(AD_ged_mat_np[,2], na.rm=TRUE, probs=c(0.01,0.05))
#      1%          5%  
# 0.01258111 0.05162260 
#
# In the $1\%$ of cases on 10000 random vectors sampled by the GED, the null hypothesis that the GED fits the standardized residuals
# empirical distribution is rejected at the $5\%$ significance level, not at the $1\%$ significance level, though. In the $5\%$ of cases 
# on 10000 random vectors sampled by the GED, the null hypothesis that the GED fits the standardized residuals empirical distribution cannot
# be rejected at the $5\%$ significance level.
#
############################################################################################################################################
############################################################################################################################################
############################################################################################################################################
# The goal is now to produce a forecast of the time series.
############################################################################################################################################
############################################################################################################################################
############################################################################################################################################
# Let us recall the defining equations of the general GARCH(1,1) model. 
# We say that a stochastic process $\left(Z_{t}\right)_{t\in\mathbb{N}_{0}}\equiv Z$ on a probability space
# $\left(\Omega,\mathcal{E},\mathbf{P}\right)\equiv\Omega$ is a GARCH(1,1) process if there exist a standard second order white noise 
# $\left(W_{t}\right)_{t\in\mathbb{N}}\equiv W$ and a positive process $\left(\sigma_{t}\right)_{t\in\mathbb{N}_{0}}\sigma$ on $\Omega$ 
# such that the following equations are satisfied
# $Z_{t} = \sigma_{t}W_{t}$
# $\sigma_{t}^{2} = \alpha_{0} + alpha_{1}Z_{t-1}^{2} + beta_{1}\sigma_{t-1}^{2}$
# for all $t=1,2,\dots$, where $\alpha_{0},alpha_{1},beta_{1}\in \mathbb{R}_{++}$ and $alpha_{1}+beta_{1}<1$.
# The standard white noise process $W$ is called the innovation of the GARCH(1,1) process $Z$ and the positive process $\sigma$ is called
# the volatility of $Z$.
#
# In the model under consideration, $\alpha_{0}$, $alpha_{1}$, and $beta_{1}$ are the parameters omega, alpha1, and beta1, respectively.
omega <- as.numeric(fGarch::coef(fGARCH_1_1_ged_shpN_lbfgsb_nm)[1])
alpha1 <- as.numeric(fGarch::coef(fGARCH_1_1_ged_shpN_lbfgsb_nm)[2])
beta1 <- as.numeric(fGarch::coef(fGARCH_1_1_ged_shpN_lbfgsb_nm)[3])
shape <- as.numeric(fGarch::coef(fGARCH_1_1_ged_shpN_lbfgsb_nm)[4])
show(c(omega, alpha1, beta1, shape))
# 0.34079449 0.08046777 0.89917369 0.88780427
# To compare with the model's summary coefficients.  
# Coefficient(s):
#   omega    alpha1     beta1     shape  
# 0.340794  0.080468  0.899174  0.887804
#
# Let $\left(\mathcal{F}_{t}^{\left(Z_{0},\sigma_{0},W\right)}\right)_{t\in\mathbb{N}_{0}}\equiv\mathfrak{F}_{Z_{0},\sigma_{0},W}$ be the
# filtration (information) generated by the initial state $Z_{0}$ of the process $Z$ and the strong white noise $W$, formally given by
# $\mathcal{F}_{0}^{\left(Z_{0},W\right)}\overset{\text{def}}{=}\sigma\left(Z_{0},\sigma_{0}\right)$
# and
# $\mathcal{F}_{t}^{\left(Z_{0},\sigma_{0},W\right)}\overset{\text{def}}{=}\sigma\left(Z_{0},\sigma_{0},W_{1},\dots,W_{t}\right)
# \quad\forall t\in\mathbb{N}$,
# in the filtration context, $\sigma\left(X,Y,Z,\dots\right)$ denoting the $\sigma$-algebra generated by the random variables $Z,Y,Z,\dots$.
# We know that we have
# $\mathbf{E}\left[Z_{t}\mid\mathcal{F}_{s}\right]=0$
# for every $t\in\mathbb{N}$ and every $s=0,1,\dots,t-1$.
# In terms of a concrete GARCH(1,1) model this implies that the fitted values are all zero.
# Eventually, in the model under consideration we have
fGARCH_1_1_ged_shpN_lbfgsb_nm_fitted <- as.numeric(fGarch::fitted(fGARCH_1_1_ged_shpN_lbfgsb_nm))
head(fGARCH_1_1_ged_shpN_lbfgsb_nm_fitted,20)
# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#
# While in a concrete GARCH(1,1) model we can observe a realization of the process $Z$, which is the path of the values of the process $Z$
# formally given by the sequence of real numbers $\left(Z_{t}\left(\omega\right)\right)_{t\in\mathbb{N}_{0}}$, for the occurred outcome 
# $\omega\in\Omega$, we cannot observe the corresponding realization of the $Z$ volatility$\sigma$, which is the path of the values of the
# process $\sigma$ formally given by $\left(\sigma_{t}\left(\omega\right)\right)_{t\in\mathbb{N}_{0}}$. This because we have no way to 
# observe the value $\sigma_{0}\left(\omega\right)$. As we discussed above, the value $\sigma_{0}\left(\omega\right)$ is typically chosen
# in different ways. In particular, in the fGARCH_1_1_ged_shpN_lbfgsb_nm model is set to
# $\sigma_{0}\left(\omega\right)\equiv\alpha_{0}+\left(\alpha_{1}+\beta_{1}\right)\frac{1}{T}\sum_{t=0}^{T-1}{Z_{t}^{2}\left(\omega\right)$.
# Concretely,
head(BTC_red_df)
tail(BTC_red_df)
nrow(BTC_red_df)
# 1871
DS_length <- nrow(BTC_red_df)
show(DS_length)
# 1871
TrnS_length <- length(BTC_red_df$log_ret_perc[which(BTC_red_df$Date<as.Date("2023-01-01"))])
show(TrnS_length)
# 1720
TstS_length <- DS_length-TrnS_length
show(TstS_length)
# 151
y <- BTC_red_df$log_ret_perc
head(y)
# NA  3.2535928  1.5906496  6.4376485  0.5608363 -1.0523309
T <- length(y[2:TrnS_length])
show(T)
# 1719
fGARCH_1_1_ged_shpN_lbfgsb_nm_sigma0 <- sqrt(omega + (alpha1+beta1)*(1/T)*sum(y[2:TrnS_length]^2))
show(fGARCH_1_1_ged_shpN_lbfgsb_nm_sigma0)
# 3.75126
#
# This corresponds to
fGarch::volatility(fGARCH_1_1_ged_shpN_lbfgsb_nm, type="sigma")[1]
# 3.75126
#
# Once $\sigma_{0}\left(\omega\right)$ is chosen, the full path $\left(\sigma_{t}\left(\omega\right)\right)_{t\in\mathbb{N}_{0}}$ can be
# built. This in the context of the concrete GARCH(1,1) process is called the conditional volatility and it is computed as follows
fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_var_cmp <- vector(mode="numeric", length=T)
fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_var_cmp[1] <- fGARCH_1_1_ged_shpN_lbfgsb_nm_sigma0^2
for(t in 2:T){
  fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_var_cmp[t] <- omega + alpha1*na.rm(y)[t-1]^2 + beta1*fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_var_cmp[t-1]
}
fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_std_dev_cmp <- sqrt(fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_var_cmp)
# 
# We obtain
head(fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_var_cmp,20)
# 14.07195 13.84574 12.99412 15.35962 14.17707 13.17755 12.35851 16.91060 22.34711 22.29552 21.22472 20.67650 18.97811 17.70007 16.39730
# 15.21540 16.32847 15.03874 14.07167 13.34326
#
head(fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_std_dev_cmp,20)
# 3.751260 3.720987 3.604736 3.919135 3.765245 3.630090 3.515467 4.112250 4.727272 4.721813 4.607029 4.547142 4.356387 4.207146 4.049358
# 3.900692 4.040850 3.877981 3.751222 3.652842
#
# The conditional standard deviation and variance can also be obtained by applying to the fGARCH_1_1_ged_shpN_lbfgsb_nm model the extractor
# functions fGarch::volatility(..., type="h") and fGarch::volatility(..., type="sigma"), respectively.
fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_var <- fGarch::volatility(fGARCH_1_1_ged_shpN_lbfgsb_nm, type="h")
head(fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_var,20)
# 14.07195 13.84574 12.99412 15.35962 14.17707 13.17755 12.35851 16.91060 22.34711 22.29552 21.22472 20.67650 18.97811 17.70007 16.39730
# 15.21540 16.32847 15.03874 14.07167 13.34326
#
identical(round(fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_var_cmp,11),round(as.vector(fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_var),11))
# TRUE
#
fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_std_dev <- fGarch::volatility(fGARCH_1_1_ged_shpN_lbfgsb_nm, type="sigma")
head(fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_std_dev,20)
# 3.751260 3.720987 3.604736 3.919135 3.765245 3.630090 3.515467 4.112250 4.727272 4.721813 4.607029 4.547142 4.356387 4.207146 4.049358
# 3.900692 4.040850 3.877981 3.751222 3.652842
#
identical(round(fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_std_dev_cmp,11),round(as.vector(fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_std_dev),11))
# TRUE
#
fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res <- fGarch::residuals(fGARCH_1_1_ged_shpN_lbfgsb_nm, standardize=TRUE)
head(fGARCH_1_1_ged_shpN_lbfgsb_nm_stand_res,20)
#  0.8673333  0.4274806  1.7858863  0.1431021 -0.2794854  0.3989901  2.3425966 -2.2355670  1.0172507 -0.6827799  0.8558484  0.1654581
# -0.4392640 -0.3147066  0.3145931  1.3725041 -0.1097089  0.4150189 -0.5556425 -0.8109007
#
# We plot the Bitcoin daily logarithm return percentage and the conditional standard deviation of the fGarch::garchFit() fitted GARCH(1,1)
# model with GED innovation training set.
head(BTC_train_df)
tail(BTC_train_df)
Data_df <- BTC_train_df
head(Data_df)
Data_df <- dplyr::rename(Data_df, x=t, y=log_ret_perc, z=fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_stand_dev)
head(Data_df)
tail(Data_df)
First_Day <- as.character(Data_df$Date[2])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Bitcoin Daily Logarithm Return Percentage  and Conditional Standard Deviation of the fGarch::garchFit() Fitted GARCH(1,1) Model with GED Innovation - Training Set - from ", .(First_Day), " to ", .(Last_Day), sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points. Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("")
# numbers::primeFactors(TrnS_length-2)
x_breaks_num <- 44 # (deduced from primeFactors(TrnS_length-2))
x_breaks_low <- Data_df$x[2]
x_breaks_up <- Data_df$x[nrow(Data_df)]
x_binwidth <- ceiling((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Date[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("daily logarithm return percentage")
y_max <- max(na.rm(Data_df$y))
y_min <- min(na.rm(Data_df$y))
as.numeric(floor(y_max-y_min))
y_breaks_num <- 10
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor((y_min/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((y_max/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth), digits=3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.5
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
point_black <- bquote("daily perc. log. ret.")
leg_point_labs <- c(point_black)
leg_point_cols <- c("point_black"="black")
leg_point_breaks <- c("point_black")
line_magenta <- bquote("cond. stand. dev. - GED inn.")
line_green <- bquote("regression line")
line_red <- bquote("LOESS curve")
leg_line_labs <- c(line_magenta, line_green, line_red)
leg_line_cols <- c("line_magenta"="magenta", "line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_magenta", "line_green", "line_red")
leg_col_labs   <- c(leg_point_labs,leg_line_labs)
leg_col_cols   <- c(leg_point_cols,leg_line_cols)
leg_col_breaks <- c(leg_point_breaks,leg_line_breaks)
BTC_perc_log_ret_fGARCH_1_1_cond_stand_dev_ged_inn_TrnS_sp <- ggplot(Data_df, aes(x=x)) +
  geom_smooth(aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_point(aes(y=y, color="point_black"), alpha=1, size=0.7, shape=19, na.rm=TRUE) + 
  geom_line(aes(y=z, color="line_magenta"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  geom_line(aes(y=-z, color="line_magenta"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  guides(colour=guide_legend(override.aes=list(shape=c(19, NA, NA, NA), linetype=c("blank", "solid", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_perc_log_ret_fGARCH_1_1_cond_stand_dev_ged_inn_TrnS_sp)
#
# The line plot.
line_black <- bquote("daily perc. log. ret.")
line_magenta <- bquote("cond. stand. dev. - GED inn.")
line_green <- bquote("regression line")
line_red <- bquote("LOESS curve")
leg_line_labs <- c(line_black, line_magenta, line_green, line_red)
leg_line_cols <- c("line_black"="black", "line_magenta"="magenta", "line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_black", "line_magenta", "line_green", "line_red")
BTC_perc_log_ret_fGARCH_1_1_cond_stand_dev_ged_inn_TrnS_lp <- ggplot(Data_df, aes(x=x)) +
  geom_smooth(aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_line(aes(y=y, color="line_black"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  geom_line(aes(y=z, color="line_magenta"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  geom_line(aes(y=-z, color="line_magenta"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="", labels=leg_line_labs, values=leg_line_cols, breaks=leg_line_breaks) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_perc_log_ret_fGARCH_1_1_cond_stand_dev_ged_inn_TrnS_lp)
#
# We can add the confidence bands to the plot by observing that, having estimated the conditional standard deviation $\sigma_{t}$ and the
# distribution of the innovation $W_{t}$, thanks to the stochastic equation
# $Z_{t} = \sigma_{t}W_{t}$,
# the desired confidence bands are given by the product of the estimated standard deviation times the corresponding quantiles of the 
# innovation estimated distribution.
#
quants_080 <- fGarch::qged(p=c(0.1,0.9), mean=0, sd=1, nu=shape)
show(quants_080)
# -1.097594  1.097594
#
fGARCH_1_1_ged_shpN_lbfgsb_nm_080_low_conf_band <- fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_stand_dev*quants_080[1]
head(fGARCH_1_1_ged_shpN_lbfgsb_nm_080_low_conf_band)
# -4.117364 -4.084138 -3.956542 -4.301632 -4.132723 -3.984377
#
fGARCH_1_1_ged_shpN_lbfgsb_nm_080_upp_conf_band <- fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_stand_dev*quants_080[2]
head(fGARCH_1_1_ged_shpN_lbfgsb_nm_080_upp_conf_band)
# 4.117364 4.084138 3.956542 4.301632 4.132723 3.984377
#
quants_085 <- fGarch::qged(p=c(0.075,0.925), mean=0, sd=1, nu=shape)
show(quants_085)
# -1.307547  1.307547
#
fGARCH_1_1_ged_shpN_lbfgsb_nm_085_low_conf_band <- fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_stand_dev*quants_085[1]
head(fGARCH_1_1_ged_shpN_lbfgsb_nm_085_low_conf_band)
# -4.904954 -4.865373 -4.713369 -5.124470 -4.923251 -4.746528
#
fGARCH_1_1_ged_shpN_lbfgsb_nm_085_upp_conf_band <- fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_stand_dev*quants_085[2]
head(fGARCH_1_1_ged_shpN_lbfgsb_nm_085_upp_conf_band)
# 4.904954 4.865373 4.713369 5.124470 4.923251 4.746528
#
quants_090 <- fGarch::qged(p=c(0.050,0.950), mean=0, sd=1, nu=shape)
show(quants_090)
# -1.608124  1.608124
#
fGARCH_1_1_ged_shpN_lbfgsb_nm_090_low_conf_band <- fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_stand_dev*quants_090[1]
head(fGARCH_1_1_ged_shpN_lbfgsb_nm_090_low_conf_band)
# -6.032497 -5.983817 -5.796871 -6.302475 -6.055000 -5.837652
#
fGARCH_1_1_ged_shpN_lbfgsb_nm_090_upp_conf_band <- fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_stand_dev*quants_090[2]
head(fGARCH_1_1_ged_shpN_lbfgsb_nm_090_upp_conf_band)
# 6.032497 5.983817 5.796871 6.302475 6.055000 5.837652
#
head(BTC_train_df)
tail(BTC_train_df)
Data_df <- BTC_train_df
head(Data_df)
Data_df <- dplyr::rename(Data_df, x=t, y=log_ret_perc, z=fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_stand_dev)
head(Data_df)
tail(Data_df)
First_Day <- as.character(Data_df$Date[2])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Bitcoin Daily Logarithm Return Percentage, Conditional Standard Deviation, and Confidence Bands of the fGarch::garchFit() Fitted GARCH(1,1) Model with GED Innovation - Training Set - from ", .(First_Day), " to ", .(Last_Day), sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points. Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("")
# numbers::primeFactors(TrnS_length-2)
x_breaks_num <- 44 # (deduced from primeFactors(TrnS_length-2))
x_breaks_low <- Data_df$x[2]
x_breaks_up <- Data_df$x[nrow(Data_df)]
x_binwidth <- ceiling((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Date[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("daily logarithm return percentage")
y_max <- max(na.rm(Data_df$y))
y_min <- min(na.rm(Data_df$y))
as.numeric(floor(y_max-y_min))
y_breaks_num <- 10
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor((y_min/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((y_max/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth), digits=3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.5
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
point_black <- bquote("daily perc. log. ret.")
leg_point_labs <- c(point_black)
leg_point_cols <- c("point_black"="black")
leg_point_breaks <- c("point_black")
line_magenta <- bquote("cond. stand. dev. - GED inn.")
line_green <- bquote("regression line")
line_red <- bquote("LOESS curve")
leg_line_labs <- c(line_magenta, line_green, line_red)
leg_line_cols <- c("line_magenta"="magenta", "line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_magenta", "line_green", "line_red")
fill_col_gray100 <- bquote("80% conf. band")
fill_col_gray55 <- bquote("85% conf. band")
fill_col_gray10 <- bquote("90% conf. band")
leg_fill_labs <- c(fill_col_gray100,fill_col_gray55,fill_col_gray10)
leg_fill_cols <- c("fill_col_gray100"="gray100","fill_col_gray55"="gray55","fill_col_gray10"="gray10")
leg_fill_breaks <- c("fill_col_gray100","fill_col_gray55","fill_col_gray10")
leg_col_labs   <- c(leg_point_labs,leg_line_labs)
leg_col_cols   <- c(leg_point_cols,leg_line_cols)
leg_col_breaks <- c(leg_point_breaks,leg_line_breaks)
BTC_perc_log_ret_fGARCH_1_1_cond_stand_dev_ged_inn_TrnS_sp <- ggplot(Data_df, aes(x=x)) +
  geom_ribbon(aes(ymin=z*quants_080[1], ymax=z*quants_080[2], fill="fill_col_gray100"), alpha=0.3, colour= "gray100") +
  geom_ribbon(aes(ymin=z*quants_085[1], ymax=z*quants_080[1], fill="fill_col_gray55"), alpha=0.3, colour= "gray55") +
  geom_ribbon(aes(ymin=z*quants_090[1], ymax=z*quants_085[1], fill="fill_col_gray10"), alpha=0.3, colour= "gray10") +
  geom_ribbon(aes(ymin=z*quants_080[2], ymax=z*quants_085[2], fill="fill_col_gray55"), alpha=0.3, colour= "gray55") +
  geom_ribbon(aes(ymin=z*quants_085[2], ymax=z*quants_090[2], fill="fill_col_gray10"), alpha=0.3, colour= "gray10") +
  geom_smooth(aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_point(aes(y=y, color="point_black"), alpha=1, size=0.7, shape=19, na.rm=TRUE) + 
  geom_line(aes(y=z, color="line_magenta"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  geom_line(aes(y=-z, color="line_magenta"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  scale_fill_manual(name= "", labels=leg_fill_labs, values=leg_fill_cols, breaks=leg_fill_breaks) +
  guides(colour=guide_legend(override.aes=list(shape=c(19, NA, NA, NA), linetype=c("blank", "solid", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_perc_log_ret_fGARCH_1_1_cond_stand_dev_ged_inn_TrnS_sp)
#
# The line plot.
line_black <- bquote("daily perc. log. ret.")
line_magenta <- bquote("cond. stand. dev. - GED inn.")
line_green <- bquote("regression line")
line_red <- bquote("LOESS curve")
leg_line_labs <- c(line_black, line_magenta, line_green, line_red)
leg_line_cols <- c("line_black"="black", "line_magenta"="magenta", "line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_black", "line_magenta", "line_green", "line_red")
fill_col_gray100 <- bquote("80% conf. band")
fill_col_gray55 <- bquote("85% conf. band")
fill_col_gray10 <- bquote("90% conf. band")
leg_fill_labs <- c(fill_col_gray100,fill_col_gray55,fill_col_gray10)
leg_fill_cols <- c("fill_col_gray100"="gray100","fill_col_gray55"="gray55","fill_col_gray10"="gray10")
leg_fill_breaks <- c("fill_col_gray100","fill_col_gray55","fill_col_gray10")
BTC_perc_log_ret_fGARCH_1_1_cond_stand_dev_ged_inn_TrnS_lp <- ggplot(Data_df, aes(x=x)) +
  geom_ribbon(aes(ymin=z*quants_080[1], ymax=z*quants_080[2], fill="fill_col_gray100"), alpha=0.3, colour= "gray100") +
  geom_ribbon(aes(ymin=z*quants_085[1], ymax=z*quants_080[1], fill="fill_col_gray55"), alpha=0.3, colour= "gray55") +
  geom_ribbon(aes(ymin=z*quants_090[1], ymax=z*quants_085[1], fill="fill_col_gray10"), alpha=0.3, colour= "gray10") +
  geom_ribbon(aes(ymin=z*quants_080[2], ymax=z*quants_085[2], fill="fill_col_gray55"), alpha=0.3, colour= "gray55") +
  geom_ribbon(aes(ymin=z*quants_085[2], ymax=z*quants_090[2], fill="fill_col_gray10"), alpha=0.3, colour= "gray10") +
  geom_smooth(aes(y=y, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_line(aes(y=y, color="line_black"), alpha=1, lwd=0.5, linetype="solid", na.rm=TRUE) +
  geom_line(aes(y=z, color="line_magenta"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  geom_line(aes(y=-z, color="line_magenta"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="", labels=leg_line_labs, values=leg_line_cols, breaks=leg_line_breaks) +
  scale_fill_manual(name= "", labels=leg_fill_labs, values=leg_fill_cols, breaks=leg_fill_breaks) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_perc_log_ret_fGARCH_1_1_cond_stand_dev_ged_inn_TrnS_lp)
#
# We also draw the plot of the Bitcoin daily squared logarithm return percentage and the conditional variance of the fGarch::garchFit() 
# fitted GARCH(1,1) model with GED innovation training set.
head(BTC_train_df)
tail(BTC_train_df)
Data_df <- BTC_train_df
head(Data_df)
Data_df <- dplyr::rename(Data_df, x=t, y=log_ret_perc, z=fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_var)
head(Data_df)
First_Day <- as.character(Data_df$Date[2])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Bitcoin Daily Squared Logarithm Return Percentage and Conditional Variance of the fGarch::garchFit() Fitted GARCH(1,1) Model with GED Innovation - Training Set - from ", .(First_Day), " to ", .(Last_Day), sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points. Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("")
# numbers::primeFactors(TrnS_length-2)
x_breaks_num <- 44 # (deduced from primeFactors(TrnS_length-2))
x_breaks_low <- Data_df$x[2]
x_breaks_up <- Data_df$x[nrow(Data_df)]
x_binwidth <- ceiling((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Date[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("daily squared logarithm return percentage")
y_max <- max(na.rm(Data_df$y^2))
y_min <- min(na.rm(Data_df$y^2))
as.numeric(floor(y_max-y_min))
y_breaks_num <- 10
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor((y_min/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((y_max/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth), digits=3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.0
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
point_black <- bquote("daily squared perc. log. ret.")
leg_point_labs <- c(point_black)
leg_point_cols <- c("point_black"="black")
leg_point_breaks <- c("point_black")
line_magenta <- bquote("GARCH(1,1) cond. var. - GED inn.")
line_green <- bquote("regression line")
line_red <- bquote("LOESS curve")
leg_line_labs <- c(line_magenta, line_green, line_red)
leg_line_cols <- c("line_magenta"="magenta", "line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_magenta", "line_green", "line_red")
leg_col_labs   <- c(leg_point_labs,leg_line_labs)
leg_col_cols   <- c(leg_point_cols,leg_line_cols)
leg_col_breaks <- c(leg_point_breaks,leg_line_breaks)
BTC_perc_log_ret_fGARCH_1_1_cond_var_ged_inn_TrnS_sp <- ggplot(Data_df, aes(x=x)) +
  geom_smooth(aes(y=y^2, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y^2, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_point(aes(y=y^2, color="point_black"), alpha=1, size=0.7, shape=19, na.rm=TRUE) + 
  geom_line(aes(y=z, color="line_magenta"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  guides(colour=guide_legend(override.aes=list(shape=c(19, NA, NA, NA), linetype=c("blank", "solid", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_perc_log_ret_fGARCH_1_1_cond_var_ged_inn_TrnS_sp)
#
# The line plot.
line_black <- bquote("daily squared perc. log. ret.")
line_magenta <- bquote("cond. var. - GED inn.")
line_green <- bquote("regression line")
line_red <- bquote("LOESS curve")
leg_line_labs <- c(line_black, line_magenta, line_green, line_red)
leg_line_cols <- c("line_black"="black", "line_magenta"="magenta", "line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_black", "line_magenta", "line_green", "line_red")
BTC_perc_log_ret_fGARCH_1_1_cond_var_ged_inn_TrnS_lp <- ggplot(Data_df, aes(x=x)) +
  geom_smooth(aes(y=y^2, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y^2, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_line(aes(y=y^2, color="line_black"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  geom_line(aes(y=z, color="line_magenta"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="", labels=leg_line_labs, values=leg_line_cols, breaks=leg_line_breaks) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_perc_log_ret_fGARCH_1_1_cond_var_ged_inn_TrnS_lp)
#
# Plot of the Bitcoin daily squared logarithm return percentage and the conditional variance of the fGarch::garchFit() fitted GARCH(1,1)
# model with GED innovation training set (with the extreme outlier at date 2020-03-11.
head(BTC_train_df)
tail(BTC_train_df)
Data_df <- BTC_train_df
head(Data_df)
Data_df <- dplyr::rename(Data_df, x=t, y=log_ret_perc, z=fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_var)
head(Data_df)
First_Day <- as.character(Data_df$Date[2])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
Rem_Day <- as.character(Data_df$Date[which.max(Data_df$y^2)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Bitcoin Daily Squared Logarithm Return Percentage and Conditional Variance of the fGarch::garchFit() Fitted GARCH(1,1) Model with GED Innovation - Training Set - from ", .(First_Day), " to ", .(Last_Day),  ", day ", .(Rem_Day), " removed", sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points. Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("")
# numbers::primeFactors(TrnS_length-2)
x_breaks_num <- 44 # (deduced from primeFactors(TrnS_length-2))
x_breaks_low <- Data_df$x[2]
x_breaks_up <- Data_df$x[nrow(Data_df)]
x_binwidth <- ceiling((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Date[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("daily squared logarithm return percentage")
y_max <- max(na.rm(Data_df$y[-which.max(Data_df$y^2)]^2))
y_min <- min(na.rm(Data_df$y[-which.max(Data_df$y^2)]^2))
as.numeric(floor(y_max-y_min))
y_breaks_num <- 10
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor((y_min/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((y_max/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth), digits=3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.0
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
point_black <- bquote("daily squared perc. log. ret.")
leg_point_labs <- c(point_black)
leg_point_cols <- c("point_black"="black")
leg_point_breaks <- c("point_black")
line_magenta <- bquote("GARCH(1,1) cond. var. - GED inn.")
line_green <- bquote("regression line")
line_red <- bquote("LOESS curve")
leg_line_labs <- c(line_magenta, line_green, line_red)
leg_line_cols <- c("line_magenta"="magenta", "line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_magenta", "line_green", "line_red")
leg_col_labs   <- c(leg_point_labs,leg_line_labs)
leg_col_cols   <- c(leg_point_cols,leg_line_cols)
leg_col_breaks <- c(leg_point_breaks,leg_line_breaks)
BTC_perc_log_ret_fGARCH_1_1_cond_var_ged_inn_TrnS_sp <- ggplot(Data_df, aes(x=x)) +
  geom_smooth(aes(y=y^2, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y^2, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_point(aes(y=y^2, color="point_black"), alpha=1, size=0.7, shape=19, na.rm=TRUE) + 
  geom_line(aes(y=z, color="line_magenta"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  guides(colour=guide_legend(override.aes=list(shape=c(19, NA, NA, NA), linetype=c("blank", "solid", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_perc_log_ret_fGARCH_1_1_cond_var_ged_inn_TrnS_sp)
#
# The line plot.
line_black <- bquote("daily squared perc. log. ret.")
line_magenta <- bquote("GARCH(1,1) cond. var. - GED inn.")
line_green <- bquote("regression line")
line_red <- bquote("LOESS curve")
leg_line_labs <- c(line_black, line_magenta, line_green, line_red)
leg_line_cols <- c("line_black"="black", "line_magenta"="magenta", "line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_black", "line_magenta", "line_green", "line_red")
BTC_perc_log_ret_fGARCH_1_1_cond_var_ged_inn_TrnS_lp <- ggplot(Data_df, aes(x=x)) +
  geom_smooth(aes(y=y^2, color="line_green"), alpha=1, lwd=0.9, linetype="solid", method="lm" , formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_smooth(aes(y=y^2, color="line_red"), alpha=1, lwd=0.9, linetype="dashed", method="loess", formula=y ~ x, na.rm=TRUE, se=FALSE) +
  geom_line(aes(y=y^2, color="line_black"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  geom_line(aes(y=z, color="line_magenta"), alpha=1, lwd=0.7, linetype="solid", na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="", labels=leg_line_labs, values=leg_line_cols, breaks=leg_line_breaks) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(BTC_perc_log_ret_fGARCH_1_1_cond_var_ged_inn_TrnS_lp)
#
############################################################################################################################################
# We now have all the elements to build the predictions of the GARCH(1,1) model.
# As an immediate consequence of the defining equations of the process, the predicted path of the process $Z$ is just a sequence of zeroes.
#
y_pred <- rep(0,TstS_length)
head(y_pred, 20)
# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# 
# To predict the volatility, we should consider that we predict conditioning to the information till the end of the training set.
# That is we need to compute $\mathbf{E}\left[\sigma_{T+t}\mid\mathcal{F}_{T}\right]=0$, for every $t\in\mathbb{N}$, where $T$ is the 
# train set length and $t$ actually varies till the the test set length. This calls for a modification of the recursive formula as follows.

# We introduce an empty vector to store the steps of the recursive formula
fGARCH_1_1_ged_shpN_lbfgsb_nm_pred_var <- vector("numeric", length=TstS_length)
# We compute the first term of the predicted variance, considering that the last observed percentage logarithm return is stored at the 
# (TrnS_length+1)th row of the BTC_red_df data frame.
# 
fGARCH_1_1_ged_shpN_lbfgsb_nm_pred_var[1] <-  omega + alpha1*BTC_red_df$log_ret_perc[TrnS_length+1]^2 + beta1*fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_var[TrnS_length]
# We launch the recursive procedure to determine all terms of the predicted variance 
for(t in 2:TstS_length){
  fGARCH_1_1_ged_shpN_lbfgsb_nm_pred_var[t] <- omega + (alpha1+beta1)*fGARCH_1_1_ged_shpN_lbfgsb_nm_pred_var[t-1]
}
head(fGARCH_1_1_ged_shpN_lbfgsb_nm_pred_var,20)
# 4.728216 4.972751 5.212307 5.446987 5.676889 5.902110 6.122746 6.338890 6.550634 6.758067 6.961277 7.160350 7.355371 7.546421 7.733581
# 7.916931 8.096548 8.272509 8.444887 8.613756
#
tail(fGARCH_1_1_ged_shpN_lbfgsb_nm_pred_var,20)
# 15.92794 15.94446 15.96065 15.97651 15.99205 16.00727 16.02218 16.03678 16.05109 16.06511 16.07884 16.09229 16.10547 16.11838 16.13103
# 16.14342 16.15556 16.16745 16.17910 16.19051
#
# We compute the predicted volatility (standard deviation) as the square root of the variance
fGARCH_1_1_ged_shpN_lbfgsb_nm_pred_std_dev <- sqrt(fGARCH_1_1_ged_shpN_lbfgsb_nm_pred_var)
head(fGARCH_1_1_ged_shpN_lbfgsb_nm_pred_std_dev,20)
# 2.174446 2.229967 2.283048 2.333878 2.382622 2.429426 2.474418 2.517715 2.559421 2.599628 2.638423 2.675883 2.712079 2.747075 2.780932
# 2.813704 2.845443 2.876197 2.906009 2.934920
#
tail(fGARCH_1_1_ged_shpN_lbfgsb_nm_pred_std_dev,20)
# 3.990982 3.993052 3.995078 3.997063 3.999006 4.000908 4.002771 4.004595 4.006381 4.008130 4.009843 4.011520 4.013162 4.014771 4.016345
# 4.017888 4.019398 4.020877 4.022325 4.023743
#
# Note that computing the long-run variance and standard deviation, we obtain
long_run_var <- omega/(1-(alpha1+beta1))
show(long_run_var)
# 16.73963
#
long_run_std_dev <- sqrt(long_run_var)
show(long_run_std_dev)
# 4.091409
#
# Hence, it is possible to grasp the asymptotic tendency of the predicted variance and standard deviation to the long-run variance and standard
# deviation, respectively. We can also obtain the predicted standard deviation as part of the fGarch::predict() function output. In fact, 
# considering prediction bands of the $80\%$, we have
fGARCH_1_1_ged_shpN_lbfgsb_nm_uncond_080_pred <- fGarch::predict(fGARCH_1_1_ged_shpN_lbfgsb_nm, n.ahead=TstS_length, nx=TrnS_length, 
                                                                 mse="uncond", conf=0.80, trace=FALSE, plot=TRUE)
head(fGARCH_1_1_ged_shpN_lbfgsb_nm_uncond_080_pred, 15)
# meanForecast meanError standardDeviation lowerInterval upperInterval
# 1             0  3.743863          2.174446     -4.109242      4.109242
# 2             0  3.743863          2.229967     -4.109242      4.109242
# 3             0  3.743863          2.283048     -4.109242      4.109242
# 4             0  3.743863          2.333878     -4.109242      4.109242
# 5             0  3.743863          2.382622     -4.109242      4.109242
# 6             0  3.743863          2.429426     -4.109242      4.109242
# 7             0  3.743863          2.474418     -4.109242      4.109242
# 8             0  3.743863          2.517715     -4.109242      4.109242
# 9             0  3.743863          2.559421     -4.109242      4.109242
# 10            0  3.743863          2.599628     -4.109242      4.109242
# 11            0  3.743863          2.638423     -4.109242      4.109242
# 12            0  3.743863          2.675883     -4.109242      4.109242
# 13            0  3.743863          2.712079     -4.109242      4.109242
# 14            0  3.743863          2.747075     -4.109242      4.109242
# 15            0  3.743863          2.780932     -4.109242      4.109242
#
tail(fGARCH_1_1_ged_shpN_lbfgsb_nm_uncond_080_pred, 15)
# meanForecast meanError standardDeviation lowerInterval upperInterval
# 137            0  3.743863          4.000908     -4.109242      4.109242
# 138            0  3.743863          4.002771     -4.109242      4.109242
# 139            0  3.743863          4.004595     -4.109242      4.109242
# 140            0  3.743863          4.006381     -4.109242      4.109242
# 141            0  3.743863          4.008130     -4.109242      4.109242
# 142            0  3.743863          4.009843     -4.109242      4.109242
# 143            0  3.743863          4.011520     -4.109242      4.109242
# 144            0  3.743863          4.013162     -4.109242      4.109242
# 145            0  3.743863          4.014771     -4.109242      4.109242
# 146            0  3.743863          4.016345     -4.109242      4.109242
# 147            0  3.743863          4.017888     -4.109242      4.109242
# 148            0  3.743863          4.019398     -4.109242      4.109242
# 149            0  3.743863          4.020877     -4.109242      4.109242
# 150            0  3.743863          4.022325     -4.109242      4.109242
# 151            0  3.743863          4.023743     -4.109242      4.109242
#
# Note that the zeros in the "meanForecast" column which represent the predicted percentage log returns. Note also that
identical(round(fGARCH_1_1_ged_shpN_lbfgsb_nm_pred_std_dev,11), round(fGARCH_1_1_ged_shpN_lbfgsb_nm_uncond_080_pred[["standardDeviation"]],11))
# TRUE
#
# Other items in the fGarch::predict() function output are the unconditional "meanError", the "lowerInterval", and the "upperInterval". We
# check them closer.
# The unconditional meanError is given by the square root of the biased mean of the squared errors in fitting the observed values of the 
# logarithm return percentage. Formally
# $\mathbf{MSE}\overset{\text{def}}{=}
# \sqrt\left(frac{1}{T}\sum_{t=1}^{T}\left(Z_{t}\left(omega\right)-\mathbf{E}\left[Z_{t}\mid\mathcal{F}_{t-1}\right]\left(omega\right)\right)^{2}\right)
# where $T$ is the train set length. On the other hand since in a GARCH(1,1) model the fitted values are all zero, the meanError is simply 
# given by the square root of the biased mean of the squared percentage returns. In fact, setting
meanError_cmp <- sqrt((1/(TrnS_length))*sum(na.rm(BTC_red_df$log_ret_perc[1:(TrnS_length+1)])^2))
show(meanError_cmp)
# 3.743863
#
# We have
round(meanError_cmp,14)==round(fGARCH_1_1_ged_shpN_lbfgsb_nm_uncond_080_pred[["meanError"]][1],14)
# TRUE
#
# Under the options mse="uncond", the prediction bands "lowerInterval" and "upperInterval" are built as products of the meanError with the
# quantiles of the estimated distribution of the innovation specified by the option conf=0.80. In our case, the estimated distribution of 
# the innovation is a GED with shape parameter given by shape=0.887804. That is
quants_080 <- fGarch::qged(p=c(0.1,0.9), mean=0, sd=1, nu=shape)
show(quants_080)
# -1.097594  1.097594
#
lowerInterval_cmp <- rep(quants_080[1]*meanError_cmp,TstS_length)
head(lowerInterval_cmp)
#  -4.109242 -4.109242 -4.109242 -4.109242 -4.109242 -4.109242
#
upperInterval_cmp <- rep(quants_080[2]*meanError_cmp,TstS_length)
head(upperInterval_cmp)
# 4.109242 4.109242 4.109242 4.109242 4.109242 4.109242
#
identical(round(lowerInterval_cmp,13),round(fGARCH_1_1_ged_shpN_lbfgsb_nm_uncond_080_pred[["lowerInterval"]],13))
# TRUE
#
identical(round(upperInterval_cmp,13),round(fGARCH_1_1_ged_shpN_lbfgsb_nm_uncond_080_pred[["upperInterval"]],13))
# TRUE
#
# In the end, under the option rms="cond", the meanError just coincides with the predicted conditional standard deviation
fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_080_pred <- fGarch::predict(fGARCH_1_1_ged_shpN_lbfgsb_nm, n.ahead=TstS_length, nx=TrnS_length, 
                                                               mse="cond", conf=0.80, trace=FALSE, plot=TRUE)
head(fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_080_pred, 15)
# meanForecast meanError standardDeviation lowerInterval upperInterval
# 1             0  2.174446          2.174446     -2.386659      2.386659
# 2             0  2.229967          2.229967     -2.447598      2.447598
# 3             0  2.283048          2.283048     -2.505860      2.505860
# 4             0  2.333878          2.333878     -2.561651      2.561651
# 5             0  2.382622          2.382622     -2.615152      2.615152
# 6             0  2.429426          2.429426     -2.666523      2.666523
# 7             0  2.474418          2.474418     -2.715907      2.715907
# 8             0  2.517715          2.517715     -2.763429      2.763429
# 9             0  2.559421          2.559421     -2.809205      2.809205
# 10            0  2.599628          2.599628     -2.853337      2.853337
# 11            0  2.638423          2.638423     -2.895918      2.895918
# 12            0  2.675883          2.675883     -2.937033      2.937033
# 13            0  2.712079          2.712079     -2.976761      2.976761
# 14            0  2.747075          2.747075     -3.015173      3.015173
# 15            0  2.780932          2.780932     -3.052334      3.052334
#
tail(fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_080_pred, 15)
# meanForecast meanError standardDeviation lowerInterval upperInterval
# 137            0  4.000908          4.000908     -4.391373      4.391373
# 138            0  4.002771          4.002771     -4.393418      4.393418
# 139            0  4.004595          4.004595     -4.395420      4.395420
# 140            0  4.006381          4.006381     -4.397380      4.397380
# 141            0  4.008130          4.008130     -4.399300      4.399300
# 142            0  4.009843          4.009843     -4.401180      4.401180
# 143            0  4.011520          4.011520     -4.403021      4.403021
# 144            0  4.013162          4.013162     -4.404823      4.404823
# 145            0  4.014771          4.014771     -4.406588      4.406588
# 146            0  4.016345          4.016345     -4.408317      4.408317
# 147            0  4.017888          4.017888     -4.410010      4.410010
# 148            0  4.019398          4.019398     -4.411667      4.411667
# 149            0  4.020877          4.020877     -4.413290      4.413290
# 150            0  4.022325          4.022325     -4.414880      4.414880
# 151            0  4.023743          4.023743     -4.416437      4.416437
#
# Consequently we have
#
lowerInterval_cmp <- quants_080[1]*fGARCH_1_1_ged_shpN_lbfgsb_nm_pred_std_dev
head(lowerInterval_cmp,20)
# -2.386659 -2.447598 -2.505860 -2.561651 -2.615152 -2.666523 -2.715907 -2.763429 -2.809205 -2.853337 -2.895918 -2.937033 -2.976761 
# -3.015173 -3.052334 -3.088305 -3.123142 -3.156897 -3.189618 -3.221351
#
identical(round(lowerInterval_cmp,12),round(fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_080_pred[["lowerInterval"]],12))
# TRUE
#
upperInterval_cmp <- quants_080[2]*fGARCH_1_1_ged_shpN_lbfgsb_nm_pred_std_dev
head(upperInterval_cmp,20)
# 2.386659 2.447598 2.505860 2.561651 2.615152 2.666523 2.715907 2.763429 2.809205 2.853337 2.895918 2.937033 2.976761 3.015173 
# 3.052334 3.088305 3.123142 3.156897 3.189618 3.221351
#
identical(round(upperInterval_cmp,12),round(fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_080_pred[["upperInterval"]],12))
# TRUE
#
# Similarly we can build the $85\%$ and $90\%$ prediction bands
fGARCH_1_1_ged_shpN_lbfgsb_nm_uncond_085_pred <- fGarch::predict(fGARCH_1_1_ged_shpN_lbfgsb_nm, n.ahead=TstS_length, nx=TrnS_length, 
                                                                 mse="uncond", conf=0.85, trace=FALSE, plot=TRUE)
head(fGARCH_1_1_ged_shpN_lbfgsb_nm_uncond_085_pred)
# meanForecast meanError standardDeviation lowerInterval upperInterval
# 1            0  3.743863          2.174446     -4.895279      4.895279
# 2            0  3.743863          2.229967     -4.895279      4.895279
# 3            0  3.743863          2.283048     -4.895279      4.895279
# 4            0  3.743863          2.333878     -4.895279      4.895279
# 5            0  3.743863          2.382622     -4.895279      4.895279
# 6            0  3.743863          2.429426     -4.895279      4.895279
#
tail(fGARCH_1_1_ged_shpN_lbfgsb_nm_uncond_085_pred)
# meanForecast meanError standardDeviation lowerInterval upperInterval
# 146            0  3.743863          4.016345     -4.895279      4.895279
# 147            0  3.743863          4.017888     -4.895279      4.895279
# 148            0  3.743863          4.019398     -4.895279      4.895279
# 149            0  3.743863          4.020877     -4.895279      4.895279
# 150            0  3.743863          4.022325     -4.895279      4.895279
# 151            0  3.743863          4.023743     -4.895279      4.895279
#
fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_085_pred <- fGarch::predict(fGARCH_1_1_ged_shpN_lbfgsb_nm, n.ahead=TstS_length, nx=TrnS_length, 
                                                               mse="cond", conf=0.85, trace=FALSE, plot=TRUE)
head(fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_085_pred)
# meanForecast meanError standardDeviation lowerInterval upperInterval
# 1            0  2.174446          2.174446     -2.843191      2.843191
# 2            0  2.229967          2.229967     -2.915787      2.915787
# 3            0  2.283048          2.283048     -2.985193      2.985193
# 4            0  2.333878          2.333878     -3.051656      3.051656
# 5            0  2.382622          2.382622     -3.115391      3.115391
# 6            0  2.429426          2.429426     -3.176589      3.176589
#
tail(fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_085_pred)
# meanForecast meanError standardDeviation lowerInterval upperInterval
# 146            0  4.016345          4.016345     -5.251562      5.251562
# 147            0  4.017888          4.017888     -5.253578      5.253578
# 148            0  4.019398          4.019398     -5.255553      5.255553
# 149            0  4.020877          4.020877     -5.257487      5.257487
# 150            0  4.022325          4.022325     -5.259380      5.259380
# 151            0  4.023743          4.023743     -5.261235      5.261235
#
fGARCH_1_1_ged_shpN_lbfgsb_nm_uncond_090_pred <- fGarch::predict(fGARCH_1_1_ged_shpN_lbfgsb_nm, n.ahead=TstS_length, nx=TrnS_length, 
                                                                 mse="uncond", conf=0.90, trace=FALSE, plot=TRUE)
head(fGARCH_1_1_ged_shpN_lbfgsb_nm_uncond_090_pred)
# meanForecast meanError standardDeviation lowerInterval upperInterval
# 1            0  3.743863          2.174446     -6.020597      6.020597
# 2            0  3.743863          2.229967     -6.020597      6.020597
# 3            0  3.743863          2.283048     -6.020597      6.020597
# 4            0  3.743863          2.333878     -6.020597      6.020597
# 5            0  3.743863          2.382622     -6.020597      6.020597
# 6            0  3.743863          2.429426     -6.020597      6.020597
#
tail(fGARCH_1_1_ged_shpN_lbfgsb_nm_uncond_090_pred)
# meanForecast meanError standardDeviation lowerInterval upperInterval
# 146            0  3.743863          4.016345     -6.020597      6.020597
# 147            0  3.743863          4.017888     -6.020597      6.020597
# 148            0  3.743863          4.019398     -6.020597      6.020597
# 149            0  3.743863          4.020877     -6.020597      6.020597
# 150            0  3.743863          4.022325     -6.020597      6.020597
# 151            0  3.743863          4.023743     -6.020597      6.020597
#
fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_090_pred <- fGarch::predict(fGARCH_1_1_ged_shpN_lbfgsb_nm, n.ahead=TstS_length, nx=TrnS_length, 
                                                               mse="cond", conf=0.90, trace=FALSE, plot=TRUE)
head(fGARCH_1_1_ged_shpN_lbfgsb_nm_uncond_090_pred)
# meanForecast meanError standardDeviation lowerInterval upperInterval
# 1            0  3.743863          2.174446     -6.020597      6.020597
# 2            0  3.743863          2.229967     -6.020597      6.020597
# 3            0  3.743863          2.283048     -6.020597      6.020597
# 4            0  3.743863          2.333878     -6.020597      6.020597
# 5            0  3.743863          2.382622     -6.020597      6.020597
# 6            0  3.743863          2.429426     -6.020597      6.020597
#
tail(fGARCH_1_1_ged_shpN_lbfgsb_nm_uncond_090_pred)
# meanForecast meanError standardDeviation lowerInterval upperInterval
# 146            0  3.743863          4.016345     -6.020597      6.020597
# 147            0  3.743863          4.017888     -6.020597      6.020597
# 148            0  3.743863          4.019398     -6.020597      6.020597
# 149            0  3.743863          4.020877     -6.020597      6.020597
# 150            0  3.743863          4.022325     -6.020597      6.020597
# 151            0  3.743863          4.023743     -6.020597      6.020597
#
# To draw some more accurate plot we prepare a suitable data frame.
# We consider
head(BTC_red_df)
nrow(BTC_red_df)
# 1871
head(BTC_train_df)
nrow(BTC_train_df)
# 1720
DS_length <- nrow(BTC_red_df)
TrnS_length <- nrow(BTC_train_df)
TstS_length==DS_length-TrnS_length
# TRUE
# and set
BTC_red_fGARCH_1_1_ged_shpN_lbfgsb_nm_df <- BTC_red_df
head(BTC_red_fGARCH_1_1_ged_shpN_lbfgsb_nm_df)
# Hence we complete the latter with all elements which are necessary to draw our plots.
BTC_red_fGARCH_1_1_ged_shpN_lbfgsb_nm_df <- add_column(BTC_red_fGARCH_1_1_ged_shpN_lbfgsb_nm_df,
                                                       log_retperc_fit_pred=rep(0,DS_length),
                                                       cond_stand_dev=c(BTC_train_df$fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_stand_dev,
                                                                        fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_080_pred$standardDeviation),
                                                       cond_var=c(BTC_train_df$fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_var,
                                                                  (fGARCH_1_1_ged_shpN_lbfgsb_nm_uncond_080_pred$standardDeviation)^2),
                                                       log_retperc_uncond_080_upp_pred_int=c(rep(NA,TrnS_length),
                                                                                             fGARCH_1_1_ged_shpN_lbfgsb_nm_uncond_080_pred$upperInterval),
                                                       log_retperc_uncond_085_upp_pred_int=c(rep(NA,TrnS_length),
                                                                                             fGARCH_1_1_ged_shpN_lbfgsb_nm_uncond_085_pred$upperInterval),
                                                       log_retperc_uncond_090_upp_pred_int=c(rep(NA,TrnS_length),
                                                                                             fGARCH_1_1_ged_shpN_lbfgsb_nm_uncond_090_pred$upperInterval),
                                                       log_retperc_cond_080_upp_pred_int=c(rep(NA,TrnS_length),
                                                                                           fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_080_pred$upperInterval),
                                                       log_retperc_cond_085_upp_pred_int=c(rep(NA,TrnS_length),
                                                                                           fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_085_pred$upperInterval),
                                                       log_retperc_cond_090_upp_pred_int=c(rep(NA,TrnS_length),
                                                                                           fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_090_pred$upperInterval))
head(BTC_red_fGARCH_1_1_ged_shpN_lbfgsb_nm_df)
tail(BTC_red_fGARCH_1_1_ged_shpN_lbfgsb_nm_df)
#
# Line plot of the Bitcoin daily logarithm return percentage.
Data_df <- BTC_red_fGARCH_1_1_ged_shpN_lbfgsb_nm_df
head(Data_df)
tail(Data_df)
Data_df <- dplyr::rename(Data_df, x=t, y=log_ret_perc, z=cond_stand_dev)
head(Data_df)
tail(Data_df)
DS_length <- length(Data_df$y)
show(DS_length)
# 1871
First_Day <- as.character(Data_df$Date[1])
show(First_Day)
# "2018-04-17"
Last_Day <- as.character(Data_df$Date[DS_length])
show(Last_Day)
# "2023-05-31"
TrnS_Last_Day <- as.character(Data_df$Date[which(Data_df$Date==as.Date("2023-01-01"))-1])
show(TrnS_Last_Day)
# "2022-12-31"
TrnS_length <- length(Data_df$Date[which(Data_df$Date<as.Date("2023-01-01"))])
show(TrnS_length)
# 1720
TstS_First_Day <- as.character(Data_df$Date[which(Data_df$Date==as.Date("2023-01-01"))])
show(TstS_First_Day)
# "2023-01-01"
TstS_length <- length(Data_df$Date[which(Data_df$Date>=as.Date("2023-01-01"))])
show(TstS_length)
# 151
TstS_length == DS_length-TrnS_length
# TRUE
First_Day <- as.character(Data_df$Date[2])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Bitcoin Daily Logarithm Return Percentage, Conditional Standard Deviation, and Conditional Prediction Bands of the fGarch::garchFit() Fitted GARCH(1,1) Model with GED Innovation - from ", .(First_Day), " to ", .(Last_Day), sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points, from ", .(First_Day), " to ", .(TrnS_Last_Day),". Test set length ", .(TstS_length), " sample points, from ", .(TstS_First_Day), " to ", .(Last_Day),". Data by courtesy of Yahoo Finance - ", .(link)))
caption_content <- "Author: Roberto Monte"
# x_name <- bquote("dates")
# numbers::primeFactors(DS_length-1)
x_breaks_num <- 55
x_breaks_low <- Data_df$x[1]
x_breaks_up <- Data_df$x[DS_length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- Data_df$Date[x_breaks]
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Daily Logarithm Return Percentage ")
y_breaks_num <- 10
y_max <- max(na.rm(Data_df$y),na.rm(Data_df$log_retperc_uncond_090_upp_pred_int))
y_min <- min(na.rm(Data_df$y),na.rm(Data_df$log_retperc_uncond_090_low_pred_int))
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor(y_min/y_binwidth)*y_binwidth
y_breaks_up <- ceiling(y_max/y_binwidth)*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
line_col_grey <- bquote("daily perc. log. ret. - training set")
line_col_black <- bquote("daily perc. log. ret. - test set")
line_col_magenta <- bquote("cond. stand. dev. - GED inn.")
line_col_green <- bquote("80% pred. band endpoints")
line_col_blue <- bquote("85% pred. band endpoints")
line_col_red <- bquote("90% pred. band endpoints")
leg_line_labs   <- c(line_col_grey, line_col_black, line_col_magenta, line_col_green, line_col_blue, line_col_red)
leg_line_breaks <- c("line_col_grey", "line_col_black", "line_col_magenta", "line_col_green", "line_col_blue", "line_col_red")
leg_line_cols   <- c("line_col_grey"="grey50", "line_col_black"="black", "line_col_magenta"="magenta", 
                     "line_col_green"="green", "line_col_blue"="blue", "line_col_red"="red")
fill_col_lightgreen <- bquote("80% pred. band")
fill_col_cyan <- bquote("85% pred. band")
fill_col_orangered <- bquote("90% pred. band")
leg_fill_labs <- c(fill_col_lightgreen, fill_col_cyan, fill_col_orangered)
leg_fill_breaks <- c("fill_col_lightgreen", "fill_col_cyan", "fill_col_orangered")
leg_fill_cols <- c("fill_col_lightgreen"="lightgreen", "fill_col_cyan"="cyan", "fill_col_orangered"="orangered")
Data_df_lp <- ggplot(Data_df, aes(x=x)) + 
  geom_ribbon(data=subset(Data_df, x >= x[TrnS_length+1]), aes(ymin=-log_retperc_cond_080_upp_pred_int, ymax=log_retperc_cond_080_upp_pred_int, fill= "fill_col_lightgreen"), 
              alpha=0.3, colour= "lightgreen") +
  geom_ribbon(data=subset(Data_df, x >= x[TrnS_length+1]), aes(ymin=-log_retperc_cond_085_upp_pred_int, ymax=-log_retperc_cond_080_upp_pred_int, fill= "fill_col_cyan"),
              alpha=0.3, colour= "cyan") +
  geom_ribbon(data=subset(Data_df, x >= x[TrnS_length+1]), aes(ymin=-log_retperc_cond_090_upp_pred_int, ymax=-log_retperc_cond_085_upp_pred_int, fill= "fill_col_orangered"), 
              alpha=0.3, colour= "orangered") +
  geom_ribbon(data=subset(Data_df, x >= x[TrnS_length+1]), aes(ymin=log_retperc_cond_080_upp_pred_int, ymax=log_retperc_cond_085_upp_pred_int, fill= "fill_col_cyan"), 
              alpha=0.3, colour= "cyan") +
  geom_ribbon(data=subset(Data_df, x >= x[TrnS_length+1]), aes(ymin=log_retperc_cond_085_upp_pred_int, ymax=log_retperc_cond_090_upp_pred_int, fill= "fill_col_orangered"), 
              alpha=0.3, colour= "orangered") +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=-log_retperc_cond_090_upp_pred_int, colour= "line_col_red"), 
            linetype= "solid", alpha=1, linewidth=1) +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=log_retperc_cond_090_upp_pred_int, colour= "line_col_red"), 
            linetype= "solid", alpha=1, linewidth=1) +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=-log_retperc_cond_085_upp_pred_int, colour= "line_col_blue"), 
            linetype= "solid", alpha=1, linewidth=1) +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=log_retperc_cond_085_upp_pred_int, colour= "line_col_blue"), 
            linetype= "solid", alpha=1, linewidth=1) +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=-log_retperc_cond_080_upp_pred_int, colour= "line_col_green"),
            linetype= "solid", alpha=1, linewidth=1) +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=log_retperc_cond_080_upp_pred_int, colour= "line_col_green"), 
            linetype= "solid", alpha=1, linewidth=1) +
  geom_line(data=subset(Data_df, x <= x[TrnS_length]), aes(y=y, color= "line_col_grey"),
            linetype= "solid", alpha=1, linewidth=0.7, group=1, na.rm=TRUE) +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=y, colour= "line_col_black"), 
            linetype= "solid", alpha=1, linewidth=0.7, group=1, na.rm=TRUE) +
  geom_line(aes(y=z, color="line_col_magenta"), linetype="solid", alpha=1, linewidth=0.7, group=1, na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, labels=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis= sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  guides(linetype= "none", shape= "none") +
  scale_colour_manual(name= "", labels=leg_line_labs, values=leg_line_cols, breaks=leg_line_breaks) +
  scale_fill_manual(name= "", labels=leg_fill_labs, values=leg_fill_cols, breaks=leg_fill_breaks) +
  guides(colour=guide_legend(order=1), fill=guide_legend(order=2)) +
  theme(plot.title=element_text(hjust=0.5, size=12), 
        plot.subtitle=element_text(hjust=0.5, size=10),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(Data_df_lp)
#
# Detail of the line plot of the Bitcoin daily logarithm return percentage.
Data_df <- BTC_red_fGARCH_1_1_ged_shpN_lbfgsb_nm_df
head(Data_df)
tail(Data_df)
Data_df <- dplyr::rename(Data_df, x=t, y=log_ret_perc, z=cond_stand_dev)
head(Data_df)
tail(Data_df)
DS_length <- length(Data_df$y)
show(DS_length)
# 1871
First_Day <- as.character(Data_df$Date[1])
show(First_Day)
# "2018-04-17"
Last_Day <- as.character(Data_df$Date[DS_length])
show(Last_Day)
# "2023-05-31"
TrnS_Last_Day <- as.character(Data_df$Date[which(Data_df$Date==as.Date("2023-01-01"))-1])
show(TrnS_Last_Day)
# "2022-12-31"
TrnS_length <- length(Data_df$Date[which(Data_df$Date<as.Date("2023-01-01"))])
show(TrnS_length)
# 1720
TstS_First_Day <- as.character(Data_df$Date[which(Data_df$Date==as.Date("2023-01-01"))])
show(TstS_First_Day)
# "2023-01-01"
TstS_length <- length(Data_df$Date[which(Data_df$Date>=as.Date("2023-01-01"))])
show(TstS_length)
# 151
TstS_length == DS_length-TrnS_length
# TRUE
First_Day <- as.character(Data_df$Date[2])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
Det_Day <- as.character(Data_df$Date[which(Data_df$Date=="2021-01-01")])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Detail of Bitcoin Daily Logarithm Return Percentage, Conditional Standard Deviation, and Conditional Prediction Bands of the fGarch::garchFit() Fitted GARCH(1,1) Model with GED Innovation - from ", .(First_Day), " to ", .(Last_Day), ", detail from ", .(Det_Day), sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points, from ", .(First_Day), " to ", .(TrnS_Last_Day),". Test set length ", .(TstS_length), " sample points, from ", .(TstS_First_Day), " to ", .(Last_Day),". Data by courtesy of Yahoo Finance - ", .(link)))
caption_content <- "Author: Roberto Monte"
# x_name <- bquote("dates")
# numbers::primeFactors(which(Data_df$Date==Last_Day)-which(Data_df$Date==Det_Day)-2)
x_breaks_num <- 55
x_breaks_low <- Data_df$x[which(Data_df$Date==Det_Day)]
x_breaks_up <- Data_df$x[DS_length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- Data_df$Date[x_breaks]
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Daily Logarithm Return Percentage ")
y_breaks_num <- 10
y_max <- max(na.rm(Data_df$y[c(which(Data_df$Date==Det_Day):which(Data_df$Date==Last_Day))]),
             na.rm(Data_df$log_retperc_cond_090_upp_pred_int[c(which(Data_df$Date==Det_Day):which(Data_df$Date==Last_Day))]))
y_min <- min(na.rm(Data_df$y[c(which(Data_df$Date==Det_Day):which(Data_df$Date==Last_Day))]),
             na.rm(-Data_df$log_retperc_cond_090_upp_pred_int[c(which(Data_df$Date==Det_Day):which(Data_df$Date==Last_Day))]))
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor(y_min/y_binwidth)*y_binwidth
y_breaks_up <- ceiling(y_max/y_binwidth)*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.0
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
line_col_grey <- bquote("daily perc. log. ret. - training set")
line_col_black <- bquote("daily perc. log. ret. - test set")
line_col_magenta <- bquote("cond. stand. dev. - GED inn.")
line_col_green <- bquote("80% pred. band endpoints")
line_col_blue <- bquote("85% pred. band endpoints")
line_col_red <- bquote("90% pred. band endpoints")
leg_line_labs   <- c(line_col_grey, line_col_black, line_col_magenta, line_col_green, line_col_blue, line_col_red)
leg_line_breaks <- c("line_col_grey", "line_col_black", "line_col_magenta", "line_col_green", "line_col_blue", "line_col_red")
leg_line_cols   <- c("line_col_grey"="grey50", "line_col_black"="black", "line_col_magenta"="magenta", 
                     "line_col_green"="green", "line_col_blue"="blue", "line_col_red"="red")
fill_col_lightgreen <- bquote("80% pred. band")
fill_col_cyan <- bquote("85% pred. band")
fill_col_orangered <- bquote("90% pred. band")
leg_fill_labs <- c(fill_col_lightgreen, fill_col_cyan, fill_col_orangered)
leg_fill_breaks <- c("fill_col_lightgreen", "fill_col_cyan", "fill_col_orangered")
leg_fill_cols <- c("fill_col_lightgreen"="lightgreen", "fill_col_cyan"="cyan", "fill_col_orangered"="orangered")
Data_df_lp <- ggplot(Data_df, aes(x=x)) + 
  geom_ribbon(data=subset(Data_df, x >= x[TrnS_length+1]), aes(ymin=-log_retperc_cond_080_upp_pred_int, ymax=log_retperc_cond_080_upp_pred_int, fill= "fill_col_lightgreen"), 
              alpha=0.3, colour= "lightgreen") +
  geom_ribbon(data=subset(Data_df, x >= x[TrnS_length+1]), aes(ymin=-log_retperc_cond_085_upp_pred_int, ymax=-log_retperc_cond_080_upp_pred_int, fill= "fill_col_cyan"),
              alpha=0.3, colour= "cyan") +
  geom_ribbon(data=subset(Data_df, x >= x[TrnS_length+1]), aes(ymin=-log_retperc_cond_090_upp_pred_int, ymax=-log_retperc_cond_085_upp_pred_int, fill= "fill_col_orangered"), 
              alpha=0.3, colour= "orangered") +
  geom_ribbon(data=subset(Data_df, x >= x[TrnS_length+1]), aes(ymin=log_retperc_cond_080_upp_pred_int, ymax=log_retperc_cond_085_upp_pred_int, fill= "fill_col_cyan"), 
              alpha=0.3, colour= "cyan") +
  geom_ribbon(data=subset(Data_df, x >= x[TrnS_length+1]), aes(ymin=log_retperc_cond_085_upp_pred_int, ymax=log_retperc_cond_090_upp_pred_int, fill= "fill_col_orangered"), 
              alpha=0.3, colour= "orangered") +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=-log_retperc_cond_090_upp_pred_int, colour= "line_col_red"), 
            linetype= "solid", alpha=1, linewidth=1) +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=log_retperc_cond_090_upp_pred_int, colour= "line_col_red"), 
            linetype= "solid", alpha=1, linewidth=1) +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=-log_retperc_cond_085_upp_pred_int, colour= "line_col_blue"), 
            linetype= "solid", alpha=1, linewidth=1) +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=log_retperc_cond_085_upp_pred_int, colour= "line_col_blue"), 
            linetype= "solid", alpha=1, linewidth=1) +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=-log_retperc_cond_080_upp_pred_int, colour= "line_col_green"),
            linetype= "solid", alpha=1, linewidth=1) +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=log_retperc_cond_080_upp_pred_int, colour= "line_col_green"), 
            linetype= "solid", alpha=1, linewidth=1) +
  geom_line(data=subset(Data_df, (x >= x[which(Data_df$Date==Det_Day)] & x <= x[TrnS_length])), aes(y=y, color= "line_col_grey"),
            linetype= "solid", alpha=1, linewidth=0.7, group=1, na.rm=TRUE) +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=y, colour= "line_col_black"), 
            linetype= "solid", alpha=1, linewidth=0.7, group=1, na.rm=TRUE) +
  geom_line(data=subset(Data_df, x >= x[which(Data_df$Date==Det_Day)]), aes(y=z, color="line_col_magenta"), 
            linetype="solid", alpha=1, linewidth=0.7, group=1, na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, labels=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis= sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  guides(linetype= "none", shape= "none") +
  scale_colour_manual(name= "", labels=leg_line_labs, values=leg_line_cols, breaks=leg_line_breaks) +
  scale_fill_manual(name= "", labels=leg_fill_labs, values=leg_fill_cols, breaks=leg_fill_breaks) +
  guides(colour=guide_legend(order=1), fill=guide_legend(order=2)) +
  theme(plot.title=element_text(hjust=0.5, size=12), 
        plot.subtitle=element_text(hjust=0.5, size=10),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(Data_df_lp)
#
# From the forecasting plot of the logarithm return percentage, it is not difficult to obtain a forecasting plot of the logarithm adjusted 
# close price.
# First, we go back from the logarithm return percentage to the logarithm return and we we build the confidence bands of the latter by the
# multiplication of the confidence bands endpoints of the former times the factor 1/100.
# Second we build the predicted values of the test set by observing that the predicted values of the logarithm returns are all zero.
# Thus, if we denote by $y_{T+t}$ and $\hat{y}_{T+t,T}$ [resp. $z_{T+t}$ and $\hat{z}_{T+t,T}$], $\forall t\geq 0$ the adjusted close price
# logarithm and predicted adjusted close price logarithm [resp. logarithm return and predicted logarithm return] at time $T+t$ given the 
# information up to the end time $T$ of the training set, thanks to the linearity of the conditional expectation operator, from the equation
# $z_{T+t}=y_{T+t}-y_{T+t-1} \forall t=1,\dots,U$ where $U$, is the end time of the test set$,
# we obtain
# $\hat{z}_{T+t,T}=\hat{y}_{T+t,T}-\hat{y}_{T+t-1,T}$.
# On the other hand,
# $\hat{z}_{T+t,T}=0, \forall t=1,\dots,U$ where $U$.
# It follows
# $\hat{y}_{T+1,T}=\hat{y}_{T,T}=y_{T}$,
# $\hat{y}_{T+2,T}=\hat{y}_{T+1,T}=y_{T}$,
# $\dots$,
# $\hat{y}_{T+t,T}=y_{T}, \forall t=1,\dots,U$.
# In addition, from
# $low_{T+t,T} < \hat{z}_{T+t,T} < upp_{T+t,T}, \forall t=1,\dots,U$, where $low_{T+t,T}$ and $upp_{T+t,T}$ are the lower and upper 
# endpoints of any confidence band at time $T+t$ given the information up to the end time $T$ of the training set, we obtain
# $low_{T+1,T} < \hat{y}_{T+1,T}-\hat{y}_{T,T} < upp_{T+1,T}$.
# Hence,
# low_{T+1,T} < \hat{y}_{T+1,T} - y_{T} < upp_{T+1,T}$,
# that is
# y_{T} + low_{T+1,T} < \hat{y}_{T+1,T} <  y_{T} + upp_{T+1,T}$.
# Similarly,
# $low_{T+2,T} < \hat{y}_{T+2,T}-\hat{y}_{T+1,T} < upp_{T+2,T}$,
# from which
# $low_{T+2,T} < \hat{y}_{T+2,T}-{y}_{T} < upp_{T+2,T}$,
# and
# ${y}_{T} + low_{T+2,T} < \hat{y}_{T+2,T} < {y}_{T} + upp_{T+2,T}$.
# In the end, we can write
# ${y}_{T} + low_{T+t,T} < \hat{y}_{T+t,T} < {y}_{T} + upp_{T+t,T}, \forall t=1,\dots,U$.
# This gives the prediction bands for the predicted values of the adjusted price logarithm.
# We plot the detail of the predicted values and prediction bands for the Bitcoin adjusted close price logarithm
Data_df <- BTC_red_fGARCH_1_1_ged_shpN_lbfgsb_nm_df
head(Data_df)
tail(Data_df)
Data_df <- add_column(Data_df, 
                      Pred_Adj.Close_log=c(rep(NA,TrnS_length),rep(Data_df$Adj.Close_log[TrnS_length],TstS_length)),
                      adj.close_log_uncond_080_upp_pred_int=c(rep(NA,TrnS_length),rep(Data_df$Adj.Close_log[TrnS_length],TstS_length)+(Data_df$log_retperc_uncond_080_upp_pred_int[c((TrnS_length+1):DS_length)]/100)),
                      adj.close_log_uncond_085_upp_pred_int=c(rep(NA,TrnS_length),rep(Data_df$Adj.Close_log[TrnS_length],TstS_length)+(Data_df$log_retperc_uncond_085_upp_pred_int[c((TrnS_length+1):DS_length)]/100)),
                      adj.close_log_uncond_090_upp_pred_int=c(rep(NA,TrnS_length),rep(Data_df$Adj.Close_log[TrnS_length],TstS_length)+(Data_df$log_retperc_uncond_090_upp_pred_int[c((TrnS_length+1):DS_length)]/100)),
                      adj.close_log_uncond_080_low_pred_int=c(rep(NA,TrnS_length),rep(Data_df$Adj.Close_log[TrnS_length],TstS_length)-(Data_df$log_retperc_uncond_080_upp_pred_int[c((TrnS_length+1):DS_length)]/100)),
                      adj.close_log_uncond_085_low_pred_int=c(rep(NA,TrnS_length),rep(Data_df$Adj.Close_log[TrnS_length],TstS_length)-(Data_df$log_retperc_uncond_085_upp_pred_int[c((TrnS_length+1):DS_length)]/100)),
                      adj.close_log_uncond_090_low_pred_int=c(rep(NA,TrnS_length),rep(Data_df$Adj.Close_log[TrnS_length],TstS_length)-(Data_df$log_retperc_uncond_090_upp_pred_int[c((TrnS_length+1):DS_length)]/100)),
                      adj.close_log_cond_080_upp_pred_int=c(rep(NA,TrnS_length),rep(Data_df$Adj.Close_log[TrnS_length],TstS_length)+(Data_df$log_retperc_cond_080_upp_pred_int[c((TrnS_length+1):DS_length)]/100)),
                      adj.close_log_cond_085_upp_pred_int=c(rep(NA,TrnS_length),rep(Data_df$Adj.Close_log[TrnS_length],TstS_length)+(Data_df$log_retperc_cond_085_upp_pred_int[c((TrnS_length+1):DS_length)]/100)),
                      adj.close_log_cond_090_upp_pred_int=c(rep(NA,TrnS_length),rep(Data_df$Adj.Close_log[TrnS_length],TstS_length)+(Data_df$log_retperc_cond_090_upp_pred_int[c((TrnS_length+1):DS_length)]/100)),
                      adj.close_log_cond_080_low_pred_int=c(rep(NA,TrnS_length),rep(Data_df$Adj.Close_log[TrnS_length],TstS_length)-(Data_df$log_retperc_cond_080_upp_pred_int[c((TrnS_length+1):DS_length)]/100)),
                      adj.close_log_cond_085_low_pred_int=c(rep(NA,TrnS_length),rep(Data_df$Adj.Close_log[TrnS_length],TstS_length)-(Data_df$log_retperc_cond_085_upp_pred_int[c((TrnS_length+1):DS_length)]/100)),
                      adj.close_log_cond_090_low_pred_int=c(rep(NA,TrnS_length),rep(Data_df$Adj.Close_log[TrnS_length],TstS_length)-(Data_df$log_retperc_cond_090_upp_pred_int[c((TrnS_length+1):DS_length)]/100)),
                      .after="Adj.Close_log")
head(Data_df)
tail(Data_df)
Data_df <- dplyr::rename(Data_df, x=t, y=Adj.Close_log, z=Pred_Adj.Close_log)
head(Data_df)
tail(Data_df)
DS_length <- length(Data_df$y)
show(DS_length)
# 1871
First_Day <- as.character(Data_df$Date[1])
show(First_Day)
# "2018-04-17"
Last_Day <- as.character(Data_df$Date[DS_length])
show(Last_Day)
# "2023-05-31"
TrnS_Last_Day <- as.character(Data_df$Date[which(Data_df$Date==as.Date("2023-01-01"))-1])
show(TrnS_Last_Day)
# "2022-12-31"
TrnS_length <- length(Data_df$Date[which(Data_df$Date<as.Date("2023-01-01"))])
show(TrnS_length)
# 1720
TstS_First_Day <- as.character(Data_df$Date[which(Data_df$Date==as.Date("2023-01-01"))])
show(TstS_First_Day)
# "2023-01-01"
TstS_length <- length(Data_df$Date[which(Data_df$Date>=as.Date("2023-01-01"))])
show(TstS_length)
# 151
TstS_length == DS_length-TrnS_length
# TRUE
First_Day <- as.character(Data_df$Date[2])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
Det_Day <- as.character(Data_df$Date[which(Data_df$Date=="2022-08-01")])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Detail of Bitcoin Daily Adjusted Close Price Logarithm, Predicted Values, and Prediction Bands of the fGarch::garchFit() Fitted GARCH(1,1) Model with GED Innovation - from ", .(First_Day), " to ", .(Last_Day), ", detail from ", .(Det_Day), sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points, from ", .(First_Day), " to ", .(TrnS_Last_Day),". Test set length ", .(TstS_length), " sample points, from ", .(TstS_First_Day), " to ", .(Last_Day),". Data by courtesy of Yahoo Finance - ", .(link)))
caption_content <- "Author: Roberto Monte"
# x_name <- bquote("dates")
# numbers::primeFactors(which(Data_df$Date==Last_Day)-which(Data_df$Date==Det_Day)-2)
x_breaks_num <- 27
x_breaks_low <- Data_df$x[which(Data_df$Date==Det_Day)]
x_breaks_up <- Data_df$x[DS_length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- Data_df$Date[x_breaks]
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Daily Adjusted Close Logarithm")
y_breaks_num <- 10
y_max <- max(na.rm(Data_df$y[c(which(Data_df$Date==Det_Day):which(Data_df$Date==Last_Day))]),
             na.rm(Data_df$adj.close_log_cond_090_upp_pred_int[c(which(Data_df$Date==Det_Day):which(Data_df$Date==Last_Day))]))
             
y_min <- min(na.rm(Data_df$y[c(which(Data_df$Date==Det_Day):which(Data_df$Date==Last_Day))]),
             na.rm(Data_df$adj.close_log_cond_090_low_pred_int[c(which(Data_df$Date==Det_Day):which(Data_df$Date==Last_Day))]))
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor(y_min/y_binwidth)*y_binwidth
y_breaks_up <- ceiling(y_max/y_binwidth)*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.0
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
line_col_grey <- bquote("adj. close log. - training set")
line_col_black <- bquote("adj. close log. - test set")
line_col_magenta <- bquote("adj. close log. - predicted test set")
line_col_green <- bquote("80% pred. band endpoints")
line_col_blue <- bquote("85% pred. band endpoints")
line_col_red <- bquote("90% pred. band endpoints")
leg_line_labs   <- c(line_col_grey, line_col_black, line_col_magenta, line_col_green, line_col_blue, line_col_red)
leg_line_breaks <- c("line_col_grey", "line_col_black", "line_col_magenta", "line_col_green", "line_col_blue", "line_col_red")
leg_line_cols   <- c("line_col_grey"="grey50", "line_col_black"="black", "line_col_magenta"="magenta", "line_col_green"="green", 
                     "line_col_blue"="blue", "line_col_red"="red")
fill_col_lightgreen <- bquote("80% pred. band")
fill_col_cyan <- bquote("85% pred. band")
fill_col_orangered <- bquote("90% pred. band")
leg_fill_labs <- c(fill_col_lightgreen, fill_col_cyan, fill_col_orangered)
leg_fill_breaks <- c("fill_col_lightgreen", "fill_col_cyan", "fill_col_orangered")
leg_fill_cols <- c("fill_col_lightgreen"="lightgreen", "fill_col_cyan"="cyan", "fill_col_orangered"="orangered")
Data_df_lp <- ggplot(Data_df, aes(x=x)) + 
  geom_ribbon(data=subset(Data_df, x >= x[TrnS_length+1]), aes(ymin=adj.close_log_cond_080_low_pred_int, ymax=adj.close_log_cond_080_upp_pred_int, fill= "fill_col_lightgreen"),
              alpha=0.3, colour= "lightgreen") +
  geom_ribbon(data=subset(Data_df, x >= x[TrnS_length+1]), aes(ymin=adj.close_log_cond_085_low_pred_int, ymax=adj.close_log_cond_080_low_pred_int, fill= "fill_col_cyan"),
              alpha=0.3, colour= "cyan") +
  geom_ribbon(data=subset(Data_df, x >= x[TrnS_length+1]), aes(ymin=adj.close_log_cond_090_low_pred_int, ymax=adj.close_log_cond_085_low_pred_int, fill= "fill_col_orangered"),
              alpha=0.3, colour= "orangered") +
  geom_ribbon(data=subset(Data_df, x >= x[TrnS_length+1]), aes(ymin=adj.close_log_cond_080_upp_pred_int, ymax=adj.close_log_cond_085_upp_pred_int, fill= "fill_col_cyan"),
              alpha=0.3, colour= "cyan") +
  geom_ribbon(data=subset(Data_df, x >= x[TrnS_length+1]), aes(ymin=adj.close_log_cond_085_upp_pred_int, ymax=adj.close_log_cond_090_upp_pred_int, fill= "fill_col_orangered"),
              alpha=0.3, colour= "orangered") +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=adj.close_log_cond_090_upp_pred_int, colour= "line_col_red"),
            linetype= "solid", alpha=1, linewidth=1) +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=adj.close_log_cond_090_low_pred_int, colour= "line_col_red"),
            linetype= "solid", alpha=1, linewidth=1) +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=adj.close_log_cond_085_upp_pred_int, colour= "line_col_blue"),
            linetype= "solid", alpha=1, linewidth=1) +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=adj.close_log_cond_085_low_pred_int, colour= "line_col_blue"),
            linetype= "solid", alpha=1, linewidth=1) +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=adj.close_log_cond_080_upp_pred_int, colour= "line_col_green"),
            linetype= "solid", alpha=1, linewidth=1) +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=adj.close_log_cond_080_low_pred_int, colour= "line_col_green"),
            linetype= "solid", alpha=1, linewidth=1) +
  geom_line(data=subset(Data_df, (x >= x[which(Data_df$Date==Det_Day)] & x <= x[TrnS_length])), aes(y=y, color= "line_col_grey"),
            linetype= "solid", alpha=1, linewidth=0.7, group=1, na.rm=TRUE) +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=y, color= "line_col_black"), 
            linetype= "solid", alpha=1, linewidth=0.7, group=1, na.rm=TRUE) +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=z, color="line_col_magenta"),
            linetype="solid", alpha=1, linewidth=0.7, group=1, na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, labels=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis= sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  guides(linetype= "none", shape= "none") +
  scale_colour_manual(name= "", labels=leg_line_labs, values=leg_line_cols, breaks=leg_line_breaks) +
  scale_fill_manual(name= "", labels=leg_fill_labs, values=leg_fill_cols, breaks=leg_fill_breaks) +
  guides(colour=guide_legend(order=1), fill=guide_legend(order=2)) +
  theme(plot.title=element_text(hjust=0.5, size=12), 
        plot.subtitle=element_text(hjust=0.5, size=10),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(Data_df_lp)
#
# The forecasting performance may appear very poor. However, it must be considered that in this forecast we only use the information
# transmitted by the training set and that the logarithm of the adjusted close price is a non-stationary process. Therefore, in the long 
# run, constant forecasting is bound to fail. In contrast, if we allow a daily update of the forecasting by a daily increasing of the
# training set the forecasting performance improves significantly.
# We denote by $y_{T+t+1}$ and $\hat{y}_{T+t+1,T+t}$ [resp. $z_{T+t+1}$ and $\hat{z}_{T+t+1,T+t}$], $\forall t\geq 0$ the adjusted close 
# price logarithm and predicted adjusted close price logarithm [resp. logarithm return and predicted logarithm return] at time $T+t+1$ given
# the information up to time $T+t$, where $T$ is the end time of the training set, thanks to the linearity of the conditional expectation 
# operator, from the equation
# $z_{T+t+1}=y_{T+t+1}-y_{T+t} \forall t=0,\dots,U-1$ where $U$, is the end time of the test set$,
# we obtain
# $\hat{z}_{T+t+1,T+t}=\hat{y}_{T+t+1,T+t}-\hat{y}_{T+t,T+t}$.
# On the other hand,
# $\hat{z}_{T+t+1,T+t}=0, \forall t=0,\dots,U-1$.
# It follows
# $\hat{y}_{T+1,T}=\hat{y}_{T,T}=y_{T}$,
# $\hat{y}_{T+2,T+1}=\hat{y}_{T+1,T+1}=y_{T+1}$,
# $\dots$,
# $\hat{y}_{T+t+1,T+1}=y_{T+t}, \forall t=0,\dots,U-1$ where $U$.
# In addition, from
# $low_{T+t+1,T+t} < \hat{z}_{T+t+1,T+t} < upp_{T+t+1,T+t}, \forall t=0,\dots,U-1$, where $low_{T+t+1,T+t}$ and $upp_{T+t+1,T+t}$ are the 
# lower and upper endpoints of any confidence band at time $T+t+1$ given the information up to the end time $T+t$ of daily increasing 
# training set, we obtain
# $low_{T+1,T} < \hat{y}_{T+1,T}-\hat{y}_{T,T} < upp_{T+1,T}$.
# Hence,
# low_{T+1,T} < \hat{y}_{T+1,T} - y_{T} < upp_{T+1,T}$,
# that is
# y_{T} + low_{T+1,T} < \hat{y}_{T+1,T} <  y_{T} + upp_{T+1,T}$.
# Similarly,
# $low_{T+2,T+1} < \hat{y}_{T+2,T+1}-\hat{y}_{T+1,T+1} < upp_{T+2,T+1}$,
# from which
# $low_{T+2,T+1} < \hat{y}_{T+2,T+1}-{y}_{T+1} < upp_{T+2,T+1}$,
# and
# ${y}_{T+1} + low_{T+2,T+1} < \hat{y}_{T+2,T+1} < {y}_{T+1} + upp_{T+2,T+1}$.
# In the end, we can write
# ${y}_{T+t} + low_{T+t+1,T+t} < \hat{y}_{T+t+1,T+t} < {y}_{T+t} + upp_{T+t+1,T+t}, \forall t=0,\dots,U-1$.
#
# The result of the daily updating forecasting can be appreciated by the following plot.
Data_df <- BTC_red_fGARCH_1_1_ged_shpN_lbfgsb_nm_df
head(Data_df)
tail(Data_df)
Data_df <- add_column(Data_df, 
                      Lagged_Adj.Close_log=c(NA,Data_df$Adj.Close_log[-length(Data_df$Adj.Close_log)]),
                      adj.close_log_uncond_080_upp_pred_int=c(rep(NA,TrnS_length),Data_df$Adj.Close_log[c(TrnS_length:(DS_length-1))]+rep(Data_df$log_retperc_uncond_080_upp_pred_int[(TrnS_length+1)]/100,TstS_length)),
                      adj.close_log_uncond_085_upp_pred_int=c(rep(NA,TrnS_length),Data_df$Adj.Close_log[c(TrnS_length:(DS_length-1))]+rep(Data_df$log_retperc_uncond_085_upp_pred_int[(TrnS_length+1)]/100,TstS_length)),
                      adj.close_log_uncond_090_upp_pred_int=c(rep(NA,TrnS_length),Data_df$Adj.Close_log[c(TrnS_length:(DS_length-1))]+rep(Data_df$log_retperc_uncond_090_upp_pred_int[(TrnS_length+1)]/100,TstS_length)),
                      adj.close_log_uncond_080_low_pred_int=c(rep(NA,TrnS_length),Data_df$Adj.Close_log[c(TrnS_length:(DS_length-1))]-rep(Data_df$log_retperc_uncond_080_upp_pred_int[(TrnS_length+1)]/100,TstS_length)),
                      adj.close_log_uncond_085_low_pred_int=c(rep(NA,TrnS_length),Data_df$Adj.Close_log[c(TrnS_length:(DS_length-1))]-rep(Data_df$log_retperc_uncond_080_upp_pred_int[(TrnS_length+1)]/100,TstS_length)),
                      adj.close_log_uncond_090_low_pred_int=c(rep(NA,TrnS_length),Data_df$Adj.Close_log[c(TrnS_length:(DS_length-1))]-rep(Data_df$log_retperc_uncond_080_upp_pred_int[(TrnS_length+1)]/100,TstS_length)),
                      adj.close_log_cond_080_upp_pred_int=c(rep(NA,TrnS_length),Data_df$Adj.Close_log[c(TrnS_length:(DS_length-1))]+rep(Data_df$log_retperc_cond_080_upp_pred_int[(TrnS_length+1)]/100,TstS_length)),
                      adj.close_log_cond_085_upp_pred_int=c(rep(NA,TrnS_length),Data_df$Adj.Close_log[c(TrnS_length:(DS_length-1))]+rep(Data_df$log_retperc_cond_085_upp_pred_int[(TrnS_length+1)]/100,TstS_length)),
                      adj.close_log_cond_090_upp_pred_int=c(rep(NA,TrnS_length),Data_df$Adj.Close_log[c(TrnS_length:(DS_length-1))]+rep(Data_df$log_retperc_cond_090_upp_pred_int[(TrnS_length+1)]/100,TstS_length)),
                      adj.close_log_cond_080_low_pred_int=c(rep(NA,TrnS_length),Data_df$Adj.Close_log[c(TrnS_length:(DS_length-1))]-rep(Data_df$log_retperc_cond_080_upp_pred_int[(TrnS_length+1)]/100,TstS_length)),
                      adj.close_log_cond_085_low_pred_int=c(rep(NA,TrnS_length),Data_df$Adj.Close_log[c(TrnS_length:(DS_length-1))]-rep(Data_df$log_retperc_cond_085_upp_pred_int[(TrnS_length+1)]/100,TstS_length)),
                      adj.close_log_cond_090_low_pred_int=c(rep(NA,TrnS_length),Data_df$Adj.Close_log[c(TrnS_length:(DS_length-1))]-rep(Data_df$log_retperc_cond_090_upp_pred_int[(TrnS_length+1)]/100,TstS_length)),
                      .after="Adj.Close_log")
head(Data_df)
tail(Data_df)
Data_df <- dplyr::rename(Data_df, x=t, y=Adj.Close_log, z=Lagged_Adj.Close_log)
head(Data_df)
tail(Data_df)
DS_length <- length(Data_df$y)
show(DS_length)
# 1871
First_Day <- as.character(Data_df$Date[1])
show(First_Day)
# "2018-04-17"
Last_Day <- as.character(Data_df$Date[DS_length])
show(Last_Day)
# "2023-05-31"
TrnS_Last_Day <- as.character(Data_df$Date[which(Data_df$Date==as.Date("2023-01-01"))-1])
show(TrnS_Last_Day)
# "2022-12-31"
TrnS_length <- length(Data_df$Date[which(Data_df$Date<as.Date("2023-01-01"))])
show(TrnS_length)
# 1720
TstS_First_Day <- as.character(Data_df$Date[which(Data_df$Date==as.Date("2023-01-01"))])
show(TstS_First_Day)
# "2023-01-01"
TstS_length <- length(Data_df$Date[which(Data_df$Date>=as.Date("2023-01-01"))])
show(TstS_length)
# 151
TstS_length == DS_length-TrnS_length
# TRUE
First_Day <- as.character(Data_df$Date[2])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
Det_Day <- as.character(Data_df$Date[which(Data_df$Date=="2022-08-01")])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Master in Data Science 2024-2025",
                             paste("Detail of Bitcoin Daily Adjusted Close Price Logarithm, Predicted Values, and Prediction Bands of the fGarch::garchFit() Fitted GARCH(1,1) Model with GED Innovation - from ", .(First_Day), " to ", .(Last_Day), ", detail from ", .(Det_Day), sep="")))
link <- "https://finance.yahoo.com/quote/BTC-USD?p=BTC-USD"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points, from ", .(First_Day), " to ", .(TrnS_Last_Day),". Test set length ", .(TstS_length), " sample points, from ", .(TstS_First_Day), " to ", .(Last_Day),". Data by courtesy of Yahoo Finance - ", .(link)))
caption_content <- "Author: Roberto Monte"
# x_name <- bquote("dates")
# numbers::primeFactors(which(Data_df$Date==Last_Day)-which(Data_df$Date==Det_Day)-2)
x_breaks_num <- 27
x_breaks_low <- Data_df$x[which(Data_df$Date==Det_Day)]
x_breaks_up <- Data_df$x[DS_length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- Data_df$Date[x_breaks]
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Daily Adjusted Close Logarithm")
y_breaks_num <- 10
y_max <- max(na.rm(Data_df$y[c(which(Data_df$Date==Det_Day):which(Data_df$Date==Last_Day))]),
             na.rm(Data_df$adj.close_log_cond_090_upp_pred_int[c(which(Data_df$Date==Det_Day):which(Data_df$Date==Last_Day))]))

y_min <- min(na.rm(Data_df$y[c(which(Data_df$Date==Det_Day):which(Data_df$Date==Last_Day))]),
             na.rm(Data_df$adj.close_log_cond_090_low_pred_int[c(which(Data_df$Date==Det_Day):which(Data_df$Date==Last_Day))]))
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor(y_min/y_binwidth)*y_binwidth
y_breaks_up <- ceiling(y_max/y_binwidth)*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.0
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
line_col_grey <- bquote("adj. close log. - training set")
line_col_black <- bquote("adj. close log. - test set")
line_col_magenta <- bquote("adj. close log. - predicted test set")
line_col_green <- bquote("80% pred. band endpoints")
line_col_blue <- bquote("85% pred. band endpoints")
line_col_red <- bquote("90% pred. band endpoints")
leg_line_labs   <- c(line_col_grey, line_col_black, line_col_magenta, line_col_green, line_col_blue, line_col_red)
leg_line_breaks <- c("line_col_grey", "line_col_black", "line_col_magenta", "line_col_green", "line_col_blue", "line_col_red")
leg_line_cols   <- c("line_col_grey"="grey50", "line_col_black"="black", "line_col_magenta"="magenta", "line_col_green"="green", 
                     "line_col_blue"="blue", "line_col_red"="red")
fill_col_lightgreen <- bquote("80% pred. band")
fill_col_cyan <- bquote("85% pred. band")
fill_col_orangered <- bquote("90% pred. band")
leg_fill_labs <- c(fill_col_lightgreen, fill_col_cyan, fill_col_orangered)
leg_fill_breaks <- c("fill_col_lightgreen", "fill_col_cyan", "fill_col_orangered")
leg_fill_cols <- c("fill_col_lightgreen"="lightgreen", "fill_col_cyan"="cyan", "fill_col_orangered"="orangered")
Data_df_lp <- ggplot(Data_df, aes(x=x)) + 
  geom_ribbon(data=subset(Data_df, x >= x[TrnS_length+1]), aes(ymin=adj.close_log_cond_080_low_pred_int, ymax=adj.close_log_cond_080_upp_pred_int, fill= "fill_col_lightgreen"),
              alpha=0.3, colour= "lightgreen") +
  geom_ribbon(data=subset(Data_df, x >= x[TrnS_length+1]), aes(ymin=adj.close_log_cond_085_low_pred_int, ymax=adj.close_log_cond_080_low_pred_int, fill= "fill_col_cyan"),
              alpha=0.3, colour= "cyan") +
  geom_ribbon(data=subset(Data_df, x >= x[TrnS_length+1]), aes(ymin=adj.close_log_cond_090_low_pred_int, ymax=adj.close_log_cond_085_low_pred_int, fill= "fill_col_orangered"),
              alpha=0.3, colour= "orangered") +
  geom_ribbon(data=subset(Data_df, x >= x[TrnS_length+1]), aes(ymin=adj.close_log_cond_080_upp_pred_int, ymax=adj.close_log_cond_085_upp_pred_int, fill= "fill_col_cyan"),
              alpha=0.3, colour= "cyan") +
  geom_ribbon(data=subset(Data_df, x >= x[TrnS_length+1]), aes(ymin=adj.close_log_cond_085_upp_pred_int, ymax=adj.close_log_cond_090_upp_pred_int, fill= "fill_col_orangered"),
              alpha=0.3, colour= "orangered") +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=adj.close_log_cond_090_upp_pred_int, colour= "line_col_red"),
            linetype= "solid", alpha=1, linewidth=1) +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=adj.close_log_cond_090_low_pred_int, colour= "line_col_red"),
            linetype= "solid", alpha=1, linewidth=1) +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=adj.close_log_cond_085_upp_pred_int, colour= "line_col_blue"),
            linetype= "solid", alpha=1, linewidth=1) +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=adj.close_log_cond_085_low_pred_int, colour= "line_col_blue"),
            linetype= "solid", alpha=1, linewidth=1) +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=adj.close_log_cond_080_upp_pred_int, colour= "line_col_green"),
            linetype= "solid", alpha=1, linewidth=1) +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=adj.close_log_cond_080_low_pred_int, colour= "line_col_green"),
            linetype= "solid", alpha=1, linewidth=1) +
  geom_line(data=subset(Data_df, (x >= x[which(Data_df$Date==Det_Day)] & x <= x[TrnS_length])), aes(y=y, color= "line_col_grey"),
            linetype= "solid", alpha=1, linewidth=0.7, group=1, na.rm=TRUE) +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=y, color= "line_col_black"), 
            linetype= "solid", alpha=1, linewidth=0.7, group=1, na.rm=TRUE) +
  geom_line(data=subset(Data_df, x >= x[TrnS_length+1]), aes(y=z, color="line_col_magenta"),
            linetype="solid", alpha=1, linewidth=0.7, group=1, na.rm=TRUE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, labels=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims, sec.axis= sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  guides(linetype= "none", shape= "none") +
  scale_colour_manual(name= "", labels=leg_line_labs, values=leg_line_cols, breaks=leg_line_breaks) +
  scale_fill_manual(name= "", labels=leg_fill_labs, values=leg_fill_cols, breaks=leg_fill_breaks) +
  guides(colour=guide_legend(order=1), fill=guide_legend(order=2)) +
  theme(plot.title=element_text(hjust=0.5, size=12), 
        plot.subtitle=element_text(hjust=0.5, size=10),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(Data_df_lp)
#
# It must be considered that in the above plot while the predicted test set values are correct, the endpoints of the confidence bands have
# been computed under the approximations
# $low(T+t+1,T+t)\approx low(T+1,T) \text{and} $upp(T+t+1,T+t)\approx upp(T+1,T), \forall t=0,\dots\U-1$.
# We should build a complete daily updating estimation procedure for an exact computation of the endpoints of the confidence bands.
################################################################################################################################################
################################################################################################################################################
# In the end, we consider the accuracy of the fGARCH_1_1_ged_shpN_lbfgsb_nm model.
#
Data_df <- BTC_red_df
head(Data_df)
tail(Data_df)
#
Data_df <- dplyr::rename(Data_df, y=log_ret_perc)
DS_length <- length(na.rm(Data_df$y))
show(DS_length)
# 1870
#
TrnS_length <- length(na.rm(Data_df$y[which(Data_df$Date<as.Date("2023-01-01"))]))
show(TrnS_length)
# 1719
#
# Data_df$Date[TrnS_length+2]
# "2023-01-01"
#
TstS_length <- DS_length-TrnS_length
show(TstS_length)
# 151
#
y_train <- Data_df$y[2:(TrnS_length+1)]
head(y_train,20)
# 3.2535928  1.5906496  6.4376485  0.5608363 -1.0523309  1.4483698  8.2353199 -9.1932107  4.8088210 -3.2239593  3.9429183  0.7523614
# -1.9136041 -1.3240167  1.2739001  5.3537158 -0.4433170  1.6094355 -2.0843383 -2.9620923
#
length(y_train)
# 1719
#
y_fit  <- fGARCH_1_1_ged_shpN_lbfgsb_nm_fitted
head(y_fit,20)
# [1]  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#
length(y_fit)
# 1719
#
y_resid  <- y_train
head(y_resid,20)
# 3.2535928  1.5906496  6.4376485  0.5608363 -1.0523309  1.4483698  8.2353199 -9.1932107  4.8088210 -3.2239593  3.9429183  0.7523614
# -1.9136041 -1.3240167  1.2739001  5.3537158 -0.4433170  1.6094355 -2.0843383 -2.9620923
#
y_test <- Data_df$y[(TrnS_length+1):DS_length]
head(y_test,20)
# -0.33236657  0.46776063  0.38057007 -0.05162549  1.09341539 -0.15728177  0.68207922  0.01834058  0.79930823  0.61485892  1.44181339
#  2.76211505  5.08031805  5.36492128  5.21924405 -0.45631528  1.37377200 -0.03833244 -2.25928311  1.90553341class(y_test)
#
length(y_test)
# 151
#
y_pred <- rep(0,TstS_length)
head(y_pred,20)
# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#
length(y_pred)
# 151
#
y_test_resid <- y_test-y_pred
head(y_test_resid,20)
# -0.33236657  0.46776063  0.38057007 -0.05162549  1.09341539 -0.15728177  0.68207922  0.01834058  0.79930823  0.61485892  1.44181339
#  2.76211505  5.08031805  5.36492128  5.21924405 -0.45631528  1.37377200 -0.03833244 -2.25928311  1.90553341class(y_mean_test_resid)
#
# library(fabletools)
fGARCH_1_1_ged_shpN_lbfgsb_nm_acc <- fabletools::accuracy(y_pred, y_test)
show(fGARCH_1_1_ged_shpN_lbfgsb_nm_acc)
#               ME       RMSE      MAE      MPE   MAPE
# Test set  0.3390455  2.548477  1.764749   100   100
#
# library(DescTools)
fGARCH_1_1_ged_shpN_lbfgsb_nm_SMAPE <- DescTools::SMAPE(y_pred, y_test)
show(fGARCH_1_1_ged_shpN_lbfgsb_nm_SMAPE)
# 2
#
fGARCH_1_1_ged_shpN_lbfgsb_nm_SMAPE_perc <- 100*mean(abs(y_test_resid)/(abs(y_pred)+abs(y_test)))
show(fGARCH_1_1_ged_shpN_lbfgsb_nm_SMAPE_perc)
# 100
#
(fGARCH_1_1_ged_shpN_lbfgsb_nm_SMAPE/2)*100==fGARCH_1_1_ged_shpN_lbfgsb_nm_SMAPE_perc
# TRUE
#
fGARCH_1_1_ged_shpN_lbfgsb_nm_MASE <- fabletools::MASE(y_test_resid, y_train, demean=FALSE, na.rm=TRUE, .period=1)
show(fGARCH_1_1_ged_shpN_lbfgsb_nm_MASE)
# 0.4664513
#
fGARCH_1_1_ged_shpN_lbfgsb_nm_RMSSE <- fabletools::RMSSE(y_test_resid, y_train, demean=FALSE, na.rm=TRUE, .period=1)
show(fGARCH_1_1_ged_shpN_lbfgsb_nm_RMSSE)
# 0.4681793
#
# Note that since a GARCH(1,1) model estimates and predicts zero-valued states, most accuracy metrics perform very poorly. On the contrary,
# the MASE and RMSSE perform very well since they compare the empirical mean of the absolute and squared residuals of the predicted values
# in the test set, respectively, with the empirical mean of the absolute residuals of the values of the lagged states of the training set 
# considered as predictors of the states themselves (random walk prediction). The fact that the percentage returns of the logarithm are 
# weakly correlated inflates the latter average and deflates MASE and RMSSE.
# For instance, in terms of the MASE we have
mean(abs(y_train[-length(y_train)]-y_train[-1]))
# 3.783351
mean(abs(y_test_resid))
# 1.764749
mean(abs(y_test_resid))/mean(abs(y_train[-length(y_train)]-y_train[-1]))
# 0.4664513
#
# For this reason, we believe it is incorrect to carry out standard accuracy tests on trivial estimates and predictions of the model states.
#
# However, rather than predicting the states, the main goal of a GARCH(1,1) is predicting the volatility. Therefore, we should apply the
# accuracy metrics to predict the conditional standard deviation. On the other hand, using our GARCH(1,1) model, we have built the daily 
# estimated and predicted conditional volatility, but we have no clue about the true values of the conditional volatility in the training
# and test set. If we had the intra-days data of the Bitcon logarithm return percentage, we could consider their empirical standard 
# deviation as a proxy of the daily conditional volatility in the test and training set and try to confront such a proxy with the daily 
# estimated and predicted conditional volatility, but we have not intra-days data. Thinking about what we can do, we currently see two
# possibilities. The simplest is to consider the absolute values of the daily logarithm return percentage as proxies for the true values
# of the daily conditional volatility. The more complex is to re-estimate the model on a different time scale, for instance a monthly time 
# scale, and consider the empirical mean of the intra-month daily squared percentage return as proxies for the monthly conditional variance.
# We will consider the first possibility, which actually leads to a more reasonable accuracy evaluation than the one obtained above.
# 
Data_df <- BTC_red_df
head(Data_df)
tail(Data_df)
#
Data_df <- dplyr::rename(Data_df, y=log_ret_perc)
Data_df <- add_column(Data_df, y_sq=Data_df$y^2, y_sqrt=abs(Data_df$y), .after="y")
head(Data_df,20)
tail(Data_df)
#
y_train <- Data_df$y_sqrt[min(which(!is.na(Data_df$y_sqr))):which(Data_df$Date=="2022-12-30")]
head(y_train,20)
# 3.2535928 1.5906496 6.4376485 0.5608363 1.0523309 1.4483698 8.2353199 9.1932107 4.8088210 3.2239593 3.9429183 0.7523614 1.9136041
# 1.3240167 1.2739001 5.3537158 0.4433170 1.6094355 2.0843383 2.9620923#
length(y_train)
# 1719
#
y_fit  <- fGARCH_1_1_ged_shpN_lbfgsb_nm_cond_std_dev
head(y_fit,20)
# 3.751260 3.720987 3.604736 3.919135 3.765245 3.630090 3.515467 4.112250 4.727272 4.721813 4.607029 4.547142 4.356387 4.207146 
# 4.049358 3.900692 4.040850 3.877981 3.751222 3.652842
#
length(y_fit)
# 1719
#
y_resid  <- y_train-y_fit[-1]
head(y_resid,20)
# -0.46739427 -2.01408619  2.51851374 -3.20440850 -2.57775874 -2.06709677  4.12306982  4.46593818  0.08700767 -1.38306961 -0.60422378
# -3.60402570 -2.29354147 -2.72534091 -2.62679175  1.31286583 -3.43466414 -2.14178646 -1.56850388 -0.64965556
#
y_test <- Data_df$y_sqrt[which(Data_df$Date=="2023-01-01"):nrow(Data_df)]
head(y_test,20)
# 0.46776063 0.38057007 0.05162549 1.09341539 0.15728177 0.68207922 0.01834058 0.79930823 0.61485892 1.44181339 2.76211505 5.08031805
# 5.36492128 5.21924405 0.45631528 1.37377200 0.03833244 2.25928311 1.90553341 7.26845526
#
length(y_test)
# 151
#
y_pred <- fGARCH_1_1_ged_shpN_lbfgsb_nm_pred_std_dev
head(y_pred,20)
# 2.174446 2.229967 2.283048 2.333878 2.382622 2.429426 2.474418 2.517715 2.559421 2.599628 2.638423 2.675883 2.712079 2.747075 2.780932
# 2.813704 2.845443 2.876197 2.906009 2.934920
#
length(y_pred)
# 151
#
y_test_resid <- y_test-y_pred
head(y_test_resid,20)
# -1.7066855 -1.8493965 -2.2314223 -1.2404627 -2.2253404 -1.7473466 -2.4560777 -1.7184071 -1.9445618 -1.1578149  0.1236918  2.4044349
#  2.6528426  2.4721691 -2.3246164 -1.4399321 -2.8071110 -0.6169138 -1.0004754  4.3335352
#
# library(fabletools)
fGARCH_1_1_ged_shpN_lbfgsb_nm_std_dev_acc <- fabletools::accuracy(y_pred, y_test)
show(fGARCH_1_1_ged_shpN_lbfgsb_nm_std_dev_acc)
#               ME      RMSE    MAE      MPE     MAPE
# Test set -1.80114 2.607779 2.342589 -1634.44 1642.694
#
# library(DescTools)
fGARCH_1_1_ged_shpN_lbfgsb_nm_std_dev_SMAPE <- DescTools::SMAPE(y_pred, y_test)
show(fGARCH_1_1_ged_shpN_lbfgsb_nm_std_dev_SMAPE)
# 1.015688
fGARCH_1_1_ged_shpN_lbfgsb_nm_std_dev_SMAPE_perc <- 100*mean(abs(y_test_resid)/(abs(y_pred)+abs(y_test)))
show(fGARCH_1_1_ged_shpN_lbfgsb_nm_std_dev_SMAPE_perc)
# 50.78442
(fGARCH_1_1_ged_shpN_lbfgsb_nm_std_dev_SMAPE/2)*100==fGARCH_1_1_ged_shpN_lbfgsb_nm_std_dev_SMAPE_perc
# TRUE
#
fGARCH_1_1_ged_shpN_lbfgsb_nm_std_dev_MASE <- fabletools::MASE(y_test_resid, y_train, demean=FALSE, na.rm=TRUE, .period=1)
show(fGARCH_1_1_ged_shpN_lbfgsb_nm_std_dev_MASE)
# 0.9684431
#
fGARCH_1_1_ged_shpN_lbfgsb_nm_std_dev_RMSSE <- fabletools::RMSSE(y_test_resid, y_train, demean=FALSE, na.rm=TRUE, .period=1)
show(fGARCH_1_1_ged_shpN_lbfgsb_nm_std_dev_RMSSE)
# 0.7008329
#
############################################################################################################################################
############################################################################################################################################
############################################################################################################################################
############################################################################################################################################
############################################################################################################################################
############################################################################################################################################
############################################################################################################################################
############################################################################################################################################
############################################################################################################################################
