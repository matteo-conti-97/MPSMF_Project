###############################################################################################################################################
############################################################## References #####################################################################
###############################################################################################################################################
#
# Rob J Hyndman and George Athanasopoulos - Forecasting: Principles and Practice
# Monash Univerisity, Australia
# https://otexts.com/fpp2/
#
# Robert H. Shumway, David S. Stoffer - Time Series Analysis and Its Applications (with R Examples) 4th Edition
# Springer Texts in Statistics - Springer Verlag
# https://www.stat.pitt.edu/stoffer/tsa4/tsa4.pdf
#
# R - Residual Diagnostic
# https://cran.r-project.org/web/packages/olsrr/vignettes/residual_diagnostics.html
#
###############################################################################################################################################
########################################################## Environmental Setting ##############################################################
###############################################################################################################################################
#
# Removes all items in Global Environment
rm(list=ls())
#
# To store options' default values.
def_options <- options()
options(def_options)
#
# To change digits option
# show(def_options$digits)
# or
# getOption("digits")
# # 7
# options(digits=22)
# getOption("digits")
# 22
# To restore digits option to default value
# options(digits=7)
# or
# options(digits=def_options$digits)
# getOption("digits")
# 7
#
# To show data frames with a large number of rows.
# show(def_options$max.print)
# or 
# getOption("max.print")
# 1000
# options(max.print=10000)
# getOption("max.print")
#
# To restore max.print option to default value
# options(max.print=1000)
# or
# options(max.print=def_options$max.print)
# getOption("max.print")
# 1000
#
# Clears all Plots
# try(dev.off(dev.list()["RStudioGD"]), silent=TRUE)
try(dev.off(),silent=TRUE)
#
# Clear the Console
cls <- function() cat(rep("\n",100))
cls()
#
# Sys.getenv('PATH')
#
# Sets the current directory as the work directory. 
WD <- dirname(rstudioapi::getSourceEditorContext()$path)
show(WD)
setwd(WD)
dir()
#
###############################################################################################################################################
################################################################# Libraries ###################################################################
###############################################################################################################################################
#
# https://www.r-project.org/other-docs.html
library(base)
# https://rdrr.io/r/#base
# https://www.math.ucla.edu/~anderson/rw1001/library/base/html/00Index.html
library(utils)
#
library(stats)
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/00Index.html
#
library(lubridate)
library(zoo)
library(xts)
library(TTR)
library(quantmod)
library(dplyr)
library(tibble)
library(numbers)
library(ggplot2)
library(DescTools)
library(fitdistrplus)
###############################################################################################################################################
###############################################################################################################################################
# Reading libraries
library(tidyverse)
library(moments)
library(strucchange)
library(broom)
library(rlang)
library(gridSVG)
library(grid)
###############################################################################################################################################
################################################################# Functions ###################################################################
###############################################################################################################################################
#
na.rm <- function(x){as.vector(x[!is.na(as.vector(x))])}
#
###############################################################################################################################################
######################################## Create a Data Frame with Standard & Poor 500 Index Data ##############################################
# library(lubridate)
# library(quantmod)
# We retrieve financial data from https://finance.yahoo.com/
Last_Day <- Sys.Date()-1
show(Last_Day)
# [1] "2024-04-10"
First_day <- Last_Day-lubridate::years(x=4)
show(First_day)
# [1] "2020-04-10"
Data <- quantmod::getSymbols(c("^DJI", "^GSPC", "^IXIC"), src='yahoo', from = First_day, to = Last_Day)
# DJI=Dow Jones Industrial Index,  GSPC=Standard and Poor's 500 Index, IXIC=NASDAQ Composite
class(Data)
# [1] "character"
Data
# [1] "DJI"  "GSPC" "IXIC"
class(GSPC)
# [1] "xts" "zoo"
head(GSPC, 3)
# GSPC.Open GSPC.High GSPC.Low GSPC.Close GSPC.Volume GSPC.Adjusted
# 2020-04-13   2782.46   2782.46  2721.17    2761.63  5319530000       2761.63
# 2020-04-14   2805.10   2851.85  2805.10    2846.06  5615730000       2846.06
tail(GSPC, 3)
# GSPC.Open GSPC.High GSPC.Low GSPC.Close GSPC.Volume GSPC.Adjusted
# 2024-04-05   5158.95   5222.18  5157.21    5204.34  3386780000       5204.34
# 2024-04-08   5211.37   5219.57  5197.35    5202.39  3278180000       5202.39
# 2024-04-09   5217.03   5224.81  5160.78    5209.91  3400680000       5209.91
#
# Since we have got our data through the quantmod::getSymbols() function, we can immediately draw a plot using the quantmod::chartSeries()
# function.
quantmod::chartSeries(GSPC, type="auto", theme=chartTheme('white'))
# The plot is a (Japanese) candlestick chart. 
# Each candlestick represents an asset trading unit of time (generally a day).
# Similarly to a box plot, each candlestick is built with a body and two whiskers, called shadows or wicks in this context.
# The lowest [resp. highest] point of the bottom [resp. top] shadow represents the lowest [resp. highest] value of the asset price 
# in the unit time of trading.
# The color of the candlestick body is white or green [resp. black or red] for an open price lower [resp. higher] than the close price.
# The lowest [resp. highest] point of the body represents the open or close [resp. close or open] asset price according to whether 
# the open asset price is lower or higher than the close price, that is, the candlestick is white or green [resp. black or red].
# The length of the body and shadows are proportional to the asset price changes given a unit measure.
# Each bar at the bottom of the plot represents the asset trading volume in the unit of time. This is the amount of shares of stock traded 
# in the unit of time. The higher the volume bar, the more actively the stock is traded. The color of the volume bar is white or green 
# [resp. black or red], depending on the color of the overhead candle is white or green [resp. black or red].
#
head(zoo::index(GSPC))
# [1] "2020-04-13" "2020-04-14" "2020-04-15" "2020-04-16" "2020-04-17" "2020-04-20"
class(index(GSPC))
# [1] "Date"
#
head(zoo::coredata(GSPC))
#       GSPC.Open GSPC.High GSPC.Low GSPC.Close GSPC.Volume GSPC.Adjusted
# [1,]   2782.46   2782.46  2721.17    2761.63  5319530000       2761.63
# [2,]   2805.10   2851.85  2805.10    2846.06  5615730000       2846.06
# [3,]   2795.64   2801.88  2761.54    2783.36  5208000000       2783.36
# [4,]   2799.34   2806.51  2764.32    2799.55  5228810000       2799.55
# [5,]   2842.43   2879.22  2830.88    2874.56  5804810000       2874.56
# [6,]   2845.62   2868.98  2820.43    2823.16  5228630000       2823.16
class(coredata(GSPC))
# [1] "matrix" "array"
#
GSPC_df <- data.frame(GSPC.Index=c(1:length(index(GSPC))), GSPC.Date=index(GSPC), zoo::coredata(GSPC))
head(GSPC_df)
#   GSPC.Index  GSPC.Date GSPC.Open GSPC.High GSPC.Low GSPC.Close GSPC.Volume GSPC.Adjusted
# 1          1 2020-04-13   2782.46   2782.46  2721.17    2761.63  5319530000       2761.63
# 2          2 2020-04-14   2805.10   2851.85  2805.10    2846.06  5615730000       2846.06
# 3          3 2020-04-15   2795.64   2801.88  2761.54    2783.36  5208000000       2783.36
# 4          4 2020-04-16   2799.34   2806.51  2764.32    2799.55  5228810000       2799.55
# 5          5 2020-04-17   2842.43   2879.22  2830.88    2874.56  5804810000       2874.56
# 6          6 2020-04-20   2845.62   2868.98  2820.43    2823.16  5228630000       2823.16
tail(GSPC_df)
#      GSPC.Index  GSPC.Date GSPC.Open GSPC.High GSPC.Low GSPC.Close GSPC.Volume GSPC.Adjusted
# 1000       1000 2024-04-02   5204.29   5208.34  5184.05    5205.81  3886590000       5205.81
# 1001       1001 2024-04-03   5194.37   5228.75  5194.37    5211.49  3703250000       5211.49
# 1002       1002 2024-04-04   5244.05   5256.59  5146.06    5147.21  4075680000       5147.21
# 1003       1003 2024-04-05   5158.95   5222.18  5157.21    5204.34  3386780000       5204.34
# 1004       1004 2024-04-08   5211.37   5219.57  5197.35    5202.39  3278180000       5202.39
# 1005       1005 2024-04-09   5217.03   5224.81  5160.78    5209.91  3400680000       5209.91
#
# library(dplyr)
GSPC_df <- dplyr::rename(GSPC_df, Index=GSPC.Index, Date=GSPC.Date, Open=GSPC.Open, High=GSPC.High, Low=GSPC.Low,
                         Close=GSPC.Close, Volume=GSPC.Volume,  Adjusted=GSPC.Adjusted)
head(GSPC_df)
# Index       Date    Open    High     Low   Close     Volume Adjusted
# 1     1 2020-04-13 2782.46 2782.46 2721.17 2761.63 5319530000  2761.63
# 2     2 2020-04-14 2805.10 2851.85 2805.10 2846.06 5615730000  2846.06
# 3     3 2020-04-15 2795.64 2801.88 2761.54 2783.36 5208000000  2783.36
# 4     4 2020-04-16 2799.34 2806.51 2764.32 2799.55 5228810000  2799.55
# 5     5 2020-04-17 2842.43 2879.22 2830.88 2874.56 5804810000  2874.56
# 6     6 2020-04-20 2845.62 2868.98 2820.43 2823.16 5228630000  2823.16
#
# save(GSPC_df, file="GSPC_df_2020_04_2024_04.RData")
# rm(GSPC_df)
# head(GSPC_df)
# load("GSPC_df_2020_04_2024_04.RData")
# head(GSPC_df)
# Index       Date    Open    High     Low   Close     Volume Adjusted
# 1     1 2020-04-13 2782.46 2782.46 2721.17 2761.63 5319530000  2761.63
# 2     2 2020-04-14 2805.10 2851.85 2805.10 2846.06 5615730000  2846.06
# 3     3 2020-04-15 2795.64 2801.88 2761.54 2783.36 5208000000  2783.36
# 4     4 2020-04-16 2799.34 2806.51 2764.32 2799.55 5228810000  2799.55
# 5     5 2020-04-17 2842.43 2879.22 2830.88 2874.56 5804810000  2874.56
# 6     6 2020-04-20 2845.62 2868.98 2820.43 2823.16 5228630000  2823.16
#
# Alternatively, we can visit Yahoo Finance US website (see https://finance.yahoo.com/) and search for the symbol ^SPX (not ^GSPC). Thereby,
# selecting "^SPX S&P 500 INDEX" - "Historical Data", we are led to the webpage https://finance.yahoo.com/quote/%5ESPX/history. Here, we can 
# select the time period we are interested in, say 2020-04-13, 2024-04-09, and apply our query. By clicking on the download button, We get 
# the desired data as the ^SPX.csv file by clicking on the download button.
# We load the ^SPX.csv file in R environment.
SPX_df <- read.csv("^SPX.csv", header=TRUE)
class(SPX_df)
# [1] "data.frame"
head(SPX_df)
# 1 2020-04-13 2782.46 2782.46 2721.17 2761.63   2761.63 5319530000
# 2 2020-04-14 2805.10 2851.85 2805.10 2846.06   2846.06 5615730000
# 3 2020-04-15 2795.64 2801.88 2761.54 2783.36   2783.36 5208000000
# 4 2020-04-16 2799.34 2806.51 2764.32 2799.55   2799.55 5228810000
# 5 2020-04-17 2842.43 2879.22 2830.88 2874.56   2874.56 5804810000
#
SPX_df <- data.frame(Date=SPX_df[,1], Open=SPX_df[,2], High=SPX_df[,3], Low=SPX_df[,4], Close=SPX_df[,5], Volume=SPX_df[,7],  
                     Adjusted=SPX_df[,6])
head(SPX_df)
#       Date    Open    High     Low   Close     Volume   Adjusted
# 1 2020-04-13 2782.46 2782.46 2721.17 2761.63 5319530000  2761.63
# 2 2020-04-14 2805.10 2851.85 2805.10 2846.06 5615730000  2846.06
# 3 2020-04-15 2795.64 2801.88 2761.54 2783.36 5208000000  2783.36
# 4 2020-04-16 2799.34 2806.51 2764.32 2799.55 5228810000  2799.55
# 5 2020-04-17 2842.43 2879.22 2830.88 2874.56 5804810000  2874.56
# 6 2020-04-20 2845.62 2868.98 2820.43 2823.16 5228630000  2823.16
#
class(SPX_df$Date)
# [1] "character"
SPX_df$Date <- as.Date(SPX_df$Date, format="%Y-%m-%d")
head(SPX_df)
# Date    Open    High     Low   Close     Volume Adjusted
# 1 2020-04-13 2782.46 2782.46 2721.17 2761.63 5319530000  2761.63
# 2 2020-04-14 2805.10 2851.85 2805.10 2846.06 5615730000  2846.06
# 3 2020-04-15 2795.64 2801.88 2761.54 2783.36 5208000000  2783.36
# 4 2020-04-16 2799.34 2806.51 2764.32 2799.55 5228810000  2799.55
# 5 2020-04-17 2842.43 2879.22 2830.88 2874.56 5804810000  2874.56
# 6 2020-04-20 2845.62 2868.98 2820.43 2823.16 5228630000  2823.16
class(SPX_df$Date)
# [1] "Date"
#
identical(GSPC_df$Date,SPX_df$Date)
# [1] FALSE
which(GSPC_df$Date!=SPX_df$Date)
# [1] 1005
# Warning message: In `!=.default`(GSPC_df$Date, SPX_df$Date): longer object length is not a multiple of shorter object length
length(GSPC_df$Date)
# [1] 1005
length(SPX_df$Date)
# [1] 1004
tail(GSPC_df$Date,3)
# [1] "2024-04-05" "2024-04-08" "2024-04-09"
tail(SPX_df$Date,3)
# [1] "2024-04-04" "2024-04-05" "2024-04-08"
#
identical(GSPC_df$Date[-1005],SPX_df$Date)
# [1] TRUE
identical(GSPC_df$Open[-1005],SPX_df$Open)
# [1] FALSE
head(which(GSPC_df$Open[-1005]!=SPX_df$Open),20)
# [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
head(GSPC_df$Open[-1005],10)
# [1] 2782.46 2805.10 2795.64 2799.34 2842.43 2845.62 2784.81 2787.89 2810.42 2812.64
head(SPX_df$Open,10)
# [1] 2782.46 2805.10 2795.64 2799.34 2842.43 2845.62 2784.81 2787.89 2810.42 2812.64
identical(round(GSPC_df$Open[-1005],6),round(SPX_df$Open,6))
# [1] TRUE
identical(round(GSPC_df$High[-1005],6),round(SPX_df$High,6))
# [1] TRUE
identical(round(GSPC_df$Low[-1005],6),round(SPX_df$Low,6))
# [1] TRUE
identical(round(GSPC_df$Close[-1005],6),round(SPX_df$Close,6))
# [1] TRUE
identical(round(GSPC_df$Volume[-1005],6),round(SPX_df$Volume,6))
# [1] TRUE
identical(round(GSPC_df$Adjusted[-1005],6),round(SPX_df$Adjusted,6))
# [1] TRUE
#
# library(tibble)
SPX_df <- tibble::add_column(SPX_df, Index=c(1:nrow(SPX_df)), .before="Date")
head(SPX_df)
# Index       Date    Open    High     Low   Close     Volume Adjusted
# 1     1 2020-04-13 2782.46 2782.46 2721.17 2761.63 5319530000  2761.63
# 2     2 2020-04-14 2805.10 2851.85 2805.10 2846.06 5615730000  2846.06
# 3     3 2020-04-15 2795.64 2801.88 2761.54 2783.36 5208000000  2783.36
# 4     4 2020-04-16 2799.34 2806.51 2764.32 2799.55 5228810000  2799.55
# 5     5 2020-04-17 2842.43 2879.22 2830.88 2874.56 5804810000  2874.56
# 6     6 2020-04-20 2845.62 2868.98 2820.43 2823.16 5228630000  2823.16
#
# save(SPX_df, file="SPX_df_2020_04_2024_04.RData")
# rm(SPX_df)
# head(SPX_df)
# load("SPX_df_2020_04_2024_04.RData")
# head(SPX_df)
# Index       Date    Open    High     Low   Close     Volume Adjusted
# 1     1 2020-04-13 2782.46 2782.46 2721.17 2761.63 5319530000  2761.63
# 2     2 2020-04-14 2805.10 2851.85 2805.10 2846.06 5615730000  2846.06
# 3     3 2020-04-15 2795.64 2801.88 2761.54 2783.36 5208000000  2783.36
# 4     4 2020-04-16 2799.34 2806.51 2764.32 2799.55 5228810000  2799.55
# 5     5 2020-04-17 2842.43 2879.22 2830.88 2874.56 5804810000  2874.56
# 6     6 2020-04-20 2845.62 2868.98 2820.43 2823.16 5228630000  2823.16
#
###############################################################################################################################################
###############################################################################################################################################
###################### Scatter and Line plot of the ^GSPC Daily Adjusted Close Price from Apr-13-2020 to Apr-09-2024 ##########################
# load("GSPC_df_2020_04_2024_04.RData")
# head(GSPC_df)
# Index     Date      Open    High     Low    Close     Volume Adjusted
# 1     1 2020-04-13 2782.46 2782.46 2721.17 2761.63 5319530000  2761.63
# 2     2 2020-04-14 2805.10 2851.85 2805.10 2846.06 5615730000  2846.06
# 3     3 2020-04-15 2795.64 2801.88 2761.54 2783.36 5208000000  2783.36
# 4     4 2020-04-16 2799.34 2806.51 2764.32 2799.55 5228810000  2799.55
# 5     5 2020-04-17 2842.43 2879.22 2830.88 2874.56 5804810000  2874.56
# 6     6 2020-04-20 2845.62 2868.98 2820.43 2823.16 5228630000  2823.16
tail(GSPC_df)
# Index         Date     Open    High     Low   Close    Volume    Adjusted
# 1000  1000 2024-04-02 5204.29 5208.34 5184.05 5205.81 3886590000  5205.81
# 1001  1001 2024-04-03 5194.37 5228.75 5194.37 5211.49 3703250000  5211.49
# 1002  1002 2024-04-04 5244.05 5256.59 5146.06 5147.21 4075680000  5147.21
# 1003  1003 2024-04-05 5158.95 5222.18 5157.21 5204.34 3386780000  5204.34
# 1004  1004 2024-04-08 5211.37 5219.57 5197.35 5202.39 3278180000  5202.39
# 1005  1005 2024-04-09 5217.03 5224.81 5160.78 5209.91 3400680000  5209.91
#
# Scatter Plot of ^GSPC Daily Adjusted Close Price
Data_df <- GSPC_df
Data_df <- rename(Data_df, x=Index, y=Adjusted)
sum(is.na(Data_df$y)) # We check whether we have NA in the BTC adjusted close price data set and how many NA we have
# [1] 0
DS_length <- length(Data_df$y)
show(DS_length)
# [1] 1005
First_Day <- as.character(Data_df$Date[1])
show(First_Day)
# [1] "2020-04-13"
Last_Day <- as.character(Data_df$Date[DS_length])
show(Last_Day)
# [1] "2024-04-09"
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Metodi Probabilistici e Statistici per i Mercati Finanziari 2023-2024",
                             paste("S&P 500 (^GSPC) Daily Adjusted Close Price from ", .(First_Day), " to ", .(Last_Day), sep="")))
link <- "https://finance.yahoo.com/quote/%5EGSPC?.tsrc=fin-srch"
subtitle_content <- bquote(paste("Data set length - ", .(DS_length), " sample points. Data by courtesy of Yahoo Finance - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("")
# library(numbers)
# numbers::primeFactors(DS_length-2)
x_breaks_num <- 59 # (deduced from primeFactors(DS_length-2))
x_breaks_low <- Data_df$x[1]
x_breaks_up <- Data_df$x[DS_length]
x_binwidth <- ceiling((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Date[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
#
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
#
point_blue <- bquote("daily adjusted close prices")
line_red  <- bquote("LOESS curve")
line_green  <- bquote("regression line")
leg_labs <- c(point_blue, line_red, line_green)
leg_cols <- c("point_blue"="blue", "line_red"="red", "line_green"="green")
leg_breaks <- c("point_blue", "line_red", "line_green")
# library(ggplot2)
GSPC_Adj.Close_sp <- ggplot2::ggplot(Data_df) +
  geom_smooth(aes(x=x, y=y, color="line_green"), method="lm", formula=y ~ x, alpha=1, lwd=0.8, linetype="solid",
              se=FALSE, fullrange=FALSE) +
  geom_smooth(aes(x=x, y=y, color="line_red"), method="loess", formula=y ~ x, alpha=1, lwd=0.8, linetype="dashed",
              se=FALSE, fullrange=FALSE) +
  geom_point(alpha=1, size=0.6, shape=19, aes(x=x, y=y, color="point_blue")) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(GSPC_Adj.Close_sp)
#
# file_name = paste("S&P 500 Daily Adjusted Close Price SP from ", First_Day, " to ", Last_Day, " - SP.tiff", sep="")
# tiff(file_name, width=1600, height=800, res=120)
# print(Data_df_SP)
# dev.off()
#
# file_name = paste("S&P 500 Daily Adjusted Close Price SP from ", First_Day, " to ", Last_Day, " - SP.png", sep="")
# png(file_name, width=1600, height=800, res=120)
# print(Data_df_SP)
# dev.off()
#
# file_name = paste("S&P 500 Daily Adjusted Close Price SP from ", First_Day, " to ", Last_Day, " - SP.pdf", sep="")
# pdf(file_name, width=12, height=7)
# print(Data_df_SP)
# dev.off()
#
# Line Plot of ^GSPC Daily Adjusted Close Price
line_blue  <- bquote("daily adjusted close prices")
line_red  <- bquote("LOESS curve")
line_green  <- bquote("regression line")
leg_labs <- c(line_blue, line_red, line_green)
leg_cols <- c("line_blue"="blue", "line_red"="red", "line_green"="green")
leg_breaks <- c("line_blue", "line_red", "line_green")
GSPC_Adj.Close_lp <- ggplot2::ggplot(Data_df) +
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
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(GSPC_Adj.Close_lp)
#
# file_name = paste("S&P 500 Daily Adjusted Close Price LP from ", First_Day, " to ", Last_Day, " - LP.tiff", sep="")
# tiff(file_name, width=1600, height=800, res=120)
# print(Data_df_SP)
# dev.off()
#
# file_name = paste("S&P 500 Daily Adjusted Close Price LP from ", First_Day, " to ", Last_Day, " - LP.png", sep="")
# png(file_name, width=1600, height=800, res=120)
# print(Data_df_LP)
# dev.off()
#
# file_name = paste("S&P 500 Daily Adjusted Close Price LP from ", First_Day, " to ", Last_Day, " - LP.pdf", sep="")
# pdf(file_name, width=12, height=7)
# print(Data_df_LP)
# dev.off()
#
summary(Data_df$y)
# Min.   1st Qu.   Median    Mean   3rd Qu.   Max. 
# 2737    3824      4148     4103    4456     5254 
#
var(Data_df$y)
# [1] 261098.8
#
# library(DescTools)
DescTools::Skew(Data_df$y, weights=NULL, method=2, conf.level=NA)
# [1] -0.3850485
#
DescTools::Kurt(Data_df$y, weights=NULL, method=2, conf.level=NA)
# [1] -0.0003901152
#
# library(fitdistrplus)
fitdistrplus::descdist(Data_df$y, discrete=FALSE, method= "sample", graph=TRUE, boot=1000)
# summary statistics
# ------
# min:  2736.56   max:  5254.35 
# median:  4147.6 
# mean:  4103.311 
# sample sd:  510.724 
# sample skewness:  -0.3844736 
# sample kurtosis:  2.993648
#
# The above-presented statistics would characterize the ^GSPC adjusted close price if the ^GSPC time series were ergodic, hence stationary. 
# On the other hand, we have visual evidence for non-stationarity both in the mean and variance. Therefore we cannot consider the above 
# statistics meaningful for the ^GSPC adjusted close price characterization unless we can establish the ergodicity of the time series with
# appropriate confidence.
###############################################################################################################################################
###############################################################################################################################################
# To build a model for forecasting the S&P 500 daily adjusted close price time series, we split the time series into two parts: an initial part,
# called the "in-sample" or "training" part, about $93\%$ of the time series starting and a final part, "out-of-sample" or "test" part, about $7\%$
# of the time series.
# The training set is chosen considering the ratio between the length of the time seires before the date "2024-01-01" and the entire length.
length(GSPC_df$Date[which(GSPC_df$Date<as.Date("2024-01-01"))])/length(GSPC_df$Date)
# [1] 0.9323383
# We consider the scatter plot of the S daily adjusted close prices training set and the test set
Data_df <- GSPC_df
head(Data_df)
tail(Data_df)
Data_df <- rename(Data_df, x=Index, y=Adjusted)
head(Data_df)
DS_length <- length(Data_df$y)
show(DS_length)
# [1] 1005
First_Day <- Data_df$Date[1]
show(First_Day)
# [1] "2020-04-13"
Last_Day <- Data_df$Date[DS_length]
show(Last_Day)
# [1] "2024-04-09"
TrnS_Last_Day <- Data_df$Date[max(which(Data_df$Date<=(as.Date("2024-01-01")-1)))]
show(TrnS_Last_Day)
# [1] "2023-12-29"
TrnS_length <- length(Data_df$Date[which(Data_df$Date<=TrnS_Last_Day)])
show(TrnS_length)
# [1] 937
TstS_First_Day <- Data_df$Date[min(which(Data_df$Date>=(TrnS_Last_Day+1)))]
show(TstS_First_Day)
# [1] "2024-01-02"
TstS_length <- length(Data_df$Date[which(Data_df$Date>=TstS_First_Day)])
show(TstS_length)
# [1] 68
TstS_length == DS_length-TrnS_length
# TRUE
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Metodi Probabilistici e Statistici per i Mercati Finanziari 2023-2024",
                             paste("S&P 500 Daily Adjusted Close Price - Training and Test Parts - from ", .(as.character(First_Day)), " to ", .(as.character(Last_Day)), sep="")))
link <- "https://finance.yahoo.com/quote/%5EGSPC?.tsrc=fin-srch"
subtitle_content <- bquote(paste("Training part length ", .(TrnS_length), " sample points, from ", .(as.character(First_Day)), " to ", .(as.character(TrnS_Last_Day)),". Test part length ", .(TstS_length), " sample points, from ", .(as.character(TstS_First_Day)), " to ", .(as.character(Last_Day)),". Data by courtesy of Yahoo Finance - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("")
# numbers::primeFactors(DS_length-2)
x_breaks_num <- 59 # (deduced from primeFactors(DS_length-2))
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
point_black <- bquote("daily adjusted close prices - training part")
point_blue <- bquote("daily adjusted close prices - test part")
line_green <- bquote("regression line (training set)")
line_red <- bquote("LOESS curve (training set)")
leg_point_labs <- c(point_black, point_blue)
leg_point_cols <- c("point_black"="black", "point_blue"="blue")
leg_point_breaks <- c("point_black", "point_blue")
leg_line_labs <- c(line_green, line_red)
leg_line_cols <- c("line_green"="green", "line_red"="red")
leg_line_breaks <- c("line_green", "line_red")
leg_col_labs   <- c(leg_point_labs,leg_line_labs)
leg_col_cols   <- c(leg_point_cols,leg_line_cols)
leg_col_breaks <- c(leg_point_breaks,leg_line_breaks)
GSPC_Adj.Close_TrnS_TstS_sp <- ggplot(Data_df, aes(x=x)) +
  geom_vline(xintercept=Data_df$x[TrnS_length], lwd=0.5, color="black") +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_green"), method="lm" , formula=y ~ x, 
              alpha=1, lwd=0.9, linetype="solid", se=FALSE, fullrange=FALSE) +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_red"), method="loess", formula=y ~ x, 
              alpha=1, lwd=0.9, linetype="dashed", se=FALSE, fullrange=FALSE) +
  geom_point(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="point_black"), alpha=1, size=0.7, shape=19) + 
  geom_point(data=subset(Data_df, Data_df$x>x[TrnS_length]), aes(y=y, color="point_blue"), alpha=1, size=0.7, shape=19) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="Legend", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  guides(colour=guide_legend(override.aes=list(shape=c(19, 19, NA, NA), linetype=c("blank", "blank", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(GSPC_Adj.Close_TrnS_TstS_sp)
#
# The line plot
GSPC_Adj.Close_TrnS_TstS_lp <- ggplot(Data_df, aes(x=x)) +
  geom_vline(xintercept=Data_df$x[TrnS_length], lwd=0.5, color="black") +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_green"), method="lm" , formula=y ~ x, 
              alpha=1, lwd=0.9, linetype="solid", se=FALSE, fullrange=FALSE) +
  geom_smooth(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="line_red"), method="loess", formula=y ~ x,
              alpha=1, lwd=0.9, linetype="dashed", se=FALSE, fullrange=FALSE) +
  geom_line(data=subset(Data_df, Data_df$x<=x[TrnS_length]), aes(y=y, color="point_black"), alpha=1, lwd=0.7, linetype="solid") +
  geom_line(data=subset(Data_df, Data_df$x>x[TrnS_length]), aes(y=y, color="point_blue"), alpha=1, lwd=0.7, linetype="solid") +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_col_labs, values=leg_col_cols, breaks=leg_col_breaks) +
  theme(plot.title=element_text(hjust=0.5, size=12),
        plot.subtitle=element_text(hjust= 0.5, size=10),
        plot.caption=element_text(hjust=1.0, size=8),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position= "bottom")
plot(GSPC_Adj.Close_TrnS_TstS_lp)
#
# From the scatter and line plot inspection, we have visual evidence for an increasing trend with some falls. Comparing the LOESS curve with 
# the regression line, the overall trend does not appear to be linear. Eventually, if it weren't for the decline from February to October 2022,
# it would appear likely exponential. 
# We do not have visual evidence for seasonality. Typically, stock market time series cannot have a pronounced seasonal component due to the
# very nature of the stock market. It is even difficult to define a seasonal period. Stock markets are usually closed on Saturday and Sunday.
# We would have been led to think over five days trading days. However, stock markets also close on national and some local holidays. This makes
# managing a five-day period complicated. Moreover, since stock price movements are due to the market traders' continuous buying and selling
# of the stock based on their expectations about stock future returns, and these expectations depend on the incoming erratic economic,
# political, and social news, it is not straightforward to conceive of a seasonal mechanism behind stock price movement. Nevertheless, some 
# evidence of hourly seasonality can be revealed on trading days. However, we would need intra-day data to manage it.
# The spread of the points of the training set around the LOESS, does not appear to be homogeneous throughout the LOESS path. We have 
# visual evidence for heteroscedasticity.
############################################################################################################################################
# For simplicity, we build a data frame containing only the training set data, before pursuing a quantitative analysis.
GSPC_train_df <- GSPC_df[which(GSPC_df$Date<TrnS_Last_Day),]
head(GSPC_train_df, 3)
# Index       Date    Open    High     Low   Close     Volume   Adjusted
# 1     1 2020-04-13 2782.46 2782.46 2721.17 2761.63 5319530000  2761.63
# 2     2 2020-04-14 2805.10 2851.85 2805.10 2846.06 5615730000  2846.06
# 3     3 2020-04-15 2795.64 2801.88 2761.54 2783.36 5208000000  2783.36
tail(GSPC_train_df, 3)
# Index       Date    Open    High     Low   Close      Volume    Adjusted
# 934   934 2023-12-26 4758.86 4784.72 4758.45 4774.75 2513910000  4774.75
# 935   935 2023-12-27 4773.45 4785.39 4768.90 4781.58 2748450000  4781.58
# 936   936 2023-12-28 4786.44 4793.30 4780.98 4783.35 2698860000  4783.35
#
# We consider the autocorrelograms of the training set. Of course, due to the clear trend, we expect a strong visual evidence for 
# autocorrelation.
# Autocorrelogram of the training set of the Bitcoin daily adjusted close prices.
Data_df <- GSPC_train_df
y <- Data_df$Adjusted
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
Aut_Fun_y_df <- data.frame(Lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
First_Day <- as.character(Data_df$Date[1])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Metodi Probabilistici e Statistici per i Mercati Finanziari 2023-2024",
                             paste("Autocorrelogram of the S&P 500 Daily Adjusted Close Price - Training Set - from ", .(as.character(First_Day)), " to ", .(as.character(Last_Day)), sep="")))
link <- "https://finance.yahoo.com/quote/%5EGSPC?.tsrc=fin-srch"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points. Lags ", .(max_lag), ". Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("lags")
x_breaks_num <- max_lag
x_binwidth <- 1
x_breaks <- Aut_Fun_y$lag
x_labs <- format(x_breaks, scientific=FALSE)
Aut_Fun_y_plot <- ggplot(Aut_Fun_y_df, aes(x=lag, y=acf)) + 
  #  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), lwd=1, col="black") +
  #  geom_col(mapping=NULL, data=NULL, position="dodge", width=0.1, col="black", inherit.aes=TRUE) + 
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
Data_df <- GSPC_train_df
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
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Essentials of Time Series Analysis \u0040 Metodi Probabilistici e Statistici per i Mercati Finanziari 2023-2024",
                             paste("Partial Autocorrelogram of the S&P 500 Daily Adjusted Close Price - Training Set - from ", .(as.character(First_Day)), " to ", .(as.character(Last_Day)), sep="")))
link <- "https://finance.yahoo.com/quote/%5EGSPC?.tsrc=fin-srch"
subtitle_content <- bquote(paste("Training set length ", .(TrnS_length), " sample points. Lags ", .(max_lag), ".  Data by courtesy of Yahoo Finance US - ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("lags")
x_breaks_num <- max_lag
x_binwidth <- 1
x_breaks <- Aut_Fun_y$lag
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
# The partial autocorrelogram reveals the autocorrelation of the time series is essentially due to a strong correlation between the time 
# series and its one-lagged copy. Consequently, combining autocorrelogram and partial autocorrelogram provides visual evidence for a random
# walk component (unit root).
############################################################################################################################################
