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
library(graphics)
library(tibble)
library(dplyr)
library(tidyverse)
library("data.table")
library(reshape2)
library(ggplot2)
library(scales)
###############################################################################################################################################
library(readxl)
library(rlist)
library(numbers)
library(lubridate)
library(zoo)
library(xts)
library(TTR)
library(quantmod)
library(lattice)
library(leaps)
library(ltsa)
library(bestglm)
library(FitAR)
library(fGarch)
# https://cran.r-project.org/web/packages/fGarch/fGarch.pdf
library(rugarch) 
# https://cran.r-project.org/web/packages/rugarch/rugarch.pdf
# https://cran.r-project.org/web/packages/rugarch/vignettes/Introduction_to_the_rugarch_package.pdf

library(moments)
library(lmtest) 
library(fBasics)
library(nortest)
library(tseries)
library(strucchange)
library(broom)
library(rlang)
library(gridSVG)
library(grid)
###############################################################################################################################################
################################################################# Functions ###################################################################
###############################################################################################################################################
#
na.rm <- function(x){x <- as.vector(x[!is.na(as.vector(x))])}
#
###############################################################################################################################################
####################################### Create a data frame of data from US Department of Treasury ############################################
# From the website "https://home.treasury.gov/policy-issues/financing-the-government/interest-rate-statistics",
# we download the "Daily Treasure Par Yield Curve Rates" selecting the years from 2020 to 2023 as the "time period".
# We obtain the files "Daily Treasury Rates - 2020.csv", ..., "Daily Treasury Rates - 2023.csv".
# Hence, we transform the above files in data frames.
US_DTR_2020_df <- read.csv("Daily Treasury Rates - 2020.csv")
class(US_DTR_2020_df)
head(US_DTR_2020_df,15)
tail(US_DTR_2020_df,15)
US_DTR_2021_df <- read.csv("Daily Treasury Rates - 2021.csv")
class(US_DTR_2021_df)
head(US_DTR_2021_df,15)
tail(US_DTR_2021_df,15)
US_DTR_2022_df <- read.csv("Daily Treasury Rates - 2022.csv")
class(US_DTR_2022_df)
head(US_DTR_2022_df,15)
tail(US_DTR_2022_df,15)
US_DTR_2023_df <- read.csv("Daily Treasury Rates - 2023.csv")
class(US_DTR_2023_df)
head(US_DTR_2023_df,15)
tail(US_DTR_2023_df,15)
US_DTR_2024_df <- read.csv("Daily Treasury Rates - 2024.csv")
class(US_DTR_2024_df)
head(US_DTR_2024_df,15)
tail(US_DTR_2024_df,15)
# Note that in the above data frames, the temporal ordering of the rows, accounted by the column Date, is decreasing: the rows go from the most
# recent to the least recent, against the order of the row names. However, for our purposes, it is more convenient to dispose of data in 
# increasing temporal order (from the least recent to the most recent). Therefore, we invert the temporal order of the rows in the data frames.
US_DTR_2020_df <- US_DTR_2020_df[nrow(US_DTR_2020_df):1,]
head(US_DTR_2020_df,15)
tail(US_DTR_2020_df,15)
US_DTR_2021_df <- US_DTR_2021_df[nrow(US_DTR_2021_df):1,]
head(US_DTR_2021_df,15)
tail(US_DTR_2021_df,15)
US_DTR_2022_df <- US_DTR_2022_df[nrow(US_DTR_2022_df):1,]
head(US_DTR_2022_df,15)
tail(US_DTR_2022_df,15)
US_DTR_2023_df <- US_DTR_2023_df[nrow(US_DTR_2023_df):1,]
head(US_DTR_2023_df,15)
tail(US_DTR_2023_df,15)
US_DTR_2024_df <- US_DTR_2024_df[nrow(US_DTR_2024_df):1,]
head(US_DTR_2024_df,15)
tail(US_DTR_2024_df,15)
# Note also that the "four months daily treasury rate" is reported in the column *X4.Mo* only from the year 2022.
# More precisely, the "four months daily treasury rate" is reported from the day US_DTR_2022_df$Date[min(which(!is.na(US_DTR_2022_df$X4.Mo)))],
# as it clearly appears by observing the data frame *US_DTR_2022_df* in the vicinity of the above determined day.
# show(US_DTR_2022_df[(min(which(!is.na(US_DTR_2022_df$X4.Mo)))-5):(min(which(!is.na(US_DTR_2022_df$X4.Mo)))+5),])
# Therefore, with the goal of merging the different data frames into a single one, we add a *X4.Mo* column with *NA* entries to the data frames 
# *US_DTR_2020_df* and *US_DTR_2021_df*.
# library(tibble)
US_DTR_2020_df <- add_column(US_DTR_2020_df, X4.Mo=rep(NA, nrow(US_DTR_2020_df)), .after="X3.Mo")
head(US_DTR_2020_df)
tail(US_DTR_2020_df)
US_DTR_2021_df <- add_column(US_DTR_2021_df, X4.Mo=rep(NA, nrow(US_DTR_2021_df)), .after="X3.Mo")
head(US_DTR_2021_df)
tail(US_DTR_2021_df)
# Hence, we merge the data frames *US_DTR_2020_df*, ..., *US_DTR_2023_df* in a single data frame.
# library(dplyr)
US_DTR_2020_2024_df <- bind_rows(US_DTR_2020_df,US_DTR_2021_df,US_DTR_2022_df,US_DTR_2023_df,US_DTR_2024_df)
head(US_DTR_2020_2024_df)
tail(US_DTR_2020_2024_df)
#
# We check whether the Date column is in "Date" format. In case it is not, we change the format to "Date".
class(US_DTR_2020_2024_df$Date)
# "character"
US_DTR_2020_2024_df$Date <- as.Date(US_DTR_2020_2024_df$Date, format="%m/%d/%Y")
class(US_DTR_2020_2024_df$Date)
# "Date"
head(US_DTR_2020_2024_df,15)
tail(US_DTR_2020_2024_df,15)
#
# In the end, although unnecessary, we rename the columns to make them more similar to those of the original website.
# library(tidyverse)
US_DTR_2020_2024_df <- rename(US_DTR_2020_2024_df, Mo01=X1.Mo, Mo02=X2.Mo, Mo03=X3.Mo, Mo04=X4.Mo,
                              Mo06=X6.Mo, Yr01=X1.Yr, Yr02=X2.Yr, Yr03=X3.Yr, Yr05=X5.Yr, Yr07=X7.Yr,
                              Yr10=X10.Yr, Yr20=X20.Yr, Yr30=X30.Yr)
head(US_DTR_2020_2024_df)
tail(US_DTR_2020_2024_df)
# We save the data frame US_DTR_2020_2024_df as a RData file, remove it from the environment, and verify that it has been 
# removed.
save(US_DTR_2020_2024_df, file="US_DTR_2020_2024_df.RData")
# rm(US_DTR_2020_2024_df)
# head(US_DTR_2020_2024_df)
# load("US_DTR_2020_2024_df.RData")
# head(US_DTR_2020_2024_df)
#
###############################################################################################################################################
# To draw a plot of the Treasury Yield Curve Rates, we start with manipulating the data frame *US_DTR_2020_2024_df*.
# First, we extract some rows (e.g., from 2023-01-03 to 2024-02-29) from the data frame and rename the rows.
init_date  <- which(US_DTR_2020_2024_df$Date=="2023-01-03")
final_date <- which(US_DTR_2020_2024_df$Date=="2024-02-29")
selec_rows <- seq.int(from=init_date, to=final_date, by=1)
selec_rows_US_DTR_2020_2024_df <- US_DTR_2020_2024_df[selec_rows,]
show(selec_rows_US_DTR_2020_2024_df)
rownames(selec_rows_US_DTR_2020_2024_df) <- seq(from=1, to=nrow(selec_rows_US_DTR_2020_2024_df))
head(selec_rows_US_DTR_2020_2024_df)
tail(selec_rows_US_DTR_2020_2024_df)
#
save(selec_rows_US_DTR_2020_2024_df, file="selec_rows_US_DTR_2020_2024_df.RData")
# rm(selec_rows_US_DTR_2020_2024_df)
# show(selec_rows_US_DTR_2020_2024_df)
# load("selec_rows_US_DTR_2020_2024_df.RData")
# show(selec_rows_US_DTR_2020_2024_df)
#
# Second, we change the data frame in a data table.
# library("data.table")
selec_rows_US_DTR_2020_2024_tb <- setDT(selec_rows_US_DTR_2020_2024_df)
class(selec_rows_US_DTR_2020_2024_tb)
#  "data.table" "data.frame"
show(selec_rows_US_DTR_2020_2024_tb)
#
# Third, by the command *melt* we reshape the wide data frame to a long data frame.
# library(reshape2)
rsh_selec_rows_US_DTR_2020_2024_tb <- melt(selec_rows_US_DTR_2020_2024_tb, id.vars="Date")
class(rsh_selec_rows_US_DTR_2020_2024_tb)
# "data.frame"
# equivalently
# rsh_selec_rows_US_DTR_2020_2024_tb <- melt(selec_rows_US_DTR_2020_2024_tb, measure.vars=c(2:ncol(selec_rows_US_DTR_2020_2024_tb)))
show(rsh_selec_rows_US_DTR_2020_2024_tb[1:20,])
show(rsh_selec_rows_US_DTR_2020_2024_tb[(nrow(rsh_selec_rows_US_DTR_2020_2024_tb)-20):nrow(rsh_selec_rows_US_DTR_2020_2024_tb),])
nrow(rsh_selec_rows_US_DTR_2020_2024_tb)
# 3783
#
# In the end, we add an Index column to the data frame rsh_sel_US_DTR_2020_2024_tb
rsh_selec_rows_US_DTR_2020_2024_tb <- add_column(rsh_selec_rows_US_DTR_2020_2024_tb,
                                                 Index=rep(1:nrow(selec_rows_US_DTR_2020_2024_df), times=(ncol(selec_rows_US_DTR_2020_2024_df)-1)),
                                                 .before="Date")
show(rsh_selec_rows_US_DTR_2020_2024_tb[1:20,])
show(rsh_selec_rows_US_DTR_2020_2024_tb[(nrow(rsh_selec_rows_US_DTR_2020_2024_tb)-20):nrow(rsh_selec_rows_US_DTR_2020_2024_tb),])
#
# We save the rsh_selec_rows_US_DTR_2020_2024_tb data frame as a RData file, remove it from the environment and check its removal.
# 
save(rsh_selec_rows_US_DTR_2020_2024_tb, file="rsh_selec_rows_US_DTR_2020_2024_tb_2023_04_11.RData")
# rm(rsh_selec_rows_US_DTR_2020_2024_tb)
# head(rsh_selec_rows_US_DTR_2020_2024_tb)
#
# We load the data frame rsh_selec_rows_US_DTR_2020_2024_tb. Note that although we have saved the data frame with a different 
# name, after reloading it retains the same name it had before saving.
# load("rsh_selec_rows_US_DTR_2020_2024_tb_2023_04_11.RData")
# head(rsh_selec_rows_US_DTR_2020_2024_tb,10)
# tail(rsh_selec_rows_US_DTR_2020_2024_tb,10)
#
# Finally, We are in a position to draw a draft plot of the Daily Treasury Yield Curve Rates
# library(ggplot2)
Data_2023_2024_df <- rsh_selec_rows_US_DTR_2020_2024_tb
#
Data_2020_2024_2023_01_03_df <- Data_2023_2024_df[which(Data_2023_2024_df$Date >= "2023-01-01"  &  Data_2023_2024_df$Date <="2023-01-10"),]
Data_df <- Data_2020_2024_2023_01_03_df
Uniq_dates <- unique(Data_df$Date)
First_Day <- as.character(first(Uniq_dates))
Last_Day <- as.character(last(Uniq_dates))
length <- nrow(Data_df)/length(Uniq_dates)
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2023-2024", 
                             paste("Scatter Plots of U.S. Treasury Yield Curve Rates (business days from ", .(First_Day), " to ", .(Last_Day),")")))
link <- "https://home.treasury.gov/policy-issues/financing-the-government/interest-rate-statistics"
subtitle_content <- bquote(paste("path length ", .(length), " sample points. Data by courtesy of  U.S. Department of the Treasure  -  ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("expiration dates")
leg_labs <- as.character(Uniq_dates)
leg_vals <- rainbow(length(levels(factor(Data_df$Index))))
US_DTR_2023_01_03_10_Curve_Rate_lp <- ggplot(Data_df, aes(x=variable, y=value, group=factor(Index))) + 
  geom_line(aes(color=factor(Index)), linewidth=0.8, linetype="solid") +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("time to maturity") + ylab("ret. rates") +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_vals) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=0, vjust=1),
        legend.key.width = unit(0.80,"cm"), legend.position="right")
plot(US_DTR_2023_01_03_10_Curve_Rate_lp)
#
Data_2020_2024_2024_01_03_df <- Data_2023_2024_df[which(Data_2023_2024_df$Date >= "2024-01-01"  &  Data_2023_2024_df$Date <="2024-01-10"),]
Data_df <- Data_2020_2024_2024_01_03_df
Uniq_dates <- unique(Data_df$Date)
First_Day <- as.character(first(Uniq_dates))
Last_Day <- as.character(last(Uniq_dates))
length <- nrow(Data_df)/length(Uniq_dates)
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2023-2024", 
                             paste("Scatter Plots of U.S. Treasury Yield Curve Rates (business days from ", .(First_Day), " to ", .(Last_Day),")")))
link <- "https://home.treasury.gov/policy-issues/financing-the-government/interest-rate-statistics"
subtitle_content <- bquote(paste("path length ", .(length), " sample points. Data by courtesy of  U.S. Department of the Treasure  -  ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("expiration dates")
leg_labs <- as.character(Uniq_dates)
leg_vals <- rainbow(length(levels(factor(Data_df$Index))))
US_DTR_2024_01_03_10_Curve_Rate_lp <- ggplot(Data_df, aes(x=variable, y=value, group=factor(Index))) + 
  geom_line(aes(color=factor(Index)), linewidth=0.8, linetype="solid") +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("time to maturity") + ylab("ret. rates") +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_vals) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=0, vjust=1),
        legend.key.width = unit(0.80,"cm"), legend.position="right")
plot(US_DTR_2024_01_03_10_Curve_Rate_lp)
###############################################################################################################################################
# We compare the Treasury Yield Curve Rates in the periods 2003-01/03-10 and 2024-01-02/10 plotted above with the curve at the beginning of 
# 2021, and 2022.
#
# First, we manipulate the data from January 4th to January 15th 2021) from the data frame.
init_date  <- which(US_DTR_2020_2024_df$Date=="2021-01-04")
final_date <- which(US_DTR_2020_2024_df$Date=="2021-01-15")
selec_rows <- seq.int(from=init_date, to=final_date, by=1)
selec_rows_US_DTR_2020_2024_df <- US_DTR_2020_2024_df[selec_rows,]
show(selec_rows_US_DTR_2020_2024_df)
rownames(selec_rows_US_DTR_2020_2024_df) <- seq(from=1, to=nrow(selec_rows_US_DTR_2020_2024_df))
head(selec_rows_US_DTR_2020_2024_df)
#
# We change the data frame in a table.
# library("data.table")
selec_rows_US_DTR_2020_2024_tb <- setDT(selec_rows_US_DTR_2020_2024_df)   
class(selec_rows_US_DTR_2020_2024_tb)
show(selec_rows_US_DTR_2020_2024_tb)
#
# We reshape the wide data frame to a long data frame.
# library(reshape2)
rsh_selec_rows_US_DTR_2020_2024_tb <- melt(selec_rows_US_DTR_2020_2024_tb, id.vars="Date")
# equivalently
# rsh_selec_rows_US_DTR_2020_2024_tb <- melt(selec_rows_US_DTR_2020_2024_tb, measure.vars=c(2:ncol(selec_rows_US_DTR_2020_2024_tb)))
show(rsh_selec_rows_US_DTR_2020_2024_tb[1:20,])
show(rsh_selec_rows_US_DTR_2020_2024_tb[(nrow(rsh_selec_rows_US_DTR_2020_2024_tb)-20):nrow(rsh_selec_rows_US_DTR_2020_2024_tb),])
nrow(rsh_selec_rows_US_DTR_2020_2024_tb)
# 130
# In the end, we add an Index column to the data frame rsh_sel_US_DTR_2020_2024_tb
rsh_selec_rows_US_DTR_2020_2024_tb <- add_column(rsh_selec_rows_US_DTR_2020_2024_tb,
                                                 Index=rep(1:nrow(selec_rows_US_DTR_2020_2024_df), times=(ncol(selec_rows_US_DTR_2020_2024_df)-1)),
                                                 .before="Date")
show(rsh_selec_rows_US_DTR_2020_2024_tb[1:20,])
show(rsh_selec_rows_US_DTR_2020_2024_tb[(nrow(rsh_selec_rows_US_DTR_2020_2024_tb)-20):nrow(rsh_selec_rows_US_DTR_2020_2024_tb),])
#
save(rsh_selec_rows_US_DTR_2020_2024_tb, file="rsh_selec_rows_US_DTR_2020_2024_tb_2021_01_04.RData")
# rm(rsh_selec_rows_US_DTR_2020_2024_tb)
# head(rsh_selec_rows_US_DTR_2020_2024_tb)
# load("rsh_selec_rows_US_DTR_2020_2024_tb_2021_01_04.RData")
# head(rsh_selec_rows_US_DTR_2020_2024_tb)
#
# We draw a plot of the Daily Treasury Yield Curve Rates
# library(ggplot2)
Data_df <- rsh_selec_rows_US_DTR_2020_2024_tb
Uniq_dates <- unique(Data_df$Date)
First_Day <- as.character(first(Uniq_dates))
Last_Day <- as.character(last(Uniq_dates))
length <- nrow(Data_df)/length(Uniq_dates)
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2024-2024", 
                             paste("Scatter Plots of U.S. Treasury Yield Curve Rates (business days from ", .(First_Day), " to ", .(Last_Day),")")))
link <- "https://home.treasury.gov/policy-issues/financing-the-government/interest-rate-statistics"
subtitle_content <- bquote(paste("path length ", .(length), " sample points. Data by courtesy of  U.S. Department of the Treasure  -  ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("expiration dates")
leg_labs <- as.character(Uniq_dates)
# leg_vals <- levels(factor(Data_df$Index))
leg_vals <- rainbow(length(levels(factor(Data_df$Index))))[as.numeric(levels(factor(Data_df$Index)))]
US_DTR_01_04_01_15_2021_Curve_Rate_lp <- ggplot(Data_df, aes(x=variable, y=value, group=factor(Index))) + 
  geom_line(aes(color=factor(Index)), linewidth=0.8, linetype="solid") +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("time to maturity") + ylab("ret. rates") +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_vals) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=0, vjust=1),
        legend.key.width = unit(0.80,"cm"), legend.position="right")
plot(US_DTR_01_04_01_15_2021_Curve_Rate_lp)
#
# Interpolating the NA value, the plot can be drawn as follows
show(rsh_selec_rows_US_DTR_2020_2024_tb[20:52,])
as.vector(which(is.na(Data_df$value)))
# [1] 31 32 33 34 35 36 37 38 39 40
NA_length <- length(as.vector(which(is.na(Data_df$value))))
# 10
#
(as.vector(which(is.na(Data_df$value)))-NA_length)[1]
(as.vector(which(is.na(Data_df$value)))+NA_length)[1]
#
(as.vector(which(is.na(Data_df$value)))-NA_length)[2]
(as.vector(which(is.na(Data_df$value)))+NA_length)[2]
#
# ...
#
Data_df$variable[(as.vector(which(is.na(Data_df$value)))- NA_length)[1]]
Data_df$value[(as.vector(which(is.na(Data_df$value)))-NA_length)[1]]
Data_df$variable[as.vector(which(is.na(Data_df$value))+NA_length)[1]]
Data_df$value[as.vector(which(is.na(Data_df$value))+NA_length)[1]]
# ...
Data_df$variable[(as.vector(which(is.na(Data_df$value)))- NA_length)[6]]
Data_df$value[(as.vector(which(is.na(Data_df$value)))-NA_length)[6]]
Data_df$variable[as.vector(which(is.na(Data_df$value))+NA_length)[6]]
Data_df$value[as.vector(which(is.na(Data_df$value))+NA_length)[6]]
# ...
#
Data_df <- rsh_selec_rows_US_DTR_2020_2024_tb
Uniq_dates <- unique(Data_df$Date)
First_Day <- as.character(first(Uniq_dates))
Last_Day <- as.character(last(Uniq_dates))
length <- nrow(Data_df)/length(Uniq_dates)
NA_length <- length(as.vector(which(is.na(Data_df$value))))
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2024-2024", 
                             paste("Scatter Plots of U.S. Treasury Yield Curve Rates (business days from ", .(First_Day), " to ", .(Last_Day),")")))
link <- "https://home.treasury.gov/policy-issues/financing-the-government/interest-rate-statistics"
subtitle_content <- bquote(paste("path length ", .(length), " sample points. Data by courtesy of  U.S. Department of the Treasure  -  ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("expiration dates")
leg_labs <- as.character(Uniq_dates)
# leg_vals <- levels(factor(Data_df$Index))
leg_vals <- rainbow(length(levels(factor(Data_df$Index))))[as.numeric(levels(factor(Data_df$Index)))]
US_DTR_01_04_01_15_2021_Interp_Curve_Rate_lp <- ggplot(Data_df, aes(x=variable, y=value, group=factor(Index))) + 
  geom_line(aes(color=factor(Index)), linewidth=0.8, linetype="solid") +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[1]],
                 y=value[(as.vector(which(is.na(value)))-NA_length)[1]],
                 xend=variable[(as.vector(which(is.na(value)))+NA_length)[1]],
                 yend=value[(as.vector(which(is.na(value)))+NA_length)[1]],
                 color=factor(Index)[1]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[2]],
                   y=value[(as.vector(which(is.na(value)))-NA_length)[2]],
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[2]],
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[2]],
                   color=factor(Index)[2]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[3]],
                   y=value[(as.vector(which(is.na(value)))-NA_length)[3]],
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[3]],
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[3]],
                   color=factor(Index)[3]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[4]],
                   y=value[(as.vector(which(is.na(value)))-NA_length)[4]],
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[4]],
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[4]],
                   color=factor(Index)[4]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[5]],
                   y=value[(as.vector(which(is.na(value)))-NA_length)[5]],
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[5]],
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[5]],
                   color=factor(Index)[5]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[6]],
                   y=value[(as.vector(which(is.na(value)))-NA_length)[6]],
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[6]],
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[6]],
                   color=factor(Index)[6]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[7]],
                   y=value[(as.vector(which(is.na(value)))-NA_length)[7]],
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[7]],
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[7]],
                   color=factor(Index)[7]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[8]],
                   y=value[(as.vector(which(is.na(value)))-NA_length)[8]],
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[8]],
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[8]],
                   color=factor(Index)[8]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[9]],
                   y=value[(as.vector(which(is.na(value)))-NA_length)[9]],
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[9]],
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[9]],
                   color=factor(Index)[9]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[10]],
                   y=value[(as.vector(which(is.na(value)))-NA_length)[10]],
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[10]],
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[10]],
                   color=factor(Index)[10]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("time to maturity") + ylab("ret. rates") +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_vals) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=0, vjust=1),
        legend.key.width = unit(0.80,"cm"), legend.position="right")
plot(US_DTR_01_04_01_15_2021_Interp_Curve_Rate_lp)
###############################################################################################################################################
# Second, we manipulate the data from January 3rd to January 14th 2022) from the data frame.
init_date  <- which(US_DTR_2020_2024_df$Date=="2022-01-03")
final_date <- which(US_DTR_2020_2024_df$Date=="2022-01-14")
selec_rows <- seq.int(from=init_date, to=final_date, by=1)
selec_rows_US_DTR_2020_2024_df <- US_DTR_2020_2024_df[selec_rows,]
show(selec_rows_US_DTR_2020_2024_df)
rownames(selec_rows_US_DTR_2020_2024_df) <- seq(from=1, to=nrow(selec_rows_US_DTR_2020_2024_df))
#
# We change the data frame in a table.
# library("data.table")
selec_rows_US_DTR_2020_2024_tb <- setDT(selec_rows_US_DTR_2020_2024_df)   
class(selec_rows_US_DTR_2020_2024_tb)
show(selec_rows_US_DTR_2020_2024_tb)
#
# We reshape the wide data frame to a long data frame.
# library(reshape2)
rsh_selec_rows_US_DTR_2020_2024_tb <- melt(selec_rows_US_DTR_2020_2024_tb, id.vars="Date")
# equivalently
# rsh_selec_rows_US_DTR_2020_2024_tb <- melt(selec_rows_US_DTR_2020_2024_tb, measure.vars=c(2:ncol(selec_rows_US_DTR_2020_2024_tb)))
show(rsh_selec_rows_US_DTR_2020_2024_tb[1:20,])
show(rsh_selec_rows_US_DTR_2020_2024_tb[(nrow(rsh_selec_rows_US_DTR_2020_2024_tb)-20):nrow(rsh_selec_rows_US_DTR_2020_2024_tb),])
nrow(rsh_selec_rows_US_DTR_2020_2024_tb)
# 130
#
# We add an Index column to the data frame rsh_sel_US_DTR_2020_2024_tb
# library(tibble)
rsh_selec_rows_US_DTR_2020_2024_tb <- add_column(rsh_selec_rows_US_DTR_2020_2024_tb,
                                                 Index=rep(1:nrow(selec_rows_US_DTR_2020_2024_df), times=(ncol(selec_rows_US_DTR_2020_2024_df)-1)),
                                                 .before="Date")
show(rsh_selec_rows_US_DTR_2020_2024_tb[1:20,])
show(rsh_selec_rows_US_DTR_2020_2024_tb[(nrow(rsh_selec_rows_US_DTR_2020_2024_tb)-20):nrow(rsh_selec_rows_US_DTR_2020_2024_tb),])
#
save(rsh_selec_rows_US_DTR_2020_2024_tb, file="rsh_selec_rows_US_DTR_2020_2024_tb__2022-01-14.RData")
# rm(rsh_selec_rows_US_DTR_2020_2024_tb)
# head(rsh_selec_rows_US_DTR_2020_2024_tb)
# load("rsh_selec_rows_US_DTR_2020_2024_tb__2022-01-14.RData")
head(rsh_selec_rows_US_DTR_2020_2024_tb)
tail(rsh_selec_rows_US_DTR_2020_2024_tb)
#
# We draw a plot of the Daily Treasury Yield Curve Rates
# library(ggplot2)
Data_df <- rsh_selec_rows_US_DTR_2020_2024_tb
Uniq_dates <- unique(Data_df$Date)
First_Day <- as.character(first(Uniq_dates))
Last_Day <- as.character(last(Uniq_dates))
length <- nrow(Data_df)/length(Uniq_dates)
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2024-2024", 
                             paste("Scatter Plots of U.S. Treasury Yield Curve Rates (business days from ", .(First_Day), " to ", .(Last_Day),")")))
link <- "https://home.treasury.gov/policy-issues/financing-the-government/interest-rate-statistics"
subtitle_content <- bquote(paste("path length ", .(length), " sample points. Data by courtesy of  U.S. Department of the Treasure  -  ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("expiration dates")
leg_labs <- as.character(Uniq_dates)
# leg_vals <- levels(factor(Data_df$Index))
leg_vals <- rainbow(length(levels(factor(Data_df$Index))))[as.numeric(levels(factor(Data_df$Index)))]
US_DTR_01_03_01_14_2022_Curve_Rate_lp <- ggplot(Data_df, aes(x=variable, y=value, group=factor(Index))) + 
  geom_line(aes(color=factor(Index)), linewidth=0.8, linetype="solid") +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("time to maturity") + ylab("ret. rates") +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_vals) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=0, vjust=1),
        legend.key.width = unit(0.80,"cm"), legend.position="right")
plot(US_DTR_01_03_01_14_2022_Curve_Rate_lp)
#
# Interpolating the NA value we the plot can be drawn as follows
show(rsh_selec_rows_US_DTR_2020_2024_tb[20:52,])
as.vector(which(is.na(Data_df$value)))
NA_length <- length(as.vector(which(is.na(Data_df$value))))
#
(as.vector(which(is.na(Data_df$value)))-NA_length)[1]
(as.vector(which(is.na(Data_df$value)))+NA_length)[1]
#
(as.vector(which(is.na(Data_df$value)))-NA_length)[2]
(as.vector(which(is.na(Data_df$value)))+NA_length)[2]
#
# ...
#
Data_df$variable[(as.vector(which(is.na(Data_df$value)))- NA_length)[1]]
Data_df$value[(as.vector(which(is.na(Data_df$value)))-NA_length)[1]]
Data_df$variable[as.vector(which(is.na(Data_df$value))+NA_length)[1]]
Data_df$value[as.vector(which(is.na(Data_df$value))+NA_length)[1]]
# ...
Data_df$variable[(as.vector(which(is.na(Data_df$value)))- NA_length)[6]]
Data_df$value[(as.vector(which(is.na(Data_df$value)))-NA_length)[6]]
Data_df$variable[as.vector(which(is.na(Data_df$value))+NA_length)[6]]
Data_df$value[as.vector(which(is.na(Data_df$value))+NA_length)[6]]
# ...
#
Data_df <- rsh_selec_rows_US_DTR_2020_2024_tb
Uniq_dates <- unique(Data_df$Date)
First_Day <- as.character(first(Uniq_dates))
Last_Day <- as.character(last(Uniq_dates))
length <- nrow(Data_df)/length(Uniq_dates)
NA_length <- length(as.vector(which(is.na(Data_df$value))))
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2024-2024", 
                             paste("Scatter Plots of U.S. Treasury Yield Curve Rates (business days from ", .(First_Day), " to ", .(Last_Day),")")))
link <- "https://home.treasury.gov/policy-issues/financing-the-government/interest-rate-statistics"
subtitle_content <- bquote(paste("path length ", .(length), " sample points. Data by courtesy of  U.S. Department of the Treasure  -  ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("expiration dates")
leg_labs <- as.character(Uniq_dates)
# leg_vals <- levels(factor(Data_df$Index))
leg_vals <- rainbow(length(levels(factor(Data_df$Index))))[as.numeric(levels(factor(Data_df$Index)))]
US_DTR_01_03_01_14_2022_Interp_Curve_Rate_lp <- ggplot(Data_df, aes(x=variable, y=value, group=factor(Index))) + 
  geom_line(aes(color=factor(Index)), linewidth=0.8, linetype="solid") +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[1]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[1]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[1]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[1]], 
                   color=factor(Index)[1]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[2]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[2]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[2]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[2]], 
                   color=factor(Index)[2]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[3]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[3]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[3]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[3]], 
                   color=factor(Index)[3]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[4]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[4]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[4]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[4]], 
                   color=factor(Index)[4]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[5]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[5]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[5]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[5]], 
                   color=factor(Index)[5]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[6]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[6]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[6]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[6]], 
                   color=factor(Index)[6]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[7]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[7]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[7]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[7]], 
                   color=factor(Index)[7]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[8]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[8]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[8]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[8]], 
                   color=factor(Index)[8]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[9]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[9]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[9]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[9]], 
                   color=factor(Index)[9]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[10]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[10]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[10]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[10]], 
                   color=factor(Index)[10]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("time to maturity") + ylab("ret. rates") +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_vals) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=0, vjust=1),
        legend.key.width = unit(0.80,"cm"), legend.position="right")
plot(US_DTR_01_03_01_14_2022_Interp_Curve_Rate_lp)
###############################################################################################################################################
###############################################################################################################################################
# https://www.usbank.com/investing/financial-perspectives/market-news/treasury-yields-invert-as-investors-weigh-risk-of-recession.html#:~:text=When%20coupon%20payments%20on%20shorter,2022%20and%20continues%20in%202023.
# https://www.reuters.com/markets/us/deeply-inverted-us-curve-flashed-bank-danger-months-2023-03-14/
# https://www.wsj.com/livecoverage/stock-market-news-today-03-13-2023/card/yield-curve-inversion-unwinds-zEqHtaxQAoKpcXd1J6mF?mod=article_inline
# https://www.forbes.com/sites/johntobey/2023/02/28/this-inverted-yield-curve-is-not-forecasting-a-recession/?sh=2f471c187450
# https://ig.ft.com/the-yield-curve-explained/
# https://fred.stlouisfed.org/series/T10Y2Y
# https://www.statista.com/statistics/1058454/yield-curve-usa/
###############################################################################################################################################
###############################################################################################################################################
# Now, we try to compute the Treasury return rates from the prices of Market Based Bills.
# https://www.treasurydirect.gov/GA-FI/FedInvest/selectSecurityPriceDate
# We build a data frame with data from the "securityprice-2023-01-03.csv" file.
US_SP_2023_01_03_df <- read.csv("securityprice-2023-01-03.csv", header=FALSE)
show(US_SP_2023_01_03_df[1:15,])
# We rename the columns according to the terminology in https://www.treasurydirect.gov/GA-FI/FedInvest/selectSecurityPriceDate.
US_SP_2023_01_03_df <- rename(US_SP_2023_01_03_df, CUSIP=V1, Security.Type=V2, Rate=V3, Maturity.Date=V4,
                              Call.Date=V5, Buy=V6, Sell=V7, End.of.Day=V8)
head(US_SP_2023_01_03_df)
# We check whether the Maturity.Date column is in "Date" format. In case it is not, we change the format to "Date".
class(US_SP_2023_01_03_df$Maturity.Date)
# "character"
US_SP_2023_01_03_df$Maturity.Date <- as.Date(US_SP_2023_01_03_df$Maturity.Date,  format="%m/%d/%Y")
class(US_SP_2023_01_03_df$Maturity.Date)
# "Date"
show(US_SP_2023_01_03_df[1:15,])
# We add an Index column to help in plots.
US_SP_2023_01_03_df <- add_column(US_SP_2023_01_03_df, Index=1:nrow(US_SP_2023_01_03_df), .before="CUSIP")
show(US_SP_2023_01_03_df[1:15,])
# We add a column *Days.to.Maturity*, which accounts for the number of days from January 1st, 2023, to the Maturity Date.
US_SP_2023_01_03_df <- add_column(US_SP_2023_01_03_df,
                                  Days.to.Maturity=as.vector(difftime(US_SP_2023_01_03_df$Maturity.Date, as.Date(as.character("2023-01-03")), units="days")),
                                  .after="Maturity.Date")
show(US_SP_2023_01_03_df[1:15,])
# We also add the column *Months.to.Maturity* [resp. *Years.to.Maturity*], which accounts for the number of months
# [resp. years] from January 1st, 2023, to the Maturity Date.
US_SP_2023_01_03_df <- add_column(US_SP_2023_01_03_df,
                                  Months.to.Maturity=as.vector(US_SP_2023_01_03_df$Days.to.Maturity/30.4369),
                                  Years.to.Maturity=as.vector(US_SP_2023_01_03_df$Days.to.Maturity/365.2425),
                                  .after="Days.to.Maturity")
show(US_SP_2023_01_03_df[1:15,])
#
# We restrict ourselves to consider only MARKET BASED BILLS.
US_SP_BILLS_2023_01_03_df <- subset(US_SP_2023_01_03_df, US_SP_2023_01_03_df$Security.Type=="MARKET BASED BILL")
head(US_SP_BILLS_2023_01_03_df)
tail(US_SP_BILLS_2023_01_03_df)
#
# Equivalently
# US_SP_2023_01_03_Rate_0_df <- subset(US_SP_2023_01_03_df, US_SP_2023_01_03_df$Rate==0)
# head(US_SP_2023_01_03_Rate_0_df)
# tail(US_SP_2023_01_03_Rate_0_df)
#
# We add a column *Ret.Rate.at.Maturity* and *Perc.Ret.Rate.at.Maturity*
US_SP_BILLS_2023_01_03_df <- add_column(US_SP_BILLS_2023_01_03_df,
                                  Ret.Rate.at.Maturity=(100-US_SP_BILLS_2023_01_03_df$End.of.Day)/US_SP_BILLS_2023_01_03_df$End.of.Day,
                                  Perc.Ret.Rate.at.Maturity=label_percent(accuracy = 0.001)((100-US_SP_BILLS_2023_01_03_df$End.of.Day)/US_SP_BILLS_2023_01_03_df$End.of.Day),
                                  .after="End.of.Day")
show(US_SP_BILLS_2023_01_03_df[1:15,])
#
# We compute the annual rate of return according to the formulas
# (1+r_a)^t=1+r_t; r_a=(1+r_t)^(1/t)-1
# where r_a=annual rate of return, t=time to maturity (in years), r_t=rate of return in the period t.
# or
# (1+r_a)^(t/365.2425)=1+r_t; r_a=(1+r_t)^(365.2425/t)-1
# where r_a=annual rate of return, t=time to maturity (in days), r_t=rate of return in the period t.
# 
# We also compute the annual continuously compound rate of return according to the formula
# 1+r_a=exp(\rho_a)
# or
# \rho_a=log(1+r_a)
#
Ann.Ret.Rate_01 <- (1+US_SP_BILLS_2023_01_03_df$Ret.Rate.at.Maturity)^(1/US_SP_BILLS_2023_01_03_df$Years.to.Maturity)-1
show(Ann.Ret.Rate_01)
Ann.Ret.Rate_02 <- (1+US_SP_BILLS_2023_01_03_df$Ret.Rate.at.Maturity)^(365.2425/US_SP_BILLS_2023_01_03_df$Days.to.Maturity)-1
show(Ann.Ret.Rate_02)
Ann.Ret.Rate_01==Ann.Ret.Rate_02
# [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
# [29] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
identical(Ann.Ret.Rate_01,Ann.Ret.Rate_02)
# [1] TRUE
#
Ann.Cont.Comp.Ret.Rate <- log(1+Ann.Ret.Rate_01)
show(Ann.Cont.Comp.Ret.Rate)
#
# We add the columns *Ann.Ret.Rate*, *Perc.Ann.Ret.Rate*, *Ann.Cont.Comp.Ret.Rate*, and *Perc.Ann.Cont.Comp.Ret.Rate*
US_SP_BILLS_2023_01_03_df <- add_column(US_SP_BILLS_2023_01_03_df,
                                  Ann.Ret.Rate=Ann.Ret.Rate_01, Perc.Ann.Ret.Rate=label_percent(accuracy = 0.001)(Ann.Ret.Rate_01),
                                  Ann.Cont.Comp.Ret.Rate=Ann.Cont.Comp.Ret.Rate, Perc.Ann.Cont.Comp.Ret.Rate = label_percent(accuracy = 0.001)(Ann.Cont.Comp.Ret.Rate),
                                  .after="Perc.Ret.Rate.at.Maturity")
show(US_SP_BILLS_2023_01_03_df[1:15,])
#
save(US_SP_BILLS_2023_01_03_df, file="US_SP_BILLS_2023_01_03_df.RData")
# rm(US_SP_BILLS_2023_01_03_df)
# head(US_SP_BILLS_2023_01_03_df)
# load("US_SP_BILLS_2023_01_03_df.RData")
# head(US_SP_BILLS_2023_01_03_df)
# tail(US_SP_BILLS_2023_01_03_df)
#
# We draw a plot of the U.S. Market Based Bill Par Yield Curve Rates on Jan, 3rd, 2023.
# library(ggplot2)
Data_df <- US_SP_BILLS_2023_01_03_df
DS_length <- nrow(Data_df)
First_Day <- as.character(Data_df$Maturity.Date[1])
Last_Day <- as.character(last(Data_df$Maturity.Date))
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2024-2024", 
                             paste("Line Plot of the U.S. Market Based Bill Par Yield Curve Rates on Jan, 3rd, 2023. Maturities from ", .(First_Day), " to ", .(Last_Day))))
link <- "https://www.treasurydirect.gov/GA-FI/FedInvest/selectSecurityPriceDate"
subtitle_content <- bquote(paste("path length ", .(DS_length), " sample points. Data by courtesy of  FedInvest, U.S. Department of the Treasure  -  ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("maturity dates (from Jan, 3rd, 2023)")
# To obtain the sub-multiples of the length of the data set as a hint on the number of breaks to use
# library(numbers)
# primeFactors(DS_length)
x_breaks_num <- 10
x_breaks_low <- first(Data_df$Index)
x_breaks_up <- last(Data_df$Index)
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Maturity.Date[x_breaks])
J <- 0.5
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("annual cont. comp. ret. rate (from Jan, 3rd, 2023 to maturity date)")
y_breaks_num <- 10
y_max <- max(na.rm(as.numeric(sub("%","", Data_df$Perc.Ann.Cont.Comp.Ret.Rate))))
y_min <- min(na.rm(as.numeric(sub("%","", Data_df$Perc.Ann.Cont.Comp.Ret.Rate))))
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor(y_min/y_binwidth)*y_binwidth
y_breaks_up <- ceiling(y_max/y_binwidth)*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- paste(format(y_breaks, scientific=FALSE),"%",sep="")
K <- 0
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
line_col_b <- bquote("annual cont. comp. ret. rate")
leg_line_labs   <- c(line_col_b)
leg_line_cols   <- c("line_col_b"="blue")
leg_line_breaks <- c("line_col_b")
US_SP_2023_01_03_BILLS_Yield_Rate_lp <- ggplot(Data_df, aes(x=Index)) + 
  geom_line(aes(y=as.numeric(sub("%","", Perc.Ann.Cont.Comp.Ret.Rate)), color="line_col_b"), linewidth=0.8, linetype="solid") +
  scale_x_continuous(breaks=x_breaks, labels=x_labs, limits=x_lims) +
  scale_y_continuous(breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis= sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("maturity dates (from Jan, 3rd, 2023)") + 
  ylab("annual cont. comp. ret. rate (from Jan, 3rd, 2023 to maturity date)") +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="Legend", labels=leg_line_labs, values=leg_line_cols, breaks=leg_line_breaks) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x=element_text(color="black", size=10, face="italic", angle=0, vjust=0),
        axis.text.y=element_text(color="black", size=11, face="italic", angle=0, vjust=0),
        axis.title.x=element_text(color="black", size=13, face="bold.italic", angle=0, vjust=-2),
        axis.title.y=element_text(color="black", size=13, face="bold.italic", angle=90, vjust=0),
        legend.key.width = unit(0.80,"cm"), legend.position="bottom")
plot(US_SP_2023_01_03_BILLS_Yield_Rate_lp)
#
###############################################################################################################################################
# Still, we compute the Treasury par yield curve rates from the prices of Market Based Bills.
# https://www.treasurydirect.gov/GA-FI/FedInvest/selectSecurityPriceDate
# We build a data frame with data from the "securityprice-2023-04-11.csv" file.
US_SP_2023_04_11_df <- read.csv("securityprice-2023-04-11.csv", header=FALSE)
show(US_SP_2023_04_11_df[1:15,])
# We rename the columns according to the terminology in https://www.treasurydirect.gov/GA-FI/FedInvest/selectSecurityPriceDate.
US_SP_2023_04_11_df <- rename(US_SP_2023_04_11_df, CUSIP=V1, Security.Type=V2, Rate=V3, Maturity.Date=V4,
                              Call.Date=V5, Buy=V6, Sell=V7, End.of.Day=V8)
head(US_SP_2023_04_11_df)
# We check whether the Maturity.Date column is in "Date" format. In case it is not, we change the format to "Date".
class(US_SP_2023_04_11_df$Maturity.Date)
US_SP_2023_04_11_df$Maturity.Date <- as.Date(US_SP_2023_04_11_df$Maturity.Date,  format="%m/%d/%Y")
show(US_SP_2023_04_11_df[1:15,])
class(US_SP_2023_04_11_df$Maturity.Date)
# We add an Index column to help in plots.
US_SP_2023_04_11_df <- add_column(US_SP_2023_04_11_df, Index=1:nrow(US_SP_2023_04_11_df), .before="CUSIP")
show(US_SP_2023_04_11_df[1:15,])
# We add a column *Days.to.Maturity*, which accounts for the number of days from January 1st, 2023, to the Maturity Date
US_SP_2023_04_11_df <- add_column(US_SP_2023_04_11_df, 
                                  Days.to.Maturity=as.vector(difftime(US_SP_2023_04_11_df$Maturity.Date, as.Date(as.character("2023-04-11")), units="days")), 
                                  .after="Maturity.Date")
show(US_SP_2023_04_11_df[1:15,])
# We also add the column *Months.to.Maturity* [resp. *Years.to.Maturity*], which accounts for the number of months 
# [resp. years] from January 1st, 2023, to the Maturity Date.
US_SP_2023_04_11_df <- add_column(US_SP_2023_04_11_df, 
                                  Months.to.Maturity=as.vector(US_SP_2023_04_11_df$Days.to.Maturity/30.4369),
                                  Years.to.Maturity=as.vector(US_SP_2023_04_11_df$Days.to.Maturity/365.2425),
                                  .after="Days.to.Maturity")
show(US_SP_2023_04_11_df[1:15,])
#
# We restrict ourselves to consider only MARKET BASED BILL.
US_SP_BILLS_2023_04_11_df <- subset(US_SP_2023_04_11_df, US_SP_2023_04_11_df$Security.Type=="MARKET BASED BILL")
head(US_SP_BILLS_2023_04_11_df)
tail(US_SP_BILLS_2023_04_11_df)
#
# Equivalently
# US_SP_2023_04_11_Rate_0_df <- subset(US_SP_2023_04_11_df, US_SP_2023_04_11_df$Rate==0)
# head(US_SP_2023_04_11_Rate_0_df)
# tail(US_SP_2023_04_11_Rate_0_df)
#
# We add a column *Ret.Rate.at.Maturity* and *Perc.Ret.Rate.at.Maturity*
US_SP_BILLS_2023_04_11_df <- add_column(US_SP_BILLS_2023_04_11_df, 
                                  Ret.Rate.at.Maturity=(100-US_SP_BILLS_2023_04_11_df$End.of.Day)/US_SP_BILLS_2023_04_11_df$End.of.Day,
                                  Perc.Ret.Rate.at.Maturity=label_percent(accuracy = 0.001)((100-US_SP_BILLS_2023_04_11_df$End.of.Day)/US_SP_BILLS_2023_04_11_df$End.of.Day),
                                  .after="End.of.Day")
show(US_SP_BILLS_2023_04_11_df[1:15,])
#
# We compute the annual rate of return according to the formulas
# (1+r_a)^t = 1+r_t; r_a = (1+r_t)^(1/t)-1
# where r_a=annual rate of return, t=time to maturity (in years), r_t=rate of return in the period t.
# or 
# (1+r_a)^(t/365.2425) = 1+r_t; r_a = (1+r_t)^(365.2425/t)-1
# where r_a=annual rate of return, t=time to maturity (in days), r_t=rate of return in the period t.
#
# We also compute the annual continuously compound rate of return according to the formula
# 1+r_a=exp(ro_a)
# or
# ro_a=log(1+r_a)
#
Ann.Ret.Rate_01 <- (1+US_SP_BILLS_2023_04_11_df$Ret.Rate.at.Maturity)^(1/US_SP_BILLS_2023_04_11_df$Years.to.Maturity)-1
show(Ann.Ret.Rate_01)
Ann.Ret.Rate_02 <- (1+US_SP_BILLS_2023_04_11_df$Ret.Rate.at.Maturity)^(365.2425/US_SP_BILLS_2023_04_11_df$Days.to.Maturity)-1
show(Ann.Ret.Rate_02)
Ann.Ret.Rate_01==Ann.Ret.Rate_02
# [1]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [24]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [47]  TRUE  TRUE  TRUE  TRUE
round(Ann.Ret.Rate_01,16)==round(Ann.Ret.Rate_02,16)
# [1]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [24]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [47]  TRUE  TRUE  TRUE  TRUE
round(Ann.Ret.Rate_01,15)==round(Ann.Ret.Rate_02,15)
# [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
# [29] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
identical(round(Ann.Ret.Rate_01,15),round(Ann.Ret.Rate_02,15))
# [1] TRUE
#
Ann.Cont.Comp.Ret.Rate <- log(1+Ann.Ret.Rate_01)
show(Ann.Cont.Comp.Ret.Rate)
#
# We add the columns *Ann.Ret.Rate*, *Perc.Ann.Ret.Rate*, *Ann.Cont.Comp.Ret.Rate*, and *Perc.Ann.Cont.Comp.Ret.Rate*
US_SP_BILLS_2023_04_11_df <- add_column(US_SP_BILLS_2023_04_11_df,
                                        Ann.Ret.Rate=round(Ann.Ret.Rate_01,15), Perc.Ann.Ret.Rate=label_percent(accuracy = 0.001)(round(Ann.Ret.Rate_01,15)),
                                        Ann.Cont.Comp.Ret.Rate=Ann.Cont.Comp.Ret.Rate, Perc.Ann.Cont.Comp.Ret.Rate = label_percent(accuracy = 0.001)(Ann.Cont.Comp.Ret.Rate),
                                        .after="Perc.Ret.Rate.at.Maturity")
show(US_SP_BILLS_2023_04_11_df[1:15,])
#
save(US_SP_BILLS_2023_04_11_df, file="US_SP_BILLS_2023_04_11_df.RData")
# rm(US_SP_BILLS_2023_04_11_df)
# head(US_SP_BILLS_2023_04_11_df)
# load("US_SP_BILLS_2023_04_11_df.RData")
# head(US_SP_BILLS_2023_04_11_df)
# tail(US_SP_BILLS_2023_04_11_df)
#
# We draw a plot of the U.S. Market Based Bill Par Yield Curve Rates on Apr, 11th, 2023.
# library(ggplot2)
Data_df <- US_SP_BILLS_2023_04_11_df
DS_length <- nrow(Data_df)
First_Day <- as.character(Data_df$Maturity.Date[1])
Last_Day <- as.character(last(Data_df$Maturity.Date))
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2024-2024", 
                             paste("Line Plot of the U.S. Market Based Bill Par Yield Curve Rates on Apr, 11th, 2023. Maturities from ", .(First_Day), " to ", .(Last_Day))))
link <- "https://www.treasurydirect.gov/GA-FI/FedInvest/selectSecurityPriceDate"
subtitle_content <- bquote(paste("path length ", .(DS_length), " sample points. Data by courtesy of  FedInvest, U.S. Department of the Treasure  -  ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("maturity dates (from Apr, 11th, 2023)")
# To obtain the sub-multiples of the length of the data set as a hint on the number of breaks to use
# library(numbers)
# primeFactors(DS_length)
x_breaks_num <- 10
x_breaks_low <- first(Data_df$Index)
x_breaks_up <- last(Data_df$Index)
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Maturity.Date[x_breaks])
J <- 0.5
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("annual cont. comp. ret. rate (from Jan, 3rd, 2023 to maturity date)")
y_breaks_num <- 10
y_max <- max(na.rm(as.numeric(sub("%","", Data_df$Perc.Ann.Cont.Comp.Ret.Rate))))
y_min <- min(na.rm(as.numeric(sub("%","", Data_df$Perc.Ann.Cont.Comp.Ret.Rate))))
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor(y_min/y_binwidth)*y_binwidth
y_breaks_up <- ceiling(y_max/y_binwidth)*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- paste(format(y_breaks, scientific=FALSE),"%",sep="")
K <- 0
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
line_col_b <- bquote("annual cont. comp. ret. rate")
leg_line_labs   <- c(line_col_b)
leg_line_cols   <- c("line_col_b"="blue")
leg_line_breaks <- c("line_col_b")
# leg_vals <- rainbow(length(levels(factor(Data_df$Index))))[as.numeric(levels(factor(Data_df$Index)))]
US_SP_BILLS_2023_04_11_Yield_Rate_lp <- ggplot(Data_df, aes(x=Index)) + 
  geom_line(aes(y=as.numeric(sub("%","", Perc.Ann.Cont.Comp.Ret.Rate)), color="line_col_b"), linewidth=0.8, linetype="solid") +
  scale_x_continuous(breaks=x_breaks, labels=x_labs, limits=x_lims) +
  scale_y_continuous(breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis= sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("maturity dates (from Apr, 11th, 2023)") + 
  ylab("annual cont. comp. ret. rate (from Apr, 11th, 2023 to maturity date)") +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="Legend", labels=leg_line_labs, values=leg_line_cols, breaks=leg_line_breaks) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x=element_text(color="black", size=10, face="italic", angle=0, vjust=0),
        axis.text.y=element_text(color="black", size=11, face="italic", angle=0, vjust=0),
        axis.title.x=element_text(color="black", size=13, face="bold.italic", angle=0, vjust=-2),
        axis.title.y=element_text(color="black", size=13, face="bold.italic", angle=90, vjust=0),
        legend.key.width = unit(0.80,"cm"), legend.position="bottom")
plot(US_SP_BILLS_2023_04_11_Yield_Rate_lp)
#
###############################################################################################################################################
# We use Dr. Simone Nicosanti's Python script to extract data from https://www.treasurydirect.gov/GA-FI/FedInvest/selectSecurityPriceDate
# and compare the U.S. Market Based Bill Par Yield Curve Rates from March 31st, 2023, to Apr 11th, 2023, with the corresponding Treasury Par
# Yield Curve Rates from https://home.treasury.gov/policy-issues/financing-the-government/interest-rate-statistics.
# Nicosanti's script builds the file "TotalOutput_2023-03-31_2023-04-11" containing the price data of market-based Treasury special securities 
# from March, 31st to April, 11th, 2023. Then, we build a data frame with data from the "TotalOutput_2023-03-31_2023-04-11.csv" file.
US_SP_2023_03_31_2023_04_11_df <- read.csv("TotalOutput_2023-03-31_2023-04-11.csv", header=TRUE)
nrow(US_SP_2023_03_31_2023_04_11_df)
head(US_SP_2023_03_31_2023_04_11_df,15)
tail(US_SP_2023_03_31_2023_04_11_df,15)
nrows <- nrow(US_SP_2023_03_31_2023_04_11_df)
show(nrows)
show(US_SP_2023_03_31_2023_04_11_df[(nrows/2-15):(nrows/2+15),])

#
# We rename the columns according to the terminology in https://www.treasurydirect.gov/GA-FI/FedInvest/selectSecurityPriceDate.
US_SP_2023_03_31_2023_04_11_df <- rename(US_SP_2023_03_31_2023_04_11_df, Security.Type=SecurityType, 
                                         Maturity.Date=MaturityDate, Call.Date=CallDate, End.of.Day=EndOfDay)
head(US_SP_2023_03_31_2023_04_11_df)
#
# We check whether the Maturity.Date column is in "Date" format. In case it is not, we change the format to "Date".
class(US_SP_2023_03_31_2023_04_11_df$Maturity.Date)
US_SP_2023_03_31_2023_04_11_df$Maturity.Date <- as.Date(US_SP_2023_03_31_2023_04_11_df$Maturity.Date,  format="%Y-%m-%d")
class(US_SP_2023_03_31_2023_04_11_df$Maturity.Date)
head(US_SP_2023_03_31_2023_04_11_df,15)
#
# We check whether the Date column is in "Date" format. In case it is not, we change the format to "Date".
class(US_SP_2023_03_31_2023_04_11_df$Date)
US_SP_2023_03_31_2023_04_11_df$Date <- as.Date(US_SP_2023_03_31_2023_04_11_df$Date,  format="%Y-%m-%d")
class(US_SP_2023_03_31_2023_04_11_df$Date)
show(US_SP_2023_03_31_2023_04_11_df[1:15,])
#
# We restrict ourselves to consider only MARKET BASED BILL.
US_SP_BILLS_2023_03_31_2023_04_11_df <- subset(US_SP_2023_03_31_2023_04_11_df, US_SP_2023_03_31_2023_04_11_df$Security.Type=="MARKET BASED BILL")
head(US_SP_BILLS_2023_03_31_2023_04_11_df)
tail(US_SP_BILLS_2023_03_31_2023_04_11_df)
nrow(US_SP_BILLS_2023_03_31_2023_04_11_df)
#
# We rename the rows 
rownames(US_SP_BILLS_2023_03_31_2023_04_11_df) <- NULL
tail(US_SP_BILLS_2023_03_31_2023_04_11_df)
#
Data_df <- US_SP_BILLS_2023_03_31_2023_04_11_df
Uniq_dates <- unique(Data_df$Date)
length(Uniq_dates)
mat_dates_subsets <- list()
for(j in 1:length(Uniq_dates)){
  mat_dates_subsets[[j]] <- Data_df$Maturity.Date[which(Data_df$Date==Uniq_dates[j])]
}
show(mat_dates_subsets)
mat_dates_cup <- mat_dates_subsets[[1]]
for(j in 2:length(Uniq_dates)){
  mat_dates_cup <- as.Date(union(mat_dates_cup, mat_dates_subsets[[j]]))
}
show(mat_dates_cup)
class(mat_dates_cup)
length(mat_dates_cup)
#
mat_dates_cap <- mat_dates_subsets[[1]]
for(j in 2:length(Uniq_dates)){
  mat_dates_cap <- as.Date(intersect(mat_dates_cap, mat_dates_subsets[[j]]))
}
show(mat_dates_cap)
class(mat_dates_cap)
length(mat_dates_cap)
#
mat_date_pos <- grep("Maturity.Date", colnames(Data_df))
US_SP_BILLS_2023_03_31_2023_04_11_ls <- list()
for(j in 1:length(Uniq_dates)){
  US_SP_BILLS_2023_03_31_2023_04_11_ls[[j]] <- subset(Data_df, Data_df$Date==Uniq_dates[j])
  rownames(US_SP_BILLS_2023_03_31_2023_04_11_ls[[j]]) <- NULL
  for(k in 1:length(mat_dates_cup)){
    if(mat_dates_cup[k] %in% US_SP_BILLS_2023_03_31_2023_04_11_ls[[j]]$Maturity.Date){}
    else{US_SP_BILLS_2023_03_31_2023_04_11_ls[[j]] <- arrange(rbind(US_SP_BILLS_2023_03_31_2023_04_11_ls[[j]], c(rep(NA,(mat_date_pos-1)),mat_dates_cup[k],rep(NA,(ncol(Data_df)-mat_date_pos)-1),Uniq_dates[j])), Maturity.Date)}
  }
}
#
for(j in 1:length(Uniq_dates)){
  US_SP_BILLS_2023_03_31_2023_04_11_ls[[j]] <- add_column(US_SP_BILLS_2023_03_31_2023_04_11_ls[[j]], Loc_Index=1:nrow(US_SP_BILLS_2023_03_31_2023_04_11_ls[[j]]),
                                                          .before="Cusip")}
#
US_SP_BILLS_2023_03_31_2023_04_11_ls[[1]]
nrow(US_SP_BILLS_2023_03_31_2023_04_11_ls[[1]])
US_SP_BILLS_2023_03_31_2023_04_11_ls[[3]]
nrow(US_SP_BILLS_2023_03_31_2023_04_11_ls[[3]])
US_SP_BILLS_2023_03_31_2023_04_11_ls[[7]]
nrow(US_SP_BILLS_2023_03_31_2023_04_11_ls[[7]])
#
US_SP_BILLS_2023_03_31_2023_04_11_df <- US_SP_BILLS_2023_03_31_2023_04_11_ls[[1]]
nrow(US_SP_BILLS_2023_03_31_2023_04_11_df)
for(j in 2:length(Uniq_dates)){
  US_SP_BILLS_2023_03_31_2023_04_11_df <- rbind(US_SP_BILLS_2023_03_31_2023_04_11_df, US_SP_BILLS_2023_03_31_2023_04_11_ls[[j]])
}
nrow(US_SP_BILLS_2023_03_31_2023_04_11_df)
head(US_SP_BILLS_2023_03_31_2023_04_11_df,15)
show(US_SP_BILLS_2023_03_31_2023_04_11_df[200:240,])
tail(US_SP_BILLS_2023_03_31_2023_04_11_df,15)
#
# We add an index column
US_SP_BILLS_2023_03_31_2023_04_11_df <- add_column(US_SP_BILLS_2023_03_31_2023_04_11_df, 
                                                   Index=1:nrow(US_SP_BILLS_2023_03_31_2023_04_11_df),
                                                   .before="Cusip")
show(US_SP_BILLS_2023_03_31_2023_04_11_df[200:240,])
# We add a column *Days.to.Maturity*, which accounts for the number of days from January 1st, 2023, to the Maturity Date
US_SP_BILLS_2023_03_31_2023_04_11_df <- add_column(US_SP_BILLS_2023_03_31_2023_04_11_df, 
                                             Days.to.Maturity=as.vector(difftime(US_SP_BILLS_2023_03_31_2023_04_11_df$Maturity.Date, 
                                                                                 US_SP_BILLS_2023_03_31_2023_04_11_df$Date, units="days")), 
                                             .after="Maturity.Date")
show(US_SP_BILLS_2023_03_31_2023_04_11_df[200:240,])
# We also add the column *Months.to.Maturity* [resp. *Years.to.Maturity*], which accounts for the number of months 
# [resp. years] from the Date, to the Maturity Date.
US_SP_BILLS_2023_03_31_2023_04_11_df <- add_column(US_SP_BILLS_2023_03_31_2023_04_11_df, 
                                             Months.to.Maturity=as.vector(US_SP_BILLS_2023_03_31_2023_04_11_df$Days.to.Maturity/30.4369),
                                             Years.to.Maturity=as.vector(US_SP_BILLS_2023_03_31_2023_04_11_df$Days.to.Maturity/365.2425),
                                             .after="Days.to.Maturity")
show(US_SP_BILLS_2023_03_31_2023_04_11_df[200:240,])
# We add a column *Ret.Rate.at.Maturity* and *Perc.Ret.Rate.at.Maturity*
US_SP_BILLS_2023_03_31_2023_04_11_df <- add_column(US_SP_BILLS_2023_03_31_2023_04_11_df, 
                                       Ret.Rate.at.Maturity=(100-US_SP_BILLS_2023_03_31_2023_04_11_df$End.of.Day)/US_SP_BILLS_2023_03_31_2023_04_11_df$End.of.Day,
                                       Perc.Ret.Rate.at.Maturity=label_percent(accuracy = 0.001)((100-US_SP_BILLS_2023_03_31_2023_04_11_df$End.of.Day)/US_SP_BILLS_2023_03_31_2023_04_11_df$End.of.Day),
                                       .after="End.of.Day")
show(US_SP_BILLS_2023_03_31_2023_04_11_df[200:240,])
#
# We compute the annual rate of return according to the formulas
# (1+r_a)^t = 1+r_t; r_a = (1+r_t)^(1/t)-1
# where r_a=annual rate of return, t=time to maturity (in years), r_t=rate of return in the period t.
# or 
# (1+r_a)^(t/365.2425) = 1+r_t; r_a = (1+r_t)^(365.2425/t)-1
# where r_a=annual rate of return, t=time to maturity (in days), r_t=rate of return in the period t.
#
# We also compute the annual continuously compound rate of return according to the formula
# 1+r_a=exp(ro_a)
# or
# ro_a=log(1+r_a)
#
Ann.Ret.Rate_01 <- (1+US_SP_BILLS_2023_03_31_2023_04_11_df$Ret.Rate.at.Maturity)^(1/US_SP_BILLS_2023_03_31_2023_04_11_df$Years.to.Maturity)-1
show(Ann.Ret.Rate_01)
Ann.Ret.Rate_02 <- (1+US_SP_BILLS_2023_03_31_2023_04_11_df$Ret.Rate.at.Maturity)^(365.2425/US_SP_BILLS_2023_03_31_2023_04_11_df$Days.to.Maturity)-1
show(Ann.Ret.Rate_02)
Ann.Ret.Rate_01==Ann.Ret.Rate_02
round(Ann.Ret.Rate_01,15)==round(Ann.Ret.Rate_02,15)
round(Ann.Ret.Rate_01,14)==round(Ann.Ret.Rate_02,14)
identical(round(Ann.Ret.Rate_01,14),round(Ann.Ret.Rate_02,14))
# [1] TRUE
#
Ann.Cont.Comp.Ret.Rate <- log(1+Ann.Ret.Rate_01)
show(Ann.Cont.Comp.Ret.Rate)
#
# We add the columns *Ann.Ret.Rate*, *Perc.Ann.Ret.Rate*, *Ann.Cont.Comp.Ret.Rate*, and *Perc.Ann.Cont.Comp.Ret.Rate*
US_SP_BILLS_2023_03_31_2023_04_11_df <- add_column(US_SP_BILLS_2023_03_31_2023_04_11_df,
                                        Ann.Ret.Rate=round(Ann.Ret.Rate_01,14), Perc.Ann.Ret.Rate=label_percent(accuracy = 0.001)(round(Ann.Ret.Rate_01,14)),
                                        Ann.Cont.Comp.Ret.Rate=Ann.Cont.Comp.Ret.Rate, Perc.Ann.Cont.Comp.Ret.Rate = label_percent(accuracy = 0.001)(Ann.Cont.Comp.Ret.Rate),
                                        .after="Perc.Ret.Rate.at.Maturity")
show(US_SP_BILLS_2023_03_31_2023_04_11_df[200:240,])
#
save(US_SP_BILLS_2023_03_31_2023_04_11_df, file="US_SP_BILLS_2023_03_31_2023_04_11_df.RData")
# rm(US_SP_BILLS_2023_03_31_2023_04_11_df)
# head(US_SP_BILLS_2023_03_31_2023_04_11_df)
# load("US_SP_BILLS_2023_03_31_2023_04_11_df.RData")
show(US_SP_BILLS_2023_03_31_2023_04_11_df[200:240,])
#
Data_df <- US_SP_BILLS_2023_03_31_2023_04_11_df
Uniq_dates <- unique(Data_df$Date)
First_Day <- as.character(first(Uniq_dates))
Last_Day <- as.character(last(Uniq_dates))
path_length <- c()
for(j in 1:length(Uniq_dates)){
path_length[j] <- length(na.rm(Data_df$Cusip[which(Data_df$Date==Uniq_dates[j])]))
}
min_path_length <- min(path_length)
max_path_length <- max(path_length)
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2024-2024", 
                             paste("Line Plot of the U.S. Market Based Bill Par Yield Curve Rates. Business Days from ", .(First_Day), " to ", .(Last_Day))))
link <- "https://www.treasurydirect.gov/GA-FI/FedInvest/selectSecurityPriceDate"
subtitle_content <- bquote(paste("path lengths from ", .(min_path_length), " to ", .(max_path_length), " sample points. Data by courtesy of  FedInvest, U.S. Department of the Treasure  -  ", .(link)))
caption_content <- "Author: Roberto Monte"
# primeFactors(min_path_length)
x_breaks_num <- 12
x_breaks_low <- first(Data_df$Loc_Index)
x_breaks_up <- last(Data_df$Loc_Index)
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Maturity.Date[x_breaks])
J <- 0.0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_breaks_num <- 10
path_max <- c()
for(j in 1:length(Uniq_dates)){
  path_max[j] <- max(na.rm(as.numeric(sub("%","", subset(Data_df, Data_df$Date == Uniq_dates[j])$Perc.Ann.Cont.Comp.Ret.Rate))))
}
y_max <- max(path_max)
path_min <- c()
for(j in 1:length(Uniq_dates)){
  path_min[j] <- min(na.rm(as.numeric(sub("%","", subset(Data_df, Data_df$Date == Uniq_dates[j])$Perc.Ann.Cont.Comp.Ret.Rate))))
}
y_min <- min(path_min)
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor(y_min/y_binwidth)*y_binwidth
y_breaks_up <- ceiling(y_max/y_binwidth)*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- paste(format(y_breaks, scientific=FALSE),"%",sep="")
K <- 0
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
line_cols <- rainbow(length(Uniq_dates))
leg_line_1 <- as.character(Uniq_dates)[1]
leg_line_2 <- as.character(Uniq_dates)[2]
leg_line_3 <- as.character(Uniq_dates)[3]
leg_line_4 <- as.character(Uniq_dates)[4]
leg_line_5 <- as.character(Uniq_dates)[5]
leg_line_6 <- as.character(Uniq_dates)[6]
leg_line_7 <- as.character(Uniq_dates)[7]
leg_line_8 <- as.character(Uniq_dates)[8]
leg_line_labs <- c(leg_line_1, leg_line_2, leg_line_3, leg_line_4, leg_line_5, leg_line_6, leg_line_7, leg_line_8)
leg_line_cols <- c("leg_line_1"="#FF0000", "leg_line_2"="#FFBF00", "leg_line_3"="#80FF00", "leg_line_4"="#00FF40",
                   "leg_line_5"="#00FFFF", "leg_line_6"="#0040FF", "leg_line_7"="#8000FF", "leg_line_8"="#FF00BF")
leg_line_breaks <- c("leg_line_1", "leg_line_2", "leg_line_3", "leg_line_4", "leg_line_5", "leg_line_6", "leg_line_7",
                     "leg_line_8")
# leg_line_labs_bis <- as.character(Uniq_dates)
# leg_line_cols_bis <- c(leg_line_bis=line_cols)
# leg_line_breaks_bis <- names(leg_line_cols_bis)
US_SP_BILLS_2023_03_31_2023_04_11_lp <- ggplot(Data_df) +
   geom_line(data=subset(Data_df, Data_df$Date == Uniq_dates[1]), alpha=1, linewidth=0.8, linetype="solid", 
              aes(x=Loc_Index, y=as.numeric(sub("%","", Perc.Ann.Cont.Comp.Ret.Rate)), color="leg_line_1", group=1)) +
   geom_line(data=subset(Data_df, Data_df$Date == Uniq_dates[2]), alpha=1, linewidth=0.8, linetype="solid",
             aes(x=Loc_Index, y=as.numeric(sub("%","", Perc.Ann.Cont.Comp.Ret.Rate)), color="leg_line_2", group=1)) +
   geom_line(data=subset(Data_df, Data_df$Date == Uniq_dates[3]), alpha=1, linewidth=0.8, linetype="solid",
             aes(x=Loc_Index, y=as.numeric(sub("%","", Perc.Ann.Cont.Comp.Ret.Rate)), color="leg_line_3", group=1)) +
   geom_line(data=subset(Data_df, Data_df$Date == Uniq_dates[4]), alpha=1, linewidth=0.8, linetype="solid",
             aes(x=Loc_Index, y=as.numeric(sub("%","", Perc.Ann.Cont.Comp.Ret.Rate)), color="leg_line_4", group=1)) +
   geom_line(data=subset(Data_df, Data_df$Date == Uniq_dates[5]), alpha=1, linewidth=0.8, linetype="solid",
             aes(x=Loc_Index, y=as.numeric(sub("%","", Perc.Ann.Cont.Comp.Ret.Rate)), color="leg_line_5", group=1)) +
   geom_line(data=subset(Data_df, Data_df$Date == Uniq_dates[6]), alpha=1, linewidth=0.8, linetype="solid",
             aes(x=Loc_Index, y=as.numeric(sub("%","", Perc.Ann.Cont.Comp.Ret.Rate)), color="leg_line_6", group=1)) +
   geom_line(data=subset(Data_df, Data_df$Date == Uniq_dates[7]), alpha=1, linewidth=0.8, linetype="solid",
             aes(x=Loc_Index, y=as.numeric(sub("%","", Perc.Ann.Cont.Comp.Ret.Rate)), color="leg_line_7",  group=1)) +
   geom_line(data=subset(Data_df, Data_df$Date == Uniq_dates[8]), alpha=1, linewidth=0.8, linetype="solid",
             aes(x=Loc_Index, y=as.numeric(sub("%","", Perc.Ann.Cont.Comp.Ret.Rate)), color="leg_line_8",  group=1)) +
   annotate(geom = "text", size=3.0, x=x_breaks, y=y_breaks_low, label=Data_df$Days.to.Maturity[x_breaks], vjust=3.0) +
   annotate(geom = "text", size=3.0, x=(x_breaks[6]+1.0), y=y_breaks_low, label="days to maturity from 2023-03-31", 
            hjust=-0.45, vjust=1.0) +
   scale_x_continuous(breaks=x_breaks, labels=x_labs, limits=x_lims) +
   scale_y_continuous(breaks=y_breaks, labels=NULL, limits=y_lims,
                      sec.axis= sec_axis(~., breaks=y_breaks, labels=y_labs)) +
   ggtitle(title_content) +
   labs(subtitle=subtitle_content, caption=caption_content) +
   xlab("maturity dates") + ylab("annual cont. comp. ret. rates") +
   scale_colour_manual(name="Legend", labels=leg_line_labs, values=leg_line_cols) +
   theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
         axis.text.x=element_text(color="black", size=10, face="italic", angle=0, vjust=0),
         axis.text.y=element_text(color="black", size=11, face="italic", angle=0, vjust=0),
         axis.title.x=element_text(color="black", size=13, face="bold.italic", angle=0, vjust=-2),
         axis.title.y=element_text(color="black", size=13, face="bold.italic", angle=90, vjust=0),
         legend.key.width = unit(0.80,"cm"), legend.position="right")
plot(US_SP_BILLS_2023_03_31_2023_04_11_lp)
#
###############################################################################################################################################
# We still use Dr. Simone Nicosanti's Python script to extract data from https://www.treasurydirect.gov/GA-FI/FedInvest/selectSecurityPriceDate
# and study the prices of the U.S. Market Based Bills from January 3rd, 2023, to June 26th, 2023.
# Nicosanti's script builds the file "TotalOutput_2023-01-03_2023-06-27.csv" with the price data of market-based Treasury special securities, 
# from January 3rd, 2023, to June 26th, 2023. Then, we build a data frame with data from the "TotalOutput_2023-01-03_2023-06-27.csv" file.
US_SP_2023_01_03_2023_06_26_df <- read.csv("TotalOutput_2023-01-03_2023-06-27.csv", header=TRUE)
head(US_SP_2023_01_03_2023_06_26_df,15)
tail(US_SP_2023_01_03_2023_06_26_df,15)
nrows <- nrow(US_SP_2023_01_03_2023_06_26_df)
show(nrows)
show(US_SP_2023_01_03_2023_06_26_df[(nrows/2-15):(nrows/2+15),])
#
# We rename the columns according to the terminology in https://www.treasurydirect.gov/GA-FI/FedInvest/selectSecurityPriceDate.
US_SP_2023_01_03_2023_06_26_df <- rename(US_SP_2023_01_03_2023_06_26_df, Security.Type=SecurityType, 
                                         Maturity.Date=MaturityDate, Call.Date=CallDate, End.of.Day=EndOfDay)
head(US_SP_2023_01_03_2023_06_26_df)
#
# We check whether the Maturity.Date column is in "Date" format. In case it is not, we change the format to "Date".
class(US_SP_2023_01_03_2023_06_26_df$Maturity.Date)
US_SP_2023_01_03_2023_06_26_df$Maturity.Date <- as.Date(US_SP_2023_01_03_2023_06_26_df$Maturity.Date,  format="%Y-%m-%d")
class(US_SP_2023_01_03_2023_06_26_df$Maturity.Date)
head(US_SP_2023_01_03_2023_06_26_df,15)
#
# We check whether the Date column is in "Date" format. In case it is not, we change the format to "Date".
class(US_SP_2023_01_03_2023_06_26_df$Date)
US_SP_2023_01_03_2023_06_26_df$Date <- as.Date(US_SP_2023_01_03_2023_06_26_df$Date,  format="%Y-%m-%d")
class(US_SP_2023_01_03_2023_06_26_df$Date)
show(US_SP_2023_01_03_2023_06_26_df[1:15,])
#
# We restrict ourselves to consider only MARKET BASED BILL.
US_SP_BILLS_2023_01_03_2023_06_26_df <- subset(US_SP_2023_01_03_2023_06_26_df, US_SP_2023_01_03_2023_06_26_df$Security.Type=="MARKET BASED BILL")
head(US_SP_BILLS_2023_01_03_2023_06_26_df)
tail(US_SP_BILLS_2023_01_03_2023_06_26_df)
nrows <- nrow(US_SP_BILLS_2023_01_03_2023_06_26_df)
show(nrows)
show(US_SP_BILLS_2023_01_03_2023_06_26_df[(nrows/2-15):(nrows/2+15),])
#
# We remove the columns "Rate" and "Call.Date"
US_SP_BILLS_2023_01_03_2023_06_26_df <- select(US_SP_BILLS_2023_01_03_2023_06_26_df, -c("Rate",Call.Date))
head(US_SP_BILLS_2023_01_03_2023_06_26_df)
#
# We switch the Maturity.Date and Date columns
US_SP_BILLS_2023_01_03_2023_06_26_df <- US_SP_BILLS_2023_01_03_2023_06_26_df[, c("Cusip", "Security.Type", "Date", "Buy", "Sell", "End.of.Day", "Maturity.Date")]
head(US_SP_BILLS_2023_01_03_2023_06_26_df)
# We rename the rows 
rownames(US_SP_BILLS_2023_01_03_2023_06_26_df) <- NULL
tail(US_SP_BILLS_2023_01_03_2023_06_26_df)
#
Data_df <- US_SP_BILLS_2023_01_03_2023_06_26_df
Uniq_Cusips <- unique(Data_df$Cusip)
show(Uniq_Cusips)
length(Uniq_Cusips)
Cusip_subsets <- list()
for(j in 1:length(Uniq_Cusips)){
  Cusip_subsets[[j]] <- add_column(subset(US_SP_BILLS_2023_01_03_2023_06_26_df, Data_df$Cusip==Uniq_Cusips[j]), 
                                   Loc_Index=1:nrow(subset(US_SP_BILLS_2023_01_03_2023_06_26_df, Data_df$Cusip==Uniq_Cusips[j])),
                                   .before="Cusip")
}
Cusip_subsets_length <- vector()
for(j in 1:length(Uniq_Cusips)){
  Cusip_subsets_length[j] <- nrow(Cusip_subsets[[j]])
}
show(Cusip_subsets_length)
Cusip_subsets_sorted <- sort.int(Cusip_subsets_length, partial = NULL, na.last = NA, decreasing = TRUE, method="auto", index.return = TRUE)
show(Cusip_subsets_sorted)
#
US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df <- data.frame()
for(j in Cusip_subsets_sorted$ix){
  US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df <- rbind(US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df,Cusip_subsets[[j]])
}
nrow(US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df)
head(US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df)
tail(US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df)
rownames(US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df) <- NULL
head(US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df)
tail(US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df)
# We add a column *Days.to.Maturity*, which accounts for the number of days from the date in which the Market Based Bill was priced to the
# Maturity Date
US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df <- add_column(US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df, 
                                                    Days.to.Maturity=as.vector(difftime(US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df$Maturity.Date, 
                                                                                        US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df$Date, units="days")), 
                                                   .after="Maturity.Date")
head(US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df)
tail(US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df)
nrows <- nrow(US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df)
show(nrows)
show(US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df[(nrows/2-15):(nrows/2+15),])
#
# We also add the column *Months.to.Maturity* [resp. *Years.to.Maturity*], which accounts for the number of months 
# [resp. years] from the Date, to the Maturity Date.
US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df <- add_column(US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df, 
                                                    Months.to.Maturity=as.vector(US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df$Days.to.Maturity/30.4369),
                                                    Years.to.Maturity=as.vector(US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df$Days.to.Maturity/365.2425),
                                                   .after="Days.to.Maturity")
head(US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df)
tail(US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df)
nrows <- nrow(US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df)
show(nrows)
show(US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df[(nrows/2-15):(nrows/2+15),])
write.csv(US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df,
          "C:/Users/rober/My Documents - Notebook (local)/My Classes/MPSMF/R - Scripts & Data/US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26.csv")
#
save(US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df, file="US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df.RData")
# rm(US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df)
# head(US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df)
# load("US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df.RData")
head(US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df)
tail(US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df)
nrows <- nrow(US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df)
show(nrows)
show(US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df[(nrows/2-15):(nrows/2+15),])
#
Data_df <- US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_df
Uniq_Cusips <- unique(Data_df$Cusip)
show(Uniq_Cusips)
length(Uniq_Cusips)
Cusip_subsets <- list()
for(j in 1:length(Uniq_Cusips)){
  Cusip_subsets[[j]] <- add_column(subset(Data_df, Data_df$Cusip==Uniq_Cusips[j]), 
                                   Loc_Index=1:nrow(subset(Data_df, Data_df$Cusip==Uniq_Cusips[j])),
                                   .before="Cusip")
}
Cusip_subsets_length <- vector()
for(j in 1:length(Uniq_Cusips)){
  Cusip_subsets_length[j] <- nrow(Cusip_subsets[[j]])
}
show(Cusip_subsets_length)
Cusip_subsets_sorted <- sort.int(Cusip_subsets_length, partial = NULL, na.last = NA, decreasing = TRUE, method="auto", index.return = TRUE)
show(Cusip_subsets_sorted)
#
Uniq_dates <- unique(Data_df$Date)
First_Day <- as.character(first(Uniq_dates))
Last_Day <- as.character(last(Uniq_dates))
min_path_length <- Cusip_subsets_sorted$x[8]
max_path_length <- Cusip_subsets_sorted$x[1]
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2024-2024", 
                             paste("Line Plot of the U.S. Market Based Bill Closing Prices. Business Days from ", .(First_Day), " to ", .(Last_Day))))
link <- "https://www.treasurydirect.gov/GA-FI/FedInvest/selectSecurityPriceDate"
subtitle_content <- bquote(paste("path lengths from ", .(min_path_length), " to ", .(max_path_length), " sample points. Data by courtesy of  FedInvest, U.S. Department of the Treasure  -  ", .(link)))
caption_content <- "Author: Roberto Monte"
# primeFactors(min_path_length)
x_breaks_num <- 11
x_breaks_low <- first(Data_df$Loc_Index)
x_breaks_up <- min_path_length
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Date[x_breaks])
J <- 0.0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_breaks_num <- 10
path_max <- c()
cn <- 1
for(j in Cusip_subsets_sorted$ix[c(1:8)]){
  path_max[cn] <- max(Cusip_subsets[[j]]$End.of.Day)
  cn <- cn+1
}
show(path_max)
y_max <- max(path_max)
path_min <- c()
cn <- 1
for(j in Cusip_subsets_sorted$ix[c(1:8)]){
  path_min[cn] <- min(Cusip_subsets[[j]]$End.of.Day)
  cn <- cn+1
}    
show(path_min)
y_min <- min(path_min)
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor(y_min/y_binwidth)*y_binwidth
y_breaks_up <- ceiling(y_max/y_binwidth)*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
line_cols <- rainbow(length(Uniq_Cusips[Cusip_subsets_sorted$ix[c(1:8)]]))
leg_line_1 <- paste("Cusip", Uniq_Cusips[Cusip_subsets_sorted$ix[1]],"- Maturity",
                    Data_df$Maturity.Date[which(Data_df$Cusip==Uniq_Cusips[Cusip_subsets_sorted$ix[1]])[1]])
leg_line_2 <- paste("Cusip", Uniq_Cusips[Cusip_subsets_sorted$ix[2]],"- Maturity", 
                    Data_df$Maturity.Date[which(Data_df$Cusip==Uniq_Cusips[Cusip_subsets_sorted$ix[2]])[1]])
leg_line_3 <- paste("Cusip",Uniq_Cusips[Cusip_subsets_sorted$ix[3]],"- Maturity", 
                    Data_df$Maturity.Date[which(Data_df$Cusip==Uniq_Cusips[Cusip_subsets_sorted$ix[3]])[1]])
leg_line_4 <- paste("Cusip", Uniq_Cusips[Cusip_subsets_sorted$ix[4]],"- Maturity", 
                    Data_df$Maturity.Date[which(Data_df$Cusip==Uniq_Cusips[Cusip_subsets_sorted$ix[4]])[1]])
leg_line_5 <- paste("Cusip", Uniq_Cusips[Cusip_subsets_sorted$ix[5]],"- Maturity", 
                    Data_df$Maturity.Date[which(Data_df$Cusip==Uniq_Cusips[Cusip_subsets_sorted$ix[5]])[1]])
leg_line_6 <- paste("Cusip", Uniq_Cusips[Cusip_subsets_sorted$ix[6]],"- Maturity", 
                    Data_df$Maturity.Date[which(Data_df$Cusip==Uniq_Cusips[Cusip_subsets_sorted$ix[6]])[1]])
leg_line_7 <- paste("Cusip", Uniq_Cusips[Cusip_subsets_sorted$ix[7]],"- Maturity", 
                    Data_df$Maturity.Date[which(Data_df$Cusip==Uniq_Cusips[Cusip_subsets_sorted$ix[7]])[1]])
leg_line_8 <- paste("Cusip", Uniq_Cusips[Cusip_subsets_sorted$ix[8]],"- Maturity", 
                    Data_df$Maturity.Date[which(Data_df$Cusip==Uniq_Cusips[Cusip_subsets_sorted$ix[8]])[1]])
leg_line_labs <- c(leg_line_1, leg_line_2, leg_line_3, leg_line_4, leg_line_5, leg_line_6, leg_line_7, leg_line_8)
leg_line_cols <- c("leg_line_1"="#FF0000", "leg_line_2"="#FFBF00", "leg_line_3"="#80FF00", "leg_line_4"="#00FF40",
                   "leg_line_5"="#00FFFF", "leg_line_6"="#0040FF", "leg_line_7"="#8000FF", "leg_line_8"="#FF00BF")
leg_line_breaks <- c("leg_line_1", "leg_line_2", "leg_line_3", "leg_line_4", "leg_line_5", "leg_line_6", "leg_line_7",
                     "leg_line_8")
US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_lp <- ggplot(Data_df) +
  geom_line(data=subset(Data_df, Data_df$Cusip == Uniq_Cusips[Cusip_subsets_sorted$ix[1]]), alpha=1, linewidth=0.8, linetype="solid", 
            aes(x=Loc_Index, y=End.of.Day, color="leg_line_1", group=1)) +
  geom_line(data=subset(Data_df, Data_df$Cusip == Uniq_Cusips[Cusip_subsets_sorted$ix[2]]), alpha=1, linewidth=0.8, linetype="solid",
            aes(x=Loc_Index, y=End.of.Day, color="leg_line_2", group=1)) +
  geom_line(data=subset(Data_df, Data_df$Cusip == Uniq_Cusips[Cusip_subsets_sorted$ix[3]]), alpha=1, linewidth=0.8, linetype="solid",
            aes(x=Loc_Index, y=End.of.Day, color="leg_line_3", group=1)) +
  geom_line(data=subset(Data_df, Data_df$Cusip == Uniq_Cusips[Cusip_subsets_sorted$ix[4]]), alpha=1, linewidth=0.8, linetype="solid",
            aes(x=Loc_Index, y=End.of.Day, color="leg_line_4", group=1)) +
  geom_line(data=subset(Data_df, Data_df$Cusip == Uniq_Cusips[Cusip_subsets_sorted$ix[5]]), alpha=1, linewidth=0.8, linetype="solid",
            aes(x=Loc_Index, y=End.of.Day, color="leg_line_5", group=1)) +
  geom_line(data=subset(Data_df, Data_df$Cusip == Uniq_Cusips[Cusip_subsets_sorted$ix[6]]), alpha=1, linewidth=0.8, linetype="solid",
            aes(x=Loc_Index, y=End.of.Day, color="leg_line_6", group=1)) +
  geom_line(data=subset(Data_df, Data_df$Cusip == Uniq_Cusips[Cusip_subsets_sorted$ix[7]]), alpha=1, linewidth=0.8, linetype="solid",
            aes(x=Loc_Index, y=End.of.Day, color="leg_line_7",  group=1)) +
  geom_line(data=subset(Data_df, Data_df$Cusip == Uniq_Cusips[Cusip_subsets_sorted$ix[8]]), alpha=1, linewidth=0.8, linetype="solid",
            aes(x=Loc_Index, y=End.of.Day, color="leg_line_8",  group=1)) +
  annotate(geom = "text", size=3.0, x=x_breaks, y=y_breaks_low, label=Data_df$Days.to.Maturity[x_breaks], vjust=3.0) +
  annotate(geom = "text", size=3.0, x=(x_breaks[5]+4.0), y=y_breaks_low, label="days to maturity to 2023-06-23",
           hjust=-0.45, vjust=1.0) +
  scale_x_continuous(breaks=x_breaks, labels=x_labs, limits=x_lims) +
  scale_y_continuous(breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis= sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("") + ylab("End of Day Price") +
  scale_colour_manual(name="Legend", labels=leg_line_labs, values=leg_line_cols) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x=element_text(color="black", size=10, face="italic", angle=0, vjust=0),
        axis.text.y=element_text(color="black", size=11, face="italic", angle=0, vjust=0),
        axis.title.x=element_text(color="black", size=13, face="bold.italic", angle=0, vjust=-2),
        axis.title.y=element_text(color="black", size=13, face="bold.italic", angle=90, vjust=0),
        legend.key.width = unit(0.80,"cm"), legend.position="bottom")
plot(US_SP_BILLS_Cusip_Grouped_2023_01_03_2023_06_26_lp)
# Note that on March 21–22, 2023, an important meeting of the Federal Open Market Committee was held about the future policy on interest rates. 
# https://www.federalreserve.gov/monetarypolicy/fomcminutes20230322.htm#:~:text=%22Effective%20March%2023%2C%202023%2C,3%2F4%20to%205%20percent.
# 
# We consider the rate of returns and log rate of returns of the bonds whose prices have been plotted.
# We start with the bond named Cusip 912796ZR3 with maturity 2023 06 29.
Cusip_912796ZR3_df <- Data_df[which(Data_df$Cusip=="912796ZR3"),]
head(Cusip_912796ZR3_df, 10)
tail(Cusip_912796ZR3_df, 10)
# We add a column *Ret.Rate.at.Maturity* and *Perc.Ret.Rate.at.Maturity*
Cusip_912796ZR3_df <- add_column(Cusip_912796ZR3_df, 
                                 Ret.Rate.at.Maturity=(100-Cusip_912796ZR3_df$End.of.Day)/Cusip_912796ZR3_df$End.of.Day,
                                 Perc.Ret.Rate.at.Maturity=label_percent(accuracy = 0.001)((100-Cusip_912796ZR3_df$End.of.Day)/Cusip_912796ZR3_df$End.of.Day),
                                 .after="End.of.Day")
head(Cusip_912796ZR3_df, 10)
tail(Cusip_912796ZR3_df, 10)
# We compute the annual rate of return according to the formulas
# (1+r_a)^t = 1+r_t; r_a = (1+r_t)^(1/t)-1
# where r_a=annual rate of return, t=time to maturity (in years), r_t=rate of return in the period t.
# or 
# (1+r_a)^(t/365.2425) = 1+r_t; r_a = (1+r_t)^(365.2425/t)-1
# where r_a=annual rate of return, t=time to maturity (in days), r_t=rate of return in the period t.
#
Ann.Ret.Rate_01 <- (1+Cusip_912796ZR3_df$Ret.Rate.at.Maturity)^(1/Cusip_912796ZR3_df$Years.to.Maturity)-1
head(Ann.Ret.Rate_01, 20)
# [1] 0.04827127 0.04794198 0.04869436 0.04778619 0.04878268 0.04899397 0.04877259 0.04768721 0.04700318 0.04787824 0.04787319 0.04776029 0.04724071
# [14] 0.04827884 0.04827358 0.04794497 0.04826299 0.04728275 0.04781076 0.04769778
tail(Ann.Ret.Rate_01, 20)
# [1] 0.04907674 0.05160531 0.05102168 0.05136425 0.04667604 0.05010201 0.04918410 0.04917844 0.04916250 0.04402504 0.04875713 0.04845873 0.04843395
# [14] 0.04728737 0.03489247 0.04464008 0.04244118 0.04164536 0.02387367 0.03223532
#
Ann.Ret.Rate_02 <- (1+Cusip_912796ZR3_df$Ret.Rate.at.Maturity)^(365.2425/Cusip_912796ZR3_df$Days.to.Maturity)-1
head(Ann.Ret.Rate_02, 20)
# [1] 0.04827127 0.04794198 0.04869436 0.04778619 0.04878268 0.04899397 0.04877259 0.04768721 0.04700318 0.04787824 0.04787319 0.04776029 0.04724071
# [14] 0.04827884 0.04827358 0.04794497 0.04826299 0.04728275 0.04781076 0.04769778
tail(Ann.Ret.Rate_02, 20)
# [1] 0.04907674 0.05160531 0.05102168 0.05136425 0.04667604 0.05010201 0.04918410 0.04917844 0.04916250 0.04402504 0.04875713 0.04845873 0.04843395
# [14] 0.04728737 0.03489247 0.04464008 0.04244118 0.04164536 0.02387367 0.03223532
#
identical(Ann.Ret.Rate_01, Ann.Ret.Rate_02)
# [1] FALSE
identical(round(Ann.Ret.Rate_01,14), round(Ann.Ret.Rate_02,14))
# [1] TRUE
#
# We also compute the annual continuously compound rate of return according to the formula
# 1+r_a=exp(ro_a)
# or
# ro_a=log(1+r_a)
#
Cont.Comp.Ann.Ret.Rate <- log(1+Ann.Ret.Rate_01)
head(Cont.Comp.Ann.Ret.Rate, 20)
# [1] 0.04714240 0.04682822 0.04754592 0.04667955 0.04763014 0.04783158 0.04762052 0.04658508 0.04593197 0.04676739 0.04676258 0.04665483 0.04615881
# [14] 0.04714962 0.04714461 0.04683108 0.04713450 0.04619896 0.04670300 0.04659516
tail(Cont.Comp.Ann.Ret.Rate, 20)
# [1] 0.04791048 0.05031786 0.04976272 0.05008861 0.04561946 0.04888732 0.04801281 0.04800742 0.04799223 0.04308347 0.04760577 0.04732121 0.04729757
# [14] 0.04620337 0.03429753 0.04367240 0.04156525 0.04080154 0.02359315 0.03172666
#
# We add the columns *Ann.Ret.Rate*, *Perc.Ann.Ret.Rate*, *Cont.Comp.Ann.Ret.Rate*, and *Perc.Cont.Comp.Ann.Ret.Rate*
Cusip_912796ZR3_df <- add_column(Cusip_912796ZR3_df,
                                 Ann.Ret.Rate=round(Ann.Ret.Rate_01,14), 
                                 Perc.Ann.Ret.Rate=label_percent(accuracy = 0.001)(round(Ann.Ret.Rate_01,14)),
                                 Cont.Comp.Ann.Ret.Rate=Cont.Comp.Ann.Ret.Rate, 
                                 Perc.Cont.Comp.Ann.Ret.Rate = label_percent(accuracy = 0.001)(Cont.Comp.Ann.Ret.Rate),
                                 .after="Perc.Ret.Rate.at.Maturity")
head(Cusip_912796ZR3_df, 20)
tail(Cusip_912796ZR3_df, 20)
#
save(Cusip_912796ZR3_df, file="Cusip_912796ZR3_df.RData")
# rm(Cusip_912796ZR3_df)
# head(Cusip_912796ZR3_df)
# load("Cusip_912796ZR3_df.RData")
# head(Cusip_912796ZR3_df, 20)
# tail(Cusip_912796ZR3_df, 20)
#
# We consider a draft plot of the percentage continuously annual compound rate of return
y <- Cusip_912796ZR3_df$Cont.Comp.Ann.Ret.Rate
x <- 1:length(y)
plot(x,y, type = "p", pch=16, col="#FF0000", xaxt = "n", xlab="", yaxt = "n", ylab="", ylim=c(round(min(y),2), round(max(y),2)),
     main="Bond Cusip 912796ZR3 - Maturity 2023 06 29 - Percentage Continuously Compound Annual Rate of Returns")
abline(lm(y ~ x), col="green", lty = 1, lwd=2)
abline(h = 0, col = "black")
arrows(120,0, 125,0, length = 0.15, angle = 20, lty=1, lwd=1, col="black")
axis(1, at=x[c(seq(1,length(x),by=11),length(x))], tick=TRUE, labels=FALSE)
text(x=x[c(seq(1,length(x),by=11),length(x))], par("usr")[3], 
     labels=Data_df$Date[c(seq(1,length(x),by=11),length(x))], 
     srt=330, xpd=TRUE, adj=c(-0.2,1.2), cex=0.80)
axis(2, at=seq(round(min(y),2), round(max(y),2), by = 0.01), tick=TRUE, labels=FALSE)
text(y=seq(round(min(y),2), round(max(y),2), by = 0.01), par("usr")[3], 
     labels=label_percent(scale=100, accuracy = 0.01)(seq(round(min(y),2), round(max(y),2), by = 0.01)), 
     srt=0, xpd=TRUE, pos=2, offset=3.3, cex=0.80)
lines(predict(loess(y ~ x)), col="magenta", lty = 3, lwd=2)
legend("topleft", 
       legend=c("perc. cont. comp. ann. returns", "regression line", "LOESS", "x-axis"),
       col=c("#FF0000", "green", "magenta", "black"), 
       pch=c(16, NA, NA, NA), lty=c(0, 1, 3, 1), lwd=0.5,
       cex=0.80, x.intersp=0.50, y.intersp=0.40, text.width=2, seg.len=1,
       inset=-0.01, bty="n")
#
# We repeat the above procedure with the bond named Cusip 912796ZN2 with maturity 2023 12 28.
Cusip_912796ZN2_df <- Data_df[which(Data_df$Cusip=="912796ZN2"),]
head(Cusip_912796ZN2_df, 10)
tail(Cusip_912796ZN2_df, 10)
# We add a column *Ret.Rate.at.Maturity* and *Perc.Ret.Rate.at.Maturity*
Cusip_912796ZN2_df <- add_column(Cusip_912796ZN2_df, 
                                 Ret.Rate.at.Maturity=(100-Cusip_912796ZN2_df$End.of.Day)/Cusip_912796ZN2_df$End.of.Day,
                                 Perc.Ret.Rate.at.Maturity=label_percent(accuracy = 0.001)((100-Cusip_912796ZN2_df$End.of.Day)/Cusip_912796ZN2_df$End.of.Day),
                                 .after="End.of.Day")
head(Cusip_912796ZN2_df, 10)
tail(Cusip_912796ZN2_df, 10)
# We compute the annual rate of return according to the formulas
# (1+r_a)^t = 1+r_t; r_a = (1+r_t)^(1/t)-1
# where r_a=annual rate of return, t=time to maturity (in years), r_t=rate of return in the period t.
# or 
# (1+r_a)^(t/365.2425) = 1+r_t; r_a = (1+r_t)^(365.2425/t)-1
# where r_a=annual rate of return, t=time to maturity (in days), r_t=rate of return in the period t.
#
Ann.Ret.Rate_01 <- (1+Cusip_912796ZN2_df$Ret.Rate.at.Maturity)^(1/Cusip_912796ZN2_df$Years.to.Maturity)-1
head(Ann.Ret.Rate_01, 20)
# [1] 0.04768009 0.04756560 0.04833899 0.04738832 0.04743708 0.04798788 0.04798427 0.04720499 0.04699537 0.04729813 0.04707334 0.04729108 0.04721748
# [14] 0.04771946 0.04738400 0.04693834 0.04715583 0.04707688 0.04758368 0.04713817
tail(Ann.Ret.Rate_01, 20)
# [1] 0.04871300 0.04897550 0.04766569 0.04809619 0.04771984 0.04851356 0.04861782 0.04828730 0.04839151 0.04971747 0.05043992 0.04945567 0.04977722
# [14] 0.05020763 0.05003958 0.05181626 0.05224711 0.05267808 0.05327433 0.05396590
#
Ann.Ret.Rate_02 <- (1+Cusip_912796ZN2_df$Ret.Rate.at.Maturity)^(365.2425/Cusip_912796ZN2_df$Days.to.Maturity)-1
head(Ann.Ret.Rate_02, 20)
# [1] 0.04768009 0.04756560 0.04833899 0.04738832 0.04743708 0.04798788 0.04798427 0.04720499 0.04699537 0.04729813 0.04707334 0.04729108 0.04721748
# [14] 0.04771946 0.04738400 0.04693834 0.04715583 0.04707688 0.04758368 0.04713817
tail(Ann.Ret.Rate_02, 20)
# [1] 0.04871300 0.04897550 0.04766569 0.04809619 0.04771984 0.04851356 0.04861782 0.04828730 0.04839151 0.04971747 0.05043992 0.04945567 0.04977722
# [14] 0.05020763 0.05003958 0.05181626 0.05224711 0.05267808 0.05327433 0.05396590
#
identical(Ann.Ret.Rate_01, Ann.Ret.Rate_02)
# [1] FALSE
identical(round(Ann.Ret.Rate_01,15), round(Ann.Ret.Rate_02,15))
# [1] TRUE
#
# We also compute the annual continuously compound rate of return according to the formula
# 1+r_a=exp(ro_a)
# or
# ro_a=log(1+r_a)
#
Cont.Comp.Ann.Ret.Rate <- log(1+Ann.Ret.Rate_01)
head(Cont.Comp.Ann.Ret.Rate, 20)
# [1] 0.04657828 0.04646900 0.04720700 0.04629975 0.04634630 0.04687202 0.04686858 0.04612470 0.04592451 0.04621364 0.04599897 0.04620691 0.04613663
# [14] 0.04661586 0.04629563 0.04587004 0.04607775 0.04600236 0.04648625 0.04606089
tail(Cont.Comp.Ann.Ret.Rate, 20)
# [1] 0.04756370 0.04781397 0.04656454 0.04697537 0.04661622 0.04737351 0.04747294 0.04715769 0.04725709 0.04852105 0.04920905 0.04827162 0.04857797
# [14] 0.04898789 0.04882786 0.05051844 0.05092799 0.05133747 0.05190372 0.05256010
#
# We add the columns *Ann.Ret.Rate*, *Perc.Ann.Ret.Rate*, *Cont.Comp.Ann.Ret.Rate*, and *Perc.Cont.Comp.Ann.Ret.Rate*
Cusip_912796ZN2_df <- add_column(Cusip_912796ZN2_df,
                                 Ann.Ret.Rate=round(Ann.Ret.Rate_01,14), 
                                 Perc.Ann.Ret.Rate=label_percent(accuracy = 0.001)(round(Ann.Ret.Rate_01,14)),
                                 Cont.Comp.Ann.Ret.Rate=Cont.Comp.Ann.Ret.Rate, 
                                 Perc.Cont.Comp.Ann.Ret.Rate = label_percent(accuracy = 0.001)(Cont.Comp.Ann.Ret.Rate),
                                 .after="Perc.Ret.Rate.at.Maturity")
head(Cusip_912796ZN2_df, 20)
tail(Cusip_912796ZN2_df, 20)
#
save(Cusip_912796ZN2_df, file="Cusip_912796ZN2_df.RData")
# rm(Cusip_912796ZN2_df)
# head(Cusip_912796ZN2_df)
# load("Cusip_912796ZN2_df.RData")
# head(Cusip_912796ZN2_df, 20)
# tail(Cusip_912796ZN2_df, 20)
#
# We consider a draft plot of the percentage continuously annual compound rate of return
y <- Cusip_912796ZN2_df$Cont.Comp.Ann.Ret.Rate
x <- 1:length(y)
plot(x,y, type = "p", pch=16, col="#FF0000", xaxt = "n", xlab="", yaxt = "n", ylab="", ylim=c(round(min(y),2), round(max(y),2)),
     main="Bond Cusip 912796ZN2 - Maturity 2023 12 28 - Percentage Continuously Compound Annual Rate of Returns")
abline(lm(y ~ x), col="green", lty = 1, lwd=2)
abline(h = 0, col = "black")
arrows(120,0, 125,0, length = 0.15, angle = 20, lty=1, lwd=1, col="black")
axis(1, at=x[c(seq(1,length(x),by=11),length(x))], tick=TRUE, labels=FALSE)
text(x=x[c(seq(1,length(x),by=11),length(x))], par("usr")[3], 
     labels=Data_df$Date[c(seq(1,length(x),by=11),length(x))], 
     srt=330, xpd=TRUE, adj=c(-0.2,1.2), cex=0.80)
axis(2, at=seq(round(min(y),2), round(max(y),2), by = 0.01), tick=TRUE, labels=FALSE)
text(y=seq(round(min(y),2), round(max(y),2), by = 0.005), par("usr")[3], 
     labels=label_percent(scale=100, accuracy = 0.005)(seq(round(min(y),2), round(max(y),2), by = 0.005)), 
     srt=0, xpd=TRUE, pos=2, offset=3.3, cex=0.80)
lines(predict(loess(y ~ x)), col="magenta", lty = 3, lwd=2)
legend("topleft", 
       legend=c("perc. cont. comp. ann. returns", "regression line", "LOESS", "x-axis"),
       col=c("#FF0000", "green", "magenta", "black"), 
       pch=c(16, NA, NA, NA), lty=c(0, 1, 3, 1), lwd=0.5,
       cex=0.80, x.intersp=0.50, y.intersp=0.40, text.width=2, seg.len=1,
       inset=-0.01, bty="n")
#
# Now, we consider the point of view of a speculator, who aims to trade bills on the basis of their daily closure price.
# The speculator is interested in the rate of return of the daily trading.
# ............
###############################################################################################################################################
##############################################################################################################################
# European Options on Standard & Poor 500 (Yahoo Finance - ^SPX)
# library(quantmod)
# SPX_Opt_2023_05_12_06_16 <- getOptionChain("^SPX", Exp="2023-06-16", src='yahoo')
# Error in getOptionChain.yahoo(Symbols = "^SPX", Exp = "2023-06-16") : 
# Unable to obtain yahoo crumb. If this is being called from a GDPR country, Yahoo requires GDPR consent, which cannot be scripted
# class(SPX_Opt_2023_05_12_06_16)
# length(SPX_Opt_2023_05_12_06_16)
# show(SPX_Opt_2023_05_12_06_16[[1]])
# class(SPX_Opt_2023_05_12_06_16[[1]])
# nrow(SPX_Opt_2023_05_12_06_16[[1]])
# show(SPX_Opt_2023_05_12_06_16[[2]])
# class(SPX_Opt_2023_05_12_06_16[[2]])
# nrow(SPX_Opt_2023_05_12_06_16[[2]])
# show(SPX_Opt_2023_05_12_06_16[[1]]$Strike)
# show(SPX_Opt_2023_05_12_06_16[[2]]$Strike)
# Strike <- sort(union(SPX_Opt_2023_05_12_06_16[[1]]$Strike, SPX_Opt_2023_05_12_06_16[[2]]$Strike))
# show(Strike)
# length(Strike)
# Call_Indx <- sapply(Strike, function(x) which(SPX_Opt_2023_05_12_06_16[[1]]$Strike==x)[1])
# Put_Indx <- sapply(Strike, function(x) which(SPX_Opt_2023_05_12_06_16[[2]]$Strike==x)[1])
# 
# SPX_Opt_2023_05_12_06_16_df <- data.frame(Indx=1:length(Strike),
#                                     Call_ContractID=SPX_Opt_2023_05_12_06_16[[1]]$ContractID[Call_Indx], 
#                                     Call_Bid=SPX_Opt_2023_05_12_06_16[[1]]$Bid[Call_Indx],
#                                     Call_Ask=SPX_Opt_2023_05_12_06_16[[1]]$Ask[Call_Indx],
#                                     Call_Vol=SPX_Opt_2023_05_12_06_16[[1]]$Vol[Call_Indx],
#                                     Call_OI=SPX_Opt_2023_05_12_06_16[[1]]$OI[Call_Indx],
#                                     Call_PrChg=SPX_Opt_2023_05_12_06_16[[1]]$Chg[Call_Indx],
#                                     Call_PrChgPct=SPX_Opt_2023_05_12_06_16[[1]]$ChgPct[Call_Indx],
#                                     Call_LastTrTime=SPX_Opt_2023_05_12_06_16[[1]]$LastTradeTime[Call_Indx],
#                                     Call_LastPr=SPX_Opt_2023_05_12_06_16[[1]]$Last[Call_Indx],
#                                     Call_ImplVol=SPX_Opt_2023_05_12_06_16[[1]]$IV[Call_Indx],
#                                     Call_ITM=SPX_Opt_2023_05_12_06_16[[1]]$ITM[Call_Indx],
#                                     Strike=Strike,
#                                     Put_ITM=SPX_Opt_2023_05_12_06_16[[2]]$ITM[Put_Indx],
#                                     Put_ImplVol=SPX_Opt_2023_05_12_06_16[[2]]$IV[Put_Indx],
#                                     Put_LastPr=SPX_Opt_2023_05_12_06_16[[2]]$Last[Put_Indx],
#                                     Put_LastTrTime=SPX_Opt_2023_05_12_06_16[[2]]$LastTradeTime[Put_Indx],
#                                     Put_PrChgPct=SPX_Opt_2023_05_12_06_16[[2]]$ChgPct[Put_Indx],
#                                     Put_PrChg=SPX_Opt_2023_05_12_06_16[[2]]$Chg[Put_Indx],
#                                     Put_OI=SPX_Opt_2023_05_12_06_16[[2]]$OI[Put_Indx],
#                                     Put_Vol=SPX_Opt_2023_05_12_06_16[[2]]$Vol[Put_Indx],
#                                     Put_Ask=SPX_Opt_2023_05_12_06_16[[2]]$Ask[Put_Indx],
#                                     Put_Bid=SPX_Opt_2023_05_12_06_16[[2]]$Bid[Put_Indx],
#                                     Put_ContractID=SPX_Opt_2023_05_12_06_16[[2]]$ContractID[Put_Indx])
# head(SPX_Opt_2023_05_12_06_16_df,10)                                   
# tail(SPX_Opt_2023_05_12_06_16_df,10)
# write.csv(SPX_Opt_2023_05_12_06_16_df,"C:/Users/rober/My Documents - Notebook (local)/My Classes/MPSMF/R-Scripts & Data/SPX_Option_Chain_2023_05_12_06_16.csv")
# dir("C:/Users/rober/My Documents - Notebook (local)/My Classes/MPSMF/R-Scripts & Data")
# rm(SPX_Opt_2023_05_12_06_16_df)
# head(SPX_Opt_2023_05_12_06_16_df)
#
SPX_Opt_2023_04_11_06_16_df <- read.csv("SPX_Option_Chain_2023_04_11_06_16.csv")
class(SPX_Opt_2023_04_11_06_16_df)
# [1] "data.frame"
head(SPX_Opt_2023_04_11_06_16_df,10)
tail(SPX_Opt_2023_04_11_06_16_df,10)
#
class(SPX_Opt_2023_04_11_06_16_df$Call_LastTrTime)
# [1] "character"
Call_LastTrDate_df <- data.frame(Call_LastTrDate=as.Date(substring(na.rm(SPX_Opt_2023_04_11_06_16_df$Call_LastTrTime), 1, 11), format="%d/%m/%Y"))
class(Call_LastTrDate_df)
# [1] "data.frame"
head(Call_LastTrDate_df,20)
tail(Call_LastTrDate_df,20)
nrow(Call_LastTrDate_df)
Call_LastTrDate_tb <- table(Call_LastTrDate_df)   
class(Call_LastTrDate_tb)
# [1] "table"
show(Call_LastTrDate_tb)
# 2022-07-13 2022-09-15 2022-10-13 2022-10-14 2022-10-31 2022-11-14 2022-11-18 2022-12-15 2022-12-19 2022-12-20 2022-12-28 2023-01-06 2023-01-10 
#      1          2          2          1          1          2          2          1          3          1          2          1          1 
# 2023-01-18 2023-01-23 2023-01-27 2023-01-30 2023-01-31 2023-02-02 2023-02-07 2023-02-08 2023-02-10 2023-02-13 2023-02-16 2023-02-17 2023-02-22 
#      1          1          1          1          2          2          1          1          2          2          1          4          2 
# 2023-02-23 2023-02-27 2023-03-02 2023-03-05 2023-03-06 2023-03-07 2023-03-10 2023-03-13 2023-03-14 2023-03-15 2023-03-16 2023-03-17 2023-03-20 
#      3          2          1          1          1          3          3          2          2          3          4          1          7 
# 2023-03-21 2023-03-22 2023-03-23 2023-03-24 2023-03-27 2023-03-28 2023-03-29 2023-03-30 2023-03-31 2023-04-03 2023-04-04 2023-04-05 2023-04-06 
#      3          3          4          8          3          7          2          8         16          7         15         13          5 
# 2023-04-10 2023-04-11 2023-04-12 
#     33         75          1 
#
Put_LastTrDate_df <- data.frame(Put_LastTrDate=as.Date(substring(na.rm(SPX_Opt_2023_04_11_06_16_df$Put_LastTrTime), 1, 11), format="%d/%m/%Y"))
class(Put_LastTrDate_df)
# [1] "data.frame"
head(Put_LastTrDate_df,20)
tail(Put_LastTrDate_df,20)
nrow(Put_LastTrDate_df)
Put_LastTrDate_tb <- table(Put_LastTrDate_df)   
class(Put_LastTrDate_tb)
# [1] "table"
show(Put_LastTrDate_tb)
Put_LastTrDate
# 2021-12-08 2022-03-30 2022-04-22 2022-05-26 2022-06-06 2022-06-13 2022-06-22 2022-07-21 2022-07-26 2022-11-10 2022-11-16 2022-11-23 2022-12-19 
#     1          1          1          1          1          1          1          1          1          1          1          1          2 
# 2022-12-20 2022-12-23 2023-02-03 2023-02-06 2023-02-15 2023-02-17 2023-02-24 2023-03-01 2023-03-06 2023-03-10 2023-03-13 2023-03-16 2023-03-21 
#     1          1          1          1          1          3          1          1          1          1          1          3          1 
# 2023-03-23 2023-03-24 2023-03-27 2023-03-29 2023-03-30 2023-03-31 2023-04-03 2023-04-04 2023-04-05 2023-04-06 2023-04-10 2023-04-11 2023-04-12 
#     2          2          3          1          4          1          3         15         14         23         44        145          2 
#
Call_LastTrDate_2023_04_11_Indx <- SPX_Opt_2023_04_11_06_16_df$Indx[which(Call_LastTrDate_df$Call_LastTrDate=="2023-04-11")]
length(Call_LastTrDate_2023_04_11_Indx)
# [1] 75
show(Call_LastTrDate_2023_04_11_Indx)
# [1]   5  15  44  52  99 121 129 130 131 134 139 144 149 150 152 153 154 157 159 164 168 169 170 171 172 175 177 178 179 181 187 189 193 194 195 197
# [37] 198 200 201 203 204 207 208 210 211 212 213 214 216 217 219 220 221 222 223 224 225 226 228 229 230 231 232 233 234 235 237 239 240 241 244 245
# [73] 246 253 257
Put_LastTrDate_2023_04_11_Indx <- SPX_Opt_2023_04_11_06_16_df$Indx[which(Put_LastTrDate_df$Put_LastTrDate=="2023-04-11")]
length(Put_LastTrDate_2023_04_11_Indx)
# [1] 145
show(Put_LastTrDate_2023_04_11_Indx)
# [1]  15  16  17  20  21  22  23  24  26  29  30  32  37  38  41  42  43  44  46  47  48  49  52  53  54  55  56  57  58  59  61  63  64  65  66  71
# [37]  72  73  74  76  77  78  80  81  82  83  85  87  88  89  94  95  98 102 103 104 106 107 108 109 113 114 116 118 119 120 122 124 125 126 129 130
# [73] 131 132 133 134 136 137 138 139 140 142 144 146 148 149 151 152 154 155 156 157 159 160 161 162 163 164 165 166 169 170 173 174 175 176 178 179
# [109] 180 181 182 183 184 185 186 187 188 189 191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 214 216 221 222 224 229 233 236 242
# [145] 246
Call_Put_2023_04_11_Indx <- intersect(Call_LastTrDate_2023_04_11_Indx, Put_LastTrDate_2023_04_11_Indx)
length(Call_Put_2023_04_11_Indx)
# [1] 41
show(Call_Put_2023_04_11_Indx)
# [1]  15  44  52 129 130 131 134 139 144 149 152 154 157 159 164 169 170 175 178 179 181 187 189 193 194 195 197 198 200 201 203 204 207 214 216 221
# [37] 222 224 229 233 246
#
Call_LastTrDate_2023_04_10_Indx <- SPX_Opt_2023_04_11_06_16_df$Indx[which(Call_LastTrDate_df$Call_LastTrDate=="2023-04-10")]
length(Call_LastTrDate_2023_04_10_Indx)
# [1] 33
show(Call_LastTrDate_2023_04_10_Indx)
# [1]  26  35  65  71  72 119 132 133 146 148 155 156 162 165 166 167 173 174 176 182 183 184 185 190 191 196 202 205 206 209 215 236 238
Put_LastTrDate_2023_04_10_Indx <- SPX_Opt_2023_04_11_06_16_df$Indx[which(Put_LastTrDate_df$Put_LastTrDate=="2023-04-10")]
length(Put_LastTrDate_2023_04_10_Indx)
# [1] 44
show(Put_LastTrDate_2023_04_10_Indx)
# [1]   7  12  13  19  28  31  34  40  50  51  60  70  90  96  97  99 101 105 110 111 123 135 141 143 150 171 172 208 210 211 213 215 217 218 219 220
# [37] 223 230 237 239 250 256 285 287
Call_Put_2023_04_10_Indx <- intersect(Call_LastTrDate_2023_04_10_Indx, Put_LastTrDate_2023_04_10_Indx)
length(Call_Put_2023_04_10_Indx)
# [1] 1
show(Call_Put_2023_04_10_Indx)
# [1] 215
#
# Put-Call parity
# P_0 = C_0 - S_0 + K/(1+r_f)
# C_0 - P_0 = S_0 - K/(1+r_f)
#
x <- SPX_Opt_2023_04_11_06_16_df$Strike[Call_Put_2023_04_11_Indx]
length(x)
# [1] 41
show(x)
# [1] 2000 3050 3240 3775 3780 3785 3800 3825 3850 3875 3890 3900 3915 3925 3950 3975 3980 4005 4020 4025 4035 4065 4075 4095 4100 4105 4115 4120 4130
# [30] 4135 4145 4150 4165 4200 4210 4235 4240 4250 4275 4310 4420
y <- SPX_Opt_2023_04_11_06_16_df$Call_LastPr[Call_Put_2023_04_11_Indx]-SPX_Opt_2023_04_11_06_16_df$Put_LastPr[Call_Put_2023_04_11_Indx]
length(y)
# [1] 41
show(y)
# [1] 2115.13  728.79      NA  257.87  244.58  242.79  316.23  323.83  264.90  263.62  226.77  233.70  202.59  206.11  193.48  166.70  140.51  135.24
# [19]  127.43  118.12   95.45   66.46   68.30   49.27   37.39   37.30   21.92   11.00    5.26    1.90   -5.00  -10.05  -19.50  -61.85  -79.46 -101.50
# [37]  -94.40 -113.10 -150.80 -179.40      NA
#
Data_df <- data.frame(x,y)
nrow(Data_df)
# [1] 41
Data_df <- na.omit(Data_df)
nrow(Data_df)
# [1] 39
head(Data_df,10)
tail(Data_df,10)
rownames(Data_df) <- 1:nrow(Data_df)
head(Data_df,10)
tail(Data_df,10)
n <- nrow(Data_df)
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2024-2024", 
                             paste("Scatter Plot of the Call-Put Difference Against the Strike Price")))
subtitle_content <- bquote(paste("Data set size",~~.(n),~~"sample points;    Evaluation Date 2023-04-11;   Maturity Date 2023-06-16"))
caption_content <- "Author: Roberto Monte" 
# To obtain the sub-multiples of the length of the data set as a hint on the number of breaks to use
# library(numbers)
# primeFactors(n)
x_breaks_num <- 8
x_breaks_low <- min(Data_df$x)
x_breaks_up <- max(Data_df$x)
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- format(x_breaks, scientific=FALSE)
J <- 0.2
x_lims <- c(x_breaks_low-J*x_binwidth,x_breaks_up+J*x_binwidth)
x_name <- bquote("strike")
y_breaks_num <- 10
y_max <- max(na.rm(Data_df$y))
y_min <- min(na.rm(Data_df$y))
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- y_min
y_breaks_up <- y_max
y_breaks <- seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth)
if((y_breaks_up-max(y_breaks))>y_binwidth/2){y_breaks <- c(y_breaks,y_breaks_up)}
y_labs <- format(y_breaks, scientific=FALSE)
y_name <- bquote("call-put difference")
K <- 0.2
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
col_1 <- bquote("data set sample points")
col_2 <- bquote("regression line")
col_3 <- bquote("LOESS curve")
leg_labs <- c(col_1, col_2, col_3)
leg_cols <- c("col_1"="blue", "col_2"="green", "col_3"="red")
leg_ord <- c("col_1", "col_2", "col_3")
Call_Put_Strike_Pr_2023_04_11_06_16_sp <- ggplot(Data_df, aes(x=x, y=y)) +
  geom_smooth(alpha=1, linewidth=0.8, linetype="dashed", aes(color="col_3"),
              method="loess", formula=y ~ x, se=FALSE, fullrange = FALSE) +
  geom_smooth(alpha=1, linewidth=0.8, linetype="solid", aes(color="col_2"),
              method="lm" , formula=y ~ x, se=FALSE, fullrange=FALSE) +
  geom_point(alpha=1, size=1.0, shape=19, aes(color="col_1")) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_ord,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x=element_text(angle=0, vjust=1),
        legend.key.width=unit(1.0,"cm"), legend.position="bottom")
plot(Call_Put_Strike_Pr_2023_04_11_06_16_sp)
#
PutCall_par_lm <- lm(y~x)
summary(PutCall_par_lm)
# Call: lm(formula = y ~ x)
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -269.692    1.934    7.759   17.328  145.753 
#
# Coefficients:
#              Estimate   Std. Error t value Pr(>|t|)    
# (Intercept) 3818.70000   94.27179   40.51   <2e-16 ***
# x             -0.92466    0.02369  -39.03   <2e-16 ***
#   ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 56.77 on 37 degrees of freedom
# (2 observations deleted due to missingness)
# Multiple R-squared:  0.9763,	Adjusted R-squared:  0.9757 
# F-statistic:  1524 on 1 and 37 DF,  p-value: < 2.2e-16
#
S_0 <- PutCall_par_lm$coefficients[1]
show(S_0)
# (Intercept) 
# 3818.7
# 4094.02
# SPX Market Price 4,108.94 -0.17 (-0.00%) At close: April 11 04:55PM EDT
#
# -1/(1+r_f)=c, -1/c=1+r_f, -(1/c+1)=r_f, 
#
r_f <- -(1/PutCall_par_lm$coefficients[2]+1)
show(r_f)
# x
# 0.08147676 
# 0.01144599
#
Days_to_Mat <- as.vector(difftime("2023-06-16", "2023-04-11"))
show(Days_to_Mat)
# 66
#
r_f_a=(1+r_f)^(365.2425/Days_to_Mat)-1
show(r_f_a)
# 0.5425894
# 0.06500779
#
label_percent(accuracy = 0.001)(r_f_a)
# 54.259%
# 6.501%
#
# Put-Call parity
# P_0 = C_0 - S_0 + K/(1+r_f)
# P_0 - C_0 + S_0 = K/(1+r_f)
# SPX Market Price 4,108.94 -0.17 (-0.00%) At close: April 11 04:55PM EDT
#
S_0 <- 4108.94
#
x <- SPX_Opt_2023_04_11_06_16_df$Strike[Call_Put_2023_04_11_Indx]
show(x)
length(x)
y <- SPX_Opt_2023_04_11_06_16_df$Put_LastPr[Call_Put_2023_04_11_Indx]-SPX_Opt_2023_04_11_06_16_df$Call_LastPr[Call_Put_2023_04_11_Indx]+S_0
show(y)
length(y)
#
Data_df <- data.frame(x,y)
nrow(Data_df)
Data_df <- na.omit(Data_df)
nrow(Data_df)
head(Data_df,10)
tail(Data_df,10)
rownames(Data_df) <- 1:nrow(Data_df)
nrow(Data_df)
head(Data_df,10)
tail(Data_df,10)
n <- nrow(Data_df)
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2024-2024", 
                             paste("Scatter Plot of the Put-Call+Price Combination Against the Strike Price")))
subtitle_content <- bquote(paste("Data set size",~~.(n),~~"sample points;    Evaluation Date 2023-04-11;   Maturity Date 2023-06-16"))
caption_content <- "Author: Roberto Monte" 
# To obtain the sub-multiples of the length of the data set as a hint on the number of breaks to use
# library(numbers)
# primeFactors(n)
x_breaks_num <- 8
x_breaks_low <- min(Data_df$x)
x_breaks_up <- max(Data_df$x)
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- format(x_breaks, scientific=FALSE)
J <- 0.2
x_lims <- c(x_breaks_low-J*x_binwidth,x_breaks_up+J*x_binwidth)
x_name <- bquote("strike")
y_breaks_num <- 10
y_max <- max(na.rm(Data_df$y))
y_min <- min(na.rm(Data_df$y))
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- y_min
y_breaks_up <- y_max
y_breaks <- seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth)
if((y_breaks_up-max(y_breaks))>y_binwidth/2){y_breaks <- c(y_breaks,y_breaks_up)}
y_labs <- format(y_breaks, scientific=FALSE)
y_name <- bquote("call-put difference")
K <- 0.2
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
col_1 <- bquote("data set sample points")
col_2 <- bquote("regression line")
col_3 <- bquote("LOESS curve")
leg_labs <- c(col_1, col_2, col_3)
leg_cols <- c("col_1"="blue", "col_2"="green", "col_3"="red")
leg_ord <- c("col_1", "col_2", "col_3")
Call_Put_Price_Strike_Pr_2023_04_11_06_16_sp <- ggplot(Data_df, aes(x=x, y=y)) +
  geom_smooth(alpha=1, linewidth=0.8, linetype="dashed", aes(color="col_3"),
              method="loess", formula=y ~ x, se=FALSE, fullrange = FALSE) +
  geom_smooth(alpha=1, linewidth=0.8, linetype="solid", aes(color="col_2"),
              method="lm" , formula=y ~ x, se=FALSE, fullrange=FALSE) +
  geom_point(alpha=1, size=1.0, shape=19, aes(color="col_1")) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_ord,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x=element_text(angle=0, vjust=1),
        legend.key.width=unit(1.0,"cm"), legend.position="bottom")
plot(Call_Put_Price_Strike_Pr_2023_04_11_06_16_sp)
#
PutCallPrice_par_lm <- lm(y~0+x)
summary(PutCallPrice_par_lm)
#
# 1/(1+r_f)=c, 1/c=1+r_f, 1/c-1=r_f, 
#
r_f <- 1/PutCallPrice_par_lm$coefficients[1]-1
show(r_f)
# 0.007666662
#
Days_to_Mat <- as.vector(difftime("2023-06-16", "2023-04-11"))
show(Days_to_Mat)
# 66
#
r_f_a=(1+r_f)^(365.2425/Days_to_Mat)-1
show(r_f_a)
# 0.04317122
#
label_percent(accuracy = 0.001)(r_f_a)
# 4.317%
#
################################################################################################################################################
