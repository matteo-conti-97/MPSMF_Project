###############################################################################################################################################
################################################################## References #################################################################
###############################################################################################################################################
### Books:
#
# Kleiber, Christian, Zeileis, Achim - Applied Econometrics with R 
# Use R! - Springer, 2008
#
# Shumway, Robert H., Stoffer, David S. - Time Series Analysis and Its Applications (with R Examples) 4th Edition
# Springer Texts in Statistics - Springer, 2017
# https://www.stat.pitt.edu/stoffer/tsa4/tsa4.pdf
#

### Websites:

# Rob J Hyndman and George Athanasopoulos - Forecasting: Principles and Practice
# Monash Univeristy, Australia
# https://otexts.com/fpp2/

# Robert H. Shumway, David S. Stoffer - Time Series Analysis and Its AP_POlications (with R Examples) 4th Edition
# SPringer Texts in Statistics - SPringer Verlag
# https://www.stat.pitt.edu/stoffer/tsa4/tsa4.pdf

# R - Residual Diagnostic
# https://cran.r-project.org/web/packages/olsrr/vignettes/residual_diagnostics.html

###############################################################################################################################################
##################################################################### Libraries ###############################################################
###############################################################################################################################################
# Reading libraries
# https://www.r-project.org/other-docs.html
library(base)
# https://rdrr.io/r/#base
# https://www.math.ucla.edu/~anderson/rw1001/library/base/html/00Index.html
#
library(DescTools)
#
library(utils)
#
library(stats)
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/00Index.html
#
library(withr)
#
library(graphics)
library(withr)
library(timeDate)
library(timeSeries)
library(fBasics)
library(fOptions)
library("data.table")
library(reshape2)
library(tidyverse)
library(dplyr)
###############################################################################################################################################
###############################################################################################################################################
library(tibble)
library(dplyr)
library(tidyverse)
library("data.table")
library(reshape2)
library(ggplot2)
library(scales)
library(survival)
library(MASS)
library(fitdistrplus)
# https://cran.r-project.org/web/packages/fitdistrplus/fitdistrplus.pdf
# https://cran.r-project.org/web/packages/fitdistrplus/vignettes/paper2JSS.pdf
#
library(lattice)
#
library(extraDistr)
library(gamlss.dist)
#
library(nleqslv)
# https://cran.r-project.org/web/packages/nleqslv/nleqslv.pdf
#
library(NlcOptim)
library(pracma)
###############################################################################################################################################

library(tibble)
library(dplyr)
library(reshape2)
library(tidyverse)
library(ggplot2)
library("numbers")
library(scales)
library(gridExtra)
library(quantmod)
library(urca)
library(moments)
library(lmtest)
library(xts)
library(zoo)
library(TTR)
###############################################################################################################################################
library("data.table")
library(lubridate)
library(moments)
library(lmtest) 

library(strucchange)
library(broom)
library(rlang)
library(gridSVG)
library(grid)
###############################################################################################################################################
########################################################## Environmental Setting ##############################################################
###############################################################################################################################################
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
###############################################################################################################################################
###############################################################################################################################################
##################################################################### Model Setting ###########################################################
###############################################################################################################################################
# Parameters of the model
r <- 0.05
u <- 1 + 0.15
d <- 1 - 0.05
(u > r + 1) & (r + 1 > d)
# [1] TRUE
p <- (1+r-d)/(u-d)
q <- (u-(1+r))/(u-d)
show(c(p,q))
# [1] 0.5 0.5
S_0 <- 100
K_0 <- 100
N <- 5
###############################################################################################################################################
# Building the model
# First Procedure
S <- matrix(NA, nrow=N+1, ncol = N+1)
S[1,1] <- S_0
for(n in 1:N){
  for(k in 0:n){S[k+1,n+1] <- S_0*u^(n-k)*d^k}
}
show(S)
# [,1] [,2]   [,3]     [,4]      [,5]      [,6]
# [1,]  100  115 132.25 152.0875 174.90062 201.13572
# [2,]   NA   95 109.25 125.6375 144.48312 166.15559
# [3,]   NA   NA  90.25 103.7875 119.35562 137.25897
# [4,]   NA   NA     NA  85.7375  98.59812 113.38784
# [5,]   NA   NA     NA       NA  81.45062  93.66822
# [6,]   NA   NA     NA       NA        NA  77.37809
#
# library(timeDate)
# library(timeSeries)
# library(fBasics)
# library(fOptions)
BinomialTreePlot(S)
#
# Second Procedure
S <- matrix(NA, nrow=N+1, ncol = N+1)
S[1,1] <- S_0
for(n in 1:N){
  for(k in 0:n){S[n+1,k+1] <- round(S_0*u^k*d^(n-k),3)}
}
show(S)
# [,1]    [,2]    [,3]    [,4]    [,5]    [,6]
# [1,] 100.000      NA      NA      NA      NA      NA
# [2,]  95.000 115.000      NA      NA      NA      NA
# [3,]  90.250 109.250 132.250      NA      NA      NA
# [4,]  85.737 103.787 125.637 152.087      NA      NA
# [5,]  81.451  98.598 119.356 144.483 174.901      NA
# [6,]  77.378  93.668 113.388 137.259 166.156 201.136
#
S_df <- as.data.frame(S)
# library("data.table")
S_tb <- setDT(S_df)   
class(S_tb)
# [1] "data.table" "data.frame"
# library(reshape2)
S_rsh_df <- melt(S_tb, na.rm=FALSE)
show(S_rsh_df[1:20,])
# variable   value
# 1        V1 100.000
# 2        V1  95.000
# 3        V1  90.250
# 4        V1  85.737
# 5        V1  81.451
# 6        V1  77.378
# 7        V2      NA
# 8        V2 115.000
# 9        V2 109.250
# 10       V2 103.787
# 11       V2  98.598
# 12       V2  93.668
# 13       V3      NA
# 14       V3      NA
# 15       V3 132.250
# 16       V3 125.637
# 17       V3 119.356
# 18       V3 113.388
# 19       V4      NA
# 20       V4      NA
# We remove the "variable" column
# library(tidyverse)
# library(dplyr)
S_mod_rsh_df <- subset(S_rsh_df, select = -variable)
head(S_mod_rsh_df,15)
#     value
# 1  100.000
# 2   95.000
# 3   90.250
# 4   85.737
# 5   81.451
# 6   77.378
# 7       NA
# 8  115.000
# 9  109.250
# 10 103.787
# 11  98.598
# 12  93.668
# 13      NA
# 14      NA
# 15 132.250
# We also change name "S_value" to the "value" column.
S_mod_rsh_df <- dplyr::rename(S_mod_rsh_df, S_value = value)
head(S_mod_rsh_df,15)
#     S_value
# 1  100.000
# 2   95.000
# 3   90.250
# 4   85.737
# 5   81.451
# 6   77.378
# 7       NA
# 8  115.000
# 9  109.250
# 10 103.787
# 11  98.598
# 12  93.668
# 13      NA
# 14      NA
# 15 132.250
#
# In the end, we add an Index identifying variable to the data frame S_rsh_df
S_mod_rsh_df <- add_column(S_mod_rsh_df, Index=rep(0:(nrow(S_df)-1), times=ncol(S_df)), .before="S_value")
head(S_mod_rsh_df,15)
# Index S_value
# 1      0 100.000
# 2      1  95.000
# 3      2  90.250
# 4      3  85.737
# 5      4  81.451
# 6      5  77.378
# 7      0      NA
# 8      1 115.000
# 9      2 109.250
# 10     3 103.787
# 11     4  98.598
# 12     5  93.668
# 13     0      NA
# 14     1      NA
# 15     2 132.250
#
# We are finally in a position to draw a draft plot of the price lattice
# library(ggplot2)
Data_df <- S_mod_rsh_df
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Corso di Metodi Probabilistici e Statistici per i Mercati Finanziari", 
                             "Example of Lattice Plot for Stock Price in CRR Model"))
subtitle_content <- bquote(paste("market periods N = ",.(N), ", risk free rate r = ",.(r), ", up factor u = ",.(u), ", down factor d = ",.(d), ", risk neutral probability distribution (p,q) = (",.(p),",",.(q),")."))
caption_content <- "Author: Roberto Monte"
y_breaks_num <- 4
y_margin <- 5
y_breaks_low <- floor(min(Data_df$S_value, na.rm =TRUE))-y_margin
y_breaks_up <- ceiling(max(Data_df$S_value, na.rm =TRUE))+y_margin
y_breaks <- seq(from=y_breaks_low, to=y_breaks_up, length.out=y_breaks_num)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0
y_lims <- c((y_breaks_low-K*y_margin), (y_breaks_up+K*y_margin))
y_name <- bquote("stock values")
y1_txt <- bquote("stock values")
leg_labs <- c(y1_txt)
leg_vals <- c("y1_txt"="black")
leg_sort <- c("y1_txt")
S_lattice_sp <- ggplot(Data_df, aes(Index, S_value)) + 
  geom_point(aes(colour="y1_txt"), na.rm = TRUE) +
  geom_text(aes(label=round(S_value, 3)), hjust=1.0, vjust=-0.7, na.rm = TRUE, show.legend=FALSE) + 
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("time") + 
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  scale_color_manual(name="Legend", labels=leg_labs, values=leg_vals, breaks=leg_sort,
                     guide=guide_legend(override.aes=list(size = 2.5, shape=16, color="black"))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=0, vjust=1),
        legend.key.width = unit(0.80,"cm"), legend.key.height = unit(1, "cm"), legend.key = element_rect(colour="grey50", linewidth= 0.5),
        legend.position="bottom")
plot(S_lattice_sp)
###############################################################################################################################################
# Assume K=S_0=100 and consider the pay off of an American put option we have
AP_PO <- matrix(NA, nrow=N+1, ncol = N+1)
AP_PO[1,1] <- 0
for(n in 1:N){
  for(k in 0:N){AP_PO[n+1,k+1] <- round(max(100-S[n+1,k+1],0),3)}
}
show(AP_PO)
# [,1]  [,2] [,3] [,4] [,5] [,6]
# [1,]  0.000    NA   NA   NA   NA   NA
# [2,]  5.000 0.000   NA   NA   NA   NA
# [3,]  9.750 0.000    0   NA   NA   NA
# [4,] 14.263 0.000    0    0   NA   NA
# [5,] 18.549 1.402    0    0    0   NA
# [6,] 22.622 6.332    0    0    0    0
#
AP_PO_df <- as.data.frame(AP_PO)
# library("data.table")
AP_PO_tb <- setDT(AP_PO_df)   
class(AP_PO_tb)
# [1] "data.table" "data.frame"
# library(reshape2)
AP_PO_rsh_df <- melt(AP_PO_tb, na.rm=FALSE)
show(AP_PO_rsh_df[1:20,])
#    variable  value
# 1        V1  0.000
# 2        V1  5.000
# 3        V1  9.750
# 4        V1 14.263
# 5        V1 18.549
# 6        V1 22.622
# 7        V2     NA
# 8        V2  0.000
# 9        V2  0.000
# 10       V2  0.000
# 11       V2  1.402
# 12       V2  6.332
# 13       V3     NA
# 14       V3     NA
# 15       V3  0.000
# 16       V3  0.000
# 17       V3  0.000
# 18       V3  0.000
# 19       V4     NA
# 20       V4     NA
AP_PO_mod_rsh_df <- subset(AP_PO_rsh_df, select = -variable)
AP_PO_mod_rsh_df <- dplyr::rename(AP_PO_mod_rsh_df, AP_PO_value=value)
head(AP_PO_mod_rsh_df,15)
#    AP_PO_value
# 1        0.000
# 2        5.000
# 3        9.750
# 4       14.263
# 5       18.549
# 6       22.622
# 7           NA
# 8        0.000
# 9        0.000
# 10       0.000
# 11       1.402
# 12       6.332
# 13          NA
# 14          NA
# 15       0.000
AP_PO_mod_rsh_df <- add_column(AP_PO_mod_rsh_df, Index=rep(0:(nrow(S_df)-1), times=ncol(AP_PO_df)), .before="AP_PO_value")
head(AP_PO_mod_rsh_df,15)
#    Index AP_PO_value
# 1      0       0.000
# 2      1       5.000
# 3      2       9.750
# 4      3      14.263
# 5      4      18.549
# 6      5      22.622
# 7      0          NA
# 8      1       0.000
# 9      2       0.000
# 10     3       0.000
# 11     4       1.402
# 12     5       6.332
# 13     0          NA
# 14     1          NA
# 15     2       0.000
AP_PO_mod_rsh_df <- add_column(AP_PO_mod_rsh_df, S_value=S_mod_rsh_df$S_value, .before="AP_PO_value")
head(AP_PO_mod_rsh_df,15)
#    Index S_value AP_PO_value
# 1      0 100.000       0.000
# 2      1  95.000       5.000
# 3      2  90.250       9.750
# 4      3  85.737      14.263
# 5      4  81.451      18.549
# 6      5  77.378      22.622
# 7      0      NA          NA
# 8      1 115.000       0.000
# 9      2 109.250       0.000
# 10     3 103.787       0.000
# 11     4  98.598       1.402
# 12     5  93.668       6.332
# 13     0      NA          NA
# 14     1      NA          NA
# 15     2 132.250       0.000
#
Data_df <- AP_PO_mod_rsh_df
length <- N
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Corso di Metodi Probabilistici e Statistici per i Mercati Finanziari", 
                             "Example of Lattice Plot for American Put Option - Exercise Values in CRR Model"))
subtitle_content <- bquote(paste("market periods N = ",.(N),", risk free rate r = ",.(r),", up factor u = ",.(u),", down factor d = ",.(d),", risk neutral probability distribution (p,q) = (",.(p),",",.(q),"), exercise price K = ",.(K_0),"."))
caption_content <- "Author: Roberto Monte"
y_breaks_num <- 4
y_margin <- 5
y_breaks_low <- floor(min(Data_df$S_value, na.rm =TRUE))-y_margin
y_breaks_up <- ceiling(max(Data_df$S_value, na.rm =TRUE))+y_margin
y_breaks <- seq(from=y_breaks_low, to=y_breaks_up, length.out=y_breaks_num)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0
y_lims <- c((y_breaks_low-K*y_margin), (y_breaks_up+K*y_margin))
y_name <- bquote("stock values and american put option exercise value")
y1_txt <- bquote("stock values")
y2_txt <- bquote("american put otion payoffs")
leg_labs <- c(y1_txt, y2_txt)
leg_vals <- c("y1_txt"="black", "y2_txt"="red")
leg_sort <- c("y1_txt", "y2_txt")
S_AP_PO_lattice_sp <- ggplot(Data_df, aes(Index, S_value, group=factor(Index))) + 
  geom_point(aes(colour=c("y1_txt")), position=position_nudge(x=0, y=0.8), na.rm = TRUE) +
  geom_point(aes(colour=c("y2_txt")), position=position_nudge(x=0, y=-0.8), na.rm = TRUE) +
  geom_text(aes(label=round(S_value, 3), colour="y1_txt"), hjust=1.0, vjust=-0.7, na.rm = TRUE, show.legend=FALSE) + 
  geom_text(aes(label=round(AP_PO_value, 3), colour="y2_txt"), hjust=1.0, vjust=1.3, na.rm = TRUE, show.legend=FALSE) + 
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("time") + 
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  scale_color_manual(name="Legend", labels=leg_labs, values=leg_vals, breaks=leg_sort,
                     guide=guide_legend(override.aes=list(size = c(2.5,2.5), c(16, 16), color=c("black","red")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=0, vjust=1),
        legend.key.width = unit(0.80,"cm"), legend.key.height = unit(1, "cm"), legend.key = element_rect(colour="grey50", linewidth= 0.5),
        legend.position="bottom")
plot(S_AP_PO_lattice_sp)
###############################################################################################################################################
AP_EP <- matrix(NA, nrow=N+1, ncol = N+1)
AP_EP[N+1,] <- AP_PO[N+1,]
for(n in N:1){
  for(k in 0:n){AP_EP[n,k] <- round((1/(1+r))*(q*AP_EP[n+1,k]+p*AP_EP[n+1,k+1]),3)}
}
AP_EP[N+1,] <- rep(0,N+1)
show(AP_EP)
# [,1]  [,2] [,3] [,4] [,5] [,6]
# [1,]  1.330    NA   NA   NA   NA   NA
# [2,]  2.466 0.326   NA   NA   NA   NA
# [3,]  4.494 0.684    0   NA   NA   NA
# [4,]  8.001 1.436    0    0   NA   NA
# [5,] 13.788 3.015    0    0    0   NA
# [6,]  0.000 0.000    0    0    0    0
#
AP_EP_df <- as.data.frame(AP_EP)
# library("data.table")
AP_EP_tb <- setDT(AP_EP_df)   
class(AP_EP_tb)
# [1] "data.table" "data.frame"
# library(reshape2)
AP_EP_rsh_df <- melt(AP_EP_tb, na.rm=FALSE)
show(AP_EP_rsh_df[1:20,])
AP_EP_mod_rsh_df <- subset(AP_EP_rsh_df, select = -variable)
AP_EP_mod_rsh_df <- dplyr::rename(AP_EP_mod_rsh_df, AP_EP_value=value)
AP_EP_mod_rsh_df <- add_column(AP_EP_mod_rsh_df, Index=rep(0:(nrow(AP_EP_df)-1), times=ncol(AP_EP_df)), .before="AP_EP_value")
head(AP_EP_mod_rsh_df,15)
#
AP_PO_mod_rsh_df <- add_column(AP_PO_mod_rsh_df, AP_EP_value=AP_EP_mod_rsh_df$AP_EP_value, .after="AP_PO_value")
show(AP_PO_mod_rsh_df[1:20,])
#
# library(ggplot2)
Data_df <- AP_PO_mod_rsh_df
length <- N
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Corso di Metodi Probabilistici e Statistici per i Mercati Finanziari", 
                             "Example of Lattice Plot for American Put Option - Exercise Values and Prosecution Values in CRR Model"))
subtitle_content <- bquote(paste("market periods N = ",.(N),", risk free rate r = ",.(r),", up factor u = ",.(u),", down factor d = ",.(d),", risk neutral probability distribution (p,q) = (",.(p),",",.(q),"), exercise price K = ",.(K_0),"."))
caption_content <- "Author: Roberto Monte"
y_breaks_num <- 4
y_margin <- 5
y_breaks_low <- floor(min(Data_df$S_value, na.rm =TRUE))-y_margin
y_breaks_up <- ceiling(max(Data_df$S_value, na.rm =TRUE))+y_margin
y_breaks <- seq(from=y_breaks_low, to=y_breaks_up, length.out=y_breaks_num)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0
y_lims <- c((y_breaks_low-K*y_margin), (y_breaks_up+K*y_margin))
y_name <- bquote("stock values, american put option exercise values, and prosecution values")
y1_txt <- bquote("stock values")
y2_txt <- bquote("put exercise values")
y3_txt <- bquote("put prosecution values")
leg_labs <- c(y1_txt, y2_txt, y3_txt)
leg_vals <- c("y1_txt"="black", "y2_txt"="red", "y3_txt"="blue")
leg_sort <- c("y1_txt", "y2_txt", "y3_txt")
S_AP_PO_AP_EP_lattice_sp <- ggplot(Data_df, aes(Index, S_value)) + 
  geom_point(aes(colour=c("y1_txt")), position=position_nudge(x=0, y=0.8), na.rm = TRUE) +
  geom_point(aes(colour=c("y2_txt")), position=position_nudge(x=0, y=-0.8), na.rm = TRUE) +
  geom_point(aes(colour=c("y3_txt")), position=position_nudge(x=0.02, y=-0.8), na.rm = TRUE) +
  geom_text(aes(label=round(S_value,3), colour="y1_txt"), hjust=1.0, vjust=-0.7, na.rm = TRUE, show.legend=FALSE) + 
  geom_text(aes(label=round(AP_PO_value,3), colour="y2_txt"), hjust=1.0, vjust=1.3, na.rm = TRUE, show.legend=FALSE) + 
  geom_text(aes(label=round(AP_EP_value,3), colour="y3_txt"), hjust=-0.2, vjust=1.3, na.rm = TRUE, show.legend=FALSE) + 
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("time") + 
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  scale_color_manual(name="Legend", labels=leg_labs, values=leg_vals, breaks=leg_sort,
                     guide=guide_legend(override.aes=list(size = c(2.5,2.5,2.5), c(16, 16, 16), color=c("black","red","blue")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=0, vjust=1),
        legend.key.width = unit(0.80,"cm"), legend.key.height = unit(1, "cm"), legend.key = element_rect(colour="grey50", linewidth= 0.5),
        legend.position="bottom")
plot(S_AP_PO_AP_EP_lattice_sp)
###############################################################################################################################################
AP_PO_mod_rsh_df <- add_column(AP_PO_mod_rsh_df, AP_MV_value=pmax(AP_PO_mod_rsh_df$AP_PO_value, AP_PO_mod_rsh_df$AP_EP_value, na.rm=TRUE), .after="AP_EP_value")
head(AP_EP_mod_rsh_df,15)
#
Data_df <- AP_PO_mod_rsh_df
length <- N
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Corso di Metodi Probabilistici e Statistici per i Mercati Finanziari", 
                             "Example of Lattice Plot for American Put Option - Exercise Values, Prosecution Values, and Market Values in CRR Model"))
subtitle_content <- bquote(paste("market periods N = ", .(N), ", risk free rate r = ", .(r), ", up factor u = ",.(u), ", down factor d = ",.(d), ", risk neutral probability distribution (p,q) = (",.(p),",",.(q),"), exercise price K = ",.(K_0),"."))
caption_content <- "Author: Roberto Monte"
y_breaks_num <- 4
y_margin <- 5
y_breaks_low <- floor(min(Data_df$S_value, na.rm =TRUE))-y_margin
y_breaks_up <- ceiling(max(Data_df$S_value, na.rm =TRUE))+y_margin
y_breaks <- seq(from=y_breaks_low, to=y_breaks_up, length.out=y_breaks_num)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0
y_lims <- c((y_breaks_low-K*y_margin), (y_breaks_up+K*y_margin))
y_name <- bquote("stock values, american put option exercise values, prosecution values, and market values")
y1_txt <- bquote("stock values")
y2_txt <- bquote("put exercise values")
y3_txt <- bquote("put prosecution values")
y4_txt <- bquote("put market values")
leg_labs <- c(y1_txt, y2_txt, y3_txt, y4_txt)
leg_vals <- c("y1_txt"="black", "y2_txt"="red", "y3_txt"="blue", "y4_txt"="magenta")
leg_sort <- c("y1_txt", "y2_txt", "y3_txt", "y4_txt")
S_AP_PO_AP_EP_AP_MV_lattice_sp <- ggplot(Data_df, aes(Index, S_value)) + 
  geom_point(aes(colour=c("y1_txt")), position=position_nudge(x=0, y=0.8), na.rm = TRUE) +
  geom_point(aes(colour=c("y2_txt")), position=position_nudge(x=0, y=-0.8), na.rm = TRUE) +
  geom_point(aes(colour=c("y3_txt")), position=position_nudge(x=0.02, y=-0.8), na.rm = TRUE) +
  geom_point(aes(colour=c("y4_txt")), position=position_nudge(x=0.02, y=+0.8), na.rm = TRUE) +
  geom_text(aes(label=round(S_value,3), colour="y1_txt"), hjust=1.0, vjust=-0.7, na.rm=TRUE, show.legend=FALSE) + 
  geom_text(aes(label=round(AP_PO_value,3), colour="y2_txt"), hjust=1.0, vjust=1.3, na.rm=TRUE, show.legend=FALSE) + 
  geom_text(aes(label=round(AP_EP_value,3), colour="y3_txt"), hjust=-0.2, vjust=1.3, na.rm=TRUE, show.legend=FALSE) + 
  geom_text(aes(label=round(AP_MV_value,3), colour="y4_txt"), hjust=-0.2, vjust=-0.7, na.rm = TRUE, show.legend=FALSE) + 
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("time") + 
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  scale_color_manual(name="Legend", labels=leg_labs, values=leg_vals, breaks=leg_sort,
                     guide=guide_legend(override.aes=list(size = c(2.5,2.5,2.5,2.5), color=c("black","red","blue","magenta")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=0, vjust=1),
        legend.key.width = unit(0.80,"cm"), legend.key.height = unit(1, "cm"), legend.key = element_rect(colour="grey50", linewidth= 0.5),
        legend.position="bottom")
plot(S_AP_PO_AP_EP_AP_MV_lattice_sp)
# Note that we have
# (1+r)^(-1)*(0.684*p+9.75*q)=4.967 < 5
# (1+r)^(-1)*(1.436*p+14.263*q)=7.476 < 9.75
# (1+r)^(-1)*(3.015*p+18.549*q)=10.269 < 14.263
# (1+r)^(-1)*(0*p+6.332*q)=3.015 > 1.402
# (1+r)^(-1)*(6.332*p+22.622*q)=13.78762 < 18.549
###############################################################################################################################################
# Still assume K=S_0=100 and consider an American call option we have
AC_PO <- matrix(NA, nrow=N+1, ncol = N+1)
AC_PO[1,1] <- 0
for(n in 1:N){
  for(k in 0:N){AC_PO[n+1,k+1] <- round(max(S[n+1,k+1]-100,0),3)}
}
show(AC_PO)
# [,1]   [,2]   [,3]   [,4]   [,5]    [,6]
# [1,]    0     NA     NA     NA     NA      NA
# [2,]    0 15.000     NA     NA     NA      NA
# [3,]    0  9.250 32.250     NA     NA      NA
# [4,]    0  3.787 25.637 52.087     NA      NA
# [5,]    0  0.000 19.356 44.483 74.901      NA
# [6,]    0  0.000 13.388 37.259 66.156 101.136
#
AC_PO_df <- as.data.frame(AC_PO)
# library("data.table")
AC_PO_tb <- setDT(AC_PO_df)   
class(AC_PO_tb)
head(AC_PO_tb)
# library(reshape2)
AC_PO_rsh_df <- melt(AC_PO_tb, na.rm=FALSE)
show(AC_PO_rsh_df[1:20,])
AC_PO_mod_rsh_df <- subset(AC_PO_rsh_df, select = -variable)
AC_PO_mod_rsh_df <- dplyr::rename(AC_PO_mod_rsh_df, AC_PO_value=value)
show(AC_PO_mod_rsh_df[1:20,])
AC_PO_mod_rsh_df <- add_column(AC_PO_mod_rsh_df, Index=rep(0:(nrow(S_df)-1), times=ncol(AC_PO_df)), .before="AC_PO_value")
show(AC_PO_mod_rsh_df[1:20,])
AC_PO_mod_rsh_df <- add_column(AC_PO_mod_rsh_df, S_value=S_mod_rsh_df$S_value, .before="AC_PO_value")
head(AC_PO_mod_rsh_df,15)
#
Data_df <- AC_PO_mod_rsh_df
length <- N
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Corso di Metodi Probabilistici e Statistici per i Mercati Finanziari", 
                             "Example of Lattice Plot for American Call Option - Exercise Values in CRR Model"))
subtitle_content <- bquote(paste("market periods N = ", .(N), ", risk free rate r = ", .(r), ", up factor u = ",.(u), ", down factor d = ",.(d), ", risk neutral probability distribution (p,q) = (",.(p),",",.(q),"), exercise price K = ",.(K_0),"."))
caption_content <- "Author: Roberto Monte"
y_breaks_num <- 4
y_margin <- 5
y_breaks_low <- floor(min(Data_df$S_value, na.rm =TRUE))-y_margin
y_breaks_up <- ceiling(max(Data_df$S_value, na.rm =TRUE))+y_margin
y_breaks <- seq(from=y_breaks_low, to=y_breaks_up, length.out=y_breaks_num)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0
y_lims <- c((y_breaks_low-K*y_margin), (y_breaks_up+K*y_margin))
y_name <- bquote("stock values and american call option exercise value")
y1_txt <- bquote("stock values")
y2_txt <- bquote("american call otion payoffs")
leg_labs <- c(y1_txt, y2_txt)
leg_vals <- c("y1_txt"="black", "y2_txt"="red")
leg_sort <- c("y1_txt", "y2_txt")
S_AC_PO_lattice_sp <- ggplot(Data_df, aes(Index, S_value, group=factor(Index))) + 
  geom_point(aes(colour=c("y1_txt")), position=position_nudge(x=0, y=0.8), na.rm = TRUE) +
  geom_point(aes(colour=c("y2_txt")), position=position_nudge(x=0, y=-0.8), na.rm = TRUE) +
  geom_text(aes(label=round(S_value, 3), colour="y1_txt"), hjust=1.0, vjust=-0.7, na.rm = TRUE, show.legend=FALSE) + 
  geom_text(aes(label=round(AC_PO_value, 3), colour="y2_txt"), hjust=1.0, vjust=1.3, na.rm = TRUE, show.legend=FALSE) + 
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("time") + 
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  scale_color_manual(name="Legend", labels=leg_labs, values=leg_vals, breaks=leg_sort,
                     guide=guide_legend(override.aes=list(size = c(2.5,2.5), c(16, 16), color=c("black","red")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=0, vjust=1),
        legend.key.width = unit(0.80,"cm"), legend.key.height = unit(1, "cm"), legend.key = element_rect(colour="grey50", linewidth= 0.5),
        legend.position="bottom")
plot(S_AC_PO_lattice_sp)
###############################################################################################################################################
AC_EP <- matrix(NA, nrow=N+1, ncol = N+1)
AC_EP[N+1,] <- AC_PO[N+1,]
for(n in N:1){
  for(k in 0:n){AC_EP[n,k] <- round((1/(1+r))*(q*AC_EP[n+1,k]+p*AC_EP[n+1,k+1]),3)}
}
show(AC_EP)
# [,1]   [,2]   [,3]   [,4]   [,5]    [,6]
# [1,] 22.977     NA     NA     NA     NA      NA
# [2,] 15.195 33.056     NA     NA     NA      NA
# [3,]  8.360 23.550 45.867     NA     NA      NA
# [4,]  3.036 14.520 34.935 61.385     NA      NA
# [5,]  0.000  6.375 24.118 49.245 79.663      NA
# [6,]  0.000  0.000 13.388 37.259 66.156 101.136
#
AC_EP_df <- as.data.frame(AC_EP)
# library("data.table")
AC_EP_tb <- setDT(AC_EP_df)   
class(AC_EP_tb)
head(AC_EP_tb)
# library(reshape2)
AC_EP_rsh_df <- melt(AC_EP_tb, na.rm=FALSE)
show(AC_EP_rsh_df[1:20,])
AC_EP_mod_rsh_df <- subset(AC_EP_rsh_df, select = -variable)
AC_EP_mod_rsh_df <- dplyr::rename(AC_EP_mod_rsh_df, AC_EP_value=value)
AC_EP_mod_rsh_df <- add_column(AC_EP_mod_rsh_df, Index=rep(0:(nrow(AC_EP_df)-1), times=ncol(AC_EP_df)), .before="AC_EP_value")
head(AC_EP_mod_rsh_df,15)
# Index AC_EP_value
# 1      0      22.977
# 2      1      15.195
# 3      2       8.360
# 4      3       3.036
# 5      4       0.000
# 6      5       0.000
# 7      0          NA
# 8      1      33.056
# 9      2      23.550
# 10     3      14.520
# 11     4       6.375
# 12     5       0.000
# 13     0          NA
# 14     1          NA
# 15     2      45.867
#
AC_PO_mod_rsh_df <- add_column(AC_PO_mod_rsh_df, AC_EP_value=AC_EP_mod_rsh_df$AC_EP_value, .after="AC_PO_value")
show(AC_PO_mod_rsh_df[1:20,])
# Index S_value AC_PO_value AC_EP_value
# 1      0 100.000     0.000      22.977
# 2      1  95.000     0.000      15.195
# 3      2  90.250     0.000       8.360
# 4      3  85.737     0.000       3.036
# 5      4  81.451     0.000       0.000
# 6      5  77.378     0.000       0.000
# 7      0      NA        NA          NA
# 8      1 115.000    15.000      33.056
# 9      2 109.250     9.250      23.550
# 10     3 103.787     3.787      14.520
# 11     4  98.598     0.000       6.375
# 12     5  93.668     0.000       0.000
# 13     0      NA        NA          NA
# 14     1      NA        NA          NA
# 15     2 132.250    32.250      45.867
# 16     3 125.637    25.637      34.935
# 17     4 119.356    19.356      24.118
# 18     5 113.388    13.388      13.388
# 19     0      NA        NA          NA
# 20     1      NA        NA          NA
#
# library(ggplot2)
Data_df <- AC_PO_mod_rsh_df
length <- N
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Corso di Metodi Probabilistici e Statistici per i Mercati Finanziari", 
                             "Example of Lattice Plot for American Call Option - Exercise Values and Prosecution Values in CRR Model"))
subtitle_content <- bquote(paste("market periods N = ", .(N), ", risk free rate r = ", .(r), ", up factor u = ",.(u), ", down factor d = ",.(d), ", risk neutral probability distribution (p,q) = (",.(p),",",.(q),"), exercise price K = ",.(K_0),"."))
caption_content <- "Author: Roberto Monte"
y_breaks_num <- 4
y_margin <- 5
y_breaks_low <- floor(min(Data_df$S_value, na.rm =TRUE))-y_margin
y_breaks_up <- ceiling(max(Data_df$S_value, na.rm =TRUE))+y_margin
y_breaks <- seq(from=y_breaks_low, to=y_breaks_up, length.out=y_breaks_num)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0
y_lims <- c((y_breaks_low-K*y_margin), (y_breaks_up+K*y_margin))
y_name <- bquote("stock values, american call option exercise values, and prosecution values")
y1_txt <- bquote("stock values")
y2_txt <- bquote("call exercise values")
y3_txt <- bquote("call prosecution values")
leg_labs <- c(y1_txt, y2_txt, y3_txt)
leg_vals <- c("y1_txt"="black", "y2_txt"="red", "y3_txt"="blue")
leg_sort <- c("y1_txt", "y2_txt", "y3_txt")
S_AC_PO_AC_EP_lattice_sp <- ggplot(Data_df, aes(Index, S_value)) + 
  geom_point(aes(colour=c("y1_txt")), position=position_nudge(x=0, y=0.8), na.rm = TRUE) +
  geom_point(aes(colour=c("y2_txt")), position=position_nudge(x=0, y=-0.8), na.rm = TRUE) +
  geom_point(aes(colour=c("y3_txt")), position=position_nudge(x=0.02, y=-0.8), na.rm = TRUE) +
  geom_text(aes(label=round(S_value,3), colour="y1_txt"), hjust=1.0, vjust=-0.7, na.rm = TRUE, show.legend=FALSE) + 
  geom_text(aes(label=round(AC_PO_value,,3), colour="y2_txt"), hjust=1.0, vjust=1.3, na.rm = TRUE, show.legend=FALSE) + 
  geom_text(aes(label=round(AC_EP_value,3), colour="y3_txt"), hjust=-0.2, vjust=1.3, na.rm = TRUE, show.legend=FALSE) + 
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("time") +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  scale_color_manual(name="Legend", labels=leg_labs, values=leg_vals, breaks=leg_sort,
                     guide=guide_legend(override.aes=list(size = c(2.5,2.5,2.5), c(16, 16, 16), color=c("black","red","blue")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=0, vjust=1),
        legend.key.width = unit(0.80,"cm"), legend.key.height = unit(1, "cm"), legend.key = element_rect(colour="grey50", linewidth= 0.5),
        legend.position="bottom")
plot(S_AC_PO_AC_EP_lattice_sp)
#
###############################################################################################################################################
AC_PO_mod_rsh_df <- add_column(AC_PO_mod_rsh_df, AC_MV_value=pmax(AC_PO_mod_rsh_df$AC_PO_value, AC_PO_mod_rsh_df$AC_EP_value, na.rm=TRUE), .after="AC_EP_value")
head(AP_PO_mod_rsh_df,15)
# Index S_value AP_PO_value AP_EP_value AP_MV_value
# 1      0 100.000       0.000       1.330       1.330
# 2      1  95.000       5.000       2.466       5.000
# 3      2  90.250       9.750       4.494       9.750
# 4      3  85.737      14.263       8.001      14.263
# 5      4  81.451      18.549      13.788      18.549
# 6      5  77.378      22.622       0.000      22.622
# 7      0      NA          NA          NA          NA
# 8      1 115.000       0.000       0.326       0.326
# 9      2 109.250       0.000       0.684       0.684
# 10     3 103.787       0.000       1.436       1.436
# 11     4  98.598       1.402       3.015       3.015
# 12     5  93.668       6.332       0.000       6.332
# 13     0      NA          NA          NA          NA
# 14     1      NA          NA          NA          NA
# 15     2 132.250       0.000       0.000       0.000
Data_df <- AC_PO_mod_rsh_df
length <- N
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Corso di Metodi Probabilistici e Statistici per i Mercati Finanziari", 
                             "Example of Lattice Plot for American Call Option - Exercise Values, Prosecution Values, and Market Values in CRR Model"))
subtitle_content <- bquote(paste("market periods N = ", .(N), ", risk free rate r = ", .(r), ", up factor u = ",.(u), ", down factor d = ",.(d), ", risk neutral probability distribution (p,q) = (",.(p),",",.(q),"), exercise price K = ",.(K_0),"."))
caption_content <- "Author: Roberto Monte"
y_breaks_num <- 4
y_margin <- 5
y_breaks_low <- floor(min(Data_df$S_value, na.rm =TRUE))-y_margin
y_breaks_up <- ceiling(max(Data_df$S_value, na.rm =TRUE))+y_margin
y_breaks <- seq(from=y_breaks_low, to=y_breaks_up, length.out=y_breaks_num)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0
y_lims <- c((y_breaks_low-K*y_margin), (y_breaks_up+K*y_margin))
y_name <- bquote("stock values, american call option exercise values, prosecution values, and market values")
y1_txt <- bquote("stock values")
y2_txt <- bquote("call exercise values")
y3_txt <- bquote("call prosecution values")
y4_txt <- bquote("call market values")
leg_labs <- c(y1_txt, y2_txt, y3_txt, y4_txt)
leg_vals <- c("y1_txt"="black", "y2_txt"="red", "y3_txt"="blue", "y4_txt"="magenta")
leg_sort <- c("y1_txt", "y2_txt", "y3_txt", "y4_txt")
S_AC_PO_AC_EP_AC_MV_lattice_sp <- ggplot(Data_df, aes(Index, S_value)) + 
  geom_point(aes(colour=c("y1_txt")), position=position_nudge(x=0, y=0.8), na.rm = TRUE) +
  geom_point(aes(colour=c("y2_txt")), position=position_nudge(x=0, y=-0.8), na.rm = TRUE) +
  geom_point(aes(colour=c("y3_txt")), position=position_nudge(x=0.02, y=-0.8), na.rm = TRUE) +
  geom_point(aes(colour=c("y4_txt")), position=position_nudge(x=0.02, y=+0.8), na.rm = TRUE) +
  geom_text(aes(label=round(S_value,3), colour="y1_txt"), hjust=1.0, vjust=-0.7, na.rm=TRUE, show.legend=FALSE) + 
  geom_text(aes(label=round(AC_PO_value,3), colour="y2_txt"), hjust=1.0, vjust=1.3, na.rm=TRUE, show.legend=FALSE) + 
  geom_text(aes(label=round(AC_EP_value,3), colour="y3_txt"), hjust=-0.2, vjust=1.3, na.rm=TRUE, show.legend=FALSE) + 
  geom_text(aes(label=round(AC_MV_value,3), colour="y4_txt"), hjust=-0.2, vjust=-0.7, na.rm = TRUE, show.legend=FALSE) + 
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("time") + 
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  scale_color_manual(name="Legend", labels=leg_labs, values=leg_vals, breaks=leg_sort,
                     guide=guide_legend(override.aes=list(size = c(2.5,2.5,2.5,2.5), color=c("black","red","blue","magenta")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=0, vjust=1),
        legend.key.width = unit(0.80,"cm"), legend.key.height = unit(1, "cm"), legend.key = element_rect(colour="grey50", linewidth= 0.5),
        legend.position="bottom")
plot(S_AC_PO_AC_EP_AC_MV_lattice_sp)
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################
# European Options on Standard & Poor 500 (Yahoo Finance - ^SPX)
# library(quantmod)
# SPX_Opt_2023_05_12_06_16 <- getOptionChain("^SPX", Exp="2023-06-16", src='yahoo')
## Error in getOptionChain.yahoo(Symbols = "^SPX", Exp = "2023-06-16") : 
## Unable to obtain yahoo crumb. If this is being called from a GDPR country, Yahoo requires GDPR consent, which cannot be scripted
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
head(SPX_Opt_2023_04_11_06_16_df,5)
# Indx     Call_ContractID Call_Bid Call_Ask Call_Vol Call_OI Call_PrChg Call_PrChgPct  Call_LastTrTime Call_LastPr Call_ImplVol Call_ITM Strike
# 1    1 SPXW230616C00200000   3899.9   3906.3        2       0          0             0 04/04/2023 11:20     3889.41     0.000010     TRUE    200
# 2    2 SPXW230616C00400000   3701.8   3708.2        4       0          0             0 30/03/2023 10:03     3642.86     0.000010     TRUE    400
# 3    3  SPX230616C00600000   3306.7   3319.9        1       1          0             0 19/12/2022 16:01     3200.07     0.000010     TRUE    600
# 4    4  SPX230616C00800000   3092.0   3104.5        1      81          0             0 20/12/2022 11:35     3000.50     0.000010     TRUE    800
# 5    5  SPX230616C01000000   3107.8   3114.1       20       0          0             0 11/04/2023 15:52     3105.83     1.312992     TRUE   1000
# Put_ITM Put_ImplVol Put_LastPr   Put_LastTrTime Put_PrChgPct Put_PrChg Put_OI Put_Vol Put_Ask Put_Bid      Put_ContractID
# 1   FALSE   1.9609377       0.05 06/04/2023 10:41            0         0      0       1    0.05       0  SPX230616P00200000
# 2   FALSE   1.5000025       0.03 16/03/2023 12:23            0         0      0     250    0.05       0  SPX230616P00400000
# 3   FALSE   1.2382851       0.05 27/03/2023 12:23            0         0      0      30    0.05       0  SPX230616P00600000
# 4   FALSE   1.0546922       0.05 27/03/2023 12:23            0         0      0     350    0.05       0 SPXW230616P00800000
# 5   FALSE   0.9531255       0.10 05/04/2023 12:40            0         0      0      13    0.10       0 SPXW230616P01000000
tail(SPX_Opt_2023_04_11_06_16_df,5)
#
# Indx    Call_ContractID Call_Bid Call_Ask Call_Vol Call_OI Call_PrChg Call_PrChgPct  Call_LastTrTime Call_LastPr Call_ImplVol Call_ITM Strike
# 299  299 SPX230616C06800000        0     0.00        2       0          0             0 10/02/2023 14:46        0.05    0.2500075    FALSE   6800
# 300  300 SPX230616C07000000        0     0.15     2510    2764          0             0 17/02/2023 10:47        0.05    0.3886780    FALSE   7000
# 301  301 SPX230616C07200000        0     0.05       13       0          0             0 03/04/2023 14:35        0.05    0.3769594    FALSE   7200
# 302  302 SPX230616C07400000        0     0.05      250       0          0             0 16/03/2023 12:23        0.03    0.3935608    FALSE   7400
# 303  303               <NA>       NA       NA       NA      NA         NA            NA             <NA>          NA           NA       NA   7500
# Put_ITM Put_ImplVol Put_LastPr   Put_LastTrTime Put_PrChgPct Put_PrChg Put_OI Put_Vol Put_Ask Put_Bid      Put_ContractID
# 299    TRUE       1e-05    2645.37 04/04/2023 12:52            0         0      0       1  2639.0  2631.7  SPX230616P06800000
# 300    TRUE       1e-05    2846.35 10/04/2023 13:36            0         0      0       1  2837.1  2829.8  SPX230616P07000000
# 301    TRUE       1e-05    3041.22 04/04/2023 12:52            0         0      0       2  3035.3  3027.9  SPX230616P07200000
# 302    TRUE       1e-05    3388.95 24/03/2023 10:31            0         0      0      52  3233.0  3226.6 SPXW230616P07400000
# 303    TRUE       1e-05    3352.92 05/04/2023 14:13            0         0      0       1  3332.5  3325.1  SPX230616P07500000
#
class(SPX_Opt_2023_04_11_06_16_df$Call_LastTrTime)
# [1] "character"
Call_LastTrDate_df <- data.frame(Call_LastTrDate=as.Date(substring(na.rm(SPX_Opt_2023_04_11_06_16_df$Call_LastTrTime), 1, 11), format="%d/%m/%Y"))
class(Call_LastTrDate_df)
# [1] "data.frame"
head(Call_LastTrDate_df, 10)
#    Call_LastTrDate
# 1       2023-04-04
# 2       2023-03-30
# 3       2022-12-19
# 4       2022-12-20
# 5       2023-04-11
# 6       2023-03-13
# 7       2023-03-31
# 8       2022-12-15
# 9       2023-02-27
# 10      2023-03-28
tail(Call_LastTrDate_df, 10)
#     Call_LastTrDate
# 268      2023-03-21
# 269      2023-02-23
# 270      2023-03-20
# 271      2023-02-17
# 272      2023-02-23
# 273      2023-02-10
# 274      2023-02-10
# 275      2023-02-17
# 276      2023-04-03
# 277      2023-03-16
nrow(Call_LastTrDate_df)
# [1] 277
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
head(Put_LastTrDate_df,10)
#    Put_LastTrDate
# 1      2023-04-06
# 2      2023-03-16
# 3      2023-03-27
# 4      2023-03-27
# 5      2023-04-05
# 6      2023-04-03
# 7      2023-04-10
# 8      2023-03-16
# 9      2023-04-04
# 10     2023-04-06
tail(Put_LastTrDate_df,10)
#     Put_LastTrDate
# 281     2023-04-04
# 282     2023-04-03
# 283     2023-04-04
# 284     2023-03-27
# 285     2023-04-10
# 286     2023-04-04
# 287     2023-04-10
# 288     2023-04-04
# 289     2023-03-24
# 290     2023-04-05
nrow(Put_LastTrDate_df)
# [1] 290
Put_LastTrDate_tb <- table(Put_LastTrDate_df)   
class(Put_LastTrDate_tb)
# [1] "table"
show(Put_LastTrDate_tb)
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
# Put-Call parity.
# C_T - P_T = S_T - K
# # Put-Call parity at time t=0.
# C_0 - P_0 = S_0 - K/(1+r_T) (discrete compound risk-free rate)
# C_0 - P_0 = S_0 - K*e(-r_T) (continuous compound risk free rate)
#
K <- SPX_Opt_2023_04_11_06_16_df$Strike[Call_Put_2023_04_11_Indx]
show(K)
# [1] 2000 3050 3240 3775 3780 3785 3800 3825 3850 3875 3890 3900 3915 3925 3950 3975 3980 4005 4020 4025 4035 4065 4075 4095 4100 4105 4115 4120 4130
# [30] 4135 4145 4150 4165 4200 4210 4235 4240 4250 4275 4310 4420
#
C_0 <- SPX_Opt_2023_04_11_06_16_df$Call_LastPr[Call_Put_2023_04_11_Indx]
show(C_0)
# [1] 2115.68  732.90      NA  290.40  278.57  277.99  355.73  364.13  309.90  314.62  275.67  286.30  258.59  266.44  257.08  237.32  212.00
# [18]  209.34  203.70  198.60  180.30  160.10  165.60  149.59  141.90  138.10  134.92  124.00  121.66  117.90  115.00  111.93  104.40   84.45
# [35]   73.10   63.20   67.30   62.30   49.60   39.00   16.15
#
C_0_ITM <- SPX_Opt_2023_04_11_06_16_df$Call_ITM[Call_Put_2023_04_11_Indx]
show(C_0_ITM)
# [1]  TRUE  TRUE    NA  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [24]  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#
P_0 <- SPX_Opt_2023_04_11_06_16_df$Put_LastPr[Call_Put_2023_04_11_Indx]
show(P_0)
# [1]   0.55   4.11   6.53  32.53  33.99  35.20  39.50  40.30  45.00  51.00  48.90  52.60  56.00  60.33  63.60  70.62  71.49  74.10  76.27  80.48
# [21]  84.85  93.64  97.30 100.32 104.51 100.80 113.00 113.00 116.40 116.00 120.00 121.98 123.90 146.30 152.56 164.70 161.70 175.40 200.40 218.40
# [41]  NA
#
P_0_ITM <- SPX_Opt_2023_04_11_06_16_df$Put_ITM[Call_Put_2023_04_11_Indx]
show(P_0_ITM)
# [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [24] FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE    NA
#
Put_Call_Par_df <- data.frame(K, C_0, P_0, C_0_ITM, P_0_ITM)
show(Put_Call_Par_df)
#       K     C_0    P_0 C_0_ITM P_0_ITM
# 1  2000 2115.68   0.55    TRUE   FALSE
# 2  3050  732.90   4.11    TRUE   FALSE
# 3  3240      NA   6.53      NA   FALSE
# 4  3775  290.40  32.53    TRUE   FALSE
# 5  3780  278.57  33.99    TRUE   FALSE
# 6  3785  277.99  35.20    TRUE   FALSE
# 7  3800  355.73  39.50    TRUE   FALSE
# 8  3825  364.13  40.30    TRUE   FALSE
# 9  3850  309.90  45.00    TRUE   FALSE
# 10 3875  314.62  51.00    TRUE   FALSE
# 11 3890  275.67  48.90    TRUE   FALSE
# 12 3900  286.30  52.60    TRUE   FALSE
# 13 3915  258.59  56.00    TRUE   FALSE
# 14 3925  266.44  60.33    TRUE   FALSE
# 15 3950  257.08  63.60    TRUE   FALSE
# 16 3975  237.32  70.62    TRUE   FALSE
# 17 3980  212.00  71.49    TRUE   FALSE
# 18 4005  209.34  74.10    TRUE   FALSE
# 19 4020  203.70  76.27    TRUE   FALSE
# 20 4025  198.60  80.48    TRUE   FALSE
# 21 4035  180.30  84.85    TRUE   FALSE
# 22 4065  160.10  93.64    TRUE   FALSE
# 23 4075  165.60  97.30    TRUE   FALSE
# 24 4095  149.59 100.32    TRUE   FALSE
# 25 4100  141.90 104.51    TRUE   FALSE
# 26 4105  138.10 100.80    TRUE   FALSE
# 27 4115  134.92 113.00   FALSE    TRUE
# 28 4120  124.00 113.00   FALSE    TRUE
# 29 4130  121.66 116.40   FALSE    TRUE
# 30 4135  117.90 116.00   FALSE    TRUE
# 31 4145  115.00 120.00   FALSE    TRUE
# 32 4150  111.93 121.98   FALSE    TRUE
# 33 4165  104.40 123.90   FALSE    TRUE
# 34 4200   84.45 146.30   FALSE    TRUE
# 35 4210   73.10 152.56   FALSE    TRUE
# 36 4235   63.20 164.70   FALSE    TRUE
# 37 4240   67.30 161.70   FALSE    TRUE
# 38 4250   62.30 175.40   FALSE    TRUE
# 39 4275   49.60 200.40   FALSE    TRUE
# 40 4310   39.00 218.40   FALSE    TRUE
# 41 4420   16.15     NA   FALSE      NA
#
Put_Call_Par_df <- na.omit(Put_Call_Par_df)
rownames(Put_Call_Par_df) <- 1:nrow(Put_Call_Par_df)
show(Put_Call_Par_df)
#       K     C_0    P_0 C_0_ITM P_0_ITM
# 1  2000 2115.68   0.55    TRUE   FALSE
# 2  3050  732.90   4.11    TRUE   FALSE
# 3  3775  290.40  32.53    TRUE   FALSE
# 4  3780  278.57  33.99    TRUE   FALSE
# 5  3785  277.99  35.20    TRUE   FALSE
# 6  3800  355.73  39.50    TRUE   FALSE
# 7  3825  364.13  40.30    TRUE   FALSE
# 8  3850  309.90  45.00    TRUE   FALSE
# 9  3875  314.62  51.00    TRUE   FALSE
# 10 3890  275.67  48.90    TRUE   FALSE
# 11 3900  286.30  52.60    TRUE   FALSE
# 12 3915  258.59  56.00    TRUE   FALSE
# 13 3925  266.44  60.33    TRUE   FALSE
# 14 3950  257.08  63.60    TRUE   FALSE
# 15 3975  237.32  70.62    TRUE   FALSE
# 16 3980  212.00  71.49    TRUE   FALSE
# 17 4005  209.34  74.10    TRUE   FALSE
# 18 4020  203.70  76.27    TRUE   FALSE
# 19 4025  198.60  80.48    TRUE   FALSE
# 20 4035  180.30  84.85    TRUE   FALSE
# 21 4065  160.10  93.64    TRUE   FALSE
# 22 4075  165.60  97.30    TRUE   FALSE
# 23 4095  149.59 100.32    TRUE   FALSE
# 24 4100  141.90 104.51    TRUE   FALSE
# 25 4105  138.10 100.80    TRUE   FALSE
# 26 4115  134.92 113.00   FALSE    TRUE
# 27 4120  124.00 113.00   FALSE    TRUE
# 28 4130  121.66 116.40   FALSE    TRUE
# 29 4135  117.90 116.00   FALSE    TRUE
# 30 4145  115.00 120.00   FALSE    TRUE
# 31 4150  111.93 121.98   FALSE    TRUE
# 32 4165  104.40 123.90   FALSE    TRUE
# 33 4200   84.45 146.30   FALSE    TRUE
# 34 4210   73.10 152.56   FALSE    TRUE
# 35 4235   63.20 164.70   FALSE    TRUE
# 36 4240   67.30 161.70   FALSE    TRUE
# 37 4250   62.30 175.40   FALSE    TRUE
# 38 4275   49.60 200.40   FALSE    TRUE
# 39 4310   39.00 218.40   FALSE    TRUE
#
x <- Put_Call_Par_df$K
y <- Put_Call_Par_df$C_0 - Put_Call_Par_df$P_0
Data_df <- data.frame(x,y)
head(Data_df)
#    x       y
# 1 2000 1993.81
# 2 3050 3380.15
# 3 3775 3851.07
# 4 3780 3864.36
# 5 3785 3866.15
# 6 3800 3792.71
#
n <- nrow(Data_df)
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2023-2024", 
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
Put_Call_Par_lm <- lm(y~x)
summary(Put_Call_Par_lm)
# Call: lm(formula = y ~ x)
# Residuals:   Min       1Q     Median     3Q      Max 
#           -269.692    1.934    7.759   17.328  145.753 
#
# Coefficients: Estimate   Std. Error t value Pr(>|t|)    
#  (Intercept) 3818.70000   94.27179   40.51   <2e-16 ***
#  x             -0.92466    0.02369  -39.03   <2e-16 ***
#   ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 56.77 on 37 degrees of freedom
# Multiple R-squared:  0.9763,	Adjusted R-squared:  0.9757 
# F-statistic:  1524 on 1 and 37 DF,  p-value: < 2.2e-16
#
S_0 <- Put_Call_Par_lm$coefficients[1]
show(S_0)
# (Intercept) 
# 3818.7
# SPX Market Price 4,108.94 -0.17 (-0.00%) At close: April 11 04:55PM EDT
#
# -1/(1+r_T_d)=c; -1/c=1+r_T_d; -(1/c+1)=r_T_d,
#
r_T_d <- -(1/Put_Call_Par_lm$coefficients[2]+1)
show(r_T_d)
# x
# 0.08147676 
# bond estimated 0.01144599
#
# - e(-r_T)=c;  -r_T_c=log(-c); r_T_c= log(-1/c)
r_T_c <- log(-(1/Put_Call_Par_lm$coefficients[2]))
show(r_T_c)
# 0.07832747
# bond estimated 0.01144599
#
# We compute the risk-free annual rate of return according to the formulas
# (1+r_A)^T = 1+r_T; r_A = (1+r_T)^(1/T)-1
# where r_A=risk-free annual rate of return, T=time to maturity (in years), r_T=risk-free rate of return in the period T.
# or 
# (1+r_A)^(T/365.2425) = 1+r_T; r_A = (1+r_T)^(365.2425/T)-1
# where r_A=risk-free annual rate of return, T=time to maturity (in days), r_T=risk-free rate of return in the period T.
#
# In case of continuous compound risk-free annual rate of return we have
# r_T = r_A*T; r_A = r_T/T; 
# where r_A=risk-free annual rate of return, T=time to maturity (in years), r_T=risk-free rate of return in the period T.
# or 
# r_A = r_T/(T/365.2425); r_A = r_T*(365.2425/T)
# where r_A=risk-free annual rate of return, T=time to maturity (in days), r_T=risk-free rate of return in the period T.
#
# Note that 1+r_A = (1+r_T)^(365.2425/T) means 1+r_A = exp((365.2425/T)*log(1+r_T)). 
# When r_T << 1, log(1+r_T) ~ r_T. Hence, exp((365.2425/T)*log(1+r_T)) ~ exp((365.2425/T)*r_T). It follows,
# log(1+r_A) ~ r_T*(365.2425/T). In the end, since also r_A << 1, we obtain
# r_A ~ r_T*(365.2425/T).
#
Days_to_Mat <- as.vector(difftime("2023-06-16", "2023-04-11"))
show(Days_to_Mat)
# 66
#
r_A_d=(1+r_T_d)^(365.2425/Days_to_Mat)-1
show(r_A_d)
# 0.5425894
# bond estimated 0.06500779
#
label_percent(accuracy = 0.001)(r_A_d)
# 54.259%
# bond estimated 6.501%
#
r_A_c=r_T_c*(365.2425/Days_to_Mat)
show(r_A_c)
# 0.4334625
label_percent(accuracy = 0.001)(r_A_c)
# 43.346%%
# bond estimated 6.501%
#
# Put-Call parity.
# C_T - P_T = S_T - K
# Put-Call parity at time t=0.
# P_0 - C_0 + S_0 = K/(1+r_T) (discrete compound risk-free rate)
# P_0 - C_0 + S_0 = K*e(-r_T) (continuous compound risk free rate)
# -> Le stime vengono un po male probabilmente per via di come investitori percepiscono il rischio e quindi fanno alzare il tasso privo di rischio, inoltre un po è dovuto che consideriamo le giornate, che però non sono omogenee. Probabilmente va un r_T aggiustato (possibile progetto)
# SPX Market Price 4,108.94 -0.17 (-0.00%) At close: April 11 04:55PM EDT
#
S_0 <- 4108.94
#
x <- Put_Call_Par_df$K
y <- Put_Call_Par_df$P_0 - Put_Call_Par_df$C_0 + S_0
Data_df <- data.frame(x,y)
head(Data_df)
#    x       y
# 1 2000 1993.81
# 2 3050 3380.15
# 3 3775 3851.07
# 4 3780 3864.36
# 5 3785 3866.15
# 6 3800 3792.71
#
n <- nrow(Data_df)
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2023-2024", 
                             paste("Scatter Plot of the Call-Put Difference Plus S_0 Against the Strike Price")))
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
Put_Call_Price_Par_lm <- lm(y~0+x)
summary(Put_Call_Price_Par_lm)
# Call: lm(formula = y ~ 0 + x)
# Residuals:  Min     1Q  Median     3Q    Max 
#           -29.39 -21.07 -14.98  -2.11 338.52 
# 
# Coefficients: Estimate Std. Error t value Pr(>|t|)    
#            x  0.997254  0.002526   394.8   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 62.78 on 38 degrees of freedom
# Multiple R-squared:  0.9998,	Adjusted R-squared:  0.9997 
# F-statistic: 1.558e+05 on 1 and 38 DF,  p-value: < 2.2e-16
#
# 1/(1+r_T)=c; 1+r_T=1/c; r_T=1/c-1
#
r_T <- 1/Put_Call_Price_Par_lm$coefficients[1]-1
show(r_T)
#     x
# 0.002753303
#
Days_to_Mat <- as.vector(difftime("2023-06-16", "2023-04-11"))
show(Days_to_Mat)
# 66
#
r_A=(1+r_T)^(365.2425/Days_to_Mat)-1
show(r_A)
# 0.01533213
# bond estimated 0.06500779
#
show(c(label_percent(accuracy = 0.001)(r_A),label_percent(accuracy = 0.001)(0.06500779)))
# "1.533%" "6.501%" 
#
################################################################################################################################################
################################################################################################################################################
SPX_Opt_2023_05_18_06_16_df <- read.csv("SPX_Option_Chain_2023_05_18_06_16.csv")
class(SPX_Opt_2023_05_18_06_16_df)
# [1] "data.frame"
head(SPX_Opt_2023_05_18_06_16_df,5)
# Indx     Call_ContractID Call_Bid Call_Ask Call_Vol Call_OI Call_PrChg Call_PrChgPct  Call_LastTrTime Call_LastPr Call_ImplVol Call_ITM Strike
# 1    1 SPXW230616C00200000   3998.8   4003.5       16       0          0             0 17/05/2023 13:05     3950.46     4.583989     TRUE    200
# 2    2 SPXW230616C00400000   3799.4   3804.1        3       0          0             0 25/04/2023 11:48     3698.20     3.549073     TRUE    400
# 3    3  SPX230616C00600000   3306.7   3319.9        1       1          0             0 19/12/2022 16:01     3200.07     0.000010     TRUE    600
# 4    4  SPX230616C00800000   3400.1   3406.8        2       0          0             0 09/05/2023 12:16     3313.96     2.612796     TRUE    800
# 5    5  SPX230616C01000000   3200.8   3206.4     2252       0          0             0 18/05/2023 15:59     3197.37     2.275273     TRUE   1000
# Put_ITM Put_ImplVol Put_LastPr   Put_LastTrTime Put_PrChgPct Put_PrChg Put_OI Put_Vol Put_Ask Put_Bid      Put_ContractID
# 1   FALSE    2.984378       0.05 11/05/2023 12:07            0         0      0       2    0.05       0  SPX230616P00200000
# 2   FALSE    2.281254       0.03 17/05/2023 15:54            0         0      0       6    0.05       0  SPX230616P00400000
# 3   FALSE    1.882813       0.05 15/05/2023 09:36            0         0      0       5    0.05       0  SPX230616P00600000
# 4   FALSE    1.609377       0.05 04/05/2023 14:54            0         0      0     120    0.05       0 SPXW230616P00800000
# 5   FALSE    1.390628       0.03 12/05/2023 09:30            0         0      0       1    0.05       0 SPXW230616P01000000
tail(SPX_Opt_2023_05_18_06_16_df,5)
# Indx    Call_ContractID Call_Bid Call_Ask Call_Vol Call_OI Call_PrChg Call_PrChgPct  Call_LastTrTime Call_LastPr Call_ImplVol Call_ITM Strike
# 320  320 SPX230616C06800000        0     0.00        2       0          0             0 10/02/2023 14:46        0.05    0.2500075    FALSE   6800
# 321  321 SPX230616C07000000        0     0.05     1080       0          0             0 10/05/2023 13:09        0.03    0.5214892    FALSE   7000
# 322  322 SPX230616C07200000        0     0.10       13     814          0             0 03/04/2023 14:35        0.05    0.5468795    FALSE   7200
# 323  323 SPX230616C07400000        0     0.10      250    1339          0             0 16/03/2023 12:23        0.03    0.5722699    FALSE   7400
# 324  324               <NA>       NA       NA       NA      NA         NA            NA             <NA>          NA           NA       NA   7500
# Put_ITM Put_ImplVol Put_LastPr   Put_LastTrTime Put_PrChgPct Put_PrChg Put_OI Put_Vol Put_Ask Put_Bid      Put_ContractID
# 320    TRUE       1e-05    2649.03 09/05/2023 10:24            0         0      0       2  2574.9  2568.1  SPX230616P06800000
# 321    TRUE       1e-05    2803.56 18/05/2023 13:34            0         0      0       2  2774.4  2767.7  SPX230616P07000000
# 322    TRUE       1e-05    3042.92 15/05/2023 13:38            0         0      0       1  2973.3  2966.7  SPX230616P07200000
# 323    TRUE       1e-05    3201.80 28/04/2023 12:02            0         0      0       3  3171.8  3167.1 SPXW230616P07400000
# 324    TRUE       1e-05    3312.20 17/05/2023 15:54            0         0      0       6  3272.4  3265.6  SPX230616P07500000
#
class(SPX_Opt_2023_05_18_06_16_df$Call_LastTrTime)
# [1] "character"
Call_LastTrDate_df <- data.frame(Call_LastTrDate=as.Date(substring(na.rm(SPX_Opt_2023_05_18_06_16_df$Call_LastTrTime), 1, 11), format="%d/%m/%Y"))
class(Call_LastTrDate_df)
# [1] "data.frame"
head(Call_LastTrDate_df,10)
#    Call_LastTrDate
# 1       2023-05-17
# 2       2023-04-25
# 3       2022-12-19
# 4       2023-05-09
# 5       2023-05-18
# 6       2023-05-09
# 7       2023-05-17
# 8       2022-12-15
# 9       2023-05-05
# 10      2023-04-25
tail(Call_LastTrDate_df,10)
#     Call_LastTrDate
# 292      2023-04-21
# 293      2023-04-21
# 294      2023-03-20
# 295      2023-02-17
# 296      2023-05-04
# 297      2023-02-10
# 298      2023-02-10
# 299      2023-05-10
# 300      2023-04-03
# 301      2023-03-16
nrow(Call_LastTrDate_df)
# [1] 301
Call_LastTrDate_tb <- table(Call_LastTrDate_df)   
class(Call_LastTrDate_tb)
# [1] "table"
show(Call_LastTrDate_tb)
# 2022-07-13 2022-09-15 2022-10-13 2022-10-14 2022-10-31 2022-11-14 2022-12-15 2022-12-19 2023-01-06 2023-01-30 2023-02-02 2023-02-08 2023-02-10 
# 1          1          1          1          1          1          1          2          1          1          1          1          2 
# 2023-02-13 2023-02-16 2023-02-17 2023-03-05 2023-03-07 2023-03-10 2023-03-13 2023-03-15 2023-03-16 2023-03-20 2023-03-21 2023-03-23 2023-03-24 
# 1          1          1          1          1          2          1          2          3          4          1          3          4 
# 2023-03-28 2023-03-29 2023-03-30 2023-03-31 2023-04-03 2023-04-04 2023-04-05 2023-04-06 2023-04-10 2023-04-11 2023-04-13 2023-04-17 2023-04-18 
# 4          1          3          4          2          3          1          1          1          1          1          3          2 
# 2023-04-19 2023-04-21 2023-04-25 2023-04-26 2023-05-01 2023-05-02 2023-05-03 2023-05-04 2023-05-05 2023-05-08 2023-05-09 2023-05-10 2023-05-11 
# 1          4          4          3          4          3          4          5          8          4          7          1          8 
# 2023-05-12 2023-05-15 2023-05-16 2023-05-17 2023-05-18 2023-05-19 
# 6          2          6         32        121         11 
#
Put_LastTrDate_df <- data.frame(Put_LastTrDate=as.Date(substring(na.rm(SPX_Opt_2023_05_18_06_16_df$Put_LastTrTime), 1, 11), format="%d/%m/%Y"))
class(Put_LastTrDate_df)
# [1] "data.frame"
head(Put_LastTrDate_df,10)
#    Put_LastTrDate
# 1      2023-05-11
# 2      2023-05-17
# 3      2023-05-15
# 4      2023-05-04
# 5      2023-05-12
# 6      2023-05-17
# 7      2023-05-17
# 8      2023-05-17
# 9      2023-05-11
# 10     2023-05-05
tail(Put_LastTrDate_df,10)
#     Put_LastTrDate
# 298     2023-05-11
# 299     2023-05-18
# 300     2023-05-18
# 301     2023-05-18
# 302     2023-05-16
# 303     2023-05-09
# 304     2023-05-18
# 305     2023-05-15
# 306     2023-04-28
# 307     2023-05-17
nrow(Put_LastTrDate_df)
# [1] 307
Put_LastTrDate_tb <- table(Put_LastTrDate_df)   
class(Put_LastTrDate_tb)
# [1] "table"
show(Put_LastTrDate_tb)
# 2021-12-08 2022-04-22 2022-05-26 2022-06-06 2022-06-13 2022-07-21 2022-07-26 2022-11-16 2022-12-19 2023-02-03 2023-02-17 2023-03-06 2023-03-10 
# 1          1          1          1          1          1          1          1          1          1          1          1          1 
# 2023-03-24 2023-03-29 2023-03-30 2023-04-04 2023-04-10 2023-04-17 2023-04-18 2023-04-19 2023-04-20 2023-04-21 2023-04-25 2023-04-26 2023-04-27 
# 1          1          1          1          1          1          2          1          1          1          1          1          1 
# 2023-04-28 2023-05-01 2023-05-02 2023-05-03 2023-05-04 2023-05-05 2023-05-09 2023-05-10 2023-05-11 2023-05-12 2023-05-15 2023-05-16 2023-05-17 
# 2          2          1          2          4          1          4          2          4          4          4          8         28 
# 2023-05-18 2023-05-19 
# 204         10 
#
Call_LastTrDate_2023_05_18_Indx <- SPX_Opt_2023_05_18_06_16_df$Indx[which(Call_LastTrDate_df$Call_LastTrDate=="2023-05-18")]
length(Call_LastTrDate_2023_05_18_Indx)
# [1] 121
show(Call_LastTrDate_2023_05_18_Indx)
# [1]   5  11  15  38  58  65  70  76  80  83  95 100 101 105 115 127 128 135 139 145 147 149 150 152 155 156 157 160 161 165 167 168 169 170 171
# [36] 172 173 174 175 176 177 178 179 180 181 182 183 184 185 186 187 188 189 191 192 193 194 195 196 197 198 199 200 201 202 203 204 206 207 208
# [71] 209 211 212 216 217 218 219 220 221 222 223 224 226 227 229 230 231 232 233 234 235 236 237 239 241 242 243 244 245 246 247 248 249 250 251
# [106] 252 253 255 256 257 258 259 261 262 263 264 265 268 269 270 277
Put_LastTrDate_2023_05_18_Indx <- SPX_Opt_2023_05_18_06_16_df$Indx[which(Put_LastTrDate_df$Put_LastTrDate=="2023-05-18")]
length(Put_LastTrDate_2023_05_18_Indx)
# [1] 204
show(Put_LastTrDate_2023_05_18_Indx)
# [1]  14  17  19  21  22  23  24  25  26  29  31  32  33  34  35  36  37  38  39  42  43  44  46  48  49  50  51  52  53  54  55  56  57  58  59
# [36]  61  63  64  65  66  68  71  72  73  74  75  76  77  78  80  81  82  83  84  87  88  89  90  91  92  93  95  96  97  99 100 101 102 103 104
# [71] 105 106 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140
# [106] 141 142 143 144 145 146 147 148 149 150 152 153 154 155 156 157 158 159 160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175 176
# [141] 178 179 180 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 198 200 201 203 204 205 206 207 208 209 210 211 212 213 214 216
# [176] 218 219 220 221 222 224 225 226 227 229 230 231 232 237 241 244 247 251 254 261 262 264 265 273 285 299 300 301 304
#
Call_Put_2023_05_18_Indx <- intersect(Call_LastTrDate_2023_05_18_Indx, Put_LastTrDate_2023_05_18_Indx)
length(Call_Put_2023_05_18_Indx)
# [1] 87
show(Call_Put_2023_05_18_Indx)
# [1]  38  58  65  76  80  83  95 100 101 105 115 127 128 135 139 145 147 149 150 152 155 156 157 160 161 165 167 168 169 170 171 172 173 174 175
# [36] 176 178 179 180 181 182 183 184 185 186 187 188 189 191 192 193 194 195 196 198 200 201 203 204 206 207 208 209 211 212 216 218 219 220 221
# [71] 222 224 226 227 229 230 231 232 237 241 244 247 251 261 262 264 265
#
# Put-Call parity (discrete compound risk-free rate)
# C_0 - P_0 = S_0 - K/(1+r_T)
#
K <- SPX_Opt_2023_05_18_06_16_df$Strike[Call_Put_2023_05_18_Indx]
show(K)
# [1] 2900 3290 3350 3440 3475 3500 3590 3615 3620 3640 3690 3750 3755 3790 3810 3840 3850 3860 3865 3875 3890 3895 3900 3915 3920 3940 3950 3955
# [29] 3960 3965 3970 3975 3980 3985 3990 3995 4005 4010 4015 4020 4025 4030 4035 4040 4045 4050 4055 4060 4070 4075 4080 4085 4090 4095 4105 4115
# [57] 4120 4130 4135 4145 4150 4155 4160 4170 4175 4195 4205 4210 4215 4220 4225 4235 4245 4250 4260 4265 4270 4275 4300 4320 4335 4350 4370 4420
# [85] 4425 4440 4450
C_0 <- SPX_Opt_2023_05_18_06_16_df$Call_LastPr[Call_Put_2023_05_18_Indx]
show(C_0)
# [1] 1218.64      NA  797.22  668.00  658.91  710.42  544.35  456.89  548.84  549.16  433.59  451.65  387.06  345.65  388.54  315.50  306.90
# [18]  342.30  340.74  310.74  286.94  229.20  314.00  264.42  284.60  241.42  267.50  215.15  268.00  196.90  227.50  242.35  190.70  240.69
# [35]  182.90  190.78  210.15  184.72  158.57  154.09  208.82  181.49  137.40  135.90  134.10  173.50  136.10  163.00  153.14  146.08  157.58
# [52]  129.60  139.21  122.10  135.52  107.70  115.29  117.20  114.40  106.42  102.90   90.25   95.00   88.09   87.00   71.50   68.13   63.90
# [69]   61.40   55.70   53.42   47.85   42.75   43.00   35.80   33.00   33.34   29.80   22.90   16.77   11.87   10.80    6.90    3.10    3.50
# [86]    2.35    2.35
#
C_0_ITM <- SPX_Opt_2023_05_18_06_16_df$Call_ITM[Call_Put_2023_05_18_Indx]
show(C_0_ITM)
# [1]  TRUE    NA  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [24]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [47]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE
# [70] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#
P_0 <- SPX_Opt_2023_05_18_06_16_df$Put_LastPr[Call_Put_2023_05_18_Indx]
show(P_0)
# [1]   0.46   1.45   1.65   2.20   2.55   2.55   3.55   3.90   3.90   4.20   4.83   5.69   6.20   7.70   7.50   8.30   8.90   9.40  10.80   9.70
# [21]  10.50  10.26   9.83  12.20  12.50  13.90  13.75  16.90  17.30  14.95  15.66  15.68  17.77  17.85  18.20  18.85  19.05  19.77  19.22  20.82
# [41]  20.38  21.50  23.80  23.10  23.70  22.20  24.65  25.19  26.04  27.27  26.21  31.60  29.42  30.60  31.59  34.48  35.53  36.67  37.60  39.90
# [61]  41.10  49.49  41.61  52.60  49.91  56.80  58.40  60.20  62.50  70.53  67.25  78.90  75.90  78.70  82.60  86.80  89.20 113.39 114.50 184.11
# [81]     NA 160.05 227.00 287.87 239.63     NA 305.63
#
P_0_ITM <- SPX_Opt_2023_05_18_06_16_df$Put_ITM[Call_Put_2023_05_18_Indx]
show(P_0_ITM)
# [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [24] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [47] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE
# [70]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE    NA  TRUE  TRUE  TRUE  TRUE    NA  TRUE
#
Put_Call_Par_df <- data.frame(K, C_0, P_0, C_0_ITM, P_0_ITM)
show(Put_Call_Par_df)
#       K     C_0    P_0 C_0_ITM P_0_ITM
# 1  2900 1218.64   0.46    TRUE   FALSE
# 2  3290      NA   1.45      NA   FALSE
# 3  3350  797.22   1.65    TRUE   FALSE
# 4  3440  668.00   2.20    TRUE   FALSE
# 5  3475  658.91   2.55    TRUE   FALSE
# 6  3500  710.42   2.55    TRUE   FALSE
# 7  3590  544.35   3.55    TRUE   FALSE
# 8  3615  456.89   3.90    TRUE   FALSE
# 9  3620  548.84   3.90    TRUE   FALSE
# 10 3640  549.16   4.20    TRUE   FALSE
# 11 3690  433.59   4.83    TRUE   FALSE
# 12 3750  451.65   5.69    TRUE   FALSE
# 13 3755  387.06   6.20    TRUE   FALSE
# 14 3790  345.65   7.70    TRUE   FALSE
# 15 3810  388.54   7.50    TRUE   FALSE
# 16 3840  315.50   8.30    TRUE   FALSE
# 17 3850  306.90   8.90    TRUE   FALSE
# 18 3860  342.30   9.40    TRUE   FALSE
# 19 3865  340.74  10.80    TRUE   FALSE
# 20 3875  310.74   9.70    TRUE   FALSE
# 21 3890  286.94  10.50    TRUE   FALSE
# 22 3895  229.20  10.26    TRUE   FALSE
# 23 3900  314.00   9.83    TRUE   FALSE
# 24 3915  264.42  12.20    TRUE   FALSE
# 25 3920  284.60  12.50    TRUE   FALSE
# 26 3940  241.42  13.90    TRUE   FALSE
# 27 3950  267.50  13.75    TRUE   FALSE
# 28 3955  215.15  16.90    TRUE   FALSE
# 29 3960  268.00  17.30    TRUE   FALSE
# 30 3965  196.90  14.95    TRUE   FALSE
# 31 3970  227.50  15.66    TRUE   FALSE
# 32 3975  242.35  15.68    TRUE   FALSE
# 33 3980  190.70  17.77    TRUE   FALSE
# 34 3985  240.69  17.85    TRUE   FALSE
# 35 3990  182.90  18.20    TRUE   FALSE
# 36 3995  190.78  18.85    TRUE   FALSE
# 37 4005  210.15  19.05    TRUE   FALSE
# 38 4010  184.72  19.77    TRUE   FALSE
# 39 4015  158.57  19.22    TRUE   FALSE
# 40 4020  154.09  20.82    TRUE   FALSE
# 41 4025  208.82  20.38    TRUE   FALSE
# 42 4030  181.49  21.50    TRUE   FALSE
# 43 4035  137.40  23.80    TRUE   FALSE
# 44 4040  135.90  23.10    TRUE   FALSE
# 45 4045  134.10  23.70    TRUE   FALSE
# 46 4050  173.50  22.20    TRUE   FALSE
# 47 4055  136.10  24.65    TRUE   FALSE
# 48 4060  163.00  25.19    TRUE   FALSE
# 49 4070  153.14  26.04    TRUE   FALSE
# 50 4075  146.08  27.27    TRUE   FALSE
# 51 4080  157.58  26.21    TRUE   FALSE
# 52 4085  129.60  31.60    TRUE   FALSE
# 53 4090  139.21  29.42    TRUE   FALSE
# 54 4095  122.10  30.60    TRUE   FALSE
# 55 4105  135.52  31.59    TRUE   FALSE
# 56 4115  107.70  34.48    TRUE   FALSE
# 57 4120  115.29  35.53    TRUE   FALSE
# 58 4130  117.20  36.67    TRUE   FALSE
# 59 4135  114.40  37.60    TRUE   FALSE
# 60 4145  106.42  39.90    TRUE   FALSE
# 61 4150  102.90  41.10    TRUE   FALSE
# 62 4155   90.25  49.49    TRUE   FALSE
# 63 4160   95.00  41.61    TRUE   FALSE
# 64 4170   88.09  52.60    TRUE   FALSE
# 65 4175   87.00  49.91    TRUE   FALSE
# 66 4195   71.50  56.80    TRUE   FALSE
# 67 4205   68.13  58.40   FALSE    TRUE
# 68 4210   63.90  60.20   FALSE    TRUE
# 69 4215   61.40  62.50   FALSE    TRUE
# 70 4220   55.70  70.53   FALSE    TRUE
# 71 4225   53.42  67.25   FALSE    TRUE
# 72 4235   47.85  78.90   FALSE    TRUE
# 73 4245   42.75  75.90   FALSE    TRUE
# 74 4250   43.00  78.70   FALSE    TRUE
# 75 4260   35.80  82.60   FALSE    TRUE
# 76 4265   33.00  86.80   FALSE    TRUE
# 77 4270   33.34  89.20   FALSE    TRUE
# 78 4275   29.80 113.39   FALSE    TRUE
# 79 4300   22.90 114.50   FALSE    TRUE
# 80 4320   16.77 184.11   FALSE    TRUE
# 81 4335   11.87     NA   FALSE      NA
# 82 4350   10.80 160.05   FALSE    TRUE
# 83 4370    6.90 227.00   FALSE    TRUE
# 84 4420    3.10 287.87   FALSE    TRUE
# 85 4425    3.50 239.63   FALSE    TRUE
# 86 4440    2.35     NA   FALSE      NA
# 87 4450    2.35 305.63   FALSE    TRUE
Put_Call_Par_df <- na.omit(Put_Call_Par_df)
rownames(Put_Call_Par_df) <- 1:nrow(Put_Call_Par_df)
show(Put_Call_Par_df)
#       K     C_0    P_0 C_0_ITM P_0_ITM
# 1  2900 1218.64   0.46    TRUE   FALSE
# 2  3350  797.22   1.65    TRUE   FALSE
# 3  3440  668.00   2.20    TRUE   FALSE
# 4  3475  658.91   2.55    TRUE   FALSE
# 5  3500  710.42   2.55    TRUE   FALSE
# 6  3590  544.35   3.55    TRUE   FALSE
# 7  3615  456.89   3.90    TRUE   FALSE
# 8  3620  548.84   3.90    TRUE   FALSE
# 9  3640  549.16   4.20    TRUE   FALSE
# 10 3690  433.59   4.83    TRUE   FALSE
# 11 3750  451.65   5.69    TRUE   FALSE
# 12 3755  387.06   6.20    TRUE   FALSE
# 13 3790  345.65   7.70    TRUE   FALSE
# 14 3810  388.54   7.50    TRUE   FALSE
# 15 3840  315.50   8.30    TRUE   FALSE
# 16 3850  306.90   8.90    TRUE   FALSE
# 17 3860  342.30   9.40    TRUE   FALSE
# 18 3865  340.74  10.80    TRUE   FALSE
# 19 3875  310.74   9.70    TRUE   FALSE
# 20 3890  286.94  10.50    TRUE   FALSE
# 21 3895  229.20  10.26    TRUE   FALSE
# 22 3900  314.00   9.83    TRUE   FALSE
# 23 3915  264.42  12.20    TRUE   FALSE
# 24 3920  284.60  12.50    TRUE   FALSE
# 25 3940  241.42  13.90    TRUE   FALSE
# 26 3950  267.50  13.75    TRUE   FALSE
# 27 3955  215.15  16.90    TRUE   FALSE
# 28 3960  268.00  17.30    TRUE   FALSE
# 29 3965  196.90  14.95    TRUE   FALSE
# 30 3970  227.50  15.66    TRUE   FALSE
# 31 3975  242.35  15.68    TRUE   FALSE
# 32 3980  190.70  17.77    TRUE   FALSE
# 33 3985  240.69  17.85    TRUE   FALSE
# 34 3990  182.90  18.20    TRUE   FALSE
# 35 3995  190.78  18.85    TRUE   FALSE
# 36 4005  210.15  19.05    TRUE   FALSE
# 37 4010  184.72  19.77    TRUE   FALSE
# 38 4015  158.57  19.22    TRUE   FALSE
# 39 4020  154.09  20.82    TRUE   FALSE
# 40 4025  208.82  20.38    TRUE   FALSE
# 41 4030  181.49  21.50    TRUE   FALSE
# 42 4035  137.40  23.80    TRUE   FALSE
# 43 4040  135.90  23.10    TRUE   FALSE
# 44 4045  134.10  23.70    TRUE   FALSE
# 45 4050  173.50  22.20    TRUE   FALSE
# 46 4055  136.10  24.65    TRUE   FALSE
# 47 4060  163.00  25.19    TRUE   FALSE
# 48 4070  153.14  26.04    TRUE   FALSE
# 49 4075  146.08  27.27    TRUE   FALSE
# 50 4080  157.58  26.21    TRUE   FALSE
# 51 4085  129.60  31.60    TRUE   FALSE
# 52 4090  139.21  29.42    TRUE   FALSE
# 53 4095  122.10  30.60    TRUE   FALSE
# 54 4105  135.52  31.59    TRUE   FALSE
# 55 4115  107.70  34.48    TRUE   FALSE
# 56 4120  115.29  35.53    TRUE   FALSE
# 57 4130  117.20  36.67    TRUE   FALSE
# 58 4135  114.40  37.60    TRUE   FALSE
# 59 4145  106.42  39.90    TRUE   FALSE
# 60 4150  102.90  41.10    TRUE   FALSE
# 61 4155   90.25  49.49    TRUE   FALSE
# 62 4160   95.00  41.61    TRUE   FALSE
# 63 4170   88.09  52.60    TRUE   FALSE
# 64 4175   87.00  49.91    TRUE   FALSE
# 65 4195   71.50  56.80    TRUE   FALSE
# 66 4205   68.13  58.40   FALSE    TRUE
# 67 4210   63.90  60.20   FALSE    TRUE
# 68 4215   61.40  62.50   FALSE    TRUE
# 69 4220   55.70  70.53   FALSE    TRUE
# 70 4225   53.42  67.25   FALSE    TRUE
# 71 4235   47.85  78.90   FALSE    TRUE
# 72 4245   42.75  75.90   FALSE    TRUE
# 73 4250   43.00  78.70   FALSE    TRUE
# 74 4260   35.80  82.60   FALSE    TRUE
# 75 4265   33.00  86.80   FALSE    TRUE
# 76 4270   33.34  89.20   FALSE    TRUE
# 77 4275   29.80 113.39   FALSE    TRUE
# 78 4300   22.90 114.50   FALSE    TRUE
# 79 4320   16.77 184.11   FALSE    TRUE
# 80 4350   10.80 160.05   FALSE    TRUE
# 81 4370    6.90 227.00   FALSE    TRUE
# 82 4420    3.10 287.87   FALSE    TRUE
# 83 4425    3.50 239.63   FALSE    TRUE
# 84 4450    2.35 305.63   FALSE    TRUE
#
# Put-Call parity.
# C_T - P_T = S_T - K
# Put-Call parity at time t=0.
# P_0 - C_0 + S_0 = K/(1+r_T) (discrete compound risk-free rate)
# P_0 - C_0 + S_0 = K*e(-r_T) (continuous compound risk free rate)
#
x <- Put_Call_Par_df$K
y <- Put_Call_Par_df$C_0 - Put_Call_Par_df$P_0
Data_df <- data.frame(x,y)
head(Data_df)
#    x       y
# 1 2900 1218.18
# 2 3350  795.57
# 3 3440  665.80
# 4 3475  656.36
# 5 3500  707.87
# 6 3590  540.80
#
n <- nrow(Data_df)
show(n)
# 84
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2023-2024", 
                             paste("Scatter Plot of the Call-Put Difference Against the Strike Price")))
subtitle_content <- bquote(paste("Data set size",~~.(n),~~"sample points;    Evaluation Date 2023-05-18;   Maturity Date 2023-06-16"))
caption_content <- "Author: Roberto Monte" 
# To obtain the submultiples of the length of the data set as a hint on the number of breaks to use
# library(numbers)
# primeFactors(n)
x_breaks_num <- 23
x_breaks_low <- min(Data_df$x)
x_breaks_up <- max(Data_df$x)
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- format(x_breaks, scientific=FALSE)
J <- 0
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
K <- 1
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
col_1 <- bquote("data set sample points")
col_2 <- bquote("regression line")
col_3 <- bquote("LOESS curve")
leg_labs <- c(col_1, col_2, col_3)
leg_cols <- c("col_1"="blue", "col_2"="green", "col_3"="red")
leg_ord <- c("col_1", "col_2", "col_3")
Call_Put_Strike_Pr_2023_05_18_06_16_sp <- ggplot(Data_df, aes(x=x, y=y)) +
  geom_smooth(alpha=1, linewidth=0.8, linetype="dashed", aes(color="col_3"),
              method="loess", formula=y ~ x, se=FALSE) +
  geom_smooth(alpha=1, linewidth=0.8, linetype="solid", aes(color="col_2"),
              method="lm" , formula=y ~ x, se=FALSE, fullrange=TRUE) +
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
plot(Call_Put_Strike_Pr_2023_05_18_06_16_sp)
#
Put_Call_Par_lm <- lm(y~x)
summary(Put_Call_Par_lm)
# Call: lm(formula = y ~ x)
# Residuals:   Min     1Q Median     3Q    Max 
#           -86.55 -21.96   9.09  20.38  60.85 
# 
# Coefficients: Estimate Std. Error t value Pr(>|t|)    
#  (Intercept) 3917.92992   47.11536   83.16   <2e-16 ***
#  x             -0.93455    0.01174  -79.63   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 27.64 on 82 degrees of freedom
# Multiple R-squared:  0.9872,	Adjusted R-squared:  0.9871 
# F-statistic:  6342 on 1 and 82 DF,  p-value: < 2.2e-16
#
S_0 <- Put_Call_Par_lm$coefficients[1]
show(S_0)
# (Intercept) 
# 3917.93
# SPX Market Price 4,198.05 At close: May 18 :55PM EDT
#
r_T <- -(1/Put_Call_Par_lm$coefficients[2]+1)
show(r_T)
# x 
# 0.07003721
# bond estimated 0.00211723
#
# We compute the risk-free annual rate of return according to the formulas
# (1+r_A)^T = 1+r_T; r_A = (1+r_T)^(1/T)-1
# where r_A=risk-free annual rate of return, T=time to maturity (in years), r_T=risk-free rate of return in the period T.
# or 
# (1+r_A)^(T/365.2425) = 1+r_T; r_A = (1+r_T)^(365.2425/T)-1
# where r_A=risk-free annual rate of return, T=time to maturity (in days), r_T=risk-free rate of return in the period T.
#
Days_to_Mat <- as.vector(difftime("2023-06-16", "2023-05-18"))
show(Days_to_Mat)
# 29
#
r_A=(1+r_T)^(365.2425/Days_to_Mat)-1
show(r_A)
# 1.345666 
#
label_percent(accuracy = 0.001)(r_A)
# 134.567%
#
# Put-Call parity.
# C_T - P_T = S_T - K
# Put-Call parity at time t=0.
# P_0 - C_0 + S_0 = K/(1+r_T) (discrete compound risk-free rate).
# P_0 - C_0 + S_0 = K*e(-r_T) (continuous compound risk free rate).
# SPX Market Price 4,198.05 At close: May 18 :55PM EDT.
#
S_0 <- 4198.05
#
x <- Put_Call_Par_df$K
y <- Put_Call_Par_df$P_0 - Put_Call_Par_df$C_0 + S_0
Data_df <- data.frame(x,y)
head(Data_df)
#    x       y
# 1 2900 2979.87
# 2 3350 3402.48
# 3 3440 3532.25
# 4 3475 3541.69
# 5 3500 3490.18
# 6 3590 3657.25
#
n <- nrow(Data_df)
show(n)
# [1] 84
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2023-2024", 
                             paste("Scatter Plot of the Call-Put Difference Adjusted by the Stock Price Against the Strike Price")))
subtitle_content <- bquote(paste("Data set size",~~.(n),~~"sample points;    Evaluation Date 2023-05-18;   Maturity Date 2023-06-16"))
caption_content <- "Author: Roberto Monte" 
# To obtain the sub-multiples of the length of the data set as a hint on the number of breaks to use
# library(numbers)
# primeFactors(n)
x_breaks_num <- 28
x_breaks_low <- Data_df$x[1]
x_breaks_up <- Data_df$x[n]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- format(x_breaks, scientific=FALSE)
J <- 0
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
K <- 1
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
col_1 <- bquote("data set sample points")
col_2 <- bquote("regression line")
col_3 <- bquote("LOESS curve")
leg_labs <- c(col_1, col_2, col_3)
leg_cols <- c("col_1"="blue", "col_2"="green", "col_3"="red")
leg_ord <- c("col_1", "col_2", "col_3")
Call_Put_Price_Strike_Pr_2023_05_18_06_16_sp <- ggplot(Data_df, aes(x=x, y=y)) +
  geom_smooth(alpha=1, linewidth=0.8, linetype="dashed", aes(color="col_3"),
              method="loess", formula=y ~ x, se=FALSE) +
  geom_smooth(alpha=1, linewidth=0.8, linetype="solid", aes(color="col_2"),
              method="lm" , formula=y ~ x, se=FALSE, fullrange=TRUE) +
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
plot(Call_Put_Price_Strike_Pr_2023_05_18_06_16_sp)
#
Put_Call_Price_Par_lm <- lm(y~0+x)
summary(Put_Call_Price_Par_lm)
# Call: lm(formula = y ~ 0 + x)
# Residuals:  Min     1Q Median     3Q    Max 
#          -34.24 -26.83 -10.70  27.51 114.96 
# 
# Coefficients: Estimate Std. Error t value Pr(>|t|)    
#           x 1.0041760  0.0008932    1124   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 32.87 on 83 degrees of freedom
# Multiple R-squared:  0.9999,	Adjusted R-squared:  0.9999 
# F-statistic: 1.264e+06 on 1 and 83 DF,  p-value: < 2.2e-16
#
r_T_d <- 1/Put_Call_Price_Par_lm$coefficients[1]-1
show(r_T_d)
# -0.004158682
#
r_T_c <- -log(Put_Call_Price_Par_lm$coefficients[1])
show(r_T_c)
# -0.004167353
#
Days_to_Mat <- as.vector(difftime("2023-06-16", "2023-05-18"))
show(Days_to_Mat)
# 29
#
r_A=(1+r_T)^(365.2425/Days_to_Mat)-1
show(r_A)
# -0.05113241 
#
label_percent(accuracy = 0.001)(r_A)
# -5.113%
#
################################################################################################################################################
################################################################################################################################################
################################################################################################################################################

