#### Impact of COVID19 on hospital bed capacity 

library(ggplot2)
library(reshape2)
library(patchwork)
library(gridGraphics)
library(tidyverse)

#### FUNCTIONS
source("functions_hosp_covid.R")

####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************

## DATA CALCULATIONS
## Admission rate for "normal" patients
# 180 days (6mo)
# BASED ON ICHNT
# Length of stay 7.9 days
# 157 ADULT beds
# 82.2% full 
occ_bed_days <- c(0.7,0.822,0.9) * 157 * 180  # number of occupied bed days
unique_pat <- occ_bed_days / 7.9 
inc_rate <- unique_pat / 180 # number arrive every day
inc_rate # mean and range - make norm around mean + 1 SD: 16.3

pop_ichnt <- 560600
ocbeds_peak_wuhan <- 2.6/10000*pop_ichnt# from Lipsitch - but this is number in beds 
ocbeds_peak_wuhan / 10



####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************

#### NUMBERS
nbeds <- 157 # ICHNT Critical care beds
#nbeds <- 4000 # UK ICU

los_norm <- c(7.9,4) # ICHNT ICU NORMAL LOS 

los_cov <- readRDS("delay_dist_linelist.rds") #sample(seq(1,length(x),1), size = n, prob = x, replace=T) #c(10,8) # GUESS 

cols <- c("3" = "lightblue", "1" = "red", "0" = "darkgreen")

nruns = 2

ndays = 90 # run for 3 months - so much uncertainty ahead

####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
### WUHAN LIKE
## peak at 15 patients per day
## peak remains flat
cov_curve <- c(sigmoid(c(15,0.3,30),seq(1,90,1)))
#plot(cov_curve)

M_wuh <- multiple_runs(nruns, nbeds = 157, los_norm, los_cov, cov_curve, ndays)
plot_multiple(M_wuh,"wuh_ichnt")

M_wuhd <- multiple_runs(nruns, nbeds = 18, los_norm, los_cov, cov_curve, ndays, inc_rate = 1)
plot_multiple(M_wuhd,"wuh_dev")

M_wuhc <- multiple_runs(nruns, nbeds = 64, los_norm, los_cov, cov_curve, ndays,inc_rate = 7)
plot_multiple(M_wuhc,"wuh_cuh")

# EG
output_wuh <- bed_filling(nbeds, los_norm, los_cov, cov_curve,ndays=90)
plot_eg(output_wuh, "wuh_ichnt")

output_wuh_devon <- bed_filling(18, los_norm, los_cov, cov_curve,ndays=90, inc_rate = 1)
plot_eg(output_wuh, "wuh_devon")

output_wuh_cuh <- bed_filling(64, los_norm, los_cov, cov_curve,ndays=90, inc_rate = 7)
plot_eg(output_wuh, "wuh_cuh")


####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
### DOUBLE PEAK HEIGHT OF WUHAN
## peak at 15 patients per day
## peak remains flat 
cov_curve <- c(sigmoid(c(30,0.3,30),seq(1,90,1)))
#plot(cov_curve)
#lines(sigmoid(c(15,0.3,30),seq(1,90,1)))

M_dbwuh <- multiple_runs(nruns, 157, los_norm, los_cov, cov_curve, ndays)
plot_multiple(M_dbwuh,"dbwuh_ichnt")

M_dbdev <- multiple_runs(nruns, 18, los_norm, los_cov, cov_curve, ndays,inc_rate = 1)
plot_multiple(M_dbdev,"dbwuh_devon")

M_dbcuh <- multiple_runs(nruns, 64, los_norm, los_cov, cov_curve, ndays,inc_rate = 7)
plot_multiple(M_dbcuh,"dbwuh_cuh")

# EG
output_dbwuh <- bed_filling(157, los_norm, los_cov, cov_curve,ndays=90)
plot_eg(output_dbwuh, "dbwuh")

output_wuh_devon <- bed_filling(18, los_norm, los_cov, cov_curve,ndays=90, inc_rate = 1)
plot_eg(output_wuh, "wuh_devon")

output_wuh_cuh <- bed_filling(64, los_norm, los_cov, cov_curve,ndays=90, inc_rate = 7)
plot_eg(output_wuh, "wuh_cuh")


####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
### HALVE RATE OF WUHAN
## peak at 15 patients per day
## peak remains flat 
## Rate to increase halved
cov_curve <- c(sigmoid(c(15,0.15,45),seq(1,90,1)))

pdf("plots/scenarios_covid.pdf")
plot(cov_curve,ylim=c(0,35),type="l",col="blue",xlab = "Days",ylab = "Daily incidence of COVID19 critical care cases")
lines(sigmoid(c(15,0.3,30),seq(1,90,1)),col="black")
lines(sigmoid(c(30,0.3,30),seq(1,90,1)),col="red")
abline(v=c(30,60,90),col="grey")
dev.off()

M_slwuh <- multiple_runs(nruns, nbeds, los_norm, los_cov, cov_curve, ndays)
plot_multiple(M_slwuh,"slwuh_ichnt")

M_dbdev <- multiple_runs(nruns, 18, los_norm, los_cov, cov_curve, ndays,inc_rate = 1)
plot_multiple(M_dbdev,"slwuh_devon")

M_dbcuh <- multiple_runs(nruns, 64, los_norm, los_cov, cov_curve, ndays,inc_rate = 7)
plot_multiple(M_dbcuh,"slwuh_cuh")

# EG
output_slwuh <- bed_filling(nbeds, los_norm, los_cov, cov_curve,ndays=90)
plot_eg(output_slwuh, "slwuh")

output_wuh_devon <- bed_filling(18, los_norm, los_cov, cov_curve,ndays=90, inc_rate = 1)
plot_eg(output_wuh, "wuh_devon")

output_wuh_cuh <- bed_filling(64, los_norm, los_cov, cov_curve,ndays=90, inc_rate = 7)
plot_eg(output_wuh, "wuh_cuh")
