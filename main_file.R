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

nruns = 100

ndays = 90 # run for 3 months - so much uncertainty ahead

####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************

######### NO COVID
# cov_curve <- 10*seq(0,0,length.out = 180)
# output_nocovid <- bed_filling(nbeds, los_norm, los_cov, cov_curve)
# 
# M_noco <- multiple_runs(nruns, nbeds, los_norm, los_cov, cov_curve)
# 
# miss_noco <- M_noco$miss
# miss_noco_month <- M_noco$miss_month
# 
# p11 <- ggplot(miss_noco, aes(x=day, y = m_norm)) + 
#   geom_ribbon(aes(ymin = m_norm - sd_norm, ymax = m_norm + sd_norm), fill = "grey70") +
#   geom_line(aes(y = m_norm)) + 
#   annotate(size = 2,'text',(15 + 30*c(0,1,2,3,4,5)), -1, 
#            label=paste("Av. miss. norm:",round(miss_noco_month$m_norm,0))) + 
#   annotate(size = 2,'text',(15 + 30*c(0,1,2,3,4,5)), -2, 
#            label=paste("Av. miss. covid:",round(miss_noco_month$m_cov,0))) 
# 
# ggsave(paste0("plots/miss_",nbeds,"_noco.pdf"))
# 
# 
# ## e.g. 
# h_noco <- melt(output_nocovid$WC,id.vars = "patno")
# colnames(h_noco) <- c("bedno","variable","time","value")
# h_noco <- dcast(h_noco, time + bedno ~variable)
# h_noco <- h_noco[-which(h_noco$time == 181),]
# ggplot(h_noco, aes(x = time, y = bedno) ) + 
#   geom_point(aes(col = factor(status))) +
#   scale_colour_discrete(name  ="Status",breaks=c("0", "1"),labels=c("Normal", "Covid")) 
# 
# pcov <- as.data.frame(cbind(seq(1,180,1),cov_curve));
# colnames(pcov)<-c("days","cprev")

####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
### WUHAN LIKE
## peak at 15 patients per day
## peak flat over a month
sigmoid = function(params, x) {
  params[1] / (1 + exp(-params[2] * (x - params[3])))
}

cov_curve <- 10 * dnorm(seq(0,1.5,length.out = 40), mean = 1, sd = 0.2)
x <- seq(0,90,1)
plot(x,sigmoid(c(15,0.3,30),x), ylim = c(0,35))
cov_curve <- c(sigmoid(c(15,0.3,30),seq(1,90,1)))

plot(cov_curve)

M_wuh <- multiple_runs(nruns, nbeds, los_norm, los_cov, cov_curve, ndays)

miss_to20 <- M_to20$miss
miss_to20_month <- M_to20$miss_month

p11 <- ggplot(miss_to20, aes(x=day, y = m_norm)) + 
  geom_ribbon(aes(ymin = m_norm - sd_norm, ymax = m_norm + sd_norm), fill = "grey70") +
  geom_line(aes(y = m_norm)) + 
  annotate(size = 2,'text',(15 + 30*c(0,1,2,3,4,5)), -1, 
           label=paste("Av. miss. norm:",round(miss_to20_month$m_norm,0))) + 
  annotate(size = 2,'text',(15 + 30*c(0,1,2,3,4,5)), -2, 
           label=paste("Av. miss. covid:",round(miss_to20_month$m_cov,0))) +
  annotate(size = 2,'text',30, 8, 
           label=paste("Extra beds needed:",round(mean(M_to20$max_bed_need),0),
                       "SD (",round(sd(M_to20$max_bed_need),0),")"))

ggsave(paste0("plots/miss_",nbeds,"_to20.pdf"))

# EG
output_to20 <- bed_filling(nbeds, los_norm, los_cov, cov_curve)
plot_eg(output_to20, "to20")

####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
# Rapid to 20% then decrease
cov_curve <- 15*dnorm(seq(0,5,length.out = 180), mean = 2.5, sd = 2)

M_to20 <- multiple_runs(nruns, nbeds, los_norm, los_cov, cov_curve)

miss_to20 <- M_to20$miss
miss_to20_month <- M_to20$miss_month

p11 <- ggplot(miss_to20, aes(x=day, y = m_norm)) + 
  geom_ribbon(aes(ymin = m_norm - sd_norm, ymax = m_norm + sd_norm), fill = "grey70") +
  geom_line(aes(y = m_norm)) + 
  annotate(size = 2,'text',(15 + 30*c(0,1,2,3,4,5)), -1, 
           label=paste("Av. miss. norm:",round(miss_to20_month$m_norm,0))) + 
  annotate(size = 2,'text',(15 + 30*c(0,1,2,3,4,5)), -2, 
           label=paste("Av. miss. covid:",round(miss_to20_month$m_cov,0))) +
  annotate(size = 2,'text',30, 8, 
           label=paste("Extra beds needed:",round(mean(M_to20$max_bed_need),0),
                       "SD (",round(sd(M_to20$max_bed_need),0),")"))

ggsave(paste0("plots/miss_",nbeds,"_to20.pdf"))

# EG
output_to20 <- bed_filling(nbeds, los_norm, los_cov, cov_curve)
plot_eg(output_to20, "to20")

####****************************************************************************************************************
# Rapid to 80% then decrease
cov_curve <- 15*dnorm(seq(0,5,length.out = 180), mean = 2.5, sd = 0.5)

M_to80 <- multiple_runs(nruns, nbeds, los_norm, los_cov, cov_curve)

miss_to80 <- M_to80$miss
miss_to80_month <- M_to80$miss_month

p11 <- ggplot(miss_to80, aes(x=day, y = m_norm)) + 
  geom_ribbon(aes(ymin = m_norm - sd_norm, ymax = m_norm + sd_norm), fill = "grey70") +
  geom_line(aes(y = m_norm)) + 
  annotate(size = 2,'text',(15 + 30*c(0,1,2,3,4,5)), -1, 
           label=paste("Av. miss. norm:",round(miss_to80_month$m_norm,0))) + 
  annotate(size = 2,'text',(15 + 30*c(0,1,2,3,4,5)), -2, 
           label=paste("Av. miss. covid:",round(miss_to80_month$m_cov,0))) +
  annotate(size = 2,'text',30, 8, 
           label=paste("Extra beds needed:",round(mean(M_to80$max_bed_need),0),
                       "SD (",round(sd(M_to80$max_bed_need),0),")"))

ggsave(paste0("plots/miss_",nbeds,"_to80.pdf"))


# EG
output_to80 <- bed_filling(nbeds, los_norm, los_cov, cov_curve)
plot_eg(output_to80, "to80")

#####*****************************************************
# Flattenend
cov_curve <- 15*dnorm(seq(0,5,length.out = 180), mean = 5, sd = 4)
output_tofl <- bed_filling(nbeds, los_norm, los_cov, cov_curve)

# EG
output_tofl <- bed_filling(nbeds, los_norm, los_cov, cov_curve)
plot_eg(output_tofl, "tofl")


#### Number per day
npd_noco <- h_noco %>% group_by(time,status) %>% summarise(n = n())
npd_to20 <- h_to20 %>% group_by(time,status) %>% summarise(n = n())
npd_to80 <- h_to80 %>% group_by(time,status) %>% summarise(n = n())
p1 <- ggplot(npd_noco, aes(x=time, y = n)) + 
  geom_bar(group = "status",stat = "identity",
           position = "dodge", aes(fill = factor(status))) + 
  scale_fill_manual(name  ="Status",values = cols,
                    breaks=c("0", "3","1"),labels=c("Normal", "Empty","Covid")) + 
  scale_y_continuous("Number of ICU beds") + scale_x_continuous("Day")

p2 <- ggplot(npd_to20, aes(x=time, y = n)) + 
  geom_bar(group = "status",stat = "identity",
           position = "dodge", aes(fill = factor(status))) + 
  scale_fill_manual(name  ="Status",values = cols,
                    breaks=c("0", "3","1"),labels=c("Normal", "Empty","Covid")) + 
  scale_y_continuous("Number of ICU beds") + scale_x_continuous("Day")

p3 <- ggplot(npd_to80, aes(x=time, y = n)) + 
  geom_bar(group = "status",stat = "identity",
           position = "dodge", aes(fill = factor(status))) + 
  scale_fill_manual(name  ="Status",values = cols,
                    breaks=c("0", "3","1"),labels=c("Normal", "Empty","Covid")) + 
  scale_y_continuous("Number of ICU beds") + scale_x_continuous("Day")

p1/p2/p3
ggsave(paste0("plots/",nbeds,"_number_beds.pdf"))



