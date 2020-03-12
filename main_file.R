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

#### Run
nbeds <- 157 # ICHNT Critical care beds
#nbeds <- 4000 # UK ICU
los_norm <- c(7.9,4) # ICHNT ICU NORMAL LOS 

los_cov <- readRDS("delay_dist_linelist.rds") #sample(seq(1,length(x),1), size = n, prob = x, replace=T) #c(10,8) # GUESS 

cols <- c("3" = "lightblue", "1" = "red", "0" = "darkgreen")

nruns = 1000

######### NO COVID
cov_curve <- 10*seq(0,0,length.out = 180)
output_nocovid <- bed_filling(nbeds, los_norm, los_cov, cov_curve)

M_noco <- multiple_runs(nruns, nbeds, los_norm, los_cov, cov_curve)

miss_noco <- M_noco$miss
miss_noco_month <- M_noco$miss_month

p11 <- ggplot(miss_noco, aes(x=day, y = m_norm)) + 
  geom_ribbon(aes(ymin = m_norm - sd_norm, ymax = m_norm + sd_norm), fill = "grey70") +
  geom_line(aes(y = m_norm)) + 
  annotate(size = 2,'text',(15 + 30*c(0,1,2,3,4,5)), -1, 
           label=paste("Av. miss. norm:",round(miss_noco_month$m_norm,0))) + 
  annotate(size = 2,'text',(15 + 30*c(0,1,2,3,4,5)), -2, 
           label=paste("Av. miss. covid:",round(miss_noco_month$m_cov,0))) 

ggsave(paste0("plots/miss_",nbeds,"_noco.pdf"))

ggplot(h_noco, aes(x = time, y = bedno) ) + 
  geom_point(aes(col = factor(status))) +
  scale_colour_discrete(name  ="Status",breaks=c("0", "1"),labels=c("Normal", "Covid")) 

pcov <- as.data.frame(cbind(seq(1,180,1),cov_curve));
colnames(pcov)<-c("days","cprev")




####****************************************************************************************************************
# Rapid to 20% then decrease
cov_curve <- 15*dnorm(seq(0,5,length.out = 180), mean = 2.5, sd = 2)
#cov_curve <- 10*c(seq(0,0.2,length.out = 100),seq(0.2,0,length.out = 80)) # GUESS
output_to20 <- bed_filling(nbeds, los_norm, los_cov, cov_curve)
M_to20 <- multiple_runs(nruns, nbeds, los_norm, los_cov, cov_curve)

miss_to20 <- M_to20$miss
miss_to20_month <- M_to20$miss_month

p11 <- ggplot(miss_to20, aes(x=day, y = m_norm)) + 
  geom_ribbon(aes(ymin = m_norm - sd_norm, ymax = m_norm + sd_norm), fill = "grey70") +
  geom_line(aes(y = m_norm)) + 
  annotate(size = 2,'text',(15 + 30*c(0,1,2,3,4,5)), -1, 
           label=paste("Av. miss. norm:",round(miss_to20_month$m_norm,0))) + 
  annotate(size = 2,'text',(15 + 30*c(0,1,2,3,4,5)), -2, 
           label=paste("Av. miss. covid:",round(miss_to20_month$m_cov,0))) 

ggsave(paste0("plots/miss_",nbeds,"_to20.pdf"))

## e.g.

h_to20 <- melt(output_to20$WC,id.vars = "patno")
colnames(h_to20) <- c("bedno","variable","time","value")
h_to20 <- dcast(h_to20, time + bedno ~variable)
p1 <- ggplot(h_to20, aes(x = time, y = bedno) ) + 
  geom_point(aes(col = factor(status))) + 
  scale_colour_manual(name  ="Status",values = cols,breaks=c("0", "3","1"),labels=c("Normal", "Empty","Covid")) + 
  xlab("Day") + ylab("Bed number")


pcov$cprev <- cov_curve
p2 <- ggplot(pcov, aes(x=days, y = cprev)) + geom_line() + 
  scale_x_continuous("Day") + scale_y_continuous(lim = c(0,10),"COV19 prevelance at entry") 

# Missing people
miss_to20 <- melt(output_to20$A[,c("day","norm_admin","cov_admin")], id.vars = "day")
perc_not_treat <- round(100*output_to20$pat_num/(output_nocovid$pat_num),0)

p3 <- ggplot(miss_to20, aes(fill=variable, y=value, x=day)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(name  ="Status",values=c("norm_admin"="darkgreen","cov_admin" = "red"),labels=c("Normal", "Covid")) +
  scale_y_continuous("Extra bed space needed") + 
  scale_x_continuous("Day") + 
  #annotate(size = 2,'text',90, 20, 
  # label=paste("Percentage of normal ICU burden treated:",perc_not_treat,"%"))+ 
  annotate(size = 2,'text',90, 19, 
           label=paste("Total missed norm:",sum(output_to20$A[,c("norm_admin")]))) +
  annotate(size = 2,'text',90, 15, 
           label=paste("Total missed covid:",sum(output_to20$A[,c("cov_admin")])))


p1/p2+p3
ggsave(paste0("plots/",nbeds,"_to20.pdf"))

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
           label=paste("Av. miss. covid:",round(miss_to80_month$m_cov,0))) 

ggsave(paste0("plots/miss_",nbeds,"_to80.pdf"))


#npd_to80 <- h_to80 %>% group_by(time,status) %>% summarise(n = n())
### e.g.
output_to80 <- bed_filling(nbeds, los_norm, los_cov, cov_curve)
h_to80 <- melt(output_to80$WC,id.vars = "patno")
colnames(h_to80) <- c("bedno","variable","time","value")
h_to80 <- dcast(h_to80, time + bedno ~variable)
p1 <- ggplot(h_to80, aes(x = time, y = bedno) ) + 
  geom_point(aes(col = factor(status))) + 
  scale_colour_manual(name  ="Status",values = cols,breaks=c("0", "3","1"),labels=c("Normal", "Empty","Covid")) + 
  xlab("Day") + ylab("Bed number")

pcov$cprev <- cov_curve
p2 <- ggplot(pcov, aes(x=days, y = cprev)) + geom_line() + 
  scale_x_continuous("Day") + scale_y_continuous(lim = c(0,10),"COV19 prevelance at entry")

# Missing people
miss_to80 <- melt(output_to80$A[,c("day","norm_admin","cov_admin")], id.vars = "day")
perc_not_treat <- round(100*output_to80$pat_num/(output_nocovid$pat_num),0)

p3 <- ggplot(miss_to80, aes(fill=variable, y=value, x=day)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(name  ="Status",values=c("norm_admin"="darkgreen","cov_admin" = "red"),labels=c("Normal", "Covid")) +
  scale_y_continuous("Extra bed space needed") + 
  scale_x_continuous("Day") + 
  #annotate(size = 2,'text',90, 20, 
  #         label=paste("Percentage of normal ICU burden treated:",perc_not_treat,"%"))+ 
  annotate(size = 2,'text',90, 19, 
           label=paste("Total missed norm:",sum(output_to80$A[,c("norm_admin")]))) +
  annotate(size = 2,'text',90, 15, 
           label=paste("Total missed covid:",sum(output_to80$A[,c("cov_admin")])))


p1/p2+p3
ggsave(paste0("plots/",nbeds,"_to80.pdf"))


# Flattenend
cov_curve <- 15*dnorm(seq(0,5,length.out = 180), mean = 5, sd = 4)
output_tofl <- bed_filling(nbeds, los_norm, los_cov, cov_curve)

h_tofl <- melt(output_tofl$WC,id.vars = "patno")
colnames(h_tofl) <- c("bedno","variable","time","value")
h_tofl <- dcast(h_tofl, time + bedno ~variable)
p1 <- ggplot(h_tofl, aes(x = time, y = bedno) ) + 
  geom_point(aes(col = factor(status))) + 
  scale_colour_manual(name  ="Status",values = cols,breaks=c("0", "3","1"),labels=c("Normal", "Empty","Covid")) + 
  xlab("Day") + ylab("Bed number")

pcov$cprev <- cov_curve
p2 <- ggplot(pcov, aes(x=days, y = cprev)) + geom_line() + 
  scale_x_continuous("Day") + scale_y_continuous(lim = c(0,10),"COV19 prevelance at entry")

# Missing people
miss_tofl <- melt(output_tofl$A[,c("day","norm_admin","cov_admin")], id.vars = "day")
perc_not_treat <- round(100*output_tofl$pat_num/(output_nocovid$pat_num),0)

p3 <- ggplot(miss_tofl, aes(fill=variable, y=value, x=day)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(name  ="Status",values=c("norm_admin"="darkgreen","cov_admin" = "red"),labels=c("Normal", "Covid")) +
  scale_y_continuous("Extra bed space needed") + 
  scale_x_continuous("Day") + 
  # annotate(size = 2,'text',90, 20, 
  #          label=paste("Percentage of normal ICU burden treated:",perc_not_treat,"%"))+ 
  annotate(size = 2,'text',90, 19, 
           label=paste("Total missed norm:",sum(output_tofl$A[,c("norm_admin")]))) +
  annotate(size = 2,'text',90, 15, 
           label=paste("Total missed covid:",sum(output_tofl$A[,c("cov_admin")])))


p1/p2+p3
ggsave(paste0("plots/",nbeds,"_tofl.pdf"))

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

