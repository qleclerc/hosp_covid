#### Impact of COVID19 on hospital bed capacity 

library(ggplot2)
library(reshape2)
library(patchwork)
library(gridGraphics)
### Fills beds for 6 months. Daily time step
# nbeds = number of beds in ward
# los = length of stay (mean, sd) for normal / covid patients
# cov_curve = changing NUMBER of patients that are covid+
# inc_rate = ICNHT data

bed_filling <- function(nbeds, los_norm, los_cov, cov_curve, inc_rate = 15.7){
  
  ## Normal patients arriving over 6mo
  A <- as.data.frame(matrix(0,180,4))
  colnames(A) <- c("day","norm_admin","cov_admin","prop_cov")
  A[,"day"] <- seq(1,180,1)
  A[,"norm_admin"] <- ceiling(rnorm(180,inc_rate,1)) #  depends on normal size / LOS of hospital
  A[,"cov_admin"] <- cov_curve
  A[,"prop_cov"] <- A$cov_admin/(A$norm_admin + A$cov_admin)
  A0 <- A
  
  WC <- array(0,c(nbeds,2,180)) # Array - 3D matrix. 
  colnames(WC)<-c("patno","status")
  WN <- array(0,c(nbeds,2,180)) # Array - 3D matrix. 
  colnames(WN)<-c("patno","status")
  # rows = bed, columns = c(patient number, actual status, presumed status, days in hospital), 3D = time
  
  # track number of patients 
  pat_num <- 0
  
  ###############**********************************************************************
  ### ICU BEDS ###
  ###############**********************************************************************
  
  ICU_fill <- as.data.frame(matrix(0,180,3)) # Want to know how many enter the ICU each day
  colnames(ICU_fill) <- c("day","norm","cov")
  ICU_fill$day <- seq(1,180,1)
  
  # fill by bed
  for(i in 1:nbeds){
    
    cumlos <- 0
    
    # first patient in bed
    pat_num <- pat_num + 1
    pat_status <- 0 # normal patient in initially - assume v low prevalence of COVID
    los <- ceiling(rnorm(1,los_norm) * runif(1)) # been in for some time already perhaps
    WC[i,c("patno","status"),1:los] <- c(pat_num,pat_status)
    
    cumlos <- cumlos + los
    
    ICU_fill[1,(pat_status+2)] <- ICU_fill[1,(pat_status+2)] + 1
    
    while(cumlos < 180){
      pat_num <- pat_num + 1 # Next patient
      pat_status <- ifelse(runif(1) < A$prop_cov[cumlos+1],1,0) # 1 = COVID positive or not (0)?
      
      ### COMPLICATED BY NEED TO HAVE PATIENTS TO ADMIT
      if(sum(A[cumlos,c("norm_admin","cov_admin")]) > 0){ # if there is a patient to be admitted
        if(A[cumlos,(pat_status+2)] > 0){ # if there is a patient of this type to be admitted
          #print(cumlos)
          ICU_fill[cumlos,(pat_status+2)] <- ICU_fill[cumlos,(pat_status+2)] + 1 # count of filled beds
          A[cumlos, (pat_status+2)] <- A[cumlos, (pat_status+2)] - 1 # remove from admin
          A$prop_cov <- A$cov_admin / (A$norm_admin + A$cov_admin)
          
          los <- ceiling(ifelse(pat_status == 1, rnorm(1,los_cov), rnorm(1,los_norm))) # length of stay
          
        }else{
          pat_status <- ifelse(pat_status == 1, 0, 1) # switch status
          # don't need to check if patient of this type as know sum (A) > 0
          
          ICU_fill[cumlos,(pat_status+2)] <- ICU_fill[cumlos,(pat_status+2)] + 1 # count of filled beds
          A[cumlos, (pat_status+2)] <- A[cumlos, (pat_status+2)] - 1 # remove from admin
          A$prop_cov <- A$cov_admin / (A$norm_admin + A$cov_admin)
          
          los <- ceiling(ifelse(pat_status == 1, rnorm(1,los_cov), rnorm(1,los_norm))) # length of stay
          
        }
      }else{
        los <- 1
        pat_status <- 3 # EMPTY BED
      }
      
      WC[i,c("patno","status"),pmin((cumlos + (1:los)),180)] <- c(pat_num,pat_status) # this patient stays until end of los
      
      cumlos <- cumlos + los # next new patient at this time point
    }
    
  }
  
  return(list(A0 = A0, A = A, WC = WC, pat_num = pat_num, ICU_fill = ICU_fill))
  
}

####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************

## Admission rate for "normal" patients
# 180 days (6mo)
# BASED ON ICHNT
# Length of stay 8 days
# 157 ADULT beds
# 80% full 
occ_bed_days <- c(0.7,0.8,0.9) * 157 * 180  # number of occupied bed days
unique_pat <- occ_bed_days / 8 
inc_rate <- unique_pat / 180 # number arrive every day
inc_rate # mean and range - make norm around mean + 1 SD



#### Run
nbeds <- 157 # ICHNT Critical care beds
#nbeds <- 4000 # UK ICU
los_norm <- c(7.9,4) # ICHNT ICU NORMAL LOS 

los_cov <- c(20,8) # GUESS 

# NO COVID
cov_curve <- 1000*seq(0,0,length.out = 180)
output_nocovid <- bed_filling(nbeds, los_norm, los_cov, cov_curve)

h_noco <- melt(output_nocovid$WC,id.vars = "patno")
colnames(h_noco) <- c("bedno","variable","time","value")
h_noco <- dcast(h_noco, time + bedno ~variable)
ggplot(h_noco, aes(x = time, y = bedno) ) + 
  geom_point(aes(col = factor(status))) +
  scale_colour_discrete(name  ="Status",breaks=c("0", "1"),labels=c("Normal", "Covid")) 

pcov <- as.data.frame(cbind(seq(1,180,1),cov_curve));
colnames(pcov)<-c("days","cprev")

# Rapid to 20% then decrease
cov_curve <- 1000*c(seq(0,0.2,length.out = 100),seq(0.2,0,length.out = 80)) # GUESS
output_to20 <- bed_filling(nbeds, los_norm, los_cov, cov_curve)

h_to20 <- melt(output_to20$WC,id.vars = "patno")
colnames(h_to20) <- c("bedno","variable","time","value")
h_to20 <- dcast(h_to20, time + bedno ~variable)
p1 <- ggplot(h_to20, aes(x = time, y = bedno) ) + 
  geom_point(aes(col = factor(status))) + 
  scale_colour_discrete(name  ="Status",breaks=c("0", "1"),labels=c("Normal", "Covid")) +
  xlab("Day") + ylab("Bed number")


pcov$cprev <- cov_curve
p2 <- ggplot(pcov, aes(x=days, y = cprev)) + geom_line() + 
  scale_x_continuous("Day") + scale_y_continuous("COV19 prevelance at entry") 

p1/p2
ggsave(paste0("plots/",nbeds,"_to20.pdf"))

# Rapid to 80% then decrease
cov_curve <- 1000*c(seq(0,0.8,length.out = 100),seq(0.8,0,length.out = 80)) # GUESS
output_to80 <- bed_filling(nbeds, los_norm, los_cov, cov_curve)

h_to80 <- melt(output_to80$WC,id.vars = "patno")
colnames(h_to80) <- c("bedno","variable","time","value")
h_to80 <- dcast(h_to80, time + bedno ~variable)
p1 <- ggplot(h_to80, aes(x = time, y = bedno) ) + 
  geom_point(aes(col = factor(status))) + 
  scale_colour_discrete(name  ="Status",breaks=c("0", "1"),labels=c("Normal", "Covid")) + 
  xlab("Day") + ylab("Bed number")

pcov$cprev <- cov_curve
p2 <- ggplot(pcov, aes(x=days, y = cprev)) + geom_line() + 
  scale_x_continuous("Day") + scale_y_continuous("COV19 prevelance at entry")

p1/p2
ggsave(paste0("plots/",nbeds,"_to80.pdf"))
