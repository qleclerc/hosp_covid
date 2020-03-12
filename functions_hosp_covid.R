## FUNCTIONS
### Fills beds for 6 months. Daily time step
# nbeds = number of beds in ward
# los = length of stay (mean, sd) for normal / covid patients
# cov_curve = changing NUMBER of patients that are covid+
# inc_rate = ICNHT data

bed_filling <- function(nbeds, los_norm, los_cov, cov_curve, inc_rate = 16.3){
  
  ## Normal patients arriving over 6mo
  A <- as.data.frame(matrix(0,180,4))
  colnames(A) <- c("day","norm_admin","cov_admin","prop_cov")
  A[,"day"] <- seq(1,180,1)
  A[,"norm_admin"] <- ceiling(rnorm(180,inc_rate,1)) #  depends on normal size / LOS of hospital
  A[,"cov_admin"] <- ceiling(cov_curve)
  A[,"prop_cov"] <- A$cov_admin/(A$norm_admin + A$cov_admin)
  A0 <- A
  
  WC <- array(0,c(nbeds,2,181)) # Array - 3D matrix. 
  colnames(WC)<-c("patno","status")
  WN <- array(0,c(nbeds,2,181)) # Array - 3D matrix. 
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
    
    cumlos <- 0 # DAY ZERO
    
    # first patient in bed
    pat_num <- pat_num + 1
    if(runif(1) < 0.2){
      pat_status <- 3 # Empty bed
      los <- 1 # check next day for patient
    }else{
      pat_status <- 0 # normal patient in initially - assume v low prevalence of COVID
      los <- ceiling(rnorm(1,los_norm) * runif(1)) # been in for some time already perhaps
    }
    
    WC[i,c("patno","status"),1:los] <- c(pat_num,pat_status)
    
    cumlos <- cumlos + los
    
    ICU_fill[1,(pat_status+2)] <- ICU_fill[1,(pat_status+2)] + 1
    
    while(cumlos < 181){
      #print(cumlos)
      pat_num <- pat_num + 1 # Next patient
      pat_status <- ifelse(runif(1) < A$prop_cov[cumlos],1,0) # 1 = COVID positive or not (0)?
      
      ### COMPLICATED BY NEED TO HAVE PATIENTS TO ADMIT
      if(sum(A[cumlos,c("norm_admin","cov_admin")]) > 0){ # if there is a patient to be admitted
        #print(c(cumlos,pat_status))
        if(A[cumlos,(pat_status+2)] > 0){ # if there is a patient of this type to be admitted
          
          ICU_fill[cumlos,(pat_status+2)] <- ICU_fill[cumlos,(pat_status+2)] + 1 # count of filled beds
          A[cumlos, (pat_status+2)] <- A[cumlos, (pat_status+2)] - 1 # remove from admin
          A[cumlos,"prop_cov"] <- ifelse((A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"])>0,A[cumlos,"cov_admin"] / (A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"]),0)
          
          los <- ceiling(ifelse(pat_status == 1, 
                                sample(delays, 1, replace = TRUE), #rnorm(1,los_cov), 
                                rnorm(1,los_norm))) # length of stay
          
        }else{
          pat_status <- ifelse(pat_status == 1, 0, 1) # switch status
          # don't need to check if patient of this type as know sum (A) > 0
          
          ICU_fill[cumlos,(pat_status+2)] <- ICU_fill[cumlos,(pat_status+2)] + 1 # count of filled beds
          A[cumlos, (pat_status+2)] <- A[cumlos, (pat_status+2)] - 1 # remove from admin
          A[cumlos,"prop_cov"] <- ifelse((A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"])>0,
                                         A[cumlos,"cov_admin"] / (A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"]),0)
          
          
          los <- ceiling(ifelse(pat_status == 1, 
                                sample(delays, 10, replace = TRUE),#rnorm(1,los_cov), 
                                rnorm(1,los_norm))) # length of stay
          
        }
      }else{
        los <- 1
        pat_status <- 3 # EMPTY BED
        pat_num <- pat_num - 1 # No patient
      }
      
      WC[i,c("patno","status"),pmin((cumlos + (1:los)),180)] <- c(pat_num,pat_status) # this patient stays until end of los
      
      cumlos <- cumlos + los # next new patient at this time point
    }
    
  }
  
  return(list(A0 = A0, A = A, WC = WC, pat_num = pat_num, ICU_fill = ICU_fill))
  
}

### Multiple runs

multiple_runs <- function(nruns, nbeds, los_norm, los_cov, cov_curve){
  
  h_store<-c()
  missing_store <- c() #matrix(0,100*180,5)
  
  for(j in 1:nruns){
    output <- bed_filling(nbeds, los_norm, los_cov, cov_curve)
    
    h <- melt(output$WC,id.vars = "patno")
    colnames(h) <- c("bedno","variable","time","value")
    h <- dcast(h, time + bedno ~variable)
    
    h_store <- rbind(h_store, cbind(j,h))
    missing_store <- rbind(missing_store, cbind(j,output$A))
  }
  
  ## Multiple runs
  h_store <- as.data.frame(h_to80_store)
  missing_store <- as.data.frame(missing_store)
  miss <- missing_store %>% group_by(day) %>% 
    summarise(m_norm = mean(norm_admin), m_cov = mean(cov_admin), 
              sd_norm = sd(norm_admin), sd_cov = sd(cov_admin)) 
  
  # mean number missed per day
  missing_store$month <- rep(1:6,each = 30)
  miss_month <- missing_store %>% group_by(month) %>% 
    summarise(m_norm = mean(norm_admin), m_cov = mean(cov_admin), 
              sd_norm = sd(norm_admin), sd_cov = sd(cov_admin)) 
  
  return(list(missing_store = missing_store, miss_month = miss_month,
              h_store = h_store, miss = miss))
  
}