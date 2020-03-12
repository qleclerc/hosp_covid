##### CHECK FOR NUMBER OF SIMULATIONS
#M_to20 <- multiple_runs(nruns, nbeds, los_norm, los_cov, cov_curve)
beds <- M_to20$max_bed_need

brk <- c(1,20,50,75,100)
m_bed <- c()
sd_bed <- c()

for(i in 1:length(brk)){
  m_bed <- c(m_bed,mean(beds[1:brk[i]]))
  sd_bed <- c(sd_bed,sd(beds[1:brk[i]]))
}

dd <- as.data.frame(cbind(brk,m_bed,sd_bed))
colnames(dd) <- c("sims","mean","sd")

ggplot(dd, aes(x=sims,y=mean)) + geom_ribbon(aes(ymin = mean-sd, ymax = mean+sd), fill = "grey70") +
  geom_line(aes(y = mean)) + scale_y_continuous(lim = c(30,50))

## 100 runs is fine!
