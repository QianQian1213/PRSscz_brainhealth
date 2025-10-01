allmatrix <- read.table("final_sczprs_cov_tbss_23468",header=TRUE,sep=" ")
sample_id <- read.table("id",sep = " ",header=TRUE) #不用质控
IMD <- read.table("f.26410.0.0.csv",sep = " ",header=TRUE) 
IMD[,1][which(IMD[,1] < 0)] <- NA
IMD1 <- cbind(sample_id,IMD)
IMD1 <- na.omit(IMD1)

allmatrix1 <- merge(x = allmatrix, y = IMD1,  by = c('f.eid'), all=F)
nrow(allmatrix1)#21270

allmatrix1$prsincome <- allmatrix1$f.26275.0.0 * allmatrix1$f.26410.0.0
ncol(allmatrix1)#398

allmatrix2 <- allmatrix1[,c(1,398,397,2:396)]

for (m in c(2:9,12:14)){
allmatrix2[,m] <- scale(allmatrix2[,m])
}
allmatrix2[,10] <- as.factor(allmatrix2[,10])
allmatrix2[,11] <- as.factor(allmatrix2[,11])


library(FRGEpistasis)
for (n in 15:ncol(allmatrix2)){
allmatrix2[,n] <- rankTransPheno(allmatrix2[,n],3/8)
}


result <- c()
for (j in 15:ncol(allmatrix2)){
fit <- lm(allmatrix2[,j] ~ prsincome + f.26275.0.0 + f.26410.0.0 + f.26201.0.0 + f.26201.0.1 + f.26201.0.2 + f.26201.0.3 + age + sex + center + age2 + agesex + age2sex, data=allmatrix2)
result <- rbind(result,c(colnames(allmatrix2)[j],coef(summary(fit))[2,c(1,2,3,4)]))
} 
write.csv(result,file="sczprs_tbss_glm_prsincome_result.csv")
