allmatrix <- read.table("final_sczprs_cov_tbss_23468",header=TRUE,sep=" ")
allmatrix$prssex <- allmatrix$f.26275.0.0 * allmatrix$sex
ncol(allmatrix)#397

allmatrix1 <- allmatrix[,c(1,397,2:396)]

for (m in c(2:8,11:13)){
allmatrix1[,m] <- scale(allmatrix1[,m])
}
allmatrix1[,9] <- as.factor(allmatrix1[,9])
allmatrix1[,10] <- as.factor(allmatrix1[,10])


library(FRGEpistasis)
for (n in 14:ncol(allmatrix1)){
allmatrix1[,n] <- rankTransPheno(allmatrix1[,n],3/8)
}


result <- c()
for (j in 14:ncol(allmatrix1)){
fit <- lm(allmatrix1[,j] ~ prssex + f.26275.0.0 + sex + f.26201.0.0 + f.26201.0.1 + f.26201.0.2 + f.26201.0.3 + age + center + age2 + agesex + age2sex, data=allmatrix1)
result <- rbind(result,c(colnames(allmatrix1)[j],coef(summary(fit))[2,c(1,2,3,4)]))
} 
write.csv(result,file="sczprs_tbss_glm_prssex_result.csv")
