allmatrix <- read.table("final_sczprs_cov_tbss_23468",header=TRUE,sep=" ")

for (m in c(2:7,10:12)){
allmatrix[,m] <- scale(allmatrix[,m])
}
allmatrix[,8] <- as.factor(allmatrix[,8])
allmatrix[,9] <- as.factor(allmatrix[,9])

library(FRGEpistasis)
for (n in 13:ncol(allmatrix)){
allmatrix[,n] <- rankTransPheno(allmatrix[,n],3/8)
}

result <- c()
for (j in 13:ncol(allmatrix)){
fit <- lm(allmatrix[,j] ~ f.26275.0.0 + f.26201.0.0 + f.26201.0.1 + f.26201.0.2 + f.26201.0.3 + age + sex + center + age2 + agesex + age2sex, data=allmatrix)
result <- rbind(result,c(colnames(allmatrix)[j],coef(summary(fit))[2,c(1,2,3,4)]))
} 
write.csv(result,file="sczprs_tbss_glm_result.csv")
