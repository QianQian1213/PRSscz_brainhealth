library(mediation)
library(FRGEpistasis)

mediation_matrix <- read.table("cma_sczprs_tbss_fluid_12672",header=TRUE,sep=" ")

for (m in c(2:7,10:12)){
mediation_matrix[,m] <- scale(mediation_matrix[,m])
}
mediation_matrix[,8] <- as.factor(mediation_matrix[,8])
mediation_matrix[,9] <- as.factor(mediation_matrix[,9])

for (n in 13:ncol(mediation_matrix)){
mediation_matrix[,n] <- rankTransPheno(mediation_matrix[,n],3/8)
}


mediation_matrix$f.26275.0.0<-as.numeric(mediation_matrix$f.26275.0.0)
mediation_matrix$f.26201.0.0<-as.numeric(mediation_matrix$f.26201.0.0)
mediation_matrix$f.26201.0.1<-as.numeric(mediation_matrix$f.26201.0.1)
mediation_matrix$f.26201.0.2<-as.numeric(mediation_matrix$f.26201.0.2)
mediation_matrix$f.26201.0.3<-as.numeric(mediation_matrix$f.26201.0.3)
mediation_matrix$age<-as.numeric(mediation_matrix$age)
mediation_matrix$age2<-as.numeric(mediation_matrix$age2)
mediation_matrix$agesex<-as.numeric(mediation_matrix$agesex)
mediation_matrix$age2sex<-as.numeric(mediation_matrix$age2sex)


set.seed(123)
med.fit <- lm(f.25071.2.0 ~ f.26275.0.0 + f.26201.0.0 + f.26201.0.1 + f.26201.0.2 + f.26201.0.3 + age + sex + center + age2 + agesex + age2sex, data = mediation_matrix)
out.fit <- lm(f.20191.0.0 ~ f.25071.2.0 + f.26275.0.0 + f.26201.0.0 + f.26201.0.1 + f.26201.0.2 + f.26201.0.3 + age + sex + center + age2 + agesex + age2sex,data = mediation_matrix)
med.out <- mediate(med.fit, out.fit, treat = "f.26275.0.0", mediator = "f.25071.2.0",
                   sims=2000, boot = T)
summary(med.out)
result <- summary(med.out)
res <- data.frame(matrix(nrow=1,ncol=27))
res[1,1]<-colnames(mediation_matrix)[6]
res[1,2]<-colnames(mediation_matrix)[13]
res[1,3]<-colnames(mediation_matrix)[74]
res[1,4]<-result$d.avg
res[1,5]<-result$d.avg.p
res[1,6:7]<-result$d.avg.ci
res[1,8]<-result$z.avg
res[1,9]<-result$z.avg.p
res[1,10:11]<-result$z.avg.ci
res[1,12]<-result$n.avg
res[1,13]<-result$n.avg.p
res[1,14:15]<-result$n.avg.ci
res[1,16]<-result$tau.coef
res[1,17]<-result$tau.p
res[1,18:19]<-result$tau.ci
res[1,20:21]<-summary(med.fit)$coefficients[2,c(1,4)]
res[1,22:23]<-confint(med.fit)[2,]
res[1,24:25]<-summary(out.fit)$coefficients[2,c(1,4)]
res[1,26:27]<-confint(out.fit)[2,]
colnames(res) <- c('treat','mediator','outcome','ACME_b','ACME_p','ACME_lower','ACME_upper','ADE_b','ADE_p','ADE_lower','ADE_upper','Prop','Prop_p','Prop_lower','Prop_upper','total_b','total_p','total_lower','total_upper','medfit_b','medfit_p','medfit_lower','medfit_upper','outfit_b','outfit_p','outfit_lower','outfit_upper')
write.csv(res,'sczprs_25071_fluid.csv')
