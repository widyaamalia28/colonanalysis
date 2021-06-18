library(survival)
data(colon)
head(colon)

summary(colon)

#=========== Kaplan Meier Curve for Each Variabel========================
#Kaplan-Meier Curve (Etype)
KM=survfit(Surv(time,status)~etype,data=colon)
par(mfrow = c(1, 2))
plot(KM, lty=1:3, xlab="survival time (in days)", ylab="survival probability")
legend(100, 0.2, c("etype1", "etype2"),lty = 1:3)
plot(KM, lty = 1:3,fun = function (s) -log(-log(s)), xlab = "survival time (in days)", 
     ylab = "-log(- log(survival probability))")

#log rank test (Etype)
survdiff(Surv(time, status) ~ etype,data=colon, rho=0)

==================================================
#Kaplan-Meier Curve (Sex)
KM2=survfit(Surv(time,status)~sex,data=colon)
par(mfrow = c(1, 2))
plot(KM2, lty=1:3, xlab="survival time (in days)", ylab="survival probability")
legend(100, 0.2, c("1=Male", "2=Female"),lty = 1:3)
plot(KM2, lty = 1:3,fun = function (s) -log(-log(s)), xlab = "survival time (in days)", 
     ylab = "-log(- log(survival probability))")

#log rank test (Sex)
survdiff(Surv(time, status) ~ sex,data=colon, rho=0)
=================================================================

#Kaplan-Meier Curve (Differ)
KM3=survfit(Surv(time,status)~differ,data=colon)
par(mfrow = c(1, 2))
plot(KM3, lty=1:3, xlab="survival time (in days)", ylab="survival probability")
legend(100, 0.2, c("1=well", "2=moderate", "3=poor"),lty = 1:3)
plot(KM3, lty = 1:3,fun = function (s) -log(-log(s)), xlab = "survival time (in days)", 
     ylab = "-log(- log(survival probability))")

#log rank test (Differ)
survdiff(Surv(time, status) ~ differ,data=colon, rho=0)

========================================================================
#Kaplan-Meier Curve (Extent)
KM4=survfit(Surv(time,status)~extent,data=colon)
par(mfrow = c(1, 2))
plot(KM4, lty=1:3, xlab="survival time (in days)", ylab="survival probability")
legend(100, 0.2, c("1=submucosa", "2=muscle", "3=serosa", "4=cont.structure"),lty = 1:3)
plot(KM4, lty = 1:3,fun = function (s) -log(-log(s)), xlab = "survival time (in days)", 
     ylab = "-log(- log(survival probability))")

#log rank test (Extent)
survdiff(Surv(time, status) ~ extent,data=colon, rho=0)
====================================================================================
#Kaplan-Meier Curve (Surg)
KM5=survfit(Surv(time,status)~surg,data=colon)
par(mfrow = c(1, 2))
plot(KM5, lty=1:3, xlab="survival time (in days)", ylab="survival probability")
legend(100, 0.2, c("0=short", "1=long"),lty = 1:3)
plot(KM5, lty = 1:3,fun = function (s) -log(-log(s)), xlab = "survival time (in days)", 
     ylab = "-log(- log(survival probability))")

#log rank test (Surg)
survdiff(Surv(time, status) ~ surg,data=colon, rho=0)

================================================================

#Model Cox PH
coxPH<-coxph(formula=Surv(time,status)~age+etype+sex+differ+extent+surg+nodes,data=colon)
summary(coxPH)  #exp(coef)=hazard_ratio

(res.zph1 <- cox.zph(coxPH))  #Grambsch_Therneaue_test
plot(res.zph1)

=========================================================================
#Stratified #Etype Differ Dihapus

table(colon$nodes)
#Etype_Dihapus_DIffer_Ada
coxPH.strata <- coxph(Surv(time,status)~age+sex+extent+surg+nodes+differ+strata(etype),data=colon)
cox.zph(coxPH.strata)
summary(coxPH.strata)

(res.zph1 <- cox.zph(coxPH.strata))  #Grambsch_Therneaue_test
plot(res.zph1)

#Differ_Dihapus_Etype_Ada
coxPH1.strata <- coxph(Surv(time,status)~age+sex+extent+surg+nodes+etype+strata(differ),data=colon)
cox.zph(coxPH1.strata)
summary(coxPH1.strata)

(res.zph1 <- cox.zph(coxPH1.strata))  #Grambsch_Therneaue_test
plot(res.zph1)


#Differ_Etype_Dihapus
coxPH2.strata <- coxph(Surv(time,status)~age+sex+extent+surg+nodes++strata(differ)+strata(etype),data=colon)
cox.zph(coxPH2.strata)
summary(coxPH2.strata)

(res.zph1 <- cox.zph(coxPH2.strata))  #Grambsch_Therneaue_test
plot(res.zph1)
