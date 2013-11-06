# LOAD REQUIRED LIBRARIES
library(dsbaseclient)
library(dsmodellingclient)

lor2p.ci<-function(beta,beta.se,confidence=0.95)
{
  mult.ci<-qnorm(1-(1-confidence)/2)
  lor.est<-beta
  lor.lci<-beta-mult.ci*beta.se
  lor.uci<-beta+mult.ci*beta.se
  or.est<-exp(lor.est)
  or.lci<-exp(lor.lci)
  or.uci<-exp(lor.uci)
  p.est<-or.est/(1+or.est)
  p.lci<-or.lci/(1+or.lci)
  p.uci<-or.uci/(1+or.uci)
  
  estimates<-c(lor.est,or.est,p.est)
  lower.ci<-c(lor.lci,or.lci,p.lci)
  upper.ci<-c(lor.uci,or.uci,p.uci)
  confidence.level<-c(mult.ci,mult.ci,mult.ci)
  out.mat<-cbind(estimates,lower.ci,upper.ci,confidence.level)
  dimnames(out.mat)<-list(c("Log Odds Ratio","Odds Ratio","Proportion"),
                         c("Estimate","Lower CI","Upper CI","Confidence Level"))
  return(out.mat)
  
}

# LOAD THE TABLE THAT CONTAINS THE LOGIN INFO
load("~/workshop-barcelona/logindata4bioshare.rda")


# LOGIN TO COLLABORATING SERVERS AND ASSIGN DATA
# the variables to assign
myvar <- list("HLTH_OBESE_STRICT","HLTH_OBESE_MODERATE","DIS_CVA","MEDI_LPD","DIS_DIAB","DIS_AMI","GENDER","PM_BMI_CATEGORIAL",
              "LAB_TSC","LAB_HDL","LAB_GLUC_FASTING","PM_BMI_CONTINUOUS","AGE_YRS")


#SELECT OR EXCLUDE SERVERS TO USE
server.names<-logindata[,1]

#With Lifelines
#opals <- datashield.login(logins=logindata[(server.names=="ncds"|server.names=="prevend"|server.names=="lifelines"|
#                                server.names=="finrisk"|server.names=="kora"),], assign=TRUE, variables=myvar)

#Without Lifelines
opals <- datashield.login(logins=logindata[(server.names=="ncds"|server.names=="prevend"|
                                              server.names=="finrisk"|server.names=="kora"),], assign=TRUE, variables=myvar)



# get the total number of participants by study and overall
ds.length(opals,xvect=quote(D$GENDER),type="split")
ds.length(opals,xvect=quote(D$GENDER),type="combine")


