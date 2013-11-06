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
myvar <- list("DIS_CVA","MEDI_LPD","DIS_DIAB","DIS_AMI","GENDER","PM_BMI_CATEGORIAL",
              "LAB_TSC","LAB_HDL","LAB_GLUC_FASTING","PM_BMI_CONTINUOUS",
              "AGE_YRS")

# run the command to login and assign data
#SELECT OR EXCLUDE SERVERS TO USE
server.names<-logindata[,1]
#opals <- datashield.login(logins=logindata[(server.names=="ncds"|server.names=="prevend"|server.names=="lifelines"|
#                                server.names=="finrisk"|server.names=="kora"),], assign=TRUE, variables=myvar)


opals <- datashield.login(logins=logindata[(server.names=="ncds"|server.names=="prevend"|
                                              server.names=="finrisk"|server.names=="kora"),], assign=TRUE, variables=myvar)



# get the total number of participants by study and overall
ds.length(opals,xvect=quote(D$GENDER),type="split")
ds.length(opals,xvect=quote(D$GENDER),type="combine")



ds.histogram(opals,quote(D$LAB_HDL))
ds.heatmapplot(opals,quote(D$AGE_YRS),quote(D$LAB_HDL),type="split")
ds.contourplot(opals,quote(D$AGE_YRS),quote(D$LAB_HDL),type="split")
ds.heatmapplot(opals,quote(D$PM_BMI_CONTINUOUS),quote(D$LAB_GLUC_FASTING),type="split")
ds.contourplot(opals,quote(D$PM_BMI_CONTINUOUS),quote(D$LAB_GLUC_FASTING),type="split")
ds.heatmapplot(opals,quote(D$PM_BMI_CONTINUOUS),quote(D$LAB_GLUC_FASTING))
ds.contourplot(opals,quote(D$PM_BMI_CONTINUOUS),quote(D$LAB_GLUC_FASTING))

glm0a <- ds.glm(datasources=opals, formula=D$LAB_GLUC_FASTING ~ D$AGE_YRS+D$GENDER+D$PM_BMI_CATEGORIAL+D$LAB_TSC+D$LAB_HDL, family=quote(gaussian), maxit=quote(20))
glm0a

glm0b <- ds.glm(datasources=opals1245, formula=D$DIS_DIAB ~ D$AGE_YRS+D$GENDER+D$PM_BMI_CATEGORIAL+D$LAB_TSC+D$LAB_HDL, family=quote(binomial), maxit=quote(20))
glm0b

means.hdl.gender<-ds.propMean(opals,quote(D),outvar=quote(D$LAB_HDL),covar1=quote(D$GENDER))
means.hdl.gender
ds.names(opals,quote(GENDER$GENDER.level_0))
ds.names(opals,quote(GENDER$GENDER.level_1))



datashield.assign(opals,"G0",quote(GENDER$GENDER.level_0))
datashield.assign(opals,"G1",quote(GENDER$GENDER.level_1))

ds.names(opals,quote(G0))
ds.names(opals,quote(G1))

ds.mean(opals,quote(G0$LAB_HDL))
ds.mean(opals,quote(G1$LAB_HDL))

glm1a <- ds.glm(datasources=opals, formula=D$LAB_HDL ~ D$GENDER, family=quote(gaussian), maxit=quote(20))
glm1a

glm1 <- ds.glm(datasources=opals, formula=D$LAB_HDL ~ D$GENDER-1, family=quote(gaussian), maxit=quote(20))
glm1

ds.propMean(opals,quote(D),outvar=quote(D$DIS_DIAB),covar1=quote(D$GENDER))
ds.names(opals,quote(GENDER$GENDER.level_0))
ds.names(opals,quote(GENDER$GENDER.level_1))

lor2p.ci(-3.1867,0.0492)
lor2p.ci(-3.6001,0.0575)

ds.table1d(opals,quote(GENDER$GENDER.level_0$DIS_DIAB))
ds.table1d(opals,quote(GENDER$GENDER.level_1$DIS_DIAB))

ds.table2d(opals,quote(D$DIS_DIAB),quote(D$GENDER))

glm2 <- ds.glm(datasources=opals, formula=D$DIS_DIAB ~ D$GENDER-1, family=quote(binomial), maxit=quote(20))
glm2

lor2p.ci(-3.1867,0.0492)
lor2p.ci(-3.6001,0.0575)

glm3 <- ds.glm(datasources=opals, formula=D$DIS_DIAB ~ D$PM_BMI_CATEGORIAL-1, family=quote(binomial), maxit=quote(20))
glm3

ds.propMean(opals,quote(D),outvar=quote(D$DIS_DIAB),covar1=quote(D$PM_BMI_CATEGORIAL))

glm4 <- ds.glm(datasources=opals, formula=D$LAB_TSC ~ D$GENDER:D$PM_BMI_CATEGORIAL-1, family=quote(gaussian), maxit=quote(20))
glm4

#ds.propMean(opals,quote(GENDER$GENDER.level_0),outvar=quote(GENDER$GENDER.level_0$DIS_DIAB),covar1=quote(GENDER$GENDER.level_0$PM_BMI_CATEGORIAL))


ds.names(opals,quote(G0))
ds.names(opals,quote(G1))

ds.propMean(opals,quote(G0),outvar=quote(G0$LAB_TSC),covar1=quote(G0$PM_BMI_CATEGORIAL))
ds.propMean(opals,quote(G1),outvar=quote(G1$LAB_TSC),covar1=quote(G1$PM_BMI_CATEGORIAL))



glm5 <- ds.glm(datasources=opals, formula=D$DIS_DIAB ~ D$GENDER:D$PM_BMI_CATEGORIAL-1, family=quote(binomial), maxit=quote(20))
glm5


datashield.assign(opals,"G0",quote(GENDER$GENDER.level_0))
datashield.assign(opals,"G1",quote(GENDER$GENDER.level_1))

ds.names(opals,quote(G0))
ds.names(opals,quote(G1))

ds.propMean(opals,quote(G0),outvar=quote(G0$DIS_DIAB),covar1=quote(G0$PM_BMI_CATEGORIAL))
ds.propMean(opals,quote(G1),outvar=quote(G1$DIS_DIAB),covar1=quote(G1$PM_BMI_CATEGORIAL))


ds.createfactor(opals,xvect=quote(D$AGE_YRS_CATEGORICAL))

ds.levels(opals,xvect=quote(D$AGE_YRS_CATEGORICAL))


opals23<-opals[2:3]
glm6 <- ds.glm(datasources=opals23, formula=D$DIS_DIAB ~ D$AGE_YRS_CATEGORICAL-1, family=quote(binomial), maxit=quote(20))
glm6

glm7 <- ds.glm(datasources=opals23, formula=D$LAB_GLUC_FASTING ~ D$AGE_YRS_CATEGORICAL-1, family=quote(gaussian), maxit=quote(20))
glm7

glm8 <- ds.glm(datasources=opals23, formula=D$LAB_HDL ~ D$AGE_YRS_CATEGORICAL-1, family=quote(gaussian), maxit=quote(20))
glm8

ds.propMean(opals23,quote(D),outvar=quote(D$DIS_DIAB),covar1=quote(D$AGE_YRS_CATEGORICAL))



ds.table2d(opals,quote(D$AGE_YRS),quote(D$AGE_YRS_CATEGORICAL))


ds.subsetvar(opals23,xvect=quote(D$AGE_YRS),threshold=30,operator='<')



ds.propMean(opals,quote(D),outvar=quote(D$DIS_DIAB),covar1=quote(D$AGE_YRS_CATEGORICAL))

ds.class(opals,quote(D$AGE_YRS))

# LOOK AT UNIVARIATE DISTRIBUTION IN MORE DETAIL

# tabulate some binary variables
ds.table1d(datasources=opals, xvect=quote(D$DIS_CVA)) 
ds.table1d(datasources=opals, xvect=quote(D$MEDI_LPD)) 
ds.table1d(datasources=opals, xvect=quote(D$DIS_DIAB))
ds.table1d(datasources=opals, xvect=quote(D$DIS_AMI)) 
ds.table1d(datasources=opals, xvect=quote(D$GENDER))
# now see what happens if attempt to tabulate a continuous variable
ds.table1d(datasources=opals, xvect=quote(D$PM_BMI_CONTINUOUS))                    

# display quantile (you can choose to display the combine -default- or separate quantiles)
ds.quantilemean(datasources=opals, xvect=quote(D$LAB_TSC))
ds.quantilemean(datasources=opals, xvect=quote(D$LAB_TSC), type="split")
ds.quantilemean(datasources=opals, xvect=quote(D$LAB_HDL))
ds.quantilemean(datasources=opals, xvect=quote(D$LAB_HDL), type="split")
ds.quantilemean(datasources=opals, xvect=quote(D$PM_BMI_CONTINUOUS), type="split")
ds.quantilemean(datasources=opals, xvect=quote(D$LAB_GLUC_FASTING))

# plot histograms
ds.histogram(datasources=opals, xvect=quote(D$LAB_HDL))
ds.histogram(datasources=opals, xvect=quote(D$LAB_HDL), type="split")
ds.histogram(datasources=opals, xvect=quote(D$PM_BMI_CONTINUOUS))
ds.histogram(datasources=opals, xvect=quote(D$PM_BMI_CONTINUOUS), type="split")


# ANSWERING SOME QUESTIONS

# Question 1
# To see if there is a difference across two variables we can use t.test.
ds.t.test(datasources=opals, x=quote(D$LAB_TSC), y=quote(D$LAB_HDL))
ds.t.test(datasources=opals, x=quote(D$LAB_TSC), y=quote(D$LAB_HDL), type="split")

# We can save the outcome to our workspace by adding in '<-'
q11 <- ds.t.test(datasources=opals, x=quote(D$LAB_TSC), y=quote(D$LAB_HDL))


# Question 2
# to explore the relationship between 'LAB_GLUC_FASTING' and 'PM_BMI_CATEGORIAL'
# let us tabulate the factor variable
ds.table1d(datasources=opals, xvect=quote(D$PM_BMI_CATEGORIAL))    

# use the glm function the explore the relationship
glm.mod2 <- ds.glm(datasources=opals, formula=D$LAB_GLUC_FASTING ~ D$PM_BMI_CATEGORIAL, family=quote(gaussian), maxit=quote(20))
# display a summary of the results
glm.mod2


# Question 3
# To determine a trend over two categorical factor variables
# let us cross tabulate those two variables using the function 'ds.table2d'
ds.table2d(datasources=opals, xvect=quote(D$DIS_DIAB), yvect=quote(D$PM_BMI_CATEGORIAL))


# We can formally test the differing proportions across categories by using the function 'ds.table2' 
# and look into the chi-squred results
q3 <- ds.table2d(datasources=opals, xvect=quote(D$DIS_DIAB), yvect=quote(D$PM_BMI_CATEGORIAL))
q3$CHI2.TESTS.FOR.HOMOGENEITY


# Question 4. 
# To determine if total serum cholesterol levels are associated with HDL cholesterol levels
# For example, this can be carried out by plotting a scatter plot or performing a correlation or
# using linear regression using LAB_TSC as an outcome and LAB_HDL  as a covariate other covariates
# can be added to determine other significant variables correlated with total serum cholesterol.
ds.heatmapplot(datasources=opals, xvect=quote(D$LAB_TSC), yvect=quote(D$LAB_HDL))
# now let us produce the hetamap plots of the studies separately 
ds.heatmapplot(datasources=opals, xvect=quote(D$LAB_TSC), yvect=quote(D$LAB_HDL), type="split")

# generate pooled contour plot
ds.contourplot(datasources=opals, xvect=quote(D$LAB_TSC), yvect=quote(D$LAB_HDL))

glm.mod4 <- ds.glm(datasources=opals, formula=D$LAB_TSC~D$LAB_HDL, family=quote(gaussian), maxit=quote(20))
glm.mod4


# Question 5
# To determine predictors of binary outcomes we need to use the function glm.
# The glm function can be used to analyse any input or output whether it be categorical or numerical.  
# If the outcome to analyse is numeric then we use the “gaussian” option of the glm.  
# If the outcome contains two categories we can use the “binomial” option.  
# So the type of glm function we need to answer research question 5 is the one which has family input “binomial”.  

# first let us tabulate the two variables
ds.table2d(datasources=opals, xvect=quote(D$DIS_DIAB), yvect=quote(D$PM_BMI_CATEGORIAL))

# now run glm to predict diabetes status using categorical bmi
glm.mod5_1 <- ds.glm(datasources=opals, formula=D$DIS_DIAB ~ D$PM_BMI_CATEGORIAL, family=quote(binomial), maxit=quote(20))
glm.mod5_1

# run glm to predict diabestes using gender, continuous bmi and hdl cholesterol
glm.mod5_2 <- ds.glm(datasources=opals, formula=D$DIS_DIAB~D$GENDER+D$PM_BMI_CONTINUOUS+D$LAB_HDL, family=quote(binomial), maxit=quote(20))
glm.mod5_2


# Question 6 
# To determine predictors of taking lipid reducing medications
# For example, a logistic regression can, again, be used to determine whether
# some HOP variables are significantly associated with a taking lipid reducing medications, MEDI_LPD. 
glm.mod6_1 <- ds.glm(datasources=opals, formula=D$MEDI_LPD~D$GENDER*D$LAB_HDL+D$PM_BMI_CONTINUOUS+D$LAB_TSC, family=quote(binomial), maxit=quote(20))
glm.mod6_1

# quantile of the variable 'LAB_HDL'
ds.quantilemean(datasources=opals, xvect=quote(D$LAB_HDL))
# substract the mean 
datashield.assign(opals, 'HDL.1.5', quote(D$LAB_HDL-1.5))

# run another glm analysis using the above adjusted hdl variable
glm.mod6_2 <- ds.glm(datasources=opals, formula=D$MEDI_LPD~D$GENDER*HDL.1.5+D$PM_BMI_CONTINUOUS+D$LAB_TSC, family=quote(binomial), maxit=quote(20))
glm.mod6_2

datashield.logout(opals)