
#SHOW dsbaseclient AND dsmodelling FUNCTIONS, HELP AND EXAMPLES

#DATA DEPLOYED IN EACH SERVER-SIDE R ENVIRONMENT IN A DATAFRAME CALLED D
#WHAT IS THERE?
ds.colnames(opals,quote(D))

ds.class(opals,quote(D$PM_BMI_CATEGORIAL))
ds.class(opals,quote(D$LAB_HDL))

#BASIC GRAPHS AND PLOTS
#MAKE SURE MARGINS ARE WIDE ENOUGH TO PLOT!
ds.histogram(opals,quote(D$LAB_HDL))
ds.histogram(opals,quote(D$LAB_HDL),type="split")

#NAVIGATE AROUND PLOTS ALREADY MADE USING BLUE ARROWS 

ds.heatmapplot(opals,quote(D$PM_BMI_CONTINUOUS),quote(D$LAB_HDL),type="combine")
ds.contourplot(opals,quote(D$PM_BMI_CONTINUOUS),quote(D$LAB_HDL),type="split")


#CREATE NEW VARIABLES IN SERVERS
datashield.assign(opals,"age.50",quote(D$AGE_YRS-50))
ds.mean(opals,quote(D$AGE_YRS))
ds.mean(opals,quote(age.50))
ds.var(opals,quote(age.50))


#Arithmetic operators, log, exponential ...

ds.quantilemean(opals,quote(D$AGE_YRS))

#ILLEGAL OR INOPERATIVE: ds.range(opals,quote(D$AGE_YRS)), ds.summary(opals,quote(D$AGE_YRS))

#TABLES
ds.table1d(opals,quote(D$PM_BMI_CATEGORIAL))
ds.table2d(opals,quote(D$PM_BMI_CATEGORIAL),quote(D$GENDER))
ds.table2d(opals,quote(D$PM_BMI_CATEGORIAL),quote(D$GENDER),type="split")

#GLMs
#LINEAR REGRESSION
glm1 <- ds.glm(datasources=opals, formula=D$LAB_TSC ~ D$AGE_YRS+D$GENDER+D$PM_BMI_CATEGORIAL, family=quote(gaussian), maxit=quote(20))
glm1

#LOGISTIC REGRESSION
glm2 <- ds.glm(datasources=opals, formula=D$DIS_CVA ~ D$AGE_YRS+D$GENDER+D$PM_BMI_CATEGORIAL, family=quote(binomial), maxit=quote(20))
glm2

#COEFFICIENTS ON SCALE OF LOG-ODDS
#ODDS=exp(LOG-ODDS)
#P=ODDS/(1+ODDS)
#ODDS=P/(1-P)

#CALCULATING MEAN OR PROPORTIONS OF A VARIABLE (WITH CONFIDENCE INTERVALS) IN SUBGROUPS
#SUBSETTING AND ESTIMATION IN SUBSETS
ds.propMean(opals,quote(D),outvar=quote(D$LAB_HDL),covar1=quote(D$GENDER))
mean.hdl.gender<-ds.propMean(opals,quote(D),outvar=quote(D$LAB_HDL),covar1=quote(D$GENDER))
mean.hdl.gender

ds.names(opals,quote(GENDER$GENDER.level_0))
ds.names(opals,quote(GENDER$GENDER.level_1))

datashield.assign(opals,"G0",quote(GENDER$GENDER.level_0))
datashield.assign(opals,"G1",quote(GENDER$GENDER.level_1))

ds.names(opals,quote(G0))
ds.names(opals,quote(G1))

ds.mean(opals,quote(G0$LAB_HDL))
ds.mean(opals,quote(G1$LAB_HDL))

mean.hdl.gender

glm3 <- ds.glm(datasources=opals, formula=D$LAB_HDL ~ D$GENDER, family=quote(gaussian), maxit=quote(20))
glm3

glm4 <- ds.glm(datasources=opals, formula=D$LAB_HDL ~ D$GENDER-1, family=quote(gaussian), maxit=quote(20))
glm4

mean.hdl.gender

prop.diab.gender<-ds.propMean(opals,quote(D),outvar=quote(D$DIS_DIAB),covar1=quote(D$GENDER))
prop.diab.gender
lor2p.ci(-3.1867,0.0492)
lor2p.ci(-3.6001,0.0575)

ds.table1d(opals,quote(GENDER$GENDER.level_0$DIS_DIAB))
ds.table1d(opals,quote(G1$DIS_DIAB))
ds.table2d(opals,quote(D$DIS_DIAB),quote(D$GENDER))

glm5 <- ds.glm(datasources=opals, formula=D$DIS_DIAB ~ D$GENDER-1, family=quote(binomial), maxit=quote(20))
glm5



glm6 <- ds.glm(datasources=opals, formula=D$LAB_HDL ~ D$GENDER:D$PM_BMI_CATEGORIAL-1, family=quote(gaussian), maxit=quote(20))
glm6

ds.propMean(opals,quote(G0),outvar=quote(G0$LAB_HDL),covar1=quote(G0$PM_BMI_CATEGORIAL))
ds.propMean(opals,quote(G1),outvar=quote(G1$LAB_HDL),covar1=quote(G1$PM_BMI_CATEGORIAL))
glm6

ds.names(opals,quote(PM_BMI_CATEGORIAL$PM_BMI_CATEGORIAL.level_1))
ds.mean(opals,quote(PM_BMI_CATEGORIAL$PM_BMI_CATEGORIAL.level_1$LAB_HDL))


glm7 <- ds.glm(datasources=opals, formula=D$DIS_DIAB ~ D$GENDER:D$PM_BMI_CATEGORIAL-1, family=quote(binomial), maxit=quote(20))
glm7

glm8 <- ds.glm(datasources=opals, formula=D$HLTH_OBESE_STRICT ~ D$GENDER:D$PM_BMI_CATEGORIAL-1, family=quote(binomial), maxit=quote(30))
glm8

#HEALTHY OBESE - CONSIDER APPROPRIATE ANALYSIS CAREFULLY