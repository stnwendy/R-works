library(readxl)
library(lmtest)
library(tseries)

#import excel data
raw_data <- read_excel("3rd year/1ST SEM/ELEC 03 Thesis Writing/Stationary_Pen.xlsx")

#assign values of variables
ncrHPI_dv <- raw_data[,7]   #HPI NCR
aoncrHPI_dv <- raw_data[,8] #HPI AONCR
  
pop_iv <- raw_data[,1] #population
emp_iv <- raw_data[,2] #employment
gdp_iv <- raw_data[,3] #GDP
len_iv <- raw_data[,4] #lending rate
inf_iv <- raw_data[,5] #inflation
pse_iv <- raw_data[,6] #PSEi

#test print DELETE
print(pop_iv)


#run ADF for stationarity
tspop <- ts(pop_iv)
tsemp <- ts(emp_iv)
tsgdp <- ts(gdp_iv)
tslen <- ts(len_iv)
tsinf <- ts(inf_iv)
tspse <- ts(pse_iv)
tsncrHPI <- ts(ncrHPI_dv)
tsaoncrHPI <- ts(aoncrHPI_dv)
tseries::adf.test(tspop, alternative = c("stationary", "explosive"), k = 3)
tseries::adf.test(tsemp, alternative = c("stationary", "explosive"), k = 3)
tseries::adf.test(tsgdp, alternative = c("stationary", "explosive"), k = 3)
tseries::adf.test(tslen, alternative = c("stationary", "explosive"), k = 3)
tseries::adf.test(tsinf, alternative = c("stationary", "explosive"), k = 3)
tseries::adf.test(tspse, alternative = c("stationary", "explosive"), k = 3)
tseries::adf.test(tsncrHPI, alternative = c("stationary", "explosive"), k = 3)
tseries::adf.test(tsaoncrHPI, alternative = c("stationary", "explosive"), k = 3)


#perform Granger-causality using lmtest package

#Granger of initial IVs to HPI NCR
grangertest(tspop, tsncrHPI, order=3, test="F")
grangertest(tsemp, tsncrHPI, order=3, test="F")
grangertest(tsgdp, tsncrHPI, order=3, test="F")
grangertest(tslen, tsncrHPI, order=3, test="F")
grangertest(tsinf, tsncrHPI, order=3, test="F")
grangertest(tspse, tsncrHPI, order=3, test="F")

#Granger of HPI NCR to initial IVs
grangertest(tsncrHPI, tspop, order=3, test="F")
grangertest(tsncrHPI, tsemp, order=3, test="F")
grangertest(tsncrHPI, tsgdp, order=3, test="F")
grangertest(tsncrHPI, tslen, order=3, test="F")
grangertest(tsncrHPI, tsinf, order=3, test="F")
grangertest(tsncrHPI, tspse, order=3, test="F")

#Granger of initial IVs to HPI AONCR
grangertest(tspop, tsaoncrHPI, order=3, test="F")
grangertest(tsemp, tsaoncrHPI, order=3, test="F")
grangertest(tsgdp, tsaoncrHPI, order=3, test="F")
grangertest(tslen, tsaoncrHPI, order=3, test="F")
grangertest(tsinf, tsaoncrHPI, order=3, test="F")
grangertest(tspse, tsaoncrHPI, order=3, test="F")

#Granger of HPI AONCR to initial IVs
grangertest(tsaoncrHPI, tspop, order=3, test="F")
grangertest(tsaoncrHPI, tsemp, order=3, test="F")
grangertest(tsaoncrHPI, tsgdp, order=3, test="F")
grangertest(tsaoncrHPI, tslen, order=3, test="F")
grangertest(tsaoncrHPI, tsinf, order=3, test="F")
grangertest(tsaoncrHPI, tspse, order=3, test="F")





