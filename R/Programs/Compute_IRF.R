rm(list=ls())

#Packages I require
library(readxl)
library(boot)
require('ggplot2')
library(BVAR)
library(bsvars)

#Run some required funcitions
source('C:/Users/ojaulime/OneDrive - Banco de la República/Documents/Research/MP transmission in Colombia/Monetary-Policy-Shocks-and-Central-bank-information-in-Colombia/R/Programs/create_lags.r')

#Set working Directory
setwd('C:/Users/ojaulime/OneDrive - Banco de la República/Documents/Research/MP transmission in Colombia/Monetary-Policy-Shocks-and-Central-bank-information-in-Colombia/R/Input/')

# Upload the data
DATA      <- read_excel('DATA.xlsx')

#Define the variables of analysis

#1). Dependent Variables
Var_names  = c('GDP','INT','CPI','EXC')
Var_names  = c('CPI')
#Var_Cont   = c('GDP','CPI','INT','EXC','CDS')
Var_Cont   = c('GDP','CPI','INT','CDS')
#2). Monetary policy shock proxies
MP_shocks  = c('ShockBloom','ShockOISL','ShockBR')
MP_shocks  = c('ShockBloom','MP_bloo_BEI','CBI_bloo_BEI')

#3) High frequency shocks in
HF_Shocks  = c('Shock CDs','Shock TRM')

## Parameters for Estimation
p          = 1
hh1        = c(1:60)
boot       = TRUE
block_size <- 4 
conf1      <- 0.32
conf2      <- 0.1
color         = "blue4"

for(ss in MP_shocks){
  #shock_anal = MP_shocks[1]
  shock_anal  = ss
  Data_Anal  <- DATA[,c('Date',unique(c(Var_names,Var_Cont)),shock_anal)]
  Data_Anal  <- Data_Anal[complete.cases(Data_Anal),]
  source('C:/Users/ojaulime/OneDrive - Banco de la República/Documents/Research/MP transmission in Colombia/Monetary-Policy-Shocks-and-Central-bank-information-in-Colombia/R/Programs/IRF_Mean.r')
}

# Extract the CDS component from the shocks
DATA_Cop= DATA
ss = MP_shocks[3]
Data_Risk = create_lagged_df(DATA_Cop, vars = c('INT','CDS','CPI','GDP'), h = 1)
Data_Shock= merge(DATA_Cop[,c('Date',ss)],Data_Risk,by = 'Date')
Data_Shock= Data_Shock[complete.cases(Data_Shock),]
fml       = as.formula(paste0(ss,'~ ',paste0(colnames(Data_Risk[,2:ncol(Data_Risk),drop=F]),collapse = '+')))
OLS       = lm(fml,data = Data_Shock)
summary(OLS)
res       = as.matrix(OLS$residuals)
Data_Shock= cbind(Data_Shock,res)
DATA_Cop      = merge(DATA_Cop,Data_Shock[,c('Date','res')],by='Date')
shock_anal  = 'res'
Var_Cont  = NULL
Data_Anal  <- DATA_Cop[,c('Date',unique(c(Var_names)),shock_anal)]
Data_Anal  <- Data_Anal[complete.cases(Data_Anal),]
source('C:/Users/ojaulime/OneDrive - Banco de la República/Documents/Research/MP transmission in Colombia/Monetary-Policy-Shocks-and-Central-bank-information-in-Colombia/R/Programs/IRF_Mean.r')

#----------------------------------------------
# Estimation of a BVAR using the package bsvars
#----------------------------------------------
pLags      = 6
nSteps     = 60
Confidence = 1-conf1
Var_shock  = c('ShockOISL')
#Var_shock  = c('CBI_OIS_BEI')
VarNames2  = c(Var_shock,Var_Cont)
#Var_shock  = c('INT')
#VarNames2  = Var_Cont
year_ini  = substr(DATA$Date,1,4)[1]
month_ini = as.numeric(substr(DATA$Date,6,7)[1])
Data_CGV  = matrix(as.numeric(as.matrix(DATA[,VarNames2])),nrow(DATA),length(VarNames2))
colnames(Data_CGV) = VarNames2
#X  <- na.omit(df[, c("Y","pi","i")])
X  <- na.omit(Data_CGV[, c(VarNames2)])

# 2) Especificación BVAR homocedástico (BVAR con Minnesota)
p <- pLags
stationary_vec <- c(FALSE,rep(TRUE, ncol(X)-1))  # ajusta según tus series

spec <- specify_bsvar$new(
  data       = as.matrix(X),
  p          = p,
  stationary = stationary_vec,
)

# 3) Estimación (burn-in + cadena principal)
fit_burn <- estimate(spec,     S = 2000,  thin = 1)
fit_bvar <- estimate(fit_burn, S = 20000, thin = 1)

# 4) Resumen
summary(fit_bvar)


irfs <- compute_impulse_responses(fit_bvar, horizon = nSteps)
plot(irfs,probability = Confidence)





#----------------------------------------------
# Estimation of a BVAR using the package bvar
#----------------------------------------------
pLags      = 13
nSteps     = 60
Confidence = 0.68
Var_shock  = c('ShockBloom')
Var_shock  = c('CBI_bloo_BEI')
VarNames2  = c(Var_shock,Var_Cont)
#Var_shock  = c('INT')
#VarNames2  = Var_Cont
year_ini  = substr(DATA$Date,1,4)[1]
month_ini = as.numeric(substr(DATA$Date,6,7)[1])
Data_CGV  = matrix(as.numeric(as.matrix(DATA[,VarNames2])),nrow(DATA),length(VarNames2))
colnames(Data_CGV) = VarNames2

Data_CGV  = Data_CGV[complete.cases(Data_CGV),]
Data_CGV  = ts(Data_CGV,frequency=12,start=c(year_ini,month_ini))

Shock_value        = 1

set.seed(123456)
mn_l <- bv_minnesota(
  lambda = bv_lambda(mode = 0.2, sd = 0.4, min = 0.0001, max = 5), #sets the prior for the tightness (lambda)
  alpha = bv_alpha(mode =2), #shrinkage parameter alpha set to 2
  var = 1e07) 

priors <- bv_priors(hyper = "auto", mn = mn_l)

mh <- bv_metropolis(scale_hess = c(0.05),
                    adjust_acc = TRUE, acc_lower = 0.25, acc_upper = 0.45)

Model1          <- bvar(Data_CGV,lags=pLags,n_draw=20000,nburn=5000,n_thin = 10L, priors = priors, mh = mh, verbose = FALSE)

## IRF of Model 1

varshock = c(Var_shock)
#Cholesky IRF
IRF_BVAR             <- irf(Model1,horizon = nSteps)
IRF_BVAR             <- array(IRF_BVAR$irf[,,,which(VarNames2==varshock)],dim=c(dim(IRF_BVAR$irf)[1],dim(IRF_BVAR$irf)[2],dim(IRF_BVAR$irf)[3]))

#Put the IRF in the format I want

IR_Ch1              = matrix(NaN,nSteps,ncol(Data_CGV))
IR_Ch_Low1          = matrix(NaN,nSteps,ncol(Data_CGV))
IR_Ch_sup1          = matrix(NaN,nSteps,ncol(Data_CGV))
for(n in 1:length(VarNames2)){
  for(h in 1:nSteps){
    IR_Ch1[h,n]         = quantile(IRF_BVAR[,n,h]*100,0.5)
    IR_Ch_Low1[h,n]     = quantile(IRF_BVAR[,n,h]*100,(1-Confidence)/2)
    IR_Ch_sup1[h,n]     = quantile(IRF_BVAR[,n,h]*100,1-(1-Confidence)/2)
  }
}

# plot the IRFs 
VarNames3 = VarNames2

par(mfrow=c(ceiling(length(VarNames3)^(1/2)),ceiling(length(VarNames3)/ceiling(length(VarNames3)^(1/2)))))

for(i in 1:length(VarNames2)) {
  #nn <- Long_Names[which(Long_Names[, 'Name'] == VarNames1[i]), 'Long_Name']
  L_Name      = VarNames2[i]
  #jpeg(paste0(L_Name, ".jpeg"), width = 350, height = 350)
  plot.ts(IR_Ch1[, i], ylim = c(min(cbind(IR_Ch_Low1[, i])), max(cbind(IR_Ch_sup1[, i]))),
          main = L_Name,
          ylab = 'Percent',
          xlab = 'Horizons',
          col = 'blue4',
          lwd = 2)
  lines(IR_Ch_Low1[, i], col = 'blue4', lty = 2, lwd = 2)
  lines(IR_Ch_sup1[, i], col = 'blue4', lty = 2, lwd = 2)
  
  
  # Agregar área sombreada
  polygon(x = c(time(IR_Ch1), rev(time(IR_Ch1))), 
          y = c(IR_Ch_Low1[, i], rev(IR_Ch_sup1[, i])), 
          col = alpha('blue4', 0.2), 
          border = NA)
  
  # Agregar línea horizontal en y=0
  abline(h = 0, col = 'azure4', lwd = 2)
  
  # Agregar rejillas (grids)
  grid(col = "gray", lty = "dotted")
  #dev.off()
}


