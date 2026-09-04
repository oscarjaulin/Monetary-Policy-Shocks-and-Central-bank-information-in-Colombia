rm(list=ls())

# Packages I require
library(readxl)
library(boot)
require('ggplot2')
library(BVAR)
library(bsvars)
library(AER)       # Required for ivreg()
library(sandwich)  # Required for Newey-West HAC standard errors
library(lmtest)    # Required for coeftest()

# Run some required funcitions
#source('C:/Users/ojaulime/OneDrive - Banco de la República/Documents/Research/MP transmission in Colombia/Monetary-Policy-Shocks-and-Central-bank-information-in-Colombia/R/Programs/create_lags.r')
source('D:/Disco C/Repositorios Git/Monetary-Policy-Shocks-and-Central-bank-information-in-Colombia/IVLP/Programs/create_lags.r')

# Set working Directory
#setwd('C:/Users/ojaulime/OneDrive - Banco de la República/Documents/Research/MP transmission in Colombia/Monetary-Policy-Shocks-and-Central-bank-information-in-Colombia/R/Input/')
setwd('D:/Disco C/Repositorios Git/Monetary-Policy-Shocks-and-Central-bank-information-in-Colombia/IVLP/Input/')

# Upload the data
DATA      <- read_excel('DATA_Monthly_2008_2025.xlsx')


# Define the variables of analysis

# 1). Dependent Variables
Var_names  = c('GDP','CPI','CPISAR')
Var_Cont   = c('GDP','CPI','CPISAR','TIB')

# 2). Shock and Instrument (Customizable)
endog_var  = 'TIB'         # The endogenous variable to be instrumented
inst_var   = 'MP'   # The instrument

## Parameters for Estimation
p          = 12
hh1        = c(1:60)
boot       = FALSE
block_size <- 4 
conf1      <- 0.32
conf2      <- 0.1
color      = "blue4"

# Ensure the data contains the endog_var and inst_var
Data_Anal  <- DATA[, c('Fecha', unique(c(Var_names, Var_Cont)), endog_var, inst_var)]
Data_Anal  <- Data_Anal[complete.cases(Data_Anal),]

IRF_RESULTS   <- array(NA, dim = c(length(Var_names), (length(Var_Cont)*p)+2, 5, length(hh1)))

#setwd('C:/Users/ojaulime/OneDrive - Banco de la República/Documents/Research/MP transmission in Colombia/Monetary-Policy-Shocks-and-Central-bank-information-in-Colombia/IVLP/Output/')
setwd('D:/Disco C/Repositorios Git/Monetary-Policy-Shocks-and-Central-bank-information-in-Colombia/IVLP/Output/')

for(n in Var_names){
  message(paste0("Estimating: ", n))
  for(h in hh1){
    nn        = which(Var_names == n)
    hh        = which(hh1 == h)
    Yt        <- create_leads_df(Data_Anal, vars = n, h = h, all_leads = FALSE) 
    Yt_lag    <- create_lagged_df(Data_Anal, vars = n, h = 1)
    
    # long difference
    Yt[,2]    <- as.numeric(Yt[,2])
    has_c <- p != 0
    
    if (has_c || !is.null(Var_Cont)) {
      lagged_c <- if (any(has_c)) create_lagged_df(dataset = Data_Anal, vars = Var_Cont, h = p) else NULL
      
      pieces <- Filter(Negate(is.null), list(lagged_c))
      
      lagged_vars <- if (length(pieces) == 1) {
        pieces[[1]]
      } else {
        Reduce(function(x, y) merge(x, y, by = "Fecha", all.x = TRUE), pieces)
      }
      
      # Merge both the endogenous variable AND the instrument with the lagged controls
      temp      <- merge(Data_Anal[, c('Fecha', endog_var, inst_var)], lagged_vars, by = "Fecha", all.x = TRUE)
    } else {
      # Endogenous variable and instrument only
      temp      <- Data_Anal[, c('Fecha', endog_var, inst_var)]
    }  
    
    # Merge the dependent variable
    temp      <- merge(Yt, temp, by = 'Fecha', all.x = TRUE)
    
    Estimation_Data = as.data.frame(temp[complete.cases(temp), -c(1)])
    Estimation_Data = convert_to_numeric(Estimation_Data)
    
    # Create the IV formula: Y ~ endog_var + controls | inst_var + controls
    y_col <- colnames(Yt[, -c(1), drop = FALSE])
    
    if(p != 0){
      ctrl_cols <- colnames(lagged_vars[, -c(1), drop = FALSE])
      ctrl_str  <- paste(ctrl_cols, collapse = " + ")
      fml_str   <- paste(y_col, "~", endog_var, "+", ctrl_str, "|", inst_var, "+", ctrl_str)
    } else {
      fml_str   <- paste(y_col, "~", endog_var, "|", inst_var)
    }
    
    fml <- as.formula(fml_str)
    
    # Estimate using IV regression
    Model   <- ivreg(fml, data = Estimation_Data)
    
    if(boot == FALSE){
      # For LP, Newey-West HAC standard errors are required due to serial correlation
      nw_vcov    <- NeweyWest(Model, lag = h + 1, prewhite = FALSE)
      coef_tests <- coeftest(Model, vcov = nw_vcov)
      
      coef_est <- coef_tests[, "Estimate"]
      se_est   <- coef_tests[, "Std. Error"]
      
      # Calculate confidence intervals manually using the normal distribution
      z_val1 <- qnorm(1 - conf1/2) 
      z_val2 <- qnorm(1 - conf2/2) 
      
      ci_lower1 = as.matrix(coef_est - z_val1 * se_est)
      ci_upper1 = as.matrix(coef_est + z_val1 * se_est)
      ci_lower2 = as.matrix(coef_est - z_val2 * se_est)
      ci_upper2 = as.matrix(coef_est + z_val2 * se_est)
      
    } else {
      # block bootstrap adapted for IV
      original_coefs <- coef(Model)
      
      # Function estimating IV coefficients
      regression_fn <- function(data, indices) {
        d <- data[indices, ]
        fit <- ivreg(fml, data = d) # Updated to use ivreg
        return(coef(fit))
      }
      
      boot_out <- tsboot(
        tseries = Estimation_Data,
        statistic = regression_fn,
        R = 1000,
        l = block_size,
        sim = "fixed" 
      )
      
      boot_out$t <- na.omit(boot_out$t)
      
      ci_lower1 <- as.matrix(apply(boot_out$t, 2, quantile, probs = conf1/2))
      ci_upper1 <- as.matrix(apply(boot_out$t, 2, quantile, probs = 1-conf1/2))
      ci_lower2 <- as.matrix(apply(boot_out$t, 2, quantile, probs = conf2/2))
      ci_upper2 <- as.matrix(apply(boot_out$t, 2, quantile, probs = 1-conf2/2))
    }
    
    # Note: Using coef_est here ensures it pulls from the original model regardless of boot
    IRF    = cbind(ci_lower2, ci_lower1, as.matrix(coef(Model)), ci_upper1, ci_upper2)
    IRF_RESULTS[nn,,,hh] = IRF  
  }
  
  ### Plot the IRF
  # Extract the row corresponding to the endogenous variable (TIB)
  IRF          = t(IRF_RESULTS[nn, which(names(Model$coefficients) == endog_var),,])
  
  jpeg(paste0(n, "_", endog_var, ".jpeg"), width = 1250, height = 1250)
  
  par(
    mar = c(6, 8.5, 4.5, 2),   
    mgp = c(4, 1.5, 0)         
  )
  
  t_vec <- 1:nrow(IRF)
  
  ylim_vals <- c(
    min(IRF[, 1], IRF[, 2], IRF[, 4], IRF[, 5], na.rm = TRUE),
    max(IRF[, 1], IRF[, 2], IRF[, 4], IRF[, 5], na.rm = TRUE)
  )
  
  plot(
    t_vec, IRF[, 3],
    type = "l",
    ylim = ylim_vals,
    main = n,
    ylab = "",
    xlab = "",
    col = color,
    lwd = 3,
    axes = FALSE,
    cex.main = 3,
    font.main = 2
  )
  
  polygon(c(t_vec, rev(t_vec)), c(IRF[, 1], rev(IRF[, 5])), col = scales::alpha(color, 0.2), border = NA)
  polygon(c(t_vec, rev(t_vec)), c(IRF[, 2], rev(IRF[, 4])), col = scales::alpha(color, 0.3), border = NA)
  
  lines(t_vec, IRF[, 3], col = color, lwd = 3)
  lines(t_vec, IRF[, 1], col = color, lty = 3, lwd = 3)
  lines(t_vec, IRF[, 5], col = color, lty = 3, lwd = 3)
  lines(t_vec, IRF[, 2], col = color, lty = 3, lwd = 3)
  lines(t_vec, IRF[, 4], col = color, lty = 3, lwd = 3)
  
  abline(h = 0, col = "azure4", lwd = 2)
  grid(col = "gray", lty = "dotted")
  
  axis(1, cex.axis = 2.7, lwd = 2)
  axis(2, cex.axis = 2.7, lwd = 2, las = 1)
  
  box(lwd = 2)
  
  mtext("Horizons", side = 1, line = 4, cex = 2.9)
  mtext("Percent", side = 2, line = 6.5, cex = 2.9)   
  
  dev.off()
}

