rm(list=ls())

#load packages I need

library(readxl)
library(tempdisagg)
library(robCompositions)
library(compositions)
library(vars)
library(bsvars)
library(bsvarSIGNs)
library(dplyr)
library(tidyr)
library(ggplot2)



# set wrd
#setwd('D:/Disco C/Repositorios Git/Labor-Market-and-Compositional-Analysis/Compositional Data and Labor MArket/R/Input/')
setwd('C:/Users/ojaulime/OneDrive - Banco de la República/Documents/Research/MP transmission in Colombia/Monetary-Policy-Shocks-and-Central-bank-information-in-Colombia/IVBVAR/Input/')


Data         = read_excel('DATA_Monthly_2008_2025.xlsx')
nSteps       = 36
ident        = 'IV'
Pandemic_priors = FALSE
names_macro  = c('CPI','CPISAR','GDP','TIB')
names_exo    = c('EXC','WTI')
names_exo    = NULL
names_VAR    = c(names_exo,names_macro)

policy_var_name = "TIB"
instrument_name = "Shockbloo"

#Convert it to ta series
Year    = year(Data$Fecha[1])
ts_Data <-ts(cbind(Data[,names_exo],Data[,names_macro]), start = Year, frequency = 12)
colnames(ts_Data) = c(names_VAR)

Cov_per         = 0L
if(Pandemic_priors) Cov_per         = 9L


#source('D:/Disco C/Repositorios Git/Labor-Market-and-Compositional-Analysis/Compositional Data and Labor MArket/R/Programs/BVAR_Pandemic_Cholesky_ts.r')
source('C:/Users/ojaulime/OneDrive - Banco de la República/Documents/Research/MP transmission in Colombia/Monetary-Policy-Shocks-and-Central-bank-information-in-Colombia/IVBVAR/Programs/BVAR_Pandemic_Cholesky_ts.r')


# ------------------------------------------------------------
# 11. Custom IRF plot of the ILR
# ------------------------------------------------------------


selected_shock <- "TIB"


# Choose response variables to display
selected_responses <- c('TIB','TIB2',"GDP", "CPI",'CPISAR','CDS','EXC')
#selected_responses <- c("GDP", "CPI",'TIB','WAGE')


plot_data <- irf_summary |>
  filter(
    shock == selected_shock,
    response %in% selected_responses
  )

ggplot(plot_data, aes(x = horizon)) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.3
  ) +
  geom_ribbon(
    aes(ymin = lower90, ymax = upper90),
    alpha = 0.20
  ) +
  geom_ribbon(
    aes(ymin = lower68, ymax = upper68),
    alpha = 0.35
  ) +
  geom_line(
    aes(y = median),
    linewidth = 0.8
  ) +
  facet_wrap(
    ~ response,
    scales = "free_y"
  ) +
  labs(
    title = paste("Impulse responses to a", selected_shock, "shock"),
    subtitle = "Posterior median with 68% credible bands",
    x = "Horizon",
    y = "Response"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )


