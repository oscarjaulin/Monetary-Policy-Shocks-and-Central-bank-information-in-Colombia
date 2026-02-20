########################
### Compute the IRFs for the variable of interest.

IRF_RESULTS   <- array(NA, dim = c(length(Var_names),(length(Var_Cont)*p)+2,5,length(hh1)))

setwd('C:/Users/ojaulime/OneDrive - Banco de la República/Documents/Research/MP transmission in Colombia/Monetary-Policy-Shocks-and-Central-bank-information-in-Colombia/R/Output/')
for(n in Var_names){
  message(paste0("Estimating: ",n))
  for(h in hh1){
    nn        = which(Var_names==n)
    hh        = which(hh1==h)
    Yt        <- create_leads_df(Data_Anal, vars = n, h = h, all_leads = FALSE) 
    Yt_lag    <- create_lagged_df(Data_Anal, vars = n, h = 1)
    # long difference
    Yt[,2]    <-as.numeric(Yt[,2])-as.numeric(Yt_lag[,2])
    
    has_c <- p != 0
    
    if (has_c||!is.null(Var_Cont)) {
      lagged_c <- if (any(has_c)) create_lagged_df(dataset = Data_Anal, vars = Var_Cont,h = p) else NULL
      
      # Build the list in the order that preserves your original "all.x = TRUE" behavior:
      # if both exist, left-join shock onto c; if only one exists, just use that one.
      pieces <- Filter(Negate(is.null), list(lagged_c))
      
      lagged_vars <- if (length(pieces) == 1) {
        pieces[[1]]
      } else {
        Reduce(function(x, y) merge(x, y, by = "Date", all.x = TRUE), pieces)
      }
      
      temp      <- merge(Data_Anal[,c('Date',shock_anal)],lagged_vars, by = "Date",
                         all.x = TRUE)
    } else {
      #Shock of analysis
      temp      <- Data_Anal[,c('Date',shock_anal)]
    }  
    
    
    #Merge the data of interest (If I want to include lagged shocks or variables of interest, I must include them)
    temp      <- merge(Yt,temp,by='Date',all.x=TRUE)
    
    Estimation_Data = as.data.frame(temp[complete.cases(temp),-c(1)])
    
    Estimation_Data = convert_to_numeric(Estimation_Data)
    
    # Create the formula
    if(p!=0){
      fml <- as.formula(paste(colnames(Yt[,-c(1),drop=FALSE]), "~", paste(c(shock_anal,colnames(lagged_vars[,-c(1),drop=FALSE])), collapse = " + ")))
    } else {
      fml <- as.formula(paste(colnames(Yt[,-c(1),drop=FALSE]), "~", shock_anal))
    }
    
    Model   <- lm(fml, data=Estimation_Data)
    if(boot == FALSE){
      ci1 <- confint(Model,level=1-conf1)
      ci_lower1 = ci1[,1,drop=FALSE]
      ci_upper1 = ci1[,2,drop=FALSE]
      ci2 <- confint(Model,level=1-conf2)
      ci_lower2 = ci2[,1,drop=FALSE]
      ci_upper2 = ci2[,2,drop=FALSE]
    } else {
      
      # block bootstrap
      
      # install.packages("bootUR")
      
      original_coefs <- coef(Model)
      
      # Función que estima los coeficientes
      regression_fn <- function(data, indices) {
        d <- data[indices, ]
        fit <- lm(fml, data = d)
        return(coef(fit))
      }
      
      # Ejecutar block bootstrap
      boot_out <- tsboot(
        tseries = Estimation_Data,
        statistic = regression_fn,
        R = 1000,
        l = block_size,
        sim = "fixed"  # o "geom" si quieres bootstrap geométrico
      )
      
      boot_out$t <- na.omit(boot_out$t)
      
      # Calcular p-values bootstrap (proporción de veces que coef ≤ 0)
      boot_pvals <- apply(boot_out$t, 2, function(x) 2 * min(mean(x <= 0), mean(x >= 0)))
      
      # Calcular percentiles para ICs
      ci_lower1 <- as.matrix(apply(boot_out$t, 2, quantile, probs = conf1/2))
      ci_upper1 <- as.matrix(apply(boot_out$t, 2, quantile, probs = 1-conf1/2))
      # Calcular percentiles para ICs
      ci_lower2 <- as.matrix(apply(boot_out$t, 2, quantile, probs = conf2/2))
      ci_upper2 <- as.matrix(apply(boot_out$t, 2, quantile, probs = 1-conf2/2))
    }
    IRF    = cbind(ci_lower2,ci_lower1,as.matrix(coef(Model)),ci_upper1,ci_upper2)
    IRF_RESULTS[nn,,,hh] = IRF  
  }
  
  ### Plot the IRF
  
  IRF          = t(IRF_RESULTS[nn,which(names(Model$coefficients)==shock_anal),,])
  
  jpeg(paste0(n,'_',shock_anal, ".jpeg"), width = 1250, height = 1250)
  plot.ts(IRF[,3], 
          ylim = c(min(IRF[,1],IRF[,2],IRF[,4],IRF[,5]), max(IRF[,1],IRF[,2],IRF[,4],IRF[,5])), 
          main = n, 
          ylab = 'Percent', 
          xlab = 'Horizons', 
          col = color,  # Color de la serie principal
          lwd = 2,       # Grosor de la serie principal   
          cex.main = 2.5,   # Título más grande
          cex.lab = 2.2,    # Etiquetas de ejes más grandes
          cex.axis = 1.8,   # Números de ejes más grandes
          font.main = 2     # Negrita para el título
  )  
  
  
  # Graficar las líneas de IR_Ch_Low y IR_Ch_sup
  lines(IRF[,1], col = color, lty = 3, lwd = 3)  # Línea punteada, más gruesa
  lines(IRF[,5], col = color, lty = 3, lwd = 3)  # Línea punteada, más gruesa
  
  lines(IRF[,2], col = color, lty = 3, lwd = 3)  # Línea punteada y roja, más gruesa
  lines(IRF[,4], col = color, lty = 3, lwd = 3)  # Línea punteada y roja, más gruesa
  
  # Agregar sombra entre las curvas superior e inferior
  t <- 1:nrow(IRF)
  polygon(c(t, rev(t)), c(IRF[, 1], rev(IRF[, 5])), col = alpha(color, 0.2), border = NA)
  polygon(c(t, rev(t)), c(IRF[, 2], rev(IRF[, 4])), col = alpha(color, 0.3), border = NA)
  # Agregar línea horizontal en y=0
  abline(h = 0, col = 'azure4', lwd = 2)
  
  # Agregar rejillas (grids)
  grid(col = "gray", lty = "dotted")
  
  dev.off()
}