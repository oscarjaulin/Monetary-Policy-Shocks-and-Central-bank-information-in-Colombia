# I want to know the response depending on the high frequency response of CDS or TRM

########################
### Compute the IRFs 

IRF_RESULTS   <- array(NA, dim = c(length(Var_names),((length(Var_names)*p+1))*2+2,5,length(hh1)))

setwd('C:/Users/ojaulime/OneDrive - Banco de la República/Documents/ESPE/Transmisión de política monetaria/Exchange Rate Puzzle/Output/')
for(n in Var_names){
  message(paste0("Estimating: ",n," with ",Hfs," in high frequency"))
  for(h in hh1){
    nn        = which(Var_names==n)
    hh        = which(hh1==h)
    Yt        <- create_leads_df(Data_Anal, vars = n, h = h, all_leads = FALSE) 
    Yt_lag    <- create_lagged_df(Data_Anal, vars = n, h = 1)
    # long difference
    Yt[,2]    <-as.numeric(Yt[,2])-as.numeric(Yt_lag[,2])
    
    has_c <- p != 0
    
    if (has_c) {
      lagged_c <- if (any(has_c)) create_lagged_df(dataset = Data_Anal, vars = Var_names,     h = p) else NULL
      
      # Build the list in the order that preserves your original "all.x = TRUE" behavior:
      # if both exist, left-join shock onto c; if only one exists, just use that one.
      pieces <- Filter(Negate(is.null), list(lagged_c))
      
      lagged_vars <- if (length(pieces) == 1) {
        pieces[[1]]
      } else {
        Reduce(function(x, y) merge(x, y, by = "Date", all.x = TRUE), pieces)
      }
      
      temp      <- merge(Data_Anal[,c('Date',shock_anal,Hfs)],lagged_vars, by = "Date",
                         all.x = TRUE)
    } else {
      #Shock of analysis
      temp      <- Data_Anal[,c('Date',shock_anal,Hfs)]
    }  
    #Merge the data of interest (If I want to include lagged shocks or variables of interest, I must include them)
    temp      <- merge(Yt,temp,by='Date',all.x=TRUE)
    
    Dummy          = as.matrix((temp[, Hfs]>=0)*1)
    temp$Dummy     = as.vector(Dummy)
    
    # Crear las dos dummies
    Dum1 <- ifelse(temp$Dummy == 0, 1, 0)
    Dum2 <- ifelse(temp$Dummy == 1, 1, 0)
    
    # Añadirlas a tu base si no existen
    temp$Dum1 <- Dum1
    temp$Dum2 <- Dum2
    
    Estimation_Data = as.data.frame(temp[complete.cases(temp),-c(1)])
    
    Estimation_Data = convert_to_numeric(Estimation_Data)
    
    # Create the formula
    if(p!=0){
      fml <- as.formula(paste(colnames(Yt[,-c(1),drop=FALSE]), "~", 
                              paste(paste0('Dum1:',c(shock_anal,colnames(lagged_vars[,-c(1)]))), collapse = " + "),
                              "+",
                              paste(paste0('Dum2:',c(shock_anal,colnames(lagged_vars[,-c(1)]))), collapse = " + "),"+ Dum1 "))
    } else {
      fml <- as.formula(paste(colnames(Yt[,-c(1),drop=FALSE]), "~ Dum1 +", 
                            paste(paste0('Dum1:',shock_anal, collapse = " + ")),
                            "+",
                            paste(paste0('Dum2:',shock_anal, collapse = " + "))))
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
  
  # --- Extraer IRF para cada régimen ---
  
  # IRF cuando dum = 0
  var_inter1 <- paste0('Dum1:',shock_anal)
  IRF1 <- t(IRF_RESULTS[nn,which(names(Model$coefficients) == var_inter1), , ])
  
  # IRF cuando dum = 1
  var_inter2 <- paste0(shock_anal,':Dum2')
  IRF2 <- t(IRF_RESULTS[nn,which(names(Model$coefficients) == var_inter2), , ])
  
  # --- Colores ---
  color1 <- "gray40"  # Dum1
  color2 <- "blue4"   # Dum2
  
  # --- Crear gráfico conjunto ---
  jpeg(paste0(n,'_',shock_anal,'HF_',Hfs,".jpeg"), width = 1600, height = 1300)
  
  # Calcular límites comunes para ambos IRF
  ylim_min <- min(IRF1[,1:5], IRF2[,1:5])
  ylim_max <- max(IRF1[,1:5], IRF2[,1:5])
  
  # Gráfico base: IRF1
  plot.ts(
    IRF1[,3],
    ylim = c(ylim_min, ylim_max),
    main = paste0(n, " – Comparativo por régimen"),
    ylab = "Response (percent)",
    xlab = "Horizons",
    col = color1,
    lwd = 3,
    cex.main = 2.5,   # Título más grande
    cex.lab = 3.2,    # Etiquetas de ejes más grandes
    cex.axis = 2.8,   # Números de ejes más grandes
    font.main = 2     # Negrita para el título
  )
  
  # Sombreado y líneas para IRF1 (gris)
  t <- 1:nrow(IRF1)
  polygon(c(t, rev(t)), c(IRF1[,1], rev(IRF1[,5])), col = alpha(color1, 0.2), border = NA)
  polygon(c(t, rev(t)), c(IRF1[,2], rev(IRF1[,4])), col = alpha(color1, 0.3), border = NA)
  lines(IRF1[,3], col = color1, lwd = 3)
  lines(IRF1[,1], col = color1, lty = 3, lwd = 2)
  lines(IRF1[,2], col = color1, lty = 3, lwd = 2)
  lines(IRF1[,4], col = color1, lty = 3, lwd = 2)
  lines(IRF1[,5], col = color1, lty = 3, lwd = 2)
  
  # Añadir IRF2 (azul)
  polygon(c(t, rev(t)), c(IRF2[,1], rev(IRF2[,5])), col = alpha(color2, 0.2), border = NA)
  polygon(c(t, rev(t)), c(IRF2[,2], rev(IRF2[,4])), col = alpha(color2, 0.3), border = NA)
  lines(IRF2[,3], col = color2, lwd = 3)
  lines(IRF2[,1], col = color2, lty = 3, lwd = 2)
  lines(IRF2[,2], col = color2, lty = 3, lwd = 2)
  lines(IRF2[,4], col = color2, lty = 3, lwd = 2)
  lines(IRF2[,5], col = color2, lty = 3, lwd = 2)
  
  # Líneas de referencia
  abline(h = 0, col = "azure4", lwd = 2)
  grid(col = "gray", lty = "dotted", lwd = 1.2)
  
  # Leyenda ampliada y clara
  legend(
    "topright",
    legend = c(paste0('decrease',Hfs), paste0('increase',Hfs)),
    col = c(color1, color2),
    lwd = 3,
    cex = 1.8,        # Tamaño de fuente de la leyenda
    bty = "n",
    text.font = 2
  )
  
  dev.off()
}