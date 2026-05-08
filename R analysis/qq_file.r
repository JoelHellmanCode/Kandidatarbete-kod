library(extRemes)
qq_plot <- function(mdl) {


    par <- mdl$results$par

    # hämtar info om mu och sigma har trend, "1" om inte trend, "GMST" om trend
    location_trend <- mdl$par.models$location[[2]]
    scale_trend <- mdl$par.models$scale[[2]]


    # om GMST finns i location/scale_trend, gör cov listan till GMST-listan och döp om till COV
    if ("GMST" %in% c(location_trend, scale_trend)) {
        cov <- mdl$cov.data$GMST
        # döper om de som heter GMST till "COV"
        if (location_trend == "GMST") location_trend <- "COV"
        if (scale_trend == "GMST") scale_trend <- "COV"
        
    } else {
        cov <- mdl$cov.data$COV
    }

    # hämtar regnintensitet samt GMST lista
    data <- mdl$cov.data$rain

    # Hämta formparameter
    xi_hat <- par["shape"]


    # Hämtar listor på parametervärden beroende på vilka parametrar som har trend
    if (location_trend == "COV" && scale_trend == "COV") {
        mu0  <- par["mu0"]
        mu1  <- par["mu1"]
        mu_hat    <- mu0 + mu1 * cov

        phi0 <- par["phi0"]
        phi1 <- par["phi1"]
        sigma_hat <- exp(phi0 + phi1 * cov) 

        trend = "Trend i μ och σ"
    }
    
    else if (location_trend == "COV") { 
        mu0  <- par["mu0"]
        mu1  <- par["mu1"]
        mu_hat    <- mu0 + mu1 * cov
        
        phi0 <- par["log.scale"]
        sigma_hat <- rep(exp(phi0), length(data))  

        trend = "Trend i μ"
        }

    else if (scale_trend == "COV") {
        mu0  <- par["location"]
        mu_hat    <- rep(mu0, length(data))
        
        phi0 <- par["phi0"]
        phi1 <- par["phi1"]
        sigma_hat <- exp(phi0 + phi1 * cov)

        trend = "Trend i σ"
        }

    else {
        mu0  <- par["location"]
        mu_hat    <- rep(mu0, length(data))
        
        phi0 <- par["log.scale"]
        sigma_hat <- rep(exp(phi0), length(data))

        trend = "Stationär"
        }

  # Beräkna empiriska kvantiler
    x_hat <- (1/xi_hat) * log(1 + xi_hat * (data - mu_hat) / sigma_hat)
    x_hat_sort <- sort(x_hat)

  # Beräkna teoretiska Gumbel-kvantiler
    m <- length(x_hat_sort)
    i <- 1:m
    Q <- -log(-log(i / (m + 1)))

    # Plotta
    plot(Q, x_hat_sort,
         xlab = "Teoretiska kvantiler",
         ylab = "Empiriska residualer",
         main = paste("QQ-plot\n", trend)
    )
    abline(0, 1, col = "red")
}