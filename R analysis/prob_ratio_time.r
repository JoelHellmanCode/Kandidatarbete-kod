prob_ratio_time <- function(mdl, year_f, year_cf, x_val, extra = FALSE){

    # ladda in datan från modellen GEV-modellen
    df = mdl$cov.data

    # Hitta COV för year_f, 2024 finns inte alltid med i datamängden, så då tar vi COV för det sista året, och adderar mellanskillnaden
    # mellan year_f och datamängdens högsta årtal till COV.
    # detta måste göras då vårat startår har COV = 0, andra året har COV = 1, osv.
    if(year_f %in% df$year){
        cov_f <- df$COV[df$year == year_f]
    }  
    else {
        nearest_year <- max(df$year)
        cov_f <- df$COV[df$year == nearest_year] + (year_f - nearest_year)
    }
    
    # Hitta år för year_cf
    cov_cf <- df$COV[df$year == year_cf]

    # Kolla om 2024 är inkluderat i dataanpassningen
    if (2024 %in% mdl$cov.data$year) {
        inc_2024 <- "Yes"   
    }
    else {
        inc_2024 <- "No"
    }

    # Ta fram parametervärden för år f och cf, samt underliggande parametervärden
    trend = ""
    
    par <- mdl$results$par
    if("mu1" %in% names(par)) {
        mu_f  <- par["mu0"] + par["mu1"] * cov_f
        mu_cf <- par["mu0"] + par["mu1"] * cov_cf
        
        mu0 = par["mu0"]
        mu1 = par["mu1"]

        trend <- paste0(trend, "mu")
        
    } else {
        # Stationär mu
        mu_f = par["location"]
        mu_cf = par["location"]

        mu0 = par["location"]
        mu1 = NA
    }

    
    # Kolla om det finns trend i sigma
    if("phi1" %in% names(par)) {
        # Log-linjär trend i sigma (use.phi = TRUE)
        sigma_f  <- exp(par["phi0"] + par["phi1"] * cov_f)
        sigma_cf <- exp(par["phi0"] + par["phi1"] * cov_cf)

        phi0 = par["phi0"]
        phi1 = par["phi1"]

        trend <- paste(trend, "sigma")
        
    } else {
        sigma_f  <- exp(par["log.scale"])
        sigma_cf <- exp(par["log.scale"])

        phi0 = par["log.scale"]
        phi1 = NA

    }

    # beräknar överskridande sannolikheter för regnet med de givna parametervärdena
    prob_f <- 1 - pevd(x_val, 
                        loc = mu_f, 
                        scale = sigma_f, 
                        shape = par["shape"], 
                        type = "GEV")
    
    
    prob_cf <- 1 - pevd(x_val, 
                        loc = mu_cf, 
                        scale = sigma_cf, 
                        shape = par["shape"], 
                        type = "GEV")

    # Beräknar intensitet på ett regn med samma sannolikhet som 144.6 i 2024 för vårat year_cf
    I0 <- unname(qevd(1-prob_f,
           loc   = mu_cf,
           scale = sigma_cf,
           shape = par["shape"],
           type  = "GEV"))
    
    # Relativ intensitetsförändring
    delta_I <- ((x_val - I0)/I0 ) * 100

    # Probability ratio
    P0 = prob_cf
    P1 = prob_f
    
    PR = P1/P0

    # Gör en lista med alla parametrar för modellen
    sigma0 = sprintf("Sigma0: %s",exp(unname(phi0)))
    shape = par["shape"]
    pars = NA
    if (extra==TRUE){
        pars = list(mu0, mu1, phi0, phi1, shape
            )
        }
    
    resultat = list("type" = sprintf("Dataset: %s, Threshold: %s, Year PR & I: %s, 2024: %s, Trend: %s, Kovariat: %s", mdl$data[2], x_val , year_cf, inc_2024, trend, "tid"),
    "P0" =  round(P0, 7), "P1" =  round(P1, 7), "PR"      = round(PR, 5),  
    "delta_I" = round(delta_I, 5), pars
)
    return(resultat)    
    }