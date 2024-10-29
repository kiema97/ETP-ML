#@param cd : un data frame contenant les prédictors (tx,tn,rh,rs et ws)
#@parm lat : la latitude de la station de mesure
#@ parm z : l'altitude de la station de mesure
PET.PM <- function(cd, lat, z, albedo = 0.23, rs_rso_min = 0.33, rs_rso_max = 1, checkTdiff = F) {
  Gsc <- 0.0820
  phi <- lat*pi/180
  patm <- 101.3*(((293-0.0065*z)/293)^5.26)
  gamma <- 0.000665*patm
  
  sigma <- 4.903e-9
  
  p <- data.frame(matrix(nrow = length(cd$Date), ncol = 0))
  p$tdiff <- cd$tx - cd$tn
  p$tm <- 0.5*(cd$tx + cd$tn)
  p$year <- as.numeric(format(cd$Date, "%Y"))
  p$J <- as.numeric(format(cd$Date, "%j"))
  p$eT <- 0.6108*exp((17.27*p$tm)/(p$tm + 237.3))
  p$ssv <- (4098*p$eT)/((p$tm + 237.3)^2)
  p$DT <- p$ssv/(p$ssv + gamma*(1+0.34*cd$ws))
  p$PT <- gamma/(p$ssv + gamma*(1+0.34*cd$ws))
  p$TT <- (900/(p$tm + 273.16))*cd$ws
  p$eTn <- 0.6108*exp((17.27*cd$tn)/(cd$tn + 237.3))
  p$eTx <- 0.6108*exp((17.27*cd$tx)/(cd$tx + 237.3))
  p$es <- 0.5* (p$eTn + p$eTx)
  p$ea <- (cd$rh/100)*p$es
  p$dr <- 1 + 0.033*cos((2*pi*p$J)/365)
  p$ds <- 0.409*sin(((2*pi*p$J)/365)-1.39)
  p$ws <- acos(-tan(phi)*tan(p$ds))
  p$Ra <- (24*60/pi)*Gsc*p$dr*((p$ws*sin(phi)*sin(p$ds))+(cos(phi)*cos(p$ds)*sin(p$ws)))
  p$Rs0 <- (0.75+0.00002*z)*p$Ra
  p$Rns <- (1-albedo)*cd$rs
  p$Rs_Rs0 <- cd$rs/p$Rs0
  p$Rs_Rs0 <- sapply(p$Rs_Rs0, function(x) max(min(x, rs_rso_max), rs_rso_min))
  p$Rnl <- sigma * ((((cd$tx+273.16)^4)+((cd$tn+273.16)^4))/2)*(0.34-0.14*sqrt(p$ea))*((1.35*p$Rs_Rs0)-0.35)
  p$Rn <- p$Rns - p$Rnl
  p$Rng <- 0.408 * p$Rn
  p$etrad <- p$DT * p$Rng
  p$etws <- p$PT * p$TT * (p$es - p$ea)
  p$PET <- (p$etrad + p$etws)
  if (checkTdiff) p$PET <- with(p, ifelse(p$tdiff < 0, NA, p$PET))
  return (p)
}


##

PET.PM2 <- function(formula, data, lat, z, albedo = 0.23, rs_rso_min = 0.33, rs_rso_max = 1, checkTdiff = FALSE) {
  terms <- terms(formula)
  response <- as.character(attr(terms, "variables"))[2]
  predictors <- attr(terms, "term.labels")
  
  cd <- data[, c("Date", predictors,response)]
  
  Gsc <- 0.0820
  phi <- lat * pi / 180
  patm <- 101.3 * (((293 - 0.0065 * z) / 293) ^ 5.26)
  gamma <- 0.000665 * patm
  
  sigma <- 4.903e-9
  
  p <- data.frame(matrix(nrow = nrow(cd), ncol = 0))
  p$tdiff <- cd$tx - cd$tn
  p$tm <- 0.5 * (cd$tx + cd$tn)
  p$year <- as.numeric(format(data$Date, "%Y"))
  p$J <- as.numeric(format(data$Date, "%j"))
  p$eT <- 0.6108 * exp((17.27 * p$tm) / (p$tm + 237.3))
  p$ssv <- (4098 * p$eT) / ((p$tm + 237.3) ^ 2)
  p$DT <- p$ssv / (p$ssv + gamma * (1 + 0.34 * cd$ws))
  p$PT <- gamma / (p$ssv + gamma * (1 + 0.34 * cd$ws))
  p$TT <- (900 / (p$tm + 273.16)) * cd$ws
  p$eTn <- 0.6108 * exp((17.27 * cd$tn) / (cd$tn + 237.3))
  p$eTx <- 0.6108 * exp((17.27 * cd$tx) / (cd$tx + 237.3))
  p$es <- 0.5 * (p$eTn + p$eTx)
  p$ea <- (cd$rh / 100) * p$es
  p$dr <- 1 + 0.033 * cos((2 * pi * p$J) / 365)
  p$ds <- 0.409 * sin(((2 * pi * p$J) / 365) - 1.39)
  p$ws <- acos(-tan(phi) * tan(p$ds))
  p$Ra <- (24 * 60 / pi) * Gsc * p$dr * ((p$ws * sin(phi) * sin(p$ds)) + (cos(phi) * cos(p$ds) * sin(p$ws)))
  p$Rs0 <- (0.75 + 0.00002 * z) * p$Ra
  p$Rns <- (1 - albedo) * cd$rs
  p$Rs_Rs0 <- cd$rs / p$Rs0
  p$Rs_Rs0 <- sapply(p$Rs_Rs0, function(x) max(min(x, rs_rso_max), rs_rso_min))
  p$Rnl <- sigma * ((((cd$tx + 273.16) ^ 4) + ((cd$tn + 273.16) ^ 4)) / 2) * (0.34 - 0.14 * sqrt(p$ea)) * ((1.35 * p$Rs_Rs0) - 0.35)
  p$Rn <- p$Rns - p$Rnl
  p$Rng <- 0.408 * p$Rn
  p$etrad <- p$DT * p$Rng
  p$etws <- p$PT * p$TT * (p$es - p$ea)
  p$PET <- p$etrad + p$etws
  if (checkTdiff) p$PET <- with(p, ifelse(p$tdiff < 0, NA, p$PET))
  return(p)
}
