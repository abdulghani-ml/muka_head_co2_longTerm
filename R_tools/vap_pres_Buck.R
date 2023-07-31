vap_pres_Buck <- function(temp,rh) {
  # Visit http://faculty.eas.ualberta.ca/jdwilson/EAS372_13/Vomel_CIRES_satvpformulae.html for details
  # Buck's equation:  e = 6.1121 exp (17.502 T)/(240.97 + T)
  #                     e = vap = vapor pressure (hPa = hecto Pascal) 
  #                     T = temp = temperature at a given height (deg C)
  #                     rh = relative humidity (fraction)
  vap <- 6.1121 * exp((17.502 * temp) / 
                        (240.97 + temp))
  vap_rh <- vap * rh # Only the last variable line in the function would be returned 
}