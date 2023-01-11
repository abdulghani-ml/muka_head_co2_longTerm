solubility <- function(temp,salinity) {
  # Read "Weiss (1974). Carbon dioxide in water and seawater: the solubility
  # of non-ideal gas. Marine Chemistry, 2(1974), 203-215." for details
  # Weiss's equation: ln K0 = A1 + A2(100/T) + A3 ln(T/100) + 
  #                           S0/00(B1 + B2(T/100) + B3(T/100)2)
  #                   K0 =  solubility at the temperature T and atmosphere fugacity
  #                         in the gas phase [mol/m3 atm]
  #                   T = temperature [deg C] 
  #                   S0/00 = salinity [parts per thousand]
  #                   A1 = -58.0931 [mol/L atm] 0r -60.2409 [mol/kg atm]
  #                   A2 = 90.5069 [mol/L atm] or 93.4517 [mol/kg atm]
  #                   A3 = 22.2940 [mol/L atm] or 23.3585 [mol/kg atm]
  #                   B1 = 0.02776 [mol/L atm] or 0.023517 [mol/kg atm]
  #                   B2 = -0.025888 [mol/L atm] or -0.023656 [mol/kg atm]
  #                   B3 = 0.0050578 [mol/L atm] or 0.0047036 [mol/kg atm]
  
  A1 <- -58.0931
  A2 <- 90.5069
  A3 <- 22.2940
  B1 <- 0.02776
  B2 <- -0.025888
  B3 <- 0.0050578
  
  temp <- temp + 273.15 #convert deg C to Kelvin
  lnK0 <- A1 + (A2 * (100/temp)) + 
    (A3 * log(temp/100)) + 
    (salinity * (B1 + (B2 * (temp/100)) + (B3 * (temp/100)^2)))
  
  K0 <- exp(lnK0)   # Only the last variable line in the function would be returned 
  K0 <- K0 / 0.001  # Convert from [mol/L atm] to [mol/m3 atm]. 1 L = 0.001 m3
}