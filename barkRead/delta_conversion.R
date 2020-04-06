# 2H/H concentration in ppm of VSMOW
VSMOW_2R <- 156
# d2H of mineral water Monte Santiago
d2H_dil_Spain <- -43
# d2H of tap water Rosinedal
d2H_dil_Sweden <- -90
# calculate ratio (in ppm) from delta (dx) in permil
calc_ratio_ppm <- function(dx, Rstd){
  Rsample <- (dx*0.001 + 1)*Rstd
  return(Rsample)
}
# calculate dx (in permil) from ratio in ppm
calc_dx_permil <- function(Rsample, Rstd){
  dx <- ((Rsample/Rstd) - 1)*1000
  return(dx)
}
# mixing deuterated water (99.9% 2H2O) in a 8:1000 vol mix
# Monte Santiago: 8 mL deut. water in 992 mL mineral water
# Rosinedal: 2 mL deut. water in 248 mL tap water
ppm_2H_label_Spain <- (calc_ratio_ppm(d2H_dil_Spain, VSMOW_2R)*992 + 99.9*0.01*2*1e06*8)/(992 + 8)
d2H_label_Spain <- calc_dx_permil(ppm_2H_label_Spain, VSMOW_2R)
ppm_2H_label_Sweden <- (calc_ratio_ppm(d2H_dil_Sweden, VSMOW_2R)*248 + 99.9*0.01*2*1e06*2)/(248 + 2)
d2H_label_Sweden <- calc_dx_permil(ppm_2H_label_Sweden, VSMOW_2R)
