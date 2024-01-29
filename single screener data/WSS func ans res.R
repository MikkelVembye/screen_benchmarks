

WSS <- function(TP, FP, N){
  
  1 - (TP + FP)/N
  
}

# FFT incl_p = 0.5
WSS(TP = 49, FP = 4066-3888, N = 4135)

# FFT incl_p = 0.3
WSS(TP = 56, FP = 4066-3809, N = 4135)

# FFT GPT-4
WSS(TP = 62, FP = 4066-3810, N = 4135)

# FRINDS incl_p = 0.5
WSS(TP = 62, FP = 2511-1930, N = 2575)

# FRINDS incl_p = 0.7
WSS(TP = 63, FP = 2521-2455, N = 2585)
