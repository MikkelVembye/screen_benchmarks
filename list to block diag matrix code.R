# Turning list of matrices to one blog diag matrix

var_list <- list(
  `1` = matrix(c(0.02, 0.03, 0.03, 0.02), nrow = 2, ncol = 2), 
  `2` = matrix(c(0.03, 0.04, 0.04, 0.03), nrow = 2, ncol = 2)
)

bdiag(var_list) |> as.matrix()
 