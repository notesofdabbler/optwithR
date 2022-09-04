library(highs)
library(ompr)
library(ompr.roi)

source("ompr_helperfns.R")

# Minimize:
# x_0 + x_1 + 3
# Subject to:
# x_1 <= 7
# 5 <= x_0 + 2x_1 <= 15
# 6 <= 3x_0 + 2x_1
# 0 <= x_0 <= 4
# 1 <= x_1

mdl = MIPModel() %>%
      add_variable(x0, lb = 0, ub = 4, type = "continuous") %>%
      add_variable(x1, lb = 1, type = "continuous") %>%
      set_objective(x0+x1+3) %>%
      add_constraint(x1 <= 7) %>%
      add_constraint(x0 + 2*x1 <= 15) %>%
      add_constraint(x0 + 2*x1 >= 5) %>%
      add_constraint(3*x0 + 2*x1 >= 6)

highs_mdl = as_highs_model(mdl)

s <- highs_solve(L = as.numeric(highs_mdl$L), lower = highs_mdl$lower, upper = highs_mdl$upper,
                 A = highs_mdl$A, lhs = highs_mdl$lhs, rhs = highs_mdl$rhs,
                 offset = highs_mdl$offset)

s[["objective_value"]]
s[["primal_solution"]]
