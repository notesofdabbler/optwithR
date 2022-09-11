#
#  Using ompr with highs
#

library(highs)
library(ompr)
library(ompr.highs)
library(dplyr)


# Minimize:
# x_0 + x_1 + 3
# Subject to:
# x_1 <= 7
# 5 <= x_0 + 2x_1 <= 15
# 6 <= 3x_0 + 2x_1
# 0 <= x_0 <= 4
# 1 <= x_1

# OMPR model
mdl = MIPModel() %>%
      add_variable(x0, lb = 0, ub = 4, type = "continuous") %>%
      add_variable(x1, lb = 1, type = "continuous") %>%
      set_objective(x0+x1+3, sense = "min") %>%
      add_constraint(x1 <= 7) %>%
      add_constraint(x0 + 2*x1 <= 15) %>%
      add_constraint(x0 + 2*x1 >= 5) %>%
      add_constraint(3*x0 + 2*x1 >= 6)

# solve model
s = mdl %>% solve_model(highs_optimizer())

# get solution
s$status
s$objective_value
s$solution

