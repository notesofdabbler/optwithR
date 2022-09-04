
library(dplyr)
library(highs)
library(ompr)
library(ompr.roi)

source("ompr_helperfns.R")


plants = c("seattle", "sandiego")
mkts = c("newyork", "chicago", "topeka")

cap = c(350, 600) # plant capacity
dem = c(325, 300, 275) # market demand

# distance and cost between plants and markets
distdf = tibble(
  plants = rep(plants, each = 3),
  mkts = rep(mkts, 2),
  dist = c(2.5, 1.7, 1.8, 2.5, 1.8, 1.4) # distance in thousands of miles
)

f = 90 # freight dollars per case per thousands of miles

distdf = distdf %>% mutate(cost = f * dist / 1000)

cost = function(i, j) {
    p = plants[i]
    m = mkts[j]
    costval = distdf %>% filter(plants == p, mkts == m) %>% pull(cost)
    return(costval)
}

np = length(plants)
nm = length(mkts)

mdl = MIPModel() %>%
  add_variable(x[i, j], i=1:np, j=1:nm, type = "continuous",lb = 0) %>%
  set_objective(sum_over(cost(i, j) * x[i, j], i = 1:np, j = 1:nm)) %>%
  add_constraint(sum_over(x[i, j], j = 1:nm) <= cap[i], i = 1:np) %>%
  add_constraint(sum_over(x[i, j], i = 1:np) >= dem[j], j = 1:nm)

highs_mdl = as_highs_model(mdl)

s <- highs_solve(L = as.numeric(highs_mdl$L), lower = highs_mdl$lower, upper = highs_mdl$upper,
                 A = highs_mdl$A, lhs = highs_mdl$lhs, rhs = highs_mdl$rhs,
                 offset = highs_mdl$offset)

sol_status = s[["status"]]
zobj = s[["objective_value"]]
xsol = s[["primal_solution"]]
names(xsol) = variable_keys(mdl)

xsolL = list()
r = 0
for (i in 1:np) {
  for (j in 1:nm) {
    r = r + 1
    xsolL[[r]] = c(plants[i], mkts[j], xsol[[glue('x[{i},{j}]')]])
  }
}
soldf = as.data.frame(do.call(rbind, xsolL))
names(soldf) = c("plants", "mkts", "qty")
