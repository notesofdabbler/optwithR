#
# Transportation Problem from GAMS model library
# https://www.gams.com/latest/gamslib_ml/libhtml/gamslib_trnsport.html
#

library(dplyr)
library(highs)
library(ompr)
library(ompr.highs)
library(glue)
library(visNetwork)

library(ompr.roi)

source("ompr_helperfns.R")
source("helper_fns.R")

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

# visualize network
visnetdf = make_visnetdf(distdf, plants, mkts)
visnetdf$nodes_df = visnetdf$nodes_df %>% mutate(level = ifelse(id <= 2, 1, 2),
                                       capdem = c(cap, dem),
                                       label = paste0(node, " (", capdem,")"),
                                       color = c(rep("red", 2), rep("green", 3)))
visNetwork(visnetdf$nodes_df, visnetdf$edges_df) %>% visEdges(arrows = "to") %>%
  visHierarchicalLayout()

cost = function(i, j) {
    p = plants[i]
    m = mkts[j]
    costval = distdf %>% filter(plants == p, mkts == m) %>% pull(cost)
    return(costval)
}

np = length(plants)
nm = length(mkts)

# create ompr model
mdl = MIPModel() %>%
  add_variable(x[i, j], i=1:np, j=1:nm, type = "continuous",lb = 0) %>%
  # objective: min cost
  set_objective(sum_over(cost(i, j) * x[i, j], i = 1:np, j = 1:nm)) %>% 
  # supply from each plant is below capacity
  add_constraint(sum_over(x[i, j], j = 1:nm) <= cap[i], i = 1:np) %>%  
  # supply to each market meets demand
  add_constraint(sum_over(x[i, j], i = 1:np) >= dem[j], j = 1:nm)

# solve model using ompr connection to highs optimizer
sol = mdl %>% solve_model(highs_optimizer())
sol$status
sol$objective_value
sol$solution
soldf = sol %>% get_solution(x[i, j])
soldf = soldf %>% mutate(plants = plants[i], mkts = mkts[j]) %>% 
  rename(qty = value)

# solve by using as_roi_model adaptation to convert to highs model and using highs syntax
# convert ompr model to highs model
highs_mdl = as_highs_model(mdl)

# solve highs model
s <- highs_solve(L = as.numeric(highs_mdl$L), lower = highs_mdl$lower, upper = highs_mdl$upper,
                 A = highs_mdl$A, lhs = highs_mdl$lhs, rhs = highs_mdl$rhs,
                 offset = highs_mdl$offset)

# get solution
s[["status"]]
s[["objective_value"]]
xsol = s[["primal_solution"]]
names(xsol) = variable_keys(mdl)

# get solution into dataframe
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
