lp  <- OP(objective = L_objective(c(7, 8), names=c("x", "y")),
          constraints = L_constraint(L = rbind(c(3, 4), c(2, 1)), 
                                     dir = c("==", ">="), rhs = c(9, 3)),
          bounds = V_bound(li = 1:2, ui = 1:2, 
                           lb = c(-100, -100), ub = c(100, 100)))
ROI_applicable_solvers(lp)
sol <- ROI_solve(lp, solver = "lpsolve")

library(ompr)
library(ompr.roi)

result <- MIPModel() |>
  add_variable(x, type = "integer") |>
  add_variable(y, type = "continuous", lb = 0) |>
  set_bounds(x, lb = 0) |>
  set_objective(x + y, "max") |>
  add_constraint(x + y <= 11.25) |>
  solve_model(with_ROI(solver = ""))
get_solution(result, x)

ompr_mdl <- MIPModel() %>%
  add_variable(x, type = "integer") %>%
  add_variable(y, type = "continuous", lb = 0) %>%
  set_bounds(x, lb = 0) %>%
  set_objective(x + y, "max") %>% 
  add_constraint(x + y <= 11.25) %>%
  add_constraint(2*x <= 3*y) %>%
  add_constraint(3*x >= 2) %>%
  add_constraint(2.5*x+3.25*y==10)

model = ompr_mdl

as_highs_model = function(model) {

    # adapted from
    # https://github.com/dirkschumacher/ompr.roi/blob/master/R/as-roi-model.R
    #
    # define the order of all variables
    column_types <- ompr::variable_types(model)
    ncols <- length(column_types)
    
    # build column bounds
    bounds <- ompr::variable_bounds(model)
    column_bounds_l <- bounds$lower
    column_bounds_u <- bounds$upper
    
    # build objective coeffcient vector
    objective <- model$objective
    has_objective <- !is.null(objective)
    obj <- ompr::objective_function(model)
    obj_vector <- obj$solution
    obj_constant <- obj$constant
    
    # build constraint matrix
    constraints <- ompr::extract_constraints(model)
    constraint_matrix <- constraints$matrix
    constraint_rhs <- constraints$rhs
    constraint_dir <- constraints$sense
    
    lhs = rep(-Inf, length(constraint_rhs))
    rhs = rep(Inf, length(constraint_rhs))
    lhs[constraint_dir %in% c(">=", "==")] = constraint_rhs[constraint_dir %in% c(">=", "==")]
    rhs[constraint_dir %in% c("<=", "==")] = constraint_rhs[constraint_dir %in% c("<=", "==")]
    
    
    # convert types to ROI codes
    column_types_chr <- character(length(column_types))
    column_types_chr[column_types == "binary"] <- "B"
    column_types_chr[column_types == "integer"] <- "I"
    column_types_chr[column_types == "continuous"] <- "C"
    
    res = list(L = as.numeric(obj_vector), 
               lower = column_bounds_l,
               upper = column_bounds_u,
               A = constraint_matrix,
               lhs = lhs,
               rhs = rhs,
               types = column_types_chr,
               offset = obj_constant
               )
  
}

tmp = as_highs_model(ompr_mdl)
