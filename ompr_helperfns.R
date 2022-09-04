
as_highs_model = function(model) {
  
  # Get parameters needed or highs model from ompr model
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
