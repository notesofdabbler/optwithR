library(highs)

# Minimize:
# x_0 + x_1 + 3
# Subject to:
# x_1 <= 7
# 5 <= x_0 + 2x_1 <= 15
# 6 <= 3x_0 + 2x_1
# 0 <= x_0 <= 4
# 1 <= x_1

A <- rbind(c(0, 1), c(1, 2), c(3, 2))

s <- highs_solve(L = c(1.0, 1), lower = c(0, 1), upper = c(4, Inf),
                 A = A, lhs = c(-Inf, 5, 6), rhs = c(7, 15, Inf),
                 offset = 3)

s[["objective_value"]]
s[["primal_solution"]]

# Minimize:
# -x_2 - 3x_3 + (1/2) * (2 x_1^2 - 2 x_1x_3 + 0.2 x_2^2 + 2 x_3^2)
# Subject to:
# x_1 + x_3 <= 2
# 0 <= x
L <- c(0, -1, -3)
Q <- rbind(c(2, 0.0, -1), c(0, 0.2, 0), c(-1, 0.0, 2))
A <- cbind(1, 0, 1)
s <- highs_solve(Q = Q, L = L, lower = 0, A = A, rhs = 2)
s[["objective_value"]]
s[["primal_solution"]]
