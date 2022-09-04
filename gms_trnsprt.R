
library(dplyr)
library(highs)

plants = c("seattle", "sandiego")
mkts = c("newyork", "chicago", "topeka")

capdf = tibble(plants = c("seattle", "sandiego"), cap = c(350, 600))
demdf = tibble(mkts = c("newyork", "chicago", "topeka"), dem = c(325, 300, 275))

distdf = tibble(
  plants = rep(plants, each = 3),
  mkts = rep(mkts, 2),
  dist = c(2.5, 1.7, 1.8, 2.5, 1.8, 1.4) # distance in thousands of miles
)

f = 90 # freight dollars per case per thousands of miles

distdf = distdf %>% mutate(cost = f * dist / 1000)

cap = setNames(as.list(capdf$cap), capdf$plants)
dem = setNames(as.list(demdf$dem), demdf$mkts)

cost = lapply(plants, function(x) {
                    distdf_loc = distdf %>% filter(plants == x)
                    costloc = distdf_loc %>% pull(cost)
                    names(costloc) = distdf_loc %>% pull(mkts)
                    as.list(costloc)
                    })
names(cost) = plants

nvars = length(plants) * length(mkts)
idx = as.list(rep(0, nvars))
idxr = list()
i = 0
for (p in plants) {
  for (m in mkts) {
    i = i + 1
    idx[[i]] = c(p, m)
    idxr[[p]][[m]] = i
  }
}

# Objective:
# totcost = sum((p, m), c(p, m) * x(p, m))
L = rep(NA, nvars)
for (i in 1:nvars) {
  p = idx[[i]][1]
  m = idx[[i]][2]
  L[i] = cost[[p]][[m]]
}


conlist = list()
c = 0

lhs = c()
rhs = c()

# Supply constraint in each plant: p
# sum(m, x(p, m)) <= cap(p)

for (p in plants) {
  c = c + 1
  conlist[[c]] = rep(0, nvars)
  for (i in 1:nvars) {
    p1 = idx[[i]][1]
    m1 = idx[[i]][2]
    if (p1 == p) {
      conlist[[c]][i] = 1
    }
  }
}

lhs = c(lhs, rep(-Inf, length(plants)))
rhs = c(rhs, as.numeric(cap))

# Demand constraint in each market
# sum(p, x(p, m)) >= dem(m)

for (m in mkts) {
  c = c + 1
  conlist[[c]] = rep(0, nvars)
  for (i in 1:nvars) {
    p1 = idx[[i]][1]
    m1 = idx[[i]][2]
    if (m1 == m) {
      conlist[[c]][i] = 1
    }
  }
}

lhs = c(lhs, as.numeric(dem))
rhs = c(rhs, rep(Inf, length(mkts)))

A = do.call(rbind, conlist)

lower = rep(0, nvars)
upper = rep(Inf, nvars)

s <- highs_solve(L = L, lower = lower, upper = upper,
                 A = A, lhs = lhs, rhs = rhs,
                 offset = 0)

zobj = s[["objective_value"]]
xsol = s[["primal_solution"]]

xlist = list()
for (i in 1:nvars) {
  p = idx[[i]][1]
  m = idx[[i]][2]
  xlist[[i]] = c(p, m, xsol[i])
}
soldf = as.data.frame(do.call(rbind, xlist))
names(soldf) = c("plant", "mkt", "qty")
