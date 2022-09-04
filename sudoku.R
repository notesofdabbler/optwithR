
#
# x(r, c, k) = 1 if cell(r, c) has value k
# number of variables = 9 * 9 * 9 = 729
#

nvars = 9 * 9 * 9

# variables to cells
idx = as.list(rep(NA, nvars))
i = 0
for (r in 1:9) {
  for (c in 1:9) {
    for (k in 1:9) {
      i = i + 1
      idx[[i]] = c(r, c, k)
    }
  }
}

# diag sets
rset = floor(seq(0, 8) / 3) + 1
cset = floor(seq(0, 8) / 3) + 1

conlist = list()
con = 0

lhs = c()
rhs = c()

# every cell (r, c) has exactly one of 1..9
# sum(k, x(r, c, k))=1

for(r in 1:9) {
  for (c in 1:9) {
    con = con + 1
    conlist[[con]] = rep(0, nvars)
    for (i in 1:nvars) {
      if ((idx[[i]][1] == r) & (idx[[i]][2] == c)) {
        conlist[[con]][i] = 1
      }
    }
    lhs = c(lhs, 1)
    rhs = c(rhs, 1)
    
  }
}

# for each column c, each value k appears in only one row r
# sum(r, x(r, c, k))=1

for(c in 1:9) {
  for (k in 1:9) {
    con = con + 1
    conlist[[con]] = rep(0, nvars)
    for (i in 1:nvars) {
      if ((idx[[i]][2] == c) & (idx[[i]][3] == k)) {
        conlist[[con]][i] = 1
      }
    }
    lhs = c(lhs, 1)
    rhs = c(rhs, 1)
    
  }
}

# for each row r, each value k appears in only one column c
# sum(c, x(r, c, k))=1

for(r in 1:9) {
  for (k in 1:9) {
    con = con + 1
    conlist[[con]] = rep(0, nvars)
    for (i in 1:nvars) {
      if ((idx[[i]][1] == r) & (idx[[i]][3] == k)) {
        conlist[[con]][i] = 1
      }
    }
    lhs = c(lhs, 1)
    rhs = c(rhs, 1)
    
  }
}

# for each 3x3 block (given rset, cset), each value of k appears only once
# sum((r in rset, c in cset), x(r, c, k)) = 1
for (k in 1:9) {
  for (rs in 1:3) {
    for (cs in 1:3) {
      con = con + 1
      conlist[[con]] = rep(0, nvars)
      for (i in 1:nvars) {
            r = idx[[i]][1]
            c = idx[[i]][2]
            if ((rset[r] == rs) & (cset[c] == cs)) {
              conlist[[con]][i] = 1  
            }
      }
      lhs = c(lhs, 1)
      rhs = c(rhs, 1)
    }
  }
}


