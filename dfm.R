# FED code does not work for different frequencies.
# Maybe forget all parameters adjusting for quarter series.

`%>%` <- dplyr::`%>%`

#define 
R_MAT_COLS = 5

x <- XZ$X[, -1]
J = nrow(x)
N = ncol(x)

# Not sure if this list is going to be needed
Par = 
list( "blocks" = spec@blocks
    ,     "nQ" = sum(spec@fields$Frequency == "q")
    ,     "nM" = sum(spec@fields$Frequency == "m")
    ,     "pc" = max(1, R_MAT_COLS)
    ,      "p" = 1
    ,      "q" = matrix(0, 4, 1)
)

R_mat = c(2, -1,  0,  0,  0,
          3,  0, -1,  0,  0,
          2,  0,  0, -1,  0,
          1,  0,  0,  0, -1) %>% 
matrix(ncol = R_MAT_COLS, byrow = TRUE)

i_idio = rbind(matrix(1, N - Par$nQ, 1), matrix(0, Par$nQ, 1))

max_iter = 5e3

xNaN <- dplyr::mutate_all(x, ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE))

#           [A, C, Q, R, Z_0, V_0] = InitCond(xNaN,r,p,blocks,R_mat,q,nQ,i_idio);
# function [ A, C, Q, R, Z_0, V_0] = InitCond(x,r,p,blocks,Rcon,q,nQ,i_idio)


xbal <- remNaNs_spline(xNaN, k = 3, method = 2)
J = nrow(xbal$X)
N = ncol(xbal$X)

resnan <- xNaN
res <-xbal$X

ri = Par$p
Ri = kronecker(R_mat, diag(ri))
qi = kronecker(Par$q, matrix(0, ri, 1))
ci = matrix(0, N, ri * Par$pc) 

# for i in 1:ncol(spec@blocks)
i = 1

idx_i  = which(spec@blocks[, i] != 0) 
idx_iM = idx_i[idx_i < Par$nM + 1]
idx_iQ = idx_i[idx_i > Par$nM]

vd <- eigen(cov(res[, idx_iM]))
d  <- vd$values[1]
v  <- vd$vectors[, 1]

ci[idx_iM, 1:ri] = v

f = as.matrix(res[, idx_iM]) %*% v
Fa <- NULL
for (k in 0:(max(Par$pc, Par$p + 1) - 1)) {
    interval <- seq(from = Par$pc - k, to = nrow(f) - k)
	Fa <- cbind(Fa, f[interval])
}

ff = Fa
xj <- resnan[(Par$pc + 1):nrow(resnan), idx_iQ]

ca <- 
lapply(colnames(xj), function(.x) {
                       interval <- seq(Par$pc, nrow(res), 1)

                       y <- dplyr::pull(xj, .x)
                       if (sum(is.na(y)) < ncol(ff) + 2) {
                          y <- dplyr::pull(res[interval, ], .x)
                       } else {
                          y <- dplyr::pull(xj, .x)
                       }

                       fj = ff[!is.na(y), ]
                       yj = y[!is.na(y)]
                       
                       ifj = solve(t(fj) %*% fj)
                       cc  = ifj %*% (t(fj) %*% yj)

                       a = (Ri %*% cc - qi)
                       b = solve(Ri %*% ifj %*% t(Ri))

                       cc = cc - ifj %*% t(Ri) %*% b %*% a
                       as.numeric(cc)
}) %>%
 setNames(colnames(xj)) %>%
 dplyr::bind_cols()

