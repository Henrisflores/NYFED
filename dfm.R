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
    ,      "r" = matrix(1, 1, ncol(spec@blocks))
)

R_mat = c(2, -1,  0,  0,  0,
          3,  0, -1,  0,  0,
          2,  0,  0, -1,  0,
          1,  0,  0,  0, -1) %>% matrix(ncol = R_MAT_COLS, byrow = TRUE)

q = matrix(0, 4, 1)
i_idio = rbind(matrix(1, N - Par$nQ, 1), matrix(0, Par$nQ, 1))

max_iter = 5e3

xNaN <- dplyr::mutate_all(x, ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE))

# InitCond <- function(xNaN, r, p, blocks, R_mat, q, nQ, i_idio) {}

#           [A, C, Q, R, Z_0, V_0] = InitCond(xNaN,r,p,blocks,R_mat,q,nQ,i_idio);
# function [ A, C, Q, R, Z_0, V_0] = InitCond(x,r,p,blocks,Rcon,q,nQ,i_idio)


xbal <- remNaNs_spline(xNaN, k = 3, method = 2)
J = nrow(xbal$X)
N = ncol(xbal$X)

resnan <- xNaN
res <-xbal$X

# for i in 1:ncol(spec@blocks)
i = 2
ri = Par$r[i]

Ci = matrix(0, N, ri * Par$pc)
idx_i  = which(spec@blocks[, i] != 0) 
idx_iM = idx_i[idx_i < Par$nM + 1]
idx_iQ = idx_i[idx_i > Par$nM]

vd <- eigen(cov(res[, idx_iM]))
d <- vd$values[1]
v <- vd$vectors[, 1]

# Replace Ci[,i] by v
# Ci[idx_iM, 1] = v

f = as.matrix(res[, idx_iM]) %*% v
Fa <- NULL
for (k in 0:(max(Par$pc, Par$p + 1) - 1)) {
	Fa <- cbind(Fa, f[seq(from = Par$pc - k, to = nrow(f) - k)])
}
















