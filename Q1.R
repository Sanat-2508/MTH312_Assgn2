#QUESTION 1

dat <- read.csv("all_data.csv")


d1 = as.matrix(dat[, 1:250])
d2 = as.matrix(dat[, 251:500])



sample1 = d1
sample2 = d2


n_randpoints = 10000
n_dim1 = dim(d1)[2]
n_dim2 = dim(d2)[2]

set.seed(3456)
library(empichar)
rand_points <- matrix(runif(n_randpoints*(n_dim1 + n_dim2),min = min(d1, d2), max = max(d1, d2)),nrow= n_randpoints, ncol= n_dim1 + n_dim2)



Ecdf_1 <- ecf(t= rand_points[, 1:n_dim1], smp=d1)

Ecdf_2 <- ecf(t= rand_points[, (1+n_dim1):(n_dim1+n_dim2)], smp=d2)

Ecdf_joint <- ecf(t = rand_points, smp = cbind(d1, d2))

product_of_marginal <- Ecdf_1*Ecdf_2



ks_result <- ks.test(as.numeric(Ecdf_joint) , as.numeric(product_of_marginal))


print(ks_result$p.value)

if(ks_result$p.value < 0.01){
  cat("Data is Not Indpendent\n")
}else{
  cat("Data is Indpendent\n")
}


