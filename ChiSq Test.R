#Chi-Sq test from raw counts vector or table
#Sam Sadow

#Actually don't really need these
#install.packages("tidyr")
#library(tidyr)

numCol <- 2 #Number of columns
data <- c(180, 90,
            180, 150) #Data entered across rows

full_chi(numCol, data)

expected
chi_sq_stat
g_sq_stat
fisher
df
p_val

full_chi <- function(c, counts) {
r <- length(counts) / c #r = Number of rows

#Optional
#obs <- read.csv("observed_counts.csv")

counts <- matrix(counts, byrow = TRUE, ncol = c)
counts

R <- numeric(0)
for(i in 1:r){
  Rt = 0
  for(j in 1:c){
    Rt = Rt + counts[i,j]
  }
  R <-c(R,Rt)
}; R

Tot <- sum(R)
#counts <- counts %>% unite(R); counts

C <- numeric(0)
for(i in 1:c){
  Ct = 0
  for(j in 1:r){
    Ct = Ct + counts[j,i]
  }
  C <-c(C,Ct)
}; C

exp <- numeric(length(counts)) #Data entered across rows
#r <- length(counts) / c
exp <- matrix(exp, byrow = TRUE, ncol = c)

observed <- as.table(counts)

for(i in 1:r){
  for(j in 1:c){
    exp[i,j] = R[i]*C[j]/Tot
  }
}; exp

expected <- as.table(exp)
#?fisher.test

chi_sq_stat <- sum((observed - expected)^2 / expected); chi_sq_stat
g_sq_stat <- 2* sum(observed * log(observed / expected)); g_sq_stat
fisher <- fisher.test(counts, alternative = "greater"); fisher
df <- (r-1)*(c-1);df

p_val <- pchisq(chi_sq_stat, df, lower.tail = F)
}
