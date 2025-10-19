# The birthday problem: Take N students in a room. We want to find out if
# at least 2 of them have the same birthday.
# We will solve as such: P(at least two)= 1 - P(none share a birthday)

# Create sample and compare birthday lists-------

n = 20
bdays = sample(1:365, size = n, replace = TRUE)
bdays
ubdays = unique(bdays)
ubdays

share = FALSE
if(length(unique(bdays)) < length(bdays)){
  share = TRUE
}
share

#create simulation--------
n = 2
numreps = 100000

share = rep(FALSE, numreps)

for (i in 1:numreps) {
  bdays = sample(1:365, size = n, replace = TRUE)
  if(length(unique(bdays)) < length(bdays)){
    share[i] = TRUE
  }
}
sum(share)/numreps

#simulate for different values n --------

n = 1:40
P = rep(0, 40)

for (i in 1:40) {
  numreps = 100000
  share = rep(FALSE, numreps)
  for (j in 1:numreps) {
    bdays = sample(1:365, size = i, replace = TRUE)
    if(length(unique(bdays)) < length(bdays)){
      share[j] = TRUE
    }
  }
  P[i] = sum(share)/numreps
}
plot(P, n, main = "Bday", type = "p", col = "blue", pch=16)
abline(h = 23, col = "red")
abline(v = P[23], col = "green")

P.data = data.frame(P)
