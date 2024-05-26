x.B <- 0.85
x.T <- 0.12
x.C <- 0.03
alpha.B <- 2.4
alpha.T <- 1
alpha.C <- 0.21

set.seed(1)

n.molecules <- 10000

m <- 220

m.B <- matrix(0, nrow = m, ncol = 38)
m.T <- matrix(0, nrow = m, ncol = 38)
m.C <- matrix(0, nrow = m, ncol = 38)

m.B[1, 1] <- n.molecules * x.B
m.T[1, 1] <- n.molecules * x.T
m.C[1, 1] <- n.molecules * x.C

p.B <- matrix(0, nrow = m, ncol = 38)
p.T <- matrix(0, nrow = m, ncol = 38)
p.C <- matrix(0, nrow = m, ncol = 38)

x.B.values <- matrix(0, nrow = m, ncol = 38)
x.T.values <- matrix(0, nrow = m, ncol = 38)
x.C.values <- matrix(0, nrow = m, ncol = 38)

x.B.values[1, 1] <- x.B
x.T.values[1, 1] <- x.T
x.C.values[1, 1] <- x.C

p.B[1, 1] <- alpha.B * m.B[1, 1]
p.T[1, 1] <- alpha.T * m.T[1, 1]
p.C[1, 1] <- alpha.C * m.C[1, 1]

n <- 37

for (i in 1:(m-1)) { 
  for (j in 1:n) {
    max.p <- max(p.B[i, 1], p.T[i, 1], p.C[i, 1])
    
    r.B <- p.B[i, 1] / max.p
    r.T <- p.T[i, 1] / max.p
    r.C <- p.C[i, 1] / max.p
    
    rand.num <- runif(1)
    
    if (r.B > rand.num) {
      m.B[i, j+1] <- m.B[i, j] - 1
    } else {
      m.B[i, j+1] <- m.B[i, j]
    }
    
    if (r.T > rand.num) {
      m.T[i, j+1] <- m.T[i, j] - 1
    } else {
      m.T[i, j+1] <- m.T[i, j]
    }
    
    if (r.C > rand.num) {
      m.C[i, j+1] <- m.C[i, j] - 1
    } else {
      m.C[i, j+1] <- m.C[i, j]
    }
  }
  
  m.B[i+1, 1] <- m.B[i, n+1]
  m.T[i+1, 1] <- m.T[i, n+1]
  m.C[i+1, 1] <- m.C[i, n+1]
  
  p.B[i+1, 1] <- alpha.B * m.B[i+1, 1]
  p.T[i+1, 1] <- alpha.T * m.T[i+1, 1]
  p.C[i+1, 1] <- alpha.C * m.C[i+1, 1]
}


for (i in 1:m) {
  for (j in 1:(n+1)) {
    total.m <- m.B[i, j] + m.T[i, j] + m.C[i, j]
    if (total.m > 0) {
      x.B.values[i, j] <- m.B[i, j] / total.m
      x.T.values[i, j] <- m.T[i, j] / total.m
      x.C.values[i, j] <- m.C[i, j] / total.m
    } else {
      x.B.values[i, j] <- 0
      x.T.values[i, j] <- 0
      x.C.values[i, j] <- 0
    }
  }
}

x.B.plot <- as.vector(t(x.B.values))
x.T.plot <- as.vector(t(x.T.values))
x.C.plot <- as.vector(t(x.C.values))
step <- rep(1:m, each = n+1)

plot.data <- data.frame(step, x.B.plot, x.T.plot, x.C.plot)

library(ggplot2)
ggplot(plot.data, aes(x = x.B.plot, y = x.T.plot, color = as.factor(step))) +
  geom_point() +
  labs(title = "Mole Fraction Changes over Steps",
       x = "Mole Fraction of Benzene",
       y = "Mole Fraction of Toluene") +
 scale_color_discrete(name = "Step") +
  theme_minimal()
