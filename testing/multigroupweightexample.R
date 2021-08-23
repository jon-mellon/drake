library(drake)
n.per.group <- 100
n.groups <- 3
real.props.per.group <- runif(n.groups, 0.1, 0.9)
observed.props.per.group <- runif(n.groups, 0.1, 0.9)
true.group.sizes <- prop.table(abs(rnorm(n.groups)))
names(real.props.per.group) <- names(observed.props.per.group) <- 
  names(true.group.sizes) <- state.abb[1:n.groups]

z.true.prop <- runif(1,0.3, 0.7)
z.observed.prop <- runif(1,0.3, 0.7)
data <- data.frame(X = unlist(lapply(real.props.per.group, rbinom, size = 1, n=n.per.group)), 
                   Z = rbinom(n.per.group * n.groups, prob = z.observed.prop, size = 1), 
                   group = inverse.rle(list(values = 1:n.groups, lengths = rep(n.per.group, n.groups))))

data$Y <- data$X + rnorm(n.per.group*n.groups) + data$Z

data$X<- LETTERS[data$X+1]
data$Z<- LETTERS[data$Z+1]
data$group <- state.abb[data$group]


prop.table(table(data$Z))
z.observed.prop

discrete.targets <- list(group = true.group.sizes, 
                         Z= c(A = 1 - z.true.prop, B = z.true.prop))

makeABTarget <- function(x)  {
  c(A = 1-x, B = x)
}

discrete.target.subset <- list(X = list(group = lapply(real.props.per.group, makeABTarget)  ))

data$group<- factor(data$group, levels = c("AZ", "AL", "AK"))


data$weights <- drake(sample = data, discrete.targets = discrete.targets, 
                      discrete.target.subset =discrete.target.subset)

discrete.targets
prop.table(wtd.table(data$group, weights = data$weights))
prop.table(wtd.table(data$Z, weights = data$weights))


discrete.target.subset
prop.table(tapply(data$weights, list(data$group, data$X), sum), 1)
prop.table(wtd.table(data$Z, weights = data$weights))

data$Y

max.weights = 25
min.weights <- 1/max.weights

sample <- data
sample$weights <- 1
var <- "X"
discrete.sub <- discrete.targets.subset$X

