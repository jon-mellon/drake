set.seed(123)

sample_small <- data.frame(
  age = c(20, 30, 40, 50),
  gender = factor(c("M", "F", "M", "F"), levels = c("M", "F")),
  region = factor(c("North", "South", "North", "South"), levels = c("North", "South")),
  turnout = factor(c("Voted", "DNV", "Voted", "DNV"), levels = c("Voted", "DNV"))
)

continuous_target_age <- density(sample_small$age, n = 64)

discrete_targets <- list(
  gender = c(M = 0.5, F = 0.5),
  region = c(North = 0.5, South = 0.5)
)

pops_small <- data.frame(
  Time = rep(2020, 5),
  AgeGrp = 18:22,
  PopFemale = c(10, 12, 11, 9, 8),
  PopMale = c(11, 10, 12, 8, 7),
  PopTotal = c(21, 22, 23, 17, 15),
  country = "BR"
)

mortality_table_small <- data.frame(
  Year = rep(2010:2011, each = 3),
  Age = rep(0:2, times = 2),
  Male = c(0.01, 0.02, 0.03, 0.01, 0.02, 0.03),
  Female = c(0.009, 0.018, 0.027, 0.009, 0.018, 0.027),
  Total = c(0.0095, 0.019, 0.028, 0.0095, 0.019, 0.028),
  OpenInterval = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE)
)
