# How many data points
years <- 5
farmers <- 30
fields <- 10
n <- years * farmers * fields

# simulate energy values
energy_range <- c(100, 2000)
energy <- runif(n = n, min = energy_range[1], max = energy_range[2])

# Model coefficients
b0 <- 0.4
b1 <- 1

# Simulating yields with noise
noise <- 100
yield <- b0 + b1*energy + rnorm(n, mean = 0, sd = noise)

# Putting all data in a data.frame
library(dplyr)
data <- expand.grid(farmer = 1:30, year = 1:5, field = 1:10) %>% 
  mutate_all(as.factor)
data <- cbind(data, yield = yield, energy = energy)

# Silly model
lm(yield ~ energy, data)


# Silly plot
library(ggplot2)
ggplot(data, aes(x = energy, y = yield, color = year)) +
  geom_point() +
  geom_smooth()

