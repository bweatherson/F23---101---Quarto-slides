utility_tibble <- tibble(
  Consumption = c(500:25000) * 10
)

f0 <- function(x) {x}
f1 <- function(x) {x^0.5}
f2 <- function(x) {log(x)}
f3 <- function(x) {-(x^-0.5)}
f4 <- function(x) {-1/x}
f3 <- function(x) {-(x^-1.5)}

u10 <- f0(20000)
u11 <- f1(20000)
u12 <- f2(20000)
u13 <- f3(20000)
u14 <- f4(20000)
u15 <- f5(20000)

u20 <- f0(40000)
u21 <- f1(40000)
u22 <- f2(40000)
u23 <- f3(40000)
u24 <- f4(40000)
u25 <- f5(40000)

a0 <- 1/(u20 - u10)
a1 <- 1/(u21 - u11)
a2 <- 1/(u22 - u12)
a3 <- 1/(u23 - u13)
a4 <- 1/(u24 - u14)
a5 <- 1/(u25 - u15)

b0 <- 1 - u10 * a0
b1 <- 1 - u11 * a1
b2 <- 1 - u12 * a2
b3 <- 1 - u13 * a3
b4 <- 1 - u14 * a4
b5 <- 1 - u15 * a5

g0 <- function(x){a0 * f0(x) + b0}
g1 <- function(x){a1 * f1(x) + b1}
g2 <- function(x){a2 * f2(x) + b2}
g3 <- function(x){a3 * f3(x) + b3}
g4 <- function(x){a4 * f4(x) + b4}
g5 <- function(x){a5 * f5(x) + b5}

utility_tibble <- tibble(
  Consumption = c(1000:7000) * 10
) |>
  mutate(
    u0 = g0(Consumption),
    u1 = g1(Consumption),
    u2 = g2(Consumption),
    u3 = g3(Consumption),
    u4 = g4(Consumption),
    u5 = g5(Consumption),
  )

base <-
  ggplot(utility_tibble) +
  theme_minimal() +
  xlim(10000, 70000) # +
  # theme(
  #   axis.text.y = element_blank(),
  #   axis.ticks.y = element_blank()
  # )


base_graph <-   ggplot(utility_tibble) +
  theme_minimal() +
  geom_line(aes(x = Consumption, y = u0, color = "eta 0")) +
  geom_line(aes(x = Consumption, y = u1, color = "eta 0.5")) +
  geom_line(aes(x = Consumption, y = u2, color = "eta 1")) +
  geom_line(aes(x = Consumption, y = u3, color = "eta 1.5")) +
  geom_line(aes(x = Consumption, y = u4, color = "eta 2")) +
  labs(y = "Utility", x = "Annual Consumption in Dollars")

the_graph +
  transition_layers(layer_length = 1, transition_length = 2) +
  transition_states(Consumption, transition_length = 500, state_length = 1)

the_graph <- base + 
  geom_function(fun = function(x) x ^ 0.5, aes(color = "eta 0.5")) +
  geom_function(fun = function(x) a * log(x) + b, aes(color = "eta 1")) +
  geom_function(fun = function(x) 300 * (1 - 200 * (x^-0.5)/3), aes(color = "eta 1.5")) +
  transition_layers(layer_length = 1, transition_length = 2) +
  transition_states(x, transition_length = 50, state_length = 1) +
  labs(y = "Utility", x = "Annual Consumption in Dollars")
the_graph