library(plotly)
library(ggplot2)

# данные
a = -0.5
b = 1.5
c = 1
To = 1.5

v = -c
q = -0.5 от - 1 до 0

# задаем шаги для x и t
M = 100
h = (b-a)/M

tau = q*h/v
N = trunc(To/tau) + 1

# составляем x и t
x = seq(from = a, b + N, by = h)
t = seq(0, To, by = tau)

ksi = function (x){
  return(cos(x))
}

fi = function (x,t){
  return (2*x + t*exp(-t))
}

# Точное решение
Ue = matrix(nrow = N-1, ncol = M - 1)
for(n in 0:(N-1)) {
  for(m in 0:(M-1)) {
    Ue[n, m] = 1 + 2*x[m]*t[n] + t[n]^2 - exp(-t[n]) - t[n]*exp(-t[n])+cos(x[m]+t[n])
  }
}

# Численное решение
Ua = matrix(nrow = N-1, ncol = N+M)
for(m in 1: (N + M - 1) ) {
  Ua[ 1, m ] = ksi(x[m])
}
for(n in 2: (N - 1) ){
  for(m in 1: (M + N - n) ){
    Ua[n, m] = fi(x[m + 1], t[n]) * tau - q * Ua[n-1, m +1] + Ua[n-1, m] * (1 + q)
  }
}
Un = matrix(0, nrow = N-1, ncol = M-1)
for(n in 1:(N-1)) {
  for(m in 1:(M-1)) {
    Un[n,m] = Ua[n, m]
  }
}

# Строим график
p <- plot_ly(x = ~x, y = ~t, z = ~Un, color = "red") %>%
  add_surface(z = ~Un, opacity = 0.7, name = "×èñëåííîå") %>%
  add_surface(z = ~Ue, opacity = 1, name = "Òî÷íîå") 
p
