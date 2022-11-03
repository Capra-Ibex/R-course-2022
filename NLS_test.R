#### Clean environnement ####
rm(list = ls())

#### WD ####
dir <- getwd(); dir
setwd(dir)

#### Packages ####
library(nls2)
library(tidyverse)

#### Dataset build with variability ####
### Base data
X1 <- 1:1000
Y1 <- 20+(-2)*log(X1)
Y2 <- Y1+rnorm(Y1, -1, 1)
G1 <- c("G1", "G2", "G3")
G2 <- sample(G1, length(X1), replace = T)

### Build data: variability per group 
D <- tibble(X = X1, Y = Y2, Group = G2) %>% 
  mutate(Y = unlist(map2(Y, Group, function(.a, .b){
    if(.b == "G1"){
      .a <- .a+2
    }else{
      if(.b == "G2"){
        .a <- .a-1.5
      }else{
        .a <- .a-0.5
      }}
  })))

### Plot 1 test
ggplot(D)+
  geom_point(aes(X, Y, col = Group))+
  theme_bw()

### Optimization ####
### NLS total
Mod_g <- nls2(Y ~ (a+b*log(X)), D, start = c(a = 0, b = 0))
summary(Mod_g)

### Prediction all
D$Pred1 <- predict(Mod_g)

### Plot 2
ggplot(D)+
  geom_point(aes(X, Y, col = Group)) +
  geom_line(aes(X, Pred1), size = 0.75) +
  theme_bw()

### NLS per group
D1 <- subset(D, Group == "G1")
Mod_g1 <- nls2(Y ~ (a+b*log(X)), D1, start = c(a = 0, b = 0))
summary(Mod_g1)
D1$Pred2 <- predict(Mod_g1)

D2 <- subset(D, Group == "G2")
Mod_g2 <- nls2(Y ~ (a+b*log(X)), D2, start = c(a = 0, b = 0))
summary(Mod_g2)
D2$Pred2 <- predict(Mod_g2)

D3 <- subset(D, Group == "G3")
Mod_g3 <- nls2(Y ~ (a+b*log(X)), D3, start = c(a = 0, b = 0))
summary(Mod_g3)
D3$Pred2 <- predict(Mod_g3)


DD <- rbind(D1, D2, D3)

### Plot 3
ggplot(DD)+
  geom_point(aes(X, Y, col = Group)) +
  geom_line(aes(X, Pred2, col = Group), size = 0.75) +
  theme_bw()
