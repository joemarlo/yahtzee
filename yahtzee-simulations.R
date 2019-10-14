library(tidyverse)
library(parallel)
source("yahtzee-functions.R") #core functions for gameplay

set.seed(65)
cpu.cores <- detectCores() #number of cores available for parallel processing

# notes -------------------------------------------------------------------
# there is a global variable "last.roll" that is updated after each call to calculate.score()
# yahtzee rule book here: https://www.hasbro.com/common/documents/dad2af551c4311ddbd0b0800200c9a66/8302F43150569047F57EB8D746BA9D86.pdf
# sample rolls for testing
# roll.results <- samp.two.kind <- c(1,1,3,4,6)
# roll.results <- samp.three.kind <- c(2,3,4,4,4)
# roll.results <- samp.four.kind <- c(2,4,2,2,2)
# roll.results <- samp.yahtzee <- c(4,4,4,4,4)
# roll.results <- samp.4straight <- c(1,4,3,2,1)
# roll.results <- samp.5straight <- c(3,4,5,1,2)
# roll.results <- samp.fullhouse <- c(2,2,2,4,4)

#test the sourced functions
calculate.score(verbose = TRUE)
calculate.die.to.keep(seed.roll = last.roll, verbose = TRUE)

# ggsave(filename = "Expected_roll_outcomes.svg",
#        plot = last_plot(),
#        device = "svg",
#        width = 8,
#        height = 7)

# simulate roll outcomes  -------------------------------------------

#simulate many rolls
n.sims <- 10000L
results <- mcreplicate(n = n.sims, mc.cores = cpu.cores, expr = calculate.score()) %>% unlist()

#density plot of score results
as.data.frame(results) %>%
  ggplot(aes(x = results)) +
  geom_density() +
  labs(title = "Density of scores",
       y = "Density",
       x = "Yahtzee score") +
  light.theme

# for testing -- seperate die rolls from results
rolls <- replicate(n.sims, sample(6, 5, T), simplify = F)
results <- mclapply(X = rolls, FUN = calculate.score, mc.cores = cpu.cores) %>% unlist()

# simulate a single Yahtzee round by rolling the dice, calculate probabilities, choose best, roll again ---------

# simulating multiple rounds
n.sims <- 500L #i.e. number of 3-roll rounds to simulate
sim.results <- rep(NA, n.sims)
for (i in 1:n.sims){
  #first roll
  calculate.score(roll.results = NULL)
  
  #second roll
  best.choice <- calculate.die.to.keep(seed.roll = last.roll)
  new.roll <- append(best.choice, sample(6, 5 - length(best.choice), replace = TRUE))
  calculate.score(roll.results = new.roll)
  
  #third roll
  best.choice <- calculate.die.to.keep(seed.roll = last.roll)
  new.roll <- append(best.choice, sample(6, 5 - length(best.choice), replace = TRUE))
  sim.results[i] <- calculate.score(roll.results = new.roll)
}

#random rolls
game.results <- mcreplicate(n = n.sims, mc.cores = cpu.cores, expr = calculate.score()) %>% unlist()

#comparison of the prediction function (Smart) and random rolls (Dumb)
tibble(Smart = sim.results, Dumb = game.results) %>%
  gather(key = "Type", value = "Score") %>%
  ggplot(aes(x = Type, y = Score)) +
  geom_boxplot(fill = "gray95") +
  stat_summary(fun.y = mean, geom = "errorbar",
               aes(ymax = ..y.., ymin = ..y..),
               width = .75, linetype = "dashed",
               color = "#2b7551") +
  labs(title = "Results from 'dumb' random rolls and optimized 'smart' rolls",
       subtitle = paste0(scales::comma(n.sims), " simulations each"),
       x = "",
       y = "Yahtzee score") +
  light.theme

# ggsave(filename = "Smart_vs_Dumb_boxplot.svg",
#        plot = last_plot(),
#        device = "svg",
#        width = 8,
#        height = 6)