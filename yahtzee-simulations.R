library(tidyverse)
library(parallel)
source("yahtzee-functions.R") #core functions for gameplay

set.seed(65)
cpu.cores <- detectCores() #number of cores available for parallel processing

# notes -------------------------------------------------------------------

# sample rolls for testing
# roll.results <- samp.two.kind <- c(1,1,3,4,6)
# roll.results <- samp.three.kind <- c(2,3,4,4,4)
# roll.results <- samp.four.kind <- c(2,4,2,2,2)
# roll.results <- samp.yahtzee <- c(4,4,4,4,4)
# roll.results <- samp.4straight <- c(1,4,3,2,1)
# roll.results <- samp.5straight <- c(3,4,5,1,2)
# roll.results <- samp.fullhouse <- c(2,2,2,4,4)

# #test the sourced functions ---------------------------------------------

#test the sourced functions
roll <- roll.5.dice()
calculate.score(roll.results = roll, verbose = TRUE)
calculate.die.to.keep(seed.roll = roll, verbose = TRUE)
calculate.die.to.keep(seed.roll = c(2, 2, 4, 5, 2), verbose = TRUE)

# ggsave(filename = "Plots/Expected_roll_outcomes.svg",
#        plot = last_plot(),
#        device = "svg",
#        width = 8,
#        height = 7)


# simulate roll outcomes  -------------------------------------------

#simulate many rolls
n.sims <- 10000L
results <- mcreplicate(n = n.sims, mc.cores = cpu.cores, expr = calculate.score(roll.5.dice())) %>% unlist()

#density plot of score results
as.data.frame(results) %>%
  ggplot(aes(x = results)) +
  geom_density() +
  labs(title = "Density of scores",
       y = "Density",
       x = "Yahtzee score") +
  light.theme

# for testing -- seperate die rolls from results
rolls <- replicate(n.sims, roll.5.dice(), simplify = F)
results <- mclapply(X = rolls, FUN = calculate.score, mc.cores = cpu.cores) %>% unlist()

# simulate a single Yahtzee round by rolling the dice, calculate probabilities, choose best, roll again ---------

# simulating multiple rounds
n.sims <- 500L #i.e. number of 3-roll rounds to simulate
sim.results <- rep(NA, n.sims)
for (i in 1:n.sims){
  #first roll
  first.roll <- roll.5.dice()
  best.choice <- calculate.die.to.keep(seed.roll = first.roll)
  
  #second roll
  second.roll <- append(best.choice, sample(6, 5 - length(best.choice), replace = TRUE))
  best.choice <- calculate.die.to.keep(seed.roll = second.roll)
  
  #third roll
  third.roll <- append(best.choice, sample(6, 5 - length(best.choice), replace = TRUE))
  sim.results[i] <- calculate.score(roll.results = third.roll)
}

#random rolls
game.results <- mcreplicate(n = n.sims, mc.cores = cpu.cores, expr = calculate.score(roll.5.dice())) %>% unlist()

#comparison of the prediction function (Smart) and random rolls (Dumb)
tibble(Smart = sim.results, Dumb = game.results) %>%
  gather(key = "Type", value = "Score") %>%
  ggplot(aes(x = Type, y = Score)) +
  geom_boxplot(fill = "gray95") +
  stat_summary(fun.y = mean, geom = "errorbar",
               aes(ymax = ..y.., ymin = ..y..),
               width = .75, linetype = "dashed",
               color = "#2b7551") +
  labs(title = "'Dumb' random rolls and optimized 'smart' rolls",
       subtitle = paste0("Results from ",
                         scales::comma(n.sims),
                         " simulations each"),
       x = "",
       y = "Yahtzee score") +
  light.theme

# ggsave(filename = "Plots/Smart_vs_Dumb_boxplot.svg",
#        plot = last_plot(),
#        device = "svg",
#        width = 8,
#        height = 6)

