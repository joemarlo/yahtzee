library(tidyverse)
library(combinat)
library(parallel)

cpu.cores <- detectCores() #number of cores available for parallel processing

# notes -------------------------------------------------------------------
# function to calculate points per round is finished
# function to calculate probabilities of the second roll and choose which die to keep is finished 
# next is to implement the box scoring; e.g. keep track of which scores
#   have been marked and update probabilities based on it
#calculate.die.to.keep may be more efficient if a lookup table of all the probabilites
#   is first implemented then reused for each function call
# note there is a global variable "last.roll" that is updated after each call to calculate.score()

# yahtzee rule book here: https://www.hasbro.com/common/documents/dad2af551c4311ddbd0b0800200c9a66/8302F43150569047F57EB8D746BA9D86.pdf

# sample rolls for testing
# roll.results <- samp.two.kind <- c(1,1,3,4,6)
# roll.results <- samp.three.kind <- c(2,3,4,4,4)
# roll.results <- samp.four.kind <- c(2,4,2,2,2)
# roll.results <- samp.yahtzee <- c(4,4,4,4,4)
# roll.results <- samp.4straight <- c(1,4,3,2,1)
# roll.results <- samp.5straight <- c(3,4,5,1,2)
# roll.results <- samp.fullhouse <- c(2,2,2,4,4)


# function to calculate score of a given roll --------------------------
calculate.score <- function(roll.results = NULL, verbose = FALSE) {
  
  #function returns the maximum score for a random dice throw
  #if no roll is provided (roll.results) then a roll is randomly generated
  #verbose argument prints out the score sheet
  
  #roll 5 die if none are provided
  if (is.null(roll.results)) {
    roll.results <- sample(6, 5, replace = TRUE)
  }
  
  # upper section of the score sheet
  Ones <- sum(roll.results[roll.results == 1])
  Twos <- sum(roll.results[roll.results == 2])
  Threes <- sum(roll.results[roll.results == 3])
  Fours <- sum(roll.results[roll.results == 4])
  Fives <- sum(roll.results[roll.results == 5])
  Sixes <- sum(roll.results[roll.results == 6])
  
  #of a kind
  calculate.kinds <- function(count) {
    sapply(1:6, function(x) {
      face.counts <- length(roll.results[roll.results == x])
      kinds <- any(face.counts == count)
      points <- kinds * sum(roll.results)
      return(points)
    }) %>% max()
  }
  
  three.of.kind <- calculate.kinds(3)
  four.of.kind <- calculate.kinds(4)
  yahtzee <- (calculate.kinds(5) > 0) * 50
  
  #full house
  full.house <- (calculate.kinds(2) > 0) * (calculate.kinds(3) > 0) * 25
  
  #straights
    #sorted roll.results
    sorted <- roll.results %>% sort() %>% unique()
    
    #list of indices to subset the results over and then match them to dices. E.g. we want to see if
    # list[1:4] matches the the straight 1:4 or list[2:5] matches the straight 1:4, etc.
    straight4.subset.matches <- expand.grid(list(1:4, 2:5), list(1:4, 2:5, 3:6))
    
    #calculates if all the numbers in a *subset index* of the list matches the input vector *matches*
    match.subset <-
      function(subset.index, matches) {
        all(sorted[subset.index] == matches)
      }
    
    #apply the function
    straight4 <- any(
      mapply(
        FUN = match.subset,
        subset.index = straight4.subset.matches$Var1,
        matches = straight4.subset.matches$Var2
      ),
      na.rm = TRUE
    ) * 30
    
    if (length(sorted) == 5) {
      straight5 <- all(sorted == 1:5) * 40
    } else {
      straight5 <- 0
    }
  
  #sum of the parts
  chance <- sum(roll.results)
  
  #results
  results <- tribble(
    ~ Result, ~ Score,
    "Roll results", paste(roll.results, collapse = "-"),
    "Ones", Ones,
    "Twos", Twos,
    "Threes", Threes,
    "Fours", Fours,
    "Fives", Fives,
    "Sixes", Sixes,
    "3 of a kind", three.of.kind,
    "4 of a kind", four.of.kind,
    "Full house", full.house,
    "Small straight", straight4,
    "Large straight", straight5,
    "Chance", chance,
    "YAHTZEE", yahtzee
  )
  
  if (verbose) {print(results)}
  
  last.roll <<- sort(roll.results)
  
  best.result <- results[2:14, 2] %>% unlist() %>% as.integer() %>% max()
  
  return(best.result)
}

#test the function
calculate.score(verbose = TRUE)


# simulate roll outcomes  -------------------------------------------

#function to parallelize replicate
mcreplicate <- function(n, expr, simplify = "array", ...) {
  mclapply(integer(n), eval.parent(substitute(function(...) expr)), 
           simplify = simplify, mc.cores = cpu.cores)
}

#simulate many rolls
n.sims <- 1000L
results <- mcreplicate(n = n.sims, mc.cores = cpu.cores, expr = calculate.score()) %>% unlist()

# for testing -- seperate die rolls from results
rolls <- replicate(n.sims, sample(6, 5, T), simplify = F)
results <- mclapply(X = rolls, FUN = calculate.score, mc.cores = cpu.cores) %>% unlist()

#theme for ggplot
seashell.theme <- theme(legend.position = "none",
                        panel.grid.minor = element_line(color = NA),
                        panel.background = element_rect(fill = "seashell2"),
                        plot.background = element_rect(fill = "seashell",
                                                       color = NA),
                        axis.title = element_text(color = "gray30",
                                                  size = 12),
                        strip.background = element_rect(fill = "seashell3"),
                        plot.title = element_text(color = "gray30",
                                                  size = 14,
                                                  face = "bold"))

#density plot of score results
ggplot(as.data.frame(results),
       aes(x = results)) +
  geom_density() +
  labs(title = "Density of outcomes",
       y = "Density",
       x = "Score") +
  seashell.theme


# automatically predict outcomes based on which die to keep --------

calculate.die.to.keep <- function(seed.roll, verbose = FALSE) {
  #function takes a current roll (seed.roll), calculates all potential combinations of die to keep,
  #then calculates the expected outcome for each combination, then returns
  #the best choice of die to keep based on mean expected outcome of next roll
  #verbose prints a table of possible outcomes and plots the densities
  
  # stop if no seed.roll is provided
  if (missing(seed.roll)) {stop("No seed roll provided; maybe you want to provide last.roll?")}
  
  # keep 0, 1, 2, 3, 4, 5 dice then generate all combinations of new die, then calculate scores
  results <- lapply(0:5, function(die.to.keep) {

    #different combinations of the original die to keep
    base.rolls <- combn(seed.roll, die.to.keep, simplify = FALSE)
    
    #set up data then generate all possible permutations of the new roll
    reps <- replicate(5 - die.to.keep, 1:6, simplify = FALSE)
    new.perms <- expand.grid(reps) #generates all permutations of new die
    new.perms <- lapply(1:nrow(new.perms), function(x){new.perms[x,] %>% as.numeric()}) #modifies structure from DF to list
    
    #combine the base.roll combinations with the new permutations to generate
    #    all possible outcomes
    if (die.to.keep > 0) {
      new.rolls <- expand.grid(base.rolls, new.perms)
      new.rolls <- lapply(1:nrow(new.rolls), function(x) {
        new.rolls[x,] %>% unlist() %>% as.vector()
      })
    } else
      new.rolls <- new.perms
    
    #calculate the maximium scores for each possible roll
    max.scores <- mclapply(X = new.rolls,
                           FUN = calculate.score,
                           mc.cores = cpu.cores) %>% unlist()

    #convert data to a clean data frame then return the results
    if (die.to.keep > 0) {
      base.rolls <- rep(base.rolls, length.out = length(new.rolls))
      base.rolls <- lapply(base.rolls, function(x) {
        paste(x, collapse = "-")
      }) %>% unlist() %>% enframe() %>% select(value)
    } else
      base.rolls <- tibble(rep("Keep no dice", length(new.rolls))
      )

    new.rolls <- lapply(new.rolls, function(x) {paste(x, collapse = "-")}) %>% unlist() %>% enframe()
    new.rolls$name <- die.to.keep

    max.scores <- max.scores %>% enframe() %>% select(value)
    
    results <- bind_cols(base.rolls, new.rolls, max.scores)
    names(results) <- c("Base_roll", "Die_to_keep", "Roll", "Max_score")
    results <- results %>% select(Die_to_keep, Base_roll, Roll, Max_score)

    return(results)

  }) %>% bind_rows()
  
  #summary results per category and plot
  summarized_results <- results %>%
    group_by(Base_roll) %>%
    summarize(
      Mean = mean(Max_score),
      Median = median(Max_score),
      SD = sd(Max_score)
    ) %>%
    arrange(desc(Mean, Median, SD))
  
  if (verbose) {
    plot <- results %>%
      group_by(Base_roll) %>%
      mutate(Mean = mean(Max_score)) %>%
      ungroup() %>%
      mutate(Base_roll = reorder(Base_roll, Mean)) %>%
      ggplot(aes(x = Max_score)) +
      # geom_histogram(binwidth = 1, color = "white") +
      geom_density() +
      facet_wrap( ~ Base_roll) +
      geom_vline(aes(xintercept = Mean,
                     group = Die_to_keep),
                 colour = 'blue') +
      labs(title = "Density of expected outcomes segmented by which die to keep",
           subtitle = paste0("The original roll is ",
                             paste(seed.roll, collapse = "-"),
                             ". The blue verticle line represents the mean expected outcome."),
           y = "Density",
           x = "Score") +
      seashell.theme
    
    print(summarized_results)
    print(plot)
    
  }

  #pick the best choice based on expected outcome
  best_choice <- summarized_results[which.max(summarized_results$Mean), "Base_roll"] %>% pull()
  best_choice <- str_split(best_choice, "-") %>% lapply(., as.numeric) %>% unlist()

  return(best_choice)
}

#test the function
calculate.score(roll.results = NULL)
calculate.die.to.keep(seed.roll = last.roll, verbose = TRUE)


# simulate a single Yahtzee round by rolling the dice, calculate probabilities, choose best, roll again ---------

#first roll
calculate.score(roll.results = NULL)

#second roll
best_choice <- calculate.die.to.keep(seed.roll = last.roll)
new.roll <- append(best_choice, sample(6, 5 - length(best_choice), replace = TRUE))
calculate.score(roll.results = new.roll)

#third roll
best_choice <- calculate.die.to.keep(seed.roll = last.roll)
new.roll <- append(best_choice, sample(6, 5 - length(best_choice), replace = TRUE))
calculate.score(roll.results = new.roll)

# simulating multiple rounds and comparing it against pure random rolls
n.sims <- 50L
sim.results <- rep(NA, n.sims)
for (i in 1:n.sims){
  #first roll
  calculate.score(roll.results = NULL)
  
  #second roll
  best_choice <- calculate.die.to.keep(seed.roll = last.roll)
  new.roll <- append(best_choice, sample(6, 5 - length(best_choice), replace = TRUE))
  calculate.score(roll.results = new.roll)
  
  #third roll
  best_choice <- calculate.die.to.keep(seed.roll = last.roll)
  new.roll <- append(best_choice, sample(6, 5 - length(best_choice), replace = TRUE))
  sim.results[i] <- calculate.score(roll.results = new.roll)
}

#random rolls
game.results <- mcreplicate(n = n.sims, mc.cores = cpu.cores, expr = calculate.score()) %>% unlist()

#comparison of the prediction function (Smart) and random rolls (Dumb)
tibble(Smart = sim.results, Dumb = game.results) %>%
  gather(key = "Type", value = "Score") %>%
  ggplot(aes(x = Type, y = Score)) +
    geom_boxplot() +
    annotate("text", label = "yahtzee roll after only 50 rolls",
             x = 1.5, y = 45,
             color = "gray20",
             size = 4) +
    labs(title = "Results from 'dumb' random rolls and optimized 'smart' rolls") +
    geom_curve(
      aes(x = 1.84, y = 45,
          xend = 1.99, yend = 49),
      arrow = arrow(length = unit(0.02, "npc")),
      color = "gray40") +
    seashell.theme

