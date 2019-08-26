library(tidyverse)
library(parallel)

cpu.cores <- detectCores() #number of cores available for parallel processing

# sample results
# roll.results <- samp.two.kind <- c(1,1,3,4,6)
# roll.results <- samp.three.kind <- c(2,3,4,4,4)
# roll.results <- samp.four.kind <- c(2,4,2,2,2)
# roll.results <- samp.yahtzee <- c(4,4,4,4,4)
# roll.results <- samp.4straight <- c(1,4,3,2,1)
# roll.results <- samp.5straight <- c(3,4,5,1,2)
# roll.results <- samp.fullhouse <- c(2,2,2,4,4)


# function for one roll -------------------------------------------------------------
roll.dice <- function(roll.results = NULL) {
  
  #roll 5 die if none is provided
  if (is.null(roll.results)) {
    roll.results <- sample(6, 5, replace = TRUE)
  }
  
  # upper section
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
  full.house <-
    (calculate.kinds(2) > 0) * (calculate.kinds(3) > 0) * 25
  
  #straights
  sorted <- roll.results %>% sort() %>% unique()
  
  #list of indices to subset the results over and then match then to dices. E.g. we want to see if
  # list[1:4] matches the the straight 1:4 or list[2:5] matches the straight 1:4, etc.
  straight4.subset.matches <-
    expand.grid(list(1:4, 2:5), list(1:4, 2:5, 3:6))
  
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
  
  # print(results)
  
  last.roll <<- roll.results
  
  best.result <- results[2:14, 2] %>% unlist() %>% as.integer() %>% max()
  
  return(best.result)
}


# simulate ----------------------------------------------------------------

#function to parallelize replicate
mcreplicate <- function(n, expr, simplify = "array", ...) {
  mclapply(integer(n), eval.parent(substitute(function(...) expr)), 
           simplify = simplify, mc.cores = cpu.cores)
}

#simulate many rolls
n.sims <- 100000L
results <- mcreplicate(n = n.sims, mc.cores = cpu.cores, expr = roll.dice()) %>% unlist()


# for testing -- seperate die rolls from results
rolls <- replicate(n.sims, sample(6, 5, T), simplify = F)
results <- mclapply(X = rolls, FUN = roll.dice, mc.cores = cpu.cores)


# automatically predict outcomes ------------------------------------------

predict.new.round <- function() {
  #function takes the original roll, simulates the potential options (i.e. how many and
  # which die to keep) then returns the best choice based on mean expected outcome of next roll
  
  # keep 0, 1, 2, 3, 4 dice, sample new die, then simulate
  results <- lapply(0:4, function(die.to.keep) {
    base.rolls <- combn(seed.roll, die.to.keep, simplify = FALSE)
    base.rolls <- rep(base.rolls, n.sims)
    new.rolls <- lapply(base.rolls, function(x) {
      x <- append(x, sample(6, 5 - die.to.keep, replace = TRUE))
    })
    max.scores <- mclapply(X = new.rolls,
                           FUN = roll.dice,
                           mc.cores = cpu.cores) %>% unlist()
    
    #convert to clean data frame
    base.rolls <- lapply(base.rolls, function(x) {
      paste(x, collapse = "-")
    }) %>% unlist() %>% enframe()
    base.rolls$name <- die.to.keep
    
    new.rolls <-
      lapply(new.rolls, function(x) {
        paste(x, collapse = "-")
      }) %>% unlist() %>% enframe() %>% select(value)
    
    max.scores <- max.scores %>% enframe() %>% select(value)
    results <- bind_cols(base.rolls, new.rolls, max.scores)
    names(results) <-
      c("Die_to_keep", "Base_roll", "Roll", "Max_score")
    
    return(results)
  }) %>% bind_rows()
  
  #summary results per category
  summarized_results <- results %>%
    group_by(Base_roll) %>%
    summarize(
      Mean = mean(Max_score),
      Median = median(Max_score),
      SD = sd(Max_score)
    ) %>%
    arrange(desc(Mean, Median, SD))
  
  print(summarized_results)

  plot <- results %>%
    group_by(Base_roll) %>%
    mutate(Mean = mean(Max_score)) %>%
    ungroup() %>%
    mutate(Base_roll = reorder(Base_roll, Mean)) %>%
    ggplot(aes(x = Max_score)) +
    geom_density() +
    facet_wrap( ~ Base_roll) +
    geom_vline(aes(xintercept = Mean,
                   group = Die_to_keep),
               colour = 'blue')

  print(plot)
  
  #pick the best choice based on expected outcome
  best_choice <- summarized_results[which.max(summarized_results$Mean), "Base_roll"] %>% pull()
  best_choice <- str_split(best_choice, "-") %>% lapply(., as.numeric) %>% unlist()
  
  return(best_choice)
}


# run the full simulation -------------------------------------------------

n.sims <- 1000L # sims per "keep" category per round

roll.dice()
seed.roll <- last.roll %>% sort()
best_choice <- predict.new.round()
new.roll <- append(best_choice, sample(6, 5 - length(best_choice), replace = TRUE))
roll.dice(roll.results = new.roll)

# pre function code -------------------------------------------------------

# keep 0, 1, 2, 3, 4 dice, sample new die, then simulate
results <- lapply(0:4, function(die.to.keep) {
  base.rolls <- combn(seed.roll, die.to.keep, simplify = FALSE)
  base.rolls <- rep(base.rolls, n.sims)
  new.rolls <- lapply(base.rolls, function(x) {
    x <- append(x, sample(6, 5 - die.to.keep, replace = TRUE))
  })
  max.scores <- mclapply(X = new.rolls,
                         FUN = roll.dice,
                         mc.cores = cpu.cores) %>% unlist()
  
  #convert to clean data frame
  base.rolls <- lapply(base.rolls, function(x) {
    paste(x, collapse = "-")
  }) %>% unlist() %>% enframe()
  base.rolls$name <- die.to.keep
  
  new.rolls <-
    lapply(new.rolls, function(x) {
      paste(x, collapse = "-")
    }) %>% unlist() %>% enframe() %>% select(value)

  max.scores <- max.scores %>% enframe() %>% select(value)
  results <- bind_cols(base.rolls, new.rolls, max.scores)
  names(results) <- c("Die_to_keep", "Base_roll", "Roll", "Max_score")
  
  return(results)
}) %>% bind_rows()

#plot the expected results
results %>%
  group_by(Base_roll) %>%
  mutate(Mean = mean(Max_score)) %>%
  ungroup() %>%
  ggplot(aes(x = Max_score)) +
    geom_density() +
    facet_wrap(~Base_roll) +
    geom_vline(aes(xintercept = Mean,
                   group = Die_to_keep),
               colour = 'blue')

#average outcome per category
summarized_results <- results %>%
  group_by(Base_roll) %>%
  summarize(Mean = mean(Max_score),
            Median = median(Max_score),
            SD = sd(Max_score))

#pick the choice based on expected outcome
best_choice <- summarized_results[which.max(summarized_results$Mean), "Base_roll"] %>% pull()
best_choice <- str_split(best_choice, "-") %>% lapply(., as.numeric) %>% unlist()

#roll the die
new.roll <- append(best_choice, sample(6, 5 - length(best_choice), replace = TRUE))
roll.dice(roll.results = new.roll)


