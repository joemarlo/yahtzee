library(tidyverse)
library(combinat)
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
  
  print(results)
  
  last.roll <<- roll.results
  
  best.result <- results[2:14, 2] %>% unlist() %>% as.integer() %>% max()
  
  return(best.result)
}


# simulate roll outcomes  -------------------------------------------

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


# automatically predict outcomes based on which die to keep --------

predict.new.round <- function() {
  #function takes the original roll, calculates all potential combination of die to keep,
  #then calculates the expected outcome for each combionation, then returns
  #the best choice based on mean expected outcome of next roll
  
  # keep 0, 1, 2, 3, 4 dice, generate all combinations of new die, then calculate scores
  results <- lapply(0:5, function(die.to.keep) {

    #different combinations of the original die to keep
    base.rolls <- combn(seed.roll, die.to.keep, simplify = FALSE)
    
    #set up data then generate all possible permutations of the new roll
    reps <- replicate(5 - die.to.keep, 1:6, simplify = FALSE)
    new.perms <- expand.grid(reps) #generates all permutations of new die
    new.perms <- lapply(1:nrow(new.perms), function(x){new.perms[x,] %>% as.numeric()}) #unlists the die
    
    #combine the base.roll combinations with the new permutations to generate
    #all possible outcomes
    if (die.to.keep > 0) {
      new.rolls <- expand.grid(base.rolls, new.perms)
      new.rolls <-
        lapply(1:nrow(new.rolls), function(x) {
          new.rolls[x, ] %>% unlist() %>% as.vector()
        })
    } else
      new.rolls <- new.perms
    
    #calculate the maxmium scores for each possible roll
    max.scores <- mclapply(X = new.rolls,
                           FUN = roll.dice,
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

    new.rolls <-
      lapply(new.rolls, function(x) {
        paste(x, collapse = "-")
      }) %>% unlist() %>% enframe()
    new.rolls$name <- die.to.keep

    max.scores <- max.scores %>% enframe() %>% select(value)
    
    results <- bind_cols(base.rolls, new.rolls, max.scores)
    names(results) <- c("Base_roll", "Die_to_keep", "Roll", "Max_score")
    results <- results %>% select(Die_to_keep, Base_roll, Roll, Max_score)

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
    # geom_histogram(binwidth = 1, color = "white") +
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


# roll the dice, calculate probabilities, choose best, roll again ---------

#first roll
roll.dice()

#second roll
seed.roll <- last.roll %>% sort()
best_choice <- predict.new.round()
new.roll <- append(best_choice, sample(6, 5 - length(best_choice), replace = TRUE))
roll.dice(roll.results = new.roll)

#third roll
seed.roll <- last.roll %>% sort()
best_choice <- predict.new.round()
new.roll <- append(best_choice, sample(6, 5 - length(best_choice), replace = TRUE))
roll.dice(roll.results = new.roll)



