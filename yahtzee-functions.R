library(tidyverse)
library(combinat)
library(parallel)
source("Plots/ggplot-theme.R") #custom format for ggplot


# function to calculate roll ----------------------------------------------

roll.5.dice <- function() sample(6, 5, replace = TRUE) %>% sort()


# function to calculate score of a given roll --------------------------

calculate.score <- function(roll.results = NULL, verbose = FALSE) {
  #function returns the maximum score for a dice throw
  #verbose argument prints out the score sheet

  # stop if no seed.roll is provided
  if (missing(roll.results)) {stop("No roll.results provided; maybe you want to call roll.5.dice?")}

  # sort seed.roll for readability
  roll.results <- sort(roll.results)
  
  #grab the number of die per face
  Ones <- roll.results[roll.results == 1]
  Twos <- roll.results[roll.results == 2]
  Threes <- roll.results[roll.results == 3]
  Fours <- roll.results[roll.results == 4]
  Fives <- roll.results[roll.results == 5]
  Sixes <- roll.results[roll.results == 6]

  #calculate frequency of each die
  counts <- sapply(list(Ones, Twos, Threes, Fours, Fives, Sixes), length)

  #find unique number of faces then check for kinds
  face.sum <- sum(roll.results)
  two.of.kind <-   any(counts == 2) #exactly two of a kind
  three.of.kind <- any(counts == 3) * face.sum #exactly three of a kind
  four.of.kind <-  any(counts == 4) * face.sum #exactly four of a kind
  yahtzee <-       any(counts == 5) * 50 #exactly five of a kind

  #full house
  full.house <- (two.of.kind & (three.of.kind > 0)) * 25

  #calculate if there is a small straight by checking if the die match either 1:4 or 2:5 or 3:6
  straight4 <- (all(1:4 %in% roll.results) | all(2:5 %in% roll.results) | all(3:6 %in% roll.results)) * 30

  #calculate if there is a large straight by checking if the die match 1:5 or 2:6
  straight5 <- (all(1:5 %in% roll.results) | all(2:6 %in% roll.results))  * 40

  #results
  results <- tribble(
    ~ Result, ~ Score,
    "Roll results", paste(roll.results, collapse = "-"),
    "Ones", sum(Ones),
    "Twos", sum(Twos),
    "Threes", sum(Threes),
    "Fours", sum(Fours),
    "Fives", sum(Fives),
    "Sixes", sum(Sixes),
    "3 of a kind", three.of.kind,
    "4 of a kind", four.of.kind,
    "Full house", full.house,
    "Small straight", straight4,
    "Large straight", straight5,
    "Chance", sum(roll.results),
    "YAHTZEE", yahtzee
  )

  if (verbose) {print(results)}
  best.result <- results[2:14, 2] %>% unlist() %>% as.integer() %>% max()
  return(best.result)
}



# automatically predict outcomes based on which die to keep --------

#calculate the scores by first creating a dataframe of all possible combinations
# then appling calculate.score() over it. This method minimizes the calls to 
# calculate.score()

#set up data then generate all possible permutations of the new roll
reps <- replicate(5, 1:6, simplify = FALSE)
perms <- expand.grid(reps) #generate all permutations of new die
#modify structure from DF to list
perms <- lapply(1:nrow(perms), function(x) {
  perms[x, ] %>% as.integer()
})
#add in the score for each perm and unlist the Roll
# this data.frame is the "master list" of the scores
all.scores <- tibble(perms) %>%
  rename(Roll = perms) %>%
  rowwise() %>%
  mutate(Max_score = calculate.score(Roll),
         Roll = paste(Roll, collapse = "-") %>% unlist()) %>%
  ungroup()
rm(reps, perms)


calculate.die.to.keep <- function(seed.roll, verbose = FALSE) {
  
  #function takes a current roll (seed.roll), calculates all potential combinations of die to keep,
  #then calculates the expected outcome for each combination,
  #then returns the best choice of die to keep based on mean expected outcome of next roll
  #verbose prints a table of possible outcomes and plots the densities
  #the function depends on the all.scores data.frame
  
  # stop if no seed.roll is provided
  if (missing(seed.roll)) {
    stop("No seed roll provided; maybe you want to provide last.roll?")
  }
  
  # sort seed.roll for readability
  seed.roll <- sort(seed.roll)
  
  # withhold 0, 1, 2, 3, 4, or 5 dice from the seed.roll then generate all combinations of new die
  # returns a data frame containing a row per each new permutation and its respective score
  # rows are also labeled according to which die were withheld (i.e. kept)
  
  results <- mclapply(X = 0:5, mc.cores = cpu.cores, FUN = function(die.to.keep) {
    #different combinations of the original die to keep
    base.rolls <- combn(seed.roll, die.to.keep, simplify = FALSE) %>% lapply(., sort)
    #remove duplicates
    base.rolls <- base.rolls[!duplicated(base.rolls)]
    
    #set up data then generate all possible permutations of the new roll
    reps <- replicate(5 - die.to.keep, 1:6, simplify = FALSE)
    new.perms <- expand.grid(reps) #generate all permutations of new die
    #modify structure from DF to list
    new.perms <- lapply(1:nrow(new.perms), function(x) {
      new.perms[x, ] %>% as.numeric()
    })
    
    #combine the base.roll combinations with the new permutations to generate
    #    all possible outcomes
    if (die.to.keep > 0) {
      new.rolls <- expand.grid(base.rolls, new.perms)
      new.rolls <- lapply(1:nrow(new.rolls), function(x) {
        new.rolls[x, ] %>% unlist() %>% as.numeric()
      })
    } else
      new.rolls <- new.perms
    
    #convert data to a clean data frame then return the results
    if (die.to.keep > 0) {
      base.rolls <- rep(base.rolls, length.out = length(new.rolls))
      base.rolls <- lapply(base.rolls, function(x) {
        paste(x, collapse = "-")
      }) %>% unlist() %>% enframe() %>% select(value)
    } else
      base.rolls <- tibble(rep("Keep no dice", length(new.rolls)))
    
    #convert new rolls to a character string then place in data frame
    new.rolls <- lapply(new.rolls, function(x) {
      paste(x, collapse = "-")
    }) %>% unlist() %>% enframe()
    #label the rows by how many die were kept
    new.rolls$name <- die.to.keep
    
    #build one data frame of the results containing the base rolls, the
    #  new roll permutations and the scores from those rolls
    results <- bind_cols(base.rolls, new.rolls)
    names(results) <- c("Base_roll", "Die_to_keep", "Roll")
    results <- results %>% select(Die_to_keep, Base_roll, Roll)
    
    return(results)
    
  }) %>% bind_rows()
  
  #add in the scores to the results
  results <- left_join(results, all.scores, by = "Roll")
  
  #summarize the results per base roll
  summarized_results <- results %>%
    group_by(Base_roll) %>%
    summarize(
      Mean = mean(Max_score),
      Median = median(Max_score),
      SD = sd(Max_score)
    ) %>%
    arrange(desc(Mean, Median, SD))
  
  #print and plot the results
  if (verbose) {
    plot <- results %>%
      group_by(Base_roll) %>%
      mutate(Mean = mean(Max_score)) %>%
      ungroup() %>%
      mutate(Base_roll = reorder(Base_roll, Mean)) %>%
      ggplot(aes(x = Max_score)) +
      # geom_histogram(binwidth = 1, color = "white") +
      geom_density() +
      facet_wrap(~ Base_roll) +
      geom_vline(aes(xintercept = Mean, group = Die_to_keep),
                 color = '#2b7551',
                 size = 1.2) +
      labs(title = "Density of expected outcomes segmented by which die to keep",
           subtitle = paste0("The original roll is ",
                             paste(seed.roll, collapse = "-"),
                             ". The green verticle line represents the mean expected outcome."),
           y = "Density",
           x = "Yahtzee score") +
      light.theme
    
    print(summarized_results)
    print(plot)
    
  }
  
  #pick the best choice based on mean expected outcome
  best.choice <-
    summarized_results[which.max(summarized_results$Mean), "Base_roll"] %>%
    pull() %>%
    str_split(., "-") %>%
    unlist() %>%
    as.integer()
  
  return(best.choice)
}


# function to parallelize replicate ---------------------------------------

mcreplicate <- function(n, expr, simplify = "array", ...) {
  #function parallelizes replicate by imitating mclapply
  #this is similar to how replicate imitates lapply
  
  mclapply(integer(n), eval.parent(substitute(function(...) expr)), 
           simplify = simplify, mc.cores = cpu.cores)
}
