library(tidyverse)
library(combinat)
library(parallel)
source("ggplot-theme.R") #custom format for ggplot


# function to calculate score of a given roll --------------------------
calculate.score <- function(roll.results = NULL, verbose = FALSE) {

  #function returns the maximum score for a random dice throw
  #if no roll is provided (roll.results) then a roll is randomly generated
  #verbose argument prints out the score sheet

  #roll 5 die if none are provided
  if (is.null(roll.results)) {
    roll.results <- sample(6, 5, replace = TRUE)
  }

  #sort results for ease of reading
  roll.results <- sort(roll.results)

  #grab the number of die per face
  Ones <- roll.results[roll.results == 1]
  Twos <- roll.results[roll.results == 2]
  Threes <- roll.results[roll.results == 3]
  Fours <- roll.results[roll.results == 4]
  Fives <- roll.results[roll.results == 5]
  Sixes <- roll.results[roll.results == 6]

  #count of each face
  counts <- sapply(list(Ones, Twos, Threes, Fours, Fives, Sixes), length)

  #find unique number of faces then check for kinds
  face.sum <- sum(roll.results)
  two.of.kind <-   any(counts == 2) #exactly two of a kind
  three.of.kind <- any(counts == 3) * face.sum #exactly three of a kind
  four.of.kind <-  any(counts == 4) * face.sum #exactly four of a kind
  yahtzee <-       any(counts == 5) * 50 #exactly five of a kind

  #full house
  full.house <- (two.of.kind & (three.of.kind > 0)) * 25

  #calculate if there is a small straight by checking if the die match either 1:4 or 2:5
  straight4 <- (all(1:4 %in% roll.results) | all(2:5 %in% roll.results)) * 30

  #calculate if there is a large straight by checking if the die match 1:5
  straight5 <- all(1:5 %in% roll.results) * 40

  #sum of the parts
  chance <- sum(roll.results)

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
    "Chance", chance,
    "YAHTZEE", yahtzee
  )

  if (verbose) {print(results)}
  last.roll <<- roll.results
  best.result <- results[2:14, 2] %>% unlist() %>% as.integer() %>% max()
  return(best.result)
}



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
  best.choice <- summarized_results[which.max(summarized_results$Mean), "Base_roll"] %>%
    pull() %>%
    str_split(., "-") %>%
    lapply(., as.numeric) %>%
    unlist()

  return(best.choice)
}


# function to parallelize replicate ---------------------------------------

mcreplicate <- function(n, expr, simplify = "array", ...) {
  
  #function parallelizes replicate similar to how replicate uses lapply
  
  mclapply(integer(n), eval.parent(substitute(function(...) expr)), 
           simplify = simplify, mc.cores = cpu.cores)
}
