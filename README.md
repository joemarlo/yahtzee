# Simulating Yahtzee and determining your next move
`yahtzee-simulations.R`: R script that generates yahtzee simulations; sources `yahtzee-functions.R`\
`yahtzee-functions.R`: R script containing the core functions to simulate yahtzee and plot probabilistic outcomes

## To-do list
- [x] Function to calculate points per round
- [x] Function to calculate probabilities of the second roll and choose which die to keep
- [ ] Implement box scoring (e.g. keep track of which scores have been marked and update probabilities based on it)
- [ ] Optimize the calculate.die.to.keep function. For simiulations, it may be more efficient calculate a lookup table of all probabilties first then search the the table

See blog post: [marlo.works/posts/yahtzee/](https://joemarlo.github.io/posts/yahtzee)