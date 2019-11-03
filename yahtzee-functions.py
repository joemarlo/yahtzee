#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Nov  3 12:40:45 2019
This script duplicates the R functions:
    roll.5.dice()
    calculate.score()
@author: joemarlo
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from plotnine import *
#from plotnine.data import mpg

# =============================================================================
# define roll die function and test it
# =============================================================================

def roll_die(ndie = 5) :
    "sample ndie with replacement"
    rolls = np.random.choice(range(1,7), ndie)
    rolls.sort()
    return(rolls)
    
# check that it works
roll_die()

# sample die 1000 times
walk5 = []
for i in range(1,1000) :
    roll = roll_die()
    walk5 = np.concatenate((walk5, roll))

#plt.hist(walk5)

# histogram of results 
walks = pd.DataFrame({'Face': walk5[:]}) # must be pandas df
ggplot(walks) +\
 aes(x = 'Face') +\
 geom_histogram(color = 'white', bins = 6) +\
 scale_x_continuous(breaks = range(1,7))

#run the function 1000 times for rolling 1,2,3,4,5 dice
walk1 = []; walk2 = []; walk3 = []; walk4 = []; walk5 = []
for i in range(1,1000) :
    roll = roll_die()
    walk5 = np.concatenate((walk5, roll))
    
    roll = roll_die(ndie = 4)
    walk4 = np.concatenate((walk4, roll))
    
    roll = roll_die(ndie = 3)
    walk3 = np.concatenate((walk3, roll))
    
    roll = roll_die(ndie = 2)
    walk2 = np.concatenate((walk2, roll))
    
    roll = roll_die(ndie = 1)
    walk1 = np.concatenate((walk1, roll))


# build dataframe of each series of rolls and stack them together
walks = pd.concat([pd.DataFrame({'Rolls': walk5, 'Type': "five"}),
                   pd.DataFrame({'Rolls': walk4, 'Type': "four"}),
                   pd.DataFrame({'Rolls': walk3, 'Type': "three"}),
                   pd.DataFrame({'Rolls': walk2, 'Type': "two"}),
                   pd.DataFrame({'Rolls': walk1, 'Type': "one"})])

# histogram of the results across different sized rolls
ggplot(walks) +\
 aes(x = 'Rolls') +\
 geom_histogram(color = 'white', bins = 6) +\
 scale_x_continuous(breaks = range(1,7)) +\
 facet_wrap('Type')

del(walk5, walk4, walk3, walk2, walk1, walks, roll, i)

# =============================================================================
# define function to calculate box score
# =============================================================================

def calculate_score(roll_results, verbose = False) :
    #function returns the maximum score for a dice throw
    #verbose argument prints out the score sheet
     
    #sort roll_results for readability
    roll_results.sort()
    
    #grab the number of die per face
    Ones = roll_results[roll_results == 1]
    Twos = roll_results[roll_results == 2]
    Threes = roll_results[roll_results == 3]
    Fours = roll_results[roll_results == 4]
    Fives = roll_results[roll_results == 5]
    Sixes = roll_results[roll_results == 6]
    
    #calculate frequency of each die  
    counts = []
    for count in [Ones, Twos, Threes, Fours, Fives, Sixes]:
        counts.append(len(count))
    
    #convert to numpy array
    counts = np.array(counts)
    
    #find unique number of faces then check for kinds
    face_sum = sum(roll_results)
    two_of_kind = any(counts == 2) #exactly two of a kind
    three_of_kind = any(counts == 3) * face_sum #exactly three of a kind
    four_of_kind = any(counts == 4) * face_sum#exactly four of a kind
    yahtzee = any(counts == 5) * 50 #exactly five of a kind
    
    #full house
    full_house = (two_of_kind & (three_of_kind > 0)) * 25
    
    #calculate if there is a small straight by checking if the die match either 1:4 or 2:5 or 3:6
    straight4 = (all(x in roll_results for x in range(1,5)) or
                 all(x in roll_results for x in range(2,6)) or
                 all(x in roll_results for x in range(3,7))) * 30
    
    #calculate if there is a large straight by checking if the die match 1:5 or 2:6
    straight5 = (all(x in roll_results for x in range(1,6)) or
                 all(x in roll_results for x in range(2,7))) * 40
    
    #https://stackoverflow.com/questions/54368328/is-there-a-panda-equivalent-to-tribble
    def tribble(columns, *data):
        return pd.DataFrame(
            data=list(zip(*[iter(data)]*len(columns))),
            columns=columns
        )
    
    #dataframe of the results
    results = tribble(
            ["Result", "Score"],
            "Roll results", np.array2string(roll_results, separator = "-"),
            "Ones", sum(Ones),
            "Twos", sum(Twos),
            "Threes", sum(Threes),
            "Fours", sum(Fours),
            "Fives", sum(Fives),
            "Sixes", sum(Sixes),
            "3 of a kind", three_of_kind,
            "4 of a kind", four_of_kind,
            "Full house", full_house,
            "Small straight", straight4,
            "Large straight", straight5,
            "Chance", sum(roll_results),
            "YAHTZEE", yahtzee
      )
    
    if verbose : print(results)
    best_result = max(results.iloc[range(1,14),1])
    return(best_result)


roll_results = roll_die()
roll_results = np.array([3,3,3,4,3])
print(roll_results)
calculate_score(roll_results = roll_results, verbose = True)


# =============================================================================
# simulate scores
# =============================================================================

# play 10000 rolls of yahtzee
scores = []
for i in range(1,10000) :
    score = calculate_score(roll_results = roll_die())
    scores.append(score)

# density plot of the scores
scores = pd.DataFrame({'Score': scores}) # must be pandas df
ggplot(scores) +\
 aes(x = 'Score') +\
 geom_density()
