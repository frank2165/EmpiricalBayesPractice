## This script is used as a case study of Empirical Bayesian Estimation, as presented
## in the book 'Introduction to Empirical Bayes' by David Robinson. In this script
## leaderboard data is scraped from a battlefield stats tracker website 
## (https://battlefieldtracker.com/bf1/leaderboards/xbox) for the following analysis:
##      1. Ranking players in the longest headshot leaderboard, to remove the 
##          influence of players who have obtained long range headshots by treason.
##      2. Predicting a players kill-death ratio based on the games played so far.
##      3. Predicting a players win rate based on the number of games played,
##          with the goal of identifying players who probably have a win rate
##          in excess of 50%.
##
## NOTE: Initial data extraction failed to produce many more than 1000 results,
##  need to determine why and pull data again.


# Globals
kd.save.file       <- "../Data/KdRatioLeaderboard.rd"
winrate.save.file  <- "../Data/WinRateLeaderboard.rd"
headshot.save.file <- "../Data/LongestHeadshotLeaderboard.rd"


# Attach Packages
library(dplyr)
library(purrr)
library(ggplot2)
library(magrittr)
library(mixtools)




###################################################################################
#                               Helper Functions                                  #
###################################################################################

mfloor <- function(x, base){
    floor(x / base) * base
}

mround <- function(x, base){
    round(x / base) * base
}

mceiling <- function(x, base){
    ceiling(x / base) * base
}



###################################################################################
#                               Getting the Data                                  #
###################################################################################

# Read leaderboard data and perform some extra cleaning
headshot <- readRDS(file = headshot.save.file) %>% 
    rename(LongestHeadshot = "Longest Headshot") %>% 
    mutate(LongestHeadshot = LongestHeadshot %>% 
               strsplit("\n") %>% 
               vapply(function(x) x[length(x)], character(1)) %>% 
               gsub("\\,", "", .) %>%
               as.numeric) %>% 
    dplyr::filter(LongestHeadshot > 0)


kdr <- readRDS(file = kd.save.file) %>% 
    rename(KDR = "K/D Ratio") %>% 
    mutate(KDR = KDR %>% 
               strsplit("\n") %>% 
               vapply(function(x) x[length(x)], character(1)) %>% 
               gsub("\\,", "", .) %>%
               as.numeric)


winrate <- readRDS(file = winrate.save.file) %>% 
    rename(WinRate = "W/L Ratio") %>% 
    mutate(WinRate = WinRate %>% 
               strsplit("\n") %>% 
               vapply(function(x) x[length(x)], character(1)) %>% 
               gsub("\\,", "", .) %>%
               as.numeric)



###################################################################################
#                               Part 1: Longest Headshot                          #
###################################################################################

## Notes: the results of this analysis are subject to the arbitrary specification
## of a decision threshold for what constitutes a "proper" headshot. The cutoff 
## point does not necessarily reflect what the maximum possible headshot is 
## within the parameters of a standard game and the whole process may have to
## be repeated given new data.


# Look at data to see if two different populations present themselves
## Observation: There appears to be two clusters of distances, the main cluster
##  occupying the 0m-959m range and a second cluster which occupies the 1032m-
##  1073m range. It looks like the main cluster has an extremely long tail and
##  the ability of a player to land a headshot at 800m without assistance (on
##  console) is doubtful; where to draw the line though? Currently 750m will
##  be used as the upper limit for the prior distribution.
xmin <- headshot$LongestHeadshot %>% min %>% mfloor(50)
xmax <- headshot$LongestHeadshot %>% max %>% mceiling(50)
hs <- ggplot(headshot, aes(x = LongestHeadshot)) + 
    geom_histogram(aes(y = ..density..), fill = "blue", colour = "black", alpha = 0.4, 
                   binwidth = 25) + 
    scale_x_continuous(limits = c(xmin, xmax), breaks = seq(xmin, xmax, 100)) + 
    theme_classic() + 
    labs(x = "Headshot Distance", y = "Count", 
         caption = "Distribution of 'Longest Headshot Distance' recorded by 'battlefield tracker' for 10,213 xbox players in the \n United States.") + 
    theme(plot.caption = element_text(hjust = 0)) + 
    guides(fill = FALSE)

hs


# Look for multiple sub-populations
em <- normalmixEM(headshot$LongestHeadshot, k = 2, fast = TRUE)
headshot <- mutate(headshot, 
                   Pr.k1 = dnorm(LongestHeadshot, em$mu[1], em$sigma[1]),
                   Pr.k2 = dnorm(LongestHeadshot, em$mu[2], em$sigma[2]), 
                   k1    = Pr.k1 > Pr.k2)

ggplot(headshot, aes(x = LongestHeadshot, group = k1)) + 
    geom_histogram(aes(fill = k1), colour = "black", alpha = 0.4, 
                   binwidth = 25) + 
    scale_x_continuous(limits = c(xmin, xmax), breaks = seq(xmin, xmax, 100)) + 
    scale_fill_manual(values = c("blue", "red")) + 
    theme_classic() + 
    labs(x = "Headshot Distance", y = "Count", 
         caption = "Distribution of 'Longest Headshot Distance' recorded by 'battlefield tracker' for 10,213 xbox players in the \n United States.") + 
    theme(plot.caption = element_text(hjust = 0)) + 
    guides(fill = FALSE)


## Notes: Could not find a right-skewed distribution that appropriately matched
##  the data. Speculate that a mixture of normal distributions will be a good
##  enough approximation, or a normal and an exponential distribution.


# Superimpose the distribution on the histogram
hs + stat_function(fun = function(x) dnorm(x, mean = normal.coefs$mu, sd = normal.coefs$sigma),
                   col = "red", size = 1)



# Estimate the probability of obtaining each observed headshot under this model




# For a given decision threshold, what is the longest headshot that is obtained
# under this model.






###################################################################################
#                               Part 2: Kill-Death Ratio                          #
###################################################################################

## Notes: Kd ratio is the number of kills expressed as a proportion of the number 
## of deaths, as such the distribution of the Kd ratio will be determined by the
## distribution of the kills and the deaths. Examination of the kills and deaths
## distributions would be complicated as both are a function not only of player
## skill but rather of time and/or games played. Therefore, propose examination
## of the kills and deaths by the number of games played to determine whether
## the distribution fits an identifiable pattern. 


# Multiply the reported kill-death ratio by the number of games played, note the
# rounding error.


# Examine the data, filtering out players that have not played many games, and 
# see if a "typical" distribution presents itself. Identify the ratio distribution
# of the Kd ratio. 
# (QUICK ALGEBRA NOTE: kills per-game / deaths per-game ~ kills / deaths).




# Fit the proposed distribution to the data




# For each player on the leaderboard, derive an estimate of their actual kill-death
# ratio and identify players that stand-out.









###################################################################################
#                                   Part 3: Win Rate                              #
###################################################################################

## Notes:
##



# Examine data and look for a "typical" distribution



# See how the beta distribution fits the data and calculate the prior parameters.




# Estimate the actual win rate for each player











