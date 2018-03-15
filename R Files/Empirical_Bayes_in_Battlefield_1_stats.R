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
winrate.conq.save.file <- "../Data/WinRateConquestLeaderboard.rd"
winrate.aus.save.file  <- "../Data/WinRateConquestLeaderboard_AUS.rd"
winrate.uk.save.file   <- "../Data/WinRateConquestLeaderboard_UK.rd"


# Attach Packages
library(VGAM)
library(dplyr)
library(purrr)
library(stats4)
library(gamlss)
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

data_histogram <- function(data, x, nbins = NULL){
    
    if(is.null(nbins)){
        nbins <- nrow(data) %>% sqrt %>% floor %>% c(20) %>% min
    }
    
    data <- rename_(data, .dots = list(x = x))
    
    
    ggplot(data, aes(x = x)) + 
        geom_histogram(fill = "blue", colour = "black", alpha = 0.4, bins = nbins) + 
        theme_classic()
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


winrate <- map2(c("United States", "Australia", "United Kingdom"), 
                c(winrate.conq.save.file, winrate.aus.save.file, winrate.uk.save.file),
                function(ctry, file) readRDS(file) %>% mutate(Country = ctry)) %>% 
    do.call("rbind", .) %>% 
    rename(WinRate = "Win %") %>% 
    mutate(WinRate = WinRate %>% 
               strsplit("\n") %>% 
               vapply(function(x) x[length(x)], character(1)) %>% 
               gsub("\\,", "", .) %>%
               as.numeric, 
           WinRate = WinRate / 100, 
           Games   = as.integer(Games), 
           Wins    = WinRate * Games, 
           Losses  = (1-WinRate) * Games, 
           Wins.approx = round(Wins), 
           Losses.approx = Games - Wins.approx)



###################################################################################
#                                   Part 1: Win Rate                              #
###################################################################################

## Notes: Both the distribution of WinRate and of the number of games played (Games)
##  seem fairly regular, i.e. unimodal with no large outliers.
##

# Model Building to be performed on the United States players
winrate.us <- dplyr::filter(winrate, Country == "United States")


# Examine data and look for a "typical" distribution
data_histogram(winrate.us, "WinRate", nbins = 50)
data_histogram(winrate.us, "Games", nbins = 100) + 
    scale_x_continuous(breaks = seq(0, max(winrate$Games), 500))

games.binned <- hist(winrate.us$Games, breaks = seq(0, max(winrate$Games)+50, 50), plot = FALSE)
games.mode   <- games.binned$mids[which.max(games.binned$density)]


mutate(winrate.us, InSample = Games > (mceiling(games.mode, 100))) %>% 
    dplyr::filter(InSample) %>% 
    ggplot(mapping = aes(x = WinRate)) + 
        geom_density(data = winrate.us, colour = "blue", size = 1) + 
        geom_density(colour = "red", size = 1) + 
        labs(x = "Win Rate (Wins/Losses)", y = "Density", 
             caption = "Distribution of WinRate for each dataset: All players (blue), experienced players (red).") + 
        theme_classic() + 
        theme(plot.caption = element_text(hjust = 0.5))


# Data for fitting prior distribution hyper-parameters
prior.data <- dplyr::filter(winrate.us, Games > mfloor(games.mode, 50)) %>% 
    mutate(Wins = round(Wins), Losses = Games - Wins)


# See how the beta distribution fits the data and calculate the prior parameters.
# Beta-Binomial distribution (mean/variance parameterisation) 
bbin.ll <- function(a, b){
    -sum(dbetabinom.ab(x = prior.data$Wins, size = prior.data$Games, shape1 = a, shape2 = b, log = TRUE))
}


mu0    <- mean(prior.data$WinRate)
sigma0 <- sd(prior.data$WinRate)
a0 <- mu0 * mean(winrate$Games)
b0 <- (1 - mu0) * mean(winrate$Games)


# Get parameters
prior.dist <- mle(minuslogl = bbin.ll, start = list(a = a0, b = b0), 
                  method = "L-BFGS-B", lower = c(1e-2, 1e-2))


prior.pars <- coef(prior.dist) %>% as.list


# Plot result
nbins <- nrow(prior.data) %>% sqrt %>% floor %>% c(20) %>% min
ggplot(prior.data, aes(x = WinRate)) + 
    geom_histogram(aes(y = ..density..), fill = "blue", colour = "black", alpha = 0.4, 
                   binwidth = 0.01) +
    stat_function(fun = function(x) dbeta(x, shape1 = prior.pars$a, shape2 = prior.pars$b),
                  colour = "red", size = 1) +
    theme_classic()



# Go ahead and make predictions anyway
prior.pars$aplusb <- prior.pars$a + prior.pars$b
winrate.us <- mutate(winrate.us, eb_WinRate = (prior.pars$a + Wins) / 
                      (prior.pars$aplusb + Games))


gather(winrate.us, Source, WinRate, WinRate, eb_WinRate) %>% 
    mutate(Source = ifelse(grepl("eb_", Source), "Predicted", "Observed")) %>% 
    ggplot(aes(x = WinRate)) + 
        geom_histogram(aes(fill = Source), colour = "black", position = "identity", 
                       alpha = 0.4, bins = 40) + 
        scale_fill_manual(values = c(Predicted = "red", Observed = "blue")) +
    theme_classic()

gather(winrate.us, Source, WinRate, WinRate, eb_WinRate) %>% 
    mutate(Source = ifelse(grepl("eb_", Source), "Predicted", "Observed")) %>% 
    ggplot(aes(x = WinRate)) + 
        geom_density(aes(colour = Source), size = 1) + 
        scale_colour_manual(values = c(Predicted = "red", Observed = "blue")) +
        theme_classic()


# Relationship (or lack thereof) between Win rate and Games played
ggplot(winrate.us, aes(x = log(Games), y = WinRate)) + 
    geom_point() + 
    geom_smooth(method = "lm", colour = "red") + 
    theme_bw()

ggplot(winrate.us, aes(x = log(Games), y = eb_WinRate)) + 
    geom_point() + 
    geom_smooth(method = "lm", colour = "red") + 
    theme_bw()



# Regression time
fit <- gamlss(cbind(Wins.approx, Losses.approx) ~ log(Games), data = winrate.us,
              family = BB(mu.link = "logit"))


tidy.fit   <- broom::tidy(fit)
winrate.us <- mutate(winrate.us, 
                     mu    = predict(fit, parameter = "mu", type = "response"),
                     sigma = predict(fit, parameter = "sigma", type = "response"), 
                     a     = mu / sigma, 
                     b     = (1/sigma) - a, 
                     eb_reg_WinRate = (a + Wins) / (a + b + Games))



# Plot of the shrinkage due to regressing WinRate on log(Games)
ggplot(winrate.us, aes(x = eb_WinRate, y = eb_reg_WinRate)) + 
    geom_point(aes(group = Games, colour = Games, size = Games)) + 
    geom_abline(intercept = 0, slope = 1, colour = "black") + 
    scale_colour_continuous(low = "blue", high = "purple") + 
    theme_bw()


# Plot of histogram overlap between WinRate and shrunken estimate
gather(winrate.us, Source, WinRate, WinRate, eb_reg_WinRate) %>% 
    mutate(Source = ifelse(grepl("eb_", Source), "Predicted", "Observed")) %>% 
    ggplot(aes(x = WinRate)) + 
        geom_histogram(aes(fill = Source), colour = "black", position = "identity", 
                       alpha = 0.4, bins = 40) + 
        scale_fill_manual(values = c(Predicted = "red", Observed = "blue")) +
        theme_classic()

gather(winrate.us, Source, WinRate, WinRate, eb_reg_WinRate) %>% 
    mutate(Source = ifelse(grepl("eb_", Source), "Predicted", "Observed")) %>% 
    ggplot(aes(x = WinRate)) + 
        geom_density(aes(colour = Source), size = 1) + 
        scale_colour_manual(values = c(Predicted = "red", Observed = "blue")) +
        theme_classic()


# Plot of WinRate histogram against Beta-Binomial model
ggplot(winrate.us) + 
    geom_histogram(aes(x = WinRate, y = ..density..), fill = "blue", colour = "black", alpha = 0.4, bins = 40) + 
    geom_density(aes(x = eb_reg_WinRate), colour = "red", size = 1) + 
    theme_classic()


## NOTES: Possible next step, Identify players that have a greater than 50% 
##  winrate, with a posterior error probability of less than 1%. See how well
##  the model applies to conquest WinRates for players in Australia and/or the
##  UK. Is there a measurable regional difference? Can you calculate a set of
##  probabilities that a player with an unknown region came from the US, UK or
##  Australia?






# Distribution of win rates for Australian and U.K. players shown against the model
# developed for the United States players
winrate <- mutate(winrate, 
                  mu    = predict(fit, newdata = winrate, parameter = "mu", type = "response"),
                  sigma = predict(fit, newdata = winrate, parameter = "sigma", type = "response"), 
                  a     = mu / sigma, 
                  b     = (1/sigma) - a, 
                  eb_reg_WinRate = (a + Wins) / (a + b + Games))


ggplot(winrate) + 
    geom_density(aes(x = eb_reg_WinRate, colour = Country), size = 1) + 
    scale_colour_manual(values = c(`United States` = "black", Australia = "blue", 
                                   `United Kingdom` = "darkgreen")) + 
    theme_classic()



dplyr::filter(winrate, Country != "United States") %>% 
    ggplot() + 
        geom_histogram(aes(x = WinRate, y = ..density..), fill = "blue", colour = "black", 
                       alpha = 0.4, bins = 29) + 
        geom_density(aes(x = eb_reg_WinRate), colour = "red", size = 1) + 
        facet_wrap(~Country) + 
        theme_classic()


ggplot(winrate, aes(x = log(Games), y = WinRate)) + 
    geom_point() + 
    geom_smooth(method = "lm", colour = "red") + 
    facet_wrap(~Country) + 
    theme_bw()


# Re-estimate parameters of experience curve
fit.aus <- dplyr::filter(winrate, Country == "Australia") %>% 
    gamlss(formula = cbind(Wins.approx, Losses.approx) ~ log(Games), data = ., 
           family = BB(mu.link = "logit"))

fit.uk <- dplyr::filter(winrate, Country == "United Kingdom") %>% 
    gamlss(formula = cbind(Wins.approx, Losses.approx) ~ log(Games), data = ., 
           family = BB(mu.link = "logit"))

tidy.fit.aus <- broom::tidy(fit.aus) %>% mutate(Country = "Aus")
tidy.fit.uk  <- broom::tidy(fit.uk) %>% mutate(Country = "UK")
tidy.fit.us  <- mutate(tidy.fit, Country = "US")

tidy.fit <- rbind(tidy.fit.aus, tidy.fit.uk, tidy.fit.us) %>% 
    dplyr::select(Country, everything())



###################################################################################
#                               Part 3: Longest Headshot                          #
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




