## This script is used to fetch the data used in seperate analysis' of 
## Battlefield 1 leaderboard statistics.
## 

# Globals
bf1URL  <- "https://battlefieldtracker.com/bf1/leaderboards/xbox/"
kd.save.file       <- "../Data/KdRatioLeaderboard.rd"
winrate.save.file  <- "../Data/WinRateLeaderboard.rd"
headshot.save.file <- "../Data/LongestHeadshotLeaderboard.rd"
winrate.conq.save.file <- "../Data/WinRateConquestLeaderboard.rd"
winrate.aus.save.file  <- "../Data/WinRateConquestLeaderboard_AUS.rd"
winrate.uk.save.file   <- "../Data/WinRateConquestLeaderboard_UK.rd"

# Make sure 'Data' directory exists
dir.create("../Data", showWarnings = FALSE)


# Attach Packages
library(httr)
library(rvest)
library(dplyr)
library(purrr)
library(magrittr)





###################################################################################
#                               Helper Functions                                  #
###################################################################################

# Crawl through each page of the leaderboard and parse the result to a data frame.
get_leaderboard <- function(platform = c("all", "xbox", "psn", "pc"),
                            stat = c("Wl", "Kd", "LongestHeadshot", "AccuracyRatio"), 
                            country = c("none", "United States", "United Kingdom", "Australia"), 
                            mode = c("none", "conquest", "rush", "domination", "operations", "frontlines"), 
                            first.page = 1, last.page = 2, 
                            save.file = NULL, load.file = NULL, ...){
    
    
    # Load save file
    if(!is.null(load.file)){
        return(readRDS(load.file))
    }
    
    # Match arguments and craft url suffix
    platform <- match.arg(platform)
    stat     <- match.arg(stat)
    country  <- match.arg(country) %>% gsub("\\s", "%20", .)
    mode     <- match.arg(mode)
    
    suffix <- if(stat == "Wl"){
        suffix <- "/Wl"
        if(mode != "none"){
            suffix <- paste(suffix, "gamemode", mode, sep = "/")
        }
        suffix
    } else if (stat == "Kd"){
        "/Kd"
    } else if (stat == "LongestHeadshot"){
        "/LongestHeadshot"
    } else if (stat == "AccuracyRatio"){
        "/AccuracyRatio"
    }
    
    if(country != "none"){
        suffix <- paste0(suffix, "?country=", country)
    }
    
    if((first.page > 1) || (last.page > first.page)){
        pages  <- seq(first.page, last.page, 1)
        suffix <- if(country != "none"){
            paste0(suffix, "&page=", pages)
        } else {
            paste0(suffix, "?page=", pages)
        }
    }
    
    
    # Retrieve webpages
    catalog <- paste0(bf1URL, suffix) %>% lapply(function(x) GET(x, ...))
    
    # NOTE: this stage may not be robust to receiving a 404 response
    status <- vapply(catalog, function(x) x$status_code, integer(1))
    
    if(any(status != 200)){
        error.codes <- status[status != 200] %>% 
            paste(collapse = ", ") %>% 
            gsub("\\,(?! .*\\,)", " &", ., perl = TRUE)
        warning("GET request returned status: ", error.codes, " for ", 
                length(which(status != 200)), " pages")
        
        catalog <- catalog[status == 200]
    }
    
    catalog <- lapply(catalog, content) %>% 
        lapply(parse_leaderboard) %>% 
        do.call("rbind", .)
    
    
    if(!is.null(save.file)){
        saveRDS(catalog, file = save.file)
    }
    
    return(catalog)
}


# Parse for the leaderboard table and convert to data frame
parse_leaderboard <- function(page){
    
    # Remove top row ('sign in' prompt)
    df <- html_table(page, fill = TRUE)
    if(length(df) == 0){
        return(NULL)
    } else if ((length(df) == 1) && !is.data.frame(df)){
        df <- df[[1]]
    } else if ((length(df) > 1) && !is.data.frame(df)){
        df <- do.call("rbind", df)
    } else {
        stop("Could not parse html table!")
    }
    
    df <- df[-1, 1:4]
    
    # Remove residual html that results from banner ads
    check.ad <- suppressWarnings(df[[1]] %>% gsub("\\,", "", .) %>% as.integer %>% is.na %>% which)
    if(length(check.ad) > 0){
        df <- df[-check.ad, ]
    }
    
    return(df)
}




###################################################################################
#                               Getting the Data                                  #
###################################################################################

# Download leaderboards
# headshot <- get_leaderboard(platform = "xbox", stat = "LongestHeadshot",
#                             country = "United States", last.page = 200,
#                             save.file = headshot.save.file)
# 
# 
# kdr <- get_leaderboard(platform = "xbox", stat = "Kd", country = "United States",
#                        last.page = 200, save.file = kd.save.file)
# 
# 
# winrate <- get_leaderboard(platform = "xbox", stat = "Wl", 
#                            country = "United States", last.page = 200,
#                            save.file = winrate.save.file)

winrate.conq <- get_leaderboard(platform = "xbox", stat = "Wl", 
                                country = "United States", mode = "conquest",
                                last.page = 200, save.file = winrate.conq.save.file)


winrate.aus <- get_leaderboard(platform = "xbox", stat = "Wl", 
                               country = "Australia", mode = "conquest", 
                               last.page = 200, save.file = winrate.aus.save.file)

winrate.uk <- get_leaderboard(platform = "xbox", stat = "Wl", 
                              country = "United Kingdom", mode = "conquest", 
                              last.page = 200, save.file = winrate.uk.save.file)
