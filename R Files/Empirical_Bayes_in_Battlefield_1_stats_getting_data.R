## This script is used to fetch the data used in seperate analysis' of 
## Battlefield 1 leaderboard statistics.
## 

# Globals
bf1URL <- "https://battlefieldtracker.com/bf1/leaderboards/xbox/"
kd.save.file       <- "../Data/KdRatioLeaderboard.rd"
winrate.save.file  <- "../Data/WinRateLeaderboard.rd"
headshot.save.file <- "../Data/LongestHeadshotLeaderboard.rd"
country            <- "United States"


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
get_leaderboard <- function(url, first.page = 1, last.page = 2, region = NULL, 
                            save.file = NULL, load.file = NULL, ...){
    
    if(!is.null(load.file)){
        return(readRDS(load.file))
    }
    
    query <- if(!is.null(region)){
        region <- gsub("\\s", "%20", region)
        seq(first.page, last.page, 1) %>% 
            paste0("?country=", region, "&page=", .)
    } else { 
        seq(first.page, last.page, 1) %>% 
            paste0("?page=", .)
    }
    
    
    catalog <- paste0(url, query) %>% lapply(function(x) GET(x, ...))
    
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
headshot <- paste0(bf1URL, "LongestHeadshot") %>% 
    get_leaderboard(last.page = 200, region = country, 
                    save.file = headshot.save.file)


kdr <- paste0(bf1URL, "Kd") %>% 
    get_leaderboard(last.page = 200, region = country, 
                    save.file = kd.save.file)


winrate <- paste0(bf1URL, "Wl") %>% 
    get_leaderboard(last.page = 200, region = country,
                    save.file = winrate.save.file)


