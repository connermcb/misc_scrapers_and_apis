library(stringr)
library(XML)
library(xml2)
library(rvest)

players <-  read.csv("C:/Users/User 1/Downloads/project8_baseball_analysiz/baseball_data.csv")

players <- players[players$avg == 0,]

players$name <- str_to_lower(players$name)

temp <- str_split_fixed(players$name, " ", 2)

players <- cbind(temp, players)

names(players)[c(1, 2)]  <- c("first", "last")

players$position <- NA
players$atbats <- NA
players$birth <- NA
players$debut <- NA
players$last_game <- NA
players$start_roster_games <- NA

xpath <- '//*[@id="info"]' 
xpath2 <- '//*[@id="all_batting_value"]//comment()'
xpath3 <- '//*[@id="necro-birth"]/a[2]'


makeUrl <- function(n, last_first, last_init){
        url <- paste0("https://www.baseball-reference.com/players/", last_init, last_first, "0", n, ".shtml")
        return(url)
}

getWeight <- function(tables){
        meta <- tables %>%
                html_node(xpath=xpath) %>%
                html_text()
        wt <- as.integer(gsub('^.*\\s([0-9]{3})lb\\s.*', '\\1', meta))
        return(wt)
}

for(row in c(1:nrow(players))){
        n <- 1
        
        last_first <- paste0("/", substr(players[row,][["last"]], 1, 5), substr(players[row,][["first"]], 1, 2))
        last_init <- substr(players[row,][["last"]], 1, 1)
        print(row)
        url <- makeUrl(n, last_first, last_init)
        print(url)
        tables <- try(read_html(url))
        if("try-error" %in% class(tables)) {
                next
        }

        wt <- getWeight(tables)
        if (!is.na(wt)){
                while (wt != players[row, "weight"] ){
                        n <- n + 1
                        url <- makeUrl(n, last_first, last_init)
                        tables <- try(read_html(url))
                        if("try-error" %in% class(tables)) {
                                next
                        }
                        wt <- getWeight(tables)
                        print(wt)
                }
        }
        print(url)


        if (players[row, "name"] == "rob woodward" |
            players[row, "name"] == "de wayne buice"){
                print(players[row, "name"])
                print(meta)
        }

        ab <- tables %>%
                html_node(xpath=xpath2) %>%
                html_text() %>% read_html() %>%
                html_table() %>% as.data.frame()
        players[row, "atbats"] = ab[nrow(ab), "PA"]
        
        birth <- tables %>%
                html_node(xpath=xpath3) %>%
                html_text()
        players[row, "birth"] = birth

        meta <- tables %>%
                html_node(xpath=xpath) %>%
                html_text()
        
        debut <- gsub('^.*Debut:\\n\\s+([A-Za-z 0-9,]+)\\n.*', '\\1', meta)
                         # format = '%B %d, %Y')
        last <-  gsub('^.*Last Game:\\n\\s+([A-Za-z 0-9,]+)\\n.*', '\\1', meta)
                         # format = '%B %d, %Y')
        games <- gsub('^.*\\nG\\n([0-9]+)\\n+.*', '\\1', meta)
        
        pos <- gsub('^.* Position[s]?:\\n\\s+([A-Za-z ,]+)\\n.*', '\\1', meta)
        
        players[row, "position"] = pos
        players[row, "debut"] = debut
        players[row, "last_game"] = last
        players[row, "start_roster_games"] = games

        
        
}




