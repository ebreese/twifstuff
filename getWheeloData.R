library(dplyr)
library(httr)
library(tidyr)
library(lubridate)
library(purrr)
library(jsonlite)
usethis::use_package("tidyr")
usethis::use_package("httr")
usethis::use_package("dplyr")
usethis::use_package("lubridate")
usethis::use_package("purrr")
usethis::use_package("jsonlite")

cache_dir <- tools::R_user_dir("baRassi", "cache")


if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)


currentYear <- as.integer(format(Sys.Date(), "%Y"))
minRound <- 0
maxRound <- 30




downloadMatchByMatchData <- function(season = currentYear,
                                round = NA,
                                forceDownload = FALSE)
{
  if(length(round) == 1 && is.na(round))
    round <- c(minRound:maxRound)
  yearRoundCombos <-
    expand.grid(season=season,round=round) %>%
    mutate(roundId = paste0(season,sprintf("%02d",round)))

  base_url <- "https://www.wheeloratings.com/src/match_stats/table_data"
  downloaded <- 0
  skipped <- 0
  failed <- 0
  RoundIDs <- yearRoundCombos$roundId
  for (RoundID in RoundIDs) {

      filename <- paste0(RoundID, ".json")
      url <- paste0(base_url, "/", filename)
      dest <- file.path(cache_dir, filename)

      # Skip if already downloaded
      if (file.exists(dest) & !forceDownload) {
        message("Skipping (exists): ", filename)
        skipped <- skipped + 1
        next
      }

      # Attempt download
      response <- tryCatch(
        GET(url, timeout(10)),
        error = function(e) NULL
      )

      if (!is.null(response) && status_code(response) == 200) {
        writeBin(content(response, "raw"), dest)
        message("Downloaded: ", filename)
        downloaded <- downloaded + 1
      } else {
        message("Not found (skipping): ", filename)
        failed <- failed + 1
      }

      Sys.sleep(0.2)  # Be polite — small delay between requests
    }


  message("\nDone! Downloaded: ", downloaded, " | Skipped: ", skipped, " | Not found: ", failed)

  #return(yearRoundCombos)
}

fetch_wheelo_playerMatchData <- function(season = currentYear,
                                         round = NA,
                                         forceDownload = FALSE)
{
  downloadMatchByMatchData(season,round,forceDownload)

  if(length(round) == 1 && is.na(round))
    round <- c(minRound:maxRound)
  yearRoundCombos <-
    expand.grid(season=season,round=round) %>%
    mutate(roundId = paste0(season,sprintf("%02d",round))) %>%
    mutate(filename = paste0(roundId,".json"))

  files <- list.files(cache_dir, pattern = "\\.json$", full.names = FALSE)
  files <- keep(files, ~ any(.x %in% yearRoundCombos$filename))
  files <- paste0(cache_dir,"/",files)

  for(i in 1:length(files))
  {
    filename <- files[i]

    allData <- fromJSON(filename)
    newTeamData <- allData$TeamData %>% unnest(cols=colnames(allData$TeamData))
    newMatchData <- allData$Matches %>% unnest(cols=colnames(allData$Matches))
    newPlayerData <- allData$Data %>% unnest(cols=colnames(allData$Data))

    opponents <- newMatchData %>%
      dplyr::select(MatchId, Team = HomeTeam, Opponent = AwayTeam) %>%
      rbind(newMatchData %>% dplyr::select(MatchId, Team = AwayTeam, Opponent = HomeTeam))

    newTeamData <- newTeamData %>% left_join(opponents,by=c("MatchId","Team"))
    newPlayerData <- newPlayerData %>% left_join(opponents,by=c("MatchId","Team"))
    #print(colnames(newPlayerData))
    if(i == 1)
    {
      teamData <- newTeamData
      matchData <- newMatchData
      playerData <- newPlayerData
    }
    else
    {
      teamData <- bind_rows(teamData,newTeamData)
      matchData <- bind_rows(matchData,newMatchData)
      playerData <- bind_rows(playerData,newPlayerData)
    }
  }
  return(playerData)
}

downloadPlayerSeasonData <- function(season = currentYear,
                                     forceDownload = FALSE)
{


  base_url <- "https://www.wheeloratings.com/src/afl_stats/player_stats/afl"
  downloaded <- 0
  skipped <- 0
  failed <- 0
  for (seasonId in season) {

    sourceFilename <- paste0(seasonId, ".json")
    destFilename <- paste0("players",seasonId,".json")
    url <- paste0(base_url, "/", sourceFilename)
    print(url)
    dest <- file.path(cache_dir, destFilename)

    # Skip if already downloaded
    if (file.exists(dest) & !forceDownload) {
      message("Skipping (exists): ", sourceFilename)
      skipped <- skipped + 1
      next
    }

    # Attempt download
    response <- tryCatch(
      GET(url, timeout(10)),
      error = function(e) NULL
    )

    if (!is.null(response) && status_code(response) == 200) {
      writeBin(content(response, "raw"), dest)
      message("Downloaded: ", sourceFilename)
      downloaded <- downloaded + 1
    } else {
      message("Not found (skipping): ", sourceFilename)
      failed <- failed + 1
    }

    Sys.sleep(0.2)  # Be polite — small delay between requests
  }


  message("\nDone! Downloaded: ", downloaded, " | Skipped: ", skipped, " | Not found: ", failed)

  #return(yearRoundCombos)
}

fetch_wheelo_playerSeasonData <- function(season = currentYear,
                                         forceDownload = FALSE)
{
  downloadPlayerSeasonData(season,forceDownload)

  seasonFiles <- paste0("players",season,".json")

  files <- list.files(cache_dir, pattern = "\\.json$", full.names = FALSE)
  files <- keep(files, ~ any(.x %in% seasonFiles))
  files <- paste0(cache_dir,"/",files)

  for(i in 1:length(files))
  {
    filename <- files[i]

    allData <- fromJSON(filename)
    newSeasonData <- as.data.frame(allData$Data)
    newSeasonData$season <- season[i]
    #newMatchData <- allData$Matches %>% unnest(cols=colnames(allData$Matches))
    #newPlayerData <- allData$Data %>% unnest(cols=colnames(allData$Data))

    #opponents <- newMatchData %>%
    #  dplyr::select(MatchId, Team = HomeTeam, Opponent = AwayTeam) %>%
    #  rbind(newMatchData %>% dplyr::select(MatchId, Team = AwayTeam, Opponent = HomeTeam))

    #newTeamData <- newTeamData %>% left_join(opponents,by=c("MatchId","Team"))
    #newPlayerData <- newPlayerData %>% left_join(opponents,by=c("MatchId","Team"))
    #print(colnames(newPlayerData))
    if(i == 1)
    {
      seasonData <- newSeasonData
      #matchData <- newMatchData
      #playerData <- newPlayerData
    }
    else
    {
      seasonData <- bind_rows(seasonData,newSeasonData)
      #matchData <- bind_rows(matchData,newMatchData)
      #playerData <- bind_rows(playerData,newPlayerData)
    }
  }
  return(seasonData)
}

fetch_wheelo_teamMatchData <- function(season = currentYear,
                                         round = NA,
                                         forceDownload = FALSE)
{
  downloadMatchByMatchData(season,round,forceDownload)

  if(length(round) == 1 && is.na(round))
    round <- c(minRound:maxRound)
  yearRoundCombos <-
    expand.grid(season=season,round=round) %>%
    mutate(roundId = paste0(season,sprintf("%02d",round))) %>%
    mutate(filename = paste0(roundId,".json"))

  files <- list.files(cache_dir, pattern = "\\.json$", full.names = FALSE)
  files <- keep(files, ~ any(.x %in% yearRoundCombos$filename))
  files <- paste0(cache_dir,"/",files)

  for(i in 1:length(files))
  {
    filename <- files[i]

    allData <- fromJSON(filename)
    newTeamData <- allData$TeamData %>% unnest(cols=colnames(allData$TeamData))
    newMatchData <- allData$Matches %>% unnest(cols=colnames(allData$Matches))
    newPlayerData <- allData$Data %>% unnest(cols=colnames(allData$Data))

    opponents <- newMatchData %>%
      dplyr::select(MatchId, Team = HomeTeam, Opponent = AwayTeam) %>%
      rbind(newMatchData %>% dplyr::select(MatchId, Team = AwayTeam, Opponent = HomeTeam))

    newTeamData <- newTeamData %>% left_join(opponents,by=c("MatchId","Team"))
    newPlayerData <- newPlayerData %>% left_join(opponents,by=c("MatchId","Team"))
    #print(colnames(newPlayerData))
    if(i == 1)
    {
      teamData <- newTeamData
      matchData <- newMatchData
      playerData <- newPlayerData
    }
    else
    {
      teamData <- bind_rows(teamData,newTeamData)
      matchData <- bind_rows(matchData,newMatchData)
      playerData <- bind_rows(playerData,newPlayerData)
    }
  }
  return(teamData)
}

fetch_wheelo_matchInfo <- function(season = currentYear,
                                       round = NA,
                                       forceDownload = FALSE)
{
  downloadMatchByMatchData(season,round,forceDownload)

  if(length(round) == 1 && is.na(round))
    round <- c(minRound:maxRound)
  yearRoundCombos <-
    expand.grid(season=season,round=round) %>%
    mutate(roundId = paste0(season,sprintf("%02d",round))) %>%
    mutate(filename = paste0(roundId,".json"))

  files <- list.files(cache_dir, pattern = "\\.json$", full.names = FALSE)
  files <- keep(files, ~ any(.x %in% yearRoundCombos$filename))
  files <- paste0(cache_dir,"/",files)

  for(i in 1:length(files))
  {
    filename <- files[i]

    allData <- fromJSON(filename)
    newTeamData <- allData$TeamData %>% unnest(cols=colnames(allData$TeamData))
    newMatchData <- allData$Matches %>% unnest(cols=colnames(allData$Matches))
    newPlayerData <- allData$Data %>% unnest(cols=colnames(allData$Data))

    opponents <- newMatchData %>%
      dplyr::select(MatchId, Team = HomeTeam, Opponent = AwayTeam) %>%
      rbind(newMatchData %>% dplyr::select(MatchId, Team = AwayTeam, Opponent = HomeTeam))

    newTeamData <- newTeamData %>% left_join(opponents,by=c("MatchId","Team"))
    newPlayerData <- newPlayerData %>% left_join(opponents,by=c("MatchId","Team"))
    #print(colnames(newPlayerData))
    if(i == 1)
    {
      teamData <- newTeamData
      matchData <- newMatchData
      playerData <- newPlayerData
    }
    else
    {
      teamData <- bind_rows(teamData,newTeamData)
      matchData <- bind_rows(matchData,newMatchData)
      playerData <- bind_rows(playerData,newPlayerData)
    }
  }
  return(matchData)
}

#teamMatchData <- fetch_wheelo_teamMatchData(c(2017:2026))
#playerMatchData <- fetch_wheelo_playerMatchData(c(2017:2026))
#playerSeasonData <- fetch_wheelo_playerSeasonData(c(2017:2026))

