library(fitzRoy)
library(dplyr)
library(httr)


teamNames<-data.frame(teamId = c("CD_T10",          "CD_T20",        "CD_T30", "CD_T40",     "CD_T50",  "CD_T60",   "CD_T1000",       "CD_T70",      "CD_T1010",  "CD_T80",  "CD_T90",   "CD_T100",        "CD_T110",      "CD_T120", "CD_T130", "CD_T140",         "CD_T150",          "CD_T160"),
                      teamLong = c("Adelaide Crows","Brisbane Lions","Carlton","Collingwood","Essendon","Fremantle","Gold Coast SUNS","Geelong Cats","GWS GIANTS","Hawthorn","Melbourne","North Melbourne","Port Adelaide","Richmond","St Kilda","Western Bulldogs","West Coast Eagles","Sydney Swans"),
                      teamShort =c("ADE","BRI","CAR","COL","ESS","FRE","GCS","GEE","GWS","HAW","MEL","NTH","PRT","RIC","STK","WBD","WCE","SYD"),
                      stringsAsFactors = FALSE)
teamNames$squadId <- as.integer(gsub("CD_T","",teamNames$teamId))


convertTeamNames<-function(teams)
{
  teams<-tolower(teams)
  for(i in 1:nrow(teamNames))
  {
    teams[teams==tolower(teamNames$teamId[i])]<-teamNames$teamShort[i]
    teams[teams==tolower(teamNames$teamLong[i])]<-teamNames$teamShort[i]
    # teams[teams==teamNames$teamId[i]],c("homeTeam.teamName",
    #                              "awayTeam.teamName",
    #                              "chainTeamId",
    #                              "team")]<-teamNames$teamShort[i]
    # teams[teams==teamNames$teamLong[i],c("homeTeam.teamName",
    #                                "awayTeam.teamName",
    #                                "chainTeamId",
    #                                "team")]<-teamNames$teamLong[i]
  }

  return(teams)
}

get_token <- function() {

  response <- POST("https://api.afl.com.au/cfs/afl/WMCTok")
  token <- httr::content(response)$token

  return(token)
}

access_api <- function(url) {

  token <- get_token()

  response <- GET(
    url = url,
    add_headers("x-media-mis-token" = token))
  content <- response %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)

  return(content)
}

getTimeInPoss <- function(matchId)
{
  print(length(matchId))
  if(length(matchId) == 1)
  {
    print(matchId)

    url <- paste0("https://api.afl.com.au/cfs/afl/coach/match/",matchId,"/teamStats")


    rawData <- access_api(url)
    homePos <- rawData$possession$squadPossessionList$matchPossessionPerc[[1]]
    awayPos <- rawData$possession$squadPossessionList$matchPossessionPerc[[2]]
    disputePos <- rawData$possession$matchInDisputePerc[[1]]

    home <- rawData$matchInfo$homeSquadName
    away <- rawData$matchInfo$awaySquadName

    homeScore <- (rawData$matchInfo$homeGoals * 6) + rawData$matchInfo$homeBehinds
    awayScore <- (rawData$matchInfo$awayGoals * 6) + rawData$matchInfo$awayBehinds

    rawData$matchInfo$homeGoals

    timeInPoss <- data.frame(matchId = matchId,
                             team = c(home,away),
                             opponent = c(away,home),
                             homeTeam = c(TRUE,FALSE),
                             score = c(homeScore,awayScore),
                             oppScore = c(awayScore,homeScore),
                             timeInPoss = c(homePos,awayPos),
                             oppTimeInPoss = c(awayPos,homePos),
                             timeInDisp = disputePos)
    timeInPoss <- timeInPoss %>%
      mutate(margin = score - oppScore,
             timeInPossDiff = timeInPoss - oppTimeInPoss)





    return(timeInPoss)
  }
  else
  {
    timeInPoss <- getTimeInPoss(matchId[1])
    for(i in 2:length(matchId))
    {
      timeInPoss <- rbind(timeInPoss,getTimeInPoss(matchId[i]))
    }
    return(timeInPoss)
  }
}


test <- getTimeInPoss(c("CD_M20210142701",
                        "CD_M20220142701",
                        "CD_M20230142701"))

