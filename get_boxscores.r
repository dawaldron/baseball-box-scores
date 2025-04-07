Sys.setlocale("LC_ALL", "en_US.UTF-8")
library(here)
library(data.table)
library(magrittr)
library(rvest)
library(httr)
library(jsonlite)
library(stringr)
library(baseballr)
library(chromote)



formatBoxName <- function(x) {
  c_bn2 <- 
    ifelse(grepl(', ', x, fixed = TRUE),
           paste0(sapply(strsplit(x, ', '), '[', 2), ''),
           '') %>%
    paste0(sapply(strsplit(x, ', '), '[', 1), '') %>%
    gsub(' ', '', ., fixed = TRUE) %>%
    gsub('.', '', ., fixed = TRUE)
  return(c_bn2)
}

#' Process all games for a given date
#' 
#' @param year Year (YYYY)
#' @param month Month (MM)
#' @param day Day (DD)
#' @param season Season year
#' @return List of all games' data
process_all_gamesMLB <- function(year, month, day) {
  
  # Get all games for the specified date
  games <- GET(paste0('http://statsapi.mlb.com/api/v1/schedule/games/?sportId=1&gameType=R&date=', year, '-', month, '-', day)) %>%
    content(as = 'text') %>%
    fromJSON()
  
  # Process each game's box score
  process_game_box_scoreMLB <- function(gameId) {
    print(gameId)
    resp_box <- GET(paste0('http://statsapi.mlb.com/api/v1/game/', gameId, '/boxscore')) %>%
      content(as = 'text') %>%
      fromJSON()
    
    resp_line <- GET(paste0('http://statsapi.mlb.com/api/v1/game/', gameId, '/linescore')) %>%
      content(as = 'text') %>%
      fromJSON()
    
    resp_teamA <- GET(paste0('http://statsapi.mlb.com/', resp_box$teams$away$team$link)) %>%
      content(as = 'text') %>%
      fromJSON()
    
    c_franchiseNameA <- resp_teamA$teams$franchiseName
    
    resp_teamH <- GET(paste0('http://statsapi.mlb.com/', resp_box$teams$home$team$link)) %>%
      content(as = 'text') %>%
      fromJSON()
    
    c_franchiseNameH <- resp_teamH$teams$franchiseName
    
    dt_bnMappingA <- lapply(resp_box$teams$away$players, function(x) {
      resp_p <- GET(paste0('http://statsapi.mlb.com/api/v1/people/', x[['person']]$id)) %>%
        content(as = 'text') %>%
        fromJSON()
      
      c_bn <- resp_p$people$boxscoreName
      c_bn2 <- formatBoxName(c_bn)
      
      data.table(
        personID = x[['person']]$id,
        boxscoreName = c_bn,
        boxscoreName2 = c_bn2
      )
    }) %>%
      rbindlist()
    
    dt_abat <- lapply(resp_box$teams$away$batters, function(x) {
      resp_p <- GET(paste0('http://statsapi.mlb.com/api/v1/people/', x)) %>%
        content(as = 'text') %>%
        fromJSON()
      
      c_bn <- resp_p$people$boxscoreName
      c_bn2 <- formatBoxName(c_bn)
      
      playerData <- resp_box$teams$away$players[[paste0('ID', x)]]
      
      data.table(
        Note = ifelse(is.null(playerData$stats$batting$note),'',playerData$stats$batting$note),
        Player = c_bn2,
        Position = playerData$allPositions$abbreviation %>% tolower() %>% paste0(collapse='-'),
        AB =  playerData$stats$batting$atBats,
        H = playerData$stats$batting$hits,
        R = playerData$stats$batting$runs,
        BI = playerData$stats$batting$rbi,
        BI.s = playerData$seasonStats$batting$rbi,
        BB = playerData$stats$batting$baseOnBalls,
        SO = playerData$stats$batting$strikeOuts,
        Avg = playerData$seasonStats$batting$avg,
        `2B` = playerData$stats$batting$doubles,
        `2B.s` = playerData$seasonStats$batting$doubles,
        `3B` = playerData$stats$batting$triples,
        `3B.s` = playerData$seasonStats$batting$triples,
        `HR` = playerData$stats$batting$homeRuns,
        `HR.s` = playerData$seasonStats$batting$homeRuns,
        S = playerData$stats$batting$sacBunts,
        SF = playerData$stats$batting$sacFlies,
        GIDP = playerData$stats$batting$groundIntoDoublePlay,
        GITP = playerData$stats$batting$groundIntoTriplePlay)
    }) %>%
      rbindlist(fill = TRUE) %>%
      .[!is.na(AB)]
    
    if (length(resp_box$teams$away$note) > 0) {
      dt_abatNote1 <- resp_box$teams$away$note %>%
        data.table() %>%
        .[, paste0(label, '-', value)] %>%
        paste0(collapse = ' ')
    } else {
      dt_abatNote1 <- ''
    }
    
    dt_abatNote2 <- lapply(resp_box$teams$away$players, function(x) {
      resp_p <- GET(paste0('http://statsapi.mlb.com/api/v1/people/', x[['person']]$id)) %>%
        content(as = 'text') %>%
        fromJSON()
      
      c_bn <- resp_p$people$boxscoreName
      c_bn2 <- formatBoxName(c_bn)
      
      data.table(
        Player = c_bn2,
        E = x$stats$fielding$errors,
        E.s = x$seasonStats$fielding$errors,
        SB = x$stats$batting$stolenBases,
        SB.s = x$seasonStats$batting$stolenBases,
        CS = x$stats$batting$caughtStealing,
        CS.s = x$seasonStats$batting$caughtStealing)
    }) %>%
      rbindlist(fill = TRUE) %>%
      .[,
        .(E = ifelse(!is.na(E) & E > 0, paste0(Player, ifelse(E > 1, paste0(' ', E), ''), ' (', E.s, ')'), ''),
          SB = ifelse(!is.na(SB) & SB > 0, paste0(Player, ifelse(SB > 1, paste0(' ', SB), ''), ' (', SB.s, ')'), ''),
          CS = ifelse(!is.na(CS) & CS > 0, paste0(Player, ifelse(CS > 1, paste0(' ', CS), ''), ' (', CS.s, ')'), ''))]
    
    dt_abatNote3 <- dt_abat %>%
      .[,
        .(`2B` = ifelse(!is.na(`2B`) & `2B` > 0, paste0(Player, ifelse(`2B` > 1, paste0(' ', `2B`), ''), ' (', `2B.s`, ')'), ''),
          `3B` = ifelse(!is.na(`3B`) & `3B` > 0, paste0(Player, ifelse(`3B` > 1, paste0(' ', `3B`), ''), ' (', `3B.s`, ')'), ''),
          `HR` = ifelse(!is.na(HR) & HR > 0, paste0(Player, ifelse(HR > 1, paste0(' ', HR), ''), ' (', `HR.s`, ')'), ''),
          `RBI` = ifelse(!is.na(BI) & BI > 0, paste0(Player, ifelse(BI > 1, paste0(' ', BI), ''), ' (', `BI.s`, ')'), ''),
          S = ifelse(!is.na(S) & S > 0, paste0(Player, ifelse(S > 1, paste0(' ', S), '')), ''),
          SF = ifelse(!is.na(S) & SF > 0, paste0(Player, ifelse(SF > 1, paste0(' ', SF), '')), ''),
          GIDP = ifelse(!is.na(GIDP) & GIDP > 0, paste0(Player, ifelse(GIDP > 1, paste0(' ', GIDP), '')), ''),
          GITP = ifelse(!is.na(GITP) & GITP > 0, paste0(Player, ifelse(GITP > 1, paste0(' ', GITP), '')), ''))]
    
    abatIndex <- which(resp_box$teams$away$info$title == 'BATTING')
    afieldIndex <- which(resp_box$teams$away$info$title == 'FIELDING')
    
    dt_abatNote4 <- data.table(
      LOB = ifelse(length(abatIndex) > 0,
                   resp_box$teams$away$info$fieldList[[abatIndex]] %>%
                     data.table() %>%
                     .[label == 'Team LOB', substr(value, 1, nchar(value) - 1)],
                   ''),
      DP = ifelse(length(afieldIndex) > 0,
                  resp_box$teams$away$info$fieldList[[afieldIndex]] %>%
                    data.table() %>%
                    .[label == 'DP', substr(value, 1, nchar(value) - 1)],
                  '')
    )
    
    dt_abatNote4[is.na(dt_abatNote4)] <- ''
    
    for (i in 1:nrow(dt_bnMappingA)) {
      dt_abatNote4[, DP := gsub(dt_bnMappingA[i, boxscoreName], dt_bnMappingA[i, boxscoreName2], DP, fixed = TRUE)]
    }
    
    dt_abatBox <- dt_abat %>%
      .[,
        .(Player = paste0(Note, Player, ' ', Position),
          AB, H, R, BI, BB, SO, Avg)] %>%
      rbind(dt_abat[,
                    .(Player = 'Totals',
                      AB = sum(AB),
                      H = sum(H),
                      R = sum(R),
                      BI = sum(BI),
                      BB = sum(BB),
                      SO = sum(SO),
                      Avg = '')])
    
    setnames(dt_abatBox, 'Player', c_franchiseNameA)
    
    dt_aStats <- data.table(
      R = resp_box$teams$away$teamStats$batting$runs,
      H = resp_box$teams$away$teamStats$batting$hits,
      E = resp_box$teams$away$teamStats$fielding$errors
    ) %>%
      t()
    
    dt_abatNote <- data.table(
      footnotes = dt_abatNote1,
      E = dt_abatNote2[E != '', paste0(E, collapse = ', ')],
      LOB = paste0(c_franchiseNameA, ' ', dt_abatNote4$LOB),
      `2B` = dt_abatNote3[`2B` != '', paste0(`2B`, collapse = ', ')],
      `3B` = dt_abatNote3[`3B` != '', paste0(`3B`, collapse = ', ')],
      HR = dt_abatNote3[HR != '', paste0(HR, collapse = ', ')],
      RBI = dt_abatNote3[RBI != '', paste0(RBI, collapse = ', ')],
      SB = dt_abatNote2[SB != '', paste0(SB, collapse = ', ')],
      CS = dt_abatNote2[CS != '', paste0(CS, collapse = ', ')],
      S = dt_abatNote3[S != '', paste0(S, collapse = ', ')],
      SF = dt_abatNote3[S != '', paste0(SF, collapse = ', ')],
      GIDP = dt_abatNote3[GIDP != '', paste0(GIDP, collapse = ', ')],
      GITP = dt_abatNote3[GITP != '', paste0(GITP, collapse = ', ')],
      DP = ifelse(dt_abatNote4$DP != '', paste0(c_franchiseNameA, ' ', dt_abatNote4$DP), '')
    )
    
    dt_bnMappingH <- lapply(resp_box$teams$home$players, function(x) {
      resp_p <- GET(paste0('http://statsapi.mlb.com/api/v1/people/', x[['person']]$id)) %>%
        content(as = 'text') %>%
        fromJSON()
      
      c_bn <- resp_p$people$boxscoreName
      c_bn2 <- formatBoxName(c_bn)
      
      data.table(
        personID = x[['person']]$id,
        boxscoreName = c_bn,
        boxscoreName2 = c_bn2
      )
    }) %>%
      rbindlist()
    
    dt_hbat <- lapply(resp_box$teams$home$batters, function(x) {
      resp_p <- GET(paste0('http://statsapi.mlb.com/api/v1/people/', x)) %>%
        content(as = 'text') %>%
        fromJSON()
      
      c_bn <- resp_p$people$boxscoreName
      c_bn2 <- formatBoxName(c_bn)
      
      playerData <- resp_box$teams$home$players[[paste0('ID', x)]]
      
      data.table(
        Note = ifelse(is.null(playerData$stats$batting$note),'',playerData$stats$batting$note),
        Player = c_bn2,
        Position = playerData$allPositions$abbreviation %>% tolower() %>% paste0(collapse='-'),
        AB =  playerData$stats$batting$atBats,
        H = playerData$stats$batting$hits,
        R = playerData$stats$batting$runs,
        BI = playerData$stats$batting$rbi,
        BI.s = playerData$seasonStats$batting$rbi,
        BB = playerData$stats$batting$baseOnBalls,
        SO = playerData$stats$batting$strikeOuts,
        Avg = playerData$seasonStats$batting$avg,
        `2B` = playerData$stats$batting$doubles,
        `2B.s` = playerData$seasonStats$batting$doubles,
        `3B` = playerData$stats$batting$triples,
        `3B.s` = playerData$seasonStats$batting$triples,
        `HR` = playerData$stats$batting$homeRuns,
        `HR.s` = playerData$seasonStats$batting$homeRuns,
        S = playerData$stats$batting$sacBunts,
        SF = playerData$stats$batting$sacFlies,
        GIDP = playerData$stats$batting$groundIntoDoublePlay,
        GITP = playerData$stats$batting$groundIntoTriplePlay)
    }) %>%
      rbindlist(fill = TRUE) %>%
      .[!is.na(AB)]
    
    if (length(resp_box$teams$home$note) > 0) {
      dt_hbatNote1 <- resp_box$teams$home$note %>%
        data.table() %>%
        .[, paste0(label, '-', value)] %>%
        paste0(collapse = ' ')
    } else {
      dt_hbatNote1 <- ''
    }
    
    dt_hbatNote2 <- lapply(resp_box$teams$home$players, function(x) {
      resp_p <- GET(paste0('http://statsapi.mlb.com/api/v1/people/', x[['person']]$id)) %>%
        content(as = 'text') %>%
        fromJSON()
      
      c_bn <- resp_p$people$boxscoreName
      c_bn2 <- formatBoxName(c_bn)
      
      data.table(
        Player = c_bn2,
        E = x$stats$fielding$errors,
        E.s = x$seasonStats$fielding$errors,
        SB = x$stats$batting$stolenBases,
        SB.s = x$seasonStats$batting$stolenBases,
        CS = x$stats$batting$caughtStealing,
        CS.s = x$seasonStats$batting$caughtStealing)
    }) %>%
      rbindlist(fill = TRUE) %>%
      .[,
        .(E = ifelse(!is.na(E) & E > 0, paste0(Player, ifelse(E > 1, paste0(' ', E), ''), ' (', E.s, ')'), ''),
          SB = ifelse(!is.na(SB) & SB > 0, paste0(Player, ifelse(SB > 1, paste0(' ', SB), ''), ' (', SB.s, ')'), ''),
          CS = ifelse(!is.na(CS) & CS > 0, paste0(Player, ifelse(CS > 1, paste0(' ', CS), ''), ' (', CS.s, ')'), ''))]
    
    dt_hbatNote3 <- dt_hbat %>%
      .[,
        .(`2B` = ifelse(!is.na(`2B`) & `2B` > 0, paste0(Player, ifelse(`2B` > 1, paste0(' ', `2B`), ''), ' (', `2B.s`, ')'), ''),
          `3B` = ifelse(!is.na(`3B`) & `3B` > 0, paste0(Player, ifelse(`3B` > 1, paste0(' ', `3B`), ''), ' (', `3B.s`, ')'), ''),
          `HR` = ifelse(!is.na(HR) & HR > 0, paste0(Player, ifelse(HR > 1, paste0(' ', HR), ''), ' (', `HR.s`, ')'), ''),
          `RBI` = ifelse(!is.na(BI) & BI > 0, paste0(Player, ifelse(BI > 1, paste0(' ', BI), ''), ' (', `BI.s`, ')'), ''),
          S = ifelse(!is.na(S) & S > 0, paste0(Player, ifelse(S > 1, paste0(' ', S), '')), ''),
          SF = ifelse(!is.na(S) & SF > 0, paste0(Player, ifelse(SF > 1, paste0(' ', SF), '')), ''),
          GIDP = ifelse(!is.na(GIDP) & GIDP > 0, paste0(Player, ifelse(GIDP > 1, paste0(' ', GIDP), '')), ''),
          GITP = ifelse(!is.na(GITP) & GITP > 0, paste0(Player, ifelse(GITP > 1, paste0(' ', GITP), '')), ''))]
    
    hbatIndex <- which(resp_box$teams$home$info$title == 'BATTING')
    hfieldIndex <- which(resp_box$teams$home$info$title == 'FIELDING')
    
    dt_hbatNote4 <- data.table(
      LOB = ifelse(length(hbatIndex) > 0,
                   resp_box$teams$home$info$fieldList[[hbatIndex]] %>%
                     data.table() %>%
                     .[label == 'Team LOB', substr(value, 1, nchar(value) - 1)],
                   ''),
      DP = ifelse(length(hfieldIndex) > 0,
                   resp_box$teams$home$info$fieldList[[hfieldIndex]] %>%
                     data.table() %>%
                     .[label == 'DP', substr(value, 1, nchar(value) - 1)],
                   '')
    )
    
    dt_hbatNote4[is.na(dt_hbatNote4)] <- ''
    
    for (i in 1:nrow(dt_bnMappingH)) {
      dt_hbatNote4[, DP := gsub(dt_bnMappingH[i, boxscoreName], dt_bnMappingH[i, boxscoreName2], DP, fixed = TRUE)]
    }
    
    dt_hbatBox <- dt_hbat %>%
      .[,
        .(Player = paste0(Note, Player, ' ', Position),
          AB, H, R, BI, BB, SO, Avg)] %>%
      rbind(dt_hbat[,
                    .(Player = 'Totals',
                      AB = sum(AB),
                      H = sum(H),
                      R = sum(R),
                      BI = sum(BI),
                      BB = sum(BB),
                      SO = sum(SO),
                      Avg = '')])
    
    setnames(dt_hbatBox, 'Player', c_franchiseNameH)
    
    dt_hStats <- data.table(
      R = resp_box$teams$home$teamStats$batting$runs,
      H = resp_box$teams$home$teamStats$batting$hits,
      E = resp_box$teams$home$teamStats$fielding$errors
    ) %>%
      t()
    
    dt_hbatNote <- data.table(
      footnotes = dt_hbatNote1,
      E = dt_hbatNote2[E != '', paste0(E, collapse = ', ')],
      LOB = paste0(c_franchiseNameH, ' ', dt_hbatNote4$LOB),
      `2B` = dt_hbatNote3[`2B` != '', paste0(`2B`, collapse = ', ')],
      `3B` = dt_hbatNote3[`3B` != '', paste0(`3B`, collapse = ', ')],
      HR = dt_hbatNote3[HR != '', paste0(HR, collapse = ', ')],
      RBI = dt_hbatNote3[RBI != '', paste0(RBI, collapse = ', ')],
      SB = dt_hbatNote2[SB != '', paste0(SB, collapse = ', ')],
      CS = dt_hbatNote2[CS != '', paste0(CS, collapse = ', ')],
      S = dt_hbatNote3[S != '', paste0(S, collapse = ', ')],
      SF = dt_hbatNote3[S != '', paste0(SF, collapse = ', ')],
      GIDP = dt_hbatNote3[GIDP != '', paste0(GIDP, collapse = ', ')],
      GITP = dt_hbatNote3[GITP != '', paste0(GITP, collapse = ', ')],
      DP = ifelse(dt_hbatNote4$DP != '', paste0(c_franchiseNameH, ' ', dt_hbatNote4$DP), '')
    )
    
    dt_batNote <- 
      c(
        ifelse(dt_abatNote$E != '' | dt_hbatNote$E != '', paste0('<b>E:</b> ', paste0(c(dt_abatNote$E,dt_hbatNote$E) %>% .[. != ''], collapse = ', ')), ''),
        ifelse(dt_abatNote$LOB != '' | dt_hbatNote$LOB != '', paste0('<b>LOB:</b> ', paste0(c(dt_abatNote$LOB,dt_hbatNote$LOB) %>% .[. != ''], collapse = ', ')), ''),
        ifelse(dt_abatNote$`2B` != '' | dt_hbatNote$`2B` != '', paste0('<b>2B:</b> ', paste0(c(dt_abatNote$`2B`,dt_hbatNote$`2B`) %>% .[. != ''], collapse = ', ')), ''),
        ifelse(dt_abatNote$`3B` != '' | dt_hbatNote$`3B` != '', paste0('<b>3B:</b> ', paste0(c(dt_abatNote$`3B`,dt_hbatNote$`3B`) %>% .[. != ''], collapse = ', ')), ''),
        ifelse(dt_abatNote$HR != '' | dt_hbatNote$HR != '', paste0('<b>HR:</b> ', paste0(c(dt_abatNote$HR,dt_hbatNote$HR) %>% .[. != ''], collapse = ', ')), ''),
        ifelse(dt_abatNote$RBI != '' | dt_hbatNote$RBI != '', paste0('<b>RBIs:</b> ', paste0(c(dt_abatNote$RBI,dt_hbatNote$RBI) %>% .[. != ''], collapse = ', ')), ''),
        ifelse(dt_abatNote$SB != '' | dt_hbatNote$SB != '', paste0('<b>SB:</b> ', paste0(c(dt_abatNote$SB,dt_hbatNote$SB) %>% .[. != ''], collapse = ', ')), ''),
        ifelse(dt_abatNote$CS != '' | dt_hbatNote$CS != '', paste0('<b>CS:</b> ', paste0(c(dt_abatNote$CS,dt_hbatNote$CS) %>% .[. != ''], collapse = ', ')), ''),
        ifelse(dt_abatNote$S != '' | dt_hbatNote$S != '', paste0('<b>SB:</b> ', paste0(c(dt_abatNote$S,dt_hbatNote$S) %>% .[. != ''], collapse = ', ')), ''),
        ifelse(dt_abatNote$SF != '' | dt_hbatNote$SF != '', paste0('<b>SF:</b> ', paste0(c(dt_abatNote$SF,dt_hbatNote$SF) %>% .[. != ''], collapse = ', ')), ''),
        ifelse(dt_abatNote$GIDP != '' | dt_hbatNote$GIDP != '', paste0('<b>GIDP:</b> ', paste0(c(dt_abatNote$GIDP,dt_hbatNote$GIDP) %>% .[. != ''], collapse = ', ')), ''),
        ifelse(dt_abatNote$GITP != '' | dt_hbatNote$GITP != '', paste0('<b>GITP:</b> ', paste0(c(dt_abatNote$GITP,dt_hbatNote$GITP) %>% .[. != ''], collapse = ', ')), ''),
        ifelse(dt_abatNote$DP != '' | dt_hbatNote$DP != '', paste0('<b>DP:</b> ', paste0(c(dt_abatNote$DP,dt_hbatNote$DP) %>% .[. != ''], collapse = ', ')), '')
      ) %>%
      .[. != ''] %>%
      paste0(collapse = '. ') %>%
      paste0(ifelse(dt_abatNote$footnote != '' | dt_hbatNote$footnote != '', paste0(paste0(c(dt_abatNote$footnote,dt_hbatNote$footnote) %>% .[. != ''], collapse = ', '),'<br>'), ''),
             .,
             '.')
    
    
    
    dt_apitch <- lapply(resp_box$teams$away$pitchers, function(x) {
      resp_p <- GET(paste0('http://statsapi.mlb.com/api/v1/people/', x)) %>%
        content(as = 'text') %>%
        fromJSON()
      
      c_bn <- resp_p$people$boxscoreName
      c_bn2 <- formatBoxName(c_bn)
      
      playerData <- resp_box$teams$away$players[[paste0('ID', x)]]
      
      data.table(
        Player = paste0(c_bn2, ifelse(is.null(playerData$stats$pitching$note),'', paste0(' ',playerData$stats$pitching$note))),
        IP =  playerData$stats$pitching$inningsPitched,
        H = playerData$stats$pitching$hits,
        R = playerData$stats$pitching$runs,
        ER = playerData$stats$pitching$earnedRuns,
        BB = playerData$stats$pitching$baseOnBalls,
        SO = playerData$stats$pitching$strikeOuts,
        NP = playerData$stats$pitching$numberOfPitches,
        ERA = playerData$seasonStats$pitching$era)
    }) %>%
      rbindlist(fill = TRUE)
    
    setnames(dt_apitch, 'Player', c_franchiseNameA)
    
    
    dt_hpitch <- lapply(resp_box$teams$home$pitchers, function(x) {
      resp_p <- GET(paste0('http://statsapi.mlb.com/api/v1/people/', x)) %>%
        content(as = 'text') %>%
        fromJSON()
      
      c_bn <- resp_p$people$boxscoreName
      c_bn2 <- formatBoxName(c_bn)
      
      playerData <- resp_box$teams$home$players[[paste0('ID', x)]]
      
      data.table(
        Player = paste0(c_bn2, ifelse(is.null(playerData$stats$pitching$note),'', paste0(' ',playerData$stats$pitching$note))),
        IP =  playerData$stats$pitching$inningsPitched,
        H = playerData$stats$pitching$hits,
        R = playerData$stats$pitching$runs,
        ER = playerData$stats$pitching$earnedRuns,
        BB = playerData$stats$pitching$baseOnBalls,
        SO = playerData$stats$pitching$strikeOuts,
        NP = playerData$stats$pitching$numberOfPitches,
        ERA = playerData$seasonStats$pitching$era)
    }) %>%
      rbindlist(fill = TRUE)
    
    setnames(dt_hpitch, 'Player', c_franchiseNameH)
    
    dt_pitchNote <- resp_box$info %>%
      data.table() %>%
      .[label %in% c('IBB','HBP','WP')] %>%
      .[, ifelse(length(label)==0, '', paste0('<b>', label, ':</b> ', value))] %>%
      paste0(collapse = ' ')
    
    for (i in 1:nrow(dt_bnMappingA)) {
      dt_pitchNote <- gsub(dt_bnMappingA[i, boxscoreName], dt_bnMappingA[i, boxscoreName2], dt_pitchNote, fixed = TRUE)
    }
    
    for (i in 1:nrow(dt_bnMappingH)) {
      dt_pitchNote <- gsub(dt_bnMappingH[i, boxscoreName], dt_bnMappingH[i, boxscoreName2], dt_pitchNote, fixed = TRUE)
    }
    
    list(
      teams = list(
        visitor = list(
          name = resp_box$teams$away$team$teamName,
          place = c_franchiseNameA,
          score = resp_box$teams$away$teamStats$batting$runs,
          line = resp_line$innings$away$runs %>% as.character %>% ifelse(is.na(.), 'x', .),
          stats = dt_aStats
        ),
        home = list(
          name = resp_box$teams$home$team$teamName,
          place = c_franchiseNameH,
          score = resp_box$teams$home$teamStats$batting$runs,
          line = resp_line$innings$home$runs %>% as.character %>% ifelse(is.na(.), 'x', .),
          stats = dt_hStats
        )
      ),
      batting = list(
        visitor = dt_abatBox,
        home = dt_hbatBox,
        notes = dt_batNote
      ),
      pitching = list(
        visitor = dt_apitch,
        home = dt_hpitch,
        notes = dt_pitchNote
      )
    )
  }
  
  all_games_data <- games$dates$games[[1]] %>%
    data.table() %>%
    .[status.statusCode == 'F', gamePk] %>%
    lapply(process_game_box_scoreMLB)
  
  # Remove any NULL entries (failed processing)
  all_games_data <- all_games_data[!sapply(all_games_data, is.null)]
  
  return(all_games_data)
}


#' Generate a newspaper-style HTML page with all box scores
#' 
#' @param games_data List of games' data
#' @param date_str Date string for display
#' @param standings_data Optional standings data.table
#' @param leaders_data Optional league leaders data.table
#' @return HTML content as a string
generate_newspaper_page <- function(games_data, date_str, 
                                    standings_data = NULL, 
                                    leaders_data = NULL) {
  # Format date for display
  display_date <- format(as.Date(paste0(
    substr(date_str, 1, 4), "-",
    substr(date_str, 5, 6), "-", 
    substr(date_str, 7, 8))), "%B %d, %Y")
  
  # Navigation JavaScript
  navigation_js <- paste0('
    document.addEventListener("DOMContentLoaded", function() {
      // Get current page filename (e.g., "20230607.html")
      const currentPath = window.location.pathname;
      const currentBasename = "', date_str, '"; // "20230607"
      const currentDir = currentPath.substring(0, currentPath.lastIndexOf("/") + 1);
      
      // Create navigation container
      const navContainer = document.createElement("div");
      navContainer.className = "nav-container";

      // Create prev link placeholder
      const prevLink = document.createElement("div");
      prevLink.className = "prev-link";
      prevLink.style.cssText = "flex: 1; text-align: left;";
      prevLink.innerHTML = ""; // Initial loading text
      
      // Create next link placeholder
      const nextLink = document.createElement("div");
      nextLink.className = "next-link";
      nextLink.style.cssText = "flex: 1; text-align: right;";
      nextLink.innerHTML = ""; // Initial loading text
      
      // Add to container
      navContainer.appendChild(prevLink);
      navContainer.appendChild(nextLink);
      
      // Insert navigation at the top and bottom of the content
      const newspaperDiv = document.querySelector(".newspaper");
      if (newspaperDiv) {
        // Insert at top, after header
        const header = document.querySelector(".header");
        if (header) {
          const topNav = navContainer.cloneNode(true);
          header.after(topNav);
        }
        
        // Insert at bottom
        const bottomNav = navContainer.cloneNode(true);
        newspaperDiv.appendChild(bottomNav);
      }
      
      // Get all instances of the navigation elements
      const prevLinks = document.querySelectorAll(".prev-link");
      const nextLinks = document.querySelectorAll(".next-link");
      
      // Function to format a date string from YYYYMMDD to a display format
      function formatDisplayDate(dateStr) {
        const year = dateStr.substring(0, 4);
        const month = dateStr.substring(4, 6);
        const day = dateStr.substring(6, 8);
        
        const date = new Date(Date.UTC(year, +month-1, day, 8, 0, 0));
        return date.toLocaleDateString("en-US", {
          year: "numeric", 
          month: "long", 
          day: "numeric"
        });
      }
      
      // Pure client-side approach to find adjacent pages
      async function findAdjacentPages() {
        // Generate arrays of possible dates to check, ordered by proximity
        // For convenience in sorting and comparisons, we\'ll use YYYYMMDD format
        const potentialPrevDates = [];
        const potentialNextDates = [];
        
        // Parse the current date
        const currentYear = parseInt(currentBasename.substring(0, 4));
        const currentMonth = parseInt(currentBasename.substring(4, 6)) - 1; // JS months are 0-indexed
        const currentDay = parseInt(currentBasename.substring(6, 8));
        let checkDate = new Date(currentYear, currentMonth, currentDay);
        
        // Generate potential previous dates (12 days back)
        for (let i = 1; i <= 12; i++) {
          checkDate.setDate(checkDate.getDate() - 1);
          const year = checkDate.getFullYear();
          const month = String(checkDate.getMonth() + 1).padStart(2, "0");
          const day = String(checkDate.getDate()).padStart(2, "0");
          potentialPrevDates.push(`${year}${month}${day}`);
        }
        
        // Reset date and generate potential next dates (12 days forward)
        checkDate = new Date(currentYear, currentMonth, currentDay);
        for (let i = 1; i <= 12; i++) {
          checkDate.setDate(checkDate.getDate() + 1);
          const year = checkDate.getFullYear();
          const month = String(checkDate.getMonth() + 1).padStart(2, "0");
          const day = String(checkDate.getDate()).padStart(2, "0");
          potentialNextDates.push(`${year}${month}${day}`);
        }
        
        // Check for the existence of files using fetch with HEAD requests
        async function checkFileExists(filename) {
          try {
            // Build a full URL from the current location
            const protocol = window.location.protocol;
            const host = window.location.host;
            const basePath = currentDir;
            const fullUrl = `${protocol}//${host}${basePath}${filename}.html`;
            
            // Use GET request with cache control
            const response = await fetch(fullUrl, { 
              method: "GET",
              headers: {
                \'Cache-Control\': \'no-cache, no-store, must-revalidate\',
                \'Pragma\': \'no-cache\',
                \'Expires\': \'0\'
              },
              mode: \'no-cors\' // Try to avoid CORS issues
            });
            
            return response.ok;
          } catch (e) {
            console.error("Error checking if file exists:", e);
            return e.toString().includes("NetworkError") || e.toString().includes("Failed to fetch");
          }
        }
        
        // Find the first existing previous page
        let foundPrev = false;
        for (const dateStr of potentialPrevDates) {
          if (await checkFileExists(dateStr)) {
            const linkHTML = `<a href="${dateStr}.html" style="text-decoration: none; color: #000; font-weight: bold;">« ${formatDisplayDate(dateStr)}</a>`;
            // Update all prev link elements
            prevLinks.forEach(el => {
              el.innerHTML = linkHTML;
            });
            foundPrev = true;
            break;
          }
        }
        
        if (!foundPrev) {
          // Clear the loading text if no previous file was found
          prevLinks.forEach(el => {
            el.innerHTML = "";
          });
        }
        
        // Find the first existing next page
        let foundNext = false;
        for (const dateStr of potentialNextDates) {
          if (await checkFileExists(dateStr)) {
            const linkHTML = `<a href="${dateStr}.html" style="text-decoration: none; color: #000; font-weight: bold;">${formatDisplayDate(dateStr)} »</a>`;
            // Update all next link elements
            nextLinks.forEach(el => {
              el.innerHTML = linkHTML;
            });
            foundNext = true;
            break;
          }
        }
        
        if (!foundNext) {
          // Clear the loading text if no next file was found
          nextLinks.forEach(el => {
            el.innerHTML = "";
          });
        }
      }
      
      // Start the process
      findAdjacentPages();
    });
    ')
  
  # Start HTML content
  html_content <- paste0(
    "<!DOCTYPE html>\n",
    "<html>\n",
    "<head>\n",
    "  <meta charset=\"UTF-8\">\n",  # Add charset meta tag
    "  <title>Baseball Box Scores - ", display_date, "</title>\n",
    "  <style>\n",
    "    @import url('https://fonts.googleapis.com/css2?family=Source+Sans+3:ital,wght@0,200..900;1,200..900&display=swap');\n",
    "    body { font-family: 'Source Sans 3', 'Segoe UI'; margin: 0; padding: 0; background-color: #f9f7f1; }\n",
    "    .newspaper { max-width: 1200px; margin: 0 auto; padding: 20px; background-color: #fff; box-shadow: 0 0 10px rgba(0,0,0,0.1); }\n",
    "    .header { text-align: center; border-bottom: 2px solid #000; padding-bottom: 10px; margin-bottom: 20px; }\n",
    "    .date { font-style: italic; margin-bottom: 10px; }\n",
    "    .main-title { font-size: 42px; font-weight: bold; margin: 0; }\n",
    "    .subtitle { font-size: 24px; margin: 5px 0 15px 0; }\n",
    "    .nav-container { display: flex; justify-content: space-between; padding: 10px 0; margin: 10px 0; border-top: 1px solid #ddd; border-bottom: 1px solid #ddd; }\n",
    "    .leaders div { font-size: 14px; margin: 8px 0px; }\n",
    "    .boxscores-title { text-align: center; font-size: 24px; font-weight: bold; margin: 20px 0; border-bottom: 2px solid #000; }\n",
    "    .boxscores-container { column-count: 3; column-gap: 20px; margin-top: 20px; }\n",
    "    .game-container { break-inside: avoid; page-break-inside: avoid; margin-bottom: 20px; border: 1px solid #ddd; padding: 10px; }\n",
    "    .game-header { font-weight: bold; font-size: 24px; border-bottom: 1px solid #000; margin-bottom: 8px; padding-bottom: 4px; }\n",
    "    .team-line { display: flex; justify-content: space-between; font-size: 18px; font-weight: 700; line-height: 1.2; margin: 2px 0; }\n",
    "    /* Improved table styling */\n",
    "    table { width: 100%; border-collapse: collapse; font-size: 14px; margin: 8px 0; table-layout: fixed; }\n",
    "    .batting tr:last-child { font-weight:700 }\n",
    "    th, td { padding: 2px 4px; text-align: left; border-bottom: 1px solid #ddd; overflow: hidden; text-overflow: ellipsis; }\n",
    "    th { border-bottom: 1px solid #000; font-weight: bold; background-color: #f8f8f8; }\n",
    "    /* Specific column widths for standings tables */\n",
    "    .standings-table .team-col { width: 30%; text-align: left; }\n",
    "    .standings-table .num-col { width: 7%; text-align: right; }\n",
    "    .standings-table .pct-col { width: 11%; text-align: right; }\n",
    "    /* General column alignment */\n",
    "    th:not(.team-col), td:not(.team-col) { text-align: right; }\n",
    "    td { white-space: nowrap; }\n",
    "    .notes { font-size: 14px; line-height: 1.2; margin-top: 8px; color: #444; }\n",
    "    .section { margin-bottom: 20px; }\n",
    "    .column-container { display: flex; gap: 15px; }\n",
    "    .column { flex: 1; }\n",
    "    .stats-header { font-size: 16px; font-weight: bold; margin: 8px 0; border-bottom: 1px solid #000; }\n",
    "    .stats-subheader { font-size: 14px; font-weight: bold; margin: 6px 0; padding-bottom: 2px; border-bottom: 1px solid #ddd; }\n",
    "    /* Batting and pitching table styles */\n",
    "    .batting-table .player-col { width: 40%; text-align: left; }\n",
    "    .batting-table .stat-col { width: 5%; text-align: right; }\n",
    "    .batting-table .avg-col { width: 10%; text-align: right; }\n",
    "    .batting-table tr:last-child { font-weight:700 }\n",
    "    .pitching-table .player-col { width: 32%; text-align: left; }\n",
    "    .pitching-table .ip-col { width: 10%; text-align: right; }\n",
    "    .pitching-table .stat-col { width: 5%; text-align: right; }\n",
    "    .pitching-table .era-col { width: 10%; text-align: right; }\n",
    "    /* Leaders table styles */\n",
    "    .leaders-table .player-col { width: 50%; text-align: left; }\n",
    "    .leaders-table .stat-col { width: 5%; text-align: right; }\n",
    "    .leaders-table .avg-col { width: 10%; text-align: right; }\n",
    "    .leaders-section { margin-bottom: 20px; }\n",
    "    .leaders-note { font-size: 13px; margin: 5px 0; }\n",
    "    @media (max-width: 1000px) {\n",
    "      .boxscores-container { column-count: 2; column-gap: 10px; margin-top: 10px; }\n",
    "      .column-container { display: block }\n",
    "      table { font-size: 18px; }\n",
    "      .leaders div { font-size: 18px; }\n",
    "      .notes { font-size: 18px; }\n",
    "    }\n",
    "    @media (max-width: 850px) {\n",
    "      .boxscores-container { column-count: 1; column-gap: 10px; margin-top: 10px; }\n",
    "      .column-container { display: block }\n",
    "      table { font-size: 18px; }\n",
    "      .leaders div { font-size: 18px; }\n",
    "      .notes { font-size: 18px; }\n",
    "    } \n",
    "    @media print {\n",
    "      body { \n",
    "        background-color: #fff;\n",
    "        width:1800px;\n",
    "      }\n",
    "      .newspaper { box-shadow: none; max-width: none; padding: 0; margin: 0; }\n",
    "      th, td { padding: 0px 4px; }\n",
    "      .nav-container { display: none; }\n",
    "      .column-container { display: flex; }\n",
    "      .boxscores-container { column-count: 5; }\n",
    "      .game-container { border: none; border-bottom: 1px solid #ddd; }\n",
    "    }\n",
    "  </style>\n",
    "  <script>\n", navigation_js, "\n    </script>\n",
    "</head>\n",
    "<body>\n",
    "  <div class='newspaper'>\n",
    "    <div class='header'>\n",
    "      <div class='date'>", display_date, "</div>\n",
    "      <h1 class='main-title'>WALDRN.COM/BOXSCORES</h1>\n",
    "      <div class='subtitle'>Daily Box Scores, Standings & League Leaders</div>\n",
    "    </div>\n",
    "    <div class='page'>\n"
  )
  
  # Add standings if available
  if (!is.null(standings_data)) {
    html_content <- paste0(
      html_content,
      "      <div class='section'>\n",
      "        <div class='column-container'>\n",
      "          <div class='column'>\n",
      "            <div class='boxscores-title'>A.L. Standings</div>\n",
      "            <div class='stats-subheader'>East Division</div>\n"
    )
    
    # Generate AL East standings table
    html_content <- paste0(
      html_content,
      "            <table class='standings-table'>\n",
      "              <thead>\n",
      "                <tr>\n",
      "                  <th class='team-col'>Team</th>\n",
      "                  <th class='num-col'>W</th>\n",
      "                  <th class='num-col'>L</th>\n",
      "                  <th class='pct-col'>Pct</th>\n",
      "                  <th class='num-col'>GB</th>\n",
      "                  <th class='num-col'>L10</th>\n",
      "                  <th class='num-col'>Strk</th>\n",
      "                  <th class='num-col'>Home</th>\n",
      "                  <th class='num-col'>Away</th>\n",
      "                </tr>\n",
      "              </thead>\n",
      "              <tbody>\n"
    )
    
    for (i in 1:nrow(standings_data$al[[1]])) {
      row <- standings_data$al[[1]][i,]
      html_content <- paste0(
        html_content,
        "                <tr>\n",
        "                  <td class='team-col'>", row$Team, "</td>\n",
        "                  <td class='num-col'>", row$W, "</td>\n",
        "                  <td class='num-col'>", row$L, "</td>\n",
        "                  <td class='pct-col'>", row$PCT, "</td>\n",
        "                  <td class='num-col'>", row$GB, "</td>\n",
        "                  <td class='num-col'>", row$L10, "</td>\n",
        "                  <td class='num-col'>", row$Strk, "</td>\n",
        "                  <td class='num-col'>", row$H, "</td>\n",
        "                  <td class='num-col'>", row$A, "</td>\n",
        "                </tr>\n"
      )
    }
    
    html_content <- paste0(
      html_content,
      "              </tbody>\n",
      "            </table>\n",
      "            <div class='stats-subheader'>Central Division</div>\n"
    )
    
    # Generate AL Central standings table
    html_content <- paste0(
      html_content,
      "            <table class='standings-table'>\n",
      "              <thead>\n",
      "                <tr>\n",
      "                  <th class='team-col'>Team</th>\n",
      "                  <th class='num-col'>W</th>\n",
      "                  <th class='num-col'>L</th>\n",
      "                  <th class='pct-col'>Pct</th>\n",
      "                  <th class='num-col'>GB</th>\n",
      "                  <th class='num-col'>L10</th>\n",
      "                  <th class='num-col'>Strk</th>\n",
      "                  <th class='num-col'>Home</th>\n",
      "                  <th class='num-col'>Away</th>\n",
      "                </tr>\n",
      "              </thead>\n",
      "              <tbody>\n"
    )
    
    for (i in 1:nrow(standings_data$al[[2]])) {
      row <- standings_data$al[[2]][i,]
      html_content <- paste0(
        html_content,
        "                <tr>\n",
        "                  <td class='team-col'>", row$Team, "</td>\n",
        "                  <td class='num-col'>", row$W, "</td>\n",
        "                  <td class='num-col'>", row$L, "</td>\n",
        "                  <td class='pct-col'>", row$PCT, "</td>\n",
        "                  <td class='num-col'>", row$GB, "</td>\n",
        "                  <td class='num-col'>", row$L10, "</td>\n",
        "                  <td class='num-col'>", row$Strk, "</td>\n",
        "                  <td class='num-col'>", row$H, "</td>\n",
        "                  <td class='num-col'>", row$A, "</td>\n",
        "                </tr>\n"
      )
    }
    
    html_content <- paste0(
      html_content,
      "              </tbody>\n",
      "            </table>\n",
      "            <div class='stats-subheader'>West Division</div>\n"
    )
    
    # Generate AL West standings table
    html_content <- paste0(
      html_content,
      "            <table class='standings-table'>\n",
      "              <thead>\n",
      "                <tr>\n",
      "                  <th class='team-col'>Team</th>\n",
      "                  <th class='num-col'>W</th>\n",
      "                  <th class='num-col'>L</th>\n",
      "                  <th class='pct-col'>Pct</th>\n",
      "                  <th class='num-col'>GB</th>\n",
      "                  <th class='num-col'>L10</th>\n",
      "                  <th class='num-col'>Strk</th>\n",
      "                  <th class='num-col'>Home</th>\n",
      "                  <th class='num-col'>Away</th>\n",
      "                </tr>\n",
      "              </thead>\n",
      "              <tbody>\n"
    )
    
    for (i in 1:nrow(standings_data$al[[3]])) {
      row <- standings_data$al[[3]][i,]
      html_content <- paste0(
        html_content,
        "                <tr>\n",
        "                  <td class='team-col'>", row$Team, "</td>\n",
        "                  <td class='num-col'>", row$W, "</td>\n",
        "                  <td class='num-col'>", row$L, "</td>\n",
        "                  <td class='pct-col'>", row$PCT, "</td>\n",
        "                  <td class='num-col'>", row$GB, "</td>\n",
        "                  <td class='num-col'>", row$L10, "</td>\n",
        "                  <td class='num-col'>", row$Strk, "</td>\n",
        "                  <td class='num-col'>", row$H, "</td>\n",
        "                  <td class='num-col'>", row$A, "</td>\n",
        "                </tr>\n"
      )
    }
    
    html_content <- paste0(
      html_content,
      "              </tbody>\n",
      "            </table>\n",
      "          </div>\n",
      "          <div class='column'>\n",
      "            <div class='boxscores-title'>N.L. Standings</div>\n",
      "            <div class='stats-subheader'>East Division</div>\n"
      
    )
    
    # Generate NL standings table
    html_content <- paste0(
      html_content,
      "            <table class='standings-table'>\n",
      "              <thead>\n",
      "                <tr>\n",
      "                  <th class='team-col'>Team</th>\n",
      "                  <th class='num-col'>W</th>\n",
      "                  <th class='num-col'>L</th>\n",
      "                  <th class='pct-col'>Pct</th>\n",
      "                  <th class='num-col'>GB</th>\n",
      "                  <th class='num-col'>L10</th>\n",
      "                  <th class='num-col'>Strk</th>\n",
      "                  <th class='num-col'>Home</th>\n",
      "                  <th class='num-col'>Away</th>\n",
      "                </tr>\n",
      "              </thead>\n",
      "              <tbody>\n"
    )
    
    for (i in 1:nrow(standings_data$nl[[1]])) {
      row <- standings_data$nl[[1]][i,]
      html_content <- paste0(
        html_content,
        "                <tr>\n",
        "                  <td class='team-col'>", row$Team, "</td>\n",
        "                  <td class='num-col'>", row$W, "</td>\n",
        "                  <td class='num-col'>", row$L, "</td>\n",
        "                  <td class='pct-col'>", row$PCT, "</td>\n",
        "                  <td class='num-col'>", row$GB, "</td>\n",
        "                  <td class='num-col'>", row$L10, "</td>\n",
        "                  <td class='num-col'>", row$Strk, "</td>\n",
        "                  <td class='num-col'>", row$H, "</td>\n",
        "                  <td class='num-col'>", row$A, "</td>\n",
        "                </tr>\n"
      )
    }
    
    html_content <- paste0(
      html_content,
      "              </tbody>\n",
      "            </table>\n",
      "            <div class='stats-subheader'>Central Division</div>\n"
      
    )
    
    # Generate NL standings table
    html_content <- paste0(
      html_content,
      "            <table class='standings-table'>\n",
      "              <thead>\n",
      "                <tr>\n",
      "                  <th class='team-col'>Team</th>\n",
      "                  <th class='num-col'>W</th>\n",
      "                  <th class='num-col'>L</th>\n",
      "                  <th class='pct-col'>Pct</th>\n",
      "                  <th class='num-col'>GB</th>\n",
      "                  <th class='num-col'>L10</th>\n",
      "                  <th class='num-col'>Strk</th>\n",
      "                  <th class='num-col'>Home</th>\n",
      "                  <th class='num-col'>Away</th>\n",
      "                </tr>\n",
      "              </thead>\n",
      "              <tbody>\n"
    )
    
    for (i in 1:nrow(standings_data$nl[[2]])) {
      row <- standings_data$nl[[2]][i,]
      html_content <- paste0(
        html_content,
        "                <tr>\n",
        "                  <td class='team-col'>", row$Team, "</td>\n",
        "                  <td class='num-col'>", row$W, "</td>\n",
        "                  <td class='num-col'>", row$L, "</td>\n",
        "                  <td class='pct-col'>", row$PCT, "</td>\n",
        "                  <td class='num-col'>", row$GB, "</td>\n",
        "                  <td class='num-col'>", row$L10, "</td>\n",
        "                  <td class='num-col'>", row$Strk, "</td>\n",
        "                  <td class='num-col'>", row$H, "</td>\n",
        "                  <td class='num-col'>", row$A, "</td>\n",
        "                </tr>\n"
      )
    }
    
    html_content <- paste0(
      html_content,
      "              </tbody>\n",
      "            </table>\n",
      "            <div class='stats-subheader'>West Division</div>\n"
      
    )
    
    # Generate NL standings table
    html_content <- paste0(
      html_content,
      "            <table class='standings-table'>\n",
      "              <thead>\n",
      "                <tr>\n",
      "                  <th class='team-col'>Team</th>\n",
      "                  <th class='num-col'>W</th>\n",
      "                  <th class='num-col'>L</th>\n",
      "                  <th class='pct-col'>Pct</th>\n",
      "                  <th class='num-col'>GB</th>\n",
      "                  <th class='num-col'>L10</th>\n",
      "                  <th class='num-col'>Strk</th>\n",
      "                  <th class='num-col'>Home</th>\n",
      "                  <th class='num-col'>Away</th>\n",
      "                </tr>\n",
      "              </thead>\n",
      "              <tbody>\n"
    )
    
    for (i in 1:nrow(standings_data$nl[[3]])) {
      row <- standings_data$nl[[3]][i,]
      html_content <- paste0(
        html_content,
        "                <tr>\n",
        "                  <td class='team-col'>", row$Team, "</td>\n",
        "                  <td class='num-col'>", row$W, "</td>\n",
        "                  <td class='num-col'>", row$L, "</td>\n",
        "                  <td class='pct-col'>", row$PCT, "</td>\n",
        "                  <td class='num-col'>", row$GB, "</td>\n",
        "                  <td class='num-col'>", row$L10, "</td>\n",
        "                  <td class='num-col'>", row$Strk, "</td>\n",
        "                  <td class='num-col'>", row$H, "</td>\n",
        "                  <td class='num-col'>", row$A, "</td>\n",
        "                </tr>\n"
      )
    }
    
    html_content <- paste0(
      html_content,
      "              </tbody>\n",
      "            </table>\n",
      "          </div>\n",
      "        </div>\n",
      "      </div>\n"
    )
  }
  
  # Add league leaders if available
  if (!is.null(leaders_data)) {
    html_content <- paste0(
      html_content,
      "      <div class='section'>\n",
      "        <div class='column-container'>\n",
      "          <div class='column'>\n",
      "            <div class='boxscores-title'>A.L. LEADERS</div>\n",
      "            <div class='column-container'>\n",
      "              <div class='column leaders'>\n"
    )
    
    html_content <- paste0(
      html_content,
      "                <table class='leaders-table'>\n",
      "                  <thead>\n",
      "                    <tr>\n",
      "                      <th class='player-col'>Batting</th>\n",
      "                      <th class='stat-col'>G</th>\n",
      "                      <th class='stat-col'>AB</th>\n",
      "                      <th class='stat-col'>R</th>\n",
      "                      <th class='stat-col'>H</th>\n",
      "                      <th class='avg-col'>Avg.</th>\n",
      "                    </tr>\n",
      "                  </thead>\n",
      "                  <tbody>\n"
    )
    
    # Add batting leaders rows with updated classes
    for (i in 1:nrow(leaders_data$al_leaders$batting$avg_leaders)) {
      row <- leaders_data$al_leaders$batting$avg_leaders[i,]
      html_content <- paste0(
        html_content,
        "                    <tr>\n",
        "                      <td class='player-col'>", row$Player, ', ', row$Team, "</td>\n",
        "                      <td class='stat-col'>", row$G, "</td>\n",
        "                      <td class='stat-col'>", row$AB, "</td>\n",
        "                      <td class='stat-col'>", row$R, "</td>\n",
        "                      <td class='stat-col'>", row$H, "</td>\n",
        "                      <td class='avg-col'>", row$Value, "</td>\n",
        "                    </tr>\n"
      )
    }
    
    html_content <- paste0(
      html_content,
      "                  </tbody>\n",
      "                </table>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Home Runs: </b>", leaders_data$al_leaders$batting$hr_leaders, '\n',
      "                </div>\n",
      "                <div class='leaders-note'>",
      "                  <b>Runs Batted In: </b>", leaders_data$al_leaders$batting$rbi_leaders, '\n',
      "                </div>\n",
      "              </div>\n",
      "              <div class='column leaders'>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Hits: </b>", leaders_data$al_leaders$batting$hits_leaders, '\n',
      "                </div>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Stolen Bases: </b>", leaders_data$al_leaders$batting$sb_leaders, '\n',
      "                </div>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Pitching: </b>", leaders_data$al_leaders$pitching$wins_leaders, '\n',
      "                </div>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Strikeouts: </b>", leaders_data$al_leaders$pitching$so_leaders, '\n',
      "                </div>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Saves: </b>", leaders_data$al_leaders$pitching$sv_leaders, '\n',
      "                </div>\n",
      "              </div>\n",
      "            </div>\n",
      "          </div>\n",
      "          <div class='column'>\n",
      "            <div class='boxscores-title'>N.L. LEADERS</div>\n",
      "            <div class='column-container'>\n",
      "              <div class='column leaders'>\n"
    )
    
    html_content <- paste0(
      html_content,
      "                <table class='leaders-table'>\n",
      "                  <thead>\n",
      "                    <tr>\n",
      "                      <th class='player-col'>Batting</th>\n",
      "                      <th class='stat-col'>G</th>\n",
      "                      <th class='stat-col'>AB</th>\n",
      "                      <th class='stat-col'>R</th>\n",
      "                      <th class='stat-col'>H</th>\n",
      "                      <th class='avg-col'>Avg.</th>\n",
      "                    </tr>\n",
      "                  </thead>\n",
      "                  <tbody>\n"
    )
    
    # Add batting leaders rows with updated classes
    for (i in 1:nrow(leaders_data$nl_leaders$batting$avg_leaders)) {
      row <- leaders_data$nl_leaders$batting$avg_leaders[i,]
      html_content <- paste0(
        html_content,
        "                    <tr>\n",
        "                      <td class='player-col'>", row$Player, ', ', row$Team, "</td>\n",
        "                      <td class='stat-col'>", row$G, "</td>\n",
        "                      <td class='stat-col'>", row$AB, "</td>\n",
        "                      <td class='stat-col'>", row$R, "</td>\n",
        "                      <td class='stat-col'>", row$H, "</td>\n",
        "                      <td class='avg-col'>", row$Value, "</td>\n",
        "                    </tr>\n"
      )
    }
    
    html_content <- paste0(
      html_content,
      "                  </tbody>\n",
      "                </table>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Home Runs: </b>", leaders_data$nl_leaders$batting$hr_leaders, '\n',
      "                </div>\n",
      "                <div class='leaders-note'>",
      "                  <b>Runs Batted In: </b>", leaders_data$nl_leaders$batting$rbi_leaders, '\n',
      "                </div>\n",
      "              </div>\n",
      "              <div class='column leaders'>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Hits: </b>", leaders_data$nl_leaders$batting$hits_leaders, '\n',
      "                </div>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Stolen Bases: </b>", leaders_data$nl_leaders$batting$sb_leaders, '\n',
      "                </div>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Pitching: </b>", leaders_data$nl_leaders$pitching$wins_leaders, '\n',
      "                </div>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Strikeouts: </b>", leaders_data$nl_leaders$pitching$so_leaders, '\n',
      "                </div>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Saves: </b>", leaders_data$nl_leaders$pitching$sv_leaders, '\n',
      "                </div>\n",
      "              </div>\n",
      "            </div>\n",
      "          </div>\n",
      "        </div>\n"
    )
  }
  
  # Add box scores section
  html_content <- paste0(
    html_content,
    "    </div>\n",
    "    <div class='boxscores-title'>BOX SCORES</div>\n",
    "    <div class='boxscores-container'>\n"
  )
  
  # Add each game box score
  for (i in seq_along(games_data)) {
    game <- games_data[[i]]
    
    # Game title with team names and scores
    game_title <- if (game$teams$visitor$score > game$teams$home$score) {
      paste0(
        game$teams$visitor$name, " ", game$teams$visitor$score, ", ",
        game$teams$home$name, " ", game$teams$home$score
      )
    } else {
      paste0(
        game$teams$home$name, " ", game$teams$home$score, ", ",
        game$teams$visitor$name, " ", game$teams$visitor$score
      )
    }
    
    html_content <- paste0(
      html_content,
      "      <div class='game-container'>\n",
      "        <div class='game-header'>", game_title, "</div>\n",
      "        <div class='team-line'>\n",
      "          <div class='team-name'>", game$teams$visitor$place, "</div>\n",
      "          <div class='team-score'>", 
      paste(gsub("(.{3})", "\\1&numsp;", paste0(game$teams$visitor$line, collapse='')), "&mdash; ",
            gsub('\\s','&numsp;',sprintf('%02s', as.integer(game$teams$visitor$stats[1]))), "&numsp;",
            gsub('\\s','&numsp;',sprintf('%02s', as.integer(game$teams$visitor$stats[2]))), "&numsp;",
            as.integer(game$teams$visitor$stats[3])), 
      "          </div>\n",
      "        </div>\n",
      "        <div class='team-line'>\n",
      "          <div class='team-name'>", game$teams$home$place, "</div>\n",
      "          <div class='team-score'>", 
      paste(gsub("(.{3})", "\\1&numsp;", paste0(game$teams$home$line, collapse='')), "&mdash; ",
            gsub('\\s','&numsp;',sprintf('%02s', as.integer(game$teams$home$stats[1]))), "&numsp;",
            gsub('\\s','&numsp;',sprintf('%02s', as.integer(game$teams$home$stats[2]))), "&numsp;",
            as.integer(game$teams$home$stats[3])), 
      "          </div>\n",
      "        </div>\n"
    )
    
    # Visitor batting
    html_content <- paste0(
      html_content,
      "        <table class='batting-table'>\n",
      "        <thead>\n",
      "          <tr>\n",
      "            <th class='player-col'>", game$teams$visitor$place, "</th>\n",
      "            <th class='stat-col'>AB</th>\n",
      "            <th class='stat-col'>R</th>\n",
      "            <th class='stat-col'>H</th>\n",
      "            <th class='stat-col'>BI</th>\n",
      "            <th class='stat-col'>BB</th>\n",
      "            <th class='stat-col'>SO</th>\n",
      "            <th class='avg-col'>Avg</th>\n",
      "          </tr>\n",
      "        </thead>\n",
      "        <tbody>\n"
    )
    
    # Add visitor batting rows
    for (j in 1:nrow(game$batting$visitor)) {
      row <- game$batting$visitor[j,]
      player_name <- names(row)[1] # First column name is team place
      html_content <- paste0(
        html_content,
        "          <tr>\n",
        "            <td class='player-col'>", row[[1]], "</td>\n", # Player name
        "            <td class='stat-col'>", row$AB, "</td>\n",
        "            <td class='stat-col'>", row$R, "</td>\n",
        "            <td class='stat-col'>", row$H, "</td>\n",
        "            <td class='stat-col'>", row$BI, "</td>\n",
        "            <td class='stat-col'>", row$BB, "</td>\n",
        "            <td class='stat-col'>", row$SO, "</td>\n",
        "            <td class='avg-col'>", row$Avg, "</td>\n",
        "          </tr>\n"
      )
    }
    
    html_content <- paste0(
      html_content,
      "        </tbody>\n",
      "        </table>\n"
    )
    
    # Home batting
    html_content <- paste0(
      html_content,
      "        <table class='batting-table'>\n",
      "        <thead>\n",
      "          <tr>\n",
      "            <th class='player-col'>", game$teams$home$place, "</th>\n",
      "            <th class='stat-col'>AB</th>\n",
      "            <th class='stat-col'>R</th>\n",
      "            <th class='stat-col'>H</th>\n",
      "            <th class='stat-col'>BI</th>\n",
      "            <th class='stat-col'>BB</th>\n",
      "            <th class='stat-col'>SO</th>\n",
      "            <th class='avg-col'>Avg</th>\n",
      "          </tr>\n",
      "        </thead>\n",
      "        <tbody>\n"
    )
    
    # Add home batting rows
    for (j in 1:nrow(game$batting$home)) {
      row <- game$batting$home[j,]
      player_name <- names(row)[1] # First column name is team place
      html_content <- paste0(
        html_content,
        "          <tr>\n",
        "            <td class='player-col'>", row[[1]], "</td>\n", # Player name
        "            <td class='stat-col'>", row$AB, "</td>\n",
        "            <td class='stat-col'>", row$R, "</td>\n",
        "            <td class='stat-col'>", row$H, "</td>\n",
        "            <td class='stat-col'>", row$BI, "</td>\n",
        "            <td class='stat-col'>", row$BB, "</td>\n",
        "            <td class='stat-col'>", row$SO, "</td>\n",
        "            <td class='avg-col'>", row$Avg, "</td>\n",
        "          </tr>\n"
      )
    }
    
    html_content <- paste0(
      html_content,
      "        </tbody>\n",
      "        </table>\n",
      "        <div class='notes'>", game$batting$notes, "</div>\n"
    )
    
    # Pitching tables - Visitor
    html_content <- paste0(
      html_content,
      "        <table class='pitching-table'>\n",
      "        <thead>\n",
      "          <tr>\n",
      "            <th class='player-col'>", game$teams$visitor$place, "</th>\n",
      "            <th class='ip-col'>IP</th>\n",
      "            <th class='stat-col'>H</th>\n",
      "            <th class='stat-col'>R</th>\n",
      "            <th class='stat-col'>ER</th>\n",
      "            <th class='stat-col'>BB</th>\n",
      "            <th class='stat-col'>SO</th>\n",
      "            <th class='stat-col'>NP</th>\n",
      "            <th class='era-col'>ERA</th>\n",
      "          </tr>\n",
      "        </thead>\n",
      "        <tbody>\n"
    )
    
    # Add visitor pitching rows
    for (j in 1:nrow(game$pitching$visitor)) {
      row <- game$pitching$visitor[j,]
      player_name <- names(row)[1] # First column name is team place
      html_content <- paste0(
        html_content,
        "          <tr>\n",
        "            <td class='player-col'>", row[[1]], "</td>\n", # Player name
        "            <td class='ip-col'>", row$IP, "</td>\n",
        "            <td class='stat-col'>", row$H, "</td>\n",
        "            <td class='stat-col'>", row$R, "</td>\n",
        "            <td class='stat-col'>", row$ER, "</td>\n",
        "            <td class='stat-col'>", row$BB, "</td>\n",
        "            <td class='stat-col'>", row$SO, "</td>\n",
        "            <td class='stat-col'>", row$NP, "</td>\n",
        "            <td class='era-col'>", row$ERA, "</td>\n",
        "          </tr>\n"
      )
    }
    
    html_content <- paste0(
      html_content,
      "        </tbody>\n",
      "        </table>\n"
    )
    
    # Pitching tables - Home
    html_content <- paste0(
      html_content,
      "        <table class='pitching-table'>\n",
      "        <thead>\n",
      "          <tr>\n",
      "            <th class='player-col'>", game$teams$home$place, "</th>\n",
      "            <th class='ip-col'>IP</th>\n",
      "            <th class='stat-col'>H</th>\n",
      "            <th class='stat-col'>R</th>\n",
      "            <th class='stat-col'>ER</th>\n",
      "            <th class='stat-col'>BB</th>\n",
      "            <th class='stat-col'>SO</th>\n",
      "            <th class='stat-col'>NP</th>\n",
      "            <th class='era-col'>ERA</th>\n",
      "          </tr>\n",
      "        </thead>\n",
      "        <tbody>\n"
    )
    
    # Add home pitching rows
    for (j in 1:nrow(game$pitching$home)) {
      row <- game$pitching$home[j,]
      player_name <- names(row)[1] # First column name is team place
      html_content <- paste0(
        html_content,
        "          <tr>\n",
        "            <td class='player-col'>", row[[1]], "</td>\n", # Player name
        "            <td class='ip-col'>", row$IP, "</td>\n",
        "            <td class='stat-col'>", row$H, "</td>\n",
        "            <td class='stat-col'>", row$R, "</td>\n",
        "            <td class='stat-col'>", row$ER, "</td>\n",
        "            <td class='stat-col'>", row$BB, "</td>\n",
        "            <td class='stat-col'>", row$SO, "</td>\n",
        "            <td class='stat-col'>", row$NP, "</td>\n",
        "            <td class='era-col'>", row$ERA, "</td>\n",
        "          </tr>\n"
      )
    }
    
    html_content <- paste0(
      html_content,
      "        </tbody>\n",
      "        </table>\n"
    )
    
    # Game notes
    html_content <- paste0(
      html_content,
      "        <div class='notes'>", game$pitching$notes, "</div>\n",
      "      </div>\n"
    )
  }
  
  # Close the boxscores-container and newspaper divs
  html_content <- paste0(
    html_content,
    "    </div>\n",
    "  </div>\n",
    "</body>\n",
    "</html>"
  )
  
  return(html_content)
}

#' Generate a newspaper-style HTML page with all box scores
#' 
#' @param games_data List of games' data
#' @param date_str Date string for display
#' @param standings_data Optional standings data.table
#' @param leaders_data Optional league leaders data.table
#' @return HTML content as a string
generate_newspaper_page2 <- function(games_data, date_str, 
                                    standings_data = NULL, 
                                    leaders_data = NULL) {
  # Format date for display
  display_date <- format(as.Date(paste0(
    substr(date_str, 1, 4), "-",
    substr(date_str, 5, 6), "-", 
    substr(date_str, 7, 8))), "%B %d, %Y")
  
  # Navigation JavaScript
  navigation_js <- paste0('
    document.addEventListener("DOMContentLoaded", function() {
      // Get current page filename (e.g., "20230607.html")
      const currentPath = window.location.pathname;
      const currentBasename = "', date_str, '"; // "20230607"
      const currentDir = currentPath.substring(0, currentPath.lastIndexOf("/") + 1);
      
      // Create navigation container
      const navContainer = document.createElement("div");
      navContainer.className = "nav-container";

      // Create prev link placeholder
      const prevLink = document.createElement("div");
      prevLink.className = "prev-link";
      prevLink.style.cssText = "flex: 1; text-align: left;";
      prevLink.innerHTML = ""; // Initial loading text
      
      // Create pdf link placeholder
      const pdfLink = document.createElement("div");
      pdfLink.className = "pdf-link";
      pdfLink.innerHTML = `<a href="', date_str, '.pdf" style="color: #000; font-weight: bold;">pdf link</a>`;
      
      // Create next link placeholder
      const nextLink = document.createElement("div");
      nextLink.className = "next-link";
      nextLink.style.cssText = "flex: 1; text-align: right;";
      nextLink.innerHTML = ""; // Initial loading text
      
      // Add to container
      navContainer.appendChild(prevLink);
      navContainer.appendChild(pdfLink);
      navContainer.appendChild(nextLink);
      
      // Insert navigation at the top and bottom of the content
      const newspaperDiv = document.querySelector(".newspaper");
      if (newspaperDiv) {
        // Insert at top, after header
        const header = document.querySelector(".header");
        if (header) {
          const topNav = navContainer.cloneNode(true);
          header.after(topNav);
        }
        
        // Insert at bottom
        const bottomNav = navContainer.cloneNode(true);
        newspaperDiv.appendChild(bottomNav);
      }
      
      // Get all instances of the navigation elements
      const prevLinks = document.querySelectorAll(".prev-link");
      const nextLinks = document.querySelectorAll(".next-link");
      
      // Function to format a date string from YYYYMMDD to a display format
      function formatDisplayDate(dateStr) {
        const year = dateStr.substring(0, 4);
        const month = dateStr.substring(4, 6);
        const day = dateStr.substring(6, 8);
        
        const date = new Date(Date.UTC(year, +month-1, day, 8, 0, 0));
        return date.toLocaleDateString("en-US", {
          year: "numeric", 
          month: "long", 
          day: "numeric"
        });
      }
      
      // Pure client-side approach to find adjacent pages
      async function findAdjacentPages() {
        // Generate arrays of possible dates to check, ordered by proximity
        // For convenience in sorting and comparisons, we\'ll use YYYYMMDD format
        const potentialPrevDates = [];
        const potentialNextDates = [];
        
        // Parse the current date
        const currentYear = parseInt(currentBasename.substring(0, 4));
        const currentMonth = parseInt(currentBasename.substring(4, 6)) - 1; // JS months are 0-indexed
        const currentDay = parseInt(currentBasename.substring(6, 8));
        let checkDate = new Date(currentYear, currentMonth, currentDay);
        
        // Generate potential previous dates (12 days back)
        for (let i = 1; i <= 12; i++) {
          checkDate.setDate(checkDate.getDate() - 1);
          const year = checkDate.getFullYear();
          const month = String(checkDate.getMonth() + 1).padStart(2, "0");
          const day = String(checkDate.getDate()).padStart(2, "0");
          potentialPrevDates.push(`${year}${month}${day}`);
        }
        
        // Reset date and generate potential next dates (12 days forward)
        checkDate = new Date(currentYear, currentMonth, currentDay);
        for (let i = 1; i <= 12; i++) {
          checkDate.setDate(checkDate.getDate() + 1);
          const year = checkDate.getFullYear();
          const month = String(checkDate.getMonth() + 1).padStart(2, "0");
          const day = String(checkDate.getDate()).padStart(2, "0");
          potentialNextDates.push(`${year}${month}${day}`);
        }
        
        // Check for the existence of files using fetch with HEAD requests
        async function checkFileExists(filename) {
          try {
            // Build a full URL from the current location
            const protocol = window.location.protocol;
            const host = window.location.host;
            const basePath = currentDir;
            const fullUrl = `${protocol}//${host}${basePath}${filename}.html`;
            
            // Use GET request with cache control
            const response = await fetch(fullUrl, { 
              method: "GET",
              headers: {
                \'Cache-Control\': \'no-cache, no-store, must-revalidate\',
                \'Pragma\': \'no-cache\',
                \'Expires\': \'0\'
              },
              mode: \'no-cors\' // Try to avoid CORS issues
            });
            
            return response.ok;
          } catch (e) {
            console.error("Error checking if file exists:", e);
            return e.toString().includes("NetworkError") || e.toString().includes("Failed to fetch");
          }
        }
        
        // Find the first existing previous page
        let foundPrev = false;
        for (const dateStr of potentialPrevDates) {
          if (await checkFileExists(dateStr)) {
            const linkHTML = `<a href="${dateStr}.html" style="text-decoration: none; color: #000; font-weight: bold;">« ${formatDisplayDate(dateStr)}</a>`;
            // Update all prev link elements
            prevLinks.forEach(el => {
              el.innerHTML = linkHTML;
            });
            foundPrev = true;
            break;
          }
        }
        
        if (!foundPrev) {
          // Clear the loading text if no previous file was found
          prevLinks.forEach(el => {
            el.innerHTML = "";
          });
        }
        
        // Find the first existing next page
        let foundNext = false;
        for (const dateStr of potentialNextDates) {
          if (await checkFileExists(dateStr)) {
            const linkHTML = `<a href="${dateStr}.html" style="text-decoration: none; color: #000; font-weight: bold;">${formatDisplayDate(dateStr)} »</a>`;
            // Update all next link elements
            nextLinks.forEach(el => {
              el.innerHTML = linkHTML;
            });
            foundNext = true;
            break;
          }
        }
        
        if (!foundNext) {
          // Clear the loading text if no next file was found
          nextLinks.forEach(el => {
            el.innerHTML = "";
          });
        }
      }
      
      // Start the process
      findAdjacentPages();
    });
    ')
  
  # Start HTML content
  html_content <- paste0(
    "<!DOCTYPE html>\n",
    "<html>\n",
    "<head>\n",
    "  <meta charset=\"UTF-8\">\n",  # Add charset meta tag
    "  <title>Baseball Box Scores - ", display_date, "</title>\n",
    "  <style>\n",
    "    @import url('https://fonts.googleapis.com/css2?family=Source+Sans+3:ital,wght@0,200..900;1,200..900&display=swap');\n",
    "    body { font-family: 'Source Sans 3', 'Segoe UI'; margin: 0; padding: 0; background-color: #f9f7f1; }\n",
    "    .newspaper { max-width: 1200px; margin: 0 auto; padding: 20px; background-color: #fff; box-shadow: 0 0 10px rgba(0,0,0,0.1); }\n",
    "    .header { text-align: center; border-bottom: 2px solid #000; padding-bottom: 10px; margin-bottom: 20px; }\n",
    "    .date { font-style: italic; margin-bottom: 10px; }\n",
    "    .main-title { font-size: 42px; font-weight: bold; margin: 0; }\n",
    "    .subtitle { font-size: 24px; margin: 5px 0 15px 0; }\n",
    "    .page { break-after: page; }\n",
    "    .nav-container { display: flex; justify-content: space-between; padding: 10px 0; margin: 10px 0; border-top: 1px solid #ddd; border-bottom: 1px solid #ddd; }\n",
    "    .leaders div { font-size: 14px; margin: 4px 0px; }\n",
    "    .leaders table { font-size: 14px; margin: 5px 0px; }\n",
    "    .boxscores-title { text-align: center; font-size: 24px; font-weight: bold; margin: 10px 0; border-bottom: 2px solid #000; }\n",
    "    .boxscores-container { column-count: 4; column-gap: 20px; margin-top: 20px; }\n",
    "    .game-container { break-inside: avoid; page-break-inside: avoid; margin-bottom: 20px; }\n",
    "    .game-header { font-weight: bold; font-size: 20px; border-bottom: 1px solid #000; }\n",
    "    .team-line { display: flex; justify-content: space-between; font-size: 14px; font-weight: 700; line-height: 1.2; margin-top: 2px; }\n",
    "    /* Improved table styling */\n",
    "    table { width: 100%; border-collapse: collapse; font-size: 14px; table-layout: fixed; }\n",
    "    .batting tr:last-child { font-weight:700 }\n",
    "    th, td { text-align: left; overflow: hidden; line-height: 1.1; }\n",
    "    th { border-top: 1px solid #000; font-weight: bold; }\n",
    "    /* Specific column widths for standings tables */\n",
    "    .standings-table th { border: none; }\n",
    "    .standings-table .team-col { width: 20%; text-align: left; }\n",
    "    .standings-table .num-col { width: 5%; text-align: right; }\n",
    "    .standings-table .pct-col { width: 8%; text-align: right; }\n",
    "    /* General column alignment */\n",
    "    th:not(.team-col), td:not(.team-col) { text-align: right; }\n",
    "    td { white-space: nowrap; }\n",
    "    .notes { font-size: 14px; line-height: 1.2; padding: 4px 0; border-top: 1px black solid; }\n",
    "    .section { margin-bottom: 20px; }\n",
    "    .column-container { display: flex; gap: 15px; }\n",
    "    .column { flex: 1; }\n",
    "    .column-40 { width: 40%; }\n",
    "    .column-60 { width: 60%; }\n",
    "    .stats-header { font-size: 16px; font-weight: bold; margin: 8px 0; border-bottom: 1px solid #000; }\n",
    "    .stats-subheader { font-size: 14px; font-weight: bold; margin: 4px 0; border-bottom: 1px solid #000; }\n",
    "    /* Batting and pitching table styles */\n",
    "    .batting-table .player-col { width: 30%; text-align: left; }\n",
    "    .batting-table .stat-col { width: 5%; text-align: right; }\n",
    "    .batting-table .avg-col { width: 8%; text-align: right; }\n",
    "    .batting-table tr:last-child { font-weight:700 }\n",
    "    .pitching-table .player-col { width: 37%; text-align: left; }\n",
    "    .pitching-table .ip-col { width: 5%; text-align: right; }\n",
    "    .pitching-table .stat-col { width: 5%; text-align: right; }\n",
    "    .pitching-table .era-col { width: 10%; text-align: right; }\n",
    "    /* Leaders table styles */\n",
    "    .leaders-table th { border: none; }\n",
    "    .leaders-table .player-col { width: 30%; text-align: left; }\n",
    "    .leaders-table .stat-col { width: 5%; text-align: right; }\n",
    "    .leaders-table .avg-col { width: 8%; text-align: right; }\n",
    "    .leaders-section { margin-bottom: 20px; }\n",
    "    .leaders-note { font-size: 13px; margin: 5px 0; }\n",
    "  @media (min-width: 700px) and (max-width: 1000px) {\n",
    "    .main-title { font-size:32px; }\n",
    "    .subtitle { font-size:20px; }\n",
    "    .stats-subheader { font-size:16px; }\n",
    "    .game-header { font-size:26px; }\n",
    "    .boxscores-container { column-count: 1; column-gap: 10px; margin-top: 10px; }\n",
    "    .column-container { display: block }\n",
    "    table { font-size: 16px; }\n",
    "    .team-line { font-size: 20px; }\n",
    "    .leaders div { font-size: 16px; }\n",
    "    .leaders table { font-size: 16px; }\n",
    "    .leaders-note { font-size:16px; }\n",
    "    .notes { font-size: 16px; }\n",
    "    .column-40, .column-60 { width: 100%; }\n",
    "  }\n",
    "  @media (max-width: 700px) {\n",
    "    .main-title { font-size:28px; }\n",
    "    .subtitle { font-size:18px; }\n",
    "    .stats-subheader { font-size:14px; }\n",
    "    .game-header { font-size:22px; }\n",
    "    .boxscores-container { column-count: 1; column-gap: 10px; margin-top: 10px; }\n",
    "    .column-container { display: block }\n",
    "    table { font-size: 14px; }\n",
    "    .team-line { font-size: 18px; }\n",
    "    .leaders div { font-size: 14px; }\n",
    "    .leaders table { font-size: 14px; }\n",
    "    .leaders-note { font-size:14px; }\n",
    "    .notes { font-size: 14px; }\n",
    "    .column-40, .column-60 { width: 100%; }\n",
    "  } \n",
    "  @media print {\n",
    "      body { \n",
    "        background-color: #fff;\n",
    "          width:1650px;\n",
    "      }\n",
    "      .newspaper { box-shadow: none; max-width: none; padding: 0; margin: 0; }\n",
    "      .nav-container { display: none; }\n",
    "      .column-container { display: flex; }\n",
    "      .boxscores-container { column-count: 5; }\n",
    "    }\n",
    "  </style>\n",
    "  <script>\n", navigation_js, "\n    </script>\n",
    "</head>\n",
    "<body>\n",
    "  <div class='newspaper'>\n",
    "    <div class='header'>\n",
    "      <div class='date'>", display_date, "</div>\n",
    "      <h1 class='main-title'>WALDRN.COM/BOXSCORES</h1>\n",
    "      <div class='subtitle'>Daily Box Scores, Standings & League Leaders</div>\n",
    "    </div>\n",
    "    <div class='page'>\n"
  )
  
  # Add standings if available
  if (!is.null(standings_data)) {
    html_content <- paste0(
      html_content,
      "      <div class='section'>\n",
      "        <div class='column-container'>\n",
      "          <div class='column-40'>\n",
      "            <div class='boxscores-title'>A.L. Standings</div>\n",
      "            <div class='stats-subheader'>East Division</div>\n"
    )
    
    # Generate AL East standings table
    html_content <- paste0(
      html_content,
      "            <table class='standings-table'>\n",
      "              <thead>\n",
      "                <tr>\n",
      "                  <th class='team-col'>Team</th>\n",
      "                  <th class='num-col'>W</th>\n",
      "                  <th class='num-col'>L</th>\n",
      "                  <th class='pct-col'>Pct</th>\n",
      "                  <th class='pct-col'>GB</th>\n",
      "                  <th class='pct-col'>L10</th>\n",
      "                  <th class='pct-col'>Strk</th>\n",
      "                  <th class='pct-col'>Home</th>\n",
      "                  <th class='pct-col'>Away</th>\n",
      "                </tr>\n",
      "              </thead>\n",
      "              <tbody>\n"
    )
    
    for (i in 1:nrow(standings_data$al[[1]])) {
      row <- standings_data$al[[1]][i,]
      html_content <- paste0(
        html_content,
        "                <tr>\n",
        "                  <td class='team-col'>", row$Team, "</td>\n",
        "                  <td class='num-col'>", row$W, "</td>\n",
        "                  <td class='num-col'>", row$L, "</td>\n",
        "                  <td class='pct-col'>", row$PCT, "</td>\n",
        "                  <td class='pct-col'>", row$GB, "</td>\n",
        "                  <td class='pct-col'>", row$L10, "</td>\n",
        "                  <td class='pct-col'>", row$Strk, "</td>\n",
        "                  <td class='pct-col'>", row$H, "</td>\n",
        "                  <td class='pct-col'>", row$A, "</td>\n",
        "                </tr>\n"
      )
    }
    
    html_content <- paste0(
      html_content,
      "              </tbody>\n",
      "            </table>\n",
      "            <div class='stats-subheader'>Central Division</div>\n"
    )
    
    # Generate AL Central standings table
    html_content <- paste0(
      html_content,
      "            <table class='standings-table'>\n",
      "              <thead>\n",
      "                <tr>\n",
      "                  <th class='team-col'>Team</th>\n",
      "                  <th class='num-col'>W</th>\n",
      "                  <th class='num-col'>L</th>\n",
      "                  <th class='pct-col'>Pct</th>\n",
      "                  <th class='pct-col'>GB</th>\n",
      "                  <th class='pct-col'>L10</th>\n",
      "                  <th class='pct-col'>Strk</th>\n",
      "                  <th class='pct-col'>Home</th>\n",
      "                  <th class='pct-col'>Away</th>\n",
      "                </tr>\n",
      "              </thead>\n",
      "              <tbody>\n"
    )
    
    for (i in 1:nrow(standings_data$al[[2]])) {
      row <- standings_data$al[[2]][i,]
      html_content <- paste0(
        html_content,
        "                <tr>\n",
        "                  <td class='team-col'>", row$Team, "</td>\n",
        "                  <td class='num-col'>", row$W, "</td>\n",
        "                  <td class='num-col'>", row$L, "</td>\n",
        "                  <td class='pct-col'>", row$PCT, "</td>\n",
        "                  <td class='pct-col'>", row$GB, "</td>\n",
        "                  <td class='pct-col'>", row$L10, "</td>\n",
        "                  <td class='pct-col'>", row$Strk, "</td>\n",
        "                  <td class='pct-col'>", row$H, "</td>\n",
        "                  <td class='pct-col'>", row$A, "</td>\n",
        "                </tr>\n"
      )
    }
    
    html_content <- paste0(
      html_content,
      "              </tbody>\n",
      "            </table>\n",
      "            <div class='stats-subheader'>West Division</div>\n"
    )
    
    # Generate AL West standings table
    html_content <- paste0(
      html_content,
      "            <table class='standings-table'>\n",
      "              <thead>\n",
      "                <tr>\n",
      "                  <th class='team-col'>Team</th>\n",
      "                  <th class='num-col'>W</th>\n",
      "                  <th class='num-col'>L</th>\n",
      "                  <th class='pct-col'>Pct</th>\n",
      "                  <th class='pct-col'>GB</th>\n",
      "                  <th class='pct-col'>L10</th>\n",
      "                  <th class='pct-col'>Strk</th>\n",
      "                  <th class='pct-col'>Home</th>\n",
      "                  <th class='pct-col'>Away</th>\n",
      "                </tr>\n",
      "              </thead>\n",
      "              <tbody>\n"
    )
    
    for (i in 1:nrow(standings_data$al[[3]])) {
      row <- standings_data$al[[3]][i,]
      html_content <- paste0(
        html_content,
        "                <tr>\n",
        "                  <td class='team-col'>", row$Team, "</td>\n",
        "                  <td class='num-col'>", row$W, "</td>\n",
        "                  <td class='num-col'>", row$L, "</td>\n",
        "                  <td class='pct-col'>", row$PCT, "</td>\n",
        "                  <td class='pct-col'>", row$GB, "</td>\n",
        "                  <td class='pct-col'>", row$L10, "</td>\n",
        "                  <td class='pct-col'>", row$Strk, "</td>\n",
        "                  <td class='pct-col'>", row$H, "</td>\n",
        "                  <td class='pct-col'>", row$A, "</td>\n",
        "                </tr>\n"
      )
    }
    
    html_content <- paste0(
      html_content,
      "              </tbody>\n",
      "            </table>\n",
      "          </div>\n",
      "          <div class='column-60'>\n",
      "            <div class='boxscores-title'>A.L. LEADERS</div>\n",
      "            <div class='column-container leaders'>\n",
      "              <div class='column-40 leaders'>\n"
    )
    
    html_content <- paste0(
      html_content,
      "                <table class='leaders-table'>\n",
      "                  <thead>\n",
      "                    <tr>\n",
      "                      <th class='player-col'>Batting</th>\n",
      "                      <th class='stat-col'>G</th>\n",
      "                      <th class='stat-col'>AB</th>\n",
      "                      <th class='stat-col'>R</th>\n",
      "                      <th class='stat-col'>H</th>\n",
      "                      <th class='avg-col'>Avg.</th>\n",
      "                    </tr>\n",
      "                  </thead>\n",
      "                  <tbody>\n"
    )
    
    # Add batting leaders rows with updated classes
    for (i in 1:nrow(leaders_data$al_leaders$batting$avg_leaders)) {
      row <- leaders_data$al_leaders$batting$avg_leaders[i,]
      html_content <- paste0(
        html_content,
        "                    <tr>\n",
        "                      <td class='player-col'>", row$Player, ', ', row$Team, "</td>\n",
        "                      <td class='stat-col'>", row$G, "</td>\n",
        "                      <td class='stat-col'>", row$AB, "</td>\n",
        "                      <td class='stat-col'>", row$R, "</td>\n",
        "                      <td class='stat-col'>", row$H, "</td>\n",
        "                      <td class='avg-col'>", row$Value, "</td>\n",
        "                    </tr>\n"
      )
    }
    
    html_content <- paste0(
      html_content,
      "                  </tbody>\n",
      "                </table>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Home Runs: </b>", leaders_data$al_leaders$batting$hr_leaders, '\n',
      "                </div>\n",
      "                <div class='leaders-note'>",
      "                  <b>Runs Batted In: </b>", leaders_data$al_leaders$batting$rbi_leaders, '\n',
      "                </div>\n",
      "              </div>\n",
      "              <div class='column-60 leaders'>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Hits: </b>", leaders_data$al_leaders$batting$hits_leaders, '\n',
      "                </div>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Stolen Bases: </b>", leaders_data$al_leaders$batting$sb_leaders, '\n',
      "                </div>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Pitching: </b>", leaders_data$al_leaders$pitching$wins_leaders, '\n',
      "                </div>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Strikeouts: </b>", leaders_data$al_leaders$pitching$so_leaders, '\n',
      "                </div>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Saves: </b>", leaders_data$al_leaders$pitching$sv_leaders, '\n',
      "                </div>\n",
      "              </div>\n",
      "            </div>\n",
      "          </div>\n",
      "        </div>\n",
      "      </div>\n"
    )
  }
  
  # Add league leaders if available
  if (!is.null(leaders_data)) {
    html_content <- paste0(
      html_content,
      "      <div class='section'>\n",
      "        <div class='column-container'>\n",
      "          <div class='column-40'>\n",
      "            <div class='boxscores-title'>N.L. Standings</div>\n",
      "            <div class='stats-subheader'>East Division</div>\n"
      
    )
    
    # Generate NL standings table
    html_content <- paste0(
      html_content,
      "            <table class='standings-table'>\n",
      "              <thead>\n",
      "                <tr>\n",
      "                  <th class='team-col'>Team</th>\n",
      "                  <th class='num-col'>W</th>\n",
      "                  <th class='num-col'>L</th>\n",
      "                  <th class='pct-col'>Pct</th>\n",
      "                  <th class='pct-col'>GB</th>\n",
      "                  <th class='pct-col'>L10</th>\n",
      "                  <th class='pct-col'>Strk</th>\n",
      "                  <th class='pct-col'>Home</th>\n",
      "                  <th class='pct-col'>Away</th>\n",
      "                </tr>\n",
      "              </thead>\n",
      "              <tbody>\n"
    )
    
    for (i in 1:nrow(standings_data$nl[[1]])) {
      row <- standings_data$nl[[1]][i,]
      html_content <- paste0(
        html_content,
        "                <tr>\n",
        "                  <td class='team-col'>", row$Team, "</td>\n",
        "                  <td class='num-col'>", row$W, "</td>\n",
        "                  <td class='num-col'>", row$L, "</td>\n",
        "                  <td class='pct-col'>", row$PCT, "</td>\n",
        "                  <td class='pct-col'>", row$GB, "</td>\n",
        "                  <td class='pct-col'>", row$L10, "</td>\n",
        "                  <td class='pct-col'>", row$Strk, "</td>\n",
        "                  <td class='pct-col'>", row$H, "</td>\n",
        "                  <td class='pct-col'>", row$A, "</td>\n",
        "                </tr>\n"
      )
    }
    
    html_content <- paste0(
      html_content,
      "              </tbody>\n",
      "            </table>\n",
      "            <div class='stats-subheader'>Central Division</div>\n"
      
    )
    
    # Generate NL standings table
    html_content <- paste0(
      html_content,
      "            <table class='standings-table'>\n",
      "              <thead>\n",
      "                <tr>\n",
      "                  <th class='team-col'>Team</th>\n",
      "                  <th class='num-col'>W</th>\n",
      "                  <th class='num-col'>L</th>\n",
      "                  <th class='pct-col'>Pct</th>\n",
      "                  <th class='pct-col'>GB</th>\n",
      "                  <th class='pct-col'>L10</th>\n",
      "                  <th class='pct-col'>Strk</th>\n",
      "                  <th class='pct-col'>Home</th>\n",
      "                  <th class='pct-col'>Away</th>\n",
      "                </tr>\n",
      "              </thead>\n",
      "              <tbody>\n"
    )
    
    for (i in 1:nrow(standings_data$nl[[2]])) {
      row <- standings_data$nl[[2]][i,]
      html_content <- paste0(
        html_content,
        "                <tr>\n",
        "                  <td class='team-col'>", row$Team, "</td>\n",
        "                  <td class='num-col'>", row$W, "</td>\n",
        "                  <td class='num-col'>", row$L, "</td>\n",
        "                  <td class='pct-col'>", row$PCT, "</td>\n",
        "                  <td class='pct-col'>", row$GB, "</td>\n",
        "                  <td class='pct-col'>", row$L10, "</td>\n",
        "                  <td class='pct-col'>", row$Strk, "</td>\n",
        "                  <td class='pct-col'>", row$H, "</td>\n",
        "                  <td class='pct-col'>", row$A, "</td>\n",
        "                </tr>\n"
      )
    }
    
    html_content <- paste0(
      html_content,
      "              </tbody>\n",
      "            </table>\n",
      "            <div class='stats-subheader'>West Division</div>\n"
      
    )
    
    # Generate NL standings table
    html_content <- paste0(
      html_content,
      "            <table class='standings-table'>\n",
      "              <thead>\n",
      "                <tr>\n",
      "                  <th class='team-col'>Team</th>\n",
      "                  <th class='num-col'>W</th>\n",
      "                  <th class='num-col'>L</th>\n",
      "                  <th class='pct-col'>Pct</th>\n",
      "                  <th class='pct-col'>GB</th>\n",
      "                  <th class='pct-col'>L10</th>\n",
      "                  <th class='pct-col'>Strk</th>\n",
      "                  <th class='pct-col'>Home</th>\n",
      "                  <th class='pct-col'>Away</th>\n",
      "                </tr>\n",
      "              </thead>\n",
      "              <tbody>\n"
    )
    
    for (i in 1:nrow(standings_data$nl[[3]])) {
      row <- standings_data$nl[[3]][i,]
      html_content <- paste0(
        html_content,
        "                <tr>\n",
        "                  <td class='team-col'>", row$Team, "</td>\n",
        "                  <td class='num-col'>", row$W, "</td>\n",
        "                  <td class='num-col'>", row$L, "</td>\n",
        "                  <td class='pct-col'>", row$PCT, "</td>\n",
        "                  <td class='pct-col'>", row$GB, "</td>\n",
        "                  <td class='pct-col'>", row$L10, "</td>\n",
        "                  <td class='pct-col'>", row$Strk, "</td>\n",
        "                  <td class='pct-col'>", row$H, "</td>\n",
        "                  <td class='pct-col'>", row$A, "</td>\n",
        "                </tr>\n"
      )
    }
    
    html_content <- paste0(
      html_content,
      "              </tbody>\n",
      "            </table>\n",
      "          </div>\n",
      "          <div class='column-60'>\n",
      "            <div class='boxscores-title'>N.L. LEADERS</div>\n",
      "            <div class='column-container leaders'>\n",
      "              <div class='column-40 leaders'>\n"
    )
    
    html_content <- paste0(
      html_content,
      "                <table class='leaders-table'>\n",
      "                  <thead>\n",
      "                    <tr>\n",
      "                      <th class='player-col'>Batting</th>\n",
      "                      <th class='stat-col'>G</th>\n",
      "                      <th class='stat-col'>AB</th>\n",
      "                      <th class='stat-col'>R</th>\n",
      "                      <th class='stat-col'>H</th>\n",
      "                      <th class='avg-col'>Avg.</th>\n",
      "                    </tr>\n",
      "                  </thead>\n",
      "                  <tbody>\n"
    )
    
    # Add batting leaders rows with updated classes
    for (i in 1:nrow(leaders_data$nl_leaders$batting$avg_leaders)) {
      row <- leaders_data$nl_leaders$batting$avg_leaders[i,]
      html_content <- paste0(
        html_content,
        "                    <tr>\n",
        "                      <td class='player-col'>", row$Player, ', ', row$Team, "</td>\n",
        "                      <td class='stat-col'>", row$G, "</td>\n",
        "                      <td class='stat-col'>", row$AB, "</td>\n",
        "                      <td class='stat-col'>", row$R, "</td>\n",
        "                      <td class='stat-col'>", row$H, "</td>\n",
        "                      <td class='avg-col'>", row$Value, "</td>\n",
        "                    </tr>\n"
      )
    }
    
    html_content <- paste0(
      html_content,
      "                  </tbody>\n",
      "                </table>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Home Runs: </b>", leaders_data$nl_leaders$batting$hr_leaders, '\n',
      "                </div>\n",
      "                <div class='leaders-note'>",
      "                  <b>Runs Batted In: </b>", leaders_data$nl_leaders$batting$rbi_leaders, '\n',
      "                </div>\n",
      "              </div>\n",
      "              <div class='column-60 leaders'>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Hits: </b>", leaders_data$nl_leaders$batting$hits_leaders, '\n',
      "                </div>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Stolen Bases: </b>", leaders_data$nl_leaders$batting$sb_leaders, '\n',
      "                </div>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Pitching: </b>", leaders_data$nl_leaders$pitching$wins_leaders, '\n',
      "                </div>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Strikeouts: </b>", leaders_data$nl_leaders$pitching$so_leaders, '\n',
      "                </div>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Saves: </b>", leaders_data$nl_leaders$pitching$sv_leaders, '\n',
      "                </div>\n",
      "              </div>\n",
      "            </div>\n",
      "          </div>\n",
      "        </div>\n",
      "      </div>\n"
    )
  }
  
  # Add box scores section
  html_content <- paste0(
    html_content,
    "    </div>\n",
    "    <div class='boxscores-title'>BOX SCORES</div>\n",
    "    <div class='boxscores-container'>\n"
  )
  
  # Add each game box score
  for (i in seq_along(games_data)) {
    game <- games_data[[i]]
    
    # Game title with team names and scores
    game_title <- if (game$teams$visitor$score > game$teams$home$score) {
      paste0(
        game$teams$visitor$name, " ", game$teams$visitor$score, ", ",
        game$teams$home$name, " ", game$teams$home$score
      )
    } else {
      paste0(
        game$teams$home$name, " ", game$teams$home$score, ", ",
        game$teams$visitor$name, " ", game$teams$visitor$score
      )
    }
    
    html_content <- paste0(
      html_content,
      "      <div class='game-container'>\n",
      "        <div class='game-header'>", game_title, "</div>\n",
      "        <div class='team-line'>\n",
      "          <div class='team-name'>", game$teams$visitor$place, "</div>\n",
      "          <div class='team-score'>", 
      paste(gsub("(.{3})", "\\1&numsp;", paste0(game$teams$visitor$line, collapse='')), "&mdash; ",
            gsub('\\s','&numsp;',sprintf('%02s', as.integer(game$teams$visitor$stats[1]))), "&numsp;",
            gsub('\\s','&numsp;',sprintf('%02s', as.integer(game$teams$visitor$stats[2]))), "&numsp;",
            as.integer(game$teams$visitor$stats[3])), 
      "          </div>\n",
      "        </div>\n",
      "        <div class='team-line'>\n",
      "          <div class='team-name'>", game$teams$home$place, "</div>\n",
      "          <div class='team-score'>", 
      paste(gsub("(.{3})", "\\1&numsp;", paste0(game$teams$home$line, collapse='')), "&mdash; ",
            gsub('\\s','&numsp;',sprintf('%02s', as.integer(game$teams$home$stats[1]))), "&numsp;",
            gsub('\\s','&numsp;',sprintf('%02s', as.integer(game$teams$home$stats[2]))), "&numsp;",
            as.integer(game$teams$home$stats[3])), 
      "          </div>\n",
      "        </div>\n"
    )
    
    # Visitor batting
    html_content <- paste0(
      html_content,
      "        <table class='batting-table'>\n",
      "        <thead>\n",
      "          <tr>\n",
      "            <th class='player-col'>", game$teams$visitor$place, "</th>\n",
      "            <th class='stat-col'>AB</th>\n",
      "            <th class='stat-col'>R</th>\n",
      "            <th class='stat-col'>H</th>\n",
      "            <th class='stat-col'>BI</th>\n",
      "            <th class='stat-col'>BB</th>\n",
      "            <th class='stat-col'>SO</th>\n",
      "            <th class='avg-col'>Avg</th>\n",
      "          </tr>\n",
      "        </thead>\n",
      "        <tbody>\n"
    )
    
    # Add visitor batting rows
    for (j in 1:nrow(game$batting$visitor)) {
      row <- game$batting$visitor[j,]
      player_name <- names(row)[1] # First column name is team place
      html_content <- paste0(
        html_content,
        "          <tr>\n",
        "            <td class='player-col'>", row[[1]], "</td>\n", # Player name
        "            <td class='stat-col'>", row$AB, "</td>\n",
        "            <td class='stat-col'>", row$R, "</td>\n",
        "            <td class='stat-col'>", row$H, "</td>\n",
        "            <td class='stat-col'>", row$BI, "</td>\n",
        "            <td class='stat-col'>", row$BB, "</td>\n",
        "            <td class='stat-col'>", row$SO, "</td>\n",
        "            <td class='avg-col'>", row$Avg, "</td>\n",
        "          </tr>\n"
      )
    }
    
    html_content <- paste0(
      html_content,
      "        </tbody>\n",
      "        </table>\n"
    )
    
    # Home batting
    html_content <- paste0(
      html_content,
      "        <table class='batting-table'>\n",
      "        <thead>\n",
      "          <tr>\n",
      "            <th class='player-col'>", game$teams$home$place, "</th>\n",
      "            <th class='stat-col'>AB</th>\n",
      "            <th class='stat-col'>R</th>\n",
      "            <th class='stat-col'>H</th>\n",
      "            <th class='stat-col'>BI</th>\n",
      "            <th class='stat-col'>BB</th>\n",
      "            <th class='stat-col'>SO</th>\n",
      "            <th class='avg-col'>Avg</th>\n",
      "          </tr>\n",
      "        </thead>\n",
      "        <tbody>\n"
    )
    
    # Add home batting rows
    for (j in 1:nrow(game$batting$home)) {
      row <- game$batting$home[j,]
      player_name <- names(row)[1] # First column name is team place
      html_content <- paste0(
        html_content,
        "          <tr>\n",
        "            <td class='player-col'>", row[[1]], "</td>\n", # Player name
        "            <td class='stat-col'>", row$AB, "</td>\n",
        "            <td class='stat-col'>", row$R, "</td>\n",
        "            <td class='stat-col'>", row$H, "</td>\n",
        "            <td class='stat-col'>", row$BI, "</td>\n",
        "            <td class='stat-col'>", row$BB, "</td>\n",
        "            <td class='stat-col'>", row$SO, "</td>\n",
        "            <td class='avg-col'>", row$Avg, "</td>\n",
        "          </tr>\n"
      )
    }
    
    html_content <- paste0(
      html_content,
      "        </tbody>\n",
      "        </table>\n",
      "        <div class='notes'>", game$batting$notes, "</div>\n"
    )
    
    # Pitching tables - Visitor
    html_content <- paste0(
      html_content,
      "        <table class='pitching-table'>\n",
      "        <thead>\n",
      "          <tr>\n",
      "            <th class='player-col'>", game$teams$visitor$place, "</th>\n",
      "            <th class='ip-col'>IP</th>\n",
      "            <th class='stat-col'>H</th>\n",
      "            <th class='stat-col'>R</th>\n",
      "            <th class='stat-col'>ER</th>\n",
      "            <th class='stat-col'>BB</th>\n",
      "            <th class='stat-col'>SO</th>\n",
      "            <th class='stat-col'>NP</th>\n",
      "            <th class='era-col'>ERA</th>\n",
      "          </tr>\n",
      "        </thead>\n",
      "        <tbody>\n"
    )
    
    # Add visitor pitching rows
    for (j in 1:nrow(game$pitching$visitor)) {
      row <- game$pitching$visitor[j,]
      player_name <- names(row)[1] # First column name is team place
      html_content <- paste0(
        html_content,
        "          <tr>\n",
        "            <td class='player-col'>", row[[1]], "</td>\n", # Player name
        "            <td class='ip-col'>", row$IP, "</td>\n",
        "            <td class='stat-col'>", row$H, "</td>\n",
        "            <td class='stat-col'>", row$R, "</td>\n",
        "            <td class='stat-col'>", row$ER, "</td>\n",
        "            <td class='stat-col'>", row$BB, "</td>\n",
        "            <td class='stat-col'>", row$SO, "</td>\n",
        "            <td class='stat-col'>", row$NP, "</td>\n",
        "            <td class='era-col'>", row$ERA, "</td>\n",
        "          </tr>\n"
      )
    }
    
    html_content <- paste0(
      html_content,
      "        </tbody>\n",
      "        </table>\n"
    )
    
    # Pitching tables - Home
    html_content <- paste0(
      html_content,
      "        <table class='pitching-table'>\n",
      "        <thead>\n",
      "          <tr>\n",
      "            <th class='player-col'>", game$teams$home$place, "</th>\n",
      "            <th class='ip-col'>IP</th>\n",
      "            <th class='stat-col'>H</th>\n",
      "            <th class='stat-col'>R</th>\n",
      "            <th class='stat-col'>ER</th>\n",
      "            <th class='stat-col'>BB</th>\n",
      "            <th class='stat-col'>SO</th>\n",
      "            <th class='stat-col'>NP</th>\n",
      "            <th class='era-col'>ERA</th>\n",
      "          </tr>\n",
      "        </thead>\n",
      "        <tbody>\n"
    )
    
    # Add home pitching rows
    for (j in 1:nrow(game$pitching$home)) {
      row <- game$pitching$home[j,]
      player_name <- names(row)[1] # First column name is team place
      html_content <- paste0(
        html_content,
        "          <tr>\n",
        "            <td class='player-col'>", row[[1]], "</td>\n", # Player name
        "            <td class='ip-col'>", row$IP, "</td>\n",
        "            <td class='stat-col'>", row$H, "</td>\n",
        "            <td class='stat-col'>", row$R, "</td>\n",
        "            <td class='stat-col'>", row$ER, "</td>\n",
        "            <td class='stat-col'>", row$BB, "</td>\n",
        "            <td class='stat-col'>", row$SO, "</td>\n",
        "            <td class='stat-col'>", row$NP, "</td>\n",
        "            <td class='era-col'>", row$ERA, "</td>\n",
        "          </tr>\n"
      )
    }
    
    html_content <- paste0(
      html_content,
      "        </tbody>\n",
      "        </table>\n"
    )
    
    # Game notes
    html_content <- paste0(
      html_content,
      "        <div class='notes'>", game$pitching$notes, "</div>\n",
      "      </div>\n"
    )
  }
  
  # Close the boxscores-container and newspaper divs
  html_content <- paste0(
    html_content,
    "    </div>\n",
    "  </div>\n",
    "</body>\n",
    "</html>"
  )
  
  return(html_content)
}

#' Main function to process box scores for a given date
#' 
#' @param year Year (YYYY)
#' @param month Month (MM)
#' @param day Day (DD)
#' @param season Season year
#' @param output_dir Directory to save output files
#' @param save_newspaper Logical, whether to save newspaper-style HTML
#' @param save_pdf Logical, whether to save pdf of newspaper-style HTML
#' @param include_standings Logical, whether to include standings in newspaper
#' @param include_leaders Logical, whether to include league leaders in newspaper
#' @param database Database choice. 'MLB' or 'BR'
#' @return List of all games' data
get_box_scores <- function(year, month, day,
                           season = NULL, 
                           output_dir = "game_data",
                           save_newspaper = TRUE,
                           save_pdf = TRUE,
                           include_standings = TRUE,
                           include_leaders = TRUE,
                           database = 'MLB') {
  # Format date for processing
  formatted_date <- paste0(
    year, "-", 
    sprintf("%02d", as.integer(month)), "-", 
    sprintf("%02d", as.integer(day))
  )
  
  if (is.null(season)) {
    season <- year
  }
  
  # Format date string for file naming
  date_str <- paste0(year, sprintf("%02d", as.integer(month)), sprintf("%02d", as.integer(day)))
  
  # Create output directory
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Process all games
  print(paste0("Processing games for ", formatted_date, "..."))
  games_data <- process_all_gamesMLB(year, month, day)
  print(paste0("Found ", length(games_data), " games"))
  
  # Generate and save newspaper-style page if requested
  if (save_newspaper) {
    print("Generating newspaper page...")
    
    # Get standings data if needed
    standings_data <- NULL
    if (include_standings) {
      print("Getting standings data...")
      if (database == 'MLB') {
        standings_data <- get_standingsMLB(formatted_date)
      } else if (database == 'BR') {
        standings_data <- get_standingsBR(formatted_date)
      }
    }
    
    # Get league leaders data if needed
    leaders_data <- NULL
    if (include_leaders) {
      print("Getting league leaders data...")
      if (database == 'MLB') {
        leaders_data <- get_league_leadersMLB(formatted_date)
      } else if (database == 'BR') {
        leaders_data <- get_league_leadersBR(formatted_date)
      }
    }
    
    # Generate the newspaper page
    print("Creating newspaper HTML...")
    newspaper_html <- generate_newspaper_page2(
      games_data, 
      date_str, 
      standings_data, 
      leaders_data
    )
    
    # Write the newspaper HTML file
    newspaper_file <- file.path(output_dir, paste0(date_str, ".html"))
    print(paste0("Writing newspaper to ", newspaper_file))
    writeLines(newspaper_html, newspaper_file)
    print("Newspaper HTML file created successfully")
  }
  
  # Generate PDF if requested
  if (save_newspaper && save_pdf) {
    print("Generating PDF version...")
    pdf_file <- file.path(output_dir, paste0(date_str, ".pdf"))
    tryCatch({
      print_to_pdf(paste0('file://', here(newspaper_file)), here(pdf_file), landscape=TRUE, paperWidth=11.7, paperHeight=15.14, wait_ = TRUE)
      print(paste0("PDF created successfully: ", pdf_file))
    }, error = function(e) {
      warning(paste0("Failed to generate PDF: ", e$message))
    })
  }
}

#' Get standings data organized by league
#' 
#' @param date Date string in format YYYY-MM-DD
#' @return List with AL and NL standings data.tables
get_standingsMLB <- function(date) {

  season <- substr(date,1,4)
  
  getStandingsLg <- function(lg) {
    resp_lg <- GET(
      paste0('https://statsapi.mlb.com/api/v1/standings?leagueId=', lg, '&season=', season, '&date=', date)
    ) %>%
      content(as = 'text') %>%
      fromJSON()
    
    resp_div <- resp_lg$records$division$link %>%
      lapply(function(x) {
        resp <- GET(paste0('https://statsapi.mlb.com/', x)) %>%
          content(as = 'text') %>%
          fromJSON()
        
        resp$divisions$nameShort %>%
          gsub('AL ', '', ., fixed = TRUE) %>%
          gsub('NL ', '', ., fixed = TRUE)
      }) %>%
      unlist()
    
    lapply(1:length(resp_div), function(i) {
      data.table(
        div = resp_div[i],
        Team = resp_lg$records$teamRecords[[i]]$team$link %>%
          lapply(function(x) {
            resp_team <- GET(
              paste0('https://statsapi.mlb.com/', x)
            ) %>%
              content(as = 'text') %>%
              fromJSON()
            
            resp_team$teams$shortName
          }) %>%
          unlist(),
        W = resp_lg$records$teamRecords[[i]]$wins,
        L = resp_lg$records$teamRecords[[i]]$losses,
        PCT = resp_lg$records$teamRecords[[i]]$winningPercentage,
        GB = resp_lg$records$teamRecords[[i]]$divisionGamesBack,
        L10 = resp_lg$records$teamRecords[[i]]$records$splitRecords %>%
          lapply(function(x) {
            x %>%
              data.table() %>%
              .[type == 'lastTen', paste0(wins, '-', losses)]
          }) %>%
          unlist(),
        H = resp_lg$records$teamRecords[[i]]$records$splitRecords %>%
          lapply(function(x) {
            x %>%
              data.table() %>%
              .[type == 'home', paste0(wins, '-', losses)]
          }) %>%
          unlist(),
        A = resp_lg$records$teamRecords[[i]]$records$splitRecords %>%
          lapply(function(x) {
            x %>%
              data.table() %>%
              .[type == 'away', paste0(wins, '-', losses)]
          }) %>%
          unlist(),
        Strk = resp_lg$records$teamRecords[[i]]$streak[, 'streakCode']
      )
    })
  }
  
  # Return as a list
  return(list(
    al = getStandingsLg(103),
    nl = getStandingsLg(104)
  ))
}

#' Get league leaders data organized by category
#' 
#' @param date Date string in format YYYY-MM-DD
#' @return List with batting and pitching leaders data.tables
get_league_leadersMLB <- function(date) {
  yr <- year(as.Date(date))
  
  # Function to process leaders by league
  process_leaders_by_league <- function(lg) {
    lg <- ifelse(lg == 'AL', 103, 104)
    
    # Create batting leader tables
    
    # Batting Average
    resp_ba <- GET(
      paste0('https://statsapi.mlb.com/api/v1/stats/leaders?leaderCategories=hits&statGroup=hitting&statType=byDateRange&limit=100&leagueId=', lg, '&season=', yr, '&startDate=', yr, '-01-01&endDate=', date)
    ) %>%
      content(as = 'text') %>%
      fromJSON()
    
    resp_tmstat <- GET(paste0('https://statsapi.mlb.com/api/v1/standings?standingsTypes=regularSeason&leagueId=', lg, '&season=', yr, '&date=', date)) %>%
      content(as = 'text') %>%
      fromJSON()
    
    dt_teamgm <- resp_tmstat$records$teamRecords %>%
      lapply(function(x) {
        data.table(id = x[['team']]$id, gamesPlayed = x[['gamesPlayed']])
      }) %>%
      rbindlist()
    
    if (length(resp_ba$leagueLeaders$leaders) > 0) {
      
      avg_leaders <- apply(resp_ba$leagueLeaders$leaders[[1]], 1, function(x) {

        resp_p <- GET(
          paste0('https://statsapi.mlb.com/', x['person.link'])
        ) %>%
          content(as = 'text') %>%
          fromJSON()
        
        c_bn <- resp_p$people$boxscoreName
        c_bn2 <- formatBoxName(c_bn)
        
        resp_tm <- GET(
          paste0('https://statsapi.mlb.com/', x['team.link'])
        ) %>%
          content(as = 'text') %>%
          fromJSON()
        
        resp_stats <- GET(
          paste0('https://statsapi.mlb.com/api/v1/people/', x['person.id'], '/stats?stats=byDateRange&group=hitting&leagueId=', lg, '&gameType=R&season=', yr, '&startDate=', yr, '-01-01&endDate=', date)
        ) %>%
          content(as = 'text') %>%
          fromJSON()
        
        dt_stats <- resp_stats$stats$splits %>%
          .[[1]] %>%
          data.table() %>%
          .[sport.abbreviation == 'MLB',
            .(G = stat.gamesPlayed,
              PA = stat.plateAppearances,
              AB = stat.atBats,
              R = stat.runs,
              H = stat.hits,
              Value = stat.avg)] %>%
          .[H > 0]
        
        dt_stats <- data.table(Player = c_bn2,
                               Team = resp_tm$teams$shortName,
                               id = resp_tm$teams$id) %>%
          cbind(dt_stats)
        
      }) %>%
        rbindlist() %>%
        dt_teamgm[., on = 'id'] %>%
        .[PA / gamesPlayed >= 3.1] %>%
        .[!is.na(as.numeric(Value))] %>%
        .[order(-as.numeric(Value))] %>%
        .[1:12]
    } else {
      avg_leaders <- data.table(Player = '',
                                Team = '',
                                AB = '',
                                R = '',
                                H = '',
                                Value = '')
    }
    
    # Home Runs
    resp_hr <- GET(
      paste0('https://statsapi.mlb.com/api/v1/stats/leaders?leaderCategories=homeRuns&statGroup=hitting&statType=byDateRange&limit=8&leagueId=', lg, '&season=', yr, '&startDate=', yr, '-01-01&endDate=', date)
    ) %>%
      content(as = 'text') %>%
      fromJSON()
    
    if (length(resp_hr$leagueLeaders$leaders) > 0) {
      
      hr_leaders <- apply(resp_hr$leagueLeaders$leaders[[1]] %>% head(12), 1, function(x) {

        resp_p <- GET(
          paste0('https://statsapi.mlb.com/', x['person.link'])
        ) %>%
          content(as = 'text') %>%
          fromJSON()
        
        c_bn <- resp_p$people$boxscoreName
        c_bn2 <- formatBoxName(c_bn)
        
        resp_tm <- GET(
          paste0('https://statsapi.mlb.com/', x['team.link'])
        ) %>%
          content(as = 'text') %>%
          fromJSON()
        
        paste0(c_bn2, ', ', resp_tm$teams$shortName, ', ', x['value'])
        
      }) %>%
        paste0(collapse='; ')
    } else {
      hr_leaders <- ''
    }
    
    # RBI
    resp_rbi <- GET(
      paste0('https://statsapi.mlb.com/api/v1/stats/leaders?leaderCategories=runsBattedIn&statGroup=hitting&statType=byDateRange&limit=8&leagueId=', lg, '&season=', yr, '&startDate=', yr, '-01-01&endDate=', date)
    ) %>%
      content(as = 'text') %>%
      fromJSON()
    
    if (length(resp_rbi$leagueLeaders$leaders) > 0) {
      
      rbi_leaders <- apply(resp_rbi$leagueLeaders$leaders[[1]] %>% head(12), 1, function(x) {

        resp_p <- GET(
          paste0('https://statsapi.mlb.com/', x['person.link'])
        ) %>%
          content(as = 'text') %>%
          fromJSON()
        
        c_bn <- resp_p$people$boxscoreName
        c_bn2 <- formatBoxName(c_bn)
        
        resp_tm <- GET(
          paste0('https://statsapi.mlb.com/', x['team.link'])
        ) %>%
          content(as = 'text') %>%
          fromJSON()
        
        paste0(c_bn2, ', ', resp_tm$teams$shortName, ', ', x['value'])
        
      }) %>%
        paste0(collapse='; ')
    } else {
      rbi_leaders <- ''
    }
    
    # Hits
    resp_hits <- GET(
      paste0('https://statsapi.mlb.com/api/v1/stats/leaders?leaderCategories=hits&statGroup=hitting&statType=byDateRange&limit=8&leagueId=', lg, '&season=', yr, '&startDate=', yr, '-01-01&endDate=', date)
    ) %>%
      content(as = 'text') %>%
      fromJSON()
    
    if (length(resp_hits$leagueLeaders$leaders) > 0) {
      
      hits_leaders <- apply(resp_hits$leagueLeaders$leaders[[1]] %>% head(12), 1, function(x) {

        resp_p <- GET(
          paste0('https://statsapi.mlb.com/', x['person.link'])
        ) %>%
          content(as = 'text') %>%
          fromJSON()
        
        c_bn <- resp_p$people$boxscoreName
        c_bn2 <- formatBoxName(c_bn)
        
        resp_tm <- GET(
          paste0('https://statsapi.mlb.com/', x['team.link'])
        ) %>%
          content(as = 'text') %>%
          fromJSON()
        
        paste0(c_bn2, ', ', resp_tm$teams$shortName, ', ', x['value'])
        
      }) %>%
        paste0(collapse='; ')
    } else {
      hits_leaders <- ''
    }
    
    # Stolen Bases
    resp_sb <- GET(
      paste0('https://statsapi.mlb.com/api/v1/stats/leaders?leaderCategories=stolenBases&statGroup=hitting&statType=byDateRange&limit=8&leagueId=', lg, '&season=', yr, '&startDate=', yr, '-01-01&endDate=', date)
    ) %>%
      content(as = 'text') %>%
      fromJSON()
    
    if (length(resp_sb$leagueLeaders$leaders) > 0) {
      
      sb_leaders <- apply(resp_sb$leagueLeaders$leaders[[1]] %>% head(12), 1, function(x) {

        resp_p <- GET(
          paste0('https://statsapi.mlb.com/', x['person.link'])
        ) %>%
          content(as = 'text') %>%
          fromJSON()
        
        c_bn <- resp_p$people$boxscoreName
        c_bn2 <- formatBoxName(c_bn)
        
        resp_tm <- GET(
          paste0('https://statsapi.mlb.com/', x['team.link'])
        ) %>%
          content(as = 'text') %>%
          fromJSON()
        
        paste0(c_bn2, ', ', resp_tm$teams$shortName, ', ', x['value'])
        
      }) %>%
        paste0(collapse='; ')
    } else {
      sb_leaders <- ''
    }
    
    # Process pitching stats
    
    # Wins
    resp_w <- GET(
      paste0('https://statsapi.mlb.com/api/v1/stats/leaders?leaderCategories=wins&statGroup=pitching&statType=byDateRange&limit=8&leagueId=', lg, '&season=', yr, '&startDate=', yr, '-01-01&endDate=', date)
    ) %>%
      content(as = 'text') %>%
      fromJSON()
    
    if (length(resp_w$leagueLeaders$leaders) > 0) {
      
      w_leaders <- apply(resp_w$leagueLeaders$leaders[[1]], 1, function(x) {

        resp_p <- GET(
          paste0('https://statsapi.mlb.com/', x['person.link'])
        ) %>%
          content(as = 'text') %>%
          fromJSON()
        
        c_bn <- resp_p$people$boxscoreName
        c_bn2 <- formatBoxName(c_bn)
        
        resp_tm <- GET(
          paste0('https://statsapi.mlb.com/', x['team.link'])
        ) %>%
          content(as = 'text') %>%
          fromJSON()
        
        resp_stats <- GET(
          paste0('https://statsapi.mlb.com/api/v1/people/', x['person.id'], '/stats?stats=byDateRange&group=pitching&leagueId=', lg, '&gameType=R&season=', yr, '&startDate=', yr, '-01-01&endDate=', date)
        ) %>%
          content(as = 'text') %>%
          fromJSON()
        
        dt_stats <- resp_stats$stats$splits %>%
          .[[1]] %>%
          data.table() %>%
          .[sport.abbreviation == 'MLB',
            .(W = stat.wins,
              L = stat.losses,
              pct = stat.wins / (stat.wins + stat.losses))]
        
        data.table(Player = c_bn2, Team = resp_tm$teams$shortName) %>%
          cbind(dt_stats)
        
      }) %>%
        rbindlist() %>%
        .[order(-W, -pct)] %>%
        .[, seq := 1:.N] %>%
        .[, rank := cumsum(W != shift(W, fill = 0))] %>%
        .[, rank2 := cumsum(pct != shift(pct, fill = 0)), rank]
      
      dt_lrank <- w_leaders[8, .(rank, rank2)]
      
      w_leaders <- w_leaders %>%
        .[W > 0 &
            seq <= 10 &
            !((rank > dt_lrank$rank) |
                (rank = dt_lrank$rank & rank2 > dt_lrank$rank2))] %>%
        .[, paste0(Player, ', ', Team, ', ', W, '-', L, ', ', sub('0\\.','.',sprintf('%#.3f', pct)))] %>%
        paste0(collapse='; ')
    } else {
      w_leaders <- ''
    }
    
    # Strikeouts
    resp_so <- GET(
      paste0('https://statsapi.mlb.com/api/v1/stats/leaders?leaderCategories=strikeOuts&statGroup=pitching&statType=byDateRange&limit=8&leagueId=', lg, '&season=', yr, '&startDate=', yr, '-01-01&endDate=', date)
    ) %>%
      content(as = 'text') %>%
      fromJSON()
    
    if (length(resp_so$leagueLeaders$leaders) > 0) {
      
      so_leaders <- apply(resp_so$leagueLeaders$leaders[[1]] %>% head(12), 1, function(x) {

        resp_p <- GET(
          paste0('https://statsapi.mlb.com/', x['person.link'])
        ) %>%
          content(as = 'text') %>%
          fromJSON()
        
        c_bn <- resp_p$people$boxscoreName
        c_bn2 <- formatBoxName(c_bn)
        
        resp_tm <- GET(
          paste0('https://statsapi.mlb.com/', x['team.link'])
        ) %>%
          content(as = 'text') %>%
          fromJSON()
        
        paste0(c_bn2, ', ', resp_tm$teams$shortName, ', ', x['value'])
        
      }) %>%
        paste0(collapse='; ')
    } else {
      so_leaders <- ''
    }
    
    # Saves
    resp_sv <- GET(
      paste0('https://statsapi.mlb.com/api/v1/stats/leaders?leaderCategories=saves&statGroup=pitching&statType=byDateRange&limit=8&leagueId=', lg, '&season=', yr, '&startDate=', yr, '-01-01&endDate=', date)
    ) %>%
      content(as = 'text') %>%
      fromJSON()
    
    if (length(resp_sv$leagueLeaders$leaders) > 0) {
      
      sv_leaders <- apply(resp_sv$leagueLeaders$leaders[[1]] %>% head(12), 1, function(x) {

        resp_p <- GET(
          paste0('https://statsapi.mlb.com/', x['person.link'])
        ) %>%
          content(as = 'text') %>%
          fromJSON()
        
        c_bn <- resp_p$people$boxscoreName
        c_bn2 <- formatBoxName(c_bn)
        
        resp_tm <- GET(
          paste0('https://statsapi.mlb.com/', x['team.link'])
        ) %>%
          content(as = 'text') %>%
          fromJSON()
        
        paste0(c_bn2, ', ', resp_tm$teams$shortName, ', ', x['value'])
        
      }) %>%
        paste0(collapse='; ')
    } else {
      sv_leaders <- ''
    }
    
    # Combine all batting leaders
    all_bat_leaders <- list(
      avg_leaders = avg_leaders,
      hr_leaders = hr_leaders,
      rbi_leaders = rbi_leaders,
      hits_leaders = hits_leaders,
      sb_leaders = sb_leaders
    )
    
    # Combine all pitching leaders
    all_pitch_leaders <- list(
      wins_leaders = w_leaders,
      so_leaders = so_leaders,
      sv_leaders = sv_leaders
    )
    
    return(list(
      batting = all_bat_leaders,
      pitching = all_pitch_leaders
    ))
  }
  
  # Process AL leaders
  al_leaders <- process_leaders_by_league("AL")
  
  # Process NL leaders
  nl_leaders <- process_leaders_by_league("NL")
  
  return(list(
    al_leaders = al_leaders,
    nl_leaders = nl_leaders
  ))
}

#' Print HTML to PDF using chromote
#'
#' @param url Input URL
#' @param filename Output file name
#' @param wait_ If TRUE, run in synchronous mode,
#' otherwise, run in asynchronous mode.
#' @param ... Additional parameters for Page.printToPDF, see
#' <https://chromedevtools.github.io/devtools-protocol/tot/Page/#method-printToPDF>
#' for possible options.
print_to_pdf <- function(url, filename = NULL, wait_ = FALSE, ...) {
  if (is.null(filename)) {
    filename <- url |>
      gsub("^.*://", "", x = _) |>
      gsub("/$", "", x = _) |>
      fs::path_sanitize(replacement = "_") |>
      paste0(".pdf")
  }
  
  b <- ChromoteSession$new()
  
  p <-
    {
      b$Page$navigate(url, wait_ = FALSE)
    } %...>%
    {
      b$Page$loadEventFired(wait_ = FALSE, timeout_ = 10)
    } %...>%
    {
      b$Page$printToPDF(..., wait_ = FALSE)
    } %...>%
    {
      .$data
    } %...>%
    {
      outfile <- file(filename, "wb")
      base64enc::base64decode(., output = outfile)
      close(outfile)
    } %...>%
    {
      message(filename)
    } %>%
    finally(~ b$close())
  
  if (wait_) {
    b$wait_for(p)
  } else {
    p
  }
  
  invisible(filename)
}


# Example usage:
# games_data <- get_box_scores("2023", "06", "07")

