Sys.setlocale("LC_ALL", "en_US.UTF-8")
library(here)
library(data.table)
library(magrittr)
library(httr)
library(jsonlite)
library(stringr)
library(chromote)

# =============================================================================
# MLB Stats API Configuration
# =============================================================================

MLB_API_BASE <- "https://statsapi.mlb.com/api/v1"
MLB_API_V1_1 <- "https://statsapi.mlb.com/api/v1.1"

# Sport and game type identifiers
SPORT_ID_MLB <- 1
GAME_TYPE_REGULAR <- "R"
GAME_TYPE_POSTSEASON <- "P"

# League identifiers
LEAGUE_ID_AL <- 103
LEAGUE_ID_NL <- 104

#' Build URL for schedule API
#' @param date Date string in YYYY-MM-DD format
#' @param sport_id Sport ID (default: MLB)
#' @param game_type Game type (default: Regular season)
#' @return URL string
build_schedule_url <- function(date, sport_id = SPORT_ID_MLB, game_type = GAME_TYPE_REGULAR) {
  paste0(MLB_API_BASE, "/schedule/games/?sportId=", sport_id,
         "&gameType=", game_type, "&date=", date)
}

#' Build URL for game boxscore
#' @param game_id MLB game ID
#' @return URL string
build_boxscore_url <- function(game_id) {
  paste0(MLB_API_BASE, "/game/", game_id, "/boxscore")
}

#' Build URL for game linescore
#' @param game_id MLB game ID
#' @return URL string
build_linescore_url <- function(game_id) {
  paste0(MLB_API_BASE, "/game/", game_id, "/linescore")
}

#' Build URL for game feed (live data including umpires, attendance)
#' @param game_id MLB game ID
#' @return URL string
build_game_feed_url <- function(game_id) {
  paste0(MLB_API_V1_1, "/game/", game_id, "/feed/live")
}

#' Build URL for player info
#' @param player_id MLB player ID
#' @return URL string
build_player_url <- function(player_id) {
  paste0(MLB_API_BASE, "/people/", player_id)
}

#' Build URL for team info
#' @param team_link Team link from API response (e.g., "/api/v1/teams/147")
#' @return URL string
build_team_url <- function(team_link) {
  paste0("https://statsapi.mlb.com", team_link)
}

#' Build URL for standings
#' @param league_id League ID (103=AL, 104=NL)
#' @param season Season year
#' @return URL string
build_standings_url <- function(league_id, season) {
  paste0(MLB_API_BASE, "/standings?leagueId=", league_id, "&season=", season)
}

#' Build URL for league leaders
#' @param stat_group "hitting" or "pitching"
#' @param stat_type Stat category (e.g., "battingAverage", "earnedRunAverage")
#' @param league_id League ID (103=AL, 104=NL)
#' @param season Season year
#' @param limit Number of leaders to fetch
#' @return URL string
build_leaders_url <- function(stat_group, stat_type, league_id, season, limit = 5) {
  paste0(MLB_API_BASE, "/stats/leaders?leaderCategories=", stat_type,
         "&statGroup=", stat_group, "&leagueId=", league_id,
         "&season=", season, "&limit=", limit)
}

# =============================================================================
# Helper Functions
# =============================================================================

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

#' Format game time from UTC to Eastern Time
#'
#' @param datetime_str ISO 8601 datetime string (e.g., "2025-04-15T23:05:00Z")
#' @return Formatted time string (e.g., "7:05 PM ET")
format_game_time <- function(datetime_str) {
  if (is.null(datetime_str) || is.na(datetime_str) || datetime_str == "") return("TBD")

  tryCatch({
    # Parse UTC time
    utc_time <- as.POSIXct(datetime_str, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

    # Convert to Eastern Time
    et_time <- format(utc_time, "%I:%M %p", tz = "America/New_York")

    # Remove leading zero from hour
    et_time <- sub("^0", "", et_time)

    paste0(et_time, " ET")
  }, error = function(e) "TBD")
}

#' Get pitcher season stats (W-L, ERA)
#'
#' @param pitcher_id Pitcher's MLB ID
#' @param season Season year
#' @return List with pitcher stats (name, wins, losses, era)
get_pitcher_statsMLB <- function(pitcher_id, season) {
  tryCatch({
    resp <- GET(
      paste0(build_player_url(pitcher_id),
             '?hydrate=currentTeam,stats(type=season,sportId=', SPORT_ID_MLB, ',season=', season, ')')
    ) %>%
      content(as = 'text') %>%
      fromJSON()

    # Get boxscore name format
    c_bn <- resp$people$boxscoreName
    c_bn2 <- formatBoxName(c_bn)

    # Extract season stats
    stats <- tryCatch({
      resp$people$stats[[1]]$splits[[1]]$stat
    }, error = function(e) NULL)

    if (is.null(stats)) {
      return(list(name = c_bn2, wins = 0, losses = 0, era = "-.--"))
    }

    list(
      name = c_bn2,
      wins = ifelse(is.null(stats$wins), 0, stats$wins),
      losses = ifelse(is.null(stats$losses), 0, stats$losses),
      era = ifelse(is.null(stats$era), "-.--", stats$era)
    )
  }, error = function(e) {
    list(name = "Unknown", wins = 0, losses = 0, era = "-.--")
  })
}

#' Get scheduled games for a date with probable pitcher information
#'
#' @param date Date string in format YYYY-MM-DD
#' @param include_pitchers Logical, whether to fetch pitcher stats
#' @return List with games data
get_scheduled_gamesMLB <- function(date, include_pitchers = TRUE) {
  tryCatch({
    season <- substr(date, 1, 4)

    # Fetch schedule with probable pitchers hydrated
    resp <- GET(
      paste0(build_schedule_url(date), '&hydrate=probablePitcher(note)')
    ) %>%
      content(as = 'text') %>%
      fromJSON()

    # Check if there are games
    if (is.null(resp$dates) || length(resp$dates) == 0 || length(resp$dates$games) == 0) {
      return(list())
    }

    games_df <- resp$dates$games[[1]]

    if (nrow(games_df) == 0) return(list())

    # Process each game
    lapply(1:nrow(games_df), function(i) {
      game <- games_df[i, ]

      # Get team info
      away_team_resp <- GET(build_team_url(game$teams$away$team$link)) %>%
        content(as = 'text') %>%
        fromJSON()

      home_team_resp <- GET(build_team_url(game$teams$home$team$link)) %>%
        content(as = 'text') %>%
        fromJSON()

      # Get probable pitcher info (if available and requested)
      away_pitcher <- NULL
      home_pitcher <- NULL

      if (include_pitchers) {
        if (!is.null(game$teams$away$probablePitcher$id) && !is.na(game$teams$away$probablePitcher$id)) {
          away_pitcher <- get_pitcher_statsMLB(game$teams$away$probablePitcher$id, season)
        }

        if (!is.null(game$teams$home$probablePitcher$id) && !is.na(game$teams$home$probablePitcher$id)) {
          home_pitcher <- get_pitcher_statsMLB(game$teams$home$probablePitcher$id, season)
        }
      }

      # Parse and format game time
      game_time <- format_game_time(game$gameDate)

      # Check if game is final
      is_final <- game$status$statusCode %in% c('F', 'FR', 'FO')

      list(
        gameId = game$gamePk,
        gameTime = game_time,
        gameTimeRaw = game$gameDate,
        statusCode = game$status$statusCode,
        isFinal = is_final,
        away = list(
          team = away_team_resp$teams$franchiseName,
          shortName = away_team_resp$teams$shortName,
          abbrev = away_team_resp$teams$abbreviation,
          score = ifelse(is_final, game$teams$away$score, NA),
          pitcher = away_pitcher
        ),
        home = list(
          team = home_team_resp$teams$franchiseName,
          shortName = home_team_resp$teams$shortName,
          abbrev = home_team_resp$teams$abbreviation,
          score = ifelse(is_final, game$teams$home$score, NA),
          pitcher = home_pitcher
        )
      )
    })
  }, error = function(e) {
    print(paste0("Error fetching scheduled games: ", e$message))
    list()
  })
}

#' Fetch boxscore name mapping for all players on a team
#'
#' @param players_list List of players from API response
#' @return data.table with personID, boxscoreName, and boxscoreName2 columns
fetch_player_name_mapping <- function(players_list) {
  lapply(players_list, function(x) {
    resp_p <- GET(build_player_url(x[['person']]$id)) %>%
      content(as = 'text') %>%
      fromJSON()
    data.table(
      personID = x[['person']]$id,
      boxscoreName = resp_p$people$boxscoreName,
      boxscoreName2 = formatBoxName(resp_p$people$boxscoreName)
    )
  }) %>% rbindlist()
}

#' Extract batting data for a team
#'
#' @param batters Vector of batter IDs
#' @param players_data List of player data from API response
#' @return data.table with batting statistics
extract_team_batting <- function(batters, players_data) {
  lapply(batters, function(x) {
    resp_p <- GET(build_player_url(x)) %>%
      content(as = 'text') %>%
      fromJSON()
    playerData <- players_data[[paste0('ID', x)]]
    data.table(
      Note = ifelse(is.null(playerData$stats$batting$note), '', playerData$stats$batting$note),
      Player = formatBoxName(resp_p$people$boxscoreName),
      Position = playerData$allPositions$abbreviation %>% tolower() %>% paste0(collapse = '-'),
      AB = playerData$stats$batting$atBats,
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
      HR = playerData$stats$batting$homeRuns,
      HR.s = playerData$seasonStats$batting$homeRuns,
      S = playerData$stats$batting$sacBunts,
      SF = playerData$stats$batting$sacFlies,
      GIDP = playerData$stats$batting$groundIntoDoublePlay,
      GITP = playerData$stats$batting$groundIntoTriplePlay
    )
  }) %>% rbindlist(fill = TRUE) %>% .[!is.na(AB)]
}

#' Extract fielding stats (E, SB, CS) for a team
#'
#' @param players_list List of players from API response
#' @return data.table with formatted E, SB, CS strings
extract_team_fielding_stats <- function(players_list) {
  lapply(players_list, function(x) {
    resp_p <- GET(build_player_url(x[['person']]$id)) %>%
      content(as = 'text') %>%
      fromJSON()
    data.table(
      Player = formatBoxName(resp_p$people$boxscoreName),
      E = x$stats$fielding$errors,
      E.s = x$seasonStats$fielding$errors,
      SB = x$stats$batting$stolenBases,
      SB.s = x$seasonStats$batting$stolenBases,
      CS = x$stats$batting$caughtStealing,
      CS.s = x$seasonStats$batting$caughtStealing
    )
  }) %>% rbindlist(fill = TRUE) %>%
    .[, .(
      E = ifelse(!is.na(E) & E > 0, paste0(Player, ifelse(E > 1, paste0(' ', E), ''), ' (', E.s, ')'), ''),
      SB = ifelse(!is.na(SB) & SB > 0, paste0(Player, ifelse(SB > 1, paste0(' ', SB), ''), ' (', SB.s, ')'), ''),
      CS = ifelse(!is.na(CS) & CS > 0, paste0(Player, ifelse(CS > 1, paste0(' ', CS), ''), ' (', CS.s, ')'), '')
    )]
}

#' Format batting extra-base/RBI notes from batting data
#'
#' @param batting_dt data.table from extract_team_batting
#' @return data.table with formatted 2B, 3B, HR, RBI, S, SF, GIDP, GITP strings
format_batting_extra_stats <- function(batting_dt) {
  batting_dt[, .(
    `2B` = ifelse(!is.na(`2B`) & `2B` > 0, paste0(Player, ifelse(`2B` > 1, paste0(' ', `2B`), ''), ' (', `2B.s`, ')'), ''),
    `3B` = ifelse(!is.na(`3B`) & `3B` > 0, paste0(Player, ifelse(`3B` > 1, paste0(' ', `3B`), ''), ' (', `3B.s`, ')'), ''),
    HR = ifelse(!is.na(HR) & HR > 0, paste0(Player, ifelse(HR > 1, paste0(' ', HR), ''), ' (', HR.s, ')'), ''),
    RBI = ifelse(!is.na(BI) & BI > 0, paste0(Player, ifelse(BI > 1, paste0(' ', BI), ''), ' (', BI.s, ')'), ''),
    S = ifelse(!is.na(S) & S > 0, paste0(Player, ifelse(S > 1, paste0(' ', S), '')), ''),
    SF = ifelse(!is.na(S) & SF > 0, paste0(Player, ifelse(SF > 1, paste0(' ', SF), '')), ''),
    GIDP = ifelse(!is.na(GIDP) & GIDP > 0, paste0(Player, ifelse(GIDP > 1, paste0(' ', GIDP), '')), ''),
    GITP = ifelse(!is.na(GITP) & GITP > 0, paste0(Player, ifelse(GITP > 1, paste0(' ', GITP), '')), '')
  )]
}

#' Extract pitching data for a team
#'
#' @param pitchers Vector of pitcher IDs
#' @param players_data List of player data from API response
#' @param franchise_name Team franchise name for column header
#' @return data.table with pitching statistics
extract_team_pitching <- function(pitchers, players_data, franchise_name) {
  dt <- lapply(pitchers, function(x) {
    resp_p <- GET(build_player_url(x)) %>%
      content(as = 'text') %>%
      fromJSON()
    playerData <- players_data[[paste0('ID', x)]]
    data.table(
      Player = paste0(formatBoxName(resp_p$people$boxscoreName),
                      ifelse(is.null(playerData$stats$pitching$note), '', paste0(' ', playerData$stats$pitching$note))),
      IP = playerData$stats$pitching$inningsPitched,
      H = playerData$stats$pitching$hits,
      R = playerData$stats$pitching$runs,
      ER = playerData$stats$pitching$earnedRuns,
      BB = playerData$stats$pitching$baseOnBalls,
      SO = playerData$stats$pitching$strikeOuts,
      NP = playerData$stats$pitching$numberOfPitches,
      ERA = playerData$seasonStats$pitching$era
    )
  }) %>% rbindlist(fill = TRUE)
  setnames(dt, 'Player', franchise_name)
  dt
}

#' Process a single game's box score data from MLB API
#'
#' @param gameId MLB game ID
#' @return List containing teams, batting, pitching, and gameInfo data
process_game_box_scoreMLB <- function(gameId) {
    print(gameId)
    resp_box <- GET(build_boxscore_url(gameId)) %>%
      content(as = 'text') %>%
      fromJSON()

    resp_line <- GET(build_linescore_url(gameId)) %>%
      content(as = 'text') %>%
      fromJSON()

    # Get game feed data for umpires, time, and attendance
    resp_game <- GET(build_game_feed_url(gameId)) %>%
      content(as = 'text') %>%
      fromJSON()

    resp_teamA <- GET(build_team_url(resp_box$teams$away$team$link)) %>%
      content(as = 'text') %>%
      fromJSON()

    c_franchiseNameA <- resp_teamA$teams$franchiseName

    resp_teamH <- GET(build_team_url(resp_box$teams$home$team$link)) %>%
      content(as = 'text') %>%
      fromJSON()
    
    c_franchiseNameH <- resp_teamH$teams$franchiseName
    
    dt_bnMappingA <- fetch_player_name_mapping(resp_box$teams$away$players)
    dt_abat <- extract_team_batting(resp_box$teams$away$batters, resp_box$teams$away$players)
    
    if (length(resp_box$teams$away$note) > 0) {
      dt_abatNote1 <- resp_box$teams$away$note %>%
        data.table() %>%
        .[, paste0(label, '-', value)] %>%
        paste0(collapse = ' ')
    } else {
      dt_abatNote1 <- ''
    }
    
    dt_abatNote2 <- extract_team_fielding_stats(resp_box$teams$away$players)
    dt_abatNote3 <- format_batting_extra_stats(dt_abat)
    
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
    
    dt_bnMappingH <- fetch_player_name_mapping(resp_box$teams$home$players)
    dt_hbat <- extract_team_batting(resp_box$teams$home$batters, resp_box$teams$home$players)
    
    if (length(resp_box$teams$home$note) > 0) {
      dt_hbatNote1 <- resp_box$teams$home$note %>%
        data.table() %>%
        .[, paste0(label, '-', value)] %>%
        paste0(collapse = ' ')
    } else {
      dt_hbatNote1 <- ''
    }
    
    dt_hbatNote2 <- extract_team_fielding_stats(resp_box$teams$home$players)
    dt_hbatNote3 <- format_batting_extra_stats(dt_hbat)
    
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
    
    dt_batNote <- format_combined_batting_notes(dt_abatNote, dt_hbatNote)
    
    
    
    dt_apitch <- extract_team_pitching(resp_box$teams$away$pitchers, resp_box$teams$away$players, c_franchiseNameA)
    dt_hpitch <- extract_team_pitching(resp_box$teams$home$pitchers, resp_box$teams$home$players, c_franchiseNameH)
    
    dt_pitchNote <- resp_box$info %>%
      data.table() %>%
      .[label %in% c('IBB','HBP','WP')]
    
    if (nrow(dt_pitchNote) > 0) {
      dt_pitchNote <- dt_pitchNote %>%
        .[, paste0('<b>', label, ':</b> ', value)] %>%
        paste0(collapse = ' ')
    } else {
      dt_pitchNote <- ''
    }
    
    for (i in 1:nrow(dt_bnMappingA)) {
      dt_pitchNote <- gsub(dt_bnMappingA[i, boxscoreName], dt_bnMappingA[i, boxscoreName2], dt_pitchNote, fixed = TRUE)
    }
    
    for (i in 1:nrow(dt_bnMappingH)) {
      dt_pitchNote <- gsub(dt_bnMappingH[i, boxscoreName], dt_bnMappingH[i, boxscoreName2], dt_pitchNote, fixed = TRUE)
    }

    # Extract game metadata (time, attendance, umpires)
    game_metadata <- extract_game_metadata(resp_game)
    c_gameTime <- game_metadata$gameTime
    n_attendance <- game_metadata$attendance
    c_umpires <- game_metadata$umpires

    list(
      teams = list(
        visitor = list(
          name = resp_box$teams$away$team$teamName,
          place = c_franchiseNameA,
          abbrev = resp_teamA$teams$abbreviation,
          score = resp_box$teams$away$teamStats$batting$runs,
          line = resp_line$innings$away$runs %>% as.character %>% ifelse(is.na(.), 'x', .),
          stats = dt_aStats
        ),
        home = list(
          name = resp_box$teams$home$team$teamName,
          place = c_franchiseNameH,
          abbrev = resp_teamH$teams$abbreviation,
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
      ),
      gameInfo = list(
        gameTime = c_gameTime,
        attendance = n_attendance,
        umpires = c_umpires
      )
    )
}

#' Process all games for a given date
#'
#' @param year Year (YYYY)
#' @param month Month (MM)
#' @param day Day (DD)
#' @return List of all games' data
process_all_gamesMLB <- function(year, month, day) {

  # Get all games for the specified date
  games <- GET(build_schedule_url(paste0(year, '-', month, '-', day))) %>%
    content(as = 'text') %>%
    fromJSON()

  all_games_data <- games$dates$games[[1]] %>%
    data.table() %>%
    .[status.statusCode %in% c('F','FR'), gamePk] %>%
    lapply(process_game_box_scoreMLB)
  
  # Remove any NULL entries (failed processing)
  all_games_data <- all_games_data[!sapply(all_games_data, is.null)]
  
  return(all_games_data)
}

#' Format leaders list into HTML with data-team attributes
#'
#' @param leaders_list List of leader objects with Player, Team, Abbrev, Value
#' @return HTML string with semicolon-separated leaders wrapped in spans
format_leaders_html <- function(leaders_list) {
  if (length(leaders_list) == 0) return("")

  leader_strings <- sapply(leaders_list, function(leader) {
    paste0("<span data-team='", leader$Abbrev, "'>",
           leader$Player, ", ", leader$Team, ", ", leader$Value,
           "</span>")
  })

  paste0(leader_strings, collapse = "; ")
}

#' Generate HTML for a division standings table
#'
#' @param division_data data.table with Team, Abbrev, W, L, PCT, GB, L10, Strk, H, A columns
#' @return HTML string for the standings table
generate_standings_table <- function(division_data) {
  # Table header
  header <- paste0(
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

  # Table rows
  rows <- ""
  for (i in 1:nrow(division_data)) {
    row <- division_data[i, ]
    rows <- paste0(
      rows,
      "                <tr data-team='", row$Abbrev, "'>\n",
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

  # Table footer
  footer <- paste0(
    "              </tbody>\n",
    "            </table>\n"
  )

  paste0(header, rows, footer)
}

#' Extract clinch indicator footnotes from team names
#'
#' @param team_names Vector of team names (may have clinch indicator prefixes)
#' @return Vector of footnote strings, or empty vector if none
extract_clinch_footnotes <- function(team_names) {
  footnotes <- c()
  if (any(grepl("^w-", team_names))) footnotes <- c(footnotes, "w-clinched wild card")
  if (any(grepl("^x-", team_names))) footnotes <- c(footnotes, "x-clinched playoff berth")
  if (any(grepl("^y-", team_names))) footnotes <- c(footnotes, "y-clinched division")
  if (any(grepl("^z-", team_names))) footnotes <- c(footnotes, "z-clinched best record")
  footnotes
}

#' Extract game metadata (time, attendance, umpires) from game feed
#'
#' @param resp_game Response from game feed API
#' @return List with gameTime, attendance, and umpires fields
extract_game_metadata <- function(resp_game) {
  # Extract game duration
  game_time <- tryCatch({
    mins <- resp_game$gameData$gameInfo$gameDurationMinutes
    paste0(floor(mins / 60), 'h ', (mins %% 60), 'm')
  }, error = function(e) "")

  # Extract attendance
  attendance <- tryCatch({
    resp_game$gameData$gameInfo$attendance
  }, error = function(e) NA)

  # Extract umpires
  umpires <- tryCatch({
    officials <- resp_game$liveData$boxscore$officials
    if (!is.null(officials) && nrow(officials) > 0) {
      ump_strings <- sapply(1:nrow(officials), function(i) {
        role <- officials[i, ]$officialType
        name <- officials[i, ]$official$fullName
        role_short <- switch(role,
          "Home Plate" = "H",
          "First Base" = "1",
          "Second Base" = "2",
          "Third Base" = "3",
          "Left Field" = "L",
          "Right Field" = "R",
          role
        )
        paste0(role_short, "-", name)
      })
      paste0(ump_strings, collapse = ", ")
    } else {
      ""
    }
  }, error = function(e) "")

  list(
    gameTime = game_time,
    attendance = attendance,
    umpires = umpires
  )
}

#' Combine and format batting notes from both teams
#'
#' @param away_notes data.table with away team batting notes
#' @param home_notes data.table with home team batting notes
#' @return Formatted HTML string with all batting notes
format_combined_batting_notes <- function(away_notes, home_notes) {
  # Define stat types and their display labels
  note_config <- list(
    list(col = "E", label = "E"),
    list(col = "LOB", label = "LOB"),
    list(col = "2B", label = "2B"),
    list(col = "3B", label = "3B"),
    list(col = "HR", label = "HR"),
    list(col = "RBI", label = "RBIs"),
    list(col = "SB", label = "SB"),
    list(col = "CS", label = "CS"),
    list(col = "S", label = "SH"),
    list(col = "SF", label = "SF"),
    list(col = "GIDP", label = "GIDP"),
    list(col = "GITP", label = "GITP"),
    list(col = "DP", label = "DP")
  )

  # Build notes for each stat type
  notes <- sapply(note_config, function(cfg) {
    away_val <- away_notes[[cfg$col]]
    home_val <- home_notes[[cfg$col]]

    if ((away_val != '' && !is.na(away_val)) || (home_val != '' && !is.na(home_val))) {
      combined <- c(away_val, home_val) %>% .[. != '' & !is.na(.)] %>% paste0(collapse = ', ')
      paste0('<b>', cfg$label, ':</b> ', combined)
    } else {
      ''
    }
  })

  # Combine non-empty notes
  result <- notes[notes != ''] %>% paste0(collapse = '. ')

  # Handle footnotes
  away_fn <- away_notes$footnotes
  home_fn <- home_notes$footnotes
  if ((away_fn != '' && !is.na(away_fn)) || (home_fn != '' && !is.na(home_fn))) {
    fn_combined <- c(away_fn, home_fn) %>% .[. != '' & !is.na(.)] %>% paste0(collapse = ', ')
    result <- paste0(fn_combined, '<br>', result)
  }

  paste0(result, '.')
}

#' Generate HTML rows for a batting table
#'
#' @param batting_dt data.table with batting stats
#' @return HTML string with table rows
generate_batting_rows <- function(batting_dt) {
  rows <- ""
  for (j in 1:nrow(batting_dt)) {
    row <- batting_dt[j, ]
    rows <- paste0(
      rows,
      "            <tr>\n",
      "              <td class='player-col'>", row[[1]], "</td>\n",
      "              <td class='stat-col'>", row$AB, "</td>\n",
      "              <td class='stat-col'>", row$R, "</td>\n",
      "              <td class='stat-col'>", row$H, "</td>\n",
      "              <td class='stat-col'>", row$BI, "</td>\n",
      "              <td class='stat-col'>", row$BB, "</td>\n",
      "              <td class='stat-col'>", row$SO, "</td>\n",
      "              <td class='avg-col'>", row$Avg, "</td>\n",
      "            </tr>\n"
    )
  }
  rows
}

#' Generate HTML rows for a pitching table
#'
#' @param pitching_dt data.table with pitching stats
#' @return HTML string with table rows
generate_pitching_rows <- function(pitching_dt) {
  rows <- ""
  for (j in 1:nrow(pitching_dt)) {
    row <- pitching_dt[j, ]
    rows <- paste0(
      rows,
      "            <tr>\n",
      "              <td class='player-col'>", row[[1]], "</td>\n",
      "              <td class='ip-col'>", row$IP, "</td>\n",
      "              <td class='stat-col'>", row$H, "</td>\n",
      "              <td class='stat-col'>", row$R, "</td>\n",
      "              <td class='stat-col'>", row$ER, "</td>\n",
      "              <td class='stat-col'>", row$BB, "</td>\n",
      "              <td class='stat-col'>", row$SO, "</td>\n",
      "              <td class='stat-col'>", row$NP, "</td>\n",
      "              <td class='era-col'>", row$ERA, "</td>\n",
      "            </tr>\n"
    )
  }
  rows
}

#' Fetch league leaders for a simple stat category
#'
#' @param category Stat category (e.g., "homeRuns", "runsBattedIn", "hits", "stolenBases", "strikeOuts", "saves")
#' @param stat_group Stat group ("hitting" or "pitching")
#' @param league_id League ID (103 for AL, 104 for NL)
#' @param season Season year
#' @param date End date for the range (YYYY-MM-DD format)
#' @param limit Number of leaders to fetch (default 8)
#' @param head_count Number of leaders to process (default 12)
#' @return List of leader data (Player, Team, Abbrev, Value)
fetch_simple_leaders <- function(category, stat_group, league_id, season, date, limit = 8, head_count = 12) {
  url <- paste0(
    'https://statsapi.mlb.com/api/v1/stats/leaders?leaderCategories=', category,
    '&statGroup=', stat_group,
    '&statType=byDateRange&limit=', limit,
    '&leagueId=', league_id,
    '&season=', season,
    '&startDate=', season, '-01-01&endDate=', date
  )

  resp <- GET(url) %>% content(as = 'text') %>% fromJSON()

  if (length(resp$leagueLeaders$leaders) > 0) {
    apply(resp$leagueLeaders$leaders[[1]] %>% head(head_count), 1, function(x) {
      resp_p <- GET(paste0('https://statsapi.mlb.com/', x['person.link'])) %>%
        content(as = 'text') %>%
        fromJSON()

      c_bn2 <- formatBoxName(resp_p$people$boxscoreName)

      resp_tm <- GET(paste0('https://statsapi.mlb.com/', x['team.link'])) %>%
        content(as = 'text') %>%
        fromJSON()

      list(Player = c_bn2, Team = resp_tm$teams$shortName, Abbrev = resp_tm$teams$abbreviation, Value = x['value'])
    })
  } else {
    list()
  }
}

#' Generate HTML for the games schedule section (yesterday/today/tomorrow)
#'
#' @param games_schedule_data List with yesterday, today, tomorrow game data
#' @return HTML string for the games schedule section
generate_schedule_html <- function(games_schedule_data) {
  if (is.null(games_schedule_data)) return("")

  html <- paste0(
    "    <div class='page'>\n",
    "      <div class='games-section'>\n",
    "        <div class='column-container'>\n",
    "          <div class='games-left'>\n",
    "            <div class='games-section-title'>TODAY'S GAMES</div>\n",
    "            <div class='games-left-inner'>\n"
  )

  # Today's games with pitchers
  if (length(games_schedule_data$today) > 0) {
    for (game in games_schedule_data$today) {
      away_pitcher_str <- if (!is.null(game$away$pitcher)) {
        paste0(game$away$pitcher$name, " (", game$away$pitcher$wins, "-",
               game$away$pitcher$losses, ", ", game$away$pitcher$era, ")")
      } else {
        "TBD"
      }

      home_pitcher_str <- if (!is.null(game$home$pitcher)) {
        paste0(game$home$pitcher$name, " (", game$home$pitcher$wins, "-",
               game$home$pitcher$losses, ", ", game$home$pitcher$era, ")")
      } else {
        "TBD"
      }

      html <- paste0(
        html,
        "              <div class='scheduled-game' data-team-away='", game$away$abbrev,
        "' data-team-home='", game$home$abbrev, "'>\n",
        "                <div class='game-time'>", game$gameTime, "</div>\n",
        "                <div class='matchup'>", game$away$shortName, " at ", game$home$shortName, "</div>\n",
        "                <div class='pitchers'>", away_pitcher_str, " vs. ", home_pitcher_str, "</div>\n",
        "              </div>\n"
      )
    }
  } else {
    html <- paste0(html, "              <div>No games scheduled</div>\n")
  }

  html <- paste0(
    html,
    "            </div>\n",
    "          </div>\n",
    "          <div class='games-right'>\n",
    "            <div>\n",
    "              <div class='games-section-title'>YESTERDAY'S SCORES</div>\n"
  )

  # Yesterday's scores
  if (length(games_schedule_data$yesterday) > 0) {
    for (game in games_schedule_data$yesterday) {
      winner <- if (game$away_score > game$home_score) "away" else "home"
      html <- paste0(
        html,
        "              <div class='game-score-line' data-team-away='", game$away_abbrev,
        "' data-team-home='", game$home_abbrev, "'>\n",
        "                <span class='", ifelse(winner == "away", "winner", ""), "'>",
        game$away_team, " ", game$away_score, "</span>, ",
        "<span class='", ifelse(winner == "home", "winner", ""), "'>",
        game$home_team, " ", game$home_score, "</span>\n",
        "              </div>\n"
      )
    }
  } else {
    html <- paste0(html, "              <div>No games</div>\n")
  }

  html <- paste0(
    html,
    "            </div>\n",
    "            <div>\n",
    "              <div class='games-section-title'>TOMORROW'S GAMES</div>\n"
  )

  # Tomorrow's games
  if (length(games_schedule_data$tomorrow) > 0) {
    for (game in games_schedule_data$tomorrow) {
      html <- paste0(
        html,
        "              <div class='tomorrow-game' data-team-away='", game$away$abbrev,
        "' data-team-home='", game$home$abbrev, "'>\n",
        "                ", game$away$shortName, " at ", game$home$shortName, "\n",
        "              </div>\n"
      )
    }
  } else {
    html <- paste0(html, "              <div>No games scheduled</div>\n")
  }

  html <- paste0(
    html,
    "            </div>\n",
    "          </div>\n",
    "        </div>\n",
    "      </div>\n"
  )

  html
}

#' Generate HTML for the box scores section
#'
#' @param games_data List of games' data
#' @return HTML string for the box scores section
generate_boxscores_html <- function(games_data) {
  html <- paste0(
    "    </div>\n",
    "    <div class='page'>\n",
    "      <div class='boxscores-title'>YESTERDAY'S BOX SCORES</div>\n",
    "      <div class='boxscores-container'>\n"
  )

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

    # Use team names instead of city names when both teams are from the same city
    visitor_display <- ifelse(game$teams$visitor$place == game$teams$home$place,
                              game$teams$visitor$name,
                              game$teams$visitor$place)
    home_display <- ifelse(game$teams$visitor$place == game$teams$home$place,
                           game$teams$home$name,
                           game$teams$home$place)

    html <- paste0(
      html,
      "        <div class='game-container' data-team-away='", game$teams$visitor$abbrev,
      "' data-team-home='", game$teams$home$abbrev, "'>\n",
      "          <div class='game-header'>", game_title, "</div>\n",
      "          <div class='team-line'>\n",
      "            <div class='team-name'>", visitor_display, "</div>\n",
      "            <div class='team-score'>",
      paste(gsub("(.{3})", "\\1&numsp;", paste0(game$teams$visitor$line, collapse='')), "&mdash; ",
            gsub('\\s','&numsp;',sprintf('%02s', as.integer(game$teams$visitor$stats[1]))), "&numsp;",
            gsub('\\s','&numsp;',sprintf('%02s', as.integer(game$teams$visitor$stats[2]))), "&numsp;",
            as.integer(game$teams$visitor$stats[3])),
      "            </div>\n",
      "          </div>\n",
      "          <div class='team-line'>\n",
      "            <div class='team-name'>", home_display, "</div>\n",
      "            <div class='team-score'>",
      paste(gsub("(.{3})", "\\1&numsp;", paste0(game$teams$home$line, collapse='')), "&mdash; ",
            gsub('\\s','&numsp;',sprintf('%02s', as.integer(game$teams$home$stats[1]))), "&numsp;",
            gsub('\\s','&numsp;',sprintf('%02s', as.integer(game$teams$home$stats[2]))), "&numsp;",
            as.integer(game$teams$home$stats[3])),
      "            </div>\n",
      "          </div>\n"
    )

    # Visitor batting
    html <- paste0(
      html,
      "          <table class='batting-table'>\n",
      "          <thead>\n",
      "            <tr>\n",
      "              <th class='player-col'>", game$teams$visitor$place, "</th>\n",
      "              <th class='stat-col'>AB</th>\n",
      "              <th class='stat-col'>R</th>\n",
      "              <th class='stat-col'>H</th>\n",
      "              <th class='stat-col'>BI</th>\n",
      "              <th class='stat-col'>BB</th>\n",
      "              <th class='stat-col'>SO</th>\n",
      "              <th class='avg-col'>Avg</th>\n",
      "            </tr>\n",
      "          </thead>\n",
      "          <tbody>\n"
    )

    html <- paste0(html, generate_batting_rows(game$batting$visitor))

    html <- paste0(
      html,
      "          </tbody>\n",
      "          </table>\n"
    )

    # Home batting
    html <- paste0(
      html,
      "          <table class='batting-table'>\n",
      "          <thead>\n",
      "            <tr>\n",
      "              <th class='player-col'>", game$teams$home$place, "</th>\n",
      "              <th class='stat-col'>AB</th>\n",
      "              <th class='stat-col'>R</th>\n",
      "              <th class='stat-col'>H</th>\n",
      "              <th class='stat-col'>BI</th>\n",
      "              <th class='stat-col'>BB</th>\n",
      "              <th class='stat-col'>SO</th>\n",
      "              <th class='avg-col'>Avg</th>\n",
      "            </tr>\n",
      "          </thead>\n",
      "          <tbody>\n"
    )

    html <- paste0(html, generate_batting_rows(game$batting$home))

    html <- paste0(
      html,
      "          </tbody>\n",
      "          </table>\n",
      "          <div class='notes'>", game$batting$notes, "</div>\n"
    )

    # Pitching tables - Visitor
    html <- paste0(
      html,
      "          <table class='pitching-table'>\n",
      "          <thead>\n",
      "            <tr>\n",
      "              <th class='player-col'>", game$teams$visitor$place, "</th>\n",
      "              <th class='ip-col'>IP</th>\n",
      "              <th class='stat-col'>H</th>\n",
      "              <th class='stat-col'>R</th>\n",
      "              <th class='stat-col'>ER</th>\n",
      "              <th class='stat-col'>BB</th>\n",
      "              <th class='stat-col'>SO</th>\n",
      "              <th class='stat-col'>NP</th>\n",
      "              <th class='era-col'>ERA</th>\n",
      "            </tr>\n",
      "          </thead>\n",
      "          <tbody>\n"
    )

    html <- paste0(html, generate_pitching_rows(game$pitching$visitor))

    html <- paste0(
      html,
      "          </tbody>\n",
      "          </table>\n"
    )

    # Pitching tables - Home
    html <- paste0(
      html,
      "          <table class='pitching-table'>\n",
      "          <thead>\n",
      "            <tr>\n",
      "              <th class='player-col'>", game$teams$home$place, "</th>\n",
      "              <th class='ip-col'>IP</th>\n",
      "              <th class='stat-col'>H</th>\n",
      "              <th class='stat-col'>R</th>\n",
      "              <th class='stat-col'>ER</th>\n",
      "              <th class='stat-col'>BB</th>\n",
      "              <th class='stat-col'>SO</th>\n",
      "              <th class='stat-col'>NP</th>\n",
      "              <th class='era-col'>ERA</th>\n",
      "            </tr>\n",
      "          </thead>\n",
      "          <tbody>\n"
    )

    html <- paste0(html, generate_pitching_rows(game$pitching$home))

    html <- paste0(
      html,
      "          </tbody>\n",
      "          </table>\n"
    )

    # Game info (time, attendance, umpires)
    c_gameInfoParts <- c()
    if (!is.null(game$gameInfo$umpires) && nchar(game$gameInfo$umpires) > 0) {
      c_gameInfoParts <- c(c_gameInfoParts, paste0('<b> Umpires: </b>', game$gameInfo$umpires,'.'))
    }
    if (!is.null(game$gameInfo$gameTime) && nchar(game$gameInfo$gameTime) > 0 && game$gameInfo$gameTime != " ") {
      c_gameInfoParts <- c(c_gameInfoParts, paste0("<b> T: </b>", game$gameInfo$gameTime, '.'))
    }
    if (!is.null(game$gameInfo$attendance) && !is.na(game$gameInfo$attendance)) {
      c_gameInfoParts <- c(c_gameInfoParts, paste0("<b> A: </b>", format(game$gameInfo$attendance, big.mark = ","), '.'))
    }

    gameInfoNotes <- ''
    if (length(c_gameInfoParts) > 0) {
      gameInfoNotes <- paste0(c_gameInfoParts, collapse = "")
    }

    # Game notes
    html <- paste0(
      html,
      "          <div class='notes'>", game$pitching$notes, gameInfoNotes, "</div>\n"
    )

    html <- paste0(
      html,
      "        </div>\n"
    )
  }

  html
}

#' Generate a newspaper-style HTML page with all box scores
#'
#' @param games_data List of games' data
#' @param date_str Date string for display
#' @param standings_data Optional standings data.table
#' @param leaders_data Optional league leaders data.table
#' @param games_schedule_data Optional games schedule data (yesterday's scores, today's games, tomorrow's games)
#' @return HTML content as a string
generate_newspaper_page2 <- function(games_data, date_str,
                                    standings_data = NULL,
                                    leaders_data = NULL,
                                    games_schedule_data = NULL) {
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
      
      // Create center links (pdf + dark mode toggle)
      const centerLinks = document.createElement("div");
      centerLinks.className = "center-links";
      const isDark = document.body.classList.contains("dark-mode");
      centerLinks.innerHTML = `<a href="', date_str, '.pdf" style="color: var(--text-primary); font-weight: bold;">pdf</a><span class="nav-divider"></span><a href="#" class="dark-toggle" style="color: var(--text-primary); font-weight: bold;">${isDark ? "light" : "dark"}</a>`;

      // Create next link placeholder
      const nextLink = document.createElement("div");
      nextLink.className = "next-link";
      nextLink.style.cssText = "flex: 1; text-align: right;";
      nextLink.innerHTML = ""; // Initial loading text

      // Add to container
      navContainer.appendChild(prevLink);
      navContainer.appendChild(centerLinks);
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
      
      // Attach dark mode toggle listeners to all cloned links
      document.querySelectorAll(".dark-toggle").forEach(link => {
        link.addEventListener("click", function(e) {
          e.preventDefault();
          document.body.classList.toggle("dark-mode");
          const isDark = document.body.classList.contains("dark-mode");
          localStorage.setItem("darkMode", isDark ? "on" : "off");
          document.querySelectorAll(".dark-toggle").forEach(l => l.textContent = isDark ? "light" : "dark");
        });
      });

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
            const linkHTML = `<a href="${dateStr}.html" style="text-decoration: none; color: var(--text-primary); font-weight: bold;">« ${formatDisplayDate(dateStr)}</a>`;
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
            const linkHTML = `<a href="${dateStr}.html" style="text-decoration: none; color: var(--text-primary); font-weight: bold;">${formatDisplayDate(dateStr)} »</a>`;
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

      // Team Highlighting Feature
      let highlightedTeam = null;

      function clearHighlights() {
        document.querySelectorAll(".team-highlight, .team-highlight-box").forEach(el => {
          el.classList.remove("team-highlight", "team-highlight-box");
        });
      }

      function toggleTeamHighlight(team) {
        // If clicking same team, unhighlight
        if (highlightedTeam === team) {
          clearHighlights();
          highlightedTeam = null;
          return;
        }

        // Clear existing highlights
        clearHighlights();

        // Set new highlighted team
        highlightedTeam = team;

        // Highlight standings rows with this team
        document.querySelectorAll(`tr[data-team="${team}"]`).forEach(el => {
          el.classList.add("team-highlight");
        });

        // Highlight leaders with this team
        document.querySelectorAll(`span[data-team="${team}"]`).forEach(el => {
          el.classList.add("team-highlight");
        });

        // Highlight box scores where team played (either home or away)
        document.querySelectorAll(`.game-container[data-team-away="${team}"], .game-container[data-team-home="${team}"]`).forEach(el => {
          el.classList.add("team-highlight-box");
        });

        // Highlight games section entries
        document.querySelectorAll(`[data-team-away="${team}"], [data-team-home="${team}"]`).forEach(el => {
          if (!el.classList.contains("game-container")) {
            el.classList.add("team-highlight");
          }
        });
      }

      // Add click handlers to standings team cells
      document.querySelectorAll(".standings-table tr[data-team]").forEach(row => {
        const teamCell = row.querySelector(".team-col");
        if (teamCell) {
          teamCell.addEventListener("click", function(e) {
            e.preventDefault();
            const team = row.getAttribute("data-team");
            toggleTeamHighlight(team);
          });
        }
      });
    });
    ')

  # Start HTML content
  html_content <- paste0(
    "<!DOCTYPE html>\n",
    "<html>\n",
    "<head>\n",
    "  <meta charset=\"UTF-8\">\n",  # Add charset meta tag
    "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
    "  <title>Baseball Box Scores - ", display_date, "</title>\n",
    "  <style>\n",
    "    @import url('https://fonts.googleapis.com/css2?family=Source+Sans+3:ital,wght@0,200..900;1,200..900&display=swap');\n",
    "    :root {\n",
    "      --bg-page: #f9f7f1;\n",
    "      --bg-paper: #fff;\n",
    "      --text-primary: #000;\n",
    "      --text-secondary: #333;\n",
    "      --text-muted: #666;\n",
    "      --border-strong: #000;\n",
    "      --border-light: #ddd;\n",
    "      --shadow: rgba(0,0,0,0.1);\n",
    "      --hover-bg: #f0f0f0;\n",
    "      --highlight-bg: #fff3cd;\n",
    "      --highlight-box-bg: #fff9e6;\n",
    "      --highlight-border: #ffc107;\n",
    "    }\n",
    "    body.dark-mode {\n",
    "      --bg-page: #121212;\n",
    "      --bg-paper: #1e1e1e;\n",
    "      --text-primary: #e0e0e0;\n",
    "      --text-secondary: #ccc;\n",
    "      --text-muted: #999;\n",
    "      --border-strong: #555;\n",
    "      --border-light: #333;\n",
    "      --shadow: rgba(0,0,0,0.4);\n",
    "      --hover-bg: #2a2a2a;\n",
    "      --highlight-bg: #3a3520;\n",
    "      --highlight-box-bg: #2e2b1a;\n",
    "      --highlight-border: #ffc107;\n",
    "    }\n",
    "    body { font-family: 'Source Sans 3', 'Segoe UI'; margin: 0; padding: 0; background-color: var(--bg-page); color: var(--text-primary); }\n",
    "    a { color: var(--text-primary); }\n",
    "    .newspaper { max-width: 1200px; margin: 0 auto; padding: 20px; background-color: var(--bg-paper); box-shadow: 0 0 10px var(--shadow); }\n",
    "    .header { text-align: center; border-bottom: 2px solid var(--border-strong); padding-bottom: 10px; margin-bottom: 20px; }\n",
    "    .date { font-style: italic; margin-bottom: 10px; }\n",
    "    .main-title { font-size: 42px; font-weight: bold; margin: 0; }\n",
    "    .subtitle { font-size: 24px; margin: 5px 0 15px 0; }\n",
    "    .page { break-after: page; }\n",
    "    .nav-container { display: flex; justify-content: space-between; align-items: center; padding: 10px 0; margin: 10px 0; border-top: 1px solid var(--border-light); border-bottom: 1px solid var(--border-light); }\n",
    "    .leaders div { font-size: 14px; margin: 4px 0px; }\n",
    "    .leaders table { font-size: 14px; margin: 5px 0px; }\n",
    "    .boxscores-title { text-align: center; font-size: 24px; font-weight: bold; margin: 10px 0; border-bottom: 2px solid var(--border-strong); }\n",
    "    .boxscores-container { column-count: 4; column-gap: 20px; margin-top: 20px; }\n",
    "    .game-container { break-inside: avoid; page-break-inside: avoid; margin-bottom: 20px; }\n",
    "    .game-header { font-weight: bold; font-size: 20px; border-bottom: 1px solid var(--border-strong); }\n",
    "    .team-line { display: flex; justify-content: space-between; font-size: 14px; font-weight: 700; line-height: 1.2; margin-top: 2px; }\n",
    "    /* Improved table styling */\n",
    "    table { width: 100%; border-collapse: collapse; font-size: 14px; table-layout: fixed; }\n",
    "    .batting tr:last-child { font-weight:700 }\n",
    "    th, td { text-align: left; overflow: hidden; line-height: 1.1; }\n",
    "    th { border-top: 1px solid var(--border-strong); font-weight: bold; }\n",
    "    /* Specific column widths for standings tables */\n",
    "    .standings-table th { border: none; }\n",
    "    .standings-table .team-col { width: 20%; text-align: left; }\n",
    "    .standings-table .num-col { width: 5%; text-align: right; }\n",
    "    .standings-table .pct-col { width: 8%; text-align: right; }\n",
    "    /* General column alignment */\n",
    "    th:not(.team-col), td:not(.team-col) { text-align: right; }\n",
    "    td { white-space: nowrap; }\n",
    "    .notes { font-size: 14px; line-height: 1.2; padding: 4px 0; border-top: 1px solid var(--border-strong); }\n",
    "    .section { margin-bottom: 20px; }\n",
    "    .column-container { display: flex; gap: 15px; }\n",
    "    .column { flex: 1; }\n",
    "    .column-40 { width: 40%; }\n",
    "    .column-60 { width: 60%; }\n",
    "    .stats-header { font-size: 16px; font-weight: bold; margin: 8px 0; border-bottom: 1px solid var(--border-strong); }\n",
    "    .stats-subheader { font-size: 14px; font-weight: bold; margin: 4px 0; border-bottom: 1px solid var(--border-strong); }\n",
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
    "    /* Games Schedule Section */\n",
    "    .games-section { margin-bottom: 20px; padding: 10px 0; border-top: 2px solid var(--border-strong); }\n",
    "    .games-section .column-container { display: flex; gap: 20px; }\n",
    "    .games-left { width: 65%; }\n",
    "    .games-left-inner { display: grid; grid-template-columns: 1fr 1fr; gap: 4px 20px; }\n",
    "    .games-right { width: 35%; display: flex; flex-direction: column; gap: 15px; }\n",
    "    .games-section-title { font-size: 16px; font-weight: bold; margin-bottom: 8px; border-bottom: 1px solid var(--border-strong); padding-bottom: 4px; }\n",
    "    .game-score-line { font-size: 13px; line-height: 1.5; margin: 2px 0; }\n",
    "    .game-score-line .winner { font-weight: bold; }\n",
    "    .scheduled-game { padding-bottom: 4px; border-bottom: 1px dotted var(--border-strong); }\n",
    "    .scheduled-game:last-child { border-bottom: none; }\n",
    "    .center-links { display: flex; align-items: center; gap: 0; }\n",
    "    .nav-divider { display: inline-block; width: 1px; height: 14px; background-color: var(--border-strong); margin: 0 10px; }\n",
    "    .dark-toggle { cursor: pointer; }\n",
    "    .game-time { font-size: 12px; color: var(--text-muted); }\n",
    "    .matchup { font-size: 14px; font-weight: bold; }\n",
    "    .pitchers { font-size: 12px; color: var(--text-secondary); }\n",
    "    .tomorrow-game { font-size: 13px; line-height: 1.6; }\n",
    "    /* Team Highlighting */\n",
    "    .standings-table .team-col { cursor: pointer; transition: background-color 0.2s; }\n",
    "    .standings-table .team-col:hover { background-color: var(--hover-bg); }\n",
    "    .team-highlight { background-color: var(--highlight-bg) !important; }\n",
    "    .team-highlight-box { background-color: var(--highlight-box-bg); outline: 2px solid var(--highlight-border); }\n",
    "    .leaders-note span.team-highlight { padding: 0 2px; }\n",
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
    "    .games-section .column-container { flex-direction: column; }\n",
    "    .games-left, .games-right { width: 100%; }\n",
    "    .games-left-inner { grid-template-columns: 1fr; }\n",
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
    "    .games-section .column-container { flex-direction: column; }\n",
    "    .games-left, .games-right { width: 100%; }\n",
    "    .games-left-inner { grid-template-columns: 1fr; }\n",
    "  }\n",
    "  @media print {\n",
    "      body { \n",
    "        background-color: #fff;\n",
    "        color: #000;\n",
    "          width:1650px;\n",
    "        --bg-page: #fff; --bg-paper: #fff; --text-primary: #000; --border-strong: #000; --border-light: #ddd;\n",
    "      }\n",
    "      .newspaper { box-shadow: none; max-width: none; padding: 0; margin: 0; }\n",
    "      .nav-container { display: none; }\n",
    "      .dark-toggle { display: none; }\n",
    "      .column-container { display: flex; }\n",
    "      .boxscores-container { column-count: 5; }\n",
    "    }\n",
    "  </style>\n",
    "  <script>\n", navigation_js, "\n    </script>\n",
    "</head>\n",
    "<body>\n",
    "<script>if(localStorage.getItem('darkMode')==='on')document.body.classList.add('dark-mode');</script>\n",
    "  <div class='newspaper'>\n",
    "    <div class='header'>\n",
    "      <div class='date'>", display_date, "</div>\n",
    "      <h1 class='main-title'>WALDRN.COM/BOXSCORES</h1>\n",
    "      <div class='subtitle'>Daily MLB Standings & Box Scores</div>\n",
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
    
    # Generate AL standings tables using helper
    html_content <- paste0(
      html_content,
      generate_standings_table(standings_data$al[[1]]),
      "            <div class='stats-subheader'>Central Division</div>\n",
      generate_standings_table(standings_data$al[[2]]),
      "            <div class='stats-subheader'>West Division</div>\n",
      generate_standings_table(standings_data$al[[3]])
    )

    # Build clinch indicator footnotes
    al_teams <- c(standings_data$al[[1]]$Team, standings_data$al[[2]]$Team, standings_data$al[[3]]$Team)
    al_clinch_footnotes <- extract_clinch_footnotes(al_teams)

    html_content <- paste0(
      html_content,
      ifelse(length(al_clinch_footnotes) > 0,
             paste0("            <div class='leaders-note' style='margin-top: 8px;'>", paste(al_clinch_footnotes, collapse = "; "), "</div>\n"),
             ""),
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
        "                    <tr data-team='", row$Abbrev, "'>\n",
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
      "                  <b>Home Runs: </b>", format_leaders_html(leaders_data$al_leaders$batting$hr_leaders), '\n',
      "                </div>\n",
      "                <div class='leaders-note'>",
      "                  <b>Runs Batted In: </b>", format_leaders_html(leaders_data$al_leaders$batting$rbi_leaders), '\n',
      "                </div>\n",
      "              </div>\n",
      "              <div class='column-60 leaders'>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Hits: </b>", format_leaders_html(leaders_data$al_leaders$batting$hits_leaders), '\n',
      "                </div>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Stolen Bases: </b>", format_leaders_html(leaders_data$al_leaders$batting$sb_leaders), '\n',
      "                </div>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Pitching: </b>", format_leaders_html(leaders_data$al_leaders$pitching$wins_leaders), '\n',
      "                </div>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Strikeouts: </b>", format_leaders_html(leaders_data$al_leaders$pitching$so_leaders), '\n',
      "                </div>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Saves: </b>", format_leaders_html(leaders_data$al_leaders$pitching$sv_leaders), '\n',
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
    
    # Generate NL standings tables using helper
    html_content <- paste0(
      html_content,
      generate_standings_table(standings_data$nl[[1]]),
      "            <div class='stats-subheader'>Central Division</div>\n",
      generate_standings_table(standings_data$nl[[2]]),
      "            <div class='stats-subheader'>West Division</div>\n",
      generate_standings_table(standings_data$nl[[3]])
    )

    # Build clinch indicator footnotes
    nl_teams <- c(standings_data$nl[[1]]$Team, standings_data$nl[[2]]$Team, standings_data$nl[[3]]$Team)
    nl_clinch_footnotes <- extract_clinch_footnotes(nl_teams)

    html_content <- paste0(
      html_content,
      ifelse(length(nl_clinch_footnotes) > 0,
             paste0("            <div class='leaders-note' style='margin-top: 8px;'>", paste(nl_clinch_footnotes, collapse = "; "), "</div>\n"),
             ""),
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
        "                    <tr data-team='", row$Abbrev, "'>\n",
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
      "                  <b>Home Runs: </b>", format_leaders_html(leaders_data$nl_leaders$batting$hr_leaders), '\n',
      "                </div>\n",
      "                <div class='leaders-note'>",
      "                  <b>Runs Batted In: </b>", format_leaders_html(leaders_data$nl_leaders$batting$rbi_leaders), '\n',
      "                </div>\n",
      "              </div>\n",
      "              <div class='column-60 leaders'>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Hits: </b>", format_leaders_html(leaders_data$nl_leaders$batting$hits_leaders), '\n',
      "                </div>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Stolen Bases: </b>", format_leaders_html(leaders_data$nl_leaders$batting$sb_leaders), '\n',
      "                </div>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Pitching: </b>", format_leaders_html(leaders_data$nl_leaders$pitching$wins_leaders), '\n',
      "                </div>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Strikeouts: </b>", format_leaders_html(leaders_data$nl_leaders$pitching$so_leaders), '\n',
      "                </div>\n",
      "                <div class='leaders-note'>\n",
      "                  <b>Saves: </b>", format_leaders_html(leaders_data$nl_leaders$pitching$sv_leaders), '\n',
      "                </div>\n",
      "              </div>\n",
      "            </div>\n",
      "          </div>\n",
      "        </div>\n",
      "      </div>\n",
      "    </div>\n"
    )
  }

  # Add games schedule section
  html_content <- paste0(html_content, generate_schedule_html(games_schedule_data))

  # Add box scores section
  html_content <- paste0(html_content, generate_boxscores_html(games_data))

  # Close the boxscores-container and newspaper divs
  html_content <- paste0(
    html_content,
    "      </div>\n",
    "    </div>\n",
    "  </div>\n",
    "</body>\n",
    "</html>"
  )
  
  return(html_content)
}

#' Main function to process box scores for a given date
#'
#' The passed date is treated as "today" (the newspaper date).
#' Box scores are fetched from "yesterday" (completed games).
#' Standings and leaders are as of "yesterday".
#' Today's games and tomorrow's games are included in the games schedule section.
#'
#' @param year Year (YYYY) - the newspaper date
#' @param month Month (MM) - the newspaper date
#' @param day Day (DD) - the newspaper date
#' @param season Season year
#' @param output_dir Directory to save output files
#' @param save_newspaper Logical, whether to save newspaper-style HTML
#' @param save_pdf Logical, whether to save pdf of newspaper-style HTML
#' @param include_standings Logical, whether to include standings in newspaper
#' @param include_leaders Logical, whether to include league leaders in newspaper
#' @param include_games_schedule Logical, whether to include games schedule section
#' @return List of all games' data
get_box_scores <- function(year, month, day,
                           season = NULL,
                           output_dir = "game_data",
                           save_newspaper = TRUE,
                           save_pdf = TRUE,
                           include_standings = TRUE,
                           include_leaders = TRUE,
                           include_games_schedule = TRUE) {

  # "today" is the newspaper date (the passed date)
  today_date <- as.Date(paste0(year, "-", sprintf("%02d", as.integer(month)), "-", sprintf("%02d", as.integer(day))))

  # "yesterday" is when box score games were completed

  yesterday_date <- today_date - 1

  # "tomorrow" for upcoming games preview
  tomorrow_date <- today_date + 1

  # Format dates for API calls
  yesterday_formatted <- format(yesterday_date, "%Y-%m-%d")
  today_formatted <- format(today_date, "%Y-%m-%d")
  tomorrow_formatted <- format(tomorrow_date, "%Y-%m-%d")

  # Date string for file naming uses "today"
  date_str <- format(today_date, "%Y%m%d")

  if (is.null(season)) {
    season <- format(today_date, "%Y")
  }

  # Create output directory
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # Process completed games from YESTERDAY
  print(paste0("Processing games for ", yesterday_formatted, " (yesterday)..."))
  games_data <- process_all_gamesMLB(
    format(yesterday_date, "%Y"),
    format(yesterday_date, "%m"),
    format(yesterday_date, "%d")
  )
  print(paste0("Found ", length(games_data), " completed games"))

  # Generate and save newspaper-style page if requested
  if (save_newspaper) {
    print("Generating newspaper page...")

    # Get standings data as of YESTERDAY
    standings_data <- NULL
    if (include_standings) {
      print("Getting standings data...")
      standings_data <- get_standingsMLB(yesterday_formatted)
    }

    # Get league leaders data as of YESTERDAY
    leaders_data <- NULL
    if (include_leaders) {
      print("Getting league leaders data...")
      leaders_data <- get_league_leadersMLB(yesterday_formatted)
    }

    # Get games schedule data
    games_schedule_data <- NULL
    if (include_games_schedule) {
      print("Getting games schedule data...")

      # Yesterday's scores (extract from games_data for brief display)
      yesterday_scores <- lapply(games_data, function(game) {
        list(
          away_team = game$teams$visitor$place,
          away_abbrev = game$teams$visitor$abbrev,
          away_score = game$teams$visitor$score,
          home_team = game$teams$home$place,
          home_abbrev = game$teams$home$abbrev,
          home_score = game$teams$home$score
        )
      })

      # Today's games with pitchers
      print(paste0("Fetching today's games (", today_formatted, ")..."))
      today_games <- get_scheduled_gamesMLB(today_formatted, include_pitchers = TRUE)

      # Tomorrow's games (just matchups, no pitcher stats needed)
      print(paste0("Fetching tomorrow's games (", tomorrow_formatted, ")..."))
      tomorrow_games <- get_scheduled_gamesMLB(tomorrow_formatted, include_pitchers = FALSE)

      games_schedule_data <- list(
        yesterday = yesterday_scores,
        today = today_games,
        tomorrow = tomorrow_games
      )
    }

    # Generate the newspaper page
    print("Creating newspaper HTML...")
    newspaper_html <- generate_newspaper_page2(
      games_data,
      date_str,
      standings_data,
      leaders_data,
      games_schedule_data
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
      paste0(build_standings_url(lg, season), '&date=', date)
    ) %>%
      content(as = 'text') %>%
      fromJSON()

    resp_div <- resp_lg$records$division$link %>%
      lapply(function(x) {
        resp <- GET(paste0("https://statsapi.mlb.com", x)) %>%
          content(as = 'text') %>%
          fromJSON()

        resp$divisions$nameShort %>%
          gsub('AL ', '', ., fixed = TRUE) %>%
          gsub('NL ', '', ., fixed = TRUE)
      }) %>%
      unlist()

    lapply(1:length(resp_div), function(i) {
      # Get team names and abbreviations
      team_info <- resp_lg$records$teamRecords[[i]]$team$link %>%
        lapply(function(x) {
          resp_team <- GET(build_team_url(x)) %>%
            content(as = 'text') %>%
            fromJSON()

          list(
            shortName = resp_team$teams$shortName,
            abbrev = resp_team$teams$abbreviation
          )
        })

      team_names <- sapply(team_info, function(x) x$shortName)
      team_abbrevs <- sapply(team_info, function(x) x$abbrev)

      # Get clinch indicators (may be NULL or NA for some teams)
      clinch_indicators <- tryCatch({
        resp_lg$records$teamRecords[[i]]$clinchIndicator
      }, error = function(e) rep("", length(team_names)))

      # If clinch_indicators is NULL, create empty vector
      if (is.null(clinch_indicators)) {
        clinch_indicators <- rep("", length(team_names))
      }

      # Replace NA with empty string
      clinch_indicators[is.na(clinch_indicators)] <- ""

      # Prepend clinch indicator to team name if it exists
      team_display <- ifelse(clinch_indicators != "",
                             paste0(clinch_indicators, "-", team_names),
                             team_names)

      data.table(
        div = resp_div[i],
        Team = team_display,
        Abbrev = team_abbrevs,
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
    al = getStandingsLg(LEAGUE_ID_AL),
    nl = getStandingsLg(LEAGUE_ID_NL)
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
    lg <- ifelse(lg == 'AL', LEAGUE_ID_AL, LEAGUE_ID_NL)
    
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
                               Abbrev = resp_tm$teams$abbreviation,
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
                                Abbrev = '',
                                AB = '',
                                R = '',
                                H = '',
                                Value = '')
    }
    
    # Home Runs
    hr_leaders <- fetch_simple_leaders("homeRuns", "hitting", lg, yr, date)
    
    # RBI
    rbi_leaders <- fetch_simple_leaders("runsBattedIn", "hitting", lg, yr, date)
    
    # Hits
    hits_leaders <- fetch_simple_leaders("hits", "hitting", lg, yr, date)
    
    # Stolen Bases
    sb_leaders <- fetch_simple_leaders("stolenBases", "hitting", lg, yr, date)
    
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

        data.table(Player = c_bn2, Team = resp_tm$teams$shortName, Abbrev = resp_tm$teams$abbreviation) %>%
          cbind(dt_stats)

      }) %>%
        rbindlist() %>%
        .[order(-W, -pct)] %>%
        .[, seq := 1:.N] %>%
        .[, rank := cumsum(W != shift(W, fill = 0))] %>%
        .[, rank2 := cumsum(pct != shift(pct, fill = 0)), rank]

      dt_lrank <- w_leaders[pmin(8, min(w_leaders$rank)), .(rank, rank2)]

      w_leaders <- w_leaders %>%
        .[W > 0 &
            seq <= 10 &
            !((rank > dt_lrank$rank) |
                (rank = dt_lrank$rank & rank2 > dt_lrank$rank2))] %>%
        .[, .(Player, Team, Abbrev, Value = paste0(W, '-', L, ', ', sub('0\\.','.',sprintf('%#.3f', pct))))] %>%
        apply(1, as.list)
    } else {
      w_leaders <- list()
    }
    
    # Strikeouts
    so_leaders <- fetch_simple_leaders("strikeOuts", "pitching", lg, yr, date)
    
    # Saves
    sv_leaders <- fetch_simple_leaders("saves", "pitching", lg, yr, date)
    
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
# get_box_scores("2025", "09", "21")
# get_box_scores("2026", "03", "26")

