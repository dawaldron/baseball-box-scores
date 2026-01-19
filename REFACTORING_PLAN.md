# Refactoring Plan: get_boxscores.r

This document outlines refactoring opportunities for the codebase, ordered by impact and complexity.

## Completed Refactors

### Team Processing Duplication (Completed 2026-01-19)
- Extracted 5 helper functions for away/home team processing
- `fetch_player_name_mapping()`, `extract_team_batting()`, `extract_team_fielding_stats()`, `format_batting_extra_stats()`, `extract_team_pitching()`
- Reduced ~190 duplicated lines

### API URL Constants and Builders (Completed 2026-01-19)
- Added constants: `MLB_API_BASE`, `MLB_API_V1_1`, `SPORT_ID_MLB`, `GAME_TYPE_REGULAR`, `LEAGUE_ID_AL`, `LEAGUE_ID_NL`
- Added URL builder functions: `build_schedule_url()`, `build_boxscore_url()`, `build_linescore_url()`, `build_game_feed_url()`, `build_player_url()`, `build_team_url()`, `build_standings_url()`, `build_leaders_url()`
- Updated helper functions and main processing functions to use builders

### Standings Table HTML Generation (Completed 2026-01-19)
- Added `generate_standings_table()` helper for division standings HTML
- Added `extract_clinch_footnotes()` helper for playoff clinch indicators
- Replaced 6 duplicated table blocks (AL/NL East/Central/West) with helper calls
- Reduced ~260 lines to ~30 lines

### Batting Notes Formatting (Completed 2026-01-19)
- Added `format_combined_batting_notes()` helper function
- Replaced 21-line ifelse chain with single helper call
- More maintainable: stat types defined in config list

### Game Metadata Extraction (Completed 2026-01-19)
- Added `extract_game_metadata()` helper function
- Consolidated 3 tryCatch blocks (game time, attendance, umpires) into one
- Reduced ~35 lines to ~5 lines

### Box Score HTML Row Generation (Completed 2026-01-19)
- Added `generate_batting_rows()` helper function for batting table rows
- Added `generate_pitching_rows()` helper function for pitching table rows
- Replaced 4 duplicated for loops with single-line helper calls
- Reduced ~68 lines to ~4 lines

### League Leaders API Calls (Completed 2026-01-19)
- Added `fetch_simple_leaders()` helper function for fetching leader stats
- Replaced 6 duplicated API call blocks (HR, RBI, hits, SB, SO, saves) with single-line helper calls
- Reduced ~180 lines to ~6 lines
- Note: Wins leaders kept inline due to special W-L record calculation logic

### Move process_game_box_scoreMLB to Top Level (Completed 2026-01-19)
- Moved `process_game_box_scoreMLB()` from nested function to top-level
- Function had no parent scope dependencies, making extraction straightforward
- Improves testability and code organization

---

## Phase 1: High Impact Refactors

### 1.1 Standings Table HTML Generation

**Current State:** 6 nearly identical table blocks for AL/NL divisions (East/Central/West)

**Lines:** 1040-1076, 1085-1121, 1130-1166 (AL), 1250-1286, 1315-1351, 1361-1397 (NL)

**Pattern:**
```r
# Repeated 6 times with only data source changing:
<table class="standings-table">
  <tr><th colspan="6">DIVISION NAME</th></tr>
  <tr><th>Team</th><th>W</th><th>L</th><th>Pct</th><th>GB</th><th>L10</th></tr>
  # Loop through teams...
</table>
```

**Proposed Helper:**
```r
#' Generate HTML for a division standings table
#'
#' @param division_data data.table with team standings
#' @param division_name Display name for the division header
#' @return HTML string for the standings table
generate_standings_table <- function(division_data, division_name) {
  # Build table header
  # Loop through teams to build rows
  # Handle clinch indicators (w-, x-, y-, z- prefixes)
  # Return complete HTML string
}
```

**Usage After Refactor:**
```r
# Replace 6 blocks with:
al_east_html <- generate_standings_table(standings$AL_East, "AL East")
al_central_html <- generate_standings_table(standings$AL_Central, "AL Central")
# ... etc
```

**Impact:** ~350 lines reduced to ~80 lines

---

### 1.2 Break Down `generate_newspaper_page2()` Function

**Current State:** 1,228-line monolithic function (lines 645-1872)

**Proposed Split:**

| New Function | Responsibility | Approx Lines |
|--------------|----------------|--------------|
| `generate_standings_html()` | All standings tables for AL/NL | ~200 |
| `generate_leaders_html()` | Batting/pitching leaders tables | ~200 |
| `generate_boxscores_html()` | Individual game box scores | ~400 |
| `generate_schedule_html()` | Scheduled games section | ~150 |
| `generate_page_wrapper()` | HTML head, CSS, navigation, footer | ~200 |

**New Structure:**
```r
generate_newspaper_page2 <- function(games_data, standings_data, leaders_data, ...) {
  # Orchestrator function
  standings_html <- generate_standings_html(standings_data)
  leaders_html <- generate_leaders_html(leaders_data)
  boxscores_html <- generate_boxscores_html(games_data)
  schedule_html <- generate_schedule_html(scheduled_games)

  generate_page_wrapper(
    standings = standings_html,
    leaders = leaders_html,
    boxscores = boxscores_html,
    schedule = schedule_html,
    ...
  )
}
```

**Impact:** Major improvement in maintainability and testability

---

### 1.3 League Leaders API Calls

**Current State:** Repeated API fetch + data extraction pattern

**Lines:**
- Batting leaders: 2169-2199, 2219-2238, 2250-2269
- Pitching leaders: 2361-2401, 2416-2435, 2447-2466

**Pattern:**
```r
# Repeated for each stat category:
resp <- GET(paste0(base_url, stat_params)) %>% content() %>% fromJSON()
lapply(resp$...$leaders, function(x) {
  # Extract player info, team info, stat value
  # Build result list
})
```

**Proposed Helper:**
```r
#' Fetch and format leader data for a stat category
#'
#' @param stat_group "hitting" or "pitching"
#' @param stat_type Stat category (e.g., "battingAverage", "earnedRunAverage")
#' @param league_id 103 (AL) or 104 (NL)
#' @param season Season year
#' @param limit Number of leaders to fetch
#' @return data.table with leader information
fetch_league_leaders <- function(stat_group, stat_type, league_id, season, limit = 5) {
  url <- build_leaders_api_url(stat_group, stat_type, league_id, season, limit)
  resp <- GET(url) %>% content(as = 'text') %>% fromJSON()
  # Extract and format leader data
  # Return standardized data.table
}
```

**Impact:** ~200 lines consolidated, easier to add new stat categories

---

## Phase 2: Medium Impact Refactors

### 2.1 API URL Constants and Builders

**Current State:** Magic strings scattered throughout

**Locations:**
- Line 93: `statsapi.mlb.com/api/v1/schedule`
- Line 305: `statsapi.mlb.com/api/v1/schedule/games`
- Line 312: `statsapi.mlb.com/api/v1/game/{id}/boxscore`
- Line 316: `statsapi.mlb.com/api/v1/game/{id}/linescore`
- Line 321: `statsapi.mlb.com/api/v1.1/game/{id}/feed/live`
- Lines 2025, 2140, 2170, 2371: Various stats endpoints

**Proposed Constants (add near top of file after library imports):**
```r
# MLB Stats API Configuration
MLB_API_BASE <- "https://statsapi.mlb.com/api/v1"
MLB_API_V1_1 <- "https://statsapi.mlb.com/api/v1.1"

SPORT_ID_MLB <- 1
GAME_TYPE_REGULAR <- "R"
GAME_TYPE_POSTSEASON <- "P"

LEAGUE_ID_AL <- 103
LEAGUE_ID_NL <- 104

#' Build URL for schedule API
build_schedule_url <- function(date, sport_id = SPORT_ID_MLB, game_type = GAME_TYPE_REGULAR) {

  paste0(MLB_API_BASE, "/schedule/games/?sportId=", sport_id,
         "&gameType=", game_type, "&date=", date)
}

#' Build URL for game boxscore
build_boxscore_url <- function(game_id) {
  paste0(MLB_API_BASE, "/game/", game_id, "/boxscore")
}

#' Build URL for game linescore
build_linescore_url <- function(game_id) {
  paste0(MLB_API_BASE, "/game/", game_id, "/linescore")
}

#' Build URL for game feed (live data)
build_game_feed_url <- function(game_id) {
  paste0(MLB_API_V1_1, "/game/", game_id, "/feed/live")
}

#' Build URL for player info
build_player_url <- function(player_id) {
  paste0(MLB_API_BASE, "/people/", player_id)
}

#' Build URL for standings
build_standings_url <- function(league_id, season) {
  paste0(MLB_API_BASE, "/standings?leagueId=", league_id, "&season=", season)
}
```

**Impact:** Centralized API management, easier maintenance

---

### 2.2 Batting Notes Formatting

**Current State:** 13-item ifelse chain (lines 493-511)

**Pattern:**
```r
dt_batNote <- c(
  ifelse(dt_abatNote$E != '' | dt_hbatNote$E != '',
         paste0('<b>E:</b> ', paste0(c(dt_abatNote$E, dt_hbatNote$E) %>% .[. != ''], collapse = ', ')), ''),
  ifelse(dt_abatNote$LOB != '' | dt_hbatNote$LOB != '',
         paste0('<b>LOB:</b> ', ...)), ''),
  # ... repeated 11 more times
)
```

**Proposed Helper:**
```r
#' Combine and format batting notes from both teams
#'
#' @param away_notes data.table with away team notes
#' @param home_notes data.table with home team notes
#' @return Formatted HTML string with all batting notes
format_combined_batting_notes <- function(away_notes, home_notes) {
  note_labels <- c("E", "LOB", "2B", "3B", "HR", "RBI", "SB", "CS", "S", "SF", "GIDP", "GITP", "DP")
  display_labels <- c("E", "LOB", "2B", "3B", "HR", "RBIs", "SB", "CS", "SB", "SF", "GIDP", "GITP", "DP")

  notes <- sapply(seq_along(note_labels), function(i) {
    label <- note_labels[i]
    away_val <- away_notes[[label]]
    home_val <- home_notes[[label]]

    if (away_val != '' || home_val != '') {
      combined <- c(away_val, home_val) %>% .[. != ''] %>% paste0(collapse = ', ')
      paste0('<b>', display_labels[i], ':</b> ', combined)
    } else {
      ''
    }
  })

  notes[notes != ''] %>% paste0(collapse = '. ') %>% paste0('.')
}
```

**Impact:** More maintainable, easier to add/modify note types

---

### 2.3 Game Metadata Extraction

**Current State:** Three similar tryCatch blocks (lines 539-571)

**Pattern:**
```r
umpires_text <- tryCatch({ ... }, error = function(e) "")
game_duration <- tryCatch({ ... }, error = function(e) "")
attendance <- tryCatch({ ... }, error = function(e) "")
```

**Proposed Helper:**
```r
#' Extract game metadata (umpires, duration, attendance) from game feed
#'
#' @param resp_game Response from game feed API
#' @return List with umpires, duration, attendance fields
extract_game_metadata <- function(resp_game) {
  list(
    umpires = tryCatch({
      ump_data <- resp_game$liveData$boxscore$officials
      if (length(ump_data) > 0) {
        # Format umpire string
      } else ""
    }, error = function(e) ""),

    duration = tryCatch({
      resp_game$gameData$gameInfo$gameDurationMinutes
    }, error = function(e) ""),

    attendance = tryCatch({
      att <- resp_game$gameData$gameInfo$attendance
      if (!is.null(att)) format(att, big.mark = ",") else ""
    }, error = function(e) "")
  )
}
```

**Impact:** Cleaner code, single point of error handling

---

### 2.4 Box Score HTML Row Generation

**Current State:** Identical row-building loops for batting/pitching tables

**Lines:** 1717-1735 (away batting), 1768-1786 (home batting), 1825-1843 (away pitching), etc.

**Pattern:**
```r
for (i in 1:nrow(dt_table)) {
  html <- paste0(html, '<tr>')
  for (col in colnames(dt_table)) {
    html <- paste0(html, '<td>', dt_table[i, get(col)], '</td>')
  }
  html <- paste0(html, '</tr>')
}
```

**Proposed Helper:**
```r
#' Generate HTML table rows from a data.table
#'
#' @param dt data.table to convert
#' @param class Optional CSS class for rows
#' @return HTML string with table rows
generate_table_rows <- function(dt, row_class = NULL) {
  class_attr <- if (!is.null(row_class)) paste0(' class="', row_class, '"') else ''

  rows <- apply(dt, 1, function(row) {
    cells <- paste0('<td>', row, '</td>', collapse = '')
    paste0('<tr', class_attr, '>', cells, '</tr>')
  })

  paste0(rows, collapse = '\n')
}
```

**Impact:** ~100 lines reduced, consistent table generation

---

## Phase 3: Low Impact Refactors

### 3.1 Clinch Indicator Logic

**Current State:** Duplicated prefix detection (lines 1169-1174, 1400-1405)

**Pattern:**
```r
clinch_footnotes <- c()
if (any(grepl("^w-", teams))) clinch_footnotes <- c(clinch_footnotes, "w-Clinched wild card")
if (any(grepl("^x-", teams))) clinch_footnotes <- c(clinch_footnotes, "x-Clinched playoff spot")
# ... etc
```

**Proposed Helper:**
```r
#' Extract clinch indicator footnotes from team names
#'
#' @param team_names Vector of team names (may have prefixes)
#' @return Vector of footnote strings
extract_clinch_footnotes <- function(team_names) {
  indicators <- list(
    "w-" = "Clinched wild card",
    "x-" = "Clinched playoff spot",
    "y-" = "Clinched division",
    "z-" = "Clinched best record"
  )

  footnotes <- sapply(names(indicators), function(prefix) {
    if (any(grepl(paste0("^", prefix), team_names))) {
      paste0(prefix, indicators[[prefix]])
    } else NULL
  })

  unlist(footnotes[!sapply(footnotes, is.null)])
}
```

---

### 3.2 Move `process_game_box_scoreMLB()` to Top Level

**Current State:** Nested inside `process_all_gamesMLB()` (lines 310-608)

**Issue:** Makes testing difficult, unclear scope dependencies

**Recommendation:** Extract to top-level function if it doesn't depend on parent scope variables

---

## Implementation Order

Recommended order based on dependencies and impact:

1. **Phase 2.1** - API URL constants (foundational, no dependencies)
2. **Phase 1.1** - Standings table HTML (high impact, self-contained)
3. **Phase 2.2** - Batting notes formatting (medium impact, self-contained)
4. **Phase 2.3** - Game metadata extraction (medium impact, self-contained)
5. **Phase 1.3** - League leaders API calls (high impact, uses URL constants)
6. **Phase 2.4** - Box score HTML rows (needed before 1.2)
7. **Phase 1.2** - Break down generate_newspaper_page2 (largest refactor, depends on others)
8. **Phase 3.x** - Low impact items (cleanup)

## Verification

After each refactor, verify with:
```r
Rscript -e 'source("get_boxscores.r"); get_box_scores("2025", "04", "15", output_dir="./game_data")'
```

Compare generated HTML/PDF against pre-refactoring output to ensure identical results.
