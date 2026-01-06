# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an R-based application that automatically generates newspaper-style MLB box scores with standings and league leader information. It scrapes data from the MLB Stats API, processes it, and deploys HTML/PDF files to a web server via FTP. The project runs daily via GitHub Actions.

## Commands

### Running the Script

Generate box scores for a specific date:
```r
Rscript -e 'source("get_boxscores.r"); get_box_scores("2025", "04", "15", output_dir="./game_data")'
```

Generate box scores for yesterday:
```r
Rscript -e 'source("get_boxscores.r"); get_box_scores(format(Sys.Date()-1, "%Y"), format(Sys.Date()-1, "%m"), format(Sys.Date()-1, "%d"), output_dir="./game_data")'
```

### Installing Dependencies

Install all required R packages:
```r
Rscript -e 'install.packages(c("here", "data.table", "magrittr", "httr", "jsonlite", "stringr", "chromote", "base64enc"))'
```

### GitHub Actions

The workflow runs automatically daily at 7 AM UTC. To manually trigger:
- Go to Actions tab in GitHub
- Select "Generate and Deploy Box Scores" workflow
- Click "Run workflow"

## Architecture

### Core Function Pipeline

The main entry point `get_box_scores()` orchestrates the entire pipeline:

1. **Data Collection** (`process_all_gamesMLB`)
   - Fetches all games for a given date from MLB Stats API
   - For each game, calls `process_game_box_scoreMLB` to collect:
     - Box score data (players, stats, positions)
     - Linescore data (innings, runs)
     - Team franchise information
     - Player boxscore names and formatting

2. **Standings & Leaders** (optional)
   - `get_standingsMLB`: Fetches division standings for AL/NL
   - `get_league_leadersMLB`: Fetches batting/pitching leader stats

3. **HTML Generation** (`generate_newspaper_page2`)
   - Creates newspaper-style layout with:
     - Box scores for all games
     - Team statistics tables
     - Pitching lines
     - Division standings
     - League leaders
     - Navigation links for previous/next dates

4. **PDF Generation** (`print_to_pdf`)
   - Uses chromote (headless Chrome) to convert HTML to PDF
   - Configured for landscape orientation (11.7" x 15.14")

### Key Functions

- `formatBoxName()`: Formats player names for display (removes punctuation, spaces)
- `process_all_gamesMLB()`: Main game data collection function
- `generate_newspaper_page2()`: HTML generation for newspaper-style layout
- `get_box_scores()`: Main orchestrator with options for output format

### Data Flow

```
MLB Stats API → process_all_gamesMLB → [games_data list]
                                              ↓
MLB Stats API → get_standingsMLB → [standings_data]
                                              ↓
MLB Stats API → get_league_leadersMLB → [leaders_data]
                                              ↓
                         generate_newspaper_page2 → HTML file
                                              ↓
                         print_to_pdf → PDF file
                                              ↓
                         GitHub Actions FTP → Web server
```

### MLB Stats API Endpoints

The script uses several MLB Stats API endpoints:
- Schedule: `http://statsapi.mlb.com/api/v1/schedule/games/`
- Box scores: `http://statsapi.mlb.com/api/v1/game/{gameId}/boxscore`
- Linescore: `http://statsapi.mlb.com/api/v1/game/{gameId}/linescore`
- Team info: `http://statsapi.mlb.com/api/v1/teams/{teamId}`
- Player info: `http://statsapi.mlb.com/api/v1/people/{playerId}`
- Standings: `https://statsapi.mlb.com/api/v1/standings`

### Output Structure

Files are generated in `./game_data/`:
- `YYYYMMDD.html`: Newspaper-style HTML page
- `YYYYMMDD.pdf`: PDF version of the page

### Deployment

The GitHub Actions workflow deploys only the most recent day's files via FTP, excluding all other HTML/PDF files to prevent overwriting the entire archive.

## Important Notes

- The main script file is ~2100 lines, containing all core functionality for data collection, HTML/PDF generation, and deployment
- Chrome/Chromium must be installed for PDF generation (chromote dependency)
- The script handles games that finish early or have incomplete data
- Player stats include both game stats and season-to-date stats for context
