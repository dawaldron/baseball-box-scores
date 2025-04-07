# MLB Box Scores Generator

Automatically generates daily MLB box scores with standings and league leader information.

Currently deploying to https://waldrn.com/boxscores/.

## Overview

This repository contains an R-based solution for scraping MLB box scores, standings, and league leader data from the MLB Stats API. It generates clean, newspaper-style HTML pages with all game information from the previous day and deploys them to a web server via FTP.

## Key Features

- Retrieves complete box scores for all MLB games, Includes team and player stats, game notes, and detailed pitching information
- Generates standings tables for all MLB divisions
- Displays current league leaders in various batting and pitching categories
- Creates both HTML and PDF versions of the content
- Responsive design works on desktop and mobile devices
- Automatic navigation between dates
- Daily automatic execution via GitHub Actions

## Technical Components

- **get_boxscores.r**: Main R script containing functions for fetching and processing game data
- **.github/workflows/main.yml**: GitHub Actions workflow configuration

## Dependencies

- R 4.2.0 or higher
- Required R packages: here, data.table, magrittr, httr, jsonlite, stringr, chromote
