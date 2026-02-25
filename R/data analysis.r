library(dplyr)
library(baseballr)
library(tidyverse)
library(ggplot2)


statcast_search_chunked <- function(start_date, end_date, player_type = "batter", step_days = 7, ...) {
    start_date <- as.Date(start_date)
    end_date <- as.Date(end_date)

    if (end_date < start_date) {
        stop("end_date must be on or after start_date")
    }

    start_points <- seq(start_date, end_date, by = step_days)

    purrr::map_dfr(start_points, function(s) {
        e <- min(s + (step_days - 1), end_date)
        message("Fetching Statcast: ", s, " to ", e)
        statcast_search(
            start_date = format(s, "%Y-%m-%d"),
            end_date = format(e, "%Y-%m-%d"),
            player_type = player_type,
            ...
        )
    })
}
cache_dir <- file.path(".", "Statcast Data")
cache_file <- file.path(cache_dir, "statcast_data.rds")

if (file.exists(cache_file)) {
    data <- readRDS(cache_file)
} else {
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    data <- statcast_search_chunked(
        start_date = "2025-05-01",
        end_date = "2025-09-30",
        player_type = "batter",
        step_days = 7
    ) %>%
        select(game_pk, inning, inning_topbot, at_bat_number, pitch_number, post_bat_score, bat_score, on_3b, on_2b, on_1b, balls, strikes, outs_when_up) %>%
        arrange(game_pk, inning, inning_topbot, at_bat_number, pitch_number) %>%
        mutate(
            runs_on_pitch = post_bat_score - bat_score,
        ) %>%
        group_by(game_pk, inning, inning_topbot) %>%
        mutate(
            inning_runs_scored = sum(runs_on_pitch, na.rm = TRUE),
            runs_prior = cumsum(runs_on_pitch) - runs_on_pitch,
            runs_after = inning_runs_scored - runs_prior
        ) %>%
        ungroup() %>%
        mutate(
            base_pos = paste0(
                if_else(!is.na(on_3b), 1, 0),
                if_else(!is.na(on_2b), 1, 0),
                if_else(!is.na(on_1b), 1, 0)
            ),
            count = paste0(balls, "-", strikes)
        ) %>%
        select(-balls, -strikes, -on_3b, -on_2b, -on_1b, -post_bat_score, -bat_score)
    saveRDS(data, cache_file)
}

# Add derived columns for runs, base state, and count

# Build run expectancy matrix by base state and outs
xR28 <- data %>%
    group_by(base_pos, outs_when_up) %>%
    summarise(
        avg_runs = mean(runs_after, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(
        base_pos = as.character(base_pos),
        outs_when_up = as.integer(outs_when_up)
    )

# Persist matrix to CSV
write_csv(xR28, file.path("data", "xR28.csv"))

xR_matrix <- data %>%
    arrange(game_pk, at_bat_number, pitch_number) %>%
    group_by(count, base_pos, outs_when_up) %>%
    summarise(
        n = n(),
        avg_runs = mean(runs_after, na.rm = TRUE)
    ) %>%
    right_join(xR28, by = c("base_pos", "outs_when_up")) %>%
    mutate(
        xR_vs_xR28 = round(avg_runs.x - avg_runs.y, 3),
        xR = avg_runs.x,
        xR28 = avg_runs.y
    ) %>%
    select(-avg_runs.x, -avg_runs.y)

write_csv(xR_matrix, file.path("data", "xR_matrix.csv"))

count_change <- xR_matrix %>%
    group_by(count) %>%
    summarise(
        change = mean(xR_vs_xR28, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(
        balls = as.integer(stringr::str_split_fixed(count, "-", 2)[, 1]),
        strikes = as.integer(stringr::str_split_fixed(count, "-", 2)[, 2])
    ) %>%
    arrange(balls, strikes)

ggplot(count_change, aes(x = count, y = change, group = 1)) +
    geom_line() +
    geom_point() +
    labs(
        title = "Change in Run Expectancy by Count",
        x = "Count (Balls-Strikes)",
        y = "Change in Run Expectancy"
    ) +
    theme_minimal()
