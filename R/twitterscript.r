library(dplyr)
library(baseballr)
library(tidyverse)
library(ggplot2)
library(httr)
library(jsonlite)

# ---- Configuration ----
date <- format(Sys.Date() - 1, "%Y-%m-%d")
xR_matrix <- read_csv(file.path("data", "xR288_matrix.csv"))

# ---- Fetch and prepare Statcast data ----
one_data <- statcast_search(
    start_date = date,
    end_date = date,
    player_type = "batter"
) %>%
    select(
        player_name, game_pk, inning, inning_topbot, at_bat_number, pitch_number,
        post_bat_score, bat_score, on_3b, on_2b, on_1b, balls, strikes, outs_when_up, events
    ) %>%
    arrange(game_pk, inning, inning_topbot, at_bat_number, pitch_number) %>%
    mutate(runs_on_pitch = post_bat_score - bat_score) %>%
    group_by(game_pk, inning, inning_topbot) %>%
    mutate(
        pitch_number_in_inning = row_number(),
        inning_runs_scored = sum(runs_on_pitch, na.rm = TRUE),
        runs_prior = cumsum(runs_on_pitch) - runs_on_pitch,
        runs_after = inning_runs_scored - runs_prior
    ) %>%
    ungroup() %>%
    mutate(
        base_pos = as.character(paste0(
            if_else(!is.na(on_3b), 1, 0),
            if_else(!is.na(on_2b), 1, 0),
            if_else(!is.na(on_1b), 1, 0)
        )),
        count = paste0(balls, "-", strikes)
    ) %>%
    left_join(xR_matrix, by = c("base_pos", "outs_when_up", "count")) %>%
    mutate(
        events = dplyr::case_when(
            events == "home_run" ~ "Home Run",
            events == "double" ~ "Double",
            events == "triple" ~ "Triple",
            events == "single" ~ "Single",
            events == "field_out" ~ "Field Out",
            events == "strikeout" ~ "Strikeout",
            events == "walk" ~ "Walk",
            events == "hit_by_pitch" ~ "Hit By Pitch",
            events == "sac_fly" ~ "Sac Fly",
            events == "sac_bunt" ~ "Sac Bunt",
            events == "double_play" ~ "Double Play",
            events == "force_out" ~ "Force Out",
            events == "fielders_choice" ~ "Fielder's Choice",
            events == "other_out" ~ "Other Out",
            is.na(events) | events == "null" ~ "Unknown Event",
            TRUE ~ events
        )
    ) %>%
    group_by(game_pk, inning, inning_topbot) %>%
    mutate(
        xR_next = lead(xRp),
        delta_xR = if_else(
            is.na(xR_next),
            runs_on_pitch - xRp,
            runs_on_pitch + xR_next - xRp
        )
    ) %>%
    ungroup()

# ---- Guard against empty data ----
if (nrow(one_data) == 0) {
    stop("No MLB data available for date: ", date)
}

# ---- Identify best player and play ----
best_player <- one_data %>%
    group_by(player_name) %>%
    summarise(total_delta_xR = sum(delta_xR, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_delta_xR)) %>%
    slice_head(n = 1)

all_best_player_plays <- one_data %>%
    filter(player_name == best_player$player_name)

best_play <- all_best_player_plays %>%
    arrange(desc(delta_xR)) %>%
    slice_head(n = 1)

base_state_desc <- dplyr::case_when(
    best_play$base_pos == "000" ~ "Bases empty",
    best_play$base_pos == "001" ~ "Runner on 1st",
    best_play$base_pos == "010" ~ "Runner on 2nd",
    best_play$base_pos == "011" ~ "Runners on 1st and 2nd",
    best_play$base_pos == "100" ~ "Runner on 3rd",
    best_play$base_pos == "101" ~ "Runners on 1st and 3rd",
    best_play$base_pos == "110" ~ "Runners on 2nd and 3rd",
    best_play$base_pos == "111" ~ "Bases loaded",
    TRUE ~ "Unknown Base State"
)

player_name <- stringr::str_replace(
    best_player$player_name,
    "^(.*),\\s*(.*)$", "\\2 \\1"
)

# ---- Build tweet text ----
play_result_text <- dplyr::if_else(
    best_play$events == "Home Run" & best_play$runs_on_pitch == 4,
    "Grand Slam",
    paste0(best_play$runs_on_pitch, "-run ", best_play$events)
)

play_text <- paste0(
    best_play$outs_when_up,
    if_else(best_play$outs_when_up == 1, " out | ", " outs | "),
    base_state_desc, "\n",
    "   ", play_result_text, "\n",
    "Impact: +", round(best_play$delta_xR, 2), " runs above expectation."
)

tweet_text <- paste0(
    "⭐ Batter of the Day: ",
    player_name, "\n\n",
    round(best_player$total_delta_xR, 2),
    " runs above expected (MLB best on ",
    format(as.Date(date), "%b %d, %Y"),
    ").\n\n",
    "Game Changing Moment: \n\n",
    play_text
)

cat(tweet_text)

# ---- Compare vs cached Statcast data ----
cache_dir <- file.path(".", "Statcast Data")
cache_file <- file.path(cache_dir, "statcast_data.rds")

add_data <- readRDS(cache_file)

day_data <- one_data %>%
    group_by(base_pos, count, outs_when_up) %>%
    summarise(avg_delta_xR = mean(delta_xR, na.rm = TRUE), .groups = "drop")

all_data_xR <- add_data %>%
    mutate(
        base_pos = paste0(
            if_else(!is.na(on_3b), 1, 0),
            if_else(!is.na(on_2b), 1, 0),
            if_else(!is.na(on_1b), 1, 0)
        ),
        count = paste0(balls, "-", strikes)
    ) %>%
    group_by(base_pos, count, outs_when_up) %>%
    summarise(mean_delta_xR = mean(delta_run_exp, na.rm = TRUE), .groups = "drop") %>%
    left_join(day_data, by = c("base_pos", "count", "outs_when_up")) %>%
    select(base_pos, count, outs_when_up, mean_delta_xR, avg_delta_xR)

ttest <- t.test(all_data_xR$mean_delta_xR, all_data_xR$avg_delta_xR, paired = TRUE)
print(ttest)

# ---- Enforce tweet length ----
tweet_text <- stringr::str_trim(tweet_text)
if (nchar(tweet_text) > 280) {
    tweet_text <- paste0(substr(tweet_text, 1, 277), "...")
}

# ---- Validate env vars ----
required_env <- c("apikey", "apikeysecret", "accesstoken", "accesstokensecret")
missing_env <- required_env[nchar(Sys.getenv(required_env)) == 0]
if (length(missing_env) > 0) {
    stop("Missing env vars: ", paste(missing_env, collapse = ", "))
}

# ---- Post tweet ----
twitter_app <- oauth_app(
    "twitter",
    key = Sys.getenv("apikey"),
    secret = Sys.getenv("apikeysecret")
)

tweet_response <- POST(
    url = "https://api.twitter.com/2/tweets",
    config = twitter_oauth,
    add_headers("Content-Type" = "application/json"),
    body = toJSON(list(text = tweet_text), auto_unbox = TRUE),
    encode = "json"
)

# ---- Error handling ----
if (http_error(tweet_response)) {
    if (status_code(tweet_response) == 401) {
        stop(
            "Tweet failed: 401 Unauthorized. ",
            "Verify API key/secret and access token/secret match the same app, ",
            "and the app has Read and Write permissions (regenerate tokens after changing permissions). "
        )
    }
    stop(
        "Tweet failed: ",
        status_code(tweet_response),
        " ",
        content(tweet_response, as = "text", encoding = "UTF-8")
    )
}
