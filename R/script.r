library(dplyr)
library(baseballr)
library(tidyverse)
library(ggplot2)
library(httr)
library(jsonlite)


xR_matrix <- read_csv(file.path("data", "xR_matrix.csv")) %>%
    mutate(
        base_pos = as.integer(base_pos),
        outs_when_up = as.integer(outs_when_up)
    )

date <- format(Sys.Date() - 1, "%Y-%m-%d")
one_data <- statcast_search(
    start_date = date,
    end_date = date,
    player_type = "batter"
) %>%
    arrange(game_pk, at_bat_number, pitch_number) %>%
    group_by(game_pk, at_bat_number, batter) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(
        player_name, events, on_3b, on_2b, on_1b,
        outs_when_up, post_bat_score, bat_score, batter
    ) %>%
    mutate(
        runs = post_bat_score - bat_score,
        base_pos = as.integer((paste0(
            if_else(!is.na(on_3b), 1, 0),
            if_else(!is.na(on_2b), 1, 0),
            if_else(!is.na(on_1b), 1, 0)
        )))
    ) %>%
    left_join(xR_matrix, by = c("base_pos", "outs_when_up")) %>%
    mutate(
        xR = round(avg_runs, 3),
        delta_xR = runs - xR
    ) %>%
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
    )

best_player <- one_data %>%
    group_by(player_name, batter) %>%
    summarise(
        total_delta_xR = sum(delta_xR, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    arrange(desc(total_delta_xR)) %>%
    slice_head(n = 1)

all_best_player_plays <- one_data %>%
    filter(batter == best_player$batter)

best_play <- all_best_player_plays %>%
    arrange(desc(delta_xR)) %>%
    slice_head(n = 1)
base_state_desc <- dplyr::case_when(
    best_play$base_pos == 0 ~ "bases empty",
    best_play$base_pos == 1 ~ "runner on 1st",
    best_play$base_pos == 10 ~ "runner on 2nd",
    best_play$base_pos == 11 ~ "runners on 1st and 2nd",
    best_play$base_pos == 100 ~ "runner on 3rd",
    best_play$base_pos == 101 ~ "runners on 1st and 3rd",
    best_play$base_pos == 110 ~ "runners on 2nd and 3rd",
    best_play$base_pos == 111 ~ "bases loaded",
    TRUE ~ "unknown base state"
)

player_name <- stringr::str_replace(
    best_player$player_name,
    "^(.*),\\s*(.*)$", "\\2 \\1"
)

play_text <- paste0(
    "Biggest swing of the day:\n",
    "With ", best_play$outs_when_up, " out(s) and ", base_state_desc, ", ",
    player_name, " delivered a ", best_play$events,
    " that drove in ", best_play$runs, " run(s).\n",
    "Impact: +", round(best_play$delta_xR, 2),
    " runs above expectation."
)

tweet_text <- paste0(
    "⭐ Player of the Day (ΔxR)\n\n",
    player_name, " generated +",
    round(best_player$total_delta_xR, 2),
    " runs above expected — the best mark in MLB on ", date, ".\n\n",
    play_text
)

player_photo <- paste0("https://img.mlbstatic.com/mlb-photos/image/upload/w_213,q_auto:best/v1/people/", best_player$batter, "/headshot/67/current")

tweet_text <- stringr::str_trim(tweet_text)
if (nchar(tweet_text) > 280) {
    tweet_text <- paste0(substr(tweet_text, 1, 277), "...")
}

required_env <- c("apikey", "apikeysecret", "accesstoken", "accesstokensecret")
missing_env <- required_env[nchar(Sys.getenv(required_env)) == 0]
if (length(missing_env) > 0) {
    stop("Missing env vars: ", paste(missing_env, collapse = ", "))
}

twitter_app <- oauth_app(
    "twitter",
    key = Sys.getenv("apikey"),
    secret = Sys.getenv("apikeysecret")
)

twitter_oauth <- sign_oauth1.0(
    app = twitter_app,
    token = Sys.getenv("accesstoken"),
    token_secret = Sys.getenv("accesstokensecret")
)

tweet_response <- POST(
    url = "https://api.twitter.com/2/tweets",
    config = twitter_oauth,
    add_headers("Content-Type" = "application/json"),
    body = toJSON(list(text = tweet_text), auto_unbox = TRUE),
    encode = "json"
)

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
