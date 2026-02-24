library(dplyr)
library(baseballr)
library(tidyverse)
library(ggplot2)
# Get data from the 2025 season
data <- statcast_search(
    start_date = "2025-05-01",
    end_date = "2025-09-30",
    player_type = "batter"
) %>%
    arrange(game_pk, at_bat_number, pitch_number) %>%
    group_by(game_pk, at_bat_number, batter) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(
        events, balls, strikes, on_3b, on_2b, on_1b, outs_when_up,
        post_bat_score, bat_score
    )

# create a new variable for the outcome of the at-bat
data1 <- data %>%
    mutate(
        runs = post_bat_score - bat_score,
        base_pos = paste0(
            if_else(!is.na(on_3b), 1, 0),
            if_else(!is.na(on_2b), 1, 0),
            if_else(!is.na(on_1b), 1, 0)
        ),
        count = paste0(balls, "-", strikes)
    )

re_matrix <- data1 %>%
    group_by(base_pos, outs_when_up) %>%
    summarise(
        avg_runs = mean(runs, na.rm = TRUE),
)
write_csv(re_matrix, file.path("data", "xR_matrix.csv"))
labels <- c(
    "0" = "Bases Empty",
    "1" = "Runner on 1st",
    "10" = "Runner on 2nd",
    "11" = "Runners on 1st and 2nd",
    "100" = "Runner on 3rd",
    "101" = "Runners on 1st and 3rd",
    "110" = "Runners on 2nd and 3rd",
    "111" = "Bases Loaded"
)
# Print the resulting matrix
ggplot(
    re_matrix,
    aes(
        x = outs_when_up,
        y = factor(base_pos, levels = names(labels)),
        fill = avg_runs
    )
) +
    geom_raster() +
    scale_y_discrete(labels = labels) +
    scale_fill_gradient(low = "white", high = "red") +
    labs(
        title = "Average Runs Scored by Base State and Outs",
        x = "Outs When Up",
        y = "Base State",
        fill = "Average Runs"
    ) +
    theme_minimal()

## Twitter Idea::
# Every day, post which MLB player scored the best against xRuns in the previous day, along with a brief analysis of their performance and how it compares to their expected performance based on xRuns. Include a visual representation of their performance, such as a heatmap or scatter plot, to engage followers and provide insights into the player's performance.

one_data <- statcast_search(
    start_date = "2026-02-23",
    end_date = "2026-02-23",
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
    left_join(re_matrix, by = c("base_pos", "outs_when_up")) %>%
    mutate(
        xR = avg_runs,
        delta_xR = runs - xR
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

print(stringr::str_replace(best_player$player_name, "^(.*),\\s*(.*)$", "\\2 \\1"))

print("The best player against xRuns on 2026-02-23 was:")
print(stringr::str_replace(best_player$player_name, "^(.*),\\s*(.*)$", "\\2 \\1"))
print("They had a total delta xR of:")
print(best_player$total_delta_xR)
print("The play with the most significant positive delta xR was:")
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

play_text <- paste0(
    "With ", best_play$outs_when_up, " out(s) and ", base_state_desc, ", ",
    "the batter recorded a '", best_play$events, "' for ",
    best_play$runs, " run(s), which is ",
    round(best_play$delta_xR, 2),
    " above expected runs."
)

print(play_text)
