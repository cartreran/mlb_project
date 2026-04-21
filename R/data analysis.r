# Load required packages ---------------------------------------------------
library(dplyr)
library(baseballr)
library(tidyverse)
library(ggplot2)

#' Fetch Statcast data in chunks to avoid API limits
#'
#' @param start_date Start date (YYYY-MM-DD).
#' @param end_date End date (YYYY-MM-DD).
#' @param player_type "batter" or "pitcher".
#' @param step_days Chunk size in days.
#' @param ... Additional args passed to `statcast_search()`.
#' @return A data frame of Statcast results.
statcast_search_chunked <- function(start_date,
                                    end_date,
                                    player_type = "batter",
                                    step_days = 7,
                                    ...) {
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

# Cache paths --------------------------------------------------------------
cache_dir <- file.path(".", "Statcast Data")
cache_file <- file.path(cache_dir, "statcast_data.rds")

# Load cached data or fetch fresh -----------------------------------------
if (file.exists(cache_file)) {
    data <- readRDS(cache_file)
} else {
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    data <- statcast_search_chunked(
        start_date = "2025-05-01",
        end_date = "2025-09-30",
        player_type = "batter",
        step_days = 7
    )
    saveRDS(data, cache_file)
}

# Keep raw data if needed --------------------------------------------------
all_data <- data

# Derive run states and contexts -------------------------------------------
data <- data %>%
    select(
        game_pk, inning, inning_topbot, at_bat_number, pitch_number,
        post_bat_score, bat_score, on_3b, on_2b, on_1b,
        balls, strikes, outs_when_up, stand, p_throws
    ) %>%
    arrange(game_pk, inning, inning_topbot, at_bat_number, pitch_number) %>%
    mutate(
        runs_on_pitch = post_bat_score - bat_score
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
        count = paste0(balls, "-", strikes),
        hands = paste0(stand, p_throws)
    ) %>%
    select(
        -balls, -strikes, -on_3b, -on_2b, -on_1b,
        -post_bat_score, -bat_score, -stand, -p_throws
    )

# Build run expectancy (xR28) ----------------------------------------------
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

# Persist xR28 matrix -------------------------------------------------------
write_csv(xR28, file.path("data", "xR28.csv"))

# Build count-based run expectancy matrix ---------------------------------
xR288 <- data %>%
    arrange(game_pk, at_bat_number, pitch_number) %>%
    group_by(count, base_pos, outs_when_up) %>%
    summarise(
        n = n(),
        avg_runs = mean(runs_after, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    right_join(xR28, by = c("base_pos", "outs_when_up")) %>%
    mutate(
        xR_vs_xR28 = round(avg_runs.x - avg_runs.y, 3),
        xR = avg_runs.x,
        xR28 = avg_runs.y
    ) %>%
    select(-avg_runs.x, -avg_runs.y)

# Partial Pooling
k <- 250

xR288 <- xR288 %>%
    mutate(
        weight = n / (n + k),
        xRp = weight * xR + (1 - weight) * xR28,
        diff = xRp - xR
    )

xR1151 <- data %>%
    arrange(game_pk, at_bat_number, pitch_number) %>%
    group_by(hands, count, base_pos, outs_when_up) %>%
    summarise(
        n = n(),
        avg_runs = mean(runs_after, na.rm = TRUE),
        .groups = "drop"
    )


# Persist count-based matrix ------------------------------------------------
write_csv(xR288, file.path("data", "xR288_matrix.csv"))

# Summarize change by count -------------------------------------------------
count_change <- xR288 %>%
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

# Plot change in run expectancy by count -----------------------------------
ggplot(count_change, aes(x = count, y = change, group = 1)) +
    geom_line() +
    geom_point() +
    labs(
        title = "Change in Run Expectancy by Count",
        x = "Count (Balls-Strikes)",
        y = "Change in Run Expectancy"
    ) +
    theme_minimal()

# Test predictor impact

lm <- lm(xR ~ count + base_pos + outs_when_up, data = xR288)

anova(lm)

# Model diagnostics --------------------------------------------------------
par(mfrow = c(2, 2))
plot(lm)
par(mfrow = c(1, 1))

# Residual checks
residuals_tbl <- tibble(
    fitted = fitted(lm),
    residuals = resid(lm)
)

ggplot(residuals_tbl, aes(fitted, residuals)) +
    geom_point(alpha = 0.4) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = "Residuals vs Fitted") +
    theme_minimal()

ggplot(residuals_tbl, aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line() +
    labs(title = "Normal Q-Q Plot") +
    theme_minimal()

# Error metrics (train)
rmse <- sqrt(mean(resid(lm)^2, na.rm = TRUE))
mae <- mean(abs(resid(lm)), na.rm = TRUE)
cat("RMSE:", round(rmse, 4), "\n")
cat("MAE:", round(mae, 4), "\n")

# Simple 5-fold CV RMSE
set.seed(123)
folds <- sample(rep(1:5, length.out = nrow(xR288)))
cv_rmse <- map_dbl(1:5, function(k) {
    train <- xR288[folds != k, ]
    test <- xR288[folds == k, ]
    m <- lm(xR ~ count + base_pos + outs_when_up, data = train)
    preds <- predict(m, newdata = test)
    sqrt(mean((test$xR - preds)^2, na.rm = TRUE))
})
cat("CV RMSE (mean):", round(mean(cv_rmse), 4), "\n")

single_models <- list(
    count = lm(xR ~ count, data = xR288),
    base_pos = lm(xR ~ base_pos, data = xR288),
    outs_when_up = lm(xR ~ outs_when_up, data = xR288)
)

model_impact <- purrr::map_dfr(names(single_models), function(nm) {
    s <- summary(single_models[[nm]])
    tibble(
        predictor = nm,
        r_squared = s$r.squared,
        adj_r_squared = s$adj.r.squared
    )
}) %>%
    arrange(desc(r_squared))

print(model_impact)

# Ideas --------------------------------------------------------------------
# - Pitcher consistency (pitch mix variation by situation and game)
# - Add L/R pitcher and batter splits to the xR model
# - Perform t-test of delta_xR vs delta_xR28
