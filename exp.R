#setwd("C:/Workspace/Projects/GithubLearning/LinearRegCP")

library(ggplot2)
library(dplyr)
library(tibble)
library(statsr)
library(GGally)

col_selection <- function (col_name) {
    is.numeric(movies_data_full[[col_name]]) ||
    ((is.character(movies_data_full[[col_name]]) ||
    is.factor(movies_data_full[[col_name]])) && 
    length(unique(movies_data_full[[col_name]])) <= 5)
}


load("movies.Rdata")
movies_data_full <- as_tibble(movies)
column_names <- names(movies_data_full)
movies_data_selected <- movies_data_full[,vapply(column_names, col_selection, logical(1))]

#ggpairs(movies_scores[,c("imdb_rating", "critics_score", "audience_score")])




movies_data_selected <- movies_data_full[,vapply(column_names, col_selection, logical(1))]

movies_data_selected <- select(movies_data_selected, -c("imdb_num_votes", "critics_rating", "critics_score",
                                                        "audience_rating", "audience_score"))
movies_data_selected <- movies_data_selected %>%
    mutate(thtr_rel_month = cut(thtr_rel_month, breaks = c(0, 3, 9, 12), labels = c("cold", "warm", "cold")),
           dvd_rel_month = cut(dvd_rel_month, breaks = c(0, 3, 9, 12), labels = c("cold", "warm", "cold")),
           thtr_rel_day = cut(thtr_rel_day, breaks = c(0, 10, 20, 31), labels = c("first", "second", "third")),
           dvd_rel_day = cut(dvd_rel_day, breaks = c(0, 10, 20, 31), labels = c("first", "second", "third")))

movies_data_selected


selected_names <- names(movies_data_selected)
selected_names <- selected_names[!(selected_names == "imdb_rating")]

formula_1 <- paste("imdb_rating", paste(selected_names, collapse = " + "), sep = " ~ ")
fit <- lm(formula = formula_1, data = movies_data_selected)
summary(fit)


fit_lm <- function (movie_df) {
    selected_names <- names(movie_df)
    selected_names <- selected_names[!(selected_names == "imdb_rating")]
    formula_1 <- paste("imdb_rating", paste(selected_names, collapse = " + "), sep = " ~ ")
    fit <- lm(formula = formula_1, data = movie_df)
    fit
}




r2 <- summary(fit)$adj.r.squared

backwards_model_selection_r2_main <- function (movies_df, r2) {
    best_names <- NULL
    iter_names <- names(movies_df)[!(names(movies_df) == "imdb_rating")]
    for (variable in iter_names) {
        selected_names <- names(movies_df)
        reduced_names <- selected_names[!(selected_names == variable)]
        selected_names <- reduced_names[!(reduced_names == "imdb_rating")]
        formula_n <- paste("imdb_rating", paste(selected_names, collapse = " + "), sep = " ~ ")
        fit <- lm(formula = formula_n, data = movies_df)
        if (summary(fit)$adj.r.squared > r2) {
            r2 <- summary(fit)$adj.r.squared
            best_names <- reduced_names
        }
    }
    if (is.null(best_names)) {
        list(movies_df, r2)
    } else {
        new_df <- list(movies_df[,best_names], r2)
        new_df
    }
}

recursive_r2 <- function (movies_df, r2) {
    new_df <- backwards_model_selection_r2_main(movies_df, r2)
    if (length(movies_df) == length(new_df[[1]])) {
        movies_df
    } else {
        recursive_r2(new_df[[1]], new_df[[2]])
    }
}

simpl_movies <- recursive_r2(movies_data_selected, r2)
fit2 <- fit_lm(simpl_movies)
summary(fit2)


r2_fit <- recursive_r2(movies_data_selected, r2_test)
fit2 <- lm(imdb_rating ~ title_type+runtime+dvd_rel_year+
               imdb_rating+best_pic_nom+best_dir_win+top200_box, data = movies_data_selected)
summary(fit2)