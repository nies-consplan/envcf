#####################################
# プロジェクトの成功・失敗を回帰モデルによって説明
#####################################
source(here::here("workflow/02-project_text_mining.R"))
library(dplyr)
library(tidyr)
library(assertr)
library(ggplot2)
library(recipes)
library(lubridate)
drake::loadd(list = c("df_readyfor_text", "df_text_token"))
source(here::here("R/cluster_count.R"))

# Data Modified -----------------------------------------------------------
plan_project_successful_model <- drake::drake_plan(
  df_target473 =
    df_target %>%
    dplyr::left_join(df_term_cluster %>%
                       dplyr::mutate(project_id = stringr::str_remove(project_id, "prj_")) %>%
                       dplyr::select(project_id, cluster = .cluster),
                     by = "project_id") %>%
    assertr::verify(dim(.) == c(473, 16)),
  df_regression_target =
    df_target473 %>%
    dplyr::mutate(cluster1_overlapped_proj = purrr::map_dbl(project_id,
                                                            ~ cluster1project_count(df_target473, project_id = .x))) %>%
    dplyr::left_join(
      seq_len(clust_num) %>%
        purrr::map_dfr(
          ~ cluster_project_count(df_target473, .x) %>%
            dplyr::select(project_id, overlapped_proj)),
      by = "project_id") %>%
    # df_target ... 473プロジェクトでの重複確認
    mutate(
      max_overlapped_proj = purrr::map_dbl(
        project_period,
        ~ sum(lubridate::int_overlaps(.x,
                                      df_target$project_period), na.rm = TRUE) - 1),
      min_overlapped_proj =　purrr::map2_dbl(open, close,
                                            ~ max(sum(dplyr::between(df_target$open, .x, .y)),
                                                  sum(dplyr::between(df_target$close, .x, .y)))) - 1) %>%
    dplyr::select(-text, -close) %>%
    dplyr::mutate(success = dplyr::case_when(
      success == "success" ~ "1_success",
      success == "failure" ~ "0_failure") %>%
        forcats::as_factor()) %>%
    dplyr::distinct(project_id, .keep_all = TRUE) %>%
    tidyr::unnest_wider(metadata) %>%
    tidyr::unnest_wider(condition) %>%
    tidyr::unnest_wider(type) %>%
    tidyr::unnest_wider(sns_share) %>%
    tidyr::unnest_wider(media) %>%
    dplyr::mutate(achievement_rate = amount_raised / goal,
                  tag_n = purrr::map_dbl(
                    tags,
                    ~ length(.x %>%
                               purrr::pluck(1))
                  ),
                  founder_project_count = purrr::map_dbl(founder,
                                                         ~ .x %>%
                                                           pull(project_count) %>%
                                                           unique()),
                  return_n   = purrr::map_dbl(
                    return,
                    ~ nrow(.x)),
                  announce_n = purrr::map2_dbl(
                    announce, project_period,
                    ~ .x %>%
                      filter(date %within% .y) %>%
                      nrow()),
                  comment_n  = purrr::map2_dbl(
                    comment, project_period,
                    ~ .x %>%
                      filter(date %within% .y) %>%
                      nrow()),
                  project_days = as.numeric(project_period, "days")) %>%
    tidyr::unnest_wider(facebook) %>%
    dplyr::mutate(
      facebook = purrr::pmap_dbl(., ~ pull(..14, share_count)),
    ) %>%
    dplyr::select(-announce, -comment, -founder, -project_period,
                  -founder, -project_period, -tags, -days,
                  -`...1`) %>%
    tidyr::unnest_wider(return) %>%
    dplyr::select(project_id, success,
                  achievement_rate, amount_raised, goal,
                  backers, found_model, project_type,
                  tag_n, project_days,
                  cluster_overlapped_proj = overlapped_proj,
                  cluster1_overlapped_proj,
                  max_overlapped_proj,
                  min_overlapped_proj,
                  founder_project_count,
                  twitter, hatena, facebook,
                  return_n,
                  announce_n,comment_n,
                  images, movies, cluster) %>%
    assertr::verify(dim(.) == c(473, 24)) %>%
    dplyr::left_join(df_readyfor_text %>%
                       dplyr::select(project_id, text_length),
                     by = "project_id") %>%
    dplyr::left_join(df_readyfor_ecology_description %>%
                       tidyr::unnest(cols = c(information)) %>%
                       dplyr::mutate(title_length = stringr::str_length(title)) %>%
                       dplyr::select(project_id, title_length),
                     by = "project_id") %>%
    dplyr::mutate(media_total = images + movies,
                  media_rate = media_total / text_length) %>%
    verify(has_all_names("project_id", "success", "achievement_rate", "amount_raised",
                         "goal", "backers",
                         "found_model", "project_type",
                         "tag_n", "project_days",
                         "cluster_overlapped_proj",
                         "cluster1_overlapped_proj",
                         "max_overlapped_proj", "min_overlapped_proj",
                         "founder_project_count",
                         "twitter", "hatena", "facebook",
                         "return_n", "announce_n", "comment_n",
                         "images", "movies",
                         "text_length", "title_length",
                         "media_total", "media_rate",
                         "cluster")),
  # the mean achievement rates by contex
  cluster_achievement_rate =
    df_regression_target %>%
    group_by(cluster) %>%
    summarise(
      n = n(),
      mean_ahieve = mean(achievement_rate, na.rm = TRUE),
      sd_achieve = sd(achievement_rate, na.rm = TRUE),
      .groups = "drop"),
  df_text_token_target =
    df_text_token %>%
    inner_join(df_regression_target %>%
                 select(project_id, success),
               by = "project_id") %>%
    assertr::verify(dim(.) == c(nrow(df_text_token), 6)),
  tgt_top_term30 =
    df_text_token_target %>%
    group_by(term) %>%
    summarise(freq = sum(freq), .groups = "drop") %>%
    slice_max(order_by = freq, n = 30) %>%
    pull(term),
  df_freq_term_duplicate =
    df_text_token_target %>%
    filter(term %in% tgt_top_term30) %>%
    group_by(success, term) %>%
    summarise(freq = sum(freq), .groups = "drop") %>%
    left_join(df_freq_term_en, by = "term") %>%
    assertr::verify(nrow(.) == 60L),
  check_freq_term_en_na =
    df_freq_term_duplicate %>%
    filter(is.na(term_en)) %>%
    assertr::verify(nrow(.) == 0L),
  df_frequency_term_prop =
    df_freq_term_duplicate %>%
    tidyr::pivot_wider(names_from = success,
                       values_from = freq,
                       values_fill = 0,
                       names_prefix = "n_") %>%
    left_join(
      df_freq_term_duplicate %>%
        group_by(term, term_en) %>%
        summarise(total = sum(freq), .groups = "drop"),
      by = c("term", "term_en")) %>%
    mutate(prop_success = n_1_success / total,
           prop_failure = n_0_failure / total) %>%
    select(!term) %>%
    arrange(desc(total)),
  df_frequency_term_project_count =
    df_text_token_target %>%
    filter(term %in% df_freq_term_duplicate$term) %>%
    count(success, term) %>%
    tidyr::pivot_wider(
      names_from = success,
      values_from = n,
      values_fill = 0,
      names_prefix = "n_") %>%
    mutate(total = n_1_success + n_0_failure) %>%
    mutate(prop_success = n_1_success / total,
           prop_failure = n_0_failure / total) %>%
    arrange(desc(total)) %>%
    left_join(df_freq_term_duplicate %>%
                distinct(term, term_en),
              by = "term") %>%
    select(!term) %>%
    select(term_en, everything()) %>%
    arrange(desc(total)),
  regression_target =
    df_regression_target %>%
    mutate(success = forcats::fct_rev(success),
           experience_dummy = if_else(founder_project_count > 1, 1, 0),
           project_type = case_when(project_type == "normal" ~ "normal",
                                    project_type == "charity" ~ "charity",
                                    project_type == "furusato_tax" ~ "government"),
           project_type = factor(project_type, levels = c("normal","charity","government")),
           project_model = case_when(
             found_model == "all_or_nothing" ~ "AON",
             found_model == "keep_it_all" ~ "KIA"),
           project_model = factor(project_model, levels = c("AON", "KIA")),
           text = text_length/1000,
           text2 = text^2) %>%
    select(-found_model) %>%
    rename(amount = amount_raised,
           experience_n = founder_project_count,
           reward = return_n,
           pics = images,
           videos = movies) %>%
    mutate(tag2 = tag_n ^ 2,
           pics2 = pics ^ 2,
           reward2 = reward ^ 2,
           videos2 = videos ^ 2,
           fb2 = facebook ^ 2),
  success_failure_count =
    regression_target %>%
    group_by(success) %>%
    count(),
  ols_all =
    lm(achievement_rate ~
         project_type  +
         project_model +
         reward +
         pics +
         videos +
         text + text2 +
         factor(cluster) +
         experience_dummy +
         facebook +
         tag_n +
         announce_n +
         max_overlapped_proj,
       data = regression_target),
  logit_all =
    glm(factor(success) ~
          project_type  +
          project_model +
          tag_n +
          facebook +
          max_overlapped_proj +
          experience_dummy +
          announce_n +
          reward +
          pics +
          videos +
          text + text2 +
          factor(cluster),
        family = "binomial",
        data = regression_target),
  ols_si =
    lm(achievement_rate ~
         project_type  +
         project_model +
         reward +
         # reward2 +
         pics +
         pics2 +
         videos +
         videos2 +
         text + text2 +
         factor(cluster) +
         experience_dummy +
         facebook +
         tag_n +
         announce_n +
         max_overlapped_proj,
       data = regression_target),
  logit_si =
    glm(
      success ~
        project_type  +
        project_model +
        reward +
        # reward2 +
        pics +
        pics2 +
        videos +
        videos2 +
        text + text2 +
        factor(cluster) +
        experience_dummy +
        facebook +
        tag_n +
        announce_n +
        max_overlapped_proj,
      data = regression_target,
      family = "binomial"),
  tbt_aer =
    AER::tobit(achievement_rate ~
                 project_type  +
                 project_model +
                 reward +
                 pics +
                 videos +
                 text + text2 +
                 factor(cluster) +
                 experience_dummy +
                 facebook +
                 tag_n +
                 announce_n +
                 max_overlapped_proj,
               data = regression_target),
  tbt_summary =
    summary(tbt_aer)
)

plan_project_successful_model <-
  drake::bind_plans(
    plan_project_term,
    plan_project_successful_model)
drake::make(plan_project_successful_model,
            packages = c("assertr", "dplyr", "lubridate", "purrr", "recipes", "tidyr", "AER"),
            seed = 123)
# drake::loadd(list = c("df_regression_target", "regression_target", "success_model_formula", "logit_all"))
