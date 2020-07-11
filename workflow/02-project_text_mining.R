library(dplyr)
library(stringr)
library(assertr)
library(ensurer)
library(tidytext)
library(recipes)
library(conflicted)
conflict_prefer("filter", "dplyr")
source(here::here("R/tfidf.R"))
source(here::here("workflow/01-data_setup.R"))

plan_project_term <-
  drake::drake_plan(
    df_token_count =
      df_text_token %>%
      token_count(term) %>%
      verify(nrow(.) == 8769L),
    df_text_token_tfidf =
      df_text_token %>%
      project_tfidf() %>%
      assertr::verify(dim(.) == c(76835, 6)),
    df_dtm_freq =
      df_text_token_tfidf %>%
      project_tfidf_dtm(df_token_count, type = "freq") %>%
      assertr::verify(dim(.) == c(473, 8652)),
    df_dtm_tfidf =
      df_text_token_tfidf %>%
      project_tfidf_dtm(df_token_count, type = "tf_idf") %>%
      verify(dim(.) == dim(df_dtm_freq)),
    df_dtm_pca =
      df_dtm_tfidf %>%
      recipe(project_id ~ .) %>%
      step_center(all_predictors()) %>%
      step_scale(all_predictors()) %>%
      step_pca(all_predictors(), num_comp = 4) %>%
      prep(training = df_dtm_tfidf, strings_as_factors = FALSE) %>%
      juice(),
    clust_num = 3,
    prj_term_kclust =
      df_dtm_pca %>%
      select(-project_id) %>%
      kmeans(centers = clust_num),
    df_term_cluster =
      prj_term_kclust %>%
      broom::augment(data = df_dtm_pca),
    out_project_cluster_csv =
      seq_len(clust_num) %>%
      purrr::map_dfr(
        ~ df_readyfor_ecology_description %>%
          select(-information) %>%
          filter(project_id %in% c(df_term_cluster %>%
                                     filter(.cluster == .x) %>%
                                     pull(project_id) %>%
                                     as.character() %>%
                                     stringr::str_remove("prj_"))),
        .id = "cluster") %>%
      arrange(cluster, project_id) %>%
      readr::write_csv(here::here("data", paste0("project_cluster", clust_num, ".csv"))),
    check_cluster_count =  {
      if(file.exists(here::here("data", paste0("project_cluster", clust_num, ".csv")))) {
        readr::read_csv(here::here("data", paste0("project_cluster", clust_num, ".csv"))) %>%
          count(cluster) %>%
          pull(n) %>%
          ensurer::ensure(all.equal(., c(155, 272, 46)))
        }
      },
    # 各クラスタで特徴的な単語 ------------------------------------------------------------
    df_cluster_import_words =
      seq_len(clust_num) %>%
      purrr::map_dfr(
        ~ df_dtm_tfidf %>%
          filter(project_id %in% c(df_term_cluster %>%
                                     filter(.cluster == .x) %>%
                                     pull(project_id))) %>%
          tidyr::gather(term, value, -project_id) %>%
          group_by(term) %>%
          summarise(value = sum(value), .groups = "drop") %>%
          arrange(desc(value)) %>%
          filter(value > 0),
        .id = "cluster") %>%
      group_by(cluster) %>%
      slice_max(value, n = 30) %>%
      ungroup() %>%
      arrange(cluster, desc(value)))

plan_project_term <-
  drake::bind_plans(plan_setup_data, plan_project_term)
# drake::make(plan_project_term,
#             packages = c("dplyr", "purrr", "recipes"),
#             seed = 123)
