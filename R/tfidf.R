project_tfidf <- function(data) {
  data %>%
    dplyr::select(project_id, term, freq) %>%
    dplyr::group_by(project_id, term) %>%
    dplyr::summarise(freq = sum(freq), .groups = "drop") %>%
    tidytext::bind_tf_idf(document = project_id, term = term, n = freq) %>%
    dplyr::arrange(desc(freq)) %>%
    dplyr::mutate(project_id = paste0("prj_", project_id))
}

project_tfidf_tdm <- function(df_project, df_term_count, type = "freq") {
  type <- rlang::enquo(type)
  df_project %>%
    dplyr::select(project_id, term, !!type) %>%
    tidyr::spread(key = "project_id", value = !!type, fill = 0) %>%
    dplyr::filter(
      !stringr::str_detect(term, stringr::regex("^[:punct:]")),
      !stringr::str_detect(term, stringr::regex("[A-Z]", ignore_case = TRUE))) %>%
    dplyr::left_join(df_term_count,
                     by = "term") %>%
    dplyr::arrange(rowid) %>%
    dplyr::mutate(term = forcats::fct_inorder(term)) %>%
    dplyr::select(-rowid, -count)
}

project_tfidf_dtm <- function(df_project, df_term_count, type = "freq") {
  type <- rlang::enquo(type)
  df_project %>%
    dplyr::filter(
      !stringr::str_detect(term, stringr::regex("^[:punct:]")),
      !stringr::str_detect(term, stringr::regex("[A-Z]", ignore_case = TRUE))) %>%
    dplyr::left_join(df_term_count, by = "term") %>%
    dplyr::arrange(rowid, dplyr::desc(count)) %>%
    dplyr::select(-rowid, -count) %>%
    dplyr::mutate(term = forcats::fct_inorder(term)) %>%
    dplyr::select(project_id, term, !!type) %>%
    tidyr::spread(key = "term", value = !!type, fill = 0)
}

token_count <- function(data, term, freq, has_id = TRUE) {
  d <-
    data %>%
    dplyr::group_by(term) %>%
    dplyr::summarise(count = sum(freq), .groups = "drop") %>%
    dplyr::arrange(desc(count))
  if (has_id == TRUE)
    d <-
      d %>%
      tibble::rowid_to_column()
  d
}

term_pca_kclust <- function(data, k) {
  df_token_count =
    data %>%
    token_count(term)
  df_text_token_tfidf =
    data %>%
    project_tfidf()
  df_dtm_freq =
    df_text_token_tfidf %>%
    project_tfidf_dtm(df_token_count, type = "freq")
  df_dtm_tfidf =
    df_text_token_tfidf %>%
    project_tfidf_dtm(df_token_count, type = "tf_idf") %>%
    assertr::verify(dim(.) == dim(df_dtm_freq))
  df_dtm_pca =
    df_dtm_tfidf %>%
    recipes::recipe(project_id ~ .) %>%
    recipes::step_center(recipes::all_predictors()) %>%
    recipes::step_scale(recipes::all_predictors()) %>%
    recipes::step_pca(recipes::all_predictors(), num_comp = 4) %>%
    recipes::prep(training = df_dtm_tfidf, strings_as_factors = FALSE) %>%
    recipes::juice()
  prj_term_kclust =
    df_dtm_pca %>%
    dplyr::select(-project_id) %>%
    stats::kmeans(centers = k)
  df_term_cluster =
    prj_term_kclust %>%
    broom::augment(data = df_dtm_pca)
  list(
    term_tfidf = df_dtm_tfidf,
    cluster = df_term_cluster
  )
}

cluster_top_terms <- function(term_tfidf, cluster, k, top_n) {
  seq_len(k) %>%
    purrr::map_dfr(
      ~ term_tfidf %>%
        dplyr::filter(project_id %in% c(cluster %>%
                                          dplyr::filter(.cluster == .x) %>%
                                          dplyr::pull(project_id))) %>%
        tidyr::gather(term, value, -project_id) %>%
        dplyr::group_by(term) %>%
        dplyr::summarise(value = sum(value), .groups = "drop") %>%
        dplyr::arrange(desc(value)) %>%
        dplyr::filter(value > 0),
      .id = "cluster") %>%
    dplyr::group_by(cluster) %>%
    dplyr::slice_max(value, n = top_n) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(cluster, dplyr::desc(value))
}

plot_cluster_freq_term <- function(data, cluster, k) {
  seq_len(k) %>%
    purrr::map(
      ~ data %>%
        inner_join(cluster %>%
                     transmute(project_id = stringr::str_remove(project_id, "prj_"),
                               .cluster),
                   by = "project_id") %>%
        mutate(.cluster = as.character(.cluster)) %>%
        group_by(.cluster, term) %>%
        summarise(freq = sum(freq), .groups = "drop") %>%
        group_by(.cluster) %>%
        slice_max(freq, n = 30) %>%
        ungroup() %>%
        left_join(df_freq_term_en, by = "term") %>%
        select(.cluster, term = term_en, freq) %>%
        filter(.cluster == .x) %>%
        ggplot(aes(forcats::fct_reorder(term, freq), freq)) +
        geom_bar(stat = "identity", fill = colorspace::qualitative_hcl(5, "Set 3")[.x]) +
        ggtitle(glue::glue("Cluster {.x}")) +
        xlab(NULL) +
        ylab("Frequency") +
        coord_flip() +
        theme_light()
    ) %>%
    purrr::reduce(`+`) +
    plot_layout(nrow = 2)
}
