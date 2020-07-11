cluster1project_count <- function(data, project_id) {
  project_id <- rlang::enquo(project_id)
  d_target <-
    data %>%
    dplyr::filter(project_id == !!project_id)
  data %>%
    dplyr::filter(cluster == 1) %>%
    dplyr::filter(dplyr::between(open, d_target$open, d_target$close) |
                    dplyr::between(close, d_target$open, d_target$close)) %>%
    nrow() - 1
}

cluster_project_count <- function(data, cluster) {
  cluster <- rlang::enquo(cluster)
  d <-
    data %>%
    dplyr::filter(cluster == !!cluster)
  d %>%
    dplyr::mutate(overlapped_proj = purrr::map_dbl(
      project_period,
      ~ sum(lubridate::int_overlaps(.x,
                                    d$project_period),
            na.rm = TRUE) - 1))
}
