##############################
# Figure1. Sankey diagram: success, failure (Words * Success)
# project-result-term_frequency_count_unique_en.png
# Figure2. Context (wordcloud) Achievement by contexts
# Figure3. achievement rate - cluster
##############################
library(dplyr)
library(ggplot2)
library(conflicted)
library(patchwork)
library(networkD3)
conflict_prefer("filter", winner = "dplyr")
source(here::here("workflow/03-success_context.R"))
# Fig1 --------------------------------------------------------------------
drake::loadd(list = c("df_freq_term_en",
                      "df_regression_target",
                      "df_freq_term_duplicate"))

d_links <-
  df_freq_term_duplicate %>%
  mutate(group = success) %>%
  mutate_at(vars(success, term_en),
            list(~ as.numeric(as.factor(.)) - 1)) %>%
  mutate(term_en = term_en + 2)
d_nodes <-
  tibble::tibble(
    name = c("success", "failure",
             df_freq_term_duplicate %>% pull(term_en) %>%
               unique() %>%
               sort()))
p_out <-
  sankeyNetwork(
    Links = as.data.frame(d_links),
    Nodes = as.data.frame(d_nodes),
    Source = "term_en",
    Target = "success",
    Value = "freq",
    NodeID = "name",
    fontSize = 10,
    nodeWidth = 50)
# htmlwidgets::saveWidget(p_out, "test.html")
# webshot::webshot("test.html", here::here("figures/Figure_1_topwords_successfailure_count.png"))
# # webshot::webshot("test.html", here::here("figures/Figure_1_topwords_successfailure_count.pdf"))
# unlink("test.html")


# Fig2 --------------------------------------------------------------------
drake::loadd(list = c("clust_num", "df_cluster_import_words"))
fig_wordcluster <- function(cluster, size = 1) {
  df_cluster_import_words %>%
    left_join(df_freq_term_en, by = "term") %>%
    select(cluster, term = term_en, value) %>%
    filter(cluster == {{ cluster }}) %>%
    arrange(desc(value)) %>%
    select(term, value) %>%
    ggwordcloud::ggwordcloud2(color = colorspace::qualitative_hcl(clust_num, "Set 3")[cluster],
                              size = size,
                              shuffle = FALSE) +
    labs(title = recode({{ cluster }},
                        `1` = "Pets",
                        `2` = "Landscape-management",
                        `3` = "Sustainable-use")) +
    theme(plot.title = element_text(hjust = 0.5, size = 22))
}
# size を大きくし過ぎると見切れる
set.seed(123)
p <-
  purrr::map2(
    .x = seq_len(3),
    .y = c(3.8, 1.3, 1.6),
    .f = ~ fig_wordcluster(cluster = .x, size = .y)) %>%
  wrap_plots() +
  plot_layout(ncol = 3)
ggsave(plot = p,
       filename = here::here("figures/Figure_2_project_cluster_wordcloud_en.eps"),
       width = 16,
       height = 7,
       dpi = 600)

# Fig3 --------------------------------------------------------------------
p <-
  df_regression_target %>%
  select(achievement_rate, cluster) %>%
  mutate(cluster = recode(cluster,
                          `1` = "Pets",
                          `2` = "Landscape-management",
                          `3` = "Sustainable-use") %>%
           forcats::fct_relevel("Pets", "Landscape-management", "Sustainable-use")) %>%
  ggplot(aes(x = factor(cluster),
             y = achievement_rate,
             fill = factor(cluster))) +
  geom_violin(draw_quantiles = c(0.5),
              trim = TRUE,
              scale = "width",
              show.legend = FALSE) +
  ggbeeswarm::geom_quasirandom(colour = "black",
                               size = 0.4,
                               method = "quasirandom",
                               alpha = 1.0,
                               show.legend = FALSE) +
  geom_hline(yintercept = 1.0,
             color = "red",
             size = 0.5) +
  scale_fill_manual(values = colorspace::qualitative_hcl(3, "Set 3", alpha = 1.0)) +
  scale_y_continuous(breaks = seq.int(0, 6)) +
  guides(fill = FALSE) +
  labs(
    y = "Achievement rate",
    colour = "cluster"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black")) +
  xlab(NULL)
ggsave(here::here("figures/Figure_3_achieve_cluster.eps"),
       plot = p,
       width = 7,
       height = 5,
       dpi = 600)
