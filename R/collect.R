#' Collect cloud found web services data
#'
#' @param url page url (`character`)
#' @param project_id project id as thunmbanil parameters
#' `https://readyfor.jp/s3/readyfor-img/project_images/<project_id>`
#' @name collect-cloudfound-data
NULL

#' @describeIn collect-cloudfound-data readyfor
#' @inheritParams collect-cloudfound-data
#' @examples
#' \dontrun{
#' collect_description_readyfor("https://readyfor.jp/tags/ecology?page=2")
#' }
#' @export
collect_description_readyfor <- function(url = NULL) {
  xml_docs <-
    url %>%
    xml2::read_html()
  # nolint start
  # collect: title, thumbnail, founder, type, url
  df_res <-
    tibble::tibble(
      title = xml_docs %>%
        rvest::html_nodes(css = 'article > a > div.Entry__body > h3') %>%
        rvest::html_text(trim = TRUE),
      thumbnail = xml_docs %>%
        rvest::html_nodes(css = 'article > a > div.Entry__img > div > figure > img') %>%
        rvest::html_attr("src"),
      founder = xml_docs %>%
        rvest::html_nodes(
          css = 'div.Entry__body > div.Entry__info-founder.hidden-while-processing > span.Entry__info-founder-name.should-be-abbriviate') %>%
        rvest::html_text(trim = TRUE),
      url = xml_docs %>%
        rvest::html_nodes(css = 'div.Grid.is-grid-s.tag > article > a') %>%
        rvest::html_attr("href") %>%
        paste0("https://readyfor.jp", .),
      project_id =
        thumbnail %>%
        gsub("https://readyfor.jp/s3/readyfor-img/project_images/", "", .) %>%
        gsub("/large.+", "", .))
  # nolint end
  df_res %>%
    dplyr::select(project_id, dplyr::everything())
}

get_json_text <- function(project_id, type = c("overview", "announcements", "comments")) {
  rlang::arg_match(type)
  type <- dplyr::recode(type, `overview` = "")
  url <-
    df_readyfor_ecology_description %>%
    dplyr::filter(
      project_id == !! rlang::enquo(project_id)) %>%
    tidyr::unnest(cols = information) %>%
    dplyr::pull(url)
  glue::glue("{url}/{type}") %>%
    xml2::read_html() %>%
    rvest::html_nodes(css = "script.js-react-on-rails-component") %>%
    rvest::html_text() %>%
    stringr::str_subset("backingStatement") %>%
    jsonlite::fromJSON()
}

readyfor_description <- function(x) {
  prj_status =
    if (lubridate::now() > lubridate::as_datetime(x$project$expiredAt, tz = "Asia/Tokyo")) {
      if (x$project$amount > x$project$goalPrice) {
        "success"
      } else {
        "failure"
      }
    } else {
      "progress"
    }
  df_meta <-
    tibble::tibble(
      tags          = list(x$project$tags$name),
      success       = prj_status,
      open          = x$project$publishedAt,
      close         = x$project$expiredAt,
      amount_raised = x$project$amount,
      condition     = list(goal     = x$project$goalPrice,
                           backers  = x$project$purchasesCount,
                           days     = x$project$expireAfterDays,
                           likes    = x$project$likedCount))
  df_meta %>%
    dplyr::mutate_at(dplyr::vars(open, close),
                     .funs = list(~ lubridate::as_datetime(., tz = "Asia/Tokyo"))) %>%
    dplyr::mutate_if(is.character,
                     list(~stringi::stri_trans_general(., id = "nfkc"))) %>%
    tidyr::nest(metadata = c(tags, condition))
}

readyfor_announements <- function(x) {
  if (identical(x$announcementSummaries, list())) {
    df_announce <- tibble::tibble(
      date = NA,
      title = NA_character_)
  } else {
    df_announce <-
      x$announcementSummaries %>%
      select(date, title) %>%
      mutate(title = stringr::str_squish(title))
  }
  df_announce %>%
    dplyr::mutate(date = lubridate::parse_date_time(date, "Ymd H!M!", tz = "Asia/Tokyo")) %>%
    dplyr::arrange(date) %>%
    tibble::as_tibble()
}

readyfor_comments <- function(x) {
  if (identical(x$comments, list())) {
    df_comment <-
      tibble::tibble(
        user_url = NA_character_,
        user_name = NA_character_,
        text = NA_character_,
        date = NA)
  } else {
    df_comment <-
      x$comments %>%
      select(user,
             text,
             date = commentedAt) %>%
      transmute(user_url = user %>% pull(userUrl),
                user_name = user %>% pull(name),
                text,
                date) %>%
      mutate_at(vars(text, user_name), stringr::str_squish)
  }
  df_comment %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    dplyr::arrange(date) %>%
    tibble::as_tibble()
}
readyfor_image <- function(url = NULL) {
  x <-
    url %>%
    xml2::read_html()
  unique(c(
    x %>%
      rvest::html_nodes(css = 'section.Tab__content') %>%
      rvest::html_nodes(xpath = "//*/img") %>%
      rvest::html_attr("src") %>%
      stringr::str_subset("ckeditor_assets/.+content_")
  ))
}

readyfor_text <- function(x) {
  tibble::tibble(
    text = x$projectDescription %>%
      xml2::read_html() %>%
      rvest::html_text(trim = TRUE) %>%
      stringr::str_remove("^プロジェクト概要") %>%
      stringr::str_split("\n{1,}", simplify = TRUE) %>%
      stringr::str_squish() %>%
      purrr::keep(~ nchar(.) >= 1) %>%
      stringr::str_subset("もっとみる", negate = TRUE)
  ) %>%
    dplyr::mutate(line = seq_len(nrow(.))) %>%
    dplyr::mutate_if(is.character,
                     list(~ stringi::stri_trans_general(stringr::str_squish(.),
                                                        id = "nfkc")))
}

readyfor_return <- function(x) {
  d <-
    x$rewardsData %>%
    tibble::as_tibble() %>%
    dplyr::select(title, price, description, supportedCount)

  if (is.data.frame(x$rewardsData$image)) {
    images <-
      seq(nrow(x$rewardsData)) %>%
      purrr::map_chr(function(.x) {
        check <- is.na(x$rewardsData$image[.x, 1])
        if (check == TRUE) {
          NA_character_
        } else {
          x$rewardsData$image[.x, ] %>% purrr::pluck("src")
        }
      })
  } else {
    images <-
      x$rewardsData$image %>%
      purrr::map_chr(function(.x) {
        check <- is.na(.x)
        if (check == TRUE) {
          NA_character_
        } else {
          x$rewardsData$image[.x, ] %>% purrr::pluck("src")
        }
      })
  }
  d %>%
    dplyr::mutate(title     = dplyr::na_if(as.character(title), ""),
                  thumbnail = images) %>%
    dplyr::select(items = title,
                  thumbnail,
                  price,
                  returns = description,
                  donors = supportedCount)
}

#' @describeIn collect-cloudfound-data Collect project summary that include description and return contents.
#' @inheritParams collect-cloudfound-data
#' @examples
#' \dontrun{
#' project_summary_readyfor(
#'   project_id = df_readyfor_ecology_description$project_id[42])
#' }
#' @export
project_summary_readyfor <- function(project_id, ...) {
  d_overview <-
    get_json_text(project_id = !! rlang::enquo(project_id),
                  type = "overview")
  df_tgt_prj <-
    tibble::tibble(project_id = !! rlang::enquo(project_id))
  df_tgt_prj_desc <-
    readyfor_description(d_overview)
  df_tgt_prj_type <-
    readyfor_type(d_overview) %>%
    tidyr::nest(type = c(found_model, project_type))
  df_tgt_founder <-
    readyfor_founder(d_overview) %>%
    tidyr::nest(founder = c(founder_page, founder_name, description, project_count))
  df_tgt_prj_return <-
    readyfor_return(d_overview) %>%
    tidyr::nest(return = c(items, thumbnail, price, returns, donors))
  df_tgt_prj_text <-
    readyfor_text(d_overview) %>%
    tidyr::nest(text = c(text, line))
  df_tgt_prj_media <-
    readyfor_media(d_overview) %>%
    tidyr::nest(media = c(images, movies))
  df_tgt_prj_announce <-
    get_json_text(project_id = !! rlang::enquo(project_id),
                  type = "announcements") %>%
    readyfor_announements() %>%
    tidyr::nest(announce = c(date, title))
  df_tgt_prj_comment <-
    get_json_text(project_id = !! rlang::enquo(project_id),
                  type = "comments") %>%
    readyfor_comments() %>%
    tidyr::nest(comment = c(user_url, user_name, text, date))
  df_tgt_prj_sns <-
    readyfor_sns_share(url = glue::glue("https://readyfor.jp",
                                        d_overview$projectUrl),
                       ...) %>%
    tidyr::nest(sns_share = c(twitter, hatena, facebook))
  bind_cols(
    df_tgt_prj,
    df_tgt_prj_desc,
    df_tgt_founder,
    df_tgt_prj_type,
    df_tgt_prj_sns,
    df_tgt_prj_return,
    df_tgt_prj_announce,
    df_tgt_prj_comment,
    df_tgt_prj_text,
    df_tgt_prj_media)
}

readyfor_sns_share <- function(url, token = NULL) {
  tibble::tibble(
    twitter = count_twitter_share(url),
    hatena = count_hatena_bookmark(url)) %>%
    dplyr::bind_cols(
      tidyr::nest(count_facebook_share(url, token),
                  facebook = c(reaction_count, comment_count, share_count, comment_plugin_count))
    )
}

readyfor_founder <- function(x) {
  tibble::tibble(
    founder_page =
      x$project$user$userUrl,
    founder_name =
      x$project$user$name,
    description =
      x$projectUserDescription,
    project_count =
      xml2::read_html(x$project$user$userUrl) %>%
      rvest::html_nodes(css = "body > div.Site-container > div.Profile-container > ul > li.active") %>%
      rvest::html_text() %>%
      stringr::str_extract("[0-9]{1,}") %>%
      as.numeric()) %>%
    dplyr::mutate_all(.funs = list(~ stringr::str_squish(.))) %>%
    dplyr::mutate(project_count = as.numeric(project_count))
}

readyfor_type <- function(x) {
  tibble::tibble(
    found_model  = x$project$fundingModel,
    project_type = x$project$projectType)
}

readyfor_media <- function(x) {
  xml_docs <-
    x$projectDescription %>%
    xml2::read_html()
  tibble::tibble(
    images =
      xml_docs %>%
      rvest::html_nodes("img") %>%
      length(),
    movies =
      xml_docs %>%
      rvest::html_nodes(css = "div") %>%
      rvest::html_attr("data-oembed-url") %>%
      na.omit() %>%
      unique() %>%
      length()
  )
}

count_twitter_share <- function(url) {
  x <-
    glue::glue("https://ceron.jp/url/",
               stringr::str_remove(url, "https://")) %>%
    httr::GET()
  if (httr::status_code(x) == 404L)
    return(0)
  else if (httr::status_code(x) == 200L)
    x %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = '//meta[@name="description"]') %>%
    rvest::html_attr(name = "content") %>%
    stringr::str_extract("[0-9]{1,}件のコメント") %>%
    stringr::str_remove("件のコメント") %>%
    as.numeric()
}

count_hatena_bookmark <- function(url) {
  x <-
    glue::glue("https://b.hatena.ne.jp/entry/s/",
               stringr::str_remove(url, "https://")) %>%
    httr::GET()
  if (httr::status_code(x) == 404L)
    return(0)
  else if (httr::status_code(x) == 200L)
    x %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = '//html') %>%
    rvest::html_attr(name = "data-bookmark-count") %>%
    as.numeric()
}

count_facebook_share <- function(url, token = NULL) {
  r <-
    httr::content(
      httr::GET(
        glue::glue("https://graph.facebook.com/?id={url}&fields=engagement&access_token={token}") # nolint
      ))
  dplyr::select(purrr::flatten_dfr(r), -id)
}
