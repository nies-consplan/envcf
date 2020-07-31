###################################
# Data extraction from Readyfor
###################################
library(rvest)
library(tibble)
library(glue)
library(dplyr)
library(stringr)
library(ensurer)
library(assertr)
source(here::here("R/collect.R"))

if (rlang::is_false(file.exists(here::here("data-raw/readyfor.rds")))) {
  if (rlang::is_false(file.exists(here::here("data-raw/df_readyfor_ecology_description.rds")))) {
    site_url <- "https://readyfor.jp/"
    # 1. readyforのカテゴリ、URLを取得 -------------------------------------------------
    # カテゴリー (tag)
    # nolint start
    x <-
      read_html(glue(site_url, "projects/successful")) %>%
      html_nodes(css = 'body > div.Site-container > div.Page-body > div > div > nav > section:nth-child(3) > div > ul')
    df_readyfor_categories <-
      seq_len(x %>%
                rvest::html_nodes(css = "li.Local-nav-menu__item") %>%
                length()) %>%
      purrr::map_dfr(~
                       dplyr::bind_cols(
                         tibble(
                           category = x %>%
                             rvest::html_nodes(css = glue::glue('li:nth-child({i}) > a.Local-nav-menu__link',
                                                                i = .x)) %>%
                             rvest::html_text(),
                           url = x %>%
                             rvest::html_nodes(css = glue::glue('li:nth-child({.x}) > a.Local-nav-menu__link')) %>%
                             rvest::html_attr("href"),
                           sub_category =     x %>%
                             rvest::html_nodes(css = glue::glue('li:nth-child({.x}) > a.Local-nav-menu__sub-link')) %>%
                             rvest::html_text(),
                           sub_category_url = x %>%
                             rvest::html_nodes(css = glue::glue('li:nth-child({.x}) > a.Local-nav-menu__sub-link')) %>%
                             rvest::html_attr("href")
                         ))) %>%
      tidyr::nest(sub = c(sub_category, sub_category_url)) %>%
      assertr::verify(dim(.) == c(5, 3)) %>%
      assertr::verify(assertr::has_all_names("category", "url", "sub"))
    # nolint end
    df_readyfor_categories <-
      df_readyfor_categories %>%
      tidyr::unnest(cols = sub) %>%
      dplyr::mutate_at(dplyr::vars(tidyselect::contains("url")),
                       .funs = list(~ paste0(gsub("\\/$", "", site_url), .))) %>%
      dplyr::group_by(category, url) %>%
      tidyr::nest()
    # 2. カテゴリー = 環境保護のコンテンツを取得 ---------------------------------------------------
    category_url <-
      df_readyfor_categories %>%
      tidyr::unnest(cols = data) %>%
      dplyr::filter_at(dplyr::vars(tidyselect::contains("category")),
                       dplyr::any_vars(. == "環境保護")) %>%
      dplyr::pull(sub_category_url)
    # https://readyfor.jp/tags/ecology?page=2
    # max pages
    n_max <-
      category_url %>%
      xml2::read_html() %>%
      rvest::html_nodes(css = 'div > div> div > div > div > nav > span:nth-child(8) > a > span > span > span') %>% # nolint
      rvest::html_text(trim = TRUE) %>%
      as.numeric() %>%
      ensurer::ensure(. >= 41)
    if (n_max < 41) {
      df_readyfor_ecology_description <-
        seq_len(n_max) %>%
        purrr::map_dfr(
          ~ glue::glue("{category_url}?page={.}",
                       . = .x) %>%
            collect_description_readyfor()) %>%
        assertr::verify(ncol(.) == 5)
      df_readyfor_ecology_description <-
        df_readyfor_ecology_description %>%
        tidyr::nest(information = c(thumbnail, founder, url)) %>%
        ensurer::ensure(identical(class(.), c("tbl_df", "tbl", "data.frame"))) %>%
        ensurer::ensure(nrow(.) >= 492) %>%
        assertr::verify(assertr::has_all_names("project_id", "title", "information"))
      test_that(
        "readyfor category: ecology", {
          expect_named(
            tidyr::unnest(df_readyfor_ecology_description, cols = information),
            c("project_id", "title", "thumbnail", "founder", "url")
          )
        }
      )
      df_readyfor_ecology_description %>%
        readr::write_rds(here::here("data-raw/df_readyfor_ecology_description.rds"),
                         compress = "xz")
    }
    rm(category_url, n_max, site_url)
  } else {
    df_readyfor_ecology_description <-
      readr::read_rds(here::here("data-raw/df_readyfor_ecology_description.rds"))
  }
  if (rlang::is_false(file.exists(here::here("data-raw/readyfor.rds")))) {
    # プロジェクトの個別ページから取得するデータ ---------------------------------------------------
    project_ids <-
      df_readyfor_ecology_description %>%
      tidyr::unnest(cols = information) %>%
      pull(project_id) %>%
      unique() %>%
      ensurer::ensure(length(.) >= 492)
    slowly_project_summary_readyfor <-
      purrr::slowly(~ project_summary_readyfor(.x, token = config::get("fb_token")),
                    rate = purrr::rate_delay(pause = 3),
                    quiet = FALSE)
    # ~ 45 min.
    # Facebook API 200/hour なので分割して実行する
    readyfor_180 <-
      project_ids[1:180] %>%
      purrr::map(slowly_project_summary_readyfor) %>%
      purrr::reduce(bind_rows)
    readyfor_360 <-
      project_ids[181:360] %>%
      purrr::map(slowly_project_summary_readyfor) %>%
      purrr::reduce(bind_rows)
    readyfor_492 <-
      project_ids[361:length(project_ids)] %>%
      purrr::map(slowly_project_summary_readyfor) %>%
      purrr::reduce(bind_rows)
    readyfor <-
      ls(pattern = "readyfor_[0-9]{3}") %>%
      purrr::map(get) %>%
      ensurer::ensure(length(.) == 3L) %>%
      purrr::reduce(rbind) %>%
      verify(dim(.) == c(492, 14))
    readyfor %>%
      readr::write_rds(here::here("data-raw/readyfor.rds"),
                       compress = "xz")
  }
}
# 1. Prepare datasets --------------------------------------------------------
source(here::here("R/term_translate.R"))
if (rlang::is_false(file.exists(here::here("inst/SlothLib_Stopword_Japanese.txt"))))
    curl::curl_download("http://svn.sourceforge.jp/svnroot/slothlib/CSharp/Version1/SlothLib/NLP/Filter/StopWord/word/Japanese.txt",
                        destfile = here::here("inst/SlothLib_Stopword_Japanese.txt"))

if (rlang::is_false(file.exists("inst/mecab-ipadic-neologd/build/mecab-ipadic-2.7.0-20070801-neologd-20200521/mecab-user-dict-seed.20200521.csv.dic"))) {
  rlang::inform("辞書ファイルが存在しません。README.mdに記載されている手順に従い、辞書ファイルを整備してください")
}

plan_text_tokenize <-
  drake::drake_plan(
    df_readyfor_text =
      readr::read_rds(here::here("data-raw/readyfor.rds")) %>%
      dplyr::filter(success %in% c("success", "failure")) %>%
      dplyr::select(project_id, text) %>%
      verify(dim(.) == c(473, 2)) %>%
      tidyr::unnest(cols = text) %>%
      distinct(project_id, text) %>%
      tidyr::nest(data = c(text)) %>%
      # 複数行に分かれているtext列を一行にまとめる
      dplyr::transmute(project_id,
                       text = purrr::pmap_chr(.,
                                              ~ stringr::str_c(unique(pull(..2, text)), collapse = "\n"))) %>%
      # 数字の置換 (0へ)
      dplyr::mutate(text = stringr::str_replace_all(text,
                                                    stringr::regex("[0-9]{1,99}"),
                                                    "0")) %>%
      # クリーニング
      dplyr::mutate(text = stringr::str_remove_all(text,
                                                   stringr::regex("(http|https)(:\\/\\/[-_.!~*\\'()a-zA-Z0-9;/?:@&=+$,%#]+)")),
                    text = stringr::str_remove_all(text,
                                                   regex("(=|-|ｰ|－|\u2014|\u30fc{2,99})")),
                    text = stringr::str_remove_all(text,
                                                   stringr::regex("\u25ef|\u25cf|\u25cb|\u25ce|\u2605|\u25c6|\u2666|\u25b2|\u25b6|\u25bc|\u25a1|\u25a0"))) %>%
      dplyr::mutate(text_length = stringr::str_length(text)) %>%
      dplyr::select(project_id, text_length, text) %>%
      verify(nrow(.) == 473L),
    df_text_token_raw =
      df_readyfor_text %>%
      group_by(project_id) %>%
      group_modify(
        ~ RcppMeCab::pos(stringr::str_squish(.x$text),
                         user_dic = "inst/mecab-ipadic-neologd/build/mecab-ipadic-2.7.0-20070801-neologd-20200521/mecab-user-dict-seed.20200521.csv.dic",
                         format = "data.frame") %>%
          dplyr::filter(pos %in% c("名詞", "動詞", "形容詞"))
      ) %>%
      ungroup() %>%
      select(project_id, term = token, pos, subtype) %>%
      verify(nrow(.) == 438036L) %>%
      # subtype %in% c("一般", "自立", "固有名詞")
      filter(!subtype %in% c("非自立", "接尾", "数", "特殊", "代名詞", "副詞可能",
                             "接続詞的", "引用文字列", "ナイ形容詞語幹", "動詞非自立的")) %>%
      # 揺れを修正
      mutate(term = stringi::stri_trans_general(term, "nfkc")) %>%
      mutate(term = stringr::str_replace(term, ".+大学$", "大学")) %>%
      mutate(term = stringr::str_replace(term, ".+(小学校|中学校)$", "学校")) %>%
      mutate(term = stringr::str_replace(term, ".+動物園$", "動物園")) %>%
      mutate(term = stringr::str_replace(term, ".+水族館$", "水族館")) %>%
      mutate(term = stringr::str_replace(term, ".+博物館$", "博物館")) %>%
      mutate(term = stringr::str_replace(term, ".+美術館$", "美術館")) %>%
      mutate(term = stringr::str_replace(term, ".+新聞$", "新聞")) %>%
      verify(nrow(.) == 331640L),
    # 活用形を原形に変換する。名詞には活用形がないのでそれ以外の品詞を対象
    term_x =
      df_text_token_raw %>%
      filter(pos %in% c("形容詞", "動詞")) %>%
      pull(term) %>%
      unique() %>%
      ensure(length(.) == 5572L),
    term_y =
      term_x %>%
      purrr::map_chr(~ RMeCab::RMeCabC(.x,
                                       mypref = 1,
                                       dic = "inst/mecab-ipadic-neologd/build/mecab-ipadic-2.7.0-20070801-neologd-20200521/mecab-user-dict-seed.20200521.csv.dic") %>%
                       purrr::flatten_chr() %>%
                       purrr::pluck(1) %>%
                       unname()) %>%
      purrr::set_names(term_x),
    df_text_token_mod =
      df_text_token_raw %>%
      mutate(term = recode(term, !!!term_y)) %>%
      mutate(term = recode(
        # 動物（哺乳類）は犬、猫を除いてカタカナ
        # 魚やカエルは魚類や両生類のように
        term,
        !!!term_noun_convert_list)),
    # 条件2. 全プロジェクトで2回未満の出現頻度の単語は除外
    few_count_words =
      df_text_token_mod %>%
      distinct(project_id, term) %>%
      count(term, sort = TRUE) %>%
      filter(n < 2) %>%
      pull(term) %>%
      sort() %>%
      unique(),
    # 条件3. ストップワードの除去 --------------------------------------------------------------
    stop_words =
      readLines(here::here("inst/SlothLib_Stopword_Japanese.txt")) %>%
      unique() %>%
      ensure(length(.) == 311L),
    ignore_words = c("゙", "゚", "ー", "ｰ",
                     "n", "r", "t", "kg",
                     "Readyfor", "ポスト", "リバースプロジェクト", "大型", "沢山", "活性",
                     "プロジェクト", "皆様", "費用", "コスト", "リターン", "特",
                     "場所", "ない", "団体", "方々", "新しい", "心", "法人", "国立", "園", "緑",
                     "状況", "自分", "多い", "良い", "名前", "商品", "目標",
                     "皆さま", "事業", "オリジナル", "情報", "きっかけ", "社会",
                     "現状", "高い", "少ない", "未来", "資金", "皆さん", "センター",
                     "協会", "手", "状態", "サイト", "セット", "引換", "力", "効果",
                     "中心", "する", "ある", "なる", "できる", "栽",
                     "ー", "機", "ホ", "得", "せき", "唯一", "奇跡", "想い", "最後", "しま",
                     "あい", "アカ", "アク", "あつい", "あたたかい", "暖かい", "温かい",
                     "あて", "あっという間",
                     "あぶない", "アポ", "あま", "アマ", "あまり", "あら", "あらい",
                     "ありがたい", "あり方", "あん", "ある日", "い", "いい", "いき",
                     "いただき", "いた事", "いち", "イチ", "ぃちゃん", "ぃちゃんだけ", "ぃちゃんですが", "ぃちゃんの",
                     "ぃちゃんのお", "ぃちゃんは", "いち早い", "一緒", "いっしょ", "イネー",
                     "いらだち", "イン", "ぅ", "ウズウズ", "うすい", "うま味", "うまみ", "うまい", "うれしい",
                     "ウンザリ", "ぇ", "エ", "ぇがはじまりました", "ぇにできないかと", "ぇや", "ぉ",
                     "おいしい", "おいで", "おおい", "長い", "美味しい", "最初", "広い",
                     "素晴らしい", "おかげ", "優しい", "早い", "嬉しい", "よい", "若い", "悪い",
                     "残り", "まち", "厳しい", "低い", "小さい", "恵み", "楽しみ", "やさしい",
                     "ご覧", "喜び", "選び", "こだわり", "流れ", "通り", "考え",
                     "悩み", "安い", "渡し", "上げ", "宜しい", "心地よい", "明るい",
                     "詳しい", "学び", "狭い", "面白い", "悲しい", "願い", "使い", "使い方",
                     "珍しい", "試し", "柔らかい", "びひろ", "立ち", "生まれ", "甘い",
                     "都合", "残", "好み", "そのもの", "基礎", "古い", "甘み", "辛い", "弱い",
                     "ひどい", "お互い", "幅広い", "さい", "すごい", "スト", "すばらしい", "すばらしさ",
                     "スジャータ・スクール", "スリ", "セカンド", "たくましい", "ただ", "たつ", "たま",
                     "タイプ", "たっぷり", "ちょう", "つながり", "つなぎ", "づがわ", "つけ", "づつ", "つまり",
                     "デゥムリ・スクール", "ティア", "チクシ", "ステムアクティブ", "チャンス", "つま",
                     "テーマ", "チウリ", "タイミング", "チャリティ・ファンラン", "トワンテ",
                     "ニライカナイクラブ", "ニャン", "ネ", "ノラ", "のら", "ベジドレ", "ペア",
                     "ピンプンガジマル", "ブドゥ", "フラフ", "リ・ファッション", "ゃぱん", "ゅもくい",
                     "ゃんだ", "ゃんぴっ", "プロジェクト", "人", "環境", "月", "ない", "皆様", "皆さま",
                     "費用", "イベント", "皆さん", "良い", "きっかけ", "状況", "写真",
                     "新しい", "思い", "想い", "情報", "商品", "金額", "手", "オリジナル", "高い",
                     "届け", "力", "姿", "一つ", "大きい", "中心", "気持ち", "子",
                     "メンバー", "少ない", "場", "強い", "センター", "一般", "形",
                     "部分", "お力", "コース", "サイズ", "自身", "足", "経費", "目", "食",
                     "美しい", "取り組み", "暮らし", "楽しい", "壁", "特徴", "足", "サイズ", "専門",
                     "税", "客", "一般", "小型", "顔",
                     "人々", "最終", "規模", "頭", "わん", "色",
                     "量", "船", "各地", "世の中", "仕組み", "耳", "日常", "お力", "砂",
                     "脚", "ヒト", "店内", "美", "身", "県内", "全力", "遊び", "柄",
                     "福", "草", "柵", "南", "実", "事項", "煙", "条件", "都道府県",
                     "礼状", "お客様", "レター", "価格", "手紙", "ネクストゴール", "送料", "ご存知",
                     "メッセージ", "弊社", "株式会社", "ネット", "ページ", "ヒント", "リスク",
                     "ケース", "ネットワーク", "第一歩", "ベース", "ノウハウ", "データ",
                     "ゴール", "ツアー", "サポーター", "アクション", "メス", "ゲーム",
                     "リターン", "場所", "方々", "自分", "目標", "多い", "クラウドファンディング",
                     "ポストカード", "内容", "目的", "課題", "興味", "原因", "種類", "対象",
                     "次", "周辺", "会", "様子", "お金", "使途", "カード", "気", "数",
                     "あと", "他", "魅力", "間", "価値", "もと", "m", "用途",
                     "上記", "深い", "近い", "通常", "元", "周り", "基本", "外", "無い", "ない",
                     "予算", "最大", "下記", "手作り", "オリジナル", "グッズ",
                     "社団", "事務所", "期間", "理由", "機関", "住所", "ワークショップ", "手数料", "無料", "事例",
                     # "光", "蜜", "食", "味", "声", "香り",
                     "さ", "~", "<", ">", "す", "+", "き", "出", "¥", "|",
                     "で", "な", "え", "ぎ", "あ", "リ", "ろ", "ま", "へ", "ぺ", "む", "ヶ",
                     "め", "クロ", "ヤマ", "姿", "こ", "思い", "青", "シ", "お", "ズ", "ヅ",
                     "に", "の", "わ", "久", "似", "倒", "御", "ぃ",
                     "\u274f", "\u00b0", "\u266a", "\u25fc", "\u21d2", "\u226a", "\u226b",
                     "\u2750", "\u2661", "\u2640", "\u00a9", "\u2715", "\u2776", "\u2777",
                     "\u2778", "\u2779",
                     "かかる", "!」", "あっ", "もつ", "いう", "しれる", ")、", "かける",
                     "...", ")」", "!!", "はじめまして。", "よろしくお願いします", "?」", ")。",
                     "):", "!)", "いたい", "つい", "にも",
                     "A", "C", "in", "B", "CO", "L", "em", "or", "background", "D", "F", "ha", "M",
                     "OK", "URL", "lineheight", "px", "R", "H", "highlight", "lineargradient", "lineheight",
                     "border", "S", "N", "T", "a", "E", "margin", "N", "h", "p", "padding", "s", "to",
                     "de", "for", "kakomu", "left", "No", "of", "Q", "the", "W", "x",
                     "absolute", "and", "double", "gmail.com", "height", "I", "II", "km", "NEXT", "NG",
                     "OPEN", "PM", "position", "relative", "solid", "top", "width", "All", "autoplay",
                     "BOX", "cbb", "dots", "e", "ff", "fontfamily", "function", "G", "hidden", "IN",
                     "J", "K", "Kg", "kw", "kW", "Neuter", "NO", "O", "OB", "OFF", "overflow", "P",
                     "projectslick", "slick", "Trap", "true", "with", "borderradius", "Center",
                     "g", "iframe", "k", "Nothing", "off", "paddingbottom", "Ready", "Return",
                     "times", "UP", "works", "www", "Youth", "ACE", "Adopt", "Ah", "auto", "Ball",
                     "Bio", "borderbottom", "Centre", "cm", "color", "content", "dashed", "day",
                     "etc", "ffff", "fontsize", "Get", "GOOD", "i", "iframecontent", "How", "l",
                     "ne.jp", "pdf", "sansserif", "serif", "td", "The", "THE", "times new roman",
                     "V", "ver", "webkittransform", "X", "Y",
                     "ネクスト", "こんにちは。", "問い合せ", "...。", "ござる", "ここに",
                     "!(", "......", "(※", "おら", "いらっしゃい", "ゆくゆく",
                     "*************************", ")(", "やら", "!!」", "!?", "!】", ");",
                     "helvetica", "_________________________",
                     ";;", ":。", ":植物", "!!』",
                     "!!】", "!)。", "!」、「", "!」(", "?!」", "?)", "...、",
                     "...?」", "......」", "...。」", "..。", ".)", ".」", ".S",
                     "\"\";", "(『", "(^", "(+", "(~", ")、『", ")!", ")」。",
                     ")*", "*。", "※(", "^)", "^^", "<<<", ">>>", "~!", "~(", "~』",
                     ":「", "\u25aa", "))", "<<",
                     "♪(", "Z", "あなたのために", "あったらいいな", "ある意味", "あれから", "いつく",
                     "いつものように", "お世話になりました", "なら", "ちる", "うさ",
                     "おかげさま", "目標達成", "transparent", "!!!", "..", "...」", ".「",
                     "~」", "TOP", "My", ".com", ">>", "(¥", "?」「", "......。",
                     "$('.", "~)", "$(", "	$('.", "	$('.",
                     "いかが", "いらう", "おい", "実は私は",
                     "寄附", "寄付", "目標金額", "ゆり", "寄附金",
                     "田中さん", "はなちゃん", "クロちゃん",
                     #"\【",
                     ":〒", ":(", ":¥", "?!", "??", "?「", "?』", "	...!",
                     "...)", ".jp", ".s", "。)", "').", '"。', "	()", "	({",
                     "(「", "(@", "(*", "(※)", ")〜", "):¥", ")”", "	)「",
                     ")」「", "	)『", ")』", ")】", ")/", ")&", "	)※", ")→",
                     "/*", "*/",
                     "**", "*「", "	』(", "});",
                     "paddingtop", "ゆり", "セル", "ゆたか", "もち",
                     "支援", "活動", "いる", "支援者", "あかり",
                     "お礼") %>%
      unique() %>%
      ensure(length(.) == 750L),
    df_text_token =
      df_text_token_mod %>%
      filter(str_detect(term, "^[[:punct:]]$", negate = TRUE)) %>%
      filter(stringr::str_detect(term, "0", negate = TRUE)) %>%
      filter(!term %in% stop_words) %>%
      filter(!term %in% ignore_words) %>%
      filter(!term %in% few_count_words) %>%
      filter(str_detect(term, "^(!|」|「)", negate = TRUE)) %>%
      filter(str_detect(term, "\u200b", negate = TRUE)) %>%
      filter(str_detect(term, "^[[:punct:]]|[[:punct:]]$", negate = TRUE)) %>%
      filter(str_detect(term, "\\A(\u30fb|\u30fc|\u3041|\u309a\u309a)", negate = TRUE)) %>%
      tidyr::separate_rows(term, sep = "_") %>%
      group_by(project_id, term, pos, subtype) %>%
      summarise(freq = n(), .groups = "drop") %>%
      verify(nrow(.) == 105961L) %>%
      filter(pos == "名詞") %>%
      verify(nrow(.) == 77820),
    invisible(
      df_text_token %>%
        pull(project_id) %>%
        n_distinct() %>%
        ensure(. == 473L)),
    tidy_eval = FALSE)
drake::make(plan_text_tokenize)
# drake::loadd(list = c("df_readyfor_text", "df_text_token"))

plan_setup_data <-
  drake::drake_plan(
    readyfor =
      readr::read_rds(here::here("data-raw/readyfor.rds")),
    df_target =
      readyfor %>%
      dplyr::filter(success %in% c("success", "failure")) %>%
      dplyr::mutate(project_period = lubridate::interval(open, close)) %>%
      assertr::verify(dim(.) == c(473, 15)),
    tgt_prj_ids =
      df_target %>%
      dplyr::pull(project_id) %>%
      unique() %>%
      sort() %>%
      ensurer::ensure(length(.) == 473L),
    df_readyfor_ecology_description =
      readr::read_rds(here::here("data-raw/df_readyfor_ecology_description.rds")) %>%
      assertr::verify(dim(.) == c(492, 3)),
    df_freq_term_en =
      tibble::tribble(
        ~term, ~term_en,
        "猫", "cat",
        "保護", "protection",
        "私たち", "we",
        "犬", "dog",
        "動物", "animal",
        "子ども", "child",
        "日本", "Japan",
        "必要", "necessity",
        "自然", "nature",
        "地域", "area",
        "森林", "forest",
        "農業", "agriculture",
        "命", "life",
        "海", "sea",
        "協力", "cooperation",
        "学校", "school",
        "自治体", "administration",
        "開催", "open",
        "ゴミ", "waste",
        "大学", "university",
        "世界", "world",
        "応援", "cheer",
        "参加", "participation",
        "予定", "plan",
        "殺処分", "euthanasia",
        "使用", "use",
        "山", "mountain",
        "挑戦", "challenge",
        "調査", "investigation",
        "感謝", "gratitude",
        "制作", "production",
        "体験", "experience",
        "農家", "farmer",
        "購入", "purchase",
        "野菜", "vegetable",
        "石鹸", "soap",
        "現地", "onsite",
        "利用", "usage",
        "地球", "earth",
        "里親", "foster",
        "手術", "surgery",
        "飼い主", "pet_owner",
        "野良猫", "stray_cat",
        "ペット", "pet",
        "不妊", "sterilization",
        "譲渡", "transfer",
        "治療", "medical_treatment",
        "シェルター", "shelter",
        "施設", "institution",
        "宝石", "jewel",
        "保健所", "health_center",
        "家族", "family",
        "訓練", "training",
        "保護犬", "rescued_dog",
        "猫カフェ", "cat_cafe",
        "イルカ", "dolphin",
        "カラス", "crow",
        "幸せ", "happiness",
        "ドッグラン", "dog_run",
        "ボランティア", "volunteer",
        "避妊", "contraception",
        "カフェ", "cafe",
        "病気", "disease",
        "竹", "bamboo",
        "樹木", "tree",
        "ウミガメ", "sea_turtle",
        "ホタル", "fire_fly",
        "学生", "student",
        "鳥類", "bird",
        "海岸", "seashore",
        "砂浜", "beach",
        "燃料", "fuel",
        "生物", "living_thing",
        "里山", "cultural_landscape",
        "栽培", "cultivation",
        "河川", "river",
        "革", "leather",
        "ブルーベリー", "blueberry",
        "ベルト", "belt",
        "シカ", "deer",
        "ペレット", "pellet",
        "バックル", "buckle",
        "ハチミツ", "honey",
        "銀", "silver",
        "リンゴ", "apple",
        "髪", "hair",
        "成分", "ingredient",
        "ココナッツ", "coconut",
        "ミツバチ", "bee",
        "肌", "skin",
        "花", "flower",
        "化粧品", "cosmetics",
        "有機", "organic_production",
        "オリーブ", "olive",
        "農薬", "herbicide",
        "製品", "product",
        "材料", "material",
        "ミネラル", "mineral",
        "モロッコ", "Morocco",
        "雪", "snow",
        "ハーブ", "herb",
        "販売", "sale",
        "大豆", "soybean",
        "加工", "manufacturing") %>%
      assertr::verify(nrow(.) == 104L))
# drake::make(plan_setup_data, packages = "magrittr")
