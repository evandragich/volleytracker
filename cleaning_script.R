library(tidyverse)
library(glue)
library(rvest)
library(janitor)

# take 2
# https://library.ndsu.edu/ir/bitstream/handle/10365/25890/Forecasting%20Point%20Spread%20for%20Women’s%20Volleyball.pdf?sequence=1&isAllowed=y




get_set_data <- function(n, url) {

  # https://www.dataquest.io/blog/web-scraping-in-r-rvest/
  x <- read_html(url) %>%
    html_element("body") %>%
    html_element("form") %>%
    html_element("main") %>%
    html_element("article") %>%
    html_element(".sidearm-responsive-tabs.main-tabs") %>%
    html_element("#play-by-play") %>%
    html_element(".sidearm-responsive-tabs") %>%
    html_elements(glue("#set-{n}")) %>%
    html_element("table") %>%
    html_table()

  if (length(x) == 0) {
    NULL
  } else {
    x <- x[[1]] %>%
      clean_names() %>%
      filter(score != "")

    # https://stackoverflow.com/questions/48211229/reorder-vector-in-r-according-to-index-vector
    temp <- colnames(x)
    index <- c(1, 2, 3, 4, 6, 5, 7, 9, 8)
    colnames(x) <- temp[order(index)]

    x %>%
      mutate(uw_serve = if_else(serve_team == "UW",
        1,
        0
      )) %>%
      mutate(opp_serve = if_else(serve_team != "UW",
        1,
        0
      )) %>%
      rename(uw_score = uw_play_description) %>%
      rename(opp_score = ends_with("_play_description")) %>%
      select(-c(scoring_team_indicator, visiting_team_score, scoring_team_logo, home_team_score)) %>%
      mutate(
        uw_score = as.numeric(uw_score),
        opp_score = as.numeric(opp_score),
        point_diff = uw_score - opp_score,
        uw_point = if_else(point_diff > lag(point_diff, default = 0), TRUE, FALSE),
        uw_so_suc = if_else((opp_serve == 1 & uw_point), 1, 0),
        uw_ps_suc = if_else((uw_serve == 1 & uw_point), 1, 0),
        uw_so_cum = cumsum(uw_so_suc),
        uw_ps_cum = cumsum(uw_ps_suc),
        uw_so_att = cumsum(opp_serve),
        uw_ps_att = cumsum(uw_serve)
      ) %>%
      mutate(
        uw_so_pct = if_else(uw_so_att != 0,
          uw_so_cum / uw_so_att,
          0
        ),
        uw_ps_pct = if_else(uw_ps_att != 0,
          uw_ps_cum / uw_ps_att,
          0
        )
      ) %>%
      rowwise() %>%
      mutate(
        uw_kill = if_else(str_detect(play_description, "Kill by") & uw_point, 1, 0),
        opp_kill = if_else(str_detect(play_description, "Kill by") & !(uw_point), 1, 0),
        opp_atk_err = if_else(str_detect(play_description, "Attack error by") & uw_point, 1, 0),
        uw_atk_err = if_else(str_detect(play_description, "Attack error by") & !(uw_point), 1, 0),
        uw_blk = if_else(str_detect(play_description, "block by") & uw_point, 1, 0),
        opp_blk = if_else(str_detect(play_description, "block by") & !(uw_point), 1, 0),
        opp_blk_err = if_else(str_detect(play_description, "Block error") & uw_point, 1, 0),
        uw_blk_err = if_else(str_detect(play_description, "Block error") & !(uw_point), 1, 0),
        uw_srv_ace = if_else(str_detect(play_description, "Service ace") & uw_point, 1, 0),
        opp_srv_ace = if_else(str_detect(play_description, "Service ace") & !(uw_point), 1, 0),
        opp_srv_err = if_else(str_detect(play_description, "Service err") & uw_point, 1, 0),
        uw_srv_err = if_else(str_detect(play_description, "Service err") & !(uw_point), 1, 0)
      ) %>%
      ungroup() %>%
      mutate(across(c(uw_kill:uw_srv_err),
        cumsum,
        .names = "cum_{.col}"
      )) %>%
      mutate(
        kill_diff = cum_uw_kill - cum_opp_kill,
        blk_diff = cum_uw_blk - cum_opp_blk,
        blk_err_diff = cum_uw_blk_err - cum_opp_blk_err,
        srv_ace_diff = cum_uw_srv_ace - cum_opp_srv_ace,
        srv_err_diff = cum_uw_srv_err - cum_opp_srv_err
      ) %>%
      select(
        c(point_diff, uw_so_pct, uw_ps_pct, kill_diff, blk_diff, blk_err_diff, srv_ace_diff, srv_err_diff, uw_score, opp_score)
      )
  }
}


get_match_data <- function(url) {
  x <- map_dfr(c(1:5),
               ~ get_set_data(
                 n = .x,
                 url = url
               ),
               .id = "set"
  ) %>%
    group_by(set) %>%
    mutate(uw_set_win = if_else(max(uw_score) > max(opp_score), 1, 0)) %>%
    rowwise() %>%
    mutate(
           pts_left = case_when( 
             (!(set == 5) & (uw_score <= 23 | opp_score <= 23)) ~ (25 - max(uw_score, opp_score)),
             set == 5 & (uw_score <= 13 | opp_score <= 13) ~ (15 - max(uw_score, opp_score)),
             abs(uw_score - opp_score) == 1     ~ 1,
             uw_score == opp_score              ~ 2
           )
    )
  
  x
}
  
urls <- list(
  "https://gohuskies.com/sports/womens-volleyball/stats/2020/arizona-state/boxscore/19499",
  "https://gohuskies.com/sports/womens-volleyball/stats/2020/arizona-state/boxscore/19500",
  "https://gohuskies.com/sports/womens-volleyball/stats/2020/arizona/boxscore/19493",
  "https://gohuskies.com/sports/womens-volleyball/stats/2020/arizona/boxscore/19501",
  "https://gohuskies.com/sports/womens-volleyball/stats/2020/oregon-state/boxscore/19502",
  "https://gohuskies.com/sports/womens-volleyball/stats/2020/oregon-state/boxscore/19503",
  "https://gohuskies.com/sports/womens-volleyball/stats/2020/ucla/boxscore/19504",
  "https://gohuskies.com/sports/womens-volleyball/stats/2020/ucla/boxscore/19495",
  "https://gohuskies.com/sports/womens-volleyball/stats/2020/colorado/boxscore/19496",
  "https://gohuskies.com/sports/womens-volleyball/stats/2020/colorado/boxscore/19497",
  "https://gohuskies.com/sports/womens-volleyball/stats/2020/utah/boxscore/19505",
  "https://gohuskies.com/sports/womens-volleyball/stats/2020/utah/boxscore/19506",
  "https://gohuskies.com/sports/womens-volleyball/stats/2020/oregon/boxscore/19494",
  "https://gohuskies.com/sports/womens-volleyball/stats/2020/oregon/boxscore/19492",
  "https://gohuskies.com/sports/womens-volleyball/stats/2020/usc/boxscore/19507",
  "https://gohuskies.com/sports/womens-volleyball/stats/2020/usc/boxscore/19508",
  "https://gohuskies.com/sports/womens-volleyball/stats/2020/stanford/boxscore/19511",
  "https://gohuskies.com/sports/womens-volleyball/stats/2020/stanford/boxscore/19512",
  "https://gohuskies.com/sports/womens-volleyball/stats/2020/california/boxscore/19513",
  "https://gohuskies.com/sports/womens-volleyball/stats/2020/california/boxscore/19514",
  "https://gohuskies.com/sports/womens-volleyball/stats/2020/dayton/boxscore/19846",
  "https://gohuskies.com/sports/womens-volleyball/stats/2020/louisville/boxscore/19848",
  "https://gohuskies.com/sports/womens-volleyball/stats/2020/pittsburgh/boxscore/19854",
  "https://gohuskies.com/sports/womens-volleyball/stats/2020/kentucky/boxscore/19858"
)


  
  