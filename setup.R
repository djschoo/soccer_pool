matches = dplyr::tribble(~match_id, ~team1, ~team2, ~score1, ~score2,
  # Round Robin
  1, 	   'Turkey',           	 'Italy',           	0, 	3,
  2, 	   'Wales',           	 'Switzerland',      	1, 	1,
  3, 	   'Denmark',            'Finland',          	0, 	1,
  4, 	   'Belgium',            'Russia',           	3, 	0,
  5, 	   'England',            'Croatia',          	1, 	0,
  6, 	   'Austria',            'North Macedonia',  	3, 	1,
  7, 	   'Netherlands',        'Ukraine',          	3, 	2,
  8, 	   'Scotland',           'Czech Republic',   	0, 	2,
  9, 	   'Poland',           	 'Slovakia',         	1, 	2,
  10, 	 'Spain',           	 'Sweden',           	0, 	0,
  11, 	 'Hungary',            'Portugal',         	0, 	3,
  12, 	 'France',           	 'Germany',          	1, 	0,
  13, 	 'Finland',            'Russia',           	0, 	1,
  14, 	 'Turkey',           	 'Wales',           	0, 	2,
  15, 	 'Italy',           	 'Switzerland',      	3, 	0,
  16, 	 'Ukraine',            'North Macedonia',  	2, 	1,
  17, 	 'Denmark',            'Belgium',          	1,  2,
  18, 	 'Netherlands',        'Austria',          	2, 	0,
  19, 	 'Sweden',           	 'Slovakia',          1, 	0,
  20, 	 'Croatia',            'Czech Republic',    1, 	1,
  21, 	 'England',            'Scotland',          0, 	0,
  22, 	 'Hungary',            'France',           	1, 	1,
  23, 	 'Portugal',           'Germany',           2, 	4,
  24, 	 'Spain',           	 'Poland',           	1, 	1,
  25, 	 'Italy',           	 'Wales',           	1, 	0,
  26, 	 'Switzerland',        'Turkey',           	3, 	1,
  27, 	 'Ukraine',            'Austria',           0,  1,
  28, 	 'North Macedonia',    'Netherlands',      	0, 	3,
  29, 	 'Finland',            'Belgium',          	0,  2,
  30, 	 'Russia',           	 'Denmark',          	1, 	4,
  31, 	 'Czech Republic',     'England',          	0, 	1,
  32, 	 'Croatia',            'Scotland',         	3, 	1,
  33, 	 'Sweden',           	 'Poland',            2,  3,
  34, 	 'Slovakia',           'Spain',           	0,  5,
  35, 	 'Germany',            'Hungary',           2,  2,
  36, 	 'Portugal',           'France',           	2,  2,

  # Round of 16
  38,    'Wales',              'Denmark',           0,  4,
  37,    'Italy',              'Austria',           2,  1,
  40,    'Netherlands',        'Czech Republic',    0,  2,
  39,    'Belgium',            'Portugal',          1,  0,
  42,    'Croatia',            'Spain',             3,  5,
  41,    'France',             'Switzerland',       3,  4,
  44,    'England',            'Germany',           2,  0,
  43,    'Sweden',             'Ukraine',           1,  2,

  # Quarter Finals
  46,    'Switzerland',        'Spain',             1,  2,
  45,    'Belgium',            'Italy',             1,  2,
  48,    'Czech Republic',     'Denmark',           1,  2,
  47,    'Ukraine',            "England",           0,  4,

  # Semi Finals
  49,    "Italy",              "Spain",             2,  1,
  50,    "England",            "Denmark",           2,  1,

  # Final
  51,    "Italy",              "England",           2,  1
)

players = tribble(~player_id, ~first_name, ~last_name, ~nickname, ~short_name,
  01L, 'R.', 'P.', 'R. "Pretty Good at Soccer Predicting"', 'R. P.',
  02L, 'T,', 'M.', '"Red Card or Ten Minute" Major', 'T. M.',
  03L, 'T.', 'B.', 'T. "Bulge in the Old Onion Bag"', 'T. B.',
  04L, 'T.', 'H.', 'T. "Liverpool versus Manc"-hester', 'T. H.',
  05L, 'C.', 'C.', 'C. "Costellinho"', 'C. C.',
  06L, 'A.', 'A.', '"Forza It"-alia', 'A. A.',
  07L, 'M.', 'G.', '"Thia"-Go', 'M. G.',
  08L, 'B.', 'J.', 'Fabien "Bart"-hez', 'B. J.',
  09L, 'E.', 'L.', '"Exekutiv Direktoren"', 'E. L.',
  10L, 'T.', 'W.', 'Tie, "Win or Lose"', 'T. W.',
  11L, 'S.', 'R.', '"EU-RO"use', 'S. R.'
)

rounds = tibble(
  round = c(rep(1,36), rep(2, 8), rep(3, 4), rep(4, 2), 5),
  points_available = sapply(round, function(x) switch(x, 1, 1, 2, 5, 10))) %>%
  mutate(match_id = row_number(), .before=1) %>%
  mutate_if(is.numeric, as.integer)

flags = c("Spain" = "es", "Netherlands" = "nl", "Germany" = "de")

matches = rounds %>%
  inner_join(matches) %>%
  mutate_if(is.numeric, as.integer) %>%
  mutate(winner = case_when(is.na(score1) ~ NA_character_, T ~ case_when(score1 > score2 ~ team1, score1 == score2 ~ "tie", T ~ team2))) %>%
  mutate(loser = case_when(is.na(score1) ~ NA_character_, T ~ case_when(score1 > score2 ~ team2, score1 == score2 ~ "tie", T ~ team1))) %>%
  select(round, match_id, points_available, team1, team2, score1, score2, winner, loser) %>%
  arrange(match_id)

teams = teams %>%
  filter(team %in% c(matches$team1, matches$team2)) %>%
  mutate(team_id = row_number())

stopifnot(length(unique(matches$match_id - lag(matches$match_id))) == 2) # every match should increment by one (except for first row)

stopifnot(c(matches$team1, matches$team2) %>% unique() %>% match(teams$team) %>% is.na() %>% sum() == 0) # error if NA for any team ID