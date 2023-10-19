# uniquify a single staffer
uniquify_staffer_ids("D15248")

# uniquify a list of staffers
uniquify_staffer_ids(sample(staffer_dictionary$chrw_id, 20))

# uniquify a table, create a new column with the Roth Lab IDs
uniquify_staffer_ids(
  tibble(
    hehe = staffer_dictionary %>% pull(chrw_id) %>% sample(15),
    fake_col = sample(letters, 15, replace = TRUE)
  ),
  ID_col = hehe,
  out_col_name = hahalol
)

# the overwrite case - uniquify staffer IDs in a table, but ovewrite 
# the given ID column
uniquify_staffer_ids(
  tibble(
    lol = staffer_dictionary %>% pull(chrw_id) %>% sample(15),
    fake_col = sample(letters, 15, replace = TRUE)
  ),
  ID_col = lol
)
