TweakModel <- function(model_sheet,
                       region_pairs,
                       region_weight = 0.25,
                       national_weight = 0.25,
                       update_weight = 100,
                       update_size = 0.05)
{
  #Creates a random swing for each state.
  national_mod <- rnorm(1)*update_size
  national_mod_weight <- update_weight*national_weight
  update_sheet <- model_sheet %>%
    mutate(Bonus = update_size*rnorm(1), Penalty = -Bonus,Update_weight = update_weight)
  #We apply regional effects and a common national effect. "Unrelated state" effects are skipped as this is equivalent to a pooled national effect, which is provided directly.
  for(i in 1:nrow(model_sheet))
  {
    state <- model_sheet$State[[i]]
    adjacent_states <- unique(unlist(region_pairs %>% filter(State1 == state | State2 == state)))
    baseline_democratic_share <- update_sheet$Share.Democratic[[i]]
    baseline_republican_share <- update_sheet$Share.Republican[[i]]
    state_update <- update_sheet %>%
      filter(State == state) %>%
      mutate(Share.Democratic = baseline_democratic_share + Bonus,
             Share.Republican = baseline_republican_share + Penalty,
             State = state,
             Weight = update_weight) %>%
      select(State,Weight,Share.Democratic,Share.Republican)
    region_update <- update_sheet %>%
      filter(State != state & State %in% adjacent_states) %>%
      mutate(Share.Democratic = baseline_democratic_share + Bonus,
             Share.Republican = baseline_republican_share + Penalty,
             Weight = Update_weight*region_weight) %>%
      select(State,Weight,Share.Democratic,Share.Republican)
    updates <- bind_rows(state_update,region_update,model_sheet[i,]) %>%
      add_case(Share.Democratic = baseline_democratic_share + national_mod,
               Share.Republican = baseline_republican_share - national_mod,
               Weight = national_mod_weight,)
    model_sheet$Share.Democratic[[i]] <- wtd.mean(updates$Share.Democratic,updates$Weight)
    model_sheet$Share.Republican[[i]] <- wtd.mean(updates$Share.Republican,updates$Weight)
    model_sheet$Weight[[i]] <- sum(updates$Weight)
  }
  model_sheet <- model_sheet %>% mutate(Democratic = Share.Democratic*Total,
                         Republican = Share.Republican*Total,
                         Democratic.EV = Total.EV*(Share.Democratic > Share.Republican),
                         Republican.EV = Total.EV*(Share.Republican > Share.Democratic))
  return(model_sheet)
}
