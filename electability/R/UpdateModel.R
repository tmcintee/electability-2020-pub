UpdateModel <- function(model_sheet,
                        update_sheet,
                        candidate_name,
                        region_pairs,
                        base_weight = 1,
                        region_weight = 0.25,
                        national_weight = 0.25,
                        unrelated_weight = 0.05,
                        poll_weight = 1,
                        election_weight = 1,
                        baseline_weight = 200)
{
  trimmed_update <- update_sheet %>%
    filter(Candidate == candidate_name)
  if(nrow(trimmed_update) == 0)
  {
    return(model_sheet)
  }
  trimmed_update[trimmed_update$Type == "Poll",]$Weight <- poll_weight*trimmed_update[trimmed_update$Type == "Poll",]$Weight
  trimmed_update[trimmed_update$Type == "Election",]$Weight <- poll_weight*trimmed_update[trimmed_update$Type == "Election",]$Weight
  trimmed_update$Share.Democratic <- 0.5
  trimmed_update$Share.Republican <- 0.5
  model_basis <- model_sheet %>% select(State,
                                        Weight,
                                        Share.Democratic,
                                        Share.Republican) %>% mutate(Weight = baseline_weight)
  update_sheet <- data.frame(State = character(),
                             Weight = numeric(),
                             Share.Democratic = numeric(),
                             Share.Republican = numeric())
  #model_basis <- update_sheet
  for(state in unique(model_sheet$State))
  {
    model_state <- model_sheet %>% filter(State == state)
    baseline_democratic_share <- wtd.mean(model_state$Share.Democratic,model_state$Weight)
    baseline_republican_share <- wtd.mean(model_state$Share.Republican,model_state$Weight)
    adjacent_states <- unique(unlist(region_pairs %>% filter(State1 == state | State2 == state)))
    state_update <- trimmed_update %>%
      filter(State == state) %>%
      mutate(Weight = base_weight*Weight,
             Share.Democratic = baseline_democratic_share + Bonus,
             Share.Republican = baseline_republican_share + Penalty,
             State = state) %>%
      select(State,Weight,Share.Democratic,Share.Republican)
    region_update <- trimmed_update %>%
      filter(State %in% adjacent_states & !State == state) %>%
      mutate(Weight = region_weight*Weight,
             Share.Democratic = baseline_democratic_share + Bonus,
             Share.Republican = baseline_republican_share + Penalty,
             State = state) %>%
      select(State,Weight,Share.Democratic,Share.Republican)
    national_update <- trimmed_update %>%
      filter(State == "") %>%
      mutate(Weight = national_weight*Weight,
             Share.Democratic = baseline_democratic_share + Bonus,
             Share.Republican = baseline_republican_share + Penalty,
             State = state) %>%
      select(State,Weight,Share.Democratic,Share.Republican)
    other_update <- trimmed_update %>%
      filter(!State %in% c("",state,adjacent_states)) %>%
      mutate(Weight = unrelated_weight*Weight,
             Share.Democratic = baseline_democratic_share + Bonus,
             Share.Republican = baseline_republican_share + Penalty,
             State = state) %>%
      select(State,Weight,Share.Democratic,Share.Republican)
    update_state <- rbind(state_update,national_update,region_update,other_update)
    model_basis <- rbind(model_basis,update_state)
  }
  new_model <- ProcessElectionModel(model_basis,model_sheet)
  return(new_model)
}
