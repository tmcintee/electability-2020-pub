TweakAndSummarize <- function(model_sheet,
                              region_pairs,
                              candidate_name,
                              model_name,
                              region_weight = 0.25,
                              national_weight = 0.25,
                              update_weight = 100,
                              update_size = 0.05)
{
  return_sheet <- TweakModel(model_sheet,region_pairs,region_weight,national_weight,update_weight,update_size) %>%
    summarise(Candidate = candidate_name,
              Model = model_name,
              Votes.For = sum(Democratic)/sum(Total),
              Votes.Against = sum(Republican)/sum(Total),
              Electoral.Votes.For = sum(Democratic.EV),
              Electoral.Votes.Against = sum(Republican.EV))
  return(return_sheet)
}
SummarizeElecsheet <- function(elecsheet)
{
  elecsheet <- elecsheet %>% summarise(Candidate = candidate_name,
            Model = model_name,
            Votes.For = sum(Democratic)/sum(Total),
            Votes.Against = sum(Republican)/sum(Total),
            Electoral.Votes.For = sum(Democratic.EV),
            Electoral.Votes.Against = sum(Republican.EV))
  return(elecsheet)
}
