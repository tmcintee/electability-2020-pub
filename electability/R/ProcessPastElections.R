ProcessPastElections <- function(elec_file,candidate_vector)
{
  elecdata <- read.csv(elec_file,stringsAsFactors = FALSE)
  elecdata <- elecdata %>%
    group_by(State,Year) %>%
    mutate(Bonus = For/Total - median(For/Total),
           Penalty = Against/Total - median(Against/Total),
           Weight = sqrt(Total)*TimeDecay(Year),
           Type = "Election") %>%
    filter(Candidate %in% candidate_vector) %>%
    select(Candidate,State,Year,Bonus,Penalty,Weight,Type)
  return(elecdata)
}
