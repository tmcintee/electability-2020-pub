require(tidyverse)
GraphElectionResults <- function(elections,candidate_name)
{
  state_names <- elections$State[elections$Candidate == candidate_name]
  election_years <- elections$Year[elections$Candidate == candidate_name]
  comparison_sheet <- elections %>%
    filter(State %in% state_names & Year %in% election_years) %>%
    group_by(State,Year) %>%
    mutate(Median_for = median(For),
           Median_against = median(Against)) %>%
    mutate(Net_for = For - Median_for,
           Net_against = Against - Median_against)
  return(comparison_sheet)
}
