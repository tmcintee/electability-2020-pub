ProcessPolls <- function(poll_file,candidate_vector)
{
  polls_raw <- read.csv(poll_file,stringsAsFactors = FALSE)
  nameVector <- as.character(polls_raw$candidate_name)
  list_names <- str_split(nameVector," ")
  for(i in 1:length(list_names))
  {
    first_name <- list_names[[i]][[1]]
    last_name <- list_names[[i]][[length(list_names[[i]])]]
    if(first_name == "JuliÃÂ¡n")
    {
      first_name <- "Julian"
    }
    else if(last_name == "Castro")
    {
      first_name <- "Julian"
    }
    else if(first_name == "Joseph")
    {
      first_name <- "Joe"
    }
    else if(first_name == "Bernard")
    {
      first_name <- "Bernie"
    }
    if(last_name == "Jr." | last_name == "III")
    {
      last_name <- list_names[[i]][[length(list_names[[i]])-1]]
    }
    else if(last_name == "Blasio")
    {
      last_name <- "de Blasio"
    }
    name_candidate_cleaned <- paste0(first_name," ",last_name)
    nameVector[[i]] <- paste0(first_name," ",last_name)
  }
  polls_raw$Candidate <- nameVector
  polls_raw$start_date <- parse_date(as.character(polls_raw$start_date),format = "%m/%d/%y")
  polls_condensed <- polls_raw %>%
    spread(key = candidate_party, value = pct) %>%
    group_by(question_id) %>%
    mutate(Share.Democratic = max(DEM,na.rm = TRUE)/100,
                                        Share.Republican = max(REP, na.rm = TRUE)/100) %>%
    ungroup %>%
    group_by(poll_id) %>%
    mutate(Bonus = Share.Democratic - median(Share.Democratic),
           Penalty = Share.Republican - median(Share.Republican),
           Year = 2020+as.numeric(start_date-parse_date("2020-11-03"))/366,
           Weight = sqrt(sample_size)*TimeDecay(Year),
           State = state,
           Type = "Poll") %>%
    filter(Candidate %in% candidate_vector) %>%
    ungroup %>%
    select(Candidate,State,Year,Bonus,Penalty,Weight,Type)
  return(polls_condensed)
}
