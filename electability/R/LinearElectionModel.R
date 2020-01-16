LinearElectionModel <- function(df, ev_frame)
{
  answer <- ev_frame %>%
    select(State, Total.EV,Total,Democratic,Republican,Year)
  answer$Variability <- 0
  answer$Share.Democratic <- 0.5
  answer$Share.Republican <- 0.5
  answer$Weight <- 0
  for(state in ev_frame$State)
  {
    print(state)
    temp_df <- df %>%
      select(State,
             Weight,
             Share.Democratic,
             Share.Republican,
             Year) %>%
      filter(State == state)
    target <- which(ev_frame$State == state)
    answer$Variability[target] <- var(temp_df$Share.Democratic-temp_df$Share.Republican,na.rm = TRUE)
    answer$Share.Democratic[target] <- lm(data = temp_df,
                                          Share.Democratic ~ Year)[[1]][[1]] +
      answer$Year[target]*lm(data = temp_df,
                             Share.Democratic ~ Year)[[1]][[2]]
    answer$Share.Republican[target] <- lm(data = temp_df,
                                          Share.Republican ~ Year)[[1]][[1]] +
      answer$Year[target]*lm(data = temp_df,
                             Share.Republican ~ Year)[[1]][[2]]
    answer$Weight[target] = sum(temp_df$Weight,na.rm = TRUE)
  }
  answer <- answer %>%
    inner_join(ev_frame %>%
                 select(State, Total.EV,Total,Democratic,Republican)) %>%
    mutate(Democratic.EV = Total.EV*(Share.Democratic > Share.Republican),
           Republican.EV = Total.EV*(Share.Republican > Share.Democratic),
           Democratic = Share.Democratic*Total*(Total > 0),
           Republican = Share.Republican*Total*(Total > 0))

  return(answer)
}
