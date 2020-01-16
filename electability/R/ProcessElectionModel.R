ProcessElectionModel <- function(df, ev_frame)
{
      df <- df %>%
        select(State,
               Weight,
               Share.Democratic,
               Share.Republican) %>%
        group_by(State) %>%
        summarise(Variability = sd(Share.Democratic-Share.Republican),
                Share.Democratic = wtd.mean(Share.Democratic,Weight),
                Share.Republican = wtd.mean(Share.Republican,Weight),
                Weight = sum(Weight)) %>%
        inner_join(ev_frame %>%
                     select(State, Total.EV,Total,Democratic,Republican)) %>%
        mutate(Democratic.EV = Total.EV*(Share.Democratic > Share.Republican),
             Republican.EV = Total.EV*(Share.Republican > Share.Democratic),
             Democratic = Share.Democratic*Total*(Total > 0),
             Republican = Share.Republican*Total*(Total > 0))
    return(df)
}
