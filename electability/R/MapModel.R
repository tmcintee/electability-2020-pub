MapModel <- function(sheet,base_d = 0,base_r = 0, base_i = 0,headline = "Election model")
{
  sheet <- sheet %>%
    filter(Total.EV > 0)
  sheet <- AddNamedColumns(sheet)
  sheet <- AddAreaCol(sheet)
  sheet <- AddHSVcolumns(sheet,
                         alphas = c(0,0,0,80,0,0))
  sheet$State <- tolower(sheet$State)
  label_text <- paste0("Democrats ",sum(sheet$Democratic.EV)+base_d,", Republicans ",sum(sheet$Republican.EV)+base_r)
  if(base_i!= 0)
  {
    label_text <- paste0(label_text,", others ",base_i)
  }
  label_text <- paste0(label_text,".")
  pop_vote_dem <- 100*round(digits = 3,x = sum(sheet$Democratic)/sum(sheet$Total))
  pop_vote_rep <- 100*round(digits = 3, x = sum(sheet$Republican)/sum(sheet$Total))
  pop_vote_text <- paste0("Democrats ",pop_vote_dem,"%, Republicans ",pop_vote_rep,"%")
  map <- MapHSV(sheet)+
           labs(x =label_text)+
  ggtitle(headline,pop_vote_text)
  return(map)
}
