AddNamedColumns <- function(elecsheet)
{
  elecsheet$`Total Elec Vote` <- elecsheet$Total.EV
  elecsheet$`Elec Vote D1` <- elecsheet$Democratic.EV
  #elecsheet$`Elec Vote O1` <- elecsheet$Other.EV
  elecsheet$`Elec Vote R1` <- elecsheet$Republican.EV
  return(elecsheet)
}
