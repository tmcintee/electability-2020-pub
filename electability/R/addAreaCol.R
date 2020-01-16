AddAreaCol <- function(elecsheet,areas = data.frame(state = state.name,area = state.area,stringsAsFactors = FALSE))
{
	elecsheet$area <- 1
	for(i in 1:nrow(elecsheet))
	{
		state_name <- elecsheet$State[i]
		print(state_name)
		if(state_name %in% areas$state)
		{
			elecsheet$area[[i]] <- areas[areas$state==state_name,2]
		}
	}
	return(elecsheet)
}
