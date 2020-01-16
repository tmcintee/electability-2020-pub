MapHSV <- function(df,base_map = fiftystater::fifty_states)
{

	ggplot(df, aes(map_id=State))+
		geom_map(aes(fill=I(hsv)),map=base_map,color=rgb(0,0,0,128,max=255))+
		expand_limits(x = base_map$long, y = base_map$lat) +
		coord_map() +
		scale_x_continuous(breaks = NULL) +
		scale_y_continuous(breaks = NULL) +
		labs(x = "", y = "") +
		theme(legend.position = "bottom",
				panel.background = element_blank())
}
