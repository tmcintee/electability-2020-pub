
#Inputs are 0-360 for hue, 0-100 for S,V, and alpha.
AddHSVcolumns <- function(elecsheet,
								  hues = c(240,0,80,160),
								  sats = c(100,100,100,100),
								  vals = c(0,0,0,0,0,0),
								  alphas = c(20,20,20,20,20,20),
								  winner = 1)
{

	nemmy <- names(elecsheet)
	numcan = sum(as.numeric(grepl("^Elec Vote",nemmy)))
	elecVoteCols <- elecsheet[,grepl("^Elec Vote",nemmy)]
	colames <- names(elecVoteCols)
	maxVotes <- max(elecsheet$'Total Elec Vote')
	elecsheet$hue <- 0
	elecsheet$sat <- 0
	elecsheet$val <- 0
	elecsheet$alpha <- 0
	maxEV <- max(elecVoteCols)

	for(i in 1:nrow(elecsheet))
	{
		targetIndex <- which.max(elecVoteCols[i,])
		temp <- elecVoteCols
		temp[[i,targetIndex]] <- 0
		secondIndex <- which.max(temp[i,])
		elecsheet$hue[[i]] <- WeightedAverageTwoHues(hues[[targetIndex]],hues[[secondIndex]],elecVoteCols[[i,targetIndex]],elecVoteCols[[i,secondIndex]])
		elecsheet$sat[[i]] <- weighted.mean(sats[1:numcan],elecVoteCols[i,])
		cappedElecVoteCols <- replace(elecVoteCols,elecVoteCols>3,3)
		elecTotal <- sum(elecVoteCols[i,])
		#Val and alpha are both set to be cumulative, with a baseline (3 EV) color. This controls how dark / intense the map is.
		elecsheet$val[[i]] <- 100-
			(vals[[1]]*cappedElecVoteCols[[i,winner]]/3+
			vals[[2]]*sum(cappedElecVoteCols[i,])/3+
			vals[[3]]*elecVoteCols[[i,winner]]/maxEV+
			vals[[4]]*sum(elecVoteCols[i,])/maxEV)
		elecsheet$alpha[[i]] <- (alphas[[1]]*cappedElecVoteCols[[i,winner]]/3+
										 		alphas[[2]]*sum(cappedElecVoteCols[i,])/3+
												alphas[[3]]*elecVoteCols[[i,winner]]/maxEV+
												alphas[[4]]*sum(elecVoteCols[i,])/maxEV)
		if(!is.null(elecsheet$area[[i]]))
		{
			minArea <- min(elecsheet$area)
			elecsheet$val[[i]] <- elecsheet$val[[i]] -
				vals[[5]]*elecVoteCols[[i,winner]]*minArea/elecsheet$area[[i]] -
				vals[[6]]*sum(elecVoteCols[i,])*minArea/elecsheet$area[[i]]
			elecsheet$alpha[[i]] <- elecsheet$alpha[[i]] +
				alphas[[5]]*elecVoteCols[[i,winner]]*minArea/elecsheet$area[[i]] +
				alphas[[6]]*sum(elecVoteCols[i,])*minArea/elecsheet$area[[i]]
		}
	}
	#scale to the 0-1 span:
	elecsheet$hue <- elecsheet$hue/360
	elecsheet$val <- elecsheet$val/100
	elecsheet$sat <- elecsheet$sat/100
	elecsheet$alpha <- elecsheet$alpha/100
	#Now cap in case anything went awry in the 0-1 restricted parameters. Hue is forced to 0 to 360 by the hue averaging function.
	elecsheet$val <- replace(elecsheet$val,elecsheet$val>1,1)
	elecsheet$val <- replace(elecsheet$val,elecsheet$val<0,0)
	elecsheet$sat <- replace(elecsheet$sat,elecsheet$sat>1,1)
	elecsheet$sat <- replace(elecsheet$sat,elecsheet$sat<0,0)
	elecsheet$alpha <- replace(elecsheet$alpha,elecsheet$alpha>1,1)
	elecsheet$alpha <- replace(elecsheet$alpha,elecsheet$alpha<0,0)
	elecsheet$hsv <- hsv(elecsheet$hue,elecsheet$sat,elecsheet$val,elecsheet$alpha)


	return(elecsheet)
}
