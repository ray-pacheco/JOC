#' Create Metrics Plot
#' 
#' This function is creates a standard time series plot for the Joint Operation Committee (JOC) presenations
#' @param POD A character string. defaults to North
#' @param target Numeric. Goal for metric. Defaults to 0.815
#' @param lowerBound A character string. Lower bound of the graph. Defaults to 0.
#' @param graphtitle A character string.
#' @param graphname must end in .png.
#' @keywords ggplot2 graph
#' @export
#' @examples
#' createMetricsPlot()


createMetricsPlot <- function(POD = 'North', target = .815, lowerBound = 0,
															graphtitle = "NTX Aetna Commercial - Generic Fill Rate (Primary Care)",
															graphname = paste0("C:/Users/e90795/Desktop/Rx1.png")){
	
#--------------------------------------
#-- Library Packages
#---------------------------------------
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
	
division <- ifelse(POD %in% c("Central", "East", "North", "Northwest", "South", "West"), "NTX", "CTX")
	
rx %>%
	group_by(POD, Ending_Date) %>%
	summarise(num = sum(Numerator), den = sum(Denominator)) %>%
	mutate(Percent = num / den) -> rx1
	
rx1$Ending_Date <- floor_date(rx1$Ending_Date, "month")
	
rx1$Ending_Date <- as.Date(rx1$Ending_Date)

lastMonth <- max(rx1$Ending_Date) #determine most recent month
	
firstMonth <- min(rx1$Ending_Date) #determine most recent month
	
rxSub <- subset(rx1, !(rx1$POD %in% c(POD,division)) & rx1$Ending_Date == lastMonth) #use last month's data to determine best and worst PODs
	
best <- as.character(rxSub$POD[which.max(rxSub$Percent)]) #best
worst <- as.character(rxSub$POD[which.min(rxSub$Percent)]) #worst
	
PODs <- c(POD, best, worst, division) #PODs for subset
	
rxSub1 <- subset(rx1, rx1$POD %in% PODs) #subset to best, worst, active and system PODs
	
rxSub2 <- subset(rx1, rx1$Ending_Date == lastMonth) #Need numbers for last month appended to POD Name for graph
	
rxSub2$Name <- paste0(rxSub2$POD, " (", comma(rxSub2$num), "/", comma(rxSub2$den), ")") # append (num /  denom) to POD name
	
rxSub2 <- rxSub2[,c("POD", "Name")] #filter to lookup table
	
rxSub1 <- left_join(rxSub1, rxSub2) # dplyr append lookup table
	
a <- unique(as.character(rxSub1$Name[rxSub1$POD == POD]))
b <- unique(as.character(rxSub1$Name[rxSub1$POD == best]))
c <- unique(as.character(rxSub1$Name[rxSub1$POD == worst]))
d <- unique(as.character(rxSub1$Name[rxSub1$POD == division]))
	
myColourNames <- c(a,b,c,d)
	
myColours <- c("#514689", "#009639", "#EE2737", "#008FBE")
	
names(myColours) <- myColourNames

upperBound <- min(1,round(max(rxSub1$Percent,target)*1.05/0.05)*0.05) 
	
formatting <- ifelse(unique(rx$Measure_ID) == 7217, comma, percent)
	
g <- ggplot(data = rxSub1, aes(x = Ending_Date, y = Percent, colour = Name)) %>%
	+ geom_line(aes(group=Name), size = 1.2) %>%
	+ geom_hline(yintercept = target, colour = "gold3", linetype = "dashed", size =1.2) %>%
	+ labs(x = "", y =  "") %>%
	+ ggtitle(graphtitle) %>%
	+ theme(legend.position = "bottom",
					panel.background = element_blank(),
					plot.title = element_text(lineheight=.8, face="bold"),
					legend.title = element_blank(),
					axis.line = element_line(),
					axis.text.x = element_text(size = 10, face = "bold"),
					axis.text.y = element_text(size = 10, face = "bold")) %>%
	+ scale_y_continuous(labels = formatting, limits = c(lowerBound,upperBound)) %>%
	+ scale_x_date(limits = c(firstMonth, lastMonth + days(15))
								 ,expand = c(0,0)
								 ,breaks = date_breaks(width = "2 months")
								 ,labels = date_format("%b %Y")) %>%
	+ scale_colour_manual(values = myColours)

ggsave(file= graphname ,width=8, height=5.5)
}