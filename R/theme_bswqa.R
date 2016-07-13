#' BSWQA ggplot2 theme
#' 
#' Theme is standard used in BSWQA JOC presentations
#' @keywords ggplot2 graph
#' @export
#' @examples
#' library(ggplot2)
#' g <- ggplot(mtcars) +
#'      geom_point(aes(x = wt, y = mpg, colour = factor(gear))) +
#'      theme_bswqa()

theme_bswqa <- function(base_size = 12, base_family = "") {
	theme_grey(base_size = base_size, base_family = base_family) %+replace%
	 theme(
		legend.position = "bottom",
		panel.background = element_blank(),
		plot.title = element_text(lineheight=.8, face="bold"),
		legend.title = element_blank(),
		axis.line = element_line(),
		axis.line.x = element_line(),
		axis.line.y = element_line(),
		axis.text.x = element_text(size = 10, face = "bold"),
		axis.text.y = element_text(size = 10, face = "bold")
	)
}