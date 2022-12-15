
#####################################################################
# FUNCTIONS FOR U5M ESTIMATION
#####################################################################
# FUNCTIONS FOR BARPLOT
##################################################
plot_2_bar <- function(data){
  gg <- ggplot(data = data) +
    geom_bar(mapping = aes(x = region,fill = outcome),
             position = "dodge") + 
    coord_flip()+
    labs(title = "Figure 1 Child Status for each of the regions", 
         x = "Regions", y = "Frequency")
  return(gg)
}
#####################################################################
dir_plot <- function(data){
  direct1<-subset(direct, region=="All")
  gg <- ggplot(data = direct1, aes(x = years, y = mean, group = region)) +
    geom_line() + 
    geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha=0.2) +
    facet_wrap(~region, ncol=5) + 
    geom_point(color = "red") + theme_bw() +
    theme(axis.text.x=element_text(angle=45, hjust = 1)) + 
    xlab("Year Period") + ylab("U5MR") + 
    ggtitle("Direct national Estimates of U5MR")
  return(gg)
}
########################################################################
dir_smooth <- function(data)
{
  fit0 <- fitINLA(data = direct, geo = NULL, Amat = NULL, year_label = years, year_range = c(1982, 2012), rw = 2, m=1)
  out0 <- getSmoothed(fit0, year_range = c(1982, 2012), year_label = years)
  gg <- ggplot(data = out0, aes(x = years, y = median, group = region)) +
    geom_line() + 
    geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha=0.2) +
    facet_wrap(~region, ncol=5) + 
    geom_point(color = "red") + theme_bw() +
    theme(axis.text.x=element_text(angle=45, hjust = 1)) + 
    xlab("Year Period") + ylab("U5MR") + 
    ggtitle("Smoothed National Estimates of U5MR")
  return(gg)
}
########################################################################
subnat_plot <- function(data)
{
  gg <- plot(out3) + ggtitle("Subnational period model") + ylim(c(0, 0.4))
  return(gg)
}
########################################################################
map_plot <- function(data){
  gg <- mapPlot(data = subset(out3, years %in% c(1982, 1987, 1992, 1997, 2002, 2007)), 
                  geo = geo.2016, variables=c("years"), values = c("median"), by.data = "region", 
                  by.geo = "REGNAME", is.long = TRUE, border = "gray80", size = 0.2, ncol = 4, per1000 = TRUE, legend.label = "U5MR") 
  return(gg)
  }  
########################################################################  





  