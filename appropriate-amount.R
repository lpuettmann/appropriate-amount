library(jsonlite)
library(ggplot2)

CalcShare <- function(fname) {
  chrdat <- fromJSON(fname)
  extrDat <- chrdat$graph_data$data[[1]]
  nr <- extrDat$article_matches
  tot <- extrDat$total_articles_published
  shareArticles <- nr / tot * 100 # in %
  return(shareArticles)
}

shareLincoln <- CalcShare("lincoln.json")
shareRoosevelt <- CalcShare("roosevelt.json")
shareKennedy <- CalcShare("kennedy.json")
shareTrump <- CalcShare("trump.json")

year <- 1851:2016

# Save in dataframe
chrDat <- data.frame(year, shareLincoln, shareRoosevelt, 
                     shareKennedy, shareTrump)

# Make plot
theme_set(theme_grey(base_size = 18)) 

ggplot(chrDat,
  aes(x=year)) +
  geom_line(aes(y=shareLincoln, colour="Lincoln"), 
            size=1.5) + 
  geom_line(aes(y=shareRoosevelt, colour="Roosevelt"), 
            size=1.5) + 
  geom_line(aes(y=shareKennedy, colour="Kennedy"), 
            size=1.5) + 
  geom_line(aes(y=shareTrump, colour="Trump"), 
            size=1.5) +
  scale_colour_manual("", 
                      breaks = c("Lincoln", "Roosevelt", 
                                 "Kennedy", "Trump"),
                      values = c("Lincoln"="#d95f02", 
                                 "Roosevelt"="#1b9e77", 
                                 "Kennedy"="#7570b3", 
                                 "Trump"="#e7298a")) +
  labs(x="", y="Percent of total articles") +
  labs(title="New York Times Articles")