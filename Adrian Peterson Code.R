################################################################################
################################################################################
##################### Code for Adrian Peterson Article #########################
### Written on March 24, 2016 by Maksim Horowitz (@bklynmaks)

################################################################################
################################################################################

# Dowloading Play-By-Play Data

pbp2009 <- season_play_by_play(2009)
Sys.sleep(sample(30:300, replace = TRUE))
pbp2010 <- season_play_by_play(2010)
Sys.sleep(sample(30:300, replace = TRUE))
pbp2011 <- season_play_by_play(2011)
Sys.sleep(sample(30:300, replace = TRUE))
pbp2012 <- season_play_by_play(2012)
Sys.sleep(sample(30:300, replace = TRUE))
#pbp2013 <- season_play_by_play(2013)
# Game with Error: 2013092206
pbp2014 <- season_play_by_play(2014)
Sys.sleep(sample(30:300, replace = TRUE))
pbp2015 <- season_play_by_play(2015)


# Play-by-play from 2013 minus the problematic game
gameids2013 <- extracting_gameids(2013)

games2013minus1 <- lapply(gameids2013[-which(gameids2013 == "2013092206")],
                          game_play_by_play)

pbp2013.minusonegame <- do.call(rbind, games2013minus1)

# Saving Data
save(pbp2009, file = "pbp2009.RData")
save(pbp2010, file = "pbp2010.RData")
save(pbp2011, file = "pbp2011.RData")
save(pbp2012, file = "pbp2012.RData")
save(pbp2013.minusonegame, file = "pbp2013m1.RData")
save(pbp2014, file = "pbp2014.RData")
save(pbp2015, file = "pbp2015.RData")



# Basic Plots
# Adrian Peterson's Run Spread

ap.rushes <- subset(total.dat, Rusher == "A.Peterson")

### Across Downs and Quarter

ap.rushes.narm <- ap.rushes[which(!is.na(ap.rushes$down)),]

ap.rushes.narm$Quarter <- ap.rushes.narm$qtr
ap.rushes.narm$Down <- ap.rushes.narm$down

ap.summarized <- ap.rushes.narm %>% group_by(Quarter, Down) %>%
  summarize_each(funs(mean, max, min), matches("Yards.Gained"))

# Quarter 2 down 2

ap.q2.d2 <- subset(ap.rushes.narm, Quarter == 2 & Down == 2)
runsover15 <-  length(which(ap.q2.d2$Yards.Gained >= 10))

# Third down Pass Plays

all.3d <- subset(total.dat, down = 3)
all.3d <- all.3d[which(!is.na(all.3d$down)),]
all.3d <- subset(all.3d, is.na(PenaltyType))

#round(prop.table(table(all.3d$PlayType)), 2)

# length(which(all.3d$PlayType == "Pass")) / nrow(all.3d)

ap.yard.qrt.down <- ggplot(ap.rushes.narm, aes(x = Yards.Gained)) + ylim(0, .25) +
  annotate(geom = "text", 
           label = "Summary Statistics of \nDistribution",
           x = 57, y = .23, size = 2.7, color = "navy") +
  geom_density(alpha = .45,  fill = "darkturquoise") + geom_rug() + 
  facet_grid(Quarter~Down, 
             labeller = label_both)  + 
  geom_text(aes(x = 57, y = .18, label= paste("Mean:", round(mean,2)), 
                group=NULL),  size = 2.8, data=ap.summarized) +
  geom_text(aes(x = 57, y = .15, label= paste("Min:", min), 
                group=NULL),  size = 2.8, data=ap.summarized) +
  geom_text(aes(x = 57, y = .12, label= paste("Max:", max), 
                group=NULL),  size = 2.8, data=ap.summarized) +
  ggtitle("Adrian Peterson Yards Gained by Down and Quarter") + xlab("Yards Gained on Rushes") + ylab("Density") +
  theme(plot.title = element_text(face = "bold"))

# Printing Table
ap.yard.qrt.down