library(reshape2)
library(dplyr)
library(ggplot2)

size_mod <- -5
cool_theme <- theme(plot.background=element_rect(fill = "transparent",
                                                 colour = "transparent"),
                    panel.grid.major = element_line(colour="lightgrey", linetype = "dashed"),
                    panel.background = element_rect(fill = "transparent",colour = "black"),
                    legend.background = element_rect(fill="NA"),
                    legend.position = "bottom",
                    axis.text = element_text(size=12 + size_mod),
                    axis.title.x = element_text(size=16 + size_mod, vjust = -1), 
                    axis.title.y = element_text(size=16 + size_mod, vjust = 1),
                    strip.text = element_text(size=17 + size_mod, face = "bold"),
                    legend.text = element_text(size=13 + size_mod), 
                    legend.title = element_text(size=17 + size_mod),
                    plot.title = element_text(size=20 + size_mod))

dat <- read.csv("dat.csv", header = FALSE)
colnames(dat) <- c("Detect", "Type", "05/23/2013", "06/10/2013", "06/17/2013", "06/13/2013")
dat <- dat[, -5]
#dat[, -c(1, 2)] <- scale(dat[, -c(1, 2)])

mdat <- melt(dat, variable.name = "Date", value.name = "Value")

dodge <- position_dodge(width=0.9)

sumdat <- mdat %>% group_by(Detect, Date) %>% mutate(z = scale(Value)) %>% 
  ungroup %>% group_by(Detect, Type) %>% summarize(Mean = mean(z), SD = sd(z)) %>%
  mutate(Up = Mean + SD, Low = Mean - SD)


mdat %>% ggplot(aes(x = Detect, y = Value, fill = Type, colour = Type, label = Date)) +
  #geom_boxplot(position = dodge, alpha = 0.2, outlier.colour = NA) + 
  geom_point(size = 4, position = position_jitterdodge(jitter.width = 0.6, dodge.width = 0.9)) +
  cool_theme +
  facet_wrap(~Date, nrow = 1)


BLs <- mdat %>% filter(Type == "WT") %>% group_by(Date) %>% summarise(BL = mean(Value), BLsd = sd(Value))
bldat <- dat

bldat[, -c(1, 2)] <- sapply(1L:length(BLs), function(i)
  (dat[, 2 + i] - unlist(BLs[i, "BL"]))/unlist(BLs[i, "BLsd"]))
bmdat <- melt(bldat, variable.name = "Date", value.name = "Value")
bmdat %>% group_by(Detect, Type) %>% summarise(Mean = mean(Value), SD = sd(Value)) %>%
  mutate(Up = Mean + SD, Low = Mean - SD) %>%
  ggplot(aes(x = Detect, y = Mean, fill = Type)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymin = Low, ymax = Up), position = dodge)
