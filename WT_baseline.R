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
mdat <- melt(dat, variable.name = "Date", value.name = "Value") %>% filter(Date != "06/17/2013")

only_WT <- mdat %>% filter(Type == "WT")

dodge <- position_dodge(width=0.9)

BL <- only_WT %>% group_by(Date) %>% summarise(mBL = median(Value), sdBL = mad(Value))

BLdat <- mdat %>% group_by(Date) %>% mutate(Norm = (Value - median(Value[Type == "WT"]))/mad(Value[Type == "WT"])) %>% ungroup

fin <- BLdat %>% group_by(Detect, Type) %>% summarise(mNorm = median(Norm), sdNorm = mad(Norm)) %>% 
  mutate(Up = mNorm + sdNorm, Low = mNorm - sdNorm)

ggplot(fin, aes(x = Detect, y = mNorm, colour = Type)) +
  geom_point(position = dodge) + 
  geom_errorbar(aes(ymax = Up, ymin = Low, x = Detect, colour = Type), position=dodge, width=0.25) + 
  cool_theme
  