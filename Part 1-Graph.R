source("Part 1.R")

ggplot(bouy.tidy[1:10,],aes(Date,ATMP,group=1)) + 
  geom_line(na.rm=TRUE) + xlab("Date") + ylab("Air Temperature") + 
  expand_limits(y=c(-12,10)) +
  theme(axis.text.x = element_text(angle = 90))
