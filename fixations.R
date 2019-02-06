library(dplyr)
source("eyetracking.R")

data <- read_avotec("10129_20180917_run1_171630.txt")

max.dist <- 0.3
num.runs <- 10

#data.x
drv <- diff(data$x_correctedgaze)
drv.rle <- rle(abs(drv)>max.dist)
#drv.rle$lengths
rle.ind <- which(drv.rle$lengths>num.runs & drv.rle$values == FALSE)
#rle.ind

rle.cs <- cumsum(drv.rle$lengths)
#rle.cs

diff.ends <- rle.cs[rle.ind]
#diff.ends

diff.start <- rle.cs[rle.ind-1]
#diff.start

#cbind(diff.start, diff.ends)[1:10,]

trues <- unlist(mapply(seq, diff.start, diff.ends))

class(data.x)

data.x$fixation <- TRUE

x_fix <- data.frame(x=matrix(unlist(data.x)))
x_fix <- x_fix %>%
  mutate(fix = FALSE)

x_fix$fix[trues] = TRUE

#print(x_fix)

#x_fix %>%
#  ggplot(aes(x, y=1:nrow(x_fix)))+geom_point(aes(color=fix))+xlim(-1,1)+ylim(10000,10500)

#add fixation to df
#color plot facet wrap to where data is fixated
#x,y will actually be x,y gaze

data$fixation <- x_fix$fix[1:(nrow(x_fix)-1)]
data

data %>% filter(event %in% c("img","mgs")) %>% mutate(trial = as.numeric(trial)) %>%
  ggplot()+aes(x=x_gaze, y=y_gaze, color=fixation)+geom_point()+facet_wrap(trial ~ event)
