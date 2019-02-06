#dataframe test
library(dplyr)

df <- data.frame("x_corr" = sample(1:9,10,rep=TRUE)/10, 
                 "y_corr" = sample(1:9,10,rep=TRUE)/10,
                 "side" = c("Left", "Left", "Left", "Left", "Right",
                            "Right", "Right", "Right", "Left", "Left"),
                 "type" = c("img","mgs","img","mgs","img","mgs","img","mgs","img","mgs"),
                 "trial"= c(1,1,2,2,3,3,4,4,5,5)
)
df

fc <- function(x) {
  left.bound <- 0
  center.bound <- 0.5
  right.bound <- 1
  
  if(x>=left.bound && x<center.bound)
    return("Left")
  else if(x<=right.bound && x>center.bound)
    return("Right")
  else
    return("Center")
}

df %>% mutate(p_side = apply(a,1,fc)) %>%
  filter(side == p_side | p_side == "Center") %>%
  select(-p_side) %>%
  group_by(trial) %>%
  filter("mgs" %in% type && "img" %in% type)

#switch(1, "CENTER", "LEFT", "RIGHT")

#switch case


# 0, 0.25, 0.5, 0.75, 1.0
# x == center, center
# x>=left, x<center = left
# x<=right, x>center = right


##OLD STRATEGY

df.g <- df %>% 
  group_by(trial) %>%
  summarise(x_mean = mean(x_corr), y_mean = mean(y_corr))
df.g

#fc <- function(x,y,s) {
#  left.bound <- 0
#  center.bound <- 0.5
#  right.bound <- 1
#  ifelse(s == "Left",ifelse(x>=left.bound,ifelse(x<=center.bound, "LEFT", "NOT LEFT"),"NOT LEFT"),
#    ifelse(s=="Right",ifelse(x>=center.bound, ifelse(x<=right.bound,"RIGHT","NOT RIGHT"), "NOT RIGHT"),"NOT RIGHT")) 
#}






#fc(0.0, 0.2, "Left")
#fc(0.25, 0.2, "Left")
#fc(0.5, 0.2, "Left")
#fc(0.75, 0.2, "Left")
#fc(1.0, 0.2, "Left")
#fc(0.0, 0.2, "Right")
#fc(0.25, 0.2, "Right")
#fc(0.5, 0.2, "Right")
#fc(0.75, 0.2, "Right")
#fc(1.0, 0.2, "Right")

#pass data frame into function
#df %>% apply(fc(x,y,s))

#mutate original with result or alter entirely to remove false results



#mutate(df, status=fc())
