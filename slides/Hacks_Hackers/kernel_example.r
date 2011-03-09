library(ggplot2)

load('../../afg.data')
data <- afg.data$data

# c(69.1628, 34.5311) kabul location
# let's subset a very rough area around Kabul
kabul = subset(data,Latitude>34 & Latitude < 35 & Longitude >69 & Longitude <70)

start = as.Date('2004-01-01')
stop = as.Date('2005-01-01')
#kabul = subset(kabul,DateOccurred < stop & DateOccurred > start)

# find the number of events in each week
events_in_week <- function(week_index){
    date_lower = start + (7*(week_index-1))
    date_upper = date_lower + 7
    nrow(subset(kabul, DateOccurred < date_upper & DateOccurred > date_lower))
}

num_weeks = round(as.integer(stop-start)/7)
df = data.frame(
    num_events_per_week=sapply(seq(num_weeks),events_in_week)
)

p <- ggplot(df, aes(x=num_events_per_week))
# look at a per-week histogram
p <- p + geom_histogram(binwidth = 1)
p <- p + xlab("number of events per week in Kabul in 2004")+ylab("Count")
ggsave("images/histogram.png",dpi=72,width=8,height=6)

# 1 sample some data from a Poisson process using the maximum likelihood estimate
# of lambda (it's easy to sound hardcore when playing with Poissons)
n = 100
lambda = mean(df['num_events_per_week'])
data = data.frame(
    number_of_events = rpois(n=n,lambda=lambda)
)
# 2 evaluate the likelihood Poisson process
x = seq(max(df['num_events_per_week']))
l = data.frame(
    x = x,
    likelihood = sapply(x,dpois,lambda=lambda)
)
# 4 draw the likelihood
p <- ggplot(l, aes(x=x,y=likelihood))
p <- p + geom_line() + geom_point(size=4) 
p <- p + xlab('number of events per week in Kabul in 2004') + ylab('count')
ggsave("images/distribution.png",dpi=72,width=8,height=6)
# 5 draw both
p <- ggplot(df,aes(x=num_events_per_week))
p <- p + geom_density(colour="red")
p <- p + geom_point(data=l, aes(x=x,y=likelihood), size=4)
p <- p + xlab('number of events per week in Kabul in 2004') + ylab('probability of seeing n events')
ggsave("images/density_estimate.png",dpi=72,width=8,height=6)
