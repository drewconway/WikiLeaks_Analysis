frequency.table <- read.csv('frequency_table.csv', header = TRUE, sep = '\t')

frequency.table <- subset(frequency.table, !is.na(BaselineFrequency))

frequency.table <- transform(frequency.table, Ratio = Frequency / BaselineFrequency)
frequency.table <- transform(frequency.table, HighlyUsed = log(Ratio))

indices <- rev(order(with(frequency.table, HighlyUsed)))

frequency.table[indices[1:100],]

tokens <- with(subset(frequency.table[indices,],
                      Occurrences > 1000 & BaselineFrequency < 0.001),
               as.character(Token))

#total.occurrences <- sum(with(frequency.table, Occurrences))
#
#for (i in 1:nrow(frequency.table))
#{
#  if (is.na(frequency.table[i, 'BaselineFrequency']))
#  {
#    next()
#  }
#  
#  binom.test.results <- binom.test(frequency.table[i, 'Occurrences'],
#                                   total.occurrences,
#                                   frequency.table[i, 'BaselineFrequency'])
#  
#  frequency.table[i, 'PValue'] <- binom.test.results$p.value
#}
