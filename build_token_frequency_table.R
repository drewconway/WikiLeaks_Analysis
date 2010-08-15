source('load_data.R')

get.tokens <- function(text, token.style = 'alphabetical')
{
  text <- tolower(text)
  
  regex <- ''
  
  if (token.style == 'alphabetical')
  {
    regex <- "[a-z]+"
  }
  
  if (token.style == 'extended-alphabetical')
  {
    regex <- "[a-z]+[a-z\\-']*[a-z]*"
  }
  
  tokens <- grep(regex, strsplit(text, '\\s+', perl = TRUE)[[1]], value = TRUE, perl = TRUE)
  
  return(tokens)
}

index.tokens <- function(text)
{
  tokens <- get.tokens(text)
  
  index <- list()
  
  for (token in tokens)
  {
    if (is.null(index[[token]]))
    {
      index[[token]] = 1
    }
    else
    {
      index[[token]] = index[[token]] + 1
    }
  }
  
  return(index)
}

raw.text <- paste(with(afg, Summary), collapse = ' ')
token.index <- index.tokens(raw.text)
lexical.database <- as.data.frame(token.index)
names(lexical.database) <- c('Token', 'Occurrences')
write.table(lexical.database,
            file = 'frequency_table.csv',
            sep = '\t',
            row.names = FALSE)
