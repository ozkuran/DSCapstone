twitterData <- './final/en_US/en_US.twitter.txt'
#blogData <- './final/en_US/en_US.blogs.txt'
#newsData <- './final/en_US/en_US.news.txt'


twitter <- readLines(twitterData)
#blogs <- readLines(blogData)
#news <- readLines(newsData)

length(twitter)
#length(news)
#length(blogs)

twitterMax <- max(nchar(twitter))
#newsMax <- max(nchar(news))
#blogMax <- max(nchar(blogs))


love<- sum(grepl("love", twitter))
hate<- sum(grepl("hate", twitter))
love/hate

biostats<- grep("biostats", twitter)
twitter[biostats]

sum(grepl("A computer once beat me at chess, but it was no match for me at kickboxing", twitter))