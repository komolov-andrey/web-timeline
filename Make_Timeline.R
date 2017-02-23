#URL страницы поиска в Yahoo: "games"
fileURL <- "https://search.yahoo.com/search;?p=cars"

library('RCurl')
library('XML')
#загрузка текста html страницы
html <- getURL(fileURL)
#разбираем как html
doc <- htmlTreeParse(html, useInternalNodes = T)
#корневой элемент
rootNode <- xmlRoot(doc)

#выбор заголовков
h <- xpathSApply(rootNode, '//a[contains(@class, "ac-algo fz-l ac-21th lh-24")]', xmlValue)

#выбор ссылок
u <- xpathSApply(rootNode, '//span[contains(@class, "fz-ms fw-m fc-12th wr-bw lh-17")]', xmlValue)

#выделение источников
s <- gsub(pattern = '/.+', replacement = '', x = u)
