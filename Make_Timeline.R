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

#объединяем вектора во фрейм
df <- data.frame(Year = 2016, Header = h, Source = s, URL = u)

#далее для теста

if(file.exists('Timeline.csv')){
  print('есть')
}
#расширяем фрейм
df1 <- data.frame(Year = 2016, Header = h, Source = s, URL = u)
df <- rbind(df, df1)

#перезапись файла
write.csv(df, './Timeline.csv', row.names = F)









