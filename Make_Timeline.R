#URL страницы поиска в Yahoo: "games"
fileURL <- "https://search.yahoo.com/search;?p=mothers%20in%20city"

library('RCurl')
library('XML')

#загрузка текста html страницы
html <- getURL(fileURL)
#разбираем как html
doc <- htmlTreeParse(html, useInternalNodes = T)
#корневой элемент
rootNode <- xmlRoot(doc)

#выбор ссылок
u <- xpathSApply(rootNode, '//span[contains(@class, "fz-ms fw-m fc-12th wr-bw lh-17")]', xmlValue)

#выделение источников
s <- gsub(pattern = '/.+', replacement = '', x = u)

#выбор заголовков
h <- xpathSApply(rootNode, '//div[contains(@class, "compTitle options-toggle")]', xmlValue)

#убираем конечный пробел
h <- gsub(pattern = '[ ]+$', replacement = '', x = h)

#т.к. в некоторых запросах теги для ссылок разные, то
#формируем новый вектор
h.short <- c()
for (i in 1:length(u)){
  #ищем совпадение с ссылками
  h.short <- c(h.short, grep(pattern = u[i], x = h[i:length(h)], value=TRUE, fixed = T))
}

#выделяем ссылку
h.short.url <- gsub(pattern = '.+[ ]', replacement = '', x = h.short)

h.title <- c()
for (i in 1:length(h.short.url)){
  #выделяем заголовок
  h.title <- c(h.title, gsub(pattern = h.short.url[i], replacement = '', x = h.short[i], fixed = T))

}
#убираем конечный пробел
h.title <- gsub(pattern = '[ ]+$', replacement = '', x = h.title)

#объединяем вектора во фрейм
df <- data.frame(Year = 2016, Header = h.title, Source = s, URL = u)



#далее для теста

if(file.exists('Timeline.csv')){
  print('есть')
}

#расширяем фрейм
df1 <- data.frame(Year = 2016, Header = h, Source = s, URL = u)
df <- rbind(df, df1)

#перезапись файла
write.csv(df, './Timeline.csv', row.names = F)









