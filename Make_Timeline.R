
#Загрузка пакетов
library("gWidgetsRGtk2")
library("gWidgets")
library("rpanel")
library("data.table")
library('RCurl')
library('XML')

flag <<- T
#форма для таблицы
panel.table <<- ""

#функция отображения таблицы
my.search <- function(h,..){ 
  
  #данные для таблицы
  df <<- data.frame()
  #поисковый запрос
  text <- svalue(panel.search)
  #выбранный год
  god <- as.numeric(svalue(panel.slider.date))
  #выбранный период
  step <- as.numeric(svalue(panel.slider.period))
  
  iter.step <- 0
  
  #цикл по годам
  while(iter.step <= step){
    #формирование 3-х запросов
    search1 <- paste0(svalue(panel.search), " in ", god+iter.step)
    search2 <- paste0(svalue(panel.search), " * in ", god+iter.step)
    search3 <- paste0(svalue(panel.search), " in ", god+iter.step, " *")
    
    svalue(panel.text.search) <- paste0(search1,"     ",
                                        search2,"     ",
                                        search3)
    #вектор запросов
    search <- c(search1, search2, search3)
    
    #цикл по запросам
    for (iter in 1:length(search)){
      
      #URL страницы поиска в Yahoo:
      fileURL <- "https://search.yahoo.com/search;?p="
      fileURL <- paste0(fileURL, search[iter])
      #замена пробелов на %20
      gsub(pattern = '[ ]', replacement = '%20', x = fileURL)
      
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
        tmp <- grep(pattern = u[i], x = h[i:length(h)], value=TRUE, fixed = T)
        if(length(tmp) != 0){
          h.short <- c(h.short, tmp[1])
        }
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
      df1 <- data.frame(Year = god+iter.step, Header = h.title, Source = s, URL = u)
      #добавляем строки во фрейм
      df <<- rbind(df, df1)
      
      #обновляем загрузку
      svalue(panel.loading) <- paste("Загрузка ", 100 * iter.step/step, " %")
    }
    #пауза от капчи
    Sys.sleep(5)
    iter.step <- iter.step + 1
  }
  
  
  if (flag){
    #добавляем таблицу в окно
    panel.table <<- gtable(items = df, container = group)
    size(panel.table) <- c(300, 500)
    
    flag <<- F
    
  }else{
    #обновляем таблицу в окне
    delete(group, panel.table)
    panel.table <<- gtable(items = df, container = group)
    size(panel.table) <- c(300, 500)
    
  }
}

#функция сохранения таблицы
my.save <- function(h,..){
  #перезапись файла
  write.csv(df, './Timeline.csv', row.names = F)
}

#!!!нужно выбрать пакет gWidgetsRGtk2
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#создание элементов в окне
panel.search <- gedit(text = "Your search", width = 40)
panel.btn <- gbutton(text = "Найти")
panel.btn.save <- gbutton(text = "Сохранить")
panel.text.search <- glabel()
panel.loading <- glabel()

#создание стилей
font(panel.loading) <- c( color = "red")
font(panel.text.search) <- c( size = "14")

#создание элементов в окне
panel.slider.period <- gslider(from=10, to=100, by=1)
panel.slider.date <- gslider(from=1900, to=2050, by=1)
panel.label.data <- glabel(text = " С какого \n года ")
panel.label.period <- glabel(text = " Число лет  ")

#Обработка события при нажатии на кнопку поиск
addHandlerClicked(panel.btn, handler=my.search)

#Обработка события при нажатии на кнопку сохранить
addHandlerClicked(panel.btn.save, handler=my.save)

#Создание окна
window <- gwindow("Результаты поисковика Yahoo", width = 900, height = 700, visible = F)

group <<- ggroup(horizontal=F, container=window)

#Добавление в окно поля загрузки
tmp <- gframe("", container=group)
add(tmp, panel.loading)

#Добавление в окно текстовой метки
glabel(text = "Что ищем?", container=group)

tmp <- gframe("Запрос на английском (желательно)", container=group)

#Добавление в окно textbox
add(tmp, panel.search)

#Добавление в окно label
add(tmp, panel.label.data)
#Добавление в окно slider даты
size(panel.slider.date) <- c(150, 40)
add(tmp, panel.slider.date)

#Добавление в окно label
add(tmp, panel.label.period)
#Добавление в окно slider - период
size(panel.slider.period) <- c(150, 40)
add(tmp, panel.slider.period)
#Добавление в окно кнопку поиска
add(tmp, panel.btn)
#Добавление в окно кнопку сохранить
add(tmp, panel.btn.save)


tmp <- gframe("Ваш текущий запрос", container=group)
#Добавление в окно label
add(tmp, panel.text.search)

visible(window) <- T