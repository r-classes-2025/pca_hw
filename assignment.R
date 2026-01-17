# В архиве letters вы найдете письма Льва Толстого в формате XML.
# Вам надо клонировать репозиторий с дз, выполнить задание и закоммитить изменения.
# Не меняйте структуру репозитория. Не переименовывайте файл с заданием.
# Не переименовывайте переменные. 

# Вам надо извлечь из каждого файла том, дату и адресата, и собрать эти данные в одну таблицу. 
# дата письма: в header — тег correspAction, тип sending — тег date, атрибут when;
# адресат: в header – тег correspAction, тип receiving — имя получателя (текст)
# том: в header — biblScope, юнит vol — номер тома

# Применяйте trimws() к результату парсинга, чтобы избавиться от лишних строк. 

library(xml2)
library(dplyr)
library(purrr)

unzip("letters.zip")

my_xmls <- list.files("letters/", full.names = TRUE)

# пишем код для одного письма
test_xml <- my_xmls[1]
doc <- read_xml(test_xml)
ns <- xml_ns(doc)

# дата письма
date <- xml_find_first(doc, "//d1:correspAction[@type='sending']//d1:date", ns) |> 
  xml_attr("when") 



# адресат письма
corresp <- xml_find_first(doc, "//d1:correspAction[@type='receiving']//d1:persName", ns) |> 
  xml_text() |> 
  trimws()



# том 

vol <- xml_find_first(doc, "//d1:biblScope[@unit='vol']", ns) |> 
  xml_text() |> 
  trimws()


### ---------------------------###
## теперь оборачиваем в функцию
read_letter <- function(xml_path) {
  doc <- read_xml(xml_path)
  ns <- xml_ns(doc)
  
  # дата письма
  date <- xml_find_first(doc, "//d1:correspAction[@type='sending']//d1:date", ns) |> 
    xml_attr("when") 
  
  # адресат письма
  corresp <- xml_find_first(doc, "//d1:correspAction[@type='receiving']//d1:persName", ns) |> 
    xml_text() |> 
    trimws()
  
  vol <- xml_find_first(doc, "//d1:biblScope[@unit='vol']", ns) |> 
    xml_text() |> 
    trimws()
  
  # записываем в тиббл
  res <- tibble(
    date = date,
    corresp = corresp,
    vol = vol
  )
  
  return(res)
}


letters_tbl <- map_dfr(my_xmls, read_letter)
