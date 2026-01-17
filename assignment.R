# установите и загрузите пакеты
library(friends)
library(tidyverse)
library(tidytext)
library(factoextra) 


# 1. отберите 6 главных персонажей (по количеству реплик)
# сохраните как символьный вектор
top_speakers <- friends |> 
  count(speaker) |> 
  arrange(-n) |> 
  slice_head(n = 6) |> 
  pull(speaker)

# 2. токенизируйте текст, удалите цифры
# стоп-слова не удаляйте!
friends_tokens <- friends |> 
  filter(speaker %in% top_speakers) |> 
  unnest_tokens(word, text) |> 
  filter(!str_detect(word, "\\d+")) |> 
  select(speaker, word) 

# 3. отберите по 500 самых частотных слов для каждого персонажа
# посчитайте относительные частотности для слов
friends_tf <- friends_tokens |>
  count(speaker, word) |> 
  group_by(speaker) |> 
  arrange(-n) |> 
  slice_head(n = 500) |> 
  ungroup() |> 
  mutate(total = sum(n), .by = speaker) |> 
  mutate(tf = n/total) |> 
  select(-n, -total) 

# 4. преобразуйте в широкий формат; 
# столбец c именем спикера превратите в имя ряда, используя подходящую функцию 
friends_tf_wide <- friends_tf |> 
  pivot_wider(names_from = word, 
              values_from = tf, values_fill = 0) |> 
  column_to_rownames("speaker")

# 5. установите зерно 123
# проведите кластеризацию k-means (k = 3) на нормализованных данных
set.seed(123)
km.out <- kmeans(scale(friends_tf_wide), centers = 3, nstart = 20)

km.out$cluster

# 6. примените к матрице метод главных компонент (prcomp)
# центрируйте и стандартизируйте, использовав аргументы функции
pca_fit <- prcomp(friends_tf_wide, scale = TRUE, center = TRUE)

# 7. Покажите наблюдения и переменные вместе (биплот)
# в качестве геома используйте текст (=имя персонажа)
# цветом закодируйте кластер, выделенный при помощи k-means
# отберите 20 наиболее значимых переменных (по косинусу, см. документацию к функции)
# сохраните график как переменную q

q <- fviz_pca_biplot(pca_fit,  geom = c("text"),
                     select.var = list(cos2 = 20),
                     habillage = as.factor(km.out$cluster)) 



# дополнительно попробуйте добиться оформления примерно как на фото в задании (не оценивается)
# весь текст шрифтом "Permanent Marker" от Google Fonts 
# вместо geom_text - geom_изображение (есть в папке) -
# для этого передайте geom_image() координаты каждого персонажа
# тема theme_friends() из ThemePark

library(showtext)
font_add_google("Permanent Marker", family = "friends")
showtext_auto()

# install.packages("remotes")
#remotes::install_github("MatthewBJane/ThemePark")
library(ThemePark)
library(ggimage)

pca_coords <- as.data.frame(pca_fit$x) |> 
  mutate(image_file = paste0("friends_images/", rownames(friends_tf), ".jpg"))

fviz_pca_biplot(pca_fit,  geom = c("image"),
                select.var = list(cos2 = 20),
                col.var = "steelblue",
                alpha.var = 0.3,
                repel = TRUE,
                ggtheme = theme_friends(),
                font.family = "friends") +
  theme(legend.position = "none") +
  geom_image(data = pca_coords, aes(x = PC1, y = PC2, image = image_file), size = 0.1) 

