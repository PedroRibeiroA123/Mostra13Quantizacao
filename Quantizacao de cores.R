library(jpeg)
library(dplyr)
library(ggplot2)

#A função readJPEG() recebe o endereço de um arquivo .jpg e o converte
#em uma matriz de três dimensões contendo a localização do pixel
#e suas intensidades RGB.
imagem <- readJPEG("Audioslave_Album.jpg")


#Em seguida utilizamos a função tibble() para converter uma série de
#informações contidas na matriz acima em um objeto data frame
#compatível com as funções de agrupamento do R.
imagemRGB <- tibble(
  x = rep(1:dim(imagem)[2], each = dim(imagem)[1]),
  y = rep(dim(imagem)[1]:1, dim(imagem)[2]),
  R = as.vector(imagem[,, 1]),
  G = as.vector(imagem[,, 2]),
  B = as.vector(imagem[,, 3]))
#Os objetos x e y armazenam as coordenadas do pixel
#enquanto R, G e B armazenam as respectivas intensidades.


#Especifica o valor de k
k <- 2


#Essa chamada de função toma como parâmetros as três colunas de imagemRGB
#referentes às cores, a quantidade de agrupamentos e por consequência
#de cores da nova imagem, e a quantidade máxima de iterações para interromper
#a função caso a taxa de variação geral não se estabilize.
kMeans <- kmeans(imagemRGB[, c("R", "G", "B")], centers = k,iter.max = 30)


#A linha abaixo adiciona localmente (sem alterar o objeto original) uma
#coluna de cores em hexadecimal obtidas a partir da função rgb().
#kMeans$center contém os centróides de cada grupo enquanto kMeans$cluster
#contém a qual grupo cada píxel pertence.
imagemRGB %>% mutate(kColours = rgb(kMeans$centers[kMeans$cluster,])) %>%
  #Estas linhas especificam que estamos construindo uma imagem a partir de
  #um gráfico de pontos, coloridos usando os valores em hexadecimal da
  #coluna kColours.
  ggplot(aes(x = x, y = y, color = I(kColours))) +
  geom_point(show.legend = FALSE) +
  #Por fim, estas linhas incluem uma legenda na imagem e especificam
  #as dimensões do gráfico.
  labs(title = paste("k-Means (k =", k, "cores)")) +
  theme_minimal()


