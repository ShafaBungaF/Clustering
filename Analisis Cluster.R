# Load Library

#```{r message=FALSE, warning=FALSE}
library(knitr)
install.packages("REdaS")
library(REdaS)
library(factoextra) 
library(clValid)
library(tidyverse)
library(cluster)
#```

# Baca dataku

#```{r}
dataku <- read.csv(file.choose(), header = T, sep = ";")
View(dataku)
boxplot(dataku[,2:9])
summary(dataku)

# Pengecekan asumsi

bart_spher(dataku[,2:9])
kmo(dataku[,2:9])

cor(dataku[,2:9])

#menghitung vektor eigen dan nilai eigen

R <- cor(dataku[,2:9])
eigen<- eigen(R)
eigen
eigen$values #eigen dapat 2

a = scale(dataku[,2:9])
a
#Melakukan PCA
pcadata<- prcomp(x = dataku[,2:9], scale. = TRUE, center = TRUE) 
View(pcadata)

#menentukan jumlah komponen utama
summary(pcadata)

#persamaan komponen utama
round(pcadata$rotation,2) #loading data
round(pcadata$sdev^2,2) #eigen

fviz_pca(pcadata) #Visualisasi data hasil rekonstruksi

#data pca fix
PCA_scoresbaru <- pcadata$x[,1:2]
View(PCA_scoresbaru)

distance <- get_dist(PCA_scoresbaru) #jarak antara obyek yang satu dengan yang lain
distance
summary(distance)

#k-medoids

library(tidyverse) # dataku manipulation
library(cluster) # clustering algorithms
library(factoextra) # clustering algorithms & visualization

# pamk (penentuan jumlah cluster)
library(fpc)
pamk.result <- pamk(PCA_scoresbaru)
pamk.result
pamk.result$nc

#menampilkan grafik sillhouette
fviz_nbclust(PCA_scoresbaru, pam, method = "silhouette")
pam.hasil  <- pam(PCA_scoresbaru, 2)

#jarak
pam.hasil$diss

#datakuframe hasil cluster
df.clusterbaruuuu = data.frame(PCA_scoresbaru,pam.hasil$cluster)
View(df.clusterbaruuuu)

#Clustering
#sesuai abjad
table(pam.result$clustering, dataku$Kepolisian)

#plot cluster
fviz_cluster(pam.hasil, data = PCA_scoresbaru)

# Validasi Cluster
intern <- clValid(PCA_scoresbaru, 2:5, clMethods = c("hierarchical", "pam"), validation = "internal")
summary(intern)
optimalScores(intern)

# Dendogram 

#single linkage
data.hcc1 <- PCA_scoresbaru %>%      
  dist(method = "euclidean") %>% 
  hclust(method = "single")
fviz_dend(data.hcc1, k = 2, 
          cex = 0.6, palette = "jco", 
          rect = TRUE, rect_border = "jco", rect_fill = TRUE,
          main = "Cluster Dendogram Single Linkage")

#complete linkage
data.hcc2 <- PCA_scoresbaru %>%      
  dist(method = "euclidean") %>% 
  hclust(method = "complete")
fviz_dend(data.hcc2, k = 2, 
          cex = 0.6, palette = "jco", 
          rect = TRUE, rect_border = "jco", rect_fill = TRUE,
          main = "Cluster Dendogram Compelete Linkage")

#average
data.hcc3 <- PCA_scoresbaru %>%      
  dist(method = "euclidean") %>% 
  hclust(method = "average")
fviz_dend(data.hcc3, k = 2, 
          cex = 0.6, palette = "jco", 
          rect = TRUE, rect_border = "jco", rect_fill = TRUE,
          main = "Cluster Dendogram Average Linkage")

#ward
data.hcc4 <- PCA_scoresbaru %>%      
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2")
fviz_dend(data.hcc4, k = 2, 
          cex = 0.6, palette = "jco", 
          rect = TRUE, rect_border = "jco", rect_fill = TRUE,
          main = "Cluster Dendogram (Ward's Method)")


#pemilihan metode terbaik
m <- c("single","complete","ward", "average")
names(m) <- c("single","complete","ward", "average") 
ac <- function(x){
  agnes(PCA_scoresbaru,method=x)$ac
}
map_dbl(m,ac)


# Deskripsi Statistik Klaster k-meodids
clust1 <- dataku[-c(2, 6, 11, 15, 27, 34 ),]
summary(clust1)
clust2 <- dataku[c(2, 6, 11, 15, 27, 34),]
summary(clust2)

# Deskripsi Statistik Klaster hierarki
clust1_hieararki <- dataku[-c(22, 6, 11, 15, 27),]
summary(clust1)
clust2_hierarki <- dataku[c(2, 6, 11, 15, 27),]
summary(clust2)
