library(gplots)
library(viridis)

data <- read.csv("F:/Thesis/Leukemia/UpDown_Leukemia.csv", row.names=1)


names_up <- vector('character')
relativec_up <- vector('numeric')
names_down <- vector('character')
relativec_down <- vector('numeric')

data_ = as.matrix(data)

for (i in 1:nrow(data)){
  vec1 = as.numeric(data[i:i,1:47])
  vec2 = as.numeric(data[i:i,48:72])
  
  vec1 = scale(vec1)
  vec2 = scale(vec2)
  
  m1 = mean(vec1)
  m2 = mean(vec2)
    #print(i)
    #print(m1)
    #print(m2)

  
  relative_change=m1/(m1+m2)
  
  print(relative_change)
  if (relative_change > 0.5)
  {
    names_up <- c(names_up, row.names(data)[i])
    relativec_up <- c(relativec_up, relative_change)
  }
  else
  {
    names_down <- c(names_down, row.names(data)[i])
    relativec_down <- c(relativec_down, relative_change)
  }
  
  
}


resUp = data.frame("Genes"=names_up, "relativechange Value"=relativec_up)
resDown = data.frame("Genes"=names_down, "relativechange Value"=relativec_down)

names = vector('character')
relativec = vector('numeric')

for (i in 1:length(names_up))
{
  names = c(names, names_up[i])
  relativec = c(relativec, relativec_up[i])
}
for (i in 1:length(names_down))
{
  names = c(names, names_down[i])
  relativec = c(relativec, relativec_down[i])
}

concol = vector('character')

for (j in 1:ncol(data_))
{
  if (j <= 47)
    concol = c(concol, '#f92c32')
  else 
    concol = c(concol, '#127cd4')  
}  

heatmap.2(data_, main="Influential Genes", trace="none", margins=c(4,14), density="none", 
          cexRow=1, cexCol=0.2, col=viridis(100), ColSideColors=concol)
legend(0.82,1.22,legend=c("ALL","AML"),fill=c('#f92c32','#127cd4'),cex=0.7)

write.csv(resUp, file="F:/Thesis/Leukemia/upregulated.csv")
write.csv(resDown, file="F:/Thesis/Leukemia/downregulated.csv")
