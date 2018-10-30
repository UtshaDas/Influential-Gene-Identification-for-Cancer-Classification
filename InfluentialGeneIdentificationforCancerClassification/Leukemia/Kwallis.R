data <- read.csv("F:/Thesis/Leukemia/leukemia_small_classified_final.csv")
alpha <- 0.05

cnt <- 0

names <- vector('character')
pvals <- vector('numeric')
row=nrow(data)
col=ncol(data)
se=data[1,2];
for (i in 1:nrow(data)){
  vec1 = as.numeric(data[i:i,1:47])
  vec2 = as.numeric(data[i:i,48:72])
  
  vec1 = scale(vec1)
  vec2 = scale(vec2)
  
  tt = kruskal.test(list(vec1, vec2))
  names <- c(names, row.names(data)[i])
  pvals <- c(pvals, tt$p.value)
}

padjusted = p.adjust(pvals, method = "bonferroni")

res = data.frame("Gene"=names, "p-value"=pvals, "Adjusted p-value"=padjusted)

kept = padjusted < 0.05

res2 = data.frame("Gene"=names[kept], "p-value"=pvals[kept], "Adjusted p-value"=padjusted[kept])

cat(nrow(res2))

write.csv(res, file="F:/Thesis/Leukemia/leukemia_small_kruskal_test_result_all_new.csv")
write.csv(res2, file="F:/Thesis/Leukemia/leukemia_small_kruskal_test_result_new.csv")

