## 0. Preparation
rm(list = ls(all = TRUE))
graphics.off()

libraries = c("dplyr", "RColorBrewer", "goft")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

wdir = "/Users/LvB/Documents/GitHub/FRM_Expectile"
setwd(wdir)

FRM_history = readRDS("FRM_expectile_0.05_SP500.rds")


N_h = length(FRM_history )
stock_names = vector()
for (t in 1:N_h) stock_names = c(stock_names, attributes(FRM_history[[t]])$dimnames[[2]])
stock_names = unique(stock_names)
N_names = length(stock_names)
lambdas_wide = matrix(0, N_h, N_names+1)
lambdas_wide[, 1] = names(FRM_history)
for (k in 1:N_names) 
  for (t in 1:N_h) 
    if (stock_names[k] %in% attributes(FRM_history[[t]])$dimnames[[2]]) 
      lambdas_wide[t, k+1] = FRM_history[[t]][, stock_names[k]]
colnames(lambdas_wide) = c("date", stock_names)

#FRM index
FRM_index = sapply(1:N_h, function(i) mean(FRM_history[[i]]))
FRM_index = data.frame(date = names(FRM_history), FRM = FRM_index)

# Market cap weighted FRM index
mktcap_hm =  read.csv("mktcap_order_index.csv", header = FALSE)
J=100
FRM_mdex = matrix(0,N_h,2)
FRM_mdex[,1]= mktcap_hm [,1]
for (i in (1:N_h)){
  FRM_mdex[i,2] = FRM_history[[i]] %*% t(as.matrix( mktcap_hm[i,1:J])) / sum(mktcap_hm[i,1:J])
}


#Daily maximum
FRM_max = sapply(1:N_h, function(i) max(FRM_history[[i]]))
name_max = sapply(1:N_h, function(i) 
  attributes(FRM_history[[i]])$dimnames[[2]][which(FRM_history[[i]] == FRM_max[i])[1]])
FRM_max = data.frame(date = names(FRM_history), name = name_max, lambda = FRM_max)


#Quantiles
quantiles = c(0.90, 0.75, 0.70, 0.50, 0.25)
for (q in quantiles) {
  FRM_q = sapply(1:N_h, function(i) quantile(FRM_history[[i]], q))
  FRM_q = data.frame(date = names(FRM_history), quantile = FRM_q)
  write.csv(FRM_q, paste0("q", q*100, "_index.csv"), 
            row.names = FALSE, quote = FALSE)
}

#boxplot
names(FRM_history)[1] = "20190601"
names(FRM_history)[176] = "20200201"
plot_labels_m = c(20190601, 20191001, 20200201, 20200601, 20201001, 20210128)
png(paste0("Boxplot.png"), width = 900, height = 600, bg = "transparent")
boxplot(FRM_history, col = "white", xaxt = "n")
lines(tail(FRM_index$FRM, N_h), col = "blue", lwd = 2)
lines(tail(FRM_max$lambda, N_h), col = "red", lwd = 2)
ll = which(names(FRM_history) %in% plot_labels_m)
axis(1, at = ll, labels = plot_labels_m)
dev.off()

#FRM index family
q25= read.csv("q25_index.csv")
q50= read.csv("q50_index.csv")
q70= read.csv("q70_index.csv")
q90= read.csv("q90_index.csv")
qfrm=read.csv("FRM_quantile_index.csv") 
comparison_data = cbind(FRM_index,FRM_mdex[,2])
comparison_data = cbind(comparison_data, q25[,2])
comparison_data = cbind(comparison_data, q50[,2])
comparison_data = cbind(comparison_data, q70[,2])
comparison_data = cbind(comparison_data, q90[,2])
comparison_data = cbind(comparison_data, qfrm[,2])
comparison_data$date =as.Date(comparison_data$date,  "%Y%m%d")
p1=ggplot(comparison_data, aes(x=date)) +
  ggtitle("FRM index family") +
  scale_x_date(date_labels = "%Y-%m",breaks='2 month',expand = c(0, 0)) +
  geom_line(aes(y = comparison_data[,3]), color="dimgrey") +
  geom_line(aes(y = comparison_data[,4]), color="black") +
  geom_line(aes(y = comparison_data[,5]), color="orange") +
  geom_line(aes(y = comparison_data[,6]), color="violet") +
  geom_line(aes(y = comparison_data[,7]), color="red") +
  geom_line(aes(y = comparison_data[,8]), color="darkgreen") +
  geom_line(aes(y = comparison_data[,2]), color="steelblue") +
  theme( panel.grid=element_blank(),
         panel.border = element_rect(color="black",fill = NA),
         legend.position = "none",
         axis.title = element_blank(),
         plot.title = element_text(hjust = 0.5),
         panel.background = element_rect(fill = "transparent",colour = NA),
         plot.background = element_rect(fill = "transparent",colour = NA),
         legend.box.background = element_rect(fill = "transparent"),
         axis.text.x = element_text(angle = 45, hjust = 1,size=14, face= "bold"),
         axis.text.y = element_text(size=14, face= "bold" ) )

png(paste0("Index_family.png"),width = 500, height = 450,bg = "transparent") 
p1
dev.off()

## 4.kernel density estimation
cols = brewer.pal(9, "Set1")
d1= as.numeric (lambdas_wide[10,-1]) 
d2= as.numeric (lambdas_wide[31,-1])
d3= as.numeric (lambdas_wide[177,-1])   
d4= as.numeric (lambdas_wide[228,-1])
d5= as.numeric (lambdas_wide[248,-1])
d6= as.numeric (lambdas_wide[267,-1])
d7= as.numeric (lambdas_wide[115,-1])
d8= as.numeric (lambdas_wide[327,-1])
d9= as.numeric (lambdas_wide[372,-1])
d10= as.numeric (lambdas_wide[416,-1])

png(paste0("Density.png"),width = 500, height = 450,
    bg = "transparent")  #width = 900, height = 600,  #paper width = 400, height = 350,

plot(density(d1, na.rm = T, kernel = "gaussian"), type = "l", ylim = c(0, 70), xlim = c(0, 0.2), main = "", xlab = "",
     cex.lab = 1.5, cex.axis = 1.5, lwd = 3)

lines(density(d2, na.rm = T, kernel = "gaussian"), type = "l", col = cols[1], lwd = 3)
lines(density(d3, na.rm = T, kernel = "gaussian"), type = "l", col = cols[2], lwd = 3)
lines(density(d4, na.rm = T, kernel = "gaussian"), type = "l", col = cols[3], lwd = 3)
lines(density(d5, na.rm = T, kernel = "gaussian"), type = "l", col = cols[4], lwd = 3)
lines(density(d6, na.rm = T, kernel = "gaussian"), type = "l", col = cols[5], lwd = 3)
lines(density(d7, na.rm = T, kernel = "gaussian"), type = "l", col = cols[6], lwd = 3)
lines(density(d8, na.rm = T, kernel = "gaussian"), type = "l", col = cols[7], lwd = 3)
lines(density(d9, na.rm = T, kernel = "gaussian"), type = "l", col = cols[8], lwd = 3)
lines(density(d10, na.rm = T, kernel = "gaussian"), type = "l", col = cols[9], lwd = 3)
dev.off()








