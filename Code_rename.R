# 安装并加载所需的包
packages <- c("haven", "dplyr", "ggplot2", "bootnet", "qgraph", "NetworkComparisonTest",
              "dplyr", "stringr", "networktools", "igraph", "mice", "lavaan", 
              "glmnet", "missForest", "readxl", "ltm", "psych", "e1071", "Hmisc")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# 清空工作空间
rm(list = ls())


###################################网络分析####################################
# 读取 Excel 文件
datax <- read_sav('F:/scientific research/08Manuscript with author details/T1T2母子追踪921T1T2完整有效家庭维度分.sav')
print(names(datax))
# 使用关键词筛选列
keywords <- "MQ21|CQ20|CQ96"
columns_s <- grep(keywords, names(datax), value = TRUE)

# 创建一个子集数据框
data <- datax[, columns_s]

# 确保数据框格式正确
data <- as.data.frame(data)

# 检查列名
print(names(data))
#data <- data[,-c(88:91)]
print(names(data))
# # 删除 "T3" 开头的列
# data <- data[, !grepl("^L2", names(data))]
# data <- data[, !grepl("^W2", names(data))]
# # 重命名其他列
# names(data) <- names(data) %>%
#   str_replace("^B_", "T1_") %>%
#   str_replace("^A_", "T1_") %>%
#   str_replace("^L2_", "T2_") %>%
#   str_replace("^W2_", "T2_") %>%
#   str_replace("网络游戏障碍量表", "网络游戏障碍") %>%
#   str_replace("社交焦虑量表", "社交焦虑")

# 查看修改后的列名
print(names(data))

t1_respondents <- sum(!is.na(data$T1MQ21_Row1))  # 计算 T1 时间点作答的人数
t2_respondents <- sum(!is.na(data$T2MQ21_Row4))  # 计算 T2 时间点作答的人数
t1_respondents  # 显示 T1 时间点的作答人数
t2_respondents  # 显示 T2 时间点的作答人数

dput(colnames(data))

summary(data)

mydata <- subset(data,select = c("T1MQ21_Row1M", "T1MQ21_Row2M", "T1MQ21_Row3M", "T1MQ21_Row4M", 
                                 "T1MQ21_Row5M", "T1MQ21_Row6M", "T1MQ21_Row7M", "T1MQ21_Row8M", "T1MQ21_Row9M", 
                                 "T1CQ20_Row1C", "T1CQ20_Row2C", "T1CQ20_Row3C", "T1CQ20_Row4C", "T1CQ20_Row5C", 
                                 "T1CQ20_Row6C", "T1CQ20_Row7C", "T1CQ20_Row8C", "T1CQ20_Row9C", "T2MQ21_Row1M", 
                                 "T2MQ21_Row2M", "T2MQ21_Row3M", "T2MQ21_Row4M", "T2MQ21_Row5M", "T2MQ21_Row6M", 
                                 "T2MQ21_Row7M", "T2MQ21_Row8M", "T2MQ21_Row9M", "T2CQ96_Row1C", "T2CQ96_Row2C", 
                                 "T2CQ96_Row3C", "T2CQ96_Row4C", "T2CQ96_Row5C", "T2CQ96_Row6C", "T2CQ96_Row7C", 
                                 "T2CQ96_Row8C", "T2CQ96_Row9C"))

data_c <- mydata[complete.cases(mydata),]
nrow(data_c) 
dput(colnames(data_c))
data_r <- data_c


old_n <- colnames(data_r)

# 更新列名
colnames(data_r) <- gsub("MQ21_Row", "MDS", colnames(data_r))
colnames(data_r) <- gsub("CQ20_Row", "CDS", colnames(data_r))
colnames(data_r) <- gsub("CQ96_Row", "CDS", colnames(data_r))

# 计算每列的偏度
skewness_values <- apply(data_r, 2, skewness)

# 计算每列的峰度
kurtosis_values <- apply(data_r, 2, kurtosis)

# 打印结果
print(skewness_values)
print(kurtosis_values)

# 使用 round() 函数保留两位小数
skewness_values_rounded <- round(skewness_values, 2)
kurtosis_values_rounded <- round(kurtosis_values, 2)

# 打印保留两位小数的结果
print(skewness_values_rounded)
print(kurtosis_values_rounded)



# 方法1：基础 R
means <- colMeans(data_r, na.rm = TRUE)
sds <- apply(data_r, 2, sd, na.rm = TRUE)

means_rounded <- round(means, 2)
sds_rounded <- round(sds, 2)

summary_df <- data.frame(
  Variable = names(means_rounded),
  Mean = means_rounded,
  SD = sds_rounded
)

print(summary_df)

# 保存为 CSV
write.csv(summary_df, "./峰度偏度.csv", row.names = FALSE)


groups <- c(rep("MDS",9),rep("CDS",9))
colors <- c(rep("#E69F00", 9),  
            rep("#009E73", 9)) 


data_r <- as.data.frame(data_r)


# 创建情绪词汇列表
MDS <- c("Anhedonia",
         "Sad Mood",
         "Sleep",
         "Energy",
         "Appetite",
         "Guilty",
         "Concentration",
         "Motor",
         "Suicide ideation"
)

CDS <- c("Anhedonia",
         "Sad Mood",
         "Sleep",
         "Energy",
         "Appetite",
         "Guilty",
         "Concentration",
         "Motor",
         "Suicide ideation"
)

label_mds <- paste0("MDS",1:9,":",MDS)
label_cds <- paste0("CDS",1:9,":",CDS)

#create a vector of variable labels for the figure
labels  <- c(paste0("MDS", 1:9), paste0("CDS", 1:9))
brieflegend <- c(label_mds,label_cds
)


dput(colnames(data_r))
data1 <- subset(data_r,select = c("T1MDS1M", "T1MDS2M", "T1MDS3M", "T1MDS4M", "T1MDS5M", "T1MDS6M", 
                                  "T1MDS7M", "T1MDS8M", "T1MDS9M", "T1CDS1C", "T1CDS2C", "T1CDS3C", "T1CDS4C", 
                                  "T1CDS5C", "T1CDS6C", "T1CDS7C", "T1CDS8C", "T1CDS9C"))
data2 <- subset(data_r,select = c("T2MDS1M", "T2MDS2M", 
                                  "T2MDS3M", "T2MDS4M", "T2MDS5M", "T2MDS6M", "T2MDS7M", "T2MDS8M", "T2MDS9M", 
                                  "T2CDS1C", "T2CDS2C", "T2CDS3C", "T2CDS4C", "T2CDS5C", "T2CDS6C", "T2CDS7C", 
                                  "T2CDS8C", "T2CDS9C"))
colnames(data1) <-  sub("T1", "", colnames(data1))
colnames(data2) <-  sub("T2", "", colnames(data2))
?estimateNetwork()
network1 <- estimateNetwork(data1, default="EBICglasso",corMethod="spearman",tuning = 0.5,labels = labels)

# Number of nodes: 18 
# Number of non-zero edges: 84 / 153 
# Mean weight: 0.05167217 


network2 <- estimateNetwork(data2, default="EBICglasso",corMethod="spearman",tuning = 0.5,labels = labels)

# Number of nodes: 18 
# Number of non-zero edges: 81 / 153 
# Mean weight: 0.05400496 

myedges1 <- getWmat(network1)
write.csv(myedges1,"T1网络边系数.csv")
summary(network1)

myedges2 <- getWmat(network2)
write.csv(myedges2,"T2网络边系数.csv")
summary(network2)

library(dplyr)

# 提取下三角矩阵
lower_tri1 <- myedges1 
lower_tri1[upper.tri(lower_tri1, diag=TRUE)] <- NA

# 输出结果
lower_tri1

# 提取下三角矩阵
lower_tri2 <- myedges2 
lower_tri2[upper.tri(lower_tri2, diag=TRUE)] <- NA

# 输出结果
lower_tri2

#对边进行大小排序
flatten_and_sort <- function(matrix) {
  as.data.frame(as.table(matrix)) %>%
    dplyr::filter(Var1 != Var2) %>% # Remove self-loops
    dplyr::mutate(AbsoluteWeight = abs(Freq)) %>%
    dplyr::arrange(desc(AbsoluteWeight)) %>%
    dplyr::select(Var1, Var2, Freq, AbsoluteWeight) # Keep the absolute weight for reference
}

# Apply the function to both edge matrices
sorted_edges1 <- flatten_and_sort(lower_tri1)
sorted_edges2 <- flatten_and_sort(lower_tri2)

# Write the sorted edges to CSV files
write.csv(sorted_edges1, "T1网络边系数排序.csv", row.names = FALSE)
write.csv(sorted_edges2, "T2网络边系数排序.csv", row.names = FALSE)


L <- averageLayout(network1,network2)
#pdf("t1t2横断图有标签2.pdf",width=16,height=6)
png("t1t2横断网络图有标签.png", width=4500, height=2400, res=400) 
layout(mat = matrix(c(1,2,3),nrow = 1), widths = c(1.5,1.5,1),heights = 2)
#par(mfrow=c(1,2))
Network1G <- plot(network1,layout = L,title="T1", groups=groups,edge.labels=TRUE,edge.label.cex = 0.9,edge.label.color= "black", minimum=0.02,cut=0,
                 vsize=9, legend=FALSE,borders=TRUE,color = colors,palette="colorblind",labels = labels,#minimum=0.03, cut=0,
                  label.cex=1,title.cex=1.2)#输出Time 1的网络结构图
Network2G <- plot(network2,layout = L,title="T2", groups=groups,edge.labels=TRUE,edge.label.cex = 0.9,edge.label.color= "black", minimum=0.02,cut=0,
                 vsize=9, legend=FALSE,borders=TRUE,color = colors,palette="colorblind",labels = labels,
                  label.cex=1,title.cex=1.2)#输出Time 2的网络结构图
#title(LabwArrow.t1.t2, line=3, cex.main=1.75)
plot.new()
par(mar = rep(0, 4))
legend(x = "center", inset = c(0, 0), bty = "n", legend = brieflegend,  
       col = colors, y.intersp = 2,
       pch = 19, cex=1.2)
dev.off()

L <- averageLayout(network1,network2)
#pdf("t1t2横断图有标签2.pdf",width=16,height=6)
png("t1t2横断网络图无标签.png", width=4500, height=2400, res=400) 
layout(mat = matrix(c(1,2,3),nrow = 1), widths = c(1.5,1.5,1),heights = 2)
#par(mfrow=c(1,2))
Network1G <- plot(network1,layout = L,title="T1", groups=groups,edge.labels=FALSE,edge.label.cex = 0.9,edge.label.color= "black", minimum=0.02,cut=0,
                  vsize=9, legend=FALSE,borders=TRUE,color = colors,palette="colorblind",labels = labels,#minimum=0.03, cut=0,
                  label.cex=1,title.cex=1.2)#输出Time 1的网络结构图
Network2G <- plot(network2,layout = L,title="T2", groups=groups,edge.labels=FALSE,edge.label.cex = 0.9,edge.label.color= "black", minimum=0.02,cut=0,
                  vsize=9, legend=FALSE,borders=TRUE,color = colors,palette="colorblind",labels = labels,
                  label.cex=1,title.cex=1.2)#输出Time 2的网络结构图
#title(LabwArrow.t1.t2, line=3, cex.main=1.75)
plot.new()
par(mar = rep(0, 4))
legend(x = "center", inset = c(0, 0), bty = "n", legend = brieflegend,  
       col = colors, y.intersp = 2,
       pch = 19, cex=1.2)
dev.off()


network1$graph
network2$graph
centrality1 <- centrality_auto(network1)
nc1 <- centrality1$node.centrality
write.csv(nc1, file = "t1网络中心性指标.csv")
centrality2 <- centrality_auto(network2)
nc2 <- centrality2$node.centrality
write.csv(nc2, file = "t2网络中心性指标.csv")
png("t1t2中心性指标对比图.png", width=1100, height=2000, res=300) 
#pdf("t1t3中心性对比图.pdf",width=4,height=10)
centralityPlot(list(T1 = network1, T2 = network2), 
               decreasing = TRUE, include = c("ExpectedInfluence"))
dev.off()

c <- bridge(myedges1,communities=groups,useCommunities = "all",directed=NULL,nodes=NULL)
d <- bridge(myedges2,communities=groups,useCommunities = "all",directed=NULL,nodes=NULL)

### Visualize bridge indices in the same graph
plot.bridge.Sam <- function(x,x2, x_name, x2_name,include, z)
{
  attr(x, "class") <- NULL
  attr(x2, "class") <- NULL
  
  nodes <- names(x[[1]])
  comm <- x$communities; commcol <- vector()
  cols <- rep("black", length(comm))
  x$communities <- NULL
  x2$communities <-NULL
  if(z)
  {
    scalenoatt <- function(y){
      y <- scale(y)
      attr(y, "scaled:center") <- NULL
      attr(y, "scaled:scale") <- NULL
      return(y)
    }
  }else
  {
    scalenoatt <- function(y){
      return(y)
    }
  }
  x <- sapply(x, scalenoatt)
  x2 <- sapply(x2, scalenoatt)
  
  Long <- reshape2::melt(x); colnames(Long)[2] <- "measure"
  Long2 <- reshape2::melt(x2); colnames(Long2)[2] <- "measure"
  
  Long$type <- rep(x_name, nrow(Long))
  Long2$type <- rep(x2_name, nrow(Long2))
  
  Long$node <- rep(nodes, length(unique(Long$measure)))
  Long2$node <- rep(nodes, length(unique(Long2$measure)))
  
  Long <- subset(Long, Long$measure %in% include)
  Long2 <- subset(Long2, Long2$measure %in% include)
  
  #changed to match centralityPlot
  Long <- Long[gtools::mixedorder(Long$node),] 
  Long2 <- Long2[gtools::mixedorder(Long2$node),]
  
  Long$node <- factor(as.character(Long$node), levels = unique(gtools::mixedsort(as.character(Long$node))))
  Long2$node <- factor(as.character(Long2$node), levels = unique(gtools::mixedsort(as.character(Long2$node))))
  
  g <- ggplot2::ggplot()
  g <- g + ggplot2::geom_path(Long, mapping = ggplot2::aes_string(x='value', y='node', group='type', colour = 'type')) + 
    ggplot2::geom_point(Long, mapping = ggplot2::aes_string(x='Long$value', y='node', group='type',colour = 'type')) + 
    ggplot2::xlab("") + ggplot2::ylab("") + 
    ggplot2::geom_path(Long2, mapping = ggplot2::aes_string(x='value', y='node', group='type',color = 'type')) + 
    ggplot2::geom_point(Long2, mapping = ggplot2::aes_string(x='value', y='node', group='type',color = 'type')) +
    ggplot2::facet_grid('~measure', scales="free") + ggplot2::scale_y_discrete(limits = rev(levels(Long$node))) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(colour=cols[order(nodes, decreasing=T)])) + theme_bw() +
    ggplot2::labs(color = "type")
  return(g)
}

bridge_Bridge1 <- c$"Bridge Expected Influence (1-step)"
bridge_Bridge1
bridge_Bridge2 <- d$"Bridge Expected Influence (1-step)"
bridge_Bridge2

write.csv(bridge_Bridge1, file = "t1网络桥指标.csv")
write.csv(bridge_Bridge2, file = "t2网络桥指标.csv")

png("t1t2桥预期对比图.png", width=1100, height=2000, res=300) 
#pdf("t1t3桥强度对比.pdf", width = 4, height =10)
plot.bridge.Sam(x = c, x2 = d,
                x_name = "T1", x2_name = "T2", 
                include=c("Bridge Expected Influence (1-step)"), z=TRUE)#
dev.off()


boot1n <- bootnet(network1, nCores = 8, nBoots = 1000, 
                  type = "nonparametric",statistics=c('bridgeExpectedInfluence','expectedInfluence',"edge"), communities=groups)
boot2n <- bootnet(network2, nCores = 8, nBoots = 1000, 
                  type = "nonparametric",statistics=c('bridgeExpectedInfluence','expectedInfluence',"edge"), communities=groups)

png("t1网络边区间.png", width=5500, height=5500, res=250) 
#pdf("t1边区间.pdf",width = 50,height = 50)#保存结果图片
plot(boot1n,lables=FALSE,order="sample")#边线权重的置信区间；order=sample用于设定边线权重从
dev.off()
## Central and BridgeEI Stability
pdf("t1网络预期差异.pdf", height=10, width=10)
plot(boot1n, "expectedInfluence",plot ="difference")
dev.off()
## Edge Difference
pdf("t1网络边差异.pdf", height = 50, width = 50)
plot(boot1n, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
dev.off()
pdf("t1网络桥预期差异.pdf", height=10, width=10)
plot(boot1n, "bridgeExpectedInfluence",plot ="difference")
dev.off()


png("t2网络边区间.png", width=5500, height=5500, res=250) 
plot(boot2n,lables=FALSE,order="sample")#边线权重的置信区间；order=sample用于设定边线权重从
dev.off()
## Central and BridgeEI Stability
pdf("t2网络预期差异.pdf", height=10, width=10)
plot(boot2n, "expectedInfluence",plot ="difference")
dev.off()
## Edge Difference
pdf("t2网络边差异.pdf", height = 50, width = 50)
plot(boot2n, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
dev.off()
pdf("t2网络桥预期差异.pdf", height=10, width=10)
plot(boot2n, "bridgeExpectedInfluence",plot ="difference")
dev.off()




boot1g <- bootnet(network1, nBoots = 1000,
                  statistics = c ("bridgeExpectedInfluence","expectedInfluence", "edge"),
                  type = "case", nCores = 8, communities=groups)
boot2g <- bootnet(network2, nBoots = 1000,
                  statistics = c ("bridgeExpectedInfluence","expectedInfluence","edge"),
                  type = "case", nCores = 8, communities=groups)
corStability(boot1g)

# bridgeExpectedInfluence: 0.283
# - For more accuracy, run bootnet(..., caseMin = 0.205, caseMax = 0.362)
# 
# edge: 0.75 (CS-coefficient is highest level tested)
# - For more accuracy, run bootnet(..., caseMin = 0.672, caseMax = 1)
# 
# expectedInfluence: 0.75 (CS-coefficient is highest level tested)
# - For more accuracy, run bootnet(..., caseMin = 0.672, caseMax = 1)

corStability(boot2g)

# bridgeExpectedInfluence: 0.205 
# - For more accuracy, run bootnet(..., caseMin = 0.128, caseMax = 0.283) 
# 
# edge: 0.75 (CS-coefficient is highest level tested)
# - For more accuracy, run bootnet(..., caseMin = 0.672, caseMax = 1) 
# 
# expectedInfluence: 0.75 (CS-coefficient is highest level tested)
# - For more accuracy, run bootnet(..., caseMin = 0.672, caseMax = 1) 

png("t1网络稳定性图.png", width=1800, height=1800, res=400) 
#pdf("稳定性图t1.pdf",width=6,height=6)
plot(boot1g, statistics=c("expectedInfluence","bridgeExpectedInfluence"))
dev.off()
png("t2网络稳定性图.png", width=1800, height=1800, res=400) 
plot(boot2g, statistics=c("expectedInfluence","bridgeExpectedInfluence"))
dev.off()
# ?NCT()
# nct_12 <- NCT(data1, data2, gamma = 0.5, it = 1000,
#               binary.data=FALSE, weighted = TRUE, progressbar = TRUE,
#               test.edges=TRUE, test.centrality=TRUE,paired = TRUE,
#               centrality=c('closeness', 'betweenness', 'strength', 'bridgeStrength'),nodes="all",communities=groups)#fz2,fz3 是对比的
# nct_12$nwinv.pval#查看整个网络在边线权重是否存在显著性差异:P值
# #(2) invariant global strength
# nct_12$glstrinv.pval#查看网络在整体强度上是否存在显著性异:P值
# #(3) global strength value estimation
# nct_12$glstrinv.sep #查看两个网络在整体强度上的具体值
# #(4) quantification of differences: count significantly different edges
# nct_12$einv.pvals###查看网络在边线权重上置换检验是否存在显著性差异: P值
# write.csv(nct_12$einv.pvals, file = "边线置换检验.csv")
# sum(nct_12$einv.pvals$"p-value"< 0.05)#查看P值小于.05的数量
# #(5) differences in centralities: p-value
# nct_12$diffcen.pval#查看网络在中心指标强度上置换检验是否存在显著性差异:P值
# sum(nct_12$diffcen.pval< 0.05)#查看P值小于.05的数量
# plot(nct_12, what="network")
# plot(nct_12, what="centrality")


k <- 18 #k is the number of variables at each time point
adjMat <- matrix(0, k, k) #set up empty matrix of coefficients
rsquarelist <- rep(0, k)
#the following code loops the regularized regression through each
#variable at time 2 as a DV, regressing on each variable at time 1
#as predictors. It assumes that the first k columns in your data
#correspond to the variables at time 1 and the next k correspond
#to the variables at time 2. These variables must be in the same 
#order at each time point.
#please see the paper for technical details

for (i in 1:k){
  set.seed(100)
  lassoreg <- cv.glmnet(as.matrix(data_r[,1:k]), data_r[,(k+i)], 
                        nfolds=10,family = "gaussian", alpha = 1, standardize=TRUE)
  lambda <- lassoreg$lambda.min
  #  rsquare[i] <- lassoreg$glmnet.fit$dev.ratio[which(lassoreg$lambda == lambda)]
  adjMat[1:k,i] <- coef(lassoreg, s = lambda, exact = FALSE)[2:(k+1)]
}
#################################################################
adjMat
adjMat2 <- adjMat
diag(adjMat2) <- 0 
str(adjMat)
str(adjMat2)
sum(adjMat!=0)# 112
sum(adjMat2!=0) # 100

# 安装并加载 writexl 包
if (!requireNamespace("writexl", quietly = TRUE)) {
  install.packages("writexl")
}
library(writexl)

# 假设 adjMat 是你的矩阵
# 将矩阵转换为数据框
adjMat_df <- as.data.frame(adjMat)

# 查看转换后的数据框（可选）
print(adjMat_df)

# 将数据框保存为 Excel 文件
write_xlsx(list(Adjacency_Matrix = adjMat_df), path = "纵向网络边系数.xlsx")



LabwArrow <- expression("T1" %->% "T2 Network") 

ARedgesT1T2 <- diag(adjMat)

shortnames = labels
ARedges <- data.frame(Factors=rep(factor(shortnames, levels=rev(shortnames)), 1),
                      Class=rep(groups, 1),
                      Network=c(rep("T1 -> T2 Network", k)),
                      ARedge=c(ARedgesT1T2)
)
ARedges$Network <- factor(ARedges$Network, levels=c("T1 -> T2 Network"))
write.csv(ARedges,"自回归边系数.csv")

png("纵向自回归指标图.png", width=1400, height=1800, res=300) 
#pdf("自回归指标.pdf", width = 10, height = 10)
ggplot(ARedges, aes(x=Factors, y=ARedge, group=Network)) +
  geom_path(aes(lty=Network)) +
  geom_point(aes(group=Network, col=Class, fill=Class), shape=21, size=3) +
  coord_flip() +
  ylab("Autoregressive Edge") + xlab("Nodes") + 
  # labs(title="Autoregressive edges") +
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73')) +
  scale_color_manual(values=c('#E69F00', '#56B4E9', '#009E73')) +
  # scale_y_continuous(expand = c(0, 0), limits = c(0,2)) + # to make bars start at 0
  guides(lty=guide_legend(order=1), # set the order of the legends
         fill=guide_legend(order=2), 
         col="none") +
  theme_bw() +
  theme(text=element_text(color="black"),
        plot.title=element_text(size=14, color="black", hjust=.5),
        axis.text=element_text(size=12, color="black"),
        axis.title=element_text(size=14, color="black"),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12, color="black"))
dev.off()

## mean of AR edges
mean(ARedgesT1T2) #0.2659268

mean(c(adjMat[lower.tri(adjMat)], 
       adjMat[upper.tri(adjMat)]))#0.02142503


maxEdge <- max(c(adjMat)) 
layoutwoAR <- averageLayout(adjMat,adjMat2)

png("纵向交叉滞后网络分析图有标签.png", width=4000, height=3000, res=350) 
layout(mat = matrix(c(1, 2), 1, 2), widths = c(1.5, 1))#to plot multiple graphs in one figure
qgraph(adjMat, groups = groups, labels = labels, legend = FALSE, posCol="blue",negCol="red", minimum=0.03,cut=0.18, layout=layoutwoAR,
       layout = "spring", theme = "colorblind",color = colors, edge.labels=TRUE,  edge.label.cex = 0.8,edge.label.color= "black")#threshold = 0.05,
#title(LabwArrow.t1.t2, line=3, cex.main=1.75)
plot.new()
par(mar = rep(0, 4))
legend(x = "center", inset = c(0, 0), bty = "n", legend = brieflegend,  
       col = colors, y.intersp = 2,
       pch = 19)
dev.off()

png("纵向交叉滞后网络分析图无标签.png", width=4000, height=3000, res=350) 
layout(mat = matrix(c(1, 2), 1, 2), widths = c(1.5, 1))#to plot multiple graphs in one figure
qgraph(adjMat2, groups = groups, labels = labels, legend = FALSE, posCol="blue",negCol="red", minimum=0.01,cut=0, layout=layoutwoAR,
       layout = "spring", theme = "colorblind",color = colors, edge.labels=FALSE,  edge.label.cex = 0.8,edge.label.color= "black")#threshold = 0.05,
#title(LabwArrow.t1.t2, line=3, cex.main=1.75)
plot.new()
par(mar = rep(0, 4))
legend(x = "center", inset = c(0, 0), bty = "n", legend = brieflegend,  
       col = colors, y.intersp = 2,
       pch = 19)
dev.off()


# 
# 
# pdf("自回归纵向无标签.pdf", height=13, width=20)
# layout(mat = matrix(c(1, 2), 1, 2), widths = c(1.5, 1))#to plot multiple graphs in one figure
#   qgraph(adjMat, groups = groups, labels = labels, legend = FALSE, posCol="blue",negCol="red", minimum=0.06,cut=0.18, layout=layoutwoAR,
#          layout = "spring", theme = "colorblind",color = colors, edge.labels=FALSE,  edge.label.cex = 0.8,edge.label.color= "black")#threshold = 0.05,
# #title(LabwArrow.t1.t2, line=3, cex.main=1.75)
# plot.new()
# par(mar = rep(0, 4))
# legend(x = "center", inset = c(0, 0), bty = "n", legend = brieflegend,  
#        col = colors, y.intersp = 2,
#        pch = 19)
# dev.off()
# 
# pdf("无自回归纵向有标签.pdf", height=13, width=20)
# layout(mat = matrix(c(1, 2), 1, 2), widths = c(1.5, 1))#to plot multiple graphs in one figure
# qgraph(adjMat2, groups = groups, labels = labels, legend = FALSE, posCol="blue",negCol="red", minimum=0.06,cut=0.18, layout=layoutwoAR,
#        layout = "spring", theme = "colorblind",color = colors, edge.labels=TRUE,  edge.label.cex = 0.8,edge.label.color= "black")#threshold = 0.05,
# #title(LabwArrow.t1.t2, line=3, cex.main=1.75)
# plot.new()
# par(mar = rep(0, 4))
# legend(x = "center", inset = c(0, 0), bty = "n", legend = brieflegend,  
#        col = colors, y.intersp = 2,
#        pch = 19)
# dev.off()


pred0 <- rep(0, k) #set up empty vectors to hold predictability coefficients
pred1 <- rep(0, k) 
pred2 <- rep(0, k)

for (i in 1:k){  #loop through each variable
  name.i <- names(data_r)[k+1] #what is the name of the DV
  group.i <- groups[i] #what construct does the DV belong to
  include.mod0 <- names(data_r)[1:k] #include self at previous time point
  include.mod1 <- names(data_r)[1:k][-i] #exclude self at previous time point
  include.mod2 <- names(data_r)[1:k][groups != group.i] #include predictors belonging to same construct
  betas.mod0 <- adjMat[1:k,i]     #fix regression coefficients to the ones obtained by glmnet
  betas.mod1 <- adjMat[1:k,i][-i] 
  betas.mod2 <- adjMat[1:k,i][groups != group.i]
  mod0 <- paste(name.i, " ~ ", paste(paste(betas.mod0,'*', include.mod0, sep = ""), collapse = "+")) 
  mod1 <- paste(name.i, " ~ ", paste(paste(betas.mod1,'*', include.mod1, sep = ""), collapse = "+")) 
  mod2 <- paste(name.i, " ~ ", paste(paste(betas.mod2,'*', include.mod2, sep = ""), collapse = "+")) 
  fit0 <- sem(mod0, sample.cov = cor(data_r), sample.nobs = nrow(data_r)) #fit model with subset of predictors to get r-square
  pred0[i] <- inspect(fit0, "rsquare")
  fit1 <- sem(mod1, sample.cov = cor(data_r), sample.nobs = nrow(data_r)) #fit model with subset of predictors to get r-square
  pred1[i] <- inspect(fit1, "rsquare")
  fit2 <- sem(mod2, sample.cov = cor(data_r), sample.nobs = nrow(data_r)) #fit model with subset of predictors to get r-square
  pred2[i] <- inspect(fit2, "rsquare")
}  

pred0
pred1
pred2


# > pred0
# [1] 0.08231708 0.08491708 0.09177643 0.09491910 0.10969757
# [6] 0.10722329 0.10492578 0.10475314 0.13398858 0.12105256
# [11] 0.14890244 0.15141202 0.15463847 0.14524125 0.13863573
# [16] 0.14806418 0.10497003 0.14362285 0.14741452
# > pred1
# [1] 0.067201496 0.046887040 0.074476761 0.055328085 0.070892532
# [6] 0.096943532 0.067456098 0.007807372 0.094533463 0.077768605
# [11] 0.094085291 0.103452288 0.129981854 0.122810299 0.064063625
# [16] 0.083607702 0.019024767 0.017558707 0.060898889
# > pred2
# [1] 0.008737251 0.009826675 0.009869789 0.010682310 0.015341425
# [6] 0.016314859 0.012436708 0.001443074 0.019601465 0.010899040
# [11] 0.029515758 0.025590163 0.032694204 0.030649955 0.023271637
# [16] 0.029491652 0.009272701 0.017558707 0.025892170



#nodewise influence: sum squared outgoing regression paths/node
infl0 <- apply(adjMat^2, 1, sum) #all outgoing nodes including AR
infl1 <- apply(adjMat2^2, 1, sum) #all outgoing nodes excluding AR

adjMat3 <- adjMat2 #adjMat3 sets paths within the same construct to 0
adjMat3[groups[row(adjMat3)] == groups[col(adjMat3)]] <- 0
infl2 <- apply(adjMat3^2, 1, sum) #rows = T1 predictors; columns = T2 DVs

infl0
infl1
infl2

# > infl0
# [1] 0.001539664 0.064596762 0.023283179 0.027251480 0.063846884
# [6] 0.001505774 0.052967074 0.114540587 0.037689178 0.015552377
# [11] 0.047522442 0.033823097 0.020395114 0.007186815 0.037129491
# [16] 0.053334521 0.075842660 0.160559006 0.093717621
# > infl1
# [1] 0.0002391370 0.0563476711 0.0218548205 0.0186365795
# [5] 0.0562037672 0.0009910784 0.0453625716 0.0263765205
# [9] 0.0295418359 0.0049170722 0.0313189330 0.0223083758
# [13] 0.0172634979 0.0045137361 0.0027077325 0.0307865699
# [17] 0.0180804920 0.0505605153 0.0557077067
# > infl2
# [1] 3.254512e-05 4.741376e-03 1.446606e-02 5.143846e-03
# [5] 5.108598e-02 4.105661e-04 2.579698e-02 1.453495e-04
# [9] 6.115128e-04 5.772016e-04 3.191486e-03 2.198347e-03
# [13] 1.251256e-03 7.790937e-04 2.446896e-03 5.258323e-03
# [17] 1.747131e-02 2.341197e-02 5.570771e-02

#create a plot of predictability and influence: 
jpeg("Predictability Influence.jpg", width=10, height=10, res=800, units="in")  #(uncomment this code to save plot as jpeg)
layout(mat = matrix(c(1, 2, 3, 3), 2, 2), widths = c(1, 1)) #to plot multiple graphs in one figure
par(mar = c(2, 4, 0, 0), las = 1) #set margins
barplot(rev(pred1), horiz = TRUE, names.arg = rev(labels),
        col = rainbow(k), ylab = "predictability")
barplot(rev(infl1), horiz = TRUE, names.arg = rev(labels), 
        col = rainbow(k), ylab = "influence")
plot.new()
legend(x = "center", inset = c(0, 0), bty = "n", fill=rev(rainbow(k)),
       legend = brieflegend, cex = 1.2) #add legend to identify variables
dev.off() #(uncomment this code to save plot as jpeg)


jpeg("跨结构Predictability Influence.jpg", width=10, height=10, res=800, units="in")  #(uncomment this code to save plot as jpeg)
layout(mat = matrix(c(1, 2, 3, 3), 2, 2), widths = c(1, 1)) #to plot multiple graphs in one figure
par(mar = c(2, 4, 0, 0), las = 1) #set margins
barplot(rev(pred2), horiz = TRUE, names.arg = rev(labels),
        col = rainbow(k), ylab = "predictability")
barplot(rev(infl2), horiz = TRUE, names.arg = rev(labels), 
        col = rainbow(k), ylab = "influence")
plot.new()
legend(x = "center", inset = c(0, 0), bty = "n", fill=rev(rainbow(k)),
       legend = brieflegend, cex = 1.2) #add legend to identify variables
dev.off() #(uncomment this code to save plot as jpeg)
#################################################################

## Compute and Plot Centrality

centPlot.t1.t2 <- centralityPlot(adjMat2, labels = labels)
# Save centrality data
write.csv(centPlot.t1.t2$data, "centPlotST1T3.csv")
centPlot.t1.t2$data
?centralityPlot()
png("纵向中心指标.png", width=4000, height=6000, res=700) 
#pdf("纵向中心指标z.pdf", height=7, width=5)
centPlot.t1.t2 <- centralityPlot(adjMat2, 
                                include=c("InExpectedInfluence", "OutExpectedInfluence"),
                                labels=labels, #scale="z-scores",
                                decreasing=TRUE)
dev.off()


centPlotdf.t1.t2 <- centPlot.t1.t2$data

centPlotdf.t1.t2$measure <- factor(centPlotdf.t1.t2$measure, levels=c("InExpectedInfluence", "OutExpectedInfluence"),
                                  labels=c("InExpectedInfluence", "OutExpectedInfluence"))

centPlotdf.t1.t2$Network <- c(rep("T1 -> T2", nrow(centPlotdf.t1.t2)))

png(file="纵向中心性指标1.png",width=4,height=5,res=600,units="in",bg="white")
ggplot(subset(centPlotdf.t1.t2, Network=="T1 -> T2"), aes(x=value, y=node, group=measure)) +
  # facet_wrap(~measure) +
  geom_path(aes(lty = measure), lineend="butt", linejoin="round") +
  geom_point(shape = 21, size = .3, stroke = 1) + #aes(color = centPlotdf$measure, fill = centPlotdf$measure), 
  xlab("Cross-Lagged Centrality") + ylab(NULL) +
  theme_bw() +
  guides(lty=guide_legend(title="Centrality Measure", title.hjust=0.5)) +
  # coord_flip() +
  # scale_y_discrete(limits = (node)) + # limits sets the order
  # scale_color_manual(values=c("black", "green")) + # manually set colors
  theme(panel.grid.minor=element_blank(), 
        axis.text = element_text(color="black", size=10),
        axis.title = element_text(color="black", size=8, face="bold"),
        # strip.text = element_text(color="black", size=12, face="bold"),
        legend.position = "right",
        legend.text=element_text(color="black", size=10),
        legend.title = element_text(color="black", size=12, face="bold"))
dev.off()

CLPN.net <- function(data) {
  ## create empty adjacency matrix
  adjMatCLPN <- matrix(0, k, k) 
  ## run CLPN loop to do series of nodewise regularized regressions
  for (i in 1:k) {
    lassoreg <- cv.glmnet(x=as.matrix(data[,1:k]), 
                          y=data[,(k+i)], nfolds=10,
                          family="gaussian", alpha=1, standardize=TRUE)
    lambda <- lassoreg$lambda.min
    ## paste coefficients into adjacency matrix
    adjMatCLPN[1:k,i] <- coef(lassoreg, s=lambda, exact=FALSE)[2:(k+1)]
  }
  return(adjMatCLPN)
}

set.seed(2)
net.t1t2 <- estimateNetwork(data_r, fun=CLPN.net, labels=labels, directed=TRUE)

sum(net.t1t2$graph!=0) - sum(diag(net.t1t2$graph)!=0) #103
?bootnet()

t <- qgraph(adjMat, groups = groups, labels = labels, legend = FALSE, posCol="blue",negCol="red", minimum=0.04,cut=0.18, layout=layoutwoAR,
            layout = "spring", theme = "colorblind",color = colors, edge.labels=FALSE,  edge.label.cex = 0.8,edge.label.color= "black")#threshold = 0.05,



#case
set.seed(3)
caseParBoot.t1t2 <- bootnet(net.t1t2, type="case", nBoots=1000, communities=groups, directed=TRUE,
                            statistics=c("inExpectedInfluence", "outExpectedInfluence","edge"),
                            nCores=1)

corStability(caseParBoot.t1t2)

# bridgeExpectedInfluence: 0.128 
# - For more accuracy, run bootnet(..., caseMin = 0.05, caseMax = 0.205) 
# 
# inExpectedInfluence: 0.283 
# - For more accuracy, run bootnet(..., caseMin = 0.205, caseMax = 0.362) 
# 
# outExpectedInfluence: 0.283 
# - For more accuracy, run bootnet(..., caseMin = 0.205, caseMax = 0.362) 

t_b<-bridge(t, communities=groups, directed=TRUE) 

# 查看原始列表名称
print(names(t_b))
# [1] "Bridge Expected Influence (1-step)" "communities"

# 重命名 "Bridge Expected Influence (1-step)" 为 "bridgeExpectedInfluence"
names(t_b)[names(t_b) == "Bridge Expected Influence (1-step)"] <- "bridgeExpectedInfluence"

# 验证重命名是否成功
print(names(t_b))

bridgeOutAdjusted <- as.vector(t_b$"bridgeExpectedInfluence") 

t_bridge_in_out<-cbind(bridgeOutAdjusted) # combine into matrix
write.csv(t_bridge_in_out, ".纵向桥预期指标从母亲到孩子.csv") # save matrix as table

plot.bridge.Sam1 <- function(x, include = NULL, z = FALSE) {
  # 确保必要的包已加载
  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop("请先安装 'reshape2' 包")
  }
  if (!requireNamespace("gtools", quietly = TRUE)) {
    stop("请先安装 'gtools' 包")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("请先安装 'ggplot2' 包")
  }
  
  # 1. 移除原有的 class 属性
  attr(x, "class") <- NULL
  
  # 2. 提取节点名称
  nodes <- names(x[[1]])
  
  # 3. 提取社区信息（如果存在，用于 y 轴文字着色）
  if("communities" %in% names(x)){
    comm <- x$communities
    cols <- rep("black", length(comm))
    x$communities <- NULL
  } else {
    comm <- NULL
    cols <- rep("black", length(nodes))
  }
  
  # 4. 处理是否需要缩放
  if(z){
    scalenoatt <- function(y){
      y <- scale(y)
      attr(y, "scaled:center") <- NULL
      attr(y, "scaled:scale") <- NULL
      return(y)
    }
  } else {
    scalenoatt <- function(y){
      return(y)
    }
  }
  
  # 5. 应用缩放函数
  x <- sapply(x, scalenoatt)
  
  # 6. 将列表 x 转成长格式数据框
  Long <- reshape2::melt(x)
  colnames(Long)[2] <- "measure"
  
  # 7. 添加节点信息
  Long$node <- rep(nodes, length(unique(Long$measure)))
  
  # 8. 如果指定了 include，进行子集筛选
  if(!is.null(include)){
    Long <- subset(Long, measure %in% include)
  }
  
  # 9. 排序节点
  Long <- Long[gtools::mixedorder(Long$node), ]
  Long$node <- factor(as.character(Long$node),
                      levels = unique(gtools::mixedsort(as.character(Long$node))))
  
  # 10. 绘图
  g <- ggplot2::ggplot(Long, ggplot2::aes(x = value, y = node)) +
    ggplot2::geom_path(group = 1, color = "black") +    # 线条颜色改为黑色
    ggplot2::geom_point(color = "black") +             # 点的颜色改为黑色
    ggplot2::facet_wrap(~ measure, ncol = 1, scales = "free") +
    ggplot2::scale_y_discrete(limits = rev(levels(Long$node))) +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::theme_bw()
  
  # 11. 如果有社区信息，设置 y 轴文字颜色
  if(!is.null(comm)){
    g <- g + ggplot2::theme(
      axis.text.y = ggplot2::element_text(colour = cols[order(nodes, decreasing = TRUE)])
    )
  }
  
  return(g)
}

png("纵向桥预期2.png",width=1000, height=2000, res=400) 
plot.bridge.Sam1(x = t_b, 
                 include = c("bridgeExpectedInfluence"), 
                 z = FALSE)
dev.off()


png("纵向稳定性图.png", width=1800, height=1800, res=340) 
#pdf("纵向稳定性图.pdf", height=7, width=6)
plot(caseParBoot.t1t2, statistics=c("inExpectedInfluence", "outExpectedInfluence","edge"))
dev.off()

LabwArrow.t1.t2 <- expression("T1" %->% "T2 Network") 

set.seed(1)
norParBoot.t1t2 <- bootnet(net.t1t2, type="nonparametric",Boots=1000, communities=groups, directed=TRUE,
                           statistics=c("inExpectedInfluence", "outExpectedInfluence","edge"),
                           nCores=1)
png("纵向网络非参数检验.png", width=3000, height=3000, res=400) 
#pdf("纵向网络非参数检验.pdf", width=8,height=7)
plot(norParBoot.t1t2, order="sample", labels=F, legend=F, meanColor=NA) + 
  xlab("Edge Weight - T1 to T2") + ggtitle(LabwArrow.t1.t2) + 
  theme(plot.title=element_text(hjust=0.5, size=18),
        strip.text=element_blank(), # remove "edge" header
        axis.text.x=element_text(size=18))
dev.off()

pdf("纵向网络边差异对比.pdf", height=10, width=20)
## Edge weights difference test
edgeDiffTest.t1t2 <-  print(plot(norParBoot.t1t2, "edge", plot="difference", onlyNonZero=TRUE, order="sample") + 
  ggtitle(LabwArrow.t1.t2) + 
  theme(plot.title=element_text(hjust=0.5, size=10, face="bold"),
        strip.text=element_blank(), # remove "edge" header
        axis.text.x=element_text(size=2.5),
        axis.text.y=element_text(size=2.5)))
dev.off()

pdf("纵向网络中心性节点差异比较.pdf", height = 7, width = 18)
plot(norParBoot.t1t2, statistics=c("inExpectedInfluence", "outExpectedInfluence", "bridgeExpectedInfluence"), plot="difference", order="sample")
dev.off()



### Identify the strongest edges (note: row=IV, col=DV)
res.t1.t2 <- order(adjMat2, decreasing = T)
pos.t1.t2 <- arrayInd(res.t1.t2, dim(adjMat2), useNames = TRUE)
posWithLabs.t1.t2 <- data.frame(nodeOut=shortnames[pos.t1.t2[,1]],
                                nodeIn=shortnames[pos.t1.t2[,2]],
                                value=adjMat[res.t1.t2],
                                DomainOut=groups[pos.t1.t2[,1]],
                                DomainIn=groups[pos.t1.t2[,2]])

posWithLabs.t1.t2
write.csv(posWithLabs.t1.t2, "posWith.csv")

subset(posWithLabs.t1.t2, DomainOut!=DomainIn) # extract bridging edges
write.csv(subset(posWithLabs.t1.t2, DomainOut!=DomainIn), "SPN2021_Bridging Edges.csv")

filtered_data1 <- posWithLabs.t1.t2 %>%
  filter(DomainOut == "GA" & DomainIn == "PG")

# 筛选 DomainOut 为 "SA" 且 DomainIn 为 "PG"
filtered_data2 <- posWithLabs.t1.t2 %>%
  filter(DomainOut == "SA" & DomainIn == "PG")

# 查看筛选后的数据
print(filtered_data1)
print(filtered_data2)

# 检查筛选结果的唯一值
print(unique(filtered_data1$DomainOut))  # 应仅包含 "GA"
print(unique(filtered_data1$DomainIn))   # 应仅包含 "PG"

print(unique(filtered_data2$DomainOut))  # 应仅包含 "SA"
print(unique(filtered_data2$DomainIn))   # 应仅包含 "PG"

# 保存筛选后的数据为 CSV 文件
library(writexl)
# 保存 filtered_data1 为 Excel 文件
write_xlsx(filtered_data1, "ga到pg.xlsx")

# 保存 filtered_data2 为 Excel 文件
write_xlsx(filtered_data2, "sa到pg.xlsx")


# 
# subset(posWithLabs.t1.t2, (DomainOut=="Sucidial potential" & DomainIn=="Risk factors") |
#          DomainOut=="Risk factors" & DomainIn=="Sucidial potential") # extracT bridging edges
# write.csv(subset(posWithLabs.t1.t2, DomainOut!=DomainIn), "SPN2021Suicidal-Risk Bridging Edges.csv")
# nrow(subset(posWithLabs.t1.t2, DomainOut=="Risk factors" & DomainIn=="Sucidial potential")) # number of Suicidal-Risk edges(9)
# 
# subset(posWithLabs.t1.t2, (DomainOut=="Sucidial potential" & DomainIn=="Protective factors") |
#          DomainOut=="Protective factors" & DomainIn=="Sucidial potential") # extracT bridging edges
# write.csv(subset(posWithLabs.t1.t2, DomainOut!=DomainIn), "SPN2021Suicidal-Protective Bridging Edges.csv")
# nrow(subset(posWithLabs.t1.t2, DomainOut=="Protective factors" & DomainIn=="Sucidial potential")) #9

## Centrality Tables
library("dplyr")
out.strength.t1.t2 = as.data.frame(centrality(adjMat)$OutDegree)
in.strength.t1.t2 = as.data.frame(centrality(adjMat)$InDegree)
closeness.t1.t2 = as.data.frame(centrality(adjMat)$Closeness)
betweenness.t1.t2 = as.data.frame(centrality(adjMat)$Betweenness)
inEI.t1.t2 = as.data.frame(centrality(adjMat)$InExpectedInfluence)
outEI.t1.t2 = as.data.frame(centrality(adjMat)$OutExpectedInfluence)
centrality.indices.t1.t2 = bind_cols(out.strength.t1.t2, in.strength.t1.t2, closeness.t1.t2,
                                     betweenness.t1.t2, inEI.t1.t2, outEI.t1.t2)
centrality.indices.t1.t2 = centrality.indices.t1.t2 %>%
  rename("OutDegree" = "centrality(adjMat)$OutDegree", "InDegree"="centrality(adjMat)$InDegree",
         "Closeness" = "centrality(adjMat)$Closeness", "Betweenness" = "centrality(adjMat)$Betweenness",
         "InEI" = "centrality(adjMat)$InExpectedInfluence", "OutEI" = "centrality(adjMat)$OutExpectedInfluence")
centrality.indices.t1.t2
write.csv(centrality.indices.t1.t2, "SPN2021_Centrality Indices.csv")
