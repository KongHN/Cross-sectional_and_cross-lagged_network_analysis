# Install and load the required packages
packages <- c("haven", "dplyr", "ggplot2", "bootnet", "qgraph", "NetworkComparisonTest",
              "dplyr", "stringr", "networktools", "igraph", "mice", "lavaan", 
              "glmnet", "missForest", "readxl", "ltm", "psych", "e1071", "Hmisc")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Clear the workspace
rm(list = ls())



###################################Network analysis####################################
# Read an Excel file
datax <- read_sav(choose.files())
print(names(datax))
# Use keyword filtering for columns
keywords <- "MQ21|CQ20|CQ96"
columns_s <- grep(keywords, names(datax), value = TRUE)

# Create a subset data frame
data <- datax[, columns_s]

# Ensure that the data frame is formatted correctly
data <- as.data.frame(data)

# Check column names
print(names(data))
#data <- data[,-c(88:91)]
print(names(data))
# # Delete the columns starting with "T3"
# data <- data[, !grepl("^L2", names(data))]
# data <- data[, !grepl("^W2", names(data))]
# # Rename other columns
# names(data) <- names(data) %>%
#   str_replace("^B_", "T1_") %>%
#   str_replace("^A_", "T1_") %>%
#   str_replace("^L2_", "T2_") %>%
#   str_replace("^W2_", "T2_") %>%
#   str_replace("网络游戏障碍量表", "网络游戏障碍") %>%
#   str_replace("社交焦虑量表", "社交焦虑")

# View the modified column names
print(names(data))

t1_respondents <- sum(!is.na(data$T1MQ21_Row1))  # Calculate the number of people who answered at time point T1
t2_respondents <- sum(!is.na(data$T2MQ21_Row4))  # Calculate the number of people who answered at time point T2
t1_respondents  # Show the number of respondents at the T1 time point
t2_respondents  # Show the number of respondents at the T2 time point

dput(colnames(data))

summary(data)

mydata <- subset(data,select = c("T1MQ21_Row1", "T1MQ21_Row2", "T1MQ21_Row3", "T1MQ21_Row4", 
                                 "T1MQ21_Row5", "T1MQ21_Row6", "T1MQ21_Row7", "T1MQ21_Row8", "T1MQ21_Row9", 
                                 "T1CQ20_Row1", "T1CQ20_Row2", "T1CQ20_Row3", "T1CQ20_Row4", "T1CQ20_Row5", 
                                 "T1CQ20_Row6", "T1CQ20_Row7", "T1CQ20_Row8", "T1CQ20_Row9", "T2MQ21_Row1", 
                                 "T2MQ21_Row2", "T2MQ21_Row3", "T2MQ21_Row4", "T2MQ21_Row5", "T2MQ21_Row6", 
                                 "T2MQ21_Row7", "T2MQ21_Row8", "T2MQ21_Row9", "T2CQ96_Row1", "T2CQ96_Row2", 
                                 "T2CQ96_Row3", "T2CQ96_Row4", "T2CQ96_Row5", "T2CQ96_Row6", "T2CQ96_Row7", 
                                 "T2CQ96_Row8", "T2CQ96_Row9"))

data_c <- mydata[complete.cases(mydata),]
nrow(data_c) 
dput(colnames(data_c))
data_r <- data_c


old_n <- colnames(data_r)

# Update column names
colnames(data_r) <- gsub("MQ21_Row", "MDS", colnames(data_r))
colnames(data_r) <- gsub("CQ20_Row", "CDS", colnames(data_r))
colnames(data_r) <- gsub("CQ96_Row", "CDS", colnames(data_r))

# Calculate the skewness of each column
skewness_values <- apply(data_r, 2, skewness)

# Calculate the kurtosis of each column
kurtosis_values <- apply(data_r, 2, kurtosis)

# Printing result
print(skewness_values)
print(kurtosis_values)

# Use the round() function to retain two decimal places.
skewness_values_rounded <- round(skewness_values, 2)
kurtosis_values_rounded <- round(kurtosis_values, 2)

# Print the result with two decimal places retained.
print(skewness_values_rounded)
print(kurtosis_values_rounded)



# Method 1: Basic R
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

# Save as CSV
write.csv(summary_df, "./Kurtosis and Skewness.csv", row.names = FALSE)


groups <- c(rep("MDS",9),rep("CDS",9))
colors <- c(rep("#E69F00", 9),  
            rep("#009E73", 9)) 


data_r <- as.data.frame(data_r)


# Create a list of emotional words
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
data1 <- subset(data_r,select = c("T1MDS1", "T1MDS2", "T1MDS3", "T1MDS4", "T1MDS5", "T1MDS6", 
                                  "T1MDS7", "T1MDS8", "T1MDS9", "T1CDS1", "T1CDS2", "T1CDS3", "T1CDS4", 
                                  "T1CDS5", "T1CDS6", "T1CDS7", "T1CDS8", "T1CDS9"))
data2 <- subset(data_r,select = c("T2MDS1", "T2MDS2", 
                                  "T2MDS3", "T2MDS4", "T2MDS5", "T2MDS6", "T2MDS7", "T2MDS8", "T2MDS9", 
                                  "T2CDS1", "T2CDS2", "T2CDS3", "T2CDS4", "T2CDS5", "T2CDS6", "T2CDS7", 
                                  "T2CDS8", "T2CDS9"))
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
write.csv(myedges1,"T1 Network Edge Coefficient.csv")
summary(network1)

myedges2 <- getWmat(network2)
write.csv(myedges2,"T2 Network Edge Coefficient.csv")
summary(network2)

library(dplyr)

# Extract the lower triangular matrix
lower_tri1 <- myedges1 
lower_tri1[upper.tri(lower_tri1, diag=TRUE)] <- NA

# Output result
lower_tri1

# Extract the lower triangular matrix
lower_tri2 <- myedges2 
lower_tri2[upper.tri(lower_tri2, diag=TRUE)] <- NA

# Output result
lower_tri2

#Sort the sizes of the opposite sides in ascending order
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
write.csv(sorted_edges1, "Sorting of T1 Network Edge Coefficients.csv", row.names = FALSE)
write.csv(sorted_edges2, "Sorting of T2 Network Edge Coefficients.csv", row.names = FALSE)


L <- averageLayout(network1,network2)
#pdf("t1t2横断图有标签2.pdf",width=16,height=6)
png("The t1t2 cross-sectional network diagram has labels.png", width=4500, height=2400, res=400) 
layout(mat = matrix(c(1,2,3),nrow = 1), widths = c(1.5,1.5,1),heights = 2)
#par(mfrow=c(1,2))

#Output the network structure diagram of Time 1
Network1G <- plot(network1,layout = L,title="T1", groups=groups,edge.labels=TRUE,edge.label.cex = 0.9,edge.label.color= "black", minimum=0.02,cut=0,
                 vsize=9, legend=FALSE,borders=TRUE,color = colors,palette="colorblind",labels = labels,#minimum=0.03, cut=0,
                  label.cex=1,title.cex=1.2)
#Output the network structure diagram of Time 2
Network2G <- plot(network2,layout = L,title="T2", groups=groups,edge.labels=TRUE,edge.label.cex = 0.9,edge.label.color= "black", minimum=0.02,cut=0,
                 vsize=9, legend=FALSE,borders=TRUE,color = colors,palette="colorblind",labels = labels,
                  label.cex=1,title.cex=1.2)
#title(LabwArrow.t1.t2, line=3, cex.main=1.75)
plot.new()
par(mar = rep(0, 4))
legend(x = "center", inset = c(0, 0), bty = "n", legend = brieflegend,  
       col = colors, y.intersp = 2,
       pch = 19, cex=1.2)
dev.off()

L <- averageLayout(network1,network2)
#pdf("The t1t2 cross-sectional image has label 2..pdf",width=16,height=6)
png("t1t2 cross-sectional network diagram without labels.png", width=4500, height=2400, res=400) 
layout(mat = matrix(c(1,2,3),nrow = 1), widths = c(1.5,1.5,1),heights = 2)
#par(mfrow=c(1,2))
Network1G <- plot(network1,layout = L,title="T1", groups=groups,edge.labels=FALSE,edge.label.cex = 0.9,edge.label.color= "black", minimum=0.02,cut=0,
                  vsize=9, legend=FALSE,borders=TRUE,color = colors,palette="colorblind",labels = labels,#minimum=0.03, cut=0,
                  label.cex=1,title.cex=1.2)#Output the network structure diagram of Time 1
Network2G <- plot(network2,layout = L,title="T2", groups=groups,edge.labels=FALSE,edge.label.cex = 0.9,edge.label.color= "black", minimum=0.02,cut=0,
                  vsize=9, legend=FALSE,borders=TRUE,color = colors,palette="colorblind",labels = labels,
                  label.cex=1,title.cex=1.2)#Output the network structure diagram of Time 2
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
write.csv(nc1, file = "T1 Network Centrality Index.csv")
centrality2 <- centrality_auto(network2)
nc2 <- centrality2$node.centrality
write.csv(nc2, file = "T2 Network Centrality Index.csv")
png("Comparison chart of centralization indicators of t1t2.png", width=1100, height=2000, res=300) 
#pdf("Centrality Comparison Chart.pdf",width=4,height=10)
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

write.csv(bridge_Bridge1, file = "T1 Network Bridge Metrics.csv")
write.csv(bridge_Bridge2, file = "T2 Network Bridge Metrics.csv")

png("Expected comparison chart of t1t2 bridge.png", width=1100, height=2000, res=300) 
#pdf("t1t3桥强度对比.pdf", width = 4, height =10)
plot.bridge.Sam(x = c, x2 = d,
                x_name = "T1", x2_name = "T2", 
                include=c("Bridge Expected Influence (1-step)"), z=TRUE)#
dev.off()


boot1n <- bootnet(network1, nCores = 8, nBoots = 1000, 
                  type = "nonparametric",statistics=c('bridgeExpectedInfluence','expectedInfluence',"edge"), communities=groups)
boot2n <- bootnet(network2, nCores = 8, nBoots = 1000, 
                  type = "nonparametric",statistics=c('bridgeExpectedInfluence','expectedInfluence',"edge"), communities=groups)

png("T1 network link interval.png", width=5500, height=5500, res=250) 
#pdf("Interval of side t1.pdf",width = 50,height = 50)#Save the result image
plot(boot1n,lables=FALSE,order="sample")#Confidence interval of edge weights; order=sample is used to set the edge weights
dev.off()
## Central and BridgeEI Stability
pdf("T1 Network's expected variance.pdf", height=10, width=10)
plot(boot1n, "expectedInfluence",plot ="difference")
dev.off()
## Edge Difference
pdf("T1 network edge difference.pdf", height = 50, width = 50)
plot(boot1n, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
dev.off()
pdf("Expected differences in the T1 network bridge.pdf", height=10, width=10)
plot(boot1n, "bridgeExpectedInfluence",plot ="difference")
dev.off()


png("T2 network link interval.png", width=5500, height=5500, res=250) 
plot(boot2n,lables=FALSE,order="sample")#Confidence interval of edge weights; order=sample is used to set the edge weights
dev.off()
## Central and BridgeEI Stability
pdf("T2 Network Expected Variance.pdf", height=10, width=10)
plot(boot2n, "expectedInfluence",plot ="difference")
dev.off()
## Edge Difference
pdf("T2 network edge differences.pdf", height = 50, width = 50)
plot(boot2n, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
dev.off()
pdf("T2 Network Bridge Expected Difference.pdf", height=10, width=10)
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

png("T1 Network Stability Graph.png", width=1800, height=1800, res=400) 
#pdf("稳定性图t1.pdf",width=6,height=6)
plot(boot1g, statistics=c("expectedInfluence","bridgeExpectedInfluence"))
dev.off()
png("T2 Network Stability Graph.png", width=1800, height=1800, res=400) 
plot(boot2g, statistics=c("expectedInfluence","bridgeExpectedInfluence"))
dev.off()
# ?NCT()
# nct_12 <- NCT(data1, data2, gamma = 0.5, it = 1000,
#               binary.data=FALSE, weighted = TRUE, progressbar = TRUE,
#               test.edges=TRUE, test.centrality=TRUE,paired = TRUE,
#               centrality=c('closeness', 'betweenness', 'strength', 'bridgeStrength'),nodes="all",communities=groups)#fz2,fz3 是对比的
# nct_12$nwinv.pval#Check whether there are significant differences in the edge weights of the entire network: P value
# #(2) invariant global strength
# nct_12$glstrinv.pval#Check whether there is a significant difference in the overall strength of the network: P value
# #(3) global strength value estimation
# nct_12$glstrinv.sep #Check the specific values of the overall strength of the two networks
# #(4) quantification of differences: count significantly different edges
# nct_12$einv.pvals###Check for significant differences in the permutation test of network edge weights: P value
# write.csv(nct_12$einv.pvals, file = "Borderline permutation test.csv")
# sum(nct_12$einv.pvals$"p-value"< 0.05)#Count the number of P-values that are less than 0.05
# #(5) differences in centralities: p-value
# nct_12$diffcen.pval#Check whether there is a significant difference in the strength of the central indicator after the network replacement test: P value
# sum(nct_12$diffcen.pval< 0.05)#Count the number of P-values that are less than 0.05
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

# Install and load the writexl package
if (!requireNamespace("writexl", quietly = TRUE)) {
  install.packages("writexl")
}
library(writexl)

# Suppose adjMat is your matrix
# Convert the matrix to a data frame
adjMat_df <- as.data.frame(adjMat)

# View the transformed data frame (optional)
print(adjMat_df)

# Save the data frame as an Excel file
write_xlsx(list(Adjacency_Matrix = adjMat_df), path = "Vertical network edge coefficient.xlsx")



LabwArrow <- expression("T1" %->% "T2 Network") 

ARedgesT1T2 <- diag(adjMat)

shortnames = labels
ARedges <- data.frame(Factors=rep(factor(shortnames, levels=rev(shortnames)), 1),
                      Class=rep(groups, 1),
                      Network=c(rep("T1 -> T2 Network", k)),
                      ARedge=c(ARedgesT1T2)
)
ARedges$Network <- factor(ARedges$Network, levels=c("T1 -> T2 Network"))
write.csv(ARedges,"Autoregressive coefficient.csv")

png("Vertical autoregressive indicator chart.png", width=1400, height=1800, res=300) 
#pdf("Autoregressive indicator.pdf", width = 10, height = 10)
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

png("The longitudinal cross-lagged network analysis diagram is labeled..png", width=4000, height=3000, res=350) 
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

png("Vertical cross-lagged network analysis diagram without labels.png", width=4000, height=3000, res=350) 
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
# pdf("Autoregressive longitudinal unlabeled.pdf", height=13, width=20)
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
# pdf("Non-autoregressive longitudinal with labels.pdf", height=13, width=20)
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


jpeg("Cross-structure Predictability Influence.jpg", width=10, height=10, res=800, units="in")  #(uncomment this code to save plot as jpeg)
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
png("Vertical central indicator.png", width=4000, height=6000, res=700) 
#pdf("Vertical central indicatorz.pdf", height=7, width=5)
centPlot.t1.t2 <- centralityPlot(adjMat2, 
                                include=c("InExpectedInfluence", "OutExpectedInfluence"),
                                labels=labels, #scale="z-scores",
                                decreasing=TRUE)
dev.off()


centPlotdf.t1.t2 <- centPlot.t1.t2$data

centPlotdf.t1.t2$measure <- factor(centPlotdf.t1.t2$measure, levels=c("InExpectedInfluence", "OutExpectedInfluence"),
                                  labels=c("InExpectedInfluence", "OutExpectedInfluence"))

centPlotdf.t1.t2$Network <- c(rep("T1 -> T2", nrow(centPlotdf.t1.t2)))

png(file="Vertical centrality index 1.png",width=4,height=5,res=600,units="in",bg="white")
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

# View the original list name
print(names(t_b))
# [1] "Bridge Expected Influence (1-step)" "communities"

# rename "Bridge Expected Influence (1-step)" to "bridgeExpectedInfluence"
names(t_b)[names(t_b) == "Bridge Expected Influence (1-step)"] <- "bridgeExpectedInfluence"

# 验证重命名是否成功
print(names(t_b))

bridgeOutAdjusted <- as.vector(t_b$"bridgeExpectedInfluence") 

t_bridge_in_out<-cbind(bridgeOutAdjusted) # combine into matrix
write.csv(t_bridge_in_out, "The longitudinal bridge expected indicators go from the mother to the child.csv") # save matrix as table

plot.bridge.Sam1 <- function(x, include = NULL, z = FALSE) {
  # 确保必要的包已加载
  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop("Please install the 'reshape2' package first.")
  }
  if (!requireNamespace("gtools", quietly = TRUE)) {
    stop("Please install the 'gtools' package first.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install the 'ggplot2' package first.")
  }
  
  # 1. Remove the original class attribute
  attr(x, "class") <- NULL
  
  # 2. Extract node name
  nodes <- names(x[[1]])
  
  # 3. Extract community information (if present, used for coloring the text on the y-axis)
  if("communities" %in% names(x)){
    comm <- x$communities
    cols <- rep("black", length(comm))
    x$communities <- NULL
  } else {
    comm <- NULL
    cols <- rep("black", length(nodes))
  }
  
  # 4. whether scaling be applied
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
  
  # 5. Apply the scaling function
  x <- sapply(x, scalenoatt)
  
  # 6. Convert list x to a long-format data frame
  Long <- reshape2::melt(x)
  colnames(Long)[2] <- "measure"
  
  # 7. Add node information
  Long$node <- rep(nodes, length(unique(Long$measure)))
  
  # 8. If "include" is specified, perform subset filtering.
  if(!is.null(include)){
    Long <- subset(Long, measure %in% include)
  }
  
  # 9. Sorting node
  Long <- Long[gtools::mixedorder(Long$node), ]
  Long$node <- factor(as.character(Long$node),
                      levels = unique(gtools::mixedsort(as.character(Long$node))))
  
  # 10. Drawing
  g <- ggplot2::ggplot(Long, ggplot2::aes(x = value, y = node)) +
    ggplot2::geom_path(group = 1, color = "black") +    
    ggplot2::geom_point(color = "black") +             
    ggplot2::facet_wrap(~ measure, ncol = 1, scales = "free") +
    ggplot2::scale_y_discrete(limits = rev(levels(Long$node))) +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::theme_bw()
  
  # 11. If there is community information, set the text color of the y-axis.
  if(!is.null(comm)){
    g <- g + ggplot2::theme(
      axis.text.y = ggplot2::element_text(colour = cols[order(nodes, decreasing = TRUE)])
    )
  }
  
  return(g)
}

png("Vertical Bridge Expected 2.png",width=1000, height=2000, res=400) 
plot.bridge.Sam1(x = t_b, 
                 include = c("bridgeExpectedInfluence"), 
                 z = FALSE)
dev.off()


png("Longitudinal stability diagram.png", width=1800, height=1800, res=340) 
#pdf("纵向稳定性图.pdf", height=7, width=6)
plot(caseParBoot.t1t2, statistics=c("inExpectedInfluence", "outExpectedInfluence","edge"))
dev.off()

LabwArrow.t1.t2 <- expression("T1" %->% "T2 Network") 

set.seed(1)
norParBoot.t1t2 <- bootnet(net.t1t2, type="nonparametric",Boots=1000, communities=groups, directed=TRUE,
                           statistics=c("inExpectedInfluence", "outExpectedInfluence","edge"),
                           nCores=1)
png("Vertical network non-parametric test.png", width=3000, height=3000, res=400) 
#pdf("Vertical network non-parametric test.pdf", width=8,height=7)
plot(norParBoot.t1t2, order="sample", labels=F, legend=F, meanColor=NA) + 
  xlab("Edge Weight - T1 to T2") + ggtitle(LabwArrow.t1.t2) + 
  theme(plot.title=element_text(hjust=0.5, size=18),
        strip.text=element_blank(), # remove "edge" header
        axis.text.x=element_text(size=18))
dev.off()

pdf("Vertical comparison of network edge differences.pdf", height=10, width=20)
## Edge weights difference test
edgeDiffTest.t1t2 <-  print(plot(norParBoot.t1t2, "edge", plot="difference", onlyNonZero=TRUE, order="sample") + 
  ggtitle(LabwArrow.t1.t2) + 
  theme(plot.title=element_text(hjust=0.5, size=10, face="bold"),
        strip.text=element_blank(), # remove "edge" header
        axis.text.x=element_text(size=2.5),
        axis.text.y=element_text(size=2.5)))
dev.off()

pdf("Comparison of differences in central nodes of the longitudinal network.pdf", height = 7, width = 18)
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

# Filter the "DomainOut" to be "SA" and the "DomainIn" to be "PG"
filtered_data2 <- posWithLabs.t1.t2 %>%
  filter(DomainOut == "SA" & DomainIn == "PG")

# View the filtered data
print(filtered_data1)
print(filtered_data2)

# Check the unique values of the screening results
print(unique(filtered_data1$DomainOut))  # Only include "GA"
print(unique(filtered_data1$DomainIn))   # Only include "PG"

print(unique(filtered_data2$DomainOut))  # Only include "SA"
print(unique(filtered_data2$DomainIn))   # Only include "PG"

# Save the filtered data as a CSV file
library(writexl)
# Save "filtered_data1" as an Excel file
write_xlsx(filtered_data1, "ga到pg.xlsx")

# Save "filtered_data2" as an Excel file
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
