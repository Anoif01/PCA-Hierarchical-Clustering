# Nettoyage de l'espace de travail
rm(list=ls())

# Lecture des donnees et construction du dataframe "fromages"
nomfile = "fromages_data.txt"
#fromages = read.table(file=nomfile,header=T, sep="\t",dec=",")
fromages = read.table(nomfile, header=T,row.names=1,sep="\t",dec=".")

# Affichage des 5 premieres lignes du dataframe
fromages[1:5,]

#每一行的名字
rownames(fromages) 
#每一列的名字
names(fromages)

#求出每一列的最小值，排列后第25%的值，中位数，平均值，排列后第75%的数，最大值
summary(fromages)

#建立一个statbase，按列（rbind）插入并存储每一列的summary()的结果
statbase = NULL
for (j in 1:ncol(fromages)){
  statbase =rbind(statbase, summary(fromages[,j]))
}

#statbase的每一行的命名对应fromage的names(),每一列的名字
rownames(statbase) = names(fromages)
statbase

#求每一列的标准差
ecart.type = apply(fromages, 2, sd)

#讲标准差按行cbind插入statbase
statbase = cbind(statbase, round(ecart.type,2))
statbase

# Histogramme en frequences de la premiere variabl
#第一个变量直方图
#直方图，质量分布图，x轴：每个组的端点，y轴：频数（出现次数）/频率（归一化，和为1）
#组数：把数据按照不同的范围分成几个组，每一个组的组距相同
#proba 为True频率 False为频数
#col 设置条的颜色
#xlab  设置x轴名字， xlim设置x轴范围
#cex 指定符号的大小
#main 图标的标题， axis设置坐标轴
j = 1
hist(as.numeric(fromages[,j]), proba=TRUE, col = "cyan", xlab=names(fromages)[j],
     cex.main=1.6, cex.axis=1.4)

#计算列于列间的相关度 correlation，保留两位小数
round(cor(fromages), 2)

#把数据集中每个变量，两两绘制散点图
#删去呈线性关系的变量，即相关度高的变量
pairs(fromages)

#利用lm函数做最小二乘算法的一元线性回归 ： yi = β0 + β1xi +εi
#使用lm函数做多元线性回归，lm(y ~ u + v+ w)
# "."代表fromages中每一列变量

# Pas a pas descendante 
reslm = lm(calories ~ .,data = fromages)
summary(reslm)

reslm1 = lm(fromages[,1] ~ fromages[,4] + fromages[,7])
summary(reslm1)

#先使用ACP来预测分类情况，再使用classification
#cor=True 代表使用样本的correlation矩阵做分析 否则使用协方差阵
#acp中包含：sdev标准差 loadings特征向量矩阵 centre平均值等
acp = princomp(fromages,cor=T,scores=T)

#screeplot - 2 axes retenus 主成分的碎石图
#碎石原则：取特征值>1且大于平行分析
#A$x 代表数据A中的变量x; lm.x代表一个变量；lm$x代表lm的一个属性
#acp$sdev^2 代表取出acp中的sdev变量并做平方（得方差，也就是特征值）
#因此 此处取前两个主成分做分析即可
plot(1:9,acp$sdev^2,type="b",xlab="Nb. de facteurs",ylab="Val. Propres")


#biplot 画出数据关于主成分的散点图 原坐标在主成分下的方向
#两变量夹角180° forte correlation negative；0° forte correlation positive；90° non correlation
#结果分为三类
biplot(acp,cex=0.65)

out.dis=dist(fromages,method="euclidean") 
out.hclust=hclust(out.dis, method = "single")
plot(out.hclust) 

rect.hclust(out.hclust, k=3) 
out.id = cutree(out.hclust, k=3) #返回每一类所包含的fromage
out.id
#classTab=table(out.id,rownames(fromages))
