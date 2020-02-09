# Nettoyage de l'espace de travail
rm(list=ls())

# Lecture des donnees et construction du dataframe "fromages"
nomfile = "fromages_data.txt"
#fromages = read.table(file=nomfile,header=T, sep="\t",dec=",")
fromages = read.table(nomfile, header=T,row.names=1,sep="\t",dec=".")

# Affichage des 5 premieres lignes du dataframe
fromages[1:5,]

#ÿһ�е�����
rownames(fromages) 
#ÿһ�е�����
names(fromages)

#���ÿһ�е���Сֵ�����к��25%��ֵ����λ����ƽ��ֵ�����к��75%���������ֵ
summary(fromages)

#����һ��statbase�����У�rbind�����벢�洢ÿһ�е�summary()�Ľ��
statbase = NULL
for (j in 1:ncol(fromages)){
  statbase =rbind(statbase, summary(fromages[,j]))
}

#statbase��ÿһ�е�������Ӧfromage��names(),ÿһ�е�����
rownames(statbase) = names(fromages)
statbase

#��ÿһ�еı�׼��
ecart.type = apply(fromages, 2, sd)

#����׼���cbind����statbase
statbase = cbind(statbase, round(ecart.type,2))
statbase

# Histogramme en frequences de la premiere variabl
#��һ������ֱ��ͼ
#ֱ��ͼ�������ֲ�ͼ��x�᣺ÿ����Ķ˵㣬y�᣺Ƶ�������ִ�����/Ƶ�ʣ���һ������Ϊ1��
#�����������ݰ��ղ�ͬ�ķ�Χ�ֳɼ����飬ÿһ����������ͬ
#proba ΪTrueƵ�� FalseΪƵ��
#col ����������ɫ
#xlab  ����x�����֣� xlim����x�᷶Χ
#cex ָ�����ŵĴ�С
#main ͼ��ı��⣬ axis����������
j = 1
hist(as.numeric(fromages[,j]), proba=TRUE, col = "cyan", xlab=names(fromages)[j],
     cex.main=1.6, cex.axis=1.4)

#���������м����ض� correlation��������λС��
round(cor(fromages), 2)

#�����ݼ���ÿ����������������ɢ��ͼ
#ɾȥ�����Թ�ϵ�ı���������ضȸߵı���
pairs(fromages)

#����lm��������С�����㷨��һԪ���Իع� �� yi = ��0 + ��1xi +��i
#ʹ��lm��������Ԫ���Իع飬lm(y ~ u + v+ w)
# "."����fromages��ÿһ�б���

# Pas a pas descendante 
reslm = lm(calories ~ .,data = fromages)
summary(reslm)

reslm1 = lm(fromages[,1] ~ fromages[,4] + fromages[,7])
summary(reslm1)

#��ʹ��ACP��Ԥ������������ʹ��classification
#cor=True ����ʹ��������correlation���������� ����ʹ��Э������
#acp�а�����sdev��׼�� loadings������������ centreƽ��ֵ��
acp = princomp(fromages,cor=T,scores=T)

#screeplot - 2 axes retenus ���ɷֵ���ʯͼ
#��ʯԭ��ȡ����ֵ>1�Ҵ���ƽ�з���
#A$x ��������A�еı���x; lm.x����һ��������lm$x����lm��һ������
#acp$sdev^2 ����ȡ��acp�е�sdev��������ƽ�����÷��Ҳ��������ֵ��
#��� �˴�ȡǰ�������ɷ�����������
plot(1:9,acp$sdev^2,type="b",xlab="Nb. de facteurs",ylab="Val. Propres")


#biplot �������ݹ������ɷֵ�ɢ��ͼ ԭ���������ɷ��µķ���
#�������н�180�� forte correlation negative��0�� forte correlation positive��90�� non correlation
#�����Ϊ����
biplot(acp,cex=0.65)

out.dis=dist(fromages,method="euclidean") 
out.hclust=hclust(out.dis, method = "single")
plot(out.hclust) 

rect.hclust(out.hclust, k=3) 
out.id = cutree(out.hclust, k=3) #����ÿһ����������fromage
out.id
#classTab=table(out.id,rownames(fromages))