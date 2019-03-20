nrow()#判断有多少行
ncol()#判断有多少列
bestgpa<-3
#使用Grid
install.packages(grid)
library(grid)
install.packages(ggplot2)
library(ggplot2)
# #创建座位
xvec <- 0.95
yvec <- 0.65
for (i in 1 : 90) {
  if(i!=16&i!=31&i!=46&i!=61&i!=76){
    #给viewport取名字方便搜索，即以座位号为名字
    vp1 <- viewport(x = xvec, y = yvec, width = 0.06, height = 0.06,name = as.character(i))#整数转换为字符
    pushViewport(vp1)
   grid.rect(gp=gpar(col="white"))
    upViewport() #回到上一个viewport（即回到的父节点）,不然现在的长宽会受前一个viewport影响
    xvec<-xvec-0.06
  }
  else{
    xvec <- 0.95
    yvec<-yvec-0.06
    vp1 <- viewport(x = xvec, y = yvec, width = 0.06, height = 0.06,name = as.character(i))#整数转换为字符
    pushViewport(vp1)
   grid.rect(gp=gpar(col="white"))
    upViewport() #回到上一个viewport（即回到的父节点）,不然现在后一个的长宽会受前一个viewport影响
    xvec<-xvec-0.06
    
  }
}

newdata<-read.table(file = "D:/work/RStudio/homework/seat.csv",header = T,sep=",")
number<-12 #所查对象的行号
#填涂查找的同学所坐的座位
for(i in 1 : nrow(newdata))
{
  if(is.na(newdata[i,number])==FALSE&newdata[i,2]>=bestgpa) #判断位置上值是否为缺省值（R语言中，NA代表位置上的值为空，NULL代表连位置都没有，变量为空。）
  {
    seekViewport(as.character(newdata[i,number]))#查找对应的Viewport对象
    grid.rect(gp=gpar(col="white",fill="red"))#col表示边框及字体颜色，fill表示填充颜色
    #grid.text(newdata[i,number])
  }
}
#计算平均值、方差
times1<-0 #记录存在的座位数
l1<-c(1,2,3,4,5,16,17,18,19,20,29,31,32,33,34,35)
l2<-c(6,7,8,9,10,21,22,23,24,25,36,37,38,39,40)
l3<-c(11,12,13,14,15,26,27,28,29,30,41,42,43,44,45)
l4<-c(46,47,48,49,50,61,62,63,64,65,76,77,78,79,80)
l5<-c(51,52,53,54,55,66,67,68,69,70,81,82,83,84,85)
l6<-c(56,57,58,59,60,71,72,73,74,75,86,87,88,89,90)
yflist<-list(l1,l2,l3,l4,l5,l6)
for(i in 1 : nrow(newdata))
{
  if(is.na(newdata[i,number])==FALSE) #判断位置上值是否为缺省值（R语言中，NA代表位置上的值为缺省值，NULL代表连位置都没有，变量为空。）
  {
    times1<-times1+1
  }
}
Mean1<-times1/6 #平均值
paste("平均值为:",Mean1)
Vartotal<-0 #方差子数的和
times2<-0 #记录每个样方中的点数
for (i in 1:6) 
{
  for(j in 1 : nrow(newdata))
  {
    if(is.na(newdata[j,number])==FALSE&newdata[j,number]%in%yflist[[i]]) #判断该座位是否在样方中，注意list[[i]]才表示嵌套list里面的第几个元素
    {
      times2<-times2+1
    }
  }
  Vartotal<-Vartotal+(times2-Mean1)^2
  times2<-0
}
Var1<-Vartotal/(6-1)
VMR<-Var1/Mean1
paste("方差均值比为:",VMR)

if(VMR>1.5){
  paste("此样方为聚集分布")
}else if(VMR<0.5){
  paste("此样方为均匀分布")
}else{
  paste("此样方为随机分布")
}




