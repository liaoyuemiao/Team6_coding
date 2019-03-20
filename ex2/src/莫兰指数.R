library(sp)
library(lctools)
library(maptools)
library(spdep)
#读取学生表
csv <- read.csv("E://seat.csv", header=T, na.strings=c("NA"), sep=',') 
#获取所有日期
dates <- colnames(csv)[c(3:28)]       

#得到所有当前日期的学生和座次信息，并将座位号转换为x,y
stu <- cbind(csv$uid, csv$gpa.all, (csv["X4月11日"] - 1)%%10, floor((csv["X4月11日"] - 1)/10)) 
#获取座位x坐标
x_df <- data.frame(x = stu[, 3])
#获取座位y坐标
y_df <- data.frame(y = stu[, 4])
#获取学生成绩
score_df <- data.frame(score = stu[, 2]) 
#关联xy 得到座位坐标
xy_df <- cbind(x_df, y_df)
#将座位坐标和成绩关联
seats <- na.omit(cbind(xy_df, score_df), cols = "x")     #处理缺省值
#处理缺省值
xy_df <- na.omit(xy_df, cols = "x") 
#通过座位坐标得到图像坐标
xy_sp_df <- SpatialPoints(coords = xy_df) 

#传入图像坐标得到knn
nbk1 <- knn2nb(knearneigh(xy_sp_df))
#设置距离的最大值
all.linked <- max(unlist(nbdists(nbk1, xy_sp_df)))
#设置距离为(0-> all.linked)
col.nb.0.all <- dnearneigh(xy_sp_df, 0, all.linked)
#得到图像
plot(col.nb.0.all, cbind(xy_df$x , xy_df$y))
snbk1 <- make.sym.nb(col.nb.0.all)
#莫兰分析
moran.test(seats$score, nb2listw(snbk1))
