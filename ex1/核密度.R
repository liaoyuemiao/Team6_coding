library(sp)
library(splancs)
library(maptools)
#读取学生表
csv <- read.csv("E://seat.csv", header=T, na.strings=c("NA"), sep=',')  
#获取日期
dates <- colnames(csv)[c(3:28)]       

#得到所有当前日期的学生和座次信息，并将座位号转换为x,y
stu <- cbind(csv$uid, csv$gpa.all, (csv["X4月11日"] - 1)%%10+1, floor((csv["X4月11日"] - 1)/10+1))   
#获取座位x坐标
xdf <- data.frame(x = stu[, 3])
#获取座位y坐标
ydf <- data.frame(y = stu[, 4])
#获取学生成绩
scoredf <- data.frame(score = stu[, 2]) 
#关联xy 得到座位坐标
xydf <- cbind(xdf, ydf)
#将座位坐标和成绩关联
seats <- na.omit(cbind(xydf, scoredf), cols = "x")   
#处理缺省值
xydf <- na.omit(xydf, cols = "0") 
#设置教室位置的长宽
xyDim = c(10, 8)   
#通过座位坐标得到图像坐标
xySpDf <- SpatialPoints(coords = xydf)  
#设置图像
grd1 <- GridTopology(cellcentre.offset = c(0, 0),cellsize=c(0.05,0.05), cells.dim = xyDim/c(0.05,0.05))
#设置矩形
polyRegion <- as.points(c(0,10,10,0), c(8,8,0,0)) 
kernelDensity <- spkernel2d(pts = xySpDf, poly = polyRegion, h0 = 1.2, grd = grd1, kernel = "quartic")   
kd_df <- data.frame(KD = kernelDensity)        
kernels <- SpatialGridDataFrame(grid = grd1, data = kd_df)
#展示，输出结果
spplot(kernels)