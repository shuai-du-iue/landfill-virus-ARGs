setwd("D:/01科研-城环所/01科研文章/垃圾填埋场-病毒/垃圾填埋场病毒ARGs/2ARGs/viral/ARG host")
library(circlize)

# 读取数据
df1 <- read.csv("type_host.csv", row.names = 1)
df2 <- read.csv("mech_host.csv", row.names = 1)

# 绘图
chordDiagram(
  x = df1,
  grid.col = hcl.colors(18),                     # 颜色方案，数字向量11要和实际的数据相符
  directional = 1,                               # 箭头方向。选项有1,0,-1
  direction.type = c("arrows", "diffHeight"),    # 线条两端的形状
  diffHeight = -0.02,                            # 线条两端距离边缘的距离差
  annotationTrack = c("name", "grid", "axis"),   # 都绘制哪些内容，name标签；grid边缘形状；axis刻度
  annotationTrackHeight = c(0.05, 0.08),         # 标签距离图形的距离; 环形边缘的宽度
  link.arr.type = "big.arrow",                   # 形状"curved", "triangle", "circle", "ellipse".
  link.sort = FALSE,                              # 内部排序
  link.largest.ontop = FALSE,                     # 控制添加链接的顺序，是否基于绝对值?
  transparency = 0.25                            # 线条透明度
)

chordDiagram(
  x = df2,
  grid.col = hcl.colors(17),                     # 颜色方案，数字向量11要和实际的数据相符
  directional = 1,                               # 箭头方向。选项有1,0,-1
  direction.type = c("arrows", "diffHeight"),    # 线条两端的形状
  diffHeight = -0.02,                            # 线条两端距离边缘的距离差
  annotationTrack = c("name", "grid", "axis"),   # 都绘制哪些内容，name标签；grid边缘形状；axis刻度
  annotationTrackHeight = c(0.05, 0.08),         # 标签距离图形的距离; 环形边缘的宽度
  link.arr.type = "big.arrow",                   # 形状"curved", "triangle", "circle", "ellipse".
  link.sort = FALSE,                              # 内部排序
  link.largest.ontop = FALSE,                     # 控制添加链接的顺序，是否基于绝对值?
  transparency = 0.25                            # 线条透明度
)
