x <- rt(2000,2)  # 定义变量的范围和间隔
x <- seq(-10, 10, length.out = 10000)
df <- 2  # t分布的自由度
mean <- 0  # 正态分布的均值
sd <- 1  # 正态分布的标准差

# 计算t分布的密度函数值和正态分布的密度函数值
t_density <- dt(x, 2)
normal_density <- dnorm(x, 0, 1)

# 计算t分布密度函数相对于正态分布密度函数的最大值
max_relative_value <- max(t_density/normal_density)#不一定是对应的x 写成一个函数以保证让他们相比的时候是同一个x去相比

print(max_relative_value)  # 输出最大值









# 定义函数
f <- function(x) {
  dt(x, 2)/dnorm(x, 0, 1)
}

# 寻找最大值点
result <- optimize(f, interval = c(-10, 10))

max_point <- result$maximum  # 最大值点
max_value <- result$objective  # 最大值

print(max_point)  # 输出最大值点
print(max_value)  # 输出最大值





# 导入stats包
library(stats)

# 定义目标分布（标准正态分布）的概率密度函数
target_density <- function(x) {
  dnorm(x, mean = 0, sd = 1)
}

# 定义辅助分布（自由度为2的t分布）的概率密度函数
aux_density <- function(x) {
  dt(x, df = 3)
}

# 计算目标分布和辅助分布比值的最大值
x <- seq(-10, 10, length.out = 10000)
ratio <- target_density(x) / aux_density(x)
max_ratio <- max(ratio, na.rm = TRUE)

max_ratio
