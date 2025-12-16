# Analogous to TechPairTrading.R
# using annual (instead of monthly) Tech link to measure similarity

# import packages
{
  # setwd("C:/Users/FanZ/OneDrive - HKUST Connect/code files")
  setwd("~/Library/CloudStorage/OneDrive-个人/CurrentProject/CHNSimilarity")
  library(xts)
  library(lmtest)
  library(readxl)
  library(tcltk)
  library(ggplot2)
  library(dplyr)
  library(quantmod)
  library(lubridate)
  library(sandwich)
  library(matrixStats)
  source('toolkit.R')
  
  num_pairs <- 10 # number of stocks in a pair
}

# import data
{
  # returns
  {# crsp <- read.csv("20082025_monthlyRet.xlsx", header = TRUE)
  crsp <- read_excel("20082025_monthlyRet.xlsx")
  names(crsp)[names(crsp) == "stock_code"] <- "permno"
  names(crsp)[names(crsp) == "monthly_return"] <- "ret"
  crsp$date <- ym(crsp$date) %>%  # 先解析为当月第一天
    ceiling_date(unit = "month") - days(1)  # 跳到下月第一天，再减1天
  # crsp$ret <- as.numeric(crsp$ret)
  rets <- xts()
  stocks <- unique(crsp$permno)
  pb <- tkProgressBar("Progress: ", "Finished %", 0, 100)
  time.begin <- Sys.time()
  for(i in 1:length(stocks)){
    tmp <- crsp[c(crsp$permno==stocks[i]),]
    tmp <- as.xts(tmp$ret,
                  order.by=as.Date(tmp$date))
    rets <- cbind(rets, tmp)
    
    info <- sprintf("Finished %d%%", round(i*100/length(stocks)))
    setTkProgressBar(pb, i*100/length(stocks), sprintf("Progress (%s)", info), info)
  }
  time.end <- Sys.time()
  close(pb)
  sprintf("Running time: ")
  time.end-time.begin
  save(rets, file = "rets.RData")
  
  index(rets) <- as.Date(index(rets))
  allStocks <- stocks
  colnames(rets) <- allStocks
  # rets <- rets[-c(1:12),] 
  }
  # further process rets
  {
    rets <- rets['2007/2023']
    clean_xts <- function(rets) {
      # 确保输入是xts对象
      if(!is.xts(rets)) stop("Input must be an xts object")
      
      # 遍历每一列
      to_keep <- sapply(colnames(rets), function(col) {
        # 提取列数据并转换为普通向量
        x <- as.vector(rets[, col])
        
        # 计算NA的游程编码
        na_rle <- rle(is.na(x))
        
        # 检查是否有>=60的连续NA
        !any(na_rle$values & (na_rle$lengths >= 180))
      })
      
      # 返回清理后的xts对象
      rets[, to_keep]
    }
    
    # 使用示例
    rets_clean <- clean_xts(rets)
    
    # 结果统计
    cat("原始股票数量:", ncol(rets), "\n")
    cat("清理后股票数量:", ncol(rets_clean), "\n")
    cat("删除的股票数量:", ncol(rets) - ncol(rets_clean), "\n")
    rets <- rets_clean
  }
  
  # market cap
  {
    names(crsp)[names(crsp) == "Msmvosd"] <- "MktCap"
    MktCap <- xts()
    pb <- tkProgressBar("Progress: ", "Finished %", 0, 100)
    time.begin <- Sys.time()
    for(i in 1:length(colnames(rets))){
      tmp <- crsp[c(crsp$permno==colnames(rets)[i]),]
      tmp <- as.xts(tmp$MktCap, order.by=as.Date(tmp$date))
      MktCap <- cbind(MktCap, tmp)
      info <- sprintf("Finished %d%%", round(i*100/length(colnames(rets))))
      setTkProgressBar(pb, i*100/length(colnames(rets)), sprintf("Progress (%s)", info), info)
    }
    time.end <- Sys.time()
    close(pb)
    sprintf("Running time: ")
    time.end-time.begin
    
    
    # MktCap <- log(MktCap)
    index(MktCap) <- as.Date(index(MktCap))
    colnames(MktCap) <- colnames(rets)
    save(MktCap, file = "MktCap_month.RData")
  }
  # further process MktCap
  {
    MktCap <- MktCap['2007/2023']
    
    # 使用示例
    MktCap_clean <- clean_xts(MktCap)
    
    # 结果统计
    cat("原始股票数量:", ncol(MktCap), "\n")
    cat("清理后股票数量:", ncol(MktCap_clean), "\n")
    cat("删除的股票数量:", ncol(MktCap) - ncol(MktCap_clean), "\n")
    MktCap <- MktCap_clean
  }
  write.csv(data.frame(Date = index(MktCap), coredata(MktCap)), 
            file = "MktCap.csv", 
            row.names = FALSE)
  
  # risk-free data
  {
    FF <- read.csv("factors/FF_Factor.csv", header = TRUE)
    FF <- FF[FF$date > as.Date("2007-01-01"), ]
    FF <- FF[FF$date < as.Date("2024-01-01"), ]
    rf <- xts(FF$RF, order.by = index(rets["/2023"]))
    colnames(rf) <- 'rf'
  }
  # factor data
  {
    FF <- read.csv("factors/FF_Factor.csv", header = TRUE)
    FF <- FF[FF$date > as.Date("2007-01-01"), ]
    FF <- FF[FF$date < as.Date("2024-01-01"), ]
    FF3 <- FF[,c(-1,-2,-6,-7)]
    FF3 <- xts(FF3, order.by = index(rets))
    
    FF5 <- FF[,c(-1,-2)]
    FF5 <- xts(FF5, order.by = index(rets)) 
    
    HXZ4 <- read.csv("factors/HXZ_Factor.csv", header = TRUE)
    HXZ4 <- HXZ4[HXZ4$date > as.Date("2007-01-01"), ]
    HXZ4 <- HXZ4[HXZ4$date < as.Date("2024-01-01"), ]
    HXZ4 <- HXZ4[,c(-1,-2)]
    HXZ4 <- xts(HXZ4, order.by = index(rets)) 
    
    SY4 <- read.csv("factors/SY_Factor.csv", header = TRUE)
    SY4 <- SY4[SY4$date > as.Date("2007-01-01"), ]
    SY4 <- SY4[SY4$date < as.Date("2024-01-01"), ]
    SY4 <- SY4[,c(-1,-2)]
    SY4 <- xts(SY4, order.by = index(rets)) 
    
    DHS3 <- read.csv("factors/DHS_Factor.csv", header = TRUE)
    DHS3 <- DHS3[DHS3$date > as.Date("2007-01-01"), ]
    DHS3 <- DHS3[DHS3$date < as.Date("2024-01-01"), ]
    DHS3 <- DHS3[,c(-1,-2)]
    DHS3 <- xts(DHS3, order.by = index(rets)) 
    
    Carhart4 <- read.csv("factors/Carhart_Factor.csv", header = TRUE)
    Carhart4 <- Carhart4[Carhart4$date > as.Date("2007-01-01"), ]
    Carhart4 <- Carhart4[Carhart4$date < as.Date("2024-01-01"), ]
    Carhart4 <- Carhart4[,c(-1,-2)]
    Carhart4 <- xts(Carhart4, order.by = index(rets)) 
    # Carhart4 <- cbind(FF3[,c(1,2)], Carhart4)
    
    China4 <- read.csv("factors/China4_Factor.csv", header = TRUE)
    China4 <- China4[China4$date > 20070100, ]
    China4 <- China4[,c(-1)]
    China4 <- xts(China4, order.by = index(rets["/2021"])) 
    China4 <- China4/100
    
    CAPM <- read.csv("factors/CAPM_Factor.csv", header = TRUE)
    CAPM <- CAPM[CAPM$date > as.Date("2007-01-01"), ]
    CAPM <- CAPM[CAPM$date < as.Date("2024-01-01"), ]
    CAPM <- CAPM[,c(-1,-2)]
    CAPM <- xts(CAPM, order.by = index(rets)) 
    CAPM <- CAPM
    
    SC4 <- read.csv("factors/SC4_Factor.csv", header = TRUE)
    SC4 <- SC4[SC4$date > as.Date("2007-01-01"), ]
    SC4 <- SC4[SC4$date < as.Date("2024-01-01"), ]
    SC4 <- SC4[,c(-1,-2)]
    SC4 <- xts(SC4, order.by = index(rets)) 

    # factors <- cbind(rf, FF3, FF_mom, FF_str)
    # factors <- cbind(factors, q_fac)
    # factors <- cbind(factors, FF_liq)
    factors <- cbind(rf, FF5, HXZ4)
    save(factors, file='factors.RData')
  }
}

# compute RetDiff OLD VERSION
{
  # Cret
  {
    Cret <- rets['2012/2023']
    Cret[] <- NA
    pb <- tkProgressBar("Progress: ", "Finished %", 0, 100)
    time.begin <- Sys.time()
    for(i in 1:length(index(Cret))){
      idx <- index(Cret)[i]
      year <- as.integer(format(idx,"%Y")) # 提取年份（如2014）
      # 获取过去5年的收益率数据（year-5到year-1）
      rets_5y <- rets[paste(as.character(year-5),as.character(year-1),sep = '/')]
      rets_5y <- na.locf(rets_5y, fromLast = FALSE)
      # 剔除含有NA值的股票
      rets_5y <- xts(t(na.omit(t(as.matrix(rets_5y)))), order.by = index(rets_5y))
      constant_cols <- apply(rets_5y, 2, function(x) var(x, na.rm = TRUE) == 0)
      rets_5y <- rets_5y[, !constant_cols]
      stocks <- names(rets_5y) # 提取有效股票代码
      # corr matrix
      cor_mat <- cor(rets_5y)
      cor_mat[cor_mat==1] <- -1 
      # 对每只股票计算 
      for(j in 1:length(stocks)){
        stock <- stocks[j] # 当前股票代码
        # 按相关性降序排序（找到与当前股票最相关的股票）
        cor_values <- as.numeric(cor_mat[stock, ])  
        names(cor_values) <- colnames(cor_mat)  # 保留股票列名
        sorted_cor <- sort(cor_values, decreasing = TRUE)
        # sorted_cor <- sort(cor_mat[stock,], decreasing = TRUE)
        # 选取相关性最高的num_pairs只股票
        pair_stocks <- names(sorted_cor)[1:num_pairs] 
        # 计算等权重组合收益, 用相关股票的平均收益率作为当前股票的预测值
        Cret[i, stock] <- rowMeans(rets[idx,pair_stocks])
      }
      info <- sprintf("Finished %d%%", round(i*100/length(index(Cret))))
      setTkProgressBar(pb, i*100/length(index(Cret)), sprintf("Progress (%s)", info), info)
    }
    time.end <- Sys.time()
    close(pb)
    sprintf("Running time: ")
    time.end-time.begin
  }
  save(Cret, file = "Cret.RData")
  write.csv(data.frame(Date = index(Cret), coredata(Cret)), 
            file = "Cret.csv", 
            row.names = FALSE)

  {
    Lret <- rets['2012/2023']
    Lret[] <- NA
    pb <- tkProgressBar("Progress: ", "Finished %", 0, 100)
    time.begin <- Sys.time()
    for(i in 1:length(index(Lret))){
      idx <- index(Lret)[i]
      Cret_stk <- colnames(t(na.omit(t(as.matrix(Cret[idx,])))))
      Lret[idx, Cret_stk] <- rets[idx, Cret_stk]
      info <- sprintf("Finished %d%%", round(i*100/length(index(Lret))))
      setTkProgressBar(pb, i*100/length(index(Lret)), sprintf("Progress (%s)", info), info)
    }
    time.end <- Sys.time()
    close(pb)
    sprintf("Running time: ")
    time.end-time.begin
  }
  save(Lret, file = "Lret.RData")
  write.csv(data.frame(Date = index(Lret), coredata(Lret)), 
            file = "Lret.csv", 
            row.names = FALSE)
  
  {
    RetDiff <- Cret # 初始化RetDiff矩阵（与Cret同结构）
    RetDiff[] <- NA
    pb <- tkProgressBar("Progress: ", "Finished %", 0, 100)
    time.begin <- Sys.time()
    # 按日期循环
    for(i in 1:length(index(RetDiff))){
      idx <- index(RetDiff)[i] # 当前日期
      year <- as.integer(format(idx,"%Y")) # 提取年份
      # 获取过去5年的收益率数据（year-5到year-1）
      rets_5y <- rets[paste(as.character(year-5),as.character(year-1),sep = '/')]
      # 剔除含有NA值的股票
      rets_5y <- xts(t(na.omit(t(as.matrix(rets_5y)))), order.by = index(rets_5y))
      constant_cols <- apply(rets_5y, 2, function(x) var(x, na.rm = TRUE) == 0)
      rets_5y <- rets_5y[, !constant_cols]
      stocks <- names(rets_5y) # 有效股票代码
      # 计算收益率相关系数矩阵（过去5年）
      cor_mat <- cor(rets_5y)
      cor_mat[cor_mat==1] <- -1 # 将自相关系数设为-1（避免选自己）
      # 对每只股票计算
      for(j in 1:length(stocks)){
        stock <- stocks[j] # 当前股票
        # 按相关性降序排序
        sorted_cor <- sort(cor_mat[stock,], decreasing = TRUE)
        # 选取相关性最高的num_pairs只股票
        pair_stocks <- names(sorted_cor)[1:num_pairs] 
        # 计算beta系数：当前股票 vs 关联股票组合的平均收益
        beta_c <- beta(rets_5y[,stock], 
                       rowMeans(rets_5y[,pair_stocks]))
        # 计算超额收益差异：beta调整后的组合超额收益 - 个股实际超额收益
        RetDiff[idx,stock] <- beta_c*(Cret[idx,stock]-rf[idx]) - (Lret[idx,stock]-rf[idx])
      }
      info <- sprintf("Finished %d%%", round(i*100/length(index(RetDiff))))
      setTkProgressBar(pb, i*100/length(index(RetDiff)), sprintf("Progress (%s)", info), info)
    }
    close(pb)
    time.end <- Sys.time()
    sprintf("Running time: ")
    time.end-time.begin
  }
  save(RetDiff, file = "RetDiff.RData")
  write.csv(data.frame(Date = index(RetDiff), coredata(RetDiff)), 
            file = "RetDiff.csv", 
            row.names = FALSE)
}

# Table 1, sorting portfolio, EW
{
  time.begin <- Sys.time()
  raw_return <- sorting_portfolio_new_EW(RetDiff, rets, 10)
  time.end <- Sys.time()
  sprintf("Running time: ")
  time.end - time.begin
  save(raw_return, file = "raw_return_EW.RData")
  # save raw_return and transform the frequency in Python
  write.zoo(raw_return, 'raw_return_EW.csv', col.names=TRUE, sep=",")
  
  # T1, column 1, 
  raw_mean <- colMeans(raw_return)
  raw_t <- raw_mean
  for(i in 1:length(raw_mean)){
    model <- lm(raw_return[,i]~1)
    raw_t[i] <- NW_tstats(model)[1]
  }
  
  # T1, column 2, FF3 pricing error
  FF3_alpha <- raw_mean
  FF3_t     <- raw_t
  for(i in 1:length(FF3_alpha)){
    # model <- lm(raw_return[,i]~FF3[index(raw_return),])
    model <- lm(raw_return[,i]~factors[index(raw_return),c(2,3,4)])
    FF3_alpha[i] <- model$coefficients[1]
    FF3_t[i] <- NW_tstats(model)[1]
  }
  
  # T1, column x, Carhart4
  Carhart4_alpha <- raw_mean
  Carhart4_t     <- raw_t
  for(i in 1:length(Carhart4_alpha)){
    model <- lm(raw_return[,i]~Carhart4[index(raw_return),c(1,2,3,4)])
    Carhart4_alpha[i] <- model$coefficients[1]
    Carhart4_t[i] <- NW_tstats(model)[1]
  }
  
  # T1, column 3, FF5(plus mom & ST-reversal) pricing error
  FF5_alpha <- raw_mean
  FF5_t     <- raw_t
  for(i in 1:length(FF5_alpha)){
    model <- lm(raw_return[,i]~factors[index(raw_return),c(2,3,4,5,6)])
    FF5_alpha[i] <- model$coefficients[1]
    FF5_t[i] <- NW_tstats(model)[1]
  }
  
  # T1, column x, HXZ4
  HXZ4_alpha <- raw_mean
  HXZ4_t     <- raw_t
  for(i in 1:length(HXZ4_alpha)){
    model <- lm(raw_return[,i]~HXZ4[index(raw_return),c(1,2,3,4)])
    HXZ4_alpha[i] <- model$coefficients[1]
    HXZ4_t[i] <- NW_tstats(model)[1]
  }
  
  # T1, column x, SY4
  SY4_alpha <- raw_mean
  SY4_t     <- raw_t
  for(i in 1:length(SY4_alpha)){
    model <- lm(raw_return[,i]~SY4[index(raw_return),c(1,2,3,4)])
    SY4_alpha[i] <- model$coefficients[1]
    SY4_t[i] <- NW_tstats(model)[1]
  }
  
  # T1, column x, DHS3
  DHS3_alpha <- raw_mean
  DHS3_t     <- raw_t
  for(i in 1:length(DHS3_alpha)){
    model <- lm(raw_return[,i]~DHS3[index(raw_return),c(1,2,3)])
    DHS3_alpha[i] <- model$coefficients[1]
    DHS3_t[i] <- NW_tstats(model)[1]
  }
  
  # T1, column x, CAPM
  CAPM_alpha <- raw_mean
  CAPM_t     <- raw_t
  for(i in 1:length(CAPM_alpha)){
    model <- lm(raw_return[,i]~CAPM[index(raw_return),c(1)])
    CAPM_alpha[i] <- model$coefficients[1]
    CAPM_t[i] <- NW_tstats(model)[1]
  }
  
  # T1, column x, SC4
  SC4_alpha <- raw_mean
  SC4_t     <- raw_t
  for(i in 1:length(SC4_alpha)){
    model <- lm(raw_return[,i]~SC4[index(raw_return),])
    SC4_alpha[i] <- model$coefficients[1]
    SC4_t[i] <- NW_tstats(model)[1]
  }
  
  # T1, column x, China4, JFE
  China4_alpha <- raw_mean
  China4_t     <- raw_t
  for(i in 1:length(China4_alpha)){
    model <- lm(raw_return[index(China4),i]~China4['2012/2021'][c(-1),])
    China4_alpha[i] <- model$coefficients[1]
    China4_t[i] <- NW_tstats(model)[1]
  }
  
  NGroup <- 10
  table1b_value <- matrix(0, ncol = 4, nrow = NGroup+1)
  table1b_value[,1] <- raw_mean * 100
  table1b_value[,2] <- FF3_alpha * 100
  table1b_value[,3] <- FF5_alpha * 100
  table1b_value[,4] <- FF6_alpha * 100
  colnames(table1b_value) <- c('Raw Return',  'Three-factor alpha',  'Five-factor alpha', 'Six-factor alpha')
  rownames(table1b_value) <- c('Decile 1','2','3','4','5','6','7','8','9','Decile 10', '10-1')
  
  table1b_tstat <- matrix(0, ncol = 4, nrow = NGroup+1)
  table1b_tstat[,1] <- raw_t
  table1b_tstat[,2] <- FF3_t
  table1b_tstat[,3] <- FF5_t
  table1b_tstat[,4] <- FF6_t
  colnames(table1b_tstat) <- c('t_Raw Return',  't_Three-factor alpha',  't_Five-factor alpha', 't_Six-factor alpha')
  rownames(table1b_tstat) <- c('Decile 1','2','3','4','5','6','7','8','9','Decile 10', '10-1')
  
  # Figure 1, EW
  # New annual return plot see data_transform.ipynb file
  barplot(raw_return$`10-1`*100)
  {
    # 将月度收益率数据转换为年度算术加总收益率
    annual_returns <- apply.yearly(raw_return$`10-1`, FUN = sum) * 100  # 算术加总并转换为百分比
    # 提取年份作为标签
    year_labels <- format(index(annual_returns), "%Y")
    # 创建更美观的柱状图
    barplot(as.numeric(annual_returns),
            names.arg = year_labels,  # 使用年份作为x轴标签
            main = "Equal-Weighted 10-1 Portfolio",
            ylab = "Annual Returns (%)",
            cex.main = 0.75,  # 调整标题字号（默认1.2）
            border = NA,
            col = ifelse(annual_returns >= 0, rich8equal[c(2)],rich8equal[c(4)]),  # 正负收益不同颜色
            space = 0.35,
            las = 2,  # 垂直x轴标签
            cex.lab = 0.7,
            cex.axis=0.7,
            cex.names = 0.7)  # 调整年份标签大小
  }
  
}

# Table 1, sorting portfolio, VW
{
  time.begin <- Sys.time()
  raw_return_VW <- sorting_portfolio_new_VW(RetDiff, rets, MktCap, 10)
  time.end <- Sys.time()
  sprintf("Running time: ")
  time.end - time.begin
  save(raw_return_VW, file = "raw_return_VW.RData")
  # save raw_return and transform the frequency in Python
  write.zoo(raw_return_VW, 'raw_return_VW.csv', col.names=TRUE, sep=",")
  
  # T1, column 1,
  raw_mean <- colMeans(raw_return_VW)
  raw_t <- raw_mean
  for(i in 1:length(raw_mean)){
    model <- lm(raw_return_VW[,i]~1)
    raw_t[i] <- NW_tstats(model)[1]
  }
  
  # T1, column 2, FF3 pricing error
  FF3_alpha <- raw_mean
  FF3_t     <- raw_t
  for(i in 1:length(FF3_alpha)){
    # model <- lm(raw_return[,i]~FF3[index(raw_return),])
    model <- lm(raw_return_VW[,i]~factors[index(raw_return),c(2,3,4)])
    FF3_alpha[i] <- model$coefficients[1]
    FF3_t[i] <- NW_tstats(model)[1]
  }
  
  # T1, column x, Carhart4
  Carhart4_alpha <- raw_mean
  Carhart4_t     <- raw_t
  for(i in 1:length(Carhart4_alpha)){
    model <- lm(raw_return_VW[,i]~Carhart4[index(raw_return),c(1,2,3,4)])
    Carhart4_alpha[i] <- model$coefficients[1]
    Carhart4_t[i] <- NW_tstats(model)[1]
  }
  
  # T1, column 3, FF5(plus mom & ST-reversal) pricing error
  FF5_alpha <- raw_mean
  FF5_t     <- raw_t
  for(i in 1:length(FF5_alpha)){
    model <- lm(raw_return_VW[,i]~factors[index(raw_return),c(2,3,4,5,6)])
    FF5_alpha[i] <- model$coefficients[1]
    FF5_t[i] <- NW_tstats(model)[1]
  }
  
  # T1, column x, HXZ4
  HXZ4_alpha <- raw_mean
  HXZ4_t     <- raw_t
  for(i in 1:length(HXZ4_alpha)){
    model <- lm(raw_return_VW[,i]~HXZ4[index(raw_return),c(1,2,3,4)])
    HXZ4_alpha[i] <- model$coefficients[1]
    HXZ4_t[i] <- NW_tstats(model)[1]
  }
  
  # T1, column x, SY4
  SY4_alpha <- raw_mean
  SY4_t     <- raw_t
  for(i in 1:length(SY4_alpha)){
    model <- lm(raw_return_VW[,i]~SY4[index(raw_return),c(1,2,3,4)])
    SY4_alpha[i] <- model$coefficients[1]
    SY4_t[i] <- NW_tstats(model)[1]
  }
  
  # T1, column x, DHS3
  DHS3_alpha <- raw_mean
  DHS3_t     <- raw_t
  for(i in 1:length(DHS3_alpha)){
    model <- lm(raw_return_VW[,i]~DHS3[index(raw_return),c(1,2,3)])
    DHS3_alpha[i] <- model$coefficients[1]
    DHS3_t[i] <- NW_tstats(model)[1]
  }
  
  # T1, column x, CAPM
  CAPM_alpha <- raw_mean
  CAPM_t     <- raw_t
  for(i in 1:length(CAPM_alpha)){
    model <- lm(raw_return_VW[,i]~CAPM[index(raw_return),c(1)])
    CAPM_alpha[i] <- model$coefficients[1]
    CAPM_t[i] <- NW_tstats(model)[1]
  }
  
  # T1, column x, SC4
  SC4_alpha <- raw_mean
  SC4_t     <- raw_t
  for(i in 1:length(SC4_alpha)){
    model <- lm(raw_return_VW[,i]~SC4[index(raw_return),])
    SC4_alpha[i] <- model$coefficients[1]
    SC4_t[i] <- NW_tstats(model)[1]
  }
  
  # T1, column x, China4, JFE
  China4_alpha <- raw_mean
  China4_t     <- raw_t
  for(i in 1:length(China4_alpha)){
    model <- lm(raw_return_VW[index(China4),i]~China4['2012/2021'][c(-1),])
    China4_alpha[i] <- model$coefficients[1]
    China4_t[i] <- NW_tstats(model)[1]
  }
  
  NGroup <- 10
  table1a_value <- matrix(0, ncol = 4, nrow = NGroup+1)
  table1a_value[,1] <- raw_mean * 100
  table1a_value[,2] <- CAPM_alpha * 100
  table1a_value[,3] <- FF3_alpha * 100
  table1a_value[,4] <- FF5_alpha * 100
  colnames(table1a_value) <- c('Raw Return',  'CAPM alpha',  'Three-factor alpha', 'Five-factor alpha')
  rownames(table1a_value) <- c('Decile 1','2','3','4','5','6','7','8','9','Decile 10', '10-1')
  
  table1a_tstat <- matrix(0, ncol = 4, nrow = NGroup+1)
  table1a_tstat[,1] <- raw_t
  table1a_tstat[,2] <- CAPM_t
  table1a_tstat[,3] <- FF3_t
  table1a_tstat[,4] <- FF5_t
  colnames(table1a_tstat) <- c('t_Raw Return',  't_CAPM alpha',  't_Three-factor alpha', 't_Five-factor alpha')
  rownames(table1a_tstat) <- c('Decile 1','2','3','4','5','6','7','8','9','Decile 10', '10-1')
  
  # Figure 1, VW
  # New annual return plot see data_transform.ipynb file
  barplot(raw_return_VW$`10-1`*100)
  {
    # 将月度收益率数据转换为年度算术加总收益率
    annual_returns_VW <- apply.yearly(raw_return_VW$`10-1`, FUN = sum) * 100  # 算术加总并转换为百分比
    # 提取年份作为标签
    year_labels <- format(index(annual_returns_VW), "%Y")
    # 创建更美观的柱状图
    barplot(as.numeric(annual_returns_VW),
            names.arg = year_labels,  # 使用年份作为x轴标签
            main = "Value-Weighted 10-1 Portfolio",
            cex.main = 0.75,  # 调整标题字号（默认1.2）
            ylab = "Annual Returns (%)",
            cex.lab = 0.7,
            border = NA,
            col = ifelse(annual_returns_VW > 0, rich8equal[c(2)], rich8equal[c(4)]),  # 正负收益不同颜色
            space = 0.35,
            las = 2,  # 垂直x轴标签
            cex.axis=0.7,
            cex.names = 0.7)  # 调整年份标签大小
  }
  
}

# Table 2, loadings on 6 factors, EW
{
  idx <- index(raw_return) 
  table2b_value <- matrix(0, ncol=6, nrow=NGroup+1)
  table2b_tstat <- matrix(0, ncol=6, nrow=NGroup+1)
  rownames(table2b_value) <- c('Decile 1','2','3','4','5','6','7','8','9','Decile 10', '10-1')
  colnames(table2b_value) <- c('Alpha','Beta_MKT','Beta_SMB','Beta_HML','Beta_RMW','Beta_CMA')
  rownames(table2b_tstat) <- c('Decile 1','2','3','4','5','6','7','8','9','Decile 10', '10-1')
  colnames(table2b_tstat) <- c('Alpha','Beta_MKT','Beta_SMB','Beta_HML','Beta_RMW','Beta_CMA')
  for(i in 1:(NGroup+1)){
    model <- lm(raw_return[idx,i]~FF5[idx,])
    table2b_value[i,] <- model$coefficients
    table2b_tstat[i,] <- NW_tstats(model)
  }
  table2b_value <- table2b_value %>%
    as.data.frame() %>%   
    mutate(Alpha = Alpha*100) %>%
    as.matrix()
}

# Table 2, loadings on 6 factors, VW
{
  idx <- index(raw_return_VW) 
  table2a_value <- matrix(0, ncol=6, nrow=NGroup+1)
  table2a_tstat <- matrix(0, ncol=6, nrow=NGroup+1)
  rownames(table2a_value) <- c('Decile 1','2','3','4','5','6','7','8','9','Decile 10', '10-1')
  colnames(table2a_value) <- c('Alpha','Beta_MKT','Beta_SMB','Beta_HML','Beta_RMW','Beta_CMA')
  rownames(table2a_tstat) <- c('Decile 1','2','3','4','5','6','7','8','9','Decile 10', '10-1')
  colnames(table2a_tstat) <- c('Alpha','Beta_MKT','Beta_SMB','Beta_HML','Beta_RMW','Beta_CMA')
  for(i in 1:(NGroup+1)){
    model <- lm(raw_return_VW[idx,i]~FF5[idx,])
    table2a_value[i,] <- model$coefficients
    table2a_tstat[i,] <- NW_tstats(model)
  }
  table2a_value <- table2a_value %>%
    as.data.frame() %>%   
    mutate(Alpha = Alpha*100) %>%
    as.matrix()
}

# Table 4, loadings on alternative factors, EW
{
    # T1, column x, Carhart4
    table_adhoc <- matrix(0, ncol=2, nrow=5)
    model <- lm(raw_return[,11]~Carhart4[index(raw_return),c(1,2,3,4)])
    table_adhoc[,1] <- model$coefficients
    table_adhoc[,2] <- NW_tstats(model)
    
    # T1, column x, China4
    table_adhoc <- matrix(0, ncol=2, nrow=5)
    model <- lm(raw_return[index(China4),i]~China4['2012/2021'][c(-1),])
    table_adhoc[,1] <- model$coefficients
    table_adhoc[,2] <- NW_tstats(model)
    
    # T1, column x, SC4
    table_adhoc <- matrix(0, ncol=2, nrow=5)
    model <- lm(raw_return[,11]~SC4[index(raw_return),c(1,2,3,4)])
    table_adhoc[,1] <- model$coefficients
    table_adhoc[,2] <- NW_tstats(model)
    
    # T1, column x, HXZ4
    table_adhoc <- matrix(0, ncol=2, nrow=5)
    model <- lm(raw_return[,11]~HXZ4[index(raw_return),c(1,2,3,4)])
    table_adhoc[,1] <- model$coefficients
    table_adhoc[,2] <- NW_tstats(model)
}

# Factor corr
{
  factor_adhoc <- cbind(raw_return_VW$`10-1`,raw_return$`10-1`,
                        FF5[,c(1,2,3,4,5)], Carhart4[,c(4)],
                        China4[,c(2,4)],SC4[,c(2,3)])
  factor_adhoc <- factor_adhoc[index(raw_return),]
  factor_adhoc <- factor_adhoc['/2021']
  names(factor_adhoc) <- c('PRG-VW','PRG-EW',
                           names(FF5),names(Carhart4[,c(4)]),
                           names(China4[,c(2,4)]), names(SC4[,c(2,3)]))
  cor(factor_adhoc)
  
  library(corrplot)
  cor_matrix <- cor(factor_adhoc)
  corrplot(cor_matrix, 
           method = "color",        # 颜色方块
           type = "lower",          # 只显示上三角
           number.cex = 0.45,
           addCoef.col = "black",   # 添加相关系数
           tl.cex = 0.45,            # 变量标签字号（默认1.0）
           tl.col = rich8equal[c(2)],     # 标签颜色
           col = rich8equal,
           diag = TRUE)            # 不显示对角线

}


# T7, for hedge portfolio
{
  long_hrz_ret <- matrix(0, nrow = as.integer(dim(RetDiff)[1]/6)+2, ncol = 6)
  long_horizon_ret <- function(sort_var, ret, groups=10){
    res <- matrix(0, nrow = as.integer((dim(sort_var)[1]-6)/6), ncol = 6)
    date_list <- index(sort_var)
    pb <- progress_bar$new(total = length(date_list))
    for(i in 2:(length(date_list)-6)){
      if(i%%6==2){
        # ÿ6??????һ?? portfolio formation
        sv_prev <- sort_var[date_list[i-1],]
        sv_prev <- xts(t(na.omit(t(as.matrix(sv_prev)))), order.by = date_list[i-1])
        sorted_sv <- sort(as.data.frame(sv_prev))
        sep1 <- as.integer(dim(sorted_sv)[2]/groups)
        Low <- names(sorted_sv)[1:sep1]
        High <- names(sorted_sv)[(dim(sorted_sv)[2]-sep1):dim(sorted_sv)[2]]
        for(j in 1:6){
          res[as.integer((i-2)/6+1),j] <- rowMeans(t(na.omit(t(as.matrix(ret[date_list[i+j-1], High]))))) - rowMeans(t(na.omit(t(as.matrix(ret[date_list[i+j-1], Low])))))
        }
      }
      pb$tick()
      Sys.sleep(1 / length(date_list))
    }
    return(res)
  }
  
  HML_RD <- long_horizon_ret(RetDiff, Lret)
  HML_Cr <- long_horizon_ret(Cret, Lret)
  HML_Lr <- long_horizon_ret(Lret, Lret)
  
  table7b_value <- matrix(0, nrow = 6, ncol = 3)
  table7b_tstat <- matrix(0, nrow = 6, ncol = 3)
  colnames(table7b_value) <- c('HML_RetDiFF','HML_Cret','HML_Lret')
  rownames(table7b_value) <- c('1','2','3','4','5','6')
  colnames(table7b_tstat) <- c('HML_RetDiFF','HML_Cret','HML_Lret')
  rownames(table7b_tstat) <- c('1','2','3','4','5','6')
  
  table7b_value[,1] <- colMeans(HML_RD)*100
  table7b_value[,2] <- colMeans(HML_Cr)*100
  table7b_value[,3] <- colMeans(HML_Lr)*100
  
  for(i in 1:6){
    table7b_tstat[i,1] <- NW_tstats(lm(HML_RD[,i]~1))[1]
  }
  for(i in 1:6){
    table7b_tstat[i,2] <- NW_tstats(lm(HML_Cr[,i]~1))[1]
  }
  for(i in 1:6){
    table7b_tstat[i,3] <- NW_tstats(lm(HML_Lr[,i]~1))[1]
  }
  
}






