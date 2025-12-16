library(xts)
library(lmtest)
library(tcltk)
library(ggplot2)
library(dplyr)
library(quantmod)
library(sandwich)
library(matrixStats)
# setwd("D:/OneDrive/Pair Trading Rep/code files")


# function toolkit
{
  beta <- function(ret_m, Cret){
    # compute beta_C
    # ret_m: Y
    # Cret: X
    res <- 0
    fit <- lm(ret_m~Cret)
    res <- fit$coefficients[2]
    names(res) <- NULL
    return(res)
  }   
  
  NW_tstats <- function(model){
    # Newey West t-statistics
    # model: LR fit
    res <- coeftest(model, vcov. = NeweyWest(model, lag=6, prewhite = F))[,3]
    return(res)
  } 
  
  sorting_portfolio <- function(sort_var, groups){
    #
    # Table 1, single sort
    # compute sorting portfolio return, from 1 to 10 plus long/short portfolio
    # groups: number of groups
    #
    date <- index(sort_var)
    raw_return <- matrix(0, ncol=groups+1,nrow = (length(date)-1))
    raw_return <- xts(raw_return, order.by = date[2:length(date)])
    names(raw_return) <- c(as.character(c(1:groups)), 
                           paste(as.character(groups),"-1", sep=''))
    pb <- tkProgressBar("Progress: ", "Finished %", 0, 100)
    for(i in 2:(length(date))){
      RD_prev <- sort_var[date[i-1],]
      RD_prev <- xts(t(na.omit(t(as.matrix(RD_prev)))), order.by = date[i-1])
      stocks <- names(RD_prev)
      
      RD_prev_values <- as.numeric(RD_prev)  
      names(RD_prev_values) <- colnames(RD_prev)  # 保留股票列名
      sorted_RD <- sort(RD_prev_values)
      for(j in 1:groups){
        if(j==1){
          brk_point <- as.integer(length(stocks)/groups)
          tmp_rmNA <- xts(t(na.omit(t(as.matrix(Lret[date[i], names(sorted_RD)[1:brk_point]])))), order.by = date[i])
          tmp <- rowMeans(tmp_rmNA)
        }
        else if(j==groups){
          brk_point <- as.integer(length(stocks)/groups) * (groups-1)
          tmp_rmNA <- xts(t(na.omit(t(as.matrix(Lret[date[i], names(sorted_RD)[(brk_point+1):length(stocks)]])))), order.by = date[i])
          tmp <- rowMeans(tmp_rmNA)
        }
        else{
          brk_point1 <- as.integer(length(stocks)/groups) * (j-1)
          brk_point2 <- as.integer(length(stocks)/groups) * j
          tmp_rmNA <- xts(t(na.omit(t(as.matrix(Lret[date[i], names(sorted_RD)[(brk_point1+1):brk_point2]])))), order.by = date[i])
          tmp <- rowMeans(tmp_rmNA)
        }
        raw_return[date[i],j] <- tmp
      }
      raw_return[date[i],groups+1] <- raw_return[date[i],groups] - raw_return[date[i],1] 
      info <- sprintf("Finished %d%%", round(i*100/length(date)))
      setTkProgressBar(pb, i*100/length(date), sprintf("Progress (%s)", info), info)
    }
    close(pb)
    return(raw_return)
  }
  
  sorting_portfolio_new_EW <- function(sort_var, rets, groups){
    #
    # Table 1, single sort
    # compute sorting portfolio return, from 1 to 10 plus long/short portfolio
    # groups: number of groups
    #
    date <- index(sort_var)
    raw_return <- matrix(0, ncol=groups+1,nrow = (length(date)-1))
    raw_return <- xts(raw_return, order.by = date[2:length(date)])
    names(raw_return) <- c(as.character(c(1:groups)), 
                           paste(as.character(groups),"-1", sep=''))
    pb <- tkProgressBar("Progress: ", "Finished %", 0, 100)
    for(i in 2:(length(date))){
      RD_prev <- sort_var[date[i-1],]
      stk_exist <- rets[date[i],]
      stk_exist <- xts(t(na.omit(t(as.matrix(stk_exist)))), order.by = index(stk_exist))
      stk_exist <- colnames(stk_exist)
      RD_prev <- xts(t(na.omit(t(as.matrix(RD_prev[,stk_exist])))), order.by = date[i-1])
      stocks <- names(RD_prev)
      # modify
      RD_prev_values <- as.numeric(RD_prev)  
      names(RD_prev_values) <- colnames(RD_prev)  # 保留股票列名
      sorted_RD <- sort(RD_prev_values)
      for(j in 1:groups){
        if(j==1){
          brk_point <- as.integer(length(stocks)/groups)
          tmp_rmNA <- xts(t(na.omit(t(as.matrix(rets[date[i], names(sorted_RD)[1:brk_point]])))), order.by = date[i])
          tmp <- rowMeans(tmp_rmNA)
        }
        else if(j==groups){
          brk_point <- as.integer(length(stocks)/groups) * (groups-1)
          tmp_rmNA <- xts(t(na.omit(t(as.matrix(rets[date[i], names(sorted_RD)[(brk_point+1):length(stocks)]])))), order.by = date[i])
          tmp <- rowMeans(tmp_rmNA)
        }
        else{
          brk_point1 <- as.integer(length(stocks)/groups) * (j-1)
          brk_point2 <- as.integer(length(stocks)/groups) * j
          tmp_rmNA <- xts(t(na.omit(t(as.matrix(rets[date[i], names(sorted_RD)[(brk_point1+1):brk_point2]])))), order.by = date[i])
          tmp <- rowMeans(tmp_rmNA)
        }
        raw_return[date[i],j] <- tmp
      }
      raw_return[date[i],groups+1] <- raw_return[date[i],groups] - raw_return[date[i],1] 
      info <- sprintf("Finished %d%%", round(i*100/length(date)))
      setTkProgressBar(pb, i*100/length(date), sprintf("Progress (%s)", info), info)
    }
    close(pb)
    return(raw_return)
  }
  
  sorting_portfolio_new_VW <- function(sort_var, rets, MktCap, groups){
    #
    # Table 1, single sort
    # compute sorting portfolio return, from 1 to 10 plus long/short portfolio
    # groups: number of groups
    #
    date <- index(sort_var)
    raw_return <- matrix(0, ncol=groups+1,nrow = (length(date)-1))
    raw_return <- xts(raw_return, order.by = date[2:length(date)])
    names(raw_return) <- c(as.character(c(1:groups)), 
                           paste(as.character(groups),"-1", sep=''))
    pb <- tkProgressBar("Progress: ", "Finished %", 0, 100)
    for(i in 2:(length(date))){
      RD_prev <- sort_var[date[i-1],]
      stk_exist <- rets[date[i],]
      stk_exist <- xts(t(na.omit(t(as.matrix(stk_exist)))), order.by = index(stk_exist))
      stk_exist <- colnames(stk_exist)
      RD_prev <- xts(t(na.omit(t(as.matrix(RD_prev[,stk_exist])))), order.by = date[i-1])
      stocks <- names(RD_prev)
      # modify
      RD_prev_values <- as.numeric(RD_prev)  
      names(RD_prev_values) <- colnames(RD_prev)  # 保留股票列名
      sorted_RD <- sort(RD_prev_values)
      MC_prev <- MktCap[date[i-1],stocks]
      MC_prev <- na.fill(MC_prev, fill = 0)
      # MC_prev[is.na(MC_prev)] <- median(MC_prev, na.rm = TRUE)
      # MC_prev[is.na(MC_prev)] <- quantile(MC_prev, probs = 0.25, na.rm = TRUE)
      for(j in 1:groups){
        if(j==1){
          brk_point <- as.integer(length(stocks)/groups)
          tmp_rmNA <- xts(t(na.omit(t(as.matrix(rets[date[i], names(sorted_RD)[1:brk_point]])))), order.by = date[i])
          # tmp <- rowMeans(tmp_rmNA)
          tmp <- weighted.mean(as.matrix(tmp_rmNA), as.matrix(MC_prev[,names(tmp_rmNA)]))
        }
        else if(j==groups){
          brk_point <- as.integer(length(stocks)/groups) * (groups-1)
          tmp_rmNA <- xts(t(na.omit(t(as.matrix(rets[date[i], names(sorted_RD)[(brk_point+1):length(stocks)]])))), order.by = date[i])
          # tmp <- rowMeans(tmp_rmNA)
          tmp <- weighted.mean(as.matrix(tmp_rmNA), as.matrix(MC_prev[,names(tmp_rmNA)]))
        }
        else{
          brk_point1 <- as.integer(length(stocks)/groups) * (j-1)
          brk_point2 <- as.integer(length(stocks)/groups) * j
          tmp_rmNA <- xts(t(na.omit(t(as.matrix(rets[date[i], names(sorted_RD)[(brk_point1+1):brk_point2]])))), order.by = date[i])
          # tmp <- rowMeans(tmp_rmNA)
          tmp <- weighted.mean(as.matrix(tmp_rmNA), as.matrix(MC_prev[,names(tmp_rmNA)]))
        }
        raw_return[date[i],j] <- tmp
      }
      raw_return[date[i],groups+1] <- raw_return[date[i],groups] - raw_return[date[i],1] 
      info <- sprintf("Finished %d%%", round(i*100/length(date)))
      setTkProgressBar(pb, i*100/length(date), sprintf("Progress (%s)", info), info)
    }
    close(pb)
    return(raw_return)
  }
  
  
  screen_stocks <- function(idx, rets){
    ### Perform:
    ### 1st screen, only select the stocks that exist at the current time
    ### 2nd screen, remove stocks without any tech links to other companies
    ### 3rd screen, remove stocks with NA in the past 5-year returns data
    ### Output:
    ### Stocks:  screened list of available stocks
    ### cor_mat: tech link matrix of available stocks, as sorting variable
    ### rets_5y: 5-year returns dataframe of available stocks, for computing beta
    
    # 1st screen
    stk_exist <- rets[idx,]
    stk_exist <- xts(t(na.omit(t(as.matrix(stk_exist)))), order.by = index(stk_exist))
    stk_exist <- colnames(stk_exist) 
    # read tech links data
    year  <- as.integer(format(idx,"%Y")) 
    month <- as.integer(format(idx,"%m")) 
    fileName <- paste(paste(year,month,sep = '_'), "csv", sep='.')
    fileName <- paste("sim",fileName,sep='/')
    cor_mat  <- read.csv(fileName, header = TRUE) # read tech link data
    stk_tech <- as.character(cor_mat[,1])
    cor_mat  <- cor_mat[,-1]
    colnames(cor_mat) <- stk_tech
    rownames(cor_mat) <- stk_tech
    cor_mat[cor_mat>=0.99]  <- 0 # reassign the value on diagonal
    cor_mat[is.na(cor_mat)] <- 0 # fill NA entry with 0
    # 2nd screen
    cor_mean <- rowMeans(cor_mat)
    cor_mean[cor_mean==0] <- NA
    cor_mean <- na.omit(cor_mean)
    stk_tech <- names(cor_mean)
    stocks  <- intersect(stk_exist, stk_tech)
    # 3rd screen
    rets_5y <- rets[paste(as.character(year-5),as.character(year-1),sep = '/'), stocks]
    rets_5y <- xts(t(na.omit(t(as.matrix(rets_5y)))), order.by = index(rets_5y))
    stocks  <- colnames(rets_5y)
    cor_mat <- cor_mat[stocks,stocks]
    if(length(stocks)<100){
      print(paste(fileName,"less than 100 available stocks!",sep=': '))
    }
    return(list(stocks, cor_mat, rets_5y))
  }
  
  
}