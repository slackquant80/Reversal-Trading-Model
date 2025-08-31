library(quantmod)
library(PerformanceAnalytics)
library(zoo)
library(TTR)
library(writexl)

# 1) 데이터
getSymbols("QQQ", from = "2000-01-01")
x <- QQQ
H <- Hi(x); L <- Lo(x); C <- Cl(x); Adj <- Ad(x)

# 저장
x_df <- data.frame(Date = index(QQQ), coredata(QQQ))
write_xlsx(x_df, "raw.xlsx")

# 2) 지표 계산
HL      <- H - L
rollHL  <- rollapplyr(HL, 25, mean,  fill = NA, align = "right") 
rangeHL <- ifelse(HL == 0, NA, HL)
IBS     <- (C - L) / rangeHL
rollH10 <- rollapplyr(H, 10, max, fill = NA, align = "right")
lower   <- rollH10 - 2.5 * rollHL

# 3) 시그널
entry_signal <- ifelse((C < lower) & (IBS < 0.3), 1L, 0L)
entry_signal[is.na(entry_signal)] <- 0L

exit_signal  <- ifelse(C > lag(H, 1, na.pad = TRUE), -1L, 0L)
exit_signal[is.na(exit_signal)] <- 0L

# 4) 포지션
position <- xts(rep(0L, NROW(C)), order.by = index(C))
for (i in 2:NROW(C)) {
  if (as.numeric(position[i-1]) == 0L) {
    if (entry_signal[i] == 1L) position[i] <- 1L else position[i] <- 0L
  } else {
    if (exit_signal[i] == -1L) position[i] <- 0L else position[i] <- 1L
  }
}

# 5) 수익률
ret <- dailyReturn(Adj)
strategy_ret <- ret * lag(position, 1)
strategy_ret[is.na(strategy_ret)] <- 0

# 6) 성과 비교
charts.PerformanceSummary(
  cbind(`Strategy` = strategy_ret, `Buy&Hold` = ret),
  legend.loc = "topleft",
  main = "QQQ: Lower-band + IBS Strategy"
)

# 7) 성과 지표 요약
perf <- cbind(`Strategy` = strategy_ret, `Buy&Hold` = ret)

cat("\n==== Annualized Returns / Volatility / Sharpe ====\n")
print(table.AnnualizedReturns(perf, Rf = 0))

cat("\n==== Maximum Drawdown ====\n")
print(maxDrawdown(perf))

cat("\n==== Top 5 Drawdowns (Strategy) ====\n")
print(table.Drawdowns(strategy_ret, top = 5))