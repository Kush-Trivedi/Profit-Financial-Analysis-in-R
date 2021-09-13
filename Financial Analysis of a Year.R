#Data
revenue <- c(14574.49, 7606.46, 8611.41, 9175.41, 8058.65, 8105.44, 11496.28, 9766.09, 10305.32, 14379.96, 10713.97, 15433.50)
expenses <- c(12051.82, 5695.07, 12319.20, 12089.72, 8658.57, 840.20, 3285.73, 5821.12, 6976.93, 16618.61, 10054.37, 3803.96)

#Solution


#Profit for each month
profit <- revenue - expenses
mProfit<-paste("$",round(profit/1000))
mProfit
#Profit after tax each month at 30%
tax <- profit * 0.3
round(tax, digits = 0)

profitAfterTax <- profit - tax
mProfitAfterTax <- paste("$",round(profitAfterTax / 1000))
mProfitAfterTax

#Profit margin of each month
profitMarginForEachMonth <- profitAfterTax / revenue
percent <- function(profitMarginForEachMonth, digits = 0, format="f"){
  paste0(formatC(profitMarginForEachMonth * 100, format = format,digits = digits),"%")
}
mProfitMargin <- percent(profitMarginForEachMonth)
mProfitMargin

#Mean for Year and their Good Months
meanProfitAfterTax <- mean(profitAfterTax)
mMeanProfitAfterTax <- paste("$",round(meanProfitAfterTax, digits = 0))
mMeanProfitAfterTax

goodMonths <- profitAfterTax > meanProfitAfterTax
mGoodMonths <- goodMonths
mGoodMonthsAsInt <- as.integer(mGoodMonths)

#Mean for Year and their Good Months
badMonths <- !goodMonths
mBadMonths <- badMonths
mBadMonthsAsInt <- as.integer(mBadMonths)

#Best Month
bestMonthOfProfitInDollor <- max(profitAfterTax)
mBestMonthOfProfitDollor <- paste("$",round(bestMonthOfProfitInDollor, digits = 0))
mBestMonthOfProfitDollor

mBestParticularMonthOfProfit <- profitAfterTax == max(profitAfterTax)
mBestParticularMonthOfProfitAsInt <- as.integer(mBestParticularMonthOfProfit)

#Worst Month
worstMonthOfProfit <- min(profitAfterTax)
mWorstMonthOfProfit <- paste("$",round(worstMonthOfProfit, digits = 0))
mWorstMonthOfProfit

mWorstParticularMonthOfProfit <- profitAfterTax == min(profitAfterTax)
mWorstParticularMonthOfProfitAsInt <- as.integer(mWorstParticularMonthOfProfit)

#Units in Thousands for all Data
mRevenueInThousands <- round (revenue / 1000)
mExpensesInThousands <- round(expenses / 1000)


#Matrices of All Data

matrix <- rbind(
  mRevenueInThousands,
  mExpensesInThousands,
  mProfit,
  mProfitAfterTax,
  mProfitMargin,
  mGoodMonthsAsInt,
  mBadMonthsAsInt,
  mBestParticularMonthOfProfitAsInt,
  mWorstParticularMonthOfProfitAsInt
)

matrix
