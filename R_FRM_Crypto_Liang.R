## R_FRM_America_whole_test_with abbreviation

## 0. Preparation

rm(list = ls(all = TRUE))
graphics.off()

setwd("/Users/LvB/Documents/FRM_code/FRM_Crypto")

#install.packages("stringr")
library(stringr)

source("R_FRM_Statistics_Algorithm.R")

## 1. Data Preprocess

# Load the market capitalization data matrix
Mktcap = read.csv("FRM_Crypto_Market_20200323_2017.csv",header = TRUE)
Mktcap = Mktcap[2 : nrow(Mktcap), 2 : ncol(Mktcap)]
Mktcap[is.na(Mktcap)] = 0

# Load the stock prices and macro-prudential data matrix
Stock_Prices = read.csv("FRM_Crypto_Prices_20200323_2017.csv", header = TRUE)
Macro = read.csv("FRM_Crypto_Macro_20200323_2017.csv", header = TRUE)

#All_prices include stock and Macro
All_prices= merge(Stock_Prices, Macro, by = "date", all.x = TRUE)

# Calculate the daily return and differences matrix of all selected financial companies and macro-prudential variables; use exponential function for selected macro-prudential variables that are expressed in first order differences
All_prices$BV010082.INDEX = exp(All_prices$BV010082.INDEX)
All_return= diff(log(cbind(as.numeric(gsub("-", "", All_prices$date)), as.matrix(All_prices[, (2 : ncol(All_prices))]))))
All_return[, 1] = as.numeric(gsub("-", "", All_prices$date[2 : nrow(All_prices)]))
All_return[is.na(All_return)] = 0

# Sorting the Market Capitalization Data
FRM_Sort = function(Data){Data[is.na(Data)] = 0; sort(Data, decreasing = TRUE, index.return = TRUE)}

# Determininig the index number of each company according to decreasing market capitalization
Mktcap_Index = matrix(0, nrow(Mktcap), ncol(Mktcap))
# Determininig the market capitalization value of each company according to decreasing market capitalization
Mktcap_Value = matrix(0, nrow(Mktcap), ncol(Mktcap))
Time_Start = Sys.time()
for (t in seq(1, nrow(Mktcap), 1)){
  Mktcap_Index[t, ] = t(apply(Mktcap, 1, FRM_Sort))[1, t][[1]] $ix
  Mktcap_Value[t, ] = data.matrix(t(apply(Mktcap, 1, FRM_Sort))[1, t][[1]]$x)
}
Mktcap_Index = cbind(All_prices[, 1], Mktcap_Index)
Mktcap_Value = cbind(All_prices[, 1], Mktcap_Value)
Time_End = Sys.time()

## 2. Save Results
#write.table(All_return, "FRM_C_Returns_20200323_2017.csv", row.names = F, col.names = T)
#write.table(Mktcap_Index, "FRM_C_Mktcap_Index_20200323_2017.csv", row.names = F, col.names = T)
#write.table(Mktcap_Value, "FRM_C_Mktcap_Value_20200323_2017.csv", row.names = F, col.names = T)
Time_End - Time_Start



## 3. Data

All_return=read.table( "FRM_C_Returns_20200323_2017.csv", header = TRUE)
Mktcap_Index= read.table("FRM_C_Mktcap_Index_20200323_2017.csv",header = TRUE)
Mktcap_Value=read.table("FRM_C_Mktcap_Value_20200323_2017.csv", header = TRUE)

All_return[is.na(All_return)] = 0
# Stock_Returns column=the numbers of Americas_Market_Capitalizations_Index
All_prices = All_return[, 1 : ncol(Mktcap_Index)]
#Macrovariables #Column 405-410
Macro_return = All_return[, (ncol(Mktcap_Index) + 1) : ncol(All_return)] 


## 4. Estimation

J = 15       # Number of largest financial companies
s = 63        # Estimation Window Size, s = 63
tau = 0.05    # Tail Risk Level, tau = 0.05
I = 25         # Number of Iterations, I = 20

Date_Start = 20200101
Date_End = 20200320

# calculate how many days for FRM series 20191101-20191111 7 days; r=7
r = sum(((All_return[, 1] >= Date_Start) & (All_return[, 1] <= Date_End)) * matrix(1, nrow(All_return), 1))
#calculte the total days in the document  20190603-20191111 113 days; Index_End =113
Index_End = max(((All_return[, 1] >= Date_Start) & (All_return[, 1] <= Date_End)) * matrix(1 : nrow(All_return), nrow(All_return), 1))
#matrix(0,7,(410-1)*(410-1+2)+2)
FRM_Americas_Statistics_Estimation_Matrix = matrix(0, r, ((ncol(All_return) - 1) * (ncol(All_return) - 1 + 2) + 2))
dim(FRM_Americas_Statistics_Estimation_Matrix)
#All_return[107:113,1] # the last column is date
FRM_Americas_Statistics_Estimation_Matrix[, dim(FRM_Americas_Statistics_Estimation_Matrix)[2]] = All_return[(Index_End - r + 1) : (Index_End), 1]

FRM_matix=matrix(0,J,(J+6))
FRM_series=matrix(0,r,(J+1))
FRM_series[,1]= All_return[(Index_End - r + 1) : (Index_End), 1]
FRM_series_final=matrix(0,r,2)
FRM_series_final[,1]=All_return[(Index_End - r + 1) : (Index_End), 1]

##For same companies
Data0 = cbind(All_return[(Index_End - r + 1 - s + 1) : (Index_End - r + 1), (as.matrix(Mktcap_Index[Index_End - r + 1, 2 : ncol(Mktcap_Index)][1 : J] + 1))], Macro_return[(Index_End - r + 1 - s + 1) : (Index_End - r + 1), ])


for (t in ((Index_End - r + 1) : (Index_End))){ #113-7+1=107:113 eg t=113
  for (j in 1 : J){ #J=1
    #the first 100 largest companies 
    #R_RETURNS=All_return[51:113,(as.matrix(Mktcap_Index[113,2:404][1:100]+1))],Macro_return[51:113,]
    #R_matrix=as.matrix(Mktcap_Index[113,2:404][1:100]+1)
    
    ##For same companies
    Data = cbind(All_return[(t - s + 1) : t, colnames(Data0)[1:J]], Macro_return[(t - s + 1) : t, ])
    ##For different companies
#    Data = cbind(All_return[(t - s + 1) : t, (as.matrix(Mktcap_Index[t, 2 : ncol(Mktcap_Index)][1 : J] + 1))], Macro_return[(t - s + 1) : t, ])
    
    #Mktcap_Index[113last day, 1 : 101], t(as.matrix(c(404:(410-1))))
    Data_Market_Index = cbind(Mktcap_Index[t, 1 : (J + 1)], t(as.matrix(c(ncol(Mktcap_Index) : (ncol(All_return) - 1)))) %x% matrix(1, 1, 1))
    #FRM_Quantile_Regression
    Est = FRM_Quantile_Regression(as.matrix(Data), j, tau, I)
    Est_hat = t(as.matrix(Est$beta[which(Est$Cgacv == min(Est$Cgacv)), ]))
    Est_lambda_hat = abs(data.matrix(Est$lambda[which(Est$Cgacv == min(Est$Cgacv))]))
    Est_FRM_Condition_hat = Est$FRM_Condition
    Vector_Data_Market_Index = Data_Market_Index[, c(-1, -(j + 1))] #Data_Market_Index[, c(-1, -101))] -1，-101 delete the column 105 columns
    FRM_series[r-(Index_End-t),j+1]=Est_lambda_hat 
    }                                                                              
}

FRM_series_final[,2]=rowSums(FRM_series[,c(2:(J+1))])/J

## 5. Save Results
dt=FRM_series_final[,1]
dFRM=round(FRM_series_final[,2],9)
dt_format=as.Date(as.character(FRM_series_final[,1]),  "%Y%m%d")
dt_format = as.Date(dt_format, format = "%Y-%m-%d")
dt_save=as.character(dt_format)
Final_FRM = cbind(dt_save, dFRM)
write.csv(Final_FRM, paste0("FRM_index_", Date_Start, "_", Date_End, ".csv"),row.names = FALSE,  quote = FALSE) 



## 6. percentage 
dFRM_all=read.csv("lambda_mean_AME.csv")[,c(2)]
dFRM_time=read.csv("lambda_mean_AME.csv")[,c(1)]
dFRM_all[length(dFRM_all)] #the number
ecdf(dFRM_all)(dFRM_all[length(dFRM_all)])
#x <- rnorm(1000, mean=0, sd=1)
#quantile(x,0.5)
#ecdf(x)(0)



