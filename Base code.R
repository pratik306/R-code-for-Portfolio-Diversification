#Loading required libraries
library("matlib")
library(tseries)
library(ggplot2)
library("quadprog")

setwd("D:/Rmetrics Project/New code with functions")

#=============USER DATA COLLECTION=====================================#
###"MSFT","WFC","AAPL","DIS","COP","XOM","GOOG","BIDU","TSLA","TTM"
tickers = c("MSFT","WFC","AAPL","DIS","COP","XOM","GOOG","BIDU","TSLA","TTM")
Dt_start <- "2010-06-30"
Dt_end <- "2016-06-30"
type <- "d"
risk_free_rate <- 0
User_expectret <- as.numeric(readline(prompt = "Enter expected return with short sales: "))
Total_Invest_val <- as.numeric(readline(prompt = "Enter total investment value: "))
User_expectret_Noshort <- as.numeric(readline(prompt = "Enter expected return with no short sales: "))


#=============USER DATA COLLECTION FINISHED============================#


#============Initialising Various Functions============================#

source("./Extract_Data.R")
source("./Return_Data.R")
source("./log_Ret_Data.R")
source("./mean_ret.R")
source("./covariance.R")
source("./Global_Minimum_Variance.R")
source("./Efficient_frontier.R")
source("./Tangency_Portfolio.R")
source("./User_Tan_profile.R")
source("./Value_At_Risk.R")
source("./No_Short_User.R")
source("./No_Short_Global_minimum.R")
source("./Optim_Risk_Noshort.R")
#==============DONE=======================================#

#===============CREATING RAW DATA=========================#

myrawdata <- Extract_Data(tickers, Dt_start, Dt_end,type)

len_tick <- length(tickers)

Dates = c("Dates")

print("Data collection successful")

#===================================DONE======================================#


#============================CREATING SIMPLE RETURN MATRIX============================#

Ret_matrix <- Return_data(myrawdata)

print("Calculating Return data Successful")

#===================================DONE======================================#

#===========================CREATING LOGARITHIMIC RETURNS MATRIX======================================#

log_Ret_matrix <- log_Ret_Data(myrawdata)

print("LOg return data creation successful")


#===================================DONE======================================#


#=========================Creation of Mean vector=======================================#

mu <- mean_ret(Ret_matrix)

print("Mean vector successfully created")

#===================================DONE======================================#


#====================COVARIANCE MATRIX======================================#

cov_matrix <- covariance(Ret_matrix, mu)

print("making covariance matrix successful")

#======================GLOBAL MINIMUM VARIANCE======================================#


Output_Global_Min <- Global_Minimum_Variance(mu, cov_matrix, tickers)
glob_min_SD <- Output_Global_Min[2,1]
Expectedreturn_Glob_Min <- Output_Global_Min[1,1]

print("Global Minimum variance Successfully created")

#===================================DONE======================================#


#==========================EFFICIENT FRONTIER======================================#

iterations_top <- 100
iterations_below <- 1

efficient_frontier <- Efficient_frontier(Output_Global_Min, mu, cov_matrix,iterations_top,iterations_below)
correspondingSD <- efficient_frontier[,1]
mu_po <- efficient_frontier[,2]

print("Calculation of efficient frontier successful")

#===================================DONE======================================#

#===================Generating points for each individual stock================#
SD_eachstock <- matrix(nrow = len_tick, ncol = 1)
SD_eachstock <- sqrt(diag(cov_matrix))

#===================================DONE======================================#

#if needed unhash this 
#Data_forplot <- as.data.frame(cbind(correspondingSD,mu_po))

#===================================DONE======================================#

#===========================Calculating tangency portfolio============================#
Tangent_portfolio <- Tangency_Portfolio(risk_free_rate,cov_matrix,mu,efficient_frontier)
tan_matSD <- Tangent_portfolio$TanSD
tan_matret <- Tangent_portfolio$TanRet
tan_mat <- Tangent_portfolio$TanMat


##computing a point on the tangency portfolio

UserPoint <- User_Tan_profile(User_expectret,tickers,tan_matSD,tan_matret,risk_free_rate)
User_profile <- UserPoint$UserProfile
User_expecttan_contri <- UserPoint$TanContri
print("UserProfile Calculations Successful")

#===================================DONE======================================#


#=======================Calculating Value at Risk for a 5% Probability==============#


Val_at_Risk <- Value_At_Risk(Total_Invest_val,User_expectret,User_expecttan_contri,tan_matSD)


#===================================DONE======================================#

#=====================User profile with No short sale restrictions=============#

User_Noshort_Profile <- No_Short_User(User_expectret_Noshort,mu,cov_matrix)
User_Noshort_risk <- User_Noshort_Profile$Risk
User_noshort_weights <- User_Noshort_Profile$weights

print("Finding user profile for No short is successful")

#===================================DONE======================================#



#=======================global Minimum profile with no short======================#####

##No short sale restrictions

global_minimum_noshort <- No_Short_Global_minimum(cov_matrix,mu)
glob_min_noshort_Risk <- global_minimum_noshort$Risk
glob_min_noshort_ret <- global_minimum_noshort$Return
glob_min_noshort_weights <- global_minimum_noshort$Weights


#===================================DONE======================================#


#=========================Optimum risky portfolio for no short conditions====#
maxret_stock <- max(mu)
iterations <- 1000
dataoptim <- Optim_Risk_NoShort(iterations,glob_min_noshort_ret,maxret_stock,mu,cov_matrix,risk_free_rate)
Optimum_return <- dataoptim$Optimret
Optimum_Risk <- dataoptim$OptimStd
Optim_weight <- dataoptim$Optimweight
plot(dataoptim$Riskdata,dataoptim$Returndata)
