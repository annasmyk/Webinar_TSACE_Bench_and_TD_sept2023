library(readxl)
library(rjd3toolkit)
library(rjd3bench)
library(rjd3sts)
library(nbbTD)

# Data (extract production side Belgian QNA (A38))
benchmarks<-as.data.frame(read_excel("input prod.xlsx", sheet = "benchmarks"))
benchmarks_ts<-ts(benchmarks[,-1], start=2009, frequency=1)
indicators<-as.data.frame(read_excel("input prod.xlsx", sheet = "indicators"))
indicators_ts<-ts(indicators[,-1], start=c(2009,1), frequency=4)

# 1. rjd3bench
## note: possibility to fix some periods already developed but not yet implemented (coming soon...)
?denton_modelbased
rslt1<- rjd3bench::denton_modelbased(benchmarks_ts[,"FF"],
                                     indicators_ts[,"FF_VAT"],
                                     conversion = "Sum",
                                     outliers = list("2020-04-01"=100))
plot(rslt1)


# 2. rjd3sts

## Prepare data
freq<-4
benchmark<-benchmarks_ts[,"FF"]
indicator<-indicators_ts[,"FF_VAT"]
yc<-matrix(nrow=freq*length(benchmark), ncol=1)
yc[freq*(1:length(benchmark)),1]<-benchmark
xt<-ts(indicator, frequency = freq, start = start(benchmark), end = c(end(benchmark)[1], freq))
stderr_beta <- rep(1,length(indicator))
stderr_beta[45] <- 10
# note that yc could be filled further (fix values at some period)...

## Modelling
### Define component
regressor<-rjd3sts::var_reg("x", xt, stderr_beta, scale=1, fixed=T)

### Introduce cumulator
cumulator<-rjd3sts::cumul("c", regressor, freq)

### Define the model
model_mbdenton<-rjd3sts::model()
rjd3sts::add(model_mbdenton, cumulator)

### Build equations
equation<-rjd3sts::equation("eq")
rjd3sts::add_equation(equation, "c")
rjd3sts::add(model_mbdenton, equation)

### Estimate the model
rslt<-rjd3sts::estimate(model_mbdenton, yc, marginal = T, initialization = "Augmented_Robust")

### Some result
beta_t<-rjd3toolkit::result(rslt, "ssf.smoothing.states")[,2]
beta_t_out<-c(beta_t,rep(beta_t[length(beta_t)], 6))
beta_t_out_ts<-ts(beta_t_out, frequency = 4, start=c(2009,1))
disag<-beta_t_out_ts*indicator
plot(disag)


# 3. nbbTD
?multiTD_fromXLSX
?multiTD
rslt <- multiTD_fromXLSX(path_data = "input prod.xlsx", 
                         forecastBI = "userdefined+auto",
                         conversion = "Sum",
                         path_output = "rslt1.xlsx")

# rslt$FF$disag
# rslt$CH$parameters
# rslt$td_series
rslt$bi_annual

runShiny(rslt)



