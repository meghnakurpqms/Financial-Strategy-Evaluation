
### Loading Packages
  
library(quantmod)
library(tidyquant)
library(tidyverse)
library(plyr)



## Strategy 1

### Load SPY data for 28 years


df <- c("SPY") %>% tq_get(get = "stock.prices",from = "1989-01-01",to = "2021-01-01")
SPY=df



### Mutate, that is create new variables(year, month, day) from the data.


df=df%>%
  dplyr::mutate(year = lubridate::year(date), 
                month = lubridate::month(date), 
                day = lubridate::day(date))
df


### Get first day of month for entire data


first_day_of_month= ddply(df, .(year, month), summarize, first=min(date)) %>% select(first)
first_day_of_month=first_day_of_month[,'first']
first_day_of_month


### Get monthly purchase


monhly_df=df[which(df$date %in% first_day_of_month),] 
monhly_df=monhly_df[3:dim(monhly_df)[1],]

monhly_df=monhly_df %>% mutate(stock_purchased = 1000/adjusted)
monhly_df


### Calculate stocks by the end of the month


stocks_by_end_of_month <- c()
stocks=0

for(i in 1:dim(monhly_df)[1]) {
  stocks=stocks+monhly_df[[i,'stock_purchased']]
  stocks_by_end_of_month=c(stocks_by_end_of_month,stocks)
}
stocks_by_end_of_month


### Stock money after every year


monhly_df=monhly_df %>% add_column(stocks_by_end_of_month = stocks_by_end_of_month)
year_end_stock=ddply(monhly_df, .(year), summarize, year_end_stocks=max(stocks_by_end_of_month)) %>% select(year_end_stocks)

year_end_stocks=monhly_df %>% group_by(year) %>%
  arrange(date) %>%
  filter(row_number()==n()) %>% select(adjusted) %>% add_column(year_end_stock = year_end_stock %>% pull)  %>%
  mutate(strategy1=adjusted *year_end_stock ) %>% select(strategy1)

year_end_stocks


### Net money after first investment


net_money_after_first_investment=monhly_df[dim(monhly_df)[1],'adjusted']*sum(monhly_df%>%select(stock_purchased))
net_money_after_first_investment


## Strategy 2

### Get data

invest2_df=df %>% add_column( SMA = SMA(df %>% select(adjusted)) ) 

getSymbols('SPY')


### Plot data for visual analysis


chartSeries(SPY)


### Add simple moving average to your plot for analysis

chartSeries(SPY)
addSMA()


### Get monthly stocks


first_day_of_month= ddply(invest2_df, .(year, month), summarize, first=min(date)) %>% select(first)
first_day_of_month=first_day_of_month[,1]
monhly_df2=invest2_df[which(invest2_df$date %in% first_day_of_month),]

monhly_df2=monhly_df2[3:dim(monhly_df2)[1],]


### Calculate stock value by the end of the month


saving=0
stocks=0
stocks_by_end_of_month <- c()
saving_by_end_of_month <- c()
for (i in 1:dim(monhly_df2)[1]){
  saving=saving+1000
  if (monhly_df2[i,'adjusted'] > monhly_df2[i,'SMA']){
    stocks=stocks + saving/(monhly_df2[[i,'adjusted']])
    saving=0
    
  }
  else{
    saving=saving + (monhly_df2[[i,'adjusted']])*stocks
    stocks=0
  }
  stocks_by_end_of_month=c(stocks_by_end_of_month,stocks)
  saving_by_end_of_month <- c(saving_by_end_of_month,saving)
  
}
saving_by_end_of_month


### Net amount after second investment

net_money_after_second_investment=saving+monhly_df2[dim(monhly_df)[1],'adjusted']*stocks
net_money_after_second_investment


### Year end stock value


monhly_df2=monhly_df2 %>% add_column(stocks_by_end_of_year = stocks_by_end_of_month, saving_by_end_of_year = saving_by_end_of_month)
year_end_strategy2=monhly_df2 %>% group_by(year) %>%
  arrange(date) %>%
  filter(row_number()==n()) %>% select(stocks_by_end_of_year,saving_by_end_of_year,adjusted) %>% mutate(strategy2=(stocks_by_end_of_year*adjusted)+saving_by_end_of_year)

year_end_strategy2


### A table indicating how much money you would  have at the end of each year by following strategy 1 and strategy 2.


yearly_investment=c(10000,12000,12000,12000,12000,12000,12000,12000,12000,12000,12000,12000,12000,12000,12000,12000,12000,12000,12000,12000,12000,12000,12000,12000,12000,12000,12000,4000)

table=year_end_strategy2 %>% add_column(strategy1=year_end_stocks%>% pull('strategy1')) %>% select(strategy1,strategy2) %>% add_column(yearly_investment=yearly_investment)
table

### Calculate the returns on the strategies


return_strategy1=c()
return_strategy2=c()
for (i in 1:dim(table)[1]){
  if (i==1){
    return_strategy1=c(return_strategy1,table[i,'strategy1'] %>% pull())
    return_strategy2=c(return_strategy2,table[i,'strategy2'] %>% pull())
  }
  else{
    x=table[i,'strategy1'] %>% pull() - table[i-1,'strategy1'] %>% pull()
    y=table[i,'strategy2'] %>% pull() - table[i-1,'strategy2'] %>% pull()
    return_strategy1=c(return_strategy1,x)
    return_strategy2=c(return_strategy2,y)
    
  }
}


### Profit on strategies


return_strategy1

return_strategy2


### Create Return Strategy table for each year based on yearly investment


fig <- data.frame(table%>% pull('year'),return_strategy1/yearly_investment,return_strategy2/yearly_investment)
colnames(fig)=c('year','return_strategy1','return_strategy2')

fig


### Plotting the returns for both strategies


ggplot(fig, aes(year)) +geom_line(aes(y=return_strategy1,color="red" )) +geom_line(aes(y=return_strategy2, color = "green"))+ scale_color_discrete(name = "Strategies", labels = c("Strategy 1", "Strategy 2"), breaks=c("red","green"),guide="legend")+labs(x = "Year",
                                                                                                                                                                                                                                                             y = "Returns")+ggtitle("Returns for both strategies over 28 years")

### Plot for the strategy comparison based on percentage of return


investment=dim(monhly_df2)[1] *1000
a=((net_money_after_first_investment-investment)*100/investment)[1,1]
b=((net_money_after_second_investment-investment)*100/investment)[1,1]
barplot(c(a,b),main='Return on Investment for a period of 28 years',xlab='Investment Strategies',ylab="Sum total of return",col=c('black','red'),names=c('strategy1','strategy2'))


### Net amount after strategy 1


print(paste('Money accumulated by strategy1 is', net_money_after_first_investment))


### Net amount after strategy 2


print(paste('Money accumulated by strategy2 is', net_money_after_second_investment))



#### Strategy 1 gives a better return than Strategy 2
