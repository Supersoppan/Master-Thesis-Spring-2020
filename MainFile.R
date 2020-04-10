packages = c("readxl", "tidyverse", "tseries", "plm", "dynlm", "ggplot2")
lapply(packages, library, character.only = TRUE) 


#To do list
"
1. merge lists and make them pretty
"
#
#loading and cleaning----
#loading gasoline
fuel_price = read_excel("fuel price yearly.xlsx", skip = 1)
fuel_price$DateTime = as.Date(fuel_price$DateTime, format = "%Y-%m-%d")
fuel_price$BF95 = as.numeric(fuel_price$BF95) #can create nice for loop here

#loading CPI
CPI = read_excel("SWECPIALLMINMEI.xlsx", skip = 10)
CPI = rename(CPI, "DateTime" = observation_date)
CPI$observation_date = as.Date(CPI$DateTime, format = "%Y-%m-%d")

#creating real prices
temp = (fuel_price$BF95/CPI[21:(nrow(CPI)-1),2])*100 #divides the price with the consumer price index and multiplies by 100. base year is 2015  #can use left_join for better joining abilities
fuel_price = data.frame(fuel_price$DateTime, temp)
gas_price = rename(fuel_price, "Real_Price" = SWECPIALLMINMEI, "Date" = fuel_price.DateTime)
remove(temp)
#replaces the date (containing month and year) in with only the year
gas_price$Date = factor(format(gas_price$Date, format = "%Y"))   
gas_price = rename(gas_price, "year" = Date)



#loading and pivoting quantity of gas per municipality
q_gas_mun = read_excel("quantity gas municipality.xlsx", skip = 2)
q_gas_mun = rename(q_gas_mun, "municipality" = ...1, "fuel type" = ...2)
q_gas_mun = q_gas_mun[1:291,]
#removing knivsta and Heby

q_gas_mun = q_gas_mun[-c(29,30, 214),]
q_gas_mun = pivot_longer(q_gas_mun, 
      c(`2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`),
      names_to ="year", values_to = "q")

#loading and pivoting population per municipality
pop =  read_excel("be0101_folkmangdkom_1950-2019.xlsx", skip = 5)
pop = pop[1:290,] #remove comments 
pop = pop[, c("Kommun", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")] #only keeps the relevant years, 2001 until 2017
pop = pop[-c(68, 103),] #removing knivsta and Heby
pop = pivot_longer(pop, 
        c(`2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`),
        names_to ="year", values_to = "population")
pop = rename(pop, "Municipality_name" = Kommun)
q_gas_mun = separate(q_gas_mun, municipality, c("Municipality_code", "Municipality_name"), sep = " ", extra = "merge")

#loading information about municipal attributes
mun_attributes = read_excel("oversiktstabell-och-lista-kommungruppsindelnin.xlsx", sheet = "Lista Kommungruppsind 2017")

mun_attributes = rename(mun_attributes, "Municipality_code" = Kommunkod, "SKR_name" = "Kommungrupp 2017 namn")
mun_attributes = select(mun_attributes, "Municipality_code", "SKR_name")
q_gas_mun = merge(q_gas_mun, mun_attributes, by = "Municipality_code")


#merging the data frames
together = merge(q_gas_mun, gas_price, by = "year")
together = merge(together, pop, by = c("Municipality_name", "year"))
together$gaspercapita = (as.numeric(together$q)/together$population)*1000000 #divided quantity used by population and multiplied by 1 million. 1000*1000 because 1 qubic meter = 1000L and the data is measured as 1000m3 for each municipality
together_pseries = pdata.frame(together, index = c("Municipality_name", "year"),  drop.index=FALSE, row.names=TRUE)  
together_pseries$q = as.numeric(together_pseries$q)

#Testing----

# NOT RUN {
data(Cigar)
library("panelvar")
ex1_feols <-
  pvarfeols(dependent_vars = c("sales", "price"),
            lags = 1,
            exog_vars = c("cpi"),
            transformation = "demean",
            data = Cigar,
            panel_identifier= c("state", "year"))
summary(ex1_feols)         
# }

p <- together %>%
  ggplot( aes(x=gaspercapita, fill=SKR_name)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  labs(fill="") +
  theme(legend.position = "top")
  xlim(0,1500)
p




#tests if the panel data has unit root. Difference in results based on pmax. pmax = 4(no rejection, Unit root present), pmax = 5(Reject, No unit root) 
data.frame(split(together$gaspercapita, together$"Municipality_name")) %>% 
  purtest( pmax = 5, exo = "intercept", test = "hadri")
  # list of tests: 
  'c("levinlin", "ips", "madwu", "Pm", "invnormal", "logit", "hadri")'

#tests if gas price has unit root
adf.test(gas_price$Real_Price) #ADF test shows that we reject the null. alternative hypothesis is stationarity
adf.test(gas_price[21:nrow(gas_price),2]) #ADF shows that the sample we´re looking at is not stationary
plot(gas_price[21:nrow(gas_price),2])
  
ctapply(together$gaspercapita, together$SKR_name, summary)

print(together_pseries)




hist(together$gaspercapita, breaks = 2000, xlim = c(0,900))#as can be seen from the histogram and summary, there appears to be a few outliers, namely those who do not consume any fuel
summary(together$gaspercapita)



asd = plm(formula = diff(gaspercapita) ~ lag(log(Real_Price),1) + lag(Real_Price:SKR_name,1), together_pseries, model = "between", effect = "individual")
summary(asd)

plot(together_pseries, N = 5)


sad = plm(formula = diff(log(q)) ~ diff(lag(log(q))) + (diff(log(Real_Price),1)) + (diff(log(Real_Price), 1))*SKR_name , together_pseries, model = "within", effect = "twoways")
summary(sad)


#Trash----
#play around with the range to get all kinds of different plots for each municipality
for (i in 20:30){
  j = i*16
  plot(x = together[(1+j):(17+j), 2], y = together[(1+j):(17+j), 7])
}

for (i in 1:2){
  j = i*17
  plot(x = together$year[j:(j+16)], y = together$q[j:(j+16)])
  print(together$Municipality_name[j])
  print(together$Municipality_name[j+17])
}



for (i in 1:1){
  print(together$Municipality_name[i])
  print(together$Municipality_name[i+16])
  print(i)
}t

print(together$Municipality_name[18])




#cointegration panel
library("pco")
data(gdi)
data(gds)
hej = pedroni99(gdi, gds)

#cointegration panel
data(Grunfeld)
Grunfeld$inv[c(2,8)] <- NA
GrunfeldNew <- subset(Grunfeld, !year %in% year[is.na(inv)])
x <- data.frame(split(GrunfeldNew$inv, GrunfeldNew$firm))
hej = purtest(x, pmax = 4, exo = "none", test = "levinlin",lags="SIC")