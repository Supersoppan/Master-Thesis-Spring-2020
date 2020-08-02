packages = c("readxl", "tidyverse", "tseries", "plm", "dynlm", "ggplot2", "urca", "punitroots")
lapply(packages, library, character.only = TRUE) 

#loading and cleaning----
#loading gasoline
fuel_price = read_excel("fuel price yearly.xlsx", skip = 1)
fuel_price$DateTime = as.Date(fuel_price$DateTime, format = "%Y-%m-%d")
fuel_price$BF95 = as.numeric(fuel_price$BF95) #can create nice for loop here
fuel_price = rename(fuel_price, "year" = DateTime)
fuel_price$year = factor(format(fuel_price$year, format = "%Y"))

#loading gasoline tax
gas_tax = read_excel("gasoline yearly price and tax.xlsx", skip = 1)
gas_tax$DateTime = as.Date(gas_tax$DateTime, format = "%Y-%m-%d")
gas_tax$DateTime = factor(format(gas_tax$DateTime, format = "%Y"))
gas_tax = rename(gas_tax, "year" = DateTime)

#loading CPI
CPI = read_excel("SWECPIALLMINMEI.xlsx", skip = 10)
CPI = rename(CPI, "year" = observation_date)
CPI$year = as.Date(CPI$year, format = "%Y-%m-%d")
CPI$year = factor(format(CPI$year, format = "%Y"))

#loading and pivoting population density
pop_dens = read_excel("BE0101U1.xlsx", skip = 2) %>% 
  rename("municipality" = ...1)
pop_dens = pop_dens[,c("municipality", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")]
pop_dens = pop_dens[1:29,] %>% #removing comments
  separate(municipality, c("Municipality_code", "Municipality_name"), sep = " ", extra = "merge")
pop_dens = pivot_longer(pop_dens, 
               c(`2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`),
               names_to ="year", values_to = "pop_dens") %>% 
  subset(select=-c(Municipality_code))

  
  
#loading and pivoting quantity of gas per municipality
q_gas_mun = read_excel("quantity gas municipality.xlsx", skip = 2)
q_gas_mun = rename(q_gas_mun, "municipality" = ...1, "fuel type" = ...2)
q_gas_mun = q_gas_mun[1:291,] #removing comments
q_gas_mun = separate(q_gas_mun, municipality, c("Municipality_code", "Municipality_name"), sep = " ", extra = "merge")
q_gas_mun = pivot_longer(q_gas_mun, 
                         c(`2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`),
                         names_to ="year", values_to = "q")%>% 
  subset(select=-c(Municipality_code))

#loading and pivoting amount of men in the population
men = read_excel("BE0101N1.xlsx", skip = 1) %>% 
  rename("municipality" = ...1)
men = men[,c("municipality", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")]
men = men[1:290,] %>% #removing comments
  separate(municipality, c("Municipality_code", "Municipality_name"), sep = " ", extra = "merge") %>% 
  pivot_longer(c(`2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`), names_to ="year", values_to = "men")%>% 
  subset(select=-c(Municipality_code))


#loading information about municipal attributes
mun_attributes = read_excel("oversiktstabell-och-lista-kommungruppsindelnin.xlsx", sheet = "Lista Kommungruppsind 2017")
mun_attributes = rename(mun_attributes, "Municipality_name" = "Kommun namn", "SKR_name" = "Kommungrupp 2017 namn")
mun_attributes = select(mun_attributes, "Municipality_name", "SKR_name")

#loading and pivoting population per municipality
pop =  read_excel("be0101_folkmangdkom_1950-2019.xlsx", skip = 5)
pop = as.numeric(pop[1:290,]) #remove comments 
pop = rename(pop, "Municipality_name" = Kommun)
pop = pop[, c("Municipality_name", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")] #only keeps the relevant years, 2001 until 2017
pop = pivot_longer(pop, c(`2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`),
        names_to ="year", values_to = "population")


#loading and pivoting information about municipal income
income = read_excel("income.xlsx", skip = 3)
income = income[1:290,] #remove comments 
income_mean = income[,-c(2:14, 32:length(income))] #divides dataset into two sets. mean
colnames(income_mean ) <- c("municipality", as.character(c(2001:2017)))
income_mean = separate(income_mean, municipality, c("Municipality_code", "Municipality_name"), sep = " ", extra = "merge")
income_mean$`2001` = as.numeric(income_mean$`2001`)
income_mean = pivot_longer(income_mean, 
                      c(`2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`), names_to ="year", values_to = "income_mean")%>% 
  subset(select=-c(Municipality_code))


income_median = income[,-c(2:42, length(income))]  #divides dataset into two sets. median                      
colnames(income_median ) <- c("municipality", as.character(c(2001:2017)))
income_median = separate(income_median, municipality, c("Municipality_code", "Municipality_name"), sep = " ", extra = "merge")
income_median$`2001` = as.numeric(income_median$`2001`)
income_median = pivot_longer(income_median, 
                           c(`2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`), names_to ="year", values_to = "income_median")%>% 
  subset(select=-c(Municipality_code))

#loading and pivoting all 3 measures of commuters
commuters = read_excel("AM0207C6.xlsx", skip = 3)%>% 
  rename("municipality" = ...1)
commuters = commuters[1:290,] #removing comments
commuters = commuters[,-2]

commuters_in = commuters[,1:16]
colnames(commuters_in) <- c("municipality", as.character(c(2004:2018)))
commuters_in = separate(commuters_in, municipality, c("Municipality_code", "Municipality_name"), sep = " ", extra = "merge") %>% 
  pivot_longer(c(`2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`), names_to ="year", values_to = "commuters_in")%>% 
  subset(select=-c(Municipality_code))

commuters_out = commuters[,c(1, 17:31)]
colnames(commuters_out) <- c("municipality", as.character(c(2004:2018)))
commuters_out = separate(commuters_out, municipality, c("Municipality_code", "Municipality_name"), sep = " ", extra = "merge") %>% 
  pivot_longer(c(`2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`), names_to ="year", values_to = "commuters_out")%>% 
  subset(select=-c(Municipality_code))

commuters_within = commuters[,c(1,32:46)]
colnames(commuters_within) <- c("municipality", as.character(c(2004:2018)))
commuters_within = separate(commuters_within, municipality, c("Municipality_code", "Municipality_name"), sep = " ", extra = "merge") %>%   pivot_longer(c(`2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`), names_to ="year", values_to = "commuters_within")%>% 
  subset(select=-c(Municipality_code))


#merging the data frames
maindata = merge(q_gas_mun, mun_attributes, by = "Municipality_name") %>% #merging time invariant variables 
  merge(CPI, by = "year") %>% #loading individual invariant variables
  merge(fuel_price, by = "year") %>% 
  merge(gas_tax, by = "year") %>% 
  merge(pop, by = c("Municipality_name", "year")) %>% #loading time and individual variant variables
  merge(income_mean, by = c("Municipality_name", "year")) %>% 
  merge(income_median, by = c("Municipality_name", "year")) %>% 
  merge(men, by = c("Municipality_name", "year")) %>% 
  merge(commuters_in, by = c("Municipality_name", "year"), all.x = TRUE) %>% 
  merge(commuters_out, by = c("Municipality_name", "year"), all.x = TRUE) %>% 
  merge(commuters_within, by = c("Municipality_name", "year"), all.x = TRUE)


#calculating relative prices and relative consumption
maindata$gaspercapita = (as.numeric(maindata$q)/maindata$population)*1000000 #divided quantity used by population and multiplied by 1 million. 1000*1000 because 1 qubic meter = 1000L and the data is measured as 1000m3 for each municipality
maindata$percentmen = as.numeric(maindata$men/maindata$population) #percantage of men
maindata$percentin = as.numeric(maindata$commuters_in/maindata$population) #percentage of population that commutes in to the municipality
maindata$percentout = as.numeric(maindata$commuters_out/maindata$population) #percentage of population that commutes out of the municipality
maindata$percentwithin = as.numeric(maindata$commuters_within/maindata$population) #percentage of population that commutes within the municipality
maindata$real_gas_price = as.numeric(maindata$BF95/maindata$SWECPIALLMINMEI)*100 #create deflated gasoline price
maindata$real_mean_income = as.numeric(maindata$income_mean/maindata$SWECPIALLMINMEI)*100 #create deflated mean income
maindata$real_median_income = as.numeric(maindata$income_median/maindata$SWECPIALLMINMEI)*100 #create deflated median income
maindata$real_gas_VAT = as.numeric(as.numeric(maindata$Moms)/maindata$SWECPIALLMINMEI)*100 #create deflated median income
maindata$real_gas_tax = as.numeric(as.numeric(maindata$Skatt)/maindata$SWECPIALLMINMEI)*100 #create deflated median income
maindata$realest_gas_tax = as.numeric(as.numeric((maindata$Skatt)-as.numeric(maindata$Moms))/maindata$SWECPIALLMINMEI)*100 #create deflated median income

maindata = maindata[-c(302, 710),] #removing observatiosn with no gasolone consumption
#removing municipalities with NA
maindata = subset(maindata, Municipality_name != "Heby") %>% #removes Heby since it´s not full
  subset(Municipality_name != "Knivsta") %>%  #removes knivsta since it´s not full
  subset(Municipality_name != "Åsele")




ps_maindata = pdata.frame(maindata, index = c("Municipality_name", "year"),  drop.index=FALSE, row.names=TRUE)  
ps_maindata$q = as.numeric(ps_maindata$q)



#Plotting----
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

p <- maindata %>%
  ggplot( aes(x=gaspercapita, fill=SKR_name)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  labs(fill="") +
  theme(legend.position = "top")
xlim(0,1500)
p

hist(maindata$percentmen, breaks = 2000, xlim = c(0.3,0.6))



plot(ps_maindata)

hist(maindata$gaspercapita, breaks = 2000, xlim = c(0,900))#as can be seen from the histogram and summary, there appears to be a few outliers, namely those who do not consume any fuel

summary(maindata$gaspercapita)

#Testing----

#unit root of gaspercapita
UR = data.frame(split(((ps_maindata$percentwithin)), ps_maindata$"Municipality_name")) %>% 
  purtest( pmax = 10,index = "Municipality_name", exo = "intercept", test = "hadri")
# list of tests: 
#  c("levinlin", "ips", "madwu", "Pm", "invnormal", "logit", "hadri")

UR$statistic
summary(UR)
UR$statistic

plot(ps_maindata$percentout[1:17])

plot(ps_maindata$percentwithin[18:36])

# gaspercapita is I(1) according to hadri test
# income_mean and income_median are I(2) according to hadri test
# percentmen is I(1) according to hadri test
# commutersin, -out and -within are all I(1) according to hadri test


#ACF and PACF on gaspercapita
aa = acf(diff(maindata$gaspercapita[18:36]))

aa$acf

print(maindata$Municipality_name[1:17])

#testing first stage between real gas price and real gas VAT
#Am not able to make the regression work
FS = dynlm(model = (diff(real_gas_price) ~ diff(real_gas_VAT)), ps_maindata[1:17])
summary(FS)

plot(x = diff(maindata$real_gas_tax), y = diff(maindata$real_gas_price)) #VAT is a really good instrument, high first stage



#tests if gas price has unit root
adf.test(diff(maindata$real_gas_price[1:17])) #ADF test shows that we DON´T reject the null. alternative hypothesis is stationarity

plot(y = (maindata$real_gas_price[1:17]), x = maindata$year[1:17])


which(is.infinite(diff(log(maindata$gaspercapita))))
        
#Regressions----


#base regression  
form1 = "diff(log(gaspercapita)) ~ diff((lag(gaspercapita))) + diff(log((real_gas_price)))*SKR_name"
instr1 =  "~ diff(lag((gaspercapita))) + (log((real_VAT_price)))*SKR_name"
model1 = plm(formula = form1, ps_maindata, model = "within", effect = "individual")  
summary(model1)

form1 = "diff(log(gaspercapita)) ~ diff((lag(gaspercapita))) + diff(log((real_gas_price)))*SKR_name"
instr1 =  "~ diff(lag((gaspercapita))) + (log((real_VAT_price)))*SKR_name"
model1IV = plm(formula = form1, instruments = instr1, ps_maindata, model = "within", effect = "individual")  
summary(model1IV)


#checking differing income elasticities
form2 = "diff(log(gaspercapita)) ~ (lag(log(gaspercapita))) + diff(log((real_gas_price)))*SKR_name + income_median*SKR_name + population"
instr2 =  "~ diff(lag(log(gaspercapita))) + (log((real_VAT_price)))*SKR_name + income_median*SKR_name + population + population"
model2 = plm(formula = form2, instruments = instr2, ps_maindata, model = "within", effect = "individual")  

#base regression with men and commuters
form3 = "diff(log(gaspercapita)) ~ (lag(log(gaspercapita))) + diff(log((real_gas_price)))*SKR_name + diff(percentmen) + percentin + percentout + percentwithin"
instr3 =  "~ diff(lag(log(gaspercapita))) + (log((real_VAT_price)))*SKR_name + population + diff(percentmen) + percentin + percentout + percentwithin"
model3 = plm(formula = form3, instruments = instr3, ps_maindata, model = "within", effect = "individual")  
summary(model3)

#Trash----
#play around with the range to get all kinds of different plots for each municipality
saved_results = c(1:286)
for (i in 0:289){
    data = data.frame(maindata$year[(1+i*17):(17+i*17)], maindata$gaspercapita[(1+i*17):(17+i*17)])
  plot(data)
  title(maindata$Municipality_name[1+i*17])
  saved_results[i] = (adf.test(diff(data$maindata.gaspercapita..1...i...17...17...i...17..)))$p.value
}

diff_results = c(1:3)
for (i in 0:30){
  data = data.frame(diff(maindata$gaspercapita[(1+i*17):(17+i*17)]))
  plot(data)
  title(maindata$Municipality_name[1+i*17])
  print(i)
  diff_results[i] = adf.test(data$diff.maindata.gaspercapita..1...i...17...17...i...17...)$p.value
  print(diff_results[i])
}

print(diff_results)

summary(diff_results)

adf.test(maindata$gaspercapita[1:17])

adf.test(maindata$gaspercapita[18:34])

adf.test(diff(maindata$gaspercapita[1:17]))

adf.test(diff(maindata$gaspercapita[18:34]))

plot(diff(maindata$gaspercapita[1:17]))

plot(diff(maindata$gaspercapita[18:33]))


dm = (diff(maindata$gaspercapita[1:17]))
plot(dm)


write_excel_csv(ps_maindata, path = getwd())

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



https://www.reed.edu/data-at-reed/resources/R/loops_with_ggplot2.html
for (i in seq_along(county_list)) { 
  
  # create plot for each county in df 
  plot <- 
    ggplot(subset(df, df$County==county_list[i]),
           aes(Year, value/1000, group = County, colour = category)) + 
    
    geom_line(size=2) +
    facet_wrap( ~  category, ncol=2) +
    
    theme_pander() +
    theme(legend.position="none") + 
    
    scale_y_continuous("County Population within Age Categories (thousands)", 
                       limits=c(0, max(df$value[df$County==county_list[i]]))/1000) +
    scale_x_continuous("Year") +
    
    ggtitle(paste(county_list[i], ' County, New Jersey \n', 
                  "County Population Projection within Age Categories (thousands) \n",
                  sep=''))
  print(plot)
}

plot()
is.numeric.Date(gas_price$year)

wut = data.frame(diff(as.numeric(gas_price$Real_Price)), gas_price$year[-1])

plot(y = wut$diff.as.numeric.gas_price.Real_Price.., x = wut$gas_price.year..1., types = "l")

ggplot(wut, aes(x = "gas_price.year..1.", y = as.numeric("diff.as.numeric.gas_price.Real_Price..")))

