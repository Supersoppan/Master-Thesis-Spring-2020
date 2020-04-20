packages = c("readxl", "tidyverse", "tseries", "plm", "dynlm", "ggplot2")
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

#loading and pivoting quantity of gas per municipality
q_gas_mun = read_excel("quantity gas municipality.xlsx", skip = 2)
q_gas_mun = rename(q_gas_mun, "municipality" = ...1, "fuel type" = ...2)
q_gas_mun = q_gas_mun[1:291,] #removing comments
q_gas_mun = separate(q_gas_mun, municipality, c("Municipality_code", "Municipality_name"), sep = " ", extra = "merge")
q_gas_mun = pivot_longer(q_gas_mun, 
                         c(`2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`),
                         names_to ="year", values_to = "q")




#loading and pivoting population per municipality
pop =  read_excel("be0101_folkmangdkom_1950-2019.xlsx", skip = 5)
pop = pop[1:290,] #remove comments 
pop = pop[, c("Kommun", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")] #only keeps the relevant years, 2001 until 2017
pop = subset(pop, Kommun != "Knivsta") #removing knivsta and Heby
#pop = subset(pop, Kommun != "Heby") #removing knivsta and Heby
pop = pivot_longer(pop, c(`2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`),
        names_to ="year", values_to = "population")
pop = rename(pop, "Municipality_name" = Kommun)


#loading information about municipal attributes
mun_attributes = read_excel("oversiktstabell-och-lista-kommungruppsindelnin.xlsx", sheet = "Lista Kommungruppsind 2017")
mun_attributes = rename(mun_attributes, "Municipality_code" = Kommunkod, "SKR_name" = "Kommungrupp 2017 namn")
mun_attributes = select(mun_attributes, "Municipality_code", "SKR_name")

#loading and pivoting information about municipal income
income = read_excel("income.xlsx", skip = 3)
income = income[1:290,] #remove comments 
income_mean = income[,-c(2:14, 32:length(income))] #divides dataset into two sets. mean
colnames(income_mean ) <- c("municipality", as.character(c(2001:2017)))
income_mean = separate(income_mean, municipality, c("Municipality_code", "Municipality_name"), sep = " ", extra = "merge")
income_mean$`2001` = as.numeric(income_mean$`2001`)
income_mean = pivot_longer(income_mean, 
                      c(`2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`), names_to ="year", values_to = "income_mean")


income_median = income[,-c(2:42, length(income))]  #divides dataset into two sets. median                      
colnames(income_median ) <- c("municipality", as.character(c(2001:2017)))
income_median = separate(income_median, municipality, c("Municipality_code", "Municipality_name"), sep = " ", extra = "merge")
income_median$`2001` = as.numeric(income_median$`2001`)
income_median = pivot_longer(income_median, 
                           c(`2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`), names_to ="year", values_to = "income_median")

#merging the data frames
maindata = merge(q_gas_mun, mun_attributes, by = "Municipality_code") %>% #merging time invariant variables 
  merge(CPI, by = "year") %>% #loading individual invariant variables
  merge(fuel_price, by = "year") %>% 
  merge(gas_tax, by = "year") %>% 
  merge(pop, by = c("Municipality_name", "year")) %>% #loading time and individual variant variables
  merge(income_mean, by = c("Municipality_name", "year")) %>% 
  merge(income_median, by = c("Municipality_name", "year")) 
maindata = subset(maindata, select=-c(Municipality_code.x,Municipality_code.y))

#calculating relative prices and relative consumption
maindata$gaspercapita = (as.numeric(maindata$q)/maindata$population)*1000000 #divided quantity used by population and multiplied by 1 million. 1000*1000 because 1 qubic meter = 1000L and the data is measured as 1000m3 for each municipality
maindata$real_gas_price = as.numeric(maindata$BF95/maindata$SWECPIALLMINMEI)*100 #create deflated gasoline price
maindata$real_mean_income = as.numeric(maindata$income_mean/maindata$SWECPIALLMINMEI)*100 #create deflated mean income
maindata$real_median_income = as.numeric(maindata$income_median/maindata$SWECPIALLMINMEI)*100 #create deflated median income
maindata$real_gas_VAT = as.numeric(as.numeric(maindata$Moms)/maindata$SWECPIALLMINMEI)*100 #create deflated median income

#removing municipalities with NA
maindata = subset(maindata, Municipality_name != "Heby") #removes Heby since it´s not full
q_gas_mun = subset(q_gas_mun, Municipality_name != "Knivsta") #removes knivsta since it´s not full

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


hist(maindata$gaspercapita, breaks = 2000, xlim = c(0,900))#as can be seen from the histogram and summary, there appears to be a few outliers, namely those who do not consume any fuel
summary(maindata$gaspercapita)

#Testing----

#tests if the panel data has unit root. Difference in results based on pmax. pmax = 4(no rejection, Unit root present), pmax = 5(Reject, No unit root) 
data.frame(split(maindata$gaspercapita, maindata$"Municipality_name")) %>% 
  purtest( pmax = 5, exo = "intercept", test = "hadri")
  # list of tests: 
  'c("levinlin", "ips", "madwu", "Pm", "invnormal", "logit", "hadri")'

jad = adf.test(ps_maindata$real_gas_price)

jad$

summary(is.na(maindata))
  


#tests if gas price has unit root
adf.test(maindata$real_gas_price) #ADF test shows that we reject the null. alternative hypothesis is stationarity
plot(y = maindata$real_gas_price, x = maindata$year)
  
asd = plm(formula = (gaspercapita) ~ lag((real_gas_price),0:1) + lag(real_gas_price:SKR_name,0:1), ps_maindata, model = "within", effect = "individual")
summary(asd)

plot(ps_maindata, N = 5)

summary(is.na(maindata))


sad = plm(formula = ((gaspercapita)) ~ (lag((gaspercapita))) + ((real_gas_price))  + (((real_gas_price)))*SKR_name + income_median | 
            (lag(log(q))) + (log(real_gas_VAT))  + ((log(real_gas_VAT)))*SKR_name + income_median, ps_maindata, model = "within", effect = "individual")
summary(sad)



#Trash----
#play around with the range to get all kinds of different plots for each municipality
saved_results = c(1:290)
for (i in 0:290){
    data = data.frame(maindata$year[(1+i*17):(17+i*17)], maindata$gaspercapita[(1+i*17):(17+i*17)])
  plot(data)
  title(maindata$Municipality_name[1+i*17])
  saved_results[i] = (adf.test(data$maindata.gaspercapita..1...i...17...17...i...17..))$p.value
}







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

