packages = c("readxl", "tidyverse", "tseries", "plm", "dynlm", "ggplot2", "urca", "AER", "stargazer", "sjPlot", "forecast", "prediction", "pco", "dLagM", "car", "htmlTable", "sf", "doBy")
lapply(packages, library, character.only = TRUE) 




keep = FALSE #only keep the main data and discard the individual data being loaded? Recommended when doing the regressions due to alot of data frames cluttering the data window in Rstudio
Swed_name = FALSE #set SKR_name to swedish names or english? if english names are set, the swedish names are still saved in SKR_name_swe

#Loading and cleaning----
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
#loading global price of brent crude oil in USD
Globaloil = read_excel("POILBREUSDA.xls", skip = 10)
Globaloil = rename(Globaloil, "year" = observation_date, "global_oil" = POILBREUSDA)
Globaloil$year = as.Date(Globaloil$year, format = "%Y-%m-%d")
Globaloil$year = factor(format(Globaloil$year, format = "%Y"))
#loading US CPI
USCPI = read_excel("CPALTT01USQ657N.xls",skip = 10)
USCPI = rename(USCPI, "year" = observation_date, "USCPI" = CPALTT01USQ657N_NBD20150101)
USCPI$year = as.Date(USCPI$year, format = "%Y-%m-%d")
USCPI$year = factor(format(USCPI$year, format = "%Y"))
#loading CPI
CPI = read_excel("SWECPIALLMINMEI.xlsx", skip = 10)
CPI = rename(CPI, "year" = observation_date)
CPI$year = as.Date(CPI$year, format = "%Y-%m-%d")
CPI$year = factor(format(CPI$year, format = "%Y"))
#loading and pivoting population density
pop_dens = read_excel("BE0101U1.xlsx", skip = 2) %>% 
  rename("municipality" = ...1)
pop_dens = pop_dens[,c("municipality", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")]
pop_dens = pop_dens[1:290,] %>% #removing comments
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
q_gas_mun$q = as.numeric(q_gas_mun$q)
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
#loading and pivoting information about median income
income_mean = income[,-c(2:14, 32:length(income))] #divides dataset into two sets. mean
colnames(income_mean ) <- c("municipality", as.character(c(2001:2017)))
income_mean = separate(income_mean, municipality, c("Municipality_code", "Municipality_name"), sep = " ", extra = "merge")
income_mean$`2001` = as.numeric(income_mean$`2001`)
income_mean = pivot_longer(income_mean, 
                      c(`2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`), names_to ="year", values_to = "income_mean")%>% 
  subset(select=-c(Municipality_code))
#loading and pivoting information about median income
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
#loading and pivoting commuters going into the municipality
commuters_in = commuters[,1:16]
colnames(commuters_in) <- c("municipality", as.character(c(2004:2018)))
commuters_in = separate(commuters_in, municipality, c("Municipality_code", "Municipality_name"), sep = " ", extra = "merge") %>% 
  pivot_longer(c(`2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`), names_to ="year", values_to = "commuters_in")%>% 
  subset(select=-c(Municipality_code))
#loading and pivoting commuters going into the municipality
commuters_out = commuters[,c(1, 17:31)]
colnames(commuters_out) <- c("municipality", as.character(c(2004:2018)))
commuters_out = separate(commuters_out, municipality, c("Municipality_code", "Municipality_name"), sep = " ", extra = "merge") %>% 
  pivot_longer(c(`2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`), names_to ="year", values_to = "commuters_out")%>% 
  subset(select=-c(Municipality_code))
#loading and pivoting commuters going into the municipality
commuters_within = commuters[,c(1,32:46)]
colnames(commuters_within) <- c("municipality", as.character(c(2004:2018)))
commuters_within = separate(commuters_within, municipality, c("Municipality_code", "Municipality_name"), sep = " ", extra = "merge") %>%   pivot_longer(c(`2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`), names_to ="year", values_to = "commuters_within")%>% 
  subset(select=-c(Municipality_code))
#merging the data frames
maindata = merge(q_gas_mun, mun_attributes, by = "Municipality_name", all.x = TRUE) %>% #merging time invariant variables 
  merge(CPI, by = "year", all.x = TRUE) %>% #loading individual invariant variables
  merge(fuel_price, by = "year", all.x = TRUE) %>% 
  merge(gas_tax, by = "year", all.x = TRUE) %>% 
  merge(Globaloil, by = "year", all.x = TRUE) %>%
  merge(USCPI, by = "year", all.x = TRUE) %>%
  merge(pop, by = c("Municipality_name", "year"), all.x = TRUE) %>% #loading time and individual variant variables
  merge(income_mean, by = c("Municipality_name", "year"), all.x = TRUE) %>% 
  merge(income_median, by = c("Municipality_name", "year"), all.x = TRUE) %>% 
  merge(men, by = c("Municipality_name", "year"), all.x = TRUE) %>% 
  merge(commuters_in, by = c("Municipality_name", "year"), all.x = TRUE) %>% 
  merge(commuters_out, by = c("Municipality_name", "year"), all.x = TRUE) %>% 
  merge(commuters_within, by = c("Municipality_name", "year"), all.x = TRUE) %>% 
  merge(pop_dens, by = c("Municipality_name", "year"), all.x = TRUE)
maindata = subset(maindata, Municipality_name != "Knivsta") %>%  #removes knivsta since it´s not full
  subset(q !="..") #removes all observations where there are no entry for quantity, also removes the duplicates of Heby
#calculating relative prices and relative consumption
maindata$gaspercapita = (as.numeric(maindata$q)*1000000/maindata$population) #divided quantity used by population and multiplied by 1 million. 1000*1000 because 1 qubic meter = 1000L and the data is measured as 1000m3 for each municipality
maindata$percentmen = as.numeric(maindata$men/maindata$population) #percantage of men
maindata$percentin = as.numeric(maindata$commuters_in/maindata$population) #percentage of population that commutes in to the municipality
maindata$percentout = as.numeric(maindata$commuters_out/maindata$population) #percentage of population that commutes out of the municipality
maindata$percentwithin = as.numeric(maindata$commuters_within/maindata$population) #percentage of population that commutes within the municipality
maindata$CPI2017 = as.numeric(maindata$SWECPIALLMINMEI/102.79643)*100 #index everything to 2017 prices
maindata$real_gas_price = as.numeric(maindata$BF95/maindata$CPI2017)*100 #create deflated gasoline price
maindata$real_income_mean = as.numeric(maindata$income_mean/maindata$CPI2017)*100 #create deflated mean income
maindata$real_income_median = as.numeric(maindata$income_median/maindata$CPI2017)*100 #create deflated median income
maindata$real_gas_VAT = as.numeric(as.numeric(maindata$Moms)/maindata$CPI2017)*100 #create deflated median income
maindata$real_gas_tax = as.numeric(as.numeric(maindata$Skatt)/maindata$CPI2017)*100 #create deflated median income
maindata$real_gas_global = maindata$global_oil/maindata$USCPI*100
maindata$gaspercapita[maindata$gaspercapita < 0.01] <- 1e-5 #setting values of zero equal to a very small value, otherwise you can´t log it
maindata$q[maindata$q < 0.01] <- 1e-5 #setting values of zero equal to a very small value, otherwise you can´t log it
ps_maindata = pdata.frame(maindata, index = c("Municipality_name", "year"),  drop.index=FALSE, row.names=TRUE, stringsAsFactors = TRUE)

#Replaces all swedish group names with the official english ones
if (Swed_name == FALSE){
  j = ps_maindata
  j$SKR_name_swe = j$SKR_name
  j$SKR_name = gsub("Pendlingskommun nära storstad", "Commuting municipalities near large cities", j$SKR_name)
  j$SKR_name = gsub("Pendlingskommun nära mindre tätort", "Commuting municipalities near small towns", j$SKR_name)
  j$SKR_name = gsub("Landsbygdskommun med besöksnäring", "Rural municipalities with a visitor industry", j$SKR_name)
  j$SKR_name = gsub("Pendlingskommun nära större stad", "Commuting municipalities near medium-sized towns", j$SKR_name)
  j$SKR_name = gsub("Lågpendlingskommun nära större stad", 
                    "Commuting municipalities with a low commuting rate near medium-sized", j$SKR_name)
  j$SKR_name = gsub("Landsbygdskommun", "Rural municipalities ", j$SKR_name)
  j$SKR_name = gsub("Mindre stad/tätort", "Small towns", j$SKR_name)
  j$SKR_name = gsub("Större stad", "Medium-sized town", j$SKR_name)
  j$SKR_name = gsub("Storstäder", "Large cities", j$SKR_name)
  ps_maindata = j  
}

if (keep == FALSE){
  remove(commuters, commuters_in, commuters_out, commuters_within, CPI, fuel_price, gas_tax, income, income_mean, income_median, men, mun_attributes, pop, pop_dens, population, q_gas_mun, Globaloil, USCPI)
}


#Plotting & summary statistics----

P_d = exp((seq(0,10, 0.01))/-1.5)*exp(3/-1.5)
P_d = exp((seq(0,10, 0.01))/-1)*exp(3/-1)


#negative externalities
#creates the data to be used for plotting and the intial plots
neg_ext = data.frame(Q = seq(0.1,10, 0.01), 
                     P_s_private = c(2.0),
                     P_s_social = c(4),
                     P_d = exp((log((seq(0.1,10, 0.01)))-2.2)/(-0.5)),
                     P_d_winner = exp((log((seq(0.1,10, 0.01)))-3.1)/(-1.5)))
p_neg_base = ggplot(neg_ext) +
  theme_classic() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(color = NA)) +
  xlab("Quantity") +
  ylab("Price") +
  coord_cartesian(xlim=c(0, 10), ylim=c(0, 7)) +
  theme(legend.position = "none") +
  geom_line(aes(x = Q, y = P_s_private)) + #private supply
  annotate("text", label = "MC private", x = 1.7, y = 1.7) +
  geom_line(aes(x = Q, y = P_s_social)) + #social supply
  annotate("text", label = "MC social", x = 1.7, y =4.4) +
  geom_line(aes(y = P_d, x = Q)) +#demand
  annotate("text", label = "Demand", x = 4.2, y = 6, angle = -75)

#the base problem of negative externalities
p_neg = p_neg_base + 
  geom_ribbon(data = subset(neg_ext, P_d <= P_s_social & P_d > P_s_private), aes(ymin = P_d, ymax = P_s_social, x = Q, alpha = 0.5), fill = "red") +
  annotate("text",label = "Negative \n Externality", x = 5.75, y = 3.35, angle =-50 )
p_neg

#pig tax - the purpose of the pivourivan tax, to correct negative externalities
p_corrected = p_neg + 
  geom_ribbon(data = subset(neg_ext, P_d <= P_s_social & P_d >= P_s_private), aes(ymin = P_s_private, ymax = P_d, x = Q, alpha = 0.5), fill = "blue") +
  geom_ribbon(data = subset(neg_ext, P_d >= P_s_social & P_d >= P_s_private), aes(ymin = P_s_private, ymax = P_s_social, x = Q, alpha = 0.5), fill = "blue") +
  geom_segment(aes(x = 1.0, xend = 1.0, y = 2.2, yend = 3.8), arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(aes(x = 8.0, xend = 8.0, y = 2.2, yend = 3.8), arrow = arrow(length = unit(0.3, "cm"))) +
  annotate("text",label = "Consumer Surplus \n loss", x = 3, y = 3, )
p_corrected


#consumer surplus loss for the INelastic consumer
p_incidence = p_neg_base
p_incidence = p_incidence
p_loser  = p_incidence + geom_ribbon(data = subset(neg_ext, P_d <= P_s_social & P_d > P_s_private), aes(ymin = P_s_private, ymax = P_d, x = Q, alpha = 0.5), fill = "blue") +
  geom_ribbon(data = subset(neg_ext, P_d >= P_s_social & P_d > P_s_private), aes(ymin = P_s_private, ymax = P_s_social, x = Q, alpha = 0.5), fill = "blue") +
  annotate("text",label = "Consumer Surplus \n loss", x = 3, y = 3, )
p_loser

#consumer surplus loss for the elastic consumer
p_incidence = p_neg_base
p_incidence$layers[[6]] = NULL #remove demand title
p_incidence$layers[[4]] = NULL #remove social supply line
p_incidence$layers[[4]] = NULL #remove inelastic demand line
p_incidence$layers[[2]] = NULL #remove private supply line
p_winner = p_incidence + geom_line(aes(y = P_d_winner, x = Q)) + #elastic demand
  annotate("text", label = "MC private", x = 1.4, y = 1.7) +
  annotate("text", label = "MC social", x = 1.4, y = 4.4)+
  geom_ribbon(data = subset(neg_ext, round(P_d_winner,2) <= P_s_social & P_d_winner > P_s_private), aes(ymin = P_s_private, ymax = P_d_winner, x = Q, alpha = 0.5), fill = "blue") +
  geom_ribbon(data = subset(neg_ext, P_d_winner >= P_s_social & P_d_winner > P_s_private), aes(ymin = P_s_private, ymax = P_s_social, x = Q, alpha = 0.5), fill = "blue") +
  annotate("text",label = "Consumer Surplus \n loss", x = 2.0, y = 3, ) +
  annotate("text", label = "Demand", x = 2.2, y = 6, angle = -75)
p_winner
  

#data exploration 
p = ps_maindata %>%
  ggplot( aes(x=gaspercapita, fill=SKR_name)) +888
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  labs(fill="") +
  theme(legend.position = "top")
xlim(0,1500)
p

hist(maindata$percentmen, breaks = 2000, xlim = c(0.3,0.6))


hist(maindata$gaspercapita, breaks = 2000, xlim = c(0,900))#as can be seen from the histogram and summary, there appears to be a few outliers, namely those who do not consume any fuel

used = maindata[c( "real_income_mean", "gaspercapita", "percentmen", "percentin", "percentout", "percentwithin", "pop_dens") ]
price = subset(maindata, Municipality_name == "Svedala", "real_gas_price",)
stargazer(used, type = "html", out = "summary_partial")
stargazer(price, type = "html", out = "summary_partial_price")

t = summaryBy(gaspercapita ~ SKR_name, maindata, FUN = c(mean, min, max ))




#Regressions----
SKR.subset.regression = function(form, p.values = TRUE, 
                                 std.errors = TRUE, 
                                 cleantable = FALSE,
                                 starTF = TRUE){
  #takes in form as its formula, the equation to be regressed and returns a list with the coefficents, p-values and significance stars for all groups of SKR
  #starTF gives the option of including the stars of the coefficents
  modellist = data.frame(1:300) #create an empty data frame which can hold the coefficents, p-values and significance stars.
  modelnames = as.character(unique(ps_maindata$SKR_name)) #get all the different names of municipality groupings
  InformationC = data.frame() #empty data frame to contain all of the AIC and BIC values
  #run the regression, assign significance stars for all of the municipality groupings
  for (m_name in modelnames){
    datasubset = filter(ps_maindata, SKR_name == m_name) #create a temporary subset of the data that will also hold the ecm
    ecm_model = plm(formula = log(gaspercapita) ~ log(real_gas_price) + log(real_income_mean), data = datasubset, model = "within", effect = "individual") #estimate the ecm
    ecm = data.frame(ecm = residuals(ecm_model), attr(residuals(ecm_model), "index")) #extract the reiduals
    datasubset = merge(datasubset, ecm, by = c("Municipality_name", "year")) #bind the residuals with its respective observation
    model = plm(formula =  form, data = datasubset, model = "within", effect = "individual")
    #runs the model according to the formula provided
    summarymodel = summary(model)
    model_values = (summarymodel$coefficients[,c(1, 2,4)]) #retrieves the coefficents (1), standard errors (2) and the p-values (4)
    ecm_values = summary(ecm_model)$coefficients[,c(1, 2,4)] #retrieves the coefficents (1), standard errors (2) and the p-values (4)
    modellist = modellist[c(1:(length(model_values[,1]) + length(ecm_values[,1]))),]
    #creates the labels for the columns
    columnnames = c()
    columnnames[1] = paste(m_name[1], "Coefficent", sep = "_")
    columnnames[2] = paste(m_name[1], "Std.errors", sep = "_")
    columnnames[3] = paste(m_name[1], "P-value", sep = "_")
    columnnames[4] = paste(m_name[1], "Significance", sep = "_")
    model_values = rbind(model_values, ecm_values)
    model_values = round(model_values, digits = 3) #rounds the digits down
    model_values = cbind(model_values, " ") #binds test with an empty column which will house the significance stars
    colnames(model_values) <- columnnames #rename columns
    R2 = summary(model)$r.squared["adjrsq"]
    IC = cbind(AIC(model), BIC(model), R2, nobs(model)) #get AIC, BIC and R2 of each municipality grouping and place them in IC
    InformationC = rbind(InformationC, IC)
    #checks which p-values that are significant
    if (starTF == TRUE){
    for (i in seq_along(model_values[,1])){ 
      p = model_values[i,3] #retrieves the p-value
      if (p < 0.05){ #checks the p-value
        model_values[i,4] = "*" #inserts star if the estimate is significant
      }
      if (p < 0.01){ #checks the p-value
        model_values[i,4] = "**" #inserts star if the estimate is significant
      }
      if (p < 0.001){ #checks the p-value
        model_values[i,4] = "***" #inserts star if the estimate is significant
      }}}
    if (starTF == FALSE){
      model_values = model_values[,-4]
  }
    if (p.values == FALSE){
      model_values = model_values[,-3]
    }
    if (std.errors == FALSE){
      model_values = model_values[,-2]
    }
    modellist = cbind(modellist, model_values)
  }
  modellist = modellist[, -1]
  rn = row.names(modellist)
  t = rn
  t = gsub("lag\\(","L.", t) %>% 
    gsub("log\\(","", .) %>% 
    gsub("diff\\(","D.", .) %>% 
    gsub("lagprice","", . ) %>% 
    gsub("lagincome","", . ) %>% 
    gsub("lagpX","", . ) %>% 
    gsub("lagY","", . ) %>% 
    gsub(",","", . ) %>% 
    gsub("","", . ) %>% 
    gsub("\\)","", . ) %>% 
    gsub(" ",".", .)
  rn = t
  rownames(InformationC) <- modelnames
  colnames(InformationC) <- c("AIC", "BIC", "R2", "N")
  #gives the option to get a nicely formatted table with only one column per estimation
  if (p.values == FALSE & std.errors == FALSE & cleantable == TRUE){
    cleantable = c() #initialise the table to hold the estimates
    cn = colnames(modellist) #get column names
    r = c(1:(ncol(modellist)/2)) #as each model covers two columns, we need to divide it by 2
    for (i in r){ #loop which takes the value of coefficent and pastes in the significance star of the next column(if star present)
      j = (i-1)*2
      cleantable = cbind(cleantable, paste(modellist[,1+j], modellist[, 2+j]))
    }
    colnames(cleantable) <- modelnames #set column names
    modellist = cleantable
    .rowNamesDF(modellist) <- rn #set row names
  } else {
   row.names(modellist) <- rn
 }
modellist = list(Coefficients = modellist, InformationCriteria = InformationC)
return(modellist)
  #end of function
}
#make a function that takes the estimates and their standard errors and shifts the stnadard errors below the estimates

CI.table = function(df, t.value = 1.96){
  #requires that coefficents, standard errors, p-values and significance stars are present
  #only accepts having std.errors and p.values and significance stars ( p.values = TRUE, std.errors = TRUE, cleantable = FALSE ")
  #this function takes in a data frame which has coefficents, standard errors, p-values and significance stars(4 variables per municipality group) and returns a list with dataframes over each variable´s confidence interval based on the t-value given in the function OR one that is supplied by the user
  grouping = ps_maindata$SKR_name
  CI = list()
  variable = data.frame()
  modelnames = as.character(unique(grouping)) #assign each row in the created data.frame a name based on which grouping used
  nr = nrow(df)
  nc = ncol(df)
  for (varnum in c(1:nr)){ #for each variable
    variable = data.frame()
    for (munnum in c(1:nc)){ #for each municipality grouping
      coefid = (munnum*4)-3
      if (coefid > nc){break}
      variable[munnum,c(1,2)] = as.numeric(df[varnum, c(coefid, coefid+1)]) #adding coefficent and its std. errors
    }
    variable[,3] = variable[1] - t.value*variable[2] #lower CI t-value
    variable[,4] = variable[1] + t.value*variable[2] #upper CI t-value
    variable[,5] = variable[1] - 1.96*variable[2] #lower CI 95%
    variable[,6] = variable[1] + 1.96*variable[2] #upper CI 95%
    rownames(variable) <- modelnames
    variable = cbind(variable, modelnames)
    colnames(variable) <- c("Estimates", "Std Errors", "Lower CI t", "Upper CI t", "Lower CI 95", "Upper CI 95", "SKR name") #set names for what each value represents
    CI[varnum] = list(variable) #add the variablés confidence interval to the list
  }
  #removes ugly name features from the variables
  t = rownames(df)
  names(CI) <- t #set each element in the list to its corresponding variable name
  return(CI)
}



#baseline regression, no X´s
#no lag - Lowest BIC median & mean aswell as AIC median & mean
lagprice = 0:0
lagincome = 0:0
lagY = 1:0
lagX = 0:0
form = "diff(log(gaspercapita)) ~ lag(ecm) + lag(diff(log(real_gas_price)),lagprice) + lag(diff(log(real_income_mean)), lagincome) "
Lag0NoX = SKR.subset.regression(form, p.values = TRUE, std.errors = TRUE, cleantable = TRUE)
#summary(Lag0NoX$InformationCriteria)

#export descriptive statistics of regressions
options(scipen=999)
Lag0NoX_appendix = SKR.subset.regression(form, p.values = TRUE, std.errors = TRUE, cleantable = TRUE, starTF = FALSE)
Lag0NoX_appendix = t(Lag0NoX_appendix$InformationCriteria) %>% round(digits = 2)
stargazer(Lag0NoX_appendix, type = "html", out = "Lag0NoX_Descriptives", summary = FALSE)
#stargazer(Lag0NoX$Coefficients, type = "html", out = "Lag0NoX", summary = FALSE)

#Speed of adjustment with no covariates. CI of 84%
plot_me = CI.table(Lag0NoX$Coefficients, t.value = 1.4)
p_noXspeed = ggplot(data = plot_me$L.ecm) +
  theme(axis.text = element_text(size=16)) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
  coord_cartesian(xlim=c(-1,0.1)) +
  labs(y = "SKR name", title = "Speed of adjustment with no covariates. CI of 84%") +
  geom_linerange(aes(y = SKR_name, xmin = Lower_CI_95, xmax = Upper_CI_95), alpha = 0.4, colour = "blue") +
  geom_linerange(aes(y = SKR_name, xmin = Lower_CI_t, xmax = Upper_CI_t), alpha = 0.6, colour = "red") +
  geom_point(aes(x = Estimates, y = rownames(plot_me$L.ecm), ))
p_noXspeed

png("p_noXspeed.png", width = 800, height = 558)
plot(p_noXspeed)
dev.off()

#Long run Gas price elasticity with no covariates. CI of 84%
plot_me = CI.table(Lag0NoX$Coefficients, t.value = 1.4)
p_noXlongP = ggplot(data = plot_me$real_gas_price) +
  theme(axis.text = element_text(size=16)) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
  coord_cartesian(xlim=c(-1,0.2)) +
  labs(y = "SKR name", title = "Long run Gas price elasticity with no covariates - CI of 84%") +
  geom_linerange(aes(y = SKR_name, xmin = Lower_CI_95, xmax = Upper_CI_95), alpha = 0.4, colour = "blue") +  
  geom_linerange(aes(y = SKR_name, xmin = Lower_CI_t, xmax = Upper_CI_t), alpha = 0.6, colour = "red") +
  geom_point(aes(x = Estimates, y = rownames(plot_me$L.ecm), ))
p_noXlongP

png("p_noXlongP.png", width = 800, height = 558)
plot(p_noXlongP)
dev.off()

#Long run income elasticity with no covariates. CI of 84%
plot_me = CI.table(Lag0NoX$Coefficients, t.value = 1.4)
p_noXlongI = ggplot(data = plot_me$real_income_mean) +
  theme(axis.text = element_text(size=16)) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
  coord_cartesian(xlim=c(-4,0.2)) +
  labs(y = "SKR name", title = "Long run income elasticity with no covariates. CI of 84%") +
  geom_linerange(aes(y = SKR_name, xmin = Lower_CI_95, xmax = Upper_CI_95), alpha = 0.4, colour = "blue") +  
  geom_linerange(aes(y = SKR_name, xmin = Lower_CI_t, xmax = Upper_CI_t), alpha = 0.6, colour = "red") +
  geom_point(aes(x = Estimates, y = rownames(plot_me$L.ecm), ))

png("p_noXlongI.png", width = 800, height = 558)
plot(p_noXlongI)
dev.off()

#short run price elasticity. CI of 84%
plot_me = CI.table(Lag0NoX$Coefficients, t.value = 1.4)
p_noXshortP = ggplot(data = plot_me$L.D.real_gas_price.) +
  theme(axis.text = element_text(size=16)) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
  coord_cartesian(xlim=c(-1,1)) +
  labs(y = "SKR name", title = "Short run price elasticity with no covariates. CI of 84%") +
  geom_linerange(aes(y = SKR_name, xmin = Lower_CI_95, xmax = Upper_CI_95), alpha = 0.4, colour = "red") +
  geom_linerange(aes(y = SKR_name, xmin = Lower_CI_t, xmax = Upper_CI_t), alpha = 0.6, colour = "red") +
  geom_point(aes(x = Estimates, y = rownames(plot_me$L.ecm), ))

png("p_noXshortP.png", width = 800, height = 558)
plot(p_noXshortP)
dev.off()


#short run income elasticity. CI of 84%
plot_me = CI.table(Lag0NoX$Coefficients, t.value = 1.4)
p_noXshortI = ggplot(data = plot_me$L.D.real_income_mean.) +
  theme(axis.text = element_text(size=16)) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
  coord_cartesian(xlim=c(-1,1)) +
  labs(y = "SKR name", title = "Short run income elasticity with no covariates. CI of 84%") +
  geom_linerange(aes(y = SKR_name, xmin = Lower_CI_95, xmax = Upper_CI_95), alpha = 0.4, colour = "red") +  
  geom_linerange(aes(y = SKR_name, xmin = Lower_CI_t, xmax = Upper_CI_t), alpha = 0.6, colour = "red") +
  geom_point(aes(x = Estimates, y = rownames(plot_me$L.ecm), ))

png("p_noXshortI.png", width = 800, height = 558)
plot(p_noXshortI)
dev.off()

#export confidence intervals
plot_me = CI.table(Lag0NoX$Coefficients, t.value = 1.4)
write.table(plot_me$L.ecm, file = "Long ecm noX.doc", sep = "\t")
write.table(plot_me$L.D.real_gas_price., file = "short price noX.doc", sep = "\t")
write.table(plot_me$L.D.real_income_mean., file = "short income noX.doc", sep = "\t")
write.table(plot_me$real_gas_price, file = "Long price noX.doc", sep = "\t")
write.table(plot_me$real_income_mean, file = "Long income noX.doc", sep = "\t")


#now with covariates
#0 lag - lowest BIC
lagprice = 0:0
lagincome = 0:0
lagY = 1:0
lagX = 0:0
form = "diff(log(gaspercapita)) ~  lag(ecm) + lag(diff(log(real_gas_price)),lagprice) + lag(diff(log(real_income_mean)), lagincome) + diff(log(pop_dens)) + diff(log(percentmen)) + (log(percentin)) + (log(percentout)) + (log(percentwithin))"
Lag0X = SKR.subset.regression(form, p.values = TRUE, std.errors = TRUE, cleantable = TRUE)
#summary(Lag0X$InformationCriteria)

stargazer(Lag0X$Coefficients, type = "html", out = "Lag0X", summary = FALSE)

options(scipen=999)
Lag0X_appendix = SKR.subset.regression(form, p.values = TRUE, std.errors = TRUE, cleantable = TRUE, starTF = FALSE)
Lag0X_appendix = t(Lag0X_appendix$InformationCriteria) %>% round(digits = 2)


#speed of adjustment
plot_me = CI.table(Lag0X$Coefficients, t.value = 1.4)
p_Xspeed = ggplot(data = plot_me$L.ecm) +
  theme(axis.text = element_text(size=16)) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
  coord_cartesian(xlim=c(-1,0.1)) +
  labs(y = "SKR name", title = "speed of adjustment with covariates. CI of 84%") +
  geom_linerange(aes(y = SKR_name, xmin = Lower_CI_95, xmax = Upper_CI_95), alpha = 0.4, colour = "blue") +  
  geom_linerange(aes(y = SKR_name, xmin = Lower_CI_t, xmax = Upper_CI_t), alpha = 0.6, colour = "red") +
  geom_point(aes(x = Estimates, y = rownames(plot_me$L.ecm), ))

png("p_Xspeed.png", width = 800, height = 558)
plot(p_Xspeed)
dev.off()


#short run price elasticity
plot_me = CI.table(Lag0X$Coefficients, t.value = 1.4)
ggplot(data = plot_me$L.D.real_gas_price.) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
  coord_cartesian(xlim=c(-1,01)) +
  labs(y = "SKR name", title = "Short run price elasticity with covariates. CI of 84%") +
  geom_linerange(aes(y = SKR_name, xmin = Lower_CI_95, xmax = Upper_CI_95), alpha = 0.4, colour = "red") +  
  geom_linerange(aes(y = SKR_name, xmin = Lower_CI_t, xmax = Upper_CI_t), alpha = 0.6, colour = "red") +
  geom_point(aes(x = Estimates, y = rownames(plot_me$L.ecm), ))

#short run income elasticity
plot_me = CI.table(Lag0X$Coefficients, t.value = 1.4)
ggplot(data = plot_me$L.D.real_income_mean.) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
  coord_cartesian(xlim=c(-1,01)) +
  labs(y = "SKR name", title = "Short run income elasticity with covariates. CI of 84%") +
  geom_linerange(aes(y = SKR_name, xmin = Lower_CI_95, xmax = Upper_CI_95), alpha = 0.4, colour = "red") +  
  geom_linerange(aes(y = SKR_name, xmin = Lower_CI_t, xmax = Upper_CI_t), alpha = 0.6, colour = "red") +
  geom_point(aes(x = Estimates, y = rownames(plot_me$L.ecm), ))

#short run income elasticity
plot_me = CI.table(Lag0X$Coefficients, t.value = 1.4)
ggplot(data = plot_me$L.D.real_income_mean.) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
  coord_cartesian(xlim=c(-1,01)) +
  labs(y = "SKR name", title = "Short run income elasticity with covariates. CI of 84%") +
  geom_linerange(aes(y = SKR_name, xmin = Lower_CI_95, xmax = Upper_CI_95), alpha = 0.4, colour = "green", position = position_dodge())+  
  geom_linerange(aes(y = SKR_name, xmin = Lower_CI_t, xmax = Upper_CI_t), alpha = 0.6, colour = "red") 

#export all tables with confidence intervals in the covariate case
plot_me = CI.table(Lag0X$Coefficients, t.value = 1.4)
write.table(plot_me$L.ecm, file = "Long ecm X.doc", sep = "\t")
write.table(plot_me$L.D.real_gas_price., file = "short price X.doc", sep = "\t")
write.table(plot_me$L.D.real_income_mean., file = "short income X.doc", sep = "\t")
write.table(plot_me$real_gas_price, file = "Long price X.doc", sep = "\t")
write.table(plot_me$real_income_mean, file = "Long income X.doc", sep = "\t")

#2 lag - lowest AIC mean and median
lagprice = 0:2
lagincome = 0:2
lagY = 1:2
lagX = 0:2
Lag2X = SKR.subset.regression(form, p.values = TRUE, std.errors = TRUE, cleantable = TRUE)
summary(Lag2X$InformationCriteria)
plot_me = CI.table(Lag2X$Coefficients, t.value = 1.4)

ggplot(data = plot_me$L.ecm) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
  coord_cartesian(xlim=c(-1,0.1)) +
  labs(y = "SKR name", title = "Short run income elasticity with no covariates. CI of 84%") +
  geom_linerange(aes(y = SKR_name, xmin = Lower_CI_95, xmax = Upper_CI_95), alpha = 0.4, colour = "red") +  
  geom_linerange(aes(y = SKR_name, xmin = Lower_CI_t, xmax = Upper_CI_t), alpha = 0.6, colour = "red") +
  geom_point(aes(x = Estimates, y = rownames(plot_me$L.ecm) ))

#rör ej, finns kopia i STATA
lagprice = 0:3
lagincome = 0:3
lagX = 0:1
model3 = plm(formula =  diff(log(gaspercapita)) ~ lag(log(gaspercapita)) + lag(log(real_gas_price)) + lag(log(real_income_mean)) + lag(diff(log(gaspercapita)), 1) + lag(diff(log(real_gas_price)),0) + lag(diff(log(real_income_mean)), 0), data = filter(ps_maindata, SKR_name == "Större stad"), model = "within", effect = "individual") 
summary(model3)



#Consumer surplus calculation----

CS.calculator = function(data, nominalVSpercent, change, summing = FALSE, descriptives = TRUE){
  #data has to be inputed as the output of CI.table
  #calcuates the change in consumer surplus as used by other agencies around the world
  #get the factual data 
  df = subset(ps_maindata, year == 2017)
  df = df[c("gaspercapita", "real_gas_price", "SKR_name", "Municipality_name", "real_income_mean")]
  if (nominalVSpercent == "percent"){
    p.change = change
    coef = data$real_gas_price[c("Estimates", "SKR_name")]
    df = merge(df,coef, by = "SKR_name")
    df$P0 = df$real_gas_price #retrieve factual current price
    df$P1 = df$P0*(1+p.change) #calculate price after price/tax increase
    df$Q0 = df$gaspercapita #retrieve factual current quantity
    df$Q1 = df$Q0 + df$Estimates*p.change*100 #calculate quantity after price change
    index <- df$Q1 < 0 #checks which Q1 that are negaitve. this can happen when the elasticity is very high
    df$Q1[index] <- 0 #replaces every negative Q1 with 0 bc logic. one cannot have negative consumption of a good, therefor it is replaced by values of 0 which is the lowest amount one can consume
    df$D.CS = 1/2*(df$Q0+df$Q1)*(df$P0 - df$P1) #calcualtes the change in CS
  }
  else if (nominalVSpercent =="nominal"){
    p.n.change = change
    coef = plot_me$real_gas_price[c("Estimates", "SKR_name")]
    df = merge(df,coef, by = "SKR_name")
    df$P0 = df$real_gas_price
    df$P1 = df$P0+p.n.change
    p.change = (p.n.change)/df$P0 #calcualtes the percentage change
    df$Q0 = df$gaspercapita
    df$Q1 = df$Q0 + df$Estimates*p.change*100
    index <- df$Q1 < 0 #checks which Q1 that are negaitve
    df$hej = df$Q1
    df$Q1[index] <- 0 #replaces every negative Q1 with 0 bc logic
    df$D.CS = 1/2*(df$Q0+df$Q1)*(df$P0 - df$P1)
    df$shareexp = df$D.CS/(df$real_income_mean*1000)
  }
  else{print('Input "nominal" or "percent" into "nominalVSpercent"')}
  if (summing == FALSE){
    t = summaryBy(D.CS ~ SKR_name, df, FUN = c(mean))
    colnames(t) <- c("SKR_name", "Mean")
    }
  else {
    t = summaryBy(D.CS ~ SKR_name, df, FUN = c(mean, min, max))
    colnames(t) <- c("SKR_name", "Mean", "Min","Max")
  }
  if (sum(index) > 0){
    print("positive values of CS detected. They have been replaced")
    print(sum(index))}
  t[-1] = round(t[-1])
  if (descriptives == TRUE){
    return(df)}
  return(t)
}
  

#small summary statistics
CS.nom = CS.calculator(plot_me, "nominal", 3, FALSE, FALSE)
CS.percent = CS.calculator(plot_me, "percent", 0.01, FALSE, FALSE)
#t = merge(j, h, by ="SKR_name")
t = cbind(CS.nom, CS.percent)
stargazer(t,  type ="html", out = "CS change 1 kr", summary = FALSE)


sverige_map = read_sf("kartor\\Kommungränser_SCB_07.shp")
#merging CS and map
sverige_map$Municipality_name = sverige_map$KNNAMN
CS = CS.calculator(plot_me, "nominal", 3, FALSE)
CS_map = merge(jjj, sverige_map, by = c("Municipality_name"), all.y = TRUE)
#plotting
p = ggplot(CS_map) +
  theme(legend.title = element_text(size = 20),
        legend.text = element_text(size = 15)) +
  geom_sf(data = CS_map$"geometry")+
  aes(fill = CS_map$D.CS) +
  labs(fill = "Change in \nConsumer surplus", size = 1)
p  

#plotting for colourblind
p_c = p + scale_fill_continuous(type = "viridis")
p_c

png("p_c.png", width = 800, height = 558)
plot(p_c)
dev.off()

#more plotting for colour blind
p_d = p + scale_fill_viridis_b()
p_d

png("p_d.png", width = 800, height = 558)
plot(p_d)
dev.off()