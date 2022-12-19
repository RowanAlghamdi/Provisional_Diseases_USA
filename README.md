---

# Statistical Report of COVID-19, Pneumonia, and Influenza Death Counts in the United States
The University of Sydney



# 1 Executive Summary

The aim of this report is to assess the data of COVID-19, Pneumonia, and
Influenza deaths counts in the United States across all ages and sexes
in order to evaluate the spread of COVID-19 compared to Pneumonia and
Influenza between males and females, and to investigate which illness,
out of COVID-19, Pneumonia, and Influenza has the greatest impact on
total deaths within the United States. The analyses include diagnoses of
the appropriateness of the statistical models used for the data.


The first discovery is that there is a greater discrepancy in death
counts for COVID-19 between males and females (across all ages) than for
Pneumonia and Influenza. Moreover, there was an approximately equal
range for males with COVID-19 and Pneumonia. There was a greater range
for males with Pneumonia and the greatest range was for females with
COVID-19. Influenza showed much lower results for both males and females
compared to COVID-19 and Pneumonia.

The second discovery is that COVID-19 showed the greatest impact on
total deaths within the US, closely followed by Pneumonia, with
Influenza showing a far less impact on total death counts. These results
have implications for the impact that an effective and wide-spread
vaccine can have on saving lives. 

# 2 Full Report

## 2.1 Initial Data Analysis (IDA)

### 2.1.1 Source of Data

```
data = read.csv("Provisional_COVID-19_Death_Counts_by_Sex__Age__and_State.csv",                 na.strings = c("0","-9"), header = TRUE) Table= read.csv("Table_of_variables.csv", header = TRUE)
```

### 2.1.2 Complexity of Data

```
head(data, n=3 )  #Returns the first three rows of the data 
dim(data)  #Returns the dimension of data as rows and columns 
class(data) #Represent the type or style of data 
names(data)  #Returns the names of columns or variables 
data$Data.as.of=as.factor(data$Data.as.of)  #Changing all the character arguments to factors 
data$Start.Date=as.factor(data$Start.Date)
data$End.Date=as.factor(data$End.Date)
data$State=as.factor(data$State)
data$Sex=as.factor(data$Sex)
data$Age.group=as.factor(data$Age.group)
str(data) #Retruns the internal structure of the data
```

### 2.1.3 Classification of Variables

``` 
library(formattable) #A table of the variables in the data with their description and classification 
formattable(Table, 
            align =c("l","l","c","c"), 
            list(`Indicator Name` = formatter(
            "span", style = ~ style(color = "grey",font.weight = "bold"))))
```

### 2.1.4 Summary

The data comes from the National Centre for Health Statistics (NCHS) in
the United States of America between 2 May 2020 and 13 January 2021. It
reports the number of deaths involving Coronavirus (COVID-19),
Pneumonia, and Influenza across age groups, sex and US states. Each row
represents a sum of death cases due to Coronavirus, Pneumonia, or
Influenza across certain age groups and sexes for each US state. There
are a total of 2661 observations in the data set collected, with 13
different variables, including states within the United States, allowing
for a data spread indicative of COVID-19 cases and their prevalence
across the country.

The data is not valid/is limited because:

-   Not all deaths are represented in the data. The death count numbers
    in the data are the deaths that have been received and coded during
    the data period. Between the time that death occurs and the time
    that a death certificate is submitted to the NCHS, there is a lag
    time that can range between 1-8 weeks, making the data set
    incomplete.

-   The data entries that had counts between 1-9 were suppressed to
    conform to NCHS confidentiality standards, making the data
    incomplete. It is unclear whether true values were included in the
    data pertaining to the entire country totals or not.

-   Adding data for total independent "COVID-19","Influenza", and
    "Pneumonia" deaths results in a value consistently greater than the
    value in "Pneumonia, Influenza, or COVID-19 Deaths". However, the
    difference is always equal to the columns "Pneumonia and COVID-19
    Deaths". This means the deaths due to both Pneumonia and COVID were
    also counted in EITHER "COVID-19 Deaths" OR "Pneumonia Deaths," but
    it has not been specified which one. Due to this reason, we are
    omitting the column of "Pneumonia and COVID-19 Deaths" from the data
    analysis.



## 2.2 Research Question 1

How does the spread of COVID-19 death counts compare to Pneumonia and
Influenza death counts in the United States? How does this compare
between females and males?

### 2.2.1 Graphical summary

```
covidUS <- data[c("State","Sex","Age.group","COVID.19.Deaths","Total.Deaths", "Pneumonia.Deaths","Influenza.Deaths")] # Creating subsets of the data 
covidUS <- covidUS[!(covidUS$State!="United States"),]
covidUS <- covidUS[!(covidUS$Sex=="Unknown"),]
covidUS <- covidUS[!(covidUS$Age.group=="All Ages"),]
covidUS <- covidUS[!(covidUS$Age.group=="0-17 years"),]
covidUS <- covidUS[!(covidUS$Age.group=="18-29 years"),]
covidUS <- covidUS[!(covidUS$Age.group=="30-49 years"),]
covidUS <- covidUS[!(covidUS$Age.group=="50-64 years"),]
genders = c("Male","Female") 

Fe_COVID = covidUS$COVID.19.Deaths[covidUS$Sex =="Female"]  # Deaths due to COVID-19
Ma_COVID = covidUS$COVID.19.Deaths[covidUS$Sex =="Male"]
Fe_Pneumonia = covidUS$Pneumonia.Deaths[covidUS$Sex =="Female"]  # Deaths due to Pneumonia
Ma_Pneumonia = covidUS$Pneumonia.Deaths[covidUS$Sex =="Male"]
Fe_Influenza = covidUS$Influenza.Deaths[covidUS$Sex =="Female"]  # Deaths due to Influenza
Ma_Influenza = covidUS$Influenza.Deaths[covidUS$Sex =="Male"]
Fe_COVID
#Box plot comparing  deaths counts due to COVID-19 between Female and Male 
boxplot(Ma_COVID,Fe_COVID,names=genders,horizontal=TRUE,
        xlab ="Death Counts",col=c("#497AF4","#FABCEE"),main="Deaths due to COVID-19 for all ages by gender")
#Box plot comparing  deaths counts due to Pneumonia between Female and Male 
boxplot(Ma_Pneumonia,Fe_Pneumonia,names=genders,horizontal=TRUE,
        xlab ="Death Counts",col=c("#497AF4","#FABCEE"),main="Deaths due to Pneumonia for all ages by gender")
#Box plot comparing  deaths counts due to Influenza between Female and Male 
boxplot(Ma_Influenza,Fe_Influenza,names=genders,horizontal=TRUE,
        xlab ="Death Counts",col=c("#497AF4","#FABCEE"),main="Deaths due to Influenza for all ages by gender")
```
![](/images/image1.png)
![](/images/image2.png)
![](/images/image3.png)


### 2.2.2 Numerical summary

``` 
summary(covidUS) 
#COVID-19 Female median
median(Fe_COVID)
#COVID-19 Female IQR
IQR(Fe_COVID)
#COVID-19 Male median
median(Ma_COVID)
#COVID-19 Male IQR
IQR(Ma_COVID) 
#Pneumonia Female median 
median(Fe_Pneumonia) 
#Pneumonia Female IQR
IQR(Fe_Pneumonia) 
#Pneumonia Male median 
median(Ma_Pneumonia) 
#Pneumonia Male IQR
IQR(Ma_Pneumonia) 
#Influenza Female Median
median(Fe_Influenza)
#Influenza Female IQR
IQR(Fe_Influenza) 
#Influenza Male median
median(Ma_Influenza)
#Influenza Male IQR
IQR(Ma_Influenza) 

##The median is the second 2nd quartile
#The lower thresholds : LT =  Q_1 − 1.5 IQR
LT_Fe_COVID =  111.5 - 1.5*(IQR(Fe_COVID))  # Deaths due to COVID-19
LT_Ma_COVID =  171  - 1.5*(IQR(Ma_COVID))
LT_Fe_Pneumonia =  176 - 1.5*(IQR(Fe_Pneumonia ))  # Deaths due to Pneumonia
LT_MA_Pneumonia =  268.5 - 1.5*(IQR(Ma_Pneumonia ))
LT_Fe_Influenza =  40.5 - 1.5*(IQR(Fe_Influenza))  # Deaths due to Influenza
LT_MA_Influenza =  37.5 - 1.5*(IQR(Ma_Influenza))
#The upper thresholds : UT = Q_3 + 1.5 IQR
UT_Fe_COVID =  20581.5 + 1.5*(IQR(Fe_COVID))  # Deaths due to COVID-19
UT_Ma_COVID =  33948 + 1.5*(IQR(Ma_COVID))
UT_Fe_Pneumonia =  22150 + 1.5*(IQR(Fe_Pneumonia ))  # Deaths due to Pneumonia
UT_MA_Pneumonia =  33854.0 + 1.5*(IQR(Ma_Pneumonia ))
UT_Fe_Influenza =  754.5 + 1.5*(IQR(Fe_Influenza))  # Deaths due to Influenza
UT_MA_Influenza =  821.5 + 1.5*(IQR(Ma_Influenza))
```

### 2.2.3 Analysis

Each graph indicates the death counts for COVID-19, Pneumonia or
Influenza within various age groups for both males and females within
the United States, including: `<br>`{=html}

Under 1 year 
1-4 years 
5-14 years
15-24 years 
25-34 years 
35-44 years 
45-54 years
55-64 years 
65-74 years 
75-84 years 
85 years and over


While some age groups include a more comprehensive age range, this
remains consistent for both males and females, so the respective
histograms are still comparable. Other data issues include:

-   There might be more men who contract a respective disease than
    women, or vice versa. Therefore, the graphs are not as reflective of
    which sex is more susceptible to die from a disease as it is
    representative of which sex is more likely to contract then die from
    the disease. There could also be unequal proportions of males and
    females within a population.

-   The histograms do not indicate which age groups fall under which
    quartile of data but instead reflects the susceptibility of the sex
    as a whole for all ages.



#### 2.2.3.1  Data analysis

Median and interquartile range (IQR) were used in place of mean and
standard deviation (SD), as our data is right-skewed.



**---COVID-19---** 
Female median: 1903 
Male median: 3839 

Female IQR: 20470 
Male IQR: 33777 

The lower female median indicates an overall lower death rate for
females than males for COVID-19, as 50% of age groups have a death count
lower than 1903. 

The middle 50% (Q2 and Q3) of values is much more spread for males,
displayed by the wider box length within the male box plot and higher
IQR value. 

For both boxes, the lowest two quartiles indicated the densest data,
showing that the COVID-19 death count is relatively low for the majority
of age groups.

The range of deaths for females (\~60,000) is higher than males
(\~50,000), thanks to an outlier, possibly showing that older females
are more likely to contract/die from COVID-19 than males. However, this
is confounded by the higher life expectancy of females.



**---PNEUMONIA---** 
Female median: 2144 
Male median: 3520 

Female IQR: 21973.5 
Male IQR: 33585.5 

Pneumonia has a lower female median than male median, similar to
COVID-19's statistical implications. This is also true for the IQR.
However, as shown on the box plots comparison, the total range for both
males and females is \~50,000. Assuming this death count belongs to an
older population (shown in the data), it can be deduced that for older
age groups, the Pneumonia death counts between males and females are
very similar, despite the lower median in females.



**---INFLUENZA---**
Female median: 184 
Male median: 3520 

Female IQR: 714 
Male IQR: 784 

The medians and IQR for males and females are almost equal with similar
ranges. This could be attributed to the prevalence of the influenza
vaccine, which subdues variations caused by age, gender and other
factors, also explaining the low death count of \~1000, compared to
+50,000 for COVID-19 and Pneumonia.



### 2.2.4 Summary:

COVID-19 indicates a greater discrepancy in death counts across ages
between males and females than Pneumonia and Influenza within the US,
with Influenza showing the lowest discrepancy. 

COVID-19 and Pneumonia reflected an approximately equal but greater
range for males in Pneumonia; however, COVID-19 reflected the greatest
range (60,000) for females. 

Influenza had much lower results for both sexes, which can be attributed
to the presence of an effective vaccine.

The Influenza data can be used as a model for COVID-19 to demonstrate
the effect a widely implemented vaccine has on a society.



## 2.3 Research Question 2

Which illness, out of COVID-19, Pneumonia, and Influenza, has the
highest impact on total death counts in the US across all ages and
sexes?

``` 
#Remove scientific notation format
options(scipen=999)

#Variables
##Deaths due to COVID-19 (all ages & sexes)
COVID_19 = covidUS$COVID.19.Deaths

##Deaths due to Pneumonia (all ages & sexes)
Pneumonia = covidUS$Pneumonia.Deaths 

##Deaths due to Influenza (all ages & sexes)
Influenza = covidUS$Influenza.Deaths 

##Total Deaths (all ages & sexes)
Total_Deaths = covidUS$Total.Deaths 
```

### 2.3.1 Correlation Coefficients

``` 
#Total Deaths vs COVID-19 
cor(COVID_19,Total_Deaths) 

#Total Deaths vs Pneumonia
cor(Pneumonia,Total_Deaths)

#Total Deaths vs Influenza
cor(Influenza,Total_Deaths)
```



### 2.3.2 Regression Line

``` 
#A regression line on the scatter plot for Total Deaths vs COVID 19
L_COVID_19 = lm(Total_Deaths~COVID_19) 
plot(COVID_19,Total_Deaths,
     main = "Total Deaths vs COVID-19 Deaths",
     xlab = "COVID-19 Deaths",
     ylab = "Total Deaths")
abline(L_COVID_19,col="blue")
legend("topright",c("Regression line"),col ="blue",lty=1:2,cex=0.8)

##Slope and Intercept
L_COVID_19$coeff

#A regression line on the scatter plot for Total Deaths vs Pneumonia
L_Pneumonia = lm(Total_Deaths~Pneumonia) 
plot(Pneumonia,Total_Deaths,
     main = "Total Deaths vs Pneumonia Deaths",
     xlab = "Pneumonia Deaths",
     ylab = "Total Deaths")
abline(L_Pneumonia,col="red")
legend("topright",c("Regression line"),col="red",lty=1:2,cex=0.8)

##Slope and Intercept
L_Pneumonia$coeff

#A regression line on the scatter plot for Total Deaths vs Influenza
L_Influenza = lm(Total_Deaths~Influenza)  
plot(Influenza,Total_Deaths,
     main = "Total Deaths vs Influenza Deaths",
     xlab = "Influenza Deaths",
     ylab = "Total Deaths")
abline(L_Influenza,col="green")
legend("topright", c("Regression line"),col="green",lty=1:2,cex=0.8)

##Slope and Intercept
L_Influenza$coeff
```
![](/images/image4.png)
![](/images/image5.png)
![](/images/image6.png)



### 2.3.3 Residuals

``` 
#Residual plot for Total Deaths vs COVID 19
plot(COVID_19,L_COVID_19$residuals,xlab="COVID-19 Deaths",ylab ="Residuals", main="Residual Plot of Total Deaths vs COVID-19 Deaths")
abline(h=0,col="blue")

#Residual plot for Total Deaths vs Pneumonia
plot(Pneumonia,L_Pneumonia$residuals,xlab="Pneumonia Deaths",ylab="Residuals",main="Residual Plot of Total Deaths vs Pneumonia Deaths")
abline(h=0,col="red")

#Residual plot for Total Deaths vs Influenza
plot(Influenza,L_Influenza$residuals,xlab="Influenza Deaths",ylab ="Residuals",main="Residual Plot of Total Deaths vs Influenza Deaths")
abline(h= 0,col="green")
```

![](/images/image7.png)
![](/images/image8.png)
![](/images/image9.png)



### 2.3.4 Vertical Strips

``` 
##A regression line on the scatter plot for Total Deaths vs COVID 19
L_COVID_19 = lm(Total_Deaths~COVID_19) 
plot(COVID_19,Total_Deaths,
     main = "Total Deaths vs COVID-19 Deaths",
     xlab = "COVID-19 Deaths",
     ylab = "Total Deaths")
abline(L_COVID_19,col="blue")
legend("topright",c("Regression line"),col ="blue",lty=1:2,cex=0.8)

##Vertical Strips
abline(v=c(0,10000,20000,30000,40000,50000,60000,70000,80000), col=c("pink")) 

#A regression line on the scatter plot for Total Deaths vs Pneumonia
L_Pneumonia = lm(Total_Deaths~Pneumonia) 
plot(Pneumonia,Total_Deaths,
     main = "Total Deaths vs Pneumonia Deaths",
     xlab = "Pneumonia Deaths",
     ylab = "Total Deaths")
abline(lm(Total_Deaths~Pneumonia),col="red")
legend("topright",c("Regression line"),col="red",lty=1:2,cex=0.8)

##Vertical Strips
abline(v=c(0,10000,20000,30000,40000,50000,60000,70000), col=c("pink")) 


#A regression line on the scatter plot for Total Deaths vs Influenza
L_Influenza = lm(Total_Deaths~Influenza)  
plot(Influenza,Total_Deaths,
     main = "Total Deaths vs Influenza Deaths",
     xlab = "Influenza Deaths",
     ylab = "Total Deaths")
abline(lm(Total_Deaths~Influenza),col="green")
legend("topright", c("Regression line"),col="green",lty=1:2,cex=0.8)

##Vertical Strips
abline(v=c(0,250,500,750,1000,1250), col=c("pink")) 
```

![](/images/image10.png)
![](/images/image11.png)
![](/images/image12.png)



### 2.3.5 Analysis

#### 2.3.5.1 Correlation:

The correlation coefficient of 1 for Total Deaths vs COVID-19 Deaths, 1
for Total Deaths vs Pneumonia Deaths, and 0.9 for Total Deaths vs
Influenza Deaths indicate strong positive relationships.



#### 2.3.5.2 Regression Line:

-   y = 19764.608100 + 8.862942x indicates if COVID-19 Deaths increase
    by 1 death, Total Deaths increase by 8.86 deaths (2dp).
-   y = 16069.767672 + 9.345382x indicates if Pneumonia Deaths increase
    by 1 death, Total Deaths increase by 9.35 deaths (2dp).
-   y = -18913.1180 + 409.3284x indicates if Influenza deaths increase
    by 1 death, Total Deaths increase by 409.33 deaths (2dp).



#### 2.3.5.3 Scatter Plot:

The scatter plot shapes for all three pairs display strong positive
relationships between the two variables in each pair.



#### 2.3.5.4 Residual Plot:

The residual plots for all three relationships display a random spread
of values about y=0. The scatter plot shapes and the absence of a
pattern in residuals indicates that a linear model is an appropriate fit
for the data.



#### 2.3.5.5 Vertical Strips:

The unequal spread of points across the vertical strips on the scatter
plots and the fanning out of points on the residual plots reveal that
the data is heteroscedastic for all three relationships, indicating that
the regression lines should not be used for predictions.


### 2.3.6 Summary

The strong positive linear relationships between Total Deaths vs
COVID-19, Pneumonia, and Influenza Deaths can be attributed to the fact
that COVID-19, Pneumonia, and Influenza death count numbers contribute
to Total Death count numbers -- as the illness death counts increase,
the total death counts increase.

The heteroscedasticity of the plots indicate that it is inappropriate to
use these relationships to make predictions about the illnesses impact
on the rate of total deaths as using extrapolation to make predictions
for values that are not within our range of data can be unreliable as it
is impossible to know whether the long-term trend will remain linear.

The slope and intercepts of the regression lines reveal that COVID-19
and Pneumonia have a similar impact on the total death count, which can
be attributed to the fact that both illnesses have similar symptoms and
complications, and Pneumonia can be a complication of COVID-19.

COVID-19 showed the highest impact on total deaths (for every COVID-19
death, there is 8.86 other deaths) closely followed by Pneumonia (for
every Pneumonia death, there is 9.35 other deaths). Influenza showed the
lowest impact (for every Influenza death, there is 409.33 other deaths),
which can be attributed to the effective employment of the Influenza
vaccine.



# 3 Contribution Statement

**490185362:**  IDA variables, Research Question 1, Research Question 2 

**500483091:** IDA variables, IDA source, Research Question 1

**450192993:** Executive summary, IDA source, Research Question 2

# 4 References

National Centre for Health Statistics. (2021). Provisional COVID-19
Deaths Counts by Sex, Age, and State \[Dataset\].
https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-by-Sex-Age-and-S/9bhg-hcku


