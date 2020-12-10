library(readxl)
library(ggplot2)
library(reshape2)
library(usmap)
library(stringr)

#path to data
excelPortfolioByDebtPath <- "C:\\Users\\rache\\Desktop\\LCSC\\CS445\\Data_Project\\Portfolio-by-Location-by-Debt-Size.xls"
excelTrendsCollegePath <- "C:\\Users\\rache\\Desktop\\LCSC\\CS445\\Data_Project\\trends-college-pricing-excel-data-2020.xlsx"

#Import data sets
Portfolio_by_Location_by_Debt_Size <- read_excel(excelPortfolioByDebtPath, na = 'N/A', skip = 6)
trends_college_pricing_excel_data_2020 <- read_excel(excelTrendsCollegePath, na = 'N/A',
                                                     sheet = "Table CP-5", col_types = c("text",
                                                                                         "numeric", "numeric", "numeric",
                                                                                         "numeric", "numeric", "numeric",
                                                                                         "numeric", "numeric", "numeric",
                                                                                         "numeric", "numeric", "numeric",
                                                                                         "numeric", "numeric", "numeric",
                                                                                         "numeric", "numeric", "numeric",
                                                                                         "numeric", "skip", "numeric", "numeric",
                                                                                         "numeric", "numeric", "numeric", "numeric", "numeric",
                                                                                         "numeric", "numeric", "numeric", "numeric", "numeric",
                                                                                         "numeric", "numeric", "numeric", "numeric", "numeric",
                                                                                         
                                                                                                                                                                                 "numeric", "numeric", "skip"), skip = 2)

#
#Debt Portfolio Data
#
#Separate the debt portfolio into Dollars and Borrowers
Dollars_Outstanding <- seq(2, 18, 2)
Borrows <- seq(3, 19, 2)


#Create new data frames to visual debt on its own and borrowers on its own
State_Outstanding_by_Category <- Portfolio_by_Location_by_Debt_Size[][Dollars_Outstanding]
States_Total_Outstanding <- rowSums(State_Outstanding_by_Category)
States_Borrowers_by_Category <- Portfolio_by_Location_by_Debt_Size[][Borrows]
States_Total_Borrowers <- rowSums(States_Borrowers_by_Category)

perPersonDebt_by_State <- States_Total_Outstanding/States_Total_Borrowers #gives us billions per thousand
perPersonDebt_by_State <- perPersonDebt_by_State/1000 #gives us billions per person
perPersonDebt_by_State <- round(perPersonDebt_by_State*(10^6), digits = 2) #gives us thousands per person

category_names <- c("Under 5k", "5k to 10k", "10k to 20k", "20k to 40k", "40k to 60k", "60k to 80k", "80k to 100k", "100k to 200k", "Over 200k")

#change the row names to States, and the column names to the categories
colnames(State_Outstanding_by_Category) <- category_names
colnames(States_Borrowers_by_Category) <- category_names
rownames(State_Outstanding_by_Category) <- Portfolio_by_Location_by_Debt_Size$Location
rownames(States_Borrowers_by_Category) <- Portfolio_by_Location_by_Debt_Size$Location

#Bar Graph of Total Debts
p <- ggplot(State_Outstanding_by_Category, aes(x=States_Total_Outstanding, y=Portfolio_by_Location_by_Debt_Size$Location, fill=States_Total_Outstanding))
p + geom_bar(stat = "unique", color="blue", fill=rgb(0.1,0.4,0.5,0.7)) + labs(title = "Total Debt per State", x = "Total Debt(in billions)", y = "States", fill = "Debt" )

#Bar Graph of Total Borrowers
b <- ggplot(States_Borrowers_by_Category, aes(x=States_Total_Borrowers, y=Portfolio_by_Location_by_Debt_Size$Location, fill=States_Total_Borrowers))
b + geom_bar(stat = "unique", color = "red4", fill = "red") + labs(title = "Total People who Borrowed by State", x = "Total Borrowers(in thousands)", y = "States", fill = "Borrowers" ) 

#Bar Graph Debt per Person
debtPerPerson_Frame <- data.frame(
  debt = perPersonDebt_by_State, state = Portfolio_by_Location_by_Debt_Size$Location
)

ggplot(debtPerPerson_Frame, aes(x= debt, y = state, fill = state)) +
  geom_bar(stat = "identity") + labs(title = "Debt Per Person Per State", x = "Debt(in thousands)", y = "State") +
  scale_fill_hue(h = c(0,90)) +
  theme(legend.position = "none")

#Stacked Plot, Borrowers
States_Borrowers_by_Category.n <- States_Borrowers_by_Category
States_Borrowers_by_Category.n$names <- Portfolio_by_Location_by_Debt_Size$Location
State_Borrowers_by_Category.m <- melt(States_Borrowers_by_Category.n)
ggplot(State_Borrowers_by_Category.m, aes(fill=variable, y=names, x=value)) +
  geom_bar(stat='identity') + labs(title = "Borrowers by State and Range", x = "Borrowers", y = "State", fill = "Range")

#Stacked Plot, Debt
State_Outstanding_by_Category.n <- State_Outstanding_by_Category
State_Outstanding_by_Category.n$names <- Portfolio_by_Location_by_Debt_Size$Location
State_Outstanding_by_Category.m <- melt(State_Outstanding_by_Category.n)
ggplot(State_Outstanding_by_Category.m, aes(fill=variable, y=names, x=value)) +
  geom_bar(stat='identity') + labs(title = "Debt by State and Range", x = "Debt(in billions)", y = "State", fill = "Range")

#
# Price of College Data 
#
#Get the data we want to look at from trends_college_pricing, includes only four year colleges in 2020 dollars
columns <- c(1, seq(21, 37, 1))
rows <- 1:52

College_Price_Useful <- trends_college_pricing_excel_data_2020[rows, columns]

#change rownames to states
College_Price_Useful$`In 2020 Dollars` <- NULL
rownames(College_Price_Useful) <- trends_college_pricing_excel_data_2020[rows,columns]$`In 2020 Dollars`
College_Price_Useful$states = trends_college_pricing_excel_data_2020[rows,columns]$`In 2020 Dollars`
College_Price_Useful$fips = fips(trends_college_pricing_excel_data_2020[rows,columns]$`In 2020 Dollars`)

#College cost heat map
collegeCostMap <- plot_usmap(data = College_Price_Useful, values = "2020-21...37")
collegeCostMap + scale_fill_continuous(
  low = "cornsilk", high = "red", name = "College average Tuition Cost", label = scales::comma
)

#Scatter Plot Comparing Debt Per person and Cost of College
perPersonDebt_by_State1 <- perPersonDebt_by_State[-54]
perPersonDebt_by_State2 <- perPersonDebt_by_State1[-53]
costsVsDebt <- data.frame(
  cp = cost_College_2020, dpp = perPersonDebt_by_State2
) 
ggplot(costsVsDebt, aes(x = cp, y = dpp )) + geom_point() + labs(title = "Debt Vs Cost of College", x = "Debt per Person (in thousands)", y = "Cost of College (in thousands)")


#
#Stats Needed
#
#total debt in the US
paste("The total amount of student debt in the US is:", round(sum(States_Total_Outstanding), 2), "billion dollars")

#total borrowers in the US
paste("Currently", sum(States_Total_Borrowers), "thousand people are borrowing money.")

#average debt per state in the US
paste("The average student debt per state in the US is:", round(mean(States_Total_Outstanding), 2), "billion dollars")

#average debt per person in the US
paste("The average student debt per person in the US is:", round(mean(perPersonDebt_by_State), 2), "thousand dollars")
  
#average cost of college in the US
cost_College_2020 <- College_Price_Useful$`2020-21...37`
paste("The average cost of college in the US is:", round(mean(cost_College_2020), 2), "thousand dollars")
paste("The average cost of four years of college in the US is:", round(4*mean(cost_College_2020), 2), "thousand dollars")