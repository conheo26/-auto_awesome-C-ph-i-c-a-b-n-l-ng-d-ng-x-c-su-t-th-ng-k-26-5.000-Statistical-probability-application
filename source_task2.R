library(readr)
library(dplyr)
library(ggplot2)
library(hrbrthemes)

rm(list = ls()) #Clear environments

dataset <- read.csv("e-commerce.csv")
head(dataset, 3) #Get first three lines of the data set

########################################################
# NA values

apply(is.na(dataset), 2, sum)

########################################################
# Warehouse_block + pie chart

ware <- as.data.frame(table(dataset$Warehouse_block))
colnames(ware) <- c("Warehouse block", "Customers count")

ware$Percentage <- round(ware$"Customers count" / sum(ware$"Customers count") * 100, digits = 2) #Add another column for percentages
ware <- ware %>%
  arrange(desc(`Warehouse block`)) %>%
  mutate(ypos = cumsum(Percentage) - 0.5*Percentage) #Set label position

mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF", "#ABCCAAFF") #Define set of colors

pie_ware <- ggplot(ware, aes(x = "", y = Percentage, fill = `Warehouse block`)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = ypos, label = Percentage), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void() #Draw pie chart

pie_ware

########################################################
# Shipment

shipment <- as.data.frame(table(dataset$Mode_of_Shipment))
colnames(shipment) <- c("Mode of Shipment", "Customers count")

shipment$Percentage <- round(shipment$"Customers count" / sum(shipment$"Customers count") * 100, digits = 2) #Add another column for percentages
shipment <- shipment %>%
  arrange(desc(`Mode of Shipment`)) %>%
  mutate(ypos = cumsum(Percentage) - 0.5*Percentage) #Set label position

mycols2 <- c("#0073C2FF", "#868686FF", "#CD534CFF") #Define set of colors

pie_shipment <- ggplot(shipment, aes(x = "", y = Percentage, fill = `Mode of Shipment`)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = ypos, label = Percentage), color = "white")+
  scale_fill_manual(values = mycols2) +
  theme_void() #Draw pie chart

pie_shipment

########################################################
#Reached on time

time <- as.data.frame(table(dataset$Reached.on.Time_Y.N))
colnames(time) <- c("Reached on time? (0) On time, (1) Not on time", "Customers count")

bar_time <- ggplot(time, aes(x=`Reached on time? (0) On time, (1) Not on time`, y=`Customers count`, fill = `Reached on time? (0) On time, (1) Not on time`)) +
  geom_bar(stat = "identity", width = 0.2) +
  theme(legend.position="none")

bar_time

########################################################
# Calls for each warehouses

total_calls <- sum(dataset$Customer_care_calls)

calls <- dataset %>%
  group_by(Warehouse_block) %>%
  summarise(
    "Customer Care Calls count" = sum(Customer_care_calls),
    "Average" = mean(Customer_care_calls),
    "Minimum" = min(Customer_care_calls),
    "Maximum" = max(Customer_care_calls)
  ) # Calls info for each Warehouse

boxplot(Customer_care_calls ~ Warehouse_block , xlab = "Warehouse block" , ylab = "Customer care calls" , main = "
Boxplot of Customer care calls for each Warehouse block" , data = dataset , col = 2:7) #Box plot for calls

########################################################
# Ratings

rating <- table(dataset$Customer_rating,dataset$Warehouse_block) #Ratings for each warehouse block
rating

barplot(rating, main = "Ratings for each Warehouse block", 
        xlab ="Warehouse blocks", ylab = "Ratings count", ylim = c(0, 5000))

########################################################
# Costs

costs <- dataset %>%
      summarise(
        "Average" = mean(Cost_of_the_Product),
        "Minimum" = min(Cost_of_the_Product),
        "Maximum" = max(Cost_of_the_Product),
        "Q1" = quantile(Cost_of_the_Product, 0.25),
        "Q2" = median(Cost_of_the_Product),
        "Q3" = quantile(Cost_of_the_Product, 0.75)
      ) # Costs summary

costs

dataset %>%
  ggplot( aes(x=Cost_of_the_Product)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Product Costs distribution") +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("(Product Cost)") +
  ylab("(Density)") +
  scale_x_continuous(limits = c(50, 350), breaks = seq(50, 350, by = 50))
  #Density chart for costs distribution


boxplot(Cost_of_the_Product ~ Warehouse_block, xlab = "Warehouse block", ylab = "Costs" , main = "
Boxplot of Product Costs for each Warehouse block", data = dataset, col = 2:7, ylim = c(50, 350)) #Box plot for costs

########################################################
# Weight

weight <- dataset %>%
  summarise(
    "Average" = mean(Weight_in_gms),
    "Minimum" = min(Weight_in_gms),
    "Maximum" = max(Weight_in_gms),
    "Q1" = quantile(Weight_in_gms, 0.25),
    "Q2" = median(Weight_in_gms),
    "Q3" = quantile(Weight_in_gms, 0.75)
  ) # Weight summary

weight

dataset %>%
  ggplot( aes(x=Weight_in_gms)) +
  geom_density(fill="#0073C2FF", color="#e9ecef", alpha=0.8) +
  ggtitle("Product Weight distribution") +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("(Weight)") +
  ylab("(Density)") +
  scale_x_continuous(limits = c(500, 8000), breaks = seq(500, 8000, by = 500))
  #Density chart for weight distribution

boxplot(Weight_in_gms ~ Warehouse_block, xlab = "Warehouse block", ylab = "Weight" , main = "
Boxplot of Product Weight for each Warehouse block", data = dataset, col = 2:7, ylim = c(500, 8000)) #Box plot for costs

########################################################
#Linear Regression

#Xay dung mo hinh 1

lm_1 = lm(Cost_of_the_Product ~ Customer_care_calls + Customer_rating + Prior_purchases + Discount_offered + Weight_in_gms + Reached.on.Time_Y.N, data = dataset)
summary(lm_1)

#Xay dung mo hinh 2

lm_2 = lm(Cost_of_the_Product ~ Customer_care_calls + Prior_purchases + Discount_offered + Weight_in_gms + Reached.on.Time_Y.N, data = dataset)
summary(lm_2)

#So sanh 2 mo hinh

anova(lm_1,lm_2)

#Do thi phan tich gia tri thang du

par(mfrow = c(2,2))
plot(lm_2)