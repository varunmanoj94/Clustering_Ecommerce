library(tidyverse)
library(readxl)
library(lubridate)
library(cluster)
library(data.table)
library(rstan)
library(rlang)
library(ggplot2)
library(ggthemes)
library(factoextra)
library(gtable)
library(kableExtra)
library(patchwork)

#Open and begin exploring excel file
retail <- read_xlsx("Online Retail.xlsx")

glimpse(retail)

#Check for irregularities
summary(retail) %>%
  kable()

class(retail$InvoiceDate)

#We can see that Quantity has at least one negative value, we can explore this more in depth
#It's also worth pointing out that InvoiceNo is a chr (we should convert this to int) and InvoiceDate is dttm

retail %>%
  summarise(sum(Quantity < 0))

retail %>%
  summarise(sum(is.na(Description)))

retail %>%
  summarise(sum(is.na(CustomerID)))

retail %>%
  summarise(sum(is.na(StockCode)))

retail%>%
  filter(!is.na(CustomerID),Quantity > 0, UnitPrice > 0)

#We can see that there's over 10,000 negative values for quantity; let's explore these records
neg_quantities <- retail %>%
  filter(Quantity < 0)

countries <- retail %>%
  group_by(Country) %>%
  summarise(n=n())

#Create column for invoice total; invoice quantiti
new_cols <- retail %>%
  group_by(InvoiceNo) %>%
  mutate(ItemTotal = Quantity * UnitPrice, InvoiceTotal = sum(ItemTotal), InvoiceQuant = sum(Quantity),
         Month = month(InvoiceDate), Hour = hour(InvoiceDate)) %>%
  ungroup()

#We need to make a decision on what to do with the missing data...if we're looking to find clusters of countries does CustomerID really matter?


#We can look at time trends by decomposing the invoice date column
#This lets us identify clusters (if any) of seasonal products
#It also lets us identify whether certain times of the day are popular acorss different countries
date <- retail %>%
  mutate(Month = month(InvoiceDate))

dates <- retail %>%
  mutate(Month = month(InvoiceDate), Hour = hour(InvoiceDate))

#We can sneak peak the differences across countries
retail %>%
  group_by(Country, InvoiceNo)

retail %>%
  filter(grepl("C",InvoiceNo))

demo$StockCode <- factor(demo$StockCode, ordered = FALSE)
str(check)

#Clustering like the textbook
#Leave this for now
check <- retail
check$StockCode <- factor(check$StockCode, ordered = FALSE)
check$CustomerID <- factor(check$CustomerID, ordered = FALSE)
check$Country <- factor(check$Country, ordered = FALSE)
check$Month <- factor(check$Month, ordered = FALSE)
check$Hour <- factor(check$Hour, ordered = FALSE)

seg.df <- check %>%
  select(-Description) %>%
  drop_na(CustomerID) %>%
  filter(Quantity > 0, !is.na(StockCode)) %>%
  ungroup() %>%
  mutate(InvoiceNo = factor(InvoiceNo, ordered = FALSE), InvoiceDate = as.Date(ymd_hms(InvoiceDate)))

##Clustering for RFM
reference_date <- as.Date(ymd_hms(max(retail$InvoiceDate) + days(1)))

receny <- seg.df %>%
  select(InvoiceDate, CustomerID) %>%
  mutate(Days_Since_Invoice = reference_date - InvoiceDate) %>%
  group_by(CustomerID) %>%
  summarise(Most_Recent_Purchase  = as.numeric(min(Days_Since_Invoice)))

summary(receny)

ggplot(receny, aes(Most_Recent_Purchase)) +
  geom_histogram(colour = "darkred", fill = "darkred") + theme(
    panel.grid.minor = element_blank(),
    panel.grid.major =  element_line(color = "#d0d0d0"),
    panel.background = element_rect(fill = "#f0f0f0", color = NA),
    plot.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.border = element_blank(),
    text = element_text(family = "Helvetica"),
    plot.title = element_text(face = "bold", size = rel(1.33))
  ) + labs(x = "Most Recent Purchase (Days)", y = "Frequency",
           title = "Recency Distribution")

ggsave("Recency_Distribution.png", height = 5, width = 5, dpi = 500)
  
##Frequency
freq <- seg.df %>%
  select(CustomerID, InvoiceNo) %>%
  group_by(CustomerID, InvoiceNo) %>%
  count() %>%
  group_by(CustomerID) %>%
  count() %>%
  ungroup()

summary(freq)

ggplot(freq, aes(n)) +
  geom_histogram(colour = "darkblue", fill = "darkblue") + theme(
    panel.grid.minor = element_blank(),
    panel.grid.major =  element_line(color = "#d0d0d0"),
    panel.background = element_rect(fill = "#f0f0f0", color = NA),
    plot.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.border = element_blank(),
    text = element_text(family = "Helvetica"),
    plot.title = element_text(face = "bold", size = rel(1.33))
  ) + labs(x = "Number of Purchases", y = "Frequency",
           title = "Frequency Distribution")

ggsave("Frequency_Distribution.png", height = 5, width = 5, dpi = 500)

##Monetary
monetary <- seg.df %>%
  select(CustomerID, InvoiceNo, UnitPrice) %>%
  group_by(CustomerID, InvoiceNo) %>%
  mutate(Invoice_Total = sum(UnitPrice)) %>%
  group_by(CustomerID) %>%
  summarise(Average_Invoice = mean(Invoice_Total)) %>%
  ungroup()

summary(monetary)

ggplot(monetary, aes(Average_Invoice)) +
  geom_histogram(colour = "darkgreen", fill = "darkgreen") + theme(
    panel.grid.minor = element_blank(),
    panel.grid.major =  element_line(color = "#d0d0d0"),
    panel.background = element_rect(fill = "#f0f0f0", color = NA),
    plot.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.border = element_blank(),
    text = element_text(family = "Helvetica"),
    plot.title = element_text(face = "bold", size = rel(1.33))
  ) + labs(x = "Cumulative Monetary Value of Orders", y = "Frequency",
           title = "Monetary Distribution")

ggsave("Monetary_Distribution.png", height = 5, width = 5, dpi = 500)

##QQ Plots for RFM
QQ_R <- ggplot(receny, aes(sample = Most_Recent_Purchase)) +
  stat_qq() + stat_qq_line() + theme(
    panel.grid.minor = element_blank(),
    panel.grid.major =  element_line(color = "#d0d0d0"),
    panel.background = element_rect(fill = "#f0f0f0", color = NA),
    plot.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.border = element_blank(),
    text = element_text(family = "Helvetica"),
    plot.title = element_text(face = "bold", size = rel(1.33))
  ) + labs(x = "Theoretical Quantiles", y = "Sample",
           title = "QQ Plot for Recency")

QQ_F <- ggplot(freq, aes(sample = n)) +
  stat_qq() + stat_qq_line() + theme(
    panel.grid.minor = element_blank(),
    panel.grid.major =  element_line(color = "#d0d0d0"),
    panel.background = element_rect(fill = "#f0f0f0", color = NA),
    plot.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.border = element_blank(),
    text = element_text(family = "Helvetica"),
    plot.title = element_text(face = "bold", size = rel(1.33))
  ) + labs(x = "Theoretical Quantiles", y = "Sample",
           title = "QQ Plot for Frequency")

QQ_M <- ggplot(monetary, aes(sample = Average_Invoice)) +
  stat_qq() + stat_qq_line() + theme(
    panel.grid.minor = element_blank(),
    panel.grid.major =  element_line(color = "#d0d0d0"),
    panel.background = element_rect(fill = "#f0f0f0", color = NA),
    plot.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.border = element_blank(),
    text = element_text(family = "Helvetica"),
    plot.title = element_text(face = "bold", size = rel(1.33))
  ) + labs(x = "Theoretical Quantiles", y = "Sample",
           title = "QQ Plot for Monetary")

QQ_R + QQ_F + QQ_M

ggsave("QQ.png", height = 5, width = 12, dpi = 500)
##Combining RFM
RFM <- inner_join(receny, freq, by = "CustomerID") %>%
  inner_join(monetary, by = "CustomerID") %>%
  mutate(Number_of_Purchases = n) %>%
  select(-n)

#Standardize values
clust.RFM <- scale(RFM[,2:4])

#From Berkley's k-means cluster analysis
distance <- get_dist(RFM)

##Elbow Method--kmeans
fviz_nbclust(clust.RFM, kmeans, method = "wss")
ggsave("Elbow_Method.png", height = 5, width = 5, dpi = 500)

##Average Silhouette Method
fviz_nbclust(clust.RFM, kmeans, method = "silhouette")
ggsave("Average_Silhouette_Method.png", height = 5, width = 5, dpi = 500)

##Gap statistic
gap_stat <- clusGap(clust.RFM, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)


##Software suggestion of Optimal Number of Clusters
res.nbclust <- NbClust(data = clust.RFM, distance = "euclidean",
                       min.nc = 2, max.nc = 10, method = "kmeans", index ="all")

fviz_nbclust(res.nbclust, ggtheme = theme_minimal())
ggsave("NbClust_Clusters.png", height = 5, width = 5, dpi = 500)

#3 clusters
three_clusters <- kmeans(clust.RFM, 3, nstart = 25)
print(three_clusters$tot.withinss)

fviz_cluster(three_clusters, data = clust.RFM, labelsize = 0, pointsize = 0.9, ellipse.alpha = 0.1) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major =  element_line(color = "#d0d0d0"),
    panel.background = element_rect(fill = "#f0f0f0", color = NA),
    plot.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.border = element_blank(),
    legend.background = element_rect(fill = "#f0f0f0", color = NA)
  )
ggsave("Three_Clusters.png", height = 5, width = 5, dpi = 500)

##Summary Statistics for 3 clusters
RFM %>%
  mutate(Cluster = three_clusters$cluster) %>%
  group_by(Cluster) %>%
  count()

RFM %>%
  mutate(Cluster = three_clusters$cluster) %>%
  select(-CustomerID)%>%
  group_by(Cluster) %>%
  summarise_all("mean")  %>%
  gt::gt()

#4 clusters
four_clusters <- kmeans(clust.RFM, 4, nstart = 25)
print(four_clusters$tot.withinss)

fviz_cluster(four_clusters, data = clust.RFM, labelsize = 0) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major =  element_line(color = "#d0d0d0"),
    panel.background = element_rect(fill = "#f0f0f0", color = NA),
    plot.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.border = element_blank(),
    legend.background = element_rect(fill = "#f0f0f0", color = NA)
  )
ggsave("Four_Clusters.png", height = 5, width = 5, dpi = 500)

RFM %>%
  mutate(Cluster = four_clusters$cluster) %>%
  group_by(Cluster) %>%
  count()

RFM %>%
  mutate(Cluster = four_clusters$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

#Final cluster summary
RFM$cluster <- final$cluster

RFM %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

RFM %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  count()

##Visualizing the clusters with the interaction of RF & M
#RF
RFM %>%
  mutate(Cluster = three_clusters$cluster) %>%
  ggplot(aes(
    x = Most_Recent_Purchase,
    y = Number_of_Purchases,
    color = factor(Cluster)
  )) +
  geom_point(alpha = 0.7) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major =  element_line(color = "#d0d0d0"),
    panel.background = element_rect(fill = "#f0f0f0", color = NA),
    plot.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.border = element_blank(),
    legend.background = element_rect(fill = "#f0f0f0", color = NA),
  ) + labs(x = "Recency of Purchases", y = "Frequency of Purchases",
           title = "Recency vs Frequency of Purhcases") +
  scale_colour_manual(
    name  = "Cluster",
    breaks = c("1", "2", "3"),
    labels = c(
      "Loyal Customers",
      "Cluster 2",
      "Cluster 3"
    ),
    values = c("coral1", "darkorchid4", "cornsilk4")
  )
ggsave("Recency_and_Freq.png", height = 5, width = 5, dpi = 500)
#RM
RFM %>%
  mutate(Cluster = three_clusters$cluster) %>%
  ggplot(aes(
    x = Most_Recent_Purchase,
    y = Average_Invoice,
    color = factor(Cluster)
  )) +
  geom_point(alpha = 0.7) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major =  element_line(color = "#d0d0d0"),
    panel.background = element_rect(fill = "#f0f0f0", color = NA),
    plot.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.border = element_blank(),
    legend.background = element_rect(fill = "#f0f0f0", color = NA),
  ) + labs(x = "Recency of Purchases", y = "Monetary Value of Purchases",
           title = "Recency vs Monetary Value of Purhcases") +
  scale_colour_manual(
    name  = "Cluster",
    breaks = c("1", "2", "3"),
    labels = c(
      "Loyal Customers",
      "Large Infrequent Customers",
      "Cluster 3"
    ),
    values = c("coral1", "darkorchid4", "cornsilk4")
  )
ggsave("Recency_and_Monetary.png", height = 5, width = 5, dpi = 500)
#FM
RFM %>%
  mutate(Cluster = three_clusters$cluster) %>%
  ggplot(aes(
    x = Number_of_Purchases,
    y = Average_Invoice,
    color = factor(Cluster)
  )) +
  geom_point(alpha = 0.7) +
  theme(
    axis.text = element_text(face = "bold", color = "darkgray"),
    panel.grid.minor = element_blank(),
    panel.grid.major =  element_line(color = "#d0d0d0"),
    panel.background = element_rect(fill = "#f0f0f0", color = NA),
    plot.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.border = element_blank(),
    legend.background = element_rect(fill = "#f0f0f0", color = NA),
  ) + labs(x = "Frequency of Purchases", y = "Monetary Value of Purchases",
           title = "Frequency vs Monetary Value of Purhcases") +
  scale_colour_manual(
    name  = "Cluster",
    breaks = c("1", "2", "3"),
    labels = c(
      "Loyal Customers",
      "Large Infrequent Customers",
      "One-Off Customers"
    ),
    values = c("coral1", "darkorchid4", "cornsilk4")
  )
ggsave("Freq_and_Monetary.png", height = 5, width = 5, dpi = 500)
##Boxplots for R, F and M
RFM %>%
  mutate(Cluster = three_clusters$cluster) %>%
  ggplot(aes(x = factor(Cluster),y = Most_Recent_Purchase, fill = factor(Cluster))) +
  geom_boxplot() +
  theme(
    axis.text = element_text(face = "bold", color = "darkgray"),
    panel.grid.minor = element_blank(),
    panel.grid.major =  element_line(color = "#d0d0d0"),
    panel.background = element_rect(fill = "#f0f0f0", color = NA),
    plot.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.border = element_blank(),
    
    legend.background = element_rect(fill = "#f0f0f0", color = NA)
    ) + labs(x = "Clusters", y = "Recency of Purchases",
           title = "Recency of Purhcases by Clusters") +
  scale_colour_discrete(
    name  = "Cluster",
    breaks = c("1", "2", "3"),
    labels = c(
      "Loyal Customers",
      "Large Infrequent Customers",
      "One-Off Frequency"
    ))



seg.df %>%
  select(CustomerID, InvoiceNo) %>%
  group_by(CustomerID, InvoiceNo) %>%
  count() %>%
  group_by(CustomerID) %>%
  count()

seg.df %>%
  select(CustomerID, InvoiceNo) %>%
  group_by(CustomerID) %>%
  count(CustomerID) %>%
  ungroup()

