customer.data=read.csv("customers.csv")
geo.data=read.csv("geolocation.csv")
item.data=read.csv("order_items.csv")
orders=read.csv("orders.csv")
payment.data=read.csv("payments.csv")
product.data=read.csv("products.csv")
sellers.data=read.csv("sellers.csv")

state.count=table(customer.data$customer_state)
state.count.sorted=sort(state.count, decreasing = TRUE)
top10.states=state.count.sorted[1:10]

top10.states.df=as.data.frame(top10.states)
colnames(top10.states.df)=c("states","count")

ggplot(data=top10.states.df)+
  geom_col(mapping=aes(x=reorder(states, count), y=count), fill="dark green")+
  labs(title="the top 10 States with the most customers",
       y="Number of customers", x="states")+
  coord_flip()



state.count.sorted=sort(state.count, decreasing=TRUE) #sort from the highest to the smallest
top5.states=state.count.sorted[1:5]
top5.states.df=as.data.frame(top5.states)
colnames(top5.states.df)=c("states","count") 


## 2
colnames(item.data)
colnames(product.data)
library(tidyverse)
product.item.join=product.data%>%
  inner_join(item.data, by=c("product_id"))
category.count=table(product.item.join$product.category)
category.sorted=sort(category.count, decreasing = TRUE)
top3.category=category.sorted[1:3]

## 3
class(orders$order_purchase_timestamp)
orders$order_purchase_timestamp=as.POSIXct(orders$order_purchase_timestamp, format= "%Y-%m-%d %H:%M:%S")
class(orders$order_purchase_timestamp)

class(orders$order_delivered_customer_date)
orders$order_delivered_customer_date=as.POSIXct(orders$order_delivered_customer_date, format= "%Y-%m-%d %H:%M:%S")
class(orders$order_purchase_timestamp)

colnames(orders)
table(orders$order_status)

delivered.order=orders %>%
  filter(order_status=="delivered")

delivered.order$delivery.time.days=as.numeric(difftime(delivered.order$order_delivered_customer_date, 
                                                       delivered.order$order_purchase_timestamp),
                                              units="days")
colnames(delivered.order)
colnames(customer.data)
cust.delivered=delivered.order%>%
  inner_join(customer.data, by="customer_id")

#analyze avarage delivery time by state
avg.delivery.states=aggregate(delivery.time.days~customer_state, data=cust.delivered, mean)

library(ggplot2)
ggplot(data=avg.delivery.states)+
  geom_col(mapping=aes(x=reorder(customer_state, delivery.time.days), y=delivery.time.days), fill="steelblue")+
  coord_flip()+
  labs(
    title = "Average Delivery Time by State",
    x = "Customer State",
    y = "Average Delivery Time (days)")


## 4
payment.count=table(payment.data$payment_type)
payment.sorted=sort(payment.count, decreasing = TRUE)
payment.df=as.data.frame(payment.sorted)
colnames(payment.df)=c("payment_type", "count")

ggplot(data=payment.df)+
  geom_col(mapping=aes(x=reorder(payment_type, count), y=count), fill="magenta")+
  coord_flip()


## product and ordertime
ggplot(data=ordertime.btb.df)+
  geom_line(mapping=aes(x=as.numeric(order_purchase_timestamp), y=Freq), 
            col="dark blue",
            size=1.5)+
  scale_x_continuous(breaks = seq(1,31, by=2))+ 
  labs(x = "Day", y = "Frequency")

ggplot(data=ordertime.HB.df)+
  geom_line(mapping=aes(x=as.numeric(order_purchase_timestamp), y=Freq), 
            col="dark blue",
            size=1.5)+
  scale_x_continuous(breaks = 1:31)+ 
  labs(x = "Day", y = "Frequency")

ggplot(data=ordertime.sport.l.df)+
  geom_line(mapping=aes(x=as.numeric(order_purchase_timestamp), y=Freq), 
            col="dark blue",
            size=1.5)+
  scale_x_continuous(breaks = 1:31)+ 
  labs(x = "Day", y = "Frequency")


## seller, order item, state

seller.count=table(sellers.data$seller_state)
colnames(seller.count.df)=c("seller_state", "count")
seller.count=sort(seller.count, decreasing = TRUE)
seller.count.df=as.data.frame(seller.count)

