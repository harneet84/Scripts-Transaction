Offers<-read.csv("C:/Data mining/offers.csv", header=TRUE)


rxDataStep(inData = "Retail.xdf", outFile = "Retail_Red.xdf",rowSelection= company %in% c(108500080,107127979,106414464,107120272,105100050,104460040,103320030,1089520383,107106878,104610040,107717272,104460040,108079383,1087744888,105190050,1076211171,105450050,103700030,104127141)|category %in% c(9115,9909,3203,5558,4401,1703,1726,3504,3509,9909,5122,5616,5619,2202,2119,6202,5824,799,4517 7205,706),overWrite=TRUE)
	
Sample<-rxDataStep(inData = "Retail_Red.xdf",rowSelection= id > 1000000)
count <- function(x) {
  
  length(na.omit(x))
 

}



for (i in 3422:6459)
	Offer[i,1]<-test[i-3421]

## Subset the Retail_Red based on ids into multiple datafile
Sample<-rxDataStep(inData = "Retail_Red.xdf",rowSelection= id>106793094)
Sample<-rxDataStep(inData = "Retail_Red.xdf",rowSelection= id>121818883)
Sample<-rxDataStep(inData = "Retail_Red.xdf",rowSelection= id>140423698)
.
.
.
t<-unique(Sample$id)
a<-data.frame(t)
for (i in 1:1)
	coup1[i,1]<-a[i,1]

coup1<-data.frame()
while(a[length(t),1]<2147483648)
{
	Sample<-rxDataStep(inData = "Retail_Red.xdf",rowSelection= id>a[length(t),1],transformObjects = list(a = a,t=t))
	t<-unique(Sample$id)
	a<-data.frame(t)
	b<-nrow(coup1)+length(t)
	s<-1
for (i in nrow(coup1):b)
  {

	coup1[i,1]<-a[s,1]
	s<-s+1
  }
	
}

	b<-nrow(coup1)+length(test)	
for (i in nrow(coup1):b)
{
	
	g[i-(nrow(coup1)-1)]<-i

	
	
	##Has_Bought_Category
	
	for(i in 1:nrow(coup_1))
	{
		if(coup_1[i,5]>0)
		coup_1[i,9]<-1
		
 		
		}


##selecting the rows based on a category or company and storing it in a variable
cat_1<-rxDataStep(inData = "Retail_Red.xdf",rowSelection=category==9909)
comp_1<-rxDataStep(inData = "Retail_Red.xdf",rowSelection=company==107127979)
## copying the count and amount to coup1
coup_1<-merge(coup_1,cnt,by.x="id",by.y="id",all.x=TRUE)
##cnt is the number of transactions for each customer for a particular company or category
cnt<-aggregate(comp_1$id, list(id = comp_1$id), count)
cnt<-aggregate(cat_1$id, list(id = cat_1$id), count)
cnt<-aggregate(comp_cat_1$id, list(id = comp_cat_1$id), count)
##amount is the total amount spent by each customer for a particular company
amount<-aggregate(cat_1$purchaseamount, list(id = cat_1$id),sum,na.rm=TRUE,na.action=NULL)
 amount<-aggregate(comp_1$purchaseamount, list(id = comp_1$id),sum,na.rm=TRUE,na.action=NULL)
##quantity is the mean quantity purchased by the customer for a particular company 
##9909 3203 5558 4401 1703 1726 3504 3509 5616 5619 2202 2119 6202
## 107127979  106414464  107120272  105100050  104460040  103320030
## 1089520383  104610040  107717272  108079383 1087744888
for(i in 1:nrow(train))
{
	if(train[i,6]=="t")
	train[i,8]=1
	else
	train[i,8]=0
	
}

quantity<-aggregate(comp_1$purchasequantity, list(id = comp_1$id),mean,na.rm=TRUE,na.action=NULL)

##replacing the NA values with 0
coup_1[is.na(coup_1)]<-0
##replacing negative values with 0
coup1[coup1<0]<-0
##naming the columns
colnames(coup_1)<-c("id","company_count","company_amount","company_avg_quantity","category_count","category_amount","category_quantity","has_bought_company","has_bought_category","comp_cat_count","comp_cat_amount","comp_cat_quantity","has_bought_comp_cat")
##Save the data frame
	
for(i in 1:length(c))
{
	if(offers[i,1]==c[i])
	{
	g[j,1]<-offers[i,2]
	g[j,2]<-offers[i,4]
}
j<-j+1
}

##Final script 

coup_14<-coup1
colnames(coup_14)<-c("id")

##Company
comp_2<-rxDataStep(inData = "Retail_Red.xdf",rowSelection=company==1089520383)
comp_2[comp_2<0]<-0
##count
cnt<-aggregate(comp_2$id, list(id = comp_2$id), count)
coup_14<-merge(coup_14,cnt,by.x="id",by.y="id",all.x=TRUE)
coup_14[is.na(coup_14)]<-0
##amount
amount<-aggregate(comp_2$purchaseamount, list(id = comp_2$id),sum,na.rm=TRUE,na.action=NULL)
coup_14<-merge(coup_14,amount,by.x="id",by.y="id",all.x=TRUE)
coup_14[is.na(coup_14)]<-0
##quantity
quantity<-aggregate(comp_2$purchasequantity, list(id = comp_2$id),mean,na.rm=TRUE,na.action=NULL)
coup_14<-merge(coup_14,quantity,by.x="id",by.y="id",all.x=TRUE)
coup_14[is.na(coup_14)]<-0

##CATEGORY

cat_2<-rxDataStep(inData = "Retail_Red.xdf",rowSelection=category==9909)
cat_2[cat_2<0]<-0
##count
cnt<-aggregate(cat_2$id, list(id = cat_2$id), count)

coup_14<-merge(coup_14,cnt,by.x="id",by.y="id",all.x=TRUE)
coup_14[is.na(coup_14)]<-0
##amount
amount<-aggregate(cat_2$purchaseamount, list(id = cat_2$id),sum,na.rm=TRUE,na.action=NULL)
coup_14<-merge(coup_14,amount,by.x="id",by.y="id",all.x=TRUE)
coup_14[is.na(coup_14)]<-0
##quantity
quantity<-aggregate(cat_2$purchasequantity, list(id = cat_2$id),mean,na.rm=TRUE,na.action=NULL)
coup_14<-merge(coup_14,quantity,by.x="id",by.y="id",all.x=TRUE)
coup_14[is.na(coup_14)]<-0

##COMPANY AND CATEGORY
comp_cat_2<-rxDataStep(inData = "Retail_Red.xdf",rowSelection=(category==9909)&(company==1089520383))
comp_cat_2[comp_cat_2<0]<-0
##count
cnt<-aggregate(comp_cat_2$id, list(id = comp_cat_2$id), count)
coup_14<-merge(coup_14,cnt,by.x="id",by.y="id",all.x=TRUE)
coup_14[is.na(coup_14)]<-0
##amount
amount<-aggregate(comp_cat_2$purchaseamount, list(id = comp_cat_2$id),sum,na.rm=TRUE,na.action=NULL)
coup_14<-merge(coup_14,amount,by.x="id",by.y="id",all.x=TRUE)
coup_14[is.na(coup_14)]<-0
##quantity
quantity<-aggregate(comp_cat_2$purchasequantity, list(id = comp_cat_2$id),mean,na.rm=TRUE,na.action=NULL)
coup_14<-merge(coup_14,quantity,by.x="id",by.y="id",all.x=TRUE)
coup_14[is.na(coup_14)]<-0

colnames(coup_14)<-c("id","company_count","company_amount","company_avg_quantity","category_count","category_amount","category_quantity","comp_cat_count","comp_cat_amount","comp_cat_quantity")


##save the offer 

dump("coup_14",file="offer14.R")
write.csv(coup_14,"offer14.csv")

##Merging train and RFM and offers
train_1<-merge(train,offers,by.x="offer",by.y="offer",all.x=TRUE)
toff3<-subset(train_1,offer==1203052)
Finaltrain2<-merge(toff3,coup_14,by.x="id",by.y="id",all.x=TRUE)
Trainfinal<-rbind(Trainfinal,Finaltrain2)







##Has bought company
	for(i in 1:nrow(coup_2))
	{
		if(coup_2[i,2]>0)
		coup_2[i,11]<-1
	}
##Has_Bought_Category
	
	for(i in 1:nrow(coup_2))
	{
		if(coup_2[i,5]>0)
		coup_2[i,12]<-1
		
 		
	}

		
##Has bought category_company
	for(i in 1:nrow(coup_2))
	{
		if(coup_2[i,8]>0)
		coup_2[i,13]<-1
		
 		
	}
	
coup_2[is.na(coup_2)]<-0



colnames(coup_2)<-c("id","company_count","company_amount","company_avg_quantity","category_count","category_amount","category_quantity","comp_cat_count","comp_cat_amount","comp_cat_quantity")


##save the offer 

dump("coup_2",file="Offer2.R")
write.csv(coup_2,"Offer2.csv")


