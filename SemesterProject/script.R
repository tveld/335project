# 335 Semester Project
# Tim Eggleston
# Troy Veldhuizen


#####################################################################
#
# Read in data files
#
#
#####################################################################

#
custs = read.table(
  "custs.txt", 
  header=TRUE,
  sep="\t"
)

#TODO read in as transaction, probably more useful
trans = read.table(
  "trans.txt",
  header=FALSE,
  sep=",",
  fill=TRUE
)
#as transaction basket
library(arules)
transdata = read.transactions("trans.txt", format="basket", sep=",")


#read products
product = read.table(
  "product.txt", 
  header=FALSE,
  sep="\t",
  col.names=c("id","cost","price", "sold")
)


# Create new attributes

# create additional attributes for product table
product["profit"] = product$price - product$cost
product["prodCost"] = product$cost * product$sold
product["prodSales"] = product$price * product$sold
product["prodProfit"] = product$profit * product$sold
product["%ofTotSales"] = product$prodSales / sum(product$prodSales)
product["%ofTotProfit"] = product$prodProfit / sum(product$prodProfit)

# create additional attributes for the custs table
custs["prod0Sales"] = product[product$id == 0,3] * custs$prod0
custs["prod1Sales"] = product[product$id == 1,3] * custs$prod1
custs["prod2Sales"] = product[product$id == 2,3] * custs$prod2
custs["prod3Sales"] = product[product$id == 3,3] * custs$prod3
custs["prod4Sales"] = product[product$id == 4,3] * custs$prod4
custs["prod5Sales"] = product[product$id == 5,3] * custs$prod5
custs["prod6Sales"] = product[product$id == 6,3] * custs$prod6
custs["prod7Sales"] = product[product$id == 7,3] * custs$prod7
custs["prod8Sales"] = product[product$id == 8,3] * custs$prod8
custs["prod9Sales"] = product[product$id == 9,3] * custs$prod9

custs["prod0Profit"] = product[product$id == 0, 5] * custs$prod0
custs["prod1Profit"] = product[product$id == 1, 5] * custs$prod1
custs["prod2Profit"] = product[product$id == 2, 5] * custs$prod2
custs["prod3Profit"] = product[product$id == 3, 5] * custs$prod3
custs["prod4Profit"] = product[product$id == 4, 5] * custs$prod4
custs["prod5Profit"] = product[product$id == 5, 5] * custs$prod5
custs["prod6Profit"] = product[product$id == 6, 5] * custs$prod6
custs["prod7Profit"] = product[product$id == 7, 5] * custs$prod7
custs["prod8Profit"] = product[product$id == 8, 5] * custs$prod8
custs["prod9Profit"] = product[product$id == 9, 5] * custs$prod9

custs["custSales"] = 
  custs$prod0Sales + 
  custs$prod1Sales + 
  custs$prod2Sales + 
  custs$prod3Sales + 
  custs$prod4Sales + 
  custs$prod5Sales + 
  custs$prod6Sales + 
  custs$prod7Sales + 
  custs$prod8Sales + 
  custs$prod9Sales

custs["custProfit"] = 
  custs$prod0Profit + 
  custs$prod1Profit + 
  custs$prod2Profit + 
  custs$prod3Profit + 
  custs$prod4Profit + 
  custs$prod5Profit + 
  custs$prod6Profit + 
  custs$prod7Profit + 
  custs$prod8Profit + 
  custs$prod9Profit


#####################################################################
#
# Preliminary analysis
#
#
#####################################################################


# bar: units sold by product
unitss = colSums(custs[3:12])
barplot(
  unitss, 
  main="Units Sold by Product",
  xlab="Product Type",
  ylab="Units Sold",
  col=rainbow(10)
)

# why the difference?
# think it's because this one does not bin by product
# not sure how to fix it

# bar: units sold by product
barplot(
  product$sold, 
  main="Units Sold by Product",
  xlab="Product Type",
  ylab="Units Sold",
  col=rainbow(10)
)

# bar: sales by product
sales = colSums(custs[15:24])
nms = c("1","2","3","4","5","6","7","8","9","10")
sales = setNames(sales,nms)
barplot(
  sales, 
  main="Sales by Product",
  xlab="Product Type",
  ylab="Sales",
  col=rainbow(10)
)

# bar: profits by product
profit = colSums(custs[25:34])
nms = c("1","2","3","4","5","6","7","8","9","10")
profit = setNames(profit,nms)
barplot(
  profit, 
  main="Profits Earned by Product",
  xlab="Product Type",
  ylab="Profit",
  col=rainbow(10)
)

# bar: units sold by gov customers
govattr = custs[custs$org == "g",]
govunits = colSums(govattr[3:12])
nms = c("1","2","3","4","5","6","7","8","9","10")
govunits = setNames(govunits,nms)
barplot(
  govunits, 
  main="Units Sold to Government Customers",
  xlab="Product Type",
  ylab="Units Sold",
  col=rainbow(10)
)

# bar: sales by gov customers
govattr = custs[custs$org == "g",]
govsales = colSums(govattr[15:24])
nms = c("1","2","3","4","5","6","7","8","9","10")
govsales = setNames(govsales,nms)
barplot(
  govsales, 
  main="Sales from Government Customers",
  xlab="Product Type",
  ylab="Sales",
  col=rainbow(10)
)


# bar: profits earned by gov customers
govattr = custs[custs$org == "g",]
govprofit = colSums(govattr[,25:34])
ms = c("1","2","3","4","5","6","7","8","9","10")
govprofit = setNames(govprofit,nms)
barplot(
  govprofit, 
  main="Profits Earned from Government Customers",
  xlab="Product Type",
  ylab="Profits Earned",
  col=rainbow(10)
)


# bar: units sold by bus customers
busattr = custs[custs$org == "b",]
busunits = colSums(busattr[3:12])
ms = c("1","2","3","4","5","6","7","8","9","10")
busunits = setNames(busunits,nms)
barplot(
  busunits, 
  main="Units Sold from Business Customers",
  xlab="Product Type",
  ylab="Units Sold",
  col=rainbow(10)
)

# bar: sales by bus customers
busattr = custs[custs$org == "b",]
bussales = colSums(busattr[15:24])
ms = c("1","2","3","4","5","6","7","8","9","10")
bussales = setNames(bussales,nms)
barplot(
  bussales, 
  main="Sales from Business Customers",
  xlab="Product Type",
  ylab="Sales",
  col=rainbow(10)
)

# bar: profits by bus customers
busattr = custs[custs$org == "b",]
busprofit = colSums(busattr[,25:34])
ms = c("1","2","3","4","5","6","7","8","9","10")
busprofit = setNames(busprofit,nms)
barplot(
  busprofit, 
  main="Profits Earned from Business Customers",
  xlab="Product Type",
  ylab="Profits Earned",
  col=rainbow(10)
)

#parallel coordinates
# not sure why not working
library(MASS)
orgsums = aggregate(. ~ custs$org, data=custs, FUN=sum)
parcoord(orgsums)

#bar: profits by gov & bus customers
x_name <- "busprofit"
y_name <- "govprofit"
df <- data.frame(busprofit,govprofit)
names(df) <- c(x_name,y_name)
barplot(as.matrix(df), main="Profits Earned by Business and Government Customers",
        xlab="Product Type", 
        ylab = "Profits Earned", 
        col=rainbow(10),
        legend = rownames(counts), 
        beside=TRUE)


# boxplot: days late
# note the outliers
boxplot(
  custs$daysLate ~ custs$org, 
  main="Days Late by Organization Type", 
  xlab="Organization", 
  ylab="Days Late"
)

# boxplot: units sold per client by org type
boxplot(
  custs$sales ~ custs$org, 
  main="Units Sold per Client by Organization Type", 
  xlab="Organization", 
  ylab="Units Sold"
)

# boxplot: sales per client by org type
boxplot(
  custs$custSales ~ custs$org, 
  main="Sales per Client by Organization Type", 
  xlab="Organization", 
  ylab="Sales"
)

# boxplot: profit per client by org type
boxplot(
  custs$custProfit ~ custs$org, 
  main="Profit per Client by Organization Type", 
  xlab="Organization", 
  ylab="Profit"
)




#####################################################################
#
# 1. Segment Customers Into Groups
#    Basics & Classifier
#
#
#####################################################################

# k clusters setup
k = 10

# k-means cluster of customers
custCluster = matrix(nrow = k, ncol = 2)
for(i in 2:k){
  clst = kmeans(custs[,3:14], i)
  custCluster[i-1,1] = i
  custCluster[i-1,2] = clst$tot.withinss
}
plot(custCluster[,1], custCluster[,2])

# k-means customer by government
govCluster = matrix(nrow = k, ncol = 2)
for(i in 2:k){
  clst = kmeans(custs[custs$org == "g",3:14], i)
  govCluster[i-1,1] = i
  govCluster[i-1,2] = clst$tot.withinss
}
plot(govCluster[,1], govCluster[,2])

# k-means customer by business
busCluster = matrix(nrow = k, ncol = 2)
for(i in 2:k){
  clst = kmeans(custs[custs$org == "b",3:14], i)
  busCluster[i-1,1] = i
  busCluster[i-1,2] = clst$tot.withinss
}
plot(busCluster[,1], busCluster[,2])

# cluster visualizations
clst = kmeans(custs[,3:14], 4)
x=cbind(custs[1,4], cluster)
table(clst$cluster, iris[,5])
ans = matrix(0,14,2)
ans[1,1] = 2

# final k means
seg = custs[3:14]
kseg = kmeans(seg, 4)
seg$cls = kseg$cluster
seg1 = seg[seg$cls == 1,]
seg2 = seg[seg$cls == 2,]
seg3 = seg[seg$cls == 3,]
seg4 = seg[seg$cls == 4,]

# barplot for 1
s1 = colSums(seg1[1:10])
name = c("1","2","3","4","5","6","7","8","9","10")
s1 = setNames(s1,name)
barplot(
  s1, 
  main="Customer Group 1",
  xlab="Product Type",
  ylab="Units Sold",
  col=rainbow(10)
)
# barplot for 2
s2 = colSums(seg2[1:10])
name = c("1","2","3","4","5","6","7","8","9","10")
s2 = setNames(s2,name)
barplot(
  s2, 
  main="Customer Group 2",
  xlab="Product Type",
  ylab="Units Sold",
  col=rainbow(10)
)
# barplot for 3
s3 = colSums(seg3[1:10])
name = c("1","2","3","4","5","6","7","8","9","10")
s3 = setNames(s3,name)
barplot(
  s3, 
  main="Customer Group 3",
  xlab="Product Type",
  ylab="Units Sold",
  col=rainbow(10)
)


# barplot for 4
s4 = colSums(seg4[1:10])
name = c("1","2","3","4","5","6","7","8","9","10")
s4 = setNames(s4,name)
barplot(
  s4, 
  main="Customer Group 4",
  xlab="Product Type",
  ylab="Units Sold",
  col=rainbow(10)
)



###################################################################
#
# 2. Identifying Late Customers
#    Basics & Classifier
#
#
#####################################################################


# boxplot: days late
boxplot(
  custs$daysLate ~ custs$org, 
  main="Days Late by Organization Type", 
  xlab="Organization", 
  ylab="Days Late"
)

# 5 number summaries
summary(custs$daysLate)

onTime = custs[(custs$daysLate < 60),]
late = custs[(custs$daysLate >= 60),]
summary(onTime$daysLate)
summary(late$daysLate)

# bar: profits by product for late people vs non-late
profitLate = colSums(late[25:34])
profitOnTime = colSums(onTime[25:34])
nms = c("1","2","3","4","5","6","7","8","9","10")
prep = as.matrix(data.frame(profitLate, profitOnTime))
barplot(
  prep, 
  main="Profits Earned by Product",
  xlab="Product Type",
  ylab="Profit",
  col=rainbow(10),
  beside = TRUE
)


# bar: lateness by organization type
govOnTime = nrow(govattr[(govattr$daysLate < 60),])
govLate = nrow(govattr[(govattr$daysLate >= 60),])
busOnTime = nrow(busattr[(busattr$daysLate < 60),])
busLate = nrow(busattr[(busattr$daysLate >= 60),])

prep = as.matrix(data.frame(govOnTime, govLate, busOnTime, busLate))
barplot(
  prep, 
  main="Profits Earned by Product",
  xlab="Product Type",
  ylab="Profit",
  col=rainbow(10),
  beside = TRUE
)

govLate / govOnTime
busLate / busOnTime


# Decision Tree Classification
# Success: discretize daysLate
# Failure: Decision Tree does not make any nodes??
library(rpart)
discriminate = custs

#t = table(discretize(discriminate$daysLate, method="cluster", categories=2))
#hist(t, breaks=20, main="K-Means")
#a = abline(v=discretize(discriminate$daysLate, method="cluster", categories=2, onlycuts=TRUE), 
#       col="red")

#discriminate$daysLate <- discretize(discriminate[, 14], method = "interval", categories = 2)
#discriminate$daysLate = as.factor(discriminate$daysLate)

discriminate[discriminate$daysLate<60, 14] = 0
discriminate[discriminate$daysLate>=60, 14] = 1

fit = rpart(daysLate ~ org + prod0 + prod1 + prod2 + prod3 + prod4 + prod5 + prod6 + prod7 + prod8 + prod9,
            method="class", data=discriminate)
plotcp(fit)
predict(fit, custs, type="class")
mean(trypredict == (custs$daysLate > 60))

# classification with SVM
# kind of works with threshold of 20
# horrible at predicting
install.packages("e1071")
library(e1071)

#build customer table with profits for each product, total profit, type, and late class
pred = custs[25:34]
pred["type"] = custs[2]
pred["late"] = custs[14]
pred[pred$late<20, 12] = 0
pred[pred$late>=20, 12] = 1
pred[pred$late == 0,12] = "Early"
pred[pred$late == 1,12] = "Late"

#pred = custs[36]
#pred["type"] = custs[2]
#pred["late"] = custs[14]
#pred[pred$late<20, 3] = 0
#pred[pred$late>=20, 3] = 1
#pred[pred$late == 0,3] = "Early"
#pred[pred$late == 1,3] = "Late"


pred$late = factor(pred$late)

# build training and test datasets
train.indeces = sample(1:nrow(pred), 1000)
pred.train = pred[train.indeces,]
pred.test = pred[-train.indeces,]

model = svm(late ~., data = pred.train)
results = predict(object = model, newdata = data.frame(pred.test), type="class")

# confusion matrix
con = table(results, pred.test$late)
#####################################################################
#

# 3. Discount to Increase Sales
#    (similar to 1.)
#
#
#####################################################################

# assotiative analysis
inspect(transdata)
ruls = apriori(transdata, parameter=list(support=.2, confidence=.5))
inspect(ruls)

# Correlations of Product:  Scatterplot Matrices from the glus Package
install.packages("gclus")
library(gclus)
productHold = product
productHold.r <- abs(cor(product)) # get correlations
productHold.col <- dmat.color(productHold.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
productHold.o <- order.single(productHold.r)
cpairs(productHold, productHold.o, panel.colors=productHold.col, gap=.5,
       main="Correlations of Product Ordered and Colored" )

# Correlations of Custs:  Scatterplot Matrices from the glus Package
custsHold = custs[,c(1,3:13)]
custsHold.r <- abs(cor(custsHold)) # get correlations
custsHold.col <- dmat.color(custsHold.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
custsHold.o <- order.single(custsHold.r)
cpairs(custsHold, custsHold.o, panel.colors=custsHold.col, gap=.5,
       main="Correlations of Custs Ordered and Colored" )

# Correlation Values for 9,0,3,5,8,7
cor(custs[,c(12,3,6,8,11,10)])