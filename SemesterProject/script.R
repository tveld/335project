# 335 Semester Project
# Tim Eggleston
# Troy Veldhuizen


#####################################################################
# Read in data files
#####################################################################


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
# Preliminary analysis
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
govprofit = colSums(govattr[25:34])
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
  bussums[0:9], 
  main="Sales from Business Customers",
  xlab="Product Type",
  ylab="Sales",
  col=rainbow(10)
)

# bar: profits by bus customers
busattr = custs[custs$org == "b",]
busprofit = colSums(busattr[25:34])
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




