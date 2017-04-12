# 335 Semester Project
# Tim Eggleston
# Troy Veldhuizen

# read in data files
custs = read.table(
  "custs.txt", 
  header=TRUE,
  sep="\t"
)

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

