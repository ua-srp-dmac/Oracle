library("RPostgres")
con <- dbConnect(RPostgres::Postgres())

table1 <- dbReadTable(con, "table1")

dbDisconnect(con)

table1 <- as.data.frame(table1)

nodes <- unique(c(table1$gene1,table1$gene2))
names(nodes) <- sample(rep(1:length(nodes)), length(nodes), replace = FALSE)

edges <- NULL

for( i in 1:dim(table1)[1]){


edges$source[i] <- table1$gene1[i]
edges$target[i] <- table1$gene2[i]

if(table1$connection[i] == "Suppresses"){
edges$connection[i] <- "red"
} else if(table1$connection[i] == "Promotes"){
edges$connection[i] <- "green"

} else if(table1$connection[i] == "Transcription Target"){
edges$connection[i] <- "blue"
} else {
edges$connection[i] <- "grey"

}

}

edges <- as.data.frame(edges)

sink("/Oracle/cf15f53/4dbc34e2bd8b2dc506b8210d1f0.js")

cat("let text_1 = '")
cat('{"nodes": [')

for(i in 1:length(nodes)){
cat('{"id": "')
cat(nodes[i])
cat('","name": "')
cat(nodes[i])
cat('","val": 10}')
if(i != length(nodes)){
cat(',')
}
}

cat("]");
cat(',"links": [')


for( i in 1:dim(edges)[1])
{
cat('{"source": "')
cat(edges$source[i])
cat('","target": "')
cat(edges$target[i])
cat('","connection": "')
cat(edges$connection[i])
cat('"}')

if(i != dim(edges)[1]){
cat(',')
}

}

cat(']}')
cat("'")
sink()


edges <- NULL

for( i in 1:dim(table1)[1]){


edges$source[i] <- names(nodes[nodes == table1$gene1[i]])
edges$target[i] <- names(nodes[nodes == table1$gene2[i]])

if(table1$connection[i] == "Suppresses"){
edges$connection[i] <- "red"
} else if(table1$connection[i] == "Promotes"){
edges$connection[i] <- "green"

} else if(table1$connection[i] == "Transcription Target"){
edges$connection[i] <- "blue"
} else {
edges$connection[i] <- "grey"

}

}

edges <- as.data.frame(edges)

sink("/Oracle/Rdata.js")

cat("let text_1 = '")
cat('{"nodes": [')

for(i in 1:length(nodes)){
cat('{"id": "')
cat(names(nodes[i]))
cat('","name": "')
cat(names(nodes[i]))
cat('","val": 10}')
if(i != length(nodes)){
cat(',')
}
}

cat("]");
cat(',"links": [')


for( i in 1:dim(edges)[1])
{
cat('{"source": "')
cat(edges$source[i])
cat('","target": "')
cat(edges$target[i])
cat('","connection": "')
cat(edges$connection[i])
cat('"}')

if(i != dim(edges)[1]){
cat(',')
}

}

cat(']}')
cat("'")
sink()
