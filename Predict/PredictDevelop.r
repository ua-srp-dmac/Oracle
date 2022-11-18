library("RPostgres")
library('org.Hs.eg.db')
library("stringr")
library("cppRouting")
library("DiagrammeR")


checkreal <- function(gene1,gene2,query1){

Upmid <- unique(query1[which(query1$gene1 == gene1 & query1$gene2 == gene2),]$pmid)

count = 0 

for(Uid in Upmid){

if(is.na(Uid)){

C1 <- length(query1[which(query1$gene1 == gene1 & query1$gene2 == gene2 & query1$connection == "Promotes" & is.na(query1$pmid)),]$pmid)

C2 <- length(query1[which(query1$gene1 == gene1 & query1$gene2 == gene2 & query1$connection == "Suppresses" & is.na(query1$pmid)),]$pmid)

C3 <- length(query1[which(query1$gene1 == gene1 & query1$gene2 == gene2 & query1$connection == "Transcription Target" & is.na(query1$pmid)),]$pmid)
} else {

C1 <- length(query1[which(query1$gene1 == gene1 & query1$gene2 == gene2 & query1$connection == "Promotes" & query1$pmid == Uid),]$pmid)

C2 <- length(query1[which(query1$gene1 == gene1 & query1$gene2 == gene2 & query1$connection == "Suppresses" & query1$pmid == Uid),]$pmid)

C3 <- length(query1[which(query1$gene1 == gene1 & query1$gene2 == gene2 & query1$connection == "Transcription Target" & query1$pmid == Uid),]$pmid)


}

if(C1 +C3 > C2){
count = count+1
} else if(C2 > C1+ C3){
count = count-1 
}

}

if(count > 0) {
return(1)
} else if(count< 0){
return(-1)
}else {
return(0)
}

}




checkreal2 <- function(gene1,gene2,query1){

Upmid <- unique(query1[which(query1$gene1 == gene1 & query1$gene2 == gene2),]$pmid)

count1 = 0 
count2 = 0 
count3 = 0 

for(Uid in Upmid){

if(is.na(Uid)){

C1 <- length(query1[which(query1$gene1 == gene1 & query1$gene2 == gene2 & query1$connection == "Promotes" & is.na(query1$pmid)),]$pmid)

C2 <- length(query1[which(query1$gene1 == gene1 & query1$gene2 == gene2 & query1$connection == "Suppresses" & is.na(query1$pmid)),]$pmid)

C3 <- length(query1[which(query1$gene1 == gene1 & query1$gene2 == gene2 & query1$connection == "Transcription Target" & is.na(query1$pmid)),]$pmid)
} else {

C1 <- length(query1[which(query1$gene1 == gene1 & query1$gene2 == gene2 & query1$connection == "Promotes" & query1$pmid == Uid),]$pmid)

C2 <- length(query1[which(query1$gene1 == gene1 & query1$gene2 == gene2 & query1$connection == "Suppresses" & query1$pmid == Uid),]$pmid)

C3 <- length(query1[which(query1$gene1 == gene1 & query1$gene2 == gene2 & query1$connection == "Transcription Target" & query1$pmid == Uid),]$pmid)


}

if(C1 > C2 && C1 > C3){
count1 = count1+1 
} else if(C2 > C1 && C2 > C3){
count2 = count2+1 
}else if(C3> C2 && C3> C1){
count3 = count3+1 
}

}

if(count1 > count2 && count1 > count3 ){
return(1)
} else if(count2 > count1 && count2 > count3 ){
return(-1)
}else if(count3 > count2 && count3 > count1 ){
return(2)
} else {
return(0)
}

}


find_shortest_path <- function(graph, start, end, path = c()) {
  # if there are no nodes linked from current node (= dead end) return NULL
  if (is.null(graph[[start]])) return(NULL)
  # add next node to path so far
  path <- c(path, start)

# base case of recursion: if end is reached return path
  if (start == end) return(path)
  
  # initialize shortest path as NULL
  shortest <- NULL
  # loop through all nodes linked from the current node (given in start)
  for (node in graph[[start]]) {
    # proceed only if linked node is not already in path
    if (!(node %in% path)) {
      # recursively call function for finding shortest path with node as start and assign it to newpath
      newpath <- find_shortest_path(graph, node, end, path)

      # if newpath is shorter than shortest so far assign newpath to shortest
      if (path_length(newpath) < path_length(shortest))
        shortest <- newpath
    }
  }
  # return shortest path
return(shortest)


}



find_all_path <- function(graph, start, end, path = c()) {
  # if there are no nodes linked from current node (= dead end) return NULL
  if (is.null(graph[[start]])) return(NULL)
  # add next node to path so far
  path <- c(path, start)

# base case of recursion: if end is reached return path
  if (start == end){

  finalpath[[length(finalpath)+1]] = path
  return(list("path" = path, "finalpath" = finalpath))

  }
  
  # initialize shortest path as NULL
  shortest <- NULL
  # loop through all nodes linked from the current node (given in start)
  for (node in graph[[start]]) {
    # proceed only if linked node is not already in path
    if (!(node %in% path)) {
      # recursively call function for finding shortest path with node as start and assign it to newpath
      newpath <- find_shortest_path(graph, node, end, path)

      # if newpath is shorter than shortest so far assign newpath to shortest
      if (path_length(newpath) < path_length(shortest))
        shortest <- newpath
    }
  }
  # return shortest path
return(shortest)


}


path_length <- function(path) {
  # if path is NULL return infinite length
  if (is.null(path)) return(Inf)
  
  # get all consecutive nodes
  pairs <- cbind(values = path[-length(path)], ind = path[-1])
  # join with G and sum over weights
  sumL <- 0
 for(i in 1:dim(pairs)[1]){
 sumL <- sumL + G[which(G$values == pairs[i,2] & G$ind == pairs[i,1]),]$weights
  }

  return(sumL)
}




dijkstra <- function(graph, start){

  n_nodes <- length(unique(c(graph$from,graph$to)))

  distances = rep(Inf, n_nodes)
  visited = rep(FALSE, n_nodes)
  path = rep("",n_nodes)

  # The distance from the start node to itself is of course 0
  distances[start] = 0
  path[start] = start
    
    # ... find the node with the currently shortest distance from the start node...
    shortest_distance = Inf
    shortest_index = -1
    for(i in seq_along(distances)) {
      # ... by going through all nodes that haven't been visited yet
      if(distances[i] < shortest_distance && !visited[i]){
        shortest_distance = distances[i]
        shortest_index = i
      }
    }
    
    if(shortest_index == -1){
      # There was no node not yet visited --> We are done
      return (distances)
    }

   tgene <- names(distances[shortest_index])

   temp_min <- Inf
   temp_minname <- NULL

   for(trow in graph[graph$from == tgene]){
		
   if(trow$dist < temp_min ){
	temp_min <- Inf
	temp_min
   }
   

   }

return(NULL)

}



homedata <- "/var/www/data"

setContentType("text/html")

cat('
<!DOCTYPE html>
<html>
<head>
<title>University of Arizona SRP-DMAC Olympus server</title>
<meta http-equiv="expires"; content="0"; charset=UTF-8">
<script src="https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js"></script>
<link rel="stylesheet" type="text/css" href="/AresC/semantic.min.css">
<script src="https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js"></script>
<link href="https://clinicaltables.nlm.nih.gov/autocomplete-lhc-versions/18.1.3/autocomplete-lhc.min.css" rel="stylesheet" />

<style>

div#banner2 { 
z-index: 6;

  overflow: hidden;
  position: fixed;
  bottom: 50px;
  left: 0;	
	background-color: #000000;
	width: 100%; 
	height: 3px;
}


div#footnotebanner { 
z-index: 6;

  overflow: hidden;
  position: fixed;
  bottom: 0;
  left: 0;	
	background-color: #FFFFFF;
	width: 100%; 
	height: 50px;
}

#SRPlogo{ 

  overflow: hidden;
  position: fixed;
bottom: 5px;

margin-left:45%;
height: 40px;

}

#NIEHSlogo{

  overflow: hidden;
  position: fixed;
bottom: 5px;

margin-left:5.4%;
height: 40px;


}


div#footnote3 { 
 position: fixed;
       width: auto;
margin-left:78%;   
       float left;
}



</style>

</head>


<body>

<div id="root">
<div class="App">
	<header class="App-header">
	<div class="ui inverted top fixed menu" style:"width:90%">
		<div class="ui container">
		<a class="header item" href="http://dmac.pharmacy.arizona.edu/Oracle/Develop">
		<h1>Oracle</h1>
		</a>
		<a class="item" href="https://dmac.pharmacy.arizona.edu/Oracle/Network.html">
		<h4> Network </h4>
		</a>
		<a class="item" href="http://dmac.pharmacy.arizona.edu/Oracle/PredictDevelop">
		<h4> Predict </h4>
		</a>
		<a class="item" href="http://dmac.pharmacy.arizona.edu">
		<h4 > Home</h4>
		</a>
		<a href="https://dmac.pharmacy.arizona.edu/Ares/redirect_uri?logout=https://dmac.pharmacy.arizona.edu" class="item">
		<h4>Logout</h4>
		</a>		
			<div class="ui float right dropdown link item">
   		 		<span class="text">Menu</span>
    					<div class="menu">    
      						<a class="item" href="http://dmac.pharmacy.arizona.edu"> Home </a>
      						<a class="item" href="http://dmac.pharmacy.arizona.edu/Oracle/Develop"> Oracle </a>
						      							
					</div>
			</div>
		</div>

	</div>
	</header>

<div class="ui container appBody" style = "padding-top: 140px;">

')


flaguser = 1


cat('<div id="loader1" class="ui disabled inverted dimmer">
    <div class="ui text loader">Loading</div>
  </div>

')

if(flaguser == 1){

if(is.null(POST)){

cat('

	<form enctype="multipart/form-data" method="POST" action="https://dmac.pharmacy.arizona.edu/Oracle/PredictDevelop">
	
        <h2> Gene to Gene networks </h2>
	<div class="ui container" >
	<div class="ui fluid segment">
	<div class="ui four column grid">
	<div class="row">
		<div class="column" style = "text-align: center;"> <h4> Gene 1 </h4> </div>
		<div class="column" style = "text-align: center;"> <h4> Gene 1 Status </h4> </div>
    		<div class="column" style = "text-align: center;"> <h4> Gene 2 </h4> </div>
    		<div class="column" style = "text-align: center;"> <h4> Path </h4> </div>
	</div>

	<div class="row">	
		<div class="column" style="text-align:center;">
		
		<input type="text"  name="gene1db" id="gene1db">
		
		</div>

               <div class="column" style="text-align:center;">
		
		<input type="text"  name="gene1status" id="gene1status">
		
		</div>


    		<div class="column" style="text-align:center;"> 
			
			<input type="text" name="gene2db" id="gene2db">
			
		</div>

    		<div class="column" style="text-align:center;"> 
		
		<input type="text" name = "pathway" id="pathway">
		
		</div>

	</div>
	<div class = "row"><div class = "column" style = "  display: flex; justify-content: center;"> <input type="submit" name="Upload" value = "Submit"> </div> </div>
	</div>
	</div>

	<br>

	<div class="ui divider"></div>
	
	<h2> Single Gene Network </h2>

	<div class="ui fluid segment">

		<div class="column" style="text-align:left;">
		
		<h4> Enter Gene: </h4> <input type="text"  name="gene3db" id="gene3db">  <input type="submit" name="UploadSingle" value = "Submit"> 
		
		</div>


	</div>

	<br>

	<div class="ui divider"></div>


	<h2> Multiple Gene Network </h2>
<div class="ui fluid segment">
	<div class="ui two column grid">
	<div class="row">
		<div class="column" style = "text-align: center;"> <h4> Up-regulated Genes </h4> </div>
		<div class="column" style = "text-align: center;"> <h4> Down-regulated Genes </h4> </div>
	</div>
	<div class="row">
		<div class="column" style = "text-align: center;"> <textarea name="Text1" cols="60" rows="20"></textarea>
 		</div>
		<div class="column" style = "text-align: center;"> <textarea name="Text2" cols="60" rows="20"></textarea>
 		</div>
	</div>

	<div class="row">
		<div class="column" style = "text-align: center;">  <input type="submit" name="UploadTextArea" value = "Submit"></textarea>
 		</div>
	</div>
	

	</div>

</div>

<br>
')

}

if(!is.null(POST[5])){
if(names(POST[5]) == "Upload"){

gene1status <- as.character(POST[[2]])
origin <- as.character(POST[[1]])
destination <- as.character(POST[[3]])
option <- as.character(POST[[4]])

if(identical(gene1status,character(0)) || identical(origin,character(0)) || identical(destination,character(0)) || identical(option,character(0))){

cat('<div>Please use all inputs </div>')

}else{

con <- dbConnect(RPostgres::Postgres())

query1 <- dbReadTable(con, "table1")

dbDisconnect(con)

query1 <- as.data.frame(query1)

nodes<- sort(unique(c(query1$gene1, query1$gene2)))
y <- as.integer(as.factor(nodes))
names(y) = nodes


graph_1 <- create_graph()
graph_1 <- add_n_nodes(graph_1,n = length(nodes))

for(temp_gene in nodes)
{

if(!identical(query1[query1$gene1 == temp_gene,]$gene2,character(0))){

for(tempgene2 in unique(query1[query1$gene1 == temp_gene,]$gene2))
{

if(checkreal(temp_gene,tempgene2,query1) != 0){
#temp_text <- paste0("graph_1 <- add_edge(graph_1,from = ",y[[temp_gene]],", to =", y[[tempgene2]],")")
graph_1 <- add_edge(graph_1,from = y[[temp_gene]], to = y[[tempgene2]])

}

}

}

}

res <- try(
if(option == "Shortest"){
nodesF <- get_paths(graph_1,from = y[[origin]],to = y[[destination]],shortest_path = TRUE)
} else if(option == "All"){
nodesF <- get_paths(graph_1,from = y[[origin]],to = y[[destination]])
}else if(option == "Longest"){
nodesF <- get_paths(graph_1,from = y[[origin]],to = y[[destination]],longest_path = TRUE)

}

)

if(class(res) == "try-error")
{
cat('<div> One of these genes are not in the network </div>')
nodes <- NA
}



if(!is.na(nodesF)){

cat('<h3>Prediction</h3>')

for(z in 1:length(nodesF)){

nodes <- names(y[nodesF[[z]]])
	
randbase = paste0("analysis",as.integer(rnorm(1,100000,100)),as.integer(rnorm(1,100000,100)),as.integer(rnorm(1,100000,100)))

dir.create(file.path("/Oracle/predictsessions", randbase))

file.copy(from="/Oracle/Network2.html", to=file.path("/Oracle/predictsessions", randbase,"Network.html"), 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

sink(file.path("/Oracle/predictsessions", randbase,"Rdata.js"))

cat("let text_1 = '")
cat('{"nodes": [')


if(gene1status == "Down-Regulated"){
cat('{"id": "')
cat(nodes[1])
cat('","name": "')
cat(nodes[1])
cat('","val": 10,"color": "black"},')
FinalPredict <- -1;
}else{
cat('{"id": "')
cat(nodes[1])
cat('","name": "')
cat(nodes[1])
cat('","val": 10,"color": "#D37600"},')
FinalPredict <- 1;
}

for(i in 2:length(nodes)){

cat('{"id": "')
cat(nodes[i])
cat('","name": "')
cat(nodes[i])
cat('","val": 10,"color": "')

tempcon <- checkreal(nodes[i-1],nodes[i],query1)

FinalPredict <- FinalPredict*tempcon


if(FinalPredict < 0)
{
cat('black')
}else{
cat('#D37600')
}

cat('"}')
if(i != length(nodes)){
cat(',')
}
}

cat("]");
cat(',"links": [')

FinalPredict <- 1;

for( i in 1:(length(nodes)-1))
{
cat('{"source": "')
cat(nodes[i])
cat('","target": "')
cat(nodes[i+1])
cat('","connection": "')

tempcon <- checkreal(nodes[i],nodes[i+1],query1)

if(tempcon == -1){
FinalPredict <- FinalPredict*(-1)
cat("red")} else if(tempcon == 1){
cat("green")} else {
cat("grey")
}

cat('"}')

if(i != (length(nodes)-1)){
cat(',')
}

}

cat(']}')
cat("'")
sink()

color1 <- NULL

if(gene1status == "Down-Regulated"){
FinalPredict <- FinalPredict*(-1)
color1 <- "red"
}else{
color1 <- "green"
}

cat('
<div class="ui fluid segment"> <p class="p-t-15" style = "font-size: large; ">')

if(FinalPredict > 0){

cat("When ")
cat(origin)
cat(' is <span style="color: ')
cat(color1)
cat('">')
cat(gene1status)
cat("</span> then ")
cat(destination)
cat(' is <span style="color:green">promoted</span>.')

} else{

cat("When ")
cat(origin)
cat(' is <span style="color: ')
cat(color1)
cat('">')
cat(gene1status)
cat("</span> then ")
cat(destination)
cat(' is <span style="color:red">surpressed</span>.')

}

cat('</p>

<div><p> Pathway: ')

for(i in 1:length(nodes)){

cat(nodes[i])
if(i < length(nodes)){
cat('->')
}

}

cat(' </p> </div>


</div>')


cat('<div> <button class="ui button" onclick="visualize(this.name)" name = "')

cat(paste0('http://dmac.pharmacy.arizona.edu/Oracle/predictsessions/',randbase,'/Network.html'))

cat('">
  Visualize
</button> </div>
')
}

}else{

cat('<h3>Prediction</h3>
<div class="ui fluid segment"> <p class="p-t-15" style = "font-size: large; ">
No connection found. </p></div>')
}
}
	
}
}

#########################################################################################################################################################################
#########################################################################################################################################################################
#########################################################################################################################################################################


if(!is.null(POST[6])){
if(names(POST[6]) == "UploadSingle"){


origin <- as.character(POST[[5]])

if(identical(origin,character(0))){

cat('<div>Please use all inputs </div>')

}else{

con <- dbConnect(RPostgres::Postgres())

query1 <- dbReadTable(con, "table1")

dbDisconnect(con)

query1 <- as.data.frame(query1)

nodes<- sort(unique(c(query1$gene1, query1$gene2)))
y <- as.integer(as.factor(nodes))
names(y) = nodes


graph_1 <- create_graph()
graph_1 <- add_n_nodes(graph_1,n = length(nodes))

for(temp_gene in nodes)
{

if(!identical(query1[query1$gene1 == temp_gene,]$gene2,character(0))){

for(tempgene2 in unique(query1[query1$gene1 == temp_gene,]$gene2))
{

checkrhold <- checkreal2(temp_gene,tempgene2,query1)

if(checkrhold  != 0){
#temp_text <- paste0("graph_1 <- add_edge(graph_1,from = ",y[[temp_gene]],", to =", y[[tempgene2]],")")

if(checkrhold != 2 && temp_gene == origin)
{
ran = 0

} else{ 
graph_1 <- add_edge(graph_1,from = y[[temp_gene]], to = y[[tempgene2]])
}

}

}

}

}

res <- try(

{
nodesF <- get_paths(graph_1,from = y[[origin]])

}

)

if(class(res) == "try-error")
{
cat('<div> One of these genes are not in the network </div>')
nodesF <- NA
}



if(!is.na(nodesF)){

gene1status <- "Up-Regulated"

cat('<h3>Prediction</h3>')

for(z in 1:length(nodesF)){

nodes <- names(y[nodesF[[z]]])

destination <- nodes[length(nodes)]
	
randbase = paste0("analysis",as.integer(rnorm(1,100000,100)),as.integer(rnorm(1,100000,100)),as.integer(rnorm(1,100000,100)))

dir.create(file.path("/Oracle/predictsessions", randbase))

file.copy(from="/Oracle/Network2.html", to=file.path("/Oracle/predictsessions", randbase,"Network.html"), 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

sink(file.path("/Oracle/predictsessions", randbase,"Rdata.js"))

cat("let text_1 = '")
cat('{"nodes": [')


if(gene1status == "Down-Regulated"){
cat('{"id": "')
cat(nodes[1])
cat('","name": "')
cat(nodes[1])
cat('","val": 10,"color": "black"},')
FinalPredict <- -1;
}else{
cat('{"id": "')
cat(nodes[1])
cat('","name": "')
cat(nodes[1])
cat('","val": 10,"color": "#D37600"},')
FinalPredict <- 1;
}

for(i in 2:length(nodes)){

cat('{"id": "')
cat(nodes[i])
cat('","name": "')
cat(nodes[i])
cat('","val": 10,"color": "')

tempcon <- checkreal(nodes[i-1],nodes[i],query1)

FinalPredict <- FinalPredict*tempcon


if(FinalPredict < 0)
{
cat('black')
}else{
cat('#D37600')
}

cat('"}')
if(i != length(nodes)){
cat(',')
}
}

cat("]");
cat(',"links": [')

FinalPredict <- 1;

for( i in 1:(length(nodes)-1))
{
cat('{"source": "')
cat(nodes[i])
cat('","target": "')
cat(nodes[i+1])
cat('","connection": "')

tempcon <- checkreal(nodes[i],nodes[i+1],query1)

if(tempcon == -1){
FinalPredict <- FinalPredict*(-1)
cat("red")} else if(tempcon == 1){
cat("green")} else {
cat("grey")
}

cat('"}')

if(i != (length(nodes)-1)){
cat(',')
}

}

cat(']}')
cat("'")
sink()

color1 <- NULL

if(gene1status == "Down-Regulated"){
FinalPredict <- FinalPredict*(-1)
color1 <- "red"
}else{
color1 <- "green"
}

cat('
<div class="ui fluid segment"> <p class="p-t-15" style = "font-size: large; ">')

if(FinalPredict > 0){

cat("When ")
cat(origin)
cat(' is <span style="color: ')
cat(color1)
cat('">')
cat(gene1status)
cat("</span> then ")
cat(destination)
cat(' is <span style="color:green">promoted</span>.')

} else{

cat("When ")
cat(origin)
cat(' is <span style="color: ')
cat(color1)
cat('">')
cat(gene1status)
cat("</span> then ")
cat(destination)
cat(' is <span style="color:red">surpressed</span>.')

}


cat('</p>

<div><p> Pathway: ')

for(i in 1:length(nodes)){

cat(nodes[i])
if(i < length(nodes)){
cat('->')
}

}

cat(' </p> </div>


</div>')



cat('<div> <button class="ui button" onclick="visualize(this.name)" name = "')

cat(paste0('http://dmac.pharmacy.arizona.edu/Oracle/predictsessions/',randbase,'/Network.html'))

cat('">
  Visualize
</button> </div>
')
}

}else{

cat('<h3>Prediction</h3>
<div class="ui fluid segment"> <p class="p-t-15" style = "font-size: large; ">
No connection found. </p></div>')
}
}
	
}
}

#########################################################################################################################################################################
#########################################################################################################################################################################
#########################################################################################################################################################################


 if(length(POST) > 7){
if(!is.null(POST[8])){
if(names(POST[8]) == "UploadTextArea"){

upreg = unlist(strsplit(as.character(POST[[6]]), split=","))
downreg = unlist(strsplit(as.character(POST[[7]]), split=","))

con <- dbConnect(RPostgres::Postgres())

query1 <- dbReadTable(con, "table1")

dbDisconnect(con)

query1 <- as.data.frame(query1)

nodes<- sort(unique(c(query1$gene1, query1$gene2)))
y <- as.integer(as.factor(nodes))
names(y) = nodes


graph_1 <- create_graph()
graph_1 <- add_n_nodes(graph_1,n = length(nodes))

for(temp_gene in nodes)
{

if(!identical(query1[query1$gene1 == temp_gene,]$gene2,character(0))){

for(tempgene2 in unique(query1[query1$gene1 == temp_gene,]$gene2))
{

if(checkreal(temp_gene,tempgene2,query1) != 0){
#temp_text <- paste0("graph_1 <- add_edge(graph_1,from = ",y[[temp_gene]],", to =", y[[tempgene2]],")")
graph_1 <- add_edge(graph_1,from = y[[temp_gene]], to = y[[tempgene2]])

}

}

}

}




res <- try(

{

cat('<h3>Prediction</h3>')

indx = 1
fullgenehold = NULL

fullgeneset = append(upreg,downreg)


for(upgene in fullgeneset ){

nodesF <- get_paths(graph_1,to = y[[upgene]])
if(!is.na(nodesF)){

gene1status <- "Up-Regulated"


for(z in 1:length(nodesF)){

nodes <- names(y[nodesF[[z]]])

checkrhold <- checkreal2(nodes[length(nodes)-1],nodes[length(nodes)],query1)

if(checkrhold == 2){

fullgenehold[indx] = nodesF[z]
indx = indx  + 1



destination <- nodes[length(nodes)]

	
randbase = paste0("analysis",as.integer(rnorm(1,100000,100)),as.integer(rnorm(1,100000,100)),as.integer(rnorm(1,100000,100)))

dir.create(file.path("/Oracle/predictsessions", randbase))

file.copy(from="/Oracle/Network2.html", to=file.path("/Oracle/predictsessions", randbase,"Network.html"), 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

sink(file.path("/Oracle/predictsessions", randbase,"Rdata.js"))

cat("let text_1 = '")
cat('{"nodes": [')


if(gene1status == "Down-Regulated"){
cat('{"id": "')
cat(nodes[1])
cat('","name": "')
cat(nodes[1])
cat('","val": 10,"color": "black"},')
FinalPredict <- -1;
}else{
cat('{"id": "')
cat(nodes[1])
cat('","name": "')
cat(nodes[1])
cat('","val": 10,"color": "#D37600"},')
FinalPredict <- 1;
}

for(i in 2:length(nodes)){

cat('{"id": "')
cat(nodes[i])
cat('","name": "')
cat(nodes[i])
cat('","val": 10,"color": "')

tempcon <- checkreal(nodes[i-1],nodes[i],query1)

FinalPredict <- FinalPredict*tempcon


if(FinalPredict < 0)
{
cat('black')
}else{
cat('#D37600')
}

cat('"}')
if(i != length(nodes)){
cat(',')
}
}

cat("]");
cat(',"links": [')

FinalPredict <- 1;

for( i in 1:(length(nodes)-1))
{
cat('{"source": "')
cat(nodes[i])
cat('","target": "')
cat(nodes[i+1])
cat('","connection": "')

tempcon <- checkreal(nodes[i],nodes[i+1],query1)

if(tempcon == -1){
FinalPredict <- FinalPredict*(-1)
cat("red")} else if(tempcon == 1){
cat("green")} else {
cat("grey")
}

cat('"}')

if(i != (length(nodes)-1)){
cat(',')
}

}

cat(']}')
cat("'")
sink()

color1 <- NULL

if(gene1status == "Down-Regulated"){
FinalPredict <- FinalPredict*(-1)
color1 <- "red"
}else{
color1 <- "green"
}

cat('
<div class="ui fluid segment"> <p class="p-t-15" style = "font-size: large; ">')

if(FinalPredict > 0){

cat("When ")
cat(origin)
cat(' is <span style="color: ')
cat(color1)
cat('">')
cat(gene1status)
cat("</span> then ")
cat(destination)
cat(' is <span style="color:green">promoted</span>.')

} else{

cat("When ")
cat(origin)
cat(' is <span style="color: ')
cat(color1)
cat('">')
cat(gene1status)
cat("</span> then ")
cat(destination)
cat(' is <span style="color:red">surpressed</span>.')

}


cat('</p>

<div><p> Pathway: ')

for(i in 1:length(nodes)){

cat(nodes[i])
if(i < length(nodes)){
cat('->')
}

}

cat(' </p> </div>


</div>')



cat('<div> <button class="ui button" onclick="visualize(this.name)" name = "')

cat(paste0('http://dmac.pharmacy.arizona.edu/Oracle/predictsessions/',randbase,'/Network.html'))

cat('">
  Visualize
</button> </div>
')
}

}

}else{

cat('<h3>Prediction</h3>
<div class="ui fluid segment"> <p class="p-t-15" style = "font-size: large; ">
No connection found. </p></div>')
}




}

if(is.null(fullgenehold)){

cat('<h3>Prediction</h3>
<div class="ui fluid segment"> <p class="p-t-15" style = "font-size: large; ">
No connections found. </p></div>')


}

}

)

if(class(res) == "try-error")
{
cat('<div> One of these genes are not in the network </div>')
nodesF <- NA
}








}
}
}


} else
{

cat('
<p class="p-t-15" style = "font-size: large; ">
		There was a problem finding your account or you do not have permission to access this app. Please contact us for assistance.  </p>

')

}



cat('
</div>

<script src=https://clinicaltables.nlm.nih.gov/autocomplete-lhc-versions/18.1.3/autocomplete-lhc.min.js></script>

<script> function visualize(this_name) {
window.open(this_name);

}

</script>

<script>

var connectionsArray = ["Shortest","Longest","All"].sort();
var statusArray = ["Up-Regulated","Down-Regulated"].sort();

new Def.Autocompleter.Prefetch("pathway", connectionsArray);
new Def.Autocompleter.Prefetch("gene1status", statusArray);
new Def.Autocompleter.Search("gene1db",
  "https://clinicaltables.nlm.nih.gov/api/ncbi_genes/v3/search?sf=Symbol,Synonyms&df=Symbol");

new Def.Autocompleter.Search("gene2db",
  "https://clinicaltables.nlm.nih.gov/api/ncbi_genes/v3/search?sf=Symbol,Synonyms&df=Symbol");

new Def.Autocompleter.Search("gene3db",
  "https://clinicaltables.nlm.nih.gov/api/ncbi_genes/v3/search?sf=Symbol,Synonyms&df=Symbol");

</script>

<script>
  function loading1() {
  
	document.getElementById("loader1").className = "ui active inverted dimmer";
  
}
</script>

<script type = "text/javascript" src="/AresC/jquery.min.js"></script>
<script type = "text/javascript" src="/AresC/semantic.min.js"></script>

<script type = "text/javascript">
$(".menu .item")
  .tab()
;

</script>
<script type = "text/javascript"> 

$(".ui.dropdown")
  .dropdown()
;

</script>

</body>
</html>
')


