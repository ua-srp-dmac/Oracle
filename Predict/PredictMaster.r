library("RPostgres")
library('org.Hs.eg.db')
library("stringr")
library("cppRouting")


checkreal <- function(gene1,gene2,query1){

C1 <- length(query1[which(query1$gene1 == gene1 & query1$gene2 == gene2 & query1$connection == "Promotes"),]$pmid)

C2 <- length(query1[which(query1$gene1 == gene1 & query1$gene2 == gene2 & query1$connection == "Suppresses"),]$pmid)

C3 <- length(query1[which(query1$gene1 == gene1 & query1$gene2 == gene2 & query1$connection == "Transcription Target"),]$pmid)

if(C1+C3 == C2){
return(1)
} else if(C1+C3 > C2)
{return(1)} else
{return(-1)}

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
  shortest
}

path_length <- function(path) {
  # if path is NULL return infinite length
  if (is.null(path)) return(Inf)
  
  # get all consecutive nodes
  pairs <- cbind(values = path[-length(path)], ind = path[-1])
  # join with G and sum over weights
  sum(merge(pairs, G)[ , "weights"])
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
		<a class="header item" href="http://dmac.pharmacy.arizona.edu/Oracle/Home">
		<h1>Oracle</h1>
		</a>
		<a class="item" href="https://dmac.pharmacy.arizona.edu/Oracle/Network.html">
		<h4> Network </h4>
		</a>
		<a class="item" href="http://dmac.pharmacy.arizona.edu/Oracle/Predict">
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
      						<a class="item" href="http://dmac.pharmacy.arizona.edu/Oracle/Home"> Oracle </a>
						      							
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

	<form enctype="multipart/form-data" method="POST" action="https://dmac.pharmacy.arizona.edu/Oracle/Predict">
	
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
')

}

if(!is.null(POST[5])){
if(names(POST[5]) != "UploadFinal"){

gene1status <- as.character(POST[[2]])
origin <- as.character(POST[[1]])
destination <- as.character(POST[[3]])

usedb <- NULL
usedb$app <- "Oracle"
usedb$username <- SERVER$headers_in$OIDC_CLAIM_preferred_username

usedb <- as.data.frame(usedb)

con2 <- dbConnect(RPostgres::Postgres())

dbWriteTable(con2, "table1u", value = usedb, append = TRUE, row.names = FALSE)

dbDisconnect(con2)


con <- dbConnect(RPostgres::Postgres())

query1 <- dbReadTable(con, "table1")

dbDisconnect(con)

query1 <- as.data.frame(query1)

nodes<- unique(c(query1$gene1, query1$gene2))

graph_1 <- NULL
weights <- NULL

for(temp_gene in nodes)
{
if(identical(query1[query1$gene1 == temp_gene,]$gene2,character(0))){
temp_text <- paste0('graph_1$',temp_gene,'<- ""')
eval(parse( text=temp_text ))
temp_text <- paste0("weights$",temp_gene,"<- 0 ")
eval(parse( text=temp_text ))

}else{
temp_text <- paste0("graph_1$",temp_gene,"<- unique(query1[query1$gene1 == temp_gene,]$gene2)")
eval(parse( text=temp_text ))
temp_text <- paste0("weights$",temp_gene,"<- rep(1,length(unique(query1[query1$gene1 == temp_gene,]$gene2)))")
eval(parse( text=temp_text ))
}
}


G <- data.frame(stack(graph_1), weights = stack(weights)[[1]])

nodes <- find_shortest_path(graph_1, origin, destination)

if(!is.null(nodes)){
	
randbase = paste0("analysis",as.integer(rnorm(1,100000,100)),as.integer(rnorm(1,100000,100)),as.integer(rnorm(1,100000,100)))

dir.create(file.path("/Oracle", randbase))

file.copy(from="/Oracle/Network2.html", to=file.path("/Oracle", randbase,"Network.html"), 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

sink(file.path("/Oracle", randbase,"Rdata.js"))

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
<h3>Prediction</h3>
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

cat('</p></div>')


cat('<div> <button class="ui button" onclick="visualize(this.name)" name = "')

cat(paste0('http://dmac.pharmacy.arizona.edu/Oracle/',randbase,'/Network.html'))

cat('">
  Visualize
</button> </div>
')
}else{

cat('<h3>Prediction</h3>
<div class="ui fluid segment"> <p class="p-t-15" style = "font-size: large; ">
No connection found. </p></div>')
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

var connectionsArray = ["Shortest"].sort();
var statusArray = ["Up-Regulated","Down-Regulated"].sort();

new Def.Autocompleter.Prefetch("pathway", connectionsArray);
new Def.Autocompleter.Prefetch("gene1status", statusArray);
new Def.Autocompleter.Search("gene1db",
  "https://clinicaltables.nlm.nih.gov/api/ncbi_genes/v3/search?sf=Symbol,Synonyms&df=Symbol");

new Def.Autocompleter.Search("gene2db",
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


