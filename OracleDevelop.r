library("RPostgres")
library('org.Hs.eg.db')
library("stringr")

check_dup <- function(x){

query <- dbReadTable(con, "table1")

query <- as.data.frame(query)

if(identical(x$pmid,numeric(0)))
{

if(dim(query[which(query$gene1 == x$gene1 & query$connection == x$connection & query$gene2 == x$gene2 & query$username == x$username),])[1] > 0)
{return(TRUE)} else {
return(FALSE)}

} else{

if(dim(query[which(query$gene1 == x$gene1 & query$connection == x$connection & query$gene2 == x$gene2 & query$pmid == x$pmid & query$username == x$username),])[1] > 0)
{return(TRUE)} else {
return(FALSE)}


}
}

homedata <- "/var/www/data"

usremail <- SERVER$headers_in$OIDC_CLAIM_email
username <- SERVER$headers_in$OIDC_CLAIM_preferred_username

Aperm2 <- 0
Aperm3 <- 0
 

if(usremail == "nirav@email.arizona.edu" || usremail == "myung@email.arizona.edu" ||  usremail == "ooi@pharmacy.arizona.edu" || usremail == "avicenti@email.arizona.edu"){

Aperm2 <- 1
Aperm3 <- 1

}

flaguser = 1

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
		</a>')
if(Aperm3 ==1)
{
cat('
<a class="item" href="https://dmac.pharmacy.arizona.edu/Oracle/cf15f53/2a8e8cc80651cba9a1864efc9c24af9cf15f5322fb4d01f065504aabd6daa992">
<h4> Total Network </h4>
</a>
<a class="item" href="https://dmac.pharmacy.arizona.edu/Oracle/9a7bd120f80c3f796b2af9df1260d9cd19621831686c72">
<h4> Usage </h4>
</a>

')
}
		cat('<a class="item" href="https://dmac.pharmacy.arizona.edu/Oracle/Temp/Network.html">
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



cat('<div id="loader1" class="ui disabled inverted dimmer">
    <div class="ui text loader">Loading</div>
  </div>

')



flag = 1

if(flag == 1){

#pstate = paste0("Delete FROM table 1 WHERE gene1 = '","ASD1","' and Connection = '", "Promotes", "' and gene2 = '","DDC"

#dbSendQuery(con, statement = "DELETE FROM table1 WHERE username = 'avicenti';")

}
		
#query1 <- dbReadTable(con, "table1")

#print(query1)

if(flaguser == 1){

if(length(POST) > 5)
{
if(!is.null(POST[6])){
if(str_split(names(POST[6]),"_")[[1]][1] == "dtable"){

con <- dbConnect(RPostgres::Postgres())

query1 <- dbReadTable(con, "table1")


lineD <- as.numeric(str_split(names(POST[6]),"_")[[1]][2])

query1 <- as.data.frame(query1)
query1 <-query1[query1$username == as.character(SERVER$headers_in$OIDC_CLAIM_preferred_username),]

eid <- query1[lineD,]$entry_id

fstate <- paste0("DELETE FROM table1 WHERE entry_id = ",eid,";")

dbSendQuery(con, statement = fstate)

dbDisconnect(con)

system("R CMD BATCH  /var/www/Rfiles/Oracle/OracleScripts/UpdateMain.r")

cat('<script> window.location.replace("https://dmac.pharmacy.arizona.edu/Oracle/Develop") </script> ')
}

} 

}

if(is.null(POST)){

cat('

	<form enctype="multipart/form-data" method="POST" action="https://dmac.pharmacy.arizona.edu/Oracle/Develop">
	
	<div class="ui container" >
	<div class="ui fluid segment">
	<div class="ui four column grid">
	<div class="row">
		<div class="column" style = "text-align: center;"> <h4> Gene 1 </h4> </div>
    		<div class="column" style = "text-align: center;"> <h4> Connection </h4> </div>
    		<div class="column" style = "text-align: center;"> <h4> Gene 2 </h4> </div>
		<div class="column" style = "text-align: center;"> <h4> PMID </h4> </div>	
	</div>

	<div class="row">	
		<div class="column" style="text-align:center;">
		
		<input type="text"  name="gene1db" id="gene1db">
		
		</div>

    		<div class="column" style="text-align:center;"> 
		
		<input type="text" name = "connections" id="connections">
		
		</div>

    		<div class="column" style="text-align:center;"> 
			
			<input type="text" name="gene2db" id="gene2db">
			
		</div>

		<div class="column" style="text-align:center;"> <input type="number" id="PMID" name="PMID" min="1"> </div>

	</div>
	<div class = "row"><div class = "column" style = "  display: flex; justify-content: center;"> <input type="submit" name="Upload" value = "Upload"> </div> </div>
	</div>
	</div>

<br>
	<div class="ui divider"></div>

	<h2> Bulk Upload </h2>
	<div class="ui fluid segment">
	<input type="file" name="FirstFile" accept=".csv">
	<input type="submit" name="UploadFile" value = "UploadFile">
	</div>
')

} else if(!is.null(POST[5])){

if(names(POST[5]) == "UploadFinal"){

cat('

	<form enctype="multipart/form-data" method="POST" action="https://dmac.pharmacy.arizona.edu/Oracle/Develop">
	
	<div class="ui container" >
	<div class="ui fluid segment">
	<div class="ui four column grid">
	<div class="row">
		<div class="column" style = "text-align: center;"> <h4> Gene 1 </h4> </div>
    		<div class="column" style = "text-align: center;"> <h4> Connection </h4> </div>
    		<div class="column" style = "text-align: center;"> <h4> Gene 2 </h4> </div>
		<div class="column" style = "text-align: center;"> <h4> PMID </h4> </div>	
	</div>

	<div class="row">	
		<div class="column" style="text-align:center;">
		
		<input type="text"  name="gene1db" id="gene1db">
		
		</div>

    		<div class="column" style="text-align:center;"> 
		
		<input type="text" name = "connections" id="connections">
		
		</div>

    		<div class="column" style="text-align:center;"> 
			
			<input type="text" name="gene2db" id="gene2db">
			
		</div>

		<div class="column" style="text-align:center;"> <input type="number" id="PMID" name="PMID" min="1"> </div>

	</div>
	<div class = "row"><div class = "column" style = "  display: flex; justify-content: center;"> <input type="submit" name="Upload" value = "Upload"> </div> </div>
	</div>
	</div>

<br>
	<div class="ui divider"></div>

	<h2> Bulk Upload </h2>
	<div class="ui fluid segment">
	<input type="file" name="FirstFile" accept=".csv">
	<input type="submit" name="UploadFile" value = "UploadFile">
	</div>
')


}

}


if(!is.null(POST[5])){
if(names(POST[5]) == "Upload"){
x <- NULL

x$gene1 <- as.character(POST[[1]])
x$connection <- as.character(POST[[2]])
x$gene2 <- as.character(POST[[3]])
x$pmid <- as.numeric(POST[[4]])
x$username <-as.character(SERVER$headers_in$OIDC_CLAIM_preferred_username)

cat('
<form enctype="multipart/form-data" method="POST" action="https://dmac.pharmacy.arizona.edu/Oracle/Develop">
<input type="hidden" name="gene1db" id="gene1db" value="') 
cat(x$gene1) 
cat('">
<input type="hidden" name="connectiondb" id="connectiondb" value = "')
cat(x$connection) 
cat('">
<input type="hidden" name="gene2db" id="gene2db" value = "')
cat(x$gene2)
cat('">
<input type="hidden" name="pmiddb" id="pmiddb" value = "')
cat(x$pmid)
cat('">

<div class="ui container">
	<div class="ui fluid segment">
	<div class="ui four column grid">
	<div class="row">
		<div class="column" style = "text-align: center;"> <h4> Gene 1 </h4> </div>
    		<div class="column" style = "text-align: center;"> <h4> Connection </h4> </div>
    		<div class="column" style = "text-align: center;"> <h4> Gene 2 </h4> </div>
		<div class="column" style = "text-align: center;"> <h4> PMID </h4> </div>	
	</div>

	<div class="row">	
		<div class="column" style = "text-align: center;">')
		if(identical(x$gene1, character(0))){ cat('<i class="exclamation triangle icon"></i>')}else{
		cat(x$gene1)}
		cat('</div>
    		<div class="column" style = "text-align: center;">') 
		if(identical(x$connection, character(0))){ cat('<i class="exclamation triangle icon"></i>')}else{
		cat(x$connection)}
		cat('</div>
    		<div class="column" style = "text-align: center;">') 
		if(identical(x$gene2, character(0))){ cat('<i class="exclamation triangle icon"></i>')}else{
		cat(x$gene2)}
		cat('</div>
		<div class="column" style = "text-align: center;">') 
		if(identical(x$pmid, numeric(0))){ if(Aperm2 != 1){ cat('<i class="exclamation triangle icon"></i>')}else{cat('<i class="exclamation icon"></i>')}}else{
		cat(x$pmid)} 
		cat('</div>	
	</div>')

con <- dbConnect(RPostgres::Postgres())


if(check_dup(x)){
cat(' <h4 style = "text-align: center;"> <i class="exclamation triangle icon"></i> This connection is already in the database!  </h4> <br> ')
} else{

if(Aperm2 != 1){
	if(identical(x$gene1, character(0)) || identical(x$connection, character(0)) || identical(x$gene2, character(0)) || identical(x$pmid, numeric(0))) {
	cat('<h4 style = "text-align: center;"> <i class="exclamation triangle icon"></i> Missing input! </h4> <br> ')

	} else{
	
	gene2pubmed <- read.csv("gene2pubmedclean.csv")
	
	check1 <- dim(gene2pubmed[which(gene2pubmed$V2== x$gene1 & gene2pubmed$V3 == x$pmid),])[1]
	check2 <- dim(gene2pubmed[which(gene2pubmed$V2== x$gene2 & gene2pubmed$V3 == x$pmid),])[1]
	
	if(check1 + check2 == 0){
	   cat('<h4 style = "text-align: center;"> <i class="exclamation triangle icon"></i> No association found between either gene and PMID. Try a different input or contact us for support.  </h4> <br> ')
	} else{
	
	cat('<div class = "row"><div class = "column" style = "  display: flex; justify-content: center;"> <input type="submit" name="UploadFinal" value = "Final Upload"> </div> </div> ')

	}
	
	}
} else{
	if(identical(x$gene1, character(0)) || identical(x$connection, character(0)) || identical(x$gene2, character(0))) {
	cat('<h4 style = "text-align: center;"> <i class="exclamation triangle icon"></i> Missing input! </h4> <br> ')

	} else{

	gene2pubmed <- read.csv("gene2pubmedclean.csv")
	
	check1 <- dim(gene2pubmed[which(gene2pubmed$V2== x$gene1 & gene2pubmed$V3 == x$pmid),])[1]
	check2 <- dim(gene2pubmed[which(gene2pubmed$V2== x$gene2 & gene2pubmed$V3 == x$pmid),])[1]
	
	if(check1 + check2 == 0){
	   cat('<h4 style = "text-align: center;"> <i class="exclamation triangle icon"></i> No association found between either gene and PMID. Your account is allowed to submit anyways.  </h4> <br> ')
	}

	cat('
	<div class = "row"><div class = "column" style = "  display: flex; justify-content: center;"> <input type="submit" name="UploadFinal" value = "Final Upload"> </div> </div> ')
	}

}

}

dbDisconnect(con)

	cat('
	</div>
	</div>
</div>
')
}		
}


if(!is.null(POST[5])){
if(names(POST[5]) == "UploadFinal"){

x <- NULL

x$gene1 <- as.character(POST[[1]])
x$connection <- as.character(POST[[2]])
x$gene2 <- as.character(POST[[3]])
x$pmid <- as.numeric(POST[[4]])
x$username <-as.character(SERVER$headers_in$OIDC_CLAIM_preferred_username)

if(identical(x$pmid, numeric(0))){
x$pmid <-NULL
}
x <- as.data.frame(x)

con <- dbConnect()


dbWriteTable(con, "table1", value = x, append = TRUE, row.names = FALSE)

dbDisconnect(con)

system("R CMD BATCH  /var/www/Rfiles/Oracle/OracleScripts/UpdateMain.r")

cat('<script> window.location.replace("https://dmac.pharmacy.arizona.edu/Oracle/Develop") </script> ')


}
}


#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################

 if(length(POST) > 5){
	if(!is.null(names(POST[6]))){
      if(names(POST[6]) == "UploadFile"){
        
        if(!is.null(FILES$FirstFile$name)){
          
          res <- try({
            oraclecsv <- read.csv(FILES$FirstFile$tmp_name)
          },silent = TRUE)
          
          if(class(res) == "try-error")
          {
            cat('<div> Could not read file</div>')
          } else
          {
            
            errflag = 0
            
            res <- try({
              gene1 <- oraclecsv[,1]
            })
            if(class(res) == "try-error")
            {
              errflag = 1
              cat('<div> Error trying to parse column 1 - gene 1</div>')
            }
            
            res <- try({
              connection <- oraclecsv[,2]
            })
            if(class(res) == "try-error")
            {
              errflag = 1
              cat('<div> Error trying to parse column 2 - connection</div>')
            }
            
            res <- try({
              gene2 <- oraclecsv[,3]
            })
            if(class(res) == "try-error")
            {
              errflag = 1
              cat('<div> Error trying to parse column 3 - gene 1</div>')
            }
            
            res <- try({
              pmid <- oraclecsv[,4]
            })
            if(class(res) == "try-error")
            {
              errflag = 1
              cat('<div> Error trying to parse column 4 - pmid<div>')
            }
            
            
          }
          
          if(errflag != 1){
            
            l1 = sum(!is.na(gene1[gene1 != ""]))
            l2 =sum(!is.na(connection[connection != ""]))
            l3 =sum(!is.na(gene2[gene2 != ""]))
            l4 =sum(!is.na(pmid[pmid != ""]))
            
            if(l1 == l2 && l1 == l2 && l1 == l3 && l1 == l4){
              
              if(typeof(gene1) != "character" || sum(is.na(gene1)) > 0 ){
                errflag = 1
                cat('<div> Error, column one should be all characters </div>')
              }
              if(typeof(connection) != "character" || sum(is.na(connection)) > 0){
                errflag = 1
                cat('<div> Error, column two should be all characters </div>')
              }
              if(typeof(gene2) != "character" || sum(is.na(gene2)) > 0){
                errflag = 1
                cat('<div> Error, column three should be all characters </div>')
              }
              if(typeof(pmid) != "integer" || sum(is.na(pmid)) > 0){
                errflag = 1
                cat('<div> Error, column four should be all integers </div>')
              }
              
            }else{
              errflag = 1
              cat('<div> All columns are not the same length or there are missing/invalid inputs </div>')
              
            }
            
            if(errflag != 1){
              
              cat('

<div class="ui container">
	<div class="ui fluid segment">
	<div class="ui four column grid">
	<div class="row">
		<div class="column" style = "text-align: center;"> <h4> Gene 1 </h4> </div>
    		<div class="column" style = "text-align: center;"> <h4> Connection </h4> </div>
    		<div class="column" style = "text-align: center;"> <h4> Gene 2 </h4> </div>
		<div class="column" style = "text-align: center;"> <h4> PMID </h4> </div>	
	</div>
')

		gene2pubmed <- read.csv("gene2pubmedclean.csv")
              
              for(i in 1:l1){
                
                x <- NULL
                
                x$gene1 <- gene1[i]
                x$connection <- connection[i]
                x$gene2 <- gene2[i]
                x$pmid <-  pmid[i]
                x$username <-as.character(SERVER$headers_in$OIDC_CLAIM_preferred_username)
                
                cat('
	<div class="row">	
		<div class="column" style = "text-align: center;">')
                if(identical(x$gene1, character(0))){ cat('<i class="exclamation triangle icon"></i>')}else{
                  cat(x$gene1)}
                cat('</div>
    		<div class="column" style = "text-align: center;">') 
                if(identical(x$connection, character(0))){ cat('<i class="exclamation triangle icon"></i>')}else{
                  cat(x$connection)}
                cat('</div>
    		<div class="column" style = "text-align: center;">') 
                if(identical(x$gene2, character(0))){ cat('<i class="exclamation triangle icon"></i>')}else{
                  cat(x$gene2)}
                cat('</div>
		<div class="column" style = "text-align: center;">') 
                if(identical(x$pmid, numeric(0))){ if(Aperm2 != 1){ cat('<i class="exclamation triangle icon"></i>')}else{cat('<i class="exclamation icon"></i>')}}else{
                  cat(x$pmid)} 
                cat('</div>	
	</div>')
                
                con <- dbConnect(RPostgres::Postgres())
                
                
                if(check_dup(x)){
                  errflag = 1
                  cat(' <h4 style = "text-align: center;"> <i class="exclamation triangle icon"></i> This connection is already in the database!  </h4> <br> ')
                } else{
                  
                  if(Aperm2 != 1){
                    if(identical(x$gene1, character(0)) || identical(x$connection, character(0)) || identical(x$gene2, character(0)) || identical(x$pmid, numeric(0))) {
                      cat('<h4 style = "text-align: center;"> <i class="exclamation triangle icon"></i> Missing input! </h4> <br> ')
                      errflag = 1
                    } else {

			check1 <- dim(gene2pubmed[which(gene2pubmed$V2== x$gene1 & gene2pubmed$V3 == x$pmid),])[1]
			check2 <- dim(gene2pubmed[which(gene2pubmed$V2== x$gene2 & gene2pubmed$V3 == x$pmid),])[1]
	
			if(check1 + check2 == 0){
	   			cat('<h4 style = "text-align: center;"> <i class="exclamation triangle icon"></i> Missing PMID association for this connection. Try a different input or contact us for support. </h4> <br> ')
				errflag = 1		
			}

			}
                  }else{
                    if(identical(x$gene1, character(0)) || identical(x$connection, character(0)) || identical(x$gene2, character(0))) {
                      cat('<h4 style = "text-align: center;"> <i class="exclamation triangle icon"></i> Missing input! </h4> <br> ')
                      errflag = 1
                    } else {

			check1 <- dim(gene2pubmed[which(gene2pubmed$V2== x$gene1 & gene2pubmed$V3 == x$pmid),])[1]
			check2 <- dim(gene2pubmed[which(gene2pubmed$V2== x$gene2 & gene2pubmed$V3 == x$pmid),])[1]
	
			if(check1 + check2 == 0){
	   			cat('<h4 style = "text-align: center;"> <i class="exclamation triangle icon"></i> Missing PMID association for this connection. Your account is allowed to submit anyways.  </h4> <br> ')		
			}

			}

                  }
                  
                }
                
                dbDisconnect(con)
                
              }
              
              if(errflag != 1){
                
                randbase = paste0("session",as.integer(rnorm(1,100000,100)),as.integer(rnorm(1,100000,100)),as.integer(rnorm(1,100000,100)))
                
                analysisdir = paste0("/Oracle/sessions/", randbase)
                
                dir.create(analysisdir)
                
                
                destination <- file.path(analysisdir,FILES$FirstFile$name)
                file.copy(FILES$FirstFile$tmp_name,destination,overwrite=TRUE)
                
                cat('<div class="row"> <form enctype="multipart/form-data" method="POST" action="https://dmac.pharmacy.arizona.edu/Oracle/Develop">
<input type="hidden" name="bulkp" id="bulkp" value="') 
                cat(destination) 
                cat('">')
                cat('

	<input type="hidden" name="gene1db" id="gene1db" value="') 
cat("fill") 
cat('">
<input type="hidden" name="connectiondb" id="connectiondb" value = "')
cat("fill2") 
cat('">
<input type="hidden" name="gene2db" id="gene2db" value = "')
cat("fill3")
cat('">
<input type="hidden" name="pmiddb" id="pmiddb" value = "')
cat("fill4")
cat('">

		<div class = "row"><div class = "column" style = "display: flex; justify-content: center; margin-left: 20px;"> <input type="submit" name="BulkUploadFinal" value = "Final Upload"> </div> </div> </div> <br>')
                
                
              }
              
              cat('
	</div>
	</div>
</div>
')
              
              
            }
            
          }
        }
        
        
      }
    }
  }




 if(length(POST)>5){ 
  if(!is.null(POST[6])){
    if(names(POST[6]) == "BulkUploadFinal"){

	dat<- read.csv(POST[[1]])

x <- NULL

x$gene1 <- as.character(dat[,1])
x$connection <-  as.character(dat[,2])
x$gene2 <-  as.character(dat[,3])
x$pmid <-  as.character(dat[,4])

x<-as.data.frame(x)
x$username <-as.character(SERVER$headers_in$OIDC_CLAIM_preferred_username)


con <- dbConnect(RPostgres::Postgres())


dbWriteTable(con, "table1", value = x, append = TRUE, row.names = FALSE)

dbDisconnect(con)

system("R CMD BATCH  /var/www/Rfiles/Oracle/OracleScripts/UpdateMain.r")

cat('<script> window.location.replace("https://dmac.pharmacy.arizona.edu/Oracle/Develop") </script> ')




    }
   }
  }

cat('

			<br>
			<p class="p-t-15" style = "font-size: large;">
			Recent Uploads </p>
			<div class="ui segment">	
			<div class="table-container" style="padding-top: 20px; height:350px;overflow-y: scroll;">
			<table class="ui very basic table">
				<thead class="">
				<tr class="">
				<th class="">
				Gene 1</th>
				<th class="">
				Connection</th>
				<th class="">
				Gene 2</th>
				<th class="">
				PMID</th>
				<th class="">
				Date Uploaded</th>
				<th>Delete</th>
				<th></th>
				</tr>
				
				</thead>

				<tbody class="">')

con <- dbConnect(RPostgres::Postgres())

query <- dbReadTable(con, "table1")

dbDisconnect(con)

query <- as.data.frame(query)

query <- query[query$username == as.character(SERVER$headers_in$OIDC_CLAIM_preferred_username),]

#print(query)

tempread <- query[1,]

if (dim(query)[1] != 0){

for(i in dim(query)[1]:1){
	
tempread <- query[i,]
	
	cat('
				<tr class="" >
				<td class="">')
	cat(tempread$gene1)
	cat('</td>
				<td class="">') 
	cat(tempread$connection)
cat('</td>
				<td class="">') 
	cat(tempread$gene2)

cat('</td>
				<td class="">') 
	cat(tempread$pmid)

	cat('</td>
				<td class="">') 
	cat(as.character(tempread$posting_date))
	cat('</td>
	<td>') 
if(Sys.Date()-tempread$posting_date < 100){

cat('<button class="ui button" type="submit" name = dtable_') 
cat(i)
cat('>
  Delete
</button>')
}
	cat('</td>
	<td></td>			
				</tr>')
	}
}

				cat('</tbody>
				</table>
		</div>
</div>
')

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

<script>

var connectionsArray = ["Promotes","Suppresses","Transcription Target"].sort();

new Def.Autocompleter.Prefetch("connections", connectionsArray);

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


