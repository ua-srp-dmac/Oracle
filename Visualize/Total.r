
homedata <- "/var/www/data"

headconv <- read.csv("/var/www/Rfiles/headconversion.csv")

usremail <- SERVER$headers_in$OIDC_CLAIM_email

Aperm3 <- headconv$OracleNet[headconv$Email==usremail]


setContentType("text/html")

if(Aperm3 == 1) {

cat('


<html>


<head>

<title>University of Arizona SRP-DMAC Olympus server</title>
<meta http-equiv="Cache-Control" content="no-cache, no-store, must-revalidate" />
<meta http-equiv="Pragma" content="no-cache" />
<meta http-equiv="expires"; content="0"; charset=UTF-8">

  <style> body { margin: 0; } </style>

  <link rel="stylesheet" type="text/css" href="/AresC/semantic.min.css">
  <script src="https://unpkg.com/three"></script>
  <script src="https://unpkg.com/three-spritetext"></script>
  <script src="https://unpkg.com/3d-force-graph"></script>
  <script src="4dbc34e2bd8b2dc506b8210d1f0.js"></script>
  
</head>

<body>

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
</div>



  <div id="3d-graph"></div>

  <script>
	
    const gData = JSON.parse(text_1);

    const Graph = ForceGraph3D()
    (document.getElementById("3d-graph"))
      .graphData(gData)
	  .nodeColor(node => "black")
	  .linkColor(link => link.connection)
	  .nodeThreeObject(node => {
          const sprite = new SpriteText(node.id);
          sprite.material.depthWrite = false; // make sprite background transparent
          sprite.color = node.color;
          sprite.textHeight = 8;
          return sprite;
        })
      .backgroundColor("white")
      .linkDirectionalArrowLength(5)
      .linkDirectionalArrowRelPos(1)
      .linkCurvature(0.25)
      .linkWidth(1.5);
	  

  </script>
</body>

<html>

')

} else 

{
cat('


<html>


<head>

<title>University of Arizona SRP-DMAC Olympus server</title>
<meta http-equiv="Cache-Control" content="no-cache, no-store, must-revalidate" />
<meta http-equiv="Pragma" content="no-cache" />
<meta http-equiv="expires"; content="0"; charset=UTF-8">

  
</head>

<body>

<div> Sorry, No Access! </div> 

 
</body>

<html>

')


}
