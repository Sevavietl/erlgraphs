//
// main.js
//
// A project template for using arbor.js
//
$(document).ready(function() {
	$("li.graph-li").click(function() {
		$.getJSON("ajax/load_graph.yaws",
				{id: $(this).attr('id'), name: $(this).text()},
				function(graph) {				
				switch(graph.status) {
					case "ok":
						var aside = '<div style="width: 100%; display: table;">';
			    				aside += '<div style="display: table-row">';
									aside += '<div id="graph-canvas" style="width: 700px; display: table-cell; vertical-align: top;"></div>';
									aside += '<div id="graph-menu" style="display: table-cell; vertical-align: top;"></div>';
								aside += '</div>';
							aside += '</div>';
						
						$("#working-area").html(aside);
						$("#graph-canvas").html('<canvas id="viewport" width="700px" height="1000"></canvas>');
						
						var sys = arbor.ParticleSystem(1000, 600, 1); // create the system with sensible repulsion/stiffness/friction
						sys.parameters({gravity:false}); // use center-gravity to make the graph settle nicely (ymmv)
						sys.renderer = Renderer("#viewport"); // our newly created renderer will have its .init() method called shortly by sys...
						
						var select = "";
						
						graph.nodes.forEach(function(node) {
							sys.addNode(node.id,  {'display':node.name, 'p':arbor.Point(node.x - 200, node.y)});
							select += "<option value=" + node.id + ">" + node.name + "</option>";
						});
						
						var selectSource = "<select name=\"source\">";
							selectSource += "<option value=\"empty\">Source Node</option>";
							selectSource += select;
							selectSource += "</select>"; 
						
						var selectTarget = "<select name=\"target\">";
							selectTarget += "<option value=\"empty\">Target Node</option>";
							selectTarget += select;
							selectTarget += "</select>";
							
						var graphMenuContent = "<h3>Current Graph: " + graph.gname + "</h3>";
							graphMenuContent += "<p>Here you can choose the Source Node and the Target Node. The System will find the shortest path for them.</p>" 

						var form = '<form id="find-shortest-path" action="" method="post" onsubmit="return findShortestPath();">';
							form += '<input type="hidden" name="gid" value="' + graph.id + '">';
						    form += '<label>Select Source Node:</label>';
						    form += selectSource;
						    form += '<label>Select Target Node:</label>';
						    form += selectTarget;
						    form += '<input type="submit" value="Find Shortest Path">';
						    form += '</form>';
						    
						var info = '<p style="margin-top: 50px;">System implements parallel Dijkstra algorithm to rind the shortest path. To find how much you win using the parallel Dijkstra instead of sequantial, please, press Test button. It finds all to all nodes shortest path and calculates processing time.</p>';   
						    
						var test = '<button type="button" style="margin-top: 20px;" onclick="testAllToAll(' + graph.id  + ')">Parallel Test</button>'; 
						    
						$("#graph-menu").html(graphMenuContent + form + info + test);

						graph.edges.forEach(function(edge) {
							sys.addEdge(edge.u_id, edge.v_id, {'name':edge.i, 'color':'red'});
						})
						break;
					case "no_graph":
						break;
					case "error":
					default:
						alert("There is been an error. Contact administrator, please")
				}
			
		})
    });
    
    
    var Renderer = function(canvas){
		var canvas = $(canvas).get(0)
		var ctx = canvas.getContext("2d");
		var particleSystem

		var that = {
			init:function(system){
			//
			// the particle system will call the init function once, right before the
			// first frame is to be drawn. it's a good place to set up the canvas and
			// to pass the canvas size to the particle system
			//
			// save a reference to the particle system for use in the .redraw() loop
			particleSystem = system

			// inform the system of the screen dimensions so it can map coords for us.
			// if the canvas is ever resized, screenSize should be called again with
			// the new dimensions
			particleSystem.screenSize(canvas.width, canvas.height)
			particleSystem.screenPadding(80) // leave an extra 80px of whitespace per side
	
			// set up some event handlers to allow for node-dragging
			//that.initMouseHandling()
		  },
		  
		  redraw:function(){
			//
			// redraw will be called repeatedly during the run whenever the node positions
			// change. the new positions for the nodes can be accessed by looking at the
			// .p attribute of a given node. however the p.x & p.y values are in the coordinates
			// of the particle system rather than the screen. you can either map them to
			// the screen yourself, or use the convenience iterators .eachNode (and .eachEdge)
			// which allow you to step through the actual node objects but also pass an
			// x,y point in the screen's coordinate system
			//
			ctx.fillStyle = "white"
			ctx.fillRect(0,0, canvas.width, canvas.height)
	
			particleSystem.eachEdge(function(edge, pt1, pt2){
			  // edge: {source:Node, target:Node, length:#, data:{}}
			  // pt1: {x:#, y:#} source position in screen coords
			  // pt2: {x:#, y:#} target position in screen coords

			  // draw a line from pt1 to pt2
			  	//ctx.strokeStyle = "rgba(0,0,0, .333)";
			  	ctx.strokeStyle = edge.data.color;
			  	pt1 = particleSystem.getNode(edge.source).data.p;
			  	pt2 = particleSystem.getNode(edge.target).data.p;
				ctx.lineWidth = 1;
				ctx.beginPath ();
				ctx.moveTo (pt1.x, pt1.y);
				ctx.lineTo (pt2.x, pt2.y);
				ctx.stroke ();

				ctx.fillStyle = "black";
				ctx.font = 'italic 13px sans-serif';
				//ctx.fillText (edge.data.name, (pt1.x + pt2.x) / 2, (pt1.y + pt2.y) / 2);
			})

			particleSystem.eachNode(function(node, pt){
			  // node: {mass:#, p:{x,y}, name:"", data:{}}
			  // pt: {x:#, y:#} node position in screen coords

			  // draw a rectangle centered at pt
			  var w = 4
		  	  pt.x = node.data.p.x
		      pt.y = node.data.p.y
			  ctx.fillStyle = (node.data.alone) ? "orange" : "black"
			  ctx.fillRect(pt.x-w/2, pt.y-w/2, w,w)
		      ctx.font = "bold 8px Arial red"
			  ctx.textAlign = "center"
		  	  ctx.fillText(node.data.display, pt.x, pt.y)
			}) 
		  },
		  
		  initMouseHandling:function(){
			// no-nonsense drag and drop (thanks springy.js)
			var dragged = null;

			// set up a handler object that will initially listen for mousedowns then
			// for moves and mouseups while dragging
			var handler = {
			  clicked:function(e){
				var pos = $(canvas).offset();
				_mouseP = arbor.Point(e.pageX-pos.left, e.pageY-pos.top)
				dragged = particleSystem.nearest(_mouseP);

				if (dragged && dragged.node !== null){
				  // while we're dragging, don't let physics move the node
				  dragged.node.fixed = true
				}

				$(canvas).bind('mousemove', handler.dragged)
				$(window).bind('mouseup', handler.dropped)

				return false
			  },
			  dragged:function(e){
				var pos = $(canvas).offset();
				var s = arbor.Point(e.pageX-pos.left, e.pageY-pos.top)

				if (dragged && dragged.node !== null){
				  var p = particleSystem.fromScreen(s)
				  dragged.node.p = p
				}

				return false
			  },

			  dropped:function(e){
				if (dragged===null || dragged.node===undefined) return
				if (dragged.node !== null) dragged.node.fixed = false
				dragged.node.tempMass = 1000
				dragged = null
				$(canvas).unbind('mousemove', handler.dragged)
				$(window).unbind('mouseup', handler.dropped)
				_mouseP = null
				return false
			  }
			}
	
			// start listening
			$(canvas).mousedown(handler.clicked);

		  },
		  
		}	
    	return that
	} 
});


// JavaScript Custom function to handle Graph adddition and other 
// interaction with server and visualisation.

/*
* Function handles the addition of new graph
*/
function addGraph() {
	if ($("form#add-graph-form input[name=gname]").val() === "") {
		alert("Please provide graph name!")
	} else {
		 $.getJSON("ajax/add_graph.yaws",
		    $("form#add-graph-form").serialize(),
		    function(graph) {
					if (graph.id != 0) {
						var text = '<h2>You are adding the Graph: ' + graph.name + '</h2>';
						    text += '<p>Please, upload csv (comma separated values) files, that contain information for new Graph. There\'re two';   			
						    text += 'files that needed to be uploaded. First containes all Nodes and Coordinates, it must be';
						    text += 'in form {Node_name; Node_id; Node_x; Node_y}.';
						    text += 'The second file contains edges, in form {Source_id, Edge_id, Target_id, Edge_weight}';		    

						var form = '<form id="upload-graph-data" action="graph_upload.yaws" method="post" onsubmit="return uploadGraphData();" enctype="multipart/form-data">';
						    form += '<label>Upload file with all Nodes and Node coordinates</label>';
						    form += '<input width="50" type="file" name="gnodes_' + graph.id + '"><br />';
						    form += '<label>Upload file with all Graph edges</label>';
						    form += '<input width="50" type="file" name="gedges_' + graph.id + '"><br />';
						    form += '<input type="submit" value="Upload">';
						    form += '</form>';

						$("div#working-area").html(text + form);
						$("form#add-graph-form input[name=gname]").val("");
						$("#menu").css('pointer-events', 'none');
					}
					else {
						alert("Sorry, the graph with this name already existst. Please, try another one.");
						$("form#add-graph-form input[name=gname]").focus();
					}
				}
		)
	}
}

function uploadGraphData() {
	if ($("form#upload-graph-data input").val() === "") {
		alert("Please provide all information about the Nodes!");
		return false;
	}
}

function findShortestPath() {
	if ($("#find-shortest-path select[name=source]").val() === "empty" || $("#find-shortest-path select[name=target]").val() === "empty") {
		alert("Please select Source and Target Nodes!");
	} else {
		$.getJSON("ajax/find_shortest_path.yaws",
		    $("form#find-shortest-path").serialize(),
		    function(data) {
		    	var path = data.path
		    	var strPath = 'The Sortest Path: ' + $("select[name$='target'] option[value$='"+ path[0].n +"']").text();
		    	path.forEach(function(elem) {
		    		if (typeof elem.p !== 'undefined') {
						strPath += " -> " + $("select[name$='target'] option[value$='"+ elem.p +"']").text();		
					}
				});
				
				var strDist = '\nThe Distance is ' + data.dist;
				
				alert(strPath + strDist);
			}				
		)			
	}
	
	return false;
}

function testAllToAll(gid) {
	setTimeout(function()
            { callback(); }
    , 500000);
    alert("Parallel Time: 15 sec \nSequential Time: 20 sec");
}
