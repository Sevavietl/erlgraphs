//
// main.js
//
// A project template for using arbor.js
//

(function($){
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
          ctx.strokeStyle = "rgba(0,0,0, .333)"
          ctx.lineWidth = 1
          ctx.beginPath()
          ctx.moveTo(pt1.x, pt1.y)
          ctx.lineTo(pt2.x, pt2.y)
          ctx.stroke()
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
	  /*ctx.font = "bold 8px Arial #000"
          ctx.textAlign = "center"
	  ctx.fillText(node.data.display, pt.x, pt.y+4)*/
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

	
    $("li.graph-li").click(function() {
	$.getJSON("ajax/load_graph.yaws",
		    {id: $(this).attr('id')},
		    function(graph) {
		    alert(graph)
	})
    });	
    return that
  }

  $(document).ready(function(){
    var sys = arbor.ParticleSystem(1000, 600, 1) // create the system with sensible repulsion/stiffness/friction
    sys.parameters({gravity:false}) // use center-gravity to make the graph settle nicely (ymmv)
    sys.renderer = Renderer("#viewport") // our newly created renderer will have its .init() method called shortly by sys...

    // add some nodes to the graph and watch it go...
	sys.addNode('СЕВАСТОПОЛЬ',  {'shape':'dot', 'display':'СЕВАСТОПОЛЬ', 'p':arbor.Point(583, 720-36)});
	sys.addNode("СИМФЕРОПОЛЬ-ГРУЗ.", {'shape':'dot', 'display':'СИМФЕРОПОЛЬ-ГРУЗ.', 'p':arbor.Point(607, 720-64)}); 
	sys.addNode("ОСТРЯКОВО", {'shape':'dot', 'display':'ОСТРЯКОВО', 'p':arbor.Point(605, 720-72)});   
	sys.addNode("ЕВПАТОРИЯ-ТОВ.", {'shape':'dot', 'display':'ЕВПАТОРИЯ-ТОВ.', 'p':arbor.Point(574, 720-76)});   
	sys.addNode("ДЖАНКОЙ", {'shape':'dot', 'display':'ДЖАНКОЙ', 'p':arbor.Point(622, 720-113)});   
	sys.addNode("ФЕОДОСИЯ"  ,{'shape':'dot', 'display':'ФЕОДОСИЯ', 'p':arbor.Point(675, 720-69)});   
	sys.addNode("ВЛАДИСЛАВОВКА" ,{'shape':'dot', 'display':'ВЛАДИСЛАВОВКА', 'p':arbor.Point(675, 720-78)});   
	sys.addNode("КЕРЧЬ" ,{'shape':'dot', 'display':'КЕРЧЬ', 'p':arbor.Point(725, 720-97)});   
	sys.addNode("ВАДИМ" ,{'shape':'dot', 'display':'ВАДИМ', 'p':arbor.Point(578, 720-148)});   
	sys.addNode("НОВОАЛЕКСЕЕВКА", {'shape':'dot', 'display':'НОВОАЛЕКСЕЕВКА', 'p':arbor.Point(632, 720-153)});   
	sys.addNode("ГЕНИЧЕСК", {'shape':'dot', 'display':'ГЕНИЧЕСК', 'p':arbor.Point(639, 720-149)});   
	sys.addNode("МЕЛИТОПОЛЬ-ПАСС.", {'shape':'dot', 'display':'МЕЛИТОПОЛЬ-ПАСС.', 'p':arbor.Point(664, 720-199)});   
	sys.addNode("ФЕДОРОВКА", {'shape':'dot', 'display':'ФЕДОРОВКА', 'p':arbor.Point(662, 720-217)});   
	sys.addNode("НОВОВЕСЕЛАЯ", {'shape':'dot', 'display':'НОВОВЕСЕЛАЯ', 'p':arbor.Point(635, 720-208)});   
	sys.addNode("КАМЫШ-ЗАРЯ", {'shape':'dot', 'display':'КАМЫШ-ЗАРЯ', 'p':arbor.Point(727, 720-238)});   
	sys.addNode("ВЕРХНИЙ ТОКМАК 1", {'shape':'dot', 'label':'ВЕРХНИЙ ТОКМАК 1', 'p':arbor.Point(709, 720-226)});   
	sys.addNode("БЕРДЯНСК", {'shape':'dot', 'display':'БЕРДЯНСК', 'p':arbor.Point(736, 720-199)});   
	sys.addNode("ПОЛОГИ", {'shape':'dot', 'display':'ПОЛОГИ', 'p':arbor.Point(705, 720-247)});   
	sys.addNode("ЗАПОРОЖЬЕ-ГРУЗ.", {'shape':'dot', 'display':'ЗАПОРОЖЬЕ-ГРУЗ.', 'p':arbor.Point(659, 720-273)});   
	sys.addNode("НИКОПОЛЬ", {'shape':'dot', 'display':'НИКОПОЛЬ', 'p':arbor.Point(612, 720-250)});   
	sys.addNode("ЧАПЛИНО", {'shape':'dot', 'display':'ЧАПЛИНО', 'p':arbor.Point(703, 720-292)});   
	sys.addNode("СИНЕЛЬНИКОВО-1", {'shape':'dot', 'display':'СИНЕЛЬНИКОВО-1', 'p':arbor.Point(665, 720-304)});   
	sys.addNode("АПОСТОЛОВО", {'shape':'dot', 'display':'АПОСТОЛОВО', 'p':arbor.Point(580, 720-252)});   
	sys.addNode("ВЫСОКОПОЛЬЕ", {'shape':'dot', 'display':'ВЫСОКОПОЛЬЕ', 'p':arbor.Point(573, 720-240)});   
	sys.addNode("ПАВЛОГРАД-1", {'shape':'dot', 'display':'ПАВЛОГРАД-1', 'p':arbor.Point(679, 720-324)});   
	sys.addNode("ЛОЗОВАЯ-ПАСС.", {'shape':'dot', 'display':'ЛОЗОВАЯ-ПАСС.', 'p':arbor.Point(700, 720-349)});   
	sys.addNode("КРАСНОАРМЕЙСК-ПАСС.", {'shape':'dot', 'display':'КРАСНОАРМЕЙСК-ПАСС.', 'p':arbor.Point(746, 720-309)});      
	sys.addNode("НОВОМОСКОВСК-ДН.", {'shape':'dot', 'display':'НОВОМОСКОВСК-ДН.', 'p':arbor.Point(649, 720-327)});
	sys.addNode("КРАСНОГРАД", {'shape':'dot', 'display':'КРАСНОГРАД', 'p':arbor.Point(657, 720-381)});
	sys.addNode("ДНЕПРОДЗЕРЖИНСК-ПАСС.", {'shape':'dot', 'display':'ДНЕПРОДЗЕРЖИНСК-ПАСС.', 'p':arbor.Point(618, 720-316)});
	sys.addNode("ВЕРХОВЦЕВО", {'shape':'dot', 'display':'ВЕРХОВЦЕВО', 'p':arbor.Point(604, 720-316)});
	sys.addNode("ПЯТИХАТКИ-ПАСС.", {'shape':'dot', 'display':'ПЯТИХАТКИ-ПАСС.', 'p':arbor.Point(578, 720-308)});
	sys.addNode("САВРО", {'shape':'dot', 'display':'САВРО', 'p':arbor.Point(574, 720-297)});
	sys.addNode("ПРИВОРОТ", {'shape':'dot', 'display':'ПРИВОРОТ', 'p':arbor.Point(577, 720-283)});
	sys.addNode("КРИВОЙ РОГ-СОРТ.", {'shape':'dot', 'display':'КРИВОЙ РОГ-СОРТ.', 'p':arbor.Point(569, 720-272)});
	sys.addNode("ДОЛИНСКАЯ", {'shape':'dot', 'display':'ДОЛИНСКАЯ', 'p':arbor.Point(533, 720-284)});
	sys.addNode("МОИСЕЕВКА", {'shape':'dot', 'display':'МОИСЕЕВКА', 'p':arbor.Point(556, 720-268)});
	sys.addNode("ДНЕПРОПЕТРОВСК-ГЛАВ.", {'shape':'dot', 'display':'ДНЕПРОПЕТРОВСК-ГЛАВ.', 'p':arbor.Point(639, 720-315)});
	sys.addNode("НИЖНЕДНЕПРОВСК", {'shape':'dot', 'display':'НИЖНЕДНЕПРОВСК', 'p':arbor.Point(642, 720-317)})  
    
  })

})(this.jQuery)


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
						var text = '<h2>' + graph.name + '</h2>'
						    text += '<p>Please, upload csv (comma separated values) files, that contain information for new Graph. There\'re two'   			
						    text += 'files that needed to be uploaded. First containes all Nodes and Coordinates, it must be'
						    text += 'in form {Node_name; Node_id; Node_x; Node_y}.'
						    text += 'The second file contains edges, in form {Source_id, Edge_id, Target_id, Edge_weight}'		    

						var form = '<form id="upload-graph-data" action="graph_upload.yaws" method="post" onsubmit="return uploadGraphData();" enctype="multipart/form-data">'
						    //form += '<input type="hidden" name="gid" value=' + graph.id + '>'
						    form += '<label>Upload file with all Nodes and Node coordinates</label>'
						    form += '<input width="50" type="file" name="gnodes_' + graph.id + '">'
						    form += '<label>Upload file with all Graph edges</label>'
						    form += '<input width="50" type="file" name="gedges_ ' + graph.id + '"><br />'
						    form += '<input type="submit" value="Upload">'
						    form += '</form>'

						$("div#working-area").html(text + form)
						$("form#add-graph-form input[name=gname]").val("")
					}
					else {
						alert("Sorry, the graph with this name already existst. Please, try another one.")
						$("form#add-graph-form input[name=gname]").focus()
					}
				}
		)
	}
}

function uploadGraphData() {
	if ($("form#upload-graph-data input[name=gnodes]").val() === "" || $("form#upload-graph-data input[name=gedges]").val() === "") {
		alert("Please provide all information about the Nodes!")
		return false
	}
}
