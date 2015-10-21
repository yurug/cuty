var nodes = null;
var edges = null;
var network = null;

var DIR = 'img/refresh-cl/';
var LENGTH_MAIN = 500;
var LENGTH_SUB = 500;
var DOMURL = window.URL || window.webkitURL || window;

function new_node (id, innerHTML) {
    var data =
	'<svg xmlns="http://www.w3.org/2000/svg" width="390" height="65">' +
        '<foreignObject x="15" y="10" width="100%" height="100%">' +
        '<div xmlns="http://www.w3.org/1999/xhtml">' +
	innerHTML +
        '</div>' +
        '</foreignObject>' +
        '</svg>';
    var img = new Image();
    var svg = new Blob([data], {type: 'image/svg+xml;charset=utf-8'});
    var url = DOMURL.createObjectURL(svg);
    nodes.push ({id: id, image: url, shape: 'image'});
    network.setData ({ nodes: nodes, edges: edges });
}

// Called when the Visualization API is loaded.
function draw() {
    nodes = [];
    edges = [];
    var container = document.getElementById('playground');
    var data = { nodes: nodes, edges: edges };
    var options = {
       physics: {stabilization: false},
	edges: {smooth: false},
	height: "800px",
	width: "100vw"
    };
    network = new vis.Network(container, data, options);
}


