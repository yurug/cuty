var nodes = null;
var edges = null;
var network = null;

var DIR = 'img/refresh-cl/';
var LENGTH_MAIN = 150;
var LENGTH_SUB = 50;
var DOMURL = window.URL || window.webkitURL || window;

function new_node (id, innerHTML, width, height) {
    var data =
	'<svg xmlns="http://www.w3.org/2000/svg" width="' + width + '" ' + 'height="' + height + '">' +
        '<foreignObject x="0" y="0" width="90%" height="90%">' +
        '<div xmlns="http://www.w3.org/1999/xhtml"><p>' +
	innerHTML +
        '</p></div>' +
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
	width: "100wv"
    };

    network = new vis.Network(container, data, options);
}

function installOnClick (callback) {
    network.on ('click', callback);
}

