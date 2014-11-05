var exec = require('child_process').exec;
var http = require('http');
var qs = require('querystring');
var fs = require('fs');
var url = require('url');

var html = null; // The HTML file to render in the HTTP response

// Code that is run when the web server starts
var port = process.env.PORT || 5000;
http.createServer(requestHandler).listen(port);
console.log("Listening on port " + port);

// Sends an HTML response using the HTML file loaded at server start
// and inserts result into the HTML
function sendHtmlResponse(response, result) {
	html = fs.readFileSync("./program-checker.html", 'utf-8').replace(/\{result\}/g, result);
	response.writeHead(200, {'Content-Type' : 'text/html'});
	response.write(html);
	response.end();
}

// Runs the program checker implemented in Lisp with the given input string
function executeChecker(response, input) {
	// Escape backslashes and double quotes to avoid errors when executing
	// the child process.
	input = input.replace(/("|\\|`)/g, "\\$1");
	var command = "./program-checker \"" + input + "\"";
	// Execute the Lisp child process
	var child = exec(command,
		function (error, stdout, stderr) {
			if (error === null) {
				// Retrieve the program output and render the response
				result = "Program output: " + stdout;
				sendHtmlResponse(response, result);
			} else {
				// Render the response with the given error
				sendHtmlResponse(response, "There was an error: " + error);
			}

		});
}

// Handles an HTTP GET request
function handleGet(request, response) {
	path = url.parse(request.url).pathname; // Get the pathname of the requested resource
	
	if (/\.(css|js)$/.test(path)) {
		// Parse the content type and send it through the HTTP response
		var content = fs.readFileSync(__dirname + path, 'utf-8');
		var suffix = path.substr(path.lastIndexOf(".") + 1, path.length - 1);
		var contentType = "text/" + (suffix == "css" ? "css" : "javascript");
		
		response.writeHead(200, {'Content-Type' : contentType});
		response.write(content);
		response.end();
	} else {
		sendHtmlResponse(response, ""); // Return the HTML file
	}
}

// Handles HTTP requests from the client
function requestHandler(request, response) {
	if (request.method == 'POST') {
		var result = null;
		var body = '';
		var input = "";
		
		// Load the POST data
		request.on('data', function (data) {
		    body += data;

		    // Too much POST data, kill the connection!
		    if (body.length > 1e6)
		        req.connection.destroy();
		});
		
		// Feed the POST data into the program checker and render the response
		request.on('end', function () {
		    var post = qs.parse(body);
			executeChecker(response, post['program']);
		});
	} else {
		handleGet(request, response);
	}
}

