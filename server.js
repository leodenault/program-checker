var exec = require('child_process').exec;
var http = require('http');
var qs = require('querystring');
var fs = require('fs');
var url = require('url');

// Map for converting characters to be used in an HTML document
var escapeHtmlCharacterMap = {
	"&": "&amp;",
	"<": "&lt;",
	">": "&gt;",
	'"': '&quot;',
	"'": '&#39;',
	"/": '&#x2F;',
	"\n": '<br>',
	"->": "→",
	"/\\": "∧",
	"\\/": "∨",
	"~": "¬"
};

// Map for converting characters received from client input
var escapeInputCharacterMap = {
	"\"": "\\\"",
	"\\": "\\\\",
	"`": "\\`"
};

// Helper function for escaping HTML characters
function escapeHtml(string) {
	return String(string).replace(/(->)|(\/\\)|(\\\/)|([\n&<>"'\/~])/g, function (replacement) {
		return escapeHtmlCharacterMap[replacement];
	});
}

// Helper function for escaping input characters
function escapeInput(string) {
	return String(string).replace(/[\"\\`]/g, function (replacement) {
		return escapeInputCharacterMap[replacement];
	});
}

// Helper function for formatting input as an argument to the Lisp process
function formatArg(string) {
	return " \"" + escapeInput(string) + "\"";
}

// Code that is run when the web server starts
var port = process.env.PORT || 5000;
http.createServer(requestHandler).listen(port);
console.log("Listening on port " + port);

// Sends an HTML response using the HTML file loaded at server start
// and inserts result into the HTML
function sendHtmlResponse(response, result) {
	var html = fs.readFileSync("./program-checker.html", 'utf-8').replace(/\{result\}/g, result);
	response.writeHead(200, {'Content-Type' : 'text/html'});
	response.write(html);
	response.end();
}

// Runs the program checker implemented in Lisp with the given string arguments
function executeChecker(response, precondition, program, postcondition) {
	// Escape backslashes and double quotes to avoid errors when executing
	// the child process.
	precondition = formatArg(precondition);
	program = formatArg(program);
	postcondition = formatArg(postcondition);
	var command = "./program-checker" + precondition + program + postcondition;
	// Execute the Lisp child process
	console.log("Executing Lisp process");
	var child = exec(command,
		function (error, stdout, stderr) {
			if (error === null) {
				// Retrieve the program output and render the response
				console.log("Lisp process executed successfully");
				result = escapeHtml("Program output: \n" + stdout);
				sendHtmlResponse(response, result);
			} else {
				// Render the response with the given error
				console.log("Lisp process failed with error: \n" + error);
				sendHtmlResponse(response, "There was an error trying to process the code!");
			}

		});
}

// Handles an HTTP GET request
function handleGet(request, response) {
	path = url.parse(request.url).pathname; // Get the pathname of the requested resource
	console.log("Handling request for resource: " + path);
	
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
	console.log("Handling request of type " + request.method);
	if (request.method == 'POST') {
		var result = null;
		var body = '';
		
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
			executeChecker(response, post['precondition'], post['program'], post['postcondition']);
		});
	} else {
		handleGet(request, response);
	}
}

