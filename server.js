var exec = require('child_process').exec;
var http = require('http');
var qs = require('querystring');
var fs = require('fs');
var url = require('url');
var express = require('express');
var bodyParser = require('body-parser')
var app = express()
app.use(bodyParser());
app.use('/static', express.static(__dirname + '/static'))

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
	"~": "¬",
	">=": "≥",
	"<=": "≤",
	"!=": "≠"
};

// Map for converting characters received from client input
var escapeInputCharacterMap = {
	"\"": "\\\"",
	"\\": "\\\\",
	"`": "\\`"
};

// Helper function for escaping HTML characters
function escapeHtml(string) {
	return String(string).replace(/(->)|(>=)|(<=)|(!=)|(\/\\)|(\\\/)|([\n&<>"'\/~])/g, function (replacement) {
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

// Runs the program checker implemented in Lisp with the given string arguments
function executeChecker(response, precondition, program, postcondition, invariants, variants) {
	// Escape backslashes and double quotes to avoid errors when executing
	// the child process.
	precondition = formatArg(precondition);
	program = formatArg(program);
	postcondition = formatArg(postcondition);
	var command = "./program-checker" + precondition + program + postcondition + invariants + variants;
	// Execute the Lisp child process
	console.log("Executing Lisp process with command: " + command);
	var child = exec(command,
		function (error, stdout, stderr) {
			var result = "";
			if (error === null) {
				// Retrieve the program output and render the response
				console.log("Lisp process executed successfully");
				console.log(stdout);
				result = escapeHtml(stdout);
			} else {
				// Render the response with the given error
				console.log("Lisp process failed with error: \n" + error);
				result = "There was an error trying to process the code!";
			}
			response.write(result);
			response.end();
		});
}

// Handles GET requests for the main page
app.get('/', function(req, res) {
	console.log("Request sent for \"" + req.url + "\"");
	var html = fs.readFileSync("./program-checker.html", 'utf-8');
	res.writeHead(200, {'Content-Type' : 'text/html'});
	res.write(html);
	res.end();
});

// Handles GET requests for the instructions. Handled in a separate
// request to allow having instructions in a different HTML file
app.get('/instructions', function(req, res) {
	console.log("Request sent for \"" + req.url + "\"");
	var html = fs.readFileSync("./instructions.html", 'utf-8');
	res.writeHead(200, {'Content-Type' : 'text/html'});
	res.write(html);
	res.end();
});

// Handles POST requests and sends the data to the program checker
app.post('/', function(req, res) {
	executeChecker(res, req.body.precondition, req.body.program,
				req.body.postcondition, extractMultiField("invariant", req), extractMultiField("variant", req));
});

// Extracts a field that is repeated multiple times in the HTML form.
// The fields will have a base name followed by a numeric identifier
function extractMultiField(baseName, req) {
	var currentField;
	var fieldArgs = "";
	
	for (var i = 0; (currentField = req.body[baseName + i]) !== undefined; i++) {
		fieldArgs += formatArg(currentField);
	}
	
	return fieldArgs;
} 

// Code that is run when the web server starts
var port = process.env.PORT || 5000;
app.listen(port);
console.log("Listening on port " + port);

