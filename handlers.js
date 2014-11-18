window.addEventListener("load", onLoad)

var form;
var invariantParent;

function onLoad() {
	form = document.getElementById("programForm");
	invariantParent = document.getElementById("invariants");
	var program = document.getElementById("program");
	program.addEventListener("input", countWhileLoops);
	
	countWhileLoops();
}

function countWhileLoops() {
	var program = document.getElementById("program");
	var text = program.value;
	
	var whiles = (text.match(/(\s+|^)while((\s*\()|\s+((true)|(false)))/g) || []).length;
	
	if (whiles != invariantParent.childNodes.length) {
		generateInvariantInputs(whiles);
	}
}

function generateInvariantInputs(count) {
	while (invariantParent.hasChildNodes()) {
		invariantParent.removeChild(invariantParent.childNodes[0]);
	}
	
	for (var i = 0; i < count; i++) {
		var input = document.createElement("input");
		input.name = "invariant" + i;
		input.type = "text";
		input.placeholder = "Invariant " + (i + 1);
		input.required = true;
		invariantParent.appendChild(input);
	}
}
