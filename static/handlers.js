window.addEventListener("load", onLoad)

var form;
var invVarParent;
var invVarParentBlock;

function onLoad() {
	form = document.getElementById("programForm");
	invVarParent = document.getElementById("invariants");
	invVarParentBlock = document.getElementById("invVarParentBlock");
	var program = document.getElementById("program");
	program.addEventListener("input", countWhileLoops);
	
	countWhileLoops();
}

function countWhileLoops() {
	var program = document.getElementById("program");
	var text = program.value;
	
	var whiles = (text.match(/(\s+|^)while((\s*\()|\s+((true)|(false)))/g) || []).length;
	
	if (whiles != invVarParent.childNodes.length) {
		generateInvVarInputs(whiles);
	}
}

function generateInvVarInputs(count) {
	while (invVarParent.hasChildNodes()) {
		invVarParent.removeChild(invVarParent.childNodes[0]);
	}
	
	if (count > 0) {
		invVarParentBlock.className = "inv-var-parent-block";
	} else {
		invVarParentBlock.className = "inv-var-parent-block-hidden";
	}
	
	for (var i = 0; i < count; i++) {
		var invInput = document.createElement("input");
		invInput.name = "invariant" + i;
		invInput.type = "text";
		invInput.placeholder = "Invariant " + (i + 1);
		invInput.required = true;
		invInput.className = "form-control";
		
		
		var varInput = document.createElement("input");
		varInput.name = "variant" + i;
		varInput.type = "text";
		varInput.placeholder = "Variant " + (i + 1);
		varInput.required = true;
		varInput.className = "form-control";
		
		var invDiv = document.createElement("div")
		var varDiv = document.createElement("div")
		invDiv.className = "col-sm-6";
		varDiv.className = "col-sm-6";
		invDiv.appendChild(invInput);
		varDiv.appendChild(varInput);
		
		var div = document.createElement("div");
		div.className = "row top-bottom-padded";
		div.appendChild(invDiv);
		div.appendChild(varDiv);
		var spacer = document.createElement("div")
		spacer.className = "col-md-12";
		spacer.appendChild(div);
		invVarParent.appendChild(spacer);
	}
}

