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
		
		
		var varInput = document.createElement("input");
		varInput.name = "variant" + i;
		varInput.type = "text";
		varInput.placeholder = "Variant " + (i + 1);
		varInput.required = true;
		
		var div = document.createElement("div");
		div.appendChild(invInput);
		div.appendChild(varInput);
		invVarParent.appendChild(div);
	}
}

