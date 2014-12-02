var invVarParent;
var invVarParentBlock;
var programForm;

$(document).ready(function () {
	programForm = $("#programForm");
	programForm.submit(submitForm);
	
	invVarParent = $("#invariants");
	invVarParentBlock = $("#invVarParentBlock");
	
	var program = $("#program");
	program.on("input", countWhileLoops);
	
	countWhileLoops();
});

function countWhileLoops() {
	var program = document.getElementById("program");
	var text = program.value;
	
	var whiles = (text.match(/(\s+|^)while((\s*\()|\s+((true)|(false)))/g) || []).length;
	
	if (whiles != $("#invariants div").length) {
		generateInvVarInputs(whiles);
	}
}

function generateInvVarInputs(count) {
	invVarParent.empty();
	
	if (count > 0) {
		invVarParentBlock.addClass("col-sm-6");
		invVarParentBlock.removeClass("hidden");
	} else {
		invVarParentBlock.removeClass("col-sm-6");
		invVarParentBlock.addClass("hidden");
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
		invVarParent.append(spacer);
	}
}

function submitForm(event) {
	event.preventDefault();
	
	var $form = $(this),
		url = $form.attr('action'),
		data = $form.serialize();

	var posting = $.post(url, data);

	posting.done(function(data) {
		$("#results").empty().append(data);
	});
}

