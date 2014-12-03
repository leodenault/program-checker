var invVarParent;
var invVarParentBlock;
var programForm;
var resultsPanel;

// Run when the document is done loading
$(document).ready(function () {
	programForm = $("#programForm");
	programForm.submit(submitForm); // Set the submit call back
	
	var getting = $.get("/instructions"); // Fetch the instructions HTML document
	
	getting.done(function(data) {
		$("body").append(data); // Append the instructions modal to the body of the document
	});
	
	invVarParent = $("#invariants > .col-md-12");
	invVarParentBlock = $("#invVarParentBlock").hide(); // Hide the invariants and variants
	
	resultsPanel = $("#resultsPanel").hide(); // Hide the panel containing the proofs
	
	var program = $("#program");
	program.on("input", countWhileLoops); // Set the callback for counting the number of while loops
	
	countWhileLoops();
});

// Counts the number of while loops within the given program
function countWhileLoops() {
	var program = $("#program");
	var text = program.val();
	
	var whiles = (text.match(/(\s+|^)while((\s*\()|\s+((true)|(false)))/g) || []).length;
	
	// If the number of while loops differs from the number
	// of invariant and variant inputs, regenerate them
	if (whiles != invVarParent.children().length) {
		generateInvVarInputs(whiles);
	}
}

// Generates sets of invariant and variant inputs according to count
function generateInvVarInputs(count) {
	if (count > 0) {
		invVarParentBlock.show(300);
	} else {
		invVarParentBlock.hide(300);
	}
	
	var numChildren = invVarParent.children().length;
	var delta = count - numChildren;
	
	if (delta < 0) {
		// Remove the extra inputs
		invVarParent.children().slice(numChildren + delta, numChildren).remove();
	} else {
		// Add the necessary number of inputs
		for (var i = numChildren; i < numChildren + delta; i++) {
			var invInput =
				$("<input/>")
					.addClass("form-control")
					.attr({
						name:			"invariant" + i,
						type:			"text",
						placeholder:	"Invariant " + (i + 1),
						required:		true
				});
			var varInput =
				invInput
					.clone()
					.attr({
						name:			"variant" + i,
						placeholder:	"Variant " + (i + 1)
					});
		
			var invDiv =
				$("<div/>")
					.addClass("col-sm-6")
					.append(invInput);
			var varDiv =
				invDiv
					.clone()
					.empty()
					.append(varInput);
		
			invVarParent.append(
				$("<div/>")
					.addClass("row")
					.addClass("top-bottom-padded")
					.append(invDiv)
					.append(varDiv)
			);
		}
	}
}

// Callback for submitting the form
function submitForm(event) {
	event.preventDefault(); // Prevent the page from being refreshed

	resultsPanel.show(300); // Show the proofs box
	
	// Send an AJAX request
	var $form = $(this),
		url = $form.attr('action'),
		data = $form.serialize();

	var posting = $.post(url, data);

	posting.done(function(data) {
		$("#results").empty().append(data);
	});
}

