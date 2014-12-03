var invVarParent;
var invVarParentBlock;
var programForm;
var resultsPanel;

$(document).ready(function () {
	programForm = $("#programForm");
	programForm.submit(submitForm);
	
	var getting = $.get("/instructions");
	
	getting.done(function(data) {
		$("body").append(data);
	});
	
	invVarParent = $("#invariants > .col-md-12");
	invVarParentBlock = $("#invVarParentBlock").hide();
	
	resultsPanel = $("#resultsPanel").hide();
	
	var program = $("#program");
	program.on("input", countWhileLoops);
	
	countWhileLoops();
});

function countWhileLoops() {
	var program = $("#program");
	var text = program.val();
	
	var whiles = (text.match(/(\s+|^)while((\s*\()|\s+((true)|(false)))/g) || []).length;
	
	if (whiles != invVarParent.children().length) {
		generateInvVarInputs(whiles);
	}
}

function generateInvVarInputs(count) {
	if (count > 0) {
		invVarParentBlock.show(300);
	} else {
		invVarParentBlock.hide(300);
	}
	
	var numChildren = invVarParent.children().length;
	var delta = count - numChildren;
	
	if (delta < 0) {
		invVarParent.children().slice(numChildren + delta, numChildren).remove();
	} else {
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

function submitForm(event) {
	event.preventDefault();

	resultsPanel.show(300);
	
	var $form = $(this),
		url = $form.attr('action'),
		data = $form.serialize();

	var posting = $.post(url, data);

	posting.done(function(data) {
		$("#results").empty().append(data);
	});
}

