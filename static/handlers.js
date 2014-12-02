var invVarParent;
var invVarParentBlock;
var programForm;

$(document).ready(function () {
	programForm = $("#programForm");
	programForm.submit(submitForm);
	
	var getting = $.get("/instructions");
	
	getting.done(function(data) {
		$("body").append(data);
	});
	
	invVarParent = $("#invariants");
	invVarParentBlock = $("#invVarParentBlock");
	invVarParentBlock.hide();
	
	var program = $("#program");
	program.on("input", countWhileLoops);
	
	countWhileLoops();
});

function countWhileLoops() {
	var program = $("#program");
	var text = program.val();
	
	var whiles = (text.match(/(\s+|^)while((\s*\()|\s+((true)|(false)))/g) || []).length;
	
	if (whiles != $("#invariants .col-md-12").length) {
		generateInvVarInputs(whiles);
	}
}

function generateInvVarInputs(count) {
	invVarParent.empty();
	
	if (count > 0) {
		invVarParentBlock.show(300);
	} else {
		invVarParentBlock.hide(300);
	}
	
	for (var i = 0; i < count; i++) {
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
				.addClass("col-md-12")
				.append(
					$("<div/>")
						.addClass("row")
						.addClass("top-bottom-padded")
						.append(invDiv)
						.append(varDiv)
				));
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

