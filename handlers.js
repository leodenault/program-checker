window.addEventListener("load", onLoad)

var programArea;

function onLoad() {
	programArea = document.getElementById("program");
	programForm = document.getElementById("programForm");
	programForm.addEventListener("submit", onCodeSubmit);
}

function onCodeSubmit() {
	alert("WHEEEE");
}
