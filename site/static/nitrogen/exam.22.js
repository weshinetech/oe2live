WstTimer = {};
WstTimer.secondsleft = 0;

WstTimer.onTimeOut = function () {
	WstTimer.secondsleft = WstTimer.secondsleft - 1;
	if (WstTimer.secondsleft < 1) {
		window.clearInterval(WstTimer.TimerId);
	}
	if ((WstTimer.secondsleft % 60 == 0) || (WstTimer.secondsleft < 0)) {
		page.timer_minute(WstTimer.secondsleft);
	}

	WstTimer.updateElement();
};

WstTimer.unload = function () {
	window.clearInterval(WstTimer.TimerId);
	page.unload(WstTimer.secondsleft);
};

WstTimer.start = function (secondsleft) {
	WstTimer.secondsleft = secondsleft;
	WstTimer.TimerId = window.setInterval(WstTimer.onTimeOut, 1000);
};

WstTimer.updateElement = function () {

	var mins = Math.floor(WstTimer.secondsleft / 60);
	var secs = WstTimer.secondsleft % 60;

	var label = "label-info";
	if (mins < 2) {
		label = "label-important";
	} else if (mins < 10) {
		label = "label-warning";
	}

	var html = "<div class='ex_element label " + label + "'><span id='ex_mins'> " + mins + "</span>m <span id='ex_secs'>" + secs + "</span>s</div>";

	$(".exam_timer").html(html);
};

// browser restrictions
// disable right click
document.oncontextmenu=RightMouseDown;
document.onmousedown = mouseDown;
function mouseDown(e) {
	if (e.which==3) {
		return false;
	}
}
function RightMouseDown() {return false;}