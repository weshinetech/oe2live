WstTimer = {};
WstTimer.secondsleft = 0;
WstTimer.prevDateNow = 0;
WstTimer.dateNowCheck = 1;
WstTimer.prevApiEventSecs = 0;;

WstTimer.onTimeOut = function () {
	var SecondsToDeduct = 1;
	// if ((WstTimer.prevDateNow > 0) && (WstTimer.dateNowCheck % 10 == 0)) {
	// 	var Diffseconds = (Date.now() - WstTimer.prevDateNow) / 1000 | 0;
	// 	if (Diffseconds < 0) Diffseconds = Diffseconds * -1;

	// 	SecondsToDeduct = Diffseconds - WstTimer.dateNowCheck;
	// 	if (SecondsToDeduct < 0) SecondsToDeduct = SecondsToDeduct * -1;
	// }
	WstTimer.secondsleft = WstTimer.secondsleft - SecondsToDeduct;
	WstTimer.dateNowCheck = WstTimer.dateNowCheck + SecondsToDeduct;
	if (WstTimer.secondsleft < 1) {
		window.clearInterval(WstTimer.TimerId);
	}
	if ((WstTimer.secondsleft % 60 == 0) || (WstTimer.secondsleft < 0) || ((WstTimer.prevApiEventSecs - WstTimer.secondsleft) > 60)) {
		page.timer_minute(WstTimer.secondsleft);
		WstTimer.prevApiEventSecs = WstTimer.secondsleft;
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
	WstTimer.prevDateNow = Date.now();
	WstTimer.prevApiEventSecs = WstTimer.secondsleft;
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

// // warn before page refresh
// window.onbeforeunload = function(e) {
// 	page.timer_minute(WstTimer.secondsleft);
// 	e = e || window.event;
// 	var message = "If you reload, you will be logged out and your exam will be interrupted.\n";
// 	if (e)
// 		e.returnValue = message;
// 	return message;
// }
