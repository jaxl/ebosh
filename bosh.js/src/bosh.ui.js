try {
	eval("bosh");
} catch (e) {
	bosh = {};
}

bosh.ui = {
	// we cache original page title, so that we can toggle with our custom message later
	page_title : "",

	// this is timer reference which toggles page title
	page_title_timer : null,

	// ui window in focus/blur
	active : true,

	// ui callbacks
	hook : null,

	//
	onload : function() {
		// cache original page title
		bosh.ui.page_title = document.title;

		// initialize ui hooks object
		bosh.ui.hook = new bosh.hook();

		// register handler for focus/blur on chat window
		if (/*@cc_on!@*/false) { // check for Internet Explorer
			document.onfocusin = bosh.ui.on_focus;
			document.onfocusout = bosh.ui.on_blur;
		} else {
			window.onfocus = bosh.ui.on_focus;
			window.onblur = bosh.ui.on_blur;
		}

		// on window unload handlers
		$(window).unload(function() {
			bosh.ui.on_unload();
		});

		// resize canvas
		$(window).resize(function() {
			bosh.ui.on_resize();
		});
	},

	on_resize : function() {
		bosh.ui.hook.execute('on_window_resize');
	},

	on_unload : function() {
		bosh.ui.hook.execute('on_window_unload');
	},

	on_focus : function() {
		bosh.ui.active = true;
		bosh.ui.hook.execute('on_window_focus');
	},

	on_blur : function() {
		bosh.ui.active = false;
		bosh.ui.hook.execute('on_window_blur');
	},

	reset_page_title : function() {
		document.title = bosh.ui.page_title;
		clearTimeout(bosh.ui.page_title_timer);
		bosh.ui.page_title_timer = null;
	},

	toggle_page_title : function(title) {
		var old_title = document.title;
		var new_title = (document.title == bosh.ui.page_title) ? title
				: bosh.ui.page_title;
		document.title = new_title;
		bosh.ui.page_title_timer = setTimeout("bosh.ui.toggle_page_title('"
				+ title + "');", 1000);
	}
};

$(document).ready(function() {
	bosh.ui.onload();
});