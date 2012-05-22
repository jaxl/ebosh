/**
 * File: bosh.js
 */

/**
 * timestamp for display on user interface
 */
bosh.get_ts = function() {
	var a;
	var cT = new Date();
	var h = cT.getHours();
	var m = cT.getMinutes();
	if (h > 11) {
		h = h - 12;
		a = "PM";
	} else {
		a = "AM";
	}
	return h + ":" + m + " " + a;
};

/**
 * returns unix epoch timestamp
 */
bosh.epoch = function() {
	var d = new Date();
	return d.getTime();
};

/**
 * 
 */
bosh.is_empty_object = function(o) {
	for ( var i in o) {
		if (o.hasOwnProperty(i)) {
			return false;
		}
	}
	return true;
};

/**
 * uri encode utility
 */
bosh.e = function(e) {
	return encodeURIComponent(e);
};

/**
 * uri decode utility
 */
bosh.d = function(d) {
	return decodeURIComponent(d.replace(/\+/g, " "));
};

/**
 * returns difference between passed array
 */
bosh.array_diff = function(arr1) {
	var retArr = {}, argl = arguments.length, k1 = '', i = 1, k = '', arr = {};
	arr1keys: for (k1 in arr1) {
		for (i = 1; i < argl; i++) {
			arr = arguments[i];
			for (k in arr) {
				if (arr[k] === arr1[k1]) {
					// If it reaches here,
					// it was found in at least one array, so try next value
					continue arr1keys;
				}
			}
			retArr[k1] = arr1[k1];
		}
	}
	return retArr;
};

/**
 * converts text string into dom element
 */
bosh.text_to_xml = function(text) {
	var doc = null;

	if (window.DOMParser) {
		var parser = new DOMParser();
		doc = parser.parseFromString(text, 'text/xml');
	} else if (window.ActiveXObject) {
		doc = new ActiveXObject("MSXML2.DOMDocument");
		doc.async = false;
		doc.loadXML(text);
	} else {
		throw {
			type : 'Parse error',
			message : "No DOM parser object found."
		};
	}

	var error = doc.getElementsByTagName("parsererror");

	if (error.length > 0) {
		return null;
	}

	var node = document.importNode(doc.documentElement, true);
	return node;
};

/**
 * parses passed hash string (minus "#") returns as an object of key:value pair
 * if no parameter is passed, window.location.hash is parsed
 */
bosh.get_hash_obj = function(hash) {
	if (hash == undefined)
		hash = window.location.hash.substr(1, window.location.hash.length);
	var regex = /([^&=]+)=?([^&]*)/g;
	var params = {};
	while (e = regex.exec(hash))
		params[bosh.d(e[1])] = bosh.d(e[2]);
	return params;
};


$(document).ready(function() {

});