try {
	eval("bosh");
} catch (e) {
	bosh = {};
}

bosh.utf8_encode = function(argString) {
	if (argString === null || typeof argString === "undefined") {
		return "";
	}

	var string = (argString + ''); // .replace(/\r\n/g, "\n").replace(/\r/g, "\n");
	var utftext = "", start, end, stringl = 0;

	start = end = 0;
	stringl = string.length;
	for ( var n = 0; n < stringl; n++) {
		var c1 = string.charCodeAt(n);
		var enc = null;

		if (c1 < 128) {
			end++;
		} else if (c1 > 127 && c1 < 2048) {
			enc = String.fromCharCode((c1 >> 6) | 192)
					+ String.fromCharCode((c1 & 63) | 128);
		} else {
			enc = String.fromCharCode((c1 >> 12) | 224)
					+ String.fromCharCode(((c1 >> 6) & 63) | 128)
					+ String.fromCharCode((c1 & 63) | 128);
		}

		if (enc !== null) {
			if (end > start) {
				utftext += string.slice(start, end);
			}
			utftext += enc;
			start = end = n + 1;
		}
	}

	if (end > start) {
		utftext += string.slice(start, stringl);
	}

	return utftext;
}