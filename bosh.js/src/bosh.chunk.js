try {
	eval("bosh");
} catch (e) {
	bosh = {};
}

jaxl.chunk = {
	pattern : /^(.*@END@)*(.*)@END@.*$/,
	start : function(url, callback) {
		var xmlhttp;

		if (window.XMLHttpRequest)
			xmlhttp = new XMLHttpRequest();
		else
			xmlhttp = new ActiveXObject("Microsoft.XMLHTTP");

		xmlhttp.onreadystatechange = function() {
			if (xmlhttp.readyState == 2 && xmlhttp.status == 200) {
				//jaxl.log(2,"server sent chunk response headers");
			} else if (xmlhttp.readyState == 3 && xmlhttp.status == 200) {
				response = xmlhttp.responseText;
				if (response.match(jaxl.chunk.pattern)) {
					var data = response.replace(jaxl.chunk.pattern, "$2");
					callback(data);
				}
			} else if (xmlhttp.readyState == 4 && xmlhttp.status == 200) {
				//jaxl.log(2,"server closed chunk response");
				response = xmlhttp.responseText;

				//jaxl.log(2,"got total response "+response);
				if (response.match(jaxl.chunk.pattern)) {
					var data = response.replace(jaxl.chunk.pattern, "$2");
					callback(data);
				}
			}
		};

		xmlhttp.open("GET", url, true);
		xmlhttp.setRequestHeader("Content-Type", "text/plain");
		xmlhttp.send();
	}
};