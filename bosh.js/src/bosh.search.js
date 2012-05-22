try {
	eval("bosh");
} catch (e) {
	bosh = {};
}

/**
 * taken directly from ladders search
 */
bosh.search = {};
bosh.search = {
	/**
	 * Search Index.
	 * 
	 * @constructor
	 */
	index : function() {
		this._index = {};
		this._analyzer = new bosh.search.StandardAnalyzer();
	},

	/**
	 * A document to be indexed.
	 * 
	 * @constructor
	 */
	document : function() {
		this._fields = {};
	},

	/**
	 * Search hit.
	 * 
	 * @constructor
	 */
	hit : function() {
		this._matchedFields = 0;
		this._score = 0;
	},

	/**
	 * StandardAnalyzer
	 * 
	 * @constructor
	 */
	StandardAnalyzer : function() {
	},

	/**
	 * Default list of stop words. These words will be removed from both indexed
	 * text and query strings.
	 */
	STOP_WORDS : [ "a", "an", "and", "are", "as", "at", "be", "but", "by",
			"for", "if", "in", "into", "is", "it", "no", "no", "not", "of",
			"on", "or", "s", "such", "t", "that", "the", "their", "they",
			"then", "there", "these", "this", "to", "was", "will", "with" ]
};

/**
 * Initialize stop words.
 */
bosh.search.StandardAnalyzer.prototype._stopWords = function(stopWords) {
	var result = {};
	for ( var i = 0; i < stopWords.length; i++) {
		result[stopWords[i]] = 1;
	}
	return result;
}(bosh.search.STOP_WORDS);

/**
 * Converts a string into an array of tokens, removing stopwords and
 * punctuation.
 */
bosh.search.StandardAnalyzer.prototype.tokens = function(s) {
	var results = new Array();

	var words = s.toLowerCase().replace(/-/g, " ").split(/\s+/);

	for ( var i = 0; i < words.length; i++) {
		var w = words[i].replace(/[,\.'"-]+/g, "");

		if (w.length > 0 && (!this._stopWords[w])) {
			results[results.length] = w;
		}
	}

	return results;
}

/**
 * Add a document to the index.
 * 
 */
bosh.search.index.prototype.addDocument = function(d) {
	for ( var f in d._fields) {
		// only index string fields
		if (typeof d._fields[f] == 'string') {
			var words = this._analyzer.tokens(d._fields[f]);

			for ( var i = 0; i < words.length; i++) {
				var word = words[i];

				var id = d._fields.id;
				this._markOccurance(id, word);
			}
		}
	}
};

bosh.search.index.prototype._getOccurances = function(word) {
	var c = word.charAt(0);

	if (!this._index[c]) {
		this._index[c] = {};
	}

	if (!this._index[c][word]) {
		this._index[c][word] = {};
	}

	return this._index[c][word];
};

bosh.search.index.prototype._markOccurance = function(id, word) {
	var a = this._getOccurances(word);

	if (!a[id]) {
		a[id] = 1;
	} else {
		a[id]++;
	}
};

/**
 * Index search.
 * 
 */
bosh.search.index.prototype.search = function(query) {
	var words = this._analyzer.tokens(query);

	var results = {};

	var findWild = /(.+)\*$/;

	for ( var i = 0; i < words.length; i++) {
		var w;

		var match = words[i].match(findWild);

		// we have a keyword with a wildcard
		if (match != null) {
			w = this._escape(match[1]);

			var wr = this.searchWildcard(w);

			for ( var j = 0; j < wr.length; j++) {
				// new document match
				if (!results[wr[j]]) {
					results[wr[j]] = new bosh.search.hit();
				}

				// increase document score
				results[wr[j]]._matchedFields++;
				results[wr[j]]._score += 1;
			}

			continue;
		}

		// we have a non-wildcard keyword
		w = this._escape(words[i]);

		// in index?
		var docs = this._getOccurances(w);

		if (docs) {
			for ( var d in docs) {
				// new document match
				if (!results[d]) {
					results[d] = new bosh.search.hit();
				}

				// increase document score
				results[d]._matchedFields++;
				results[d]._score += docs[d];
			}
		}
	}

	var ar = new Array();

	// AND match, all words must match
	var needed = words.length;

	for ( var r in results) {
		var hit = results[r];

		if (hit._matchedFields == needed) {
			ar[ar.length] = r;
		}
	}

	return ar;
};

/**
 * Perform a "starts-with" match against the index.
 */
bosh.search.index.prototype.searchWildcard = function(word) {
	var pattern = new RegExp(word.toLowerCase() + ".*", "g");

	var results = {};

	// go through all words in the index...

	var possible = this._index[word.charAt(0)];

	for ( var k in possible) {
		if (pattern.test(k)) {
			for ( var d in possible[k]) {
				// always mark as one hit
				results[d] = 1;
			}
		}
	}

	var ar = new Array();
	for (d in results) {
		ar[ar.length] = d;
	}

	return ar;
}

/**
 * escape our strings.
 */
bosh.search.index.prototype._escape = function(word) {
	var quote = /([\\\!\@\#\$\%\^\&\.\+\?])/g;
	return word.replace(quote, "\\$1");
}

/**
 * Add a fieldName, fieldText pair to the document.
 *
 * Adding the same field more than once erases the previous contents.
 *
 */
bosh.search.document.prototype.add = function(k, v) {
	this._fields[k] = v;
};

/**
 * Get the fieldText for a given fieldName from a document.
 *
 */
bosh.search.document.prototype.get = function(k) {
	return this._fields[k];
};