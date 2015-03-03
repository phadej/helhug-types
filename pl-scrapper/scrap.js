"use strict";

var request = require("request");
var cheerio = require("cheerio");
var fs = require("fs");
var crypto = require("crypto");
var _ = require("lodash");

function sha1(str) {
  var shasum = crypto.createHash("sha1");
  shasum.update(str);
  return shasum.digest("hex");
}

var startUrl = "http://en.wikipedia.org/wiki/Rust_(programming_language)";

var pending = [startUrl];
var mapping = {};

function getUrls($, url, ths, after) {
  var urls = [];

  ths = ths.filter(function (idx, th) {
    return $(th).text().trim().toLowerCase() === after;
  });

  var theTh = ths[0];
  if (!theTh) {
    console.log("no section", after, url);
    return [];
  }

  var theTr = $(theTh).parent().next();
  var links = $("a", theTr);

  links.each(function (idx, link) {
    var href = $(link).attr("href");
    if (href[0] === "#") return;
	if (href.match(/Wikipedia:Citation_needed/)) return;

    if (href.substr(0, 5) === "/wiki") {
      href = "http://en.wikipedia.org" + href;
      urls.push(href);
    }
  });

  return urls;
}

function fetch(url, callback) {
  var hash = sha1(url);
  try {
    var contents = fs.readFileSync("cache/" + hash, "utf8");
    setTimeout(function () {
      callback(contents);
    }, 1);
  } catch (e) {
    request(url, function (error, response, body) {
      fs.writeFileSync("cache/" + hash, body);
      callback(body);
    });
  }
}

function getRowData($, rowRegex) {
  var theTh = $("th[scope='row']").filter(function (idx, th) {
    return $(th).text().toLowerCase().trim().match(rowRegex);
  });

  return $(theTh).next().text();
}

function getAppearedIn($) {
  return getRowData($, /^appeared.in/);
}

function getTypingDiscipline($) {
  return getRowData($, /^typing.*discipline$/);
}

function loop() {
  if (pending.length === 0) {
    finish();
    return;
  }

  var url = pending.pop();

  if (mapping[url]) {
    loop();
    return;
  }

  console.log("Crawling", url);

  fetch(url, function (body) {
    var $ = cheerio.load(body);
    var ths = $("th");

    var title = $("title").text().replace(/ - Wikipedia.*$/, "").replace(/\s*\(programming\s+language\)/, "").trim();
    var appearedIn = getAppearedIn($).replace(/(\d{4}).*$/, "$1");
    var typingDiscipline = getTypingDiscipline($).split(/,\s*/);

    console.log("TITLE:       ", title);
    console.log("APPEAREAD IN:", appearedIn);
    console.log("TYPING:      ", typingDiscipline);

    var influencedBy = getUrls($, url, ths, "influenced by");
    var influenced = getUrls($, url, ths, "influenced");

    mapping[url] = {
      title: title,
      appearedIn: appearedIn,
      typingDiscipline: typingDiscipline,
      influenced: influenced,
      influencedBy: influencedBy,
    };

    pending = pending.concat(influenced).concat(influencedBy);

    loop();
  });
}

function mangle() {
  var idMapping = {};

  var counter = 0;
  _.chain(mapping).keys().each(function (url) {
    idMapping[url] = counter++;
  }).value();


  return _.map(mapping, function (pl, url) {
    var id = idMapping[url];
    var entry = {
      id: id,
      url: url,
      title: pl.title,
      typingDiscipline: pl.typingDiscipline.filter(_.identity).map(function (x) { return x.toLowerCase(); }),
      influencedBy: _.sortBy(pl.influencedBy.map(function (iurl) {
        return idMapping[iurl];
      })),
      influenced: _.sortBy(pl.influenced.map(function (iurl) {
        return idMapping[iurl];
      })),
    };

    if (pl.appearedIn) {
      var year = parseInt(pl.appearedIn, 10);
      if (year) {
        entry.appearedIn = year;
      }
    }

    return entry;
  });
}

function finish() {
  var data = mangle();
  fs.writeFileSync("results.json", JSON.stringify(data, null, 2));
}

loop();
