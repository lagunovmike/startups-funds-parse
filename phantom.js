// scraper_PaddyPower.js

// Create a webpage object
var page = require('webpage').create();

// Include the File System module for writing to files
var fs = require('fs');

// Specify source and path to output file
var url  = 'https://navigator.sk.ru'
var path = 'skolkovo.html'

page.open(url, function (status) {
  var content = page.content;
  fs.write(path,content,'w')
  phantom.exit();
});