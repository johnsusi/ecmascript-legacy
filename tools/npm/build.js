var mkdirp = require('mkdirp').sync;
var exec = require('child_process').execSync;
var path = require('path');

mkdirp('build');

console.log( exec("cmake .. -GNinja -Wno-dev", {
  cwd: path.join(process.cwd(), 'build')
}).toString() );

console.log( exec("cmake --build build").toString() );
