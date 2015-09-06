arguments.forEach(function (filename) {

  print(
    JSON.stringify(
      Reflect.parse(read(filename), {
        loc: false,
        source: false,
        line: false,
      }), null, 2))
});
