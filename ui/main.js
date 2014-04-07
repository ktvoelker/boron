
require(['ui/dom', 'ui/ext/reqwest/reqwest'], function(dom, reqwest) {

  var template = dom.templates('#templates');
  var buildsElem = dom.one('#builds');
  var buildElems = {};

  function expandRun(name, run, runElem) {
    reqwest('/output/' + name + '/' + run, function(info) {
      var elem = template('.run--detail', {
        '.run--start': info.start,
        '.run--end': info.end,
        '.run--result': info.ok ? 'Success' : 'Failure (' + info.code + ')'
      });
      runElem.appendChild(elem);
    });
  }

  function addBuild(name, runs) {
    var elem = template('.build', {'.build--name': name});
    buildElems[name] = elem;
    buildsElem.appendChild(elem);
    for (var i = runs.length - 1; i >= 0; --i) {
      (function(i) {
        var runElem = template('.run', {'.run--number': runs[i]});
        dom.one('.run--number', runElem).addEventListener('click', function() {
          var detailElem = dom.one('.run--detail', runElem);
          if (detailElem) {
            dom.toggle(detailElem, 'block');
          } else {
            expandRun(name, runs[i], runElem);
          }
        });
        elem.appendChild(runElem);
        if (i == runs.length - 1) {
          expandRun(name, runs[i], runElem);
        }
      })(i);
    }
  }

  reqwest('/output/builds.json', function(builds) {
    builds.forEach(function(build) {
      reqwest('/output/' + build + '/index.json', function(runs) {
        addBuild(build, runs);
      });
    });
  });
});

