
require(['ui/dom', 'ui/ext/reqwest/reqwest'], function(dom, reqwest) {

  var template = dom.templates('#templates');
  var buildsElem = dom.one('#builds');
  var numbersElem = dom.one('#numbers');
  var detailElem = dom.one('#detail');

  var buildsNeedUpdating = true;

  function updateBuilds() {
    if (!buildsNeedUpdating) {
      return;
    }
    buildsNeedUpdating = true;
    buildsElem.style.display = 'none';
    dom.clear(buildsElem);
    reqwest('/output/builds.json', function(builds) {
      builds.forEach(function(build) {
        var buildElem = template('.build', {'.build--name': build});
        buildsElem.appendChild(buildElem);
        dom.one('.build--click', buildElem).addEventListener('click', function() {
          window.location.hash = '#/build/' + build;
        });
      });
      buildsElem.style.display = 'block';
    });
    buildsNeedUpdating = false;
  }

  Path.map('#/').to(function() {
    numbersElem.style.display = 'none';
    detailElem.style.display = 'none';
    updateBuilds();
  });

  var currentNumbers = null;

  function forEachReversed(arr, fn) {
    for (var i = arr.length - 1; i >= 0; --i) {
      fn(arr[i]);
    }
  }

  function updateNumbers(build) {
    if (currentNumbers == build) {
      return;
    }
    currentNumbers = null;
    numbersElem.style.display = 'none';
    dom.clear(numbersElem);
    reqwest('/output/' + build + '/index.json', function(index) {
      forEachReversed(index, function(number) {
        var numberElem = template('.number', {'.number--number': number});
        numbersElem.appendChild(numberElem);
        dom.one('.number--click', numberElem).addEventListener('click', function() {
          window.location.hash = '#/build/' + build + '/' + number;
        });
      });
      numbersElem.style.display = 'block';
    });
    currentNumbers = build;
  }

  Path.map('#/build/:build').to(function() {
    updateBuilds();
    updateNumbers(this.params.build);
    detailElem.style.display = 'none';
  });

  var currentNumber = null;

  function updateDetail(build, number) {
    if (number === currentNumber) {
      return;
    }
    currentNumber = null;
    detailElem.style.display = 'none';
    reqwest('/output/' + build + '/' + number, function(detail) {
      dom.one('#detail--result').innerText =
        detail.ok ? 'Pass' : 'Fail (' + detail.code + ')';
      dom.one('#detail--elapsed').innerText =
        (new Date(detail.end) - new Date(detail.start)) + ' ms';
      dom.one('#detail--start').innerText = detail.start;
      dom.one('#detail--end').innerText = detail.end;
      detailElem.style.display = 'block';
    });
    currentNumber = number;
  }

  Path.map('#/build/:build/:number').to(function() {
    updateBuilds();
    updateNumbers(this.params.build);
    updateDetail(this.params.build, this.params.number);
  });

  Path.root('#/');
  Path.rescue(function() {
    console.error('Unknown URL');
  });
  Path.listen();

});

