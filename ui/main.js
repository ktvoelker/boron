
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
        var id = 'x--build--' + build;
        var inputElem = dom.one('.build--click', buildElem);
        inputElem.setAttribute('id', id);
        dom.one('.build--name', buildElem).setAttribute('for', id);
        dom.one('.build--click', buildElem).addEventListener('click', function() {
          window.location.hash = '#/build/' + build;
        });
        buildsElem.appendChild(buildElem);
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
    updateBuilds();
    if (currentNumbers == build) {
      numbersElem.style.display = 'block';
      return;
    }
    currentNumbers = null;
    numbersElem.style.display = 'none';
    dom.clear(numbersElem);
    reqwest('/output/' + build + '/index.json', function(index) {
      forEachReversed(index, function(number) {
        var numberElem = template('.number', {'.number--number': number});
        var id = 'x--number--' + build + '--' + number;
        var inputElem = dom.one('.number--click', numberElem);
        inputElem.setAttribute('id', id);
        dom.one('.number--number', numberElem).setAttribute('for', id);
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
    updateNumbers(this.params.build);
    detailElem.style.display = 'none';
  });

  var currentNumber = null;

  function updateDetail(build, number) {
    updateNumbers(build);
    if (number === currentNumber) {
      detailElem.style.display = 'block';
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
    updateDetail(this.params.build, this.params.number);
  });

  Path.root('#/');
  Path.rescue(function() {
    console.error('Unknown URL');
  });
  Path.listen();

});

