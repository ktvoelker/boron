
require.config({
  baseUrl: '/ui',
  paths: {
    'classy': 'ext/classy/src/classy',
    'path': 'ext/pathjs/path',
    'q': 'ext/q/q',
    'reqwest': 'ext/reqwest/reqwest'
  },
  shim: {
    'path': {
      exports: 'Path'
    }
  }
});

require(['dom', 'path', 'reqwest'], function(dom, router, reqwest) {

  var template = dom.templates('#templates');
  var buildsElem = dom.one('#builds');
  var numbersElem = dom.one('#numbers');
  var detailElem = dom.one('#detail');

  var buildsNeedUpdating = true;

  function checkBuild(build) {
    if (build !== null) {
      dom.one('#x--build--' + build).checked = true;
    }
  }

  function updateBuilds(currentBuild) {
    if (!buildsNeedUpdating) {
      checkBuild(currentBuild);
      buildsElem.style.display = 'block';
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
          hash = '#/build/' + build;
          if (hash != window.location.hash) {
            window.location.hash = hash;
          }
        });
        buildsElem.appendChild(buildElem);
      });
      checkBuild(currentBuild);
      buildsElem.style.display = 'block';
    });
    buildsNeedUpdating = false;
  }

  router.map('#/').to(function() {
    numbersElem.style.display = 'none';
    detailElem.style.display = 'none';
    updateBuilds(null);
  });

  var currentNumbers = null;

  function forEachReversed(arr, fn) {
    for (var i = arr.length - 1; i >= 0; --i) {
      fn(arr[i]);
    }
  }

  function checkNumber(build, number) {
    if (number !== null) {
      dom.one('#x--number--' + build + '--' + number).checked = true;
    }
  }

  function updateNumbers(build, currentNumber) {
    updateBuilds(build);
    if (currentNumbers == build) {
      checkNumber(build, currentNumber);
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
        dom.one('.number--click', numberElem).addEventListener('change', function() {
          hash = '#/build/' + build + '/' + number;
          if (hash != window.location.hash) {
            window.location.hash = hash;
          }
        });
      });
      checkNumber(build, currentNumber);
      numbersElem.style.display = 'block';
    });
    currentNumbers = build;
  }

  router.map('#/build/:build').to(function() {
    updateNumbers(this.params.build, null);
    detailElem.style.display = 'none';
  });

  var currentNumber = null;

  function updateDetail(build, number) {
    updateNumbers(build, number);
    if (number === currentNumber) {
      detailElem.style.display = 'block';
      return;
    }
    currentNumber = null;
    detailElem.style.display = 'none';
    reqwest('/output/' + build + '/' + number, function(detail) {
      dom.one('#detail--number').innerText = number;
      dom.one('#detail--result').innerText =
        detail.status + ('code' in detail ? ' (' + detail.code + ')' : '');
      dom.one('#detail--start').innerText = detail.start;
      if ('end' in detail) {
        dom.one('#detail--elapsed').innerText =
          (new Date(detail.end) - new Date(detail.start)) + ' ms';
        dom.one('#detail--end').innerText = detail.end;
      } else {
        dom.one('#detail--elapsed').innerText = '';
        dom.one('#detail--end').innerText = '';
      }
      detailElem.style.display = 'block';
    });
    currentNumber = number;
  }

  router.map('#/build/:build/:number').to(function() {
    updateDetail(this.params.build, this.params.number);
  });

  router.root('#/');
  router.rescue(function() {
    console.error('Unknown URL');
  });
  router.listen();

});

