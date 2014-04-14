
define('api', ['classy', 'q', 'reqwest'], function(classy, Q, reqwest) {

  var cache = {};

  var get = function(url, skipCache) {
    if (!skipCache && url in cache) {
      return cache[url];
    }
    var d = Q.defer();
    cache[url] = d.promise;
    reqwest(url, d.resolve.bind(d), d.reject.bind(d));
    return d.promise;
  };

  var fmap = function(x, f) {
    return x === null ? null : f(x);
  };

  var maybeTime = function(x) {
    return fmap(x, function(x) {
      return new Date(x);
    });
  };

  var formatTime = function(dt) {
    return dt.toLocaleString();
  };

  var formatRunFacts = function() {
    var formatted = {};
    formatted.status = this.status[0].toLocaleUpperCase() + this.status.substring(1);
    formatted.status_verbose = formatted.status;
    if (this.status == 'fail') {
      formatted.status_verbose += ' (' + this.code + ')';
    }
    formatted.elapsed = this.elapsed + ' ms';
    formatted.start = formatTime(this.start);
    formatted.end = this.end == null ? 'Not yet' : formatTime(this.end);
    this.formatted = formatted;
  }

  var initRun = function(details) {
    this.number = details.number;
    this.status = details.status;
    this.code = details.code;
    this.start = maybeTime(details.start);
    this.end = maybeTime(details.end);
    if (this.end === null) {
      this.elapsed = new DateTime() - this.start;
    } else {
      this.elapsed = this.end - this.start;
    }
    formatRunFacts.call(this);
  };

  var Run = classy.Class({
    __static__: {
      cache: {},
      instance: function(build, number) {
        if (!(build.name in this.cache)) {
          this.cache[build.name] = {};
        }
        if (!(number in this.cache[build.name])) {
          var runUrl = '/output/' + build.name + '/' + number + '.json';
          this.cache[build.name][number] = get(runUrl).then(function(details) {
            return new Run(build, runUrl, details);
          });
        }
        return this.cache[build.name][number];
      }
    },
    constructor: function(build, url, details) {
      this.build = build;
      this.url = url;
      initRun.call(this, details);
    },
    isDone: function() {
      return this.status == 'pass' || this.status == 'fail';
    },
    refresh: function() {
      if (this.isDone()) {
        return Q(this);
      }
      var thisRun = this;
      return get(this.url, true).then(function(details) {
        initRun.call(thisRun, details);
        return thisRun;
      });
    }
  });

  var Lazy = classy.Class({
    __static__: {
      create: function(f) {
        return Q(new Lazy(f));
      }
    },
    constructor: function(f) {
      this.f = f;
    },
    then: function(onFulfilled, onRejected, onProgress) {
      var f = this.f;
      this.f = null;
      this.p = f();
      return p.then(onFulfilled, onRejected, onProgress);
    }
  });

  function forEachReversed(arr, fn) {
    for (var i = arr.length - 1; i >= 0; --i) {
      fn(arr[i]);
    }
  }

  var Build = classy.Class({
    __static__: {
      cache: {},
      instance: function(name) {
        if (!(name in this.cache)) {
          this.cache[name] = new Build(name);
        }
        return this.cache[name];
      }
    },
    constructor: function(name) {
      this.name = name;
      this.url = '/output/' + name + '/index.json';
      this.runs = null;
    },
    getRuns: function(skipCache) {
      var thisBuild = this;
      if (skipCache || this.runs === null) {
        this.runs = get(this.url, true).then(function(numbers) {
          var runs = [];
          numbers.sort(function(a, b) {
            return a - b;
          });
          forEachReversed(numbers, function(number) {
            runs.push(Run.instance(thisBuild, number));
          });
          return Q.all(runs);
        });
      }
      return this.runs;
    }
  });

  var builds = null;

  var getBuilds = function(skipCache) {
    if (skipCache || builds === null) {
      builds = get('/output/builds.json').then(function(buildNames) {
        return buildNames.map(function(buildName) {
          return Build.instance(buildName);
        });
      });
    }
    return builds;
  };

  return {
    getBuilds: getBuilds,
    Build: Build,
    Run: Run
  };
});

