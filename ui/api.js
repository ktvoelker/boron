
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
      return new DateTime(x);
    });
  };

  var initRun = function(details) {
    this.status = details.status;
    this.code = details.code;
    this.start = maybeTime(details.start);
    this.end = maybeTime(details.end);
    if (this.end === null) {
      this.elapsed = new DateTime() - this.start;
    } else {
      this.elapsed = this.end - this.start;
    }
  };

  var Run = classy.Class({
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

  var Build = classy.Class({
    constructor: function(name) {
      this.name = name;
      this.url = '/output/' + name + '.json';
      this.runs = null;
    },
    getRuns: function(skipCache) {
      var thisBuild = this;
      if (skipCache || this.runs === null) {
        this.runs = get(this.url, true).then(function(runFiles) {
          return runFiles.map(function(runFile) {
            return new Lazy(function() {
              var runUrl = '/output/' + thisBuild.name + '/' + runFile;
              return get(runUrl, function(dets) {
                return new Run(thisBuild, runUrl, dets);
              });
            });
          });
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
          return new Build(buildName);
        });
      });
    }
    return builds;
  };
});

