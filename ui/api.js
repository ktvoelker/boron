
define('api', ['classy', 'q', 'reqwest'], function(classy, Q, reqwest) {

  var get = function(url) {
    var d = Q.defer();
    reqwest(url, d.resolve.bind(d), d.reject.bind(d));
    return d.promise;
  };

  var Url = classy.Class({
    constructor: function(url) {
      this.url = url;
    },
    get: function() {
      return get(this.url);
    }
  });

  var RunUrl = classy.Class({Extends: Url}, {
    constructor: function(build, number) {
      this.build = build;
      this.number = number;
      this.url = '/output/' + build.name + '/' + number + '.json';
    }
  });

  var BuildUrl = classy.Class({Extends: Url}, {
    constructor: function(name) {
      this.name = name;
      this.url = '/output/' + name + '/index.json';
    }
  });

  var BuildsUrl = classy.Class({Extends: Url}, {
    constructor: function() {
      this.url = '/output/builds.json';
    }
  });

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

  var property = function(name) {
    return function(x) {
      return x[name];
    };
  };

  var compose = function(g, f) {
    return function(x) {
      return g(f(x));
    }
  };

  const RUN_DETAILS = {
    'status': property('status'),
    'code': property('code'),
    'start': compose(maybeTime, property('start')),
    'end': compose(maybeTime, property('end'))
  };

  const FORMATTED_RUN_DETAILS = {
    'status': function() {
      return this.status.then(function(status) {
        return status[0].toLocaleUpperCase() + status.substring(1);
      })
    },
    'status_verbose': function() {
      return Q.all([this.status, this.code, this.formatted.status]).then(function(arr) {
        var result = arr[2];
        if (arr[0] == 'fail') {
          result += ' (' + arr[1] + ')';
        }
        return result;
      });
    },
    'elapsed': function() {
      return this.elapsed.then(function(elapsed) {
        return elapsed + ' ms';
      });
    },
    'start': function() {
      return this.start.then(formatTime);
    },
    'end': function() {
      return this.end.then(function(end) {
        return end == null ? 'Not yet' : formatTime(end);
      });
    }
  };

  var forEach = function(obj, f) {
    for (var key in obj) {
      f(obj[key], key);
    }
  };

  var Run = classy.Class({
    constructor: function(source) {
      var thisRun = this;
      this.build = source.build;
      this.number = source.number;
      this.source = source;
      Object.defineProperty(thisRun, 'details', {
        get: source.get.bind(source)
      });
      forEach(RUN_DETAILS, function(detail, name) {
        Object.defineProperty(thisRun, name, {
          get: function() {
            return thisRun.details.then(detail);
          }
        });
      });
      Object.defineProperty(thisRun, 'elapsed', {
        get: function() {
          return thisRun.end.then(function(end) {
            if (end === null) {
              end = new DateTime();
            }
            return thisRun.start.then(function(start) {
              return end - start;
            });
          });
        }
      });
      var formatted = {};
      forEach(FORMATTED_RUN_DETAILS, function(detail, name) {
        Object.defineProperty(formatted, name, {
          get: detail.bind(thisRun)
        });
      });
      Object.defineProperty(thisRun, 'formatted', {
        value: formatted
      });
    },
    load: function() {
      window.location.hash = '/build/' + this.build.name + '/' + this.number;
    }
  });

  function forEachReversed(arr, fn) {
    for (var i = arr.length - 1; i >= 0; --i) {
      fn(arr[i]);
    }
  }

  var LatestRunSource = classy.Class({
    constructor: function(json) {
      this.json = json;
      this.number = json.number;
    },
    get: function() {
      return Q(this.json);
    }
  });

  var Build = classy.Class({
    constructor: function(source) {
      var thisBuild = this;
      this.source = source;
      this.name = source.name;
      Object.defineProperty(this, 'runs', {
        get: function() {
          return thisBuild.source.get().then(function(data) {
            var numbers = data.all;
            var runs = [];
            numbers.sort(function(a, b) {
              return a - b;
            });
            forEachReversed(numbers, function(number) {
              runs.push(new Run(new RunUrl(thisBuild, number)));
            });
            return Q.all(runs);
          });
        }
      });
      Object.defineProperty(this, 'latest', {
        get: function() {
          return thisBuild.source.get().then(function(data) {
            return new Run(new LatestRunSource(data.latest));
          });
        }
      });
    },
    load: function() {
      window.location.hash = '/build/' + this.name;
    }
  });

  var getBuilds = function() {
    return (new BuildsUrl()).get().then(function(names) {
      return Q.all(names.map(function(name) {
        return new Build(new BuildUrl(name));
      }));
    });
  };

  return {
    getBuilds: getBuilds,
    Build: Build,
    Run: Run,
    BuildUrl: BuildUrl,
    RunUrl: RunUrl
  };
});

