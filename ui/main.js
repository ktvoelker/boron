
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

require(['router'], function(router) {
  router.run();
});

