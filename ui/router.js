
define('router', ['api', 'views', 'path'], function(api, views, path) {

  var run = function() {

    var view = new views.Boron();

    path.map('#/').to(function() {
      view.renderRoot();
    });

    path.map('#/build/:build').to(function() {
      view.renderBuild(Build.instance(this.params.build));
    });

    path.map('#/build/:build/:number').to(function() {
      view.renderRun(Run.instance(this.params.build, this.params.number));
    });

    path.root('#/');

    path.rescue(function() {
      console.error('Unknown URL');
    });

    path.listen();
  };

  return {run: run};

});

