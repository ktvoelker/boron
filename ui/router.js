
define('router', ['api', 'views', 'path'], function(api, views, path) {

  var run = function() {

    var view = new views.Boron();

    path.map('#/').to(function() {
      view.renderRoot();
    });

    path.map('#/build/:build').to(function() {
      view.renderBuild(api.Build.instance(this.params.build));
    });

    path.map('#/build/:build/:number').to(function() {
      var number = this.params.number;
      api.Build.instance(this.params.build).done(function(build) {
        view.renderRun(api.Run.instance(build, number));
      });
    });

    path.root('#/');

    path.rescue(function() {
      console.error('Unknown URL');
    });

    path.listen();
  };

  return {run: run};

});

