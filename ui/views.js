
define('views', ['api', 'q', 'dom', 'classy'], function(api, Q, dom, classy) {

  var List = classy.Class({
    constructor: function(children, ordered) {
      this.children = children;
      this.ordered = ordered;
    },
    render: function() {
      var elem = dom.create(this.ordered ? 'ol' : 'ul', {class: 'list'});
      this.children.forEach(function(child) {
        var li = dom.create('li');
        li.appendChild(child.render());
        elem.appendChild(li);
      });
      return elem;
    }
  });

  var RadioButton = classy.Class({
    constructor: function(label, name, value, checked) {
      this.label = label;
      this.name = name;
      this.value = value;
      this.checked = checked;
    },
    activate: function() {
      console.warn('Subclass did not implement RadioButton.activate!');
    },
    render: function() {
      var elem = dom.create('div', {class: 'radio'});
      var id = dom.genId();
      var attrs = {
        type: 'radio',
        name: this.name,
        value: this.value,
        id: id
      };
      if (this.checked) {
        attrs.checked = 'checked';
      }
      var radio = dom.create('input', attrs);
      radio.addEventListener('change', this.activate.bind(this));
      elem.appendChild(radio);
      var label = dom.create('label', {'for': id});
      var labelTitle = dom.create('span', {'class': 'title'});
      labelTitle.innerText = this.label;
      label.appendChild(labelTitle);
      var labelData = this.renderLabelData();
      if (labelData != null) {
        label.appendChild(labelData);
      }
      elem.appendChild(label);
      return elem;
    },
    renderLabelData: function() {
      return null;
    }
  });

  var renderStatus = function(elem, run) {
    run.status.done(function(status) {
      dom.addClass(elem, 'status-' + status);
    });
  };

  var renderTime = function(elem, run) {
    dom.addClass(elem, 'time');
    run.formatted.start.done(function(start) {
      elem.innerText = start;
    });
  };

  var BuildButton = classy.Class({Extends: RadioButton}, {
    constructor: function(build, checked) {
      this.build = build;
      this.$super('constructor', build.name, 'build', build.name, checked);
    },
    activate: function() {
      this.build.load();
    },
    render: function() {
      var elem = this.$super('render');
      this.build.latest.then(function(run) {
        renderStatus(elem, run);
      });
      return elem;
    },
    renderLabelData: function() {
      var elem = dom.create('span');
      this.build.latest.then(function(run) {
        renderTime(elem, run);
      });
      return elem;
    }
  });

  var RunButton = classy.Class({Extends: RadioButton}, {
    constructor: function(run, checked) {
      this.run = run;
      this.$super('constructor', run.number, 'run', run.number, checked);
    },
    activate: function() {
      this.run.load();
    },
    render: function() {
      var elem = this.$super('render');
      renderStatus(elem, this.run);
      return elem;
    },
    renderLabelData: function() {
      var elem = dom.create('span');
      renderTime(elem, this.run);
      return elem;
    }
  });

  var RunDetail = classy.Class({
    constructor: function(run) {
      this.run = run;
    },
    render: function() {
      var elem = dom.create('div', {'class': 'detail'});
      var title = dom.create('h2');
      title.innerText = this.run.build.name + ': ' + this.run.number;
      elem.appendChild(title);
      var table = dom.create('table');
      var tbody = dom.create('tbody');
      var addFact = function(key, value) {
        var tr = dom.create('tr');
        var tdKey = dom.create('td');
        tdKey.innerText = key;
        tr.appendChild(tdKey);
        var tdValue = dom.create('dd');
        value.done(function(value) {
          tdValue.innerText = value;
        });
        tr.appendChild(tdValue);
        tbody.appendChild(tr);
      };
      var f = this.run.formatted;
      addFact('Status', f.status_verbose);
      addFact('Started', f.start);
      addFact('Ended', f.end);
      addFact('Elapsed', f.elapsed);
      table.appendChild(tbody);
      elem.appendChild(table);
      return elem;
    }
  });

  var Boron = classy.Class({Extends: List}, {
    constructor: function() {
      this.parentForBuilds = dom.one('#parent-for-builds');
      this.parentForRuns = dom.one('#parent-for-runs');
      this.parentForDetail = dom.one('#parent-for-detail');
    },
    renderRoot: function(selectedBuild) {
      var thisBoron = this;
      dom.clear(this.parentForDetail);
      dom.clear(this.parentForRuns);
      dom.clear(this.parentForBuilds);
      api.getBuilds().done(function(builds) {
        var views = builds.map(function(build) {
          return new BuildButton(
            build,
            selectedBuild != null && build.name == selectedBuild.name
          );
        });
        var view = new List(views);
        var elem = view.render();
        thisBoron.parentForBuilds.appendChild(elem);
      });
    },
    renderBuild: function(build, selectedRun) {
      var thisBoron = this;
      this.renderRoot(build);
      build.runs.done(function(runs) {
        var view = new List(runs.map(function(run) {
          return new RunButton(
            run,
            selectedRun != null && run.number == selectedRun.number
          );
        }));
        thisBoron.parentForRuns.appendChild(view.render());
      });
    },
    renderRun: function(run) {
      var thisBoron = this;
      thisBoron.renderBuild(run.build, run);
      var view = new RunDetail(run);
      thisBoron.parentForDetail.appendChild(view.render());
    }
  });

  return {
    List: List,
    RadioButton: RadioButton,
    BuildButton: BuildButton,
    RunButton: RunButton,
    RunDetail: RunDetail,
    Boron: Boron
  };

});

