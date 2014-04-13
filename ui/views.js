
define('views', ['api', 'dom', 'classy'], function(api, dom, classy) {

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
      var activateThis = this.activate.bind(this);
      radio.addEventListener('change', activateThis);
      elem.appendChild(radio);
      var label = dom.create('label', {'for': id});
      label.innerText = this.label;
      elem.appendChild(label);
      if (this.checked) {
        window.setTimeout(activateThis, 0);
      }
      return elem;
    }
  });

  var BuildButton = classy.Class({Extends: RadioButton}, {
    constructor: function(build, selected, selectedRun) {
      this.build = build;
      this.selectedRun = selectedRun;
      this.$super('constructor', build.name, 'build', build.name, selected);
    },
    activate: function() {
      var thisButton = this;
      var target = dom.one('#parent-for-runs');
      dom.clear(target);
      this.build.getRuns().done(function(runs) {
        var view = new List(runs.map(function(run) {
          // TODO "run" is a Promise, not a Run
          console.log(run);
          var selected = false;
          if (thisButton.selectedRun === run) {
            thisButton.selectedRun = null;
            selected = true;
          }
          return new RunButton(run, selected);
        }));
        target.appendChild(view.render());
      });
    }
  });

  var RunButton = classy.Class({Extends: RadioButton}, {
    constructor: function(run, selected) {
      this.run = run;
      this.$super('constructor', run.number, 'run', run.number, selected);
    },
    activate: function() {
      var target = dom.one('#parent-for-detail');
      dom.clear(target);
      var view = new RunDetail(this.run);
      target.appendChild(view.render());
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
        tdValue.innerText = value;
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
    renderRoot: function(selectedBuild, selectedRun) {
      var thisBoron = this;
      dom.clear(this.parentForDetail);
      dom.clear(this.parentForRuns);
      dom.clear(this.parentForBuilds);
      api.getBuilds().done(function(builds) {
        var views = builds.map(function(build) {
          return new BuildButton(build, build === selectedBuild, selectedRun);
        });
        var view = new List(views);
        var elem = view.render();
        thisBoron.parentForBuilds.appendChild(elem);
      });
    },
    renderBuild: function(build) {
      this.renderRoot(build);
    },
    renderRun: function(run) {
      this.renderRoot(run.build, run);
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

