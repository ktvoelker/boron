
define('views', ['dom', 'classy'], function(dom, classy) {

  var List = classy.Class({
    constructor: function(children, ordered) {
      this.children = children;
      this.ordered = ordered;
    },
    render: function() {
      var elem = dom.create(this.ordered ? 'ol' : 'ul');
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
      var elem = dom.create('div');
      var id = dom.genId();
      var radio = dom.create('input', {
        type: 'radio',
        name: this.name,
        value: this.value,
        checked: this.checked,
        id: id
      });
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
    constructor: function(build, selected) {
      this.build = build;
      this.$super('constructor', build.name, 'build', build.name, selected);
    },
    activate: function() {
      var target = dom.one('#parent-for-runs');
      this.build.getRuns().then(function(runs) {
        var view = new List(runs.map(function(run) { return new RunButton(run); }));
        view.render(target);
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
      var view = new RunDetail(this.run);
      view.render(target);
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

  return {
    List: List,
    RadioButton: RadioButton,
    BuildButton: BuildButton,
    RunButton: RunButton,
    RunDetail: RunDetail
  };

});

