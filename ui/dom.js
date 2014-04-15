
define('dom', [], function() {

  var idCounter = 0;

  function genId() {
    var id = idCounter;
    idCounter += 1;
    return 'gen-' + id;
  }

  function create(tag, attrs) {
    var elem = document.createElement(tag);
    if (attrs !== null) {
      for (key in attrs) {
        elem.setAttribute(key, attrs[key]);
      }
    }
    return elem;
  }

  function all(thing, root) {
    if (thing instanceof Array) {
      return thing;
    } else if (thing instanceof HTMLElement) {
      return [thing];
    } else if (!thing || thing.length == 0) {
      console.error('dom.one: invalid input');
      return null;
    } else {
      if (root) {
        root = one(root);
      } else {
        root = document;
      }
      var sigil = thing[0];
      var key = thing.substr(1);
      if (sigil == '#') {
        return [root.getElementById(key)];
      } else if (sigil == '.') {
        return root.getElementsByClassName(key);
      } else {
        return root.getElementsByTagName(key);
      }
    }
  }

  function one(thing, root) {
    return all(thing, root)[0];
  }

  function clone(elem) {
    return elem.cloneNode(true);
  }

  function toggle(elem, display) {
    if (!elem instanceof HTMLElement) {
      elem = one(elem);
    }
    var prevDisplay = elem.style.display;
    if (prevDisplay == 'none') {
      elem.style.display = display;
    } else {
      elem.style.display = 'none';
    }
  }

  function templates(par) {
    return function(tmpl, subst) {
      var elem = clone(one(tmpl, par));
      for (var key in subst) {
        var elems = all(key, elem);
        for (var i in elems) {
          elems[i].innerText = subst[key];
        }
      }
      return elem;
    };
  }

  function clear(elem) {
    if (!elem instanceof HTMLElement) {
      elem = one(elem);
    }
    elem.innerHTML = '';
  }

  function getClasses(elem) {
    return elem.hasAttribute('class') ? elem.getAttribute('class').split(' ') : [];
  }

  function addClass(elem, cls) {
    var classes = getClasses(elem);
    if (classes.indexOf(cls) == -1) {
      classes.push(cls);
      elem.setAttribute('class', classes.join(' '));
    }
  }

  return {
    create: create,
    all: all,
    one: one,
    clone: clone,
    toggle: toggle,
    templates: templates,
    clear: clear,
    genId: genId,
    getClasses: getClasses,
    addClass: addClass
  };

});

