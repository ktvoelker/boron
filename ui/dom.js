
define('ui/dom', [], function() {

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

  return {
    all: all,
    one: one,
    clone: clone,
    toggle: toggle,
    templates: templates
  };

});

