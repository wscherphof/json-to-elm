import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

var app = Elm.Main.init({
  node: document.getElementById('root')
});

registerServiceWorker();

(function port_value() {
  app.ports.toValue.subscribe(function(msg) {
    var stuff;
    try {
        stuff = JSON.parse(msg.text);
    } catch (e) {
        stuff = {};
    }

    var fields = generateFields(stuff, msg.aliasName);

    app.ports.updateValue.send({
      stuff: stuff,
      fields: fields
    });
  });

  function capitalize (string) {
    return string.replace(/(.)(.*)/,
      function(match, first, rest, offset, string) {
        return first.toUpperCase() + rest;
      }
    );
  }

  function generateFields (obj, base) {
    var list = [{
      name: base,
      base: "",
      typeName: "Something",
      value: obj
    }];
    function generateFields_ (obj, base) {
      Object.keys(obj).forEach(function(key) {
          var value = obj[key];
          var name = makeGuessAtType(value);
          var newBase = base + key;
          var field = {
              base: base.trim(),
              name: key.trim(),
              typeName: name,
              value: value
          };
          if (name === "Something") {
            generateFields_(value, base + capitalize(key));
          }
          list.push(field);
        });
    }
    generateFields_(obj, base); 
    return list;
  }

  function isInt (n) {
    return n % 1 === 0;
  }

  function makeGuessAtType (item) {
    if (item === null) {
      return 'Maybe _Unknown';
    }

    var type = typeof(item);

    if (type === 'boolean'){
      return 'Bool';
    }

    if (type === 'string'){
      return 'String';
    }

    if (type === 'number'){
      if (isInt(item)){
        return 'Int';
      }
      return 'Float';
    }

    if (Array.isArray(item)){
      if (item.length === 0){
        return 'List a';
      }
      return 'List ' + makeGuessAtType(item[0]);
    }

    if (type === 'object'){
      return 'Something';
    }

    return 'Unknown';
  }
})();
