import React from 'react';

var LumiInputElement = function (_props) {
  this.inputRef = React.createRef();
  return this;
};

LumiInputElement.prototype = Object.create(React.Component.prototype);

LumiInputElement.displayName = "LumiInputElement";

LumiInputElement.prototype.setIndeterminate = function () {
  if (this.inputRef.current.indeterminate !== this.props.indeterminate) {
    this.inputRef.current.indeterminate = this.props.indeterminate;
  }
};

LumiInputElement.prototype.componentDidMount = function () {
  this.setIndeterminate();
};

LumiInputElement.prototype.componentDidUpdate = function () {
  this.setIndeterminate();
};

LumiInputElement.prototype.render = function () {
  var this_ = this;
  var props = {};
  Object.keys(this.props).forEach(function (key) {
    if (key === "indeterminate") return;
    props[key] = this_.props[key];
  });
  props.ref = this.inputRef;
  return React.createElement("input", props);
};

export const lumiInputElement = LumiInputElement;
