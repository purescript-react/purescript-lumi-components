"use strict";

// This is here until we can get rid of Immutable
exports.dataSize = function (data) {
  return data.size ? data.size : data.length;
};

exports.getMouseEventPositionWithOffset = function (domNode, e) {
  var bounds = domNode.getBoundingClientRect();
  return {
    x: e.pageX - bounds.left,
    y: e.pageY - bounds.top
  };
};

exports.checkIsEventTargetInTree = function (domNode, e) {
  return e.target === domNode || domNode.contains(e.target);
};

exports.isRightClick = function (e) {
  return e.button === 2;
};

exports.hasWindowSelection = function () {
  return window.getSelection().type === "Range";
};
