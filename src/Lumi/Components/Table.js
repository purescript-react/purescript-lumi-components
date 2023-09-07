

// This is here until we can get rid of Immutable
export const dataSize = function (data) {
  return data.size ? data.size : data.length;
};

export const getMouseEventPositionWithOffset = function (domNode, e) {
  var bounds = domNode.getBoundingClientRect();
  return {
    x: e.pageX - window.scrollX - bounds.left,
    y: e.pageY - window.scrollY - bounds.top
  };
};

export const checkIsEventTargetInTree = function (domNode, e) {
  return e.target === domNode || domNode.contains(e.target);
};

export const isRightClick = function (e) {
  return e.button === 2;
};

export const hasWindowSelection = function () {
  return window.getSelection().type === "Range";
};
