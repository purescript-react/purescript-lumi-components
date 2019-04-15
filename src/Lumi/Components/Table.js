"use strict";

var React = require("react");

// This is here until we can get rid of Immutable
exports.dataSize = function(data) {
  return data.size ? data.size : data.length;
};

exports.getMouseEventPositionWithOffset = function(domNode, e) {
  var bounds = domNode.getBoundingClientRect();
  return {
    x: e.pageX + domNode.scrollLeft - bounds.left,
    y: e.pageY + domNode.scrollTop - bounds.top
  };
};

exports.checkIsEventTargetInTree = function(domNode, e) {
  return e.target === domNode || domNode.contains(e.target);
};

exports.hasWindowSelection = function() {
  return window.getSelection().type === "Range";
};

exports.scrollObserver = function ScrollObserver(props) {
  var yArr = React.useState(false);
  var hasScrolledY = yArr[0];
  var setHasScrolledY = yArr[1];
  var xArr = React.useState(false);
  var hasScrolledX = xArr[0];
  var setHasScrolledX = xArr[1];
  React.useEffect(
    function() {
      var scrollParent = getScrollParent(props.node);
      if (scrollParent == null) return;
      function onScroll() {
        setHasScrolledY(scrollParent.scrollTop > 0);
        setHasScrolledX(scrollParent.scrollLeft > 0);
      }
      onScroll();
      scrollParent.addEventListener("scroll", onScroll, { passive: true });
      return function() {
        scrollParent.removeEventListener("scroll", onScroll, { passive: true });
      };
    },
    [props.root]
  );
  return props.render({
    hasScrolledY: hasScrolledY,
    hasScrolledX: hasScrolledX
  });
};

// Walks up the DOM tree starting at the given node to find
// the first parent with a scroll bar (including auto, which
// on some devices hides the scroll bar until hovered).
function getScrollParent(node) {
  var isElement = node instanceof HTMLElement;
  var computedStyle = isElement && window.getComputedStyle(node);
  var overflowY = computedStyle && computedStyle.overflowY;
  var overflowX = computedStyle && computedStyle.overflowX;
  var isScrollable =
    (overflowY &&
      !(overflowY.includes("visible") || overflowY.includes("hidden"))) ||
    (overflowX &&
      !(overflowX.includes("visible") || overflowX.includes("hidden")));

  if (!node) {
    return null;
  } else if (isScrollable && node.scrollHeight >= node.clientHeight) {
    return node;
  }

  return getScrollParent(node.parentNode) || document.body;
}
