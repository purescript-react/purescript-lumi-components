"use strict";

// Walks up the DOM tree starting at the given node to find
// the first parent with a scroll bar (including auto, which
// on some devices hides the scroll bar until hovered).
exports.getScrollParent = node => () => {
  const isElement = node instanceof HTMLElement;
  const computedStyle = isElement && window.getComputedStyle(node);
  const overflowY = computedStyle && computedStyle.overflowY;
  const overflowX = computedStyle && computedStyle.overflowX;
  const isScrollable =
    (overflowY &&
      !(overflowY.includes("visible") || overflowY.includes("hidden"))) ||
    (overflowX &&
      !(overflowX.includes("visible") || overflowX.includes("hidden")));

  if (!node) {
    return document.body;
  } else if (isScrollable && node.scrollHeight >= node.clientHeight) {
    return node;
  } else {
    return exports.getScrollParent(node.parentNode);
  }
};

exports.addPassiveEventListener = type => listener => capture => target => () =>
  target.addEventListener(type, listener, { passive: true, capture });

exports.removePassiveEventListener = type => listener => capture => target => () =>
  target.removeEventListener(type, listener, { passive: true, capture });
