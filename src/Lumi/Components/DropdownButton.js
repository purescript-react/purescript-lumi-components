export const checkIsEventTargetInTree = function (domNode, e) {
  return e.target === domNode || domNode.contains(e.target);
};
