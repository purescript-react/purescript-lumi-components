"use strict";

const selectElementContents = el => {
  const range = document.createRange();
  range.selectNodeContents(el);
  var sel = window.getSelection();
  sel.removeAllRanges();
  sel.addRange(range);
  return sel.toString();
};

exports.copyElementContents = el => () => {
  const content = selectElementContents(el);
  if (window.navigator.clipboard != null) {
    window.navigator.clipboard
      .writeText(content)
      .then(() => {
        console.log("copied", content);
      })
      .catch(e => console.error("copy failed", e));
  } else {
    window.document.execCommand("copy");
  }
};
