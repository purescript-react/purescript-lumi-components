

const selectNodeContents = node => {
  const range = document.createRange();
  range.selectNodeContents(node);
  var sel = window.getSelection();
  sel.removeAllRanges();
  sel.addRange(range);
  return sel;
};

export const copyNodeContents = (success, failure, node) => {
  try {
    const sel = selectNodeContents(node);
    if (window.navigator.clipboard != null) {
      window.navigator.clipboard
        .writeText(sel.toString().trim())
        .then(() => {
          sel.removeAllRanges();
        })
        .then(success, failure);
      return;
    } else {
      const copyResult = window.document.execCommand("copy");
      if (copyResult) {
        sel.removeAllRanges();
        return success();
      } else {
        return failure(new Error("Failed to copy"));
      }
    }
  } catch (e) {
    failure(e);
  }
};
