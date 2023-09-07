

export const urlParts = function (href) {
  var base = href;
  var path = "";
  var query = "";
  var hash = { path: "", query: "" };
  try {
    var a = document.createElement("a");
    a.href = href;
    base = a.protocol + "//" + a.host;
    path = a.pathname;
    query = a.search;
    var hashParts = a.hash.split("?");
    hash.path = hashParts[0] || "";
    hash.query = hashParts[1] != null ? "?" + hashParts[1] : "";
  } catch (err) {
    console.warn(err);
  }
  return {
    base: base,
    path: path,
    query: query,
    hash: hash
  };
};
