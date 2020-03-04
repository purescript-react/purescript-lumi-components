"use strict";

exports.qrcode_ = require("qrcode.react");

exports.saveOnClick = ref => filename => () => {
  const containerNode = ref.current;
  if (containerNode == null) {
    throw new Error("Cannot save the contents of an empty ref");
  }
  const svgNode = containerNode.querySelector("svg");
  if (svgNode == null) {
    throw new Error("Inner SVG node not found");
  }

  const data = new XMLSerializer().serializeToString(svgNode);
  const svg = new Blob([data], { type: "image/svg+xml;charset=utf-8" });
  const url = URL.createObjectURL(svg);

  const a = document.createElement("a");
  document.body.appendChild(a);
  a.style = "display: none";
  a.href = url;
  a.download = filename;
  a.click();
  window.URL.revokeObjectURL(url);
  document.body.removeChild(a);
};
