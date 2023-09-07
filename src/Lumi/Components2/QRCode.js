export {default as qrcode_} from "qrcode.react";

export const generateSVGUrl = ref => () => {
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
  const dispose = () => URL.revokeObjectURL(url);
  return { url, dispose };
};
