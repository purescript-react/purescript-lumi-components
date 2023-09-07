import * as jssGlobal from 'jss';
export {default as preset} from 'jss-preset-default';

export const createInstance_ = function (p) {
  return jssGlobal.create(p());
};

export const createStyleSheet_ = function (jss, styles) {
  return jss.createStyleSheet(styles);
};

export const globalAttachStyleSheet_ = function (stylesheet) {
  return stylesheet.attach();
};

export const toStringStyleSheet = function (stylesheet) {
  return stylesheet.toString();
};
