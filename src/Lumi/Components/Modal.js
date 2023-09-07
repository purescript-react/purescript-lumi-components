

import React from 'react';
var h = React.createElement;
import ReactDOM from 'react-dom';
var modalRoot = document.getElementById("modal-root");

function assign(target, from) {
  for (var nextKey in from) {
    if (Object.prototype.hasOwnProperty.call(from, nextKey)) {
      target[nextKey] = from[nextKey];
    }
  }
  return target;
}

export const modalBuilder = function (ModalPortal) {
  var ModalComponent = function constructor(_props) {
    this.el = document.createElement("div");
    var this_ = this;
    this.requestClose = function (event) {
      if (this_.ownerHandlesClose()) {
        this_.props.onRequestClose(event);
      }
    };
    this.ownerHandlesClose = function () {
      return !!this_.props.onRequestClose;
    };
    this.portalRef = function (ref) {
      this_.portal = ref;
    };
    return this;
  };
  ModalComponent.prototype = Object.create(React.Component.prototype);
  ModalComponent.displayName = "Modal";

  ModalComponent.prototype.componentDidMount = function () {
    modalRoot.appendChild(this.el);
  };

  ModalComponent.prototype.componentWillUnmount = function () {
    modalRoot.removeChild(this.el);
  };

  ModalComponent.prototype.render = function () {
    return ReactDOM.createPortal(
      h(
        ModalPortal,
        assign(
          {
            ref: this.portalRef,
            requestClose: this.requestClose
          },
          this.props
        )
      ),
      this.el
    );
  };

  return ModalComponent;
};

export const toggleBodyClass = function (className, on) {
  if (on) {
    document.body.classList.add(className);
  } else {
    document.body.classList.remove(className);
  }
};
