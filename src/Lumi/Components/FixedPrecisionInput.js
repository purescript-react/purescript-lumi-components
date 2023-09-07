import React from 'react';

function Input(props) {
  var this_ = this;
  this_.bindRef = function (input) {
    this_.inputRef = input;
  };
  this_.onChange = function (e) {
    var target = e.target;
    var valueStr = target.value;
    var validationError = props.validate(valueStr);
    if (validationError) {
      target.setCustomValidity(validationError);
      return;
    }
    var value = parseFloat(valueStr);
    if (!isNaN(value)) {
      if (value > props.max) {
        target.setCustomValidity(
          "Value must be less than or equal to " + props.max + "."
        );
      } else if (value < props.min) {
        target.setCustomValidity(
          "Value must be greater than or equal to " + props.min + "."
        );
      } else {
        target.setCustomValidity("");
      }
    }
  };
  return this_;
}

Input.prototype = Object.create(React.Component.prototype);

Input.displayName = "FixedPrecisionInputInner";

Input.prototype.componentWillReceiveProps = function (newProps) {
  if (this.inputRef != null && this.inputRef.value !== newProps.defaultValue) {
    this.inputRef.value = newProps.defaultValue;
  }
};

Input.prototype.render = function () {
  return React.createElement(
    "input",
    Object.assign({}, this.props, {
      ref: this.bindRef,
      onChange: this.onChange,
      validate: undefined
    })
  );
};

export const input_ = Input;

export const cancelWhenNotMatch = function (regex, e) {
  if (e.cancelable && !e.defaultPrevented && !regex.test(e.key)) {
    e.preventDefault();
  }
};
