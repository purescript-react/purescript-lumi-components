import React from "react";
import ReactDOM from "react-dom";
import { HashRouter } from "react-router-dom";
import ScrollManager from "./ScrollManager";
import App from "./App";

ReactDOM.render(
  <HashRouter>
    <ScrollManager>
      <App />
    </ScrollManager>
  </HashRouter>,
  document.getElementById("root")
);
