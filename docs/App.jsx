/* eslint-disable react/jsx-key */
/* global COMMITHASH, VERSION */

import React, { Component, useState, useEffect } from "react";
import { Link, NavLink, Redirect, Route, Switch } from "react-router-dom";
import Media from "react-media";
import Loadable from "react-loadable";
import changeCase from "change-case";

import { colors, colorNames } from "purs/Lumi.Components.Color";
import { column, column_ } from "purs/Lumi.Components.Column";
import { row, row_ } from "purs/Lumi.Components.Row";
import {
  text,
  body_,
  sectionHeader,
  subsectionHeader,
  subsectionHeader_,
  mainHeader_,
  subtext_,
  h1_,
  h3_
} from "purs/Lumi.Components.Text";
import { icon, iconRearrange } from "purs/Lumi.Components.Icon";
import { loader } from "purs/Lumi.Components.Loader";
import { exampleStyleToggle } from "purs/Lumi.Components.Example";
import { dragDropContext } from "purs/AppSetup";
import { cssStringHSLA } from "purs/Color";
import { attachGlobalComponentStyles } from "purs/Lumi.Components.Styles";

attachGlobalComponentStyles();

const repoSourceBasePath = `https://github.com/lumihq/purescript-lumi-components/blob/${COMMITHASH}`;
const pursuitBasePath = `https://pursuit.purescript.org/packages/purescript-lumi-components/${VERSION.replace(
  "v",
  ""
)}`;

const fromComponentPath = title => ({
  docs: Loadable({
    loader: () =>
      // Note: The string bits inside `require(...)` need to stay static strings, or Webpack
      // won't be able to infer which files need to be included in the bundle.
      import(`purs/Lumi.Components.Examples.${title}`).then(module_ => () =>
        module_.docs
      ),
    loading: () => null // these load quickly enough that a noisy loader makes it look slower
  }),
  title,
  path: `/${changeCase.hyphen(title)}`,
  componentSource: `${repoSourceBasePath}/src/Lumi/Components/${title}.purs`,
  exampleSource: `${repoSourceBasePath}/docs/Examples/${title}.example.purs`,
  apiReference: `${pursuitBasePath}/docs/Lumi.Components.${title}`
});

const fromComponentPathv2 = title => ({
  docs: Loadable({
    loader: () =>
      // Note: The string bits inside `require(...)` need to stay static strings, or Webpack
      // won't be able to infer which files need to be included in the bundle.
      import(`purs/Lumi.Components2.Examples.${title}`).then(module_ => () =>
        module_.docs
      ),
    loading: () => null // these load quickly enough that a noisy loader makes it look slower
  }),
  title,
  path: `/v2/${changeCase.hyphen(title)}`,
  componentSource: `${repoSourceBasePath}/src/Lumi/Components2/${title}.purs`,
  exampleSource: `${repoSourceBasePath}/docs/Examples2/${title}.example.purs`,
  apiReference: `${pursuitBasePath}/docs/Lumi.Components2.${title}`
});

const componentLoaders = [
  "Badge",
  "Border",
  "Breadcrumb",
  "Button",
  "ButtonGroup",
  "Card",
  "CardGrid",
  "Color",
  "Column",
  "Details",
  "Divider",
  "DropdownButton",
  "EditableTable",
  "FixedPrecisionInput",
  "Form",
  "Icon",
  "Images",
  "Input",
  "InputGroup",
  "LabeledField",
  "Layouts",
  "Link",
  "List",
  "Loader",
  "Lockup",
  "Modal",
  "NativeSelect",
  "Navigation",
  "Pagination",
  "Pill",
  "Progress",
  "Responsive",
  "Row",
  "Select",
  "Slider",
  "Spacing",
  "StatusSlat",
  "Svg",
  "Tab",
  "Table",
  "Text",
  "Textarea",
  "Toast",
  "Tooltip",
  "Upload",
  "Wizard"
].map(fromComponentPath);

const componentv2Loaders = [
  "Box",
  "Button",
  "ButtonGroup",
  "Clip",
  "Link",
  "QRCode",
  "Slat",
  "Text"
].map(fromComponentPathv2);

const App = () => {
  const [menuOpen, setMenuOpen] = useState(false);
  const toggleMenu = () => setMenuOpen(menuOpen => !menuOpen);
  const closeMenu = () => setMenuOpen(false);

  const [components, setComponents] = useState(null);
  const [componentsv2, setComponentsv2] = useState(null);
  useEffect(() => {
    Promise.all(componentLoaders).then(setComponents);
    Promise.all(componentv2Loaders).then(setComponentsv2);
  }, []);

  return (
    <ErrorBoundary>
      {dragDropContext(
        <Media query="(max-width: 799px)">
          {isMobile => (
            <Root>
              <Header isMobile={isMobile} toggleMenu={toggleMenu} />
              {(components && componentsv2) == null ? (
                loader({})
              ) : (
                <Content closeMenu={closeMenu}>
                  <SideNav isMobile={isMobile} menuOpen={menuOpen}>
                    <NavSubtitle>
                      Components v2
                      <div
                        style={{
                          fontSize: 12,
                          color: colors.black5
                        }}
                      >
                        (alpha)
                      </div>
                    </NavSubtitle>
                    {componentsv2.map(renderNavLink)}
                    <NavSubtitle>Components</NavSubtitle>
                    {components.map(renderNavLink)}
                  </SideNav>

                  <ExampleArea>
                    <ErrorBoundary>
                      <Switch>
                        <Route exact path="/" render={() => h1_("Welcome")} />
                        {componentsv2.map(renderRoute)}
                        {components.map(renderRoute)}
                        <Redirect to="/" />
                      </Switch>
                    </ErrorBoundary>
                  </ExampleArea>
                </Content>
              )}
            </Root>
          )}
        </Media>
      )}
    </ErrorBoundary>
  );
};

export default App;

const renderNavLink = component => (
  <NavLink
    className="lumi"
    key={component.path}
    exact
    to={component.path}
    style={{ color: cssStringHSLA(colors.black1) }}
    activeStyle={{ color: cssStringHSLA(colors.black) }}
  >
    {body_(component.title)}
  </NavLink>
);

const renderRoute = component => (
  <Route
    key={component.path}
    path={component.path}
    render={() =>
      column_([
        row({
          style: {
            alignItems: "center",
            marginBottom: "24px",
            flexWrap: "wrap"
          },
          children: [
            h1_(component.title),
            <a
              className="lumi"
              target="_blank"
              rel="noopener noreferrer"
              href={component.componentSource}
              style={{
                marginLeft: "8px",
                marginRight: "8px"
              }}
            >
              Component Source
            </a>,
            "|",
            <a
              className="lumi"
              target="_blank"
              rel="noopener noreferrer"
              href={component.exampleSource}
              style={{
                marginLeft: "8px",
                marginRight: "8px"
              }}
            >
              Example Source
            </a>,
            "|",
            <a
              className="lumi"
              target="_blank"
              rel="noopener noreferrer"
              href={component.apiReference}
              style={{
                marginLeft: "8px"
              }}
            >
              API Reference
            </a>,
            <div
              style={{
                flex: "1",
                display: "flex",
                justifyContent: "flex-end",
                alignItems: "center"
              }}
            >
              <span style={{ margin: "0 22px" }}>
                {subsectionHeader_("Example style:")}
              </span>
              {exampleStyleToggle}
            </div>
          ]
        }),
        <component.docs />
      ])
    }
  />
);

const Root = ({ children }) =>
  column({
    children: children,
    style: { minHeight: "100vh", alignItems: "center" }
  });

const Title = ({ children }) => (
  <Link
    to="/"
    style={{
      color: cssStringHSLA(colors.black1),
      textDecoration: "none",
      whiteSpace: "nowrap"
    }}
  >
    {mainHeader_(children)}
  </Link>
);

const Version = ({ children }) =>
  text({
    ...subsectionHeader,
    color: colorNames.black1,
    children: children,
    style: { marginLeft: 10, whiteSpace: "nowrap" }
  });

const MenuLink = ({ toggleMenu }) =>
  mainHeader_(
    <div
      onClick={toggleMenu}
      style={{
        color: cssStringHSLA(colors.black1),
        marginRight: "10px",
        display: "flex",
        justifyContent: "center",
        cursor: "pointer",
        WebkitUserSelect: "none",
        MozUserSelect: "none",
        msUserSelect: "none",
        userSelect: "none"
      }}
    >
      {icon({ type_: iconRearrange })}
    </div>
  );

const Header = ({ isMobile, toggleMenu }) =>
  row({
    style: {
      position: "fixed",
      top: 0,
      zIndex: 1,
      height: 60,
      width: "100%",
      justifyContent: "center",
      backgroundColor: "#f6f5f2",
      boxSizing: "border-box",
      borderBottom: "1px solid rgb(229, 229, 229)"
    },
    children: [
      row({
        style: {
          maxWidth: 1200,
          width: "100%",
          alignItems: "center",
          paddingLeft: 10,
          paddingRight: 10
        },
        children: [
          isMobile && <MenuLink toggleMenu={toggleMenu} />,
          <Title>Lumi Components</Title>,
          <Version>{VERSION}</Version>
        ]
      })
    ]
  });

const Content = ({ children, closeMenu }) => (
  <div
    onClick={closeMenu}
    style={{
      flex: 1,
      width: "100%",
      maxWidth: 1200,
      margin: "auto",
      marginTop: 60
    }}
  >
    {row_(children)}
  </div>
);

const SideNav = ({ children, isMobile, menuOpen }) => {
  const nav = {
    minWidth: 200,
    backgroundColor: "white",
    whiteSpace: "nowrap"
  };
  const navMobile = {
    position: "fixed",
    top: 60,
    left: 0,
    zIndex: 1,
    height: "100%",
    transition: "transform 200ms ease-in-out",
    transform: "translateX(-100%)",
    boxSizing: "border-box",
    borderRightWidth: 1,
    borderRightColor: "rgba(0, 0, 0, 0.2)",
    borderRightStyle: "solid"
  };
  const navMobileInner = {
    padding: "10px",
    height: "100%",
    overflowY: "auto",
    WebkitOverflowScrolling: "touch"
  };
  const navMobileOpen = {
    transform: "translateX(0)"
  };
  return column({
    children: column({ children: children, style: navMobileInner }),
    style: {
      ...nav,
      ...(isMobile ? navMobile : null),
      ...(isMobile && menuOpen ? navMobileOpen : null)
    }
  });
};

const NavSubtitle = ({ children }) =>
  text({
    ...sectionHeader,
    children,
    style: {
      fontSize: "16px",
      marginBottom: "10px",
      marginTop: "20px"
    }
  });

const ExampleArea = ({ children }) =>
  column({
    children,
    style: {
      width: "100%",
      padding: "20px"
    }
  });

class ErrorBoundary extends Component {
  state = { error: null };

  componentWillReceiveProps() {
    this.setState({ error: null });
  }

  componentDidCatch(error, info) {
    console.error(error);
    this.setState({
      error: {
        message: error.message,
        stack: error.stack,
        componentStack: info.componentStack
      }
    });
  }

  render() {
    if (this.state.error !== null) {
      return column({
        children: [
          h1_("Render Error"),
          h3_(this.state.error.message),
          subtext_(
            <pre>
              {this.state.error.stack}
              {"\n"}
              {this.state.error.componentStack}
            </pre>
          )
        ],
        style: { padding: "10px" }
      });
    }
    return this.props.children;
  }
}
