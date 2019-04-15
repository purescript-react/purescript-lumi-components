import React, { Component, useState, useEffect } from "react";
import { Link, NavLink, Redirect, Route, Switch } from "react-router-dom";
import Media from "react-media";
import Loadable from "react-loadable";
import changeCase from "change-case";

import { colors, colorNames } from "../src/Lumi/Components/Color";
import { column, column_ } from "../src/Lumi/Components/Column";
import { row, row_ } from "../src/Lumi/Components/Row";
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
} from "../src/Lumi/Components/Text";
import { icon, iconRearrange } from "../src/Lumi/Components/Icon";
import { loader } from "../src/Lumi/Components/Loader";
import { exampleStyleToggle } from "./Example";
import { dragDropContext } from "./App.purs_bundle_hacks";
import { cssStringHSLA } from "./ColorHelper";
import { attachGlobalComponentStyles } from "../src/Lumi/Components/Styles";

attachGlobalComponentStyles();

const repoSourceBasePath = `https://github.com/lumihq/purescript-lumi-components/blob/${COMMITHASH}`;

const fromComponentPath = title => ({
  docs: Loadable({
    loader: () =>
      // Note: The string bits inside `require(...)` need to stay static strings, or Webpack
      // won't be able to infer which files need to be included in the bundle.
      import(`./Examples/${title}.example`).then(module_ => () => module_.docs),
    loading: () => null // these load quickly enough that a noisy loader makes it look slower
  }),
  title,
  path: `/${changeCase.snake(title)}`,
  componentSource: `${repoSourceBasePath}/src/Lumi/Components/${title}.purs`,
  exampleSource: `${repoSourceBasePath}/docs/Examples/${title}.example.purs`
});

const componentLoaders = [
  "Breadcrumb",
  "Button",
  "ButtonGroup",
  "Card",
  "CardGrid",
  "Color",
  "Column",
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

const App = () => {
  const [menuOpen, setMenuOpen] = useState(false);
  const toggleMenu = () => setMenuOpen(menuOpen => !menuOpen);
  const closeMenu = () => setMenuOpen(false);

  const [components, setComponents] = useState(null);
  useEffect(() => {
    Promise.all(componentLoaders).then(setComponents);
  }, []);

  return (
    <ErrorBoundary>
      {dragDropContext(
        <Media query="(max-width: 799px)">
          {isMobile => (
            <Root>
              <Header isMobile={isMobile} toggleMenu={toggleMenu} />
              {components === null ? (
                loader({})
              ) : (
                <Content closeMenu={closeMenu}>
                  <SideNav isMobile={isMobile} menuOpen={menuOpen}>
                    <NavSubtitle>Components</NavSubtitle>
                    {components.map(renderNavLink)}
                  </SideNav>

                  <ExampleArea>
                    <ErrorBoundary>
                      <Switch>
                        <Route exact path="/" render={() => h1_("Welcome")} />
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
          style: { alignItems: "center", marginBottom: "24px" },
          children: [
            h1_(component.title),
            <a
              className="lumi"
              target="_blank"
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
              href={component.exampleSource}
              style={{
                marginLeft: "8px"
              }}
            >
              Example Source
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
    style={{ color: cssStringHSLA(colors.black1), textDecoration: "none" }}
  >
    {mainHeader_(children)}
  </Link>
);

const Version = ({ children }) =>
  text({
    ...subsectionHeader,
    color: colorNames.black1,
    children: children,
    style: { marginLeft: 10 }
  });

const MenuLink = ({ toggleMenu }) =>
  mainHeader_(
    <div
      onClick={toggleMenu}
      style={{
        color: cssStringHSLA(colors.black1),
        marginRight: "1rem",
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
    padding: "1em",
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
    children: children,
    style: {
      fontSize: "1.6em",
      marginBottom: "1em",
      marginTop: "1em"
    }
  });

const ExampleArea = ({ children }) =>
  column({
    children: children,
    style: {
      width: "100%",
      padding: "2em"
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
        style: { padding: "1rem" }
      });
    }
    return this.props.children;
  }
}
