const path = require("path");
const Webpack = require("webpack");
const { CleanWebpackPlugin } = require("clean-webpack-plugin");
const TerserPlugin = require("terser-webpack-plugin");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const ResourceHintWebpackPlugin = require("resource-hints-webpack-plugin");
const ScriptExtHtmlWebpackPlugin = require("script-ext-html-webpack-plugin");
const SubresourceIntegrityWebpackPlugin = require("webpack-subresource-integrity");
const ManifestPlugin = require("webpack-manifest-plugin");
const { BundleAnalyzerPlugin } = require("webpack-bundle-analyzer");
const GitRevisionPlugin = require("git-revision-webpack-plugin");
const gitRevisionPlugin = new GitRevisionPlugin();

const outputPath = "build";

const isProd = ({ mode }) => mode === "production";

module.exports = (_env, options) => ({
  entry: {
    main: "./docs/index.jsx"
  },
  output: {
    filename: "[name].[chunkhash].js",
    chunkFilename: "[name].[chunkhash].js",
    path: path.resolve(__dirname, outputPath),
    crossOriginLoading: "anonymous"
  },
  optimization: {
    minimizer: [
      new TerserPlugin({
        cache: true,
        parallel: true,
        sourceMap: false,
        exclude: [/\.min\.js$/gi],
        terserOptions: {
          mangle: true,
          ie8: false,
          safari10: false
        }
      })
    ],
    runtimeChunk: "single",
    splitChunks: {
      chunks: "all",
      maxInitialRequests: Infinity,
      minSize: 0,
      cacheGroups: {
        npm: {
          test: /[\\/]node_modules[\\/]/
          // name(module) {
          //   // get the name, e.g. node_modules/packageName/not/this/part.js
          //   const packageName = module.context.match(
          //     /[\\/]node_modules[\\/](.*?)([\\/]|$)/
          //   )[1];

          //   // npm package names are URL-safe, but some servers don't like @ symbols
          //   return `npm.${packageName.replace("@", "")}`;
          // }
        }
      }
    }
  },
  plugins: [
    gitRevisionPlugin,
    new Webpack.DefinePlugin({
      VERSION: JSON.stringify(gitRevisionPlugin.version()),
      COMMITHASH: JSON.stringify(gitRevisionPlugin.commithash()),
      BRANCH: JSON.stringify(gitRevisionPlugin.branch())
    }),
    new Webpack.HashedModuleIdsPlugin(),
    new CleanWebpackPlugin(),
    new HtmlWebpackPlugin({
      template: "docs/index.html"
    }),
    new ResourceHintWebpackPlugin(),
    new ScriptExtHtmlWebpackPlugin({
      defaultAttribute: "async"
    }),
    new SubresourceIntegrityWebpackPlugin({
      hashFuncNames: ["sha256", "sha384"],
      // this is here as an example, comes at a perf cost so
      // we probably only want to use it for 3rd party scripts
      enabled: false // isProd(options)
    }),
    new ManifestPlugin(),
    new BundleAnalyzerPlugin({
      analyzerMode: isProd(options) ? "static" : "disabled",
      openAnalyzer: false
    })
  ],
  module: {
    rules: [
      {
        test: /\.js$/,
        loader: "source-map-loader",
        exclude: /node_modules|bower_components/
      },
      {
        oneOf: [
          {
            test: /\.jsx?$/,
            exclude: /node_modules|bower_components/,
            use: {
              loader: "babel-loader",
              options: {
                babelrc: false,
                presets: [
                  [
                    "@babel/preset-env",
                    {
                      targets: {
                        browsers: ["cover 90% in US", "ie 11"]
                      }
                    }
                  ],
                  "@babel/preset-react"
                ],
                plugins: [
                  require("@babel/plugin-proposal-object-rest-spread"),
                  require("@babel/plugin-proposal-class-properties"),
                  require("@babel/plugin-syntax-dynamic-import")
                ]
              }
            }
          }
        ]
      }
    ]
  },
  resolve: {
    extensions: [".js", ".jsx"],
    alias: {
      purs: path.join(__dirname, "output")
    }
  },
  devServer: {
    stats: "minimal"
  },
  watchOptions: {
    aggregateTimeout: 2000
  }
});
