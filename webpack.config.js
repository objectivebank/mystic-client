const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const { CleanWebpackPlugin } = require('clean-webpack-plugin');
const TerserPlugin = require('terser-webpack-plugin');

module.exports = (env, argv) => {
  return {
    entry: './src/index.js',
    output: {
      filename: 'app-[chunkhash].js',
      path: path.resolve(__dirname, 'dist'),
    },
    devServer: {
      contentBase: './dist',
    },
    plugins: [
      new CleanWebpackPlugin(),
      new HtmlWebpackPlugin({
        title: 'Objective Bank',
        template: 'src/index.html',
        inject: false,
      }),
    ],
    module: {
      rules: [{
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: {
          loader: 'elm-webpack-loader',
           options: {
             cwd: path.resolve(__dirname),
             maxInstances: 1,
             optimize: argv.mode === 'production',
           }
         }
      }]
    },
    optimization: {
      minimize: argv.mode === 'production',
      minimizer: [
        new TerserPlugin({
          terserOptions: {
            compress: {
              pure_funcs: ["F2","F3","F4","F5","F6","F7","F8","F9","A2","A3","A4","A5","A6","A7","A8","A9"],
              pure_getters: true,
              keep_fargs: false,
              unsafe_comps: true,
              unsafe: true
            },
          }
        }),
        new TerserPlugin({
          terserOptions: {
            mangle: true
          }
        }),
      ],
    },
  };
};