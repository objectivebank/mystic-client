const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const { CleanWebpackPlugin } = require('clean-webpack-plugin');

module.exports = (env, argv) => {
  return {
    entry: './src/index.js',
    output: {
      filename: 'app-[chunkhash].js',
      path: path.resolve(__dirname, 'dist'),
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
    devServer: {
      contentBase: './dist',
    },
  };
};