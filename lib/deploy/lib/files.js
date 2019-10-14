const fs = require('fs');
const path = require('path');
const glob = require("glob");
const chalk = require('chalk');

module.exports = {
  getCurrentDirectoryBase : () => {
    return path.basename(process.cwd());
  },

  directoryExists : (filePath) => {
    try {
      return fs.statSync(filePath).isDirectory();
    } catch (err) {
      return false;
    }
  },

  jsFile : () => {
    return (glob.sync("dist/app-*.js", {}) || [])[0];
  },

  cssFile : () => {
    return (glob.sync("dist/app-*.css", {}) || [])[0];
  }
};
