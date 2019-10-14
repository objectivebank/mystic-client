/*
 * Given a production build of the client app, upload it to an S3 bucket.
 */
const files = require('./lib/files');
const s3 = require('./lib/s3');
const chalk = require('chalk');
const zlib = require('zlib');
const fs = require('fs');

const buildCommand = 'yarn build';


if (!files.directoryExists('./dist')) {
  console.log(chalk.red('Cannot find dist directory. Please run from the repository root.'));
  process.exit();
} else {
  console.log('Found dist directory.');
}

const jsFile = files.jsFile();
if (!jsFile) {
  console.log(chalk.red(`Deployment-style app JS build not found in ./dist. Please run ${buildCommand} first.`));
  process.exit();
}

console.log('Found JS build: ' + jsFile);

const jsData = fs.readFileSync(jsFile);
const compressedJSData = zlib.gzipSync(jsData);

var jsKeyName = jsFile.split('/')[1];

var bucketName = 'objective-bank';

s3.upload(bucketName, jsKeyName, compressedJSData,
  { ACL: 'public-read', ContentEncoding: 'gzip', ContentType: 'application/javascript' }
).then(
  function(data) {
    console.log("Successfully uploaded data to " + bucketName + "/" + jsKeyName);
    printActivateCommand();
  }).catch(
    function(err) {
      console.error(err, err.stack);
      process.exit();
  });

function printActivateCommand() {
  const clientJSID = jsKeyName.replace('app-', '').replace('.js', '');

  console.log('\n');
  if (!process.env.HEROKU_APP_NAME) {
    console.log(chalk.yellow('HEROKU_APP_NAME unset, you will need to specify app name.'))
  }

  const appName = process.env.HEROKU_APP_NAME || '<app name>';
  console.log('Run the following command to activate this client build:');
  console.log('heroku config:set -a ' + appName + ' CLIENT_JS_ID=' + clientJSID);
}
