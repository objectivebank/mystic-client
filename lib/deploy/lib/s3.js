var AWS = require('aws-sdk');

module.exports = {
  upload: (bucketName, keyName, body, options) => {
    var objectParams = {Bucket: bucketName, Key: keyName, Body: body, ...options};
    return new AWS.S3({apiVersion: '2006-03-01'}).putObject(objectParams).promise();
  },
}