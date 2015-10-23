var child_process = require('child_process');
var AWS = require('aws-sdk');
var fs = require('fs');
var s3 = new AWS.S3();

exports.handler = function(event, context) {

  var configStream = s3.getObject({Bucket: "better-bot", Key: "config/config.yaml"})
                       .createReadStream();

  var options = {
      stdio: ['pipe', 1, 2]
    , env: { "LD_LIBRARY_PATH": "." }
  };

  var child = child_process.spawn('./better-bot-exe', [], options);
  configStream.pipe(child.stdin);

  child.on('close', function(code) {
    if(code !== 0) {
      return context.done(new Error("Process exited with non-zero status code"));
    }

    context.done(null);
  });
}
