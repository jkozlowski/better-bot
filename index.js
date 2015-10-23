var child_process = require('child_process');
var AWS = require('aws-sdk');
var s3 = new AWS.S3();

exports.handler = function(event, context) {

  s3.getObject({Bucket: "better-bot", Key: "config/config.yaml"}, function(err, data) {
        if (err) {
            console.log("Error reading the config from S3");
            context.fail ("Error getting file: " + err)
        } else {

          var proc = child_process.spawn('./dist/better-bot', [ JSON.stringify(event) ], { stdio: 'inherit' });

          proc.on('close', function(code) {
            if(code !== 0) {
              return context.done(new Error("Process exited with non-zero status code"));
            }

            context.done(null);
          });

          // console.log('Data :', );
          // context.succeed();
        }
    });

  //
}
