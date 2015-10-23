var child_process = require('child_process');
var AWS = require('aws-sdk');
var fs = require('fs');
var s3 = new AWS.S3();

exports.handler = function(event, context) {

  s3.getObject({Bucket: "better-bot", Key: "config/config.yaml"}, function(err, data) {
        if (err) {
            console.log("Error reading the config from S3");
            context.fail ("Error getting file: " + err)
        } else {

          console.log(fs.readdirSync("/usr/lib"));
          console.log(fs.readdirSync("/usr/lib64"));

          fs.symlinkSync("/usr/lib64/libgmp.so.3", "/usr/lib64/libgmp.so.10")

          console.log(fs.readdirSync("/usr/lib64"));

          var proc = child_process.spawn('./better-bot-exe', [ JSON.stringify(event) ], { stdio: 'inherit' });

          proc.on('close', function(code) {
            if(code !== 0) {
              return context.done(new Error("Process exited with non-zero status code"));
            }

            context.done(null);
          });
        }
    });

  //
}
