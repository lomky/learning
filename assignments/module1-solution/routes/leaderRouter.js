const express = require('express');
const bodyParser = require('body-parser');

const leaderRouter = express.Router();

leaderRouter.use(bodyParser.json());

leaderRouter.route('/')
.all((req,res,next) => {
    res.statusCode = 200;
    res.setHeader('Content-Type','text/plain');
    next(); //keep going to the next matching route

})
.get((req,res,next) => {
  res.end('Will send all the leaders to you!');
  //does not continue on, as the .end was invoked
})
.post((req,res,next) => {
  res.end('Will add the leader: ' + req.body.name +
      ' with details: ' + req.body.description);
})
.put((req,res,next) => {
  res.statusCode = 403;
  res.end('PUT op not supported on /leaders');
})
.delete((req,res,next) => {
  res.end('Deleting all the leaders!');
});

leaderRouter.route('/:leaderId')
.get((req,res,next) => {
  res.end('Will send details the leader' +
      req.params.leaderId + ' to you!');
})
.post((req,res,next) => {
  res.statusCode = 403;
  res.end('POST op not supported on /leaders/' + req.params.leaderId);
})
.put((req,res,next) => {
  res.write('Updating the leader: ' + req.params.leaderId + '\n');
  res.end('Will update the leader: ' + req.body.name +
      ' with details: ' + req.body.description);
})
.delete((req,res,next) => {
  res.end('Deleting the leader' + req.params.leaderId );
});

module.exports = leaderRouter;
