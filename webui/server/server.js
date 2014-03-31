
/**
 * Module dependencies.
 */

// http://evanhahn.com/understanding-express/

var express = require('express')
  , routes = require('./grading')
  , routes = require('./routes')
  , user = require('./routes/user')
  , http = require('http')
  , path = require('path');

var app = express();

// all environments
app.use( function( req, res, next ) { 
  console.log( req.method + ' ' + req.url ); 
  next();
} );
app.set( 'port', process.env.PORT || 3000 );
app.set( 'views', __dirname + '/views' );
app.set( 'view engine', 'ejs' );
app.use( express.favicon( path.join( __dirname, '../res/favicon.ico' )));
app.use( express.logger('dev') );
// DEPRECATED as of Connect 3.0: app.use( express.bodyParser() );
// NOT USED: app.use( express.urlencoded() )
app.use( express.json() )
app.use( express.methodOverride() );
app.use( express.cookieParser('your secret here') );
app.use( express.session() );

app.use( app.router );
app.use( '/res', express.static(path.join(__dirname, '../res')) );
app.use( '/data', express.static(path.join(__dirname, '../data')) );
app.use( 
  '/grade', 
  function( req, res ) { 
    console.log( 'grading requested' );
    console.log( req.body );
    res.json( '200', { grade: 100 } );
  }
  );
app.use( '/webui', express.static(path.join(__dirname, '..')) );
app.use( express.static(path.join(__dirname, 'public')) );

// development only
if ( 'development' == app.get('env') ) {
  app.use( express.errorHandler() );
}

app.get( '/', routes.index );
app.get( '/users', user.list );

http.createServer(app).listen(
  app.get('port'),
  function(){
    console.log( 'Express server listening on port ' + app.get('port') );
  }
);
