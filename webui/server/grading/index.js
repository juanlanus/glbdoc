/*
 * evaluations home page
 */

exports.index = function(req, res){
  console.log( 'building grade module' );
  res.render('index', { title: 'Express' });
};
