var gulp = require('gulp');
var elm = require('gulp-elm');
var uglify = require('gulp-uglify');
var rename = require('gulp-rename');

gulp.task('elm-init', elm.init);

function swallowError(error) {
  console.log(error.toString());
  this.emit('end');
}

gulp.task('elm', ['elm-init'], function() {
  return gulp.src('./src/App.elm')
    .pipe(elm())
    .on('error', swallowError)
    .pipe(uglify())
    .pipe(rename('app-min.js'))
    .pipe(gulp.dest('./dist/'))
});

gulp.task('default', ['elm'], function() {
  gulp.watch('./*.elm', ['elm']);
});
