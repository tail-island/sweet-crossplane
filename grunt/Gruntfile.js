module.exports = function (grunt) {
  grunt.initConfig({
    bower: {
      install: {
        options: {
          targetDir: "../resources/public/lib",
          cleanup: true,
          layout: "byComponent",
          verbose: true
        }
      }
    }
  });
  
  grunt.loadNpmTasks('grunt-bower-task');

  grunt.registerTask('default', ['bower:install']);
};
