
child_process = require('child_process')
http          = require('http')

compileServer = false
useCabalDev   = false
serverPort    = 8000
retryTimeout  = 300

serverProc    = null

module.exports = (grunt) ->
  grunt.initConfig
    coffee:
      compile:
        files:
          "public/js/modules/*.js": "src/coffee/**/*.coffee"

        options:
          flatten: false
          bare: false

    jade:
      compile:
        files:
          "public/index.html": "src/jade/index.jade"

    stylus:
      compile:
        files:
          "public/css/styles.css": "src/stylus/styles.styl"

        options:
          compress: true

    watch:
      coffee:
        files: ["src/coffee/**/*.coffee"]
        tasks: ["coffee", "reload"]

      stylus:
        files: ["src/stylus/*.styl"]
        tasks: ["stylus", "reload"]

      jade:
        files: ["src/jade/*.jade"]
        tasks: ["jade", "reload"]

      server:
        files: ["src/hs/**/*.hs"]
        tasks: ["server"]

    reload:
      port: 6001
      proxy:
        host: 'localhost'
        port: serverPort


  grunt.registerTask "server", () ->
    done = @async()

    waitForServer = (callback) ->
      req = http.get "http://localhost:#{serverPort}/", callback
      req.on 'error', () ->
        setTimeout (-> waitForServer callback), retryTimeout

    triggerReload = () ->
      grunt.task.run "reload"

    execServer = () ->
      if compileServer
        console.log "===== Compiling httpd"
        child_process.exec 'cabal-dev build', (err, stdout, stderr) ->
          console.log stdout if stdout?
          console.log stderr if stderr?
          console.log "===== Starting httpd"
          serverProc = child_process.spawn 'dist/build/hackfest-httpd/hackfest-httpd'
          done()
      else
        console.log "===== Starting httpd"
        if useCabalDev
          child_process.exec 'ls -d cabal-dev/packages-*.conf', (err, pkgdir) ->
            serverProc = child_process.spawn 'runghc', ['-isrc/hs', "-package-conf=#{pkgdir}", 'Main'],
              stdio: 'inherit'
            waitForServer triggerReload
        else
          serverProc = child_process.spawn 'runghc', ['-isrc/hs', 'Main'],
            stdio: 'inherit'
          waitForServer triggerReload
        done()

    if serverProc?
      console.log "trying to kill old server"
      if compileServer
        serverProc.on 'close', execServer
        serverProc.kill()
      else
        child_process.exec 'killall ghc', execServer
    else
      execServer()
    return

  grunt.loadNpmTasks "grunt-reload"
  grunt.loadNpmTasks "grunt-contrib-coffee"
  grunt.loadNpmTasks "grunt-contrib-jade"
  grunt.loadNpmTasks "grunt-contrib-stylus"

  grunt.registerTask "default", "coffee jade stylus server reload watch"
