require.config
  baseUrl: "js/modules",
  paths:
    knockout: 'http://cdnjs.cloudflare.com/ajax/libs/knockout/2.2.0/knockout-min'
    jquery:   'https://ajax.googleapis.com/ajax/libs/jquery/1.8.2/jquery.min'
    mapping:  '../vendor/knockout.mapping'


define (require) ->

  $   = require 'jquery'
  ko  = require 'knockout'
  msg = require 'messaging'

  class GameViewModel
    constructor: (@conn) ->
      @name = ko.observable()
      @view = ko.observable()
      @board = ko.observable()
      @conn.onNotify "GameBoard", (board) =>
        console.log "Board:", board
        @board board

    enterName: () ->
      @conn.notify "Name", @name()
      false

  vmo = new GameViewModel(msg.connect("ws://localhost:8000/"))

  ko.applyBindings vmo

  $('#main').show()
