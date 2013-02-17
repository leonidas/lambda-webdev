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
      @name         = ko.observable()
      @view         = ko.observable("game")
      @opponentName = ko.observable()
      @constructBoard()

      @nameRequest = null

      @conn.onRequest "AskName", (data, callback) =>
        n = @name()
        if n? then callback n else @nameRequest = callback

      @conn.onNotify "GameBoard", (board) =>
        console.log "Board:", board
        #@board board

    constructBoard: () ->
      @board = ko.observableArray()
      for r in [1..3]
        row = ko.observableArray()
        @board().push row
        for c in [1..3]
          row().push ko.observable()
      return

    enterName: () ->
      n = @name()
      return if not n

      @view "waiting"
      if @nameRequest?
        @nameRequest n
        @nameRequest = null

      false

  vmo = new GameViewModel(msg.connect("ws://localhost:8000/"))

  ko.applyBindings vmo

  $('#main').show()
