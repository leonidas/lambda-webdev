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
    constructor: () ->
      @name         = ko.observable()
      @view         = ko.observable("enterName")
      @opponentName = ko.observable()

      @constructBoard()

      @nameRequest = null
      @moveRequest = null

      @reconnect()

      @conn.onRequest "AskName", (data, callback) =>
        n = @name()
        if n? then callback n else @nameRequest = callback
        return

      @conn.onNotify "GameBoard", (board) =>
        @updateBoard board
        return

      @conn.onNotify "FoundOpponent", (name) =>
        @opponentName name
        @view "game"
        return

      @conn.onRequest "AskMove", (data, callback) =>
        @moveRequest = callback
        return

      @conn.onDisconnect =>
        @view "disconnected"

    reconnect: () ->
      @conn = msg.connect("ws://localhost:8000/")
      return

    clearBoard: () ->
      for row in @board()
        for col in row
          col.value null
      return

    updateBoard: (board) ->
      @clearBoard()
      for [[col, row], piece] in board
        @board()[row]()[col].value piece
      return

    constructBoard: () ->
      @board = ko.observableArray()
      for r in [1..3]
        row = ko.observableArray()
        @board().push row
        for c in [1..3]
          row().push
            row: r
            col: c
            value: ko.observable(null)
      return

    makeMove: (cell) -> () ->
      return if not @moveRequest?
      @moveRequest [cell.col, cell.row]
      @moveRequest = null
      return

    enterName: () ->
      n = @name()
      return if not n

      @view "waiting"
      if @nameRequest?
        @nameRequest n
        @nameRequest = null

      false

  vmo = new GameViewModel()

  ko.applyBindings vmo

  $('#main').show()
