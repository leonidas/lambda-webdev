require.config
  baseUrl: "js/modules",
  paths:
    knockout: 'http://cdnjs.cloudflare.com/ajax/libs/knockout/2.2.0/knockout-min'
    jquery:   'https://ajax.googleapis.com/ajax/libs/jquery/1.8.2/jquery.min'
    mapping:  '../vendor/knockout.mapping'


define (require) ->

  $ = require 'jquery'
  ko = require 'knockout'

  class GameViewModel
    constructor: () ->
      @name = ko.observable()
      @view = ko.observable("enterName")

    enterName: () ->
      false

  vmo = new GameViewModel()

  ko.applyBindings vmo

  $('#main').show()
