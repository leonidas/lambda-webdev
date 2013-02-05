require.config
  baseUrl: "js/modules",
  paths:
    knockout: 'http://cdnjs.cloudflare.com/ajax/libs/knockout/2.2.0/knockout-min'
    jquery:   'https://ajax.googleapis.com/ajax/libs/jquery/1.8.2/jquery.min'



define ['jquery'], ($) ->

  $('#main').show()
