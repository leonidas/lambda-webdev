
define (require) ->

  class Connection
    constructor: (url) ->
      @sock = new WebSocket(url)
      @reqHandlers = {}
      @ntfyHandlers = {}
      @reqId  = 0
      @reqMap = {}

      @sock.onmessage = (msg) =>
        data = JSON.parse msg.data

        switch data[0]
          when "request"
            [_, rqId, payload] = data
            @reqHandlers[payload.msg]? payload.data, (resp) =>
              @sock.send JSON.stringify ["response", rqId, resp]

          when "notify"
            [_, payload] = data
            @ntfyHandlers[payload.msg]? payload.data

          when "response"
            [_, rqId, payload] = data
            @reqMap[rqId]? payload

          when "error"
            [_, errMsg] = data
            console.log "Messaging ERROR: #{errMsg}"

          when "debug"
            [_, debug] = data
            console.log "Messaging DEBUG: #{debug}"
        return

    onConnect: (callback) ->
      @sock.onopen callback
      return

    onDisconnect: (callback) ->
      @sock.onclose callback
      return

    onRequest: (type, callback) ->
      @reqHandlers[type] = callback
      return

    onNotify: (type, callback) ->
      @ntfyHandlers[type] = callback
      return

    request: (type, data, callback) ->
      rqId = @reqId
      @reqId += 1
      @reqMap[rqId] = callback
      @sock.send JSON.stringify ["request", rqId, {msg:type, data:data}]
      return

    notify: (type, data) ->
      @sock.send JSON.stringify ["notify", {msg:type, data:data}]
      return

  connect: (url) -> new Connection(url)
