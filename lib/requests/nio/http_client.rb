require 'socket'
require 'uri'


class HTTPClient
  attr_reader :uri, :socket

  def initialize(host)
    @host    = URI(host)
    @socket  = Socket.new(:INET, :STREAM)
  end

  def connect_nonblock
    remote_addr = Socket.pack_sockaddr_in(@host.port, @host.host)
    socket.connect_nonblock(remote_addr, exception: false)
  end

  def get(path)
    lines = ["GET #{path} HTTP/1.0"]
    lines << "Host: #{@host.host}\r\n"
    lines << "\r\n"
    line = lines.join("\r\n")

    socket.write line
  end
end

class AsyncGetClient
  def initialize(host, path, event_loop)
    @host        = host
    @path        = path
    @event_loop  = event_loop
    @client      = HTTPClient.new(@host)
  end

  def start
    @event_loop.attach(@client.socket, self, :w)

    @state = :waiting_connection

    @client.connect_nonblock
  end

  def writable
    if @state == :waiting_connection
      @state = :waiting_response

      @event_loop.update_interest(@client.socket, :r)
      @client.get(@path)
    end
  end

  def readable(data)
    if @state == :waiting_response
      puts "receive #{data}"
      @event_loop.detach(@client.socket, self)
    end
  end

  def error(e)
    puts "error: #{e.message}"
  end

  def close
    @client.socket.close
  end
end

# event_loop = EventLoop.new
# client = AsyncGetClient.new("http://127.0.0.1:4000", "/", event_loop)

# client.start

# sleep(10)
