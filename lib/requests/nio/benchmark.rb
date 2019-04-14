require "./lib/requests/nio/http_client"
require "./lib/requests/nio/event_loop"

class Supervisor
  def monitor(instance)
    instance.monitored(self)
  end

  def exist_normal
  end

  def exist_exception
  end
end

class RequestSupervisor < Supervisor
  attr_reader :success_count

  def initialize(client_count, &finished_callback)
    @lock              = Mutex.new
    @success_count     = 0
    @client_count      = client_count
    @finished_callback = finished_callback
  end

  def exist_normal
    @lock.synchronize do
      @success_count += 1
      if @success_count > @client_count * 0.9
        @finished_callback.call
      end
    end
  end
end

class Client < AsyncGetClient
  def monitored(monitor)
    @monitor = monitor
  end

  def readable(data)
    super

    @monitor.exist_normal
  end
end

event_loop = EventLoop.new

start_at = Time.now

REQUESTS = 1000

supervisor = RequestSupervisor.new(REQUESTS) do
  end_at = Time.now

  spend = end_at - start_at

  puts "Spend: #{spend} seconds"

  puts "Request per second #{ REQUESTS.to_f / spend }"
end

REQUESTS.times do
  client = Client.new("http://127.0.0.1:4000", "/", event_loop)

  supervisor.monitor(client)

  client.start
end

sleep(100)
