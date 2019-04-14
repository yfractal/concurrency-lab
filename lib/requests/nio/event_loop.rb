require "nio"
require "thread"

class EventLoop
  def initialize
    @nio = @thread = nil
    @map = {}
    @stopping = false
    @todo = Queue.new

    @spawn_mutex = Mutex.new
    spawn
  end

  def attach(io, stream, mode)
    @todo << lambda do
      @map[io] = @nio.register(io, mode)
      @map[io].value = stream
    end

    wakeup
  end

  def update_interest(io, mode)
    @map[io].interests = mode
  end

  def detach(io, stream)
    @todo << lambda do
      @nio.deregister io
      @map.delete io
    end

    wakeup
  end

  def stop
    @stopping = true
    wakeup if @nio
  end

  private
  def spawn
    return if @thread && @thread.status

    @spawn_mutex.synchronize do
      return if @thread && @thread.status

      @nio ||= NIO::Selector.new

      @thread = Thread.new { run }

      return true
    end
  end

  def wakeup
    spawn || @nio.wakeup
  end

  def run
    loop do
      if @stopping
        @nio.close

        break
      end

      until @todo.empty?
        @todo.pop(true).call
      end

      monitors = @nio.select(0.1)
      next unless monitors

      monitors.each do |monitor|
        io = monitor.io
        stream = monitor.value

        begin
          if monitor.writable?
            stream.writable
          end

          if monitor.readable?
            incoming = io.read_nonblock(4096, exception: false)

            case incoming
            when :wait_readable
              next
            else
              stream.readable incoming
            end
          end
        rescue Exception => e
          stream.error(e)

          detach(io, stream)
        end
      end
    end
  end
end
