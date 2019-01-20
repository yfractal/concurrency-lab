require "fiber"

module Goroutine
  class Channel
    def initialize
      @queue = []
    end

    def add(e)
      @queue << e

      @fiber.resume(@queue.shift)
    end

    def take
      if @queue.empty?
        @fiber = Fiber.yield

        Fiber.yield
      else
        @queue.shift
      end
    end
  end
end

def go(&block)
  fiber = Fiber.new &block
  fiber.resume

  fiber.resume(fiber)
end

# example 1
channel = Goroutine::Channel.new

f = go do
  e = channel.take
  puts e
end

channel.add(1)
