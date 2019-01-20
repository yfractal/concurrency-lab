require "fiber"

module Goroutine
  class Channel
    def initialize
      @queue = []
    end

    def add(e)
      @queue << e

      @fiber.resume(@queue.shift) if @fiber
    end

    def take
      if @queue.empty?
        @fiber = Fiber.yield

        Fiber.yield
      else
        @fiber = Fiber.yield
        @fiber = nil

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

# example 1: take then add
channel = Goroutine::Channel.new

go do
  e = channel.take
  puts e
end

channel.add(1)

# example 2: add then take
channel = Goroutine::Channel.new
channel.add(1)

go do
  e = channel.take
  puts e
end
