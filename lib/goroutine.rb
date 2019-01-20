require "fiber"

module Goroutine
  class Channel
    def initialize
      @queue = []
    end

    def add(e)
      @queue << e
      if @go_fiber && @go_fiber.alive?
        @go_fiber.resume(@queue.shift)
      else
        @go_fiber = nil
      end
    end

    def take
      if @queue.empty?
        # set fiber
        @go_fiber = Fiber.yield unless @go_fiber

        Fiber.yield
      else
        @go_fiber = Fiber.yield unless @go_fiber

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

puts "example 1: take then add"
channel = Goroutine::Channel.new

go do
  e = channel.take
  puts e
end
channel.add(1)

puts "example 2: add then take"
channel = Goroutine::Channel.new
channel.add(1)

go do
  e = channel.take
  puts e
end

puts "example 3: add, add, take"
channel = Goroutine::Channel.new
channel.add(1)
channel.add(2)

go do
  e1 = channel.take
  puts e1

  e2 = channel.take
  puts e2
end


puts "example 4: add, take, add, take"
channel = Goroutine::Channel.new
channel.add(1)

go do
  e1 = channel.take
  puts e1

  e2 = channel.take
  puts e2
end

channel.add(2)
channel.add(3)
