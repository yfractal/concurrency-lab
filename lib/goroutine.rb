require "fiber"

module Goroutine
  def self.go(&block)
    fiber = Fiber.new &block
    fiber.resume

    fiber.resume(fiber)
  end

  class Channel
    def initialize
      @queue = []
    end

    def add(e)
      @queue << e

      if @waiting
        @waiting = false
        @go_fiber.resume(@queue.shift)
      end
    end

    def take
      @go_fiber = Fiber.yield unless @go_fiber

      if @queue.empty?
        @waiting = true
        Fiber.yield
      else
        @queue.shift
      end
    end
  end
end

puts "example 1: take then add"
channel = Goroutine::Channel.new

Goroutine.go do
  e = channel.take
  puts e
end
channel.add(1)

puts "example 2: add then take"
channel = Goroutine::Channel.new
channel.add(1)

Goroutine.go do
  e = channel.take
  puts e
end

puts "example 3: add, add, take"
channel = Goroutine::Channel.new
channel.add(1)
channel.add(2)

Goroutine.go do
  e1 = channel.take
  puts e1

  e2 = channel.take
  puts e2
end


puts "example 4: add, take, add, take"
channel = Goroutine::Channel.new
channel.add(1)

Goroutine.go do
  e1 = channel.take
  puts e1

  e2 = channel.take
  puts e2
end

channel.add(2)
channel.add(3)
