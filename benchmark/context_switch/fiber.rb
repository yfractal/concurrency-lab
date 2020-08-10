# coding: utf-8
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


channel = Goroutine::Channel.new

Goroutine.go do
  iterations_count.times do
    e = channel.take
  end
end

iterations_count.times do
  channel.add(1)
end

def ping_pong_between_fibers(iterations_count)
  started_at = Time.now

  channel = Goroutine::Channel.new

  Goroutine.go do
    iterations_count.times do
      e = channel.take
    end
  end

  iterations_count.times do
    channel.add(1)
  end

  ended_at = Time.now
  ended_at - started_at
end

def ping_pong(iterations_count)
  started_at = Time.now

  channel = Goroutine::Channel.new

  Goroutine.go do
    iterations_count.times do
      channel.add(1)
      e = channel.take
    end
  end

  ended_at = Time.now
  ended_at - started_at
end

iterations_count = 1_000
fibers_cost = ping_pong_between_fibers(iterations_count)
ping_pong_cost = ping_pong(iterations_count)
cost = fibers_cost / ( 2 * iterations_count) - ping_pong_cost / iterations_count

puts %{
     Iterations_count=#{iterations_count}, fibers_cost=#{fibers_cost}s, ping_pong_cost=#{ping_pong_cost * 1000}ns
     Thread context swith cost: #{cost}s, #{cost * 1_000_000}µs
}
