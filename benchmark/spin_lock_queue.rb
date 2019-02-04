require "bundler/setup"

require "./lib/spinlock_queue"

THREADS_COUNT = 1
ITEMS_COUNT_FOR_EACH_THREAD = 1000000

puts "Benchmark for spin lock implement queue with thread #{THREADS_COUNT}"

lock_free_queue = SpinlockQueue.new

started_at = Time.now

puts "Start at #{started_at}"

threads = THREADS_COUNT.times.collect do
  Thread.new do
    i = 0
    n = ITEMS_COUNT_FOR_EACH_THREAD

    while i < n
      lock_free_queue.push i

      i += 1
    end
  end
end

threads.map &:join


ended_at = Time.now
puts "End at #{ended_at}"

size = lock_free_queue.size

puts "Takes #{ended_at - started_at}"
puts "The queue size is: #{size}"
