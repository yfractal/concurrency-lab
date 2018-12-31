require "concurrent"
module OTP
  class Spinlock
    def initialize(spin = 524288)
      @lock = Concurrent::AtomicFixnum.new
      @spin = spin
    end

    def lock
      while true
        return if @lock.compare_and_set(0, 1)

        n = 1
        while n < @spin
          n << 1
          i = 0

          while i < n
            i += 1
          end

          puts "Current spin: #{n}"

          return if @lock.compare_and_set(0, 1)
        end
      end
    end

    def unlock
      @lock.value = 0
    end
  end
end
