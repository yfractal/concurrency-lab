require "concurrent"
module OTP
  class Spinlock
    def initialize(name = "", spin = 1024)
      @lock = Concurrent::AtomicFixnum.new
      @spin = spin
      @name = name
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

          # puts "Confilic for #{@name}"
          return if @lock.compare_and_set(0, 1)
        end
      end
    end

    def unlock
      @lock.value = 0
    end
  end
end
