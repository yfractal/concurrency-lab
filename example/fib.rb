require "bundler/setup"
require "OTP"

class Reporter < OTP::Process
  def initialize
    super()
    @t0 = Time.now
    @result_count = 0

    receive(:start) do |total|
      start(total)
    end
  end

  def start(total)
    total.times do |i|
      fib = OTP::Scheduler.spawn(Fib)
      OTP::Scheduler.send_message(fib, :cal, self_pid)
    end

    wait_for_result(total)
  end

  def wait_for_result(total)
    receive(:result) do |r|
      # puts "Current #{total}" if total % 100 == 0 || total > 9900
      if total == 1
        puts "Takes #{(Time.now - @t0) * 1000} ms."
      else
        wait_for_result(total - 1)
      end
    end
  end
end

class Fib < OTP::Process
  def initialize
    super()

    receive(:cal) do |receiver|
      cal(23, receiver)
    end
  end

  def cal(n, receiver)
    r = do_cal(n)

    OTP::Scheduler.send_message(receiver, :result, r)
  end

  def do_cal(n)
    case n
    when 0, 1
      1
    else
      do_cal(n-2) + do_cal(n-1)
    end
  end
end

OTP::Scheduler.start_schedulers(2)

reporter = OTP::Scheduler.spawn(Reporter)

OTP::Scheduler.send_message(reporter, :start, 10000)

while true
  sleep(10)

  puts "looping"
end
