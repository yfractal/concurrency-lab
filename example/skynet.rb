require "bundler/setup"
require "OTP"

class Reporter < OTP::Process
  def initialize
    super()
    t0 = Time.now
    receive(:report) do |r|
      t1 = Time.now
      puts "The result is #{r}."
      puts "Takes #{(t1 - t0) * 1000} ms."
    end
  end
end

class Skynet < OTP::Process
  def initialize(parent, reporter = nil)
    super()
    @parent = parent
    @reporter = reporter if reporter
    spawn_children
  end

  def spawn_children
    receive(:spawn_children) do |num, size, div|
      if size <= 1
        OTP::Scheduler.send_message(@parent, :sum, num) if @parent
      else
        do_spawn_children(num, size, div)

        process_sum(div, 0, 0)
      end
    end
  end

  def process_sum(div, received, total)
    receive(:sum) do |n|
      if received < div - 1
        process_sum(div, received + 1, total + n)
      else
        if @parent
          OTP::Scheduler.send_message(@parent, :sum, total + n)
        else
          OTP::Scheduler.send_message(@reporter, :report, total + n)
        end
      end
    end
  end

  def do_spawn_children(num, size, div)
    new_size = size / div

    div.times do |x|
      pid = OTP::Scheduler.spawn(Skynet, self_pid)

      OTP::Scheduler.
        send_message(pid, :spawn_children, num + x * new_size, new_size, div)
    end
  end
end

OTP::Scheduler.start_schedulers(4)

num, size, div = 0, 100000, 10

reporter = OTP::Scheduler.spawn(Reporter)

skynet_pid = OTP::Scheduler.spawn(Skynet, nil, reporter)

OTP::Scheduler.send_message(skynet_pid, :spawn_children, num, size, div)

while true
  sleep(5)

  puts "looping"
end
