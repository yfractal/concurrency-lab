require "bundler/setup"
require "OTP"

class Skynet < OTP::Process
  def initialize(scheduler, parent)
    super()
    @scheduler = scheduler
    @parent = parent
    spawn_children
  end

  def spawn_children
    receive(:spawn_children) do |num, size, div|
      if size <= 1
        @scheduler.send_message(@parent, :sum, num) if @parent
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
          @scheduler.send_message(@parent, :sum, total + n)
        else
          puts "The result is #{total + n}"
        end
      end
    end
  end

  def do_spawn_children(num, size, div)
    new_size = size / div

    div.times do |x|
      pid = @scheduler.spawn(Skynet, @scheduler, self_pid)

      @scheduler.
        send_message(pid, :spawn_children, num + x * new_size, new_size, div)
    end
  end
end


scheduler = OTP::Scheduler.new
num, size, div = 0, 1000000, 10

skynet_pid = scheduler.spawn(Skynet, scheduler, nil)
scheduler.send_message(skynet_pid, :spawn_children, num, size, div)

t0 = Time.now
scheduler.loop

t1 = Time.now
puts "Takes #{(t1 - t0) * 1000} ms."
