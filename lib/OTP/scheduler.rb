module OTP
  class Scheduler
    def initialize
      @_pid = 0
      @pid_to_object = {}
      @runnable_pids = []
    end

    def spawn(klass, *args)
      process = klass.send(:new, *args)

      pid = register_process(process)
      process.self_pid = pid
    end

    def send_message(pid, *args)
      process = @pid_to_object[pid]

      process.send_to_mailbox(args)
      @runnable_pids << pid
    end

    def loop
      while pid = @runnable_pids.shift
        process = @pid_to_object[pid]

        state = process.resume

        @pid_to_object.delete(pid) if state == :dead
      end
    end

    private
    def gen_pid
      @_pid += 1
    end

    def register_process(process)
      pid = gen_pid
      @pid_to_object[pid] = process

      pid
    end
  end

end
