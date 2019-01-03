require 'thwait'

module OTP
  class Scheduler
    @@schedulers = []
    @@pid_to_scheduler_num = {}
    @@_pid = 0

    @@pid_lock = OTP::Spinlock.new("Pid")
    @@lock = OTP::Spinlock.new("Spawn")

    @@pid_to_scheduler_num_lock = OTP::Spinlock.new("PidToSchedulerNum")

    class << self
      def start_schedulers(n)
        n.times do
          scheduler = self.new
          @@schedulers << scheduler

          Thread.new do
            scheduler.loop
          end
        end

        @@schedulers
      end

      def spawn(klass, *args)
        scheduler_num = @@_pid % @@schedulers.count

        process = klass.send(:new, *args)
        @@lock.lock
        pid = gen_pid

        @@schedulers[scheduler_num].set_pid_to_process(pid, process)

        @@pid_to_scheduler_num_lock.lock
        @@pid_to_scheduler_num[pid] = scheduler_num
        @@pid_to_scheduler_num_lock.unlock

        process.self_pid = pid

        @@lock.unlock

        pid
      end

      def pid_to_scheduler(pid)
        @@pid_to_scheduler_num_lock.lock
        scheduler_num = @@pid_to_scheduler_num[pid]
        @@pid_to_scheduler_num_lock.unlock

        @@schedulers[scheduler_num]
      end

      def send_message(pid, *args)
        scheduler = pid_to_scheduler(pid)
        scheduler.send_message(pid, *args)
      end

      private
      def gen_pid
        @@_pid += 1
      end
    end

    def initialize
      @runnable_pids = []
      @lock = OTP::Spinlock.new "Runnable pids"

      @pid_to_process = {}
      @pid_to_process_lock = OTP::Spinlock.new "pid_to_process_lock"
    end

    def send_message(pid, *args)
      process = get_process(pid)

      process.send_to_mailbox(args)

      @lock.lock
      @runnable_pids << pid
      @lock.unlock
    end

    def loop
      while true
        @lock.lock
        pid = @runnable_pids.shift
        @lock.unlock

        while pid
          process = get_process(pid)
          state = process.resume

          delete_process(pid) if state == :dead

          @lock.lock
          pid = @runnable_pids.shift
          @lock.unlock
        end
      end
    end

    def set_pid_to_process(pid, process)
      @pid_to_process_lock.lock
      @pid_to_process[pid] = process
      @pid_to_process_lock.unlock
    end

    def get_process(pid)
      @pid_to_process_lock.lock
      process = @pid_to_process[pid]
      @pid_to_process_lock.unlock

      process
    end

    def delete_process(pid)
      @pid_to_process_lock.lock
      @pid_to_process.delete(pid)
      @pid_to_process_lock.unlock

      @@pid_to_scheduler_num_lock.lock
      @@pid_to_scheduler_num.delete(pid)
      @@pid_to_scheduler_num_lock.unlock
    end
  end
end
