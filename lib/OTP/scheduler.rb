require 'thwait'

module OTP
  class Scheduler
    @@schedulers = []
    @@pid_to_object = {}
    @@pid_to_scheduler_num = {}
    @@_pid = 0

    @@pid_lock = OTP::Spinlock.new("pid_lock")
    @@lock = OTP::Spinlock.new("spawn")

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

        @@pid_lock.lock
        @@pid_to_object[pid] = process
        @@pid_lock.unlock

        @@pid_to_scheduler_num[pid] = scheduler_num


        process.self_pid = pid

        @@lock.unlock
        pid
      end

      def pid_to_scheduler(pid)
        scheduler_num = @@pid_to_scheduler_num[pid]

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
    end

    def send_message(pid, *args)
      @@pid_lock.lock
      process = @@pid_to_object[pid]
      @@pid_lock.unlock

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
          @@pid_lock.lock
          process = @@pid_to_object[pid]
          @@pid_lock.unlock

          state = process.resume

          if state == :dead
            @@pid_lock.lock

            @@pid_to_object.delete(pid)
            @@pid_to_scheduler_num.delete(pid)

            @@pid_lock.unlock
          end

          @lock.lock
          pid = @runnable_pids.shift
          @lock.unlock
        end
      end
    end
  end
end
