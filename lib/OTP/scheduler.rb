require 'thwait'

module OTP
  class Scheduler
    @@schedulers = []
    @@pid_to_object = {}
    @@pid_to_scheduler_num = {}
    @@process_counter = 0
    @@_pid = 0

    class << self
      def start_schedulers(n)
        threads = []
        n.times do
          scheduler = self.new
          @@schedulers << scheduler

          threads << Thread.new do
            scheduler.loop
          end
        end

        @@schedulers
      end

      def spawn(klass, *args)
        scheduler_num = @@process_counter % @@schedulers.count
        scheduler = @@schedulers[scheduler_num]

        process = klass.send(:new, *args)

        pid = gen_pid
        @@pid_to_object[pid] = process
        @@pid_to_scheduler_num[pid] = scheduler_num

        process.self_pid = pid

        @@process_counter += 1

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
    end

    def send_message(pid, *args)
      process = @@pid_to_object[pid]

      process.send_to_mailbox(args)
      @runnable_pids << pid
    end

    def loop
      while true
        while pid = @runnable_pids.shift
          process = @@pid_to_object[pid]

          state = process.resume

          @@pid_to_object.delete(pid) if state == :dead
        end

        sleep(0.01)
      end
    end
  end
end
