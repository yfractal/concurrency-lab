require 'thwait'

module OTP
  class Scheduler
    @@schedulers = []
    @@pid_to_object = {}
    @@pid_to_scheduler_num = {}
    @@process_counter = 0
    @@_pid = 0
    @@pid_to_object_mutex = Mutex.new
    @@spawn_mutex = Mutex.new

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
        @@spawn_mutex.synchronize {
          scheduler_num = @@process_counter % @@schedulers.count
          scheduler = @@schedulers[scheduler_num]

          process = klass.send(:new, *args)

          pid = gen_pid

          @@pid_to_object_mutex.synchronize {
            @@pid_to_object[pid] = process
          }
          @@pid_to_scheduler_num[pid] = scheduler_num

          process.self_pid = pid

          @@process_counter += 1

          pid
        }
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
      @runnable_pids_mutex = Mutex.new
    end

    def send_message(pid, *args)
      process = @@pid_to_object_mutex.synchronize {
        @@pid_to_object[pid]
      }

      process.send_to_mailbox(args)

      @runnable_pids_mutex.synchronize {
        @runnable_pids << pid
      }
    end

    def loop
      while true
        pid = @runnable_pids_mutex.synchronize {
          @runnable_pids.shift
        }

        while pid
          process = @@pid_to_object_mutex.synchronize {
            @@pid_to_object[pid]
          }

          state = process.resume

          if state == :dead
            @@pid_to_object_mutex.synchronize {
              @@pid_to_object.delete(pid)
            }
          end

          pid = @runnable_pids_mutex.synchronize {
            @runnable_pids.shift
          }
        end

        sleep(0.01)
      end
    end
  end
end
