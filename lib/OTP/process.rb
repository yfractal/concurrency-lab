module OTP
  class Process
    attr_accessor :self_pid

    def initialize
      @mailbox = []
      @receivers = []

      @lock = OTP::Spinlock.new "Mailbox"
    end

    def send_to_mailbox(msg)
      @lock.lock
      @mailbox << msg
      @lock.unlock
    end

    ## TODO: find a better receive syntax
    def receive(type, &block)
      @receivers << [type, block.arity + 1, block]
    end

    def resume
      l = @mailbox.length
      i = 0

      while i < l
        msg = @mailbox[i]
        @receivers.each do |receiver|
          if msg[0] == receiver[0] && msg.count == receiver[1]
            @receivers = []

            receiver[2].call *msg[1..-1]

            @lock.lock
            @mailbox.delete_at i
            @lock.unlock

            state = if @receivers.empty?
                      # no receivers, process dead
                      :dead
                    elsif @mailbox.count == 0
                      # have receivers && no message, waiting for more message
                      :wait
                    else
                      # have receivers && has message, runnable
                      :runnable
                    end
            return state
          end
        end
        i += 1
      end

      ## no message matched, wait new mesage comes
      :wait
    end
  end
end
