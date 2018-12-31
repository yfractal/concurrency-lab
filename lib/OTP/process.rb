module OTP
  class Process
    attr_accessor :self_pid

    def initialize
      @mailbox = []
      @receivers = []
    end

    def send_to_mailbox(msg)
      @mailbox << msg
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

            @mailbox.delete_at i
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
