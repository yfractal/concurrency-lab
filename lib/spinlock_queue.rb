require "OTP"

class SpinlockQueue
  class Node
    attr_accessor :item, :successor

    def initialize(item, successor)
      self.item      = item
      self.successor = successor
    end
  end

  attr_accessor :head, :tail

  def initialize
    @lock = OTP::Spinlock.new
    dummy_node = Node.new(:dummy, nil)

    self.head = dummy_node
    self.tail = dummy_node
  end

  def push(item)
    new_node = Node.new(item, nil)

    @lock.lock

    tail.successor = new_node
    self.tail = new_node

    @lock.unlock

    true
  end

  def size
    successor = head.successor
    count     = 0

    while true
      break if successor.nil?

      current_node = successor
      successor    = current_node.successor
      count        += 1
    end

    count
  end
end
