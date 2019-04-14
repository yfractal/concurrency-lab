require 'httparty'
require 'concurrent'
require 'curb'

@counter = 0
@lock    = Mutex.new

def request
  url = "http://127.0.0.1:4000"
  Curl.get url

  @lock.synchronize do
    @counter += 1
  end
end

REQUESTS = 1000

start_time = Time.now

REQUESTS.times do |i|
  Concurrent::Future.execute {
    request
  }
end

# wait those requests finished
while @counter < REQUESTS * 0.99
end

end_time = Time.now

spend = end_time - start_time

puts "Spend: #{spend} seconds"

puts "Request per second #{ REQUESTS.to_f / spend }"

puts "Finished #{@counter}"
