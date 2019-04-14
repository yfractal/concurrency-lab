require 'curb'

url = "http://127.0.0.1:4000"

REQUESTS = 1000

start_time = Time.now

REQUESTS.times do |i|
  Curl.get url
end

end_time = Time.now

spend = end_time - start_time

puts "Spend: #{spend} seconds"
