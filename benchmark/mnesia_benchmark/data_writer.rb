require 'securerandom'

# write_random_kv("data.txt", 100000)
def write_random_kv(file, num)
  File.open(file, "w+") do |f|
    num.times {
      f.puts "#{SecureRandom.uuid} #{SecureRandom.hex}"
    }
  end
end
