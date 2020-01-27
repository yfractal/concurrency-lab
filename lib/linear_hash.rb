module LinearHash
  class Segnment
    attr_reader :segnment, :overflow_segnment

    def initialize
      @segnment = []
      @overflow_segnment = []
    end

    def put(index, key, val)
      if @segnment[index]
        @overflow_segnment << [key, val]
      else
        @segnment[index] = [key, val]
      end
    end

    def get(index, key)
      bucket = @segnment[index] || []

      k, v = bucket[0], bucket[1]

      if k == key
        v
      else
        @overflow_segnment.each do |k, v|
          return v if k == key
        end

        nil
      end
    end
  end

  class Hash
    def initialize
      @level = 0
      @segnment_size = 4
      segnment = Segnment.new
      @table = [segnment]
    end

    def put(key, val)
      hash_val = hash_val(key)
      segnment_index = segnment_index(hash_val)
      segnment = @table[segnment_index]
      bucket_index = bucket_index(hash_val)

      segnment.put(bucket_index, key, val)
    end

    def get(key)
      hash_val = hash_val(key)
      segnment_index = segnment_index(hash_val)
      segnment = @table[segnment_index]
      bucket_index = bucket_index(hash_val)

      segnment.get(bucket_index, key)
    end

    def grow
      @next_segnment_index = 0
      from_segnment_index = @next_segnment_index
      original_segnment = @table[@next_segnment_index]

      segnment_for_replace = Segnment.new
      new_segnment = Segnment.new

      next_level = @level + 1

      original_segnment.segnment.each do |key, val|
        next if key == nil

        hash_val = hash_val(key, next_level)
        segnment_index = segnment_index(hash_val)

        bucket_index = bucket_index(hash_val)

        if segnment_index == @next_segnment_index
          segnment_for_replace.put(bucket_index, key, val)
        else

          new_segnment.put(bucket_index, key, val)
        end
      end

      original_segnment.overflow_segnment.each do |key, val|
        next if key == nil

        hash_val = hash_val(key, next_level)

        segnment_index = segnment_index(hash_val)
        bucket_index = bucket_index(hash_val)

        if segnment_index == @next_segnment_index
          segnment_for_replace.put(bucket_index, key, val)
        else
          new_segnment.put(bucket_index, key, val)
        end
      end

      @table[from_segnment_index] = segnment_for_replace
      @table << new_segnment

      @level += 1
    end

    def hash_val(key, level = @level)
      raise "Key should be integer, but the key is #{key.inspect}" if key.class != Integer
      total_buckets = 2.pow(level) * @segnment_size

      key % total_buckets
    end

    def segnment_index(hash_val)
      segnment_bit = Math.log @segnment_size, 2

      hash_val >> segnment_bit
    end

    def bucket_index(hash_val)
      hash_val & (@segnment_size - 1)
    end
  end
end
