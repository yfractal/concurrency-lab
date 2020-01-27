module LinearHash
  class Segnment
    attr_reader :segnment, :overflow_segnment

    def initialize
      @segnment = []
      @overflow_segnment = []
      @overflowed = false
    end

    def put(index, key, val)
      if @segnment[index]
        @overflow_segnment << [key, val]
        if !@overflowed
          @overflowed = true
          :need_grow
        end
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
      @table = [Segnment.new]
      @times_to_grow = 0
      @next_segnment_index = 0
    end

    def put(key, val)
      hash_val = hash_val(key)
      segnment_index = segnment_index(hash_val)
      segnment = @table[segnment_index]
      bucket_index = bucket_index(hash_val)

      put_segnment(segnment, bucket_index, key, val)

      maybe_grow
    end

    def put_segnment(segnment, bucket_index, key, val)
      @times_to_grow += 1 if :need_grow == segnment.put(bucket_index, key, val)
    end

    def get(key)
      hash_val = hash_val(key)
      segnment_index = segnment_index(hash_val)
      segnment = @table[segnment_index]
      bucket_index = bucket_index(hash_val)

      segnment.get(bucket_index, key)
    end

    def maybe_grow
      return if @times_to_grow == 0

      from_segnment_index = @next_segnment_index
      original_segnment = @table[@next_segnment_index]

      segnment_for_replace = Segnment.new
      new_segnment = Segnment.new

      next_level = @level + 1

      original_segnment.segnment.each do |key, val|
        next if key == nil

        hash_val = hash_val(key)
        segnment_index = segnment_index_next(hash_val)

        bucket_index = bucket_index(hash_val)

        if segnment_index == @next_segnment_index
          put_segnment(segnment_for_replace, bucket_index, key, val)
        else
          put_segnment(new_segnment, bucket_index, key, val)
        end
      end

      original_segnment.overflow_segnment.each do |key, val|
        next if key == nil

        hash_val = hash_val(key)

        segnment_index = segnment_index_next(hash_val)
        bucket_index = bucket_index(hash_val)

        if segnment_index == @next_segnment_index
          put_segnment(segnment_for_replace, bucket_index, key, val)
        else
          put_segnment(new_segnment, bucket_index, key, val)
        end
      end

      @table[from_segnment_index] = segnment_for_replace
      @table << new_segnment
      @times_to_grow -= 1
      @next_segnment_index += 1

      if @table.size == 2.pow(next_level)
        @level = next_level
        @next_segnment_index = 0
      end
    end

    def hash_val(key)
      raise "Key should be integer, but the key is #{key.inspect}" if key.class != Integer
      total_buckets = 2.pow(@level + 1) * @segnment_size

      key.abs % total_buckets
    end

    def segnment_index(hash_val)
      segnment_bit = Math.log @segnment_size, 2

      index = hash_val >> segnment_bit
      if index <= @next_segnment_index * 2
        index
      else
        # drop first bit
        index & (2.pow(@level) - 1)
      end
    end

    def segnment_index_next(hash_val)
      segnment_bit = Math.log @segnment_size, 2
      hash_val >> segnment_bit
    end

    def bucket_index(hash_val)
      hash_val & (@segnment_size - 1)
    end
  end
end
