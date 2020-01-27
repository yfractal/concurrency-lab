module LinearHash
  class Segment
    attr_reader :segment, :overflow_segment

    def initialize
      @segment = []
      @overflow_segment = []
      @overflowed = false
    end

    def put(index, key, val)
      if @segment[index]
        @overflow_segment << [key, val]
        if !@overflowed
          @overflowed = true
          :need_grow
        end
      else
        @segment[index] = [key, val]
      end
    end

    def get(index, key)
      bucket = @segment[index] || []

      k, v = bucket[0], bucket[1]

      if k == key
        v
      else
        @overflow_segment.each do |k, v|
          return v if k == key
        end

        nil
      end
    end
  end

  class Hash
    def initialize
      @level = 0
      @segment_size = 4
      @table = [Segment.new]
      @times_to_grow = 0
      @next_segment_index = 0
    end

    def put(key, val)
      hash_val = hash_val(key)
      segment_index = segment_index(hash_val)
      segment = @table[segment_index]
      bucket_index = bucket_index(hash_val)

      put_segment(segment, bucket_index, key, val)

      maybe_grow
    end

    def put_segment(segment, bucket_index, key, val)
      @times_to_grow += 1 if :need_grow == segment.put(bucket_index, key, val)
    end

    def get(key)
      hash_val = hash_val(key)
      segment_index = segment_index(hash_val)
      segment = @table[segment_index]
      bucket_index = bucket_index(hash_val)

      segment.get(bucket_index, key)
    end

    def maybe_grow
      return if @times_to_grow == 0

      from_segment_index = @next_segment_index
      segment = @table[@next_segment_index]

      new_segment1 = Segment.new
      new_segment2 = Segment.new

      next_level = @level + 1

      segment.segment.each do |key, val|
        next if key == nil

        hash_val = hash_val(key)
        segment_index = segment_index_next(hash_val)

        bucket_index = bucket_index(hash_val)

        if segment_index == @next_segment_index
          put_segment(new_segment1, bucket_index, key, val)
        else
          put_segment(new_segment2, bucket_index, key, val)
        end
      end

      segment.overflow_segment.each do |key, val|
        next if key == nil

        hash_val = hash_val(key)

        segment_index = segment_index_next(hash_val)
        bucket_index = bucket_index(hash_val)

        if segment_index == @next_segment_index
          put_segment(new_segment1, bucket_index, key, val)
        else
          put_segment(new_segment2, bucket_index, key, val)
        end
      end

      @table[from_segment_index] = new_segment1
      @table << new_segment2
      @times_to_grow -= 1
      @next_segment_index += 1

      if @table.size == 2.pow(next_level)
        @level = next_level
        @next_segment_index = 0
      end
    end

    def hash_val(key)
      raise "Key should be positive integer, but the key is #{key.inspect}" if key.class != Integer or key < 0
      slots = 2.pow(@level) * @segment_size * 2

      key % slots
    end

    def segment_index(hash_val)
      segment_bit = Math.log @segment_size, 2

      index = hash_val >> segment_bit
      if index < @table.size
        index
      else
        # drop first bit
        index & (2.pow(@level) - 1)
      end
    end

    def segment_index_next(hash_val)
      segment_bit = Math.log @segment_size, 2
      hash_val >> segment_bit
    end

    def bucket_index(hash_val)
      hash_val & (@segment_size - 1)
    end
  end
end
