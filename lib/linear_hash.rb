module LinearHash
  class Segnment
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
      @level = 1
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

    def hash_val(key)
      raise "Key should be integer" if key.class != Integer
      total_buckets = 2.pow(@level) * @segnment_size

      key % total_buckets
    end

    def segnment_index(hash_val)
      0 # tmp hard code
    end

    def bucket_index(hash_val)
      hash_val & (@segnment_size - 1)
    end
  end
end
