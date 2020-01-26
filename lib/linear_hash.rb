module LinearHash
  class Hash
    def initialize
      @level = 1
      @segnment_size = 4
      @table = [[]]
    end

    def get(key)
    end

    def put(key, val)
      hash_val = hash_val(key)
      segnment_index = segnment_index(hash_val)
      segnment = @table[segnment_index]
      bucket_index = bucket_index(hash_val)

      segnment[bucket_index] = [key, val]
    end

    def get(key)
      hash_val = hash_val(key)
      segnment_index = segnment_index(hash_val)
      segnment = @table[segnment_index]
      bucket_index = bucket_index(hash_val)

      bucket = segnment[bucket_index] || []

      k, v = bucket[0], bucket[1]

      if k == key
        v
      else
        nil
      end
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
