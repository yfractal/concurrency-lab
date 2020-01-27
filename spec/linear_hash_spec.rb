require "linear_hash"

RSpec.describe LinearHash do
  describe "without grow" do
    it "get none exist key" do
      hash = LinearHash::Hash.new
      expect(hash.get(1024)).to eq(nil)
    end

    it "put and get" do
      hash = LinearHash::Hash.new
      hash.put(1, 10)

      expect(hash.get(1)).to eq(10)
    end

    it "for same position" do
      hash = LinearHash::Hash.new
      hash.put(1, 10)

      expect(hash.get(5)).to eq(nil)
    end
  end

  describe "overflow without grow" do
    it "put and get" do
      hash = LinearHash::Hash.new
      hash.put(1, 10)
      hash.put(5, 50)

      expect(hash.get(1)).to eq(10)
      expect(hash.get(5)).to eq(50)
    end
  end

  describe "handle grow" do
    it "grow once" do
      hash = LinearHash::Hash.new

      hash.put(1, 10)
      hash.put(5, 50) # will split one

      expect(hash.get(1)).to eq(10)
      expect(hash.get(5)).to eq(50)

      hash.maybe_grow # grow should be idempotent

      expect(hash.get(1)).to eq(10)
      expect(hash.get(5)).to eq(50)
    end

    it "grow part of level" do
      hash = LinearHash::Hash.new

      hash.put(1, 10) # 1: 1
      hash.put(5, 50) # 5: 101

      expect(hash.get(1)).to eq(10)
      expect(hash.get(5)).to eq(50)

      hash.put(9, 90) # 9: 1001
      expect(hash.get(9)).to eq(90)

      expect(hash.get(9)).to eq(90)
      expect(hash.get(1)).to eq(10)
      expect(hash.get(5)).to eq(50)
    end
  end

  describe "integration test" do
    it "test" do
      keys = [5, 1, 3, 20, 16, 4, 13, 17, 8, 15, 11, 12, 7, 14, 19]
      # puts "for random keys #{keys}"
      hash = LinearHash::Hash.new

      keys.each do |key|
        hash.put(key, key * 10)
        expect(hash.get(key)).to eq(key * 10)
      end

      keys.each do |key|
        expect(hash.get(key)).to eq(key * 10)
      end
    end

    it "100 random keys" do
      keys = (1..100).to_a.shuffle
      hash = LinearHash::Hash.new

      keys.each do |key|
        hash.put(key, key * 10)
        expect(hash.get(key)).to eq(key * 10)
      end

      keys.each do |key|
        expect(hash.get(key)).to eq(key * 10)
      end
    end

    it "100000 random keys" do
      keys = []
      100000.times do
        keys << rand(1000000)
      end
      keys.uniq! # do not support duplicated keys

      hash = LinearHash::Hash.new

      keys.each do |key|
        hash.put(key, key * 10)
        expect(hash.get(key)).to eq(key * 10)
      end

      keys.each do |key|
        expect(hash.get(key)).to eq(key * 10)
      end

      table = hash.instance_variable_get("@table")
      max_overflow_segnment_size = 0

      table.each do |segnment|
        overflow_segnment_size = segnment.overflow_segnment.size
        max_overflow_segnment_size = [max_overflow_segnment_size, overflow_segnment_size].max
      end

      puts "max_overflow_segnment_size is #{max_overflow_segnment_size}"
    end
  end
end
