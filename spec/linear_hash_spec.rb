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

  describe "handle grow manualy" do
    it "grow once" do
      hash = LinearHash::Hash.new

      hash.put(1, 10)
      hash.put(5, 50)

      expect(hash.get(1)).to eq(10)
      expect(hash.get(5)).to eq(50)

      hash.grow
      # hash.grow # grow should be idempotent

      expect(hash.get(1)).to eq(10)
      expect(hash.get(5)).to eq(50)
    end

    it "grow part of level" do
      hash = LinearHash::Hash.new

      hash.put(1, 10) # 1: 1
      hash.put(5, 50) # 5: 101

      expect(hash.get(1)).to eq(10)
      expect(hash.get(5)).to eq(50)

      hash.grow

      hash.put(9, 90) # 9: 1001
      expect(hash.get(9)).to eq(90)

      hash.grow

      expect(hash.get(9)).to eq(90)
      expect(hash.get(1)).to eq(10)
      expect(hash.get(5)).to eq(50)
    end
  end
end
