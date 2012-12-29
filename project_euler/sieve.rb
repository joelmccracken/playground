require_relative 'lib'

module ProjectEuler
  class Sieve
    def initialize
      @primes = [2] # we leave 1 out because very number is divisible
      # by it
      @found_up_to = 2
    end

    def primes_up_to max
      find_primes_up_to max
      return_primes_up_to max
    end

    def find_primes_up_to max
      unless @found_up_to >= max
        possible = PossiblePrimeList.new(@found_up_to..max)

        @primes.each do |prime|
          possible.remove_multiples_of prime
        end

        while next_prime = possible.pop
          @primes << next_prime
          possible.remove_multiples_of next_prime
        end
        @found_up_to = max
      end
    end

    def return_primes_up_to max
      [1] + @primes.select { |i| i < max  }
    end

    class PossiblePrimeList
      def initialize(list)
        @list = list
      end
      def remove_multiples_of n
        @list = @list.reject do |i|
          i % n == 0
        end
      end

      def empty?
        @list.empty?
      end

      def list
        @list
      end

      def pop
        @list.shift
      end
    end
  end
end



describe "possible prime list handler" do
  let(:ppl) { ->(list){ ProjectEuler::Sieve::PossiblePrimeList.new(list) } }
  it do
    list = ppl.call((1..10))
    list.remove_multiples_of 2
    list.list.must_equal [1, 3, 5, 7, 9]

    list.pop.must_equal 1
    list.list.must_equal [3, 5, 7, 9]
  end
end

describe "sieve" do
  let(:sieve) { ProjectEuler::Sieve.new }
  it do
    sieve.primes_up_to(10).must_equal [1, 2, 3, 5, 7]
    sieve.primes_up_to(20).must_equal [1, 2, 3, 5, 7, 11, 13, 17, 19]
    sieve.primes_up_to(10).must_equal [1, 2, 3, 5, 7]
  end
end

require "minitest/autorun" if __FILE__ == $0
