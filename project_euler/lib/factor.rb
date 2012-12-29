require_relative 'lib'
require 'ostruct'

module ProjectEuler
  class Factor
    def initialize opts
      @prime_source = opts[:prime_source]
    end

    def prime_factors_for n
      max_prime_required = Math.sqrt(n).floor
      primes = @prime_source.primes_up_to max_prime_required

      primes = primes.select do |p|
        n % p == 0
      end
    end

    def all_factors_for n
      prime_factors = prime_factors_for n

      factors = prime_factors.clone
      factors_stack = [n]

      while num = factors_stack.pop
        new_factors = prime_factors.map do |p|
          new_fac = num / p
          if num % p == 0 and !factors.member?(new_fac)
            new_fac
          end
        end.reject &:nil?
        factors += new_factors
        factors_stack += new_factors
        factors_stack.uniq!
      end
      factors.sort
    end
  end
end



describe "factor" do
  it do
    primes = [1, 2, 3, 5, 7]
    class << primes
      def primes_up_to n; self; end
    end

    ProjectEuler::Factor.new(prime_source: primes).prime_factors_for(100).must_equal [1, 2, 5]
    ProjectEuler::Factor.new(prime_source: primes).all_factors_for(100).must_equal [1, 2, 4, 5, 10, 20, 25, 50, 100]

  end
end

require "minitest/autorun" if __FILE__ == $0
