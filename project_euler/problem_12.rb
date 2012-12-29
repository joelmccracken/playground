require_relative "lib/sieve"
require_relative "lib/factor"

module ProjectEuler
  module Problem12
    class TriangleNumberGenerator
      def initialize(factors_calc)
        @at = 0
        @total = 0
        @factors_calc = factors_calc
      end
      def next
        @at += 1
        @total += @at
        facs = @factors_calc.all_factors_for(@total)
        TriangleNumberWithFactors.new @at, @total, nil, facs.length
      end
    end

    TriangleNumberWithFactors = Struct.new(:nth, :num, :factors, :factors_length)

    def self.call
      fact_calc = Factor.new(prime_source: Sieve.new)
      tn = TriangleNumberGenerator.new(fact_calc)

      largest_tn_factors = nil
      found = nil
      30000.times do |i|
        it = tn.next

        if it.factors_length > 500
          found = it
          puts "HERE #{it.inspect}"
          exit
          break
        end
        if !largest_tn_factors || it.factors_length > largest_tn_factors.factors_length
          largest_tn_factors = it
        end
      end
      puts "largest: #{largest_tn_factors.inspect}, #{found.inspect}"
    end
  end
end


ProjectEuler::Problem12.call()
