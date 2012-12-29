require_relative "fibonacci"

module ProjectEuler
  module Problem25
    def self.call
      fng = FibonacciNumberGenerator.new
      10000.times do |i|
        fib = FibonacciNumberDigitsCounter.new(fng.next)
        if fib.number_of_digits == 1000
          printf("nth: %3d value: %100d num_digits: %3d\n", fib.nth, fib.value, fib.number_of_digits)
          break
        end
      end
    end
  end
end


ProjectEuler::Problem25.call
