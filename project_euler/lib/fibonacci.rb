require_relative "lib"


module ProjectEuler

  FibonacciNumber = Struct.new(:nth, :value) do
    def inspect
      "Fib(#{nth})=#{value}"
    end
  end


  class FibonacciNumberDigitsCounter < SimpleDelegator
    def number_of_digits
      value.to_s.length
    end
  end


  class FibonacciNumberGenerator
    def initialize
      @at = 0
      @fibs = []
    end
    def next
      @at += 1
      @fibs[@at] = FibonacciNumber.new(@at, fib_value_for(@at))
      @fibs.last
    end

    private
    def fib_value_for n
      if n == 1 || n == 2
        1
      else
        if @fibs[n] then
          @fibs[n].value
        else
          fib_value_for(n-1) + fib_value_for(n-2)
        end
      end
    end
  end

end


describe "fibonacci number" do
  let(:fib) { ->(nth, val){ ProjectEuler::FibonacciNumber.new(nth, val) }}
  it "face" do
    fib.call(1, 1).inspect.must_equal "Fib(1)=1"
  end
  it do
    fng = ProjectEuler::FibonacciNumberGenerator.new
    fng.next.must_equal fib.call(1,1)
    fng.next.must_equal fib.call(2,1)
    fng.next.must_equal fib.call(3,2)
    fng.next.must_equal fib.call(4,3)
    fng.next.must_equal fib.call(5,5)
    fng.next.must_equal fib.call(6,8)
    fng.next.must_equal fib.call(7,13)
  end
end

require "minitest/autorun" if __FILE__ == $0
