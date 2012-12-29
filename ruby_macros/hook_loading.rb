module Kernel
  alias :original_load :load
  def load *args
    puts "IN LOAD FUNCTION"
    original_load *args
  end

  alias :original_eval :eval
  def eval *args
    puts 'hie'
    original_eval *args
  end
end
