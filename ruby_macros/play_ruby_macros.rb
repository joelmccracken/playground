# NOTE this project has morpheddd and no longer is really updatedd!!!!

require 'melbourne'
require 'debugger'
require 'ostruct'
require 'ripper'
require 'sorcerer'
require 'ruby_parser'
require 'ripper'

class Melbourne::AST::Node
  def find_macros; nil; end
end

class Melbourne::AST::Block
  def find_macros
    array.map(&:find_macros).reject &:nil?
  end
end

class Melbourne::AST::SendWithArguments
  def find_macros
    if @name == :defmacro
      Macro.new(@arguments.array[0].value)
    end
  end
end

Macro = Struct.new(:name) do
end





class MacroProcessor
  def initialize src
    @src = src
  end

  def macros
    @macros ||= find_macros(ast)
  end

  def find_macros ast
    ast.find_macros
  end

  def ast
    @src.to_ast
  end
end

describe MacroProcessor do
  it "can find defmacro calls" do
    code_with_defmacro = <<-EOF
x = 10
defmacro :lol do |block_ast, env|
end

lol { here }

EOF
    mp = MacroProcessor.new(code_with_defmacro)
    mp.macros.length.must_equal 1
    mp.macros[0].name.must_equal :lol
  end
end

class Melbourne::AST::SendWithArguments
  def to_print
    "#{@receiver.to_print}.#{@name}(#{@arguments.to_print})"
  end
end

class Melbourne::AST::ActualArguments
  def to_print
    array.map(&:to_print).join ","
  end
end

class Melbourne::AST::FixnumLiteral
  def to_print
    @value
  end
end

class Melbourne::AST::Class
  def to_print
    "class #{@name.to_print}; end"
  end
end

class Melbourne::AST::ClassName
  def to_print
    @name
  end
end




describe "printers" do
  it do
    '1+1'.to_ast.to_print.must_equal '1.+(1)'
    '3+4'.to_ast.to_print.must_equal '3.+(4)'

    class_code = <<-CODE
    class Doot
    end
    CODE
    class_code.to_ast.to_print.must_equal 'class Doot; end'
    class_code.to_ast.to_print.must_equal 'class Doot; end'

  end
end
