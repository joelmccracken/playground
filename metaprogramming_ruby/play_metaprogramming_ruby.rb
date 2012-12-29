puts "playing with metaprogramming ruby"


# TODO for each


require "rubygems"
gem "minitest"
require "minitest/spec"
require "minitest/autorun"

require "active_support"
require "active_support/core_ext/string"

describe "chapter 1: objects" do

  describe "section 1.2" do
    it "shows how to open a class to add a method" do

      def to_alphanumeric(s)
        s.gsub /[^\w\s]/, ''
      end

      to_alphanumeric('#3, the *Magic, Number*?').must_equal '3 the Magic Number'

      # cleanup
      self.class.send(:remove_method, :to_alphanumeric)


      # open classes
      class String
        def to_alphanumeric
          gsub /[^\w\s]/, ''
        end
      end

      '#3, the *Magic, Number*?'.to_alphanumeric.must_equal '3 the Magic Number'

      # clean up
      String.send(:remove_method, :to_alphanumeric)
    end

    it "shows that the class body is executed like any other code" do

      # save original stdout
      orig_stdout = $stdout

      # set stdout to our dummy string
      $stdout = StringIO.new

      3.times do
        class C
          puts "Hello"
        end
      end

      # verify that this worked
      $stdout.string.must_equal "Hello\nHello\nHello\n"

      # return stdout
      $stdout = orig_stdout

      # undo creating a class
      Object.send(:remove_const, :C)

    end

    it "shows that classes are reopened, not recreated" do
      class D
        def x; 'x'; end
      end

      class D
        def y; 'y'; end
      end

      d = D.new


      d.x.must_equal 'x'
      d.y.must_equal 'y'

      Object.send(:remove_const, :D)
    end
  end

  describe "section 1.3" do

    it "describes the inner workings of a class" do
      class MyClass
        def my_method
          @v = 1
        end
      end

      obj = MyClass.new
      obj.class.must_equal MyClass

      obj.instance_variables.must_equal []

      #instantiate my method
      obj.my_method

      obj.instance_variables.must_equal [:@v]

      # and objects methods are available via
      obj.methods.include?(:my_method).must_equal true

      # an object's class holds a reference to its methods in instance_methods
      obj.methods.must_equal obj.class.instance_methods


      # string has reference to class, which is string
      "hello".class.must_equal String

      # string's class is a Class
      String.class.must_equal Class


      # class has the following instace methods
      Class.instance_methods(false).must_equal [:allocate, :new, :superclass]

      # strings superclass is Object
      String.superclass.must_equal Object

      Object.superclass.must_equal BasicObject

      BasicObject.superclass.must_equal nil


      Class.superclass.must_equal Module

      Module.superclass.must_equal Object
    end

    it "constants description" do
      module MyModule
        MyConstant = 'outer constant'
        class MyClass
          MyConstant = 'inner constant'
        end
      end

      MyModule::MyConstant.must_equal 'outer constant'

      MyModule::MyClass::MyConstant.must_equal 'inner constant'

      #demonstrate removing a constant out of a class / module
      MyModule::MyClass.send(:remove_const, :MyConstant)

      # clean up at the root
      Object.send(:remove_const, :MyModule)
    end

    it "module has a constants method" do
      module M
        class C
          X = 'a constant'
        end

        C::X.must_equal 'a constant'

        Y = 'another constant'

        class C
          ::M::Y.must_equal 'another constant'
        end
      end

      M::Y.must_equal 'another constant'

      M::C::X.must_equal 'a constant'

      M.constants.must_equal [:C, :Y]

      Module.constants[0..1].must_equal [:Object, :Module]

      # clean up
      Object.send(:remove_const, :M)

    end

    it "illusrates module nesting" do
      module M
        class C
          module M2
            Module.nesting.must_equal [M::C::M2, M::C, M]
          end
        end
      end
    end
  end

  describe "section 1.4" do

    it "is a quiz! here is the answer" do


      class MyClass; end
      obj1 = MyClass.new
      obj2 = MyClass.new


      # this set of assertions represent the diagram

      obj1.class.must_equal MyClass
      obj2.class.must_equal MyClass

      MyClass.superclass.must_equal Object
      MyClass.class.must_equal Class
      Class.superclass.must_equal Module



      # and, now "filling out the missing lines"
      Object.class.must_equal Class

      Module.superclass.must_equal Object

      Class.class.must_equal Class


      obj3 = MyClass.new
      obj3.instance_variable_set("@x", 10)

      obj3.class.must_equal MyClass


      # add attr_accessor for verification
      class MyClass
        attr_accessor :x
      end

      obj3.x.must_equal 10

      obj2.x.wont_equal 10

      Object.send(:remove_const, :MyClass)
    end
  end

  describe "section 1.5" do
    it "shows how method inheritance works" do
      class MyClass
        def my_method; 'my_method()'; end
      end
      class MySubclass < MyClass
      end
      obj = MySubclass.new
      obj.my_method.must_equal 'my_method()'

      # demonstrate the ancestors call
      MySubclass.ancestors[0..2].must_equal [MySubclass, MyClass, Object]


      Object.send(:remove_const, :MyClass)
      Object.send(:remove_const, :MySubclass)

    end
    it "shows a simple example of how modules work themselves into the mix" do

      module M
        def my_method
          'M#my_method()'
        end
      end
      class C
        include M
      end
      class D < C; end

      D.new.my_method.must_equal 'M#my_method()'

      D.ancestors.must_include M

      Object.send(:remove_const, :C)
      Object.send(:remove_const, :D)
    end

    it "mentions that Object includes kernel" do
      # so ancestors must have kernel inside it

      Object.ancestors.must_include Kernel
    end

    describe "a brief interlude personal exercise" do
      it "shows a method that returns the class for execution" do
        class C
        end

        def will_execute_from obj, meth
          obj.class.ancestors.select do |klass|
            # dont factor in included methods
            klass.instance_methods(false).include? meth
          end.first
        end

        will_execute_from(C.new, :doopdoop).must_equal nil
        will_execute_from(C.new, :tap).must_equal Kernel

        Object.send(:remove_const, :C)
      end
    end

    describe "execution and self" do
      it "" do


        class MyClass
          attr_accessor :val

          def testing_self
            @val = 10
            my_method
            self
          end

          def my_method
            @val = @val + 1
          end
        end

        obj = MyClass.new
        obj.testing_self
        obj.val.must_equal 11
      end

    end
  end

  describe "section 1.6: a quiz!" do
    it "has a solution?" do
      module Printable
        def print; 'printable print'; end
        def prepare_cover; 'printable prepare_cover'; end
      end

      module Document
        def print_to_screen
          prepare_cover
          format_for_screen
          print
        end

        def format_for_screen; 'document format_for_screen'; end
        def print; 'document print'; end
      end

      class Book
        include Document
        include Printable
      end


      b = Book.new
      b.print_to_screen
      #at this point, this is wrong.
      b.print.must_equal 'printable print'
      Object.send(:remove_const, :Book)

      # to fix, we reorder the includes
      class Book
        include Printable
        include Document
      end
      Book.new.print.must_equal 'document print'
    end
  end


  describe "chapter 2: tuesday: methods" do
    describe "2.1 a duplication problem" do
      it "shows a bunch of nasty duplicated code" do
        # basically ignoring this
      end
    end
  end
end




describe "metaprogramming ruby techniques" do
  describe "hacking on the object model" do
    describe "preliminaries (things I use later)" do
      # misc things that
      it "is possible to remove a class from the system" do
        class MyClass; end
        c = MyClass.new
        Object.send(:remove_const, :MyClass)
        proc do
          c = MyClass.new
        end.must_raise NameError
      end

      it "is possible to remove a method from a class" do
        class MyClass
          def my_method
            'my_method()'
          end
        end
        c = MyClass.new
        c.my_method.must_equal 'my_method()'

        # and here we do the business of remoing the method
        MyClass.send(:remove_method, :my_method)

        proc do
          c.my_method
        end.must_raise NoMethodError

        Object.send(:remove_const, :MyClass)
      end
    end

    it "is possible to add a method to an existing class" do
      class MyClass
      end
      c = MyClass.new

      # show that method does not exist
      proc do
        c.my_method
      end.must_raise NoMethodError

      # "reopening" the class
      class MyClass
        def my_method; 'hi!'; end
      end

      # show that
      c.my_method.must_equal 'hi!'

      Object.send(:remove_const, :MyClass)
    end
  end

  describe "ways to define message receipt on an object" do
    describe "first off, this needs to be nomenclature clarification" do
      it "shows that invoking a method is just sending a message in disguise" do
        class MyClass
          def doot; 10; end;
        end
        c = MyClass.new

        # invoke a method
        c.doot.must_equal(10)

        # send a message
        c.send(:doot).must_equal 10

        # can use string to send a message
        c.send("doot").must_equal 10

        Object.send(:remove_const, :MyClass)
      end
    end
    it "is possible to define methods dynamically with define_method" do
      class MyClass
        [:a, :b, :c].each do |letter|
          define_method letter do
            letter
          end
        end
      end

      m = MyClass.new
      m.a.must_equal :a
      m.b.must_equal :b
      m.c.must_equal :c

      Object.send(:remove_const, :MyClass)
    end
  end
end


describe "ruby classes" do
  describe "definitions are just regular code" do

    it "evaluates regular code" do
      $the_secret = nil

      class ClassDefSideEffects
        $the_secret = "shh"
      end

      $the_secret.must_equal "shh"
    end

    it "return what was last evaluated" do
      (class Doot
         1 + 1
       end).must_equal 2
    end
  end


  describe "classes are objects" do
    it "has methods (such as class) and reports that it has the type class" do
      class Dootz; end
      Dootz.class.must_equal Class
    end
  end

  describe "dynamically creating classes" do
    it "can create new classes" do
      x = Class.new
      y = x.new
      z = x.new

      y.class.must_equal z.class

      y.class.must_equal x
    end

    it "is just objects, so can be passed around" do
      class Dootz; end

      x = Dootz.class.new.new

      x.class.class.must_equal Class
    end
  end
end


describe "ruby method creation" do
  describe "creating them" do
    it "supports 'dynamic' method creation via the private method define_method" do
      class Microphone
        define_method :doot do
          "dootdoot"
        end
      end
      Microphone.new.doot.must_equal "dootdoot"
    end
  end

  describe "calling methods dynamically" do
    it "should support looking up the method name via string" do
      # use the method "to_s" on array as an example method

      [1, 2, 3].to_s.must_equal "[1, 2, 3]"


      # it can be called via :send
      [1, 2, 3].send(:to_s).must_equal "[1, 2, 3]"

      # send also accepts a strin
      [1, 2, 3].send("to_s").must_equal "[1, 2, 3]"


      # a more complicated example
      %w{to_a to_s inspect}.each do |method|
        [1, 2, 3].send(method)
      end
    end

    it "allows you to call private methods (ooo!)" do
      class Test
        private
        def a_private_method
          "olala a secret"
        end
      end

      proc do
        Test.new.a_private_method
      end.must_raise NoMethodError

      # however, we can get through it here.
      Test.new.send(:a_private_method).must_equal "olala a secret"
    end
  end

  describe "dynamically creating methods" do
    it "allows an external caller to e.g. create methods on a class" do
      Test.send(:define_method, :funky) {}
    end
  end
end


describe "method_missing" do
  it "intercepts method calls" do
    class MethodRecorder
      attr_reader :calls
      def initialize
        @calls = []
      end
      def method_missing *args
        @calls << args
      end
    end

    x = MethodRecorder.new
    x.asdf :qwerty, "lol"

    x.calls.first.must_equal [:asdf, :qwerty, "lol"]
  end
end



class MyOpenStruct
  def initialize(vals={})
    @vals = vals
  end

  def method_missing method, value=nil
    method = method.to_s
    if method =~ /=$/
      @vals[method.chop.to_sym] = value
    else
      @vals[method.to_sym]
    end
  end
end

describe "my own openstruct" do
  # as an exercise, inspried by metaprogramming ruby
  it "takes a list of options" do
    MyOpenStruct.new :key => :val
  end


  it "provides access to those options" do
    x = MyOpenStruct.new :doot => 10
    x.doot.must_equal 10
  end

  it "gives accessors to the options" do
    x = MyOpenStruct.new :doot => 10
    x.doot.must_equal 10
    x.doot = 20
    x.doot.must_equal 20
  end
end



describe "removing methods" do
  it "clearing up my confusion" do
    class A
      def testing
        "testing 1 2 3"
      end
    end

    class B < A
      def scoot
        "scoot scoot"
      end
    end
    class C < A; end

    b = B.new
    c = C.new

    # okay, both can access method A#a
    b.testing.must_equal 'testing 1 2 3'
    c.testing.must_equal 'testing 1 2 3'



    # calling remove method when method not actually on class is wrong
    proc do
      class B
        remove_method :testing
      end
    end.must_raise NameError

    b.testing.must_equal 'testing 1 2 3'

    # remove method works on b's own method, though

    b.scoot.must_equal 'scoot scoot'
    class B
      remove_method :scoot
    end
    proc { b.scoot }.must_raise NoMethodError


    # undef_method can be used for ancestors

    class B
      undef_method :testing
    end
    proc { b.testing }.must_raise NoMethodError

    # however, it still works on c..

    c.testing.must_equal 'testing 1 2 3'

  end
end



describe "difference in return between procs and lambdas..." do

  describe "lambdas" do
    it "returns as normal things do..." do
      def a_method callable
        callable.call
        :asdf
      end

      a_method(->{return :qwerty}).must_equal :asdf
    end

    it "errors on the wrong number of arguments" do
      proc do
        one_arg = ->(x){ 10 }
        one_arg.call
      end.must_raise ArgumentError
    end
  end

  describe "procs" do
    it "returns from where it was defined (what?)" do
      def do_it
        def a_method callable
          callable.call
          :asdf
        end
        a_method(Proc.new { return :qwerty })
      end
      do_it.must_equal :qwerty
    end

    it "allows the correct number of arguments" do
      identity = proc{ |x| x }
      identity.call(:unique).must_equal :unique
    end
  end
end


describe "method objects" do
  it "wraps the method as callable" do
    class MyClass
      def initialize arg
        @arg = arg
      end
      def get_arg
        @arg
      end
    end
    MyClass.new(:hi).method(:get_arg).call.must_equal :hi
  end
end


def a_method_on_main
  "within a method on main!"
end
the_outer_self = self

describe "the top-level context" do
  it "has a class" do
    the_outer_self.class.must_equal Object
  end
  it "has methods that are private" do
    the_outer_self.private_methods.must_include :a_method_on_main
  end
end

describe "instance eval" do
  it "will add a method onto the current object" do
    class MyClass; end
    inst = MyClass.new
    inst.instance_eval do
      def face
        :lol
      end
    end
    inst.face.must_equal :lol
    inst.methods.must_include :face
  end

  it "does not allow adding methods to the 'class' itself" do
    class MyClass; end
    inst = MyClass.new
    inst.instance_eval do
      def face
        :lol
      end
    end
    inst.face.must_equal :lol
    MyClass.new.methods.include?(:face).must_equal false
    Object.send(:remove_const, :MyClass)
  end
end

describe "class eval" do
  it "lets you add methods to classes" do
    MyClass.class_eval do
      def lol
        :yo
      end
    end

    MyClass.new.lol.must_equal :yo
    Object.send(:remove_const, :MyClass)
  end
end


describe "creating a class without class" do
  it do
    MyClass = Class.new(Array) do
      def my_method
        "Hello!"
      end
    end
    Object.send(:remove_const, :MyClass)
  end
end


describe "the friday quizzes" do
  describe "part 1: a really simple method" do
    before do
      module Checked
        def self.included klass
          klass.extend ClassMethods
        end

        module ClassMethods
          def checked_attribute attr, &validator
            define_method attr do
              @checked_attributes ||= {}
              @checked_attributes[attr]
            end
            define_method "#{attr}=" do |val|
              @checked_attributes ||= {}
              if !validator.call(val)
                raise "Invalid attribute"
              else
                @checked_attributes[attr] = val
              end
            end
          end
        end
      end

      class Person
        include Checked
        checked_attribute(:name) { |name| !name.blank? }
      end
      @me = Person.new
    end

    after do
      Object.send(:remove_const, :Person)
    end

    it "provides an accessor that accepts a valid name" do
      @me.name = "sam"
      @me.name.must_equal "sam"
    end

    it "raises errors if assigned false or nil" do
      ->{
        @me.name = nil
      }.must_raise RuntimeError, "Invalid attribute"
    end

    it "raises an error if assigned a string which would fail the test" do

      ->{
        @me.name = "  "
      }.must_raise RuntimeError, "Invalid attribute"

    end
  end
end


