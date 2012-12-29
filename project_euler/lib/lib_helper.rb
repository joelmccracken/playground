require "rubygems"
gem 'minitest'
require 'minitest/spec'

require 'delegate'
require 'ostruct'



module MethodObject
  def self.included(klass)
    klass.extend ClassMethods
  end

  module ClassMethods
    def method_objects
      @method_objects
    end

    def method_object(name, &definition)
      @method_objects ||= {}
      @method_objects[name] = mo = Struct.new(:other, &definition)

      define_method name do |*args|
        mo.new(self).call(*args)
      end
    end
  end
end


