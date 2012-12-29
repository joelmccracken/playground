require_relative "lib/maximum_sum_path_triangle"

module ProjectEuler
  module Problem64
    def self.call


      triangle_file_name = File.join(File.dirname(__FILE__), "data/triangle.txt")
      triangle_file = File.open(triangle_file_name, "r")

      triangle = MaximumPathSumTriangle::Triangle.parse_from triangle_file.read


      puts MaximumPathSumTriangle::TriangleMaxPathFinder.new.call(triangle).inspect

    end
  end
end


ProjectEuler::Problem64.call
