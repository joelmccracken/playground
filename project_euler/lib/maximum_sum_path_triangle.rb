require_relative "lib_helper"


#    3
#   7 4
#  2 4 6
# 8 5 9 3

module ProjectEuler
  module MaximumPathSumTriangle
    class Triangle
      def self.parse_from str
        t = self.new
        t.parse str
        t
      end

      def parse triangle_repr
        # how much do I love ruby? this much:
        rows = triangle_repr.split("\n").map do |row|
          row.split.map(&:to_i)
        end.reject do |line|
          line.length == 0
        end

        @rows = rows.each_with_index.map do |row, row_i|
          row.each_with_index.map do |num, num_j|
            NumberInTriangle.new(num, row_i, num_j)
          end
        end
      end

      def rows
        @rows
      end

      def height
        @rows.length
      end

      def top
        @rows[0][0]
      end

      def next_steps_for number_in_triangle
        next_row = number_in_triangle.row + 1
        next_row_item_positions = [ number_in_triangle.position, number_in_triangle.position + 1 ]
        next_row_items = next_row_item_positions.map do |pos|
          @rows[next_row][pos] if @rows[next_row]
        end
        next_row_items.compact
      end
    end

    NumberInTriangle = Struct.new(:value, :row, :position)

    class Path
      attr_accessor :value
      attr_accessor :steps
      def initialize
        @steps = []
        @value = 0
      end

      def add_step step
        @steps << step
        @value += step.value
      end
      def last_step
        @steps.last
      end
      def <=> other
        self.value.<=>(other.value)
      end

      def inspect
        "Path val: #{@value} steps: #{@steps.map(&:value).join ','}"
      end
      def initialize_copy other
        super
        @steps = other.steps.clone
      end
    end


    class TriangleMaxPathFinder
      attr_accessor :triangle
      attr_accessor :current_paths

      include MethodObject

      def call triangle
        @triangle = triangle

        head = @triangle.top

        @current_paths = [Path.new]
        @current_paths[0].add_step(head)

        while can_any_be_longer?(@current_paths)
          new_paths = @current_paths.flat_map do |path|
            next_paths_for_path path
          end
          @current_paths = remove_duplicate_paths_selecting_better new_paths
        end
        @current_paths.sort.last
      end

      method_object :remove_duplicate_paths_selecting_better do
        def call(paths)
          best_paths_ending_in(unique_last_steps(paths), paths)
        end

        def unique_last_steps paths
          paths.map do |path|
            path.last_step
          end.uniq
        end

        def best_paths_ending_in end_points, paths
          end_points.map do |end_point|
            paths.select do |path|
              path.last_step == end_point
            end.sort.last
          end
        end
      end

      method_object :can_any_be_longer? do
        def call(paths)
          paths.select do |path|
            has_another_path other.triangle, path
          end.compact.first
        end
        def has_another_path(triangle, path)
          not triangle.next_steps_for(path.last_step).empty?
        end
      end

      method_object :next_paths_for_path do
        def call(path)
          a, b = other.triangle.next_steps_for(path.last_step)

          path_a = path
          path_b = path.dup

          path_a.add_step a
          path_b.add_step b

          [path_a, path_b]
        end
      end
    end
  end
end

describe "max sum triangle" do
  let(:triangle) { ProjectEuler::MaximumPathSumTriangle::Triangle.new }


  describe "parsing/construction" do
    it do
      triangle.parse %q{
    3
   7 4
    }
      triangle.rows[0].map(&:value).must_equal [3]
      triangle.rows[1].map(&:value).must_equal [7, 4]
    end

    it do
      triangle.parse %q{
    3
   7 4
    }
      triangle.height.must_equal 2
    end
  end
end

describe "num from triangle" do
  let(:tri_num) { proc { |*args| ProjectEuler::MaximumPathSumTriangle::NumberInTriangle.new *args } }
  describe "equality" do
    it do
      tri_num.call(1,2,3).must_equal tri_num.call(1,2,3)
      tri_num.call(1,2,3).wont_equal tri_num.call(1,2,4)
    end
  end
end


describe "remove duplicate paths selectin better" do
  it do
    fake_path = Struct.new(:last_step, :value) do
      def <=> other
        self.value.<=>(other.value)
      end
    end

    paths = [fake_path.new(:a, 1), fake_path.new(:b, 2), fake_path.new(:b, 3), fake_path.new(:c, 4)]
    unique_better_paths = ProjectEuler::MaximumPathSumTriangle::RemoveDuplicatePathsSelectingBetter.new(paths).call

    unique_better_paths.
      must_equal [fake_path.new(:a, 1), fake_path.new(:b, 3), fake_path.new(:c, 4)]
  end
end


describe "triangle max path finder -- some paths can get longer method object " do
  it do
    sut = ProjectEuler::MaximumPathSumTriangle::TriangleMaxPathFinder.method_objects[:some_paths_can_get_longer]

    triangle_max_path_finder = OpenStruct.new
    class << triangle_max_path_finder
      def doot
        10
      end
    end

    triangle_max_path_finder.current_paths = [1,2]
    triangle_max_path_finder.doot.must_equal 10

    sut.new(triangle_max_path_finder).call.must_equal true
  end
end



require 'minitest/autorun' if __FILE__ == $0
