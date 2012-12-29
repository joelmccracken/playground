

=begin
I think there might be an 'automatic' way to approach this problem,
but I am having trouble figuring it out. I'd like to reapproach this some day
with a better knowledge of cobinatorial logic

for now, i'll try a more brute force calculation style approach.

First, lets discusss the probelm.

    https://projecteuler.net/problem=15

Our goal is to decide



a mathematical approch
----------------------

first, intuitively, this seems like there should be an automatic way to decide
this problem.

The factorial function seems to map it rather closely.


=end

module ProjectEuler
  module Problem13
    def self.call
      lat = Lattice.new(20)
      puts lat.calculate
      puts lat.inspect
    end

    class Point
      def initialize lattice, row, column
        @lattice = lattice
        @row = row
        @column = column
      end

      def inspect
        "Point(r: #{@row}, c: #{@column}, p:#{@num_paths_to_last})"
      end

      def is_lattice_end!
        @num_paths_to_last = 0
      end

      def num_paths_to_last
        @num_paths_to_last ||= if cell_below.nil? || cell_right.nil? then
                                 1
                               else
                                 cell_below.num_paths_to_last + cell_right.num_paths_to_last
                               end
      end

      def get_distance_up
        if cell_below then
          cell_below.num_paths_to_last
        else
          0
        end
      end

      def get_distance_left
        if cell_right then
          cell_right.num_paths_to_last
        else
          0
        end
      end

      def cell_right
        @cell_right ||= @lattice.point_at(@row, @column+1)
      end

      def cell_below
        @cell_below ||= @lattice.point_at(@row+1, @column)
      end
    end

    # a lattice is the entire problem description
    #
    # org. into rows of cells.
    class Lattice
      def initialize size
        @size = size
        @points = (0..size).map do |row|
          (0..size).map do |column|
            Point.new(self, row, column)
          end
        end
        bottommost_rightmost_point.is_lattice_end!
      end

      def calculate
        # start from last cell, fill in paths "points"
        topmost_leftmost_point.num_paths_to_last
      end

      def bottommost_rightmost_point
        @points.last.last
      end

      def topmost_leftmost_point
        @points.first.first
      end

      def point_at row, column
        @points[row] && @points[row][column]
      end

      def inspect
        points_grid = @points.map do |row|
          row.inspect
        end.join(",\n")

        num_paths_grid = @points.map do |row|
          row.map do |point|
            sprintf("%2d", point.num_paths_to_last)
          end.join(" ")
        end.join "\n"
        "Lattice(s: #{@size} points: [\n" + points_grid + "\n] distances: [\n#{num_paths_grid}\n])"
      end
    end
  end
end


ProjectEuler::Problem13.call

#size = 2
#((1..size).map { :right } + (1..size).map { :down }).permutation(size*2).to_a.uniq.inspect


