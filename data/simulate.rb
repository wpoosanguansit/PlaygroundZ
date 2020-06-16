#!/usr/bin/env ruby

# ----------------------------------------------------------------------
# ruby script to run the agent
#
# To run the script go to the directory where the script is in the
# shell and issue the command:
#
# ./simulate.rb <path-to>/solution.jar <path-to>/grids.data
# ----------------------------------------------------------------------

require 'open3'

# quit unless our script gets two command line arguments
unless ARGV.length == 2
  puts "Two arguments are needed <path/to/jar/file> <path/to/data/file> /n"
  puts "Usage: ../simulate.rb <path-to>/solution.jar <path-to>/grids.data /n"
  exit
end


iteration 	= 0

jar 		= ARGV[0]

# our output file should be the second command line arg
data 		= ARGV[1]

cmd 		= "java -jar #{jar}"

arr 		= []
array 		= File.readlines(data).map do |line|
	line.chomp.each_char do |char|
		arr.push char
	end
end

y 			= array.length
x			= array[0].length

puts "We start the game at: \n\n"

puts "y is : " << y.to_s
puts "x is : " << x.to_s

puts "\n\n"

r 			= Random.new

startX 		= r.rand(1..x-1)
startY 		= r.rand(1..y-1)

until (array[startY - 1][startX - 1] != '#')
	startX 	= r.rand(1..x-1) 
	startY 	= r.rand(1..y-1)
end

puts "startX is : " << startX.to_s
puts "startY is : " << startY.to_s

Point 		= Struct.new(:x, :y)

current 	= Point.new(startX - 1, startY - 1)

####################################################
# return string of current top bottom left right
####################################################
def find_new_input_string (point, array)
	x 		= point.x
	y 		= point.y
	str 	= array[y][x] << array[y - 1][x] << array[y + 1][x] << array[y][x - 1] << array[y][x + 1]
end

begin
	stdin, stdout, stderr = Open3.popen3 cmd
	done 		= 0
	line        = ""
	until (done == 1)
		begin
			iteration 	= iteration + 1
			input 		= find_new_input_string(current, array)
			puts "string input is : |" << input << "|"
			stdin.print input
			stdin.print "\n"
			line 		= stdout.gets.chomp
			puts "current position is: " << current.to_s
			puts "output: #{line}"
			if line.include?("The agent seems to have completed the grid")
				done 	= 1
			elsif line.eql?("U")
				current = Point.new(current.x, current.y - 1)
			elsif line.eql?("D")
				current = Point.new(current.x, current.y + 1)
			elsif line.eql?("L")
				current = Point.new(current.x - 1, current.y)
			elsif line.eql?("R")
				current = Point.new(current.x + 1, current.y)
			end
			puts "next position is: " 	<< current.to_s
			puts "iteration is: " 		<< iteration.to_s
		rescue => err
			p err
		end
	end
end