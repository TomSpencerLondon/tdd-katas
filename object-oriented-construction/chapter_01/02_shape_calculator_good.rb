# Exercise 2B: GOOD Example - Highly Extendible
#
# This demonstrates Meyer's principles for extendibility:
# 1. DESIGN SIMPLICITY: Each shape is a separate, simple class
# 2. DECENTRALIZATION: Shapes are autonomous modules
#
# Benefits:
# - Adding a new shape requires NO changes to existing code
# - Each shape is responsible for its own calculations
# - The system remains simple even as it grows
# - Changes to one shape don't affect others (no chain reaction)

# Base class defines the interface (contract)
class Shape
  def area
    raise NotImplementedError, "Subclasses must implement area()"
  end

  def perimeter
    raise NotImplementedError, "Subclasses must implement perimeter()"
  end

  def describe
    raise NotImplementedError, "Subclasses must implement describe()"
  end
end

# Each shape is a DECENTRALIZED, AUTONOMOUS module
class Circle < Shape
  def initialize(radius)
    @radius = radius
  end

  def area
    Math::PI * @radius * @radius
  end

  def perimeter
    2 * Math::PI * @radius
  end

  def describe
    "Circle with radius #{@radius}"
  end
end

class Rectangle < Shape
  def initialize(width, height)
    @width = width
    @height = height
  end

  def area
    @width * @height
  end

  def perimeter
    2 * (@width + @height)
  end

  def describe
    "Rectangle #{@width}x#{@height}"
  end
end

class Triangle < Shape
  def initialize(base, height, side1, side2, side3)
    @base = base
    @height = height
    @side1 = side1
    @side2 = side2
    @side3 = side3
  end

  def area
    0.5 * @base * @height
  end

  def perimeter
    @side1 + @side2 + @side3
  end

  def describe
    "Triangle with base #{@base}, height #{@height}"
  end
end

# NOW WATCH: We can EXTEND the system by adding a new shape
# WITHOUT modifying ANY existing code!
class Pentagon < Shape
  def initialize(side_length)
    @side_length = side_length
  end

  def area
    # Using formula for regular pentagon: (5 * s^2) / (4 * tan(π/5))
    (5 * @side_length ** 2) / (4 * Math.tan(Math::PI / 5))
  end

  def perimeter
    5 * @side_length
  end

  def describe
    "Regular Pentagon with side length #{@side_length}"
  end
end

# And another one! Still no changes to existing code!
class Square < Shape
  def initialize(side)
    @side = side
  end

  def area
    @side * @side
  end

  def perimeter
    4 * @side
  end

  def describe
    "Square with side #{@side}"
  end
end

# Generic calculator that works with ANY shape (even ones not yet invented!)
class ShapeCalculator
  def report(shape)
    puts shape.describe
    puts "  Area: #{shape.area.round(2)}"
    puts "  Perimeter: #{shape.perimeter.round(2)}"
    puts
  end

  def total_area(shapes)
    shapes.sum(&:area)
  end
end

puts "=" * 60
puts "GOOD EXAMPLE - Highly Extendible (Decentralized Design)"
puts "=" * 60

calculator = ShapeCalculator.new

puts "\nOriginal shapes:"
circle = Circle.new(5)
rectangle = Rectangle.new(4, 6)
triangle = Triangle.new(6, 4, 5, 5, 6)

calculator.report(circle)
calculator.report(rectangle)
calculator.report(triangle)

puts "=" * 60
puts "EXTENDING: Adding new shapes (Pentagon, Square)"
puts "Changes required to existing code: ZERO!"
puts "=" * 60
puts

pentagon = Pentagon.new(3)
square = Square.new(4)

calculator.report(pentagon)
calculator.report(square)

puts "=" * 60
puts "The calculator works with ALL shapes without modification:"
puts "=" * 60

all_shapes = [circle, rectangle, triangle, pentagon, square]
total = calculator.total_area(all_shapes)
puts "Total area of all shapes: #{total.round(2)}"

puts "\n" + "=" * 60
puts "KEY INSIGHTS FROM CHAPTER 1:"
puts "=" * 60
puts "DESIGN SIMPLICITY: Each shape is a simple, focused class"
puts "DECENTRALIZATION: Shapes are autonomous - changes to one don't affect others"
puts "EXTENDIBILITY: New shapes can be added without modifying existing code"
puts ""
puts "Quote from Meyer: 'The object-oriented method is, before anything"
puts "else, a system architecture method which helps designers produce"
puts "systems whose structure remains both simple (even for large"
puts "systems) and decentralized.'"
puts "=" * 60
