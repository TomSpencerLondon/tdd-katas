# Exercise 2A: BAD Example - Not Extendible
#
# This demonstrates what Meyer calls the "giant house of cards" - where
# "pulling out any one element might cause the whole edifice to collapse"
#
# Problems:
# 1. Adding a new shape requires modifying EXISTING code in multiple places
# 2. All logic is centralized in one monolithic class
# 3. Uses type codes (strings) instead of proper types
# 4. Violates the "design simplicity" and "decentralization" principles

class BadShapeCalculator
  def calculate_area(shape_type, *params)
    case shape_type
    when "circle"
      radius = params[0]
      Math::PI * radius * radius
    when "rectangle"
      width = params[0]
      height = params[1]
      width * height
    when "triangle"
      base = params[0]
      height = params[1]
      0.5 * base * height
    else
      raise "Unknown shape type: #{shape_type}"
    end
  end

  def calculate_perimeter(shape_type, *params)
    case shape_type
    when "circle"
      radius = params[0]
      2 * Math::PI * radius
    when "rectangle"
      width = params[0]
      height = params[1]
      2 * (width + height)
    when "triangle"
      side1 = params[0]
      side2 = params[1]
      side3 = params[2]
      side1 + side2 + side3
    else
      raise "Unknown shape type: #{shape_type}"
    end
  end

  def describe_shape(shape_type, *params)
    case shape_type
    when "circle"
      "Circle with radius #{params[0]}"
    when "rectangle"
      "Rectangle #{params[0]}x#{params[1]}"
    when "triangle"
      "Triangle with base #{params[0]}, height #{params[1]}"
    else
      "Unknown shape"
    end
  end

  # Imagine we now need to add a SQUARE shape...
  # We would need to modify ALL three methods above!
  # This is a maintenance nightmare.
end

puts "=" * 60
puts "BAD EXAMPLE - Not Extendible (Centralized Design)"
puts "=" * 60

calculator = BadShapeCalculator.new

puts "\nCurrent shapes work fine:"
puts "Circle area: #{calculator.calculate_area('circle', 5)}"
puts "Rectangle area: #{calculator.calculate_area('rectangle', 4, 6)}"

puts "\n" + "=" * 60
puts "PROBLEM: To add a new shape (e.g., Pentagon), we must:"
puts "1. Modify calculate_area() - add another 'when' clause"
puts "2. Modify calculate_perimeter() - add another 'when' clause"
puts "3. Modify describe_shape() - add another 'when' clause"
puts "4. Risk breaking existing functionality with each change"
puts ""
puts "This violates Meyer's principles:"
puts "- NOT SIMPLE: All logic in one place creates complexity"
puts "- NOT DECENTRALIZED: Every shape change affects the whole system"
puts "=" * 60
