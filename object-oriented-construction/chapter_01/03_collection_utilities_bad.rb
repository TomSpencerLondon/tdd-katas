# Exercise 3A: BAD Example - Not Reusable
#
# This demonstrates code that is NOT reusable because:
# 1. Specific to one data type (hardcoded for integers)
# 2. Duplicated logic across different use cases
# 3. Cannot be generalized to other applications
# 4. Solving the same problem repeatedly in different places
#
# Meyer's point: "It should be possible to exploit commonality and
# avoid reinventing solutions to problems that have been encountered before"

# Application 1: Student grade analysis
class StudentGradeAnalyzer
  def average_grade(grades)
    return 0 if grades.empty?
    sum = 0
    grades.each { |g| sum += g }
    sum.to_f / grades.length
  end

  def max_grade(grades)
    return nil if grades.empty?
    max = grades[0]
    grades.each { |g| max = g if g > max }
    max
  end

  def min_grade(grades)
    return nil if grades.empty?
    min = grades[0]
    grades.each { |g| min = g if g < min }
    min
  end
end

# Application 2: Sales analysis (DUPLICATE CODE!)
class SalesAnalyzer
  def average_sales(amounts)
    return 0 if amounts.empty?
    sum = 0
    amounts.each { |a| sum += a }
    sum.to_f / amounts.length
  end

  def max_sales(amounts)
    return nil if amounts.empty?
    max = amounts[0]
    amounts.each { |a| max = a if a > max }
    max
  end

  def min_sales(amounts)
    return nil if amounts.empty?
    min = amounts[0]
    amounts.each { |a| min = a if a < min }
    min
  end
end

# Application 3: Temperature analysis (MORE DUPLICATE CODE!)
class TemperatureAnalyzer
  def average_temp(temperatures)
    return 0 if temperatures.empty?
    sum = 0
    temperatures.each { |t| sum += t }
    sum.to_f / temperatures.length
  end

  def max_temp(temperatures)
    return nil if temperatures.empty?
    max = temperatures[0]
    temperatures.each { |t| max = t if t > max }
    max
  end

  def min_temp(temperatures)
    return nil if temperatures.empty?
    min = temperatures[0]
    temperatures.each { |t| min = t if t < min }
    min
  end
end

puts "=" * 60
puts "BAD EXAMPLE - Not Reusable (Duplicated Code)"
puts "=" * 60

grade_analyzer = StudentGradeAnalyzer.new
grades = [85, 92, 78, 95, 88]
puts "\nStudent Grades Analysis:"
puts "Average: #{grade_analyzer.average_grade(grades)}"
puts "Max: #{grade_analyzer.max_grade(grades)}"
puts "Min: #{grade_analyzer.min_grade(grades)}"

sales_analyzer = SalesAnalyzer.new
sales = [1500, 2300, 1800, 2100, 1900]
puts "\nSales Analysis:"
puts "Average: #{sales_analyzer.average_sales(sales)}"
puts "Max: #{sales_analyzer.max_sales(sales)}"
puts "Min: #{sales_analyzer.min_sales(sales)}"

puts "\n" + "=" * 60
puts "PROBLEMS:"
puts "1. Same logic written 3 times (grades, sales, temperatures)"
puts "2. Each class is NOT reusable - only works for its specific case"
puts "3. Bug in one? Must fix it 3 times!"
puts "4. Want to add median? Must implement it 3 times!"
puts "5. Cannot use with other types (strings, dates, custom objects)"
puts ""
puts "This violates Meyer's principle of reusability:"
puts "We're 'reinventing solutions to problems that have been"
puts "encountered before' instead of capturing the common pattern."
puts "=" * 60
