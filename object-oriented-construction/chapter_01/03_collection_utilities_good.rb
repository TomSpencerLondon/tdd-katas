# Exercise 3B: GOOD Example - Highly Reusable
#
# This demonstrates Meyer's principle of reusability:
# "By capturing a pattern, a reusable software element will be
# applicable to many different developments"
#
# Benefits:
# 1. Generic - works with ANY comparable data type
# 2. Single implementation serves multiple applications
# 3. Fix a bug once, all applications benefit
# 4. Add a feature once, all applications get it
# 5. Reduces total code to maintain

# REUSABLE module that captures the common pattern
module CollectionStatistics
  def self.average(collection)
    return 0 if collection.empty?
    collection.sum.to_f / collection.length
  end

  def self.maximum(collection)
    return nil if collection.empty?
    collection.max
  end

  def self.minimum(collection)
    return nil if collection.empty?
    collection.min
  end

  def self.median(collection)
    return nil if collection.empty?
    sorted = collection.sort
    mid = sorted.length / 2
    sorted.length.odd? ? sorted[mid] : (sorted[mid - 1] + sorted[mid]) / 2.0
  end

  def self.range(collection)
    return nil if collection.empty?
    maximum(collection) - minimum(collection)
  end

  def self.summary(collection, label = "Data")
    return "#{label}: No data" if collection.empty?

    <<~SUMMARY
      #{label} Summary:
        Count: #{collection.length}
        Average: #{average(collection).round(2)}
        Minimum: #{minimum(collection)}
        Maximum: #{maximum(collection)}
        Median: #{median(collection).round(2)}
        Range: #{range(collection)}
    SUMMARY
  end
end

# Now we can reuse this for MANY different applications!

puts "=" * 60
puts "GOOD EXAMPLE - Highly Reusable (Generic Implementation)"
puts "=" * 60

# Application 1: Student grades
grades = [85, 92, 78, 95, 88, 91, 76]
puts CollectionStatistics.summary(grades, "Student Grades")

# Application 2: Sales data (REUSING the same code!)
sales = [1500, 2300, 1800, 2100, 1900, 2500]
puts CollectionStatistics.summary(sales, "Daily Sales ($)")

# Application 3: Temperature data (STILL reusing!)
temperatures = [72, 68, 75, 71, 69, 73, 70]
puts CollectionStatistics.summary(temperatures, "Temperature (°F)")

# Application 4: Employee ages (Works with new data type!)
ages = [25, 32, 28, 45, 38, 29, 41, 35]
puts CollectionStatistics.summary(ages, "Employee Ages")

# Application 5: Even works with floats!
measurements = [3.14, 2.71, 1.41, 1.73, 2.23]
puts CollectionStatistics.summary(measurements, "Scientific Measurements")

# Application 6: Works with dates too!
require 'date'
dates = [
  Date.new(2024, 1, 15),
  Date.new(2024, 3, 22),
  Date.new(2024, 2, 10),
  Date.new(2024, 4, 5)
]
puts "\nProject Dates Analysis:"
puts "  Earliest: #{CollectionStatistics.minimum(dates)}"
puts "  Latest: #{CollectionStatistics.maximum(dates)}"
puts "  Range: #{CollectionStatistics.range(dates)} days"

puts "\n" + "=" * 60
puts "KEY INSIGHTS FROM CHAPTER 1:"
puts "=" * 60
puts "REUSABILITY BENEFITS:"
puts "1. ONE implementation serves 6+ different applications"
puts "2. Bug fixes benefit all applications immediately"
puts "3. New features (like median) added once, available everywhere"
puts "4. Works with ANY comparable type (numbers, dates, etc.)"
puts "5. Less code = more time for correctness & robustness"
puts ""
puts "Meyer's insight: 'Solving the reusability problem essentially"
puts "means that less software must be written, and hence that more"
puts "effort may be devoted (for the same total cost) to improving"
puts "the other factors, such as correctness and robustness.'"
puts "=" * 60
