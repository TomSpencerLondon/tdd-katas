# Exercise 1A: BAD Example - Neither Correct nor Robust
#
# This demonstrates what happens when we ignore both correctness and robustness.
# The specification: Validate user registration data (email, age, password)
#
# Problems with this code:
# 1. No correctness: Doesn't properly validate according to clear specifications
# 2. No robustness: Crashes or behaves unexpectedly on invalid input
# 3. No clear contract: Users don't know what inputs are valid

class BadUserValidator
  def validate_user(email, age, password)
    # Problem: No validation at all!
    # This will "succeed" even with completely invalid data
    puts "User registered: #{email}"
    return true
  end

  def calculate_discount(age)
    # Problem: Will crash on non-numeric input
    # Problem: No handling of negative ages or unreasonable values
    if age > 65
      return 0.20  # 20% senior discount
    elsif age < 18
      return 0.10  # 10% youth discount
    else
      return 0.0
    end
  end
end

# Demonstration of failures
puts "=" * 60
puts "BAD EXAMPLE - Neither Correct nor Robust"
puts "=" * 60

validator = BadUserValidator.new

# This should fail but doesn't - LACKS CORRECTNESS
puts "\n1. Invalid email accepted (lacks correctness):"
validator.validate_user("not-an-email", 25, "weak")

# This will crash - LACKS ROBUSTNESS
puts "\n2. Will crash on bad input (lacks robustness):"
begin
  discount = validator.calculate_discount("twenty-five")
  puts "Discount: #{discount}"
rescue => e
  puts "CRASHED: #{e.message}"
end

# This produces nonsensical results - LACKS CORRECTNESS
puts "\n3. Nonsensical input produces nonsensical output:"
discount = validator.calculate_discount(-50)
puts "Discount for age -50: #{discount}"

puts "\n" + "=" * 60
puts "See how the program either crashes or produces wrong results?"
puts "=" * 60
