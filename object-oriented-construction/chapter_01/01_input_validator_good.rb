# Exercise 1B: GOOD Example - Both Correct and Robust
#
# This demonstrates proper correctness and robustness as described in Chapter 1.
#
# CORRECTNESS: Clear specification and proper validation of normal cases
# - Email must match a valid pattern
# - Age must be between 0 and 150
# - Password must be at least 8 characters
#
# ROBUSTNESS: Graceful handling of abnormal cases
# - Invalid input types (nil, wrong types)
# - Edge cases (negative numbers, extreme values)
# - Clear error messages instead of crashes

class ValidationResult
  attr_reader :success, :errors

  def initialize(success, errors = [])
    @success = success
    @errors = errors
  end

  def valid?
    @success
  end

  def error_message
    @errors.join(", ")
  end
end

class GoodUserValidator
  # SPECIFICATION (Contract):
  # - email: String matching basic email pattern
  # - age: Integer between 0 and 150
  # - password: String of at least 8 characters

  def validate_user(email, age, password)
    errors = []

    # ROBUSTNESS: Handle nil/wrong types gracefully
    if email.nil? || !email.is_a?(String)
      errors << "Email must be a string"
    elsif !valid_email?(email)
      # CORRECTNESS: Check against specification
      errors << "Email must be in valid format"
    end

    if age.nil? || !age.is_a?(Integer)
      errors << "Age must be an integer"
    elsif age < 0 || age > 150
      # CORRECTNESS: Validate within reasonable range
      errors << "Age must be between 0 and 150"
    end

    if password.nil? || !password.is_a?(String)
      errors << "Password must be a string"
    elsif password.length < 8
      # CORRECTNESS: Meet minimum security requirement
      errors << "Password must be at least 8 characters"
    end

    ValidationResult.new(errors.empty?, errors)
  end

  def calculate_discount(age)
    # ROBUSTNESS: Validate input type first
    unless age.is_a?(Integer)
      raise ArgumentError, "Age must be an integer, got #{age.class}"
    end

    # ROBUSTNESS: Check for abnormal values
    if age < 0
      raise ArgumentError, "Age cannot be negative"
    end

    if age > 150
      raise ArgumentError, "Age seems unrealistic (#{age})"
    end

    # CORRECTNESS: Calculate discount according to specification
    if age >= 65
      0.20  # 20% senior discount
    elsif age < 18
      0.10  # 10% youth discount
    else
      0.0   # No discount
    end
  end

  private

  def valid_email?(email)
    # Simple email validation (specification)
    email.match?(/\A[^@\s]+@[^@\s]+\.[^@\s]+\z/)
  end
end

# Demonstration of correctness and robustness
puts "=" * 60
puts "GOOD EXAMPLE - Demonstrating Correctness & Robustness"
puts "=" * 60

validator = GoodUserValidator.new

# CORRECTNESS: Valid input succeeds
puts "\n1. CORRECTNESS - Valid input is accepted:"
result = validator.validate_user("user@example.com", 25, "SecurePass123")
if result.valid?
  puts "✓ User validated successfully!"
else
  puts "✗ Validation failed: #{result.error_message}"
end

# CORRECTNESS: Invalid email is rejected
puts "\n2. CORRECTNESS - Invalid email is rejected:"
result = validator.validate_user("not-an-email", 25, "SecurePass123")
if result.valid?
  puts "✓ User validated successfully!"
else
  puts "✗ Validation failed: #{result.error_message}"
end

# ROBUSTNESS: Wrong type doesn't crash, gives clear error
puts "\n3. ROBUSTNESS - Wrong type handled gracefully:"
result = validator.validate_user("user@example.com", "twenty-five", "SecurePass123")
if result.valid?
  puts "✓ User validated successfully!"
else
  puts "✗ Validation failed: #{result.error_message}"
end

# ROBUSTNESS: Multiple errors reported clearly
puts "\n4. ROBUSTNESS - Multiple errors reported:"
result = validator.validate_user(nil, -5, "weak")
if result.valid?
  puts "✓ User validated successfully!"
else
  puts "✗ Validation failed: #{result.error_message}"
end

# ROBUSTNESS: Discount calculation with error handling
puts "\n5. ROBUSTNESS - Abnormal cases handled explicitly:"
begin
  discount = validator.calculate_discount("not-a-number")
  puts "Discount: #{discount}"
rescue ArgumentError => e
  puts "✗ Error caught and handled: #{e.message}"
end

begin
  discount = validator.calculate_discount(-50)
  puts "Discount: #{discount}"
rescue ArgumentError => e
  puts "✗ Error caught and handled: #{e.message}"
end

# CORRECTNESS: Normal case works perfectly
puts "\n6. CORRECTNESS - Normal cases work as specified:"
discount = validator.calculate_discount(70)
puts "✓ Senior (70) gets #{(discount * 100).to_i}% discount"
discount = validator.calculate_discount(15)
puts "✓ Youth (15) gets #{(discount * 100).to_i}% discount"
discount = validator.calculate_discount(30)
puts "✓ Adult (30) gets #{(discount * 100).to_i}% discount"

puts "\n" + "=" * 60
puts "KEY INSIGHTS FROM CHAPTER 1:"
puts "=" * 60
puts "CORRECTNESS: Handles all cases WITHIN the specification correctly"
puts "ROBUSTNESS: Handles cases OUTSIDE the specification gracefully"
puts "Together, they create reliable software that users can trust!"
puts "=" * 60
