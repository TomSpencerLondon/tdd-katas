# Exercise 4A: BAD Example - Incompatible Components
#
# This demonstrates the compatibility disaster Meyer describes:
# "Different modules could not pull the information needed from the other side"
#
# Problems:
# 1. Each component uses a different data format
# 2. No standardized interface for communication
# 3. Components cannot work together without custom translation code
# 4. Adding a new format breaks the whole system

# Component 1: Customer data processor (uses hash)
class CustomerProcessor
  def process(customer_hash)
    name = customer_hash[:name]
    email = customer_hash[:email]
    "Customer: #{name} (#{email})"
  end
end

# Component 2: Order data processor (uses array)
class OrderProcessor
  def process(order_array)
    order_id = order_array[0]
    amount = order_array[1]
    date = order_array[2]
    "Order ##{order_id}: $#{amount} on #{date}"
  end
end

# Component 3: Shipping data processor (uses custom object)
class ShippingData
  attr_accessor :tracking_number, :carrier, :status

  def initialize(tracking, carrier, status)
    @tracking_number = tracking
    @carrier = carrier
    @status = status
  end
end

class ShippingProcessor
  def process(shipping_obj)
    "Shipment #{shipping_obj.tracking_number} via #{shipping_obj.carrier}: #{shipping_obj.status}"
  end
end

# Now try to build a report generator that uses ALL components
class BadReportGenerator
  def initialize
    @customer_processor = CustomerProcessor.new
    @order_processor = OrderProcessor.new
    @shipping_processor = ShippingProcessor.new
  end

  def generate_report(data_type, data)
    # INCOMPATIBILITY: Must know internal format of each component!
    case data_type
    when :customer
      @customer_processor.process(data)  # Expects hash
    when :order
      @order_processor.process(data)     # Expects array
    when :shipping
      @shipping_processor.process(data)  # Expects custom object
    else
      "Unknown data type"
    end
  end

  # Want to process a collection? More compatibility nightmares!
  def batch_process(items)
    items.each do |item_type, item_data|
      # Each type requires different handling - no common interface!
      puts generate_report(item_type, item_data)
    end
  end
end

puts "=" * 60
puts "BAD EXAMPLE - Incompatible Components"
puts "=" * 60

generator = BadReportGenerator.new

# Each component requires data in a different format - INCOMPATIBLE!
customer_data = { name: "John Doe", email: "john@example.com" }
order_data = ["ORD-123", 99.99, "2024-01-15"]
shipping_data = ShippingData.new("TRACK-456", "FedEx", "In Transit")

puts "\nProcessing different components:"
puts generator.generate_report(:customer, customer_data)
puts generator.generate_report(:order, order_data)
puts generator.generate_report(:shipping, shipping_data)

puts "\n" + "=" * 60
puts "COMPATIBILITY PROBLEMS:"
puts "1. Each component uses a different data format"
puts "2. Cannot treat all data uniformly"
puts "3. Adding a new component requires modifying ReportGenerator"
puts "4. Cannot easily pass data between components"
puts "5. Like the AMR disaster: components 'did not work with each other'"
puts ""
puts "Meyer's warning: Components 'developed separately, by different"
puts "methods. When put together, they did not work with each other.'"
puts "=" * 60
