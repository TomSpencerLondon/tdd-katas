# Exercise 4B: GOOD Example - Compatible Components
#
# This demonstrates Meyer's solution to compatibility:
# "The key to compatibility lies in homogeneity of design, and in
# agreeing on standardized conventions for inter-program communication"
#
# Benefits:
# 1. All components share a common interface (standardized protocol)
# 2. Components can be combined without custom translation code
# 3. New components integrate seamlessly
# 4. Data can flow between components easily

# STANDARDIZED INTERFACE: All data processors implement this
class DataRecord
  def initialize
    @fields = {}
  end

  def set_field(name, value)
    @fields[name] = value
  end

  def get_field(name)
    @fields[name]
  end

  def to_s
    @fields.map { |k, v| "#{k}: #{v}" }.join(", ")
  end
end

# COMPATIBLE: All processors use the same interface
class CustomerProcessor
  def process(record)
    name = record.get_field(:name)
    email = record.get_field(:email)
    "Customer: #{name} (#{email})"
  end
end

class OrderProcessor
  def process(record)
    order_id = record.get_field(:order_id)
    amount = record.get_field(:amount)
    date = record.get_field(:date)
    "Order ##{order_id}: $#{amount} on #{date}"
  end
end

class ShippingProcessor
  def process(record)
    tracking = record.get_field(:tracking_number)
    carrier = record.get_field(:carrier)
    status = record.get_field(:status)
    "Shipment #{tracking} via #{carrier}: #{status}"
  end
end

# NEW: Product processor - integrates seamlessly due to compatibility!
class ProductProcessor
  def process(record)
    sku = record.get_field(:sku)
    name = record.get_field(:product_name)
    price = record.get_field(:price)
    "Product #{sku}: #{name} - $#{price}"
  end
end

# Report generator works with ANY processor - they're all compatible!
class GoodReportGenerator
  def initialize
    @processors = {}
  end

  def register_processor(type, processor)
    @processors[type] = processor
  end

  def generate_report(data_type, record)
    processor = @processors[data_type]
    if processor
      processor.process(record)  # Same interface for ALL processors!
    else
      "Unknown data type: #{data_type}"
    end
  end

  def batch_process(records)
    records.each do |type, record|
      # COMPATIBILITY: All records use the same interface!
      puts generate_report(type, record)
    end
  end

  def pipeline_process(record, *processor_types)
    # COMPATIBILITY enables pipelines: data flows between components!
    puts "Pipeline processing:"
    processor_types.each do |type|
      result = generate_report(type, record)
      puts "  #{type}: #{result}"
    end
  end
end

# Factory to create compatible data records from various sources
class DataRecordFactory
  def self.from_hash(hash)
    record = DataRecord.new
    hash.each { |k, v| record.set_field(k, v) }
    record
  end

  def self.from_csv(csv_string)
    # Compatible conversion from CSV
    record = DataRecord.new
    # Simple parsing example
    record
  end

  def self.from_json(json_string)
    # Compatible conversion from JSON
    require 'json'
    hash = JSON.parse(json_string)
    from_hash(hash)
  end
end

puts "=" * 60
puts "GOOD EXAMPLE - Compatible Components"
puts "=" * 60

generator = GoodReportGenerator.new

# Register all processors (all compatible through shared interface)
generator.register_processor(:customer, CustomerProcessor.new)
generator.register_processor(:order, OrderProcessor.new)
generator.register_processor(:shipping, ShippingProcessor.new)
generator.register_processor(:product, ProductProcessor.new)

# All data uses the SAME standardized format
customer_record = DataRecord.new
customer_record.set_field(:name, "John Doe")
customer_record.set_field(:email, "john@example.com")

order_record = DataRecord.new
order_record.set_field(:order_id, "ORD-123")
order_record.set_field(:amount, 99.99)
order_record.set_field(:date, "2024-01-15")

shipping_record = DataRecord.new
shipping_record.set_field(:tracking_number, "TRACK-456")
shipping_record.set_field(:carrier, "FedEx")
shipping_record.set_field(:status, "In Transit")

product_record = DataRecord.new
product_record.set_field(:sku, "PROD-789")
product_record.set_field(:product_name, "Widget Pro")
product_record.set_field(:price, 49.99)

puts "\n1. Processing different components (all compatible):"
puts generator.generate_report(:customer, customer_record)
puts generator.generate_report(:order, order_record)
puts generator.generate_report(:shipping, shipping_record)
puts generator.generate_report(:product, product_record)

puts "\n2. Batch processing (works uniformly):"
batch = [
  [:customer, customer_record],
  [:order, order_record],
  [:product, product_record]
]
generator.batch_process(batch)

puts "\n3. Data can be created from different sources:"
json_customer = '{"name":"Jane Smith","email":"jane@example.com"}'
converted_record = DataRecordFactory.from_json(json_customer)
puts generator.generate_report(:customer, converted_record)

puts "\n4. Components can be combined in pipelines:"
combined_record = DataRecord.new
combined_record.set_field(:name, "Alice")
combined_record.set_field(:email, "alice@example.com")
combined_record.set_field(:order_id, "ORD-999")
combined_record.set_field(:amount, 199.99)
combined_record.set_field(:date, "2024-02-01")

generator.pipeline_process(combined_record, :customer, :order)

puts "\n" + "=" * 60
puts "KEY INSIGHTS FROM CHAPTER 1:"
puts "=" * 60
puts "COMPATIBILITY ACHIEVED THROUGH:"
puts "1. Standardized interface (DataRecord) for ALL components"
puts "2. Homogeneity of design - same patterns throughout"
puts "3. Components work together without custom translation"
puts "4. New components integrate seamlessly"
puts "5. Data can flow between components easily"
puts ""
puts "Meyer's solution: 'The key to compatibility lies in homogeneity"
puts "of design, and in agreeing on standardized conventions for"
puts "inter-program communication.'"
puts ""
puts "This is the foundation of object-oriented design and protocols"
puts "like CORBA and OLE-COM that Meyer mentions!"
puts "=" * 60
