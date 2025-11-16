class StringCalculator
  def initialize
    @call_count = 0
  end

  def add(numbers)
    @call_count += 1

    return 0 if numbers.empty?

    delimiter = /,|\n/

    if numbers.start_with?("//")
      parts = numbers.split("\n", 2)
      delimiter_part = parts[0][2..-1]

      # Check if delimiter is wrapped in brackets: [***] or multiple [*][%]
      if delimiter_part.include?("[")
        # Extract all delimiters between brackets
        delimiters = delimiter_part.scan(/\[([^\]]+)\]/).flatten
        # Escape each delimiter and join with OR (|)
        delimiter = Regexp.new(delimiters.map { |d| Regexp.escape(d) }.join("|"))
      else
        delimiter = Regexp.new(Regexp.escape(delimiter_part))
      end

      numbers = parts[1]
    end

    nums = numbers.split(delimiter).map(&:to_i)

    negatives = nums.select { |n| n < 0 }
    unless negatives.empty?
      raise "negatives not allowed: #{negatives.join(', ')}"
    end

    nums.select { |n| n <= 1000 }.sum
  end

  def get_called_count
    @call_count
  end
end
