class Bottles
  def song
    verses(99, 0)
  end

  def verses(upper, lower)
    upper.downto(lower).collect { |i| verse(i) }.join("\n")
  end

  def verse(number)
    bottle_number = BottleNumber.new(number)
    next_bottle_number = BottleNumber.new(bottle_number.successor)

    "#{bottle_number.pronoun.capitalize} of beer on the wall, " +
    "#{bottle_number.pronoun} of beer.\n" +
    "#{bottle_number.action}, " +
    "#{next_bottle_number.pronoun} of beer on the wall.\n"
  end
end

class BottleNumber
  attr_reader :number

  def initialize(number)
    @number = number
  end

  def container
    number == 1 ? 'bottle' : 'bottles'
  end

  def quantity
    number == 0 ? 'no more' : number.to_s
  end

  def action
    if number == 0
      'Go to the store and buy some more'
    elsif number == 1
      'Take it down and pass it around'
    else
      'Take one down and pass it around'
    end
  end

  def pronoun
    if number == 6
      "1 six-pack"
    else
      "#{quantity} #{container}"
    end
  end

  def successor
    if number == 0
      99
    else
      number - 1
    end
  end
end
