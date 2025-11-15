class Bottles
  def song
    verses(99, 0)
  end

  def verses(upper, lower)
    upper.downto(lower).collect { |i| verse(i) }.join("\n")
  end

  def verse(number)
    if number == 0
      "#{pronoun(0).capitalize} of beer on the wall, " +
      "#{pronoun(0)} of beer.\n" +
      "Go to the store and buy some more, " +
      "#{pronoun(99)} of beer on the wall.\n"
    else
      "#{pronoun(number)} of beer on the wall, " +
      "#{pronoun(number)} of beer.\n" +
      "#{action(number)}, " +
      "#{pronoun(number-1)} of beer on the wall.\n"
    end
  end

  def container(number)
    number == 1 ? 'bottle' : 'bottles'
  end

  def quantity(number)
    number == 0 ? 'no more' : number.to_s
  end

  def action(number)
    number == 1 ? 'Take it down and pass it around' : 'Take one down and pass it around'
  end

  def pronoun(number)
    "#{quantity(number)} #{container(number)}"
  end
end
