class Bottles
  def song
    verses(99, 0)
  end

  def verses(upper, lower)
    upper.downto(lower).collect { |i| verse(i) }.join("\n")
  end

  def verse(number)
    if number == 0
      "No more bottles of beer on the wall, " +
      "no more bottles of beer.\n" +
      "Go to the store and buy some more, " +
      "99 bottles of beer on the wall.\n"
    else
      "#{quantity(number)} #{container(number)} of beer on the wall, " +
      "#{quantity(number)} #{container(number)} of beer.\n" +
      "#{action(number)}, " +
      "#{quantity(number-1)} #{container(number-1)} of beer on the wall.\n"
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
end
