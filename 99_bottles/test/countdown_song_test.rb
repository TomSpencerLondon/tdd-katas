require 'minitest/autorun'
require_relative '../lib/bottles'

class CountdownSongTest < Minitest::Test
  def test_a_couple_verses
    expected =
      "99 bottles of beer on the wall, " +
      "99 bottles of beer.\n" +
      "Take one down and pass it around, " +
      "98 bottles of beer on the wall.\n" +
      "\n" +
      "98 bottles of beer on the wall, " +
      "98 bottles of beer.\n" +
      "Take one down and pass it around, " +
      "97 bottles of beer on the wall.\n"
    song = CountdownSong.new(verse_template: BottleVerse, max: 99, min: 0)
    assert_equal expected, song.verses(99, 98)
  end

  def test_verse_general_rule
    expected =
      "99 bottles of beer on the wall, " +
      "99 bottles of beer.\n" +
      "Take one down and pass it around, " +
      "98 bottles of beer on the wall.\n"
    song = CountdownSong.new(verse_template: BottleVerse, max: 99, min: 0)
    assert_equal expected, song.verse(99)
  end
end
