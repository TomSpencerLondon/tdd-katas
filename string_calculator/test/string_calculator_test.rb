require 'minitest/autorun'
require_relative '../lib/string_calculator'

class StringCalculatorTest < Minitest::Test
  def test_empty_string_returns_zero
    assert_equal 0, StringCalculator.new.add("")
  end

  def test_single_number_returns_itself
    assert_equal 1, StringCalculator.new.add("1")
  end

  def test_two_numbers_comma_separated_returns_sum
    assert_equal 3, StringCalculator.new.add("1,2")
  end

  def test_unknown_amount_of_numbers_returns_sum
    assert_equal 15, StringCalculator.new.add("1,2,3,4,5")
  end

  def test_handles_newline_as_delimiter
    assert_equal 6, StringCalculator.new.add("1\n2,3")
  end

  def test_handles_custom_delimiter
    assert_equal 3, StringCalculator.new.add("//;\n1;2")
  end

  def test_negative_number_throws_exception
    error = assert_raises(RuntimeError) do
      StringCalculator.new.add("-1,2")
    end
    assert_equal "negatives not allowed: -1", error.message
  end

  def test_multiple_negatives_show_all_in_exception
    error = assert_raises(RuntimeError) do
      StringCalculator.new.add("1,-2,3,-4")
    end
    assert_equal "negatives not allowed: -2, -4", error.message
  end

  def test_get_called_count_tracks_add_invocations
    calc = StringCalculator.new
    assert_equal 0, calc.get_called_count

    calc.add("1,2")
    assert_equal 1, calc.get_called_count

    calc.add("3,4")
    assert_equal 2, calc.get_called_count

    calc.add("5")
    assert_equal 3, calc.get_called_count
  end

  def test_numbers_bigger_than_1000_are_ignored
    assert_equal 2, StringCalculator.new.add("2,1001")
    assert_equal 1002, StringCalculator.new.add("1000,2")
  end

  def test_delimiters_can_be_any_length
    assert_equal 6, StringCalculator.new.add("//[***]\n1***2***3")
  end

  def test_multiple_delimiters
    assert_equal 6, StringCalculator.new.add("//[*][%]\n1*2%3")
  end

  def test_multiple_delimiters_with_length_longer_than_one
    assert_equal 6, StringCalculator.new.add("//[**][%%]\n1**2%%3")
  end
end
