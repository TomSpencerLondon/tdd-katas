require 'minitest/autorun'
require_relative '../lib/movie'
require_relative '../lib/rental'
require_relative '../lib/customer'

class CustomerTest < Minitest::Test
  def setup
    @customer = Customer.new("John Doe")
  end

  def test_statement_with_single_regular_movie_short_rental
    movie = Movie.new("The Godfather", Movie::REGULAR)
    rental = Rental.new(movie, 1)
    @customer.add_rental(rental)

    expected = "Rental Record for John Doe\n" \
               "\tThe Godfather\t2\n" \
               "Amount owed is 2\n" \
               "You earned 1 frequent renter points"

    assert_equal expected, @customer.statement
  end

  def test_statement_with_single_regular_movie_long_rental
    movie = Movie.new("The Godfather", Movie::REGULAR)
    rental = Rental.new(movie, 5)
    @customer.add_rental(rental)

    # 2 base + (5-2) * 1.5 = 2 + 4.5 = 6.5
    expected = "Rental Record for John Doe\n" \
               "\tThe Godfather\t6.5\n" \
               "Amount owed is 6.5\n" \
               "You earned 1 frequent renter points"

    assert_equal expected, @customer.statement
  end

  def test_statement_with_new_release_short_rental
    movie = Movie.new("Inception", Movie::NEW_RELEASE)
    rental = Rental.new(movie, 1)
    @customer.add_rental(rental)

    # 1 * 3 = 3, only 1 point (not 2+ days)
    expected = "Rental Record for John Doe\n" \
               "\tInception\t3\n" \
               "Amount owed is 3\n" \
               "You earned 1 frequent renter points"

    assert_equal expected, @customer.statement
  end

  def test_statement_with_new_release_long_rental
    movie = Movie.new("Inception", Movie::NEW_RELEASE)
    rental = Rental.new(movie, 3)
    @customer.add_rental(rental)

    # 3 * 3 = 9, 2 points (bonus for 2+ days)
    expected = "Rental Record for John Doe\n" \
               "\tInception\t9\n" \
               "Amount owed is 9\n" \
               "You earned 2 frequent renter points"

    assert_equal expected, @customer.statement
  end

  def test_statement_with_childrens_movie_short_rental
    movie = Movie.new("Toy Story", Movie::CHILDRENS)
    rental = Rental.new(movie, 2)
    @customer.add_rental(rental)

    # 1.5 base (only 2 days, threshold is 3)
    expected = "Rental Record for John Doe\n" \
               "\tToy Story\t1.5\n" \
               "Amount owed is 1.5\n" \
               "You earned 1 frequent renter points"

    assert_equal expected, @customer.statement
  end

  def test_statement_with_childrens_movie_long_rental
    movie = Movie.new("Toy Story", Movie::CHILDRENS)
    rental = Rental.new(movie, 5)
    @customer.add_rental(rental)

    # 1.5 base + (5-3) * 1.5 = 1.5 + 3 = 4.5
    expected = "Rental Record for John Doe\n" \
               "\tToy Story\t4.5\n" \
               "Amount owed is 4.5\n" \
               "You earned 1 frequent renter points"

    assert_equal expected, @customer.statement
  end

  def test_statement_with_multiple_rentals
    movie1 = Movie.new("The Godfather", Movie::REGULAR)
    rental1 = Rental.new(movie1, 3)
    @customer.add_rental(rental1)

    movie2 = Movie.new("Inception", Movie::NEW_RELEASE)
    rental2 = Rental.new(movie2, 2)
    @customer.add_rental(rental2)

    movie3 = Movie.new("Toy Story", Movie::CHILDRENS)
    rental3 = Rental.new(movie3, 4)
    @customer.add_rental(rental3)

    # Regular: 2 + (3-2) * 1.5 = 3.5
    # New Release: 2 * 3 = 6, +1 bonus point
    # Children's: 1.5 + (4-3) * 1.5 = 3.0
    # Total: 12.5, 4 points
    expected = "Rental Record for John Doe\n" \
               "\tThe Godfather\t3.5\n" \
               "\tInception\t6\n" \
               "\tToy Story\t3.0\n" \
               "Amount owed is 12.5\n" \
               "You earned 4 frequent renter points"

    assert_equal expected, @customer.statement
  end

  def test_statement_with_no_rentals
    expected = "Rental Record for John Doe\n" \
               "Amount owed is 0\n" \
               "You earned 0 frequent renter points"

    assert_equal expected, @customer.statement
  end
end
