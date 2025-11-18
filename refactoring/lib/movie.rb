require_relative 'regular_price'
require_relative 'new_release_price'
require_relative 'childrens_price'

class Movie
  REGULAR = 0
  NEW_RELEASE = 1
  CHILDRENS = 2

  attr_reader :title
  attr_accessor :price_code

  def initialize(title, price_code)
    @title, @price_code = title, price_code
  end

  def charge(days_rented)
    price_class.charge(days_rented)
  end

  def frequent_renter_points(days_rented)
    price_class.frequent_renter_points(days_rented)
  end

  private

  def price_class
    case price_code
    when REGULAR
      RegularPrice.new
    when NEW_RELEASE
      NewReleasePrice.new
    when CHILDRENS
      ChildrensPrice.new
    end
  end
end
