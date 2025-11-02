require "curses"

class World
  attr_reader :rows, :cols, :age

  def initialize rows, cols, age = 0
    @rows = rows
    @cols = cols
    @cells = Array.new rows * cols, false
    @age = age
  end

  def [] row, col
    return false if row < 0 || row >= @rows || col < 0 || col >= @cols
    @cells[row * @cols + col]
  end

  def []= row, col, x
    @cells[row * @cols + col] = x
  end

  # to be overridden by worlds
  def generation
    # default is to return same world
    @age += 1
    self
  end

  def population
    @cells.count(true)
  end

  def to_s
    # we rely on wrapping.  eww, but fast..ish.
    @cells.map { it ? "*" : " " }.each_slice(@cols).to_a.join
  end

  protected

  DIRECTIONS = {
    northwest: [-1, -1],
    north: [-1, 0],
    northeast: [-1, 1],
    west: [0, -1],
    east: [0, 1],
    southwest: [1, -1],
    south: [1, 0],
    southeast: [1, 1],
  }.freeze

  def count_neighbors row, col
    DIRECTIONS.count do |_, pair|
      self[row + pair.first, col + pair.last]
    end
  end
    # I know, but measurably faster open coded this way
  #   pairs =
  #     [[row - 1, col - 1], [row - 1, col], [row - 1, col + 1],
  #      [row, col - 1],                     [row, col + 1],
  #      [row + 1, col - 1], [row + 1, col], [row + 1, col + 1]]

  #   pairs.map do |pair|
  #     self[pair[0], pair[1]]
  #   end
  # end
end
