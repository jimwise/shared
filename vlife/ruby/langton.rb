#!/opt/homebrew/opt/ruby/bin/ruby

require "curses"
require "./world"

class LangtonWorld < World
  def initialize rows, cols, age = 0
    super
    @ant_row = @rows / 2
    @ant_col = @cols / 2
    @ant_facing = :north
  end

  def generation
    if self[@ant_row, @ant_col]
      left
    else
      right
    end
    flip
    forward
  end

  def put
    Curses.clear
    Curses.setpos 0, 0
    Curses.addstr to_s
    Curses.setpos @ant_row, @ant_col
    Curses.attron Curses::A_STANDOUT
    Curses.addch ant_char @ant_facing
    Curses.attroff Curses::A_STANDOUT
    Curses.refresh
    sleep 0.05

    # XXX stop (and wait for keypress) when reaching edge of world
  end

  protected

  def ant_char direction
    case direction
    when :north
      "^"
    when :east
      ">"
    when :south
      "v"
    when :west
      "<"
    end
  end

  def right
    @ant_facing =
      case @ant_facing
      when :north
        :east
      when :east
        :south
      when :south
        :west
      when :west
        :north
      end
    self
  end

  def left
    @ant_facing =
      case @ant_facing
      when :north
        :west
      when :east
        :north
      when :south
        :east
      when :west
        :south
      end
    self
  end

  def flip
    self[@ant_row, @ant_col] = !self[@ant_row, @ant_col]
    self
  end

  def forward
    case @ant_facing
    when :north
      @ant_row -= 1
    when :east
      @ant_col += 1
    when :south
      @ant_row += 1
    when :west
      @ant_col -= 1
    end
    self
  end
end

Curses.init_screen
Curses.curs_set 0
win = Curses::Window.new 0, 0, 0, 0
world = LangtonWorld.new win.maxy, win.maxx

loop do
  world.put
  world = world.generation
end

Curses.close_screen
