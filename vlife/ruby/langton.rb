#!/usr/local/bin/ruby

require 'curses'
require './world'

class LangtonWorld < World
  attr_accessor :ant_row, :ant_col, :ant_facing

  def initialize rows, cols, age = 0
    super rows, cols, age
    @ant_row = @rows/2
    @ant_col = @cols/2
    @ant_facing = :north
  end
  
  def generation
    new = self.clone
    if self[@ant_row, @ant_col]
      new.left
    else
      new.right
    end
    new.flip.forward
  end

  protected
  
  def right
    @ant_facing = case @ant_facing
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
    @ant_facing = case @ant_facing
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

def put world
  Curses::clear
  Curses::setpos 0,0
  Curses::addstr world.to_s
  Curses::setpos world.ant_row, world.ant_col
  Curses::attron Curses::A_STANDOUT
  Curses::addch case world.ant_facing
                when :north
                  '^'
                when :east
                  '>'
                when :south
                  'v'
                when :west
                  '<'
                end
  Curses::attroff Curses::A_STANDOUT
  Curses::refresh
end

Curses::init_screen()
Curses::curs_set 0
win = Curses::Window.new 0, 0, 0, 0
world = LangtonWorld.new win.maxy, win.maxx

while true
  put world
  world = world.generation
end

Curses::close_screen
