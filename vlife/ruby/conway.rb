#!/usr/local/bin/ruby

require 'curses'
require './world'

class ConwayWorld < World
  def generation
    new = self.class.new @rows, @cols, @age + 1
    @rows.times do |row|
      @cols.times do |col|
        count = neighbors(row, col).count(true)
        new[row, col] = if self[row, col]
                          (2..3).include? count
                        else
                          count == 3
                        end
      end
    end
    new
  end
end

def put world, win, infowin
  win.clear
  win.setpos 0,0
  win.addstr world.to_s
  win.refresh

  infowin.clear
  infowin.addstr "Generation: #{world.age}, Population: #{world.population}"
  infowin.clrtoeol
  infowin.refresh
end

Curses::init_screen()
Curses::curs_set 0
win = Curses::Window.new 0, 0, 1, 0
infowin = Curses::Window.new 1, 0, 0, 0
infowin.attron Curses::A_REVERSE
world = ConwayWorld.new win.maxy, win.maxx

randomize = true
acorn = false
density = 0.35

if randomize
  world.rows.times do |r|
    world.cols.times do |c|
      world[r, c] = rand < density
    end
  end
elsif acorn
  world[41,40] = world[43,41] = world[40,42] = world[41,42] = world[44,42] = world[45,42] = world[46,42] = true
else
  # glider
  world[0, 1] = world[1, 2] = world[2, 0] = world[2, 1] = world[2, 2] = true

  # block
  world[20, 10] = world[20, 11] = world[21, 10] = world[21, 11] = true

  # blinker
  world[5,20] = world[5,21] = world[5,22] = true

  # T-tetromino (evolves into traffic light)
  world[20,21] = world[20,22] = world[20,23] = world[21,22] = true
end

while true
  put world, win, infowin
  world = world.generation
end

Curses::close_screen
