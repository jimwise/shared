#!/opt/homebrew/opt/ruby/bin/ruby

require "curses"

class Rules
  def initialize num
    @rules = (0..7).collect do |n|
      mask = 1 << n
      num & mask > 0
    end
  end

  def [] l, m, r
    @rules[key(l, m, r)]
  end

  def []= l, m, r, x
    @rules[key(l, m, r)] = x
  end

  def to_s
    (0..7).collect do |x|
      @rules[x] ? "*" : " "
    end.join
  end

  private

  def key l, m, r
    (l ? 4 : 0) | (m ? 2 : 0) | (r ? 1 : 0)
  end
end

class World
  attr_reader :rules

  def initialize size, rules, blank = false
    @size = size
    @cells = Array.new size, false
    @rules = rules

    @cells[size / 2 + 1] = true unless blank
  end

  def to_s
    @cells.collect { |cell| cell ? "*" : " " }.join
  end

  def generation
    new = World.new @size, @rules, true
    @cells.each_index do |i|
      new[i] = @rules[@cells[i - 1], @cells[i], @cells[i + 1]]
    end
    new
  end

  def [] n
    # allow edge cells to apply rules normally
    if n < 0 or n > size - 1
      false
    else
      @cells[n]
    end
  end

  def []= n, x
    @cells[n] = x
  end
end
#  onedee.c -- a simple two dimensional cellular automoton with a variable
#  ruleset, based on the work of Stephen Wolfram.
#
#  onedee is copyright (c) 2015, Jim Wise
#
#  You may redistribute this code freely.
#  You may modify and redistribute this code freely as long as you retain
#  this paragraph and an indication that the code has been modified.
#  pedant comes with absolutely NO WARRANTY.

# the @rules structure is an array indexed by the values of each of three
# relevant cells.

# USAGE = "usage: %s [-ha]\n";
# HELP =  "  -h	show this help\n" +
#   "  -a	run all 256 possible @rulesets, instead of prompting for one\n";

def getrules
  rules = Rules.new
  # this is silly, but allows easy expansion to more relevant squares...
  (0..1).each do |l|
    (0..1).each do |m|
      (0..1).each do |r|
        until x == 0 or x == 1
          printf "%d%d%d --> ", l, m, r
          scanf "%d", x
        end
        rules[l][m][r] = x
      end
    end
  end
end

# run -- run the simulation

def run world, rows
  Curses.setpos 0, 0
  Curses.attron Curses::A_REVERSE
  Curses.addstr "01234567"
  Curses.attroff Curses::A_REVERSE
  Curses.setpos 1, 0
  Curses.addstr world.rules.to_s
  (2..rows).each do |row|
    Curses.setpos row, 0
    Curses.addstr world.to_s
    world = world.generation
  end
  Curses.refresh
end

# setrules -- given an int, convert it into a rule and store it in @rules[][][]

allrules = true

# while ((c = getopt(argc, argv, "ha")) != -1) {
#     switch(c) {
#     case 'h':
#       printf(usage, argv[0]);
#       printf(help);
#       exit(0);
#       break;
#     case 'a':
#       allrules = 1;
#       break;
#     case '?':
#       fprintf(stderr, usage, argv[0]);
#       exit(1);
#     }
#   }

Curses.init_screen
win = Curses::Window.new 0, 0, 0, 0

rows = win.maxy
cols = win.maxx

Curses.curs_set 0
Curses.cbreak
Curses.noecho
Curses.clear
Curses.refresh

begin
  if allrules
    (0..255).each do |n|
      run World.new(cols, Rules.new(n)), rows
      Curses.getch
    end
  else
    run World.new(cols, getrules), rows
    Curses.getch
  end
ensure
  Curses.close_screen
end
