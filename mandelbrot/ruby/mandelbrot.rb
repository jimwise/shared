#!/usr/bin/ruby

require 'complex'
require 'rubygems'
require 'chunky_png'

width = ARGV[0].to_i
height = ARGV[1].to_i
$cap = ARGV[2].to_i

img = ChunkyPNG::Image.new(width, height, ChunkyPNG::Color::TRANSPARENT)

$xstep = 3.5 / width            # -2.5 .. 1.0
$ystep = 2.0 / height           # -1.0 .. 1.0

def scalex n
  -2.5 + (n * $xstep)
end

def scaley n
  -1.0 + (n * $ystep)
end

def color n
  return ChunkyPNG::Color::BLACK if n == $cap

  n = n * 255 ** 3 / $cap

  r = n / (255 ** 2)
  g = (n / 255) % 255
  b = n % (255 ** 2)
  ChunkyPNG::Color::rgb(r,g,b)
end  

(0..(height-1)).each do |y|
  (0..(width-1)).each do |x|
    z = 0
    c = Complex scalex(x), scaley(y)

    it = 0
    
    while (it < $cap) do
      z = z ** 2 + c
      break if z.abs2 > 4.0     # 2.0 * 2.0
      it = it + 1
    end
    
    img[x, y] = color it
   end
  STDERR.print "+"
end 
STDERR.puts ""

img.save("mandelbrot.png")
