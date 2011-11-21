#!/usr/bin/ruby

require 'complex'
require 'rubygems'
require 'chunky_png'

class Mandelbrot
  def initialize cap
    @cap = cap
  end

  # given a complex number, return iterations to escape, up to cap,
  # or zero if no escape occurs
  def escape c
    z = Complex(0, 0)
    for it in 1 .. @cap
      z = z ** 2 + c
      break if z.abs2 > 4.0     # 2.0 * 2.0
    end
    (it == @cap) ? 0 : it
  end
end

class MandelImage < Mandelbrot
  DENSITY_FACTOR = 16
  def initialize width, height, cap
    super cap
    @width = width
    @height = height
    @scalec = 0xffffff / cap * DENSITY_FACTOR
    zoom                        # start at default zoom
  end

  def zoom xmin = -2.5, xmax = 1.0, ymin = -1.0, ymax = 1.0
    @xmin = xmin
    @xmax = xmax
    @ymin = ymin
    @ymax = ymax
    @xstep = (xmax - xmin).abs / @width
    @ystep = (ymax - ymin).abs / @height
    self
  end

  def image
    img = ChunkyPNG::Image.new(@width, @height, ChunkyPNG::Color::TRANSPARENT)

    (0..(@height-1)).each do |y|
      (0..(@width-1)).each do |x|
        c = Complex scalex(x), scaley(y)
        it = escape c
        img[x, y] = color it
      end
    end 
    img
  end

private

  def scalex n
    @xmin + (n * @xstep)
  end

  def scaley n
    @ymin + (n * @ystep)
  end

  def color n
    return ChunkyPNG::Color::BLACK if n == 0
    n = n * @scalec
    
    r = n & 0xff
    g = (n & 0xff00) >> 8
    b = (n & 0xff0000) >> 16
    ChunkyPNG::Color::rgb(r,g,b)
  end  
end

MandelImage.new(1280, 800, 1000).image.save("mandelbrot-ruby.png")
MandelImage.new(1280, 800, 1000).zoom(-0.5, 0.5, 0.0, 0.75).image.save("mandelzoom1-ruby.png")
