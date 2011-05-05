#!/usr/bin/ruby

require 'rubygems'
require 'ambit'

# yes, I know, no monaco, luxembourg, or andorra
WesternEurope = {
  :portugal => [:spain],
  :spain => [:france, :portugal],
  :france => [:spain, :belgium, :germany, :switzerland, :italy],
  :belgium => [:france, :netherlands, :germany],
  :netherlands => [:belgium, :germany],
  :germany => [:france, :belgium, :netherlands, :switzerland, :denmark, :austria],
  :denmark => [:germany],
  :switzerland => [:france, :germany, :austria, :italy],
  :italy => [:france, :switzerland, :austria],
  :austria => [:germany, :switzerland, :italy]
}

Colors = [:red, :yellow, :blue, :green]

def colorize map
  # map from country to its color
  colorized = {}
  map.each do |country, neighbors|
    color = Ambit.choose Colors
    neighbors.each {|n| Ambit.assert colorized[n] != color}
    colorized[country] = color
  end
  colorized
end

# a deterministic check, for comparison purposes
def check map, colorized
  map.each do |country, neighbors|
    color = colorized[country]
    neighbors.each do |neighbor|
      if colorized[neighbor] == color
        raise StandardError "country #{country} and neighbor #{neighbor} have the same color!"
      end
    end
  end
end

colorized = colorize WesternEurope
check WesternEurope, colorized

colorized.each do |country, color|
  puts "#{country} => #{color}"
end
