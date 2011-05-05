#!/usr/bin/ruby

require 'rubygems'
require 'ambit'

# yes, I know, no monaco, luxembourg, or andorra
WesternEurope = {
  :portugal => [:spain],
  :spain => [:france, :portugal, :andorra],
  :france => [:spain, :belgium, :germany, :switzerland, :italy, :luxembourg, :andorra],
  :belgium => [:france, :netherlands, :germany, :luxembourg],
  :netherlands => [:belgium, :germany],
  :germany => [:france, :belgium, :netherlands, :switzerland, :denmark, :austria, :luxembourg],
  :denmark => [:germany],
  :switzerland => [:france, :germany, :austria, :italy],
  :italy => [:france, :switzerland, :austria],
  :austria => [:germany, :switzerland, :italy],
  :luxembourg => [:france, :belgium, :germany],
  :andorra => [:spain, :france],
}

Colors = [:red, :yellow, :blue, :green]

def colorize map
  # map from country to its color
  colorized = {}
  map.each do |country, neighbors|
    local_colorized = colorized.clone # fake a functional view of colorized
    color = Ambit.choose Colors
    puts "considering #{color} for #{country}"
    neighbors.each {|n| Ambit.assert colorized[n] != color}
    local_colorized[country] = color
    colorized = local_colorized
  end
  colorized
end

# a deterministic check, for comparison purposes
def check map, colorized
  map.each do |country, neighbors|
    color = colorized[country]
    neighbors.each do |neighbor|
      if colorized[neighbor] == color
        raise "country #{country} and neighbor #{neighbor} have the same color!"
      end
    end
  end
end

colorized = colorize WesternEurope
check WesternEurope, colorized

colorized.each do |country, color|
  puts "#{country} => #{color}"
end
