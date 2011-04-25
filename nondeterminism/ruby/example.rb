#!/usr/bin/ruby

require 'nondeterminism'

$nd = Nondeterminism::Generator.new

# Choose 2 example  (see _On Lisp_, sec, 22.1 (pp. 286-289)
def choose2
  x = $nd.choose([1, 2])
  $nd.require x.even?
  x
end

puts "chose #{choose2}"

$nd.clear!

# Parlor Trick example (see _On Lisp_, sec, 22.2 (pp. 290-292)
# note that this tests that we have real call/cc, not just downward-facing
def two_numbers
  return $nd.choose(0..5), $nd.choose(0..5)
end

def parlor_trick sum
  a, b = two_numbers
  $nd.fail! unless a + b == sum
  return a, b
end

n = 7
x, y = parlor_trick n
puts "#{n} is the sum of #{x} and #{y}"

$nd.clear!

# Chocoblob Coin Search example (with cuts) (see _On Lisp_, sec, 22.5 (pp. 298-302)
def coin? x
  [["LA", 1, 2], ["NY", 1, 1], ["Boston", 2, 2]].member? x
end

# (define (coin? x)
#   (member x '((la 1 2) (ny 1 1) (bos 2 2))))

def find_boxes
  city = $nd.choose(["LA", "NY", "Boston"])
  $nd.mark
  puts ""
  store = $nd.choose([1, 2])
  box = $nd.choose([1, 2])
  print "#{city} #{store} #{box} "
  if coin? [city, store, box]
    $nd.cut!
    puts "C"
  end
  $nd.fail!
rescue Nondeterminism::ChoicesExhausted
  puts "Done."
end

find_boxes
