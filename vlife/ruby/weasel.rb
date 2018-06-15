#!/usr/local/bin/ruby

GENERATION_SIZE = 100
MUTATION_RATE = 0.05
CHARS = (('A' .. 'Z').to_a << ' ')
TARGET = if ARGV.size == 0
         then
           'METHINKS IT IS LIKE A WEASEL'
         else
           ARGV.join(' ').upcase.split('').select {|c| CHARS.any? c}.join
         end

puts TARGET

def randchar
  CHARS.sample
end

def randstr
  TARGET.size.times.collect {randchar}.join
end

def mutatestr s
  s.chars.collect {|c| if rand <= MUTATION_RATE then randchar else c end}.join
end

def score s
  s.size.times.count {|i| s[i] == TARGET[i]}
end

def generation s
  GENERATION_SIZE.times.collect {mutatestr s}.max_by {|s| score s}
end

def out i, s
  puts "#{i}: '#{s}'"
end

s = randstr
i = 0

until s == TARGET
  out i, s
  s = generation s
  i += 1
end

out i, s
