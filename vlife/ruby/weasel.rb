#!/opt/homebrew/opt/ruby/bin/ruby

GENERATION_SIZE = 100
MUTATION_RATE = 0.05
CHARS = (("A".."Z").to_a << " ")
TARGET =
  if ARGV.empty?
    "METHINKS IT IS LIKE A WEASEL"
  else
    ARGV.join(" ").upcase.chars.select { CHARS.any? it }.join
  end

puts TARGET
puts "-" * TARGET.size

def randchar
  CHARS.sample
end

def randstr
  TARGET.size.times.collect { randchar }.join
end

def mutatestr string
  string.chars.collect { |char| (rand <= MUTATION_RATE) ? randchar : char }.join
end

def score string
  string.size.times.count { |index| string[index] == TARGET[index] }
end

def generation string
  GENERATION_SIZE.times.collect { mutatestr string }.max_by { score it }
end

def out generation, string
  puts "#{string} (#{generation})"
end

s = randstr
i = 0

until s == TARGET
  out i, s
  s = generation s
  i += 1
end

out i, s
