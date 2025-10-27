#!/usr/bin/ruby

# RPN calculator
module RPNCalc
  def self.signal_error str
    puts "ERROR: #{str}"
  end

  # an operation
  class Op
    attr_reader :doc

    def initialize calc, doc, &bod
      @calc = calc
      @doc = doc
      @bod = bod
      @arity = bod.arity
    end

    def call stack
      if @calc.size < @arity
        RPNCalc.signal_error "stack underflow"
      else
        args = stack.pop @arity
        results = @bod.call(*args)
        stack.push(*results)
      end
    end
  end

  # the calculator
  class Calc
    def initialize
      @stack = []
      @ops = {}

      add_op(".", "display top value on the stack") do |first|
        puts first
        first
      end

      add_op("#", "display number of values on the stack") { puts @stack.size }
      add_op("+", "replace top two values on the stack with their sum") { |first, second| first + second }
      add_op("-", "replace top two values on the stack with their difference") { |first, second| first - second }
      add_op("*", "replace top two values on the stack with their product") { |first, second| first * second }
      add_op("/", "replace top two values on the stack with their quotient") { |first, second| first / second }
      add_op("^", "replace top two values on the stack, x and y, with x to the yth power") { |first, second| first**second }
      add_op("drop", "remove top value from the stack") { |_| }
      add_op("dup", "duplicate top value on the stack") { |first| [first, first] }
      add_op("swap", "swap top two values on the stack") { |first, second| [second, first] }

      add_op "help", "display this help" do
        puts "#{@ops.size} Commands:"
        @ops.each do |name, op|
          puts "  #{name} -- #{op.doc}"
        end
      end
    end

    def add_op name, doc, &bod
      @ops[name] = Op.new self, doc, &bod
    end

    def size
      @stack.size
    end

    def action str
      if (op = @ops[str])
        op.call @stack
      elsif (num = Float str, exception: false)
        @stack.push num
      else
        RPNCalc.signal_error "unknown operation"
      end
    end

    def repl
      loop do
        print "> "
        action gets.strip
      end
      puts
    end
  end
end

RPNCalc::Calc.new.repl
