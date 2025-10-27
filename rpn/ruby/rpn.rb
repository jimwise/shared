#!/usr/bin/ruby

# RPN calculator
module RPNCalc
  def self.signal_error str
    puts "ERROR: #{str}"
  end

  # an operation
  class Op
    attr_reader :doc

    def initialize calc, arity, doc, &bod
      @calc = calc
      @arity = arity
      @doc = doc
      @bod = bod
    end

    def call
      if @calc.size < @arity
        RPNCalc.signal_error 'stack underflow'
      else
        @bod.call
      end
    end
  end

  # the calculator
  class Calc
    def initialize
      @stack = []
      @ops = {}

      add_op '.', 1, 'display top value on the stack' do
        puts @stack.last
      end

      add_op '#', 0, 'display number of values on the stack' do
        puts @stack.size
      end

      add_op '+', 1, 'replace top two values on the stack with their sum' do
        x, y = @stack.pop 2
        @stack.push x + y
      end

      add_op '-', 1, 'replace top two values on the stack with their difference' do
        x, y = @stack.pop 2
        @stack.push x - y
      end

      add_op '*', 1, 'replace top two values on the stack with their product' do
        x, y = @stack.pop 2
        @stack.push x * y
      end

      add_op '/', 1, 'replace top two values on the stack with their quotient' do
        x, y = @stack.pop 2
        @stack.push x / y
      end

      add_op '^', 1, 'replace top two values on the stack, x and y, with x to the yth power' do
        x, y = @stack.pop 2
        @stack.push x ** y
      end

      add_op 'drop', 1, 'remove top value from the stack' do
        @stack.pop
      end

      add_op 'dup', 1, 'duplicate top value on the stack' do
        x = @stack.pop
        @stack.push x, x
      end

      add_op 'swap', 1, 'swap top two values on the stack' do
        x, y = @stack.pop 2
        @stack.push y, x
      end

      add_op 'help', 0, 'display this help' do |_|
        puts "#{@ops.size} Commands:"
        @ops.each do |name, op|
          puts "  #{name} -- #{op.doc}"
        end
      end
    end

    def add_op name, arity, doc, &bod
      @ops[name] = Op.new self, arity, doc, &bod
    end

    def size
      @stack.size
    end

    def action str
      if (op = @ops[str])
        op.call
      elsif (num = Float str, exception: false)
        @stack.push num
      else
        RPNCalc.signal_error 'unknown operation'
      end
    end

    def repl
      loop do
        print '> '
        action gets.strip
      end
      puts
    end
  end
end

RPNCalc::Calc.new.repl
