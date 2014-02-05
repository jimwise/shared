#!/usr/bin/ruby

module RPNCalc
  def self.signal_error str
    puts "ERROR: #{str}"
  end

  class Op
    attr_reader :doc

    def initialize arity, doc, &bod
      @arity = arity
      @doc = doc
      @bod = bod
    end

    def call stack
      if stack.size < @arity
        RPNCalc.signal_error "stack underflow"
        stack
      else
        @bod.call stack
      end
    end
  end

  class Calc
    attr_reader :ops
    def initialize
      @stack = []
      @ops = {}

      add_op ".", 1, "display top value on the stack" do |stack|
        puts stack[0]
      end

      add_op "#", 0, "display number of values on the stack" do |stack|
        puts stack.size
      end

      add_op "+", 1, "replace top two values on the stack with their sum" do |stack|
        stack[0,2] = stack[1] + stack[0]
      end

      add_op "-", 1, "replace top two values on the stack with their difference" do |stack|
        stack[0,2] = stack[1] - stack[0]
      end

      add_op "*", 1, "replace top two values on the stack with their product" do |stack|
        stack[0,2] = stack[1] * stack[0]
      end

      add_op "/", 1, "replace top two values on the stack with their quotient" do |stack|
        stack[0,2] = stack[1] / stack[0]
      end

      add_op "^", 1, "replace top two values on the stack, x and y, with x to the yth power" do |stack|
        stack[0,2] = stack[1] ** stack[0]
      end

      add_op "drop", 1, "remove top value from the stack" do |stack|
        stack.slice! 0
      end

      add_op "dup", 1, "duplicate top value on the stack" do |stack|
        stack.unshift stack[0]
      end

      add_op "swap", 1, "swap top two values on the stack" do |stack|
        stack[0], stack[1] = stack[1], stack[0]
      end

      add_op "help", 0, "display this help" do |stack|
        puts "#{@ops.size} Commands:"
        @ops.map do |k, v|
          puts "  #{k} -- #{v.doc}"
        end
      end
    end

    def add_op name, arity, doc, &bod
      ops[name] = Op.new arity, doc, &bod
    end

    def action str
      if @ops[str]
        @ops[str].call @stack
      else
          @stack.unshift Float str
      end
    rescue
      RPNCalc.signal_error "unknown operation"
    end

    def repl
      print "> "
      STDIN.each do |s|
        action s.strip
        print "> "
      end
      puts
    end
  end
end

RPNCalc::Calc.new.repl
