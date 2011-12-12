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
        RPNCalc::signal_error "stack underflow"
        stack
      else
        @bod.call(stack)
      end
    end
  end

  class Calc
    attr_reader :ops
    def initialize
      @stack = []
      @ops = {}

      self.add_op(".", 1, "display top value on stack") {|stack|
        puts stack[0]
      }
      self.add_op("+", 1, "replace top two values on stack with their sum") {|stack|
        stack[0,2] = stack[1] + stack[0]
      }
      self.add_op("-", 1, "replace top two values on stack with their difference") {|stack|
        stack[0,2] = stack[1] - stack[0]
      }
      self.add_op("*", 1, "replace top two values on stack with their product") {|stack|
        stack[0,2] = stack[1] * stack[0]
      }
      self.add_op("/", 1, "replace top two values on stack with their quotient") {|stack|
        stack[0,2] = stack[1] / stack[0]
      }
      self.add_op("drop", 1, "remove top value on stack") {|stack|
        stack[0,1] = nil
      }
      self.add_op("dup", 1, "duplicate top value on stack") {|stack|
        stack.unshift stack[0]
      }
      self.add_op("swap", 1, "swap top two values on stack") {|stack|
        stack[0], stack[1] = stack[1], stack[0]
      }
      self.add_op("help", 0, "display this help") {|stack|
        puts "#{@ops.size} Commands:"
        @ops.map {|k, v|
          puts "  #{k} -- #{v.doc}"
        }
      }
    end

    def add_op name, arity, doc, &bod
      ops[name] = Op.new(arity, doc, &bod)
    end

    def action str
      if @ops[str]
        @ops[str].call(@stack)
      else
        begin
          @stack.unshift(Float(str))
        rescue
          RPNCalc::signal_error("unknown operation")
        end
      end
    end

    def repl
      print "> "
      while s = gets do
        self.action s.strip
        print "> "
      end
      puts
    end
  end
end

RPNCalc::Calc.new.repl
