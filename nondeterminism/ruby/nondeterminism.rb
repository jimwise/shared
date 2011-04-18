module Nondeterminism

  class ChoicesExhausted < StandardError
  end

  class Generator
    def initialize
      @paths = []
    end

    def clear!
      @paths = []
    end

    # choose -- given an enumerator, begin a generate-and-test process.
    #   this method returns with the first member of the enumerator
    #   a later call to fail! on the same generator will backtrack and
    #   try the next value in the enumerator.
    #   Multiple calls to choose will nest, so that backtracking forms
    #   a tree-like execution path
    def choose choices
      ch = choices.clone          # clone it in case it's modified by the caller
      ch.each do |choice|
        callcc do |cc|
          @paths.unshift cc
          return choice
        end
      end
      self.fail!                  # if we get here, we've exhausted the choices
    end

    def fail!
      raise ChoicesExhausted.new if @paths.empty?
      cc = @paths.shift
      # if it quacks (or can be called) like a duck, call it -- it's either a Proc from #mark or a Continuation from 
      cc.call
    end

    def mark 
      @paths.unshift Proc.new {self.fail!}
    end

    def cut!
      return if @paths.empty?
      # rewind paths back to the last mark
      @paths = @paths.drop_while {|x| x.instance_of? Continuation}
      # drop up to one mark
      @paths = @paths.drop(1) unless @paths.empty?
    end
      
  # (define (mark) (set! *paths* (cons _fail *paths*)))
  # (define (cut)
  #   (cond ((null? *paths*))
  #         ((equal? (car *paths*) _fail)
  #          (set! *paths* (cdr *paths*)))
  #         (else
  #          (set! *paths* (cdr *paths*))
  #          (cut))))
  end
end
