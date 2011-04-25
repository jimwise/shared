# This module allows choose/fail (amb) style non-deterministic programming in Ruby
#
# Specifically, this is an all-ruby implementation of choose/fail
# nondeterministic programming with branch cut, as described in Chapter 22
# of Paul Graham's _On Lisp_[1].
#
# This code will not work in JRuby or MacRuby (no callcc).  It should work
# in 1.9.2 with minor changes (callcc has moved to the 'continuation'
# stdlib).
#
# Due to Ruby containing a true call/cc, this is a much straighter port of
# Paul Graham's scheme version of this code than his Common Lisp or my C
# versions are.  :-)
#
#   [1] Graham, Paul, _On Lisp_, Prentice Hall, 1993, ISBN 0130305529.
#       Available online at http://www.paulgraham.com/onlisp.html
#
# Author::    Jim Wise  (mailto:jwise@draga.com)
# Copyright:: Copyright (c) 2011 Jim Wise
# License::   2-clause BSD-Style (see link:LICENSE)

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
    def choose choices = []
      ch = choices.clone          # clone it in case it's modified by the caller
      ch.each do |choice|
        callcc do |cc|
          @paths.unshift cc
          return choice
        end
      end
      self.fail!                  # if we get here, we've exhausted the choices
    end

    alias amb choose

    def fail!
      raise ChoicesExhausted.new if @paths.empty?
      cc = @paths.shift
      # if it quacks (or can be called) like a duck, call it -- it's either a Proc from #mark or a Continuation from #choose
      cc.call
    end

    def require cond
      fail! unless cond
    end

    alias assert require

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
  end
end
