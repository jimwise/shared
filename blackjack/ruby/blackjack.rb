#!/opt/csw/bin/ruby -I.

require 'cards'

module Blackjack

  Cards::Vals = {}
  Cards::CARDS.zip([ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10 ]).each do |(k, v)|
    Cards::Vals[k] = v
  end

  class Cards::Card
    def val
      Cards::Vals[@card]
    end
  end

  class Hand
    def initialize shoe
      @shoe = shoe
      @cards = []
    end

    def deal!
      add! @shoe.draw!
      add! @shoe.draw!
    end

    def add! card
      @cards << card
      card
    end

    def muck!
      @cards = []
    end

    def hit!
      puts add! @shoe.draw!
      show_value true
      if busted?
        puts "[BUST]"
        return 0
      end
      return value
    end

    def show reveal=false
      if @cards.empty?
        puts "[no cards]"
      else
        puts (reveal ? "  #{@cards[0]}" :"  one face down card")
        @cards.drop(1).each {|c| puts "  #{c}"}
        show_value reveal
      end
    end

    def show_value reveal=false
      if reveal
        puts "Total value: #{values.join('/')}"
      else
        puts "Total value: ???"
      end
    end

    def busted?
      value == 0
    end

    def blackjack?
      (@cards.size == 2) and (value == 21)
    end

    def values
      # push zero in case only cards are aces
      vals = [@cards.reject {|c| c.card == :ace}.map {|c| c.val}.push(0).reduce(:+)]

      @cards.count {|c| c.card == :ace}.times do
        vals = vals.map {|v| [v+1, v+11]}.flatten.uniq.sort
      end

      vals
    end

    def value
      # push 0 in case busted
      values.push(0).reject {|x| x > 21}.max
    end
  end

  class DealerHand < Hand
    def show reveal=false
      puts ""
      puts "Dealer has:"
      super reveal
    end

    def hit!
      print "Dealer draws the "
      super
    end

    def play!
      puts ""
      if blackjack?
        puts "[BLACKJACK]"
        return 21
      end

      puts "The dealer reveals the #{@cards[0]}"

      show_value true
      
      # XXX XXX should dealer hit a soft 17?  should this be configurable?
      until value >= 17 do
        return 0 if hit! == 0
      end

      puts "Dealer stands"
      return value
    end
  end

  class PlayerHand < Hand
    attr_reader :purse

    def initialize shoe, stake, min, limit
      super shoe
      @purse = Blackjack::Purse.new stake, min, limit
    end

    def show
      puts ""
      puts "Player has:"
      super true
    end

    def hit!
      print "You draw the "
      super
    end

    def play!
      puts ""
      if blackjack?
        puts "[BLACKJACK]"
        return 21
      end

      first_draw = true
      # XXX - insurance
  
      while true do            # actually, until we bust, surrender or stand
        # XXX - split
        # XXX - this is `late surrender', early surrender has to be
        # handled at insurance time, if it is to be offered
        case if first_draw
               action = Blackjack::get_resp "[H]it, [D]ouble down, [S]tand, or S[u]rrender (HDSU)? ",
                                            "Please enter [H], [D], [S], or [U]: ",
                                            {"h" => :hit, "d" => :doubledown, "s" => :stand,
                                              "u" => :surrender}
             else
               action = Blackjack::get_resp "[H]it or [S]tand (HS)? ",
                                            "Please enter [H] or [S]: ",
                                            {"h" => :hit,  "s" => :stand}
             end
        when :hit
          return 0 if hit! == 0 
          # XXX some casinos allow DD after split.  some don't (confirm)
          first_draw = false
        when :stand
          puts "You stand"
          return value
        when :doubledown
          if @purse.broke?
            puts "You cannot afford to double down!"
            next
          end
          @purse.double_down!
          return hit!
        when :surrender
          puts "You surrender"
          @purse.surrender!
          return 0
        end
      end
    end
  end

  class Purse
    attr_reader :purse, :table_min, :curr_bet

    def initialize stake, table_min, table_limit
      @purse = stake
      @table_min = table_min
      @table_limit = table_limit
      @curr_bet = 0.0
      @last_bet = table_min
    end

    def to_s
      "$%.2f" % @purse
    end

    def broke?
      @purse < @table_min
    end

    def get_bet max=false
      max = @table_limit unless max

      @last_bet = [@last_bet, @purse].min

      # XXX check actual min/table_limit rules
      printf "Please enter a bet (min = $%.2f, limit = $%.2f) [%.2f]: ",
             @table_min, max, @last_bet

      while s = gets.strip.sub(/^\$/, "")
        return @last_bet if s.empty?

        # this will return 0.0 if they enter a non-number.
        # good enough for here, as @table_min will be > 0
        # (use Float(s) if this isn't good enough)
        bet = s.to_f

        if bet < @table_min
          printf "Bet must be at least $%.2f, try again: ", @table_min
          next
        end

        if (bet % @table_min) != 0
          print  "Bet must be a multiple of $%.2f, try again: ", @table_min
          next
        end

        if bet > @table_limit
          print "Bet must be less than or equal to $%.2f, try again: ", @table_min
          next
        end

        # if we get here, bet is good
        @last_bet = bet
        return bet
      end
    end

    def bet!
      @curr_bet = get_bet
      @purse -= @curr_bet
    end

    def double_down!
      b = get_bet @curr_bet
      @purse -= b
      @curr_bet += b
    end

    #     # XXX XXX 3:2 (configurable) on blackjack
    def blackjack!
      @purse += 2.5 * @curr_bet
    end

    def surrender!
      @purse += 0.5 * @curr_bet
    end

    def win!
      @purse += 2.0 * @curr_bet
    end

    # a no-op, but we may want to keep stats later...
    def lose!
    end

    def push!
      @purse += @curr_bet
    end
  end

  # class utility method;  in other ports, this goes in its own module, but it's so tiny here :-)
  def Blackjack::get_resp prompt1, prompt2, allowed, default=false
    print prompt1, " "
    while r = gets.strip.downcase
      return default if default and r == ""
      return allowed[r] if allowed.has_key? r
      print prompt2, " "
    end
  end
end
