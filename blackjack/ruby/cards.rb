#!/opt/csw/bin/ruby

module Cards
  Suits = [:hearts, :diamonds, :clubs, :spades]
  Cards = [:ace, :two, :three, :four, :five, :six, :seven, :eight, :nine, :ten, :jack, :queen, :king]

  class Card
    attr_reader :suit, :card
    def initialize (card, suit)
      raise "Bad Card (#{card} of #{suit})" unless Suits.find {|s| s == suit} and Cards.find {|c| c == card}
      @card, @suit = card, suit
    end

    def to_s
      "#{@card.to_s.capitalize} of #{@suit.to_s.capitalize}"
    end
  end

  OneDeck = Suits.flat_map do |s|
    Cards.map do |c|
      Card.new(c, s)
    end
  end

  class Shoe
    private_methods :shuffle!

    DecksInShoe = 6

    def initialize
      @shoe = []
    end

    def shuffle!
      @shoe = (OneDeck * DecksInShoe).shuffle!
    end

    def draw!
      # note that a real shoe is emptied and refilled at a defined point before empty
      if @shoe.empty?
        puts("Refilling shoe with #{DecksInShoe} decks")
        shuffle!
      end
      @shoe.shift
    end
  end
end
