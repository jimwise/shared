#!/opt/csw/bin/ruby

module Cards
  class Card
    Suits = [:hearts, :diamonds, :clubs, :spades]
    Cards = [:ace, :two, :three, :four, :five, :six, :seven, :eight, :nine, :ten, :jack, :queen, :king]

    attr_reader :suit, :card
    def initialize (card, suit)
      throw :bad_card unless Suits.find {|s| s == suit} and Cards.find {|c| c == card}
      @card = card
      @suit = suit
    end

    def to_s
      "#{@card.to_s.capitalize} of #{@suit.to_s.capitalize}"
    end
  end


  class Shoe
    private_methods :shuffle!

    DecksInShoe = 6

    OneDeck = Card::Suits.map do |s|
      Card::Cards.map do |c|
        Card.new(c, s)
      end
    end.flatten

    def initialize
      @shoe = []
    end

    def shuffle!
      # not needed in this version, but a real shoe is emptied and refilled at a defined point before empty
      @shoe = []

      DecksInShoe.times do
        @shoe = @shoe + OneDeck
        @shoe.shuffle!
      end
    end

    def draw!
      if @shoe.empty?
        puts("Refilling shoe with #{DecksInShoe} decks")
        shuffle!
      end
      @shoe.shift
    end
  end
end
