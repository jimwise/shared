# frozen_string_literal: true

module Cards
  SUITS = %i[hearts diamonds clubs spades].freeze
  CARDS = %i[ace two three four five six seven eight nine ten jack queen king].freeze

  class Card
    attr_reader :suit, :card
    def initialize card, suit
      raise "Bad Card (#{card} of #{suit})" unless SUITS.find(suit) && CARDS.find(card)
      @card, @suit = card, suit
    end

    def to_s
      "#{@card.to_s.capitalize} of #{@suit.to_s.capitalize}"
    end
  end

  OneDeck = SUITS.flat_map do |suit|
    CARDS.map do |card|
      Card.new(card, suit)
    end
  end

  class Shoe
    private_methods :shuffle!

    DECKS_IN_SHOE = 6

    def initialize
      @shoe = []
    end

    def shuffle!
      @shoe = (OneDeck * DECKS_IN_SHOE).shuffle!
    end

    def draw!
      # TODO: a real shoe is emptied and refilled at a defined point before empty
      if @shoe.empty?
        puts("Refilling shoe with #{DECKS_IN_SHOE} decks")
        shuffle!
      end
      @shoe.shift
    end
  end
end
