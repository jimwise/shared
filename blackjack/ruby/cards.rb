# frozen_string_literal: true

module Cards
  SUITS = %i[hearts diamonds clubs spades].freeze
  CARDS = %i[ace two three four five six seven eight nine ten jack queen king].freeze

  # one card, inherit to override contents of one deck, or add functionality
  class Card
    attr_reader :suit, :card
    def initialize card, suit
      raise "Bad Card (#{card} of #{suit})" unless SUITS.find(suit) && CARDS.find(card)
      @card, @suit = card, suit
    end

    def to_s
      "#{@card.to_s.capitalize} of #{@suit.to_s.capitalize}"
    end

    def self.deck
      @deck ||= SUITS.product(CARDS).map { |suit, card| new(card, suit) }.freeze
      @deck
    end
  end

  # an N-card (default 6) shoe which refills when empty
  # TODO: a real shoe is emptied and refilled at a defined point before empty
  class Shoe
    private_methods :shuffle!

    def initialize klass, decks = 6
      @card = klass
      @decks = decks
      @shoe = []
    end

    def shuffle!
      @shoe = (@card.deck * @decks).shuffle!
    end

    def draw!
      if @shoe.empty?
        puts("Refilling shoe with #{@decks} decks")
        shuffle!
      end
      @shoe.shift
    end
  end
end
