#!/usr/bin/ruby -I.

require 'cards'
require 'blackjack'

Stake = 1000.00
TableMin = 5.00
TableLimit = 1000.00

def play_one_hand player_hand, dealer_hand
  player_hand.purse.bet!
  player_hand.deal!
  dealer_hand.deal!

  dealer_hand.show false
  player_hand.show
  printf "Bet: %.2f\n", player_hand.purse.curr_bet

  playersbest = player_hand.play!
  if playersbest == 0
    puts "Dealer wins"
    player_hand.purse.lose!
    return
  end

  if player_hand.blackjack? and not dealer_hand.blackjack?
    puts "Player wins"
    player_hand.purse.blackjack!
    return
  end

  dealersbest = dealer_hand.play!
  if dealersbest == 0
    puts "Player wins"
    player_hand.purse.win!
    return
  end

  dealer_hand.show true
  player_hand.show

  puts ""

  if dealersbest > playersbest
    puts "Dealer wins"
    player_hand.purse.lose!
  elsif playersbest > dealersbest
    puts "Player wins"
    player_hand.purse.win!
  else
    puts "Push"
    player_hand.purse.push!
  end
end

shoe = Cards::Shoe.new
player_hand = Blackjack::PlayerHand.new shoe, Stake, TableMin, TableLimit
dealer_hand = Blackjack::DealerHand.new shoe

puts "You have: #{player_hand.purse}"

loop do
  player_hand.muck!
  dealer_hand.muck!

  play_one_hand player_hand, dealer_hand

  if player_hand.purse.broke?
    puts "You're out of money!"
    break
  end

  puts "You have: #{player_hand.purse}"

  break unless Blackjack.get_resp "Continue ([Y]es or [N]o) ([Y]N)? ",
                                  "Please anser [Y]es or [N]o (default Y): ",
                                  {"y" => true, "n" => false}, true
end
