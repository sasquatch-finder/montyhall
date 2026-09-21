#' Create a new Monty Hall Problem game
#'
#' @description
#' `create_game()` generates a new game consisting of three doors,
#' with two goats and one car placed randomly behind the doors.
#'
#' @details
#' The game setup replicates the game on the TV show "Let's
#' Make a Deal," where there are three doors for a contestant
#' to choose from. One door has a car behind it and two have
#' goats. The contestant selects a door, then the host opens
#' a door to reveal a goat. The contestant is then given an
#' opportunity to stay with their original selection or switch
#' to the other unopened door.
#'
#' @return A length-three character vector indicating the
#' positions of the goats and the car.
#'
#' @examples
#' create_game()
#'
#' @export
create_game <- function()
{
  a.game <- sample(x = c("goat", "goat", "car"), size = 3, replace = F)
  return(a.game)
}


#' Select a door
#' @description
#' `select_door()` randomly selects one of the three doors
#' as the contestant's initial choice.
#'
#' @return A number between 1 and 3 representing the
#' contestant's selected door.
#'
#' @examples
#' select_door()
#'
#' @export
select_door <- function()
{
  doors <- c(1, 2, 3)
  a.pick <- sample(doors, size = 1)
  return(a.pick)
}


#' Open a goat door
#'
#' `open_goat_door()` determines which door with a goat the host opens
#' based on which door the contestant picks as their initial selection.
#'
#' @param game A character vector containing
#' two goats and one car, as created by `create_game()`.
#' @param a.pick The contestant's initial door selection,
#' represented by a number between 1 and 3.
#'
#' @return The number of the goat door opened by the host.
#'
#' @examples
#' game <- create_game()
#' pick <- select_door()
#' open_goat_door(game, pick)
#'
#' @export
open_goat_door <- function(game, a.pick)
{
  doors <- c(1, 2, 3)
  
  # If contestant selected the car,
  # randomly select one of two goats.
  if(game[a.pick] == "car")
  {
    goat.doors <- doors[game != "car"]
    opened.door <- sample(goat.doors, size = 1)
  }
  
  # If contestant selected a goat,
  # open the other goat door.
  if(game[a.pick] == "goat")
  {
    opened.door <- doors[game != "car" & doors != a.pick]
  }
  
  return(opened.door)
}


#' Switch or keep the selected door
#'
#' @description `change_door()` determines the contestant's final door
#' selection based on whether they stay with their original
#' choice or switch to the remaining unopened door.
#'
#' @param stay Logical value indicating whether the contestant
#' stays with their original selection. `TRUE` means stay and
#' `FALSE` means switch.
#' @param opened.door The number of the door opened by the host.
#' @param a.pick The contestant's original door selection.
#'
#' @return The number of the contestant's final door selection.
#'
#' @examples
#' game <- create_game()
#' pick <- select_door()
#' opened <- open_goat_door(game, pick)
#' change_door(TRUE, opened, pick)
#'
#' @export
change_door <- function(stay = T, opened.door, a.pick)
{
  doors <- c(1, 2, 3)
  
  if(stay)
  {
    final.pick <- a.pick
  }
  
  if(!stay)
  {
    final.pick <- doors[doors != opened.door & doors != a.pick]
  }
  
  return(final.pick)
}


#' Determine the winner of a Monty Hall game
#'
#' @description `determine_winner()` determines whether the contestant's
#' final door contains the car or a goat.
#'
#' @param final.pick The contestant's final door selection.
#' @param game A character vector containing two goats and one car.
#'
#' @return `"WIN"` if the final selection contains the car,
#' or `"LOSE"` if it contains a goat.
#'
#' @examples
#' game <- create_game()
#' determine_winner(1, game)
#'
#' @export
determine_winner <- function(final.pick, game)
{
  if(game[final.pick] == "car")
  {
    return("WIN")
  }
  
  if(game[final.pick] == "goat")
  {
    return("LOSE")
  }
}


#' Play one Monty Hall game
#'
#' @description `play_game()` simulates one Monty Hall game and calculates
#' the outcome of both the stay and switch strategies.
#'
#' @return A data frame containing the strategy used and
#' whether that strategy resulted in a win or loss.
#'
#' @examples
#' play_game()
#'
#' @export
play_game <- function()
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door(new.game, first.pick)
  
  final.pick.stay <- change_door(
    stay = T,
    opened.door,
    first.pick
  )
  
  final.pick.switch <- change_door(
    stay = F,
    opened.door,
    first.pick
  )
  
  outcome.stay <- determine_winner(
    final.pick.stay,
    new.game
  )
  
  outcome.switch <- determine_winner(
    final.pick.switch,
    new.game
  )
  
  strategy <- c("stay", "switch")
  outcome <- c(outcome.stay, outcome.switch)
  
  game.results <- data.frame(
    strategy,
    outcome,
    stringsAsFactors = F
  )
  
  return(game.results)
}


#' Simulate many Monty Hall games
#'
#' @description `play_n_games()` simulates the Monty Hall game multiple
#' times, either designated or default, and returns the results for both the stay and
#' switch strategies. The result demonstrates the results of each 
#' strategy over the number of attempts.
#'
#' @param n The number of games to simulate. The default is 100.
#'
#' @return A data frame containing the strategy and outcome
#' for each simulated game.
#'
#' @examples
#' play_n_games(100)
#'
#' @export
play_n_games <- function(n = 100)
{
  library(dplyr)
  
  results.list <- list()
  loop.count <- 1
  
  for(i in 1:n)
  {
    game.outcome <- play_game()
    results.list[[loop.count]] <- game.outcome
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows(results.list)
  
  table(results.df) %>%
    prop.table(margin = 1) %>%
    round(2) %>%
    print()
  
  return(results.df)
}