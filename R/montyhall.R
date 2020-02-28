#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
}



#' @title
#'   Select a door in the Monty Hall problem.
#'
#' @description
#'   `select_door()` selects one of the three doors as the contestant's door.
#'
#' @details
#'   Once the "Let's Make a Deal" game is set up with three doors,
#'   the contestant selects a door.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns the contestant's selection.
#'
#' @examples
#'   select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Open a goat door.
#'
#' @description
#'   `open_goat_door()` selects a goat door to open.
#'
#' @details
#'   Once the contestant picks a door, then one of the doors
#'   that is not the contestant's selection must be opened.
#'   The host will open a door that has a goat behind it, so
#'   the contestant must choose if they want to stick with
#'   their original door or choose the other unopened door.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a goat door that was not selected
#'   by the contestant.
#'
#' @examples
#'   open_goat_door()
#'
#' @export
open_goat_door <- function( game, a.pick )
{
  doors <- c(1,2,3)
  # if contestant selected car,
  # randomly select one of two goats
  if( game[ a.pick ] == "car" )
  {
    goat.doors <- doors[ game != "car" ]
    opened.door <- sample( goat.doors, size=1 )
  }
  if( game[ a.pick ] == "goat" )
  {
    opened.door <- doors[ game != "car" & doors != a.pick ]
  }
  return( opened.door ) # number between 1 and 3
}



#' @title
#'   The contestant can stay or change doors.
#'
#' @description
#'   `change_door()` selects either stay or change door based
#'   on the contestant's choice.
#'
#' @details
#'   The contestant is given the option to change doors or
#'   to stay with their original choice. Depending on the
#'   contestant's choice, the function will open the opposite door.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns the contestant's final choice.
#'
#' @examples
#'   change_door()
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
  doors <- c(1,2,3)

  if( stay )
  {
    final.pick <- a.pick
  }
  if( ! stay )
  {
    final.pick <- doors[ doors != opened.door & doors != a.pick ]
  }

  return( final.pick )  # number between 1 and 3
}



#' @title
#'   Determine if the contestant wins.
#'
#' @description
#'   `determine_winner()` returns if the contestant wins or loses.
#'
#' @details
#'   Once the contestant makes their final selection, the door
#'   that they chose is opened. Upon seeing the results, the
#'   show can determine if they win the car or win the goat.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns win or lose.
#'
#' @examples
#'   determine_winner()
#'
#' @export
determine_winner <- function( final.pick, game )
{
  if( game[ final.pick ] == "car" )
  {
    return( "WIN" )
  }
  if( game[ final.pick ] == "goat" )
  {
    return( "LOSE" )
  }
}





#' @title
#'   Play Monty Hall problem game.
#'
#' @description
#'   `play_game()` runs the Monty Hall problem steps.
#'
#' @details
#'   The game sets up and replicates the game on the TV show
#'   "Let's Make a Deal". It begins by creating the game,
#'   setting up three doors, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. Once the contestant
#'   selects their answer, the game determines if the contestant
#'   wins or loses.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function runs the game from start to finish.
#'
#' @examples
#'   play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'   Play Monty Hall problem game 100 times.
#'
#' @description
#'   `play_n_game()` runs the Monty Hall problem 100 times.
#'
#' @details
#'   The game replicates the game on the TV show
#'   "Let's Make a Deal" 100 times to determine if it is better to
#'   stay or switch the door once the goat door is opened. It
#'   shows what is the best strategy for the game.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns the wins and loses after 100 games.
#'
#' @examples
#'   play_n_game()
#'
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
    prop.table( margin=1 ) %>%  # row proportions
    round( 2 ) %>%
    print()

  return( results.df )

}

