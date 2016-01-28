import Effects exposing (Never)
import Game
import StartApp
import Task


app =
  StartApp.start
    { init =  (Game.initialState, Effects.tick Game.Init)
    , update = Game.update
    , view = Game.view
    , inputs = []
    }


main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
