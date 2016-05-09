import Graphics.Element exposing (show)
import Task exposing (Task)
import TaskTutorial exposing (print)
import Time exposing (second, Time)


-- A signal that updates to the current time every second
clock : Signal Time
clock =
  Time.every second


-- Turn the clock into a signal of tasks
printTasks : Signal (Task x ())
printTasks =
  Signal.map print clock


-- Actually perform all those tasks
port frunner : Signal (Task x ())
port frunner =
  printTasks

-- main : Graphics.Element.Element
-- main =
--   show "Open your browser's Developer Console."
