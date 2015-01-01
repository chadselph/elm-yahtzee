import Random
import Time
import Signal
import Text (asText)

intTimeSignal = Signal.map
  (\(time,_) -> round time)
  (Time.timestamp (Signal.constant ()))

seededRandomSignal = Signal.map Random.initialSeed intTimeSignal

main = Signal.map asText intTimeSignal
