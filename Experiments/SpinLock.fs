module spin

open System.Threading

let safeCount() =
  let n = 1000000
  let counter = ref 0
  let spinlock = ref <| SpinLock(false)
  let run i0 i1 () =
    for i=i0 to i1-1 do
      let locked = ref false
      try
        spinlock.contents.Enter locked
        if !locked then
          counter := !counter + 1
      finally
        if !locked then
          spinlock.contents.Exit()
  let thread = System.Threading.Thread(run 0 (n/2))
  thread.Start()
  run (n/2) n ()
  thread.Join()
  !counter
  
safeCount()