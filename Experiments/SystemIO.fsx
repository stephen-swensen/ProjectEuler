open System.IO

let allFiles dir =
    let rec allFiles depth dir =
        seq { for file in Directory.GetFiles dir -> (file,dir,depth)
              for subdir in Directory.GetDirectories dir do yield! subdir |> allFiles (depth+1) }
    allFiles 0 dir
    
let allDirs dir =
    let rec allDirs depth dir =
        seq { for subdir in Directory.GetDirectories dir do 
                  yield (subdir,depth)
                  yield! subdir |> allDirs (depth+1) }
    allDirs 0 dir

let deleteAllMatchingDirs matchingName dir =
    allDirs dir
    |> Seq.filter(fun (dir,_) -> dir.EndsWith(@"\" + matchingName))
    |> Seq.sortBy (((-) 0) << snd)
    |> Seq.iter(
        fun (dir,_) -> 
            printfn "deleting dir %A" dir
            allFiles dir |> Seq.iter(
                fun (file,_,_) -> 
                    printfn "deleting file %A" file
                    FileInfo(file).Delete()
                    printfn "deleted file %A" file)
            Directory.Delete(dir,true)
            printfn "deleted dir %A" dir)
            
let deleteAllSvnDirs = deleteAllMatchingDirs ".svn"