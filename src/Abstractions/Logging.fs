namespace FsEventStorage.Logging

type ILog =
    abstract member Append: bool -> unit
