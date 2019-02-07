module Now.Option

let ofBool x = function
| true -> Some x
| false -> None