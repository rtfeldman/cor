type memory_cell = Word of int | Block of memory_cell list
type memory = (string * memory_cell) list
