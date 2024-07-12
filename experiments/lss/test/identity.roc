# cor +solve -elab

let id = \x -> x;;
#   ^^

run idint = id 1;;
#   ^^^^^
run idstr = id "hello";;
#   ^^^^^
