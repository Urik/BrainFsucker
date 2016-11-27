module Instructions

type Instruction = 
    | Point
    | Comma
    | Plus
    | Minus
    | Increment
    | Decrement
    | StartOfLoop
    | EndOfLoop
    | Debug

let parseProgram (brainfuckProgram:string) =
    let rec parseProgram' rawInstructions =
        match rawInstructions with
        | [] -> []
        | instruction::rawProgram -> 
            let parsedInstruction = 
                match instruction with
                | '.' -> Some Point
                | ',' -> Some Comma
                | '+' -> Some Plus
                | '-' -> Some Minus
                | '>' -> Some Increment
                | '<' -> Some Decrement
                | '[' -> Some StartOfLoop
                | ']' -> Some EndOfLoop
                | '#' -> Some Debug
                | _ -> None
            match parsedInstruction with
            | Some instruction' -> instruction'::(parseProgram' rawProgram)
            | None -> parseProgram' rawProgram

    let separatedInstructions = [for c in brainfuckProgram -> c]
    parseProgram' separatedInstructions

let skipLoop instructions =
    let rec iterate passedLoops instructions =
        match instructions with
        | [] -> failwith "Unmatched end of loop"
        | StartOfLoop::xs -> iterate (passedLoops+1) xs
        | EndOfLoop::xs ->
            if passedLoops=0 
            then EndOfLoop::xs 
            else iterate (passedLoops-1) xs
        | _::xs -> iterate passedLoops xs
    
    iterate 0 instructions