// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
module BrainFuckInstruction
type Instruction = 
    | Point
    | Comma
    | Plus
    | Minus
    | Increment
    | Decrement
    | StartOfLoop
    | EndOfLoop
and MachineState = {
    tape: byte list;
    position: int;
    instructions: Instruction list
}

type Program = Instruction list

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
                | _ -> None
            match parsedInstruction with
            | Some instruction' -> instruction'::(parseProgram' rawProgram)
            | None -> parseProgram' rawProgram

    let separatedInstructions = [for c in brainfuckProgram -> c]
    parseProgram' separatedInstructions

let initializeMachine instructions =
    {
        tape = [ for i in 0 .. 30000 -> 0uy ];
        position = 0;
        instructions = instructions
    }

let advanceInstruction ({ instructions = instructions } as state) =
    match instructions with
    | [] -> state
    | x::xs -> { state with instructions = xs }

let processProgram initialState =
    let processTapeElem elemPosition tape elemAction =
        let myTape = tape |> List.mapi (fun currentPosition currentElem ->
                if currentPosition = elemPosition 
                then elemAction currentElem 
                else currentElem)
        myTape
    let operateOnTapeElem state = processTapeElem state.position state.tape    

    let rec processRoutine instructionsAtBeginningOfRoutine currentState = 
        let handleInstruction instruction =
            let newState = 
                match instruction with
                | Increment -> { currentState with position = currentState.position+1 }
                | Decrement -> { currentState with position = currentState.position-1 }
                | Plus -> { currentState with tape = operateOnTapeElem currentState (fun elem -> elem+1uy) }
                | Minus -> { currentState with tape = operateOnTapeElem currentState (fun elem -> if elem > 0uy then elem-1uy else 0uy) }
                | StartOfLoop -> processRoutine (Some currentState.instructions.Tail) (currentState |> advanceInstruction)
                | EndOfLoop -> 
                    match instructionsAtBeginningOfRoutine with
                    | Some [] -> currentState
                    | Some (_::xs as instructions) -> 
                        if (List.item currentState.position currentState.tape) <> 0uy
                        then processRoutine instructionsAtBeginningOfRoutine { currentState with instructions = instructions }
                        else currentState
                    | None ->
                        failwith "Unmatched end of loop operator"
                | _ -> currentState
                |> advanceInstruction
            newState

        let newState = handleInstruction currentState.instructions.Head 
        match newState.instructions with 
        | [] -> newState
        | x::xs -> processRoutine instructionsAtBeginningOfRoutine newState

    processRoutine None initialState


[<EntryPoint>]
let main argv = 
    let finalState = 
        "+++>++[-]<+++"
            |> parseProgram
            |> initializeMachine
            |> processProgram
    0 // return an integer exit code
