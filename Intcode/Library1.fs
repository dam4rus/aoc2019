namespace Intcode

module Computer =
    let private instructionBinaryOp = function
        | 1 -> (+)
        | 2 -> (*)
        | 7 -> fun n m -> if n < m then 1 else 0
        | 8 -> fun n m -> if n = m then 1 else 0
        | _ -> failwith "Invalid opcode"

    let private instructionUnaryOp = function
        | 5 -> ((<>) 0)
        | 6 -> ((=) 0)
        | _ -> failwith "Invalid opcode"

    let private getValueByMode instructionPtr (state: int array) = function
        | 0 -> state.[state.[instructionPtr]]
        | 1 -> state.[instructionPtr]
        | _ -> failwith "Invalid opcode"

    type Input = | Persistent of int | Draining of int list
    
    type State = {
        instructionPtr: int
        inputValue: Input option
        program: int []
        output: int list
    }

    let rec private processOpCode state =
        let opCode = (Array.item state.instructionPtr state.program)
        let getParameterMode paramIndex = opCode / (pown 10 (paramIndex + 2)) % 10
        let getParameterValue paramIndex = getValueByMode (state.instructionPtr + paramIndex + 1) state.program (getParameterMode paramIndex)

        let (|BinaryOp|JumpIf|WriteInput|PrintOutput|ProgramEnd|) = function
            | 1 | 2 | 7 | 8 as instruction -> BinaryOp ((instructionBinaryOp instruction) (getParameterValue 0) (getParameterValue 1))
            | 5 | 6 as instruction -> JumpIf ((instructionUnaryOp instruction) (getParameterValue 0))
            | 3 -> WriteInput
            | 4 -> PrintOutput
            | 99 -> ProgramEnd
            | _ -> failwith "Invalid opcode"

        match opCode % 100 with
        | BinaryOp result ->
            let resultPos = state.program.[state.instructionPtr + 3]
            state.program.[resultPos] <- result
            processOpCode { state with instructionPtr = state.instructionPtr + 4 }
        | JumpIf result ->
            let nextAddress = if result then (getParameterValue 1) else state.instructionPtr + 3
            processOpCode { state with instructionPtr = nextAddress }
        | WriteInput ->
            let outputPosition = state.program.[state.instructionPtr + 1]
            match state.inputValue with
            | Some (Persistent input) ->
                state.program.[outputPosition] <- input
                processOpCode { state with instructionPtr = state.instructionPtr + 2 }
            | Some (Draining (input :: rest)) -> 
                state.program.[outputPosition] <- input
                processOpCode { state with instructionPtr = state.instructionPtr + 2; inputValue = Some (Draining rest) }
            | Some (Draining []) -> failwith "Too few input for program"
            | None -> failwith "No input provided for program"
        | PrintOutput ->
            let position = state.program.[state.instructionPtr + 1]
            processOpCode { state with instructionPtr = state.instructionPtr + 2; output = state.output @ [ state.program.[position] ] }
        | ProgramEnd -> state

    let run inputValue program =
        processOpCode { instructionPtr = 0; inputValue = inputValue; program = program; output = List.empty }

    let runWithOutput inputValue program =
        (run inputValue program).output