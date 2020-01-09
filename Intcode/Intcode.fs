namespace Intcode

type Input = | Persistent of int64 | Draining of int64 list

type Program = {
    instructionPtr: int64
    input: Input option
    code: Map<int64, int64>
    relativeBase: int64
    lastOutput: int64 option
}

module Program =
    let private instructionBinaryOp = function
        | 1 -> (+)
        | 2 -> (*)
        | 7 -> fun n m -> if n < m then 1L else 0L
        | 8 -> fun n m -> if n = m then 1L else 0L
        | _ -> failwith "Invalid opcode"

    let private instructionUnaryOp = function
        | 5 -> ((<>) 0L)
        | 6 -> ((=) 0L)
        | _ -> failwith "Invalid opcode"

    type ProcessEvent = | Output of (int64 * Program) | ProgramEnd

    let rec processOpCode program =
        let opCode = int(Map.find program.instructionPtr program.code)
        let getParameterMode paramIndex = opCode / (pown 10 (paramIndex + 2)) % 10
        let getValueByMode instructionPtr = function
            | 0 -> Map.tryFind program.code.[instructionPtr] program.code |> Option.defaultValue 0L
            | 1 -> program.code.[instructionPtr]
            | 2 -> Map.tryFind (program.relativeBase + program.code.[instructionPtr]) program.code |> Option.defaultValue 0L
            | _ -> failwith "Invalid opcode"

        let getInputParameterValue paramIndex =
            getValueByMode (program.instructionPtr + (int64 paramIndex) + 1L) (getParameterMode paramIndex)

        let getOutputParameterPosition paramIndex =
            let offset = if (getParameterMode paramIndex) = 2 then program.relativeBase else 0L
            offset + program.code.[program.instructionPtr + (int64 paramIndex) + 1L]

        let (|BinaryOp|JumpIf|WriteInput|PrintOutput|AdjustRelativeBase|ProgramEnd|) = function
            | 1 | 2 | 7 | 8 as instruction ->
                let firstParameter = (int64 (getInputParameterValue 0))
                let secondParamter = (int64 (getInputParameterValue 1))
                BinaryOp ((instructionBinaryOp instruction) firstParameter secondParamter)
            | 3 -> WriteInput
            | 4 -> PrintOutput (getInputParameterValue 0)
            | 5 | 6 as instruction ->
                let testResult = ((instructionUnaryOp instruction) (getInputParameterValue 0))
                JumpIf (if testResult then (getInputParameterValue 1) else program.instructionPtr + 3L)
            | 9 -> AdjustRelativeBase (getInputParameterValue 0)
            | 99 -> ProgramEnd
            | _ -> failwith "Invalid opcode"

        match opCode % 100 with
        | BinaryOp result ->
            let resultPos = getOutputParameterPosition 2
            let newState =
                { program with
                    instructionPtr = program.instructionPtr + 4L
                    code = Map.add resultPos result program.code }

            processOpCode newState
        | JumpIf nextAddress -> processOpCode { program with instructionPtr = nextAddress }
        | WriteInput ->
            let outputPosition = getOutputParameterPosition 0
            match program.input with
            | Some (Persistent input) ->
                let newState =
                    { program with
                        instructionPtr = program.instructionPtr + 2L
                        code = Map.add outputPosition input program.code }

                processOpCode newState
            | Some (Draining (input :: rest)) ->
                let newState =
                    { program with
                        instructionPtr = program.instructionPtr + 2L
                        input = Some (Draining rest)
                        code = Map.add outputPosition input program.code }

                processOpCode newState
            | Some (Draining []) -> failwith "Too few input provided"
            | None -> failwith "No input provided for program"
        | PrintOutput value ->
            let newState =
                { program with instructionPtr = program.instructionPtr + 2L; lastOutput = Some value }
            Output (value, newState)
        | AdjustRelativeBase adjustment ->
            let newState = 
                { program with
                    instructionPtr = program.instructionPtr + 2L
                    relativeBase = program.relativeBase + adjustment }

            processOpCode newState
        | ProgramEnd -> ProgramEnd

    let create input program =
        {
            instructionPtr = 0L
            input = input
            code = Seq.indexed program |> Seq.map (fun (index, code) -> int64 index, code) |> Map.ofSeq
            relativeBase = 0L
            lastOutput = None
        }

    let rec iterateOutput state =
        seq {
            match processOpCode state with
            | Output (output, state) ->
                yield output
                yield! iterateOutput state
            | ProgramEnd -> ()
        }
