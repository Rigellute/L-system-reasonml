open Reprocessing;

/**
 * L-system details -
 * variables : F
 * constants : + − [ ]
 * start  : X
 * rule(s)  :  (X → F+[[X]-X]-F[-FX]+X), (F → FF)
 * angle  : 25°
 * "F" means "draw forward" "−" means "turn left by angle (25°)", and + means "turn right 25°".
 * The square bracket "[" corresponds to saving the current values for position and angle, which are restored when the corresponding "]" is executed.
 */
let _DIMENSION = 600;
let c = 4.0;
let xRule = "F+[[X]-X]-F[-FX]+X";
let fRule = "FF";
let axiom = "X";
let angle = Utils.radians(25.);
let initBranchLength = 300.0;
let minBranchLength = 5.0;

type state = {
    branchLength: float,
    sentence: string,
};

type envType = Reprocessing_Types.Types.glEnvT;

let save = (env: envType) => {
    Draw.pushMatrix(env);
};
let revert = (env: envType) => {
    Draw.popMatrix(env);
};
let turnRight = (env: envType) => {
    Draw.rotate(angle, env);
};
let turnLeft = (env: envType) => {
    Draw.rotate(-. angle, env);
};
let drawBranch = (state: state, env: envType) => {
    Draw.line(~p1=(0, 0), ~p2=(0, int_of_float(-. state.branchLength)), env);
    Draw.translate(~x=0.0, ~y= -. state.branchLength, env);
};


let setup = (env) => {
    Env.size(~width=_DIMENSION, ~height=_DIMENSION, env);

    Draw.background(Utils.color(~r=51, ~g=51, ~b=51, ~a=255), env);

    let initState = {
        branchLength: initBranchLength,
        sentence: axiom
    };

    initState
};

let generate = (state: state, strList: list(string)) => {
    let newSentence = List.fold_left((str, char) => {
        let sentence = switch char {
            | "" => ""
            | "X" => str ++ xRule
            | "F" => str ++ fRule
            | _ => char ++ state.sentence
        };

        sentence
    }, "", strList);

    newSentence
};

let turtle = (state: state, env) => {
    Draw.translate(~x=float_of_int(_DIMENSION) /. 2.0, ~y=float_of_int(_DIMENSION), env);
    Draw.stroke(Utils.color(~r=255, ~g=255, ~b=255, ~a=51), env);

    let sentence = Str.split(Str.regexp({||}), state.sentence);

    let rec turtleRun = (lst: list(string)) => {
        let [head, ...tail] = lst;

        let _ = if (List.length(tail) > 0) {
            switch head {
                | "F" => drawBranch(state, env)
                | "+" => turnRight(env)
                | "-" => turnLeft(env)
                | "[" => save(env)
                | "]" => revert(env)
                | _ => ()
            };

            turtleRun(tail);
        };
    };

    turtleRun(sentence);
};

let draw = (state: state, env) => {
    let sentence = if (state.branchLength > minBranchLength) {
        let s = generate(state, Str.split(Str.regexp({|,|}), state.sentence));
        turtle(state, env);
        print_endline(s);
        s
    } else {
        state.sentence
    };

    {
        sentence: sentence,
        branchLength: state.branchLength *. 0.5
    }
};

run(~setup, ~draw, ());
