open Reprocessing;

/**
 * L-system details -
 * variables : X F
 * constants : + − [ ]
 * start  : X
 * rule(s)  :  (X → F+[[X]-X]-F[-FX]+X), (F → FF)
 * angle  : 25°
 * "F" means "draw forward" "−" means "turn left by angle (25°)", and + means "turn right 25°".
 * The square bracket "[" corresponds to saving the current values for position and angle, which are restored when the corresponding "]" is executed.
 */
let windowDimension = 600;
let xRule = "F+[[X]-X]-F[-FX]+X";
let fRule = "FF";
let axiom = 'X';
let angle = Utils.radians(25.);
let initBranchLength = 100.0;
let minBranchLength = 3.0;
let maxIterations = 2;

type state = {
    sentence: list(string),
    branchLength: float,
};

type envType = Reprocessing_Types.Types.glEnvT;

let rec split = (acc: list(char), str: string) => {
  	let len = String.length(str);
	let stringList = if (len > 0) {
  		split(acc @ [str.[0]], String.sub(str, 1, len - 1))
  	} else {
      acc
  };

  stringList
};

let save = (env: envType) => {
    print_endline("saving");

    Draw.pushMatrix(env);
};
let revert = (env: envType) => {
    print_endline("reverting");
    Draw.popMatrix(env);
};
let turnRight = (env: envType) => {
    print_endline("turning right");
    Draw.rotate(angle, env);
};
let turnLeft = (env: envType) => {
    print_endline("turning left");
    Draw.rotate(-. angle, env);
};
let drawBranch = (branchLength: float, env: envType) => {
    print_endline("Branch length " ++ string_of_float(branchLength));
    Draw.line(~p1=(0, 0), ~p2=(0, int_of_float(-. branchLength)), env);
    Draw.translate(~x=0.0, ~y= -. branchLength, env);
};

let headAndTail = (xs: list('a)) => (List.hd(xs), List.tl(xs));

let rec generate = (accumulator: list(string), strList: list(char), iterations: int) => {
    let newSentence = if (iterations <= maxIterations) {
        let sent = List.fold_left((str, char: char) => {
            let sentence = switch char {
                | 'X' => str ++ xRule
                | 'F' => str ++ fRule
                | _ => str ++ String.make(1, char)
            };

            sentence
        }, "", strList);

        let sentenceArray = split([], sent);

        generate(accumulator @ [sent], sentenceArray, iterations + 1)
    } else {
        accumulator
    };

    newSentence
};

let turtle = (sentence: string, env: envType, branchLength: float) => {
    let sentenceArr = split([], sentence);

    let rec turtleRun = (lst: list(char)) => {
        let _ = if (List.length(lst) > 0) {
            let (head, tail) = headAndTail(lst);
            switch head {
                | 'F' => drawBranch(branchLength, env)
                | '+' => turnRight(env)
                | '-' => turnLeft(env)
                | '[' => save(env)
                | ']' => revert(env)
                | _ => ()
            };

            turtleRun(tail);
        };
    };

    turtleRun(sentenceArr);
};

let setup = (env) => {
    Env.size(~width=windowDimension, ~height=windowDimension, env);

    Draw.background(Utils.color(~r=51, ~g=51, ~b=51, ~a=255), env);
    Draw.translate(~x=float_of_int(windowDimension) /. 2.0, ~y=float_of_int(windowDimension), env);
    Draw.stroke(Utils.color(~r=113, ~g=247, ~b=159, ~a=190), env);
    Draw.strokeWeight(1, env);

    let sentence = generate([], [axiom], 0);

    {
        sentence: sentence,
        branchLength: initBranchLength
    }
};

let draw = (state, env) => {
    let newSentence = if (List.length(state.sentence) > 0) {
        let (head, tail) = headAndTail(state.sentence);
        turtle(head, env, state.branchLength);
        tail
    } else {
        []
    };
    {
        sentence: newSentence,
        branchLength: state.branchLength *. 0.5,
    }
};

run(~setup, ~draw, ());
