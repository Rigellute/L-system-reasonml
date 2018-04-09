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
let maxIterations = 3;

type state = {
    iterationCount: int,
    sentence: list(char),
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
let drawBranch = (branchLength: float, env: envType) => {
    Draw.line(~p1=(0, 0), ~p2=(0, int_of_float(-. branchLength)), env);
    Draw.translate(~x=0.0, ~y= -. branchLength, env);
};

let rec generate = (strList: list(char), iterations: int) => {

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

        generate(sentenceArray, iterations + 1)
    } else {
        List.fold_left((str, char: char) => str ++ String.make(1, char), "", strList)
    };

    newSentence
};

let turtle = (initBranchLength: float, sentence: string, env: envType) => {
    Draw.translate(~x=float_of_int(windowDimension) /. 2.0, ~y=float_of_int(windowDimension), env);
    Draw.stroke(Utils.color(~r=113, ~g=247, ~b=159, ~a=190), env);

    let sentenceArr = split([], sentence);

    let rec turtleRun = (lst: list(char), branchLength: float) => {
        let [head, ...tail] = lst;
        let _ = if (List.length(tail) > 0) {
            switch head {
                | 'F' => drawBranch(branchLength, env)
                | '+' => turnRight(env)
                | '-' => turnLeft(env)
                | '[' => save(env)
                | ']' => revert(env)
                | _ => ()
            };

            let newBranchLength = switch head {
                | "F" =>  branchLength *. 0.5
                | _ => branchLength
            };

            turtleRun(tail, newBranchLength);
        };
    };

    turtleRun(sentenceArr, initBranchLength);
};

let setup = (env) => {
    Env.size(~width=windowDimension, ~height=windowDimension, env);

    Draw.background(Utils.color(~r=51, ~g=51, ~b=51, ~a=255), env);

    Draw.translate(~x=float_of_int(windowDimension) /. 2.0, ~y=float_of_int(windowDimension), env);
    Draw.stroke(Utils.color(~r=113, ~g=247, ~b=159, ~a=190), env);
    Draw.strokeWeight(1, env);
    let sentence = generate([axiom], 0);

    print_endline(sentence);

    let initState = {
        sentence: split([], sentence),
        iterationCount: 0,
        branchLength: initBranchLength,
    };

    turtle(initBranchLength, sentence, env);

run(~setup, ());
