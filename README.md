L-system plant in [Reprocessing](https://github.com/Schmavery/reprocessing)
---
<img width="600" alt="screen shot 2018-04-08 at 12 11 35" src="https://user-images.githubusercontent.com/12150276/38467034-7275ed62-3b2a-11e8-997c-e64011cc9269.png">

An L-system or Lindenmayer system is a parallel rewriting system and a type of formal grammar. An L-system consists of an alphabet of symbols that can be used to make strings, a collection of production rules that expand each symbol into some larger string of symbols, an initial "axiom" string from which to begin construction, and a mechanism for translating the generated strings into geometric structures.

This fractal plant is based on 
```
variables : X F
constants : + − [ ]
start  : X
rules  : (X → F+[[X]-X]-F[-FX]+X), (F → FF)
angle  : 25°
```

Here, F means "draw forward", − means "turn left 25°", and + means "turn right 25°". X does not correspond to any drawing action and is used to control the evolution of the curve. The square bracket "[" corresponds to saving the current values for position and angle, which are restored when the corresponding "]" is executed.

## How to Run

### Clone
```
https://github.com/Rigellute/L-system-reasonml.git
```

### Install
```
yarn
```

### Build
```
yarn build
```

### Start
```
yarn start
```

To build to JS run `npm run build:web` and then run a static server, like `python -m SimpleHTTPServer` and go to `localhost:8000`. If you're using safari you can simply open the `index.html` and tick `Develop > Disable Cross-Origin Restrictions`.

To build to native run `npm run build:native` and run `npm run start:native`

The build system used is [bsb-native](https://github.com/bsansouci/bsb-native).
