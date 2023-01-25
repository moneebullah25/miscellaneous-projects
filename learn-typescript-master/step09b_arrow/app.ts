// var add: (a: number, b: number) => string // Automatically deduce //
var add = (a: number, b: number) => { 
    return a + "1";
}

let add1 = (x: number, y: number) => (x + y);
// output var add1 = function(x, y){return x + y};

