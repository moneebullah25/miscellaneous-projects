let myname: unknown = "Zia";
console.log((myname as string).length);
console.log((<string> myname).length);

// let mynum: unknown = 100;
// console.log(Math.pow((<number> myname), 2)); // NaN