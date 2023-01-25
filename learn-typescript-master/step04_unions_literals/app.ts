type Customer = {
    birthdate: {
        year: string,
        month: string,
        day: string
    } | undefined;
    name: string | undefined
} | undefined;

let customer: Customer;
console.log(customer?.birthdate?.year);
//console.log(customer.birthdate.year);//Error; poosible undefined

// let customer1: Customer=undefined;
// console.log(customer1?.birthdate?.year); //Error; customer1 is initialized undefined

let myname: string | null;

myname = null;
console.log(myname);

myname = "zia";
console.log(myname);

//myname = undefined; //Error
//myname = 12; //Error

let myAge: string | number;

myAge = 16;//narrowing
console.log(myAge);

//console.log(myAge.toLowerCase());//Error

myAge = "Dont Know";//narrowing
console.log(myAge);

console.log(myAge.toString()); // common to both types 
                               //can be called even without narrowing

console.log(myAge.toLowerCase());//Can be called on string 
                                //because of narrowing

let newAge = Math.random() > 0.6 ? "Khan": 60;

//newAge.toLowerCase();//Error: Transpiler cannot narrow

// === also checks type and data, == only check data
if (newAge === "Khan") { 
    // Type of newAge: string
    newAge.toUpperCase(); // Can be called
}

if(typeof newAge === "string"){
    // Type of newAge: string
    newAge.toUpperCase(); // Can be called
}

typeof newAge === "string"
? newAge.toUpperCase() // Ok: string
: newAge.toFixed(); // Ok: number


let age: number | "died" | "unknown";

age = 90;//OK
age = "died";//OK
age = "unknown";//OK
//age = "living";//Error


let zia: "zia";

zia = "zia";
//zia = "khan";//Error


let yourName = Math.random() > 0.9 ? "Hira Khan": null;

if (yourName) {// check for undefined and null, however unknown is datatype can't be use as a value
    yourName.toUpperCase(); // Ok: string
}

//yourName.toUpperCase();//Error: Object is possibly 'undefined'.

yourName?.toUpperCase();//OK check for undefined and null

// You can also define a type alias
type RawData = boolean | number | string | null | undefined;

let data: RawData;

// You can even combine them

type Id = number | string;

type IdMaybe = Id | undefined | null;


