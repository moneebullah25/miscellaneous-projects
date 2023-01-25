class FieldTrip {
    destination: string;
    constructor(destination: string) {
        this.destination = destination; // Ok
        console.log(`We're going to ${this.destination}!`);
        // this.nonexistent = destination; // Error: Property 'nonexistent' does not exist on type 'FieldTrip'.
    }
}
   

class Greeter {
    name: string;
    constructor(name: string){
        this.name = name; // Without if definately assignment
        // if (name !== undefined) this.name = name; // With if not definately assignment
    }

    err() {
        // this.name = "Hello"; // Error can't modify readonly variable
    }

    greet(){
        console.log("Hello " + this.name);
    }    
}

let g = new Greeter("Hello");
console.log(g.name);
new Greeter("Muneeb").greet();

class Panaverse {
    std_section: string;
    std_count: number;
    city: string;

    
    constructor(std_section: string, std_count: number, city: string)
    {
        this.std_section =std_section;
        this.std_count = std_count;
        this.city = city.toUpperCase();
    }

    /* Overloading constructor not working */
    // constructor(std_count: number, city: string)
    // {
    //     this.std_count = std_count;
    //     this.city = city.toUpperCase();
    //     this.std_section = "A";
    // }

    
    getDetail() {
        console.log("Section: ", this.std_section);
        console.log("Student Count: ", this.std_count);
        console.log("City: ", this.city);
    }
}

const panaverse_krc = new Panaverse("A", 500, "Karachi");
const panaverse_lhr = new Panaverse("A", 500, "Lahore");
panaverse_krc.getDetail();
panaverse_lhr.getDetail();


class A { 
    k: string; 
    constructor(k: string="A"){this.k = k;} 
    set setK(k: string){this.k = k;}
    get getK(){return this.k;}
}
class B extends A { 
    l: string; 
    constructor(l: string="B"){super(); this.l = l;} 
    set setL(l: string) {this.l = l}
    get getL(){return this.l;}
}
class C extends B { 
    m: string;
    constructor(m: string="C") {
        super();
        this.m = m;
        // console.log(super.k, super.l, this.m); // undefined undefined C
    }
    printDetail() {
        super.k = "A"; super.l="B";
        console.log(super.getK, super.getL, this.m); // undefined undefined C
    } 
    get getM(){return this.m;}
    set setM(m: string){this.m = m;}
}

let c = new C;
c.printDetail();
c.setM = "C1";
console.log(c.getM);