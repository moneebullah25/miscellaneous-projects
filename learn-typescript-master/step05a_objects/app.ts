let teacher = {
    name: "Zeeshan",
    experience: "10"
}

console.log(teacher.name);
console.log(teacher["experience"]);


// Type Declaration
type Student = {
    name: string,
    age?: number // Optional Parameter
}

let student: Student = {
    name: "Hira",
}

console.log(student["name"]);
console.log(student.age);

        
