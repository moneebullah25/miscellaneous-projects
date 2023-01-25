// function validateCourse(course: any)
// {
//     type ErrorType = {
//         details: [{
//             message: string
//         }
//         ]
//     }
    
//     let error: ErrorType = {
//         details: [{
//             message: ""
//         }
//         ]
//     }

//     let result = {
//         error
//     }

//     if (!course.body.name) result.error.details[0].message = "name is required";
//     else if (course.body.name.length < 3) result.error.details[0].message = "name should be more than 3 characters";
//     return result;
// }

// let course = {
//     body: {
//     }
// }

// console.log(validateCourse(course).error.details[0].message);


/* PIAIC Class 11 Dec 2022 */
let x: number = 12;

let a: {name: string, name2: string} = {name: "Muneeb", name2: "Moiz"};
let b: {name: string} = a;

console.log(b);
console.log(a);