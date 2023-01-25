import inquirer from 'inquirer';

function validateNumber(x: any) : string | boolean {
    if (isNaN(x)) return "Please Enter a valid Number ";
    else return true;
}

type Answers = {
    firstNumber : string,
    secondNumber: string,
    operators : "+" | "-" | "*" | "/" | "%" | "^"
}

var repeat: boolean = true;

async function getInput(){
const answers : Answers = await inquirer.prompt(
[{
    type: "number",
    name: "firstNumber",
    validate: validateNumber,
    message: "Enter first number"
}, 
{
    type: "number",
    name: "secondNumber",
    validate: validateNumber,
    message: "Enter second number"
},
{
    type: 'list',
    choices: ["+", "-", "*", "/", "%", "^"],
    name: "operators",
    message: "Choose one Option"
}])

const fno = Number(answers.firstNumber);
const sno = Number(answers.secondNumber);
switch(answers.operators){
    case "+":
        console.log("Result : ", fno + sno); break;
    case "-":
        console.log("Result : ", fno - sno); break;
    case "*":
        console.log("Result : ", fno * sno); break;
    case "/":
        console.log("Result : ", fno / sno); break;
    case "%":
        console.log("Result : ", fno % sno); break;
    case "^":
        console.log("Result : ", Math.pow(fno, sno)); break;
    default: break;
    }

}

do{
    await getInput();
    const ans = await inquirer.prompt([
        {
            type: "confirm",
            name: "confirm", 
            message: "Do you want to run the program again"
        }
    ])
    repeat = ans.confirm;
} while(repeat);