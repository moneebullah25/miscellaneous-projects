import inquirer from 'inquirer';
function validateNumber(n) {
    if (isNaN(n))
        return "Please enter a valid number";
    return true;
}
let rightAns = Math.floor(Math.random() * 101);
let count = 0;
while (true) {
    const answer = await inquirer.prompt([
        {
            type: "number",
            name: "guessNumber",
            message: "Guess a number to get right answer",
            validate: validateNumber
        }
    ]);
    let guessed = Number(answer.guessNumber);
    if (guessed === rightAns) {
        console.log(`You make the right guess in ${count + 1} turns`);
        break;
    }
    else if (guessed < rightAns)
        console.log("Guess bigger number");
    else
        console.log("Guess smaller number");
    count++;
}
